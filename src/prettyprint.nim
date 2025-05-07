import std/cmdline
import options
import sequtils
import strformat
import strutils
import sugar
import terminal

type OutputBuffer = ref object
  data: string
  lastLineBreak: int

proc add(buffer: OutputBuffer, text: string) =
  buffer.data.add(text)

proc currentLineLength(buffer: OutputBuffer): int =
  buffer.data.len - buffer.lastLineBreak

proc newline(this: OutputBuffer) =
  this.data.add("\n")
  this.lastLineBreak = this.data.len

type ParenType {.pure.} = enum
  paren
  squareBracket
  curlyBracket

# TODO consider using case-of to guarantee handling all cases
func opening(parenType: ParenType): char =
  const mapping =
    [ParenType.paren: '(', ParenType.squareBracket: '[', ParenType.curlyBracket: '{']
  mapping[parenType]

func closing(parenType: ParenType): char =
  const mapping =
    [ParenType.paren: ')', ParenType.squareBracket: ']', ParenType.curlyBracket: '}']
  mapping[parenType]

func closingWithComma(parenType: ParenType): set[char] =
  const mapping = [
    ParenType.paren: {',', ')'},
    ParenType.squareBracket: {',', ']'},
    ParenType.curlyBracket: {',', '}'},
  ]
  mapping[parenType]

type Tree = object
  prefix: string
  parenType: Option[ParenType]
  children: seq[Tree]
  suffix: string

proc init(
    _: typedesc[Tree],
    prefix: string,
    parenType: Option[ParenType] = none(ParenType),
    children: seq[Tree] = @[],
    suffix: string = "",
): Tree =
  Tree(prefix: prefix, parenType: parenType, children: children, suffix: suffix)

proc toString(tree: Tree): string =
  if tree.parenType.isNone:
    fmt"{tree.prefix}{tree.suffix}"
  else:
    let formattedChildren = tree.children.map((t) => t.toString).join(", ")
    fmt"{tree.prefix}{tree.parenType.get.opening}{formattedChildren}{tree.parenType.get.closing}"

proc lengthRemainsOf(tree: Tree, length: int): int =
  var leng = length
  leng -= tree.prefix.len
  leng -= tree.suffix.len
  leng -= (if tree.parenType.isNone: 0 else: 2)
  if leng >= 0:
    for child in tree.children:
      leng = child.lengthRemainsOf(leng)
      if leng < 0:
        break
  leng

proc lengthExceeds(tree: Tree, limit: int): bool =
  tree.lengthRemainsOf(limit) < 0

type QuotedText = ref object
  text: string
  textBeforeSkip: string

proc init(_: typedesc[QuotedText], text: string): QuotedText

proc consumeQuote(this: QuotedText): QuotedText =
  QuotedText.init(this.text)

proc textUntil(this: QuotedText, other: QuotedText): string =
  assert(this.textBeforeSkip.endsWith(other.text))
  this.textBeforeSkip[0 ..< this.textBeforeSkip.len - other.text.len]

proc empty(this: QuotedText): bool =
  this.text.len == 0

proc front(this: QuotedText): char =
  this.text[0]

proc skipQuote(this: QuotedText) =
  proc skippedQuote(marker: char, escapeChars: bool): bool =
    if this.text.len == 0 or this.text[0] != marker:
      return false
    this.text = this.text[1 ..^ 1]

    while this.text.len > 0 and this.text[0] != marker:
      if escapeChars and this.text[0] == '\\':
        this.text = this.text[1 ..^ 1]
      if this.text.len > 0:
        this.text = this.text[1 ..^ 1]

    if this.text.len > 0:
      this.text = this.text[1 ..^ 1]
    return true

  while (
    skippedQuote('"', true) or skippedQuote('\'', true) or skippedQuote('`', false)
  )
  :
    discard

proc popFront(this: QuotedText) =
  this.text = this.text[1 ..^ 1]
  this.textBeforeSkip = this.text
  this.skipQuote()

proc findAmong(this: QuotedText, chars: set[char]): QuotedText =
  var this = QuotedText(text: this.text, textBeforeSkip: this.textBeforeSkip)
  while not this.empty:
    if this.front in chars:
      return this
    this.popFront()
  return this

proc init(_: typedesc[QuotedText], text: string): QuotedText =
  var quotedText = QuotedText(text: text, textBeforeSkip: text)
  quotedText.skipQuote()
  quotedText

proc parseSuffix(range: var QuotedText, tree: Tree): Tree =
  assert(len(tree.suffix) == 0)
  result = tree
  if not range.empty and range.front == ',':
    range.popFront
    result.suffix = ","

proc parse(
    textRange: var QuotedText, expectedClosers: set[char] = {','}
): Option[Tree] =
  let parenStart = textRange.findAmong({'(', '{', '['})
  let closer = textRange.findAmong(expectedClosers)

  if textRange.textUntil(closer).len < textRange.textUntil(parenStart).len:
    let prefix = textRange.textUntil(closer)

    textRange = closer.consumeQuote

    return (
      if prefix.len == 0: none(Tree)
      else: some(textRange.parseSuffix(Tree.init(prefix)))
    )

  let prefix = textRange.textUntil(parenStart)

  if parenStart.empty:
    textRange = parenStart.consumeQuote

    return (
      if prefix.len == 0: none(Tree)
      else: some(textRange.parseSuffix(Tree.init(prefix)))
    )

  let parenType =
    case parenStart.front
    of '(':
      ParenType.paren
    of '[':
      ParenType.squareBracket
    of '{':
      ParenType.curlyBracket
    else:
      raiseAssert(fmt"Invalid ParenType char: {parenStart.front}")

  textRange = parenStart
  textRange.popFront

  var children: seq[Tree]

  while true:
    if textRange.empty:
      return none(Tree)
    if textRange.front == parenType.closing:
      let quoteChild = textRange.textUntil(textRange)

      if quoteChild.len > 0:
        children.add(Tree.init(quoteChild))

      textRange.popFront

      return some(
        textRange.parseSuffix(
          Tree(prefix: prefix, parenType: some(parenType), children: children)
        )
      )

    let child = textRange.parse(parenType.closingWithComma())

    if child.isNone:
      return none(Tree)

    children.add(child.get)

proc parse(text: string): seq[Tree] =
  var textRange = QuotedText.init(text)
  var trees: seq[Tree]

  # do-while
  while true:
    let tree = textRange.parse()

    if tree.isNone:
      return newSeq[Tree]()
    trees.add(tree.get)

    if textRange.empty():
      break

  return trees

const indent = repeat(" ", 4).join

proc prettyprint(buffer: OutputBuffer, tree: Tree, width: int) =
  proc renderSingleLine(tree: Tree) =
    if tree.parenType.isNone:
      buffer.add(tree.suffix)
      return
    buffer.add($tree.parenType.get.opening)
    for child in tree.children:
      buffer.add(child.prefix)
      renderSingleLine(child)
    buffer.add($tree.parenType.get.closing)
    buffer.add(tree.suffix)

  proc renderIndented(tree: Tree, level: int = 0) =
    let remainingWidth = width - buffer.currentLineLength

    buffer.add(
      if level == 0:
        tree.prefix
      else:
        tree.prefix.strip(trailing = false)
    )
    if not tree.lengthExceeds(remainingWidth):
      renderSingleLine(tree)
      return
    if tree.parenType.isNone:
      buffer.add(tree.suffix)
      return
    buffer.add($tree.parenType.get.opening)
    for child in tree.children:
      buffer.newline
      for _ in 0 .. level:
        buffer.add(indent)
      renderIndented(child, level + 1)
    buffer.newline
    for _ in 0 ..< level:
      buffer.add(indent)
    buffer.add($tree.parenType.get.closing)
    buffer.add($tree.suffix)

  renderIndented(tree)

proc prettyPrint(text: string, columnWidth: int = 80): string =
  let trees = text.parse

  if trees.len == 0:
    text
  else:
    var buffer: OutputBuffer
    new(buffer)
    for tree in trees:
      prettyprint(buffer, tree, columnWidth)
    buffer.data

when isMainModule:
  let columns = terminal.terminalWidth()

  let args = commandLineParams()

  proc processLines(file: File) =
    for line in file.lines():
      echo prettyprint(line, columns)

  if args.len == 0:
    processLines(stdin)
  else:
    let files = args.map((args) => open(args))
    for file in files:
      processLines(file)
