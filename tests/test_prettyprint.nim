import prettyprint {.all.}

import std/unittest
import std/strutils
import std/options

suite "prettyprint":
  test "pretty print a string":
    check(prettyprint("Foo") == "Foo")
    check(prettyprint("Foo(") == "Foo(")
    check(prettyprint("Foo()") == "Foo()")
    check(prettyprint("Foo[]") == "Foo[]")
    check(prettyprint("Foo{}") == "Foo{}")
    check(prettyprint("Foo(A, B)") == "Foo(A, B)")
    check(prettyprint("Foo(Bar(Baz()), Baq())", 16) == """
      Foo(
          Bar(Baz()),
          Baq()
      )""".dedent)
    check(prettyprint("Foo(Bar(Baz()), Baq())", 12) == """
        Foo(
            Bar(
                Baz(
                )
            ),
            Baq()
        )""".dedent)

  test "list of strings":
    check(prettyprint("""["a", "b"]""") == """["a", "b"]""")

  test "linebreak with comma separated elements without children":
    let filler = repeat("-", 80).join
    check(prettyprint(filler & """("a", "b")""") == filler & """
      (
          "a",
          "b"
      )""".dedent)

suite "parse":
  test "parse a paren expression to a tree":
    check(parse("Foo") == @[Tree.init("Foo")])
    check(parse("\"Foo\"") == @[Tree.init("\"Foo\"")])
    check(parse("Foo,") == @[Tree.init("Foo", none(ParenType), @[], ",")])
    check(parse("Foo, Bar") == @[
      Tree.init("Foo", none(ParenType), @[], ","),
      Tree.init(" Bar")
    ])
    check(parse("Foo()") == @[Tree.init("Foo", some(ParenType.paren))])
    check(parse("Foo[a, b]") == @[Tree.init(
      "Foo",
      some(ParenType.squareBracket),
      @[Tree.init("a", none(ParenType), @[], ","), Tree.init(" b")],
      ""
    )])
    check(parse("""Foo{"\""}""") == @[Tree.init(
        "Foo",
        some(ParenType.curlyBracket),
        @[Tree.init(""""\""""")]
      )])
    check(parse("Foo{`\"`}") == @[Tree.init(
      "Foo",
      some(ParenType.curlyBracket),
      @[Tree.init("`\"`")])])
    check(parse("Foo{'\"'}") == @[Tree.init(
      "Foo",
      some(ParenType.curlyBracket),
      @[Tree.init("'\"'")])])
    check(parse("Foo() Bar()") == @[
      Tree.init("Foo", some(ParenType.paren)),
      Tree.init(" Bar", some(ParenType.paren))])

  test "tree with trailing text":
    check(parse("""(() )""") == @[
      Tree.init(
        "",
        some(ParenType.paren),
        @[Tree.init("", some(ParenType.paren)), Tree.init(" ")]
      )
    ])

  test "quote followed by wrong closing paren":
    let text = """("",""]"""
    check(parse(text) == newSeq[Tree]())

  test "list of strings":
    check(parse("""["a", "b"]""") == @[Tree.init("", some(ParenType.squareBracket), @[
      Tree.init(""""a"""", none(ParenType), @[], ","),
      Tree.init(""" "b""""),
    ])])

  test "estimate the print length of a tree":
    check(parse("Foo(Bar(Baz()), Baq())")[0].lengthRemainsOf(10) < 0)

suite "QuotedText":
  test "\"\" quote at the beginning and end of a range":
    let range = QuotedText.init(""""Foo"""")

    check(range.textUntil(range) == """"Foo"""")

  test "`` quote at the beginning and end of a range":
    let range = QuotedText.init("`Foo\\`")

    check(range.textUntil(range) == "`Foo\\`")

  test "'' quote at the beginning and end of a range":
    let range = QuotedText.init("'Foo'")

    check(range.textUntil(range) == "'Foo'")
