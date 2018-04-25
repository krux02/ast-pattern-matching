import ast_pattern_matching, strformat, macros, unittest

type
  H = enum HInt, HString, HContainer

  C = enum CArray, CSeq

  Container = ref object
    case kind: C:
    of CArray:
      items: seq[A]
    of CSeq:
      elements: seq[A]

  A = ref object
    case kind: H:
    of HInt:
      i: int
    of HString:
      s: string
    of HContainer:
      c: Container

proc `$`(a: A): string =
  case a.kind:
  of HInt:
    &"A({a.i})"
  of HString:
    &"A({a.s})"
  of HContainer:
    &"A([..])"

proc hInt(i: int): A =
  A(kind: HInt, i: i)

proc hString(s: string): A =
  A(kind: HString, s: s)


let container = A(
  kind: HContainer,
  c: Container(
    kind: CSeq,
    elements: @[
      A(
        kind: HInt,
        i: 0)]))



macro matchLit*(typ: typed, litType: typed, f: untyped): untyped =
  discard


macro matchArgs*(typ: typed, f: untyped): untyped =
  discard

macro match*(obj: typed, args: varargs[untyped]): untyped =
  discard

macro matchtest(s: untyped, test: untyped): untyped =
  result = quote:
    test `s`:
      var expected {.inject.} = false
      `test`
      checkpoint "didn't match variant"
      check expected

template succeed: untyped =
  expected {.inject.} = true

suite "plan":
  matchtest "match different object":
    container.match:
    of HContainer(c: CSeq(elements: @[`value`])):
      echo value
      check value.kind == HInt and value.i == 0
      succeed
    else:
      discard

     
    # block match0:
    #   var error0: MatchingError
    #   block branch0:
    #     if not (container.kind == HContainer):
    #       error0 = ..
    #       break branch0
    #     if not (container.c.kind == CSeq):
    #       error0 = ..
    #       break branch0
    #     if not (container.c.elements.len == 0): # elements[0] is true
    #       error0 = ..
    #       break branch0
    #     let value = container.c.elements[0]
    #     echo value
    #     check value.kind == HInt and value.i == 0
    #     succeed
    #     break match0
    #   discard

  matchtest "match variant field":
    var a = A(kind: HInt, i: 0)

    a.match:
    of HInt(i: `i`):
      check i == 0
      succeed
    else:
      discard


  matchtest "match variant literal":
    A.matchLit int, HInt
    A.matchLit string, HString

    var a = A(kind: HInt, i: 0)
    var b = A(kind: HString, s: "e")

    a.match:
    of 0:
      succeed
    else:
      discard

    # block match0:
    #   var error0: MatchingError
    #   block branch0:
    #     if not (a.kind == HInt and a.i == 0):
    #       error0 = ..
    #       break branch0
    #     succeed
    #     break match0
    #   discard

    b.match:
    of "f":
      discard
    of "e":
      succeed
    else:
      discard

    # block match0:
    #   var error0: MatchingError
    #   block branch0:
    #     if not (a.kind == HString and a.s == "f"):
    #       error0 = ..
    #       break branch0
    #     break match0
    #   var error1: MatchingError
    #   block branch1:
    #     if not (a.kind == HString and a.s == "e"):
    #       error1 = ..
    #       break branch1
    #     succeed
    #     break match0
    #   discard

  matchtest "match variant args with auto":
    Container.matchArgs elements

    var a = Container(kind: CSeq, elements: @[A(kind: HInt, i: 2), A(kind: HInt, i: 4)])
    a.match:
    of CSeq(HInt(2), HInt(4)):
      succeed
    else:
      discard

  matchtest "match normal objects":
    type
      Obj = object
        a*: int
        b*: int

    var a = Obj(a: 0, b: 0)
    a.match:
    of Obj(a: `a`, b: 0):
      check a == 0
      succeed
    else:
      discard

  matchtest "match seq":
    var a = @[0, 1, 2]
    a.match:
    of @[0]:
      discard
    of @[0, 1, `last`]:
      check last == 2
      succeed
    else:
      discard

    # block match0:
    #   var error0: ..
    #   block branch0:
    #     if not (a.len == 1 and a[0] == 0):
    #       error0 = ..
    #       break branch0
    #   var error1: ..
    #   block branch1:
    #     if not (a.len == 3 and a[0] == 0 and a[1] == 1):
    #       error1 = ..
    #       break branch1
    #     let last = a[2]
    #     check last == 2
    #     succeed

    a.match:
    of @[`first`]:
      discard
    of @[0, *`last`]:
      check last == @[1, 2]
      succeed
    else:
      discard

  matchtest "match array":
    var a = [0, 1, 2]
    a.match:
    of [0, 1, `last`]:
      check last == 2
      succeed
    else:
      discard
