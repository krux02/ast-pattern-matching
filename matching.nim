import macros, strutils, tables

type SomeFloat = float | float32 | float64

proc expectIdent(arg: NimNode; value: string): void {.compileTime.} =
  arg.expectKind nnkIdent
  if not arg.eqIdent value:
    error("expected ident " & value & " but got " & arg.repr, arg)

proc expectValue(arg: NimNode; value: SomeInteger): void {.compileTime.} =
  `arg`.expectKind nnkLiterals
  if arg.intVal != int(value):
    error("expected value " & $value & " but got " & arg.repr, arg)

proc expectValue(arg: NimNode; value: SomeFloat): void {.compileTime.} =
  `arg`.expectKind nnkLiterals
  if arg.floatVal != float(value):
    error("expected value " & $value & " but got " & arg.repr, arg)

proc expectValue(arg: NimNode; value: string): void {.compileTime.} =
  `arg`.expectKind nnkLiterals
  if arg.strVal != value:
    error("expected value " & value & " but got " & arg.repr, arg)

proc expectValue[T](arg: NimNode; value: pointer): void {.compileTime.} =
  `arg`.expectKind nnkLiterals
  if value != nil:
    error("Expect Value for pointers works only on `nil` when the argument is a pointer.")
  arg.expectKind nnkNilLit

# type
#   MatchingErrorKind = enum
#     WrongKind
#     WrongLength
#     WrongIdent
#     WrongLiteral

#   MatchingError = object
#     message: string
#     case kind: MatchingErrorKind
#     of WrongKind:
#       expectedKind: set[NimNodeKind]
#     of WrongLength:
#       expectedLength: int
#     of WrongIdent:
#       expectedIdent: NimNode
#     of WrongLiteral:
#       expectedLiteral: NimNode

static:
  var literals: array[19, string]
  var i = 0
  for litKind in nnkLiterals:
    literals[i] = ($litKind)[3..^1]
    i += 1

  var nameToKind = newTable[string, NimNodeKind]()
  for kind in NimNodeKind:
    nameToKind[ ($kind)[3..^1] ] = kind

proc nodevisiting(astSym: NimNode, pattern: NimNode, depth: int, result: NimNode): void =
  let ind = "  ".repeat(depth) # indentation

  # generate recursively a matching expression
  if pattern.kind in {nnkCall, nnkCommand}:
    pattern[0].expectKind nnkIdent
    if pattern[0].eqIdent "Ident":
      echo ind, "Ident(", pattern[1].repr, ")"
      let identStr =
        if pattern[1].kind == nnkStrLit:
          pattern[1].strVal
        else:
          pattern[1][0].expectIdent "ident"
          pattern[1][1].strVal
      result.add quote do:
        `astSym`.expectIdent `identStr`
    elif $pattern[0] in literals:
      echo ind, "newLit(", pattern[1].repr, ")"
      let literal = pattern[1]
      result.add quote do:
        `astSym`.expectValue(`literal`)
    else:
      echo ind, pattern[0], "("
      let kindLit = ident("nnk" & $pattern[0])
      result.add quote do:
        `astSym`.expectKind `kindLit`

      if pattern[0].len > 0:
        let lengthLit = newLit(pattern[0].len - 1)
        result.add quote do:
          `astSym`.expectLen `lengthLit`

      for i in 1 ..< pattern.len:
        let childSym = genSym(nskLet)
        let indexLit = newLit(i - 1)
        result.add quote do:
          let `childSym` = `astSym`[`indexLit`]
        nodeVisiting(childSym, pattern[i], depth + 1, result)
      echo ind, ")"

  elif pattern.kind == nnkAccQuoted:
    echo ind, pattern.repr
    let matchedExpr = pattern[0]
    matchedExpr.expectKind nnkIdent
    result.add quote do:
      let `matchedExpr` = `astSym`

  elif pattern.kind == nnkIdent:
    echo ind, pattern.repr
    let kindLit = ident("nnk" & $pattern)
    result.add quote do:
      `astSym`.expectKind `kindLit`


  elif pattern.kind == nnkInfix:
    pattern[0].expectIdent("@")
    pattern[1].expectKind nnkAccQuoted
    let matchedExpr = pattern[1][0]
    matchedExpr.expectKind nnkIdent
    result.add quote do:
      let `matchedExpr` = `astSym`

    echo ind, pattern[1].repr, " = "
    nodeVisiting(matchedExpr, pattern[2], depth + 1, result)


  else:
    echo ind, pattern.repr,  " WARNING: unhandled "

macro matchAst(ast: NimNode, pattern, code: untyped): untyped =
  let pattern = if pattern.kind == nnkStmtList and pattern.len == 1: pattern[0] else: pattern
  result = newStmtList()
  nodevisiting(ast, pattern, 0, result)
  result.add code
  echo result.repr

# TODO pattern expressions as prefixes (+ * ?)
# TODO pattern any of several subexpressions
# TODO matching on kind only without subtree
# TODO pattern for named subexpressions  `` `mysym` @ Ident`` `` `subtree` @ StmtList``
# TODO how are named subexpressions handled in optional pattern branches?


macro foo(arg: untyped): untyped =
  matchAst(arg) do:
    StmtList(
      LetSection(
        IdentDefs(
          Ident(ident"a"),
          Empty, IntLit(123)
        )
      ),
      LetSection(
        IdentDefs(
          Ident("b"),
          Empty,
          IntLit(342)
        )
      ),
      ForStmt(
        Ident(ident"i"),
        Infix,
        `mysym` @ StmtList
      )
    )
  do:
    echo "The AST did match!!!"
    echo "The matched sub tree is the following:"
    echo mysym.lispRepr

foo:
  let a = 123
  let b = 342
  for i in a ..< b:
    echo "Hallo", i
