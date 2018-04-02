import macros, strutils, tables

type SomeFloat = float | float32 | float64

proc expectValue(arg: NimNode; value: SomeInteger): void {.compileTime.} =
  if arg.intVal != int(value):
    error("expected value " & $value & " but got " & arg.repr, arg)

proc expectValue(arg: NimNode; value: SomeFloat): void {.compileTime.} =
  if arg.floatVal != float(value):
    error("expected value " & $value & " but got " & arg.repr, arg)

proc expectValue(arg: NimNode; value: string): void {.compileTime.} =
  if arg.strVal != value:
    error("expected value " & value & " but got " & arg.repr, arg)

proc expectValue[T](arg: NimNode; value: pointer): void {.compileTime.} =
  if value != nil:
    error("Expect Value for pointers works only on `nil` when the argument is a pointer.")
  arg.expectKind nnkNilLit

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
  # generate recursively a matching expression
  pattern.expectKind(nnkCallKinds)
  let ind = "  ".repeat(depth) # indentation
  pattern[0].expectKind nnkIdent
  if pattern[0].eqIdent "Ident":
    echo ind, "Ident(", pattern[1].repr, ")"
    let identStr = $pattern[1][1]
    result.add quote do:
      `astSym`.expectKind nnkIdent
      assert eqIdent(`astSym`,`identStr`)
  elif pattern[0].eqIdent "Empty":
    echo ind, "Empty()"
    result.add quote do:
      expectKind(`astSym`, nnkEmpty)
  elif $pattern[0] in literals:
    echo ind, "newLit(", pattern[1].repr, ")"
    let literal = pattern[1]
    result.add quote do:
      `astSym`.expectKind nnkLiterals
      `astSym`.expectValue(`literal`)
  else:
    echo ind, pattern[0], "("
    let kindLit = ident("nnk" & $pattern[0])
    result.add quote do:
      `astSym`.expectKind `kindLit`
    for i in 1 ..< pattern.len:
      let childSym = genSym(nskLet)
      let indexLit = newLit(i - 1)
      result.add quote do:
        let `childSym` = `astSym`[`indexLit`]
      nodeVisiting(childSym, pattern[i], depth + 1, result)
    echo ind, ")"

macro matchAst(ast: NimNode, pattern, code: untyped): untyped =
  let pattern = if pattern.kind == nnkStmtList and pattern.len == 1: pattern[0] else: pattern
  result = newStmtList()
  nodevisiting(ast, pattern, 0, result)
  echo result.repr

macro foo(arg: untyped): untyped =
  matchAst(arg) do:
    StmtList(
      LetSection(
        IdentDefs(
          Ident(ident"a"),
          Empty(), IntLit(123)
        )
      ),
      LetSection(
        IdentDefs(
          Ident(ident"b"),
          Empty(),
          IntLit(342)
        )
      ),
      ForStmt(
        Ident(ident"i"),
        Infix(
          Ident(ident"..<"),
          Ident(ident"a"),
          Ident(ident"b")
        ),
        StmtList(
          Command(
            Ident(ident"echo"),
            StrLit("Hallo"),
            Ident(ident"i")
          )
        )
      )
    )
  do:
    discard

foo:
  let a = 123
  let b = 342
  for i in a ..< b:
    echo "Hallo", i
