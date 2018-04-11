# TODO pattern expressions as prefixes (+ * ?)
# TODO pattern any of several subexpressions (|)
# TODO how are named subexpressions handled in optional pattern branches?
# TODO arbitrary matching conditions with if
# TODO use pattern matching block as expression
# TODO documentation
# TODO no else branch assignments

import macros, strutils, tables

export macros


when isMainModule:
  template debug(args: varargs[untyped]): untyped =
    echo args
else:
  template debug(args: varargs[untyped]): untyped =
    discard


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

type
  MatchingErrorKind = enum
    NoError
    WrongKind
    WrongLength
    WrongIdent
    WrongLiteral

  MatchingError = object
    node: NimNode
    case kind: MatchingErrorKind
    of NoError:
      discard
    of WrongKind:
      expectedKind: set[NimNodeKind]
    of WrongLength:
      expectedLength: int
    of WrongIdent:
      expectedIdent: NimNode
    of WrongLiteral:
      expectedLiteral: NimNode


proc `$`(arg: MatchingError): string =
  result = ""
  result.add "MatchError:\n"
  result.add "  node:\n"
  for line in arg.node.treeRepr.splitLines:
    result.add "    "
    result.add line
    result.add "\n"
  result.add "  lininfo:\n    "
  result.add arg.node.lineinfo
  result.add "\n  kind: "
  result.add $arg.kind
  result.add "\n"
  case arg.kind
  of NoError:
    discard
  of WrongKind:
    result.add "  expectedKind: "
    result.add $arg.expectedKind
    result.add "\n"
  of WrongLength:
    result.add "  expectedLength: "
    result.add arg.expectedLength
    result.add "\n"
  of WrongIdent:
    result.add "  expectedIdent: "
    result.add arg.expectedIdent.repr
    result.add "\n"
  of WrongLiteral:
    result.add "  expectedLiteral: "
    result.add arg.expectedLiteral.repr
    result.add "\n"


proc matchLen(arg: NimNode; len: int): MatchingError {.compileTime.} =
  if arg.len != len:
    result.node = arg
    result.kind = WrongLength

proc matchKind(arg: NimNode; kind: NimNodeKind): MatchingError {.compileTime.} =
  if arg.kind != kind:
    result.node = arg
    result.kind = WrongKind
    result.expectedKind = {kind}

proc matchKind(arg: NimNode; kind: set[NimNodeKind]): MatchingError {.compileTime.} =
  if arg.kind notin kind:
    result.node = arg
    result.kind = WrongKind
    result.expectedKind = kind

# wtf why this star?

proc matchIdent*(arg: NimNode; value: string): MatchingError {.compileTime.} =
  if not arg.eqIdent value:
    result.node = arg
    result.kind = WrongIdent
    result.expectedIdent = ident(value)

proc matchValue(arg: NimNode; value: SomeInteger): MatchingError {.compileTime.} =
  if arg.intVal != int(value):
    result.node = arg
    result.kind = WrongLiteral
    result.expectedLiteral = newLit(value)

proc matchValue(arg: NimNode; value: SomeFloat): MatchingError {.compileTime.} =
  if arg.floatVal != float(value):
    result.node = arg
    result.kind = WrongLiteral
    result.expectedLiteral = newLit(value)

proc matchValue(arg: NimNode; value: string): MatchingError {.compileTime.} =
  if arg.strVal != value:
    result.node = arg
    result.kind = WrongLiteral
    result.expectedLiteral = newLit(value)

proc matchValue[T](arg: NimNode; value: pointer): MatchingError =
  `arg`.expectKind nnkLiterals
  if value != nil:
    error("Expect Value for pointers works only on `nil` when the argument is a pointer.")
  arg.expectKind nnkNilLit

static:
  var literals: array[19, NimNode]
  var i = 0
  for litKind in nnkLiterals:
    literals[i] = ident($litKind)
    i += 1

  var nameToKind = newTable[string, NimNodeKind]()
  for kind in NimNodeKind:
    nameToKind[ ($kind)[3..^1] ] = kind

proc generateMatchingCode(astSym: NimNode, pattern: NimNode, depth: int, blockLabel, errorSym, result: NimNode): void =

  proc nodeVisiting(astSym: NimNode, pattern: NimNode, depth: int): void =
    let ind = "  ".repeat(depth) # indentation

    proc genMatchLogic(matchProc, argumentSym: NimNode): void =
      result.add quote do:
        `errorSym` = `astSym`.`matchProc`(`argumentSym`)
        if `errorSym`.kind != NoError:
          break `blockLabel`

    proc handleIdent(ident: NimNode): void =
      if not ident.eqIdent("_"):
        genMatchLogic(bindSym"matchKind", ident)

    # generate recursively a matching expression
    if pattern.kind in {nnkCall, nnkCommand}:
      # TODO: pattern[0] could be nnkPar with branching!
      if pattern[0].kind == nnkIdent:
        if pattern[0].eqIdent "nnkIdent":
          debug ind, "Ident(", pattern[1].repr, ")"
          let identStr =
            if pattern[1].kind == nnkStrLit:
              pattern[1].strVal
            else:
              pattern[1][0].expectIdent "ident"
              pattern[1][1].strVal
          genMatchLogic(bindSym"matchIdent", newLit(identStr))

        elif pattern[0] in literals:

          if pattern.len == 1:
            pattern[0].expectIdent "nnkNilLit"
            debug ind, "newNilLit()"
            debug pattern.lispRepr

          else:
            debug ind, "newLit(", pattern[1].repr, ")"
            genMatchLogic(bindSym"matchValue", pattern[1])

        else:
          debug ind, pattern[0], "("
          handleIdent(pattern[0])

          if pattern[0].len > 0:
            let lengthLit = newLit(pattern[0].len - 1)
            genMatchLogic(bindSym"matchLen", lengthLit)

          for i in 1 ..< pattern.len:
            let childSym = genSym(nskLet)
            let indexLit = newLit(i - 1)
            result.add quote do:
              let `childSym` = `astSym`[`indexLit`]
            nodeVisiting(childSym, pattern[i], depth + 1)
          debug ind, ")"

      elif pattern[0].kind == nnkPar and pattern[0].len == 0 and pattern[0].kind == nnkIdent:
        handleIdent(pattern[0][0])

      else:
        debug ">>>> ", ind, pattern.lispRepr, " <<<< WARNING: unhandled!!! "

    elif pattern.kind == nnkAccQuoted:
      debug ind, pattern.repr
      let matchedExpr = pattern[0]
      matchedExpr.expectKind nnkIdent
      result.add quote do:
        let `matchedExpr` = `astSym`

    elif pattern.kind == nnkIdent:
      debug ind, pattern.repr
      handleIdent(pattern)

    elif pattern.kind == nnkInfix:
      pattern[0].expectIdent("@")
      pattern[1].expectKind nnkAccQuoted

      let matchedExpr = pattern[1][0]
      matchedExpr.expectKind nnkIdent
      result.add quote do:
        let `matchedExpr` = `astSym`

      debug ind, pattern[1].repr, " = "
      nodeVisiting(matchedExpr, pattern[2], depth + 1)

    elif pattern.kind == nnkPar:
      # parens are just for ast generation. No special semantic meaning
      # after the tree has been constructed.
      debug pattern.lispRepr
      pattern.expectLen 1
      nodeVisiting(astSym, pattern[0], depth)

    else:
      debug ">>>> ", ind, pattern.repr, " <<<< WARNING: unhandled!!! "

  nodeVisiting(astSym, pattern, depth)

macro matchAst*(ast: NimNode; args: varargs[untyped]): untyped =
  let beginBranches = if args[0].kind == nnkIdent: 1 else: 0
  let endBranches   = if args[^1].kind == nnkElse: args.len - 1 else: args.len
  for i in beginBranches ..< endBranches:
    args[i].expectKind nnkOfBranch

  let outerErrorSym: NimNode =
    if beginBranches == 1:
      args[0].expectKind nnkIdent
      args[0]
    else:
      genSym(nskLet, "errosSeq")

  let elseBranch =
    if endBranches == args.len - 1:
      args[^1].expectKind(nnkElse)
      args[^1][0]
    else:
      newEmptyNode()

  let outerBlockLabel = genSym(nskLabel, "matchingSection")
  let outerStmtList = newStmtList()
  let errorSymbols = nnkBracket.newTree

  for i in beginBranches ..< endBranches:
    let ofBranch = args[i]

    ofBranch.expectKind(nnkOfBranch)
    let pattern = ofBranch[0]
    let code = ofBranch[1]
    let stmtList = newStmtList()
    let blockLabel = genSym(nskLabel, "matchingBranch")
    let errorSym = genSym(nskVar, "branchError")
    errorSymbols.add errorSym
    generateMatchingCode(ast, pattern, 0, blockLabel, errorSym, stmtList)
    stmtList.add code
    stmtList.add nnkBreakStmt.newTree(outerBlockLabel)

    outerStmtList.add quote do:
      var `errorSym`: MatchingError
      block `blockLabel`:
        `stmtList`

  outerStmtList.add quote do:
    let `outerErrorSym` = @`errorSymbols`
    `elseBranch`

  result = quote do:
    block `outerBlockLabel`:
      `outerStmtList`

  debug result.repr

################################################################################
################################# Example Code #################################
################################################################################


when isMainModule:


  static:
    let mykinds = {nnkIdent, nnkCall}

    echo myKinds

  dumpTree:
    (var x: int; x += 1; x)
    (StmtList | StmtListExpr | StmtExpr)(
      foo @ Ident,
      Empty
    )

  macro foo(arg: untyped): untyped =
    matchAst(arg, matchError):
    of nnkStmtList(nnkIdent, nnkIdent, nnkIdent):
      echo(88*88+33*33)
    of nnkStmtList(
      _(
        nnkIdentDefs(
          nnkIdent("a"),
          nnkEmpty, nnkIntLit(123)
        )
      ),
      _,
      nnkForStmt(
        nnkIdent("i"),
        nnkInfix,
        `mysym` @ nnkStmtList
      )
    ):
      echo "The AST did match!!!"
      echo "The matched sub tree is the following:"
      echo mysym.lispRepr
    else:
      echo matchError
      echo "sadly the AST did not match :("

  foo:
    let a = 123
    let b = 342
    for i in a ..< b:
      echo "Hallo", i