# TODO pattern any of several subexpressions (|)
# TODO how are named subexpressions handled in optional pattern branches?
# TODO arbitrary matching conditions with if
# TODO pattern matching as expression
# TODO make it a nimble package
# TODO join WrongLengthKind

import macros, strutils, tables

export macros

when isMainModule:
  template debug(args: varargs[untyped]): untyped =
    echo args
else:
  template debug(args: varargs[untyped]): untyped =
    discard

type SomeFloat = float | float32 | float64

type
  MatchingErrorKind = enum
    NoError
    WrongKind
    WrongLength
    WrongValue

  MatchingError = object
    node: NimNode
    case kind: MatchingErrorKind
    of NoError:
      discard
    of WrongKind:
      expectedKind: set[NimNodeKind]
    of WrongLength:
      expectedLength: int
    of WrongValue:
      expectedValue: NimNode


proc `$`*(arg: MatchingError): string =
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
    result.add $arg.expectedLength
    result.add "\n"
  of WrongValue:
    result.add "  expectedLiteral: "
    result.add arg.expectedValue.repr
    result.add "\n"

proc matchLen*(arg: NimNode; len: int): MatchingError {.compileTime.} =
  if arg.len != len:
    result.node = arg
    result.kind = WrongLength
    result.expectedLength = len

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

proc failWithMatchingError*(arg: MatchingError): void {.compileTime, noReturn.} =
  let n = arg.node
  case arg.kind
  of NoError:
    error("argument isn't even an error")
  of WrongKind:
    let k = arg.expectedKind
    error("Expected one of " & $k & ", got " & $n.kind, n)
  of WrongLength:
    let len = arg.expectedLength
    error("macro expects a node with " & $len & " child(ren), got " & $n.kind & " with " & $n.len & " child(ren)", n)
  of WrongValue:
    let value = arg.expectedValue.repr
    error("expected value " & value & " but got " & n.repr, n)

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


proc matchValue(arg: NimNode; value: SomeInteger): MatchingError {.compileTime.} =
  if arg.intVal != int(value):
    result.node = arg
    result.kind = WrongValue
    result.expectedValue = newLit(value)

proc matchValue(arg: NimNode; value: SomeFloat): MatchingError {.compileTime.} =
  if arg.floatVal != float(value):
    result.node = arg
    result.kind = WrongValue
    result.expectedValue = newLit(value)

proc matchValue(arg: NimNode; value: string): MatchingError {.compileTime.} =
  if arg.strVal != value:
    result.node = arg
    result.kind = WrongValue
    result.expectedValue = newLit(value)

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

proc generateMatchingCode(astSym: NimNode, pattern: NimNode, depth: int, blockLabel, errorSym: NimNode; result: NimNode): void =

  proc nodeVisiting(astSym: NimNode, pattern: NimNode, depth: int): void =
    let ind = "  ".repeat(depth) # indentation

    proc genMatchLogic(matchProc, argumentSym: NimNode): void =
      result.add quote do:
        `errorSym` = `astSym`.`matchProc`(`argumentSym`)
        if `errorSym`.kind != NoError:
          break `blockLabel`

    proc handleKindMatching(kindExpr: NimNode): void =
      if kindExpr.eqIdent("_"):
        # this is the wildcand that matches any kind
        return
      else:
        genMatchLogic(bindSym"matchKind", kindExpr)

    # generate recursively a matching expression
    if pattern.kind == nnkCall:
      debug ind, pattern[0].repr, "("

      handleKindMatching(pattern[0])

      if pattern.len == 2 and pattern[1].kind in nnkLiterals:
        genMatchLogic(bindSym"matchValue", pattern[1])

      else:
        if pattern.len > 0:
          let lengthLit = newLit(pattern.len - 1)
          genMatchLogic(bindSym"matchLen", lengthLit)

        for i in 1 ..< pattern.len:
          let childSym = genSym(nskLet)
          let indexLit = newLit(i - 1)
          result.add quote do:
            let `childSym` = `astSym`[`indexLit`]
          nodeVisiting(childSym, pattern[i], depth + 1)
      debug ind, ")"

    elif pattern.kind == nnkPar and pattern.len == 1:
      nodeVisiting(astSym, pattern[0], depth)
    elif pattern.kind == nnkPrefix:
      error("prefix patterns not implemented", pattern)
    elif pattern.kind == nnkAccQuoted:
      debug ind, pattern.repr
      let matchedExpr = pattern[0]
      matchedExpr.expectKind nnkIdent
      result.add quote do:
        let `matchedExpr` = `astSym`

    elif pattern.kind == nnkInfix and pattern[0].eqIdent("@"):
      pattern[1].expectKind nnkAccQuoted

      let matchedExpr = pattern[1][0]
      matchedExpr.expectKind nnkIdent
      result.add quote do:
        let `matchedExpr` = `astSym`

      debug ind, pattern[1].repr, " = "
      nodeVisiting(matchedExpr, pattern[2], depth + 1)

    elif pattern.kind in nnkCallKinds:
      error("only boring call syntax allowed, this is " & $pattern.kind & ".", pattern)
    elif pattern.kind in nnkLiterals:
      genMatchLogic(bindSym"matchValue", pattern)
    else:
      # When it is not one of the other branches, it is simply treated
      # as an expression for the node kind, without checking child
      # nodes.
      debug ind, pattern.repr
      handleKindMatching(pattern)

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
      nil

  let elseBranch: NimNode =
    if endBranches == args.len - 1:
      args[^1].expectKind(nnkElse)
      args[^1][0]
    else:
      nil

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

  if elseBranch != nil:
    if outerErrorSym != nil:
      outerStmtList.add quote do:
        let `outerErrorSym` = @`errorSymbols`
        `elseBranch`
    else:
      outerStmtList.add elseBranch

  else:
    if errorSymbols.len == 1:
      # there is only one of branch and no else branch
      # the error message can be very precise here.
      let errorSym = errorSymbols[0]
      outerStmtList.add quote do:
        failWithMatchingError(`errorSym`)
    else:
      outerStmtList.add quote do:
        error("Ast does not match.", `ast`)

  result = quote do:
    block `outerBlockLabel`:
      `outerStmtList`

  debug result.repr

################################################################################
################################# Example Code #################################
################################################################################

dumpTree:
  *foobar(a,b,c)
  {nnkA, nnkB}



when isMainModule:


  static:
    let mykinds = {nnkIdent, nnkCall}

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
      echo "sadly the AST did not match :("
      echo arg.treeRepr
      failWithMatchingError(matchError[1])

  foo:
    let a = 123
    let b = 342
    for i in a ..< b:
      echo "Hallo", i

  static:

    var ast = quote do:
      type
        A[T: static[int]] = object

    ast = ast[0]
    ast.matchAst(err):  # this is a sub ast for this a findAst or something like that is useful
    of nnkTypeDef(_, nnkGenericParams( nnkIdentDefs( nnkIdent("T"), nnkStaticTy( _ ), nnkEmpty )), _):
      echo "ok"

    ast = quote do:
      if cond1: expr1 elif cond2: expr2 else: expr3

    ast.matchAst:
    of {nnkIfExpr, nnkIfStmt}(
      {nnkElifExpr, nnkElifBranch}(`cond1`, `expr1`),
      {nnkElifExpr, nnkElifBranch}(`cond2`, `expr2`),
      {nnkElseExpr, nnkElse}(`expr3`)
    ):
      echo "ok"
