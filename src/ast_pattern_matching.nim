# ast_pattern_matching
# Copyright Arne Döring
# a general ast pattern matching library with a focus on correctness and good error messages
# TODO arbitrary matching conditions with if
# TODO matchRepr
# DONE make it a nimble package
# DONE document match constant as literal
# DONE all tests pass
# DONE strVal etc restriction
# DONE ident matching
# DONE join WrongLengthKind
# DONE resolue ambiguety

import macros, strutils, tables

export macros

when isMainModule:
  template debug(args: varargs[untyped]): untyped =
    echo args
else:
  template debug(args: varargs[untyped]): untyped =
    discard

const
  nnkIntLiterals*   = nnkCharLit..nnkUInt64Lit
  nnkStringLiterals* = nnkStrLit..nnkTripleStrLit
  nnkFloatLiterals* = nnkFloatLit..nnkFloat64Lit

proc newLit[T: enum](arg: T): NimNode =
  newIdentNode($arg)

proc newLit[T](arg: set[T]): NimNode =
  ## does not work for the empty sets
  result = nnkCurly.newTree
  for x in arg:
    result.add newLit(x)

type SomeFloat = float | float32 | float64

proc len[T](arg: set[T]): int = card(arg)

type
  MatchingErrorKind* = enum
    NoError
    WrongKindLength
    WrongKindValue
    WrongIdent

  MatchingError = object
    node*: NimNode
    expectedKind*: set[NimNodeKind]
    case kind*: MatchingErrorKind
    of NoError:
      discard
    of WrongKindLength:
      expectedLength*: int
    of WrongKindValue:
      expectedValue*: NimNode
    of WrongIdent:
      strVal*: string

proc `$`*(arg: MatchingError): string =
  let n = arg.node
  case arg.kind
  of NoError:
    "no error"
  of WrongKindLength:
    let k = arg.expectedKind
    let l = arg.expectedLength
    var msg = "expected "
    if k.len == 0:
      msg.add "any node"
    elif k.len == 1:
      for el in k:  # only one element but there is no index op for sets
        msg.add $el
    else:
      msg.add "a node in" & $k

    if l >= 0:
      msg.add " with " & $l & " child(ren)"
    msg.add ", but got " & $n.kind
    if l >= 0:
      msg.add " with " & $n.len & " child(ren)"
    msg
  of WrongKindValue:
    let k = $arg.expectedKind
    let v = arg.expectedValue.repr
    var msg = "expected " & k & " with value " & v & " but got " & n.lispRepr
    if n.kind in {nnkOpenSymChoice, nnkClosedSymChoice}:
      msg = msg & " (a sym-choice does not have a strVal member, maybe you should match with `ident`)"
    msg
  of WrongIdent:
    let prefix = "expected ident `" & arg.strVal & "` but got "
    if n.kind in {nnkIdent, nnkSym, nnkOpenSymChoice, nnkClosedSymChoice}:
      prefix & "`" & n.strVal & "`"
    else:
      prefix & $n.kind & " with " & $n.len & " child(ren)"

proc failWithMatchingError*(arg: MatchingError): void {.compileTime, noReturn.} =
  error($arg, arg.node)

proc expectValue(arg: NimNode; value: SomeInteger): void {.compileTime.} =
  arg.expectKind nnkLiterals
  if arg.intVal != int(value):
    error("expected value " & $value & " but got " & arg.repr, arg)

proc expectValue(arg: NimNode; value: SomeFloat): void {.compileTime.} =
  arg.expectKind nnkLiterals
  if arg.floatVal != float(value):
    error("expected value " & $value & " but got " & arg.repr, arg)

proc expectValue(arg: NimNode; value: string): void {.compileTime.} =
  arg.expectKind nnkLiterals
  if arg.strVal != value:
    error("expected value " & value & " but got " & arg.repr, arg)

proc expectValue[T](arg: NimNode; value: pointer): void {.compileTime.} =
  arg.expectKind nnkLiterals
  if value != nil:
    error("Expect Value for pointers works only on `nil` when the argument is a pointer.")
  arg.expectKind nnkNilLit

proc expectIdent(arg: NimNode; strVal: string): void {.compileTime.} =
  if not arg.eqIdent(strVal):
    error("Expect ident `" & strVal & "` but got " & arg.repr)

proc matchLengthKind*(arg: NimNode; kind: set[NimNodeKind]; length: int): MatchingError {.compileTime.} =
  let kindFail   = not(kind.card == 0 or arg.kind in kind)
  let lengthFail = not(length < 0 or length == arg.len)
  if kindFail or lengthFail:
    result.node = arg
    result.kind = WrongKindLength
    result.expectedLength = length
    result.expectedKind   = kind


proc matchLengthKind*(arg: NimNode; kind: NimNodeKind; length: int): MatchingError {.compileTime.} =
  matchLengthKind(arg, {kind}, length)

proc matchValue(arg: NimNode; kind: set[NimNodeKind]; value: SomeInteger): MatchingError {.compileTime.} =
  let kindFail   = not(kind.card == 0 or arg.kind in kind)
  let valueFail  = arg.intVal != int(value)
  if kindFail or valueFail:
    result.node = arg
    result.kind = WrongKindValue
    result.expectedKind  = kind
    result.expectedValue = newLit(value)

proc matchValue(arg: NimNode; kind: NimNodeKind; value: SomeInteger): MatchingError {.compileTime.} =
  matchValue(arg, {kind}, value)

proc matchValue(arg: NimNode; kind: set[NimNodeKind]; value: SomeFloat): MatchingError {.compileTime.} =
  let kindFail   = not(kind.card == 0 or arg.kind in kind)
  let valueFail  = arg.floatVal != float(value)
  if kindFail or valueFail:
    result.node = arg
    result.kind = WrongKindValue
    result.expectedKind  = kind
    result.expectedValue = newLit(value)

proc matchValue(arg: NimNode; kind: NimNodeKind; value: SomeFloat): MatchingError {.compileTime.} =
  matchValue(arg, {kind}, value)

const nnkStrValKinds = {nnkStrLit, nnkRStrLit, nnkTripleStrLit, nnkIdent, nnkSym}

proc matchValue(arg: NimNode; kind: set[NimNodeKind]; value: string): MatchingError {.compileTime.} =
  # if kind * nnkStringLiterals TODO do something that ensures that here is only checked for string literals
  let kindFail   = not(kind.card == 0 or arg.kind in kind)
  let valueFail  =
    if kind.card == 0:
      false
    else:
      arg.kind notin (kind * nnkStrValKinds) or arg.strVal != value
  if kindFail or valueFail:
    result.node = arg
    result.kind = WrongKindValue
    result.expectedKind  = kind
    result.expectedValue = newLit(value)

proc matchValue(arg: NimNode; kind: NimNodeKind; value: string): MatchingError {.compileTime.} =
  matchValue(arg, {kind}, value)

proc matchValue[T](arg: NimNode; value: pointer): MatchingError {.compileTime.} =
  if value != nil:
    error("Expect Value for pointers works only on `nil` when the argument is a pointer.")
  arg.matchLengthKind(nnkNilLit, -1)

proc matchIdent*(arg:NimNode; value: string): MatchingError =
  if not arg.eqIdent(value):
    result.node = arg
    result.kind = Wrongident
    result.strVal = value

static:
  var literals: array[19, NimNode]
  var i = 0
  for litKind in nnkLiterals:
    literals[i] = ident($litKind)
    i += 1

  var nameToKind = newTable[string, NimNodeKind]()
  for kind in NimNodeKind:
    nameToKind[ ($kind)[3..^1] ] = kind

  let identifierKinds = newLit({nnkSym, nnkIdent, nnkOpenSymChoice, nnkClosedSymChoice})

proc generateMatchingCode(astSym: NimNode, pattern: NimNode, depth: int, blockLabel, errorSym: NimNode; result: NimNode): void =

  proc nodeVisiting(astSym: NimNode, pattern: NimNode, depth: int): void =
    let ind = "  ".repeat(depth) # indentation

    proc genMatchLogic(matchProc, argSym1, argSym2: NimNode): void =
      result.add quote do:
        `errorSym` = `astSym`.`matchProc`(`argSym1`, `argSym2`)
        if `errorSym`.kind != NoError:
          break `blockLabel`

    proc genIdentMatchLogic(identValueLit: NimNode): void =
      result.add quote do:
        `errorSym` = `astSym`.matchIdent(`identValueLit`)
        if `errorSym`.kind != NoError:
          break `blockLabel`

    # proc handleKindMatching(kindExpr: NimNode): void =
    #   if kindExpr.eqIdent("_"):
    #     # this is the wildcand that matches any kind
    #     return
    #   else:
    #     genMatchLogic(bindSym"matchKind", kindExpr)

    # generate recursively a matching expression
    if pattern.kind == nnkCall:
      pattern.expectMinLen(1)

      debug ind, pattern[0].repr, "("

      let kindSet = if pattern[0].eqIdent("_"): nnkCurly.newTree else: pattern[0]
      # handleKindMatching(pattern[0])

      if pattern.len == 2 and pattern[1].kind == nnkExprEqExpr:
        if pattern[1][1].kind in nnkStringLiterals:
          pattern[1][0].expectIdent("strVal")
        elif pattern[1][1].kind in nnkIntLiterals:
          pattern[1][0].expectIdent("intVal")
        elif pattern[1][1].kind in nnkFloatLiterals:
          pattern[1][0].expectIdent("floatVal")

        genMatchLogic(bindSym"matchValue", kindSet, pattern[1][1])

      else:
        let lengthLit = newLit(pattern.len - 1)
        genMatchLogic(bindSym"matchLengthKind", kindSet, lengthLit)

        for i in 1 ..< pattern.len:
          let childSym = genSym(nskLet)
          let indexLit = newLit(i - 1)
          result.add quote do:
            let `childSym` = `astSym`[`indexLit`]
          nodeVisiting(childSym, pattern[i], depth + 1)
      debug ind, ")"
    elif pattern.kind == nnkCallStrLit and pattern[0].eqIdent("ident"):
      genIdentMatchLogic(pattern[1])

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
      genMatchLogic(bindSym"matchValue", nnkCurly.newTree, pattern)
    elif not pattern.eqIdent("_"):
      # When it is not one of the other branches, it is simply treated
      # as an expression for the node kind, without checking child
      # nodes.
      debug ind, pattern.repr
      genMatchLogic(bindSym"matchLengthKind", pattern, newLit(-1))

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


proc recursiveNodeVisiting(arg: NimNode, callback: proc(arg: NimNode): bool) =
  if callback(arg):
    for child in arg:
      recursiveNodeVisiting(child, callback)


macro matchAstRecursive*(ast: NimNode; args: varargs[untyped]): untyped =
  if args[^1].kind == nnkElse:
    error("Recursive matching with an else branch is pointless.", args[^1])

  let visitor = genSym(nskProc, "visitor")
  let visitorArg = genSym(nskParam, "arg")
  let visitorStmtList = newStmtList()

  let matchingSection = genSym(nskLabel, "matchingSection")

  for ofBranch in args:
    ofBranch.expectKind(nnkOfBranch)
    let pattern = ofBranch[0]
    let code = ofBranch[1]

    let stmtList = newStmtList()
    let matchingBranch = genSym(nskLabel, "matchingBranch")
    let brachError = genSym(nskVar, "branchError")
    generateMatchingCode(visitorArg, pattern, 0, matchingBranch, brachError, stmtList)
    stmtList.add code
    stmtList.add nnkBreakStmt.newTree(matchingSection)

    visitorStmtList.add quote do:
      var `brachError`: MatchingError
      block `matchingBranch`:
        `stmtList`


  let resultIdent = ident"result"

  let visitingProc = bindSym"recursiveNodeVisiting"

  result = quote do:
    proc `visitor`(`visitorArg`: NimNode): bool =
      block `matchingSection`:
        `visitorStmtList`
        `resultIdent` = true

    `visitingProc`(`ast`, `visitor`)

  debug result.repr


################################################################################
################################# Example Code #################################
################################################################################

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
          ident"a",
          nnkEmpty, nnkIntLit(intVal = 123)
        )
      ),
      _,
      nnkForStmt(
        nnkIdent(strVal = "i"),
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
    of nnkTypeDef(_, nnkGenericParams( nnkIdentDefs( nnkIdent(strVal = "T"), nnkStaticTy( _ ), nnkEmpty )), _):
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

    let ast2 = nnkStmtList.newTree( newLit(1) )

    ast2.matchAst:
    of nnkIntLit( 1 ):
      echo "fail"
    of nnkStmtList( 1 ):
      echo "ok"

    ast = bindSym"[]"
    ast.matchAst(errors):
    of nnkClosedSymChoice(strVal = "[]"):
      echo "fail, this is the wrong syntax, a sym choice does not have a `strVal` member."
    of ident"[]":
      echo "ok"

    const myConst = 123
    ast = newLit(123)

    ast.matchAst:
    of _(intVal = myConst):
      echo "ok"

    macro testRecCase(ast: untyped): untyped =
      ast.matchAstRecursive:
      of nnkIdentDefs(`a`,`b`,`c`):
        echo "got ident defs a: ", a.repr, " b: ", b.repr, " c: ", c.repr
      of ident"m":
        echo "got the ident m"

    testRecCase:
      type Obj[T] = object {.inheritable.}
        name: string
        case isFat: bool
        of true:
          m: array[100_000, T]
        of false:
          m: array[10, T]
