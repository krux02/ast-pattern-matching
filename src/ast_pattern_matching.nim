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

import macros, strformat, strutils, tables, sets

export macros

when isMainModule:
  template debug(args: varargs[untyped]): untyped =
    echo args
else:
  template debug(args: varargs[untyped]): untyped =
    echo args

import typetraits

const
  nnkIntLiterals*   = nnkCharLit..nnkUInt64Lit
  nnkStringLiterals* = nnkStrLit..nnkTripleStrLit
  nnkFloatLiterals* = nnkFloatLit..nnkFloat64Lit



proc objectType(node: NimNode): NimNode {.compileTime.} =
  result = node.getType()
  while result.kind == nnkBracketExpr and result[0].kind == nnkSym:
    result = result[1].getType()

# returns the kind node: it can be e.g. mode, not kind
proc getKind(t: NimNode): NimNode {.compileTime.} =
  if t.kind != nnkObjectTy:
    return nil
  for child in t[2]:
    if child.kind == nnkRecCase:
      return child[0]


# proc fieldTypeInternal(nimType: NimNode, field: string): NimNode {.compileTime.} =
#   for child in nimType[2]:
#     if child.kind == nnkSym and child.repr == field:
#       return child.getType
#     elif child.kind == nnkRecCase:
#       for branch in child:
#         if branch.kind in {nnkOfBranch, nnkElse}:
#           for element in branch[1]:
#             if element.kind == nnkSym and element.repr == field:
#               return element.getType

# proc fieldType(nimType: NimNode, sym: NimNode, field: string): NimNode {.compileTime.} =
#   let t = nnkDotExpr.newTree(sym, ident(field))
#   let n = bindSym(field)
#   echo n.getType.repr
#   let realType = nimType.objectType
#   if realType.kind != nnkObjectTy:
#     error(&"no field of {nimType.repr}")
#   fieldTypeInternal(realType, field)

# proc indexType(nimType: NimNode): NimNode {.compileTime.} =
#   if nimType.kind == nnkBracketExpr:
#     if nimType[0].repr == "seq":
#       result = nimType[1]
#     elif nimType[0].repr == "array":
#       result = nimType[2]
#     else:
#       error(&"unknown collection {nimType[0].repr}")
#   else:
#     error(&"no index of {nimType.repr}")

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

    WrongKindValueGeneric
    WrongTypeGeneric
    WrongLengthGeneric



  # TODO: make generic
  MatchingError = object
    node*: string
    expectedKind*: string
    case kind*: MatchingErrorKind
    of NoError:
      discard
    of WrongKindLength:
      expectedLength*: int
      actualLength*: int
    of WrongKindValue:
      expectedValue*: string
    of WrongIdent:
      strVal*: string
  
    # TODO: generic
    of WrongKindValueGeneric:
      actual*: string
      expected*: string
      expectedVal*: string
    of WrongTypeGeneric:
      actualType*: string
      expectedType*: string
    of WrongLengthGeneric:
      actualLen*: int
      expectedLen*: int

proc `$`*(arg: MatchingError): string =
  let n = arg.node
  case arg.kind
  of NoError:
    "no error"
  of WrongKindLength:
    let k = arg.expectedKind
    let l = arg.expectedLength
    var msg = "expected "
    # if k.len == 0:
    #   msg.add "any node"
    # elif k.len == 1:
    #   for el in k:  # only one element but there is no index op for sets
    #     msg.add $el
    # else:
    #   msg.add "a node in" & $k
    msg.add k
    if l >= 0:
      msg.add " with " & $l & " child(ren)"
    msg.add ", but got " & $n
    if l >= 0:
      msg.add " with " & $arg.actualLength & " child(ren)"
    msg
  of WrongKindValue:
    let k = arg.expectedKind
    let v = arg.expectedValue
    var msg = "expected " & k & " with value " & v & " but got " & n
    # if n.kind in {nnkOpenSymChoice, nnkClosedSymChoice}:
    #   msg = msg & " (a sym-choice does not have a strVal member, maybe you should match with `ident`)"
    msg
  of WrongIdent:
    let prefix = "expected ident `" & arg.strVal & "` but got "
    prefix
    # if n.kind in {nnkIdent, nnkSym, nnkOpenSymChoice, nnkClosedSymChoice}:
    #   prefix & "`" & n.strVal & "`"
    # else:
    #   prefix & $n.kind & " with " & $n.len & " child(ren)"
  of WrongKindValueGeneric:
    "expected " & arg.expected & " with " & arg.expectedVal & " but got " & arg.actual
  of WrongTypeGeneric:
    "expected " & arg.expectedType & " but got " & arg.actualType
  of WrongLengthGeneric:
    "expected " & $arg.expectedLen & " but got " & $arg.actualLen

proc failWithMatchingError*(arg: MatchingError): void {.compileTime, noReturn.} =
  error($arg) #arg.node)

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


macro litMappingCheck*(node: untyped, pattern: untyped, literalField: untyped): untyped =
  let literalName = newLit(literalField.repr)
  result = quote:
    if `node`.`literalField` != `pattern`:
      MatchingError(
        kind: WrongKindValueGeneric,
        expected: `literalName`,
        expectedVal: $`node`.`literalField`,
        actual: $`pattern`)
    else:
      MatchingError(
        kind: NoError)
  # echo result.repr

macro matchLitProperty*(objType: typed, litType: typed, literalField: untyped): untyped =
  let literalFieldName = literalField.repr
  result = quote:
    macro litMapping*[T](objType: type `objType`, litType: type `litType`, enumType: typedesc[T], node: NimNode, pattern: `litType`): untyped =
      let res = nnkCall.newTree(
        ident"litMappingCheck",
        node,
        pattern,
        ident(`literalFieldName`))
      # echo res.repr
      res

macro matchLitInt*(objType: typed, litType: typed, enumValue: static[int]): untyped =
  let objInternalType = objType.objectType
  
  var literalField: NimNode
  
  for child in objInternalType[2][0]:
    if child.kind == nnkOfBranch and child[0].kind == nnkIntLit and child[0].intVal == enumValue:
      for field in child[1]:
        # TODO why
        # if sameType(litType, field.getType):
        if litType.repr == field.getType.repr:
          literalField = field
          break
      break

  # have to paste it here so I can *
  let literalFieldName = literalField.repr
  result = quote:
    macro litMapping*[T](objType: type `objType`, litType: type `litType`, enumType: typedesc[T], node: NimNode, pattern: `litType`): untyped =
      # hack: I pass the node as ident(node) sometimes, fix
      let obj = if node.kind == nnkCall and node[0].repr == "ident" and node.len == 2:
          ident(node[1].repr[1..^2])
        else:
          node
      let res = nnkCall.newTree(
        ident"litMappingCheck",
        obj,
        pattern,
        ident(`literalFieldName`))
      res

  # matchLitProperty(objType, litType, literalField)

macro matchLit*(objType: typed, litType: typed, enumValue: typed): untyped =
  result = quote:
    matchLitInt(`objType`, `litType`, `enumValue`.int)

const intTypenames = ["int", "int8", "int16", "int32", "int64", "uint", "uint8", "uint16", "uint32", "uint64"].toSet()

proc compatibleLiteralType(literalType: NimNode, kind: NimNodeKind): bool =
  if literalType.kind != nnkSym:
    return false

  let typename = literalType.repr
  
  if kind in nnkIntLiterals: typename in intTypenames
  elif kind in nnkFloatLiterals: typename == "float"
  elif kind in nnkStringLiterals: typename == "string"
  elif kind == nnkIdent: typename == "bool"
  else: false

macro litCheck*(astSym: typed, name: untyped, pattern: untyped): untyped =
  let astType = astSym.getType
  if compatibleLiteralType(astType, pattern.kind):
    result = quote do: matchLiteral(`astSym`, `pattern`)
  else:
    let objectType = if astType.kind == nnkBracketExpr: astType[1] else: astType
    var objectName = ident(objectType.repr.split(':', 1)[0])
    var kind = ident(astSym.objectType.getKind.getType[1].repr)
    var arg = astSym
    # TODO: somehow generalize
    if objectName.repr == "NimNodeObj":
      objectName = ident("NimNode")
      kind = ident("NimNodeKind")
    # let astName = newLit(name.repr[1..^1]) #newLit(astSym.repr)
    else:
      let astRepr = newLit(astSym.repr)
      arg = quote do: ident(`astRepr`)
    result = quote do: litMapping(`objectName`, `pattern`.type, `kind`.type, `arg`, `pattern`)

static:
  var typeArgs* = initTable[string, string]()

macro matchArgs*(typ: typed, f: untyped): untyped =
  # strip typedesc
  typeArgs[typ.getType[1].repr] = f.repr

macro matchArgs*(typ: typed): untyped =
  typeArgs[typ.getType[1].repr] = "_"

macro matchLength*(arg: typed, _: untyped, length: untyped): untyped =
  # TODO: better errors for collections?
  # let argType = arg.getType()
  # if argType.kind != nnkBracketExpr or argType[0].kind != nnkSym or argType[0].repr != "seq":
  #   error(&"expected seq but got {argType.repr}")
  result = quote:
    if `arg`.len != `length`:
      MatchingError(
        kind: WrongLengthGeneric,
        expectedLen: `arg`.len,
        actualLen: `length`)
    else:
      MatchingError(
        kind: NoError)

macro matchLengthKind*(arg: typed, kindArg: typed, length: untyped): untyped =
  let argType = arg.objectType()
  let kind = argType.getKind()
  let kindArgType = kindArg.getType()

  if kind.isNil:
    # deal with typedesc
    if not sameType(argType, kindArg.getType[1].getType):
      error(&"expected {kindArg.repr} but got {argType.repr}")
    else:
      result = quote:
        MatchingError(
          kind: NoError)
  else:
    let kindType = kind.getType
    let expected = kindArgType.repr.newLit
    let expectedVal = kindArg.repr.newLit
    let actual = kindType.repr.newLit
    let errorMsg = quote:
      MatchingError(
          kind: WrongKindValueGeneric,
          expected: `expected`,
          expectedVal: `expectedVal`,
          actual: `actual`)
    
    # TODO: actualValue
    if kindType != kindArgType:
      error(&"expected {kindType.repr} but got {kindArgType.repr}")
    else:
      result = quote:
        if `arg`.`kind` != `kindArg`:
          `errorMsg`
        else:
          MatchingError(
            kind: NoError)

proc matchLengthKind*(arg: NimNode; kind: set[NimNodeKind]; length: int): MatchingError {.compileTime.} =
  let kindFail   = not(kind.card == 0 or arg.kind in kind)
  let actualLength = if arg.kind != nnkNilLit and (arg.kind == nnkIdent or arg.kind in nnkLiterals): 1 else: arg.len
  let lengthFail = not(length < 0 or length == actualLength)
  if kindFail or lengthFail:
    result.node = arg.repr
    result.kind = WrongKindLength
    result.expectedLength = length
    result.expectedKind = $kind
    result.actualLength = actualLength

proc matchLengthKind*(arg: NimNode; kind: NimNodeKind; length: int): MatchingError {.compileTime.} =
  matchLengthKind(arg, {kind}, length)

proc matchField*(arg, kind, field, fieldPattern, blockLabel, errorSym: NimNode): NimNode {.compileTime.} =
  #(arg: typed, kind: typed, field: untyped, fieldPattern: untyped, blockLabel: untyped, errorSym: untyped): untyped =
  case fieldPattern.kind:
  of nnkIdent:
    result = quote:
      if `arg`.`field` != `fieldPattern`:
        result = quote:
          `errorSym` = MatchingError(
            kind: WrongFieldValueGeneric,
            expectedFieldValue: $`arg`.`field`,
            actualFieldValue: $`fieldPattern`)
          break `blockLabel`
      else:
        nil
  of nnkAccQuoted:
    expectKind fieldPattern[0], nnkIdent
    let name = fieldPattern[0]
    result = quote:
      let `name` = `arg`.`field`
  else:
    result = nil

macro matchLiteral*(arg: typed, value: typed): MatchingError =
  let valueType = ident(value.getType.repr)
  result = quote:
    if `arg` != `value`:
      MatchingError(
        kind: WrongKindValueGeneric,
        expected: $`valueType`,
        expectedVal: $`value`, 
        actual: $`arg`,)
    else:
      MatchingError(
        kind: NoError)

proc matchValue(arg: NimNode; kind: set[NimNodeKind]; value: SomeInteger): MatchingError {.compileTime.} =
  let kindFail   = not(kind.card == 0 or arg.kind in kind)
  let valueFail  = arg.intVal != int(value)
  if kindFail or valueFail:
    result.node = arg.repr
    result.kind = WrongKindValue
    result.expectedKind  = $kind
    result.expectedValue = $value

proc matchValue(arg: NimNode; kind: NimNodeKind; value: SomeInteger): MatchingError {.compileTime.} =
  matchValue(arg, {kind}, value)

proc matchValue(arg: NimNode; kind: set[NimNodeKind]; value: SomeFloat): MatchingError {.compileTime.} =
  let kindFail   = not(kind.card == 0 or arg.kind in kind)
  let valueFail  = arg.floatVal != float(value)
  if kindFail or valueFail:
    result.node = arg.repr
    result.kind = WrongKindValue
    result.expectedKind  = $kind
    result.expectedValue = $value

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
    result.node = arg.repr
    result.kind = WrongKindValue
    result.expectedKind  = $kind
    result.expectedValue = $value

proc matchValue(arg: NimNode; kind: NimNodeKind; value: string): MatchingError {.compileTime.} =
  matchValue(arg, {kind}, value)

proc matchValue[T](arg: NimNode; value: pointer): MatchingError {.compileTime.} =
  if value != nil:
    error("Expect Value for pointers works only on `nil` when the argument is a pointer.")
  arg.matchLengthKind(nnkNilLit, -1)

proc matchIdent*(arg: NimNode; value: string): MatchingError =
  if not arg.eqIdent(value):
    result.node = arg.repr
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

    # echo ind, "MATCHING CODE: ", astSym.repr, " ", pattern.repr

    proc genMatchLogic(matchProc, argSym1, argSym2: NimNode): void =
      result.add quote do:
        `errorSym` = `astSym`.`matchProc`(`argSym1`, `argSym2`)
        if `errorSym`.kind != NoError:
          break `blockLabel`

    # TODO: varargs
    proc genMatchLogic(matchProc, argSym1, argSym2, argSym3: NimNode): void =
      result.add quote do:
        `errorSym` = `astSym`.`matchProc`(`argSym1`, `argSym2`, `argSym3`)
        if `errorSym`.kind != NoError:
          break `blockLabel`

    proc genIdentMatchLogic(identValueLit: NimNode): void =
      result.add quote do:
        `errorSym` = `astSym`.matchIdent(`identValueLit`)
        if `errorSym`.kind != NoError:
          break `blockLabel`
    
    proc isVariadic(pattern: NimNode): bool =
      pattern.kind == nnkPrefix and pattern[0].repr == "*" or
        pattern.kind == nnkInfix and pattern[0].repr == "@" and
        pattern[2].isVariadic

    proc collectionMatch(pattern: NimNode): void =
      let lengthLit = newLit(pattern.len)
      var checkLength = true
      for i in 0 ..< pattern.len:
        if pattern[i].isVariadic:
          checkLength = false
          break
      if checkLength:
        genMatchLogic(bindSym"matchLength", nil, lengthLit)

      for i in 0 ..< pattern.len:
        let childSym = genSym(nskLet)
        let indexLit = newLit(i)
        if pattern[i].isVariadic:
          # *pattern
          # create new seq with the next elements
          result.add quote do:
            let `childSym` = `astSym`[`indexLit` .. ^1]
        else:
          result.add quote do:
            let `childSym` = `astSym`[`indexLit`]
        
        nodeVisiting(childSym, pattern[i], depth + 1)


    # proc handleKindMatching(kindExpr: NimNode): void =
    #   if kindExpr.eqIdent("_"):
    #     # this is the wildcand that matches any kind
    #     return
    #   else:
    #     genMatchLogic(bindSym"matchKind", kindExpr)

    # generate recursively a matching expression
    if pattern.kind in {nnkCall, nnkObjConstr}:
      pattern.expectMinLen(1)

      debug ind, pattern[0].repr, "("

      let kindSet = if pattern[0].eqIdent("_"): nnkCurly.newTree else: pattern[0]
      # handleKindMatching(pattern[0])

      # if pattern.len == 2 and pattern[1].kind in {nnkExprEqExpr, nnkExprColonExpr}:
      #   # TODO: matchLit logic for those

      let lengthLit = newLit(pattern.len - 1)
      genMatchLogic(bindSym"matchLengthKind", kindSet, lengthLit)
      if true:
        for i in 1 ..< pattern.len:
          let childSym = genSym(nskLet)
          if pattern[i].kind in {nnkExprEqExpr, nnkExprColonExpr}:
            let (field, fieldPattern) = (pattern[i][0], pattern[i][1])
            result.add quote do:
              let `childSym` = `astSym`.`field`
            debug ind, "visit child"
            nodeVisiting(childSym, pattern[i][1], depth + 1)
          else:
            echo astSym.repr
            # let astType = astSym.getType
            # let args = typeArgs.getOrDefault(astType.repr)
            let args = "_"
            if args.len == 0:
              error(&"{astSym.repr} does not support f(a, b..) match")
            let argsIdent = ident(args)
            let indexLit = newLit(i - 1)
            if args != "_":
              result.add quote do:
                let `childSym` = `astSym`.`argsIdent`[`indexLit`]
            else:
              result.add quote do:
                let `childSym` = `astSym`[`indexLit`]
            debug ind, "visit child"
            nodeVisiting(childSym, pattern[i], depth + 1)
      debug ind, ")"
    elif pattern.kind == nnkCallStrLit and pattern[0].eqIdent("ident"):
      genIdentMatchLogic(pattern[1])
    elif pattern.kind == nnkPar and pattern.len == 1:
      nodeVisiting(astSym, pattern[0], depth)
    elif pattern.kind == nnkPrefix:
      if pattern[0].kind == nnkIdent:
        if pattern[0].repr == "@" and pattern[1].kind == nnkBracket:
          collectionMatch(pattern[1])
        elif pattern[0].repr == "*":
          let index = ident("index0")
          let childSym = quote do:
            `astSym`[`index`]
          let oldLength = result.len
          nodeVisiting(childSym, pattern[1], depth + 1)
          if result.len > oldLength:
            var childLogic = nnkStmtList.newTree()
            for z in oldLength ..< result.len:
              childLogic.add(result[z])
            result.del(oldLength, childLogic.len)
            result.add quote do:
              for `index` in `astSym`:
                `childLogic`
        else:
          error("only seq or * prefix patterns supported", pattern)
      else:
        error("only seq or * prefix patterns supported", pattern)
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
      let name = ident(astSym.repr)
      result.add quote do:
        `errorSym` = `astSym`.litCheck(`name`, `pattern`)
        if `errorSym`.kind != NoError:
          break `blockLabel`
    elif pattern.kind == nnkBracket:
      # TODO: compile time length, enums would complicate a bit
      collectionMatch(pattern)
    elif not pattern.eqIdent("_"):
      # When it is not one of the other branches, it is simply treated
      # as an expression for the node kind, without checking child
      # nodes.
      debug ind, pattern.repr
      genMatchLogic(bindSym"matchLengthKind", pattern, newLit(-1))

  nodeVisiting(astSym, pattern, depth)

macro match*(node: typed; args: varargs[untyped]): untyped =
  let t = node.getType()
  let ast = node
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
    # maybe there is a better mechanism disable errors for statement after return
    if code[^1].kind != nnkReturnStmt:
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

      var patterns: string = ""
      for i in beginBranches ..< endBranches:
        let ofBranch = args[i]
        let pattern = ofBranch[0]
        patterns.add pattern.repr
        patterns.add "\n"

      let patternsLit = newLit(patterns)
      outerStmtList.add quote do:
        error("Ast pattern mismatch: got " & `ast`.lispRepr & "\nbut expected one of:\n" & `patternsLit`, `ast`)

  result = quote do:
    block `outerBlockLabel`:
      `outerStmtList`

  # echo result.repr


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
  # static:
  #   let mykinds = {nnkIdent, nnkCall}

  # macro foo(arg: untyped): untyped =
  #   matchAst(arg, matchError):
  #   of nnkStmtList(nnkIdent, nnkIdent, nnkIdent):
  #     echo(88*88+33*33)
  #   of nnkStmtList(
  #     _(
  #       nnkIdentDefs(
  #         ident"a",
  #         nnkEmpty, nnkIntLit(intVal = 123)
  #       )
  #     ),
  #     _,
  #     nnkForStmt(
  #       nnkIdent(strVal = "i"),
  #       nnkInfix,
  #       `mysym` @ nnkStmtList
  #     )
  #   ):
  #     echo "The AST did match!!!"
  #     echo "The matched sub tree is the following:"
  #     echo mysym.lispRepr
  #   else:
  #     echo "sadly the AST did not match :("
  #     echo arg.treeRepr
  #     failWithMatchingError(matchError[1])

  # foo:
  #   let a = 123
  #   let b = 342
  #   for i in a ..< b:
  #     echo "Hallo", i

  static:

    var ast = quote do:
      type
        A[T: static[int]] = object

    ast = ast[0]
    ast.matchAst(err):  # this is a sub ast for this a findAst or something like that is useful
    of nnkTypeDef(_, nnkGenericParams( nnkIdentDefs( nnkIdent(strVal = "T"), nnkStaticTy( _ ), nnkEmpty )), _):
      echo "ok"


    # ast = quote do:
    #   if cond1: expr1 elif cond2: expr2 else: expr3

    # ast.matchAst:
    # of {nnkIfExpr, nnkIfStmt}(
    #   {nnkElifExpr, nnkElifBranch}(`cond1`, `expr1`),
    #   {nnkElifExpr, nnkElifBranch}(`cond2`, `expr2`),
    #   {nnkElseExpr, nnkElse}(`expr3`)
    # ):
    #   echo "ok"

    # let ast2 = nnkStmtList.newTree( newLit(1) )

    # ast2.matchAst:
    # of nnkIntLit( 1 ):
    #   echo "fail"
    # of nnkStmtList( 1 ):
    #   echo "ok"

    # ast = bindSym"[]"
    # ast.matchAst(errors):
    # of nnkClosedSymChoice(strVal = "[]"):
    #   echo "fail, this is the wrong syntax, a sym choice does not have a `strVal` member."
    # of ident"[]":
    #   echo "ok"

    # const myConst = 123
    # ast = newLit(123)

    # ast.matchAst:
    # of _(intVal = myConst):
    #   echo "ok"

    # macro testRecCase(ast: untyped): untyped =
    #   ast.matchAstRecursive:
    #   of nnkIdentDefs(`a`,`b`,`c`):
    #     echo "got ident defs a: ", a.repr, " b: ", b.repr, " c: ", c.repr
    #   of ident"m":
    #     echo "got the ident m"

    # testRecCase:
    #   type Obj[T] = object {.inheritable.}
    #     name: string
    #     case isFat: bool
    #     of true:
    #       m: array[100_000, T]
    #     of false:
    #       m: array[10, T]
