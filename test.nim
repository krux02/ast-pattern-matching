import astmatching

# TODO test on matching failures

static:
  template testPattern(pattern, astArg: untyped): untyped =
    let ast = quote do: `astArg`

    ast.matchAst:
    of `pattern`:
      echo "ok"

  testPattern nnkIntLit(42)            , 42
  testPattern nnkInt8Lit(42)           , 42'i8
  testPattern nnkInt16Lit(42)          , 42'i16
  testPattern nnkInt32Lit(42)          , 42'i32
  testPattern nnkInt64Lit(42)          , 42'i64
  testPattern nnkUInt8Lit(42)          , 42'u8
  testPattern nnkUInt16Lit(42)         , 42'u16
  testPattern nnkUInt32Lit(42)         , 42'u32
  testPattern nnkUInt64Lit(42)         , 42'u64
  #testPattern nnkFloat64Lit(42.0)      , 42.0
  testPattern nnkFloat32Lit(42.0)      , 42.0'f32
  #testPattern nnkFloat64Lit(42.0)      , 42.0'f64
  testPattern nnkStrLit("abc")         , "abc"
  testPattern nnkRStrLit("abc")        , r"abc"
  testPattern nnkTripleStrLit("abc")   , """abc"""
  testPattern nnkCharLit(32)           , ' '
  testPattern nnkNilLit()              , nil
  testPattern nnkIdent("myIdentifier") , myIdentifier

static:
  ## Command call
  block:

    let ast = quote do:
      echo "abc", "xyz"

    ast.matchAst:
    of nnkCommand(
      nnkSym("echo"),
      nnkStrLit("abc"),
      nnkStrLit("xyz")
    ):
      echo "ok"
    else:
      error "fail", ast

  ## Call with ``()``

  block call:
    let ast = quote do:
      echo("abc", "xyz")

    ast.matchAst:
    of nnkCall(
      {nnkIdent, nnkSym}("echo"),
      nnkStrLit("abc"),
      nnkStrLit("xyz")
    ):
      echo "ok"
    else:
      error "fail", ast


  ## Infix operator call

  macro testInfixOperatorCall(ast: untyped): untyped =


    ast.matchAst:
    of nnkInfix(
      nnkIdent("&"),
      nnkStrLit("abc"),
      nnkStrLit("xyz")
    ):
      echo "ok1"
    of nnkInfix(
      nnkIdent("+"),
      nnkIntLit(5),
      nnkInfix(
        nnkIdent("*"),
        nnkIntLit(3),
        nnkIntLit(4)
      )
    ):
      echo "ok2"
    of nnkCall(
      nnkAccQuoted(
        {nnkIdent, nnkSym}("+")
      ),
      nnkIntLit(3),
      nnkIntLit(4)
    ):
      echo "ok3"

  testInfixOperatorCall("abc" & "xyz")
  testInfixOperatorCall(5 + 3 * 4)
  testInfixOperatorCall(`+`(3, 4))


  ## Prefix operator call

  block:

    let ast = quote do:
      ? "xyz"

    ast.matchAst(err):
    of nnkPrefix(
      {nnkIdent, nnkSym}("?"),
      nnkStrLit("xyz")
    ):
      echo "ok"
    else:
      echo ast.lispRepr
      echo err
      error "fail", ast


  ## Postfix operator call

  block:

    let ast = quote do:
      proc identifier*

    ast[0].matchAst(err):
    of nnkPostfix(
      nnkIdent("*"),
      nnkIdent("identifier")
    ):
      echo "ok"
    else:

      echo ast.lispRepr
      echo err[0]
      error "fail", ast


  ## Call with named arguments

  macro testCallWithNamedArguments(ast: untyped): untyped =
    ast.peelOff(nnkStmtList).matchAst:
    of nnkCall(
      nnkIdent("writeLine"),
      nnkExprEqExpr(
        nnkIdent("file"),
        nnkIdent("stdout")
      ),
      nnkStrLit("hallo")
    ):
      echo "ok"
    else:
      error "fail", ast

  testCallWithNamedArguments:
    writeLine(file=stdout, "hallo")



  ## Call with raw string literal

  block:

    let ast = quote do:
      echo"abc"

    ast.matchAst(err):
    of nnkCallStrLit(
      nnkIdent("echo"),
      nnkRStrLit("abc")
    ):
      echo "ok"
    else:
      echo ast.lispRepr
      echo err
      error "fail", ast

  ## Dereference operator ``[]``

  block:
    # The dereferece operator exists only on a typed ast.
    macro testDereferenceOperator(ast: typed): untyped =
      ast.matchAst(err):
      of nnkDerefExpr(_):
        echo "ok"
      else:
        var err0 = err[0]

        #echo ast.lispRepr
        echo err0.node.lispRepr
        err0.node = nil
        echo err0
        error "fail", ast

    var x: ptr int
    testDereferenceOperator(x[])



  ## Addr operator

  block:
    # The addr operator exists only on a typed ast.
    macro testAddrOperator(ast: typed): untyped =
      ast.matchAst(err):
      of nnkAddr({nnkIdent, nnkSym}("x")):
        echo "ok"

    var x: int
    testAddrOperator(addr(x))


  ## Cast operator

  block:

    let ast = quote do:
      cast[T](x)

    ast.matchAst:
    of nnkCast(nnkIdent("T"), nnkIdent("x")):
      echo "ok"
    else:
      error "fail", ast


  ## Object access operator ``.``

  block:

    let ast = quote do:
      x.y

    ast.matchAst:
    of nnkDotExpr(nnkIdent("x"), nnkIdent("y")):
      echo "ok"
    else:
      error "fail", ast

  ## Array access operator ``[]``

  macro testArrayAccessOperator(ast: untyped): untyped =
    ast.matchAst:
    of nnkBracketExpr(nnkIdent("x"), nnkIdent("y")):
      echo "ok"
    else:
      error "fail", ast

  testArrayAccessOperator(x[y])



  ## Parentheses

  block:

    let ast = quote do:
      (1, 2, (3))

    ast.matchAst:
    of nnkPar(nnkIntLit(1), nnkIntLit(2), nnkPar(nnkIntLit(3))):
      echo "ok"
    else:
      error "fail", ast


  ## Curly braces

  block:

    let ast = quote do:
      {1, 2, 3}

    ast.matchAst:
    of nnkCurly(nnkIntLit(1), nnkIntLit(2), nnkIntLit(3)):
      echo "ok"
    else:
      error "fail", ast

  block:

    let ast = quote do:
      {a: 3, b: 5}

    ast.matchAst:
    of nnkTableConstr(
      nnkExprColonExpr(nnkIdent("a"), nnkIntLit(3)),
      nnkExprColonExpr(nnkIdent("b"), nnkIntLit(5))
    ):
      echo "ok"
    else:
      error "fail", ast


  ## Brackets

  block:

    let ast = quote do:
      [1, 2, 3]

    ast.matchAst:
    of nnkBracket(nnkIntLit(1), nnkIntLit(2), nnkIntLit(3)):
      echo "ok"
    else:
      error "fail", ast


  ## Ranges

  block:

    let ast = quote do:
      1..3

    ast.matchAst:
    of nnkInfix(
      nnkIdent(".."),
      nnkIntLit(1),
      nnkIntLit(3)
    ):
      echo "ok"
    else:
      error "fail", ast


  ## If expression

  block:

    let ast = quote do:
      if cond1: expr1 elif cond2: expr2 else: expr3

    ast.matchAst:
    of {nnkIfExpr, nnkIfStmt}(
      {nnkElifExpr, nnkElifBranch}(`cond1`, `expr1`),
      {nnkElifExpr, nnkElifBranch}(`cond2`, `expr2`),
      {nnkElseExpr, nnkElse}(`expr3`)
    ):
      echo "ok"

  ## Documentation Comments

  block:

    let ast = quote do:
      ## This is a comment
      ## This is part of the first comment
      stmt1
      ## Yet another

    ast.matchAst:
    of nnkStmtList(
      nnkCommentStmt(),
      `stmt1`,
      nnkCommentStmt()
    ):
      echo "ok"
    else:
      error "fail", ast

  ## Pragmas

  block:

    let ast = quote do:
      {.emit: "#include <stdio.h>".}

    ast.matchAst:
    of nnkPragma(
      nnkExprColonExpr(
        nnkIdent("emit"),
        nnkStrLit("#include <stdio.h>") # the "argument"
      )
    ):
      echo "ok"
    else:
      error "fail", ast

  block:

    let ast = quote do:
      {.pragma: cdeclRename, cdecl.}

    ast.matchAst:
    of nnkPragma(
      nnkExprColonExpr(
        nnkIdent("pragma"), # this is always first when declaring a new pragma
        nnkIdent("cdeclRename") # the name of the pragma
      ),
      nnkIdent("cdecl")
    ):
      echo "ok"
    else:
      error "fail", ast

  ## If statement

  block:

    let ast = quote do:
      if cond1:
        stmt1
      elif cond2:
        stmt2
      elif cond3:
        stmt3
      else:
        stmt4

    ast.matchAst:
    of nnkIfStmt(
      nnkElifBranch(`cond1`, `stmt1`),
      nnkElifBranch(`cond2`, `stmt2`),
      nnkElifBranch(`cond3`, `stmt3`),
      nnkElse(`stmt4`)
    ):
      echo "ok"
    else:
      error "fail", ast


  ## Assignment

  block:

    let ast = quote do:
      x = 42

    ast.matchAst:
    of nnkAsgn(nnkIdent("x"), nnkIntLit(42)):
      echo "ok"
    else:
      error "fail", ast


  ## Statement list

  block:

    let ast = quote do:
      stmt1
      stmt2
      stmt3

    ast.matchAst:
    of nnkStmtList(`stmt1`, `stmt2`, `stmt3`):
      assert stmt1.strVal == "stmt1"
      assert stmt2.strVal == "stmt2"
      assert stmt3.strVal == "stmt3"
      echo "ok"
    else:
      error "fail", ast

  ## Case statement

  block:

    let ast = quote do:
      case expr1
      of expr2, expr3..expr4:
        stmt1
      of expr5:
        stmt2
      elif cond1:
        stmt3
      else:
        stmt4

    ast.matchAst:
    of nnkCaseStmt(
      `expr1`,
      nnkOfBranch(`expr2`, {nnkRange, nnkInfix}(_, `expr3`, `expr4`), `stmt1`),
      nnkOfBranch(`expr5`, `stmt2`),
      nnkElifBranch(`cond1`, `stmt3`),
      nnkElse(`stmt4`)
    ):
      echo "ok"

  ## While statement

  block:

    let ast = quote do:
      while expr1:
        stmt1

    ast.matchAst:
    of nnkWhileStmt(`expr1`, `stmt1`):
      echo "ok"
    else:
      error "fail", ast


  ## For statement

  block:

    let ast = quote do:
      for ident1, ident2 in expr1:
        stmt1

    ast.matchAst:
    of nnkForStmt(`ident1`, `ident2`, `expr1`, `stmt1`):
      echo "ok"
    else:
      error "fail", ast


  ## Try statement

  block:

    let ast = quote do:
      try:
        stmt1
      except e1, e2:
        stmt2
      except e3:
        stmt3
      except:
        stmt4
      finally:
        stmt5

    ast.matchAst:
    of nnkTryStmt(
      `stmt1`,
      nnkExceptBranch(`e1`, `e2`, `stmt2`),
      nnkExceptBranch(`e3`, `stmt3`),
      nnkExceptBranch(`stmt4`),
      nnkFinally(`stmt5`)
    ):
      echo "ok"
    else:
      error "fail", ast


  ## Return statement

  block:

    let ast = quote do:
      return expr1

    ast.matchAst:
    of nnkReturnStmt(`expr1`):
      echo "ok"
    else:
      error "fail", ast


  ## Continue statement

  block:
    let ast = quote do:
      continue

    ast.matchAst:
    of nnkContinueStmt:
      echo "ok"

  ## Break statement

  block:

    let ast = quote do:
      break otherLocation

    ast.matchAst:
    of nnkBreakStmt(nnkIdent("otherLocation")):
      echo "ok"
    else:
      error "fail", ast

  ## Block statement

  block:

    let ast = quote do:
      block name:
        discard

    ast.matchAst:
    of nnkBlockStmt(nnkIdent("name"), nnkStmtList):
      echo "ok"
    else:
      error "fail", ast

  ## Asm statement

  block:

    let ast = quote do:
      asm """some asm"""

    ast.matchAst:
    of nnkAsmStmt(
      nnkEmpty(), # for pragmas
      nnkTripleStrLit("some asm"),
    ):
      echo "ok"

  ## Import section

  block:

    let ast = quote do:
      import math

    ast.matchAst:
    of nnkImportStmt(nnkIdent("math")):
      echo "ok":
    else:
      error "fail", ast

  block:

    let ast = quote do:
      import math except pow

    ast.matchAst:
    of nnkImportExceptStmt(nnkIdent("math"),nnkIdent("pow")):
      echo "ok"
    else:
      error "fail", ast

  block:

    let ast = quote do:
      import strutils as su

    ast.matchAst:
    of nnkImportStmt(
      nnkInfix(
        nnkIdent("as"),
        nnkIdent("strutils"),
        nnkIdent("su")
      )
    ):
      echo "ok"
    else:
      error "fail", ast

  ## From statement

  block:

    let ast = quote do:
      from math import pow

    ast.matchAst:
    of nnkFromStmt(nnkIdent("math"), nnkIdent("pow")):
      echo "ok"
    else:
      error "fail", ast

  ## Export statement

  block:

    let ast = quote do:
      export unsigned

    ast.matchAst:
    of nnkExportStmt(nnkIdent("unsigned")):
      echo "ok"
    else:
      error "fail", ast

  block:

    let ast = quote do:
      export math except pow # we're going to implement our own exponentiation

    ast.matchAst:
    of nnkExportExceptStmt(nnkIdent("math"),nnkIdent("pow")):
      echo "ok"
    else:
      error "fail", ast

  ## Include statement

  block:

    let ast = quote do:
      include blocks

    ast.matchAst:
    of nnkIncludeStmt(nnkIdent("blocks")):
      echo "ok"
    else:
      error "fail", ast

  ## Var section

  block:

    let ast = quote do:
      var a = 3

    ast.matchAst:
    of nnkVarSection(
      nnkIdentDefs(
        nnkIdent("a"),
        nnkEmpty(), # or nnkIdent(...) if the variable declares the type
        nnkIntLit(3),
      )
    ):
      echo "ok"
    else:
      error "fail", ast

  ## Let section

  block:

    let ast = quote do:
      let a = 3

    ast.matchAst:
    of nnkLetSection(
      nnkIdentDefs(
        nnkIdent("a"),
        nnkEmpty(), # or nnkIdent(...) for the type
        nnkIntLit(3),
      )
    ):
      echo "ok"
    else:
      error "fail", ast

  ## Const section

  block:

    let ast = quote do:
      const a = 3

    ast.matchAst:
    of nnkConstSection(
      nnkConstDef( # not nnkConstDefs!
        nnkIdent("a"),
        nnkEmpty(), # or nnkIdent(...) if the variable declares the type
        nnkIntLit(3), # required in a const declaration!
      )
    ):
      echo "ok"
    else:
      error "fail", ast

  ## Type section

  block:

    let ast = quote do:
      type A = int

    ast.matchAst:
    of nnkTypeSection(
      nnkTypeDef(
        nnkIdent("A"),
        nnkEmpty(),
        nnkIdent("int")
      )
    ):
      echo "ok"
    else:
      error "fail", ast

  block:

    let ast = quote do:
      type MyInt = distinct int

    ast.peelOff({nnkTypeSection}).matchAst:
    of# ...
      nnkTypeDef(
      nnkIdent("MyInt"),
      nnkEmpty(),
      nnkDistinctTy(
        nnkIdent("int")
      )
    ):
      echo "ok"

  block:

    let ast = quote do:
      type A[T] = expr1

    ast.matchAst:
    of nnkTypeSection(
      nnkTypeDef(
        nnkIdent("A"),
        nnkGenericParams(
          nnkIdentDefs(
            nnkIdent("T"),
            nnkEmpty(), # if the type is declared with options, like
                        # ``[T: SomeInteger]``, they are given here
            nnkEmpty()
          )
        ),
        `expr1`
      )
    ):
      echo "ok"
    else:
      error "fail", ast

  block:

    let ast = quote do:
      type IO = object of RootObj

    ast.peelOff(nnkTypeSection).matchAst:
    of nnkTypeDef(
      nnkIdent("IO"),
      nnkEmpty(),
      nnkObjectTy(
        nnkEmpty(), # no pragmas here
        nnkOfInherit(
          nnkIdent("RootObj") # inherits from RootObj
        ),
        nnkEmpty()
      )
    ):
      echo "ok"

  block:
    macro testRecCase(ast: untyped): untyped =
      ast.peelOff({nnkStmtList, nnkTypeSection})[2].matchAst:
      of nnkObjectTy(
        nnkPragma(
          nnkIdent("inheritable")
        ),
        nnkEmpty(),
        nnkRecList( # list of object parameters
          nnkIdentDefs(
            nnkIdent("name"),
            nnkIdent("string"),
            nnkEmpty()
          ),
          nnkRecCase( # case statement within object (not nnkCaseStmt)
            nnkIdentDefs(
              nnkIdent("isFat"),
              nnkIdent("bool"),
              nnkEmpty()
            ),
            nnkOfBranch(
              nnkIdent("true"),
              nnkRecList( # again, a list of object parameters
                nnkIdentDefs(
                  nnkIdent("m"),
                  nnkBracketExpr(
                    nnkIdent("array"),
                    nnkIntLit(100000),
                    nnkIdent("T")
                  ),
                  nnkEmpty()
                )
              )
            ),
            nnkOfBranch(
              nnkIdent("false"),
              nnkRecList(
                nnkIdentDefs(
                  nnkIdent("m"),
                  nnkBracketExpr(
                    nnkIdent("array"),
                    nnkIntLit(10),
                    nnkIdent("T")
                  ),
                  nnkEmpty()
                )
              )
            )
          )
        )
      ):
        echo "ok"



    testRecCase:
      type Obj[T] = object {.inheritable.}
        name: string
        case isFat: bool
        of true:
          m: array[100_000, T]
        of false:
          m: array[10, T]

  block:

    let ast = quote do:
      type X = enum
        First

    ast.peelOff({nnkStmtList, nnkTypeSection})[2].matchAst:
    of nnkEnumTy(
      nnkEmpty(),
      nnkIdent("First") # you need at least one nnkIdent or the compiler complains
    ):
      echo "ok"

  block:

    let ast = quote do:
      type Con = concept x,y,z
        (x & y & z) is string

    ast.peelOff({nnkStmtList, nnkTypeSection}).matchAst:
    of nnkTypeDef(_, _, nnkTypeClassTy(nnkArgList, _, _, nnkStmtList)):
      # note this isn't nnkConceptTy!
      echo "ok"


  block:

    let astX = quote do:
      type
        A[T: static[int]] = object

    let ast = astX.peelOff({nnkStmtList, nnkTypeSection})

    ast.matchAst(err):  # this is a sub ast for this a findAst or something like that is useful
    of nnkTypeDef(_, nnkGenericParams( nnkIdentDefs( nnkIdent("T"), nnkStaticTy( _ ), nnkEmpty )), _):
      echo "ok"

  block:
    let ast = quote do:
      type MyProc[T] = proc(x: T)

    ast.peelOff({nnkStmtList, nnkTypeSection}).matchAst(err):
    of nnkTypeDef(
      nnkIdent("MyProc"),
      nnkGenericParams, # here, not with the proc
      nnkProcTy( # behaves like a procedure declaration from here on
        nnkFormalParams
      )
    ):
      echo "ok"
    else:
      echo err[0].node.lispRepr

  ## Mixin statement

  macro testMixinStatement(ast: untyped): untyped =
    ast.peelOff(nnkStmtList).matchAst:
    of nnkMixinStmt(nnkIdent("x")):
      echo "ok"

  testMixinStatement:
    mixin x

  ## Bind statement


  macro testBindStmt(ast: untyped): untyped =
    ast[0].matchAst:
    of `node` @ nnkBindStmt(nnkIdent("x")):
      echo "ok"

  testBindStmt:
    bind x

  ## Procedure declaration

  block:

    let ast = quote do:
      proc hello*[T: SomeInteger](x: int = 3, y: float32): int {.inline.} = discard

    echo ast.treeRepr
    ast.matchAst:
    of nnkProcDef(
      nnkPostfix(nnkIdent("*"), nnkIdent("hello")), # the exported proc name
      nnkEmpty, # patterns for term rewriting in templates and macros (not procs)
      nnkGenericParams( # generic type parameters, like with type declaration
        nnkIdentDefs(
          nnkIdent("T"),
          nnkIdent("SomeInteger")
        )
      ),
      nnkFormalParams(
        nnkIdent("int"), # the first FormalParam is the return type. nnkEmpty if there is none
        nnkIdentDefs(
          nnkIdent("x"),
          nnkIdent("int"), # type type (required for procs, not for templates)
          nnkIntLit(3) # a default value
        ),
        nnkIdentDefs(
          nnkIdent("y"),
          nnkIdent("float32"),
          nnkEmpty
        ),
        nnkPragma(nnkIdent("inline")),
        nnkEmpty, # reserved slot for future use
        nnkStmtList(nnkDiscardStmt(nnkEmpty)) # the meat of the proc
      )
    ):
      echo "ok"

  block:

    let ast = quote do:
      proc foobar(a, b: int): void

    echo ast.treeRepr

    ast.matchAst:  # sub expression
    of nnkFormalParams(
      nnkEmpty(), # no return here
      nnkIdentDefs(
        nnkIdent("a"), # the first parameter
        nnkIdent("b"), # directly to the second parameter
        nnkIdent("int"), # their shared type identifier
        nnkEmpty(), # default value would go here
      )
    ):
      echo "ok"

  block:

    let ast = quote do:
      proc hello(): var int

    ast.matchAst: # subAst
    of nnkFormalParams(
      nnkVarTy(
        nnkIdent("int")
      )
    ):
      echo "ok"
    else:
      error "fail", ast

  ## Iterator declaration

  block:

    let ast = quote do:
      iterator nonsense[T](x: seq[T]): float {.closure.} =
        discard

    echo "foobar"
    echo ast.treeRepr

    ast.matchAst:
    of nnkIteratorDef(
      nnkIdent("nonsense"),
      nnkEmpty()
    ):
      echo "ok"

  ## Converter declaration

  block:

    let ast = quote do:
      converter toBool(x: float): bool

    ast.matchAst:
    of nnkConverterDef(
      nnkIdent("toBool"),
      # ...
    ):
      echo "ok"

  ## Template declaration

  block:
    let ast = quote do:
      template optOpt{expr1}(a: int): int

    ast.matchAst:
    of nnkTemplateDef(
      nnkIdent("optOpt"),
      nnkStmtList( # instead of nnkEmpty()
        `expr1`
      )
      # follows like a proc or iterator
    ):
      echo "ok"
