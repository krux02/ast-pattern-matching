import ast_pattern_matching, strformat

# This macro mimics the using statement from C#

macro autoClose(args: varargs[untyped]): untyped =
  var body = args[^1]

  var
    variables : seq[NimNode]
    closingCalls : seq[NimNode]

  newSeq(variables, 0)
  newSeq(closingCalls, 0)

  for i in 0 ..< args.len-1:
    args[i].matchAst:
    of `arg` @ nnkExprEqExpr:

      var varName = arg[0]
      var varValue = arg[1]

      var varAssignment = newNimNode(nnkIdentDefs)
      varAssignment.add(varName)
      varAssignment.add(newNimNode(nnkEmpty)) # empty means no type
      varAssignment.add(varValue)
      variables.add(varAssignment)

      closingCalls.add(newCall("close", varName))

  var varSection = newNimNode(nnkVarSection)
  varSection.add(variables)

  var finallyBlock = newNimNode(nnkStmtList)
  finallyBlock.add(closingCalls)

  result = quote do:
    `varSection`
    try:
      `body`
    finally:
      `finallyBlock`

type
  TResource* = object
    field*: string

proc openResource(param: string): TResource =
  result.field = param

proc close(r: var TResource) =
  writeLine stdout, fmt"Closing {r.field}."

proc use(r: var TResource) =
  writeLine stdout, fmt"Using {r.field}."

autoClose(
    r1 = openResource("test"),
    r2 = openResource("tast"),
    uidtarne
):
  use r1
  use r2
