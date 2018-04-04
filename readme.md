# Ast Pattern Matching


    matchAst(arg, matchError):
    of StmtList(
      _,
      _,
      ForStmt(
        Ident(ident"i"),
        Infix,
        `mysym` @ StmtList
      )
    ):
      echo "The AST did match!!!"
      echo "The matched sub tree is the following:"
      echo mysym.lispRepr
    else:
      echo matchError
      echo "sadly the AST did not match :("


`matchAst` is where the magic happens. In the of-branch is a tree that
can at any time be generated vial `lispRepr` on an arbitrary
`NimNode`. But the pattern language also allows for some additional
constructs, to winden the possibilities of the pattern.


  * `_` the wildcard can be any subtree without checking
  * `` `somename` ``named subtree. Like the wildcard, can be any
    subtree, but it creates an identifier for that subtree, so that it
    can be used in the branch body
  * `` `somename` @ <pattern> `` a named subtree that is further
    checked by _<pattern>_
  * `Infix` an example of just the node kind without braces. in this
    case the childern of the infix node are not checked.
  * `matchError` is the identifier that is used, so that in the else
    branch the programmer can loop up where the pattern matching did
    fail and can produce either an error or do other kinds of error
    handling.
  * ``else:`` the else branch is the code path that is taken when the
    pattern fails to match.
