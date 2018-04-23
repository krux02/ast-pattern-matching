# Ast Pattern Matching


    matchAst(arg, matchErrors):
    of nnkStmtList(
      _,
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
      echo matchErrors
      echo "sadly the AST did not match :("


`matchAst` is where the magic happens. In the of-branch is a tree that
can at any time be generated via `lispRepr` on an arbitrary
`NimNode`. But the pattern language also allows for some additional
constructs, to widen the possibilities of the pattern.


The grammar for the pattern matching looks like this:

  * ``<pattern>`` is an arbitrary pattern

  * ``of <pattern>:`` is a branch of the pattern matching library.

  * ``nnkInfix`` an example how to just check the node kind. It matches
    on any node of kind ``nnkInfix``. It won't check the length or any
    of the children.

  * ``nnkStmtList(<pattern>, <pattern>, <pattern>)`` will match on any
    any node of kind `nnkStmtList` with length 3, if also the
    subpatterns of the children match.

  * ``nnkStmtList`` without an opening brace `(` is just like
    ``nnkInfix`` again. It won't check for any children.

  * ``nnkStmtList()`` matches only an empty statement list.

  * `_` the wildcard, matches on everything.
  .
  * `` `somename` `` is a named subtree. Like the wildcard, it matches
    on everything. But it creates an identifier for that subtree, so that
    it can be called by name in the branch body.

  * ``nnkIntLit(intVal = 123)`` will match the integer literal `123` but
    not ``123'u32``.

  * `_(intVal = myConst)` matches any literal with the value of
    `myConst`. This is the recommended way if you want to mach
    constants. Currently the lhs side of the matching expression is
    completely ignored, so you could match strings with ``_(intVal =
    myStrConst)`` and the other way around. But please don't do
    that. It will confuse programmers, and I will break that code if
    I found a reliable way to disallow it.

  * Just literals like `123` will also match any literals with the
    same value.  So it would match on a literal such as `123`, but not on
    literals of a different kind such as ``123'u32``.

  * `` `somename` @ <pattern> `` a named subtree. it matches on
    _<pattern>_ but binds the name `somename` to that node.
    checked by _<pattern>_

  * ``else:`` the else branch is the code path that is taken when the
    pattern fails to match. The else branch is optional. When a
    pattern does not match, an error will be raised.

  * `matchErrors` in the example above is used as an identifier for a
    seq of match errors. it will be avalilable in the else branch as
    ``let matchErrors: seq[MatchingError]``. It's an optional argument
    and the purpose is to debug matching expressions.

  * ``ident"abc"`` will match all symbols and identifiers etc that are
    equal to "abc". For equality checks eqIdent will be used. A node
    `n` that returns true in ``eqIdent(n, "abc")`` will be matched
    with that expression.

  * An identifier can also be matched with ``nnkIdent(strVal =
    "abc")`` but that would not match on symbols choices or identifiers with a
    different style such as `aB_c`. Please don't use this for
    identifier matching.

  * ``<expr>(...)`` any expression can be used to match for the
    idintifier, as long as it is an expression that evaluates to
    ``NimNodeKind`` or ``set[NimNodeKind]``. For example
    ``{nnkElifExpr, nnkElifBranch}(_)`` is a totally valid pattern to
    match for ``elif:``.

  * ``<expr>`` meaning any expression that is not one of the other
    expressions is expected to evaluate to a ``NimNodeKind`` or
    ``set[NimNodeKind]`` and is used to mach the kind for an expression.

## Examples

When you pass a second argument to mach as, as in the following
example with `matchErrors`, this identifier will be usable in the
_else_ branch as a name for all error kinds. But please use this error
type only for debugging purpose. If for the sake of nice error messages
the type has to chage, it will be changed, please don't rely on
the strucuture of this type.


    matchAst(arg, matchErrors):
    of <pattern>: # branch A
      discard
    of <pattern>: # branch B
      discard
    of <pattern>: # branch C
      discard
    else:
      echo "branch A could not match because:"
      echo matchErrors[0]
      echo "branch B could not match because:"
      echo matchErrors[1]
      echo "branch C could not match because:"
      echo matchErrors[2]


When you leave out the else branch and there is only one of branch,
you will get the nicest error message possible, why the pattern did
not match.  Just try it out with this example:


    let ast = quote do:
      abc
      def
      ghi
      jkl

    ast.matchAst:
    of nnkStmtList( `a`, `b`, `c`):
      echo "the branch did match"
      echo "but I do know that is impossible"

for more examples, take a look at the sourcecode. The file
`tests/test1.nim` has a lot of examples that should you get started.


## discussion

Maybe at some point the pattern matching library will contain
operators to match more flexible patterns with operators such as `+`,
`*`, `|`, `?`. but that is not implemented. It would be possible though.

It would just raise the question how are named subexpressions are
handled in such optional pattern branches.

if the parser allows it to add custom conditions to of branches, such
as ``of <patter> if a > b:`` it will be implemented.


The ast matching statement does not work as an expression (yet).
