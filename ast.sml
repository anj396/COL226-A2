(* ast.sml *)
structure AST =
struct
datatype fo = Id of string
| CONST
| BinExp of binop * fo * fo
| Unexp of unop * fo
| Triexp of triop *fo*triop*fo*triop*fo
and binop = OR | XOR | EQUALS | AND | IMPLIES
and unop = NOT
and triop = IF|THEN|ELSE
end

