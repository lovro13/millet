open Utils
module Ast = Language.Ast
module Const = Language.Const
module Primitives = Language.Primitives

let binary_function f = function
  | Ast.Tuple [ expr1; expr2 ] -> f expr1 expr2
  | expr -> Error.runtime "Pair expected but got %t" (Ast.print_expression expr)

let get_int = function
  | Ast.Const (Const.Integer n) -> n
  | expr ->
      Error.runtime "Integer expected but got %t" (Ast.print_expression expr)

let get_float = function
  | Ast.Const (Const.Float n) -> n
  | expr ->
      Error.runtime "Float expected but got %t" (Ast.print_expression expr)

let int_to f expr =
  let n = get_int expr in
  f n

let int_int_to f expr =
  binary_function
    (fun expr1 expr2 ->
      let n1 = get_int expr1 in
      let n2 = get_int expr2 in
      f n1 n2)
    expr

let float_to f expr =
  let n = get_float expr in
  f n

let float_float_to f expr =
  binary_function
    (fun expr1 expr2 ->
      let n1 = get_float expr1 in
      let n2 = get_float expr2 in
      f n1 n2)
    expr

let int_to_int f =
  int_to (fun n -> Ast.Return (Ast.Const (Const.Integer (f n))))

let int_int_to_int f =
  int_int_to (fun n1 n2 -> Ast.Return (Ast.Const (Const.Integer (f n1 n2))))

let float_to_float f =
  float_to (fun n -> Ast.Return (Ast.Const (Const.Float (f n))))

let float_float_to_float f =
  float_float_to (fun n1 n2 -> Ast.Return (Ast.Const (Const.Float (f n1 n2))))

let rec comparable_expression = function
  | Ast.Var _ -> true
  | Ast.TopDef _ -> true
  | Const _ -> true
  | Annotated (e, _) -> comparable_expression e
  | Tuple es -> List.for_all comparable_expression es
  | Variant (_, e) -> Option.fold ~none:true ~some:comparable_expression e
  | Lambda _ -> false
  | RecLambda _ -> false

let comparison_to_int = function
  | Const.Less -> -1
  | Const.Equal -> 0
  | Const.Greater -> 1

let expression_tag = function
  | Ast.Var _ -> 0
  | Ast.TopDef _ -> 1
  | Ast.Const _ -> 2
  | Ast.Annotated _ -> 3
  | Ast.Tuple _ -> 4
  | Ast.Variant _ -> 5
  | Ast.Lambda _ -> 6
  | Ast.RecLambda _ -> 7

let rec compare_expression expression1 expression2 =
  match (expression1, expression2) with
  | Ast.Var variable1, Ast.Var variable2 ->
      Int.compare (Bindlib.uid_of variable1) (Bindlib.uid_of variable2)
  | Ast.TopDef top_def1, Ast.TopDef top_def2 ->
      Int.compare (Bindlib.uid_of top_def1) (Bindlib.uid_of top_def2)
  | Ast.Const constant1, Ast.Const constant2 ->
      comparison_to_int (Const.compare constant1 constant2)
  | Ast.Annotated (expression1, ty1), Ast.Annotated (expression2, ty2) ->
      let result = compare_expression expression1 expression2 in
      if result = 0 then Stdlib.compare ty1 ty2 else result
  | Ast.Tuple expressions1, Ast.Tuple expressions2 ->
      List.compare compare_expression expressions1 expressions2
  | Ast.Variant (label1, expression1), Ast.Variant (label2, expression2) ->
      let result = Int.compare (Bindlib.uid_of label1) (Bindlib.uid_of label2) in
      if result = 0 then Option.compare compare_expression expression1 expression2
      else result
  | (Ast.Lambda _ | Ast.RecLambda _), (Ast.Lambda _ | Ast.RecLambda _) ->
      assert false
  | _ -> Int.compare (expression_tag expression1) (expression_tag expression2)

let comparison f =
  binary_function (fun e1 e2 ->
      if not (comparable_expression e1) then
        Error.runtime "Incomparable expression %t"
          (Ast.print_expression ~max_level:0 e1)
      else if not (comparable_expression e2) then
        Error.runtime "Incomparable expression %t"
          (Ast.print_expression ~max_level:0 e2)
      else Ast.Return (Ast.Const (Const.Boolean (f e1 e2))))

let primitive_function = function
  | Primitives.CompareEq -> comparison (fun e1 e2 -> compare_expression e1 e2 = 0)
  | Primitives.CompareLt -> comparison (fun e1 e2 -> compare_expression e1 e2 < 0)
  | Primitives.CompareGt -> comparison (fun e1 e2 -> compare_expression e1 e2 > 0)
  | Primitives.CompareLe -> comparison (fun e1 e2 -> compare_expression e1 e2 <= 0)
  | Primitives.CompareGe -> comparison (fun e1 e2 -> compare_expression e1 e2 >= 0)
  | Primitives.CompareNe -> comparison (fun e1 e2 -> compare_expression e1 e2 <> 0)
  | Primitives.IntegerAdd -> int_int_to_int ( + )
  | Primitives.IntegerMul -> int_int_to_int ( * )
  | Primitives.IntegerSub -> int_int_to_int ( - )
  | Primitives.IntegerDiv -> int_int_to_int ( / )
  | Primitives.IntegerMod -> int_int_to_int ( mod )
  | Primitives.IntegerNeg -> int_to_int ( ~- )
  | Primitives.FloatAdd -> float_float_to_float ( +. )
  | Primitives.FloatMul -> float_float_to_float ( *. )
  | Primitives.FloatSub -> float_float_to_float ( -. )
  | Primitives.FloatDiv -> float_float_to_float ( /. )
  | Primitives.FloatPow -> float_float_to_float ( ** )
  | Primitives.FloatNeg -> float_to_float ( ~-. )
  | Primitives.ToString ->
      fun expr ->
        Ast.Return (Ast.Const (Const.String (Ast.string_of_expression expr)))
