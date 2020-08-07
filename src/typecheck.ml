open Syntax

exception TypecheckError of string

type env = (id * type_) list

let addid env id ty = (id, ty) :: env

let rec check_pattern env pat ty =
  match (pat, ty) with
  | PWildcard, _ -> env
  | PBool _, TyBool -> env
  | PInt _, TyInt -> env
  | PId id, _ -> addid env id ty
  | PTuple pats, TyTuple tys -> List.fold_left2 check_pattern env pats tys
  | _ -> raise (TypecheckError "Pattern of wrong type")

let rec check_term env = function
  | TApply (t1, t2) -> (
      let ty2 = check_term env t2 in
      match check_term env t1 with
      | TyFn (inty, outty) when inty = ty2 -> outty
      | _ -> raise (TypecheckError "Type mismatch") )
  | TLet (p, ty, t1, t2) ->
      let ty1 = check_term env t1 in
      if ty1 <> ty then raise (TypecheckError "Type mismatch")
      else
        let env' = check_pattern env p ty in
        check_term env' t2
  | TCase (t, cs) ->
      let ty = check_term env t in
      let check_case (pat, term) =
        let env' = check_pattern env pat ty in
        check_term env' term
      in
      let term_type = check_case (List.hd cs) in
      List.iter
        (fun case ->
          if check_case case = term_type then ()
          else raise (TypecheckError "Type mismatch"))
        cs;
      term_type
  | TIf (t1, t2, t3) ->
      let ty1 = check_term env t1 in
      if ty1 <> TyBool then raise (TypecheckError "Type mismatch")
      else
        let ty2 = check_term env t2 in
        let ty3 = check_term env t3 in
        if ty2 = ty3 then ty2 else raise (TypecheckError "Type mismatch")
  | TFn (p, ty, t) -> (
      match ty with
      | TyFn (inty, outty) ->
          let env' = check_pattern env p inty in
          let ty' = check_term env' t in
          if outty == ty' then ty else raise (TypecheckError "Type mismatch")
      | _ -> raise (TypecheckError "Type mismatch") )
  | TId id -> List.assoc id env
  | TBool _ -> TyBool
  | TInt _ -> TyInt
  | TTuple t -> TyTuple (List.map (check_term env) t)
