(**
   Boilerplate to be used as a template when mapping the rust CST
   to another type of tree.
*)

(* Disable warnings against unused variables *)
[@@@warning "-26-27"]

(* Disable warning against unused 'rec' *)
[@@@warning "-39"]

type env = unit

let token (env : env) (_tok : Tree_sitter_run.Token.t) =
  failwith "not implemented"

let blank (env : env) () =
  failwith "not implemented"

let todo (env : env) _ =
   failwith "not implemented"

let map_fragment_specifier (env : env) (x : CST.fragment_specifier) =
  (match x with
  | `Blk tok -> (* "block" *) token env tok
  | `Expr tok -> (* "expr" *) token env tok
  | `Id tok -> (* "ident" *) token env tok
  | `Item tok -> (* "item" *) token env tok
  | `Life tok -> (* "lifetime" *) token env tok
  | `Lit tok -> (* "literal" *) token env tok
  | `Meta tok -> (* "meta" *) token env tok
  | `Pat tok -> (* "pat" *) token env tok
  | `Path tok -> (* "path" *) token env tok
  | `Stmt tok -> (* "stmt" *) token env tok
  | `Tt tok -> (* "tt" *) token env tok
  | `Ty tok -> (* "ty" *) token env tok
  | `Vis tok -> (* "vis" *) token env tok
  )

let map_block_comment (env : env) (tok : CST.block_comment) =
  (* block_comment *) token env tok

let map_metavariable (env : env) (tok : CST.metavariable) =
  (* pattern \$[a-zA-Z_]\w* *) token env tok

let map_string_content (env : env) (tok : CST.string_content) =
  (* string_content *) token env tok

let map_anon_choice_PLUS_348fa54 (env : env) (x : CST.anon_choice_PLUS_348fa54) =
  (match x with
  | `PLUS tok -> (* "+" *) token env tok
  | `STAR tok -> (* "*" *) token env tok
  | `QMARK tok -> (* "?" *) token env tok
  )

let map_line_comment (env : env) (tok : CST.line_comment) =
  (* line_comment *) token env tok

let map_anon_choice_u8_6dad923 (env : env) (x : CST.anon_choice_u8_6dad923) =
  (match x with
  | `U8 tok -> (* "u8" *) token env tok
  | `I8 tok -> (* "i8" *) token env tok
  | `U16 tok -> (* "u16" *) token env tok
  | `I16 tok -> (* "i16" *) token env tok
  | `U32 tok -> (* "u32" *) token env tok
  | `I32 tok -> (* "i32" *) token env tok
  | `U64 tok -> (* "u64" *) token env tok
  | `I64 tok -> (* "i64" *) token env tok
  | `U128 tok -> (* "u128" *) token env tok
  | `I128 tok -> (* "i128" *) token env tok
  | `Isize tok -> (* "isize" *) token env tok
  | `Usize tok -> (* "usize" *) token env tok
  | `F32 tok -> (* "f32" *) token env tok
  | `F64 tok -> (* "f64" *) token env tok
  | `Bool tok -> (* "bool" *) token env tok
  | `Str tok -> (* "str" *) token env tok
  | `Char tok -> (* "char" *) token env tok
  )

let map_float_literal (env : env) (tok : CST.float_literal) =
  (* float_literal *) token env tok

let map_integer_literal (env : env) (tok : CST.integer_literal) =
  (* integer_literal *) token env tok

let map_pat_a8c54f1 (env : env) (tok : CST.pat_a8c54f1) =
  (* pattern [/_\-=->,;:::!=?.@*&#%^+<>|~]+ *) token env tok

let map_escape_sequence (env : env) (tok : CST.escape_sequence) =
  (* escape_sequence *) token env tok

let map_char_literal (env : env) (tok : CST.char_literal) =
  (* char_literal *) token env tok

let map_pat_1e84e62 (env : env) (tok : CST.pat_1e84e62) =
  (* pattern [^+*?]+ *) token env tok

let map_boolean_literal (env : env) (x : CST.boolean_literal) =
  (match x with
  | `True tok -> (* "true" *) token env tok
  | `False tok -> (* "false" *) token env tok
  )

let map_raw_string_literal (env : env) (tok : CST.raw_string_literal) =
  (* raw_string_literal *) token env tok

let map_reserved_identifier (env : env) (x : CST.reserved_identifier) =
  (match x with
  | `Defa tok -> (* "default" *) token env tok
  | `Union tok -> (* "union" *) token env tok
  )

let map_pat_36c5a8e (env : env) (tok : CST.pat_36c5a8e) =
  (* pattern "b?\"" *) token env tok

let map_identifier (env : env) (tok : CST.identifier) =
  (* identifier *) token env tok

let map_string_literal (env : env) ((v1, v2, v3) : CST.string_literal) =
  let v1 = (* pattern "b?\"" *) token env v1 in
  let v2 =
    List.map (fun x ->
      (match x with
      | `Esc_seq tok -> (* escape_sequence *) token env tok
      | `Str_content tok -> (* string_content *) token env tok
      )
    ) v2
  in
  let v3 = (* "\"" *) token env v3 in
  todo env (v1, v2, v3)

let map_literal_pattern (env : env) (x : CST.literal_pattern) =
  (match x with
  | `Str_lit x -> map_string_literal env x
  | `Raw_str_lit tok -> (* raw_string_literal *) token env tok
  | `Char_lit tok -> (* char_literal *) token env tok
  | `Bool_lit x -> map_boolean_literal env x
  | `Int_lit tok -> (* integer_literal *) token env tok
  | `Float_lit tok -> (* float_literal *) token env tok
  | `Nega_lit (v1, v2) ->
      let v1 = (* "-" *) token env v1 in
      let v2 =
        (match v2 with
        | `Int_lit tok -> (* integer_literal *) token env tok
        | `Float_lit tok -> (* float_literal *) token env tok
        )
      in
      todo env (v1, v2)
  )

let map_extern_modifier (env : env) ((v1, v2) : CST.extern_modifier) =
  let v1 = (* "extern" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> map_string_literal env x
    | None -> todo env ())
  in
  todo env (v1, v2)

let map_literal (env : env) (x : CST.literal) =
  (match x with
  | `Str_lit x -> map_string_literal env x
  | `Raw_str_lit tok -> (* raw_string_literal *) token env tok
  | `Char_lit tok -> (* char_literal *) token env tok
  | `Bool_lit x -> map_boolean_literal env x
  | `Int_lit tok -> (* integer_literal *) token env tok
  | `Float_lit tok -> (* float_literal *) token env tok
  )

let map_loop_label (env : env) ((v1, v2) : CST.loop_label) =
  let v1 = (* "'" *) token env v1 in
  let v2 = (* identifier *) token env v2 in
  todo env (v1, v2)

let map_lifetime (env : env) ((v1, v2) : CST.lifetime) =
  let v1 = (* "'" *) token env v1 in
  let v2 = (* identifier *) token env v2 in
  todo env (v1, v2)

let map_anon_choice_field_id_8184947 (env : env) (x : CST.anon_choice_field_id_8184947) =
  (match x with
  | `Id tok -> (* identifier *) token env tok
  | `Meta tok -> (* pattern \$[a-zA-Z_]\w* *) token env tok
  )

let map_function_modifiers (env : env) (xs : CST.function_modifiers) =
  List.map (fun x ->
    (match x with
    | `Async tok -> (* "async" *) token env tok
    | `Defa tok -> (* "default" *) token env tok
    | `Const tok -> (* "const" *) token env tok
    | `Unsafe tok -> (* "unsafe" *) token env tok
    | `Extern_modi x -> map_extern_modifier env x
    )
  ) xs

let map_non_special_token (env : env) (x : CST.non_special_token) =
  (match x with
  | `Lit x -> map_literal env x
  | `Id tok -> (* identifier *) token env tok
  | `Meta tok -> (* pattern \$[a-zA-Z_]\w* *) token env tok
  | `Muta_spec tok -> (* "mut" *) token env tok
  | `Self tok -> (* "self" *) token env tok
  | `Super tok -> (* "super" *) token env tok
  | `Crate tok -> (* "crate" *) token env tok
  | `Choice_u8 x -> map_anon_choice_u8_6dad923 env x
  | `Pat_a8c54f1 tok ->
      (* pattern [/_\-=->,;:::!=?.@*&#%^+<>|~]+ *) token env tok
  | `SQUOT tok -> (* "'" *) token env tok
  | `As tok -> (* "as" *) token env tok
  | `Async tok -> (* "async" *) token env tok
  | `Await tok -> (* "await" *) token env tok
  | `Brk tok -> (* "break" *) token env tok
  | `Const tok -> (* "const" *) token env tok
  | `Cont tok -> (* "continue" *) token env tok
  | `Defa tok -> (* "default" *) token env tok
  | `Enum tok -> (* "enum" *) token env tok
  | `Fn tok -> (* "fn" *) token env tok
  | `For tok -> (* "for" *) token env tok
  | `If tok -> (* "if" *) token env tok
  | `Impl tok -> (* "impl" *) token env tok
  | `Let tok -> (* "let" *) token env tok
  | `Loop tok -> (* "loop" *) token env tok
  | `Match tok -> (* "match" *) token env tok
  | `Mod tok -> (* "mod" *) token env tok
  | `Pub tok -> (* "pub" *) token env tok
  | `Ret tok -> (* "return" *) token env tok
  | `Static tok -> (* "static" *) token env tok
  | `Struct tok -> (* "struct" *) token env tok
  | `Trait tok -> (* "trait" *) token env tok
  | `Type tok -> (* "type" *) token env tok
  | `Union tok -> (* "union" *) token env tok
  | `Unsafe tok -> (* "unsafe" *) token env tok
  | `Use tok -> (* "use" *) token env tok
  | `Where tok -> (* "where" *) token env tok
  | `While tok -> (* "while" *) token env tok
  )

let map_for_lifetimes (env : env) ((v1, v2, v3, v4, v5, v6) : CST.for_lifetimes) =
  let v1 = (* "for" *) token env v1 in
  let v2 = (* "<" *) token env v2 in
  let v3 = map_lifetime env v3 in
  let v4 =
    List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_lifetime env v2 in
      todo env (v1, v2)
    ) v4
  in
  let v5 =
    (match v5 with
    | Some tok -> (* "," *) token env tok
    | None -> todo env ())
  in
  let v6 = (* ">" *) token env v6 in
  todo env (v1, v2, v3, v4, v5, v6)

let rec map_token_tree (env : env) (x : CST.token_tree) =
  (match x with
  | `LPAR_rep_choice_tok_tree_RPAR (v1, v2, v3) ->
      let v1 = (* "(" *) token env v1 in
      let v2 = List.map (map_tokens env) v2 in
      let v3 = (* ")" *) token env v3 in
      todo env (v1, v2, v3)
  | `LBRACK_rep_choice_tok_tree_RBRACK (v1, v2, v3) ->
      let v1 = (* "[" *) token env v1 in
      let v2 = List.map (map_tokens env) v2 in
      let v3 = (* "]" *) token env v3 in
      todo env (v1, v2, v3)
  | `LCURL_rep_choice_tok_tree_RCURL (v1, v2, v3) ->
      let v1 = (* "{" *) token env v1 in
      let v2 = List.map (map_tokens env) v2 in
      let v3 = (* "}" *) token env v3 in
      todo env (v1, v2, v3)
  )

and map_tokens (env : env) (x : CST.tokens) =
  (match x with
  | `Tok_tree x -> map_token_tree env x
  | `Tok_repe (v1, v2, v3, v4, v5, v6) ->
      let v1 = (* "$" *) token env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 = List.map (map_tokens env) v3 in
      let v4 = (* ")" *) token env v4 in
      let v5 =
        (match v5 with
        | Some tok -> (* pattern [^+*?]+ *) token env tok
        | None -> todo env ())
      in
      let v6 = map_anon_choice_PLUS_348fa54 env v6 in
      todo env (v1, v2, v3, v4, v5, v6)
  | `Choice_lit x -> map_non_special_token env x
  )

let rec map_token_pattern (env : env) (x : CST.token_pattern) =
  (match x with
  | `Tok_tree_pat x -> map_token_tree_pattern env x
  | `Tok_repe_pat (v1, v2, v3, v4, v5, v6) ->
      let v1 = (* "$" *) token env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 = List.map (map_token_pattern env) v3 in
      let v4 = (* ")" *) token env v4 in
      let v5 =
        (match v5 with
        | Some tok -> (* pattern [^+*?]+ *) token env tok
        | None -> todo env ())
      in
      let v6 = map_anon_choice_PLUS_348fa54 env v6 in
      todo env (v1, v2, v3, v4, v5, v6)
  | `Tok_bind_pat (v1, v2, v3) ->
      let v1 = (* pattern \$[a-zA-Z_]\w* *) token env v1 in
      let v2 = (* ":" *) token env v2 in
      let v3 = map_fragment_specifier env v3 in
      todo env (v1, v2, v3)
  | `Choice_lit x -> map_non_special_token env x
  )

and map_token_tree_pattern (env : env) (x : CST.token_tree_pattern) =
  (match x with
  | `LPAR_rep_tok_pat_RPAR (v1, v2, v3) ->
      let v1 = (* "(" *) token env v1 in
      let v2 = List.map (map_token_pattern env) v2 in
      let v3 = (* ")" *) token env v3 in
      todo env (v1, v2, v3)
  | `LBRACK_rep_tok_pat_RBRACK (v1, v2, v3) ->
      let v1 = (* "[" *) token env v1 in
      let v2 = List.map (map_token_pattern env) v2 in
      let v3 = (* "]" *) token env v3 in
      todo env (v1, v2, v3)
  | `LCURL_rep_tok_pat_RCURL (v1, v2, v3) ->
      let v1 = (* "{" *) token env v1 in
      let v2 = List.map (map_token_pattern env) v2 in
      let v3 = (* "}" *) token env v3 in
      todo env (v1, v2, v3)
  )

let map_macro_rule (env : env) ((v1, v2, v3) : CST.macro_rule) =
  let v1 = map_token_tree_pattern env v1 in
  let v2 = (* "=>" *) token env v2 in
  let v3 = map_token_tree env v3 in
  todo env (v1, v2, v3)

let rec map_anon_choice_SEMI_226cc40 (env : env) (x : CST.anon_choice_SEMI_226cc40) =
  (match x with
  | `SEMI tok -> (* ";" *) token env tok
  | `Decl_list x -> map_declaration_list env x
  )

and map_anon_choice_field_id_02b4436 (env : env) (x : CST.anon_choice_field_id_02b4436) =
  (match x with
  | `Id tok -> (* identifier *) token env tok
  | `Scoped_type_id x -> map_scoped_type_identifier env x
  | `Gene_type x -> map_generic_type env x
  | `Func_type x -> map_function_type env x
  )

and map_anon_choice_field_id_2c46bcf (env : env) (x : CST.anon_choice_field_id_2c46bcf) =
  (match x with
  | `Id tok -> (* identifier *) token env tok
  | `Scoped_type_id x -> map_scoped_type_identifier env x
  )

and map_anon_choice_field_id_f1f5a37 (env : env) (x : CST.anon_choice_field_id_f1f5a37) =
  (match x with
  | `Id tok -> (* identifier *) token env tok
  | `Scoped_id x -> map_scoped_identifier env x
  )

and map_anon_choice_field_pat_8e757e8 (env : env) (x : CST.anon_choice_field_pat_8e757e8) =
  (match x with
  | `Field_pat (v1, v2, v3) ->
      let v1 =
        (match v1 with
        | Some tok -> (* "ref" *) token env tok
        | None -> todo env ())
      in
      let v2 =
        (match v2 with
        | Some tok -> (* "mut" *) token env tok
        | None -> todo env ())
      in
      let v3 =
        (match v3 with
        | `Id tok -> (* identifier *) token env tok
        | `Id_COLON_pat (v1, v2, v3) ->
            let v1 = (* identifier *) token env v1 in
            let v2 = (* ":" *) token env v2 in
            let v3 = map_pattern env v3 in
            todo env (v1, v2, v3)
        )
      in
      todo env (v1, v2, v3)
  | `Rema_field_pat tok -> (* ".." *) token env tok
  )

and map_anon_choice_life_859e88f (env : env) (x : CST.anon_choice_life_859e88f) =
  (match x with
  | `Life x -> map_lifetime env x
  | `Meta tok -> (* pattern \$[a-zA-Z_]\w* *) token env tok
  | `Id tok -> (* identifier *) token env tok
  | `Cons_type_param x -> map_constrained_type_parameter env x
  | `Opt_type_param (v1, v2, v3) ->
      let v1 =
        (match v1 with
        | `Id tok -> (* identifier *) token env tok
        | `Cons_type_param x -> map_constrained_type_parameter env x
        )
      in
      let v2 = (* "=" *) token env v2 in
      let v3 = map_type_ env v3 in
      todo env (v1, v2, v3)
  | `Const_param (v1, v2, v3, v4) ->
      let v1 = (* "const" *) token env v1 in
      let v2 = (* identifier *) token env v2 in
      let v3 = (* ":" *) token env v3 in
      let v4 = map_type_ env v4 in
      todo env (v1, v2, v3, v4)
  )

and map_anon_choice_lit_pat_0884ef0 (env : env) (x : CST.anon_choice_lit_pat_0884ef0) =
  (match x with
  | `Lit_pat x -> map_literal_pattern env x
  | `Choice_self x -> map_path env x
  )

and map_anon_choice_meta_item_fefa160 (env : env) (x : CST.anon_choice_meta_item_fefa160) =
  (match x with
  | `Meta_item x -> map_meta_item env x
  | `Lit x -> map_literal env x
  )

and map_anon_choice_param_2c23cdc (env : env) (x : CST.anon_choice_param_2c23cdc) =
  (match x with
  | `Param x -> map_parameter env x
  | `Self_param (v1, v2, v3, v4) ->
      let v1 =
        (match v1 with
        | Some tok -> (* "&" *) token env tok
        | None -> todo env ())
      in
      let v2 =
        (match v2 with
        | Some x -> map_lifetime env x
        | None -> todo env ())
      in
      let v3 =
        (match v3 with
        | Some tok -> (* "mut" *) token env tok
        | None -> todo env ())
      in
      let v4 = (* "self" *) token env v4 in
      todo env (v1, v2, v3, v4)
  | `Vari_param tok -> (* "..." *) token env tok
  | `X__ tok -> (* "_" *) token env tok
  | `Type x -> map_type_ env x
  )

and map_anon_choice_pat_4717dcc (env : env) (x : CST.anon_choice_pat_4717dcc) =
  (match x with
  | `Pat x -> map_pattern env x
  | `Param x -> map_parameter env x
  )

and map_anon_choice_shor_field_init_9cb4441 (env : env) (x : CST.anon_choice_shor_field_init_9cb4441) =
  (match x with
  | `Shor_field_init (v1, v2) ->
      let v1 = List.map (map_attribute_item env) v1 in
      let v2 = (* identifier *) token env v2 in
      todo env (v1, v2)
  | `Field_init (v1, v2, v3, v4) ->
      let v1 = List.map (map_attribute_item env) v1 in
      let v2 = (* identifier *) token env v2 in
      let v3 = (* ":" *) token env v3 in
      let v4 = map_expression env v4 in
      todo env (v1, v2, v3, v4)
  | `Base_field_init x -> map_base_field_initializer env x
  )

and map_anon_choice_type_39799c3 (env : env) (x : CST.anon_choice_type_39799c3) =
  (match x with
  | `Type x -> map_type_ env x
  | `Type_bind (v1, v2, v3, v4) ->
      let v1 = (* identifier *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> map_type_arguments env x
        | None -> todo env ())
      in
      let v3 = (* "=" *) token env v3 in
      let v4 = map_type_ env v4 in
      todo env (v1, v2, v3, v4)
  | `Life x -> map_lifetime env x
  | `Lit x -> map_literal env x
  | `Blk x -> map_block env x
  )

and map_anon_choice_type_d689819 (env : env) (x : CST.anon_choice_type_d689819) =
  (match x with
  | `Type x -> map_type_ env x
  | `Life x -> map_lifetime env x
  | `Higher_ranked_trait_bound x ->
      map_higher_ranked_trait_bound env x
  | `Remo_trait_bound (v1, v2) ->
      let v1 = (* "?" *) token env v1 in
      let v2 = map_type_ env v2 in
      todo env (v1, v2)
  )

and map_anon_pat_rep_COMMA_pat_2a80f16 (env : env) ((v1, v2) : CST.anon_pat_rep_COMMA_pat_2a80f16) =
  let v1 = map_pattern env v1 in
  let v2 =
    List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_pattern env v2 in
      todo env (v1, v2)
    ) v2
  in
  todo env (v1, v2)

and map_arguments (env : env) ((v1, v2, v3, v4) : CST.arguments) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2, v3) ->
        let v1 = List.map (map_attribute_item env) v1 in
        let v2 = map_expression env v2 in
        let v3 =
          List.map (fun (v1, v2, v3) ->
            let v1 = (* "," *) token env v1 in
            let v2 = List.map (map_attribute_item env) v2 in
            let v3 = map_expression env v3 in
            todo env (v1, v2, v3)
          ) v3
        in
        todo env (v1, v2, v3)
    | None -> todo env ())
  in
  let v3 =
    (match v3 with
    | Some tok -> (* "," *) token env tok
    | None -> todo env ())
  in
  let v4 = (* ")" *) token env v4 in
  todo env (v1, v2, v3, v4)

and map_attribute_item (env : env) ((v1, v2, v3, v4) : CST.attribute_item) =
  let v1 = (* "#" *) token env v1 in
  let v2 = (* "[" *) token env v2 in
  let v3 = map_meta_item env v3 in
  let v4 = (* "]" *) token env v4 in
  todo env (v1, v2, v3, v4)

and map_base_field_initializer (env : env) ((v1, v2) : CST.base_field_initializer) =
  let v1 = (* ".." *) token env v1 in
  let v2 = map_expression env v2 in
  todo env (v1, v2)

and map_binary_expression (env : env) (x : CST.binary_expression) =
  (match x with
  | `Exp_AMPAMP_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "&&" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_BARBAR_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "||" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_AMP_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "&" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_BAR_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "|" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_HAT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "^" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_choice_EQEQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 =
        (match v2 with
        | `EQEQ tok -> (* "==" *) token env tok
        | `BANGEQ tok -> (* "!=" *) token env tok
        | `LT tok -> (* "<" *) token env tok
        | `LTEQ tok -> (* "<=" *) token env tok
        | `GT tok -> (* ">" *) token env tok
        | `GTEQ tok -> (* ">=" *) token env tok
        )
      in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_choice_LTLT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 =
        (match v2 with
        | `LTLT tok -> (* "<<" *) token env tok
        | `GTGT tok -> (* ">>" *) token env tok
        )
      in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_choice_PLUS_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 =
        (match v2 with
        | `PLUS tok -> (* "+" *) token env tok
        | `DASH tok -> (* "-" *) token env tok
        )
      in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_choice_STAR_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 =
        (match v2 with
        | `STAR tok -> (* "*" *) token env tok
        | `SLASH tok -> (* "/" *) token env tok
        | `PERC tok -> (* "%" *) token env tok
        )
      in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  )

and map_block (env : env) ((v1, v2, v3, v4) : CST.block) =
  let v1 = (* "{" *) token env v1 in
  let v2 = List.map (map_statement env) v2 in
  let v3 =
    (match v3 with
    | Some x -> map_expression env x
    | None -> todo env ())
  in
  let v4 = (* "}" *) token env v4 in
  todo env (v1, v2, v3, v4)

and map_bounded_type (env : env) (x : CST.bounded_type) =
  (match x with
  | `Life_PLUS_type (v1, v2, v3) ->
      let v1 = map_lifetime env v1 in
      let v2 = (* "+" *) token env v2 in
      let v3 = map_type_ env v3 in
      todo env (v1, v2, v3)
  | `Type_PLUS_type (v1, v2, v3) ->
      let v1 = map_type_ env v1 in
      let v2 = (* "+" *) token env v2 in
      let v3 = map_type_ env v3 in
      todo env (v1, v2, v3)
  | `Type_PLUS_life (v1, v2, v3) ->
      let v1 = map_type_ env v1 in
      let v2 = (* "+" *) token env v2 in
      let v3 = map_lifetime env v3 in
      todo env (v1, v2, v3)
  )

and map_bracketed_type (env : env) ((v1, v2, v3) : CST.bracketed_type) =
  let v1 = (* "<" *) token env v1 in
  let v2 =
    (match v2 with
    | `Type x -> map_type_ env x
    | `Qual_type x -> map_qualified_type env x
    )
  in
  let v3 = (* ">" *) token env v3 in
  todo env (v1, v2, v3)

and map_closure_parameters (env : env) ((v1, v2, v3) : CST.closure_parameters) =
  let v1 = (* "|" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) ->
        let v1 = map_anon_choice_pat_4717dcc env v1 in
        let v2 =
          List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_anon_choice_pat_4717dcc env v2 in
            todo env (v1, v2)
          ) v2
        in
        todo env (v1, v2)
    | None -> todo env ())
  in
  let v3 = (* "|" *) token env v3 in
  todo env (v1, v2, v3)

and map_const_block (env : env) ((v1, v2) : CST.const_block) =
  let v1 = (* "const" *) token env v1 in
  let v2 = map_block env v2 in
  todo env (v1, v2)

and map_constrained_type_parameter (env : env) ((v1, v2) : CST.constrained_type_parameter) =
  let v1 =
    (match v1 with
    | `Life x -> map_lifetime env x
    | `Id tok -> (* identifier *) token env tok
    )
  in
  let v2 = map_trait_bounds env v2 in
  todo env (v1, v2)

and map_declaration_list (env : env) ((v1, v2, v3) : CST.declaration_list) =
  let v1 = (* "{" *) token env v1 in
  let v2 = List.map (map_declaration_statement env) v2 in
  let v3 = (* "}" *) token env v3 in
  todo env (v1, v2, v3)

and map_declaration_statement (env : env) (x : CST.declaration_statement) =
  (match x with
  | `Const_item (v1, v2, v3, v4, v5, v6, v7) ->
      let v1 =
        (match v1 with
        | Some x -> map_visibility_modifier env x
        | None -> todo env ())
      in
      let v2 = (* "const" *) token env v2 in
      let v3 = (* identifier *) token env v3 in
      let v4 = (* ":" *) token env v4 in
      let v5 = map_type_ env v5 in
      let v6 =
        (match v6 with
        | Some (v1, v2) ->
            let v1 = (* "=" *) token env v1 in
            let v2 = map_expression env v2 in
            todo env (v1, v2)
        | None -> todo env ())
      in
      let v7 = (* ";" *) token env v7 in
      todo env (v1, v2, v3, v4, v5, v6, v7)
  | `Macro_invo x -> map_macro_invocation env x
  | `Macro_defi (v1, v2, v3) ->
      let v1 = (* "macro_rules!" *) token env v1 in
      let v2 =
        (match v2 with
        | `Id tok -> (* identifier *) token env tok
        | `Choice_defa x -> map_reserved_identifier env x
        )
      in
      let v3 =
        (match v3 with
        | `LPAR_rep_macro_rule_SEMI_opt_macro_rule_RPAR_SEMI (v1, v2, v3, v4, v5) ->
            let v1 = (* "(" *) token env v1 in
            let v2 =
              List.map (fun (v1, v2) ->
                let v1 = map_macro_rule env v1 in
                let v2 = (* ";" *) token env v2 in
                todo env (v1, v2)
              ) v2
            in
            let v3 =
              (match v3 with
              | Some x -> map_macro_rule env x
              | None -> todo env ())
            in
            let v4 = (* ")" *) token env v4 in
            let v5 = (* ";" *) token env v5 in
            todo env (v1, v2, v3, v4, v5)
        | `LCURL_rep_macro_rule_SEMI_opt_macro_rule_RCURL (v1, v2, v3, v4) ->
            let v1 = (* "{" *) token env v1 in
            let v2 =
              List.map (fun (v1, v2) ->
                let v1 = map_macro_rule env v1 in
                let v2 = (* ";" *) token env v2 in
                todo env (v1, v2)
              ) v2
            in
            let v3 =
              (match v3 with
              | Some x -> map_macro_rule env x
              | None -> todo env ())
            in
            let v4 = (* "}" *) token env v4 in
            todo env (v1, v2, v3, v4)
        )
      in
      todo env (v1, v2, v3)
  | `Empty_stmt tok -> (* ";" *) token env tok
  | `Attr_item x -> map_attribute_item env x
  | `Inner_attr_item (v1, v2, v3, v4, v5) ->
      let v1 = (* "#" *) token env v1 in
      let v2 = (* "!" *) token env v2 in
      let v3 = (* "[" *) token env v3 in
      let v4 = map_meta_item env v4 in
      let v5 = (* "]" *) token env v5 in
      todo env (v1, v2, v3, v4, v5)
  | `Mod_item (v1, v2, v3, v4) ->
      let v1 =
        (match v1 with
        | Some x -> map_visibility_modifier env x
        | None -> todo env ())
      in
      let v2 = (* "mod" *) token env v2 in
      let v3 = (* identifier *) token env v3 in
      let v4 = map_anon_choice_SEMI_226cc40 env v4 in
      todo env (v1, v2, v3, v4)
  | `Fore_mod_item (v1, v2, v3) ->
      let v1 =
        (match v1 with
        | Some x -> map_visibility_modifier env x
        | None -> todo env ())
      in
      let v2 = map_extern_modifier env v2 in
      let v3 = map_anon_choice_SEMI_226cc40 env v3 in
      todo env (v1, v2, v3)
  | `Struct_item (v1, v2, v3, v4, v5) ->
      let v1 =
        (match v1 with
        | Some x -> map_visibility_modifier env x
        | None -> todo env ())
      in
      let v2 = (* "struct" *) token env v2 in
      let v3 = (* identifier *) token env v3 in
      let v4 =
        (match v4 with
        | Some x -> map_type_parameters env x
        | None -> todo env ())
      in
      let v5 =
        (match v5 with
        | `Opt_where_clause_field_decl_list (v1, v2) ->
            let v1 =
              (match v1 with
              | Some x -> map_where_clause env x
              | None -> todo env ())
            in
            let v2 = map_field_declaration_list env v2 in
            todo env (v1, v2)
        | `Orde_field_decl_list_opt_where_clause_SEMI (v1, v2, v3) ->
            let v1 = map_ordered_field_declaration_list env v1 in
            let v2 =
              (match v2 with
              | Some x -> map_where_clause env x
              | None -> todo env ())
            in
            let v3 = (* ";" *) token env v3 in
            todo env (v1, v2, v3)
        | `SEMI tok -> (* ";" *) token env tok
        )
      in
      todo env (v1, v2, v3, v4, v5)
  | `Union_item (v1, v2, v3, v4, v5, v6) ->
      let v1 =
        (match v1 with
        | Some x -> map_visibility_modifier env x
        | None -> todo env ())
      in
      let v2 = (* "union" *) token env v2 in
      let v3 = (* identifier *) token env v3 in
      let v4 =
        (match v4 with
        | Some x -> map_type_parameters env x
        | None -> todo env ())
      in
      let v5 =
        (match v5 with
        | Some x -> map_where_clause env x
        | None -> todo env ())
      in
      let v6 = map_field_declaration_list env v6 in
      todo env (v1, v2, v3, v4, v5, v6)
  | `Enum_item (v1, v2, v3, v4, v5, v6) ->
      let v1 =
        (match v1 with
        | Some x -> map_visibility_modifier env x
        | None -> todo env ())
      in
      let v2 = (* "enum" *) token env v2 in
      let v3 = (* identifier *) token env v3 in
      let v4 =
        (match v4 with
        | Some x -> map_type_parameters env x
        | None -> todo env ())
      in
      let v5 =
        (match v5 with
        | Some x -> map_where_clause env x
        | None -> todo env ())
      in
      let v6 = map_enum_variant_list env v6 in
      todo env (v1, v2, v3, v4, v5, v6)
  | `Type_item (v1, v2, v3, v4, v5, v6, v7) ->
      let v1 =
        (match v1 with
        | Some x -> map_visibility_modifier env x
        | None -> todo env ())
      in
      let v2 = (* "type" *) token env v2 in
      let v3 = (* identifier *) token env v3 in
      let v4 =
        (match v4 with
        | Some x -> map_type_parameters env x
        | None -> todo env ())
      in
      let v5 = (* "=" *) token env v5 in
      let v6 = map_type_ env v6 in
      let v7 = (* ";" *) token env v7 in
      todo env (v1, v2, v3, v4, v5, v6, v7)
  | `Func_item (v1, v2, v3, v4, v5, v6, v7, v8, v9) ->
      let v1 =
        (match v1 with
        | Some x -> map_visibility_modifier env x
        | None -> todo env ())
      in
      let v2 =
        (match v2 with
        | Some x -> map_function_modifiers env x
        | None -> todo env ())
      in
      let v3 = (* "fn" *) token env v3 in
      let v4 = map_anon_choice_field_id_8184947 env v4 in
      let v5 =
        (match v5 with
        | Some x -> map_type_parameters env x
        | None -> todo env ())
      in
      let v6 = map_parameters env v6 in
      let v7 =
        (match v7 with
        | Some (v1, v2) ->
            let v1 = (* "->" *) token env v1 in
            let v2 = map_type_ env v2 in
            todo env (v1, v2)
        | None -> todo env ())
      in
      let v8 =
        (match v8 with
        | Some x -> map_where_clause env x
        | None -> todo env ())
      in
      let v9 = map_block env v9 in
      todo env (v1, v2, v3, v4, v5, v6, v7, v8, v9)
  | `Func_sign_item (v1, v2, v3, v4, v5, v6, v7, v8, v9) ->
      let v1 =
        (match v1 with
        | Some x -> map_visibility_modifier env x
        | None -> todo env ())
      in
      let v2 =
        (match v2 with
        | Some x -> map_function_modifiers env x
        | None -> todo env ())
      in
      let v3 = (* "fn" *) token env v3 in
      let v4 = map_anon_choice_field_id_8184947 env v4 in
      let v5 =
        (match v5 with
        | Some x -> map_type_parameters env x
        | None -> todo env ())
      in
      let v6 = map_parameters env v6 in
      let v7 =
        (match v7 with
        | Some (v1, v2) ->
            let v1 = (* "->" *) token env v1 in
            let v2 = map_type_ env v2 in
            todo env (v1, v2)
        | None -> todo env ())
      in
      let v8 =
        (match v8 with
        | Some x -> map_where_clause env x
        | None -> todo env ())
      in
      let v9 = (* ";" *) token env v9 in
      todo env (v1, v2, v3, v4, v5, v6, v7, v8, v9)
  | `Impl_item (v1, v2, v3, v4, v5, v6, v7) ->
      let v1 =
        (match v1 with
        | Some tok -> (* "unsafe" *) token env tok
        | None -> todo env ())
      in
      let v2 = (* "impl" *) token env v2 in
      let v3 =
        (match v3 with
        | Some x -> map_type_parameters env x
        | None -> todo env ())
      in
      let v4 =
        (match v4 with
        | Some (v1, v2) ->
            let v1 =
              (match v1 with
              | `Id tok -> (* identifier *) token env tok
              | `Scoped_type_id x -> map_scoped_type_identifier env x
              | `Gene_type x -> map_generic_type env x
              )
            in
            let v2 = (* "for" *) token env v2 in
            todo env (v1, v2)
        | None -> todo env ())
      in
      let v5 = map_type_ env v5 in
      let v6 =
        (match v6 with
        | Some x -> map_where_clause env x
        | None -> todo env ())
      in
      let v7 =
        (match v7 with
        | `Decl_list x -> map_declaration_list env x
        | `SEMI tok -> (* ";" *) token env tok
        )
      in
      todo env (v1, v2, v3, v4, v5, v6, v7)
  | `Trait_item (v1, v2, v3, v4, v5, v6, v7, v8) ->
      let v1 =
        (match v1 with
        | Some x -> map_visibility_modifier env x
        | None -> todo env ())
      in
      let v2 =
        (match v2 with
        | Some tok -> (* "unsafe" *) token env tok
        | None -> todo env ())
      in
      let v3 = (* "trait" *) token env v3 in
      let v4 = (* identifier *) token env v4 in
      let v5 =
        (match v5 with
        | Some x -> map_type_parameters env x
        | None -> todo env ())
      in
      let v6 =
        (match v6 with
        | Some x -> map_trait_bounds env x
        | None -> todo env ())
      in
      let v7 =
        (match v7 with
        | Some x -> map_where_clause env x
        | None -> todo env ())
      in
      let v8 = map_declaration_list env v8 in
      todo env (v1, v2, v3, v4, v5, v6, v7, v8)
  | `Asso_type (v1, v2, v3, v4, v5) ->
      let v1 = (* "type" *) token env v1 in
      let v2 = (* identifier *) token env v2 in
      let v3 =
        (match v3 with
        | Some x -> map_type_parameters env x
        | None -> todo env ())
      in
      let v4 =
        (match v4 with
        | Some x -> map_trait_bounds env x
        | None -> todo env ())
      in
      let v5 = (* ";" *) token env v5 in
      todo env (v1, v2, v3, v4, v5)
  | `Let_decl (v1, v2, v3, v4, v5, v6) ->
      let v1 = (* "let" *) token env v1 in
      let v2 =
        (match v2 with
        | Some tok -> (* "mut" *) token env tok
        | None -> todo env ())
      in
      let v3 = map_pattern env v3 in
      let v4 =
        (match v4 with
        | Some (v1, v2) ->
            let v1 = (* ":" *) token env v1 in
            let v2 = map_type_ env v2 in
            todo env (v1, v2)
        | None -> todo env ())
      in
      let v5 =
        (match v5 with
        | Some (v1, v2) ->
            let v1 = (* "=" *) token env v1 in
            let v2 = map_expression env v2 in
            todo env (v1, v2)
        | None -> todo env ())
      in
      let v6 = (* ";" *) token env v6 in
      todo env (v1, v2, v3, v4, v5, v6)
  | `Use_decl (v1, v2, v3, v4) ->
      let v1 =
        (match v1 with
        | Some x -> map_visibility_modifier env x
        | None -> todo env ())
      in
      let v2 = (* "use" *) token env v2 in
      let v3 = map_use_clause env v3 in
      let v4 = (* ";" *) token env v4 in
      todo env (v1, v2, v3, v4)
  | `Extern_crate_decl (v1, v2, v3, v4, v5, v6) ->
      let v1 =
        (match v1 with
        | Some x -> map_visibility_modifier env x
        | None -> todo env ())
      in
      let v2 = (* "extern" *) token env v2 in
      let v3 = (* "crate" *) token env v3 in
      let v4 = (* identifier *) token env v4 in
      let v5 =
        (match v5 with
        | Some (v1, v2) ->
            let v1 = (* "as" *) token env v1 in
            let v2 = (* identifier *) token env v2 in
            todo env (v1, v2)
        | None -> todo env ())
      in
      let v6 = (* ";" *) token env v6 in
      todo env (v1, v2, v3, v4, v5, v6)
  | `Static_item (v1, v2, v3, v4, v5, v6, v7, v8, v9) ->
      let v1 =
        (match v1 with
        | Some x -> map_visibility_modifier env x
        | None -> todo env ())
      in
      let v2 = (* "static" *) token env v2 in
      let v3 =
        (match v3 with
        | Some tok -> (* "ref" *) token env tok
        | None -> todo env ())
      in
      let v4 =
        (match v4 with
        | Some tok -> (* "mut" *) token env tok
        | None -> todo env ())
      in
      let v5 = (* identifier *) token env v5 in
      let v6 = (* ":" *) token env v6 in
      let v7 = map_type_ env v7 in
      let v8 =
        (match v8 with
        | Some (v1, v2) ->
            let v1 = (* "=" *) token env v1 in
            let v2 = map_expression env v2 in
            todo env (v1, v2)
        | None -> todo env ())
      in
      let v9 = (* ";" *) token env v9 in
      todo env (v1, v2, v3, v4, v5, v6, v7, v8, v9)
  )

and map_else_clause (env : env) ((v1, v2) : CST.else_clause) =
  let v1 = (* "else" *) token env v1 in
  let v2 =
    (match v2 with
    | `Blk x -> map_block env x
    | `If_exp x -> map_if_expression env x
    | `If_let_exp x -> map_if_let_expression env x
    )
  in
  todo env (v1, v2)

and map_enum_variant (env : env) ((v1, v2, v3, v4) : CST.enum_variant) =
  let v1 =
    (match v1 with
    | Some x -> map_visibility_modifier env x
    | None -> todo env ())
  in
  let v2 = (* identifier *) token env v2 in
  let v3 =
    (match v3 with
    | Some x ->
        (match x with
        | `Field_decl_list x -> map_field_declaration_list env x
        | `Orde_field_decl_list x ->
            map_ordered_field_declaration_list env x
        )
    | None -> todo env ())
  in
  let v4 =
    (match v4 with
    | Some (v1, v2) ->
        let v1 = (* "=" *) token env v1 in
        let v2 = map_expression env v2 in
        todo env (v1, v2)
    | None -> todo env ())
  in
  todo env (v1, v2, v3, v4)

and map_enum_variant_list (env : env) ((v1, v2, v3, v4) : CST.enum_variant_list) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2, v3) ->
        let v1 = List.map (map_attribute_item env) v1 in
        let v2 = map_enum_variant env v2 in
        let v3 =
          List.map (fun (v1, v2, v3) ->
            let v1 = (* "," *) token env v1 in
            let v2 = List.map (map_attribute_item env) v2 in
            let v3 = map_enum_variant env v3 in
            todo env (v1, v2, v3)
          ) v3
        in
        todo env (v1, v2, v3)
    | None -> todo env ())
  in
  let v3 =
    (match v3 with
    | Some tok -> (* "," *) token env tok
    | None -> todo env ())
  in
  let v4 = (* "}" *) token env v4 in
  todo env (v1, v2, v3, v4)

and map_expression (env : env) (x : CST.expression) =
  (match x with
  | `Un_exp (v1, v2) ->
      let v1 =
        (match v1 with
        | `DASH tok -> (* "-" *) token env tok
        | `STAR tok -> (* "*" *) token env tok
        | `BANG tok -> (* "!" *) token env tok
        )
      in
      let v2 = map_expression env v2 in
      todo env (v1, v2)
  | `Ref_exp (v1, v2, v3) ->
      let v1 = (* "&" *) token env v1 in
      let v2 =
        (match v2 with
        | Some tok -> (* "mut" *) token env tok
        | None -> todo env ())
      in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Try_exp (v1, v2) ->
      let v1 = map_expression env v1 in
      let v2 = (* "?" *) token env v2 in
      todo env (v1, v2)
  | `Bin_exp x -> map_binary_expression env x
  | `Assign_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "=" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Comp_assign_expr (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 =
        (match v2 with
        | `PLUSEQ tok -> (* "+=" *) token env tok
        | `DASHEQ tok -> (* "-=" *) token env tok
        | `STAREQ tok -> (* "*=" *) token env tok
        | `SLASHEQ tok -> (* "/=" *) token env tok
        | `PERCEQ tok -> (* "%=" *) token env tok
        | `AMPEQ tok -> (* "&=" *) token env tok
        | `BAREQ tok -> (* "|=" *) token env tok
        | `HATEQ tok -> (* "^=" *) token env tok
        | `LTLTEQ tok -> (* "<<=" *) token env tok
        | `GTGTEQ tok -> (* ">>=" *) token env tok
        )
      in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Type_cast_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "as" *) token env v2 in
      let v3 = map_type_ env v3 in
      todo env (v1, v2, v3)
  | `Range_exp x -> map_range_expression env x
  | `Call_exp (v1, v2) ->
      let v1 = map_expression env v1 in
      let v2 = map_arguments env v2 in
      todo env (v1, v2)
  | `Ret_exp x -> map_return_expression env x
  | `Lit x -> map_literal env x
  | `Id tok -> (* identifier *) token env tok
  | `Choice_u8 x -> map_anon_choice_u8_6dad923 env x
  | `Choice_defa x -> map_reserved_identifier env x
  | `Self tok -> (* "self" *) token env tok
  | `Scoped_id x -> map_scoped_identifier env x
  | `Gene_func (v1, v2, v3) ->
      let v1 =
        (match v1 with
        | `Id tok -> (* identifier *) token env tok
        | `Scoped_id x -> map_scoped_identifier env x
        | `Field_exp x -> map_field_expression env x
        )
      in
      let v2 = (* "::" *) token env v2 in
      let v3 = map_type_arguments env v3 in
      todo env (v1, v2, v3)
  | `Await_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "." *) token env v2 in
      let v3 = (* "await" *) token env v3 in
      todo env (v1, v2, v3)
  | `Field_exp x -> map_field_expression env x
  | `Array_exp (v1, v2, v3, v4) ->
      let v1 = (* "[" *) token env v1 in
      let v2 = List.map (map_attribute_item env) v2 in
      let v3 =
        (match v3 with
        | `Exp_SEMI_exp (v1, v2, v3) ->
            let v1 = map_expression env v1 in
            let v2 = (* ";" *) token env v2 in
            let v3 = map_expression env v3 in
            todo env (v1, v2, v3)
        | `Opt_exp_rep_COMMA_exp_opt_COMMA (v1, v2) ->
            let v1 =
              (match v1 with
              | Some (v1, v2) ->
                  let v1 = map_expression env v1 in
                  let v2 =
                    List.map (fun (v1, v2) ->
                      let v1 = (* "," *) token env v1 in
                      let v2 = map_expression env v2 in
                      todo env (v1, v2)
                    ) v2
                  in
                  todo env (v1, v2)
              | None -> todo env ())
            in
            let v2 =
              (match v2 with
              | Some tok -> (* "," *) token env tok
              | None -> todo env ())
            in
            todo env (v1, v2)
        )
      in
      let v4 = (* "]" *) token env v4 in
      todo env (v1, v2, v3, v4)
  | `Tuple_exp (v1, v2, v3, v4, v5, v6, v7) ->
      let v1 = (* "(" *) token env v1 in
      let v2 = List.map (map_attribute_item env) v2 in
      let v3 = map_expression env v3 in
      let v4 = (* "," *) token env v4 in
      let v5 =
        List.map (fun (v1, v2) ->
          let v1 = map_expression env v1 in
          let v2 = (* "," *) token env v2 in
          todo env (v1, v2)
        ) v5
      in
      let v6 =
        (match v6 with
        | Some x -> map_expression env x
        | None -> todo env ())
      in
      let v7 = (* ")" *) token env v7 in
      todo env (v1, v2, v3, v4, v5, v6, v7)
  | `Macro_invo x -> map_macro_invocation env x
  | `Unit_exp (v1, v2) ->
      let v1 = (* "(" *) token env v1 in
      let v2 = (* ")" *) token env v2 in
      todo env (v1, v2)
  | `Choice_unsafe_blk x ->
      map_expression_ending_with_block env x
  | `Brk_exp (v1, v2, v3) ->
      let v1 = (* "break" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> map_loop_label env x
        | None -> todo env ())
      in
      let v3 =
        (match v3 with
        | Some x -> map_expression env x
        | None -> todo env ())
      in
      todo env (v1, v2, v3)
  | `Cont_exp (v1, v2) ->
      let v1 = (* "continue" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> map_loop_label env x
        | None -> todo env ())
      in
      todo env (v1, v2)
  | `Index_exp (v1, v2, v3, v4) ->
      let v1 = map_expression env v1 in
      let v2 = (* "[" *) token env v2 in
      let v3 = map_expression env v3 in
      let v4 = (* "]" *) token env v4 in
      todo env (v1, v2, v3, v4)
  | `Meta tok -> (* pattern \$[a-zA-Z_]\w* *) token env tok
  | `Clos_exp (v1, v2, v3) ->
      let v1 =
        (match v1 with
        | Some tok -> (* "move" *) token env tok
        | None -> todo env ())
      in
      let v2 = map_closure_parameters env v2 in
      let v3 =
        (match v3 with
        | `Opt_DASHGT_type_blk (v1, v2) ->
            let v1 =
              (match v1 with
              | Some (v1, v2) ->
                  let v1 = (* "->" *) token env v1 in
                  let v2 = map_type_ env v2 in
                  todo env (v1, v2)
              | None -> todo env ())
            in
            let v2 = map_block env v2 in
            todo env (v1, v2)
        | `Exp x -> map_expression env x
        )
      in
      todo env (v1, v2, v3)
  | `Paren_exp (v1, v2, v3) ->
      let v1 = (* "(" *) token env v1 in
      let v2 = map_expression env v2 in
      let v3 = (* ")" *) token env v3 in
      todo env (v1, v2, v3)
  | `Struct_exp (v1, v2) ->
      let v1 =
        (match v1 with
        | `Id tok -> (* identifier *) token env tok
        | `Scoped_type_id_in_exp_posi x ->
            map_scoped_type_identifier_in_expression_position env x
        | `Gene_type_with_turb x ->
            map_generic_type_with_turbofish env x
        )
      in
      let v2 = map_field_initializer_list env v2 in
      todo env (v1, v2)
  | `Ellips tok -> (* "..." *) token env tok
  | `Deep_ellips (v1, v2, v3) ->
      let v1 = (* "<..." *) token env v1 in
      let v2 = map_expression env v2 in
      let v3 = (* "...>" *) token env v3 in
      todo env (v1, v2, v3)
  )

and map_expression_ending_with_block (env : env) (x : CST.expression_ending_with_block) =
  (match x with
  | `Unsafe_blk (v1, v2) ->
      let v1 = (* "unsafe" *) token env v1 in
      let v2 = map_block env v2 in
      todo env (v1, v2)
  | `Async_blk (v1, v2, v3) ->
      let v1 = (* "async" *) token env v1 in
      let v2 =
        (match v2 with
        | Some tok -> (* "move" *) token env tok
        | None -> todo env ())
      in
      let v3 = map_block env v3 in
      todo env (v1, v2, v3)
  | `Blk x -> map_block env x
  | `If_exp x -> map_if_expression env x
  | `If_let_exp x -> map_if_let_expression env x
  | `Match_exp (v1, v2, v3) ->
      let v1 = (* "match" *) token env v1 in
      let v2 = map_expression env v2 in
      let v3 = map_match_block env v3 in
      todo env (v1, v2, v3)
  | `While_exp (v1, v2, v3, v4) ->
      let v1 =
        (match v1 with
        | Some (v1, v2) ->
            let v1 = map_loop_label env v1 in
            let v2 = (* ":" *) token env v2 in
            todo env (v1, v2)
        | None -> todo env ())
      in
      let v2 = (* "while" *) token env v2 in
      let v3 = map_expression env v3 in
      let v4 = map_block env v4 in
      todo env (v1, v2, v3, v4)
  | `While_let_exp (v1, v2, v3, v4, v5, v6, v7) ->
      let v1 =
        (match v1 with
        | Some (v1, v2) ->
            let v1 = map_loop_label env v1 in
            let v2 = (* ":" *) token env v2 in
            todo env (v1, v2)
        | None -> todo env ())
      in
      let v2 = (* "while" *) token env v2 in
      let v3 = (* "let" *) token env v3 in
      let v4 = map_pattern env v4 in
      let v5 = (* "=" *) token env v5 in
      let v6 = map_expression env v6 in
      let v7 = map_block env v7 in
      todo env (v1, v2, v3, v4, v5, v6, v7)
  | `Loop_exp (v1, v2, v3) ->
      let v1 =
        (match v1 with
        | Some (v1, v2) ->
            let v1 = map_loop_label env v1 in
            let v2 = (* ":" *) token env v2 in
            todo env (v1, v2)
        | None -> todo env ())
      in
      let v2 = (* "loop" *) token env v2 in
      let v3 = map_block env v3 in
      todo env (v1, v2, v3)
  | `For_exp (v1, v2, v3, v4, v5, v6) ->
      let v1 =
        (match v1 with
        | Some (v1, v2) ->
            let v1 = map_loop_label env v1 in
            let v2 = (* ":" *) token env v2 in
            todo env (v1, v2)
        | None -> todo env ())
      in
      let v2 = (* "for" *) token env v2 in
      let v3 = map_pattern env v3 in
      let v4 = (* "in" *) token env v4 in
      let v5 = map_expression env v5 in
      let v6 = map_block env v6 in
      todo env (v1, v2, v3, v4, v5, v6)
  | `Const_blk x -> map_const_block env x
  )

and map_expression_statement (env : env) (x : CST.expression_statement) =
  (match x with
  | `Choice_exp_SEMI x ->
      (match x with
      | `Exp_SEMI (v1, v2) ->
          let v1 = map_expression env v1 in
          let v2 = (* ";" *) token env v2 in
          todo env (v1, v2)
      | `Choice_unsafe_blk x ->
          map_expression_ending_with_block env x
      )
  | `Ellips_SEMI (v1, v2) ->
      let v1 = (* "..." *) token env v1 in
      let v2 = (* ";" *) token env v2 in
      todo env (v1, v2)
  | `Ellips tok -> (* "..." *) token env tok
  )

and map_field_declaration (env : env) ((v1, v2, v3, v4) : CST.field_declaration) =
  let v1 =
    (match v1 with
    | Some x -> map_visibility_modifier env x
    | None -> todo env ())
  in
  let v2 = (* identifier *) token env v2 in
  let v3 = (* ":" *) token env v3 in
  let v4 = map_type_ env v4 in
  todo env (v1, v2, v3, v4)

and map_field_declaration_list (env : env) ((v1, v2, v3, v4) : CST.field_declaration_list) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2, v3) ->
        let v1 = List.map (map_attribute_item env) v1 in
        let v2 = map_field_declaration env v2 in
        let v3 =
          List.map (fun (v1, v2, v3) ->
            let v1 = (* "," *) token env v1 in
            let v2 = List.map (map_attribute_item env) v2 in
            let v3 = map_field_declaration env v3 in
            todo env (v1, v2, v3)
          ) v3
        in
        todo env (v1, v2, v3)
    | None -> todo env ())
  in
  let v3 =
    (match v3 with
    | Some tok -> (* "," *) token env tok
    | None -> todo env ())
  in
  let v4 = (* "}" *) token env v4 in
  todo env (v1, v2, v3, v4)

and map_field_expression (env : env) ((v1, v2, v3) : CST.field_expression) =
  let v1 = map_expression env v1 in
  let v2 = (* "." *) token env v2 in
  let v3 =
    (match v3 with
    | `Id tok -> (* identifier *) token env tok
    | `Int_lit tok -> (* integer_literal *) token env tok
    )
  in
  todo env (v1, v2, v3)

and map_field_initializer_list (env : env) ((v1, v2, v3, v4) : CST.field_initializer_list) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) ->
        let v1 = map_anon_choice_shor_field_init_9cb4441 env v1 in
        let v2 =
          List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_anon_choice_shor_field_init_9cb4441 env v2 in
            todo env (v1, v2)
          ) v2
        in
        todo env (v1, v2)
    | None -> todo env ())
  in
  let v3 =
    (match v3 with
    | Some tok -> (* "," *) token env tok
    | None -> todo env ())
  in
  let v4 = (* "}" *) token env v4 in
  todo env (v1, v2, v3, v4)

and map_function_type (env : env) ((v1, v2, v3, v4) : CST.function_type) =
  let v1 =
    (match v1 with
    | Some x -> map_for_lifetimes env x
    | None -> todo env ())
  in
  let v2 =
    (match v2 with
    | `Choice_id x -> map_anon_choice_field_id_2c46bcf env x
    | `Opt_func_modifs_fn (v1, v2) ->
        let v1 =
          (match v1 with
          | Some x -> map_function_modifiers env x
          | None -> todo env ())
        in
        let v2 = (* "fn" *) token env v2 in
        todo env (v1, v2)
    )
  in
  let v3 = map_parameters env v3 in
  let v4 =
    (match v4 with
    | Some (v1, v2) ->
        let v1 = (* "->" *) token env v1 in
        let v2 = map_type_ env v2 in
        todo env (v1, v2)
    | None -> todo env ())
  in
  todo env (v1, v2, v3, v4)

and map_generic_type (env : env) ((v1, v2) : CST.generic_type) =
  let v1 = map_anon_choice_field_id_2c46bcf env v1 in
  let v2 = map_type_arguments env v2 in
  todo env (v1, v2)

and map_generic_type_with_turbofish (env : env) ((v1, v2, v3) : CST.generic_type_with_turbofish) =
  let v1 = map_anon_choice_field_id_f1f5a37 env v1 in
  let v2 = (* "::" *) token env v2 in
  let v3 = map_type_arguments env v3 in
  todo env (v1, v2, v3)

and map_higher_ranked_trait_bound (env : env) ((v1, v2, v3) : CST.higher_ranked_trait_bound) =
  let v1 = (* "for" *) token env v1 in
  let v2 = map_type_parameters env v2 in
  let v3 = map_type_ env v3 in
  todo env (v1, v2, v3)

and map_if_expression (env : env) ((v1, v2, v3, v4) : CST.if_expression) =
  let v1 = (* "if" *) token env v1 in
  let v2 = map_expression env v2 in
  let v3 = map_block env v3 in
  let v4 =
    (match v4 with
    | Some x -> map_else_clause env x
    | None -> todo env ())
  in
  todo env (v1, v2, v3, v4)

and map_if_let_expression (env : env) ((v1, v2, v3, v4, v5, v6, v7) : CST.if_let_expression) =
  let v1 = (* "if" *) token env v1 in
  let v2 = (* "let" *) token env v2 in
  let v3 = map_pattern env v3 in
  let v4 = (* "=" *) token env v4 in
  let v5 = map_expression env v5 in
  let v6 = map_block env v6 in
  let v7 =
    (match v7 with
    | Some x -> map_else_clause env x
    | None -> todo env ())
  in
  todo env (v1, v2, v3, v4, v5, v6, v7)

and map_last_match_arm (env : env) ((v1, v2, v3, v4, v5) : CST.last_match_arm) =
  let v1 = List.map (map_attribute_item env) v1 in
  let v2 = map_match_pattern env v2 in
  let v3 = (* "=>" *) token env v3 in
  let v4 = map_expression env v4 in
  let v5 =
    (match v5 with
    | Some tok -> (* "," *) token env tok
    | None -> todo env ())
  in
  todo env (v1, v2, v3, v4, v5)

and map_macro_invocation (env : env) ((v1, v2, v3) : CST.macro_invocation) =
  let v1 =
    (match v1 with
    | `Scoped_id x -> map_scoped_identifier env x
    | `Id tok -> (* identifier *) token env tok
    | `Choice_defa x -> map_reserved_identifier env x
    )
  in
  let v2 = (* "!" *) token env v2 in
  let v3 = map_token_tree env v3 in
  todo env (v1, v2, v3)

and map_match_arm (env : env) ((v1, v2, v3, v4) : CST.match_arm) =
  let v1 = List.map (map_attribute_item env) v1 in
  let v2 =
    (match v2 with
    | `Macro_invo x -> map_macro_invocation env x
    | `Match_pat x -> map_match_pattern env x
    )
  in
  let v3 = (* "=>" *) token env v3 in
  let v4 =
    (match v4 with
    | `Exp_COMMA (v1, v2) ->
        let v1 = map_expression env v1 in
        let v2 = (* "," *) token env v2 in
        todo env (v1, v2)
    | `Choice_unsafe_blk x ->
        map_expression_ending_with_block env x
    )
  in
  todo env (v1, v2, v3, v4)

and map_match_block (env : env) ((v1, v2, v3) : CST.match_block) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) ->
        let v1 = List.map (map_match_arm env) v1 in
        let v2 = map_last_match_arm env v2 in
        todo env (v1, v2)
    | None -> todo env ())
  in
  let v3 = (* "}" *) token env v3 in
  todo env (v1, v2, v3)

and map_match_pattern (env : env) ((v1, v2) : CST.match_pattern) =
  let v1 = map_pattern env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) ->
        let v1 = (* "if" *) token env v1 in
        let v2 = map_expression env v2 in
        todo env (v1, v2)
    | None -> todo env ())
  in
  todo env (v1, v2)

and map_meta_arguments (env : env) ((v1, v2, v3, v4) : CST.meta_arguments) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) ->
        let v1 = map_anon_choice_meta_item_fefa160 env v1 in
        let v2 =
          List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_anon_choice_meta_item_fefa160 env v2 in
            todo env (v1, v2)
          ) v2
        in
        todo env (v1, v2)
    | None -> todo env ())
  in
  let v3 =
    (match v3 with
    | Some tok -> (* "," *) token env tok
    | None -> todo env ())
  in
  let v4 = (* ")" *) token env v4 in
  todo env (v1, v2, v3, v4)

and map_meta_item (env : env) ((v1, v2) : CST.meta_item) =
  let v1 = map_path env v1 in
  let v2 =
    (match v2 with
    | Some x ->
        (match x with
        | `EQ_lit (v1, v2) ->
            let v1 = (* "=" *) token env v1 in
            let v2 = map_literal env v2 in
            todo env (v1, v2)
        | `Meta_args x -> map_meta_arguments env x
        )
    | None -> todo env ())
  in
  todo env (v1, v2)

and map_ordered_field_declaration_list (env : env) ((v1, v2, v3, v4) : CST.ordered_field_declaration_list) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2, v3, v4) ->
        let v1 = List.map (map_attribute_item env) v1 in
        let v2 =
          (match v2 with
          | Some x -> map_visibility_modifier env x
          | None -> todo env ())
        in
        let v3 = map_type_ env v3 in
        let v4 =
          List.map (fun (v1, v2, v3, v4) ->
            let v1 = (* "," *) token env v1 in
            let v2 = List.map (map_attribute_item env) v2 in
            let v3 =
              (match v3 with
              | Some x -> map_visibility_modifier env x
              | None -> todo env ())
            in
            let v4 = map_type_ env v4 in
            todo env (v1, v2, v3, v4)
          ) v4
        in
        todo env (v1, v2, v3, v4)
    | None -> todo env ())
  in
  let v3 =
    (match v3 with
    | Some tok -> (* "," *) token env tok
    | None -> todo env ())
  in
  let v4 = (* ")" *) token env v4 in
  todo env (v1, v2, v3, v4)

and map_parameter (env : env) ((v1, v2, v3, v4) : CST.parameter) =
  let v1 =
    (match v1 with
    | Some tok -> (* "mut" *) token env tok
    | None -> todo env ())
  in
  let v2 =
    (match v2 with
    | `Pat x -> map_pattern env x
    | `Self tok -> (* "self" *) token env tok
    | `Choice_defa x -> map_reserved_identifier env x
    )
  in
  let v3 = (* ":" *) token env v3 in
  let v4 = map_type_ env v4 in
  todo env (v1, v2, v3, v4)

and map_parameters (env : env) ((v1, v2, v3, v4) : CST.parameters) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2, v3) ->
        let v1 =
          (match v1 with
          | Some x -> map_attribute_item env x
          | None -> todo env ())
        in
        let v2 = map_anon_choice_param_2c23cdc env v2 in
        let v3 =
          List.map (fun (v1, v2, v3) ->
            let v1 = (* "," *) token env v1 in
            let v2 =
              (match v2 with
              | Some x -> map_attribute_item env x
              | None -> todo env ())
            in
            let v3 = map_anon_choice_param_2c23cdc env v3 in
            todo env (v1, v2, v3)
          ) v3
        in
        todo env (v1, v2, v3)
    | None -> todo env ())
  in
  let v3 =
    (match v3 with
    | Some tok -> (* "," *) token env tok
    | None -> todo env ())
  in
  let v4 = (* ")" *) token env v4 in
  todo env (v1, v2, v3, v4)

and map_path (env : env) (x : CST.path) =
  (match x with
  | `Self tok -> (* "self" *) token env tok
  | `Choice_u8 x -> map_anon_choice_u8_6dad923 env x
  | `Meta tok -> (* pattern \$[a-zA-Z_]\w* *) token env tok
  | `Super tok -> (* "super" *) token env tok
  | `Crate tok -> (* "crate" *) token env tok
  | `Id tok -> (* identifier *) token env tok
  | `Scoped_id x -> map_scoped_identifier env x
  | `Choice_defa x -> map_reserved_identifier env x
  )

and map_pattern (env : env) (x : CST.pattern) =
  (match x with
  | `Lit_pat x -> map_literal_pattern env x
  | `Choice_u8 x -> map_anon_choice_u8_6dad923 env x
  | `Id tok -> (* identifier *) token env tok
  | `Scoped_id x -> map_scoped_identifier env x
  | `Tuple_pat (v1, v2, v3, v4) ->
      let v1 = (* "(" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> map_anon_pat_rep_COMMA_pat_2a80f16 env x
        | None -> todo env ())
      in
      let v3 =
        (match v3 with
        | Some tok -> (* "," *) token env tok
        | None -> todo env ())
      in
      let v4 = (* ")" *) token env v4 in
      todo env (v1, v2, v3, v4)
  | `Tuple_struct_pat (v1, v2, v3, v4, v5) ->
      let v1 = map_anon_choice_field_id_f1f5a37 env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 =
        (match v3 with
        | Some x -> map_anon_pat_rep_COMMA_pat_2a80f16 env x
        | None -> todo env ())
      in
      let v4 =
        (match v4 with
        | Some tok -> (* "," *) token env tok
        | None -> todo env ())
      in
      let v5 = (* ")" *) token env v5 in
      todo env (v1, v2, v3, v4, v5)
  | `Struct_pat (v1, v2, v3, v4, v5) ->
      let v1 = map_anon_choice_field_id_2c46bcf env v1 in
      let v2 = (* "{" *) token env v2 in
      let v3 =
        (match v3 with
        | Some (v1, v2) ->
            let v1 = map_anon_choice_field_pat_8e757e8 env v1 in
            let v2 =
              List.map (fun (v1, v2) ->
                let v1 = (* "," *) token env v1 in
                let v2 = map_anon_choice_field_pat_8e757e8 env v2 in
                todo env (v1, v2)
              ) v2
            in
            todo env (v1, v2)
        | None -> todo env ())
      in
      let v4 =
        (match v4 with
        | Some tok -> (* "," *) token env tok
        | None -> todo env ())
      in
      let v5 = (* "}" *) token env v5 in
      todo env (v1, v2, v3, v4, v5)
  | `Ref_pat_a3d7f54 (v1, v2) ->
      let v1 = (* "ref" *) token env v1 in
      let v2 = map_pattern env v2 in
      todo env (v1, v2)
  | `Slice_pat (v1, v2, v3, v4) ->
      let v1 = (* "[" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> map_anon_pat_rep_COMMA_pat_2a80f16 env x
        | None -> todo env ())
      in
      let v3 =
        (match v3 with
        | Some tok -> (* "," *) token env tok
        | None -> todo env ())
      in
      let v4 = (* "]" *) token env v4 in
      todo env (v1, v2, v3, v4)
  | `Capt_pat (v1, v2, v3) ->
      let v1 = (* identifier *) token env v1 in
      let v2 = (* "@" *) token env v2 in
      let v3 = map_pattern env v3 in
      todo env (v1, v2, v3)
  | `Ref_pat_dbbcf07 (v1, v2, v3) ->
      let v1 = (* "&" *) token env v1 in
      let v2 =
        (match v2 with
        | Some tok -> (* "mut" *) token env tok
        | None -> todo env ())
      in
      let v3 = map_pattern env v3 in
      todo env (v1, v2, v3)
  | `Rema_field_pat tok -> (* ".." *) token env tok
  | `Mut_pat (v1, v2) ->
      let v1 = (* "mut" *) token env v1 in
      let v2 = map_pattern env v2 in
      todo env (v1, v2)
  | `Range_pat (v1, v2, v3) ->
      let v1 = map_anon_choice_lit_pat_0884ef0 env v1 in
      let v2 =
        (match v2 with
        | `DOTDOTDOT tok -> (* "..." *) token env tok
        | `DOTDOTEQ tok -> (* "..=" *) token env tok
        )
      in
      let v3 = map_anon_choice_lit_pat_0884ef0 env v3 in
      todo env (v1, v2, v3)
  | `Or_pat (v1, v2, v3) ->
      let v1 = map_pattern env v1 in
      let v2 = (* "|" *) token env v2 in
      let v3 = map_pattern env v3 in
      todo env (v1, v2, v3)
  | `Const_blk x -> map_const_block env x
  | `X__ tok -> (* "_" *) token env tok
  )

and map_pointer_type (env : env) ((v1, v2, v3) : CST.pointer_type) =
  let v1 = (* "*" *) token env v1 in
  let v2 =
    (match v2 with
    | `Const tok -> (* "const" *) token env tok
    | `Muta_spec tok -> (* "mut" *) token env tok
    )
  in
  let v3 = map_type_ env v3 in
  todo env (v1, v2, v3)

and map_qualified_type (env : env) ((v1, v2, v3) : CST.qualified_type) =
  let v1 = map_type_ env v1 in
  let v2 = (* "as" *) token env v2 in
  let v3 = map_type_ env v3 in
  todo env (v1, v2, v3)

and map_range_expression (env : env) (x : CST.range_expression) =
  (match x with
  | `Exp_choice_DOTDOT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 =
        (match v2 with
        | `DOTDOT tok -> (* ".." *) token env tok
        | `DOTDOTDOT tok -> (* "..." *) token env tok
        | `DOTDOTEQ tok -> (* "..=" *) token env tok
        )
      in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_DOTDOT (v1, v2) ->
      let v1 = map_expression env v1 in
      let v2 = (* ".." *) token env v2 in
      todo env (v1, v2)
  | `DOTDOT_exp x -> map_base_field_initializer env x
  | `DOTDOT tok -> (* ".." *) token env tok
  )

and map_reference_type (env : env) ((v1, v2, v3, v4) : CST.reference_type) =
  let v1 = (* "&" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> map_lifetime env x
    | None -> todo env ())
  in
  let v3 =
    (match v3 with
    | Some tok -> (* "mut" *) token env tok
    | None -> todo env ())
  in
  let v4 = map_type_ env v4 in
  todo env (v1, v2, v3, v4)

and map_return_expression (env : env) (x : CST.return_expression) =
  (match x with
  | `Ret_exp (v1, v2) ->
      let v1 = (* "return" *) token env v1 in
      let v2 = map_expression env v2 in
      todo env (v1, v2)
  | `Ret tok -> (* "return" *) token env tok
  )

and map_scoped_identifier (env : env) ((v1, v2, v3) : CST.scoped_identifier) =
  let v1 =
    (match v1 with
    | Some x ->
        (match x with
        | `Choice_self x -> map_path env x
        | `Brac_type x -> map_bracketed_type env x
        | `Gene_type_with_turb x ->
            map_generic_type_with_turbofish env x
        )
    | None -> todo env ())
  in
  let v2 = (* "::" *) token env v2 in
  let v3 = (* identifier *) token env v3 in
  todo env (v1, v2, v3)

and map_scoped_type_identifier (env : env) ((v1, v2, v3) : CST.scoped_type_identifier) =
  let v1 =
    (match v1 with
    | Some x ->
        (match x with
        | `Choice_self x -> map_path env x
        | `Gene_type_with_turb x ->
            map_generic_type_with_turbofish env x
        | `Brac_type x -> map_bracketed_type env x
        | `Gene_type x -> map_generic_type env x
        )
    | None -> todo env ())
  in
  let v2 = (* "::" *) token env v2 in
  let v3 = (* identifier *) token env v3 in
  todo env (v1, v2, v3)

and map_scoped_type_identifier_in_expression_position (env : env) ((v1, v2, v3) : CST.scoped_type_identifier_in_expression_position) =
  let v1 =
    (match v1 with
    | Some x ->
        (match x with
        | `Choice_self x -> map_path env x
        | `Gene_type_with_turb x ->
            map_generic_type_with_turbofish env x
        )
    | None -> todo env ())
  in
  let v2 = (* "::" *) token env v2 in
  let v3 = (* identifier *) token env v3 in
  todo env (v1, v2, v3)

and map_statement (env : env) (x : CST.statement) =
  (match x with
  | `Exp_stmt x -> map_expression_statement env x
  | `Choice_const_item x -> map_declaration_statement env x
  )

and map_trait_bounds (env : env) ((v1, v2, v3) : CST.trait_bounds) =
  let v1 = (* ":" *) token env v1 in
  let v2 = map_anon_choice_type_d689819 env v2 in
  let v3 =
    List.map (fun (v1, v2) ->
      let v1 = (* "+" *) token env v1 in
      let v2 = map_anon_choice_type_d689819 env v2 in
      todo env (v1, v2)
    ) v3
  in
  todo env (v1, v2, v3)

and map_tuple_type (env : env) ((v1, v2, v3, v4, v5) : CST.tuple_type) =
  let v1 = (* "(" *) token env v1 in
  let v2 = map_type_ env v2 in
  let v3 =
    List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_type_ env v2 in
      todo env (v1, v2)
    ) v3
  in
  let v4 =
    (match v4 with
    | Some tok -> (* "," *) token env tok
    | None -> todo env ())
  in
  let v5 = (* ")" *) token env v5 in
  todo env (v1, v2, v3, v4, v5)

and map_type_ (env : env) (x : CST.type_) =
  (match x with
  | `Abst_type (v1, v2) ->
      let v1 = (* "impl" *) token env v1 in
      let v2 = map_anon_choice_field_id_02b4436 env v2 in
      todo env (v1, v2)
  | `Ref_type x -> map_reference_type env x
  | `Meta tok -> (* pattern \$[a-zA-Z_]\w* *) token env tok
  | `Poin_type x -> map_pointer_type env x
  | `Gene_type x -> map_generic_type env x
  | `Scoped_type_id x -> map_scoped_type_identifier env x
  | `Tuple_type x -> map_tuple_type env x
  | `Unit_type (v1, v2) ->
      let v1 = (* "(" *) token env v1 in
      let v2 = (* ")" *) token env v2 in
      todo env (v1, v2)
  | `Array_type (v1, v2, v3, v4) ->
      let v1 = (* "[" *) token env v1 in
      let v2 = map_type_ env v2 in
      let v3 =
        (match v3 with
        | Some (v1, v2) ->
            let v1 = (* ";" *) token env v1 in
            let v2 = map_expression env v2 in
            todo env (v1, v2)
        | None -> todo env ())
      in
      let v4 = (* "]" *) token env v4 in
      todo env (v1, v2, v3, v4)
  | `Func_type x -> map_function_type env x
  | `Id tok -> (* identifier *) token env tok
  | `Macro_invo x -> map_macro_invocation env x
  | `Empty_type tok -> (* "!" *) token env tok
  | `Dyna_type (v1, v2) ->
      let v1 = (* "dyn" *) token env v1 in
      let v2 = map_anon_choice_field_id_02b4436 env v2 in
      todo env (v1, v2)
  | `Boun_type x -> map_bounded_type env x
  | `Choice_u8 x -> map_anon_choice_u8_6dad923 env x
  )

and map_type_arguments (env : env) ((v1, v2, v3, v4, v5) : CST.type_arguments) =
  let v1 = (* < *) token env v1 in
  let v2 = map_anon_choice_type_39799c3 env v2 in
  let v3 =
    List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_anon_choice_type_39799c3 env v2 in
      todo env (v1, v2)
    ) v3
  in
  let v4 =
    (match v4 with
    | Some tok -> (* "," *) token env tok
    | None -> todo env ())
  in
  let v5 = (* ">" *) token env v5 in
  todo env (v1, v2, v3, v4, v5)

and map_type_parameters (env : env) ((v1, v2, v3, v4, v5) : CST.type_parameters) =
  let v1 = (* "<" *) token env v1 in
  let v2 = map_anon_choice_life_859e88f env v2 in
  let v3 =
    List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_anon_choice_life_859e88f env v2 in
      todo env (v1, v2)
    ) v3
  in
  let v4 =
    (match v4 with
    | Some tok -> (* "," *) token env tok
    | None -> todo env ())
  in
  let v5 = (* ">" *) token env v5 in
  todo env (v1, v2, v3, v4, v5)

and map_use_clause (env : env) (x : CST.use_clause) =
  (match x with
  | `Choice_self x -> map_path env x
  | `Use_as_clause (v1, v2, v3) ->
      let v1 = map_path env v1 in
      let v2 = (* "as" *) token env v2 in
      let v3 = (* identifier *) token env v3 in
      todo env (v1, v2, v3)
  | `Use_list x -> map_use_list env x
  | `Scoped_use_list (v1, v2, v3) ->
      let v1 =
        (match v1 with
        | Some x -> map_path env x
        | None -> todo env ())
      in
      let v2 = (* "::" *) token env v2 in
      let v3 = map_use_list env v3 in
      todo env (v1, v2, v3)
  | `Use_wild (v1, v2) ->
      let v1 =
        (match v1 with
        | Some (v1, v2) ->
            let v1 = map_path env v1 in
            let v2 = (* "::" *) token env v2 in
            todo env (v1, v2)
        | None -> todo env ())
      in
      let v2 = (* "*" *) token env v2 in
      todo env (v1, v2)
  )

and map_use_list (env : env) ((v1, v2, v3, v4) : CST.use_list) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) ->
        let v1 =
          (match v1 with
          | `Use_clause x -> map_use_clause env x
          )
        in
        let v2 =
          List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 =
              (match v2 with
              | `Use_clause x -> map_use_clause env x
              )
            in
            todo env (v1, v2)
          ) v2
        in
        todo env (v1, v2)
    | None -> todo env ())
  in
  let v3 =
    (match v3 with
    | Some tok -> (* "," *) token env tok
    | None -> todo env ())
  in
  let v4 = (* "}" *) token env v4 in
  todo env (v1, v2, v3, v4)

and map_visibility_modifier (env : env) (x : CST.visibility_modifier) =
  (match x with
  | `Crate tok -> (* "crate" *) token env tok
  | `Pub_opt_LPAR_choice_self_RPAR (v1, v2) ->
      let v1 = (* "pub" *) token env v1 in
      let v2 =
        (match v2 with
        | Some (v1, v2, v3) ->
            let v1 = (* "(" *) token env v1 in
            let v2 =
              (match v2 with
              | `Self tok -> (* "self" *) token env tok
              | `Super tok -> (* "super" *) token env tok
              | `Crate tok -> (* "crate" *) token env tok
              | `In_choice_self (v1, v2) ->
                  let v1 = (* "in" *) token env v1 in
                  let v2 = map_path env v2 in
                  todo env (v1, v2)
              )
            in
            let v3 = (* ")" *) token env v3 in
            todo env (v1, v2, v3)
        | None -> todo env ())
      in
      todo env (v1, v2)
  )

and map_where_clause (env : env) ((v1, v2, v3, v4) : CST.where_clause) =
  let v1 = (* "where" *) token env v1 in
  let v2 = map_where_predicate env v2 in
  let v3 =
    List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_where_predicate env v2 in
      todo env (v1, v2)
    ) v3
  in
  let v4 =
    (match v4 with
    | Some tok -> (* "," *) token env tok
    | None -> todo env ())
  in
  todo env (v1, v2, v3, v4)

and map_where_predicate (env : env) ((v1, v2) : CST.where_predicate) =
  let v1 =
    (match v1 with
    | `Life x -> map_lifetime env x
    | `Id tok -> (* identifier *) token env tok
    | `Scoped_type_id x -> map_scoped_type_identifier env x
    | `Gene_type x -> map_generic_type env x
    | `Ref_type x -> map_reference_type env x
    | `Poin_type x -> map_pointer_type env x
    | `Tuple_type x -> map_tuple_type env x
    | `Higher_ranked_trait_bound x ->
        map_higher_ranked_trait_bound env x
    | `Choice_u8 x -> map_anon_choice_u8_6dad923 env x
    )
  in
  let v2 = map_trait_bounds env v2 in
  todo env (v1, v2)

let map_source_file (env : env) (x : CST.source_file) =
  (match x with
  | `Rep_stmt xs -> List.map (map_statement env) xs
  | `Semg_exp (v1, v2) ->
      let v1 = (* "__SEMGREP_EXPRESSION" *) token env v1 in
      let v2 = map_expression env v2 in
      todo env (v1, v2)
  | `Semg_stmt (v1, v2) ->
      let v1 = (* "__SEMGREP_STATEMENT" *) token env v1 in
      let v2 = List.map (map_statement env) v2 in
      todo env (v1, v2)
  )
