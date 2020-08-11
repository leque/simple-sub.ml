type ctx

val builtins : ctx

val type_top :
  ?ctx:ctx ->
  (bool * string * Term.t) list ->
  (string * TypeScheme.t) list * ctx

val expand_simple_type :
  SimpleType.t -> Type.t

val compact_type :
  SimpleType.t -> CompactTypeScheme.t

val simplify_compact_type :
  CompactTypeScheme.t -> CompactTypeScheme.t

val expand_compact_type :
  CompactTypeScheme.t -> Type.t
