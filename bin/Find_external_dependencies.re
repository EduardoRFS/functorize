type path_type =
  | Type(Types.type_declaration)
  | Value(Types.value_description)
  | Module(Types.module_declaration);

let load_paths_from_cmt = cmt =>
  cmt.Cmt_format.cmt_loadpath
  |> List.map(fpath =>
       Filename.is_relative(fpath)
         ? Filename.concat(cmt.Cmt_format.cmt_builddir, fpath) : fpath
     )
  |> /* TODO: extracted from cmt2annot */ List.rev
  |> List.iter(Load_path.add_dir);

// TODO: Envaux.Error
let normalize_env = (rebuild_env, env) =>
  rebuild_env ? Env.env_of_only_summary(Envaux.env_from_summary, env) : env;
let normalize_path = (env, path) =>
  path
  |> Env.normalize_module_path(None, env)
  |> Env.normalize_path_prefix(None, env)
  |> Env.normalize_type_path(None, env);

// TODO: Env.error
let find_path_type = (kind, env, path) =>
  switch (kind) {
  | `Type => Type(env |> Env.find_type(path))
  | `Value => Value(env |> Env.find_value(path))
  | `Module => Module(env |> Env.find_module(path))
  };
let find_all_paths_on_typedtree = tast => {
  open Typedtree;
  // TODO: functor
  let acc = ref([]);
  let accumulate = (kind, after, get_env, get_paths, iterator, value) => {
    let env = get_env(value);
    let paths = get_paths(value);
    paths |> List.iter(path => acc := [(kind, env, path), ...acc^]);
    after(iterator, value);
  };

  let super = Tast_iterator.default_iterator;
  let iterator = {
    ...super,
    // TODO: types, pat
    // TODO: module_coercion, module_type_desc, with_constraint
    typ:
      accumulate(
        `Type,
        super.typ,
        ctyp => ctyp.ctyp_env,
        ctyp =>
          switch (ctyp.ctyp_desc) {
          | Ttyp_constr(path, _, _)
          | Ttyp_class(path, _, _) => [path]
          | _ => []
          },
      ),
    class_expr:
      accumulate(
        `Value,
        super.class_expr,
        expr => expr.cl_env,
        expr =>
          switch (expr.cl_desc) {
          | Tcl_ident(path, _, _) => [path]
          | _ => []
          },
      ),
    module_expr:
      accumulate(
        `Module,
        super.module_expr,
        expr => expr.mod_env,
        expr =>
          switch (expr.mod_desc) {
          | Tmod_ident(path, _) => [path]
          // TODO: Tmod_constraint Tmod_apply
          | _ => []
          },
      ),
    expr:
      accumulate(
        `Value,
        super.expr,
        expr => expr.exp_env,
        expr =>
          switch (expr.exp_desc) {
          | Texp_ident(path, _, _)
          | Texp_new(path, _, _)
          | Texp_instvar(_ /* path_to_self */, path, _)
          | Texp_setinstvar(_ /* path_to_self */, path, _, _) => [path]
          | Texp_override(_ /* path_to_self */, overrides) =>
            overrides |> List.map(((path, _, _)) => path)
          // TODO: Texp_extension_constructor
          | _ => []
          },
      ),
  };
  switch (tast) {
  | `Signature(tast) => iterator.signature(iterator, tast)
  | `Structure(tast) => iterator.structure(iterator, tast)
  };
  acc^;
};
let path_is_global = path => Path.scope(path) == Ident.lowest_scope;
let find = cmt_path => {
  let cmt = Cmt_format.read_cmt(cmt_path);
  let typedtree =
    switch (cmt.Cmt_format.cmt_annots) {
    | Cmt_format.Implementation(structure) => `Structure(structure)
    | Cmt_format.Interface(signature) => `Signature(signature)
    | _ => failwith("cmt is not an implementation")
    };

  Envaux.reset_cache();
  load_paths_from_cmt(cmt);
  find_all_paths_on_typedtree(typedtree)
  |> List.filter_map(((kind, env, path)) => {
       let env = env |> normalize_env(cmt.Cmt_format.cmt_use_summaries);
       let path = path |> normalize_path(env);
       let path_type = find_path_type(kind, env, path);
       path_is_global(path) ? Some((path, path_type)) : None;
     });
};

let find = (~cmt_path) =>
  try(find(cmt_path)) {
  | Envaux.Error(error) =>
    Format.asprintf("%a", Envaux.report_error, error) |> failwith
  | Env.Error(error) =>
    Format.asprintf("%a", Env.report_error, error) |> failwith
  };
