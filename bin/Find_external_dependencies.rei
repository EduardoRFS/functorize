type path_type =
  | Type(Types.type_declaration)
  | Value(Types.value_description)
  | Module(Types.module_declaration);
let find: (~cmt_path: string) => list((Path.t, path_type));
