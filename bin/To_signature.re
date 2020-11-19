open Types;
open Find_external_dependencies;

module StringMap = Map.Make(String);

let fold_paths_to_nested_map = external_paths => {
  let rec recursive_merge = (left, right) =>
    StringMap.merge(
      (_key, left, right) =>
        switch (left, right) {
        | (None, None) => None
        | (Some(value), None)
        | (None, Some(value)) => Some(value)
        | (Some(`Module(left)), Some(`Module(right))) =>
          Some(`Module(recursive_merge(left, right)))
        | (Some(_), Some(_)) => assert(false)
        },
      left,
      right,
    );
  let map_to_nested_map = ((path, path_type)) => {
    // TODO: this expects data to not have a Papply
    let (ident, path) =
      switch (Path.flatten(path)) {
      | `Contains_apply => assert(false)
      | `Ok(ident, rest) => (ident, rest)
      };

    let rec to_map = (name, rest) =>
      StringMap.singleton(
        name,
        switch (rest) {
        | [] => path_type
        | [name, ...rest] => `Module(to_map(name, rest))
        },
      );
    to_map(Ident.name(ident), path);
  };

  external_paths
  |> List.sort_uniq(((left, _), (right, _)) => Path.compare(left, right))
  |> List.map(map_to_nested_map)
  |> List.fold_left(recursive_merge, StringMap.empty);
};
let rec of_external_paths = map =>
  StringMap.fold(
    (key, value, acc) =>
      [
        switch (value) {
        | `Type(td) =>
          Sig_type(Ident.create_local(key), td, Trec_first, Exported)
        | `Value(vd) => Sig_value(Ident.create_local(key), vd, Exported)
        | `Module(sub_map) =>
          Sig_module(
            Ident.create_local(key),
            Mp_present,
            {
              md_type: Mty_signature(of_external_paths(sub_map)),
              md_attributes: [],
              md_loc: Location.none,
            },
            Trec_not,
            Exported,
          )
        },
        ...acc,
      ],
    map,
    [],
  );
let of_external_paths = external_paths => {
  let map =
    external_paths
    |> List.filter_map(
         fun
         | (_, Module(_)) => None
         | (path, Value(vd)) => Some((path, `Value(vd)))
         | (path, Type(td)) => Some((path, `Type(td))),
       )
    |> fold_paths_to_nested_map;
  of_external_paths(map);
};
