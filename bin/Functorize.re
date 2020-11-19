let generate = files => {
  let external_paths =
    files
    |> List.map(cmt_path => Find_external_dependencies.find(~cmt_path))
    |> List.concat;
  let signature = To_signature.of_external_paths(external_paths);

  Format.printf("%a%!", Printtyp.signature, signature);
};

open Cmdliner;

let files = Arg.(value & pos_all(file, []) & info([], ~docv="FILE"));

let generate_t = Term.(const(generate) $ files);
let () = Term.exit @@ Term.eval((generate_t, Term.info("functorize")));
