let to_string _ = Error (
    `Msg "\027[0;33merror: yaml not installed\027[0m\n\
          If you need to output yaml try: `opam install yaml`@.")
