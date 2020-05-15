(lang dune 2.0)
(name ez-api)
(allow_approximate_merlin)

(generate_opam_files true)

(source (github ocamlpro/ez-api))
(license GPL-3.0-or-later)
(authors
 "Cagdas Bozman <cagdas.bozman@ocamlpro.com>"
 "Michael Laporte <michael.laporte@ocamlpro.com>"
 "Fabrice Le Fessant <fabrice.le_fessant@ocamlpro.com>"
 "Maxime Levillain <maxime.levillain@ocamlpro.com>")
(maintainers "Fabrice Le Fessant <fabrice.le_fessant@ocamlpro.com>")

(package
 (name ez-api)
 (synopsis "Easy API library over ocplib-resto/ocplib-directory")
 (description "Easy API library over ocplib-resto/ocplib-directory")
 (documentation "https://ocamlpro.github.io/ez-api/doc")
 (depends
  (ocplib-json-encoding (>= 0.7.0))
  (lwt (>= 4.0.0))
  (ezjsonm (>= 1.0.0))
  (ocplib-compat (>= 1.0))
  (ocplib-lang (>= 1.0)))
 (depopts
  (js_of_ocaml-lwt (>= 3.4.0))
  (calendar (>= 2.03))
  (geoip (>= 0.0.3))
  (cohttp-lwt-unix (>= 2.0.0))
  (re (>= 1.0))
  (httpaf-lwt-unix (>= 0.6.0))
  (extunix (>= 0.2.0))
  (ocurl (>= 0.8.0))))