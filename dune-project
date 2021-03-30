(lang dune 2.6)
(name ez-api)

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
 (documentation "https://ocamlpro.github.io/ez-api/doc")
 (depends
  (json-data-encoding (>= 0.7.0))
  (lwt (>= 4.0.0))
  (ezjsonm (>= 1.0.0))
  uuidm)
 (depopts
  (geoip (>= 0.0.3))
  (js_of_ocaml-lwt (>= 3.4.0))
  (calendar (>= 2.03))
  cohttp-lwt-unix
  (cohttp-lwt-jsoo (>= 4.0.0))
  lwt_log
  tls
  (httpaf-lwt-unix (>= 0.6.0))
  (ocurl (>= 0.8.0))
  (ezjs_fetch (> 0.2.0))
  websocket-lwt-unix
  websocket-httpaf-lwt
  (ppxlib (and :build (>= 0.16.0))))
 (conflicts
  (cohttp-lwt-jsoo (< 4.0.0))))
