![Build Status](https://github.com/OCamlPro/ez_api/workflows/CI/badge.svg?branch=master)

# ez_api : a simple library to write client/server web APIs

EzAPI is a collection of libraries and tools to handle APIs that can be installed with opam:
```
opam install ez_api
```

The main package `ez_api` accessible via the module `EzAPI` defines services handling encoding (via json-data-encoding), errors, securites, and documentation.

Around this `ez_api` provides a multitude of sub-package, sometimes depending on optional libraries.

## Encoding

ez_api proposes a small addition to `json-data-encoding` for easier coding (`ez_api.encoding`) and to avoid overflow when using js_of_ocaml (`ez_api.encoding_js`).

## Client

ez_api implements client side requests for unix and web using different libraries.
The main sub-package `ez_api.req` (or `ez_api.req_lwt` if you're using lwt) is a virtual implemetation that needs to be complemented with a implementation of your choice:

For unix:
- curl implementation `ez_api.icurl` or `ez_api.icurl_lwt` that requires curl library:
```
opam depext ocurl
opam install ocurl
```
- cohttp implementation `ez_api.icohttp` or `ez_api.icohttp_lwt` that requires tls and cohttp-lwt-unix libraries:
```
opam depext tls
opam install tls cohttp-lwt-unix
```

For web:
- javascript XHR implemenation `ez_api.ixhr` or `ez_api.ixhr_lwt`
- cohttp web `ez_api.icoxhr` or `ez_api.icoxhr_lwt` that requires cohttp-lwt-jsoo:
```
opam install cohttp-lwt-jsoo
```
- fetch implemetation `ez_api.ifetch` or `ez_api.ifetch_lwt` requiring `ezjs_fetch`
```
opam install ezjs_fetch
```

All these implementation can also be used directly by removing the `i` preceding the sub-package name.

## Websocket Client

As for the traditional client, ez_api provides a virtual implementation of a websocket client: `ez_api.ws`
It can be complemented with:
- For unix the implementation `ez_api.ws_cohttp` requires cohttp-lwt-unix and websocket-lwt-unix:
```
opam install cohttp-lwt-unix websocket-lwt-unix
```
- For javascript `ez_api.ws_js` that uses the native implementation of the browsers


## Server

The server part (`ez_api.server`) requires more optional unix dependencies from geoip:
```
opam depext geoip
opam install calendar geoip
```
As for the client, the server has a virtual implementation (`ez_api.server`) that can be complemented with either cohttp or httpaf:

- for cohttp (`ez_api.iserver_cohttp`), you will need cohttp-lwt-unix:
```
opam install cohttp-lwt-unix
```
- for httpaf (`ez_api.iserver_httpaf`), you will need httpaf-lwt-unix:
```
opam install httpaf-lwt-unix
```

These implementation can also be used directly by removing the `i` preceding the sub-package name.

If you need your server to handle websocket, you will need to install the corresponding dependencies:
- for cohttp server:
```
opam install websocket-lwt-unix
```
- for httpaf server:
```
opam pin add websocket-httpaf.~dev git+https://github.com/anmonteiro/websocket-httpaf.git
opam pin add websocket-httpaf-lwt.~dev git+https://github.com/anmonteiro/websocket-httpaf.git
```

## More Tools

ez_api provides useful tools for server and client:
- server side sessions with different kind of authentication. (`ez_api.session`, `ez_api.session_client`, `ez_api.server_session`)
- recaptcha verification (`ez_api.recaptcha`)
- sending email via sendgrid (`ez_api.sendgrid`)
- exporting the documentation of your API to openapi-specs JSON (`ez_api.openAPI`)
- ppx rewriter (`ez_api.ppx`) to simplify some server/handler registration

## Documentation

The documentation for the different libraries can be found here: [API Reference](https://ocamlpro.github.io/ez_api/ez_api/index.html)

## Usage

Have a look at the files in `test`
