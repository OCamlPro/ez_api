![Build Status](https://github.com/ocpmax/ez-api/workflows/CI/badge.svg?branch=master)

# ez-api : a simple library to write client/server web APIs


## Encoding

Ez-api proposes a small addition to `ocplib-json-typed/ezjsonm` for easier coding and to avoid overflow when using js_of_ocaml.

## Server

The server can be implememted using different libraries cohttp or httpaf.
Some optional libraries are required to use the server part:
```
opam depext geoip
opam install calendar geoip re
```

In order to use cohttp, the optional library `cohttp-lwt-unix` must be installed:
```
opam install cohttp-lwt-unix
```

Same thing to use httpaf, the optional library `httpaf-lwt-unix` must be installed:
```
opam install extunix httpaf-lwt-unix
```

Moreover ez-api provides useful tools for server:
- server side sessions with different kind of authentication.
- recaptcha verification
- sending email via sendgrid

## Client

Ez-api implements client side requests for unix and web using different libraries.

For unix:
- curl implementation that requires curl library:
```
opam depext ocurl
opam install ocurl
```
- cohttp implementation that requires tls library:
```
opam depext tls
opam install tls
```

For web:
- javascript XHR
- cohttp web requiring `cohttp-lwt-jsoo`
```
opam install cohttp-lwt-jsoo
```
- fetch requiring `ezjs-fetch`
```
opam pin add ezjs-fetch git+https://github.com/ocamlpro/ezjs.git
```

## Installation

After you installed the optional dependencies needed for your setup, you can install ez-api with opam:
```
opam install .
```

## Documentation

The documentation for the different libraries can be found here: [API Reference](https://ocpmax.github.io/ez-api/ez-api/index.html)

## Usage

Have a look at the files in `libs/ez-api/test`
