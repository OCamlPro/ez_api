
CLIENT_BIN_PATH=../../_build/default/test/session/test_cookie_client.bc.js
SERVER_BIN_PATH=../../_build/default/test/session/test_cookie_server.exe

all: build website api-server

build:
	dune build --profile release

website: $(CLIENT_BIN_PATH)
	cp -f $(CLIENT_BIN_PATH) client.js

api-server: $(SERVER_BIN_PATH)
	cp -f $(SERVER_BIN_PATH) server.exe

run-web: website
	php -S localhost:8885

run-api: api-server
	./server.exe
