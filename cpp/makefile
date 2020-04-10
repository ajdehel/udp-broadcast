SHELL := /bin/bash

all:
	if [ ! -d bin ]; then mkdir bin; fi
	g++ --std=c++11 -Isrc src/server.cpp -o bin/server
	g++ --std=c++11 -Isrc src/client.cpp -o bin/client
	g++ --std=c++11 -Isrc src/sink.cpp   -o bin/sink

clean:
	if [ -d bin ]; then rm -rf bin; fi
