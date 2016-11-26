LDFLAGS=-loil -lsqlite3 -lpq -lcurl
CXXFLAGS=-std=c++17 -Wall -Wextra -g -rdynamic

default: schema_parse

clean:
	rm schema_parse

