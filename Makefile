OBJ=obj
SRC=src

BIN=bin

OBJS=
OBJS+=${OBJ}/parser.o ${OBJ}/sql.o ${OBJ}/py.o

CXXFLAGS+=-std=c++17 -Wall -Wextra
LDFLAGS=-loil -lsqlite3 -lpq -lcurl


BINS=${BIN}/schema_parse

debug: CXXFLAGS+=-g -rdynamic
debug: dirs ${BINS}

release: CXXFLAGS+=-O2
release: dirs ${BINS}

dirs:
	mkdir -p ${OBJ} ${BIN}

# src targets
${BIN}/%: ${SRC}/%.cpp ${OBJS}
	${CXX} ${CXXFLAGS} -o $@ $^ ${LDFLAGS}

${OBJ}/%.o: ${SRC}/%.cpp ${SRC}/%.hpp
	${CXX} ${CXXFLAGS} -o $@ -c $<


clean:
	rm -f ${OBJ}/*.o ${BINS}

