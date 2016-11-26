OBJ=obj
SRC=src

BIN=bin

OBJS=
#OBJS+=${OBJ}/

CXXFLAGS+=-std=c++17 -Wall -Wextra
LDFLAGS=-loil -lsqlite3 -lpq -lcurl


BINS=${BIN}/schema_parse

debug: CXXFLAGS+=-g -rdynamic
debug: dirs ${BINS}

release: CXXFLAGS+=-O2
release: dirs ${BINS}

dirs:
	mkdir -p ${OBJ} ${LIB} ${BIN}

# src targets
${BIN}/%: ${SRC}/%.o ${OBJS}
	${CXX} ${CXXFLAGS} -o $@ $< ${LDFLAGS}

${OBJ}/%.o: ${SRC}/%.cpp ${SRC}/%.hpp
	${CXX} ${CXXFLAGS} -o $@ -c $<


clean:
	rm -f ${OBJ}/*.o ${BINS}

