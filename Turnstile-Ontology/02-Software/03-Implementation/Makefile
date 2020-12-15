CC = gcc
CFLAGS = -g -O1 -Wall -Werror
TAR = tar

APP = counter
EXE = $(APP).exe
OBJECTS = input.o output.o
LIBS = libhw.so
DIST = dist.tar.gz

TEST = $(APP)test
TESTEXE = $(TEST).exe

build: $(EXE)
dist: $(DIST)

%.o: %.c %.h
	$(CC) $(CFLAGS) -c $<

lib%.so: %.o
	$(CC) $(CFLAGS) -shared -o $@ $<

$(EXE): $(APP).c $(APP).h $(OBJECTS) $(LIBS)
	$(CC) $(CFLAGS) -L$(PWD) -lhw -o $@ $< $(OBJECTS)

$(DIST): conf.json $(LIBS) $(EXE)
	$(TAR) cf $@ $^

$(TESTEXE): test.c test.h $(EXE)
	$(CC) $(CFLAGS) -o $@ $<

.PHONY: test
test: $(TESTEXE)
	./$(TESTEXE)

.PHONY: clean
clean:
	rm -f $(OBJECTS) $(LIBS) $(EXE) $(DIST) $(TESTEXE) .*.d .*.rack
