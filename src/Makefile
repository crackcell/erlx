include ../include.mk

all: $(EBIN_FILES)

nodoc: $(EBIN_FILES_NO_DOCS)

release:

debug:
	$(MAKE) DEBUG=-DDEBUG

test: all
	$(ERL) -noshell -pa ../ebin -s eunit test 'kerltools_SUIT' -s init stop

clean:
	rm -rf $(EBIN_FILES)
