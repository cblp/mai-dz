.DELETE_ON_ERROR:
SHELL = bash -eu -o pipefail

QTAH_LIB = lib/libqtah.dylib
# QTAH_LIB = lib/libqtah.so

.PHONY: run
run: db.sqlite $(QTAH_LIB)
	LD_LIBRARY_PATH=lib stack build --exec aw

build/AdventureWorks-oltp-install-script.zip:
	mkdir -p build
	cd build && \
	wget https://github.com/Microsoft/sql-server-samples/releases/download/adventureworks/AdventureWorks-oltp-install-script.zip

build/instawdb.sql: build/AdventureWorks-oltp-install-script.zip
	cd build && \
	unzip AdventureWorks-oltp-install-script.zip
	iconv -f UTF-16 build/instawdb.sql | sponge build/instawdb.sql

db.sqlite: build/instawdb.sql loadCsv.sql
	rm -f $@
	cat loadCsv.sql | sqlite3 $@

$(QTAH_LIB):
	stack build --only-dependencies
	mkdir -p lib
	stack exec --							\
		ghc-pkg --simple-output field qtah-cpp library-dirs	\
	| cut -d ' ' -f 1						\
	| while read -r library_dirs; do				\
		cp -r "$$library_dirs"/* lib/;				\
	done
