.DELETE_ON_ERROR:
SHELL = bash -eu -o pipefail

QTAH_LIB = lib/libqtah.dylib
# QTAH_LIB = lib/libqtah.so

.PHONY: aw
aw: db.sqlite $(QTAH_LIB)
	LD_LIBRARY_PATH=lib stack build :aw --exec aw

.PHONY: sks
sks: $(QTAH_LIB)
	LD_LIBRARY_PATH=lib stack build :sks --exec sks

build/AdventureWorks-oltp-install-script.zip:
	mkdir -p build
	cd build && \
	wget https://github.com/Microsoft/sql-server-samples/releases/download/adventureworks/AdventureWorks-oltp-install-script.zip

build/instawdb.sql: build/AdventureWorks-oltp-install-script.zip
	cd build && \
	unzip AdventureWorks-oltp-install-script.zip
	iconv -f UTF-16 build/instawdb.sql | sponge build/instawdb.sql

db.sqlite: build/instawdb.sql aw/db/loadCsv.sql
	rm -f $@
	sqlite3 $@ < aw/db/loadCsv.sql

$(QTAH_LIB):
	stack build --only-dependencies
	mkdir -p lib
	stack exec --							\
		ghc-pkg --simple-output field qtah-cpp library-dirs	\
	| cut -d ' ' -f 1						\
	| while read -r library_dirs; do				\
		cp -r "$$library_dirs"/* lib/;				\
	done
