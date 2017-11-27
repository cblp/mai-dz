.DELETE_ON_ERROR:
SHELL = bash -eu -o pipefail

.PHONY: run
run: db.sqlite lib/libqtah.so
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

lib/libqtah.so:
	mkdir -p lib
	stack exec --							\
		ghc-pkg --simple-output field qtah-cpp library-dirs	\
	| while read -r library_dirs; do				\
		cp -r "$$library_dirs"/* lib/;				\
	done
