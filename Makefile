.DELETE_ON_ERROR:

default: test

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

test: db.sqlite
