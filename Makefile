all:
	(cd deps/log4erl;$(MAKE) all)
	(cp deps/log4erl/ebin/* ebin/)
	(cd src; $(MAKE) clean; $(MAKE) all;etags *.erl)
#	(cp src/*.boot src/*.script ebin/)

dialyzer:all
	(dialyzer --plt $(PLT) -Werror_handling -Wrace_conditions  -r .)

debug:
	(cd src; $(MAKE) clean; $(MAKE) debug)

env:
	(cd deps;./get_deps.sh)
	(make env0)

env0:all
	echo "creating app dir ...";
	mkdir -p /data/apps/kerlmq
	mkdir -p /data/apps/kerlmq/ebin
	echo "creating log dir ..."
	mkdir -p /data/logs/kerlmq
	echo "creating data dir ..."
	mkdir -p /data/appdatas/kerlmq/data
	echo "creating tmp dir ..."
	mkdir -p /data/appdatas/kerlmq/tmp
	echo "createing config dir ..."
	mkdir -p /data/confs/kerlmq
	echo "creating metadata dir ..."
	mkdir -p /data/appdatas/kerlmq/mnesia
	echo "creating metadata db ..."
	erl -noshell -pa ebin -mnesia dir '"/data/appdatas/kerlmq/mnesia"' -s kerlmq_ringqueue create_db -s kerlmq_incrqueue create_db -s init stop
	echo "copying config files ..."
	cp release/*.config /data/confs/kerlmq/
	echo "copying app files ..."
	cp ebin/* /data/apps/kerlmq/ebin/
	cp scripts/start.sh /data/apps/kerlmq/
	cp scripts/debug.sh /data/apps/kerlmq/

test: all
	(cd test;$(MAKE) all)
	(cd src;$(MAKE) test)

clean:
	(cd src;$(MAKE) clean)
	(cd test;$(MAKE) clean)
	(cd deps/log4erl;$(MAKE) clean)
	(rm -rf doc/*.html)


