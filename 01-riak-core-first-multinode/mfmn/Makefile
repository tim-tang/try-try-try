.PHONY: deps

all: deps compile

compile:
	./rebar compile

deps:
	./rebar get-deps
	# patch the riak_core rebar.config to work around: "Warning: crypto:sha/1 is deprecated and will be removed in in a future release; use crypto:hash/2"
	sed -i 's/warnings_as_errors,//g' deps/riak_core/rebar.config

clean:
	./rebar clean

distclean: clean devclean relclean
	./rebar delete-deps

rel: all
	./rebar generate

devrel: dev1 dev2 dev3

dev1 dev2 dev3:
	mkdir -p dev
	(cd rel && ../rebar generate target_dir=../dev/$@ overlay_vars=vars/$@.config)
