.PHONY: all deps compile clean doc test hexbuild release

REBAR=$(shell which rebar3 || echo ./rebar3)
REBAR_URL=https://s3.amazonaws.com/rebar3/rebar3

all: clean compile doc hexbuild test

clean: $(REBAR)
	@echo "$(REBAR) clean"
	$(REBAR) clean
	rm -rf ./_build
	rm -f ./erl_crash.dump
	rm -f ./rebar.lock

deps: $(REBAR)
	$(REBAR) get-deps

compile: $(REBAR)
	@echo "$(REBAR) compile"
	$(REBAR) compile

test: $(REBAR)
	$(REBAR) eunit

doc: $(REBAR)
	@echo "$(REBAR) ex_doc"
	$(REBAR) ex_doc

hexbuild: $(REBAR)
	@echo "$(REBAR) hex build"
	$(REBAR) hex build

release: $(REBAR)
	@echo "$(REBAR) release"
	$(REBAR) release

# Get rebar3 if it doesn't exist. If rebar3 was found on PATH, the
# $(REBAR) dep will be satisfied since the file will exist.
./rebar3:
	@echo "Fetching rebar3 from $(REBAR_URL)"
	@erl -noinput -noshell -s inets -s ssl  -eval '{ok, _} = httpc:request(get, {"${REBAR_URL}", []}, [], [{stream, "${REBAR}"}])' -s init stop
		chmod +x ${REBAR}

