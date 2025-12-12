hpack:
	hpack

gen-hie:
	gen-hie . > hie.yaml

pre-run: hpack gen-hie

build: pre-run
	cabal build quaalude

repl: pre-run
	cabal v2-repl

tests: pre-run
	cabal v2-test

test: pre-run
	ghciwatch \
	--before-startup-shell hpack \
	--after-startup-ghci 'import System.Environment (withArgs)' \
    --test-ghci 'withArgs ["--color"] main' \
	--command "cabal -O0 -j --ghc-options=-j repl quaalude-tests" \
	--watch test \
	--restart-glob '**/package.yaml' \
	--poll 1s
