SOURCES = 					\
	README.md				\

help: ## This help.
	@awk 'BEGIN {FS = ":.*?## "} /^[a-zA-Z_-]+:.*?## / {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}' $(MAKEFILE_LIST) | sort

local/lib:
	mkdir -p local/lib

yxml: local/lib
	cd submodules/yxml && make
	cd submodules/yxml && gcc -shared -o libyxml.so yxml.o
	cp submodules/yxml/libyxml.so local/lib/

okvslite: local/lib
	cp patches/MakefileLSM submodules/sqlite/
	cd submodules/sqlite/ && make -f MakefileLSM lsm.so
	cp submodules/sqlite/lsm.so local/lib

termbox: local/lib
	cd submodules/termbox/ && ./waf configure
	cd submodules/termbox/ && ./waf
	cp submodules/termbox/build/src/libtermbox.so local/lib/

chez:
	mkdir -p local/src
	sudo apt install uuid-dev
	cd local/src && git clone --recursive --depth=1 https://github.com/cisco/ChezScheme/
	cd local/src/ChezScheme/ && ./configure --disable-x11 --disable-curses --threads
	cd local/src/ChezScheme/ && make
	cd local/src/ChezScheme/ && sudo make install

init: chez

doc:
	cat $(SOURCES) > arew-scheme.md
	pandoc arew-scheme.md -o arew-scheme.html
	pandoc arew-scheme.html -o arew-scheme.pdf

repl: ## repl for the win
	@./run

profile-clean:
	rm -rf profile
	mkdir -p profile

check: profile-clean ## run tests using the library test runner
	./venv scheme --program src/arew.scm check src/check-check.scm
	./venv scheme --program src/arew.scm check src/

todo: ## Things that should be done
	@grep -nR --color=always  --before-context=2  --after-context=2 TODO src/

xxx: ## Things that require attention
	@grep -nR --color=always --before-context=2  --after-context=2 XXX src/

clean: ## Remove useless files...
	rm arew-scheme.*
