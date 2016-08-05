all:
	stack build
	mkdir -p _dist/js
	cp .stack-work/dist/x86_64-osx/Cabal-1.22.8.0_ghcjs/build/example/example.jsexe/all.js _dist/js/app.js
	cp index.html _dist/
	cp style.css _dist/
	cp green-cup.png _dist/

clean:
	rm -rf _dist
