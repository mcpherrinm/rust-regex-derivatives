.PHONY: serve

regex: regex.rs
	rustc regex.rs

regex-dbg: regex.rs
	rustc -g --test regex.rs -o regex-dbg

test: regex-dbg
	./regex-dbg

kcov: regex-dbg
	kcov kcov ./regex-dbg

serve: kcov
	cd kcov && python -m SimpleHTTPServer

foo.png: regex
	./regex | dot -Tpng -o foo.png
