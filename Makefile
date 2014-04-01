.PHONY: serve

regex: regex.rs
	rustc regex.rs

regex-dbg: regex.rs
	rustc -g --test regex.rs

kcov: regex-dbg
	kcov kcov ./regex

serve: kcov
	cd kcov && python -m SimpleHTTPServer
