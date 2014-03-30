.PHONY: serve

regex: regex.rs
	rustc -g --test regex.rs

kcov: regex
	kcov kcov ./regex

serve: kcov
	cd kcov && python -m SimpleHTTPServer
