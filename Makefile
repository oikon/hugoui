macosx:
	mkdir -p build/bin
	mkdir -p build/macosx/
	raco exe --gui --icns icon.icns -o build/bin/Blog main.rkt
	raco distribute build/macosx/ build/bin/Blog.app
