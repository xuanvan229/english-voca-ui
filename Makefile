build-elm: 
	elm make src/Main.elm --output=dist/elm.js

build-css:
	@echo "=== Compiling CSS ..."
	@sass assets/sass/index.scss dist/css/style.css