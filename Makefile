build:
	docker build -t paskell .

test: build
	docker run paskell stack test --ghc-options=-Werror

bash: build
	docker run -it paskell bash
