hanagram:
	ghc --make main.hs -o hanagram

test:
	@runhaskell HanagramTest.hs

install:
	cp hanagram /usr/local/bin/hanagram

uninstall:
	rm /usr/local/bin/hanagram

clean:
	@find . -name "*.o" -o -name "*.hi" -o -name "hanagram" | xargs rm
