hanagram:
	ghc --make main.hs -o hanagram

test:
	@runhaskell HanagramTest.hs

clean:
	@find . -name "*.o" -o -name "*.hi" -o -name "hanagram" | xargs rm
