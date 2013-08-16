hanagram:
	ghc --make main.hs -o hanagram

test:
	@runhaskell HanagramTest.hs

clean:
	@rm -Rfv *.hi *.o hanagram
