all:
	@ghc --make main.hs -o hanagram

test:
	@runhaskell HanagramTest.hs

clean:
	@rm *.hi *.o hanagram
