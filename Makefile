all: 2048 243

2048:
	ghc --make main.hs -cpp -Dhs2048
	mv main 2048

243: 
	ghc --make main.hs 
	mv main 243

clean:
	rm *.hi *.o 2048 243

