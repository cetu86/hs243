all: 2048 243

2048.hs: main.hs
	cp main.hs 2048.hs

243.hs: main.hs
	cp main.hs 243.hs

2048: 2048.hs
	ghc --make 2048.hs -cpp -Dhs2048

243: 243.hs
	ghc --make 243.hs 

clean:
	rm *.hi *.o 2048 243

