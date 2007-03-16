maketea: *.hs
	ghc -o maketea -fglasgow-exts --make Main.hs

clean: 
	rm -f *.hi *.o

cleanall: clean
	rm -f maketea
