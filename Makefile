maketea: *.hs
	ghc -o maketea -fglasgow-exts --make Main.hs

optimized: cleanall
	ghc -o maketea -fglasgow-exts --make Main.hs -O -fvia-C 

profiled: cleanall
	ghc -o maketea -fglasgow-exts --make Main.hs -prof -auto-all

clean: 
	rm -f *.hi *.o

cleanall: clean
	rm -f maketea
