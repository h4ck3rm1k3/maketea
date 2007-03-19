maketea: *.hs
	ghc -o maketea -fglasgow-exts -fallow-undecidable-instances --make Main.hs

optimized: cleanall
	ghc -o maketea -fglasgow-exts -fallow-undecidable-instances --make Main.hs -O -fvia-C 

profiled: cleanall
	ghc -o maketea -fglasgow-exts -fallow-undecidable-instances --make Main.hs -prof -auto-all

clean: 
	rm -f *.hi *.o

cleanall: clean
	rm -f maketea
