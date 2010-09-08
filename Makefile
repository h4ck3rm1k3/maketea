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

# TODO: clean is important to avoid the 'module 'x' is not interpreted' error.
tags: clean
	ghc -fglasgow-exts *.hs -e :ctags
