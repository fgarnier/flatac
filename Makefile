.PHONY: clean all
all:
	@cd /Users/maximegaudin/Documents/Professionel/Stages/Verimag/Developpement/flatac/src/SSL/ && make -f Makefile.sub && cd /Users/maximegaudin/Documents/Professionel/Stages/Verimag/Developpement/flatac/src
	@cd /Users/maximegaudin/Documents/Professionel/Stages/Verimag/Developpement/flatac/src/c_upon_SSLandnts_sem/ && make -f Makefile.sub && cd /Users/maximegaudin/Documents/Professionel/Stages/Verimag/Developpement/flatac/src
	@cd /Users/maximegaudin/Documents/Professionel/Stages/Verimag/Developpement/flatac/src/global_mem/ && make -f Makefile.sub && cd /Users/maximegaudin/Documents/Professionel/Stages/Verimag/Developpement/flatac/src
	@cd /Users/maximegaudin/Documents/Professionel/Stages/Verimag/Developpement/flatac/src/ && make -f Makefile.sub && cd /Users/maximegaudin/Documents/Professionel/Stages/Verimag/Developpement/flatac/src
clean:
	@cd /Users/maximegaudin/Documents/Professionel/Stages/Verimag/Developpement/flatac/src/SSL/ && make -f Makefile.sub clean && cd /Users/maximegaudin/Documents/Professionel/Stages/Verimag/Developpement/flatac/src
	@cd /Users/maximegaudin/Documents/Professionel/Stages/Verimag/Developpement/flatac/src/c_upon_SSLandnts_sem/ && make -f Makefile.sub clean && cd /Users/maximegaudin/Documents/Professionel/Stages/Verimag/Developpement/flatac/src
	@cd /Users/maximegaudin/Documents/Professionel/Stages/Verimag/Developpement/flatac/src/global_mem/ && make -f Makefile.sub clean && cd /Users/maximegaudin/Documents/Professionel/Stages/Verimag/Developpement/flatac/src
	@cd /Users/maximegaudin/Documents/Professionel/Stages/Verimag/Developpement/flatac/src/./ && make -f Makefile.sub clean && cd /Users/maximegaudin/Documents/Professionel/Stages/Verimag/Developpement/flatac/src
