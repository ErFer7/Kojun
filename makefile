# Define o comando de remoção com base no sistema operacional
ifeq ($(OS),Windows_NT)
	RM = del /f
else
	RM = rm -f
endif

Kojun: KojunSolver.hs Source/Structure.hs Source/Parser.hs Source/Printer.hs Source/Solver.hs
	ghc KojunSolver.hs Source/Structure.hs Source/Parser.hs Source/Printer.hs Source/Solver.hs

clean:
	$(RM) *.exe
	$(RM) *.hi
	$(RM) *.o
	$(RM) Source/*.hi
	$(RM) Source/*.o
