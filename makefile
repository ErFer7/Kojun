# Define o comando de remoção com base no sistema operacional
ifeq ($(OS),Windows_NT)
	RM = del /f /s
else
	RM = rm -r -f
endif

Kojun: KojunSolver.hs Source/Structure.hs Source/Parser.hs Source/Printer.hs Source/Solver.hs
	ghc KojunSolver.hs Source/Structure.hs Source/Parser.hs Source/Printer.hs Source/Solver.hs
	$(RM) *.hi
	$(RM) *.o

clean:
	$(RM) *.exe
	$(RM) *.hi
	$(RM) *.o
