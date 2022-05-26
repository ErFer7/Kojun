# Define o comando de remoção com base no sistema operacional
ifeq ($(OS),Windows_NT)
	CLEAN = del *.hi *.o del /f /s
else
	CLEAN = rm -rf *.hi *.o Source/*.hi Source/*.o
endif

Kojun: KojunSolver.hs Source/Structure.hs Source/Parser.hs Source/Printer.hs Source/Solver.hs
	ghc KojunSolver.hs Source/Structure.hs Source/Parser.hs Source/Printer.hs Source/Solver.hs

clean:
	$(CLEAN)
