# Define o comando de remoção com base no sistema operacional
ifeq ($(OS),Windows_NT)
	RM = del /f /s
else
	RM = rm -r -f
endif

Kojun: KojunSolver.hs Source/PuzzleStructure.hs
	ghc KojunSolver.hs Source/PuzzleStructure.hs
	$(RM) *.hi
	$(RM) *.o

clean:
	$(RM) *.exe
