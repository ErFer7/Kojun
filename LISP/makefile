# Define o comando de remoção com base no sistema operacional
ifeq ($(OS),Windows_NT)
	CLEAN = del *.lib *.fas del /f /s
else
	CLEAN = rm -rf *.lib *.fas Source/*.lib Source/*.fas
endif

Kojun: KojunSolver.lisp
	clisp -c KojunSolver.lisp
clean:
	$(CLEAN)
