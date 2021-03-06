FC = gfortran
RULESPATH = ./src/Rules/
STRATPATH = ./src/Strategies/
GAMEPATH = ./src/Game/
OBJ = ./obj/
BIN = ./bin/


FFLAGS=-fimplicit-none -Ofast -march=native 
#~ FFLAGS=-fbounds-check -fbacktrace -fimplicit-none

all:
	
	$(FC) -J$(OBJ) -I$(OBJ) -c $(RULESPATH)rules.f90 $(FFLAGS) -o $(OBJ)rules.o
	$(FC) -J$(OBJ) -I$(OBJ) -c $(STRATPATH)strategies.f90 $(FFLAGS) -o $(OBJ)strategies.o
	$(FC) -J$(OBJ) -I$(OBJ) -c $(GAMEPATH)game.f90 $(FFLAGS) -o $(OBJ)game.o
	
	$(FC) $(OBJ)game.o $(OBJ)strategies.o $(OBJ)rules.o $(FFLAGS) -o $(BIN)game.exe 
	
	time $(BIN)game.exe
