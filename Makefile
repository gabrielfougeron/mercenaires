FC = gfortran
RULESPATH = ./src/Rules/
STRATPATH = ./src/Strategies/
GAMEPATH = ./src/Game/
OBJ = ./obj/
BIN = ./bin/


#~ FFLAGS=-fimplicit-none -Ofast
FFLAGS=-fbounds-check -fbacktrace -fimplicit-none

all:
	
	$(FC) -J$(OBJ) -I$(OBJ) -c $(RULESPATH)rules.f90 $(FFLAGS) -o $(OBJ)rules.o
	$(FC) -J$(OBJ) -I$(OBJ) -c $(STRATPATH)strategies.f90 $(FFLAGS) -o $(OBJ)strategies.o
	$(FC) -J$(OBJ) -I$(OBJ) -c $(GAMEPATH)game.f90 $(FFLAGS) -o $(OBJ)game.o
	
	$(FC) $(OBJ)game.o $(FFLAGS) -o $(BIN)game.exe 
	
	
