FC = gfortran
RULESPATH = ./src/Rules/
STRATPATH = ./src/Strategies/
GAMEPATH = ./src/Game/
OBJ = ./obj/
BIN = ./bin/
# LDFLAGS = -L/usr/lib/non/default/path/
# LIBS = -lm

FFLAGS=-fimplicit-none -Ofast -march=native 
#~ FFLAGS=-fbounds-check -fbacktrace -fimplicit-none

all: run

$(OBJ)rules.o: $(RULESPATH)rules.f90
	$(FC) -J$(OBJ) -I$(OBJ) -c $< $(FFLAGS) -o $(OBJ)rules.o
	
$(OBJ)strategies.o: $(STRATPATH)strategies.f90
	$(FC) -J$(OBJ) -I$(OBJ) -c $< $(FFLAGS) -o $(OBJ)strategies.o

$(OBJ)game.o: $(GAMEPATH)game.f90
	$(FC) -J$(OBJ) -I$(OBJ) -c $< $(FFLAGS) -o $(OBJ)game.o

$(BIN)game.exe: $(OBJ)game.o $(OBJ)strategies.o $(OBJ)rules.o
	$(FC) $^ $(FFLAGS) $(LDFLAGS) $(LIBS) -o $(BIN)game.exe 

run: $(BIN)game.exe
	time $(BIN)game.exe

.PHONY: clean

clean:
	rm $(OBJ)*.o $(BIN)*.exe
