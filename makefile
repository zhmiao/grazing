FC=ifort
FFLAGS=-module obj/ -c
LDFLAGS=-O3

MOD_SRC = $(wildcard src/mod/*.f90)
MOD_OBJ = $(addprefix obj/,$(notdir $(MOD_SRC:.f90=.o)))
FUNC_SRC = $(wildcard src/func/*.f90)
FUNC_OBJ = $(addprefix obj/,$(notdir $(FUNC_SRC:.f90=.o)))
MAIN_SRC = $(wildcard src/*.f90)
MAIN_OBJ = $(addprefix obj/,$(notdir $(MAIN_SRC:.f90=.o)))
EXE=RUN

all: $(EXE)

$(EXE): $(MOD_OBJ) $(FUNC_OBJ) $(MAIN_OBJ)
	$(FC) $(LDFLAGS) -o $@ $^

obj/%.o: src/func/%.f90 $(MOD_OBJ)
	$(FC) $(LDFLAGS) $(FFLAGS) $< -o $@

obj/%.o: src/%.f90 $(MOD_OBJ)
	$(FC) $(LDFLAGS) $(FFLAGS) $< -o $@

obj/%.o : src/mod/%.f90
	$(FC) $(LDFLAGS) $(FFLAGS) $< -o $@

# # clean: 
# # rm obj/*.o obj/*.mod 
