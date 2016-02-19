FC=ifort
FFLAGS=-c -module mod/
LDFLAGS=
SOURCES=$(wildcard src/*.f90)
OBJECTS=$(addprefix obj/,$(notdir $(SOURCES:.f90=.o)))
EXECUTABLE=START

all:$(SOURCES) $(EXECUTABLE)

$(EXECUTABLE): $(OBJECTS)
	$(FC) $(LDFLAGS) $^ -o $@

obj/%.o: src/%.f90
	$(FC) $(FFLAGS) $< -o $@

# clean: 
# 	rm obj/*.o 

