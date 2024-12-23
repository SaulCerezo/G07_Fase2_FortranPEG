.PHONY: clean

test: test.o tokenizer.o
	gfortran -o $@ $^

test.o: test.f90 tokenizer.o tokenizer.mod 
	gfortran -c $<

tokenizer.o tokenizer.mod: tokenizer.f90
	gfortran -c $<

clean:
	rm *.o *.mod