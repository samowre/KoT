#include <stdio.h>
#include "picosat.h"


int main() {

char *file = "test.cnf";
char *proof_name = "proof.cnf";

FILE *f = fopen(file, "w");
FILE *proof = fopen(proof_name, "w");

picosat_init();
picosat_enable_trace_generation();

picosat_add(1);
picosat_add(2);
picosat_add(0);

//picosat_add(-1);
picosat_add(-2);
picosat_add(0);

//picosat_add(-1);
//picosat_add(2);
//picosat_add(0);

//picosat_add(1);
//picosat_add(-2);
//picosat_add(0);

//picosat_set_verbosity (1000);



picosat_print (f);

fclose(f);

int res = picosat_sat (-1);

picosat_write_compact_trace (proof);

fclose(proof);

printf("res = %i\n", res);

return 0;

}
