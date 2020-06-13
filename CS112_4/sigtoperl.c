// $Id: sigtoperl.c,v 1.8 2015-11-03 13:58:40-08 - - $
#define _GNU_SOURCE
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
int main (void) {
   system ("echo '#' `uname -ops`");
   printf ("my %%strsignal = (\n");
   for (int sig = 0; sig < _NSIG; ++sig) {
      char *strsig = strsignal (sig);
      if (strsig == NULL) continue;
      printf ("%5d => \"%s\",\n", sig, strsig);
   }
   printf (");\n");
   return 0;
}