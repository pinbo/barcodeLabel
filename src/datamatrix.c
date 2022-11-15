// IEC16022 bar code generation
// Copyright (c) 2007 Adrian Kennard Andrews & Arnold Ltd
// This software is provided under the terms of the GPL v2 or later.
// This software is provided free of charge with a full "Money back" guarantee.
// Use entirely at your own risk. We accept no liability. If you don't like that - don't use it.

// TBA, structured append, and ECI options.
// TBA, library for odd size ECC000-140 codes

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include "iec16022ecc200.h"

#include <R.h>
#include <Rinternals.h>
SEXP dmcode(SEXP text, SEXP square)
{
   const char* barcode = CHAR(asChar(text));
   unsigned int sq = asInteger(square);
   unsigned int W = 0, H = 0;
   int barcodelen = 0;
   char *encoding = NULL;
   unsigned int len = 0,
       maxlen = 0,
       ecclen = 0,
       noquiet = 0;;
   unsigned char *grid = 0;
   barcodelen = strlen(barcode);
   // make grid
    grid = iec16022ecc200(&W, &H, &encoding, barcodelen, (unsigned char *) barcode, &len, &maxlen, &ecclen, sq, noquiet);
   // output
   if (!grid || !W)
      printf("No barcode produced\n");
   SEXP dm = PROTECT(allocMatrix(INTSXP, H, W));
   int *pout = INTEGER(dm);
   printf("Size    : %dx%d\n", W, H);
   // printf("Encoded : %d of %d bytes with %d bytes of ecc\n", len, maxlen, ecclen);
   // printf("Barcode : %s\n", barcode);
   // printf("Encoding: %s\n", encoding ? : "");

   int i = 0;
   for (int y = 0; y < H; y++) // y is row
   {
      for (int x = 0; x < W; x++) // x is column
      {
         // printf("%s", y < H && y >= 0 && x < W && x >= 0 && grid[W * y + x] ? 1 : 0);
         pout[i] = grid[W * y + x] ? 0 : 1;
         i++;
      }
      // printf("\n");
   }
   free(grid);
   UNPROTECT(1); // for string vector
   return dm;

}

