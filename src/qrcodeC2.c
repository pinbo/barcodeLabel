/* 
 * QR Code generator demo (C)
 * 
 * Run this command-line program with no arguments. The program
 * computes a demonstration QR Codes and print it to the console.
 * 
 * Copyright (c) Project Nayuki. (MIT License)
 * https://www.nayuki.io/page/qr-code-generator-library
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 * - The above copyright notice and this permission notice shall be included in
 *   all copies or substantial portions of the Software.
 * - The Software is provided "as is", without warranty of any kind, express or
 *   implied, including but not limited to the warranties of merchantability,
 *   fitness for a particular purpose and noninfringement. In no event shall the
 *   authors or copyright holders be liable for any claim, damages or other
 *   liability, whether in an action of contract, tort or otherwise, arising from,
 *   out of or in connection with the Software or the use or other dealings in the
 *   Software.
 */

// to compile: R CMD SHLIB  qrcodeC2.c qrcodegen.c

#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "qrcodegen.h"

#include <R.h>
#include <Rinternals.h>

// Creates a single QR Code, return a matrix (actually a vector in the C code) of 0 and 1.
SEXP qrcode2(SEXP text, SEXP errCor) {
    const char* text_s = CHAR(asChar(text));
    int ecc = asInteger(errCor); // 1=Low, 2=Med, 3=Quar, 4=High
    // printf("Input text is: %s\n", text_s);
    enum qrcodegen_Ecc errCorLvl; // = qrcodegen_Ecc_LOW;  // Error correction level
    if (ecc == 1) errCorLvl = qrcodegen_Ecc_LOW;
    else if (ecc == 2) errCorLvl = qrcodegen_Ecc_MEDIUM;
    else if (ecc == 3) errCorLvl = qrcodegen_Ecc_QUARTILE;
    else if (ecc == 4) errCorLvl = qrcodegen_Ecc_HIGH;
    else {
        errCorLvl = qrcodegen_Ecc_LOW;
        printf("Wrong number for error correction!\nPlease provide number 1 to 4.\nUse level 1.\n");
    }
    // Make and print the QR Code symbol
    uint8_t qrcode[qrcodegen_BUFFER_LEN_MAX];
    uint8_t tempBuffer[qrcodegen_BUFFER_LEN_MAX];
    bool ok = qrcodegen_encodeText(text_s, tempBuffer, qrcode, errCorLvl,
        qrcodegen_VERSION_MIN, qrcodegen_VERSION_MAX, qrcodegen_Mask_AUTO, true);
    if (ok) {
        int size = qrcodegen_getSize(qrcode);
        int border = 1;
        int ll = size + 2 * border;
        SEXP qr = PROTECT(allocMatrix(INTSXP, ll, ll));
        int *pout = INTEGER(qr);
        // printf("QR code dimension is %d * %d\n", ll, ll);
        int i = 0;
        for (int y = -border; y < size + border; y++) {
            for (int x = -border; x < size + border; x++) {
                pout[i] = qrcodegen_getModule(qrcode, x, y) ? 0 : 1;
                // printf("%d", pout[i]);
                i++;
            }
        // printf("\n");
        }
        UNPROTECT(1); // for string vector
        return qr;
    } else {return errCor;}
}
