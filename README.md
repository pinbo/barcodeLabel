# barcodeLabel
A simple R package for QR code generation and barcode label sheet design (such as Avery label sheets).

A shiny app for making labels is available here:
https://junli.shinyapps.io/label_barcodes/

## Why did I make this package?

I need to print a lot of barcode labels with Avery 5967 and 5960. I tried the R package [baRcodeR](https://github.com/ropensci/baRcodeR) but found it is very slow to create QR codes. To speed up the QR code generation, I found the [C version of QR generator](https://github.com/nayuki/QR-Code-generator) and used it in this R package.

## Installation

You can install the package with 3 methods:

1. Install the development version from GitHub with "devtools" within R:
``` sh
# install.packages("devtools")
devtools::install_github("pinbo/barcodeLabel")
```

2. Git clone the development version and build by yourself:
``` sh
# in your shel terminal
git clone https://github.com/pinbo/barcodeLabel.git

# remove the old version if you have installed
R CMD REMOVE barcodeLabel
# build
R CMD build barcodeLabel
# install
R CMD INSTALL barcodeLabel
```
3. Download the prebuilt [releases](https://github.com/pinbo/barcodeLabel/releases/) and install it manually:
``` sh
# within R
install.packages(path_to_tar.gz_file, repos = NULL, type="source")
# OR within a shell terminal
R CMD INSTALL path_to_tar.gz_file
```
## Quick Start

``` r
# to make an QR code
library(barcodeLabel)
qrout <- qrcode("Hello, world!", 1)
plotqr(qrout)

# to create labels for printing
# check the example in ?make_custom_label
# or the R codes from the Shiny app for https://junli.shinyapps.io/label_barcodes/
```
## Updates
- 2023-09-30: v0.8.0. Add metric length unit "mm" support for those not using "inch"
- 2023-02-12: v0.7.0. Added string wrap support. You can still manually set a small font size to fit all inital strings in one line.
- 2023-02-04: v0.6.0. Make exact layout of labels on the page; add font color and only text option; add "tough-spots-3/8inch" preset; and circle border.
- 2022-11-23: v0.5.0. Replace Data Matrix C code from [Datamatrix](https://github.com/revk/Datamatrix) with C code from [iec16022](https://github.com/rdoeffinger/iec16022) due to a bug with long strings.
- 2022-11-15: v0.4.0. Add Data Matrix code and scaling for 2D barcode.
- 2022-11-06: v0.3.0. Add support for mixed fontfaces (bold, italic and both) with function `richtext`.

## Acknowledgements
Special thanks to the authors of following packages and repositories.
1. [R package baRcodeR](https://github.com/ropensci/baRcodeR): most of the makeLabel codes are from there.
2. [QR-Code-generator](https://github.com/nayuki/QR-Code-generator): I get the codes for the C version qrcode function.
3. [R package qrcode](https://github.com/ThierryO/qrcode) : a lot of insparation from this package.
4. [R package roxygen2](https://github.com/r-lib/roxygen2): Documents were made with command `roxygen2::roxygenise()`
5. [iec16022](https://github.com/rdoeffinger/iec16022): get the C codes for making Data Matrix barcodes.
6. [Datamatrix](https://github.com/revk/Datamatrix): get the idea how to make square datamatrix C codes for making Data Matrix barcodes.