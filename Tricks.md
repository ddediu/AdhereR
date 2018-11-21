# This document contains various tricks, including for building the package for `CRAN`

## Compress PDF vignettes while keeping (hyper)links

When using PDF vignettes, the check system may complain that those PDFs are too bg and that one should use `tools::compactPDF(gs_quality = "ebook")` to compress them, but when doing so, the (hyper)links in the document are lost.
The solution is to pass an extra argument: `tools::compactPDF([PDF_FILE_NAME], gs_quality = "ebook", gs_extras="-dPrinted=false")` (see, for example, [here](https://tex.stackexchange.com/questions/456896/set-the-print-flag-on-links-with-hyperref-to-preserve-them-with-ghostscript-9) for details) .

## Make sure to install vignettes from `RStudio`

As described [here](https://yihui.name/knitr/demo/vignette/), by default `devtools` do not install the vignettes; for that one should run `devtools::install(build_vignettes = TRUE)`.

## HTML vignettes too big

This is most probably due to the high quality of the images and can be fixed by adding the following code chunk at the top of the file:

```{r, echo=FALSE, message=FALSE, warning=FALSE, results='hide'}
# Various Rmarkdown output options:
# center figures and reduce their file size:
knitr::opts_chunk$set(fig.align = "center", dpi=100, dev="jpeg"); 
```


