# `AdhereR`: an `R` package for computing adherence to medications and for visualising medication histories

This is a package for the [`R`](https://www.r-project.org) open-source statistical environment implementing various ways of computing adherence to medications and visualisations of medication histories, both interactive (intended for data exploration) and high quality (intended for publication).

More information is given in the package's various vignettes and in the accompanying paper:

> [Dima AL, Dediu D (2017). Computation of adherence to medication and visualization of medication histories in R with AdhereR: Towards transparent and reproducible use of electronic healthcare data. *PLoS ONE*, **12**(4): e0174426. doi:10.1371/journal.pone.0174426](http://journals.plos.org/plosone/article?id=10.1371/journal.pone.0174426)

While the package can be created from the source or installed using the source package **AdhereR_X.tar.gz** provided in this repository (where **X** refers to the package's version), we recommend that users install it directly from [The Comprehensive R Archive Network (CRAN)](https://cran.r-project.org/) using the normal procedure<sup>1</sup> (i.e., `install.packages("AdhereR", dep=TRUE)`).

The package is implemented in pure `R`, but optimised to work on mid-range hardware even for large databases (stored, for example, in an `SQL` `RDBMS`), single-threaded or parallelized in large heterogeneous clusters (using, for example, `Apache Hadoop`), and runs on any platform that supports `R`. 
A standardized interface allows `AdhereR` to be used transparently from other programming languages or platforms, and a fully functional bridge to `Psython 3` is provided as an example.
The code is released under [GPL v3](https://www.gnu.org/licenses/gpl-3.0-standalone.html).

Feedback is welcome either directly by e-mailing us or by using GitHub's system for bug reporting and suggestions.

For what is new in each release, please see the `NEWS.md` file.

Copyright (by period, in alphabetical order by family name; please see individual files for details) (C):

  - 2015-2018: Alexandra Dima, Dan Dediu
  - 2018: Samuel Allemann, Alexandra Dima, Dan Dediu

-----

<sup>1</sup> There might be a delay between the release of new versions here and their availability on CRAN; in this case, we would recommend, if possible, to wait for the version to appear on CRAN.
