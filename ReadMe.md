# `AdhereR`: two `R` packages for computing adherence to medications and for visualising medication histories

This is a collection of two packages for the [`R`](https://www.r-project.org) open-source statistical environment implementing various ways of computing adherence to medications and visualisations of medication histories, both interactive (intended for data exploration) and high quality (intended for publication).

The first package, named **`AdhereR`**, implements the actual computations and plotting, while the second package, named **`AdhereRViz`**, implements an interactive graphical user interface (a **GUI**) that allows the user to explore various datasets, computations and visualisations. The two packages are split so that, one the one hand, their development can be decoupled, and, on the other, to avoid the installation of all the dependencies needed only for the interactive GUI by those users wishing (or forced to) access to the actual computations and plotting only (without interactivity). For example, in some server/batch processing settings or on older systems, not all bells and whistles required by an interactive GUI can or should be installed...

More information is given in the packages' various vignettes and in the accompanying paper:

> [Dima AL, Dediu D (2017). Computation of adherence to medication and visualization of medication histories in R with AdhereR: Towards transparent and reproducible use of electronic healthcare data. *PLoS ONE*, **12**(4): e0174426. doi:10.1371/journal.pone.0174426](http://journals.plos.org/plosone/article?id=10.1371/journal.pone.0174426)

While the packages can be created from source or installed using the source packages (`.tar.gz`) provided in this repository, we recommend that users rather install them directly from [The Comprehensive R Archive Network (CRAN)](https://cran.r-project.org/) using the normal procedure<sup>1</sup> (i.e., `install.packages("AdhereR", dep=TRUE)` and/or `install.packages("AdhereRViz", dep=TRUE)`).

The packages are implemented in pure `R`<sup>2</sup>, but optimised to work on mid-range hardware even for large databases (stored, for example, in an `SQL` `RDBMS`), single-threaded or parallelized in large heterogeneous clusters (using, for example, `Apache Hadoop`), and runs on any platform that supports `R`. 
A standardized interface allows `AdhereR` to be used transparently from other programming languages or platforms, and a fully functional bridge to `Psython 3` is provided as an example.

All the code and example data are released under [GPL v3](https://www.gnu.org/licenses/gpl-3.0-standalone.html).

Feedback is welcome, preferably by using the GitHub's *Issues* system for bug reporting and suggestions.

For what is new in each release, please see the `NEWS.md` file of the concerned package.

Copyright (by period, in alphabetical order by family name; please see individual files for details) (C):

  - 2015-2018: Alexandra Dima, Dan Dediu
  - 2018-2019: Samuel Allemann, Alexandra Dima, Dan Dediu

-----

<sup>1</sup> There might be a delay between the release of new versions here and their availability on CRAN; in this case, we would recommend, if possible, to wait for the version to appear on CRAN.

<sup>2</sup> We also use some `Python` for the Python 3 bridge, and `HTML`/`JavaScript`/`CSS`/`SVG` for generating SVG plots and for the interactive GUI.

