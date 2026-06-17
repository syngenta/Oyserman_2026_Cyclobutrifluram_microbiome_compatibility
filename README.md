# Source code - Compatibility of Cyclobutrifluram with arbuscular mycorrhiza and soil microbiomes

This project contains the code used to generate the results and figures in the paper "Compatibility
of Cyclobutrifluram with arbuscular mycorrhiza and soil microbiomes".
For the paper, we used `R 4.6.0` to execute the scripts.


## How to run

After cloning this project, load it into your favourite IDE (e.g., RStudio).
Make sure the `data_files/` directory contains the required data.
These can be found in either the supplements of the paper or in the repo as files stored in
[git-lfs](https://git-lfs.com/) (make sure you have this set up properly in case you go this route).

With the R working directory set to the root of this project (default behaviour of RStudio when
loading the project), install the dependencies using [renv](https://rstudio.github.io/renv/articles/renv.html): `renv::restore()`.

Execute the `src/000_runall.R` script to run everything top-to-bottom, or pick any individual script
and execute it.


## Figure compilation

By default, all output figures are exported as EPS files into the `results/` directory.
As a convenience function, you can automatically render these to PNG using the included `Makefile`.
This uses [ImageMagick](https://imagemagick.org/), so make sure this is installed and on your `$PATH` beforehand.
When ready, simply run `make` in the project root to render all figures to PNG files, which will be stored in a subdirectoy under `results/`.
`make clean` will remove the PNG files from this directory, if desired.
