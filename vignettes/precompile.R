# Precompile vignettes that take a long time to run.
#
# Method from https://ropensci.org/blog/2019/12/08/precompute-vignettes/.
#
# Each `vignettes/<name>.Rmd.orig` is the source of a precompiled vignette.
# It is knitted to `vignettes/<name>.Rmd`, a markdown file with the results
# already embedded, so the code is not re-run by R CMD build, R CMD check or
# pkgdown. Edit the `.Rmd.orig` file, not the `.Rmd` file.
#
# Re-knitting is automated by `.github/workflows/render-vignettes.yaml`
# (on changes to DESCRIPTION, a `.Rmd.orig` file or this script, and on manual
# dispatch), which opens a PR with the updated output when run on main.

# run from the package root
knitr::knit(
  "vignettes/parameter-sweep.Rmd.orig",
  output = "vignettes/parameter-sweep.Rmd"
)
