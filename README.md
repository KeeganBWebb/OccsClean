# OccsClean

Guided Shiny app in R for cleaning biodiversity species occurrence data. Built for researchers and students who have little coding experience and want established R cleaning methods without having to dive into coding (yet...).

**Early development** (`0.0.10`). OccsClean never overwrites your uploaded file, exports are separate downloads.

## Install

```r
# remotes
install.packages("remotes")
remotes::install_github("KeeganBWebb/OccsClean")

# or pak
install.packages("pak")
pak::pak("KeeganBWebb/OccsClean")
```

```r
OccsClean::run_app()
```

## License

MIT
