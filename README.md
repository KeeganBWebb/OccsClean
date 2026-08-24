# OccsClean

Guided Shiny app in R for cleaning biodiversity species occurrence data. Built for researchers and students who have little coding experience and want established R cleaning methods without having to dive into coding (yet...).

**Early development** (`0.3.0`). OccsClean never overwrites your uploaded file, exports are separate downloads.

## Walkthrough

New to OccsClean? This short video walks through the app workflow from import to export.

[![Watch the OccsClean walkthrough on YouTube](https://img.youtube.com/vi/YFFRodQXWQg/hqdefault.jpg)](https://www.youtube.com/watch?v=YFFRodQXWQg)

[Watch on YouTube](https://www.youtube.com/watch?v=YFFRodQXWQg)

## Install

```r
# remotes
install.packages("remotes")
remotes::install_github("KeeganBWebb/OccsClean")

# or pak
install.packages("pak")
pak::pak("KeeganBWebb/OccsClean")
```

### Launching the app

```r
OccsClean::run_app()
```

## License

MIT
