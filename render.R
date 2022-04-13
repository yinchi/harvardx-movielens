bookdown::render_book(output_format = "all")
file.rename('movielens.Rmd', 'docs/movielens.Rmd')
knitr::purl('docs/movielens.Rmd', 'docs/movielens.R')
file.copy('svd.cpp', 'docs/svd.cpp', overwrite = T)
