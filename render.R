bookdown::render_book()
file.rename('movielens.Rmd', 'docs/movielens.Rmd')
knitr::purl('docs/movielens.Rmd', 'docs/movielens.R')
file.copy('svd.cpp', 'docs/svd.cpp')
