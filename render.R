bookdown::render_book()
file.rename('movielens.Rmd', 'bookdown_output/movielens.Rmd')
knitr::purl('bookdown_output/movielens.Rmd', 'bookdown_output/movielens.R')
file.copy('svd.cpp', 'bookdown_output/svd.cpp')
