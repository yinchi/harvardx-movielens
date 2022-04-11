Bookdown project for partial fufilment of Harvard 125.9x "Data Science: Capstone" requirements.
Builds and evaluates recommender systems for the [MovieLens](https://grouplens.org/datasets/movielens/)
dataset.  This was split into a main and valiation set using a fixed seed as part of the project
specifications.

An Rcpp implementation of [Simon Funk's matrix factorization algorithm](https://sifter.org/~simon/journal/20061211.html)
was applied to the rating residuals after user, movie, genre, and time-based biases were removed.
The final RMSE (root mean squared error) of the developed model as compared against the validation set was:

### 0.7939817

compared to a target RMSE of 0.86490 set by the course.
