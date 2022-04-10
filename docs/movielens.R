## ----Load-libraries-------------------------------------------------------------------------------------------------------------------------

# R 4.1 key features: new pipe operator, \(x) as shortcut for function(x)
# R 4.0 key features: stringsAsFactors = FALSE by default, raw character strings r"()"
if (packageVersion('base') < '4.1.0') {
  stop('This code requires R >= 4.1.0!')
}

if(!require("pacman")) install.packages("pacman")
library(pacman)

# Ensure packages are installed but do not load them
p_install(Rcpp, force = F)
p_install(RcppArmadillo, force = F)
p_install(RcppProgress, force = F)
p_install(mgcv, force = F)          # provides mgcv::gam and mgcv::predict.gam
p_install(raster, force = F)        # provides raster::clamp

# Load these packages
p_load(magrittr, knitr, kableExtra, data.table, latex2exp, patchwork,
       tidyverse, caret, lubridate)


## ----Create-edx-and-validation-sets---------------------------------------------------------------------------------------------------------

##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

# NOTE: this code was modified from the course-provided version for speed.

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip


# STEP 1: download and unzip 'ml-10m.zip' as necessary

if(!dir.exists("ml-10M100K")) dir.create("ml-10M100K")
dl <- "ml-10M100K/ratings.zip"
if(!file.exists(dl))
  download.file("https://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings_file <- "ml-10M100K/ratings.dat"
if(!file.exists(ratings_file)) unzip(dl, ratings_file)

movies_file <- "ml-10M100K/movies.dat"
if(!file.exists(movies_file)) unzip(dl, movies_file)


# STEP 2a: Load the ratings file.  This file is delimited using double colons.

ratings <- str_split(read_lines(ratings_file), fixed("::"), simplify = T) |> 
  as.data.frame() |> 
  set_colnames(c("userId", "movieId", "rating", "timestamp")) |>
  mutate(userId = as.integer(userId),
         movieId = as.integer(movieId),
         rating = as.numeric(rating),
         timestamp = as_datetime(as.integer(timestamp)))


# STEP 2b: Load the movies file.  Again, this file is delimited using double colons.
movies <- str_split(read_lines(movies_file), fixed("::"), simplify = T) |> 
  as.data.frame() |> 
  set_colnames(c("movieId", "title", "genres")) |>
  mutate(movieId = as.integer(movieId))


# STEP 3: Join the `ratings` and `movies` data tables and save to `movielens`.
movielens <- left_join(ratings, movies, by = "movieId")


# STEP 4: Split the `movielens` dataset into the `edx` and `validation` sets.

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp |> semi_join(edx, by = "movieId") |> semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)


# STEP 5; convert timestamps to datetime
edx <- edx |> mutate(timestamp = as_datetime(timestamp)) |> as.data.table()
validation <- validation |> mutate(timestamp = as_datetime(timestamp)) |> as.data.table()

rm(dl, ratings, movies, test_index, temp, movielens, removed)


## ----Colnames-------------------------------------------------------------------------------------------------------------------------------

colnames(edx)


## ----Earliest-half-star-rating--------------------------------------------------------------------------------------------------------------

temp <- edx[edx$rating %% 1 == 0.5]
temp[which.min(temp$timestamp)] |> kable(align='rrrrll', booktabs = T) |> row_spec(0, bold = T)


## ----Matrix-density-------------------------------------------------------------------------------------------------------------------------

nrow(edx) / max(edx$userId) / max(edx$movieId)


## ----Ratings-per-movie-and-user, fig.height=3, fig.width=6----------------------------------------------------------------------------------

# Count the number of ratings for each movie and rank the movies by ratings received
ratings_per_movie <- edx |>
  group_by(movieId) |>
  summarise(title = first(title), genres = first(genres), n_ratings = n()) |>
  mutate(rank = frank(-n_ratings))

# Count the number of ratings for each user and rank the users by ratings given
ratings_per_user <- edx |>
  group_by(userId) |>
  summarise(n_ratings = n()) |>
  mutate(rank = frank(-n_ratings))

# Plot the number of ratings for each movie and user, sorted by rank
plot1 <- ratings_per_movie |>
  ggplot(aes(rank,n_ratings)) + geom_line() +
  scale_x_log10() + scale_y_log10() +
  xlab('Rank') + ylab('Number of ratings') + labs(title = 'Ratings per movie')
plot2 <- ratings_per_user |>
  ggplot(aes(rank,n_ratings)) + geom_line() +
  scale_x_log10() + scale_y_log10() +
  xlab('Rank') + ylab('Number of ratings') + labs(title = 'Ratings per user')

rm(ratings_per_movie, ratings_per_user)
par(cex = 0.7)
plot1 + plot2


## ----Genre-summary--------------------------------------------------------------------------------------------------------------------------

# Get the list of possible genres

genres <- edx$genres |> unique() |> str_split('\\|') |> flatten_chr() |>
  unique() |> sort() |> 
  tail(-1) # remove "(no genres listed)"

# Construct a data.table with one entry per movie
temp <- edx |> group_by(movieId) |>
  summarise(title = first(title), genres = first(genres))

# Find the number of movies and ratings for each genre
genre_summary <-
  data.table(
    Genre = genres,
    Movies = sapply(genres, function(g)
      sum(temp$genres %flike% g)),
    Ratings = sapply(genres, function(g)
      sum(edx$genres %flike% g)),
    "Mean Rating" = sapply(genres, function(g) {
      edx[edx$genres %flike% g,'rating']$rating |> mean()
    })
  ) |>
  mutate("Ratings per movie" = Ratings / Movies)

rm(temp)
genre_summary |> arrange(desc(`Mean Rating`)) |>
  kable(align='lrrrr', digits = c(0,0,0,2,1), booktabs = T, linesep = "") |> row_spec(0, bold = T)


## ----Genre-counts-hist, fig.height=3, fig.width=4-------------------------------------------------------------------------------------------

genre_counts <- table(str_count(edx$genres, '\\|') + 1 - str_count(edx$genres, 'no genres'))
par(cex = 0.7)
barplot(genre_counts, xlab = 'Number of genres', ylab = 'Count', main = 'Genres per movie')


## ----Genre-counts---------------------------------------------------------------------------------------------------------------------------

sum(genre_counts[c('2','3')])/sum(genre_counts)


## ----EDX-split, warning=FALSE---------------------------------------------------------------------------------------------------------------

# Test set will be 10% of edx data
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.1, list = FALSE)
edx_train <- edx[-test_index,]
temp <- edx[test_index,]

# Make sure userId and movieId in edx_test set are also in edx_train set
edx_test <- temp |>
  semi_join(edx_train, by = "movieId") |>
  semi_join(edx_train, by = "userId") |>
  as.data.table()

# Add rows removed from edx_test set back into edx_train set
removed2 <- anti_join(temp, edx_test)
edx_train <- rbind(edx_train, removed2) |> as.data.table()

rm(removed2, temp, test_index)


## ----Mean-only-model------------------------------------------------------------------------------------------------------------------------

mu <- mean(edx_test$rating)
mu


## ----Mean-only-RMSE-------------------------------------------------------------------------------------------------------------------------

# When multiple effects (movie, user, genre) are added in our model, some predictions
# may fall out of the valid range.  This function fixes these predictions to the range
# [0.5, 5].
clamp <- function(x) raster::clamp(as.numeric(x), 0.5, 5)

# Compute RMSE and add to a tibble.
RMSEs <- tibble(Method = c("Mean only"),
                RMSE = RMSE(mu, edx_test$rating),
                "RMSE (clamped estimates)" = RMSE(mu, edx_test$rating))
RMSEs[[nrow(RMSEs),'RMSE']]


## ----Movie-effects, fig.height=3, fig.width=4-----------------------------------------------------------------------------------------------

# Least-squares estimate of movie effect is the mean of (rating - mu) for all
# ratings of that movie.
movie_biases <- edx_train |> 
  group_by(movieId) |> 
  summarize(b_i = mean(rating - mu))

# Plot a histogram of the movie effects
par(cex = 0.7)
hist(movie_biases$b_i, 30, xlab = TeX(r'[$\hat{b}_{1,i}$]'),
     main = TeX(r'[Histogram of $\hat{b}_{1,i}$]'))


## ----Movie-effects-RMSE---------------------------------------------------------------------------------------------------------------------

# Obtain predictions for the edx_test set
predicted_ratings <- edx_test |> 
  left_join(movie_biases, by='movieId') |>
  mutate(pred = mu + b_i) |> pull(pred)

# Compute RMSE and add to data.table
RMSEs <- RMSEs |>
  add_row(Method = "Movie effects",
          RMSE = RMSE(predicted_ratings, edx_test$rating),
          "RMSE (clamped estimates)" = RMSE(clamp(predicted_ratings), edx_test$rating))

RMSEs[nrow(RMSEs),] |> kable(align='lrr', booktabs = T) |> row_spec(0, bold = T)


## ----User-effects, fig.height=3, fig.width=4------------------------------------------------------------------------------------------------

# Estimate user effects
user_biases <- edx_train |> 
  left_join(movie_biases, by='movieId') |>
  group_by(userId) |>
  summarize(b_u = mean(rating - mu - b_i))

# Plot a histogram of the user effects
par(cex = 0.7)
hist(user_biases$b_u, 30, xlab = TeX(r'[$\hat{b}_{2,u}$]'),
     main = TeX(r'[Histogram of $\hat{b}_{2,u}$]'))


## ----User-effects-RMSE----------------------------------------------------------------------------------------------------------------------

# Obtain predictions for the edx_test set
predicted_ratings <- edx_test |> 
  left_join(movie_biases, by='movieId') |>
  left_join(user_biases, by='userId') |>
  mutate(pred = mu + b_i + b_u) |> pull(pred)

# Compute RMSE and add to data.table
RMSEs <- RMSEs |>
  add_row(Method = "Movie + user effects",
          RMSE = RMSE(predicted_ratings, edx_test$rating),
          "RMSE (clamped estimates)" = RMSE(clamp(predicted_ratings), edx_test$rating))
RMSEs[nrow(RMSEs),] |> kable(align='lrr', booktabs = T) |> row_spec(0, bold = T)


## ----Genre-effects, fig.height=3, fig.width=4-----------------------------------------------------------------------------------------------

# Estimate genre effects
genre_biases <- edx_train |> 
  left_join(movie_biases, by='movieId') |>
  left_join(user_biases, by='userId') |>
  group_by(genres) |>
  summarize(b_g = mean(rating - mu - b_i - b_u))

# Plot a histogram of the genre effects
par(cex = 0.7)
hist(genre_biases$b_g, 30, xlab = TeX(r'[$\hat{b}_{3,g}$]'),
     main = TeX(r'[Histogram of $\hat{b}_{3,g}$]'))


## ----Genre-effects-RMSE---------------------------------------------------------------------------------------------------------------------

# Obtain predictions for the edx_test set
predicted_ratings <- edx_test |> 
  left_join(movie_biases, by='movieId') |>
  left_join(user_biases, by='userId') |>
  left_join(genre_biases, by='genres') |>
  mutate(pred = mu + b_i + b_u + b_g) |> pull(pred)

# Compute RMSE and add to data.table
RMSEs <- RMSEs |>
  add_row(Method = "Movie + user + genre effects",
          RMSE = RMSE(predicted_ratings, edx_test$rating),
          "RMSE (clamped estimates)" = RMSE(clamp(predicted_ratings), edx_test$rating))
RMSEs[nrow(RMSEs),] |> kable(align='lrr', booktabs = T) |> row_spec(0, bold = T)


## ----Time-averages, fig.height=3, fig.width=4-----------------------------------------------------------------------------------------------

# Add a week number to each rating in the edx_train and edx_test datasets
edx_train <- edx_train |>
  mutate(weekNum = (timestamp - min(timestamp)) |>
           as.numeric(unit = "days") |> {\(x) floor(x/7) + 1}() )
edx_test <- edx_test |>
  mutate(weekNum = (timestamp - min(timestamp)) |>
           as.numeric(unit = "days") |> {\(x) floor(x/7) + 1}() )

# Fit a smooth curve to the ratings as a function of time
fit <- mgcv::gam(rating ~ s(weekNum, bs = "cs"),
                 family = gaussian(), data = edx_train) # apply smoothing

# Evaluate the fitted curve for each week number
r <- seq(1,max(edx_train$weekNum))
f_t <- mgcv::predict.gam(fit, data.frame(weekNum = r)) - mu
rm(fit)

# Plot the fitted curve
ggplot(data.frame(weekNum = r, f_t), aes(weekNum, f_t)) + geom_line() +
  xlab(TeX(r'[$t_{u,i}$]')) + ylab(TeX(r'[$f\,(t_{u,i}\,)$]'))


## ----Time-effect-model-and-RMSE, fig.height=3, fig.width=4----------------------------------------------------------------------------------

# Compute the biases

movie_biases_t <- edx_train |> 
  mutate(f_t = f_t[weekNum]) |> 
  group_by(movieId) |> 
  summarize(b_i = mean(rating - mu - f_t))

user_biases_t <- edx_train |> 
  mutate(f_t = f_t[weekNum]) |> 
  left_join(movie_biases_t, by='movieId') |>
  group_by(userId) |>
  summarize(b_u = mean(rating - mu - b_i - f_t))

genre_biases_t <- edx_train |> 
  mutate(f_t = f_t[weekNum]) |> 
  left_join(movie_biases_t, by='movieId') |>
  left_join(user_biases_t, by='userId') |>
  group_by(genres) |>
  summarize(b_g = mean(rating - mu - b_i - b_u - f_t))

# Obtain predictions for the edx_test set
predicted_ratings <- edx_test |> 
  mutate(f_t = f_t[weekNum]) |> 
  left_join(movie_biases_t, by='movieId') |>
  left_join(user_biases_t, by='userId') |>
  left_join(genre_biases_t, by='genres') |>
  mutate(pred = mu + b_i + b_u + b_g + f_t) |>
  pull(pred)

# Compute RMSE and add to data.table
RMSEs <- RMSEs |>
  add_row(Method = "Movie + user + genre + time effects",
          RMSE = RMSE(predicted_ratings, edx_test$rating),
          "RMSE (clamped estimates)" = RMSE(clamp(predicted_ratings), edx_test$rating))
RMSEs[nrow(RMSEs),] |> kable(align='lrr', booktabs = T) |> row_spec(0, bold = T)


## ----Regularization, fig.height=3, fig.width=4----------------------------------------------------------------------------------------------

# List of regularization parameter values to try.
# Since I know the approximate optimal value, I added more points
# in this range.
lambdas <- c(0,1,2,3,4,seq(4.5,5.5,0.1),6,7,8,9,10)

# Compute RMSE values for each lambda using the *test set.
rmses <- sapply(lambdas, function(l){
  message("lambda = ", l)
  
  # Compute movie, user, genre, and time effects using the test set.
  # Note that f_t here refers to the variable f_t and not the f_t column in
  # any of the data.tables.
  movie_biases_reg <-
    edx_train[, .(b_i = sum(rating - mu - f_t[weekNum])/(.N+l)), by = 'movieId']
  
  temp <- movie_biases_reg[edx_train, on = 'movieId']
  user_biases_reg <-
    temp[, .(b_u = sum(rating - mu - b_i - f_t[weekNum])/(.N+l)), by = 'userId']
  
  temp <- user_biases_reg[temp, on = 'userId']
  genre_biases_reg <-
    temp[, .(b_g = sum(rating - mu - b_i - b_u - f_t[weekNum])/(.N+l)), by = 'genres']
  
  # Generate predictions
  predicted_ratings <- genre_biases_reg[
    user_biases_reg[
      movie_biases_reg[
        edx_test, on = 'movieId'],
      on = 'userId'],
    on = 'genres'] |>
    mutate(pred = mu + b_i + b_u + b_g + f_t[weekNum]) |> 
    pull(pred)
  
  # Compute RMSE
  return(RMSE(predicted_ratings, edx_test$rating))
})

# Plot RMSE against lambda
par(cex = 0.7)
qplot(lambdas, rmses, xlab = TeX(r'($\lambda)'), ylab = 'RMSE', geom = c('point', 'line'))


## ----Optimal-lambda-------------------------------------------------------------------------------------------------------------------------

lambda <- lambdas[which.min(rmses)]
lambda


## ----Regularized-model-and-RMSE-------------------------------------------------------------------------------------------------------------

movie_biases_reg <-
  edx_train[, .(b_i = sum(rating - mu - f_t[weekNum])/(.N+lambda)), by = 'movieId']

temp <- movie_biases_reg[edx_train, on = 'movieId']
user_biases_reg <-
  temp[, .(b_u = sum(rating - mu - b_i - f_t[weekNum])/(.N+lambda)), by = 'userId']

temp <- user_biases_reg[temp, on = 'userId']
genre_biases_reg <-
  temp[, .(b_g = sum(rating - mu - b_i - b_u - f_t[weekNum])/(.N+lambda)), by = 'genres']

# Generate predictions for the *edx_test* set.
predicted_ratings_reg <- genre_biases_reg[
  user_biases_reg[
    movie_biases_reg[
      edx_test, on = 'movieId'],
    on = 'userId'],
  on = 'genres'] |> 
  mutate(pred = mu + b_i + b_u + b_g + f_t[weekNum]) |> 
  pull(pred)

rm(temp)

# Compute RMSE and add to data.table
RMSEs <- RMSEs |>
  add_row(Method = "Movie + user + genre + time effects (regularized)",
          RMSE = RMSE(predicted_ratings_reg, edx_test$rating),
          "RMSE (clamped estimates)" =
            RMSE(clamp(predicted_ratings_reg), edx_test$rating))
RMSEs[nrow(RMSEs),] |> kable(align='lrr', booktabs = T) |> row_spec(0, bold = T)


## ----RMSE-summary---------------------------------------------------------------------------------------------------------------------------

RMSEs |> kable(align='lrr', booktabs = T, linesep = "") |> row_spec(0, bold = T)


## ----Funk-MF, fig.height=3, fig.width=3-----------------------------------------------------------------------------------------------------

# Residuals from previous best model
previous_train <- genre_biases_reg[
  user_biases_reg[
    movie_biases_reg[
      edx_train, on = 'movieId'],
    on = 'userId'],
  on = 'genres'] |> 
  mutate(pred = mu + b_i + b_u + b_g + f_t[weekNum]) |> 
  pull(pred)

residuals_train <- edx_train$rating - previous_train

# Test set predictions for previous best model
previous_test <- genre_biases_reg[
  user_biases_reg[
    movie_biases_reg[
      edx_test, on = 'movieId'],
    on = 'userId'],
  on = 'genres'] |> 
  mutate(pred = mu + b_i + b_u + b_g + f_t[weekNum]) |> 
  pull(pred)

# Obtain new movie and user indices **without gaps**, and save the mappings
Uidx <- numeric(max(edx_train$userId))
Uidx[unique(edx_train$userId)] = seq(uniqueN(edx_train$userId))

Vidx <- numeric(max(edx_train$movieId))
Vidx[unique(edx_train$movieId)] = seq(uniqueN(edx_train$movieId))

# Funk matrix factorization. See C++ source for full documentation.
# Values for regCoef and learningRate are as suggested by [Funk 2006].
Rcpp::sourceCpp("svd.cpp")
funk <- function(Uidx, Vidx, residuals, nFeatures, steps = 500,
                 regCoef = 0.02, learningRate = 1e-3) {
  
  # Change Uidx and Vidx to 0-based, for C++ only.
  funkCpp(Uidx[edx_train$userId] - 1,
          Vidx[edx_train$movieId] - 1,
          residuals_train,
          nFeatures, steps, regCoef, learningRate)
}

# Compute RMSE values for varying number of MF features.
set.seed(1)
if (!file.exists('funk_tuning.Rdata')) {
  nFeatures <- c(1, 2, 4, 8, seq(12,20), 24, 28, 32)
  rmses <- sapply(nFeatures, \(nF){
    
    message(nF, ' features')
    
    # Run Funk MF
    set.seed(1)
    funkResult <- funk(Uidx, Vidx, residuals_train, nFeatures = nF, steps = 500)
    U <- funkResult$U
    V <- funkResult$V
    
    # Uidx[u] is the row index of user u in matrix U
    # Vidx[v] is the row index of movie v in matrix V
    predicted_ratings_funk <- edx_test |>
      mutate(pred = previous_test +
               map2_dbl(userId, movieId, \(u,v) U[Uidx[u],] %*% V[Vidx[v],])) |>
      pull(pred)
    rmse <- RMSE(predicted_ratings_funk, edx_test$rating)
    
    message(rmse,'\n')
    return(rmse)
  })
  
  save(nFeatures,rmses, file = 'funk_tuning.Rdata')
}
set.seed(1)

load('funk_tuning.Rdata')
par(cex = 0.7)
qplot(nFeatures, rmses, xlab = 'rank(UV)', ylab = 'RMSE', geom = c('point','line'))


## ----Optimal-num-features-Funk--------------------------------------------------------------------------------------------------------------

nFeaturesOpt <- nFeatures[which.min(rmses)]
nFeaturesOpt


## ----Funk-MF-prediction---------------------------------------------------------------------------------------------------------------------

# Run Funk MF
set.seed(1)
if (!file.exists('funk.Rdata')) {
funkResult <- funk(Uidx, Vidx, residuals_train, nFeatures = nFeaturesOpt, steps = 500)
save(nFeaturesOpt, funkResult, file = 'funk.Rdata')
}
set.seed(1)

load('funk.Rdata')
U <- funkResult$U
V <- funkResult$V


# Uidx[u] is the row index of user u in matrix U
# Vidx[v] is the row index of movie v in matrix V
predicted_ratings_funk <- edx_test |>
  mutate(pred = previous_test +
           map2_dbl(userId, movieId, \(u,v) U[Uidx[u],] %*% V[Vidx[v],])) |>
  pull(pred)
rmse <- RMSE(predicted_ratings_funk, edx_test$rating)

# Compute RMSE and add to data.table
RMSEs <- RMSEs |>
  add_row(Method = "Section 2 best model + Matrix factorization",
          RMSE = RMSE(predicted_ratings_funk, edx_test$rating),
          "RMSE (clamped estimates)" = RMSE(clamp(predicted_ratings_funk), edx_test$rating))

RMSEs[nrow(RMSEs),] |> kable(align='lrr', booktabs = T) |> row_spec(0, bold = T)


## ----RMSE-summary-2-------------------------------------------------------------------------------------------------------------------------

RMSEs |> kable(align='lrr', booktabs = T, linesep = "") |> row_spec(0, bold = T)


## ----Save-model-----------------------------------------------------------------------------------------------------------------------------

save(mu, movie_biases_reg, user_biases_reg, genre_biases_reg,
     f_t, Uidx, Vidx, U, V, file = 'FINAL_model.Rdata')


## ----Num-parameters-------------------------------------------------------------------------------------------------------------------------

nrow(movie_biases_reg) + nrow(user_biases_reg) + nrow(genre_biases_reg) +
  length(f_t) + length(U) + length(V)


## ----Final-RMSE-validation-set--------------------------------------------------------------------------------------------------------------

predicted_ratings_FINAL_VALIDATION <- validation |>
  mutate(weekNum = (timestamp - min(timestamp)) |>
           as.numeric(unit = "days") |> {\(x) floor(x/7) + 1}() ) |> 
  mutate(f_t = f_t[weekNum]) |> 
  left_join(movie_biases_reg, by='movieId') |>
  left_join(user_biases_reg, by='userId') |>
  left_join(genre_biases_reg, by='genres') |>
  mutate(pred = mu + b_i + b_u + b_g + f_t +
           map2_dbl(userId, movieId, \(u,v) U[Uidx[u],] %*% V[Vidx[v],])) |>
  pull(pred) |> clamp()

# Compute RMSE and add to data.table
RMSE(predicted_ratings_FINAL_VALIDATION, validation$rating)


## ----Prediction-error-plot, fig.height=3, fig.width=4---------------------------------------------------------------------------------------

par(cex = 0.7)
hist(predicted_ratings_FINAL_VALIDATION - validation$rating, 50,
     xlab = 'Prediction Error', main = '')


## ----Absolute-error-ECDF, fig.height=3, fig.width=3-----------------------------------------------------------------------------------------

the_ecdf <- ecdf(predicted_ratings_FINAL_VALIDATION - validation$rating)
par(cex = 0.7)
qplot(seq(0,4.5,0.001), the_ecdf(seq(0,4.5,0.001)),
     xlab = 'Absoulute error of prediction', ylab = 'Empirical CDF', geom = 'line')


## ----Within-half-star-----------------------------------------------------------------------------------------------------------------------

mean(abs(predicted_ratings_FINAL_VALIDATION - validation$rating) < 0.5)


## ----Confusion-matrix-3.5-------------------------------------------------------------------------------------------------------------------

confusionMatrix(as.factor(ifelse(predicted_ratings_FINAL_VALIDATION >= 3.5, 'Good', 'Bad')),
                as.factor(ifelse(validation$rating >= 3.5, 'Good', 'Bad')),
                positive = 'Good')






## -------------------------------------------------------------------------------------------------------------------------------------------
sessionInfo()


## -------------------------------------------------------------------------------------------------------------------------------------------
tidyverse::tidyverse_conflicts()

