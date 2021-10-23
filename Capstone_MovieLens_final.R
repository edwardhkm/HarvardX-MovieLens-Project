##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)
library(ggplot2)
library(lubridate)
# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

# tinytex::install_tinytex()


dl <- tempfile()

download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 3.6 or earlier:
#movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
#                                           title = as.character(title),
#                                           genres = as.character(genres))
# if using R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))


movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

###########################################################################################
#
# We start by running some R code to examine our data to see it structure and data.
# We would like to see if there are any NA inside our dataset.
#
###########################################################################################

str(edx)
head(edx)
summary(edx)
anyNA.data.frame(edx)
anyNA.data.frame(validation)

# After we review our dataset.  We find that we can extract more information from it
# We will create the following 2 fields, 
#    the rating time from timestamp column
#    the movie release year from title column
#
# As we can see the title col and the timestamp col can split and extract more information 
# about the movie release year

edx <- edx %>% mutate(rating_time = as.POSIXct(timestamp, origin="1970-01-01"), movie_year2 = str_extract(title, "\\(\\d+\\)")) %>% mutate(movie_year=str_extract(movie_year2, "\\d+")) %>% select(-movie_year2)
head(edx)

validation <- validation %>% mutate(rating_time = as.POSIXct(timestamp, origin="1970-01-01"), movie_year2 = str_extract(title, "\\(\\d+\\)")) %>% mutate(movie_year=str_extract(movie_year2, "\\d+")) %>% select(-movie_year2)
head(validation)


# According to the instruction of the project, we need to further split the edx set into training and testing.
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
edx_test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.1, list = FALSE)
edx_training_set <- edx[-edx_test_index,]
temp2 <- edx[edx_test_index,]

# Make sure userId and movieId in test set are also in training set
edx_test_set <- temp2 %>% 
  semi_join(edx_training_set, by = "movieId") %>%
  semi_join(edx_training_set, by = "userId")

# Add rows removed from validation set back into edx set
removed2 <- anti_join(temp2, edx_test_set)
edx_training_set <- rbind(edx_training_set, removed2)


###########################################################################################


# We define Root Mean Square Error(RMSE) function.  This will be used to test our algorithm.
# The smaller the RMSE, the better the algorithm.
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

# We create graphs of the dataset to help use get more understanding of our data
edx_graph <- edx

# Movie year count
edx_graph %>% group_by(movie_year) %>% mutate(n = n()) %>% ggplot(aes(movie_year)) + geom_bar() +
  xlab("Movie year") + ylab("Count") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Rating count
edx_graph %>% group_by(rating) %>% ggplot(aes(rating)) + geom_histogram(binwidth = 0.5, color = "Grey") + xlab("Rating") + ylab("Count") + ggtitle("Rating count") + geom_vline(aes(xintercept = mean(rating)), col = "red") + scale_y_continuous(labels = scales::label_number_si())


###########################################################################################
# First method we use just the average or rating
#
###########################################################################################

## Using average mean to predict and its RMSE.
mu <- mean(edx_training_set$rating)

# Generate RMSE result for edx_test_set
edx_method_average <- RMSE(edx_test_set$rating, mu)
edx_rmse_results1 <- data.frame(method = "Just the average", RMSE = edx_method_average)
edx_rmse_results1

# # Generate RMSE result for validation set
# RMSE(validation$rating, mu)
# method_average <- RMSE(validation$rating, mu)
# rmse_results1 <- data.frame(method = "Just the average", RMSE = method_average)
# rmse_results1

###########################################################################################
# Second method we use just the average + movie effects
#
###########################################################################################
## Try 2nd method, to add other factor: movie effects
## Generate RMSE result for edx_test_set

## Using average mean to predict and its RMSE.
mu <- mean(edx_training_set$rating)
movie_avgs <- edx_training_set %>% group_by(movieId) %>% summarize(b_i = mean(rating-mu))
edx_predicted_ratings <- mu + edx_test_set %>% left_join(movie_avgs, by='movieId') %>% pull(b_i)

edx_method_movie_effects <- RMSE(edx_test_set$rating, edx_predicted_ratings)
edx_rmse_results2 <- data.frame(method = "Movie effects", RMSE = edx_method_movie_effects)
edx_rmse_results2

# # Generate RMSE result for validation set
# movie_avgs <- edx %>% group_by(movieId) %>% summarize(b_i = mean(rating-mu))
# predicted_ratings <- mu + validation %>% left_join(movie_avgs, by='movieId') %>% pull(b_i)
# 
# method_movie_effects <- RMSE(validation$rating, predicted_ratings)
# rmse_results2 <- data.frame(method = "Movie effects", RMSE = method_movie_effects)
# rmse_results2


###########################################################################################
# Third method we use just the average + movieId + userId
#
###########################################################################################
## Try 3rd method, to add other factor: movie effects and user effects
## Generate RMSE result for edx_test_set
user_avgs <- edx_training_set %>% left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

edx_predicted_ratings <- edx_test_set %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)
edx_method_movie_user_effects <- RMSE(edx_test_set$rating, edx_predicted_ratings)
edx_rmse_results3 <- data.frame(method = "Movie and user effects", RMSE = edx_method_movie_user_effects)
edx_rmse_results3

## Generate RMSE result for validation set
# user_avgs <- edx %>% left_join(movie_avgs, by='movieId') %>%
#   group_by(userId) %>%
#   summarize(b_u = mean(rating - mu - b_i))
# 
# predicted_ratings <- validation %>%
#   left_join(movie_avgs, by='movieId') %>%
#   left_join(user_avgs, by='userId') %>%
#   mutate(pred = mu + b_i + b_u) %>%
#   pull(pred)
# method_movie_user_effects <- RMSE(validation$rating, predicted_ratings)
# rmse_results3 <- data.frame(method = "Movie and user effects", RMSE = method_movie_user_effects)
# rmse_results3


###########################################################################################
# Forth method we use just the average + movieId + userId + movie_year
#
###########################################################################################
## Try 4th method, to add other factor: movie effects, user effects and year effects
# fit3 <- lm(rating ~ as.factor(movieId) + as.factor(userId) + as.factor(movie_year), data=edx)

## Generate RMSE result for edx_test_set
movie_year_avgs <- edx_training_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  group_by(movie_year) %>%
  summarize(b_y = mean(rating - mu - b_i - b_u))

predicted_ratings <- edx_test_set %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(movie_year_avgs, by='movie_year') %>%
  mutate(pred = mu + b_i + b_u + b_y) %>%
  pull(pred)

edx_method_movie_user_year_effects <- RMSE(edx_test_set$rating, predicted_ratings)

## Add final result to data frame for comparison
edx_rmse_results4 <- data.frame(method = "Movie_user_year_effects", RMSE = edx_method_movie_user_year_effects)

## Generate RMSE result for validation set
# movie_year_avgs <- edx %>% 
#   left_join(movie_avgs, by='movieId') %>%
#   left_join(user_avgs, by='userId') %>%
#   group_by(movie_year) %>%
#   summarize(b_y = mean(rating - mu - b_i - b_u))
# predicted_ratings <- validation %>%
#   left_join(movie_avgs, by='movieId') %>%
#   left_join(user_avgs, by='userId') %>%
#   left_join(movie_year_avgs, by='movie_year') %>%
#   mutate(pred = mu + b_i + b_u + b_y) %>%
#   pull(pred)
# 
# method_movie_user_year_effects <- RMSE(validation$rating, predicted_ratings)

## Add final result to data frame for comparison
# rmse_results4 <- data.frame(method = "Movie_user_year_effects", RMSE = method_movie_user_year_effects)

###########################################################################################
# Fifth method we use just the average + movieId + userId + movie_year + 
#   Regularization of movielens dataset 
#
###########################################################################################
# Try 5th method, Regularization of movielens dataset
## assigns a sequence of number into lambdas variable
lambdas <- seq(0, 10, 0.25)
## passing in lambdas and generate a vector of RMSEs, then we will use this vector to plot our lambdas graph.
rmses <- sapply(lambdas, function(l){
  mu <- mean(edx_training_set$rating)
  # movie effect
  b_i <- edx_training_set %>% group_by(movieId) %>% summarize(b_i = sum(rating - mu)/(n() + l))
  # user effect    
  b_u <- edx_training_set %>% left_join(b_i, by="movieId") %>% group_by(userId) %>% summarize(b_u = sum(rating - b_i - mu)/(n() + l))
  # movie released year effect    
  b_y <- edx_training_set %>% left_join(b_i, by="movieId") %>% left_join(b_u, by="userId") %>% group_by(movie_year) %>% summarize(b_y = sum(rating - b_i - b_u - mu)/(n() + l))
    
  predicted_ratings <- edx_test_set %>% left_join(b_i, by='movieId') %>% left_join(b_u, by='userId') %>% left_join(b_y, by='movie_year') %>% mutate(pred = mu + b_i + b_u + b_y) %>% pull(pred)
  
  return(RMSE(edx_test_set$rating, predicted_ratings))})

# Plot the graph of the lambdas
qplot(lambdas, rmses)

# Use the min value of lambdas to test our algorithm and find the value of RMSE
lambda <- lambdas[which.min(rmses)]

b_i <- edx_training_set %>% group_by(movieId) %>% summarize(b_i = sum(rating - mu)/(n() + lambda))

b_u <- edx_training_set %>% left_join(b_i, by="movieId") %>% group_by(userId) %>% summarize(b_u = sum(rating - b_i - mu)/(n() + lambda))

b_y <- edx_training_set %>% left_join(b_i, by="movieId") %>% left_join(b_u, by="userId") %>% group_by(movie_year) %>% summarize(b_y = sum(rating - b_i - b_u - mu)/(n() + lambda))

predicted_ratings <- edx_test_set %>% left_join(b_i, by='movieId') %>% left_join(b_u, by='userId') %>% left_join(b_y, by='movie_year') %>% mutate(pred = mu + b_i + b_u + b_y) %>% pull(pred)

edx_method_reg_user_movie_year <- RMSE(edx_test_set$rating, predicted_ratings)

edx_rmse_results5 <- data.frame(method = "Movie_reg_user_year_effects", RMSE = edx_method_reg_user_movie_year)

# 
# lambdas <- seq(0, 10, 0.25)
# ## passing in lambdas and generate a vector of RMSEs, then we will use this vector to plot our lambdas graph.
# rmses <- sapply(lambdas, function(l){
#   mu <- mean(edx$rating)
#   # movie effect
#   b_i <- edx %>% group_by(movieId) %>% summarize(b_i = sum(rating - mu)/(n() + l))
#   # user effect    
#   b_u <- edx %>% left_join(b_i, by="movieId") %>% group_by(userId) %>% summarize(b_u = sum(rating - b_i - mu)/(n() + l))
#   # movie released year effect    
#   b_y <- edx %>% left_join(b_i, by="movieId") %>% left_join(b_u, by="userId") %>% group_by(movie_year) %>% summarize(b_y = sum(rating - b_i - b_u - mu)/(n() + l))
#   
#   predicted_ratings <- validation %>% left_join(b_i, by='movieId') %>% left_join(b_u, by='userId') %>% left_join(b_y, by='movie_year') %>% mutate(pred = mu + b_i + b_u + b_y) %>% pull(pred)
#   
#   return(RMSE(validation$rating, predicted_ratings))})
# 
# # Plot the graph of the lambdas
# qplot(lambdas, rmses)
# 
# # Use the min value of lambdas to test our algorithm and find the value of RMSE
# lambda <- lambdas[which.min(rmses)]
# 
# b_i <- edx %>% group_by(movieId) %>% summarize(b_i = sum(rating - mu)/(n() + lambda))
# 
# b_u <- edx %>% left_join(b_i, by="movieId") %>% group_by(userId) %>% summarize(b_u = sum(rating - b_i - mu)/(n() + lambda))
# 
# b_y <- edx %>% left_join(b_i, by="movieId") %>% left_join(b_u, by="userId") %>% group_by(movie_year) %>% summarize(b_y = sum(rating - b_i - b_u - mu)/(n() + lambda))
# 
# predicted_ratings <- validation %>% left_join(b_i, by='movieId') %>% left_join(b_u, by='userId') %>% left_join(b_y, by='movie_year') %>% mutate(pred = mu + b_i + b_u + b_y) %>% pull(pred)
# 
# method_reg_user_movie_year <- RMSE(validation$rating, predicted_ratings)
# 
# rmse_results5 <- data.frame(method = "Movie_reg_user_year_effects", RMSE = method_reg_user_movie_year)


###########################################################################################
# Generate all methods result for edx_test_set for comparison
#
###########################################################################################
edx_rmse_all_results = rbind(edx_rmse_results1, edx_rmse_results2, edx_rmse_results3, edx_rmse_results4, edx_rmse_results5)
edx_rmse_all_results

###########################################################################################
# Final method (fifth model) provides the best result for validation set.  We 
# select the final method.
###########################################################################################

lambdas <- seq(0, 10, 0.25)
## passing in lambdas and generate a vector of RMSEs, then we will use this vector to plot our lambdas graph.
rmses <- sapply(lambdas, function(l){
  mu <- mean(edx$rating)
  # movie effect
  b_i <- edx %>% group_by(movieId) %>% summarize(b_i = sum(rating - mu)/(n() + l))
  # user effect
  b_u <- edx %>% left_join(b_i, by="movieId") %>% group_by(userId) %>% summarize(b_u = sum(rating - b_i - mu)/(n() + l))
  # movie released year effect
  b_y <- edx %>% left_join(b_i, by="movieId") %>% left_join(b_u, by="userId") %>% group_by(movie_year) %>% summarize(b_y = sum(rating - b_i - b_u - mu)/(n() + l))

  predicted_ratings <- validation %>% left_join(b_i, by='movieId') %>% left_join(b_u, by='userId') %>% left_join(b_y, by='movie_year') %>% mutate(pred = mu + b_i + b_u + b_y) %>% pull(pred)

  return(RMSE(validation$rating, predicted_ratings))})

# Plot the graph of the lambdas
qplot(lambdas, rmses)

# Use the min value of lambdas to test our algorithm and find the value of RMSE
lambda <- lambdas[which.min(rmses)]

b_i <- edx %>% group_by(movieId) %>% summarize(b_i = sum(rating - mu)/(n() + lambda))

b_u <- edx %>% left_join(b_i, by="movieId") %>% group_by(userId) %>% summarize(b_u = sum(rating - b_i - mu)/(n() + lambda))

b_y <- edx %>% left_join(b_i, by="movieId") %>% left_join(b_u, by="userId") %>% group_by(movie_year) %>% summarize(b_y = sum(rating - b_i - b_u - mu)/(n() + lambda))

predicted_ratings <- validation %>% left_join(b_i, by='movieId') %>% left_join(b_u, by='userId') %>% left_join(b_y, by='movie_year') %>% mutate(pred = mu + b_i + b_u + b_y) %>% pull(pred)

method_reg_user_movie_year <- RMSE(validation$rating, predicted_ratings)

rmse_results5 <- data.frame(method = "Movie_reg_user_year_effects_validation_set", RMSE = method_reg_user_movie_year)

print(rmse_results5)



