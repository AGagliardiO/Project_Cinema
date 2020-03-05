
# The following code presupose that the edx and validation datasets are alredy in the workspace, to avoid a unnecesary download of two heavy files.

# 1) Creation of training and test datasets

if (!require(caret)) install.packages('caret')
library(caret)
if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)      
if (!require(lubridate)) install.packages('lubridate')
library(lubridate)

set.seed(1)
y <-  edx$userId
test_index <- createDataPartition(y, times = 1, p = 0.2, list = FALSE)
test_set <- edx[test_index,]
train_set <- edx[-test_index,]
rm(y)

# 2) Visualization of the variables 


train_set %>% group_by(movieId) %>% summarize(movie_avg = mean(rating)) %>% 
  ggplot() + geom_histogram(aes(movie_avg), bins = 100) + 
  labs(title = "Movie Score Dispersion")

train_set %>% group_by(userId) %>% summarize(user_avg = mean(rating)) %>% 
  ggplot() + geom_histogram(aes(user_avg), bins = 100) + 
  labs(title = "User Score Dispersion")

train_set %>% group_by(genres) %>% summarize(genre_avg = mean(rating)) %>% 
  ggplot() + geom_histogram(aes(genre_avg), bins = 100) + 
  labs(title = "Gender Score Dispersion")

train_set %>% mutate(rating_week = round_date(as_datetime(timestamp), unit = weeks())) %>% 
  group_by(rating_week) %>% summarize(week_avg = mean(rating)) %>% 
  ggplot(aes(rating_week,week_avg)) + geom_point() +
labs(title = "Time Score Dispersion")


# 3) Parameter Estimation

global_avg <- mean(train_set$rating)
train_set <- train_set %>% mutate(rating_week = round_date(as_datetime(timestamp), unit = weeks()))

 ## a) Movie Effect

movie_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - global_avg))

predicted_ratings_1 <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>% 
  mutate(pred = global_avg + b_i ) %>% pull(pred) 
  predicted_ratings_1[is.na(predicted_ratings_1)] <- global_avg

Test_1 <- RMSE(predicted_ratings_1, test_set$rating)
Test_1 


 ## b) User Effect

user_avgs <- train_set %>% left_join(movie_avgs, by = "movieId") %>% 
  group_by(userId) %>% 
  summarize(b_u = mean(rating - global_avg - b_i))

predicted_ratings_2 <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>% 
  left_join(user_avgs, by='userId') %>% 
  mutate(pred = global_avg + b_i + b_u) %>% pull(pred)
predicted_ratings_2[is.na(predicted_ratings_2)] <- global_avg

Test_2 <- RMSE(predicted_ratings_2, test_set$rating)
Test_2

## c) Genre Effect

genre_avgs <- train_set %>% left_join(movie_avgs, by = "movieId") %>% 
  left_join(user_avgs, by = "userId") %>% 
  group_by(genres) %>% 
  summarize(b_g = mean(rating - global_avg - b_u - b_i))

predicted_ratings_3 <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>% 
  left_join(user_avgs, by='userId') %>% 
  left_join(genre_avgs, by = 'genres') %>%
  mutate(pred = global_avg + b_i + b_u + b_g) %>% pull(pred)
predicted_ratings_3[is.na(predicted_ratings_3)] <- global_avg

Test_3 <- RMSE(predicted_ratings_3, test_set$rating)
Test_3


## c) Time Effect

time_avgs <- train_set %>% 
  left_join(movie_avgs, by = "movieId") %>% 
  left_join(user_avgs, by = "userId") %>% 
  left_join(genre_avgs, by = "genres") %>% group_by(rating_week) %>% 
  summarize(b_t = mean(rating - global_avg - b_u - b_i - b_g))

predicted_ratings_4  <- test_set %>% mutate(rating_week = round_date(as_datetime(timestamp), unit = weeks())) %>%
  left_join(movie_avgs, by='movieId') %>% 
  left_join(user_avgs, by='userId') %>% 
  left_join(genre_avgs, by = 'genres') %>%
  left_join(time_avgs, by='rating_week') %>% 
  mutate(pred = global_avg + b_i + b_u + b_g + b_t) %>% pull(pred)
predicted_ratings_4[is.na(predicted_ratings_4)] <- global_avg

Test_4 <- RMSE(predicted_ratings_4, test_set$rating)

Test_4


# 4) Regularization of the user and movies parametres

lambdas <- seq(0, 10, 0.25)

rmses <- sapply(lambdas, function(l){
  
  b_i_r <- train_set %>%
    group_by(movieId) %>%
    summarize(b_i_r = sum(rating - global_avg )/(n()+l))
  
  b_u_r <- train_set %>% 
    group_by(userId) %>%
    left_join(b_i_r, by="movieId") %>%
    summarize(b_u_r = sum(rating -  global_avg - b_i_r)/(n()+l))
  
  predicted_ratings <- test_set %>%
  left_join(b_i_r, by = "movieId") %>%
  left_join(b_u_r, by = "userId") %>% 
  mutate(pred = global_avg + b_i_r + b_u_r) %>% .$pred
  predicted_ratings[is.na(predicted_ratings)] <- global_avg

return(RMSE(predicted_ratings, test_set$rating))
})

lambdas[which.min(rmses)]

# 5) Final Evaluation 

## a) Running the code on the edx datasets

movie_avgs_r <- edx %>% 
  group_by(movieId) %>% 
  summarize(b_i_r = sum(rating - global_avg)/(n() + lambdas[which.min(rmses)]))

user_avgs_r <- edx %>% 
  left_join(movie_avgs_r, by = "movieId") %>% 
  group_by(userId) %>% 
  summarize(b_u_r = sum(rating - global_avg - b_i_r)/(n() + lambdas[which.min(rmses)]))

genre_avgs <- edx %>% 
  left_join(movie_avgs_r, by = "movieId") %>% 
  left_join(user_avgs_r, by = "userId") %>% 
  group_by(genres) %>% 
  summarize(b_g = mean(rating - global_avg - b_u_r - b_i_r))

time_avgs <- edx %>%  mutate(rating_week = round_date(as_datetime(timestamp), unit = weeks())) %>%
  left_join(movie_avgs_r, by = "movieId") %>% 
  left_join(user_avgs_r, by = "userId") %>% 
  left_join(genre_avgs, by = "genres") %>% 
  group_by(rating_week) %>% 
  summarize(b_t = mean(rating - global_avg - b_u_r - b_i_r - b_g))

predicted_ratings_edx <- edx %>% mutate(rating_week = round_date(as_datetime(timestamp), unit = weeks())) %>%
  left_join(movie_avgs_r, by='movieId') %>% 
  left_join(user_avgs_r, by='userId') %>% 
  left_join(genre_avgs, by = "genres") %>%
  left_join(time_avgs, by='rating_week') %>%
  mutate(pred = global_avg + b_i_r + b_u_r + b_g + b_t) %>% pull(pred) 
predicted_ratings_edx[is.na(predicted_ratings_edx)] <- global_avg

RMSE_edx <- RMSE(predicted_ratings_edx, edx$rating)
RMSE_edx

## b) Running the code on the validation datasets

predicted_ratings_validation <- validation %>% mutate(rating_week = round_date(as_datetime(timestamp), unit = weeks())) %>% 
  left_join(movie_avgs_r, by='movieId') %>% 
  left_join(user_avgs_r, by='userId') %>% 
  left_join(genre_avgs, by = "genres") %>%
  left_join(time_avgs, by='rating_week') %>% 
  mutate(pred = global_avg + b_i_r + b_u_r + b_g + b_t) %>% pull(pred) 
predicted_ratings_validation[is.na(predicted_ratings_validation)] <- global_avg

RMSE_validation <- RMSE(predicted_ratings_validation, validation$rating)
RMSE_validation


