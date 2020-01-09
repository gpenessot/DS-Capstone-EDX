#######################################################
#    install package if not available on computer     #
#######################################################

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(recosystem)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("data.table", repos = "http://cran.us.r-project.org")


#######################################################
#                   load packages                     #
#######################################################

library(tidyverse, warn.conflicts = FALSE)
library(caret, warn.conflicts = FALSE)
library(data.table, warn.conflicts = FALSE)
library(lubridate, warn.conflicts = FALSE)
library(ggplot2, warn.conflicts = FALSE)

#######################################################
#                    load datasets                    #
#######################################################

edx <- readRDS("edx.rds")
validation <- readRDS("validation.rds")

# edx snapshot
glimpse(edx)

# print edx dimensions
dim(edx)

# brief edx summary
summary(edx)

# Extract the genre in edx datasets

edx <- edx %>%
       mutate(genre = fct_explicit_na(genres, na_level = "NA")) %>%
       separate_rows(genre, sep = "\\|")

# Extract the genre in validation datasets

validation <- validation %>%
              mutate(genre = fct_explicit_na(genres, na_level = "NA")) %>%
              separate_rows(genre, sep = "\\|")

# new dataset dimensions
dim(edx)

# histogram of ratings

edx %>% select("rating") %>% 
        ggplot(aes(x = rating)) +
            geom_histogram(binwidth = 0.2) +
            ggtitle("Rating distribution") +
            scale_x_continuous(breaks = seq(0, 5, by = 0.5)) +
            labs(x = "Rating", y = "Number of ratings", caption = "source: edx dataset")

# histogram of ratings by movieId

edx %>% count(movieId) %>% 
        ggplot(aes(n)) + 
            geom_histogram(bins = 40) +
            scale_x_log10() + 
            ggtitle("Rating distribution by movie") +
            labs(x="movieId" , 
                 y="Number of ratings", caption = "source : edx dataset")

# histogram ratings by userId

edx %>% count(userId) %>% 
        ggplot(aes(n)) + 
            geom_histogram( bins = 40) +
            scale_x_log10() + 
            ggtitle("Rating by user") +
            labs(x = "userId" , 
                 y = "Number of ratings")

# the RMSE function is defined as :

RMSE <- function(measured_ratings, 
                 predicted_ratings){
    sqrt(mean((measured_ratings - predicted_ratings)^2))
  }

# load recosystem
library(recosystem, warn.conflicts = FALSE)

######################################################
#               Evaluate movie effect                #
######################################################

# the first step is to calculate the average of all ratings in the dataset
mu <- mean(edx$rating)

# next we have to evaluate b_i coefficient
movie_avgs <- edx %>% 
              group_by(movieId) %>% 
              summarize(b_i = mean(rating - mu))

# finally, predictions are calculated
predicted_ratings_bi <- mu + validation %>% 
                        left_join(movie_avgs, by = 'movieId') %>% .$b_i

# RMSE for this first model is then evaluated 
RMSE_reg1 <- format(RMSE(validation$rating, predicted_ratings_bi), 
                    digits = 5, 
                    nsmall = 2)  
RMSE_reg1 

######################################################
#          Evaluate movie and user effect            #
######################################################

# now it is time to evaluate the b_u coefficient
user_avgs <- edx %>%  
            left_join(movie_avgs, by = 'movieId') %>%
            group_by(userId) %>%
            summarize(b_u = mean(rating - mu - b_i))

# predictions are calculated
predicted_ratings_bu <- validation %>% 
                        left_join(movie_avgs, by = 'movieId') %>%
                        left_join(user_avgs, by = 'userId') %>%
                        mutate(pred = mu + b_i + b_u) %>% .$pred


# RMSE for this second model is then evaluated 
RMSE_reg2 <- format(RMSE(validation$rating, predicted_ratings_bu), 
                    digits = 5, 
                    nsmall = 2)  
RMSE_reg2

######################################################
#      Evaluate movie, user and genre effects        #
######################################################

# we calculate the coefficient associated to the genre feature 

genre_avg <- edx %>%
             left_join(movie_avgs, by = 'movieId') %>%
             left_join(user_avgs, by = 'userId') %>%
             group_by(genre) %>%
             summarize(b_g = mean(rating - mu - b_i - b_u))

# we compute the predicted ratings on validation dataset
   
predicted_ratings_bg <- validation %>%
                        left_join(movie_avgs, by = 'movieId') %>%
                        left_join(user_avgs, by = 'userId') %>%
                        left_join(genre_avg, by = 'genre') %>%
                        mutate(pred = mu + b_i + b_u + b_g) %>% .$pred

# RMSE for this second model is then evaluated 

RMSE_reg3 <- format(RMSE(validation$rating, predicted_ratings_bg), 
                    digits = 5, 
                    nsmall = 2)  
RMSE_reg3

######################################################
#      Evaluate movie, user and time effect          #
######################################################

# we create a subset of edx to select only user, movie and rating columns
edx_subset <- edx %>% select("userId", "movieId", "rating")

# glimpse(ratings_data)

# we create a test set (20%) and a train set (80%)
smp_size <- floor(0.8 * nrow(edx_subset))
train_ind <- sample(1: nrow(edx_subset), size = smp_size)

train <- edx_subset[train_ind, ]
test <- edx_subset[-train_ind, ]

# we specify user_index, item_index and rating for train data
train_data <- data_memory(user_index = train$userId, 
                          item_index = train$movieId, 
                          rating = train$rating, 
                          index1 = T)

# we specify user_index, item_index and rating for test data
test_data <- data_memory(user_index = test$userId, 
                         item_index = test$movieId, 
                         rating = test$rating, 
                         index1 = T)

# we build the recommander object
recommender <- Reco()

# we train the model 
recommender$train(train_data, opts = c(dim = 30, costp_l2 = 0.1, costq_l2 = 0.1, 
                                       lrate = 0.1, niter = 100, nthread = 6, verbose = F))

# let's make predictions on test set
test$prediction <- recommender$predict(test_data, out_memory())

# RMSE calculated with test set
RMSE_MFGD <- format(RMSE(test$rating, test$prediction), 
                    digits = 5, 
                    nsmall = 2)  
RMSE_MFGD

# we build a subset to select only user, movie and rating colunms
validation2 <- validation %>% select("userId", "movieId", "rating")

# we prepare the validation data set
validation_data <- data_memory(user_index = validation2$userId, 
                               item_index = validation2$movieId, 
                               rating = validation2$rating, 
                               index1 = T)

validation2$pred <- recommender$predict(validation_data, out_memory())

# we calculate RMSE on validation set
RMSE_MFGD_val <- format(RMSE(validation2$rating, validation2$pred), 
                    digits = 5, 
                    nsmall = 2)  
RMSE_MFGD_val


