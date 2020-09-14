#####################################################
#           Load Packages (if Required)             #
#####################################################

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org", quiet=TRUE)
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org", quiet=TRUE)
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org", quiet=TRUE)
if(!require(stringi)) install.packages("stringi", repos = "http://cran.us.r-project.org", quiet=TRUE)
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org", quiet=TRUE)
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org", quiet=TRUE)
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org", quiet=TRUE)
if(!require(rmarkdown)) install.packages("rmarkdown", repos = "http://cran.us.r-project.org", quiet=TRUE)
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org", quiet=TRUE)
if(!require(devtools)) install.packages("devtools", repos = "http://cran.us.r-project.org", quiet=TRUE)
if(!require(tinytex)) install.packages("latexpdf", repos = "http://cran.us.r-project.org", quiet=TRUE)
if(!require(tinytex)) install.packages("tinytex", repos = "http://cran.us.r-project.org", quiet=TRUE)
if(!require(packrat)) install.packages("packrat", repos = "http://cran.us.r-project.org", quiet=TRUE)
if(!require(rsconnect)) install.packages("rsconnect", repos = "http://cran.us.r-project.org", quiet=TRUE)

#####################################################
#                 Load Libraries                    #
#####################################################

library(tidyverse)
library(lubridate)
library(caret)
library(stringi)
library(data.table)
library(gridExtra)
library(ggplot2)
library(rmarkdown)
library(knitr)
library(devtools)
library(latexpdf)
library(tinytex)
library(packrat)
library(rsconnect)

#####################################################
#            Reset Variables / Set Options          #
#####################################################

options(digits=5) 
rm(list=ls())  # Clears all variables

#####################################################
#     Movielens Data Set Download / Wrangle         # 
#       Est Run Time = 2 Minutes 30 Seconds         #
#####################################################

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile() 
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl, quiet=TRUE)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

#####################################################
#   90% Edx / 10% Validation Data Sets Creation     # 
#####################################################

set.seed(1)
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

validation <- temp %>% 
   semi_join(edx, by = "movieId") %>%
   semi_join(edx, by = "userId")

removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

#####################################################
#         Edx Data Set Initial Exploration          #
#####################################################

##### Data Set Overview

str(edx) # Shows structure of the combined Training/Test Data Set

##### Genre breakout for analysis (not used in modeling)

edx_genres <- edx %>% separate_rows(genres,sep="\\|")  # Combined Training / Test Data Set by distinct genres   

#####################################################
#   90% Training / 10% Test Data Sets Creation      # 
#####################################################

set.seed(1) # Set Seed to 1
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.1, list = FALSE)
training <- edx[-test_index,]  # Create Training Data Set
test <- edx[test_index,] # Create Test Data Set

##### List Distinct of Values in the Data Set

data.frame(
   ratings = n_distinct(edx$rating),
   movies = n_distinct(edx$movieId),
   users = n_distinct(edx$userId),
   genres = n_distinct(edx_genres$genres),
   released = n_distinct(data.frame(table(as.numeric(stri_sub(edx$title,-5,-2))))),
   reviewed= n_distinct(year(as_datetime(edx$timestamp))))

#####################################################
#       Model Rating (RMSE Official Rating)         #
#####################################################

##### 5 Models to be Analyzed 

Models = c("Model 1",
           "Model 2", "Model 2",
           "Model 3", "Model 3",
           "Model 4", "Model 4",
           "Model 5", "Model 5")

##### List of Predictors to Use in the Models

Predictors = c("mu", 
               "mu+Movie", "mu+Movie+Reg", 
               "mu+Movie+User", "mu+Movie+User+Reg", 
               "mu+Movie+User+Genre", "mu+Movie+User+Genre+Reg", 
               "mu+Movie+User+Genre+Time", "mu+Movie+User+Genre+Time+Reg")

# Initialize Variables to use to collect Training and Validation Stats

training_stats <- data.frame(Models = Models, Predictors=Predictors, Lambda=NA, RMSE=NA)
validation_stats <- data.frame(Models = Models, Predictors=Predictors, Lambda=NA, RMSE=NA)

##### RMSE Function Utilizing True Rating and Predicted Rating (Lambda Passed for Tracking Only)

Model_Rating <- function(l, true_rating, predicted_rating) {
   RMSE = sqrt(mean((true_rating - predicted_rating)^2))
   data.frame(Lambda = l, RMSE=RMSE)
}

##### List the parameters contained in data to be used for predictors

names(edx)  # Database Columns Names

#####################################################
#                    Model 1 (mu)                   #
#             Est Run Time = 10 Seconds             #
#####################################################

############### Model 1 Data Analysis ###############

ratings_fig <- as.data.frame(table(edx$rating)) %>% 
   mutate(Var1=as.character(Var1)) %>% 
   ggplot(aes(Var1, Freq, color="black")) + 
   geom_bar(stat="identity", show.legend=FALSE) +
   annotate("text", x = 5.7, y = 2500000, label = "Overall Mean = 3.5125") +
   geom_vline(xintercept=2*mean(edx$rating), size=1.0, color="red") +
   scale_y_continuous(name="Ratings Count", labels = scales::comma) +
   ggtitle("Overall Ratings Distribution") +
   xlab("Movie Ratings") +
   ylab("Reviews Count") 

ratings_fig  # Plot Ratings Distribution

############ Model 1 Creation / Testing ############# 

mu <- mean(training$rating)  # Simple Average of Training Data Set

################# Model 1 Testing ################### 

training_stats[1,3:4] <- Model_Rating(0, test$rating, mu)[,1:2] # Determine Result of Testing Model 1
training_stats[1,] # Print out Result of Testing of Model 1

#####################################################
#             Model 2 (mu + Movie)                  #
#           Est Run Time = 30 Seconds               #
#####################################################

############### Model 2 Data Analysis ################

##### Movie Review Distribution Figure

movie_review_distro_fig <- edx %>% 
   group_by(movieId) %>%
   summarise(n=length(rating), .groups = 'drop') %>%
   ggplot(aes(n, color="black")) +
   scale_x_log10() +
   geom_histogram(bins=30, show.legend=FALSE) +
   ggtitle("Movie Reviews Distribution") +
   xlab("Number Reviews by Movie (log10)") +
   ylab("Movie Count") 

###### Movie Ratings Distribution Figure

movie_rating_distro <- edx %>% group_by(movieId) %>% 
   summarise(rev=length(rating), rat=mean(rating), .groups="drop") %>% 
   ggplot(aes(rev,rat)) + 
   geom_smooth(method='gam', formula = y ~ s(x, bs = "cs")) +
   geom_point() +
   annotate("text", x = 27050, y = 3.09, label = "Mean = 3.1921") +
   geom_hline(yintercept=3.1921, size=1.0, color="red") +
   ggtitle("Movie Reviews vs Ratings") +
   xlab("Number of Reviews by Movie") +
   ylab("Average Movie Ratings") 

grid.arrange(movie_review_distro_fig, movie_rating_distro, ncol=2)  # Plot Movie Review/Ratings Distributions

##### Determine Mean Ratings of Distinct Movies

movie_stats <- edx %>% 
   group_by(movieId) %>% 
   summarise(num_reviews = length(rating), avg_rating=mean(rating), .groups = 'drop') %>% 
   left_join(.,data.frame(movieId=edx$movieId, title=edx$title), by="movieId") %>%   
   unique.data.frame() %>%
   arrange(desc(avg_rating)) 

movie_stats %>% head(5)  # List Top 5 Rated Movies
movie_stats %>% tail(5) # List Bottom 5 Rated Movies

################## Model 2 Creation ################# 

model2 <- function(l, test_or_valid) {
   
   ##### Account for Movie Effects
   
   movie_avgs <- training %>% # Calculate (b_i)
      group_by(movieId) %>%
      summarize(b_i = sum(rating - mu)/(n() + l), .groups = 'drop')
   
   ##### Determine Predict Ratings
   
   predicted_rating2 <- test_or_valid %>%  # Return Predictions  
      left_join(movie_avgs, by='movieId') %>%
      mutate(pred=ifelse(!is.na(b_i), mu + b_i, mu)) %>%
      .$pred
   
   Model_Rating(l, test_or_valid$rating, predicted_rating2)  # Return RMSE  
}

################# Model 2 Training ##################

lambda_untuned2 <- seq(0, 6, 0.25)  # Unknown Lambda 
training_model2 <- sapply(lambda_untuned2, model2, test) # Run Model Over Range of Lambdas
lambda_tuned2 <- lambda_untuned2[which.min(training_model2[2,])] # Determine Best-Tuned Lambda

################## Model 2 Testing ##################

training_stats[2,3:4] <- sapply(0, model2, test) # Model Result Without Regularization (Lambda = 0)
training_stats[3,3:4] <- sapply(lambda_tuned2, model2, test) # Model Result with Regularization
training_stats[2:3,] # Return Results Without and With Regularization

#####################################################
#           Model 3 (mu + Movie + User)             #
#      Est Run Time = 1 Minute 10 Seconds           #
#####################################################

############### Model 3 Data Analysis ###############

##### User Review Distribution Figure

user_review_distro_fig <- edx %>% 
   group_by(userId) %>%
   summarise(n=length(rating), .groups = 'drop') %>%
   ggplot(aes(n, color="black")) +
   geom_histogram(bins=30, show.legend=FALSE) +
   scale_x_log10() +
   ggtitle("User Reviews Distribution") +
   xlab("Number of Reviews by User (log10)") +
   ylab("User Count") 

##### User Rating Distribution Figure

user_rating_distro <- edx %>% group_by(userId) %>% 
   summarise(rev=length(rating), rat=mean(rating), .groups="drop") %>% 
   ggplot(aes(rev,rat)) + 
   geom_point() +
   geom_smooth(method='gam', formula = y ~ s(x, bs = "cs")) +
   annotate("text", x = 5650, y = 3.80, label = "Mean = 3.6137") +
   geom_hline(yintercept=3.6137, size=1.0, color="red") +
   ggtitle("User Reviews vs Ratings") +
   xlab("Number of Reviews by User") +
   ylab("Average User Ratings") 

grid.arrange(user_review_distro_fig, user_rating_distro, ncol=2) # Plot User Reviews/Ratings Distribution

################## Model 3 Creation ################## 

model3 <- function(l, test_or_valid) {
   
   ##### Account for Movie Effects (b_i)
   
   movie_avgs <- training %>%
      group_by(movieId) %>%
      summarize(b_i = sum(rating - mu)/(n() + l), 
                .groups = 'drop')
   
   ##### Account for User Effects (b_u)
   
   user_avgs <- training %>%
      left_join(movie_avgs, by='movieId') %>%
      group_by(userId) %>%
      summarize(b_u = sum(rating - mu - b_i)/(n() + l), 
                .groups = 'drop')
   
   ##### Determine Predicted Ratings
   
   predicted_rating <- test_or_valid %>%
      left_join(movie_avgs, by='movieId') %>%
      left_join(user_avgs, by='userId') %>%
      mutate(pred=ifelse(!is.na(b_i + b_u), mu + b_i + b_u, mu)) %>%
      .$pred
   
   Model_Rating(l, test_or_valid$rating, predicted_rating) # Return RMSE
}

################# Model 3 Training ##################

lambda_untuned3 <- seq(0, 6, 0.25)  # Unknown Lambda
training_model3 <- sapply(lambda_untuned3, model3, test) # Run Model Over Range of Lambdas
lambda_tuned3 <- lambda_untuned3[which.min(training_model3[2,])] # Determine Best-Tuned Lambda

################## Model 3 Testing ##################

training_stats[4,3:4] <- sapply(0, model3, test) # Model Result Without Regularization (Lambda = 0)
training_stats[5,3:4] <- sapply(lambda_tuned3, model3, test) # Model Result with Regularization
training_stats[4:5,] # Return Results Without and With Regularization

#####################################################
#        Model 4 (mu + Movie + User + Genre)        #
#        Est Run Time = 1 Minute 40 Seconds         #
#####################################################

############### Model 4 Data Analysis ###############

##### Genre Review Distribution Figure

genre_review_distro_fig <- edx_genres %>% 
   group_by(genres) %>%
   summarise(rating_count=length(rating), .groups = 'drop') %>% 
   ggplot(aes(genres, rating_count)) + 
   geom_bar(stat="identity", show.legend=FALSE) +
   theme(axis.text.x = element_text(angle = 90)) +
   ggtitle("Review Distribution by Genre") +
   xlab("Genres") +
   scale_y_continuous(name="Number of Reviews", labels = scales::comma)  

##### Genre Review Distribution Figure

genre_rating_avg_fig <- edx_genres %>%  
   group_by(genres) %>% 
   summarise(mean_rating = mean(rating), .groups = 'drop') %>%
   ggplot(aes(genres, mean_rating, color="black")) + 
   geom_bar(stat="identity", show.legend=FALSE) +
   annotate("text", x = 3.5, y = 3.74, label = "Mean = 3.5786") +
   geom_hline(yintercept=3.5786, size=1.0, color="red") +
   theme(axis.text.x = element_text(angle = 90)) +
   ggtitle("Ratings Distribution by Genre") +
   xlab("Genres") +
   ylab("Average Rating") 

grid.arrange(genre_review_distro_fig, genre_rating_avg_fig, ncol=2) # Plot Genre Reviews/Ratings Distribution

##### Distinct Genres Overview

genres_overview <- edx_genres %>% 
   group_by(genres) %>% 
   summarise(users=n_distinct(userId), movies=n_distinct(movieId), reviews = length(rating), ratings=mean(rating), 
             .groups = 'drop') %>%
   arrange(genres)

data.frame(genres_overview) %>% arrange(desc(ratings)) %>% head(10) # List Top 10 Rated Distinct Genres

################## Model 4 Creation ################## 

model4 <- function(l, test_or_valid) {
   
   ##### Account for Movie Effects (b_i)
   
   movie_avgs <- training %>%
      group_by(movieId) %>%
      summarize(b_i = sum(rating - mu)/(n() + l), 
                .groups = 'drop')
   
   ##### Account for User Effects (b_u)
   
   user_avgs <- training %>%
      left_join(movie_avgs, by='movieId') %>%
      group_by(userId) %>%
      summarize(b_u = sum(rating - mu - b_i)/(n() + l), 
                .groups = 'drop')
   
   ##### Account for Genre Effects (b_g)
   
   genre_avgs <- training %>%
      left_join(movie_avgs, by='movieId') %>%
      left_join(user_avgs, by='userId') %>%
      group_by(genres) %>%
      summarize(b_g = sum(rating - mu - b_i - b_u)/(n() + l), 
                .groups = 'drop')
   
   ##### Determine Predicted Ratings
   
   predicted_rating <- test_or_valid %>%
      left_join(movie_avgs, by='movieId') %>%
      left_join(user_avgs, by='userId') %>%
      left_join(genre_avgs, by='genres') %>%
      mutate(pred=ifelse(!is.na(b_i + b_u + b_g), mu + b_i + b_u + b_g, mu)) %>%
      .$pred
   
   Model_Rating(l, test_or_valid$rating, predicted_rating) # Return RMSE
}

################# Model 4 Training ###################

lambda_untuned4 <- seq(0, 6, 0.25)  # Unknown Lambda
training_model4 <- sapply(lambda_untuned4, model4, test)  # Run Model Over Range of Lambdas
lambda_tuned4 <- lambda_untuned4[which.min(training_model4[2,])] # Determine Best-Tuned Lambda

################## Model 4 Testing ###################

training_stats[6, 3:4] <- sapply(0, model4, test) # Model Result Without Regularization (Lambda = 0)
training_stats[7, 3:4] <- sapply(lambda_tuned4, model4, test) # Model Result with Regularization
training_stats[6:7,] # Return Results Without and With Regularization

#####################################################
#    Model 5 (mu + Movie + User + Genre + Time)     #
#       Est Run Time = 10 Minutes 30 Seconds        #
#####################################################

############### Model 5 Data Analysis ###############

##### Reviews Based on Year Released Figure

time_review_release_fig <- edx %>% 
   mutate(yr_released = as.numeric(stri_sub(title,-5,-2))) %>%
   group_by(yr_released) %>%
   summarise(num_rvws = length(rating), .groups = 'drop') %>%
   ggplot(aes(yr_released, num_rvws)) + 
   geom_smooth(method = 'loess', formula = 'y ~ x') +
   geom_point() +
   ggtitle("Reviews by Release Year") +
   xlab("Release Year") +
   scale_y_continuous(name="Number of Reviews", labels = scales::comma) 

##### Ratings Based on Year Released Figure

time_rating_release_fig <- edx %>% 
   mutate(yr_released = as.numeric(stri_sub(title,-5,-2))) %>%
   group_by(yr_released) %>%
   summarise(meanrating = mean(rating), .groups = 'drop') %>%
   ggplot(aes(yr_released, meanrating)) + 
   geom_smooth(method = 'loess', formula = 'y ~ x') +
   geom_point() +
   annotate("text", x = 1994, y = 3.75, label = "Mean = 3.7211") +
   geom_hline(yintercept=3.7211, size=1.0, color="red") +
   ggtitle("Ratings by Year") +
   ggtitle("Ratings by Release Year") +
   ylab("Average Rating") +
   xlab("Release Year") 

grid.arrange(time_rating_release_fig,time_rating_release_fig, ncol=2) # Plot Released Years Reviews/Ratings Distribution

##### Reviews Based on Year Reviewed Figure

time_yr_reviews_fig <- edx %>% 
   mutate(yr=year(as_datetime(timestamp))) %>%
   group_by(yr) %>%
   summarise(yr_review=mean(length(rating)), .groups = 'drop') %>%
   ggplot(aes(yr, yr_review)) +
   geom_smooth(method = 'loess', formula = 'y ~ x') +
   geom_point() +
   ggtitle("Yearly Reviews") +
   xlab("Review Year") +
   scale_y_continuous(name="Number of Reviews", labels = scales::comma)  

##### Ratings Based on Year Reviewed Figure

time_yr_ratings_fig <- edx %>% 
   mutate(yr=year(as_datetime(timestamp))) %>%
   group_by(yr) %>%
   summarise(yr_rating=mean(rating), .groups = 'drop') %>%
   ggplot(aes(yr, yr_rating)) +
   geom_smooth(method = 'loess', formula = 'y ~ x') +
   geom_point() +
   annotate("text", x = 1997.1, y = 3.45, label = "Mean = 3.4737") +
   geom_hline(yintercept=3.4737, size=1.0, color="red") +
   ggtitle("Ratings by Review Year") +
   xlab("Review Year") +
   ylab("Average Rating") 

grid.arrange(time_yr_reviews_fig, time_yr_ratings_fig, ncol=2) # Plot Reviewed Years Reviews/Ratings Distribution

##### Reviews Based on Week Released Figure

time_wk_reviews_fig <- edx %>% 
   mutate(wk=week(as_datetime(timestamp))-1) %>%
   group_by(wk) %>%
   summarise(wk_review=mean(length(rating)), .groups = 'drop') %>%
   ggplot(aes(wk, wk_review)) +
   geom_smooth(method = 'loess', formula = 'y ~ x') +
   geom_point() +
   ggtitle("Weekly Reviews") +
   xlab("Review Week") +
   scale_y_continuous(name="Number of Reviews", labels = scales::comma)  

##### Ratings Based on Week Released Figure

time_wk_ratings_fig <- edx %>% 
   mutate(wk=week(as_datetime(timestamp))) %>%
   group_by(wk) %>%
   summarise(wk_rating=mean(rating), .groups = 'drop') %>%
   ggplot(aes(wk, wk_rating)) +
   geom_smooth(method = 'loess', formula = 'y ~ x') +
   geom_point() +
   annotate("text", x = 43, y = 3.510, label = "Mean = 3.515") +
   geom_hline(yintercept=3.515, size=1.0, color="red") +
   ggtitle("Ratings by Week Reviewed") +
   xlab("Review Week") +
   ylab("Average Rating") 

grid.arrange(time_wk_reviews_fig,time_wk_ratings_fig, ncol=2) # Plot Reviewed Weeks Reviews/Ratings Distribution

##### Reviews Based on Hour Released Figure

time_hr_reviews_fig <- training %>% 
   mutate(hr=hour(as_datetime(timestamp))) %>%
   group_by(hr) %>%
   summarise(hr_review=mean(length(rating)), .groups = 'drop') %>%
   ggplot(aes(hr, hr_review)) +
   geom_smooth(method = 'loess', formula = 'y ~ x') +
   geom_point() +
   ggtitle("Hourly Reivews") +
   xlab("Review Hour") + 
   scale_y_continuous(name="Number of Reviews", labels = scales::comma)  

##### Reviews Based on Hour Released Figure

time_hr_ratings_fig <- training %>% 
   mutate(hr=hour(as_datetime(timestamp))) %>%
   group_by(hr) %>%
   summarise(hr_rating=mean(rating), .groups = 'drop') %>%
   ggplot(aes(hr, hr_rating)) +
   geom_smooth(method = 'loess', formula = 'y ~ x') +
   geom_point() +
   annotate("text", x = 4, y = 3.5095, label = "Mean = 3.511") +
   geom_hline(yintercept=3.511, size=1.0, color="red") +
   ggtitle("Ratings by Hour Reviewed") +
   xlab("Review Hour") +
   ylab("Average Rating")  

grid.arrange(time_hr_reviews_fig,time_hr_ratings_fig, ncol=2) # Plot Reviewed Hours Reviews/Ratings Distribution

################## Model 5 Creation ################## 

model5 <- function(l, test_or_valid) {
   
   ##### Account for Movie Effects (b_i)
   
   movie_avgs <- training %>%
      group_by(movieId) %>%
      summarise(b_i = sum(rating - mu)/(n() + l), 
                .groups = 'drop')
   
   ##### Account for User Effects (b_u)
   
   user_avgs <- training %>%
      left_join(movie_avgs, by='movieId') %>%
      group_by(userId) %>%
      summarise(b_u = sum(rating - mu - b_i)/(n() + l), 
                .groups = 'drop')
   
   ##### Account for User Effects (b_g)
   
   genre_avgs <- training %>%
      left_join(movie_avgs, by='movieId') %>%
      left_join(user_avgs, by='userId') %>%
      group_by(genres) %>%
      summarise(b_g = sum(rating - mu - b_i - b_u)/(n() + l), 
                .groups = 'drop')
   
   ##### Account for Time Effects (b_t1) - Year Released
   
   time_yr_released_avgs <- training %>%
      left_join(movie_avgs, by='movieId') %>%
      left_join(user_avgs, by='userId') %>%
      left_join(genre_avgs, by='genres') %>%
      group_by(yr_released = as.numeric(stri_sub(title,-5,-2))) %>% 
      summarise(b_t1 = sum(rating - mu - b_i - b_u - b_g)/(n() + l), 
                .groups = 'drop')
   
   ##### Account for Time Effects (b_t2) - Year Reviewed
   
   time_yr_reviewed_avgs <- training %>%
      left_join(movie_avgs, by = 'movieId') %>%
      left_join(user_avgs, by = 'userId') %>%
      left_join(genre_avgs, by = 'genres') %>%
      mutate(yr_released = as.numeric(stri_sub(title,-5,-2))) %>%
      left_join(time_yr_released_avgs, by='yr_released') %>%
      group_by(yr_reviewed=year(as_datetime(timestamp))) %>%
      summarize(b_t2 = sum(rating - mu - b_i - b_u - b_g - b_t1)/(n() + l),
                .groups = 'drop')
   
   ##### Account for Time Effects (b_t3) - Week Reviewed
   
   time_wk_reviewed_avgs <- training %>%
      left_join(movie_avgs, by = 'movieId') %>%
      left_join(user_avgs, by = 'userId') %>%
      left_join(genre_avgs, by = 'genres') %>%
      mutate(yr_released = as.numeric(stri_sub(title,-5,-2))) %>%
      left_join(time_yr_released_avgs, by='yr_released') %>%
      mutate(yr_reviewed=year(as_datetime(timestamp))) %>%
      left_join(time_yr_reviewed_avgs, by='yr_reviewed') %>% 
      group_by(wk_reviewed=week(as_datetime(timestamp))) %>%
      summarize(b_t3 = sum(rating - mu - b_i - b_u - b_g - b_t1 - b_t2)/(n() + l),
                .groups = 'drop')
   
   ##### Account for Time Effects (b_t4) - Hour Reviewed
   
   time_hr_reviewed_avgs <- training %>%
      left_join(movie_avgs, by = 'movieId') %>%
      left_join(user_avgs, by = 'userId') %>%
      left_join(genre_avgs, by = 'genres') %>%
      mutate(yr_released = as.numeric(stri_sub(title,-5,-2))) %>%
      left_join(time_yr_released_avgs, by='yr_released') %>%
      mutate(yr_reviewed=year(as_datetime(timestamp))) %>%
      left_join(time_yr_reviewed_avgs, by='yr_reviewed') %>% 
      mutate(wk_reviewed=week(as_datetime(timestamp))) %>%
      left_join(time_wk_reviewed_avgs, by='wk_reviewed') %>%       
      group_by(hr_reviewed=hour(as_datetime(timestamp))) %>%
      summarize(b_t4 = sum(rating - mu - b_i - b_u - b_g - b_t1 - b_t2 - b_t3)/(n() + l),
                .groups = 'drop')
   
   ##### Determine Predicted Ratings
   
   predicted_rating <- test_or_valid %>%
      left_join(movie_avgs, by='movieId') %>%
      left_join(user_avgs, by='userId') %>%
      left_join(genre_avgs, by='genres') %>%
      mutate(yr_released = as.numeric(stri_sub(title,-5,-2))) %>%
      left_join(time_yr_released_avgs, by='yr_released') %>%
      mutate(yr_reviewed=year(as_datetime(timestamp))) %>%
      left_join(time_yr_reviewed_avgs, by='yr_reviewed') %>% 
      mutate(wk_reviewed=week(as_datetime(timestamp))) %>%
      left_join(time_wk_reviewed_avgs, by='wk_reviewed') %>% 
      mutate(hr_reviewed=hour(as_datetime(timestamp))) %>%
      left_join(time_hr_reviewed_avgs, by='hr_reviewed') %>% 
      mutate(pred=ifelse(!is.na(b_i + b_u + b_g + b_t1 + b_t2 + b_t3 + b_t4), mu + b_i + b_u + b_g + b_t1 + b_t2 + b_t3 + b_t4, mu)) %>%
      .$pred
   
   Model_Rating(l, test_or_valid$rating, predicted_rating) # Return RMSE
} 

################# Model 5 Training ###################

lambda_untuned5 <- seq(0, 6, 0.25)  # Unknown Lambda
training_model5 <- sapply(lambda_untuned5, model5, test) # Run Model Over Range of Lambdas
lambda_tuned5 <- lambda_untuned5[which.min(training_model5[2,])] # Determine Best-Tuned Lambda

################## Model 5 Testing ###################

training_stats[8,3:4] <- sapply(0, model5, test) # Model Result Without Regularization (Lambda = 0)
training_stats[9,3:4] <- sapply(lambda_tuned5, model5, test) # Model Result with Regularization
training_stats[8:9,] # Return Results Without and With Regularization  

#####################################################
#                Validation Summary                 #
#             Est Run Time = 1 Minute               #
#####################################################

##### Model Runs on Validation Data Set

validation_stats[1,3:4] <- Model_Rating(NA, validation$rating, mu) # Model 1
validation_stats[2,3:4] <- sapply(0, model2, validation) # Model 2:  Non-Regularization Model
validation_stats[3,3:4] <- sapply(lambda_tuned2, model2, validation) # Model 2: Regularization
validation_stats[4,3:4] <- sapply(0, model3, validation) # Model 3:  Non-Regularization
validation_stats[5,3:4] <- sapply(lambda_tuned3, model3, validation) # Model 3:  Regularization
validation_stats[6,3:4] <- sapply(0, model4, validation) # Model 4:  Non-Regularization
validation_stats[7,3:4] <- sapply(lambda_tuned4, model4, validation) #Model 4:  Regularization
validation_stats[8,3:4] <- sapply(0, model5, validation) # Model 5:  Non-Regularization
validation_stats[9,3:4] <- sapply(lambda_tuned5, model5, validation) # Model 5:  Regularization
validation_stats

##### Performance of Models Without Regularization

without_regularization <- validation_stats[c(1,2,4,6,8),]
without_regularization # Returns Models that did not use Regularization

##### Lambda Tuning Results Used for Regularization

##### Model 2 Lambda Tuning Figure

tuning_line2 <- data.frame(Model = "Model 2", Lambda = lambda_untuned2, RMSE = unlist(training_model2[2,])) %>% 
   ggplot() +
   geom_line(aes(Lambda, RMSE, color="blue"), show.legend=FALSE) +
   ggtitle("Model 2 Lambda Tuning") +
   xlab("Lambda") +
   ylab("RMSE")  

##### Model 3 Lambda Tuning Figure

tuning_line3 <- data.frame(Model = "Model 3", Lambda = lambda_untuned3, RMSE = unlist(training_model3[2,])) %>% 
   ggplot() +
   geom_line(aes(Lambda, RMSE, color="blue"), show.legend=FALSE) +
   ggtitle("Model 3 Lambda Tuning") +
   xlab("Lambda") +
   ylab("RMSE")  

##### Model 4 Lambda Tuning Figure

tuning_line4 <- data.frame(Model = "Model 4", Lambda = lambda_untuned4, RMSE = unlist(training_model4[2,])) %>% 
   ggplot() +
   geom_line(aes(Lambda, RMSE, color="blue"), show.legend=FALSE) +
   ggtitle("Model 4 Lambda Tuning") +
   xlab("Lambda") +
   ylab("RMSE")  

##### Model 5 Lambda Tuning Figure

tuning_line5 <- data.frame(Model = "Modle 5", Lambda = lambda_untuned5, RMSE = unlist(training_model5[2,])) %>% 
   ggplot() +
   geom_line(aes(Lambda, RMSE, color="blue"), show.legend=FALSE) +
   ggtitle("Model 5 Lambda Tuning") +
   xlab("Lambda") +
   ylab("RMSE")  

grid.arrange(tuning_line2, tuning_line3, tuning_line4, tuning_line5, ncol=2)  # Plot Model 2 - 4 Lambda Tuning Results

##### Performance of Models With Regularization

with_regularization <- validation_stats[c(1, 3,5,7,9),]  
with_regularization  # Return Models that used Regularization

##### Return Best Model To Determine Project Points

validation_stats %>% arrange(RMSE) %>% head(1) # Best Model that meets Project Goal