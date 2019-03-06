#Dataset Description: Chuan Sun (@sundeepblue on Github) scraped tons of metadata 
#using a combination of www.the-numbers.com, IMDB.com, and a Python library 
#called "scrapy".
#He was able to obtain 28 variables for 5043 movies and 4906 posters (998MB), 
#spanning across 100 years in 66 countries. There are 2399 unique director names, 
#and thousands of actors/actresses.

# LIBRARIES ---------------------------------------------------------------
library(tidyverse)
library(psych)
library(GGally)
library(ggplot2)
library(dplyr)

# IMPORT DATA -------------------------------------------------------------
imdbData <- read.csv("Data/IMDB Data.csv", header = TRUE)

# HAVE A LOOK -------------------------------------------------------------
glimpse(imdbData) #14 numeric variables and 14 factor variables

#Change title_year and aspect_ratio to factor
imdbData[c(24,27)] <- lapply(imdbData[c(24,27)], factor) 

#Missing values per column
missingData <- which(is.na(imdbData) | imdbData == "", arr.ind = TRUE) #2698 Missing values
nrow(missingData)

missing_in_each_col <- matrix(NA, nrow = 28, ncol = 2)
for(i in 1:ncol(imdbData)){
  missing_in_each_col[i, 1] <- sum(is.na(imdbData[, i]))
  missing_in_each_col[i, 2] <- sum(imdbData[, i] == "")
}
colnames(missing_in_each_col) <- c("NAs", "Missing Text")
rownames(missing_in_each_col) <- names(imdbData)
missing_in_each_col

#Filter out the non-numerical data
imdbData_numeric <- imdbData %>% 
                      select_if(is.numeric)
missingData_numeric <- which(is.na(imdbData_numeric) | imdbData_numeric == "", 
                             arr.ind = TRUE)
nrow(missingData_numeric) #1622 NA Missing values

describe(imdbData_numeric, skew = FALSE)

#Filter out the numerical data
imdbData_factor <- imdbData %>% 
  select_if(is.factor)
missingData_factor <- which(is.na(imdbData_factor) | imdbData_factor == "", 
                            arr.ind = TRUE)
nrow(missingData_factor) #437 NA Missing values and 639 "" Missing Values

summary(imdbData_factor, skew = FALSE)

content_ratings_levels <- levels(imdbData$content_rating) #19 levels

# GRAPHICAL ANALYSIS ------------------------------------------------------
#pairs(imdbData_numeric) #Not easy to read - toomany variables


# we transform all of the numeric in order to limit their value between a small range
imdbData_numeric_trans<-imdbData_numeric
imdbData_numeric_trans$num_critic_for_reviews<-(imdbData_numeric$num_critic_for_reviews-140.19)/100
imdbData_numeric_trans$duration<-(imdbData_numeric$duration-107.20)/100
imdbData_numeric_trans$director_facebook_likes<-(imdbData_numeric$director_facebook_likes-686.51)/10000
imdbData_numeric_trans$actor_3_facebook_likes<-(imdbData_numeric$actor_3_facebook_likes-645.01)/10000
imdbData_numeric_trans$actor_1_facebook_likes<-(imdbData_numeric$actor_1_facebook_likes- 6560.05)/100000
imdbData_numeric_trans$gross<-(imdbData_numeric$gross-48468407.53)/100000000
imdbData_numeric_trans$num_voted_users<-(imdbData_numeric$num_voted_users-83668.16)/1000000
imdbData_numeric_trans$cast_total_facebook_likes<-(imdbData_numeric$cast_total_facebook_likes-9699.06)/100000
imdbData_numeric_trans$facenumber_in_poster<-(imdbData_numeric$facenumber_in_poster-1.37)/10
imdbData_numeric_trans$budget<-(imdbData_numeric$budget-39752620.44)/10000000000
imdbData_numeric_trans$num_user_for_reviews<-(imdbData_numeric$num_user_for_reviews-272.77)/1000
imdbData_numeric_trans$actor_2_facebook_likes<-(imdbData_numeric$actor_2_facebook_likes- 1651.75)/100000
imdbData_numeric_trans$imdb_score<-imdbData_numeric$imdb_score-6.44
imdbData_numeric_trans$movie_facebook_likes<-(imdbData_numeric$movie_facebook_likes-7525.96)/100000

imdbData_numeric_1<-imdbData_numeric_trans[,1:7]
imdbData_numeric_2<-imdbData_numeric_trans[,8:14]

library(PerformanceAnalytics)
chart.Correlation(imdbData_numeric_1, bg=imdbData_numeric_1)
chart.Correlation(imdbData_numeric_1, bg=imdbData_numeric_2)
chart.Correlation(imdbData_numeric_2, bg=imdbData_numeric_2)

# Similarly,we seperate imdbData_factor into two parts for a clear observation.
library(PerformanceAnalytics)
imdbData_factor_1<-imdbData_factor[,1:7]
imdbData_factor_2<imdbData_factor[,8:14]

# chart.Correlation seems function well for numeric data but not for factor
chart.Correlation(imdbData_factor_1, bg=imdbData_factor_1)
chart.Correlation(imdbData_factor_1, bg=imdbData_factor_2)
chart.Correlation(imdbData_factor_2, bg=imdbData_factor_2)



# set duration as colour with sample
# sparsed
pairs(sample_n(imdbData, 1000), pch = 19,  cex = 0.5,
      col = imdbData$duration,
      lower.panel=NULL)

# set language as colour with sample
# mainly English i suppose
pairs(sample_n(imdbData, 1000), pch = 19,  cex = 0.5,
      col = imdbData$language,
      lower.panel=NULL)

# set genres as colour with sample
pairs(sample_n(imdbData, 1000), pch = 19,  cex = 0.5,
      col = imdbData$genres,
      lower.panel=NULL)

# ggplot 
ggplot(sample_n(imdbData, 1000), aes(x=imdb_score, y=gross,
      color=duration)) + geom_point()
str(imdbData)


# ggcorr
ggcorr(imdbData_numeric, label = TRUE)


par(mfrow = c(2, 2))
apply(imdbData_numeric, MARGIN = 2, FUN = hist)
sampleScore <- sample(imdbData_numeric$imdb_score, size = 2500)
shapiro.test(sampleScore) #Not normal

# Check dist looks the same
hist(imdbData_numeric$imdb_score)
hist(sampleScore)


