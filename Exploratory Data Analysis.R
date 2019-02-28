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
ggcorr(imdbData_numeric, label = TRUE)

par(mfrow = c(2, 2))
apply(imdbData_numeric, MARGIN = 2, FUN = hist)
sampleScore <- sample(imdbData_numeric$imdb_score, size = 2500)
shapiro.test(sampleScore) #Not normal

# Check dist looks the same
hist(imdbData_numeric$imdb_score)
hist(sampleScore)


