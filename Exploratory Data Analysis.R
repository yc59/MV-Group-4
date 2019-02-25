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

missingData <- which(is.na(imdbData), arr.ind = TRUE) #2059 Missing values
nrow(missingData)

#Filter out the non-numerical data
imdbData_numeric <- imdbData %>% 
                      select_if(is.numeric)
missingData_numeric <- which(is.na(imdbData_numeric), arr.ind = TRUE)
nrow(missingData_numeric) #1622 Missing values

describe(imdbData_numeric, skew = FALSE)

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
