#Dataset Description: Chuan Sun (@sundeepblue on Github) scraped tons of metadata 
#using a combination of www.the-numbers.com, IMDB.com, and a Python library 
#called "scrapy".
#He was able to obtain 28 variables for 5043 movies and 4906 posters (998MB), 
#spanning across 100 years in 66 countries. There are 2399 unique director names, 
#and thousands of actors/actresses.

# LIBRARIES ---------------------------------------------------------------
library(tidyverse)
library(psych)

# IMPORT DATA -------------------------------------------------------------
imdbData <- read.csv("Data/IMDB Data.csv", header = TRUE)

# HAVE A LOOK -------------------------------------------------------------
glimpse(imdbData) #16 numeric variables and 12 factor variables
missingData <- which(is.na(imdbData), arr.ind = TRUE) #2059 Missing values
nrow(missingData)

#Filter out the non-numerical data
imdbData_numeric <- imdbData %>% 
                      select_if(is.numeric)
missingData_numeric <- which(is.na(imdbData_numeric), arr.ind = TRUE) #2059 Missing values
nrow(missingData_numeric) #Still 2059 Missing values

describe(imdbData_numeric, skew = FALSE)

# GRAPHICAL ANALYSIS ------------------------------------------------------
#pairs(imdbData_numeric) #Not easy to read - toomany variables

par(mfrow = c(2, 2))
apply(imdbData_numeric, MARGIN = 2, FUN = hist)