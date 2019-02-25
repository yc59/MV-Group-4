# read data
setwd ("../MV-Group-4/Data/")
movie <- read.csv("../Data/IMDB Data.csv", header = T)
# as.factor for title year and aspect ratio
movie[c(24,27)] <- lapply(movie[c(24,27)], factor) 

# sample 500 rows and extract only numeric for cor and pairs plot
sam <- movie[sample(nrow(movie),1000),]    
num <- sam[sapply(sam, function(x) is.numeric(x))]
str(num)
cor(num, use="complete.obs")
library(GGally)
ggcorr(num)
pairs(num)

# Tree for gross
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(rpart)
train <- rpart(gross ~ duration + imdb_score + budget + language + num_critic_for_reviews +
                 cast_total_facebook_likes , data=movie)
fancyRpartPlot(train)

# some plotting for the possible response
hist(num$imdb_score)
require(car)
qqPlot(num$imdb_score, main = "imdb_score")
hist(num$gross)
qqPlot(num$gross, main = "Gross")

# PCA
pmovie <- princomp(na.omit(num), scores=T, cor=T)
screeplot(pmovie,type="l")

pscores <- pmovie$scores
pload <- pmovie$loadings
pload[,1:3]

plot(pscores[,1], pscores[,2], type="n")
text(pscores[,1], pscores[,2])

