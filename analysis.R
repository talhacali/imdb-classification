library(ggplot2)
library(purrr)
library(dplyr)
library(jsonlite)
library(stringr)
library(classInt)
library(tree)

movies <- read.csv("tmdb_5000_movies.csv",stringsAsFactors = FALSE)

movies = subset(movies,select = c(id,title,genres,popularity,revenue,runtime,vote_average,vote_count)) #drop unnecessary columns

movies = movies[movies$genres != "[]",]


convert_json <- function(x){
  json_df = fromJSON(x)
  
  ret_genres=''
  for(i in 1:nrow(json_df)){
    ret_genres = paste(ret_genres,json_df[i,2],sep=',') 
  }
  
  return(ret_genres)
}


for(i in 1:nrow(movies)){
  movies$genres[i] = convert_json(movies$genres[i])
}


t <- strsplit(movies$genres, split = ",")
tags <- unique(str_trim(unlist(t)))
tags = tags[tags!=""]
df2 <- as.data.frame(Reduce(cbind, lapply(tags, function(i) sapply(t, function(j) +(any(grepl(i, j), na.rm = TRUE))))))
names(df2) <- tags


movies=cbind(movies,df2)

movies = subset(movies,select = c(id,title,popularity,revenue,runtime,9:28,vote_average,vote_count)) #change column orderings

movies=movies[movies$vote_average != 0,]

movies=movies[!is.na(movies$runtime),]

cor(movies$vote_average,movies$vote_count)

cor(movies$vote_average,movies$runtime)

revenue_zeros = movies$revenue[movies$revenue == 0]
length(revenue_zeros)
length(revenue_zeros) / length(movies$revenue)

popularity_zeros = movies$popularity[movies$popularity == 0]
length(popularity_zeros)
length(popularity_zeros) / length(movies$popularity)



colnames(movies)[9] = "Science_Fiction"
colnames(movies)[25] = "TV_Movie"

movies$revenue = movies$revenue/max(movies$revenue)
movies$popularity = movies$popularity/max(movies$popularity)
movies$budget = movies$budget/max(movies$budget)
movies$runtime = movies$runtime/max(movies$runtime)
movies$vote_count = movies$vote_count/max(movies$vote_count)



ggplot(movies, aes(x=vote_average)) + geom_histogram()
ggplot(movies, aes(x=vote_count)) + geom_histogram()

ggplot(data=movies,mapping = aes(x=movies$vote_count,y=movies$vote_average))+
  geom_point()

ggplot(data=movies,mapping = aes(x=movies$runtime,y=movies$vote_average))+
  geom_point()

summary(movies$vote_average)
ggplot(data=movies,aes(x="vote_average",y=vote_average))+geom_boxplot()



#Data Preparation Start

# 1- Popularity

#draw box plot of Popularity feature.(column=3)
OUTLIERS <- boxplot(movies$popularity)$out #outlier values.
movies <- movies[-which(movies$popularity %in% OUTLIERS),] # Removed outlier values from popularity column

###############################################
#BINS_COUNT <- 20
# bucketing data points into bins
#movies$popularity_cat <- cut(movies[,3], BINS_COUNT, include.lowest = T, right=FALSE, labels=)
#plot(POPULARITY_BINS, main="20 bins", ylab="Number of Popularity Instance",xlab="Popularity Values")
##############################################

# 2- Revenue

#draw box plot of Revenue feature.(column=4)
OUTLIERS <- boxplot(movies$revenue)$out #outlier values.
movies <- movies[-which(movies$revenue %in% OUTLIERS),] # Removed outlier values from revenue column

##############################################
#BINS_COUNT <- 20
# bucketing data points into bins
#movies$revenue_cat <- cut(movies[,4], BINS_COUNT, include.lowest = T, right=FALSE, labels=)
###############################################


# 3- Vote-Count

#draw box plot of Vote-Count feature.(column=27)
OUTLIERS <- boxplot(movies$vote_count)$out #outlier values.
movies <- movies[-which(movies$vote_count %in% OUTLIERS),] # Removed outlier values from vote_count column

#######################################################
#BINS_COUNT <- 30
# bucketing data points into bins
#movies$vote_count_cat <- cut(movies[,27], BINS_COUNT, include.lowest = T, right=FALSE, labels=)
#####################################################

sep = c(-Inf,mean(movies$vote_average),Inf)
movie_types = c("Bad","Good")

movies$vote_average_cat <- cut(movies$vote_average,breaks = sep,labels = movie_types)

colnames(movies)[9] = "Science_Fiction"
colnames(movies)[25] = "TV_Movie"

#movies[,6:25] = movies[,6:25] == 1

library(corrplot) # to draw correlation matrix


# Draw correlation matrix
# We first looked at the matrix and popularity, revenue and vote_count were 
#found to be correlated.
sub = subset(movies,select = c(popularity,revenue, runtime, Action, Adventure, Fantasy, Science_Fiction, Crime, Drama, Thriller, Animation, Family, Western, Comedy, Romance, Horror,Mystery,History,War,Music,Documentary,Foreign,TV_Movie,vote_count,vote_average))

summary(sub)
cor(sub)
forcorrplot <- cor(sub)
corrplot(forcorrplot)
corrplot(forcorrplot, method="color")
corrplot(forcorrplot, method="color", order="hclust")
corrplot.mixed(forcorrplot, upper="number", lower="color", order="hclust")

cor(movies$popularity,movies$vote_average)

movies = subset(movies,select = -c(vote_count))



#MODEL DEVELOPMENT
#Desicion Tree
dt_acc <- numeric()
set.seed(1815850)

for(i in 1:100){
  sub <- sample(1:nrow(movies), size=nrow(movies)*0.7)
  fit2 <- tree(vote_average_cat ~ Action + Adventure + Fantasy + Science_Fiction + Crime +
                 Drama + Thriller + Animation + Family + Western + Comedy + Romance + Horror + 
                 Mystery + History + War + Music + Documentary + Foreign + TV_Movie +
                 popularity + revenue + vote_count,data=movies, split = "deviance",subset = sub)
  test_predict <- table(predict(fit2, movies[-sub, ], type = "class"), movies[-sub, "vote_average_cat"])
  dt_acc <- c(dt_acc, sum(diag(test_predict)) / sum(test_predict))
}

mean(dt_acc)
sd(dt_acc)*100
max(dt_acc)*100 - min(dt_acc)*100

plot(1-dt_acc, type="l", ylab="Error Rate", xlab="Iterations", main="Error Rate for Movies With Different Subsets of Data with Desicion Trees")

dt.tr <- tree(vote_average_cat ~ Action + Adventure + Fantasy + Crime + Science_Fiction +
                Drama + Thriller + Animation + Family + Western + Comedy + Romance + Horror + 
                Mystery + History + War + Music + Documentary + Foreign + TV_Movie +
                popularity + revenue + vote_count,data=movies, split = "deviance")
dt.tr
# display the results
summary(dt.tr)
misclass.tree(dt.tr)

# visualize DT 
plot(dt.tr,  type = "uniform")
text(dt.tr)



#Random Forest
require(randomForest)


dt_acc <- numeric()
set.seed(1815850)

for(i in 1:20){
  sub <- sample(1:nrow(movies), size=nrow(movies)*0.7)
  fit2 <- randomForest(vote_average_cat ~ Action + Adventure + Fantasy + Science_Fiction + Crime +
                         Drama + Thriller + Animation + Family + Western + Comedy + Romance + Horror + 
                         Mystery + History + War + Music + Documentary + Foreign + TV_Movie +
                         popularity + revenue + vote_count,data=movies,subset = sub)
  test_predict <- table(predict(fit2, movies[-sub, ], type = "class"), movies[-sub, "vote_average_cat"])
  rf_acc <- c(dt_acc, sum(diag(test_predict)) / sum(test_predict))
}

mean(rf_acc)
sd(rf_acc)*100
max(rf_acc)*100 - min(rf_acc)*100

plot(1-rf_acc, type="l", ylab="Error Rate", xlab="Iterations", main="Error Rate for Movies With Different Subsets of Data with Random Forests")



#Knn
library(class) # Contains the "knn" function
set.seed(498593) #Set the seed for reproducibility

#Create partitions in the Iris data set (70% for training, 30% for testing/evaluation)
movies_sample <- sample(1:nrow(movies), size=nrow(movies)*0.7)
movies_train <- movies[movies_sample, ] #Select the 70% of rows
movies_test <- movies[-movies_sample, ] #Select the 30% of rows

#First try to determine the right K-value 
movies_acc <- numeric() #holding variable

for(i in 1:20){
  #Apply knn with k = i
  predict <- knn(train=movies_train[,3:26], test=movies_test[,3:26], cl=movies_train$vote_average_cat, k=i)
  movies_acc <- c(movies_acc, mean(predict==movies_test$vote_average_cat))
}

#Plot error rates for k=1 to 20
plot(1-movies_acc, type="l", ylab="Error Rate",  xlab="K", main="Error Rate for Movies with varying K")

# Average accuracy of 20 k-values
mean(movies_acc)


trial_sum <- numeric(30)
trial_n <- numeric(30)

for(i in 1:100){
  
  movies_sample <- sample(1:nrow(movies), size=nrow(movies)*0.7)
  movies_train <- movies[movies_sample,]
  movies_test <- movies[-movies_sample,]
  test_size <- nrow(movies_test)
  
  for(j in 1:30){
    predict <- knn(movies_train[,3:26], movies_test[,3:26], movies_train$vote_average_cat, k=j)
    trial_sum[j] <- trial_sum[j] + sum(predict==movies_test$vote_average_cat)
    trial_n[j] <- trial_n[j] + test_size
  }
}

plot(1-trial_sum / trial_n, type="l", ylab="Error Rate",xlab="K",main="Error Rate for Movies With Varying K (100 Samples)")

max(trial_sum / trial_n)
sd(trial_sum / trial_n)*100

