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

#Parse json in genres column
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

#Split genres into multiple binary columns by finding the unique values. 
t <- strsplit(movies$genres, split = ",")
tags <- unique(str_trim(unlist(t)))
tags = tags[tags!=""]
df2 <- as.data.frame(Reduce(cbind, lapply(tags, function(i) sapply(t, function(j) +(any(grepl(i, j), na.rm = TRUE))))))
names(df2) <- tags


movies=cbind(movies,df2)

movies = subset(movies,select = c(id,title,popularity,revenue,runtime,9:28,vote_average,vote_count)) #change column orderings

#Remove missing values
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

#Normalization
movies$revenue = movies$revenue/max(movies$revenue)
movies$popularity = movies$popularity/max(movies$popularity)
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
OUTLIERS <- boxplot(movies$popularity,xlab="Popularity")$out #outlier values.
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


library(corrplot) # to draw correlation matrix


# Draw correlation matrix
# We first looked at the matrix and popularity, revenue and vote_count were 
#found to be correlated.
sub = subset(movies,select = c(popularity,revenue, runtime, Action, Adventure, Fantasy, Science_Fiction, Crime, Drama, Thriller, Animation, Family, Western, Comedy, Romance, Horror,Mystery,History,War,Music,Documentary,Foreign,TV_Movie,vote_count,vote_average))

summary(sub)
cor(sub)
forcorrplot <- cor(sub)
corrplot(forcorrplot)

cor(movies$popularity,movies$vote_average)

movies = subset(movies,select = -c(vote_count))

#Corralation matrix after vote_count is removed
sub = subset(movies,select = c(popularity,revenue, runtime, Action, Adventure, Fantasy, Science_Fiction, Crime, Drama, Thriller, Animation, Family, Western, Comedy, Romance, Horror,Mystery,History,War,Music,Documentary,Foreign,TV_Movie,vote_average))


forcorrplot <- cor(sub)
corrplot(forcorrplot)


#MODEL DEVELOPMENT
dt_acc <- numeric()
dt_recall <- numeric()
dt_precision <- numeric()
dt_f <- numeric()


trial_sum <- numeric(100)
trial_n <- numeric(100)
knn_recall <- numeric()
knn_precision <- numeric()
knn_f <- numeric()
knn_ks = numeric()


rf_acc <- numeric()
rf_recall <- numeric()
rf_precision <- numeric()
rf_f <- numeric()

dt_time = 0
rf_time = 0
knn_time = 0

library(class)
require(randomForest)


set.seed(1815850)

#ONE LOOP that executes every model to give them the same parts of the dataset.
t_1=Sys.time()
for(i in 1:100){
  sub <- sample(1:nrow(movies), size=nrow(movies)*0.7)
  
  #Desicion Tree
  time_a = Sys.time()
  fit2 <- tree(vote_average_cat ~ Action + Adventure + Fantasy + Science_Fiction + Crime +
                 Drama + Thriller + Animation + Family + Western + Comedy + Romance + Horror + 
                 Mystery + History + War + Music + Documentary + Foreign + TV_Movie +
                 popularity + revenue,data=movies, split = "deviance",subset = sub)
  test_predict <- table(predict(fit2, movies[-sub, ], type = "class"), movies[-sub, "vote_average_cat"])
  dt_acc <- c(dt_acc, sum(diag(test_predict)) / sum(test_predict))
  curr_recall = test_predict[1,"Bad"]/(test_predict[1,"Bad"]+test_predict[2,"Bad"])
  curr_precision = test_predict[1,"Bad"]/(test_predict[1,"Bad"]+test_predict[1,"Good"])
  curr_f = 2 * (curr_precision * curr_recall) / (curr_precision + curr_recall)
  dt_f <- c(dt_f,curr_f)
  dt_recall <- c(dt_recall,curr_recall)
  dt_precision <- c(dt_precision,curr_precision)
  time_b = Sys.time()
  
  dt_time = dt_time + (time_b-time_a)
  
  #Random Forest
  time_a = Sys.time()
  fit2 <- randomForest(vote_average_cat ~ Action + Adventure + Fantasy + Science_Fiction + Crime +
                         Drama + Thriller + Animation + Family + Western + Comedy + Romance + Horror + 
                         Mystery + History + War + Music + Documentary + Foreign + TV_Movie +
                         popularity + revenue,data=movies,subset = sub)
  test_predict <- table(predict(fit2, movies[-sub, ], type = "class"), movies[-sub, "vote_average_cat"])
  rf_acc <- c(rf_acc, sum(diag(test_predict)) / sum(test_predict))
  curr_recall = test_predict[1,"Bad"]/(test_predict[1,"Bad"]+test_predict[2,"Bad"])
  curr_precision = test_predict[1,"Bad"]/(test_predict[1,"Bad"]+test_predict[1,"Good"])
  curr_f = 2 * (curr_precision * curr_recall) / (curr_precision + curr_recall)
  rf_f <- c(rf_f,curr_f)
  rf_recall <- c(rf_recall,curr_recall)
  rf_precision <- c(rf_precision,curr_precision)
  time_b = Sys.time()
  
  rf_time = rf_time+(time_b-time_a)
  
  #KNN
  time_a=Sys.time()
  movies_train <- movies[sub,]
  movies_test <- movies[-sub,]
  test_size <- nrow(movies_test)
  best_k = 0
  best_acc=0
  
  #Find the best k value based on best accuracy.
  for(j in 1:30){
    predict <- knn(movies_train[,3:26], movies_test[,3:26], movies_train$vote_average_cat, k=j)
    curr_acc = sum(predict==movies_test$vote_average_cat) / test_size
    if(curr_acc > best_acc){
      best_acc = curr_acc
      best_k = j
    }
  }
  
  time_b=Sys.time()
  
  predict <- knn(movies_train[,3:26], movies_test[,3:26], movies_train$vote_average_cat, k=best_k)
  trial_sum[i] <- trial_sum[i] + sum(predict==movies_test$vote_average_cat)
  trial_n[i] <- trial_n[i] + test_size
  test_predict <- table(predict(fit2, movies[-sub, ], type = "class"), movies[-sub, "vote_average_cat"])
  curr_recall = test_predict[1,"Bad"]/(test_predict[1,"Bad"]+test_predict[2,"Bad"])
  curr_precision = test_predict[1,"Bad"]/(test_predict[1,"Bad"]+test_predict[1,"Good"])
  curr_f = 2 * (curr_precision * curr_recall) / (curr_precision + curr_recall)
  knn_f <- c(knn_f,curr_f)
  knn_recall <- c(knn_recall,curr_recall)
  knn_precision <- c(knn_precision,curr_precision)
  knn_ks <- c(knn_ks,best_k)
  
  knn_time = knn_time + (time_b-time_a)
}

t_2 = Sys.time()

#Desicion Tree
mean(dt_acc)
mean(dt_recall)
mean(dt_precision)
mean(dt_f)
sd(dt_acc)*100
max(dt_acc)*100 - min(dt_acc)*100

plot(1-dt_acc, type="l", ylab="Error Rate", xlab="Iterations", main="Error Rate for Movies With Different Subsets of Data with Desicion Trees")

dt.tr <- tree(vote_average_cat ~ Action + Adventure + Fantasy + Crime + Science_Fiction +
                Drama + Thriller + Animation + Family + Western + Comedy + Romance + Horror + 
                Mystery + History + War + Music + Documentary + Foreign + TV_Movie +
                popularity + revenue,data=movies, split = "deviance")
dt.tr
# display the results
summary(dt.tr)
misclass.tree(dt.tr)

# visualize DT 
plot(dt.tr,  type = "uniform")
text(dt.tr)




#Random Forest Results
mean(rf_acc)*100
mean(rf_recall)
mean(rf_precision)
mean(rf_f)
sd(rf_acc)*100
max(rf_acc)*100 - min(rf_acc)*100

plot(1-rf_acc, type="l", ylab="Error Rate", xlab="Iterations", main="Error Rate for Movies With Different Subsets of Data with Random Forests")


#KNN Results
mean(trial_sum / trial_n)*100
mean(knn_recall)
mean(knn_precision)
mean(knn_f)
sd(trial_sum / trial_n)*100


plot((1-trial_sum / trial_n)*100, type="l", ylab="Error Rate",xlab="K",main="Error Rate for Movies With Varying K (100 Samples)")
plot(x=knn_ks,y=(1-trial_sum / trial_n)*100, type="p", ylab="Error Rate",xlab="K",main="Error Rate for Movies With Varying K (100 Samples)")


#Plot with two y axis to show the best k values and accuracy rates in each fold.
accs = (trial_sum / trial_n)*100
d = data.frame(x =seq(1:100),
               ks = knn_ks,
               accs = accs)


par(mar = c(5,5,2,5))
with(d, plot(x, accs, type="l", col="red3",xlab="Iterations",
             ylab="Accuracy",
             ylim=c(95,99)))

par(new = T)
with(d, plot(x, ks, pch=16, axes=F, xlab=NA, ylab=NA, cex=1.2))
axis(side = 4)
mtext(side = 4, line = 3, 'Best K Values')

#legend("top",
#       legend=c("Accuracy", "Best K Values"),
#       lty=c(1,0), pch=c(NA, 16), col=c("red3", "black"))



#Running times
dt_time/100
rf_time/100
knn_time/100
t_2 - t_1

