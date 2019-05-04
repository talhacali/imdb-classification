library(ggplot2)
library(purrr)
library(dplyr)
library(jsonlite)
library(stringr)
library(classInt)

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





#Data Preparation Start

# 1- Popularity
orderedData <- movies[order(movies$popularity),]

#draw box plot of Popularity feature.(column=3)
OUTLIERS <- boxplot(orderedData$popularity)$out #outlier values.
orderedData <- orderedData[-which(orderedData$popularity %in% OUTLIERS),] # Removed outlier values from popularity column

BINS_COUNT <- 20
# bucketing data points into bins
POPULARITY_BINS <- cut(orderedData[,3], BINS_COUNT, include.lowest = T, right=FALSE, labels=)
# inspect bins
summary(POPULARITY_BINS)
#plot(POPULARITY_BINS, main="20 bins", ylab="Number of Popularity Instance",xlab="Popularity Values")


# 2- Revenue
orderedData <- orderedData[order(orderedData$revenue),]

#draw box plot of Revenue feature.(column=4)
OUTLIERS <- boxplot(orderedData$revenue)$out #outlier values.
orderedData <- orderedData[-which(orderedData$revenue %in% OUTLIERS),] # Removed outlier values from revenue column

BINS_COUNT <- 20
# bucketing data points into bins
REVENUE_BINS <- cut(orderedData[,4], BINS_COUNT, include.lowest = T, right=FALSE, labels=)
# inspect bins
summary(REVENUE_BINS)
#plot(REVENUE_BINS, main="20 bins", ylab="Number of Revenue Instance",xlab="Revenue Values")



# 3- Vote-Average
orderedData <- orderedData[order(orderedData$vote_average),]

#draw box plot of Vote-Average feature.(column=26)
OUTLIERS <- boxplot(orderedData$vote_average)$out #outlier values.
orderedData <- orderedData[-which(orderedData$vote_average %in% OUTLIERS),] # Removed outlier values from vote_average column

BINS_COUNT <- 30
# bucketing data points into bins
VOTE_AVERAGE_BINS <- cut(orderedData[,26], BINS_COUNT, include.lowest = T, right=FALSE, labels=)
# inspect bins
summary(VOTE_AVERAGE_BINS)
plot(VOTE_AVERAGE_BINS, main="20 bins", ylab="Number of Vote-Average Instance",xlab="Vote-Average Values")




# 4- Vote-Count
orderedData <- orderedData[order(orderedData$vote_count),]

#draw box plot of Vote-Count feature.(column=27)
OUTLIERS <- boxplot(orderedData$vote_count)$out #outlier values.
orderedData <- orderedData[-which(orderedData$vote_count %in% OUTLIERS),] # Removed outlier values from vote_count column

BINS_COUNT <- 30
# bucketing data points into bins
VOTE_COUNT_BINS <- cut(orderedData[,27], BINS_COUNT, include.lowest = T, right=FALSE, labels=)
# inspect bins
summary(VOTE_COUNT_BINS)
plot(VOTE_COUNT_BINS, main="20 bins", ylab="Number of Vote-Count Instance",xlab="Vote-Count Values")






#replace missing value with mean value of runtime (2)
MEAN <- mean(orderedData[, 3])
#Data Preparation End





summary(movies$vote_average)
ggplot(data=movies,aes(x="vote_average",y=vote_average))+geom_boxplot()
summary(movies$popularity)
ggplot(data=movies,aes(x="popularity",y=popularity))+geom_boxplot()


ggplot(data=movies,mapping = aes(x=movies$vote_average,y=movies$budget))+
  geom_point()

ggplot(data=movies,mapping = aes(x=movies$vote_average,y=movies$popularity))+
  geom_point()

ggplot(data=movies,mapping = aes(x=movies$vote_average,y=movies$vote_count))+
  geom_point()

ggplot(data=movies,mapping = aes(x=movies$vote_average,y=movies$revenue))+
  geom_point()

ggplot(data=movies,mapping = aes(x=movies$vote_average,y=movies$runtime))+
  geom_point()











"
int = (25/100)*nrow(orderedData)
rem = int - as.integer(int)
int = as.integer(int)

if(rem == 0){
Q1 = orderedData[int, 3]
}else{
Q1 = (1 - rem)*orderedData[int, 3]+(rem*orderedData[int+1, 3])
}
#Q1 percentile found

int = (75/100)*nrow(orderedData)
rem = int - as.integer(int)
int = as.integer(int)

if(rem == 0){
Q3 = orderedData[int, 3]
}else{
Q3 = (1 - rem)*orderedData[int, 3]+(rem*orderedData[int+1, 3])
}
#Q3 percentile found


int = (50/100)*nrow(orderedData)
rem = int - as.integer(int)
int = as.integer(int)

if(rem == 0){
MEDIAN = orderedData[int, 3]
}else{
MEDIAN = (1 - rem)*orderedData[int, 3]+(rem*orderedData[int+1, 3])
}
#MEDIAN or 50th percentile found

IQR = Q3 - Q1 #Inter quantile range found

MIN = Q1 - (1.5 * IQR)
MAX = Q3 + (1.5 * IQR)

"
