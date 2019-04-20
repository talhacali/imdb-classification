library(ggplot2)
library(purrr)
library(dplyr)
library(jsonlite)
library(stringr)

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
