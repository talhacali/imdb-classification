library(ggplot2)

movies <- read.csv("tmdb_5000_movies.csv")

str(movies)

movies$genres.

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
