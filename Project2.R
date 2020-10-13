#Jiayuan Xu
#TopicModel Clustering example with movie data.

# setup libraries to prepare your library
if (!require(reshape2)) {install.packages("reshape2"); library(reshape2)}
if (!require(readr)) {install.packages("readr"); library(readr)}
if (!require(NLP)) {install.packages("NLP"); library(NLP)}
if (!require(topicmodels)) {install.packages("topicmodels"); library(topicmodels)}
if (!require(tm)) {install.packages("NLP"); library(tm)}
if (!require(parallelDist)) {install.packages("parallelDist"); library(parallelDist)}
if (!require(slam)) {install.packages("slam"); library(slam)}
#if (!require(stringi)) {install.packages("stringi"); library(stringi)}
if (!require(tidyr)) {install.packages("tidyr"); library(tidyr)}

# Import files. 
#opus_movielens_tags <- read_delim("opus_movielens_tags.csv", 
#                                  "\t", escape_double = FALSE, trim_ws = TRUE)

opus_movies <- read_delim("opus_movies.csv", 
                          "\t", escape_double = FALSE, trim_ws = TRUE)

tags=read.delim("opus_movielens_tags.txt",header=T)
tags$tag <- stri_encode(tags$tag, "", "latin1")

#Prepare data
release_movie = toString(opus_movies[opus_movies['display_name'] == 'The Maze Runner',]['odid']) #targeted movie id
#tags = tags[tags$odid %in% movie2014$odid,] # select all 2014 movies
limited_count = 10
tags = tags[tags["count"] >= limited_count,] #select all rows which its count is greater than limited_count
tags$odid=as.factor(tags$odid)
tags$tag=as.factor(tags$tag)
mterms=simple_triplet_matrix(i=as.integer(tags$odid),j=as.integer(tags$tag),v=tags$count,
                             dimnames=list(levels(tags$odid),levels(tags$tag)))

# stri_enc_mark(tags$tag)
# all(stri_enc_isutf8(tags$tag))
# reshaped_movies[is.na(reshaped_movies)] = 0 #fill NA value to 0
# selected_movies = dcast(mul_movie, odid~paste0(tag), value.var='count') #reshaped the selected_movie
# row.names(selected_movies) = selected_movies$odid
# selected_movies$odid = NULL
# selected_movies[is.na(selected_movies)] = 0
# distHellinger(release_movie_tag,selected_movies)
# distance_matrix <- parDist(selected_movies, method="hellinger")

# estimate a series of LDA models
ldac_movie = LDA(mterms,k=3,method="Gibbs")

# show the measures and associated topics
ClustTopics_movie = round(exp(ldac_movie@beta)*100)  # matrix with probabilities of each measure per topic
colnames(ClustTopics_movie)=mterms[['dimnames']][[2]]  # use the variable names from the movie dataset

# probability of topic assignments across flowers
ClustAssign_movie = ldac_movie@gamma   # this is a matrix with the row as the user and column as the topic
rownames(ClustAssign_movie)=mterms[['dimnames']][[1]]

targeted_movie_ClustAssign = ClustAssign_movie[rownames(ClustAssign_movie) == release_movie,]
boxplot(targeted_movie_ClustAssign,xlab="Topic",ylab="Probability of Topic") #target movie boxplot

ClustAssign_movie%*%ClustTopics_movie
#distance_matrix <- parallelDist(ClustAssign_movie, method="hellinger")
#distance_matrix

topicdist = ClustAssign_movie-matrix(ClustAssign_movie[release_movie,],nrow=nrow(ClustAssign_movie),ncol=ncol(ClustAssign_movie),byrow=T)
topicdistss = sqrt(apply(topicdist^2,1,sum))
topicdistss = sort(topicdistss)

#typeof(topicdistss)
topicdist_names = as.numeric(names(topicdistss))

movie2014 = opus_movies[opus_movies['release_date'] >= '2014-01-01' & opus_movies['release_date'] <= '2014-12-31',]
movie2014_dist = movie2014[match(topicdist_names, movie2014$odid),]
movie2014_dist = movie2014_dist[,c(2,3,17,20,21,22,23)]
View(movie2014_dist)
