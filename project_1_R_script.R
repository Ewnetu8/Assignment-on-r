library(recommenderlab)
library(tidyverse)
library(reshape2)
setwd('/home/ewnetu/ALL Computational Data Science Courses/Second Yeaar CDSC/First Semister/COdata Research ICTP/Project_1_Ewnetu/')

df <- read.csv('Project.csv')
str(df)
summary(df)
head(df)

ratingMatrix <- dcast((df), userId~movieId, value.var = "rating", na.rm=FALSE)
ratingMatrix <- as.matrix(ratingMatrix[,-1]) #remove userIds
#Convert rating matrix into a recommenderlab sparse matrix
ratingMatrix <- as(ratingMatrix, "realRatingMatrix")
image(ratingMatrix[1:20, 1:30], axes = FALSE, main = "Heatmap for first 20 users and 30 movies")

#ratingMatrix <- normalize(ratingMatrix)

recommendation_model <- recommenderRegistry$get_entries(dataType = "realRatingMatrix")
names(recommendation_model)
lapply(recommendation_model, "[[", "description")

recommendation_system <- recommenderRegistry$get_entries(dataType ="realRatingMatrix")
recommendation_system$IBCF_realRatingMatrix$parameters

sampled_data<- sample(x = c(TRUE, FALSE),
                      size = nrow(ratingMatrix),
                      replace = TRUE,
                      prob = c(0.8, 0.2))
training_data <- ratingMatrix[sampled_data, ]
testing_data <- ratingMatrix[!sampled_data, ]

training_data
testing_data


recommen_model <- Recommender(data = training_data,
                              method = "IBCF",
                              parameter = list(k = 30))
recommen_model
class(recommen_model)

model_info <- getModel(recommen_model)
class(model_info$sim)
dim(model_info$sim)
top_items <-40 
image(model_info$sim[1:top_items, 1:top_items],
      main = "Heatmap of the first rows and columns")

top_recommendations <- 10 # the number of items to recommend to each user
predicted_recommendations <- predict(object = recommen_model,
                                     newdata = testing_data,
                                     n = top_recommendations)
predicted_recommendations

user1 <- predicted_recommendations@items[[1]] # recommendation for the first user
movies_user1 <- predicted_recommendations@itemLabels[user1]
print('top 10 recommended movies id for user 1')
movies_user1

