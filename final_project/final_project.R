library(shiny)
library(proxy)
#setwd("E:\\Igor\\CUNY\\DATA 643 - Recommender Systems\\Projects\\Project_05\\Data")
ratings<-read.csv("ratings.csv",stringsAsFactors=FALSE)
movies<-read.csv("movies.csv",stringsAsFactors=FALSE)
result<-read.csv("saved_result.csv",stringsAsFactors=FALSE)
result<-result[,-c(1)]
#genre_matrix3<-read.csv("saved_genre_matrix3.csv",stringsAsFactors=FALSE)
#genre_matrix3<-genre_matrix3[,-c(1)]

ratings2 <- ratings[,2:3]
ratings2 <- aggregate(ratings2[, 2], list(ratings2$movieId), mean)
colnames(ratings2)[1] <- "Movie Id"
colnames(ratings2)[2] <- "Mean Rating"
 


ratings_ordered <- ratings2[order(-ratings2$"Mean Rating"),]
movie_title<-movies[,c(1:2)]


movie_title2<-merge(x=ratings_ordered ,y=movie_title,by.x=c("Movie Id"),by.y=c("movieId"))
    movie_title3<-movie_title2
    movie_title4<-movie_title3[order(-movie_title3$"Mean Rating"),]
    mean_rating<- movie_title4[,3:2]

ratings3 <- ratings[,2:3]
ratings3 <- aggregate(ratings3[, 2], list(ratings3$movieId), length)
colnames(ratings3)[1] <- "Movie Id"
colnames(ratings3)[2] <- "Number of Ratings"
ratings_ordered3 <- ratings3[order(-ratings3$"Number of Ratings"),]
movie_title21<-merge(x=ratings_ordered3 ,y=movie_title,by.x=c("Movie Id"),by.y=c("movieId"))
movie_title31<-movie_title21
movie_title41<-movie_title31[order(-movie_title31$"Number of Ratings"),]
rating_count<- movie_title41[,3:2]

# % of ratings 4+
ratings4 <- ratings[,2:3]
ratings4_top <- ratings[ratings$rating>=4,2:3]
ratings4 <- aggregate(ratings4[, 2], list(ratings4$movieId), length)
ratings4_top <- aggregate(ratings4_top[, 2], list(ratings4_top$movieId), length)
colnames(ratings4)[1] <- "Movie Id"
colnames(ratings4)[2] <- "Number of Ratings"
colnames(ratings4_top)[1] <- "Movie Id"
colnames(ratings4_top)[2] <- "Number of Ratings"
ratings4_pct <- merge(x=ratings4_top ,y=ratings4,by.x=c("Movie Id"),by.y=c("Movie Id"))
ratings4_pct$pct_top_ratings <- (ratings4_pct$"Number of Ratings.x"/ratings4_pct$"Number of Ratings.y")*100

ratings4_pct <- ratings4_pct[ratings4_pct$"Number of Ratings.y">100,]
ratings4_pct <- ratings4_pct[order(-ratings4_pct$"pct_top_ratings"),]
movie_title22<-merge(x=ratings4_pct ,y=movie_title,by.x=c("Movie Id"),by.y=c("movieId"))
movie_title32<-movie_title22
movie_title42<-movie_title32[order(-movie_title32$"pct_top_ratings"),]
top_pct_rating<- movie_title42
colnames(top_pct_rating)[2] <- "Number of Ratings 4+"
colnames(top_pct_rating)[3] <- "Total Number of Ratings"
colnames(top_pct_rating)[4] <- "Percent 4+ Ratings"
colnames(top_pct_rating)[5] <- "Title"
top_pct_rating <- top_pct_rating[,c(5,2,3,4)]

#mean_rating<-read.csv("custom_mean_rating.csv")
#rating_count<-read.csv("custom_rating_count.csv")
#top_pct_rating <-read.csv("custom_top_pct_rating.csv")

your_choices <- as.list(movies$movieId)
names(your_choices) <- movies$title

library(reshape2)
#Create ratings matrix. Rows = userId, Columns = movieId
ratingmat <- dcast(ratings, userId~movieId, value.var = "rating", na.rm=FALSE)
ratingmat <- as.matrix(ratingmat[,-1]) #remove userIds

library(recommenderlab)
#Convert rating matrix into a recommenderlab sparse matrix
ratingmat <- as(ratingmat, "realRatingMatrix")
 
#Normalize the data
ratingmat_norm <- normalize(ratingmat)
 
#Create Recommender Model. "UBCF" stands for User-Based Collaborative Filtering
recommender_model <- Recommender(ratingmat_norm, method = "UBCF", param=list(method="Cosine",nn=30))
  recom <- predict(recommender_model, ratingmat[1], n=10) #Obtain top 10 recommendations for 1st user in dataset
  recom_list <- as(recom, "list") #convert recommenderlab object to readable list
 
   #Obtain recommendations
   recom_result <- matrix(0,10)
   for (i in c(1:10)){
     recom_result[i] <- movies[as.integer(recom_list[[1]][i]),2]
   }
   ubcf<- as.data.frame(recom_result)

#evaluation_scheme <- evaluationScheme(ratingmat, method="cross-validation", k=5, given=3, goodRating=5) #k=5 meaning a 5-fold cross validation. given=3 meaning a Given-3 protocol
#evaluation_results <- evaluate(evaluation_scheme, method="UBCF", n=c(1,3,5,10,15,20))
#eval_results <- getConfusionMatrix(evaluation_results)[[1]]


### User-User Collaborative filtering
ratings1 <- ratings[,c(1,2,3)]
ratingmat1 <- dcast(ratings1, userId~movieId, value.var = "rating", na.rm=FALSE)
ratingmat1 <- as.matrix(ratingmat1[,-1]) 
ratingmat1 <- as(ratingmat1, "realRatingMatrix")
ratingmat1 <- ratingmat1[rowCounts(ratingmat1)>= 50,colCounts(ratingmat1)>=50 ]
set.seed(1)
percentage_training <-0.8
items_to_keep <- 5
rating_threshold <- 3
n_eval <- 1
eval_sets <- evaluationScheme(data=ratingmat1, method="split", train=percentage_training, given=items_to_keep,
goodRating=rating_threshold,k=n_eval)
d<-eval_sets
getData(eval_sets, "train")
predict_by_method <- function(p_data,p_method,p_normalize,p_dist_method,p_n)
{
recommender_model <- Recommender(getData(p_data, "train"), p_method, param=list(p_normalize ,p_dist_method,p_n))
recom <- predict(recommender_model,getData(p_data, "known"), type="ratings")
predict_acc_ubcf_cosine_z <- calcPredictionAccuracy(recom, getData(p_data, "unknown"))[1] 
return (predict_acc_ubcf_cosine_z)
}
predict_acc_ubcf_cosine_z <- predict_by_method(d,"UBCF","Z-Score","Cosine",30)


#evaluate performance of recommenders
eval_ubcf_list <- list(
  "Center_Cosine" = list(name="UBCF", param=list(normalize = "center",
                                         method="Cosine",
                                         nn=30)),
  "Z-score_Cosine" = list(name="UBCF", param=list(normalize = "Z-score",
                                         method="Cosine",
                                         nn=30)),
  "Center_jaccard" = list(name="UBCF", param=list(normalize = "center",
                                         method="jaccard",
                                         nn=30)),
  "Z-score_jaccard" = list(name="UBCF", param=list(normalize = "Z-score",
                                         method="jaccard",
                                         nn=30)),
  "Center_pearson" = list(name="UBCF", param=list(normalize = "center",
                                         method="pearson",
                                         nn=30)),
  "Z-score_pearson" = list(name="UBCF", param=list(normalize = "Z-score",
                                         method="pearson",
                                         nn=30))
)
number_of_recomm <- c(1,5, 10, 15, 20, 25)
eval_ubcf <- evaluate(d, eval_ubcf_list , n = number_of_recomm , progress = FALSE)


#Content-based filtering

genres <- as.data.frame(movies$genres, stringsAsFactors=FALSE)
library(data.table)
genres2 <- as.data.frame(tstrsplit(genres[,1], '[|]', type.convert=TRUE), stringsAsFactors=FALSE)
colnames(genres2) <- c(1:7)


genre_list <- c("Action", "Adventure", "Animation", "Children", "Comedy", "Crime","Documentary", "Drama", "Fantasy","Film-Noir", "Horror", "Musical", "Mystery","Romance","Sci-Fi", "Thriller", "War", "Western")
 
genre_matrix <- matrix(0,9126,18) #empty matrix
genre_matrix[1,] <- genre_list #set first row to genre list
colnames(genre_matrix) <- genre_list #set column names to genre list
 
#iterate through matrix
for (i in 1:nrow(genres2)) {
 for (c in 1:ncol(genres2)) {
 genmat_col = which(genre_matrix[1,] == genres2[i,c])
 genre_matrix[i+1,genmat_col] <- 1
 }
}
 
#convert into dataframe
genre_matrix2 <- as.data.frame(genre_matrix[-1,], stringsAsFactors=FALSE) #remove first row, which was the genre list
for (c in 1:ncol(genre_matrix2)) {
  genre_matrix2[,c] <- as.integer(genre_matrix2[,c])
} #convert from characters to integers


binaryratings <- ratings
for (i in 1:nrow(binaryratings)){
 if (binaryratings[i,3] > 3){
   binaryratings[i,3] <- 1
 }
 else{
   binaryratings[i,3] <- -1
 }
}
binaryratings <-  binaryratings[,c(1:3)]

binaryratings2 <- dcast(binaryratings, movieId~userId, value.var = "rating", na.rm=FALSE)
for (i in 1:ncol(binaryratings2)){
  binaryratings2[which(is.na(binaryratings2[,i]) == TRUE),i] <- 0
}
binaryratings2 = binaryratings2[,-1] #remove movieIds col. Rows are movieIds, cols are userIds



#Remove rows that are not rated from movies dataset
movieIds <- length(unique(movies$movieId)) #8570
ratingmovieIds <- length(unique(ratings$movieId)) #8552
movies2 <- movies[-which((movieIds %in% ratingmovieIds) == FALSE),]
rownames(movies2) <- NULL
#Remove rows that are not rated from genre_matrix2
genre_matrix3 <- genre_matrix2[-which((movieIds %in% ratingmovieIds) == FALSE),]
rownames(genre_matrix3) <- NULL

genre_matrix3 <- genre_matrix3[1:9066,]
#Calculate dot product for User Profiles
result = matrix(0,18,671)
for (c in 1:ncol(binaryratings2)){
  for (i in 1:ncol(genre_matrix3)){
    result[i,c] <- sum((genre_matrix3[,i]) * (binaryratings2[,c]))
  }
}
## END Content based filtering

server <- function(input, output) {

  

  datasetInput <- reactive({
    switch(input$dataset,
           "Mean Rating" = mean_rating,
           "Popularity (Number of Ratings)" = rating_count,
           "Percent of Ratings 4+" = top_pct_rating)
  })

datasetInput2 <- reactive({
    
  })

datasetInput3 <- reactive({
    predict_by_method(d,input$recommendation_method_3,input$normalization_method_3,input$distance_method_3,input$neighborhood_size_3)

  })




  output$caption3 <- renderPrint({
     datasetInput3()
  })

  output$summary <- renderPrint({
    dataset <- datasetInput()
    summary(dataset)
  })
  
  output$view <- renderTable({
    head(datasetInput(), n = input$obs)
  })
  output$view2 <- renderTable({
    #Create ratings matrix. Rows = userId, Columns = movieId
    ratingmat2 <- dcast(ratings, userId~movieId, value.var = "rating", na.rm=FALSE)
    ratingmat2 <- as.matrix(ratingmat2[,-1]) #remove userIds
    ## prepare correlation matrix
    ratingmat_corr <- ratingmat2
    ratingmat_corr[is.na(ratingmat_corr)] = 0
    ## end - prepare correlation matrix

    corMatrix<-cor(ratingmat_corr[,input$movie2], ratingmat_corr[,], use = 'pairwise.complete.obs')
    melt(corMatrix)
    corMatrix_sub<-subset(melt(corMatrix), value > .19)
    corMatrix_sub2 <- corMatrix_sub[order(-corMatrix_sub$value),]
    movie_title<-movies[movies$movieId %in% corMatrix_sub$Var2,c(1:2)]
    movie_title2<-merge(x=corMatrix_sub2,y=movie_title,by.x=c("Var2"),by.y=c("movieId"))
    movie_title3<-subset(movie_title2, value<1)
    movie_title4<-movie_title3[order(-movie_title3$value),]
    colnames(movie_title4)[3] <- "Correlation"
    colnames(movie_title4)[4] <- "Movie"

    head(movie_title4[,c(4,3)], n = input$obs2)
  })

output$view51 <- renderTable({

      result2 <- result[,input$obs5]
   genre_list <- c("Action", "Adventure", "Animation", "Children", "Comedy", "Crime","Documentary", "Drama", "Fantasy","Film-Noir", "Horror", "Musical", "Mystery","Romance","Sci-Fi", "Thriller", "War", "Western")
   v<- character(0)
   for (i in 1:length(result2)) {
      if (result2[i]==1) {
         v <- c(v, genre_list[i])
      }
   }
   v
})

output$view5 <- renderTable({


    result2 <- result[,input$obs5] #First user's profile
    sim_mat <- rbind.data.frame(result2, genre_matrix3)
    sim_mat <- data.frame(lapply(sim_mat,function(x){as.integer(x)})) #convert data to type integer
 
    #Calculate Jaccard distance between user profile and all movies

    sim_results <- dist(sim_mat, method = "Jaccard")
    sim_results <- as.data.frame(as.matrix(sim_results[1:9066]))
    rows <- which(sim_results == min(sim_results))
    #Recommended movies
    recomm_movies<-movies[rows,2:3]
    #colnames(movie_title4)[3] <- "Correlation"
    #colnames(movie_title4)[4] <- "Movie"

    head(recomm_movies)
  })



   output$plot31 <- renderPlot({
     eval_ubcf_list31 <- list(
     "Center_Cosine" = list(name=input$recommendation_method_3, param=list(normalize = input$normalization_method_3,
                                         method=input$distance_method_3,
                                         nn=input$neighborhood_size_3))
    )
   number_of_recomm31 <- c(1,5, 10, 15, 20, 25)
   eval_ubcf31 <- evaluate(d, eval_ubcf_list31 , n = number_of_recomm31 , progress = FALSE)
 
    plot(x = eval_ubcf31, y = "ROC", annotate = 4)

  })
   output$plot3 <- renderPlot({
    plot(x = eval_ubcf, y = "ROC", annotate = 4)

  })

# output$UUCF_recomm <- renderTable({
#    head(datasetInput(), n = input$obs)
#  })

}

shinyApp(
   shinyUI(
      navbarPage("Recommender Demo",
             tabPanel("Non-Personalized",
      titlePanel("Non-Personalized Recommendations"),
      sidebarLayout(
        sidebarPanel(
          selectInput("dataset", "Choose a Recommender Metric:", 
                  choices = c("Mean Rating", "Popularity (Number of Ratings)", "Percent of Ratings 4+")),
          numericInput("obs", "Number of Records to View:", 10),
          helpText("Non-personalized recommenders recommend items to customers based on what other customers have said about the items on average. 
          Since the recommendations are independent of a specific customer, each customer gets the same recommendation.")
          
    ),

    mainPanel(h4("Recommended Movies"),
      tableOutput("view")
    )
  )
),
            tabPanel(
               "Correlation-Based",
               titlePanel("Correlation-Based Recommendations"),
               sidebarLayout(
                 sidebarPanel(
                   selectInput("movie2", "Select Movie:",choices = your_choices ),
                    numericInput("obs2", "Number of records to view:", 10),
                    helpText("This recommender recommends movies that most often occur with the Selected Movie.")
                    ),

                 mainPanel(h3("Top Movies Highly Correlated with the Selected Movie:"),
                 tableOutput("view2")
                 )
               )
             ),
           tabPanel(
               "Content-Based",
               titlePanel("Content-Based Recommendations"), 
               sidebarLayout(
                 sidebarPanel(
                    numericInput("obs5", "Enter User ID (number between 1 and 6000):", 1),
                    helpText("The Content-Based Filtering approach involves analyzing an item a user interacted with, 
                              and giving recommendations that are similar in content to that item. On this page we will be recommending movies that are similar in genre 
                              to the selected movie. ")
                    ),                   
                 mainPanel(
                 h3("Selected User Genre Preferences:"),  
                 tableOutput("view51"),
                 h3("Movies Recommended for the Selected User:"),
                 tableOutput("view5")
                 
                 )
               )
             ),
           tabPanel(
               "Collaborative Filtering",
               sidebarLayout(
                 sidebarPanel(
                    selectInput("recommendation_method_3","Recommendation Method:", 
                    choices = c("UBCF", "IBCF")),
                    selectInput("normalization_method_3","Normalization Method:", 
                    choices = c("Z-Score", "Center")),
                    selectInput("distance_method_3","Distance Method:", 
                    choices = c("Cosine", "Jaccard","Pearson")), 
                    numericInput("neighborhood_size_3", "Neighborhood size:", 10)
                    ),

                 mainPanel(h3(textOutput("caption3", container = span)),
                 h4("ROC Curve for Selected Parameters"),
                 plotOutput('plot31'),
                 h4("ROC Curves for All Combinations of Parameters"), 
                 plotOutput('plot3'),
                 tableOutput("view3")
                 )
               )
             )



             
  )
)
,server=server)