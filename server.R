## server.R

# load functions
source('functions/cf_algorithm.R') # collaborative filtering
source('functions/similarity_measures.R') # similarity measures

# define functions
get_user_ratings = function(value_list) {
    dat = data.table(MovieID = sapply(strsplit(names(value_list), "_"), 
                                      function(x) ifelse(length(x) > 1, x[[2]], NA)),
                     Rating = unlist(as.character(value_list)))
    dat = dat[!is.null(Rating) & !is.na(MovieID)]
    dat[Rating == " ", Rating := 0]
    dat[, ':=' (MovieID = as.numeric(MovieID), Rating = as.numeric(Rating))]
    dat = dat[Rating > 0]
    
    # get the indices of the ratings
    # add the user ratings to the existing rating matrix
    user_ratings <- sparseMatrix(i = dat$MovieID, 
                                 j = rep(1,nrow(dat)), 
                                 x = dat$Rating, 
                                 dims = c(nrow(ratingmat), 1))
}
                     
top_n_most_popular <- function(movie_rating_df, genre, n = 5){
  s = movie_rating_df[ (movie_rating_df[genre] == TRUE) & (movie_rating_df$rating_count > 100),] #alteast 100 ratings
  s$Genre = genre
  return(s[order(-s$rating_count),][1:n,c("Title","MovieID","Genre","rating_count")])
}

# read in data
myurl = "https://liangfgithub.github.io/MovieData/"
movies = readLines(paste0(myurl, 'movies.dat?raw=true'))
movies = strsplit(movies, split = "::", fixed = TRUE, useBytes = TRUE)
movies = matrix(unlist(movies), ncol = 3, byrow = TRUE)
movies = data.frame(movies, stringsAsFactors = FALSE)
colnames(movies) = c('MovieID', 'Title', 'Genres')
movies$MovieID = as.integer(movies$MovieID)
movies$Title = iconv(movies$Title, "latin1", "UTF-8")

small_image_url = "https://liangfgithub.github.io/MovieImages/"
movies$image_url = sapply(movies$MovieID, 
                          function(x) paste0(small_image_url, x, '.jpg?raw=true'))
ratings = read.csv(paste0(myurl, 'ratings.dat?raw=true'), 
                   sep = ':',
                   colClasses = c('integer', 'NULL'), 
                   header = FALSE)
colnames(ratings) = c('UserID', 'MovieID', 'Rating', 'Timestamp')
ratings$Timestamp = NULL

# reshape to movie x user matrix 
ratingmat <- sparseMatrix(ratings$MovieID, ratings$UserID, x=ratings$Rating) # movie x user matrix
ratingmat <- ratingmat[, unique(summary(ratingmat)$j)] # remove users with no ratings
dimnames(ratingmat) <- list(movie_id = as.character(1:3952), user_id = as.character(sort(unique(ratings$UserID))))
#dimnames(ratingmat) <- list(MovieID = as.character(1:10000), UserID = as.character(sort(unique(ratings$UserID))))

#SysI movie data summary
sys1_movie_data = readRDS("data/movies_for_sys1.rdata")

                          
shinyServer(function(input, output, session) {
    
    # show the books to be rated
    output$ratings <- renderUI({
        num_rows <- 20
        num_movies <- 6 # movies per row
        
        lapply(1:num_rows, function(i) {
            list(fluidRow(lapply(1:num_movies, function(j) {
                list(box(width = 2,
                         div(style = "text-align:center", img(src = movies$image_url[(i - 1) * num_movies + j], height = 150)),
                         #div(style = "text-align:center; color: #999999; font-size: 80%", books$authors[(i - 1) * num_books + j]),
                         div(style = "text-align:center", strong(movies$Title[(i - 1) * num_movies + j])),
                         div(style = "text-align:center; font-size: 150%; color: #f0ad4e;", ratingInput(paste0("select_", movies$MovieID[(i - 1) * num_movies + j]), label = "", dataStop = 5)))) #00c0ef
            })))
        })
    })
    
    # Calculate recommendations when the sbumbutton is clicked
    df <- eventReactive(input$btn, ignoreInit = T, {
        withBusyIndicatorServer("btn", { # showing the busy indicator
            
            print("sys2 button was clicked")
            
            # hide the rating container
            useShinyjs()
            jsCode <- "document.querySelector('[data-widget=collapse]').click();"
            runjs(jsCode)
            
            # get the user's rating data
            value_list <- reactiveValuesToList(input)
            user_ratings <- get_user_ratings(value_list)
            
            
            print(user_ratings)
            
            
            # add user's ratings as first column to rating matrix
            rmat <- cbind(user_ratings, ratingmat)
            
            # get the indices of which cells in the matrix should be predicted
            # predict all books the current user has not yet rated
            items_to_predict <- which(rmat[, 1] == 0)
            prediction_indices <- as.matrix(expand.grid(items_to_predict, 1))
            
            # run the ubcf-alogrithm
            res <- predict_cf(rmat, prediction_indices, "ubcf", TRUE, cal_cos, 1000, FALSE, 2000, 1000)
            
            # sort, organize, and return the results
            user_results <- sort(res[, 1], decreasing = TRUE)[1:10]
            user_predicted_ids = c()
            for(i in 1:length(user_results)){
                user_predicted_ids <- append(user_predicted_ids, which(movies$MovieID == as.numeric(names(user_results[i]))))
            }
            recom_results <- data.table(Rank = 1:10, 
                                        MovieID = movies$MovieID[user_predicted_ids], 
                                        Title = movies$Title[user_predicted_ids], 
                                        Predicted_rating =  user_results)
            
        }) # still busy
        
    }) # clicked on button
    
    
   
    
    # Calculate recommendations when the sbumbutton is clicked: System I
    df2 <- eventReactive(input$btn_show_genre_results,ignoreInit = T, {
        withBusyIndicatorServer("btn_show_genre_results", { # showing the busy indicator
           
            value_list <- reactiveValuesToList(input)
            
            # get the user's rating data
            user_genre <-  value_list$genre_input
            user_num_movies_str <- value_list$num_movies
            
            if(user_num_movies_str == "Top 3"){
               user_num_movies = 3
            }else if (user_num_movies_str == "Top 5"){
               user_num_movies = 5
            }else if(user_num_movies_str == "Top 10"){
               user_num_movies = 10
            }
            
           
            
            #print(user_genre)
            #print(user_num_movies)
            topmovies = top_n_most_popular(sys1_movie_data, user_genre,user_num_movies)
            #print(topmovies)
            return(list(toplist = topmovies,num_movies = user_num_movies))
        }) # still busy
        
    }) # clicked on button
    

      
    
    # display the recommendations
    output$results <- renderUI({
        num_rows <- 2
        num_movies <- 5
        recom_result <- df()
        print(recom_result)
        lapply(1:num_rows, function(i) {
            list(fluidRow(lapply(1:num_movies, function(j) {
                c_movie_id = which(movies$MovieID == recom_result$MovieID[(i - 1) * num_movies + j])
                box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Rank ", (i - 1) * num_movies + j),
                    
                    div(style = "text-align:center", 
                        a(img(src = movies$image_url[c_movie_id], height = 150))
                    ),
                    div(style="text-align:center; font-size: 100%", 
                        strong(movies$Title[c_movie_id])
                    )
                    
                )        
            }))) # columns
        }) # rows
        
    }) # renderUI function
    
    
    # display the recommendations for system1
    output$results2 <- renderUI({
        
         #value_list <- reactiveValuesToList(input)
         #user_num_movies <- value_list$num_movies
        
        res_ <- df2()
        recom_result = res_$toplist
        
        num_movies <- as.numeric(res_$num_movies)
        col_count = min(num_movies,5)
        
        if(num_movies <= 5){
            num_rows <- 1    
        }else if(num_movies == 10){
            num_rows <- 2
            
        }
        
        
        
        
        lapply(1:num_rows, function(i) {
            list(fluidRow(lapply(1:col_count, function(j) {
                c_movie_id = which(movies$MovieID == recom_result$MovieID[(i - 1) * col_count + j])
                
                box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Rank ", (i - 1) * col_count + j),
                    
                    div(style = "text-align:center", 
                        a(img(src = movies$image_url[c_movie_id], height = 150))
                    ),
                    div(style="text-align:center; font-size: 100%", 
                        strong(movies$Title[c_movie_id])
                    )
                    
                )        
            }))) # columns
        }) # rows
        
    }) # renderUI function
    
    
    
}) # server function
