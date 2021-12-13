## ui.R
library(shiny)
library(shinydashboard)
library(recommenderlab)
library(data.table)
library(ShinyRatingInput)
library(shinyjs)

source('functions/helpers.R')

shinyUI(
    dashboardPage(
        skin = "blue",
        dashboardHeader(title = "Movie Recommender"),
        dashboardSidebar(
            sidebarMenu(
                menuItem("Recommender by Rating", tabName = "movieRecommend"),
                menuItem("Recommender by Genre", tabName = "genre")
            )
        ),
        dashboardBody(
            tabItems(
                tabItem("movieRecommend",
                        includeCSS("css/movies.css"),
                          fluidRow(
                              box(width = 12, title = "Step 1: Rate as many movies as possible", status = "info", solidHeader = TRUE, collapsible = TRUE,
                                  div(class = "rateitems",
                                      uiOutput('ratings')
                                  )
                              )
                          ),
                          fluidRow(
                              useShinyjs(),
                              box(
                                  width = 12, status = "info", solidHeader = TRUE,
                                  title = "Step 2: Discover movies you might like",
                                  br(),
                                  withBusyIndicatorUI(
                                      actionButton("btn", "Click here to get your recommendations", class = "btn-warning")
                                  ),
                                  br(),
                                  tableOutput("results")
                              )
                          )
                ),
                tabItem("genre",
                        fluidRow(
                            column(2,selectInput("genreinput", "Select a Movie Genre:",
                                                  list("Action","Adventure","Animation","Children's","Comedy","Crime","Documentary","Drama","Fantasy","Film-Noir","Horror","Musical",
                                                     "Mystery","Romance","Sci-Fi","Thriller","War","Western"))),
                            column(2,selectInput("nummovies", "Number of Movies to Show:", list("Top 3","Top 5","Top 10"))),
                            column(2,style='padding: 20px 0;',
                            withBusyIndicatorUI(
                            actionButton("btnshowgenreresults", "Show Movie Results"))
                                ),
                            column(6,)
                            
                            ),
                        fluidRow(
                            column(12,style='padding:15px;',
                            tableOutput("results2"))
                            )
                )
            )
        )
    )
) 




