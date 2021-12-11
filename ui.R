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
        menuItem("Movie Recommend", tabName = "movieRecommend"),
        menuItem("Genre", tabName = "genre")
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
        tabItem(tabName = "genre",

                            selectInput("genre_input", "Select a Movie Genre:",
                                                  list("Action","Adventure","Animation","Children's","Comedy","Crime","Documentary","Drama","Fantasy","Film-Noir","Horror","Musical",
                                                     "Mystery","Romance","Sci-Fi","Thriller","War","Western")),
                           
               
               
               )
        
      )
    )
  )
)


