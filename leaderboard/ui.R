library(shiny)
library(googlesheets)
library('DT')

shinyUI(
  fluidPage(
    titlePanel('paper reading club leaderboard'),
    fluidRow(
      tabsetPanel(
        tabPanel('leaderboard',
          fluidPage(
            plotOutput('p_user_count'),
            DT::dataTableOutput("user_count")
            )
          ),
        tabPanel('DOI info',
          {
            DT::dataTableOutput("doi_info")
          }),
        tabPanel('raw data',
          {
            dataTableOutput('raw_data')
          })
        ) # end tabsetPanel
      )
    )
  )
