library(shiny)
library(googlesheets)
library(DT)
library('dplyr')
library('ggplot2')
library('cowplot')

source('url.R')

# takes a DOI or DOI url and turns into DOI
sanitize_link <- function(doi) {
  if (grepl('*doi*', doi)) {
    doi <- sub('(http)*(s)*[[:punct:]]*doi(.org)?[[:punct:]]*', '', doi)
  }
  doi
}

prc <- gs_url(SHEET_URL)

papers <- gs_read(prc, 'read')
papers <- mutate(papers, doi = sapply(doi, sanitize_link))

user_count <- group_by(papers, handle)
user_count <- summarize(user_count, n = length(doi))

doi_count <- group_by(papers, doi)
doi_count <- summarize(doi_count, n = length(doi))

doi_info <- gs_read(prc, 'read_only_doi')
doi_info <- left_join(doi_count, doi_info, by = c('doi'))

shinyServer(function(input, output, session) {

  output$user_count <- renderDataTable({
    datatable(user_count)
  })

  output$doi_count <- renderDataTable({
    datatable(doi_count)
  })

  output$p_user_count <- renderPlot({
    p <- ggplot(user_count, aes(reorder(handle, -n), n))
    p <- p + geom_bar(stat = 'identity')
    p <- p + xlab('user handle')
    p <- p + ylab('number of papers')
    p <- p + scale_y_continuous(breaks = 1:max(user_count$n))
    p
  })

  output$doi_info <- renderDataTable({
    res <- mutate(doi_info,
      url = ifelse(is_valid_doi, paste0('<a href="', url, '" target="_blank">', doi, '</a>'), doi))
    res <- select(res, -c(is_valid_doi, doi) )
    datatable(res, escape = FALSE)
  })

  output$raw_data <- renderDataTable({
    res <- left_join(papers, doi_info, by = 'doi')
    datatable(res)
  })
})
