library(shiny)
library(stringr)
library(ggplot2)
library(ggthemes)
library(shinythemes)
library(roxygen2)
library(tidyverse)
#library(dplyr)
#library(tidyr)

# Build app and set theme for it
ui <- fluidPage(theme = shinytheme("spacelab"),
          
  # HTML content made with Shiny tags
  h2("Yksi koronastatistiikka lisää..."),

  # Menu - input
  
  fluidRow(
    column(
      width=12,
      checkboxGroupInput("metrics",
                         label = h5("Valitse tapaustyyppi:"),
                         choices = c("Vahvistetut tapaukset", 
                                     "Kuolleet",
                                     "Parantuneet"), 
                         selected = "Vahvistetut tapaukset",
                         inline = TRUE,
                         width="100%"
      )
    )
  ),
  
  # Plot - output

  fluidRow(
    column(
      width=12,
    plotOutput('plot')
    )
  ),
  
  # Footer
  
  fluidRow(
    column(
      width=12,
      p(),
      a("Datan lähde",
        href = "https://raw.githubusercontent.com/eparker12/nCoV_tracker/master/input_data/jhu_data.csv"),
      p(),
      a("Katso koodi GitHubista",
        href = "https://github.com/HannaKi/Soveltava_projekti_tyo")
    )
  )
)
  

server <- function(input, output) {
  
  # Read data in
  
  urlfile="https://raw.githubusercontent.com/eparker12/nCoV_tracker/master/input_data/jhu_data.csv"

  #' Function with try-cath to read a raw csv table from url
  #'
  #' @param url 
  #'
  #' @return csv table as a dataframe
  #'
  #' 
  readUrl <- function(url) {
    out <- tryCatch(
      {
        message("Retrieving data")
        readr::read_csv(url(url))
      },
      error=function(cond) {
        message(paste("URL does not seem to exist:", url))
        message("Here's the original error message:")
        message(cond)
        # Return value in case of error
        return(NA)
        # TODO: in case of error, read latest downloaded data
      },
      warning=function(cond) {
        message(paste("URL caused a warning:", url))
        message("Here's the original warning message:")
        message(cond)
        # Return value in case of warning
        # Since the data causes error we have to accept it!
        return(readr::read_csv(url(url)))
      },
      finally={
        message(paste("Processed URL:", url))
        message("Data red in")
      }
    )    
    return(out)
  }
  
  mydata <- readUrl(urlfile)
  
  # Data manipulation:
  
  # Select columns using partial match
  # first use regex to form binary vector. Regex details: https://stringr.tidyverse.org/articles/regular-expressions.html
  nordics <- stringr::str_detect(colnames(mydata), "Date|Finland.|Sweden.|Iceland.|Norway.|Denmark.")
  fin <- stringr::str_detect(colnames(mydata), "Date|Finland.")
  if(rlang::is_empty(fin)) { 
    stop(paste0("Error: Data for Finland not found")) 
    }
  # then use gained binary vector to select desired columns from the data
  nordics <- mydata[ , nordics,  drop=FALSE]
  fin <- mydata[ , fin,  drop=FALSE]
  colnames(fin) <- c("Date", "Vahvistetut tapaukset", "Kuolleet", "Parantuneet")
  # from wide to long format
  fin <- reshape2::melt(fin, id.vars = 'Date')
  
  # Build output 
  
  output$plot = renderPlot({
    # set ggplot theme. ggplot does NOT follow shiny theme set at UI!
    #theme_set(theme_minimal())
    # list of input values has to be vector
    vals <- unlist(input$metrics, use.names=FALSE)
    fin <- fin[fin$variable %in% vals, ]
    # ggplot 
    ggplot(fin) +
      geom_line(mapping = aes(x = Date, y = value, colour = variable),size=1) + 
      labs (x = "", y = "", title = "Tilastoidut Covid-19 -tapaukset Suomessa") + 
      scale_colour_discrete(name = "") +
      theme_fivethirtyeight()
  })
}

shinyApp(ui = ui, server = server)
