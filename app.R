if(!require(shiny)) install.packages("shiny")
if(!require(here)) install.packages("here")
if(!require(readr)) install.packages("readr")
if(!require(stringr)) install.packages("stringr")
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(dplyr)) install.packages("dplyr")
if(!require(tidyr)) install.packages("tidyr")

library(plotly)
palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
          "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))

# call helpers.R to gain data
source(here("helpers.R"))

# Build app
ui <- fluidPage(
  
  # HTML content made with Shiny tags
  h1(style = "font-family:Impact",
     "Yet another corona stat"),
  p(style = "font-family:Impact",
    "See other apps in the",
    a("Shiny Showcase",
      href = "http://www.rstudio.com/
      products/shiny/shiny-user-showcase/")
    ),
  
  # Sidebar - input
    
  sidebarPanel(
    selectizeInput("metrics",
                   "Select case:",
                   choices = c("Cases", 
                               "Deaths",
                               "Recovered"),
                   selected = "Cases",
                   multiple = TRUE
    )
  ),
  
  
  # Plot - output
  mainPanel(
    plotOutput('plot'),
  
  # Footer
  a("Data source",
    href = "http://www.rstudio.com/
    products/shiny/shiny-user-showcase/")
  )
  )

server <- function(input, output) {
  
  urlfile="https://raw.githubusercontent.com/eparker12/nCoV_tracker/master/input_data/jhu_data.csv"

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
  
  # Select columns using partial match
  # first use regex to form binary vector. Regex details: https://stringr.tidyverse.org/articles/regular-expressions.html
  nordics <- str_detect(colnames(mydata), "Date|Finland.|Sweden.|Iceland.|Norway.|Denmark.")
  fin <- str_detect(colnames(mydata), "Date|Finland.")
  if(is_empty(fin)) { stop(paste0("Error: Data for Finland not found")) }
  
  # then use gained binary vector to select desired columns from the data
  nordics <- mydata[ , nordics,  drop=FALSE]
  fin <- mydata[ , fin,  drop=FALSE]
  colnames(fin) <- c("Date", "Cases", "Deaths", "Recovered")
  # from wide to long format
  # fin <- gather(fin, condition, nums, Confirmed:Recovered)
  
  
  # https://stackoverflow.com/questions/45286622/plotting-multiple-lines-on-a-single-graph-using-shiny-and-ggplot2
  output$plot = renderPlot({
    #plot.data <- fin
    fin <- melt(fin, id.vars = 'Date')
    #not sure if input$cnt is a list or a vector
    #may need to manipulate that before passing
    vals <- unlist(input$metrics, use.names=FALSE)
    fin <- fin[fin$variable %in% vals, ]
    ggplot(fin) +
      geom_line(mapping = aes(x = Date, y = value, colour = variable)) + 
      labs (x = "Years", y = "Nights spent per 1000", title = "Tourism") + 
      scale_colour_discrete(name = "Country")
  })
}

shinyApp(ui = ui, server = server)
