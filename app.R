if(!require(shiny)) install.packages("shiny")
if(!require(readr)) install.packages("readr")
if(!require(stringr)) install.packages("stringr")
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(dplyr)) install.packages("dplyr")
if(!require(tidyr)) install.packages("tidyr")

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

  fluidRow(
    column(
      width=12,
      checkboxGroupInput("metrics",
                         label = h5("Select case:", style = ("font-family:Impact")),
                         
                         choices = c("Confirmed cases", 
                                     "Deaths",
                                     "Recovered"), 
                         selected = "Confirmed cases",
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
  )),
  
  # Footer
  fluidRow(
    column(
      width=12,
  a("Data source",
    href = "http://www.rstudio.com/
    products/shiny/shiny-user-showcase/")
  
    )
  )
  )

server <- function(input, output) {
  
  urlfile="https://raw.githubusercontent.com/eparker12/nCoV_tracker/master/input_data/jhu_data.csv"

  # function with try-cath to read the url
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
  colnames(fin) <- c("Date", "Confirmed cases", "Deaths", "Recovered")
  # from wide to long format
  fin <- reshape2::melt(fin, id.vars = 'Date')
  
  # https://stackoverflow.com/questions/45286622/plotting-multiple-lines-on-a-single-graph-using-shiny-and-ggplot2
  output$plot = renderPlot({
    # list of input values to vector
    vals <- unlist(input$metrics, use.names=FALSE)
    fin <- fin[fin$variable %in% vals, ]
    ggplot(fin) +
      geom_line(mapping = aes(x = Date, y = value, colour = variable)) + 
      labs (x = "Date", y = "", title = "Corona cases in Finland") + 
      scale_colour_discrete(name = "")
  })
}

shinyApp(ui = ui, server = server)
