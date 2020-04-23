if(!require(shiny)) install.packages("shiny")
if(!require(here)) install.packages("here")
if(!require(readr)) install.packages("readr")
if(!require(stringr)) install.packages("stringr")
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(ggplot2)) install.packages("ggplot2")

urlfile="https://raw.githubusercontent.com/eparker12/nCoV_tracker/master/input_data/jhu_data.csv"
# mydata <- readr::read_csv(url(urlfile))

readUrl <- function(url) {
  out <- tryCatch(
    {
      # Just to highlight: if you want to use more than one 
      # R expression in the "try" part then you'll have to 
      # use curly brackets.
      # 'tryCatch()' will return the last evaluated expression 
      # in case the "try" part was completed successfully
      
      message("This is the 'try' part")
      
      readr::read_csv(url(url))
      # The return value of `readLines()` is the actual value 
      # that will be returned in case there is no condition 
      # (e.g. warning or error). 
      # You don't need to state the return value via `return()` as code 
      # in the "try" part is not wrapped insided a function (unlike that
      # for the condition handlers for warnings and error below)
      
    },
    error=function(cond) {
      message(paste("URL does not seem to exist:", url))
      message("Here's the original error message:")
      message(cond)
      # Choose a return value in case of error
      return(NA)
      # TODO: in case of error, read latest downlaoded data
    },
    warning=function(cond) {
      message(paste("URL caused a warning:", url))
      message("Here's the original warning message:")
      message(cond)
      # Choose a return value in case of warning
      return(readr::read_csv(url(url)))
    },
    finally={
      # NOTE:
      # Here goes everything that should be executed at the end,
      # regardless of success or error.
      # If you want more than one expression to be executed, then you 
      # need to wrap them in curly brackets ({...}); otherwise you could
      # just have written 'finally=<expression>' 
      message(paste("Processed URL:", url))
      message("Data red in")
    }
  )    
  return(out)
}

mydata <- readUrl(urlfile)

# Select columns using partial match

# first use regex to form binary vector. Regex details: https://stringr.tidyverse.org/articles/regular-expressions.html
# nordics <- str_detect(colnames(mydata), "Date|Finland.|Sweden.|Iceland.|Norway.|Denmark.")
fin <- str_detect(colnames(mydata), "Date|Finland.")
if(is_empty(fin)) { stop(paste0("Error: Data for Finland not found")) }

# then use gained binary vector to select desired columns from the data
# nordics <- mydata[ , nordics,  drop=FALSE]
fin <- mydata[ , fin,  drop=FALSE]
colnames(fin) <- c("Date", "Cases", "Deaths", "Recovered")

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
    selectInput('xcol', 'X Variable', names(fin)),
    checkboxGroupInput('ycol', 'Y Variable', names(fin),
                       selected = names(fin)[[2]])
  ),
  
  # Plot - output
  mainPanel(
    plotOutput('lineplot2'),
    plotOutput('hist'),
    
    a("Data source",
      href = "http://www.rstudio.com/
      products/shiny/shiny-user-showcase/")
  )
  )

server <- function(input, output) {
  
  selectedData <- reactive({
    # select all rows and columns specified in ui
    fin[, c(input$xcol, input$ycol)]
  })

  output$lineplot1 <- renderPlot({
    par(mar = c(5.1, 4.1, 0, 1))
    title <- "jokin otsikko"
    lines(selectedData())
  })
  
  output$hist <- renderPlot({
    title <- "100 random normal values"
    hist(rnorm(100), main = title)
  })
}

shinyApp(ui = ui, server = server)
