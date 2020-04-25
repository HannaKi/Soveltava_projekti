# load libraries

if(!require(readr)) install.packages("readr")
if(!require(stringr)) install.packages("stringr")
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(ggplot2)) install.packages("ggplot2")
library(reshape2)

# Download data. NOTE! Fethc raw format

urlfile="https://raw.githubusercontent.com/eparker12/nCoV_tracker/master/input_data/jhu_data.csv"


urlconfirmed <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
urldeath <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
urlrecovered <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv"
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

confirmed <- readUrl(urlconfirmed)
dead <- readUrl(urldeath)
recovered <- readUrl(urlrecovered)


nor <- c("Finland", "Sweden", "Iceland", "Norway", "Denmark")

confirmed <- confirmed %>% filter(`Country/Region` %in% nor) %>% mutate(Case = "Confirmed")
dead <- dead %>% filter(`Country/Region` %in% nor) %>% mutate(Case = "Dead")
recovered <- recovered %>% filter(`Country/Region` %in% nor) %>% mutate(Case = "Recovered")

dat <- bind_rows(confirmed, recovered, dead)


fin <- reshape2::melt(fin, id.vars = 'Date')

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

tes <- gather(fin, condition, nums, Cases:Recovered)
tes2 <- melt(fin, id.vars = 'Date')

# ggplot(fin, aes(x=fin$Date, y=fin$Cases)) +
#   geom_line()

p <- ggplot(data = fin,
            mapping = aes(x = Date,
                          y = Cases))
p + geom_line(aes())
