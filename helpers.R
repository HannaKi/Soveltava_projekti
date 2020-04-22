# load libraries

if(!require(readr)) install.packages("readr")
if(!require(stringr)) install.packages("stringr")
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(ggplot2)) install.packages("ggplot2")

# library(readr)
# library(stringr)
# library(tidyverse)

# Download data 
# instruction: https://rpubs.com/lokraj/github_csv

urlfile="https://raw.githubusercontent.com/eparker12/nCoV_tracker/master/input_data/jhu_data.csv"
mydata <- readr::read_csv(url(urlfile))

# Select columns using partial match

# first use regex to form binary vector. Regex details: https://stringr.tidyverse.org/articles/regular-expressions.html
nordics <- str_detect(colnames(mydata), "Date|Finland.|Sweden.|Iceland.|Norway.|Denmark.")
fin <- str_detect(colnames(mydata), "Date|Finland.")

# then use gained binary vector to select desired columns from the data
nordics <- mydata[ , nordics,  drop=FALSE]
fin <- mydata[ , fin,  drop=FALSE]
colnames(fin) <- c("Date","Cases", "Deaths", "Recovered")

ggplot(fin, aes(x=fin$Date, y=fin$Cases)) +
  geom_line()
