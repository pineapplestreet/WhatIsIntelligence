#########################################################################
# required libraries
library(dplyr)
library(ggplot2)
library(gganimate)
library(gridExtra)
library(scales)
library(stringr)
library(tictoc)
library(tidyr)
library(transformr)
library(zoo)

#########################################################################
# load a single dataframe
get_dataframe <- function(item, folder="data") {
  tmp <- readRDS(paste0(folder,"/",item,".Rds")) 
  return(tmp)
}

#########################################################################
# load up all the needed Rds files as a combined dataframe
get_combined_dataframe <- function(sources, folder="data") {
  combined_data <- data.frame()
  for(item in sources) {
    print(item)
    print(folder)
    tmp <- get_dataframe(item, folder)
    combined_data <- rbind(combined_data,tmp)
  }
  return(combined_data)
}

#########################################################################
# function to make ngrams
# only include ngrams of length n, ignore the cutoff
get_ngrams <- function(data, n) {
  ngrams <- data %>%
    mutate(
      ngram=sapply(
        1:nrow(.),
        function(x) paste(symbol[pmax(1, x):pmin(x + n-1, nrow(.))], collapse = "")
      )) %>%
    mutate(length=str_length(ngram)) %>%
    filter(length==n) %>%
    mutate(n=length) %>%
    select(source, n, symbol_order, ngram)
}  

#########################################################################
# function to make ngrams of length x to y
get_ngrams_iterate <- function(data, sequence) {
  response <- data.frame()
  for(i in sequence) {
    print(paste0(max(data$source),", n=",i))
    response<- rbind(response,get_ngrams(data, i))
  }
  return(response)
} 