library(tidyverse) #✨
library(jsonlite)
library(rvest)

source("scrape_html.R") #Load the function

beehive <- map_df(1:4, ~ scrape_search_results(paste0("html/page", ., ".html")))

saveRDS(beehive, "beehive") #Save the beehive website data

#Extract data to search wikipedia
minister_names <- beehive %>%
  separate_rows(ministers, sep = ";") %>%
  pull(ministers) %>%
  str_remove("Hon ") %>%
  unique()


source("get_wikipedia_infobox.R")

ministers <- map_df(minister_names, ~ get_wikipedia_infobox(.)) #search wikipedia

saveRDS(ministers, "ministers") #save the wikipedia data for later
