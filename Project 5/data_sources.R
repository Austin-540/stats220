library(tidyverse) #✨
library(jsonlite)
library(rvest)

#Use comments for sections

source("scrape_html.R")

beehive <- map_df(1:4, ~ scrape_search_results(paste0("html/page", ., ".html")))
#check if that code approach is ok

saveRDS(beehive, "beehive")

minister_names <- beehive %>%
  separate_rows(ministers, sep = ";") %>%
  pull(ministers) %>%
  str_remove("Hon ") %>%
  unique()


source("get_wikipedia_infobox.R")

ministers <- map_df(minister_names, ~ get_wikipedia_infobox(.))

saveRDS(ministers, "ministers")
