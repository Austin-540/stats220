library(tidyverse)
library(rvest)

beehive <- readRDS("beehive")
ministers <- readRDS("ministers")

beehive <- beehive %>%
  mutate(minister_clean = str_remove_all(ministers, "Hon |Rt "))

minister_parties <- ministers %>%
  filter(label == "Party")

ministers_with_parties <- beehive %>%
  separate_rows(minister_clean, sep=";") %>%
  group_by(minister_clean) %>%
  summarise(n()) %>%
  left_join(minister_parties, by=c("minister_clean"="minister"))

ministers_with_parties <- ministers_with_parties %>%
  rename(name=1, releases=2, party_from_wikipedia=6) %>%
  select(name, releases, party_from_wikipedia) %>%
  mutate(party_clean = case_when(
    str_detect(party_from_wikipedia, "National") ~ "National", #Scott Simpson's page says "National Party" instead of "National" like all the others
    str_detect(party_from_wikipedia, "Labour") ~ "Labour",
    str_detect(party_from_wikipedia, "Green") ~ "Green",
    str_detect(party_from_wikipedia, "ACT") ~ "ACT",
    str_detect(party_from_wikipedia, "NZ First") ~ "NZ First", #Shane Jones' party affiliation on Wikipedia is listed with the date included, so this cleaning step is necessary
    str_detect(party_from_wikipedia, "Maori|Māori") ~ "Māori",
    TRUE ~ "Unknown Party"
    
  ),
  ) %>%
  group_by(party_clean) %>%
  mutate(party_max_num_releases = max(releases)) %>% #So that I can make the legend show in non-alphabetical order
  ungroup()




wikipedia_parties_page <- read_html("https://en.wikipedia.org/wiki/List_of_political_parties_in_New_Zealand")

wikipedia_table <- wikipedia_parties_page %>% html_element(".wikitable")

header_done <- FALSE
party_data <- tibble()
for (row in wikipedia_table %>% html_elements("tr")) { #woah its a for loop before module 6
  if (!header_done) {#The header row doesn't have any data in it
  header_done <- TRUE
  next #skip the rest of this iteration
  }
  
  
  party_colour <- row %>% 
    html_element("td") %>% 
    html_attr("style") %>%
    substr(18, 24)
  
  party_name <- row %>% #I promise chatgpt didn't write this
    html_elements("td:nth-child(2)") %>% #there was a weird error message where R didn't want to let me index into the list. So here I used a fancy html selector instead
    html_text2()
  
  party_data <- bind_rows(party_data, tibble(party_colour=c(party_colour), party_name=c(party_name)))
}

party_data <- party_data %>% #Same as before, but now its because most of them have the word "Party" after them
mutate(party_clean = case_when(
  str_detect(party_name, "National") ~ "National",
  str_detect(party_name, "Labour") ~ "Labour",
  str_detect(party_name, "Green") ~ "Green",
  str_detect(party_name, "ACT") ~ "ACT",
  str_detect(party_name, "First") ~ "NZ First", 
  str_detect(party_name, "Maori|Māori") ~ "Māori",
  TRUE ~ "Unknown Party"
))

ministers_with_parties <- ministers_with_parties %>%
  left_join(party_data, by=c("party_clean"="party_clean"))

ministers_with_parties %>%
  ggplot(aes(fill=reorder(party_colour, -party_max_num_releases))) +
  geom_col(aes(y=reorder(name, +releases), x=releases)) +
  theme_minimal() +
  labs(
    title = "Who is making Energy Portfolio announcements in the current government?",
    x = "Number of releases",
    y = "Minister",
    caption = "Sources: beehive.govt.nz (releases, data collected 20/05/2026), Wikipedia (party affiliation & colours, data collected 24/05/2026) ",
    fill = "Party"
  ) +
  theme(
    panel.grid.major.y = element_blank()
  ) +
  scale_fill_identity(
    guide = "legend", #by default there is no guide
    labels = c("National", "NZ First", "ACT") #I tried to make this dynamic but it was way too much effort so i just hardcoded these labels
  )
  
ggsave("my_viz.png", units = "in", width = 12, height = 8)


