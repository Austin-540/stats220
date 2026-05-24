library(tidyverse)

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

ministers_with_parties %>%
  ggplot() +
  geom_col(aes(y=reorder(name, +releases), x=releases, fill=reorder(party_clean, -party_max_num_releases))) +
  theme_minimal() +
  labs(
    title = "Who is making Energy Portfolio announcements in the current government?",
    x = "Number of releases",
    y = "Minister",
    fill = "Party",
    caption = "Sources: beehive.govt.nz (releases, data collected 20/05/2026), Wikipedia (party affiliation, data collected 24/05/2026) "
  ) +
  scale_fill_discrete(
    palette = c("#00529F", "#000000", "#FDE401"),
    ) +
  theme(
    panel.grid.major.y = element_blank()
  )
  
ggsave("my_viz.png", units = "in", width = 12, height = 8)


