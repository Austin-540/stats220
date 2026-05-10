library(tidyverse)

logged_data <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRetKKj9bMRzvsbYOusSWE0uw2oEJIxmDjFP6C2U79SCUlN05jlOH4OHHo5kVT4mpo5BdYi9q9NZh7w/pub?gid=49993136&single=true&output=csv")

logged_data <- logged_data %>%
  rename(submitted_timestamp = 1,
         method = 2,
         duration = 3,
         start_time = 4)

my_colours = c("#ECEBE4", "#49306B", "#ED7B84", "#6290C8")

ggplot(logged_data) +
  geom_histogram(aes(x=duration, fill=method),
                 binwidth = 5) +
  theme(
    plot.background = element_rect(fill= my_colours[1]),
    panel.background = element_rect(fill= my_colours[1]),
    panel.grid = element_blank(),
    
  ) +
  guides(
    fill = guide_legend( #From the tidyverse docs
      title = "Who commuted?",
    )
  ) + 
  scale_fill_discrete( #Also from the tidyverse docs
    labels = c("Austin", "Armand"),
    palette = my_colours[2:4]
  ) + 
  labs(
    title = "How long does it take Austin and Armand to commute to/from university?",
    x = "Length of commute in minutes (rounded to the nearest 5 minutes)",
    y = "Number of commutes"
  )
#Based on data frames you have created by manipulating data x2 ❌
#Uses timestamp ❌
#From a dataframe that uses group_by() and summarise() ❌


logged_data <- logged_data %>%
  mutate(
    am_or_pm = ifelse( #I'm using this indirectly to tell if the commute was to or from uni
      hms(start_time)$hour >= 12,
      "PM",
      "AM"
    )
  )


