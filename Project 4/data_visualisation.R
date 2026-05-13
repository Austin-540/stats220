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
                 binwidth = 5) + #rounds to 5 minutes
  theme(
    plot.background = element_rect(fill= my_colours[1]),
    panel.background = element_rect(fill= my_colours[1]),
    panel.grid = element_blank(),
  ) +
  scale_fill_discrete( #from the ggplot docs
    labels = c("Austin", "Armand"), #"Bus" = Austin, "Car" = Armand
    palette = my_colours[2:4]
  ) + 
  labs(
    title = "How long does it take Austin and Armand to commute to/from university?",
    x = "Length of commute in minutes (rounded to the nearest 5 minutes)",
    y = "Number of commutes",
    fill = "Who commuted?"
  )
ggsave("plot1.png", width = 9, height = 6, unit= "in")



logged_data <- logged_data %>%
  mutate(
    am_or_pm = ifelse( #I'm using this indirectly to tell if the commute was to or from uni
      hms(start_time)$hour >= 12,
      "PM",
      "AM"
    )
  )


logged_data %>%
  ggplot() +
  geom_violin(aes(x=method, y=hour(hms(start_time)), fill=method)) + #Yipee, a function in a function in aes()
  theme(
    plot.background = element_rect(fill= my_colours[1]),
    panel.background = element_rect(fill= my_colours[1]),
    panel.grid = element_blank(),
    legend.position = "none", #Banish the useless legend
    
  ) +
  facet_wrap(
    vars(am_or_pm),
    scale="free"
  ) +
  labs(
    title = "What times of day are we commuting?",
    y = "Commute start hour",
    x = "Who commuted?"
  ) + 
  scale_fill_discrete( #from the ggplot docs
    palette = my_colours[2:4]
  ) +
  scale_y_continuous(
    labels = c(
      "6"="6", "7"="7", "8"="8", "9"="9", "10"="10", "11"="11", 
      "12"="12", "13"="1", "14"="2","15"="3", "16"="4","17"="5", "18"="6","19"="7", "20"="8",
      "21"="9"
      #Make it so it all shows up as 12h time
    ), 
    breaks = c(6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21)
  ) +
  scale_x_discrete(
    labels = c(
      "Bus" = "Austin",
      "Car" = "Armand"
    )
  ) 
ggsave("plot2.png", width = 9, height = 6, unit= "in")



logged_data <- logged_data %>%
  mutate(project_number = ifelse(
    dmy_hms(submitted_timestamp) < ymd("2026-05-01"),
    "Project 2",
    "Project 4"
  )) %>%
  mutate(
    person = case_when(
      method == "Car" ~ "Armand",
      method == "Bus" ~ "Austin",
      TRUE ~ "Unknown person"
    )
  )
           



logged_data %>%
  group_by(project_number, person) %>%
  summarise(median_duration = median(duration)) %>%
  ggplot() +
  geom_col(
    aes(x = project_number, y=median_duration, fill = project_number)
  ) +
  facet_wrap(
    vars(person),
  ) +
  theme(
    plot.background = element_rect(fill= my_colours[1]),
    panel.background = element_rect(fill= my_colours[1]),
    panel.grid = element_blank(),
    legend.position = "none", #Banish the legend
    axis.ticks.x = element_blank(), #Banish the ticks
    axis.text.x = element_text(margin = margin(t=-13)) #Move the labels up a bit so there isnt as much empty space
  ) + 
  scale_fill_discrete( #from the ggplot docs
    palette = my_colours[2:4]
  ) +
  labs(
    title = "Has our median commute time changed between the project 2 and 4 data?",
    x = "Where is the data from?",
    y = "Median commute duration"
  ) 
ggsave("plot3.png", width = 9, height = 6, unit= "in") # inches 🤮
    
  




