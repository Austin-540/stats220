library(tidyverse)

logged_data <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRetKKj9bMRzvsbYOusSWE0uw2oEJIxmDjFP6C2U79SCUlN05jlOH4OHHo5kVT4mpo5BdYi9q9NZh7w/pub?gid=49993136&single=true&output=csv")

latest_data <- rename(logged_data, timestamp = 1, method = 2, duration = 3, start_time = 4)

#View(latest_data)

print(str_glue("There have been {nrow(latest_data)} journeys tracked by this form.")) #Useful
print(str_glue("Min: {min(latest_data$duration)}, Max: {max(latest_data$duration)}, Mean: {mean(latest_data$duration)}")) #The mean is the most useful here

ggplot(latest_data) + geom_bar(aes(x=duration))
#Not very useful

latest_data_durations.rounded <- latest_data$duration %>% 
  round(-1) #Round the durations to the 10s place
latest_data.rounded <- latest_data
latest_data.rounded$duration <- latest_data_durations.rounded

ggplot(latest_data.rounded) + 
  geom_bar(aes(x=duration, fill = method)) + 
  theme_minimal() +
  labs(x = "Duration in minutes - rounded to the closest 10 minutes",
       y = "Number of journies",
       title = "Durations of people in the data's commutes to and from campus.",
       )
#Thats a lot more useful than before

ggplot(latest_data.rounded) + 
  geom_bar(aes(x=method), fill="#000080") +
  theme_minimal() +
  labs(x = "Transportation method",
       y = "Number of journies",
       title = "Number of journies made to campus by transportation method")
#Interesting to see the ratio








# For my report I will use the rounded data chart and number of journeys by method plots
# And I will include the number of rows of data collected, and the mean journey time
# This will require the following R code:

logged_data <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRetKKj9bMRzvsbYOusSWE0uw2oEJIxmDjFP6C2U79SCUlN05jlOH4OHHo5kVT4mpo5BdYi9q9NZh7w/pub?gid=49993136&single=true&output=csv")

latest_data <- rename(logged_data, timestamp = 1, method = 2, duration = 3, start_time = 4)



print(str_glue("There have been {nrow(latest_data)} journeys tracked by this form.")) #Useful
print(str_glue("Mean: {mean(latest_data$duration)}")) #The mean is the most useful here



latest_data_durations.rounded <- latest_data$duration %>% 
  round(-1) #Round the durations to the 10s place
latest_data.rounded <- latest_data
latest_data.rounded$duration <- latest_data_durations.rounded

ggplot(latest_data.rounded) + 
  geom_bar(aes(x=duration, fill = method)) + 
  theme_minimal() +
  labs(x = "Duration in minutes - rounded to the closest 10 minutes",
       y = "Number of journies",
       title = "Durations of people in the data's commutes to and from campus.",
  )

ggplot(latest_data.rounded) + 
  geom_bar(aes(x=method), fill="#000080") +
  theme_minimal() +
  labs(x = "Transportation method",
       y = "Number of journies",
       title = "Number of journies made to campus by transportation method")