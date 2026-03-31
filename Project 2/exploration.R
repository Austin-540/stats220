#
# I have split this document into sections so that I'm not repeating a bunch of code and making
# things confusing with my comments at the bottom. The section number is in the comment *after* the code.
#

library(tidyverse)

logged_data <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRetKKj9bMRzvsbYOusSWE0uw2oEJIxmDjFP6C2U79SCUlN05jlOH4OHHo5kVT4mpo5BdYi9q9NZh7w/pub?gid=49993136&single=true&output=csv")

latest_data <- rename(logged_data, timestamp = 1, method = 2, duration = 3, start_time = 4)
# ^^^^^ section 1



#View(latest_data)

print(str_glue("There have been {nrow(latest_data)} journeys tracked by this form.")) #Useful
print(str_glue("Min: {min(latest_data$duration)}, Max: {max(latest_data$duration)}, Mean: {mean(latest_data$duration)}")) #The mean is the most useful here

ggplot(latest_data) + geom_bar(aes(x=duration))
#Not very useful

# ^^^^^ section 2

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

#^^^^^^ section 3

ggplot(latest_data.rounded) + 
  geom_bar(aes(x=method), fill="#000080") +
  theme_minimal() +
  labs(x = "Transportation method",
       y = "Number of journies",
       title = "Number of journies made to campus by transportation method")
#Kind of interesting to see the ratio, but its not that useful.


#^^^^^^^ section 4

latest_data.bus <- latest_data
latest_data.car <- latest_data
for (i in nrow(latest_data):1) { #I feel like there might be a better way to do this in module 3...
  if (latest_data$method[i] == "Bus") {
    latest_data.car <- latest_data.car[-i,]
  } else if (latest_data$method[i] == "Car") {
    latest_data.bus <- latest_data.bus[-i,]
 } 
} #Starting with the final element, go through each element and remove it from the different dfs if it isn't the corresponding method

mean_times <- data.frame(method=c("Bus", "Car"), duration=c(mean(latest_data.bus$duration), mean(latest_data.car$duration)))
ggplot(mean_times) + 
  geom_col(aes(x=method, y=duration), fill="#36fd45") + #Had to switch the type of geom here - not sure why but the ggplot docs say to
  theme_minimal()
#Honestly right now it doesn't show a big difference but maybe in the future it will. Either way it's useful.

#^^^^^^ section 5



# For my report I will use the rounded data chart and mean duration by method chart
# And I will include the number of rows of data collected, and the mean journey time
# This will require the following R code:

logged_data <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRetKKj9bMRzvsbYOusSWE0uw2oEJIxmDjFP6C2U79SCUlN05jlOH4OHHo5kVT4mpo5BdYi9q9NZh7w/pub?gid=49993136&single=true&output=csv")

latest_data <- rename(logged_data, timestamp = 1, method = 2, duration = 3, start_time = 4)

print(str_glue("There have been {nrow(latest_data)} journeys tracked by this form.")) 
print(str_glue("Mean: {mean(latest_data$duration)}"))
# and sections 3 & 5
