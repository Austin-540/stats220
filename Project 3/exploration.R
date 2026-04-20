library(tidyverse)
library(httr)
library(magick)
library(stringr)

api_key <- Sys.getenv("apikey") #so I can upload this to github

url <- "https://api.pexels.com/v1/search?query=blue%20cars&per_page=80"

response <- httr::GET(url, 
                      add_headers(Authorization = api_key))

data <- httr::content(response, 
                      as = "parsed", 
                      type = "application/json")

photo_data <- tibble(photos = data$photos) %>%
  unnest_wider(photos) %>%
  unnest_wider(src)

selected_photos <- photo_data %>%
  filter(str_to_lower(substr(alt, 1, 1)) == "a") %>%
  #Filter it down to only photos where the alt text either starts with "A" or "a".
  #This is personally relevant to me as A is the first letter of my name.
  mutate(
    is_portrait = width < height, #If the photo is a square then it isn't portrait, so this will be false
    words_in_photographers_username = str_count(photographer, "\\S+"), 
    main_colour = ifelse( #If the red value is greater than the blue and green value
        strtoi(
          substr(avg_color, 2, 3), #Get the 2 hexadecimal digits after the "#"
          base=16
          ) > strtoi(
            substr(avg_color, 4, 5), 
            base=16
          ) & strtoi(
            substr(avg_color, 2, 3), 
            base=16
          ) > strtoi(
            substr(avg_color, 6, 7), 
            base=16
          ), 
        "red",
        ifelse( #yay nested ifelse :)
          #If the green value is greater than the red and blue
          strtoi(
            substr(avg_color, 4, 5), 
            base=16
          ) > strtoi(
            substr(avg_color, 2, 3), 
            base=16
          ) & strtoi(
            substr(avg_color, 4, 5), 
            base=16
          ) > strtoi(
            substr(avg_color, 6, 7), 
            base=16
          ), 
          "green",
          "blue" 
          #If any of the values are equal it will end up as blue
          #I don't think this is a problem with the data I selected
        )
        
    )
    
  )

write_csv(selected_photos, "selected_photos.csv")

