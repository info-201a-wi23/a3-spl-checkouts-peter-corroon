## Summary Info

# Load libraries
library("dplyr")
library("stringr")
library("ggplot2")

# Load the data
checkouts <- read.csv("~/Desktop/Checkouts-SPL.csv", stringsAsFactors = FALSE)
checkouts[checkouts == ""] <- NA

# Filter by subjects containing "African American"
afram <- checkouts %>% 
  filter(grepl('African American', Subjects))

# Find 5 summary values
# 1. The percentage of all items checkedout that contain "African American" within the subjects
all_total_checkouts <- checkouts %>% 
  summarize(TotalCheckouts = sum(Checkouts))

afram_total_checkouts <- afram %>% 
  summarize(TotalCheckouts = sum(Checkouts))

percent_afram <- paste0(round(afram_total_checkouts / all_total_checkouts, digits = 2), "%")

# 2. The item with the highest total number of checkouts for items that contain "African American" within the subjects
most_checkouts <- afram %>% 
  group_by(Title) %>% 
  summarize(TotalCHeckouts = sum(Checkouts)) %>% 
  arrange(-TotalCHeckouts) %>% 
  slice_head(n = 1)

# 3. The top three creators with the most checkouts for items that contain "African American" within the subjects
top_creators <- afram %>% 
  group_by(Creator) %>% 
  summarize(TotalCheckouts = sum(Checkouts)) %>% 
  arrange(-TotalCheckouts) %>% 
  slice(2:4)
  
# 4. The item  containing "African American" within the subjects that has the highest number of checkouts so far in 2023
top_recent_total_checkouts <- afram %>% 
  filter(CheckoutYear == "2023") %>% 
  group_by(Title) %>% 
  summarize(TotalCheckouts = sum(Checkouts)) %>% 
  arrange(-TotalCheckouts) %>% 
  slice_head(n = 1) %>% 
  pull(Title)
  
# 5. The subjects containing "African American" with the highest number of checkouts
top_subject <- afram %>% 
  group_by(Subjects) %>% 
  summarize(TotalCheckouts = sum(Checkouts)) %>% 
  arrange(-TotalCheckouts) %>% 
  slice_head(n = 1) %>% 
  pull(Subjects)
  
#  6. The year with the highest number of checkouts for items that contain "African American" within the subjects
top_year <- afram %>% 
  group_by(CheckoutYear) %>% 
  summarize(TotalCheckouts = sum(Checkouts)) %>% 
  arrange(-TotalCheckouts) %>% 
  slice_head(n = 1) %>% 
  pull(CheckoutYear)

# 7. The number of checkouts in 2020 foritems that contain "African American" within the subjects
checkouts_2020 <- afram %>% 
  filter(CheckoutYear == "2020") %>% 
  summarize(TotalCheckouts = sum(Checkouts)) 

# Create a list containing each of these values
summary_info <- list()
summary_info$percent_afram <- percent_afram
summary_info$most_checkouts <- most_checkouts
summary_info$top_creators <- top_creators
summary_info$top_recent_total_checkouts <- top_recent_total_checkouts
summary_info$top_subject <- top_subject 
summary_info$top_year <- top_year
summary_info$checkouts_2020 <- checkouts_2020 