## Summary Info

# Load libraries
library("dplyr")
library("stringr")
library("ggplot2")

# Load the data
checkouts <- read.csv("~/Desktop/Checkouts-SPL.csv", stringsAsFactors = FALSE)
checkouts[checkouts == ""] <- NA

# Filter by Subjects containing "African American"
afram <- checkouts %>% 
  filter(grepl('African American', Subjects))


creator_total_checkouts_2020 <- afram_2020_checkouts %>%
  group_by(Title) %>%
  summarize(TotalCheckouts = sum(Checkouts, na.rm = TRUE)) %>%
  arrange(-TotalCheckouts)

checkouts_year <- afram %>%
  group_by(CheckoutYear) %>%
  summarize(TotalCheckouts = sum(Checkouts, na.rm = TRUE)) %>%
  arrange(-TotalCheckouts) %>%
  slice(1)


$percent_afram
$total_checkouts
$creator_total_checkouts
$top_recent_total_checkouts
$checkouts_subject
$checkouts_year
$checkouts_2020