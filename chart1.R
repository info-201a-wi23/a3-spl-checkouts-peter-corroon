## Chart 1

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

# Find the average checkouts per item per year for all items
avg_checkouts <- checkouts %>% 
  group_by(CheckoutYear) %>% 
  summarize(AvgCheckouts = mean(Checkouts, na.rm = TRUE)) %>% 
  mutate(Subject = "All")

# Find the average checkouts per item per year for "African American" subject items
avg_afram_checkouts <- afram %>% 
  group_by(CheckoutYear) %>% 
  summarize(AvgCheckouts = mean(Checkouts, na.rm = TRUE)) %>% 
  mutate(Subject = "Includes 'African American'") 

# Combine this data into one data frame
avg_checkouts_joined <- rbind(avg_afram_checkouts, avg_checkouts) %>% 
  arrange(-CheckoutYear) %>% 
  slice(3:22)

# Create a line chart displaying average checkouts by year based on the two item type
ggplot(avg_checkouts_joined, aes(x = CheckoutYear, y = AvgCheckouts, group = Subject)) +
  geom_line(aes(linetype = Subject)) +
  labs(title = "Average Monthly Checkouts for 'African American' Subject Items vs All Items Over Time (2013-2022)", x = "Year", y = "Average Monthly Checkouts per Item") +
  scale_x_continuous(breaks = seq(2013, 2022, 1)) +
  scale_y_continuous(limits = c(10, 30)) +
  geom_point() +
  scale_linetype_manual(name = "Item Subject", values=c("dashed", "solid")) +
  theme(legend.position = "top") + 
  geom_text(
    aes(label = round(AvgCheckouts, digits = 1)),
    nudge_x = 0.25,
    nudge_y = 0.25,
    size = 4
  )