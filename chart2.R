## Chart 2

# Load libraries
library("dplyr")
library("stringr")
library("ggplot2")

# Load the data
afram_2020_checkouts <- read.csv("~/Desktop/2020-AFRAM-Checkouts.csv", stringsAsFactors = FALSE)
afram_2020_checkouts[afram_2020_checkouts == ""] <- NA

# Find the top 5 creators by checkouts in 2020, save these as a vector
creator_total_checkouts_2020 <- afram_2020_checkouts %>% 
  group_by(Title) %>% 
  summarize(TotalCheckouts = sum(Checkouts, na.rm = TRUE)) %>% 
  arrange(-TotalCheckouts) 

# Create a data frame for each of these Creator sdisplaying their total checkouts per month in 2020
# "So You Want to Talk about Race"
book1 <- afram_2020_checkouts  %>% 
  filter(grepl('So You Want to Talk about Race', Title)) %>% 
  group_by(CheckoutMonth) %>% 
  summarize(TotalCheckouts = sum(Checkouts, na.rm = TRUE)) %>% 
  mutate(Title = "So You Want to Talk about Race")

zero_rows <- data.frame(CheckoutMonth = c(1:5),
                   TotalCheckouts = 0,
                   Title = "So You Want to Talk about Race")

book1.1 <- rbind(book1, zero_rows)

# "The New Jim Crow"
book2 <- afram_2020_checkouts  %>% 
  filter(grepl('The New Jim Crow|The new Jim Crow', Title)) %>% 
  group_by(CheckoutMonth) %>% 
  summarize(TotalCheckouts = sum(Checkouts, na.rm = TRUE)) %>% 
  mutate(Title = "The New Jim Crow")

# "The New Jim Crow"
book3 <- afram_2020_checkouts  %>% 
  filter(grepl('Between the World|Between the world', Title)) %>% 
  group_by(CheckoutMonth) %>% 
  summarize(TotalCheckouts = sum(Checkouts, na.rm = TRUE)) %>% 
  mutate(Title = "Between the World and Me")

# Combine this data into one data frame
compare_books <- rbind(book1.1, book2, book3) %>% 
  mutate(date = paste0("2020", "-", CheckoutMonth, "-01"))

compare_books$date <- as.Date(compare_books$date, format = "%Y-%m-%d")

# Create a line chart displaying the total checkouts per month for each book in 2020
ggplot(compare_books, aes(x = date, y = TotalCheckouts, color = Title)) +
  geom_line(aes(color = Title)) +
  labs(title = "Total Monthly Checkouts for Top Books covering Racial Injustice (2020)", x = "Month", y = "Total Checkouts") +
  guides(fill = guide_legend(title = "New Legend Title")) +
  scale_x_date(date_breaks = "1 month", date_labels = "%B") + 
  scale_y_continuous(limits = c(0, 5000)) +
  scale_color_manual(values = c('#12853F', '#E31B23', '#000000')) +
  theme(legend.position = "top") +
  scale_size_manual(values = 3) + 
  guides(color = guide_legend(title = "Book Title"))