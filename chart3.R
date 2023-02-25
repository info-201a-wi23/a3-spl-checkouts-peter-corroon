## Chart 3

# Load libraries
library("treemapify")
library("dplyr")
library("stringr")
library("ggplot2")

# Load the data
checkouts <- read.csv("~/Desktop/Checkouts-SPL.csv", stringsAsFactors = FALSE)
checkouts[checkouts == ""] <- NA

# Filter by Subjects containing "African American"
afram <- checkouts %>% 
  filter(grepl('African American', Subjects))

# Filter by the total number of checkouts each year excluding 2023
total_checkouts_year <- afram %>% 
  group_by(CheckoutYear) %>% 
  summarize(Checkouts = sum(Checkouts, na.rm = TRUE)) %>% 
  slice(1:10)

#Add a column to distinguish the years before and afer george floyds death 
total_checkouts_year$GeorgeFloydDeath <- c("Before George Floyd's Death", "Before George Floyd's Death", "Before George Floyd's Death","Before George Floyd's Death", "Before George Floyd's Death", "Before George Floyd's Death", "Before George Floyd's Death", "Year of George Floyd's Death", "After George Floyd's Death", "After George Floyd's Death")

# Create a treemap displaying the number of checkouts for each year (2013-2022)
ggplot(total_checkouts_year, aes(area = Checkouts, label = CheckoutYear, 
                                 subgroup = GeorgeFloydDeath)) +
  geom_treemap(aes(alpha = Checkouts), fill = "#F9A32C", color = "#E31B23") +
  labs(title = "Total Checkouts for 'African American' Subject Items by Year (2013-2022)") +
  geom_treemap_text(reflow = T, color = "Black", place = "middle") + 
  geom_treemap_subgroup_border(color = "Black", size = 5) +
  geom_treemap_subgroup_text(place = "centre", grow = T, alpha = .7, colour =
                               "#12853F", fontface = "italic", min.size = 0)