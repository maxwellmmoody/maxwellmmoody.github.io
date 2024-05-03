require(tidyverse)
require(sf)
library(statebins)

# Load data
data <- read_csv('WFHdata_March24.csv')

# Set theme
theme_set(
  theme_void() +
    theme(
      # Set text
      text = element_text(family = "Helvetica"),
      # Set plot
      plot.background = element_rect(color="#ffffff", fill="#ffffff"),
      plot.title = element_text(
        face = "bold",
        margin = margin(b = 5)),
      plot.subtitle = element_text(
        face = "italic",
        margin = margin(b = 10)),
      plot.caption = element_text(
        hjust = 0,
        margin = margin(t = 10)),
      plot.margin = margin(10,10,10,10),
      # Set axis
      axis.text = element_text(color = "grey"),
      # Set panel
      panel.grid.major.y = element_line(color = "lightgrey")))

# Exhibit 1: Choropleth map that shows amount of WFH in each state as time goes by.

# Select variables
data_selected <- data %>%
  select(region, date, wfhcovid_frac) %>% # Choose from wfhcovid, wfhcovid_ever, wfhcovid_frac
  mutate(date = as.Date(paste0(sub("m", "-", date), "-01"))) %>% # Convert date to standard format
  na.omit()

# Group by state and date
data_grouped <- data_selected %>%
  group_by(region, date) %>%
  summarize(average = mean(wfhcovid_frac)) %>% # Set outcome here
  filter(region != "AE" & region != "AP")

# Set value breaks, colors, and labels
breaks <- c(0,25,50,75,100)
colors <- c("#ffffb2","#fecc5c","#fd8d3c","#f03b20","#bd0026")
labels <- c("0-25%","25-50%","50-75%","75-100%")

# Create list of unique dates and sort from least to most recent
unique_dates <- sort(unique(data_grouped$date), decreasing = FALSE)

# Set the date manually
i = 24
  
  # Make a statebin map
  map <- ggplot(data_grouped %>%
                  filter(date == unique_dates[i]),
                aes(state = region,
                fill = cut(average, breaks),
                text = paste(region, ": ", average))) +
    geom_statebins() +
    # Add fill
    scale_fill_manual(values = colors,
                      name = "",
                      labels = labels) +
    # Add labels
    labs(title = "Average share of paid working days WFH in each state", # "Average share of paid working days WFH in each state", "Percentage of workers that WFH"
         subtitle = format(unique_dates[i], "%B %Y"),
         caption = "Source: U.S. Survey of Working Arrangements and Attitudes (SWAA)") +
    # Remove axis text and gridlines
    theme(axis.text = element_blank(),
          panel.grid.major.y = element_blank(),
          legend.position = "none")
  
  # Save the map as a PDF file in the directory
  ggsave(filename = paste0("images/wfhcovid_frac/wfhcovid_frac_map", i, ".pdf"),
         plot = map,
         width = 8, height = 6, units = "in", dpi = 300)

# Exhibit 2: Visual representation of the average amount of dollars invested in learning to WFH effectively, over time.
  
# Select variables
data_selected <- data %>%
  select(date, wfh_invest_quant) %>% # Choose from wfhcovid, wfhcovid_ever, wfhcovid_frac
  mutate(date = as.Date(paste0(sub("m", "-", date), "-01"))) %>% # Convert date to standard format
  na.omit()

# Group by date
data_grouped <- data_selected %>%
  group_by(date) %>%
  summarize(average = mean(wfh_invest_quant))

# Plot a line graph of average wfh_invest_quant over time
# Plotting the line graph
plot <- ggplot(data_grouped, aes(x = date, y = average)) +
  geom_line() +
  labs(title = "Average amount of money invested in WFH equipment/infrastructure, 2020")

plot

# Save the graph as a PDF file in the directory
ggsave(filename = paste0("images/money_invested.pdf"),
       plot = plot,
       width = 8, height = 6, units = "in", dpi = 300)
# Exhibit 3: Ridge graph showing popular WFH labels in patent applications over time.

# Backup: Line graph showing frequency of WFH-related terms in patent applications.

monthly_stats <- read_csv('monthly_stats.csv')

# Convert 'date' column to date format
monthly_stats$date <- as.Date(paste0(monthly_stats$date, "-01"), format = "%Y-%m-%d")

line <- ggplot(monthly_stats, aes(x = date, y = WFH_related)) +
  geom_line() +
  labs(title = "Percentage of WFH-enabling patent applications, 2019 to 2024")

# Save the graph as a PDF file in the directory
ggsave(filename = paste0("images/monthly_stats.pdf"),
       plot = line,
       width = 8, height = 6, units = "in", dpi = 300)

# Exhibit 4: Heatmap mapping desired number of paid WFH days post-COVID across income and education category bins.

# Select variables
data_selected <- data %>%
  select(wfh_days_postCOVID_s, incomebin, education_s) %>%
  na.omit()

# Group by date
data_grouped <- data_selected %>%
  group_by(incomebin, education_s) %>%
  summarize(average = mean(wfh_days_postCOVID_s))

heatmap <- ggplot(data_grouped, aes(x = incomebin, y = education_s, fill = average)) +
  geom_tile(color = "black") +
  scale_fill_gradient(low = "#83aff0", high = "#1b2d48") +
  labs(title = "Heatmap of Desired Number of Paid WFH Days Post-COVID",
       x = "Income Category",
       y = "Education Category",
       fill = "Average WFH Days") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(aes(label = round(average, 1)), color = "white", size = 3) # Add text labels with rounded average values

print(heatmap)

# Save the heatmap as a PDF file in the directory
ggsave(filename = paste0("images/heatmap2.pdf"),
       plot = heatmap,
       width = 8, height = 6, units = "in", dpi = 300)