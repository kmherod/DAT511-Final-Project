# Automated package installation and loading
required_packages <- c("dplyr", "tidyr", "readr","ggplot2", "knitr")


for(pkg in required_packages) {
  
  if(!require(pkg, character.only = TRUE)) {
    
    cat(paste("Installing", pkg, "...\n"))
    
    install.packages(pkg, repos = "https://cran.r-project.org")
    
    library(pkg, character.only = TRUE)
    
  } else {
    
    cat(paste(pkg, "version", packageVersion(pkg), "loaded\n"))
    
  }
  
}

# Define the folder path
folder_path <- "C:/Users/kelse/OneDrive/Documents/DAT511/Final Project"


# Define the file paths for the unzipped CSV files
details_file <- file.path(folder_path, "StormEvents_details-ftp_v1.0_d2024_c20251118.csv")
fatalities_file <- file.path(folder_path, "StormEvents_fatalities-ftp_v1.0_d2024_c20251118.csv")
locations_file <- file.path(folder_path, "StormEvents_locations-ftp_v1.0_d2024_c20251118.csv")


# Load the CSV files into R
details <- read_csv(details_file)
fatalities <- read_csv(fatalities_file)
locations <- read_csv(locations_file)


# Join the datasets by EVENT_ID
joined_data <- details %>%
  left_join(locations, by = "EVENT_ID") %>%
  left_join(fatalities, by = "EVENT_ID")


# Save the joined data to a new CSV file
output_file <- file.path(folder_path, "StormEvents_joined_data.csv")
write_csv(joined_data, output_file)


# Inform the user of output
message("Joined data saved to: ", output_file)


# 1.	Across the United States, which types of events (as indicated in the EVENT_TYPE variable) are most harmful with respect to population health?

# Combine total number of injuries and deaths into one column
storm <- joined_data
storm$injuries <- storm$INJURIES_DIRECT + storm$INJURIES_INDIRECT
storm$deaths <- storm$DEATHS_DIRECT + storm$DEATHS_INDIRECT
storm$health <- storm$injuries + storm$deaths

# Create table for storm event rankings
event_summary <- storm %>%
  group_by(EVENT_TYPE) %>%
  summarize(
    total_inj = sum(injuries, na.rm = TRUE),
    total_deaths = sum(deaths, na.rm = TRUE),
    total_health_impact = sum(health, na.rm = TRUE)
  )

event_ranking <- event_summary %>%
  arrange(desc(total_health_impact))

# Isolate top ten most harmful storm events
top10_events <- head(event_ranking, 10)

# Create a bar chart for top ten events
ggplot(top10_events, aes(x = reorder(EVENT_TYPE, total_health_impact),
                         y = total_health_impact)) +
  geom_col(color = "black", fill = "black") +
  coord_flip() +
  labs(title = "Top 10 Harmful Storm Events on Population Health",
       x = "Event Type",
       y = "Injuries & Deaths") +
  theme_minimal()


# 2. Across the United States, which types of events happen the most for each state/territory?

# Summary count of storm event type per state/territory
events_by_state <- joined_data %>%
  group_by(STATE, EVENT_TYPE) %>%
  summarize(count = n())

# Storm event type with the highest count in each state/territory
most_common_event_by_state <- events_by_state %>%
  group_by(STATE) %>%
  filter(count == max(count))

# Summary table of most frequent storm event types by state/territory
kable(
  most_common_event_by_state,
  caption = "Most Frequent Storm Event Type by State/Territory",
  col.names = c("State", "Event Type", "Event Count"),
  align = c("l", "l", "r")
)


# 3.	Which types of events are characterized by which months?

# Summary table of storm event count per month
events_per_month_event <- joined_data %>%
  group_by(MONTH_NAME, EVENT_TYPE) %>%
  summarize(event_count = n(), .groups = "drop") %>%
  mutate(MONTH_NAME = factor(MONTH_NAME,
                             levels = c("January", "February", "March", "April",
                                        "May", "June", "July", "August",
                                        "September", "October", "November", "December"))) %>%
  arrange(MONTH_NAME, desc(event_count))

# Summary table of most frequent storm event per month
top_event_each_month <- events_per_month_event %>%
  group_by(MONTH_NAME) %>%
  arrange(desc(event_count)) %>%
  slice(1)


# Bar chart of most frequent storm event per month
ggplot(top_event_each_month, aes(x = MONTH_NAME, y = event_count, fill = EVENT_TYPE)) +
  geom_col() +
  labs(
    title = "Most Frequent Storm Event Type Per Month",
    x = "Month",
    y = "Event Count",
    fill = "Event Type"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# 4. Which county has been harmed the most by storm events?

# Filter county FIPS number for only county rows
county_data <- joined_data %>%
  filter(CZ_TYPE == "C")

# Create unique county FIPS number by including state FIPS number
county_data <- county_data %>%
  mutate(
    county_fips = sprintf("%02d%03d", STATE_FIPS, CZ_FIPS)
  )

# Combine total number of injuries and deaths into one column
county_data <- county_data %>%
  mutate(
    injuries_county = INJURIES_DIRECT + INJURIES_INDIRECT,
    deaths_county   = DEATHS_DIRECT + DEATHS_INDIRECT,
    health_county   = injuries_county + deaths_county
  )

# Total storm harm (injuries and deaths) by county
harm_by_county <- county_data %>%
  group_by(county_fips, STATE, CZ_NAME) %>%
  summarize(
    total_injuries = sum(injuries_county, na.rm = TRUE),
    total_deaths   = sum(deaths_county, na.rm = TRUE),
    total_harm     = sum(health_county, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(total_harm))


# Bar chart of top ten harmed counties
top10_counties <- harm_by_county %>%
  arrange(desc(total_harm)) %>%
  head(10)

ggplot(top10_counties,
       aes(x = reorder(paste(CZ_NAME, STATE, sep = ", "), total_harm),
           y = total_harm)) +
  geom_col(fill = "red") +
  coord_flip() +
  labs(
    title = "Top 10 Most Harmed Counties in the U.S.",
    x = "County",
    y = "Total Injuries & Deaths"
  ) +
  theme_minimal()
