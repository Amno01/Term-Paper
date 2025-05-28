# Term Paper

###############################################################################

# Set working directory (change this)
setwd("C:/Users/Amanda/Downloads/Time series 3b/Assignments/Term paper/Script")

# Load required libraries
library(readxl)
library(dplyr)
library(zoo)
library(lubridate)
library(openxlsx)
library(writexl)

###############################################################################

# Convert monthly CPIF-data to quarterly

# Read in the Excel file
cpif <- read_excel("../Data/CPIF_index_monthly_2001_2024.xlsx")

# Rename columns
colnames(cpif) <- c("date", "cpif_index")

# Convert the date to a working Date format 
# (assumes YYYY-MM format, adds "-01" for the first of the month)
cpif <- cpif %>%
  mutate(date = as.Date(paste0(gsub("M", "-", date), "-01")))

# Create a quarterly variable using zoo::as.yearqtr
cpif <- cpif %>%
  mutate(quarter = as.yearqtr(date))

# Aggregate to quarterly data (average of index per quarter)
cpif_quarterly <- cpif %>%
  group_by(quarter) %>%
  summarise(cpif_q = mean(cpif_index, na.rm = TRUE)) %>%
  ungroup()

# Save the new excel
# write.xlsx(cpif_quarterly, "../Data/cpif_quarterly_2001_2024.xlsx", overwrite = TRUE)

###############################################################################

# Combine the data files 

# File names in order
file_list <- c("../Data/total_unemployment_2001_2024.xlsx", "../Data/15-24_unemployment_2001_2024.xlsx",
 "../Data/GDP_fixed_2001_2024.xlsx", "../Data/cpif_quarterly_2001_2024.xlsx")

# Read all data frames
data_list <- lapply(file_list, read_excel)

# Merge all data frames by "Date" column using reduce + full_join
merged_data <- Reduce(function(x, y) full_join(x, y, by = "Date"), data_list)

# Sort by Date
merged_data <- merged_data %>% arrange(Date)

# Rename columns
colnames(merged_data) <- c("date", "total_unemployment", "youth_unemployment", "gdp", "cpif")

# Write to Excel
# write_xlsx(merged_data, "../Data/unemployment_gdp_cpif_data.xlsx")

###############################################################################

# Add the NEET to another excel 

# Read the new file (2007–2021 data)
neet_data <- read_excel("../Data/NEET_15-24_2007_2024.xlsx")

# Merge the new data into the existing dataset by "date"
combined_data <- full_join(merged_data, neet_data, by = "date") %>%
  arrange(date)  # Optional: keep it in chronological order

# Rename columns
colnames(combined_data) <- c("date", "total_unemployment", "youth_unemployment", "gdp", "cpif", "neet")

# Write to a new Excel file
write_xlsx(combined_data, "../Data/neet_unemployment_gdp_cpif_data.xlsx")

###############################################################################
