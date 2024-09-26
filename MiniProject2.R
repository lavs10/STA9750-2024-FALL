# Load necessary libraries
library(tidyverse)
library(readxl)
library(lubridate)


FARES <- read_xlsx("2022_fare_revenue.xlsx") %>%
  select(-`State/Parent NTD ID`, -`Reporter Type`, -`Reporting Module`, -`TOS`, 
         -`Passenger Paid Fares`, -`Organization Paid Fares`) %>%
  filter(`Expense Type` == "Funds Earned During Period") %>%
  select(-`Expense Type`) %>%
  group_by(`NTD ID`, `Agency Name`, `Mode`) %>%
  summarize(`Total Fares` = sum(`Total Fares`), .groups = 'drop')


EXPENSES <- readr::read_csv("2022_expenses.csv") %>%
  select(`NTD ID`, `Agency`, `Total`, `Mode`) %>%
  mutate(`NTD ID` = as.integer(`NTD ID`)) %>%
  rename(Expenses = Total) %>%
  group_by(`NTD ID`, `Mode`) %>%
  summarize(Expenses = sum(Expenses), .groups = 'drop')


FINANCIALS <- inner_join(FARES, EXPENSES, by = c("NTD ID", "Mode"))


head(FINANCIALS)

# Check for duplicates in TRIPS
TRIPS %>%
  group_by(`NTD ID`, `Agency`, `UZA Name`, `Mode`, `month`) %>%
  filter(n() > 1) %>%
  summarize(count = n())

# Check for duplicates in MILES
MILES %>%
  group_by(`NTD ID`, `Agency`, `UZA Name`, `Mode`, `month`) %>%
  filter(n() > 1) %>%
  summarize(count = n())

# Remove duplicates in TRIPS and MILES
TRIPS <- TRIPS %>%
  distinct(`NTD ID`, `Agency`, `UZA Name`, `Mode`, `month`, .keep_all = TRUE)

MILES <- MILES %>%
  distinct(`NTD ID`, `Agency`, `UZA Name`, `Mode`, `month`, .keep_all = TRUE)

USAGE <- inner_join(TRIPS, MILES, by = c("NTD ID", "Agency", "UZA Name", "Mode", "month"), relationship = "many-to-many")


TRIPS <- read_xlsx("ridership.xlsx", sheet = "UPT") %>%
  filter(`Mode/Type of Service Status` == "Active") %>%
  select(-`Legacy NTD ID`, -`Reporter Type`, -`Mode/Type of Service Status`, -`UACE CD`, -`TOS`) %>%
  pivot_longer(-c(`NTD ID`, `Agency`, `UZA Name`, `Mode`, `3 Mode`), 
               names_to = "month", 
               values_to = "UPT") %>%
  drop_na() %>%
  mutate(month = lubridate::my(month))


MILES <- read_xlsx("ridership.xlsx", sheet = "VRM") %>%
  filter(`Mode/Type of Service Status` == "Active") %>%
  select(-`Legacy NTD ID`, -`Reporter Type`, -`Mode/Type of Service Status`, -`UACE CD`, -`TOS`) %>%
  pivot_longer(-c(`NTD ID`, `Agency`, `UZA Name`, `Mode`, `3 Mode`), 
               names_to = "month", 
               values_to = "VRM") %>%
  drop_na() %>%
  mutate(month = lubridate::my(month))


USAGE <- inner_join(TRIPS, MILES, by = c("NTD ID", "Agency", "UZA Name", "Mode", "month"))


head(USAGE)


USAGE <- USAGE %>%
  mutate(`NTD ID` = as.character(`NTD ID`))

FINANCIALS <- FINANCIALS %>%
  mutate(`NTD ID` = as.character(`NTD ID`))


USAGE_AND_FINANCIALS <- left_join(USAGE, FINANCIALS, by = c("NTD ID", "Mode"))


head(USAGE_AND_FINANCIALS)

# Analysis

# Find the transit system with the most UPT in 2022
most_upt <- USAGE_AND_FINANCIALS %>%
  group_by(`NTD ID`, `Agency`, `Mode`) %>%
  summarize(Total_UPT = sum(UPT, na.rm = TRUE), .groups = 'drop') %>%
  arrange(desc(Total_UPT)) %>%
  slice(1)

most_upt

# Find the transit system with the highest farebox recovery
highest_farebox <- USAGE_AND_FINANCIALS %>%
  filter(!is.na(Expenses), Expenses > 0) %>%  
  mutate(Farebox_Recovery = `Total Fares` / Expenses) %>%
  arrange(desc(Farebox_Recovery)) %>%
  slice(1)

highest_farebox

# Find the system with the lowest expenses per UPT
lowest_exp_per_upt <- USAGE_AND_FINANCIALS %>%
  filter(UPT > 0) %>%  
  mutate(Expenses_per_UPT = Expenses / UPT) %>%
  arrange(Expenses_per_UPT) %>%
  slice(1)

lowest_exp_per_upt

# Find the system with the highest total fares per UPT
highest_fares_per_upt <- USAGE_AND_FINANCIALS %>%
  filter(UPT > 0) %>%  
  mutate(Fares_per_UPT = `Total Fares` / UPT) %>%
  arrange(desc(Fares_per_UPT)) %>%
  slice(1)

highest_fares_per_upt

# Find the system with the lowest expenses per VRM
lowest_exp_per_vrm <- USAGE_AND_FINANCIALS %>%
  filter(VRM > 0) %>%  
  mutate(Expenses_per_VRM = Expenses / VRM) %>%
  arrange(Expenses_per_VRM) %>%
  slice(1)

lowest_exp_per_vrm

# Find the transit agency with the most total VRM
most_vrm_agency <- USAGE_AND_FINANCIALS %>%
  group_by(`NTD ID`, `Agency`, `Mode`) %>%
  summarize(Total_VRM = sum(VRM, na.rm = TRUE), .groups = 'drop') %>%
  arrange(desc(Total_VRM)) %>%
  slice(1)

most_vrm_agency

# Find the transit mode with the most total VRM
most_vrm_mode <- USAGE_AND_FINANCIALS %>%
  group_by(Mode) %>%
  summarize(Total_VRM = sum(VRM, na.rm = TRUE), .groups = 'drop') %>%
  arrange(desc(Total_VRM)) %>%
  slice(1)

most_vrm_mode

# Filter NYC Subway in May 2024 and calculate total UPT
nyc_may_2024_upt <- USAGE_AND_FINANCIALS %>%
  filter(`Agency` == "New York City Transit", Mode == "HR", month == "2024-05-01") %>%
  summarize(Total_UPT = sum(UPT, na.rm = TRUE))

nyc_may_2024_upt

# Calculate the longest average trip in May 2024
longest_avg_trip <- USAGE_AND_FINANCIALS %>%
  filter(month == "2024-05-01") %>%
  mutate(Avg_Trip_Length = VRM / UPT) %>%
  arrange(desc(Avg_Trip_Length)) %>%
  slice(1)

longest_avg_trip

# Calculate the drop in NYC subway ridership between April 2019 and April 2020
nyc_ridership_fall <- USAGE_AND_FINANCIALS %>%
  filter(`Agency` == "New York City Transit", Mode == "HR", month %in% c("2019-04-01", "2020-04-01")) %>%
  group_by(month) %>%
  summarize(Total_UPT = sum(UPT, na.rm = TRUE)) %>%
  arrange(month)

# Calculate the drop in ridership
nyc_ridership_fall %>%
  mutate(Difference = Total_UPT - lag(Total_UPT))


USAGE_2022_ANNUAL <- USAGE_AND_FINANCIALS %>%
  filter(lubridate::year(month) == 2022) %>%
  group_by(`NTD ID`, `Agency`, `UZA Name`, `Mode`) %>%
  summarize(Total_UPT = sum(UPT, na.rm = TRUE),
            Total_VRM = sum(VRM, na.rm = TRUE), .groups = 'drop')

# View summarized data for 2022
head(USAGE_2022_ANNUAL)

# Merge 2022 usage data with financial data
USAGE_AND_FINANCIALS_2022 <-
  