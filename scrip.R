# =================================== #
# Empirical Analysis on the FX Market #
# =================================== #

# Pre-Settings -----
rm(list=ls())
wd <- "~/Documents/Research/2024-FX market"
input <- paste0(wd, "/raw_data/")
output <- paste0(wd, "/new_data/")
plots <- paste0(wd, "/plots/")
tables <- paste0(wd, "/tables/")
setwd(wd)

# R-Libraries -----
library(readxl)
library(tidyverse)

# I. Section - Data preparation =====
# 1. Exchange Rates -----
daily_exchange_rates_all <- read_excel(paste0(input, "arfolyam.xlsx"), col_types = c("date", rep("numeric", 75)))
daily_exchange_rates_all <- daily_exchange_rates_all %>%
  select(-2) %>%
  slice(-1) %>%
  rename("Date" = "Dátum/ISO") %>%
  mutate_at("Date", as.Date)
write_csv(daily_exchange_rates_all, paste0(output, "daily_exchange_rates_all.csv"))

daily_exchange_rates <- daily_exchange_rates_all %>%
  select("Date", "EUR", "USD", "GBP") %>%
  filter(Date > "2000-01-01")
write_csv(daily_exchange_rates, paste0(output, "daily_exchange_rates.csv"))

# 2. Interest Rates -----
daily_interest_rates_all <- as_tibble(read.csv(paste0(input, "WS_CBPOL_D_csv_row.csv"), skip = 8))
daily_interest_rates_all <- daily_interest_rates_all %>%
  rename_all(~str_remove(., "D\\.")) %>%
  rename("Date" = "Time.Period") %>%
  rename("EU" = "XM") %>%
  mutate_at("Date", as.Date)
write_csv(daily_interest_rates_all, paste0(output, "daily_interest_rates_all.csv"))

daily_interest_rates <- daily_interest_rates_all %>%
  select("Date", "HU", "EU", "US", "GB") %>%
  fill(HU, EU, US, GB) %>%
  filter(Date > "2000-01-01")
write_csv(daily_interest_rates, paste0(output, "daily_interest_rates.csv"))

# 3. Data tables -----
daily_data <- as_tibble(data.frame(Date = seq(as.Date("2000-01-01"), as.Date("2023-07-10"), by = "day"))) # update the dates later
daily_data <- left_join(daily_data, daily_exchange_rates, by = "Date")
daily_data <- left_join(daily_data, daily_interest_rates, by = "Date")
daily_data <- daily_data %>% fill(EUR, USD, GBP)


# II. Section - =====








