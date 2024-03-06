# =================================== #
# Empirical Analysis on the FX Market #
# =================================== #

# Pre-Settings -----
rm(list=ls())
wd <- "~/Documents/Research/2024 - Foreign Exchange Market"
input <- paste0(wd, "/raw_data/")
output <- paste0(wd, "/new_data/")
plots <- paste0(wd, "/plots/")
tables <- paste0(wd, "/tables/")
setwd(wd)

# R-Libraries -----
library(readxl)
library(tidyverse)
library(ggplot2)
library(showtext)

# I. Section - Data preparation =====
# 1. Exchange Rates -----
daily_exchange_rates_all <- read_excel(paste0(input, "arfolyam.xlsx"), col_types = c("date", rep("numeric", 75)))
daily_exchange_rates_all <- daily_exchange_rates_all %>%
  slice(-1) %>%
  rename("Date" = "Dátum/ISO") %>%
  mutate_at("Date", as.Date) %>%
  mutate("HUF" = 1)

write_csv(daily_exchange_rates_all, paste0(output, "daily_exchange_rates_all.csv"))

# 2. Interest Rates -----
daily_interest_rates_all <- as_tibble(read.csv(paste0(input, "WS_CBPOL_csv_row.csv"), skip = 8))
daily_interest_rates_all <- daily_interest_rates_all %>%
  select(-starts_with("M.")) %>%
  rename_all(~str_remove(., "D\\.")) %>%
  rename("Date" = "Time.Period") %>%
  rename("EU" = "XM") %>%
  mutate_at("Date", as.Date) %>%
  rename_with(~paste0(., "_I"), -Date)

write_csv(daily_interest_rates_all, paste0(output, "daily_interest_rates_all.csv"))

# 3. Data tables -----
# Selecting Countries of the Exchange and Interest Rates
Countries <- c("HU", "EU", "US", "GB", "CH", "CZ", "PL")

# Creating a data table for the examined daily time interval
daily_data <- as_tibble(data.frame(Date = seq(as.Date("2020-12-31"), as.Date("2024-01-01"), by = "day")))

# Adding columns
daily_data <- left_join(daily_data, daily_exchange_rates_all %>% select(Date, starts_with(Countries)), by = "Date")
daily_data <- left_join(daily_data, daily_interest_rates_all %>% select(Date, starts_with(Countries)), by = "Date")

# Filling NA/NaN values and renaming the currencies
daily_data <- daily_data %>% 
  fill(all_of(starts_with(Countries))) %>%
  slice(-1) %>% 
  rename_at(vars(-c(ends_with("_I"), Date)), ~ paste0(sub(".$", "_C", .)))

# Transform data and add new columns
daily_data <- daily_data %>%
  mutate(across(matches(paste0("^", Countries, "_I$")), ~ ifelse(lead(.) - . == 0, NA, lead(.) - .), .names = "{.col}_c")) %>% # Nominal changes for Interest rates
  mutate(across(matches(paste0("^", Countries, "_C$")), ~ (lead(.) - .), .names = "{.col}_c")) %>% # Nominal changes for Exchange rates
  mutate(across(matches(paste0("^", Countries, "_C$")), ~ c(0, diff(log(.))) * 100, .names = "{.col}_ln_c")) %>% # Percentage changes for Exchange rates
  mutate(across(matches(paste0("^", Countries, "_C$")), ~ cumsum(c(0, diff(log(.))) * 100), .names = "{.col}_cum_ln_c")) # Cumulative percentage changes for Exchange rates

# Exporting final data set
write_csv(daily_data, paste0(output, "daily_data.csv"))

# Transform to pivot table
Variables <- c("_C", "_I", "_I_c", "_C_c", "_C_ln_c", "_C_cum_ln_c")
Variables_names <- c("Nominal_Exchange_Rate", "Nominal_Interest_Rate", "Nominal_Interest_Rate_Change", "Nominal_Exchange_Rate_Change", "Percentage_Exchange_Rate_Change", "Cumulative_Percentage_Exchange_Rate_Change")

pivot_list <- list()
for (i in 1:length(Variables)) {
  pivot_list[[i]] <- daily_data %>% 
    select("Date", paste0(Countries, Variables[i])) %>% 
    pivot_longer(cols = matches(paste0(Countries, Variables[i], "$")),
                 names_to = "Country",
                 names_pattern = "(..)",
                 values_to = Variables[i])
}

daily_pivot <- pivot_list[[1]]
for (i in 2:length(pivot_list)) {
  daily_pivot <- merge(daily_pivot, pivot_list[[i]], by = c("Date", "Country"), all.x = TRUE)
}

# Interest Rate Changes - Tables
interest_changes <- list()
for (i in 1:length(Countries)) {
  interest_changes[[i]] <- daily_data %>% 
    select("Date", paste0(Countries[i], "_I_c")) %>%
    rename_all(~str_remove(., "_I_c")) %>% 
    na.omit()
}



# II. Section - Charts and Plots =====
# 1. Dependencies -----
# Color palettes
palettes <- list()
for (i in 2:10) {
  palettes[[i]] <- hcl.colors(i, palette = "Zissou 1", rev = TRUE)
}

# Import custom font - Alright Sans
ttf <- list.files(paste0(input, "fonts/"), full.names = FALSE)
fonts <- gsub("\\.ttf$", "", ttf)
for (i in 1:16) {
  #print(fonts[i])
  font_add(fonts[i], paste0(input, "fonts/", ttf[i]))
}
showtext_auto()

# Define custom theme for plots
fx_theme <- function() {
  theme(
    plot.background = element_rect(fill = "#FFF9F5"), # Set plot background color
    plot.title = element_text(color = "#262A32", size = 14, hjust = 0.5, family = "AlrightSans-BoldItalic"), # Set plot title color, size, and style
    plot.margin = unit(c(0.5, 0, 0.5, 0.25), "cm"), # Set plot margins
    
    panel.background = element_rect(fill = "#FFF9F5"), # Set panel background color
    panel.border = element_rect(color = "#262A32", fill = NA, linewidth = 0.5, linetype = "solid"), # Set panel border color and size
    panel.grid.major = element_line(color = "#E7E9EC", linewidth = 0.5, linetype = "solid"),
    panel.grid.minor = element_line(color = "#E7E9EC", linewidth = 0.25, linetype = "dotted"),
    
    #axis.line = element_line(color = "#262A32", size = 0.5), # Set axis line color and size
    axis.text = element_text(color = "#262A32", size = 8, family = "AlrightSans-LightItalic"), #  Set axis text color, size, and style
    axis.title = element_text(color = "#262A32", size = 12, family = "AlrightSans-MediumItalic"), # Set axis title color, size, and style
    axis.ticks = element_line(color = "#262A32", linewidth = 0.25),
    
    axis.title.x = element_text(margin = margin(t = .25, unit = "cm")),
    axis.title.y = element_text(margin = margin(r = 0.25, unit = "cm")),
    
    #legend.position = ("right"),
    legend.title = element_text(color = "#262A32", size = 12, family = "AlrightSans-MediumItalic"), # Set legend title color, size, and style
    legend.text = element_text(color = "#262A32", size = 10, family = "AlrightSans-LightItalic"), # Set legend text color, size, and style
    legend.key = element_rect(fill = "#FFF9F5"),
    legend.background = element_rect(fill = "#FFF9F5"), # Set legend background color
    legend.box.margin = margin(0, 0, 0, 0, "cm"),
    
    strip.background = element_rect(fill = "#FFF9F5"),
    strip.text = element_text(color = "#262A32", size = 12, family = "AlrightSans-MediumItalic")
    
  )
}

# 2. Plots -----
# 2.a Figure X.x -----
# Transform data
plot_data <- daily_pivot[c("Date", "Country", "_C_cum_ln_c", "_I_c")] %>%
  rename(ER = "_C_cum_ln_c", IR = "_I_c") %>%
  mutate(IR = ifelse(IR < 0, NA, IR)) # Removing interest rate decreases

# Labels
lab_count_exchange_names <- c("Hungary - HUF",
                              "European Union - EUR/HUF",
                              "United States - USD/HUF",
                              "United Kingdom - GBP/HUF",
                              "Switzerland - CHF/HUF",
                              "Czech Republic - CZK/HUF",
                              "Poland - PLN/HUF")
names(lab_count_exchange_names) <- Countries

# Plot
figureXx <- ggplot(plot_data) +
  geom_point(aes(x = Date, y = 0, size = IR, color = Country), alpha = 0.5, na.rm = TRUE) +
  geom_line(aes(x = Date, y = ER, color = Country), linewidth = 0.5) +
  labs(x = "Date",
       y = "Cumulated Daily Exchange Rate Changes",
       size = "Size of Interest Rate Increase") +
  #labs(title = "Interest Rate Changes and Daily Exchange Rates") +
  scale_x_date(expand = expansion(0),
               date_labels = "%b\n%Y") + 
  scale_y_continuous(expand = expansion(0),
                     sec.axis = sec_axis(~.)) +
  scale_color_manual(values = palettes[[length(Countries)]],
                     guide = "none") +
  facet_wrap(vars(Country),
             nrow = length(Countries),
             labeller = labeller(Country = lab_count_exchange_names)) +
  theme(legend.position = "right",
        legend.direction = "vertical",
        legend.title = element_text(angle = 270),
        legend.margin = margin(0, 0.25, 0, 0, "cm")) +
  fx_theme()

ggsave(filename = paste0(plots, "figureXx3.pdf"), plot = figureXx, width = 159, height = 226, units = "mm", dpi = 300)













