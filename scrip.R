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
daily_data <- as_tibble(data.frame(Date = seq(as.Date("2020-12-21"), as.Date("2024-01-02"), by = "day")))

# Adding columns
daily_data <- left_join(daily_data, daily_exchange_rates_all %>% select(Date, starts_with(Countries)), by = "Date")
daily_data <- left_join(daily_data, daily_interest_rates_all %>% select(Date, starts_with(Countries)), by = "Date")

# Filtering non-trading days out; filling additional NA/NaN values; renaming the currencies
daily_data <- daily_data %>% 
  filter(!is.na(HUF)) %>% 
  fill(everything()) %>%
  slice(-1) %>% # this depends on the first day(s) of the table
  rename_at(vars(-c(ends_with("_I"), Date)), ~ paste0(sub(".$", "_C", .)))

# Transform data and add new columns
daily_data <- daily_data %>%
  # Interest rates
  mutate(across(matches(paste0("^", Countries, "_I$")), ~ ifelse(lead(.) - . == 0, NA, lead(.) - .), .names = "{.col}_c")) %>% # Nominal changes for Interest rates
  # Exchange rates
  mutate(across(matches(paste0("^", Countries, "_C$")), ~ (lead(.) - .), .names = "{.col}_c")) %>% # Nominal changes for Exchange rates
  mutate(across(matches(paste0("^", Countries, "_C$")), ~ c(0, diff(log(.))) * 100, .names = "{.col}_ln_c")) %>% # Percentage changes for Exchange rates
  mutate(across(matches(paste0("^", Countries, "_C$")), ~ cumsum(c(0, diff(log(.))) * 100), .names = "{.col}_cum_ln_c")) %>% # Cumulative percentage changes for Exchange rates
  # Returns
  mutate(across(matches(paste0("^", Countries, "_C$")), ~ (c(0, diff(log(.))) * 100) - HU_I + get(sub("_C$", "_I", cur_column())), .names = "{sub('_C$', '_R', .col)}")) %>% # Arithmetic daily returns (interest rate parity)
  mutate(across(matches(paste0("^", Countries, "_R$")), ~ cumsum(.), .names = "{.col}_cum")) # Cumulative arithmetic daily returns (interest rate parity)
  # létezik?????

# Exporting final data set
write_csv(daily_data, paste0(output, "daily_data.csv"))

# Transform to pivot table
Variables <- c("_C", "_I", "_I_c", "_C_c", "_C_ln_c", "_C_cum_ln_c", "_R")
Variables_names <- c("Nominal_Exchange_Rate", "Nominal_Interest_Rate", "Nominal_Interest_Rate_Change", "Nominal_Exchange_Rate_Change", "Percentage_Exchange_Rate_Change", "Cumulative_Percentage_Exchange_Rate_Change", "Daily_Return")

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
    filter(.data[[paste0(Countries[i], "_I_c")]] > 0) %>% 
    rename_all(~str_remove(., "_I_c"))
}
# ez kell meg?????????

# Data tables with event windows
daily_event <- split(daily_pivot, daily_pivot$Country) # creating list
daily_event <- lapply(daily_event, function(df) {rownames(df) <- NULL; df}) # resetting the row numbers

# indices
index <- which(daily_event[[i]]$Date %in% event_dates)

window_size = 4
w_index <- as.integer()
for (j in 1:length(index)) {w_index <- c(w_index, (index[j] - window_size):(index[j] + window_size))}


for (i in Countries) {
  # print(i)
  daily_event[[i]]$Country <- NULL
  
  daily_event[[i]]$Event <- ifelse(daily_event[[i]]$Date %in% event_dates, 1, NA)
  
  daily_event[[i]]$Windows <- ifelse(row.names(daily_event[[i]]) %in% w_index, 1, NA)
  
  
}

# Event and Window Dates
event_dates <- daily_event[["HU"]] %>% filter(Event == 1) %>% pull(Date)
window_dates <- daily_event[["HU"]] %>% filter(Windows == 1) %>% pull(Date)


event_data <- data.frame(xstart = rep(as.Date("1000-01-01"), length(event_dates)), xend = rep(as.Date("1000-01-01"), length(event_dates)), xintercept = rep(as.Date("1000-01-01"), length(event_dates)))
for (i in 1:length(event_dates)) {
  event_data[i, 1] <- window_dates[(i - 1)*9 + 1]
  event_data[i, 2] <- window_dates[(i - 1)*9 + 9]
  event_data[i, 3] <- event_dates[i]
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
    plot.margin = unit(c(0.5, 0.25, 0.5, 0.25), "cm"), # Set plot margins
    
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
# 2.a figure_I_intro -----
# Transform data
plot_data <- daily_pivot[c("Date", "Country", "_C", "_I_c")] %>%
  rename(ER = "_C", IR = "_I_c") %>%
  mutate(IR = ifelse(IR < 0, NA, IR)) %>% #Removing interest rate decreases
  filter(Date >= "2020-12-31" & Date <= "2024-01-02") %>% 
  mutate(Date = if_else(Date == as.Date("2020-12-31"), as.Date("2021-01-01"), Date)) %>% #Modifying dates for plots
  mutate(y_ER = case_when(Country == "HU" ~ 1,
                          Country == "CH" ~ 350,
                          Country == "EU" ~ 360,
                          Country == "CZ" ~ 15,
                          Country == "GB" ~ 400,
                          Country == "PL" ~ 80,
                          Country == "US" ~ 300,
                          .default = 0))

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
plot <- ggplot(plot_data, aes(x = Date, color = Country)) +
  geom_point(aes(y = y_ER, size = IR), alpha = 0.5, na.rm = TRUE) +
  geom_line(aes(y = ER), linewidth = 0.5) +
  labs(x = "Date",
       y = "Exchange Rate",
       size = "Size of Interest Rate Increase") +
  #labs(title = "Daily Spot Exchange Rates and Interest Rate Changes") +
  
  scale_x_date(expand = expansion(0),
               date_labels = "%b\n%Y",
               date_breaks = "6 months") + 
  scale_y_continuous(sec.axis = sec_axis(~.),
                     ) +
  scale_color_manual(values = palettes[[length(Countries)]],
                     guide = "none") +
  
  facet_wrap(vars(Country),
             scales = "free_y",
             nrow = length(Countries),
             labeller = labeller(Country = lab_count_exchange_names)) +
  
  fx_theme()  +
  theme(legend.direction = "vertical",
        legend.title = element_text(angle = 270),
        legend.margin = margin(0, 0.25, 0, 0, "cm"))

ggsave(filename = paste0(plots, "figure_I_intro.pdf"), plot = plot, width = 160, height = 226, units = "mm", dpi = 300)


# 2.b figure_III_interest -----
# Transform data
plot_data <- daily_pivot[c("Date", "Country", "_I")] %>% 
  rename(IR = "_I") %>% 
  filter(Date >= "2020-12-31" & Date <= "2024-01-02") %>% 
  mutate(Date = if_else(Date == as.Date("2020-12-31"), as.Date("2021-01-01"), Date)) #Modifying dates for plots

# Plot
plot <- ggplot(plot_data) +
  geom_line(aes(x = Date, y = IR, color = Country), linewidth = 0.5) +
  labs(x = "Date",
       y = "Interest Rate") +
  #labs(title = "Interest Rate Changes and Daily Exchange Rates") +
  
  scale_x_date(expand = expansion(0),
               date_labels = "%b\n%Y",
               date_breaks = "6 months") + 
  scale_y_continuous(breaks = seq(0, 15, by = 2.5),
                     minor_breaks = seq(-0.5, 15, by = 0.5)) +
  
  scale_color_manual(values = palettes[[length(Countries)]],
                     name = "Country") +
  
  fx_theme()

ggsave(filename = paste0(plots, "figure_III_interest.pdf"), plot = plot, width = 160, height = 90, units = "mm", dpi = 300)



# 2.c figure_III_exchange -----
# Transform data
plot_data <- daily_pivot[c("Date", "Country", "_C_cum_ln_c")] %>% 
  filter(Country != "HU") %>%
  rename(ER = "_C_cum_ln_c") %>% 
  filter(Date >= "2020-12-31" & Date <= "2024-01-02") %>% 
  mutate(Date = if_else(Date == as.Date("2020-12-31"), as.Date("2021-01-01"), Date)) #Modifying dates for plots

# Labels
lab_count_exchange_names <- c("HUF", "EUR", "USD", "GBP", "CHF", "CZK", "PLN")
names(lab_count_exchange_names) <- Countries

# Plot
plot <- ggplot(plot_data) +
  geom_line(aes(x = Date, y = ER, color = Country), linewidth = 0.5) +
  labs(x = "Date",
       y = "Cumulative Changes") +
  #labs(title = "Interest Rate Changes and Daily Exchange Rates") +
  
  scale_x_date(expand = expansion(0),
               date_labels = "%b\n%Y",
               date_breaks = "6 months") + 
  scale_y_continuous(breaks = seq(-10, 45, by = 5),
                     minor_breaks = seq(-10, 45, by = 2.5)) +
  
  scale_color_manual(values = palettes[[length(Countries)-1]],
                     labels = lab_count_exchange_names,
                     name = "Currency") +

  fx_theme()

ggsave(filename = paste0(plots, "figure_III_exchange.pdf"), plot = plot, width = 160, height = 100, units = "mm", dpi = 300)


# 2.d figure_III_event -----
# Transform data
plot_data <- daily_pivot[c("Date", "Country", "_I_c")] %>% 
  rename(IR = "_I_c") %>% 
  mutate(IR_inc = ifelse(IR > 0, IR, NA)) %>% 
  mutate(IR_dec = ifelse(IR < 0, IR, NA)) %>% 
  filter(Date >= "2020-12-31" & Date <= "2024-01-02") %>% 
  mutate(Date = if_else(Date == as.Date("2020-12-31"), as.Date("2021-01-01"), Date)) #Modifying dates for plots
  
# Plot
plot <- ggplot(plot_data, aes(x = Date, y = Country, color = Country)) +
  geom_point(aes(size = IR_inc), alpha = 0.5, na.rm = TRUE) +
  geom_point(aes(size = IR_dec), shape = 18, alpha = 0.5, na.rm = TRUE, show.legend = F) +
  labs(x = "Date",
       y = "Country",
       size = "Size of Increase") +
  #labs(title = "Interest Rate Changes") +
  scale_x_date(expand = expansion(0),
               date_labels = "%b\n%Y",
               date_breaks = "6 months") + 
  scale_color_manual(values = palettes[[length(Countries)]],
                     guide = "none") +
  scale_size(breaks = c(0.25, 0.75, 1, 1.5, 2)) +
  
  fx_theme() +
  theme(legend.position = "bottom",
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.25), "cm"))

ggsave(filename = paste0(plots, "figure_III_event.pdf"), plot = plot, width = 160, height = 90, units = "mm", dpi = 300)



# 2.e figure_III_windows -----
# Transform data
plot_data <- daily_pivot[c("Date", "Country", "_C", "_I_c")] %>% 
  rename(ER = "_C", IR = "_I_c") %>% 
  mutate(Event = ifelse(IR > 0, ER, NA)) %>% 
  filter(Date >= "2021-04-01" & Date <= "2023-01-02") %>% #Modifying dates for plots
  filter(Country != "HU")


# Labels
lab_count_exchange_names <- c("Hungarian Forint - HUF",
                              "Euro - EUR/HUF",
                              "US Dollar - USD/HUF",
                              "Pound Sterling - GBP/HUF",
                              "Swiss Franc - CHF/HUF",
                              "Czech Koruna - CZK/HUF",
                              "Polish Złoty - PLN/HUF")
names(lab_count_exchange_names) <- Countries

# Plot
plot <- ggplot() +
  geom_line(data = plot_data, aes(x = Date, y = ER, color = Country), linewidth = 0.5) +
  geom_rect(data = event_data, aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf), color = "#3B99B1", fill = "#3B99B1", alpha = 0.25, linetype = "dashed", linewidth = 0.25) +
  geom_vline(xintercept = event_data$xintercept, color = "#F5191C", linetype = "dashed", linewidth = 0.25, na.rm = TRUE) +
  
  geom_point(data = plot_data, aes(x = Date, y = Event), shape = 18, size = 3, color = "#3B99B1", alpha = 0.75, na.rm = TRUE) +
  
  labs(x = "Date",
       y = "Exchange Rate") +
  #labs(title = "Interest Rates of the certain Central Banks and the Hungarian National Bank - MNB") +
  
  scale_x_date(expand = expansion(0),
               date_labels = "%b\n%Y",
               date_breaks = "3 months") + 
  scale_y_continuous(sec.axis = sec_axis(~.)) +
  
  scale_color_manual(values = palettes[[length(Countries)]],
                     guide = "none") +
  
  facet_wrap(vars(Country),
             scales = "free_y",
             nrow = length(Countries),
             labeller = labeller(Country = lab_count_exchange_names)) +
  
  fx_theme()
    
    
ggsave(filename = paste0(plots, "figure_III_windows_2.pdf"), plot = plot, width = 160, height = 230, units = "mm", dpi = 300)


# 2.f figure_III_return -----
# Transform data
plot_data <- daily_pivot[c("Date", "Country", "_C_cum_ln_c", "_I", "_R")] %>% 
  filter(Country != "HU") %>% 
  filter(Date >= "2020-12-31" & Date <= "2023-01-02") %>%
  rename(ER = "_C_cum_ln_c", IR = "_I", R = "_R") %>% 
  left_join(daily_pivot[c("Date", "Country", "_I")] %>% filter(Country == "HU") %>% select(-Country), by = "Date") %>%
  rename(HU_IR = "_I")

# Labels 1
lab_count_exchange_names <- c("Hungarian National Bank - MNB",
                              "European Central Bank - ECB",
                              "Federal Reserve - FED",
                              "Bank of England - BoE",
                              "Swiss National Bank - BNS",
                              "Czech National Bank - ČNB",
                              "National Bank of Poland - NBP")
names(lab_count_exchange_names) <- Countries

# Plot 1
plot <- ggplot(plot_data, aes(x = Date)) +
  geom_line(aes(y = IR, color = Country), linewidth = 0.5) +
  geom_point(aes(y = HU_IR), linewidth = 0.5, color = "#3B99B1") +
  
  labs(x = "Date",
       y = "Interest Rate") +
  #labs(title = "Interest Rates of the certain Central Banks and the Hungarian National Bank - MNB") +
  
  scale_x_date(expand = expansion(0),
               date_labels = "%b\n%Y") + 
  scale_y_continuous(sec.axis = sec_axis(~.)) +
  scale_color_manual(values = palettes[[length(Countries)]],
                     guide = "none") +
  
  facet_wrap(vars(Country),
             scales = "free_y",
             nrow = length(Countries),
             labeller = labeller(Country = lab_count_exchange_names)) +
  
  fx_theme()

ggsave(filename = paste0(plots, "figure_III_return_1.pdf"), plot = plot, width = 80, height = 230, units = "mm", dpi = 300)

# Labels 2
lab_count_exchange_names <- c("Hungarian Forint - HUF",
                              "Euro - EUR/HUF",
                              "US Dollar - USD/HUF",
                              "Pound Sterling - GBP/HUF",
                              "Swiss Franc - CHF/HUF",
                              "Czech Koruna - CZK/HUF",
                              "Polish Złoty - PLN/HUF")
names(lab_count_exchange_names) <- Countries

# Plot 2
plot <- ggplot(plot_data, aes(x = Date)) +
  geom_line(aes(y = ER, color = Country), linewidth = 0.5, alpha = 0.5) +
  geom_line(aes(y = R, color = Country), linewidth = 0.5) +

  labs(x = "Date",
       y = "Return") +
  #labs(title = "Arithmetic Daily Returns with Interest Rate Parity") +

    scale_x_date(expand = expansion(0),
               date_labels = "%b\n%Y") + 
  scale_y_continuous(sec.axis = sec_axis(~.)) +
  scale_color_manual(values = palettes[[length(Countries)-1]],
                     guide = "none") +
  
  facet_wrap(vars(Country),
             scales = "free_y",
             nrow = length(Countries),
             labeller = labeller(Country = lab_count_exchange_names)) +
  
  fx_theme() +
  theme()
  

ggsave(filename = paste0(plots, "figure_III_return_2.pdf"), plot = plot, width = 80, height = 226, units = "mm", dpi = 300)





# III. Section - Statistics =====






















