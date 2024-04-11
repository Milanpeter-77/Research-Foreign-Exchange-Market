# =================================== #
# Empirical Analysis on the FX Market #
# =================================== #
# I. Section - Pre-Settings -----
rm(list=ls())
wd <- "~/Documents/Research/2024 - Foreign Exchange Market"
input <- paste0(wd, "/raw_data/")
output <- paste0(wd, "/new_data/")
plots <- paste0(wd, "/plots/")
tables <- paste0(wd, "/tables/")
setwd(wd)

# R-Libraries
library(readxl)
library(tidyverse)
library(ggplot2)
library(showtext)
library(writexl)
library(tseries)
library(moments)
library(psych)

# I. Section - Data preparation =====
# - 1. Exchange Rates -----
daily_exchange_rates_all <- read_excel(paste0(input, "arfolyam.xlsx"), col_types = c("date", rep("numeric", 75)))
daily_exchange_rates_all <- daily_exchange_rates_all %>%
  slice(-1) %>%
  rename("Date" = "Dátum/ISO") %>%
  mutate_at("Date", as.Date) %>%
  mutate("HUF" = 1)

write_csv(daily_exchange_rates_all, paste0(output, "daily_exchange_rates_all.csv"))

# - 2. Interest Rates -----
daily_interest_rates_all <- as_tibble(read.csv(paste0(input, "WS_CBPOL_csv_row.csv"), skip = 8))
daily_interest_rates_all <- daily_interest_rates_all %>%
  select(-starts_with("M.")) %>%
  rename_all(~str_remove(., "D\\.")) %>%
  rename("Date" = "Time.Period") %>%
  rename("EU" = "XM") %>%
  mutate_at("Date", as.Date) %>%
  rename_with(~paste0(., "_I"), -Date)

write_csv(daily_interest_rates_all, paste0(output, "daily_interest_rates_all.csv"))

# - 3. Data tables -----
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
for (i in Countries) {
  interest_changes[[i]] <- daily_data %>% 
    select("Date", paste0(i, "_I_c")) %>%
    filter(.data[[paste0(i, "_I_c")]] > 0) %>% 
    rename_all(~str_remove(., "_I_c"))
}

# Data tables with event windows
daily_event <- split(daily_pivot %>% select(c("Date", "Country", "_R")) %>% rename(Return = "_R"), daily_pivot$Country) # creating list
daily_event <- lapply(daily_event, function(df) {rownames(df) <- NULL; df}) # resetting the row numbers

# indices
index <- which(daily_event[["HU"]]$Date %in% interest_changes[["HU"]]$Date)

window_size = 4
w_index <- as.integer()
for (j in 1:length(index)) {w_index <- c(w_index, (index[j] - window_size):(index[j] + window_size))}
window_days <- rep(seq(-4, 4, 1), times = 17)
window_dateforevent <- rep(interest_changes[["HU"]]$Date, each = 9)

for (i in Countries) {
  # print(i)
  daily_event[[i]]$Country <- NULL
  
  daily_event[[i]]$Event <- ifelse(daily_event[[i]]$Date %in% interest_changes[["HU"]]$Date, 1, NA)
  
  daily_event[[i]]$Windows <- ifelse(row.names(daily_event[[i]]) %in% w_index, 1, NA)
  
  daily_event[[i]]$Days <- ifelse(row.names(daily_event[[i]]) %in% w_index, 1, NA)

  daily_event[[i]]$Estimation <- NA
  daily_event[[i]]$Estimation[7:107] <- 1 # because the first date is 2020-12-21, and I want it to start at 2021
}

# Event and Window Dates
event_dates <- daily_event[["HU"]] %>% filter(Event == 1) %>% pull(Date)
window_dates <- daily_event[["HU"]] %>% filter(Windows == 1) %>% pull(Date)
estimate_dates <- daily_event[["HU"]] %>% filter(Estimation == 1) %>% pull(Date)

for (i in Countries) for (j in 1:nrow(daily_event[[i]])) {
  # print(j)
  daily_event[[i]]$Days[j] <- ifelse(daily_event[[i]]$Days[j] == 1,
                                     window_days[which(window_dates == daily_event[[i]]$Date[j])], NA)
  }


event_data <- data.frame(xstart = rep(as.Date("1000-01-01"), length(event_dates)), xend = rep(as.Date("1000-01-01"), length(event_dates)), xintercept = rep(as.Date("1000-01-01"), length(event_dates)))
for (i in 1:length(event_dates)) {
  event_data[i, 1] <- window_dates[(i - 1)*9 + 1]
  event_data[i, 2] <- window_dates[(i - 1)*9 + 9]
  event_data[i, 3] <- event_dates[i]
}
estimate_data <- data.frame(xstart = as.Date(estimate_dates[1]), xend = as.Date(estimate_dates[length(estimate_dates)]))


# Inference windows for robustness testing
inference_windows <- list()
for (i in Countries[!Countries == "HU"]) {
  inference_windows[[i]] <- as.Date("1001-01-01")
  
  for (j in seq_along(interest_changes[[i]]$Date)) {

    if (interest_changes[[i]]$Date[j] %in% window_dates) {
      
      start_index <- ((ceiling(which(window_dates == interest_changes[[i]]$Date[j]) / 9) - 1) * 9 + 1)
      end_index <- start_index + 8
      

      inference_windows[[i]] <- c(inference_windows[[i]], window_dates[start_index:end_index])
    }
  }
  inference_windows[[i]] <- inference_windows[[i]][-1]
}

for (i in Countries) {
  daily_event[[i]]$Inference <- ifelse(daily_event[[i]]$Date %in% inference_windows[[i]], 1, NA)
}


# Average Interest Rate Difference
interest_data <- daily_interest_rates_all %>%
  select(Date, starts_with(Countries)) %>% 
  filter(Date %in% window_dates) %>% 
  select(-Date) %>% 
  fill(everything()) %>%
  rename_at(vars(c(ends_with("_I"))), ~ paste0(sub("_I", "", .))) %>% 
  mutate(across(everything(), ~ . - !!sym("HU"))) %>%
  summarise(across(everything(), mean)) %>% 
  mutate(mean = rowMeans(across(everything())))


# II. Section - Charts and Plots =====
# - 1. Dependencies -----
# Color palettes
palettes <- list()
for (i in 2:20) {
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
    plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"), # Set plot margins
    
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

# - 2. Plots -----
# -- 2.a figure_I_intro -----
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
        legend.title = element_text(angle = 270))

ggsave(filename = paste0(plots, "figure_I_intro.pdf"), plot = plot, width = 175, height = 240, units = "mm", dpi = 300)


# --  2.b figure_III_interest -----
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

ggsave(filename = paste0(plots, "figure_III_interest.pdf"), plot = plot, width = 160, height = 80, units = "mm", dpi = 300)



# --  2.c figure_III_exchange -----
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


# -- 2.d figure_III_event -----
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



# -- 2.e figure_III_windows -----
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


# -- 2.f figure_III_return -----
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
  geom_line(aes(y = HU_IR), linewidth = 0.5, color = "#3B99B1") +
  
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





# III. Section - Excess Returns  =====
# - 1. Calculating Expected Returns -----
for (i in Countries) {
  # print(i)
  # Random Walk Model
  daily_event[[i]]$RW <- 0
  
  # Mean-adjusted Returns
  daily_event[[i]]$MeA <- mean(daily_event[[i]]$Return[daily_event[[i]]$Estimation == 1], na.rm = TRUE)
  
  # Market-adjusted Returns
  daily_event[[i]]$MaA <- rowMeans(do.call(cbind, Map(`[[`, daily_event[Countries[!Countries %in% c("HU", i)]], "Return"))) # * it helps you understand
  
  # Market Model
  daily_event[[i]]$MM <- predict(lm(Return ~ MaA, subset(daily_event[[i]], Estimation == 1)), newdata = daily_event[[i]])
  
}
# *
# rowMeans(cbind(daily_event[["EU"]][["_R"]],
#                daily_event[["US"]][["_R"]],
#                daily_event[["GB"]][["_R"]],
#                daily_event[["CH"]][["_R"]],
#                daily_event[["CZ"]][["_R"]],
#                daily_event[["PL"]][["_R"]]))

# - 2. Plots of Returns and Windows -----
Models <- c("RW", "MeA", "MaA", "MM")
plot_list <- list()
for (i in Countries) {
  plot_list[[i]] <- daily_event[[i]] %>%
    select(c(Date, Return, RW, MeA, MaA, MM)) %>%
    filter(Date >= "2020-12-31" & Date <= "2022-11-01") %>% # or "2023-01-02"
    mutate(Date = if_else(Date == as.Date("2020-12-31"), as.Date("2021-01-01"), Date)) %>% 
    pivot_longer(cols = c(RW, MeA, MaA, MM),
                 names_to = "Model",
                 values_to = "E") %>%
    mutate(Model = case_when(
      Model == "RW" ~ "Random Walk Model",
      Model == "MeA" ~ "Mean-adjusted Returns",
      Model == "MaA" ~ "Market-adjusted Returns",
      Model == "MM" ~ "Market Model"))
}

# Labels
lab_count_exchange_names <- c("Hungarian Forint - HUF",
                              "Euro - EUR/HUF",
                              "US Dollar - USD/HUF",
                              "Pound Sterling - GBP/HUF",
                              "Swiss Franc - CHF/HUF",
                              "Czech Koruna - CZK/HUF",
                              "Polish Złoty - PLN/HUF")
names(lab_count_exchange_names) <- Countries

# Plots
for (i in Countries[Countries != "HU"]) {
  plot <- ggplot(data = plot_list[[i]]) +
    geom_rect(data = estimate_data, aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf), color = "#3F6A25", fill = "#3F6A25", alpha = 0.1, linetype = "dashed", linewidth = 0.25) +
    
    geom_rect(data = event_data, aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf), color = "#3B99B1", fill = "#3B99B1", alpha = 0.15, linetype = "dashed", linewidth = 0.25) +
    geom_vline(xintercept = event_data$xintercept, color = "#F5191C", linetype = "dashed", linewidth = 0.25, na.rm = TRUE) +

    geom_line(aes(x = Date, y = Return), color = palettes[[7]][which(Countries == i) - 1], linewidth = 0.5) +
    geom_line(aes(x = Date, y = E), color = palettes[[7]][7], linewidth = 0.5) +
    
    labs(x = "Date",
         y = "Return",
         title = lab_count_exchange_names[i]) +
    
    scale_x_date(expand = expansion(0),
                 date_labels = "%b\n%Y",
                 date_breaks = "3 months") + 
    scale_y_continuous(sec.axis = sec_axis(~.)) +
    
    facet_wrap(~ factor(Model, c("Random Walk Model", "Mean-adjusted Returns", "Market-adjusted Returns", "Market Model")),
               nrow = length(Models)) +
    
    fx_theme()
  
  ggsave(filename = paste0(plots, "Appendix/", "figure_III_excess_", i, ".pdf"), plot = plot, width = 160, height = 110, units = "mm", dpi = 300)
}

# IV. Section - Statistical Tests =====
# - 1. Estimation Data Testing -----
# ADF-test
# Create the (still empty) table
statistics <- as.data.frame(matrix(NA, nrow = length(Countries), ncol = length(Models) + 1))
colnames(statistics) <- c("Return", Models)
rownames(statistics) <- Countries
# 
for (i in Countries) for (j in c("Return", Models)) {
  statistics[i, j] <- adf.test(daily_event[[i]] %>% filter(Estimation == 1) %>% pull(j))[["p.value"]]
}
write_csv(statistics, paste0(tables, "adf_estimation_window.csv"))
#test <- adf.test(daily_event[["EU"]] %>% filter(Estimation == 1) %>% pull(Return))

# -- 1.a figure_IV_mean -----
# Transform data
plot_data <- expand.grid(Models, Countries)
colnames(plot_data) <- c("Models", "Country")
plot_data[c("R_Mean", "R_SD", "E_Mean", "E_SD")] <- NA

for (i in Countries) for (j in Models) {
  plot_data[which(plot_data$Country == i & plot_data$Models == j), "R_Mean"] <-
    mean(daily_event[[i]] %>% filter(Estimation == 1) %>% pull(Return))
  plot_data[which(plot_data$Country == i & plot_data$Models == j), "R_SD"] <-
    sd(daily_event[[i]] %>% filter(Estimation == 1) %>% pull(Return))
  
  plot_data[which(plot_data$Country == i & plot_data$Models == j), "E_Mean"] <-
    mean(daily_event[[i]] %>% filter(Estimation == 1) %>% pull(j))
  plot_data[which(plot_data$Country == i & plot_data$Models == j), "E_SD"] <-
    sd(daily_event[[i]] %>% filter(Estimation == 1) %>% pull(j))
}

plot_data <- plot_data %>% filter(Country != "HU")

# Labels
lab_count_exchange_names <- c("EUR", "USD", "GBP", "CHF", "CZK", "PLN")
names(lab_count_exchange_names) <- Countries[!Countries == "HU"]

# Plot
plot <- ggplot(data = plot_data) +
  
  geom_point(aes(x = as.numeric(Models) - 0.15, y = R_Mean, color = Country), shape = 18, size = 2.5) +
  geom_errorbar(aes(x = as.numeric(Models) - 0.15, ymin = R_Mean - R_SD, ymax = R_Mean + R_SD, color = Country), width = 0.2, linewidth = 0.25) +
  
  geom_point(aes(x = as.numeric(Models) + 0.15, y = E_Mean, color = Country), shape = 18, size = 2.5, alpha = 0.5) +
  geom_errorbar(aes(x = as.numeric(Models) + 0.15, ymin = E_Mean - E_SD, ymax = E_Mean + E_SD, color = Country), alpha = 0.5, width = 0.2, linewidth = 0.25) +
  
  labs(x = "Models",
       y = "Mean with Standard Error") + 
  
  scale_x_continuous(breaks = 1:4, labels = c("RW", "MeA", "MaA", "MM")) +
  scale_y_continuous(sec.axis = sec_axis(~.)) +
  scale_color_manual(values = palettes[[length(Countries) - 1]],
                     guide = "none") +  
  
  facet_wrap(vars(Country),
             nrow = 2,
             scales = "free_x",
             labeller = labeller(Country = lab_count_exchange_names)) +
  
  fx_theme()

ggsave(filename = paste0(plots, "figure_IV_mean.pdf"), plot = plot, width = 160, height = 85, units = "mm", dpi = 300)


# - 2. Event Data Testing -----

# Pivot table for aggregation
agg_pivot <- daily_pivot %>% 
  select(Date, Country, `_R`) %>% 
  filter(Country != "HU") %>% 
  rename(Return = `_R`) %>% 
  mutate(Event = ifelse(Date %in% event_dates, 1, NA),
         Windows = ifelse(Date %in% window_dates, 1, NA),
         Days = Windows,
         DateforEvent = as.Date("1000-01-01"))
for (i in 1:nrow(agg_pivot)) {
  ifelse(agg_pivot$Windows[i] == 1,
         agg_pivot$Days[i] <- window_days[which(window_dates == agg_pivot$Date[i])], NA)
  ifelse(agg_pivot$Windows[i] == 1,
         agg_pivot$DateforEvent[i] <- as.Date(window_dateforevent[which(window_dates == agg_pivot$Date[i])]),
         agg_pivot$DateforEvent[i] <- NA) # does not work but does not matter...
}

# -- 2.a Tables and Plots for Aggregated Data -----
# Table - Returns aggregated by Date
statistics <- as.data.frame(matrix(0, nrow = length(Countries), ncol = (window_size * 2 + 2)))
colnames(statistics) <- c("Currency", seq(-3, 4, 1), "Average")
statistics$Currency <- c(Countries[!Countries == "HU"], "Average")
rownames(statistics) <- statistics$Currency

for (i in 1:length(Countries[!Countries == "HU"])) for (j in -3:4) {
  statistics[i, as.character(j)] <- mean(agg_pivot %>% filter(Country == Countries[!Countries == "HU"][i] & Days == j) %>% pull(Return))
}
statistics$Average <- rowMeans(statistics[2:9])
statistics["Average", 2:10] <- colMeans(statistics[1:6, 2:10])

statistics[, 2:10] <- round(statistics[, 2:10], 2)
write_csv(statistics, paste0(tables, "aggdata_bydate.csv"))

# -- 2.b figure_IV_aggbydate -----
# 1
plot_data <- as.data.frame(t(rbind(colnames(statistics), statistics))[2:9, ]) %>%
  pivot_longer(cols = -1, names_to = "Country", values_to = "Return") %>% rename(Day = `1`) %>% mutate(Return = as.numeric(Return),
                                                                                                       Day = as.numeric(Day))
lab_count_exchange_names <- c("EUR", "USD", "GBP", "CHF", "CZK", "PLN")
names(lab_count_exchange_names) <- Countries[!Countries == "HU"]

plot <- ggplot(data = plot_data) +
  geom_line(data = (plot_data %>% filter(Country != "Average")), aes(x = Day, y = Return, color = Country), linewidth = 1.5, alpha = 0.5) +
  geom_line(data = (plot_data %>% filter(Country == "Average")), aes(x = Day, y = Return), linewidth = 2, color = "#F5191C") +
  geom_vline(xintercept = 0, color = "#262A32", linetype = "dashed", linewidth = 0.25) +
  
  labs(x = "Event Days",
     y = "Return") +
  
  scale_x_continuous(expand = expansion(0),
                     breaks = seq(-3, 4, by = 1)) + 
  scale_y_continuous(breaks = seq(1, -6, by = -0.5),
                     minor_breaks = seq(1, -6, by = -0.25)) +
  scale_color_manual(values = palettes[[length(Countries)-1]],
                     labels = lab_count_exchange_names,
                     name = "Currency") +
  fx_theme()

ggsave(filename = paste0(plots, "figure_IV_aggbydate1.pdf"), plot = plot, width = 118, height = 80, units = "mm", dpi = 300)

# 2
plot_data <- as.data.frame(t(statistics[7, 2:9]))
plot_data <- cbind(rownames(plot_data), plot_data)
colnames(plot_data) <- c("Days", "Average")

plot <- ggplot(data = plot_data) +
  geom_bar(aes(x = Days, y = Average, fill = Days), stat = "identity", show.legend = FALSE, alpha = 0.5) +
  geom_hline(yintercept = statistics[7, 10], color = "#F5191C", linewidth = 2) +
  
  coord_flip() +
  
  labs(x = NULL,
       y = "Average Return") +
  
  scale_y_reverse(breaks = seq(1, -6, by = -1),
                  minor_breaks = seq(1, -6, by = -0.5)) +
  scale_fill_manual(values = rev(palettes[[8]])) +
  
  fx_theme() +
  theme(axis.title = element_text(size = 10))

ggsave(filename = paste0(plots, "figure_IV_aggbydate2.pdf"), plot = plot, width = 38, height = 80, units = "mm", dpi = 300)

# Table - Returns aggregated by Currency
statistics <- as.data.frame(matrix(0, nrow = length(event_dates) + 1, ncol = (window_size * 2 + 2)))
colnames(statistics) <- c("EventDate", seq(-3, 4, 1), "Average")
statistics$EventDate <- c(as.character(event_dates), "Average")
rownames(statistics) <- statistics$EventDate

for (i in 1:length(event_dates)) for (j in -3:4) {
  statistics[i, as.character(j)] <- mean(agg_pivot %>% filter(DateforEvent == event_dates[i] & Days == j) %>% pull(Return))
}
statistics$Average <- rowMeans(statistics[2:9])
statistics["Average", 2:10] <- colMeans(statistics[1:17, 2:10])

statistics[, 2:10] <- round(statistics[, 2:10], 2)
write_csv(statistics, paste0(tables, "aggdata_bycurrency.csv"))

# -- 2.c figure_IV_aggbycurrency -----
# 1
plot_data <- as.data.frame(t(rbind(colnames(statistics), statistics))[2:9, ]) %>%
  pivot_longer(cols = -1, names_to = "Date", values_to = "Return") %>% rename(Day = `1`) %>% filter(Date != "Average") %>% 
  mutate(Return = as.numeric(Return), Day = as.numeric(Day))
plot_data$Date <- factor(plot_data$Date, levels = unique(plot_data$Date), ordered = TRUE)

plot <- ggplot(data = plot_data) +
  geom_line(aes(x = Return, y = Date, color = Date), linewidth = 1.5, lineend = "round", show.legend = FALSE) +
  geom_hline(yintercept = 10.5, color = "#262A32", linetype = "dashed", linewidth = 0.25) +

  labs(x = "Return",
       y = NULL) +

  scale_color_manual(values = palettes[[17]], 
                     breaks = unique(plot_data$Date), 
                     labels = unique(plot_data$Date)) +
  
  scale_x_continuous(breaks = seq(1, -10, by = -1),
                     minor_breaks = seq(1, -10, by = -0.5)) +
  
  scale_y_discrete(limits = rev(levels(plot_data$Date)),
                   position = "right") +
  
  fx_theme()


ggsave(filename = paste0(plots, "figure_IV_aggbycurrency1.pdf"), plot = plot, width = 128, height = 80, units = "mm", dpi = 300)

# 2
plot_data <- statistics[1:17, ] %>% select(EventDate, Average)
plot_data$EventDate <- factor(plot_data$EventDate, levels = rev(unique(plot_data$EventDate)), ordered = TRUE)

plot <- ggplot(data = plot_data) +
  geom_bar(aes(x = EventDate, y = Average, fill = EventDate), stat = "identity", show.legend = FALSE) +
  geom_hline(yintercept = statistics[7, 10], color = "#F5191C", linewidth = 2, alpha = 0.5) +
  geom_vline(xintercept = 10.5, color = "#262A32", linetype = "dashed", linewidth = 0.25) +

  coord_flip() +
  
  labs(x = NULL,
       y = "Average Return") +
  
  scale_y_reverse(breaks = seq(0, -9, by = -1),
                  minor_breaks = seq(1, -9, by = -0.5)) +
  
  scale_fill_manual(values = palettes[[17]], 
                     breaks = unique(plot_data$EventDate), 
                     labels = unique(plot_data$EventDate)) +
  
  fx_theme() +
  theme(axis.text.x = element_text(size = 6),
        axis.text.y = element_blank(),
        axis.title = element_text(size = 10))

ggsave(filename = paste0(plots, "figure_IV_aggbycurrency2.pdf"), plot = plot, width = 28, height = 80, units = "mm", dpi = 300)

# Table - Returns aggregated by Event Days
statistics <- as.data.frame(matrix(0, nrow = length(event_dates) + 1, ncol = length(Countries[!Countries == "HU"]) + 2))
colnames(statistics) <- c("EventDate", Countries[!Countries == "HU"], "Average")
statistics$EventDate <- c(as.character(event_dates), "Average")
rownames(statistics) <- statistics$EventDate

for (i in 1:length(event_dates)) for (j in Countries[!Countries == "HU"]) {
  statistics[i, j] <- mean(agg_pivot %>% filter(DateforEvent == event_dates[i] & Country == j & Days != -4) %>% pull(Return))
}
statistics$Average <- rowMeans(statistics[2:7])
statistics["Average", 2:8] <- colMeans(statistics[1:17, 2:8])

statistics[, 2:8] <- round(statistics[, 2:8], 2)
write_csv(statistics, paste0(tables, "aggdata_byevent.csv"))

# -- 2.d figure_IV_aggbyevent -----
# 1
plot_data <- as.data.frame(t(rbind(colnames(statistics), statistics))[2:8, ]) %>%
  pivot_longer(cols = -1, names_to = "Date", values_to = "Return") %>% rename(Country = `1`) %>% 
  mutate(Return = as.numeric(Return), Date = as.Date(Date))
# plot_data$Date <- factor(plot_data$Date, levels = unique(plot_data$Date), ordered = TRUE)

plot <- ggplot(data = plot_data) +
  geom_line(data = (plot_data %>% filter(Country != "Average")), aes(x = Date, y = Return, color = Country), linewidth = 1.5, alpha = 0.5, na.rm = TRUE) +
  geom_line(data = (plot_data %>% filter(Country == "Average")), aes(x = Date, y = Return), linewidth = 2, color = "#F5191C", na.rm = TRUE) +
  geom_vline(xintercept = as.Date("2022-01-01"), color = "#262A32", linetype = "dashed", linewidth = 0.25) +
  
  labs(x = "Event Date",
       y = "Return") +
  
  scale_x_date(expand = expansion(0),
               date_labels = "%m/\n'%y",
               date_breaks = "1 month") + 
  scale_color_manual(values = palettes[[6]]) +
  scale_y_continuous(breaks = seq(2, -15, by = -1),
                     minor_breaks = seq(2, -15, by = -0.5)) +
  
  fx_theme()

ggsave(filename = paste0(plots, "figure_IV_aggbyevent1.pdf"), plot = plot, width = 128, height = 80, units = "mm", dpi = 300)

# 2
plot_data <- statistics[1:6, ] %>% select(Currency, Average)
plot_data$Currency <- factor(plot_data$Currency, levels = c("US", "PL", "GB", "EU", "CZ", "CH"))

plot <- ggplot(data = plot_data) +
  geom_bar(aes(x = Currency, y = Average, fill = Currency), stat = "identity", show.legend = FALSE, alpha = 0.5) +
  geom_hline(yintercept = aggdata_bydate[7, 10], color = "#F5191C", linewidth = 2) +
  
  coord_flip() +
  
  labs(x = NULL,
       y = "Average Return") +
  
  scale_y_reverse(breaks = seq(1, -6, by = -1),
                  minor_breaks = seq(1, -6, by = -0.5)) +
  scale_fill_manual(values = rev(palettes[[length(Countries)-1]])) +
  
  fx_theme() +
  theme(axis.text.y = element_blank(),
        axis.title = element_text(size = 10))

ggsave(filename = paste0(plots, "figure_IV_aggbyevent2.pdf"), plot = plot, width = 28, height = 80, units = "mm", dpi = 300)

# ADF-test
# Create the (still empty) table
adf_event_window <- as.data.frame(matrix(0, nrow = length(event_dates) + 1, ncol = length(Countries[!Countries == "HU"]) + 2))
colnames(adf_event_window) <- c("EventDate", Countries[!Countries == "HU"], "AllCurrencies")
adf_event_window$EventDate <- c(as.character(event_dates), "AllDates")
pp_event_window <- adf_event_window

for (i in 1:length(event_dates)) for (j in Countries[!Countries == "HU"]) {
  adf_event_window[i, j] <- adf.test(agg_pivot %>% filter(DateforEvent ==  event_dates[i] & Country == j) %>% pull(Return))[["p.value"]]
  adf_event_window[i, j] <- round(adf_event_window[i, j], 2)
  pp_event_window[i, j] <- round(pp.test(agg_pivot %>% filter(DateforEvent ==  event_dates[i] & Country == j) %>% pull(Return))[["p.value"]], 2)
}

for (i in 1:length(event_dates)) {
  adf_event_window[i, "AllCurrencies"] <- adf.test(agg_pivot %>% filter(DateforEvent ==  event_dates[i]) %>% pull(Return))[["p.value"]]
  adf_event_window[i, "AllCurrencies"] <- round(adf_event_window[i, "AllCurrencies"], 2)
  pp_event_window[i, "AllCurrencies"] <- round(pp.test(agg_pivot %>% filter(DateforEvent ==  event_dates[i]) %>% pull(Return))[["p.value"]], 2)
}

for (j in Countries[!Countries == "HU"]) {
  adf_event_window[18, j] <- adf.test(agg_pivot %>% filter(Country == j) %>% pull(Return))[["p.value"]]
  adf_event_window[18, j] <- round(adf_event_window[18, j], 2)
  pp_event_window[18, j] <- round(pp.test(agg_pivot %>% filter(Country == j) %>% pull(Return))[["p.value"]], 2)
}

adf_event_window[18, "AllCurrencies"] <- adf.test(agg_pivot %>% pull(Return))[["p.value"]]
adf_event_window[18, "AllCurrencies"] <- round(adf_event_window[18, "AllCurrencies"], 2)
pp_event_window[18, "AllCurrencies"] <- round(pp.test(agg_pivot %>% pull(Return))[["p.value"]], 4)

write_csv(adf_event_window, paste0(tables, "adf_event_window.csv"))
write_csv(pp_event_window, paste0(tables, "pp_event_window.csv"))


# V. Section - Abnormal Returns =====
# - 1. Calculating Abnormal Returns -----
for (i in Countries) for (j in Models) {
  daily_event[[i]][paste0("AR_", j)] <- daily_event[[i]]$Return - daily_event[[i]][j]
}
# paste0("AR_", Models)

# Calculating (Mean and) Variance
for (i in Countries) for (j in Models) {
  # daily_event[[i]][paste0("A_", j)] <- mean(daily_event[[i]] %>% filter(Estimation == 1) %>% pull(paste0("AR_", j)))
  daily_event[[i]][paste0("V_", j)] <- sd(daily_event[[i]] %>% filter(Estimation == 1) %>% pull(paste0("AR_", j)))
  # daily_event[[i]][paste0("N_", j)] <- rnorm(nrow(daily_event[[i]]), mean = daily_event[[i]][1, paste0("A_", j)], sd = daily_event[[i]][1, paste0("V_", j)])
}

# Pivot table for abnormal returns
ar_pivot <- data.frame()
for (i in Countries[!Countries == "HU"]) {
  ar_pivot <- rbind(ar_pivot,
                    cbind(as.data.frame(matrix(data = i, nrow = nrow(daily_event[[i]]))), daily_event[[i]]))
}

# Sample wide deviations
deviations <- data.frame()
deviations <- ar_pivot %>% 
  filter(Estimation == 1) %>% 
  select(all_of(Models)) %>% 
  summarise(across(everything(), \(x) sd(x)))
deviations[2, ] <- ar_pivot %>% 
  filter(Estimation == 1) %>% 
  select(all_of(Models)) %>% 
  summarise(across(everything(), \(x) mad(x)))
rownames(deviations) <- c("sd", "mad")
deviations <- as.data.frame(t(deviations))

# Robustness pivot
ar_pivot_without_inference <- ar_pivot %>% 
  rename(Country = "V1") %>% 
  filter(Windows == 1) %>% 
  filter(is.na(Inference)) %>% 
  select(Country, Days, paste0("AR_", Models)) %>% 
  filter(Days != "-4") %>% 
  pivot_longer(cols = -c("Country", "Days"), names_to = "Model", values_to = "AR")

# Pivot
ar_pivot <- ar_pivot %>% 
  rename(Country = "V1") %>% 
  filter(Windows == 1) %>% 
  select(Country, Days, paste0("AR_", Models)) %>% 
  filter(Days != "-4") %>% 
  pivot_longer(cols = -c("Country", "Days"), names_to = "Model", values_to = "AR")


# - 2. Plots of Distributions -----
plot_data <- ar_pivot %>%
  select(Country, Days, Model, AR) %>%
  mutate(Days = as.factor(Days)) %>% 
  rename(AR_Values = "AR") %>%
  mutate(Model = case_when(
    Model == "AR_RW" ~ "Random Walk",
    Model == "AR_MeA" ~ "Mean-adjusted",
    Model == "AR_MaA" ~ "Market-adjusted",
    Model == "AR_MM" ~ "Market Model")) %>% 
  mutate(Country = case_when(
    Country == "CH" ~ "CHF",
    Country == "CZ" ~ "CZK",
    Country == "EU" ~ "EUR",
    Country == "GB" ~ "GBP",
    Country == "PL" ~ "PLN",
    Country == "US" ~ "USD"))

# -- 2.a figure_V_density -----
plot <- ggplot(data = plot_data) +
  geom_histogram(aes(x = AR_Values, y = ..density..), bins = 30, color = "#3B99B1", fill = "#3B99B1", alpha = 0.75, linewidth = 0.25) +
  geom_boxplot(aes(x = AR_Values, y = -0.02), color = "#3B99B1", fill = "#3B99B1", alpha = 0.5, width = 0.015, linewidth = 0.25, outlier.shape = NA) +
  
  labs(x = "Abnormal Returns",
       y = "Density") +

  stat_function(fun = dnorm,
                args = list(mean = mean(plot_data$AR_Values), sd = sd(plot_data$AR_Values)),
                color = "#F5191C", linetype = "dashed") +
  
  facet_wrap(~ factor(Model, c("Random Walk", "Mean-adjusted", "Market-adjusted", "Market Model")),
             nrow = 1) +
  
  fx_theme()

ggsave(filename = paste0(plots, "figure_V_density.pdf"), plot = plot, width = 160, height = 55, units = "mm", dpi = 300)

# -- 2.b figure_V_currency -----
plot <- ggplot(data = plot_data, aes(color = Country, fill = Country)) +
  geom_histogram(aes(x = AR_Values, y = ..density..), bins = 30, alpha = 0.75, linewidth = 0.25) +
  geom_boxplot(aes(x = AR_Values, y = -0.02), alpha = 0.5, width = 0.015, linewidth = 0.25, outlier.shape = NA) +
  
  labs(x = "Abnormal Returns",
       y = "Density") +
  
  scale_color_manual(values = palettes[[length(Countries) - 1]],
                     guide = "none") +
  scale_fill_manual(values = palettes[[length(Countries) - 1]],
                     guide = "none") +
  
  stat_function(fun = dnorm,
                args = list(mean = mean(plot_data$AR_Values), sd = sd(plot_data$AR_Values)),
                color = "#F5191C", linetype = "dashed") +
  
  facet_grid(Country ~ factor(Model, c("Random Walk", "Mean-adjusted", "Market-adjusted", "Market Model"))) +
  
  fx_theme()

ggsave(filename = paste0(plots, "figure_V_currency.pdf"), plot = plot, width = 160, height = 237, units = "mm", dpi = 300)

# -- 2.c figure_V_days -----
plot <- ggplot(data = plot_data, aes(color = Days, fill = Days)) +
  geom_histogram(aes(x = AR_Values, y = ..density..), bins = 30, alpha = 0.75, linewidth = 0.25) +
  geom_boxplot(aes(x = AR_Values, y = -0.02), alpha = 0.5, width = 0.015, linewidth = 0.25, outlier.shape = NA) +
  
  labs(x = "Abnormal Returns",
       y = "Density") +
  
  scale_color_manual(values = palettes[[8]],
                     guide = "none") +
  scale_fill_manual(values = palettes[[8]],
                    guide = "none") +
  
  stat_function(fun = dnorm,
                args = list(mean = mean(plot_data$AR_Values), sd = sd(plot_data$AR_Values)),
                color = "#F5191C", linetype = "dashed") +
  
  facet_grid(factor(Days, c("-3", "-2", "-1", "0", "1", "2", "3", "4")) ~ 
                      factor(Model, c("Random Walk", "Mean-adjusted", "Market-adjusted", "Market Model"))) +
  
  fx_theme()

ggsave(filename = paste0(plots, "figure_V_days.pdf"), plot = plot, width = 158, height = 237, units = "mm", dpi = 300)

# - 3. Descriptive statistics tables -----
# Group: Model
statistics <- describeBy(ar_pivot$AR, group = ar_pivot$Model, mat = TRUE, digits = 2, type = 1)
statistics <- statistics %>% 
  select(-c("item", "vars", "trimmed", "mad", "se")) %>% 
  mutate(jarque = NA, shapiro = NA, ks = NA)

for (i in paste0("AR_", Models)) {
  statistics[which(statistics$group1 == i), "jarque"] <- round(jarque.test(ar_pivot %>% filter(Model == i) %>% pull(AR))[["p.value"]], 2)
  statistics[which(statistics$group1 == i), "shapiro"] <- round(shapiro.test(ar_pivot %>% filter(Model == i) %>% pull(AR))[["p.value"]], 2)
  statistics[which(statistics$group1 == i), "ks"] <- round(ks.test(ar_pivot %>% filter(Model == i) %>% pull(AR), "pnorm")[["p.value"]], 2)
}

write_csv(statistics, paste0(tables, "statistics_G_model.csv"))

# Group: Model:Country
statistics <- describeBy(ar_pivot$AR, group = list(ar_pivot$Country, ar_pivot$Model), mat = TRUE, digits = 2, type = 1)
statistics <- statistics %>% 
  select(-c("item", "vars", "trimmed", "mad", "se")) %>% 
  mutate(jarque = NA, shapiro = NA, ks = NA)

for (i in paste0("AR_", Models)) for (j in Countries[!Countries == "HU"]) {
  statistics[which(statistics$group2 == i & statistics$group1 == j), "jarque"] <- round(jarque.test(ar_pivot %>% filter(Model == i & Country == j) %>% pull(AR))[["p.value"]], 2)
  statistics[which(statistics$group2 == i & statistics$group1 == j), "shapiro"] <- round(shapiro.test(ar_pivot %>% filter(Model == i & Country == j) %>% pull(AR))[["p.value"]], 2)
  statistics[which(statistics$group2 == i & statistics$group1 == j), "ks"] <- round(ks.test(ar_pivot %>% filter(Model == i & Country == j) %>% pull(AR), "pnorm")[["p.value"]], 2)
}

write_csv(statistics, paste0(tables, "statistics_G_country.csv"))

# Group: Model:Days
statistics <- describeBy(ar_pivot$AR, group = list(ar_pivot$Days, ar_pivot$Model), mat = TRUE, digits = 2, type = 1)
statistics <- statistics %>% 
  select(-c("item", "vars", "trimmed", "mad", "se")) %>% 
  mutate(jarque = NA, shapiro = NA, ks = NA)

for (i in paste0("AR_", Models)) for (j in -3:4) {
  statistics[which(statistics$group2 == i & statistics$group1 == j), "jarque"] <- round(jarque.test(ar_pivot %>% filter(Model == i & Days == j) %>% pull(AR))[["p.value"]], 2)
  statistics[which(statistics$group2 == i & statistics$group1 == j), "shapiro"] <- round(shapiro.test(ar_pivot %>% filter(Model == i & Days == j) %>% pull(AR))[["p.value"]], 2)
  statistics[which(statistics$group2 == i & statistics$group1 == j), "ks"] <- round(ks.test(ar_pivot %>% filter(Model == i & Days == j) %>% pull(AR), "pnorm")[["p.value"]], 2)
}

write_csv(statistics, paste0(tables, "statistics_G_days.csv"))


# - 4. Measuring abnormal returns -----
# -- 4.a Tables of Mean and Median values -----
statistics <- as.data.frame(matrix(data = 0, nrow = 8, ncol = 11))
colnames(statistics) <- c("f", "Model", seq(-3, 4, 1), "Average")
statistics$Model <- c(rep(Models, each = 2))
statistics$f <- rep(c("m", "d"), times = 4)

# Mean with Standard Deviation
for (i in Models) for (j in -3:4) {
  statistics[which(statistics$f == "m" & statistics$Model == i), as.character(j)] <- mean(ar_pivot %>% filter(Model == paste0("AR_", i) & Days == j) %>% pull(AR))
  statistics[which(statistics$f == "d" & statistics$Model == i), as.character(j)] <- sd(ar_pivot %>% filter(Model == paste0("AR_", i) & Days == j) %>% pull(AR))
  
  statistics[which(statistics$f == "m" & statistics$Model == i), "Average"] <- mean(ar_pivot %>% filter(Model == paste0("AR_", i)) %>% pull(AR))
  statistics[which(statistics$f == "d" & statistics$Model == i), "Average"] <- sd(ar_pivot %>% filter(Model == paste0("AR_", i)) %>% pull(AR))
}

statistics[, 3:11] <- round(statistics[, 3:11], 2)
statistics[seq(2, nrow(statistics), by = 2), 3:11] <- lapply(statistics[seq(2, nrow(statistics), by = 2), 3:11], function(x) paste0("(", x, ")"))
statistics[, 3:11] <- lapply(statistics[, 3:11], function(x) paste0("'", x))

write_csv(statistics, paste0(tables, "statistics_AR_mean.csv"))

statistics[, 3:11] <- 0
# Median with Median Absolute Deviation
for (i in Models) for (j in -3:4) {
  statistics[which(statistics$f == "m" & statistics$Model == i), as.character(j)] <- median(ar_pivot %>% filter(Model == paste0("AR_", i) & Days == j) %>% pull(AR))
  statistics[which(statistics$f == "d" & statistics$Model == i), as.character(j)] <- mad(ar_pivot %>% filter(Model == paste0("AR_", i) & Days == j) %>% pull(AR))
  
  statistics[which(statistics$f == "m" & statistics$Model == i), "Average"] <- median(ar_pivot %>% filter(Model == paste0("AR_", i)) %>% pull(AR))
  statistics[which(statistics$f == "d" & statistics$Model == i), "Average"] <- mad(ar_pivot %>% filter(Model == paste0("AR_", i)) %>% pull(AR))
}

statistics[, 3:11] <- round(statistics[, 3:11], 2)
statistics[seq(2, nrow(statistics), by = 2), 3:11] <- lapply(statistics[seq(2, nrow(statistics), by = 2), 3:11], function(x) paste0("(", x, ")"))
statistics[, 3:11] <- lapply(statistics[, 3:11], function(x) paste0("'", x))

write_csv(statistics, paste0(tables, "statistics_AR_median.csv"))


# -- 4.b Plots of Mean and Median values -----
# Plot for Student's t-test
plot_data <- ar_pivot %>%
  select(-Country) %>% 
  group_by(Model, Days) %>%
  summarise(mean = mean(AR)) %>% 
  mutate(sd = deviations[substr(Model, 4, nchar(Model)), "sd"])

plot <- ggplot(data = plot_data %>% filter(Model == "AR_MM")) +
  geom_line(aes(x = Days, y = mean), linewidth = 1.5, color = "#F5191C") +
  geom_ribbon(aes(x = Days, ymin = mean-sd, ymax = mean+sd), alpha = 0.05, color = "#F5191C", fill = "#F5191C", linetype = "dashed") +
  geom_hline(yintercept = 0, color = "#262A32", linetype = "solid", linewidth = 0.25) +
  geom_vline(xintercept = 0, color = "#262A32", linetype = "longdash", linewidth = 0.25) +
  
  labs(x = "Event Days",
       y = "Mean Return") +
  
  scale_x_continuous(expand = expansion(0),
                     breaks = seq(-3, 4, by = 1)) + 
  scale_y_continuous(breaks = seq(1, -1, by = -0.1),
                     minor_breaks = seq(1, -1, by = -0.05)) +

  fx_theme()

ggsave(filename = paste0(plots, "figure_V_student.pdf"), plot = plot, width = 150, height = 65, units = "mm", dpi = 300)

# Plot for Wilcoxon T-test
plot_data <- ar_pivot %>%
  select(-Country) %>% 
  group_by(Model, Days) %>%
  summarise(mean = median(AR)) %>% 
  mutate(sd = deviations[substr(Model, 4, nchar(Model)), "mad"]) %>% 
  mutate(Model = substr(Model, 4, nchar(Model))) %>% 
  mutate(Model = as.factor(Model))

plot <- ggplot(data = plot_data) +
  geom_line(aes(x = Days, y = mean, color = Model), linewidth = 1) +
  geom_ribbon(aes(x = Days, ymin = mean-sd, ymax = mean+sd, color = Model, fill = Model), alpha = 0.05, linetype = "dashed") +
  geom_hline(yintercept = 0, color = "#262A32", linetype = "solid", linewidth = 0.25) +
  geom_vline(xintercept = 0, color = "#262A32", linetype = "longdash", linewidth = 0.25) +
  
  labs(x = "Event Days",
       y = "Mean Return") +
  
  scale_x_continuous(expand = expansion(0),
                     breaks = seq(-3, 4, by = 1)) + 
  scale_y_continuous(limits = c(-2.75, 0.25),
                     breaks = seq(1, -5, by = -0.25),
                     minor_breaks = seq(1, -5, by = -0.25/2)) +
  
  scale_color_manual(values = palettes[[length(Models)]]) +
  
  guides(color = guide_legend(label.position = "bottom")) +
  
  fx_theme()

ggsave(filename = paste0(plots, "figure_V_wilcoxon.pdf"), plot = plot, width = 158, height = 80, units = "mm", dpi = 300)



# -- 4.c Abnormality Tests -----
# Student's t-test results
statistics <- as.data.frame(matrix(data = 0, nrow = 1, ncol = 13))
colnames(statistics) <- c("Model", seq(-3, 4, 1), "Average", "pre_event", "post_event", "paired")
statistics$Model <- "MM"

for (j in -3:4) {
  statistics[which(statistics$Model == "MM"), as.character(j)] <- t.test(ar_pivot %>% filter(Model == "AR_MM" & Days == j) %>% pull(AR))[["p.value"]]
}
statistics[which(statistics$Model == "MM"), "Average"] <- t.test(ar_pivot %>% filter(Model == "AR_MM") %>% pull(AR))[["p.value"]]
statistics[which(statistics$Model == "MM"), "pre_event"] <- t.test(ar_pivot %>% filter(Model == "AR_MM" & Days %in% -3:0) %>% pull(AR))[["p.value"]]
statistics[which(statistics$Model == "MM"), "post_event"] <- t.test(ar_pivot %>% filter(Model == "AR_MM" & Days %in% 1:4) %>% pull(AR))[["p.value"]]
statistics[which(statistics$Model == "MM"), "paired"] <- t.test(ar_pivot %>% filter(Model == "AR_MM" & Days %in% -3:0) %>% pull(AR),
                                                                ar_pivot %>% filter(Model == "AR_MM" & Days %in% 1:4) %>% pull(AR), paired = TRUE)[["p.value"]]

statistics[, 2:13] <- round(statistics[, 2:13], 3)

write_csv(statistics, paste0(tables, "statistics_AR_student.csv"))

# Wilcoxon T-test results
statistics <- as.data.frame(matrix(data = 0, nrow = 4, ncol = 13))
colnames(statistics) <- c("Model", seq(-3, 4, 1), "Average", "pre_event", "post_event", "paired")
statistics$Model <- Models


for (i in Models) for (j in -3:4) {
  statistics[which(statistics$Model == i), as.character(j)] <- wilcox.test(ar_pivot %>% filter(Model == paste0("AR_", i) & Days == j) %>% pull(AR))[["p.value"]]
  
  statistics[which(statistics$Model == i), "Average"] <- wilcox.test(ar_pivot %>% filter(Model == paste0("AR_", i)) %>% pull(AR))[["p.value"]]
  statistics[which(statistics$Model == i), "pre_event"] <- wilcox.test(ar_pivot %>% filter(Model == paste0("AR_", i) & Days %in% -3:0) %>% pull(AR))[["p.value"]]
  statistics[which(statistics$Model == i), "post_event"] <- wilcox.test(ar_pivot %>% filter(Model == paste0("AR_", i) & Days %in% 1:4) %>% pull(AR))[["p.value"]]
  statistics[which(statistics$Model == i), "paired"] <- wilcox.test(ar_pivot %>% filter(Model == paste0("AR_", i) & Days %in% -3:0) %>% pull(AR),
                                                                    ar_pivot %>% filter(Model == paste0("AR_", i) & Days %in% 1:4) %>% pull(AR), paired = TRUE)[["p.value"]]
  
}

statistics[, 2:13] <- round(statistics[, 2:13], 3)

write_csv(statistics, paste0(tables, "statistics_AR_wilcoxon.csv"))

# -- 4.d Robustness Tests -----
# Stats
statistics <- as.data.frame(matrix(data = 0, nrow = 8, ncol = 11))
colnames(statistics) <- c("f", "Model", seq(-3, 4, 1), "Average")
statistics$Model <- c(rep(Models, each = 2))
statistics$f <- rep(c("m", "d"), times = 4)

# Mean with Standard Deviation
for (i in Models) for (j in -3:4) {
  statistics[which(statistics$f == "m" & statistics$Model == i), as.character(j)] <- mean(ar_pivot_without_inference %>% filter(Model == paste0("AR_", i) & Days == j) %>% pull(AR))
  statistics[which(statistics$f == "d" & statistics$Model == i), as.character(j)] <- sd(ar_pivot_without_inference %>% filter(Model == paste0("AR_", i) & Days == j) %>% pull(AR))
  
  statistics[which(statistics$f == "m" & statistics$Model == i), "Average"] <- mean(ar_pivot_without_inference %>% filter(Model == paste0("AR_", i)) %>% pull(AR))
  statistics[which(statistics$f == "d" & statistics$Model == i), "Average"] <- sd(ar_pivot_without_inference %>% filter(Model == paste0("AR_", i)) %>% pull(AR))
}

statistics[, 3:11] <- round(statistics[, 3:11], 2)
statistics[seq(2, nrow(statistics), by = 2), 3:11] <- lapply(statistics[seq(2, nrow(statistics), by = 2), 3:11], function(x) paste0("(", x, ")"))
statistics[, 3:11] <- lapply(statistics[, 3:11], function(x) paste0("'", x))

write_csv(statistics, paste0(tables, "statistics_AR_robust_mean.csv"))

statistics[, 3:11] <- 0
# Median with Median Absolute Deviation
for (i in Models) for (j in -3:4) {
  statistics[which(statistics$f == "m" & statistics$Model == i), as.character(j)] <- median(ar_pivot_without_inference %>% filter(Model == paste0("AR_", i) & Days == j) %>% pull(AR))
  statistics[which(statistics$f == "d" & statistics$Model == i), as.character(j)] <- mad(ar_pivot_without_inference %>% filter(Model == paste0("AR_", i) & Days == j) %>% pull(AR))
  
  statistics[which(statistics$f == "m" & statistics$Model == i), "Average"] <- median(ar_pivot_without_inference %>% filter(Model == paste0("AR_", i)) %>% pull(AR))
  statistics[which(statistics$f == "d" & statistics$Model == i), "Average"] <- mad(ar_pivot_without_inference %>% filter(Model == paste0("AR_", i)) %>% pull(AR))
}

statistics[, 3:11] <- round(statistics[, 3:11], 2)
statistics[seq(2, nrow(statistics), by = 2), 3:11] <- lapply(statistics[seq(2, nrow(statistics), by = 2), 3:11], function(x) paste0("(", x, ")"))
statistics[, 3:11] <- lapply(statistics[, 3:11], function(x) paste0("'", x))

write_csv(statistics, paste0(tables, "statistics_AR_robust_median.csv"))

# Plots
plot_data <- ar_pivot_without_inference %>%
  select(-Country) %>% 
  group_by(Model, Days) %>%
  summarise(mean = mean(AR)) %>% 
  mutate(sd = deviations[substr(Model, 4, nchar(Model)), "sd"])

plot <- ggplot(data = plot_data %>% filter(Model == "AR_MM")) +
  geom_line(aes(x = Days, y = mean), linewidth = 1.5, color = "#F5191C") +
  geom_ribbon(aes(x = Days, ymin = mean-sd, ymax = mean+sd), alpha = 0.05, color = "#F5191C", fill = "#F5191C", linetype = "dashed") +
  geom_hline(yintercept = 0, color = "#262A32", linetype = "solid", linewidth = 0.25) +
  geom_vline(xintercept = 0, color = "#262A32", linetype = "longdash", linewidth = 0.25) +
  
  labs(x = "Event Days",
       y = NULL) +
  
  scale_x_continuous(expand = expansion(0),
                     breaks = seq(-3, 4, by = 1)) + 
  scale_y_continuous(limits = c(-2.75, 0.25),
                     breaks = seq(1, -5, by = -0.25),
                     minor_breaks = seq(1, -5, by = -0.25/2),
                     position = "right") +
  
  fx_theme()

ggsave(filename = paste0(plots, "figure_V_robust_right.pdf"), plot = plot, width = 65, height = 65, units = "mm", dpi = 300)

plot_data <- ar_pivot_without_inference %>%
  select(-Country) %>% 
  group_by(Model, Days) %>%
  summarise(mean = median(AR)) %>% 
  mutate(sd = deviations[substr(Model, 4, nchar(Model)), "mad"]) %>% 
  mutate(Model = substr(Model, 4, nchar(Model))) %>% 
  mutate(Model = as.factor(Model))

plot <- ggplot(data = plot_data) +
  geom_line(aes(x = Days, y = mean, color = Model), linewidth = 1) +
  geom_ribbon(aes(x = Days, ymin = mean-sd, ymax = mean+sd, color = Model, fill = Model), alpha = 0.05, linetype = "dashed") +
  geom_hline(yintercept = 0, color = "#262A32", linetype = "solid", linewidth = 0.25) +
  geom_vline(xintercept = 0, color = "#262A32", linetype = "longdash", linewidth = 0.25) +
  
  labs(x = "Event Days",
       y = "Mean Return") +
  
  scale_x_continuous(expand = expansion(0),
                     breaks = seq(-3, 4, by = 1)) + 
  scale_y_continuous(limits = c(-2.75, 0.25),
                     breaks = seq(1, -5, by = -0.25),
                     minor_breaks = seq(1, -5, by = -0.25/2)) +
  
  scale_color_manual(values = palettes[[length(Models)]]) +
  
  guides(color = guide_legend(label.position = "bottom")) +
  
  fx_theme()

ggsave(filename = paste0(plots, "figure_V_robust_left.pdf"), plot = plot, width = 93, height = 65, units = "mm", dpi = 300)

# Tests
# Student's t-test results for robustness
statistics <- as.data.frame(matrix(data = 0, nrow = 1, ncol = 13))
colnames(statistics) <- c("Model", seq(-3, 4, 1), "Average", "pre_event", "post_event", "paired")
statistics$Model <- "MM"

for (j in -3:4) {
  statistics[which(statistics$Model == "MM"), as.character(j)] <- t.test(ar_pivot_without_inference %>% filter(Model == "AR_MM" & Days == j) %>% pull(AR))[["p.value"]]
}
statistics[which(statistics$Model == "MM"), "Average"] <- t.test(ar_pivot_without_inference %>% filter(Model == "AR_MM") %>% pull(AR))[["p.value"]]
statistics[which(statistics$Model == "MM"), "pre_event"] <- t.test(ar_pivot_without_inference %>% filter(Model == "AR_MM" & Days %in% -3:0) %>% pull(AR))[["p.value"]]
statistics[which(statistics$Model == "MM"), "post_event"] <- t.test(ar_pivot_without_inference %>% filter(Model == "AR_MM" & Days %in% 1:4) %>% pull(AR))[["p.value"]]
statistics[which(statistics$Model == "MM"), "paired"] <- t.test(ar_pivot_without_inference %>% filter(Model == "AR_MM" & Days %in% -3:0) %>% pull(AR),
                                                                ar_pivot_without_inference %>% filter(Model == "AR_MM" & Days %in% 1:4) %>% pull(AR), paired = TRUE)[["p.value"]]

statistics[, 2:13] <- round(statistics[, 2:13], 3)

write_csv(statistics, paste0(tables, "statistics_AR_robust_student.csv"))

# Wilcoxon T-test results for robustness
statistics <- as.data.frame(matrix(data = 0, nrow = 4, ncol = 13))
colnames(statistics) <- c("Model", seq(-3, 4, 1), "Average", "pre_event", "post_event", "paired")
statistics$Model <- Models


for (i in Models) for (j in -3:4) {
  statistics[which(statistics$Model == i), as.character(j)] <- wilcox.test(ar_pivot_without_inference %>% filter(Model == paste0("AR_", i) & Days == j) %>% pull(AR))[["p.value"]]
  
  statistics[which(statistics$Model == i), "Average"] <- wilcox.test(ar_pivot_without_inference %>% filter(Model == paste0("AR_", i)) %>% pull(AR))[["p.value"]]
  statistics[which(statistics$Model == i), "pre_event"] <- wilcox.test(ar_pivot_without_inference %>% filter(Model == paste0("AR_", i) & Days %in% -3:0) %>% pull(AR))[["p.value"]]
  statistics[which(statistics$Model == i), "post_event"] <- wilcox.test(ar_pivot_without_inference %>% filter(Model == paste0("AR_", i) & Days %in% 1:4) %>% pull(AR))[["p.value"]]
  statistics[which(statistics$Model == i), "paired"] <- wilcox.test(ar_pivot_without_inference %>% filter(Model == paste0("AR_", i) & Days %in% -3:0) %>% pull(AR),
                                                                    ar_pivot_without_inference %>% filter(Model == paste0("AR_", i) & Days %in% 1:4) %>% pull(AR), paired = TRUE)[["p.value"]]
  
}

statistics[, 2:13] <- round(statistics[, 2:13], 3)

write_csv(statistics, paste0(tables, "statistics_AR_robust_wilcoxon.csv"))



















# X. Section - End =====