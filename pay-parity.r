
if(!require(tidyverse)) install.packages("tidyverse", repos = "https://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "https://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "https://cran.us.r-project.org") 
if(!require(tidyr)) install.packages("tidyr", repos = "https://cran.us.r-project.org") 
if(!require(ggthemes)) install.packages("ggthemes", repos = "https://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "https://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "https://cran.us.r-project.org")
if(!require(corrplot)) install.packages("corrplot", repos = "https://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")
if(!require(WDI)) install.packages("WDI", repos = "http://cran.us.r-project.org")
if(!require(ggmap)) install.packages("ggmap", repos = "http://cran.us.r-project.org")
# ======== Load Libraries ========
library(readxl)
library(dplyr)
library(ggplot2)
library(countrycode)
library(WDI)

# ======== 1. Load Gender Pay Parity Data ========
# Ensure WEF-GGR.xlsx is in your working directory
gender_gap_data <- read_excel("/home/psymeg/dev/r/pay-parity/WEF-GGR.xlsx", sheet = "Data")

# Filter for Wage equality indicator
payparity_data <- gender_gap_data %>%
  filter(Indicator == "Global Gender Gap Report: Wage equality between women and men for similar work") %>%
  select(country = `Economy Name`,
         iso3c = `Economy ISO3`,
         pay_parity = `2022`) %>%
  filter(!is.na(pay_parity))

# ======== 2. Load Life Expectancy Data from World Bank ========
# Fetch latest life expectancy data
lifeexp_raw <- WDI(indicator = "SP.DYN.LE00.IN", start = 2022, end = 2022)

# Clean life expectancy dataset
lifeexp_data <- lifeexp_raw %>%
  select(iso3c = iso2c,
         life_expectancy = SP.DYN.LE00.IN) %>%
  filter(!is.na(life_expectancy)) %>%
  mutate(iso3c = countrycode(iso3c, "iso2c", "iso3c"))

# ======== 3. Merge Datasets ========
merged_data <- left_join(payparity_data, lifeexp_data, by = "iso3c") %>%
  filter(!is.na(life_expectancy))

# Optional: Add region info (to colour points by region)
merged_data$region <- countrycode(merged_data$iso3c, "iso3c", "region")

# ======== 4. Plot ========
ggplot(merged_data, aes(x = pay_parity, y = life_expectancy)) +
  geom_point(aes(color = region), size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +
  theme_minimal(base_size = 14) +
  labs(title = "Pay Parity vs Life Expectancy (2022)",
       subtitle = "Data: WEF Global Gender Gap Report 2023 & World Bank WDI",
       x = "Pay Parity (0 = low, 1 = full parity)",
       y = "Life Expectancy (years)",
       color = "Region") +
  theme(legend.position = "bottom")

# Female labor force participation rate (2022)
femlabforce_raw <- WDI(indicator = "SL.TLF.CACT.FE.ZS", start = 2022, end = 2022)

femlabforce_data <- femlabforce_raw %>%
  select(iso3c = iso2c,
         female_labor_force = SL.TLF.CACT.FE.ZS) %>%
  filter(!is.na(female_labor_force)) %>%
  mutate(iso3c = countrycode(iso3c, "iso2c", "iso3c"))

# Merge with pay parity data
merged_data2 <- left_join(payparity_data, femlabforce_data, by = "iso3c") %>%
  filter(!is.na(female_labor_force))

ggplot(merged_data, aes(x = pay_parity, y = femlabforce_data)) +
  geom_point(aes(color = region), size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +
  theme_minimal(base_size = 14) +
  labs(title = "Pay Parity vs Female labor force participation rate (2022)",
       subtitle = "Data: WEF Global Gender Gap Report 2023 & World Bank WDI",
       x = "Pay Parity (0 = low, 1 = full parity)",
       y = "Female labor force participation rate",
       color = "Region") +
  theme(legend.position = "bottom")
