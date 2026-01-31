######################
# Data Visualization for Social Scientists
# Problem Set 2 (PS02)
# Author: Sein Kim
######################

#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())

# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
lapply(c("tidyverse", "ggplot2", "readr", "ggridges"),  pkgTest)

# set working directory to current parent folder
if (rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}

#####################
# Data Manipulation
#####################

### 1

# Load the NCSS .csv file
ncss_data <- read_csv("NCSS_v1.csv")

# Use select() to keep specific variables
df <- ncss_data %>%
  select(CASEID, YEAR, GDREGION, NUMOFFMBR, TRAD6, TRAD12, INCOME)

### 2

# Filter for Christian, Jewish, and Muslim congregations
# Note: Using unicode \u00E9 for 'e' with accent to prevent LaTeX errors
df_filtered <- df %>%
  filter(TRAD6 %in% c("Chr\u00E9tiennes", "Juives", "Musulmanes")) ## LaTeX encoding fix

### 3

# Compute number of congregations by religious classification and year
cong_count <- df_filtered %>%
  count(YEAR, TRAD6)

print(cong_count)

# Compute mean and median total income
income_stats <- df_filtered %>%
  group_by(YEAR, TRAD6) %>%
  summarise(
    mean_income = mean(INCOME, na.rm = TRUE),
    median_income = median(INCOME, na.rm = TRUE)
  )

print(income_stats)

### 4

# Create categorical variable AVG_INCOME
# First, calculate average income per year
df_aug <- df_filtered %>%
  group_by(YEAR) %>%
  mutate(yearly_avg = mean(INCOME, na.rm = TRUE)) %>%
  ungroup()

# Create binary variable: 1 if >= average, 0 if < average
df_aug$AVG_INCOME <- ifelse(df_aug$INCOME >= df_aug$yearly_avg, 1, 0)

# Convert to factor for better plotting
df_aug$AVG_INCOME <- factor(df_aug$AVG_INCOME, levels = c(0, 1), labels = c("Below Avg", "Above/Avg"))

#####################
# Data Visualization
#####################

### 1

# Bar plot: Proportion of congregations above/below avg income
prop_income <- df_aug %>%
  filter(!is.na(AVG_INCOME)) %>%
  group_by(YEAR, TRAD12, AVG_INCOME) %>%
  summarise(N = n()) %>%
  mutate(prop = N / sum(N))

pdf("Plot1_Income_Proportion.pdf", width = 10, height = 6)
ggplot(prop_income, aes(x = TRAD12, y = prop, fill = AVG_INCOME)) +
  geom_col(position = "fill") +
  facet_wrap(~YEAR) +
  labs(title = "Proportion of Income Levels by Religious Tradition",
       x = "12-level Religious Classification", y = "Proportion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()

### 2

# Histogram: Number of official members in 2022
df_2022 <- df_aug %>% filter(YEAR == 2022)

members_summary <- df_2022 %>%
  group_by(TRAD6, TRAD12) %>%
  summarise(total_members = sum(NUMOFFMBR, na.rm = TRUE))

pdf("Plot2_Members_Histogram.pdf", width = 10, height = 6)
ggplot(members_summary, aes(x = TRAD12, y = total_members, fill = TRAD6)) +
  geom_col(position = "dodge") +
  facet_wrap(~TRAD6, scales = "free_x") +
  labs(title = "Total Official Members by Tradition (2022)",
       x = "12-level Religious Classification", y = "Number of Members") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "none")
dev.off()

### 3

# Ridge plot: Distribution of yearly INCOME in 2022
pdf("Plot3_Income_Ridge.pdf", width = 8, height = 6)
ggplot(df_2022, aes(x = INCOME, y = GDREGION, fill = GDREGION)) +
  geom_density_ridges(alpha = 0.5) +
  labs(title = "Income Distribution by Region (2022)",
       x = "Total Income (Log Scale)", y = "Region") +
  scale_fill_brewer(palette="Set3") +
  theme_ridges() +
  theme(legend.position = "none")
dev.off()

### 4

# Boxplot: Number of official members per congregation (2022)
pdf("Plot4_Members_Boxplot.pdf", width = 12, height = 12)
ggplot(df_2022, aes(x = TRAD6, y = NUMOFFMBR, fill = TRAD6)) +
  geom_boxplot() +
  facet_wrap(~GDREGION) +
  scale_y_log10() + 
  labs(title = "Official Members per Congregation by Region (2022)",
       x = "Religious Classification", y = "Number of Members (Log Scale)") +
  scale_fill_brewer(palette="Pastel1") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()