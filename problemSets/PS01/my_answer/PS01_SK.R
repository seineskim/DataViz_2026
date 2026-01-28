######################
# Data Visualization for Social Scientists
# Problem Set 1 (PS01)
# Author: Sein Kim
######################

# Packages
library(tidyverse)
library(readr)
library(readxl)
library(ggplot2)

############################################################
# Q1. Load datasets into the global environment
############################################################
# Load EP1 roll-call votes (wide format)
rcv_ep1 <- read_csv("rcv_ep1.txt", show_col_types = FALSE)

# Load MEP-level information (EP1 sheet is sheet = 2)
mep_info <- read_excel("mep_info_26Jul11.xls", sheet = 2) %>%
  rename(MEPID = `MEP id`)

############################################################
# Q3. Identify ID/metadata vs vote columns + tidy wide -> long
#     + summary table of decision categories
############################################################
# Identify ID/metadata columns and vote decision columns
id_cols <- c("MEPID", "MEPNAME", "MS", "NP", "EPG")
vote_cols <- names(rcv_ep1) %>% str_subset("^V\\d+$")  # V1, V2, ..., Vn

# Check
print(setdiff(id_cols, names(rcv_ep1)))
print(length(vote_cols))                  

# Tidy voting data: wide -> long (one row = one MEP x one roll-call)
rcv_ep1_long <- rcv_ep1 %>%
  pivot_longer(
    cols = all_of(vote_cols),
    names_to = "rollcall_id",
    values_to = "vote_code"
  ) %>%
  mutate(
    decision = case_when(
      vote_code == 1 ~ "Yes",
      vote_code == 2 ~ "No",
      vote_code == 3 ~ "Abstain",
      vote_code == 4 ~ "Present but did not vote",
      vote_code == 0 ~ "Absent",
      vote_code == 5 ~ "Not an MEP",
      TRUE ~ "Other/Unknown"
    )
  )

# Summary table of decision categories across all votes
decision_counts <- rcv_ep1_long %>%
  count(decision, sort = TRUE)
print(decision_counts)

############################################################
# Q4. Merge MEP-level info with long-format votes + missingness
############################################################
# Merge MEP information with voting data (by MEPID)
rcv_ep1_merged <- rcv_ep1_long %>%
  left_join(mep_info, by = "MEPID")

# Check missingness after merge (key variables)
missing_summary <- rcv_ep1_merged %>%
  summarise(
    missing_nomd1 = sum(is.na(`NOM-D1`)),
    missing_nomd2 = sum(is.na(`NOM-D2`)),
    missing_epg   = sum(is.na(`EP Group`))
  )
print(missing_summary)

############################################################
# Q5. EP-group summaries:
#     - mean Yes rate (Yes/(Yes+No+Abstain))
#     - mean abstention rate
#     - mean NOM-D1 and NOM-D2
############################################################
# Convert "." to NA and cast NOMINATE dimensions to numeric
rcv_ep1_merged <- rcv_ep1_merged %>%
  mutate(
    `NOM-D1` = na_if(as.character(`NOM-D1`), "."),
    `NOM-D2` = na_if(as.character(`NOM-D2`), "."),
    `NOM-D1` = as.numeric(`NOM-D1`),
    `NOM-D2` = as.numeric(`NOM-D2`)
  )

ep_group_summary <- rcv_ep1_merged %>%
  filter(vote_code %in% c(1, 2, 3)) %>%   # valid votes: Yes/No/Abstain
  group_by(`EP Group`) %>%
  summarise(
    yes_rate = mean(vote_code == 1, na.rm = TRUE),
    abstention_rate = mean(vote_code == 3, na.rm = TRUE),
    mean_nomdim1 = mean(`NOM-D1`, na.rm = TRUE),
    mean_nomdim2 = mean(`NOM-D2`, na.rm = TRUE),
    .groups = "drop"
  )
print(ep_group_summary)

############################################################
# Data Visualization
############################################################

############################################################
# V1. Distribution of NOM-D1 by EP group (boxplot + jitter)
############################################################
mep_ep1 <- rcv_ep1_merged %>%
  distinct(MEPID, `EP Group`, `NOM-D1`, `NOM-D2`) %>%
  filter(!is.na(`NOM-D1`))

pdf("viz1_SK.pdf", width = 8, height = 5)
ggplot(mep_ep1, aes(x = `EP Group`, y = `NOM-D1`)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.4, size = 1) +
  labs(
    title = "Distribution of NOMINATE Dimension 1 by EP Group (EP1)",
    x = "EP Group",
    y = "NOM-D1"
  ) +
  theme_minimal()
dev.off()

############################################################
# V2. Scatterplot NOM-D1 vs NOM-D2, colored by EP group
############################################################
mep_ep1_scatter <- rcv_ep1_merged %>%
  distinct(MEPID, `EP Group`, `NOM-D1`, `NOM-D2`) %>%
  filter(!is.na(`NOM-D1`), !is.na(`NOM-D2`))

pdf("viz2_SK.pdf", width = 8, height = 5)
ggplot(mep_ep1_scatter, aes(x = `NOM-D1`, y = `NOM-D2`, color = `EP Group`)) +
  geom_point(alpha = 0.7, size = 1.5) +
  labs(
    title = "MEP Ideal Points in EP1 (NOM-D1 vs NOM-D2)",
    x = "NOM-D1",
    y = "NOM-D2",
    color = "EP Group"
  ) +
  theme_minimal()
dev.off()

############################################################
# V3. Cohesion: boxplot of MEP-level Yes share by EP group
############################################################
mep_yes_share <- rcv_ep1_merged %>%
  filter(vote_code %in% c(1, 2, 3)) %>%
  group_by(MEPID, `EP Group`) %>%
  summarise(
    yes_share = mean(vote_code == 1, na.rm = TRUE),
    n_votes = n(),
    .groups = "drop"
  )

pdf("viz3_SK.pdf", width = 8, height = 5)
ggplot(mep_yes_share, aes(x = `EP Group`, y = yes_share)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.4, size = 1) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "MEP Yes-vote Share by EP Group (EP1)",
    x = "EP Group",
    y = "Yes share (Yes / (Yes + No + Abstain))"
  ) +
  theme_minimal()
dev.off()

############################################################
# V4. Yes share per year by national party (bar plot)
############################################################
# Load roll-call metadata to obtain year information
vote_info <- read_excel("vote_info_Jun2010.xls", sheet = "EP1")

vote_info_ep1 <- vote_info %>%
  filter(`EP No.` == 1) %>%
  mutate(
    vote_no = as.integer(`Vote No. in RCV_EP1 file`),
    # Date is stored as Excel serial number (character)
    date = as.Date(as.numeric(Date), origin = "1899-12-30"),
    year = format(date, "%Y")
  ) %>%
  select(vote_no, year) %>%
  filter(!is.na(year))   # drop roll-calls with missing dates

# Merge year onto rcv long data
rcv_ep1_long_year <- rcv_ep1_long %>%
  mutate(vote_no = as.integer(sub("^V", "", rollcall_id))) %>%
  left_join(vote_info_ep1, by = "vote_no")

# Check remaining missing years
print(sum(is.na(rcv_ep1_long_year$year)))

# Year x NP yes share
np_year_yes <- rcv_ep1_long_year %>%
  filter(vote_code %in% c(1, 2, 3)) %>%
  filter(!is.na(year)) %>%
  group_by(year, NP) %>%
  summarise(yes_share = mean(vote_code == 1), .groups = "drop")

# Keep top 10 national parties for readability
top_np <- np_year_yes %>%
  count(NP, sort = TRUE) %>%
  slice_head(n = 10) %>%
  pull(NP)

np_year_yes_top <- np_year_yes %>%
  filter(NP %in% top_np)

pdf("viz4_SK.pdf", width = 11, height = 6)
ggplot(np_year_yes_top, aes(x = year, y = yes_share, fill = factor(NP))) +
  geom_col(position = "dodge") +
  labs(
    title = "Yes Share per Year by National Party (Top 10 NP, EP1)",
    x = "Year",
    y = "Yes share",
    fill = "NP"
  ) +
  theme_minimal()
dev.off()

############################################################
# V5. Average Yes share per year by EP group (line plot)
############################################################
# Merge year onto merged dataset (EP Group included)
rcv_ep1_merged_year <- rcv_ep1_merged %>%
  mutate(vote_no = as.integer(sub("^V", "", rollcall_id))) %>%
  left_join(vote_info_ep1, by = "vote_no")

epg_year_yes <- rcv_ep1_merged_year %>%
  filter(vote_code %in% c(1, 2, 3)) %>%
  filter(!is.na(year)) %>%
  group_by(year, `EP Group`) %>%
  summarise(avg_yes = mean(vote_code == 1), .groups = "drop")

pdf("viz5_SK.pdf", width = 8, height = 5)
ggplot(epg_year_yes, aes(x = year, y = avg_yes,
                         group = `EP Group`, color = `EP Group`)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Average Yes Share per Year by EP Group (EP1)",
    x = "Year",
    y = "Average Yes share",
    color = "EP Group"
  ) +
  theme_minimal()
dev.off()

print(head(epg_year_yes, 20))
