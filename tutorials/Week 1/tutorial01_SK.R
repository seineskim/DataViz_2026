######################
# Data Viz  
# Tutorial 1:  
# Tidyverse and ggplot        
######################

# Remove objects
rm(list=ls())

# Detach all libraries
detachAllPackages <- function() {
    basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
    package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
    package.list <- setdiff(package.list, basic.packages)
    if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
    }
detachAllPackages()

# Load libraries
pkgTest <- function(pkg){
    new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
    if (length(new.pkg)) 
        install.packages(new.pkg,  dependencies = TRUE)
    sapply(pkg,  require,  character.only = TRUE)
    }

# Load any necessary packages
lapply(c("tidyverse", "ggplot2"),  pkgTest)

# Set working directory for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

#################
### In class demo
#################

# load data
AB_ZIM <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/DataViz_2026/refs/heads/main/datasets/AB_ZIM.csv")
# reduce data
AB_ZIM <- AB_ZIM |> select(Q1, Q101, Q102, Q94A, Q97, Q98)
# organize data
AB_ZIM <- AB_ZIM |> 
  # rename some columns
  rename(age = `Q1`, 
         gender = `Q101`,
         interview_lang = `Q102`,
         employed = `Q94A`,
         religion = `Q97`,
         party_vote = `Q98`) 

str(AB_ZIM)

# histogram example
pdf("AB_ZIM_hist1.pdf")
ggplot(data = AB_ZIM, aes(x=age)) + 
  geom_histogram() 
dev.off()

pdf("AB_ZIM_hist2.pdf")
ggplot(data = AB_ZIM, aes(x=age)) +
  geom_histogram() + 
  scale_y_continuous(breaks = c(20, 40, 60, 80, 100))
dev.off()

pdf("AB_ZIM_hist3.pdf")
ggplot(data = AB_ZIM, aes(x=age)) +
  geom_histogram() + 
  scale_y_continuous(breaks = c(20, 40, 60, 80, 100)) +
  facet_wrap(vars(gender))
dev.off()

pdf("AB_ZIM_hist4.pdf")
ggplot(data = AB_ZIM, aes(x=age, fill=gender)) +
  geom_histogram() + 
  scale_y_continuous(breaks = c(20, 40, 60, 80, 100)) +
  facet_wrap(vars(gender)) +
  coord_flip()
dev.off()

pdf("AB_ZIM_hist5.pdf")
ggplot(data = AB_ZIM, aes(x=age, fill=gender)) +
  geom_histogram() + 
  scale_y_continuous(breaks = c(20, 40, 60, 80, 100)) +
  facet_wrap(vars(gender)) +
  coord_flip() +
  labs(x="\nAge", y="\nCount", fill="Gender")
dev.off()

pdf("AB_ZIM_hist6.pdf")
ggplot(data = AB_ZIM, aes(x=age, fill=gender)) +
  geom_histogram() + 
  scale_y_continuous(breaks = c(20, 40, 60, 80, 100)) +
  facet_wrap(vars(gender)) +
  coord_flip() +
  labs(x="\nAge", y="\nCount", fill="Gender") +
  theme_bw()
dev.off()

##############
### Group work
##############

# (1) Organize data yourself in groups using tidy
# (2) Create informative plots of example RQs
# (3) Start to add basic elements using ggplot

# Research questions: 
# What is the relationship between social demographic characteristics (education, employment, age, gender) & informal politics (official political parties vs traditional leaders)?

# download data from AfroBarometer (Malawi R10): https://www.afrobarometer.org/survey-resource/malawi-round-10-data-2024/
# here is the codebook: https://www.afrobarometer.org/survey-resource/malawi-round-10-codebook-2024/
AB_MALAWI <- read.csv("t2_data_SK.csv")
View(df)

# reduce your data to these variables: 
# URBRUR - urban/rural respondent
# Q1 - age 
# Q101 - gender
# Q102 - interview language
# Q94A - employed
# Q97 - religion
# Q98 - voted for party in last election
# Q12B - contacted party official
# Q12C - contacted traditional leader

# rename your variables to informative/easy names
AB_MALAWI <- AB_MALAWI |> select(URBRUR, Q1, Q101, Q102, Q94A, Q97, Q98, Q12B, Q12C)
AB_MALAWI <- AB_MALAWI |> 
rename(Rural_Urban = `URBRUR`,
       age = `Q1`, 
       gender = `Q101`,
       interview_lang = `Q102`,
       employed = `Q94A`,
       religion = `Q97`,
       party_vote = `Q98`,
       party_off = `Q12B`,
       trad_off = `Q12C`
       ) 

str(AB_MALAWI)

AB_MALAWI$age <- as.integer(AB_MALAWI$age)

# create a couple of visualizations that shed light on our RQ

# Plot 1: Party vote × Contact with party official
# 설명: 정당에 투표한 경험과 정당 관계자 접촉 여부의 관계를 비율로 비교
ggplot(AB_MALAWI, aes(x = party_vote, fill = party_off)) +
  geom_bar(position = "fill") +
  labs(x = "Voted for party", y = "Proportion", fill = "Contacted party official") +
  theme_minimal()

# Plot 2: Party vote × Contact with traditional leader (by gender)
# 설명: 정당 투표 여부와 전통 지도자 접촉의 관계가 성별에 따라 다른지 확인
ggplot(AB_MALAWI, aes(x = party_vote, fill = trad_off)) +
  geom_bar(position = "fill") +
  facet_wrap(vars(gender)) +
  labs(x = "Voted for party",
       y = "Proportion",
       fill = "Contacted traditional leader") +
  theme_

# Plot 3: Employment status × Contact with party official
# 설명: 고용 여부에 따라 정당 관계자 접촉 비율이 어떻게 다른지 비교
ggplot(AB_MALAWI, aes(x = employed, fill = party_off)) +
  geom_bar(position = "fill") +
  coord_flip() +
  labs(x = "Employed",
       y = "Proportion",
       fill = "Contacted party official")

# Plot 4: Age distribution by contact with traditional leader
# 설명: 전통 지도자 접촉 여부에 따른 응답자의 연령 분포 비교
ggplot(AB_MALAWI, aes(x = age, fill = trad_off)) +
  geom_histogram() +
  labs(x = "Age",
       y = "Count",
       fill = "Contacted traditional leader")

# Plot 5: Rural / Urban × Contact with traditional leader
# 설명: 거주 지역(도시/농촌)에 따라 전통 지도자 접촉 비율이 다른지 확인
ggplot(AB_MALAWI, aes(x = Rural_Urban, fill = trad_off)) +
  geom_bar(position = "fill") +
  theme_bw() +
  labs(x = "Rural / Urban",
       y = "Proportion",
       fill = "Contacted traditional leader")

# =========================================================
# Plot A: "Who do people contact more?" (Party vs Traditional) - Overall comparison
# I compare overall contact patterns: party officials versus traditional leaders.
# =========================================================
AB_long <- AB_MALAWI |>
  select(party_off, trad_off) |>
  pivot_longer(cols = c(party_off, trad_off),
               names_to = "contact_type", values_to = "response")

ggplot(AB_long, aes(x = contact_type, fill = response)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Contact type (party_off vs trad_off)", y = "Proportion", fill = "Response") +
  theme_minimal()

# =========================================================
# Plot C: Rural/Urban × Traditional leader contact (by employment) using facets
#I split the rural/urban comparison by employment status using facets.
# =========================================================
ggplot(AB_MALAWI, aes(x = Rural_Urban, fill = trad_off)) +
  geom_bar(position = "fill") +
  facet_wrap(vars(employed)) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Rural / Urban", y = "Proportion", fill = "Contacted traditional leader") +
  theme_minimal()

# (we will present your "findings" to the class)
