#Load libraries
library(tidyverse)
library(haven)
library(naniar)
library(brms)
library(sf)

#For reproducibility
set.seed(1234)

#Load in data
Census <- read.csv("D:\\Datasets\\Census_Microdata.csv")
head(Census)
tibble(Census)

#Check for missing values
sum(is.na(Census))

#Inspect data
colnames(Census)
unique(Census$residence_type)

#Load in BES data
BES <- read_spss("D:\\Datasets\\bes_rps_2024_1.0.1.sav")
head(BES)

#Create a new BES dataframe with the dependent variable and demographic variables also in the Census microdata 
cleanBES <- subset(BES, select = c(finalserialno, h01, edlevel, Dwelling_type, y11, Age, y09, Region, y17, y26, y06))
sum(is.na(cleanBES))

#Remove NAs and rename 
cleanBES <- na.omit(cleanBES)
cleanBES <- rename(cleanBES, "ID" = finalserialno, "EnvironmentEconomy" = h01, "Ethnicity" = y11, "Sex" = y09, "Employment" = y17, "MaritalStatus" = y26, "Religion" = y06)

#Check response codes in both datasets and ensure they match
unique(Census$ethnic_group_tb_6a)
unique(cleanBES$Ethnicity)
cleanBES$Ethnicity <- case_when(cleanBES$Ethnicity == 1 ~ 4,
                                cleanBES$Ethnicity == 2 ~ 4,
                                cleanBES$Ethnicity == 3 ~ 4,
                                cleanBES$Ethnicity == 4 ~ 4,
                                cleanBES$Ethnicity == 5 ~ 3,
                                cleanBES$Ethnicity == 6 ~ 3,
                                cleanBES$Ethnicity == 7 ~ 3,
                                cleanBES$Ethnicity == 8 ~ 3,
                                cleanBES$Ethnicity == 9 ~ 1,
                                cleanBES$Ethnicity == 10 ~ 1,
                                cleanBES$Ethnicity == 11 ~ 1,
                                cleanBES$Ethnicity == 12 ~ 1,
                                cleanBES$Ethnicity == 13 ~ 1,
                                cleanBES$Ethnicity == 14 ~ 2,
                                cleanBES$Ethnicity == 15 ~ 2,
                                cleanBES$Ethnicity == 16 ~ 2,
                                cleanBES$Ethnicity == 17 ~ 5,
                                cleanBES$Ethnicity == 18 ~ 5,
                                cleanBES$Ethnicity == 19 ~ 5,
                                cleanBES$Ethnicity == -999 ~ -8,
                                cleanBES$Ethnicity == -2 ~ -8)

cleanBES$Age <- as.numeric(cleanBES$Age)

cleanBES$Age <- case_when(
  between(cleanBES$Age, 1, 15) ~ 1,
  between(cleanBES$Age, 16, 24) ~ 2,
  between(cleanBES$Age, 25, 34) ~ 3,
  between(cleanBES$Age, 35, 44) ~ 4,
  between(cleanBES$Age, 45, 54) ~ 5,
  between(cleanBES$Age, 55, 64) ~ 6,
  cleanBES$Age >= 65 ~ 7,
  cleanBES$Age == -2 ~ NA
)

cleanBES <- na.omit(cleanBES)

unique(cleanBES$Age)                              

unique(cleanBES$Sex)
unique(Census$sex)
sum(cleanBES$Sex == -999)

cleanBES$Sex <- case_when(
  cleanBES$Sex == -999 ~ -8,
  cleanBES$Sex == 1 ~ 1,
  cleanBES$Sex == 2 ~ 2,
  cleanBES$Sex == 3 ~ 1,
  cleanBES$Sex == 4 ~ 2
)

unique(cleanBES$Sex)

unique(cleanBES$Region)
unique(Census$region)

cleanBES$Region <- case_when(
  cleanBES$Region == 1  ~ 'E12000004',
  cleanBES$Region == 2  ~ 'E12000006',
  cleanBES$Region == 3  ~ 'E12000007',
  cleanBES$Region == 4  ~ 'E12000001',
  cleanBES$Region == 5  ~ 'E12000002',
  cleanBES$Region == 6  ~ 'S99999999',
  cleanBES$Region == 7  ~ 'E12000008',
  cleanBES$Region == 8  ~ 'E12000009',
  cleanBES$Region == 9  ~ 'W92000004',
  cleanBES$Region == 10 ~ 'E12000005',
  cleanBES$Region == 11 ~ 'E12000003',
)

cleanBES$y

unique(Census$religion_tb)
unique(cleanBES$Religion)

cleanBES$Religion <- case_when(
  cleanBES$Religion == -999 ~ -8,
  cleanBES$Religion == -2 ~ 9,
  cleanBES$Religion == 0 ~ -8,
  cleanBES$Religion == 1 ~ 2,
  cleanBES$Religion == 2 ~ 2,
  cleanBES$Religion == 3 ~ 2,
  cleanBES$Religion == 4 ~ 2,
  cleanBES$Religion == 5 ~ 2,
  cleanBES$Religion == 6 ~ 2,
  cleanBES$Religion == 7 ~ 2,
  cleanBES$Religion == 8 ~ 2,
  cleanBES$Religion == 9 ~ 2,
  cleanBES$Religion == 10 ~ 2,
  cleanBES$Religion == 11 ~ 2,
  cleanBES$Religion == 12 ~ 5,
  cleanBES$Religion == 13 ~ 4,
  cleanBES$Religion == 14 ~ 6,
  cleanBES$Religion == 15 ~ 7,
  cleanBES$Religion == 16 ~ 3,
  cleanBES$Religion == 17 ~ 8
)

cleanBES$Employment <- case_when(
  cleanBES$Employment == 1  ~ 1,   
  cleanBES$Employment == 2  ~ 2,   
  cleanBES$Employment == 3  ~ 1,   
  cleanBES$Employment == 4  ~ 2,   
  cleanBES$Employment == 5  ~ 3,   
  cleanBES$Employment == 6  ~ 9,   
  cleanBES$Employment == 7  ~ 6,   
  cleanBES$Employment == 8  ~ 7,   
  cleanBES$Employment == 9  ~ 9,   
  cleanBES$Employment == 10 ~ 8,   
  cleanBES$Employment == 11 ~ 5,   
  cleanBES$Employment == 12 ~ 9,
  cleanBES$Employment == -999 ~ -8 
)

unique(cleanBES$MaritalStatus)
unique(Census$y26)

cleanBES$MaritalStatus <- case_when(
  cleanBES$MaritalStatus == 1 ~ 2,   
  cleanBES$MaritalStatus == 2 ~ 1,   
  cleanBES$MaritalStatus == 3 ~ 1,   
  cleanBES$MaritalStatus == 4 ~ 5,   
  cleanBES$MaritalStatus == 5 ~ 3,   
  cleanBES$MaritalStatus == 6 ~ 4,   
  cleanBES$MaritalStatus == -999 ~ -8  
)

#Take a subset of the full census data so that only relevant variables are kept
cleanCensus <- subset(Census, select = c(resident_id_m, resident_age_7d, sex, ethnic_group_tb_6a, region, religion_tb, economic_activity_status_10m, legal_partnership_status_6a))

cleanCensus <- rename(cleanCensus, "ID" = resident_id_m, "Age" = resident_age_7d, "Sex" = sex, "Ethnicity" = ethnic_group_tb_6a, "Region" = region, "Religion" = religion_tb, "Employment" = economic_activity_status_10m, "MaritalStatus" = legal_partnership_status_6a)

#Ensure that age is numeric so it can be parsed correctly by the model
cleanCensus$Age <- as.numeric(cleanCensus$Age)

colnames(cleanBES)
colnames(cleanCensus)

colnames(Census)

unique(Census$residence_type)
unique(cleanBES$Dwelling_type)

cleanBES <- subset(cleanBES, select = -c(Dwelling_type, edlevel))

cleanBES <- as.data.frame(cleanBES)

#Ensure dependent variable is parsed as ordinal
cleanBES$EnvironmentEconomy <- ordered(as.factor(cleanBES$EnvironmentEconomy))

#Fit model to cleanBES
Model <- brm(formula = EnvironmentEconomy ~ Sex + Ethnicity + Religion + Employment + MaritalStatus + (1 | Region:Age) + (1 | Region),
              data = cleanBES,
              family = cumulative("logit"), #For ordinal data
              chains = 4,
              iter = 2000,
              cores = 4,
              backend = 'cmdstanr'
)

summary(Model)

pp_check(Model)

#Create post-stratification frame
Frame <- cleanCensus %>% 
  count(Sex, Ethnicity, Religion, Employment, MaritalStatus, Region, Age, name = "n") %>%
  as.data.frame()

nrow(cleanCensus)
nrow(Frame)

#Predict on the post-stratification frame
Poststrat <- posterior_epred(
  Model,
  newdata = Frame,
  ndraws = 1000,
  allow_new_levels = TRUE
)

#Use these predictions to apply a weighted mean across the 
nCategories <- dim(Poststrat)[3] #Find how many categories there are
categoryVals <- as.numeric(dimnames(Poststrat)[[3]]) #Find the numeric label of the category (likert scale score)

predScore <- apply(Poststrat, c(1, 2), function(p) sum(p * categoryVals)) #Multiply each category's probability by numeric value and sum them
#predScore contains a single score rather than a probability vector

#Convert to dpylr compatible object
predLong <- as.data.frame.table(predScore, responseName = "score")
names(predLong)[1:2] <- c("draw", "cell")
predLong <- predLong %>% mutate(cell = as.integer(cell))

#Find which region each cell corresponds to
cellSearch <- Frame %>% mutate(cell = row_number()) %>% select(cell, Region, n)

#Add the result to predLong
predLong <- predLong %>% left_join(cellSearch, by = "cell")

#Calculate a population-weighted average per region, per draw
regionDraws <- predLong %>% group_by(Region, draw) %>% summarise(WeightedScore = sum(score * n) / sum(n), .groups = "drop")
 
#Take a single number from the weighted average probability distribution, and construct 95% credible intervals
regionSummary <- regionDraws %>% group_by(Region) %>% summarise(
  mean = mean(WeightedScore),
  lower = quantile(WeightedScore, 0.025),
  upper = quantile(WeightedScore, 0.975),
  .groups = "drop"
)

#Read in geojson object and test that it works
SF <- read_sf("C:\\Users\\shark\\Downloads\\EER_Dec_2016_SGCB_GB_2022_-4221829217313930924.geojson")
ggplot(data = SF) + geom_sf(fill = "white", color = "black", linewidth = 0.3) + theme_void()

#Ensure that the region codes in the geojson object match up to the ones in the data
SF$eer16cd <- case_when(
  SF$eer16cd == "E15000001" ~ "E12000001",
  SF$eer16cd == "E15000002" ~ "E12000002",
  SF$eer16cd == "E15000003" ~ "E12000003",
  SF$eer16cd == "E15000004" ~ "E12000004",
  SF$eer16cd == "E15000005" ~ "E12000005",
  SF$eer16cd == "E15000006" ~ "E12000006",
  SF$eer16cd == "E15000007" ~ "E12000007",
  SF$eer16cd == "E15000008" ~ "E12000008",
  SF$eer16cd == "E15000009" ~ "E12000009",
  SF$eer16cd == "W08000001" ~ "W92000004",
)

#Join the geojson data to the post-stratification data
mapData <- SF %>% left_join(regionSummary, by = c("eer16cd" = "Region"))

sum(is.na(mapData$mean))
#1 NA - Scotland

#Remove NA (Scotland isn't included)
mapData <- na.omit(mapData)

#Create the plot
choropleth <- ggplot(data = mapData) + 
  geom_sf(aes(fill = mean), color = "black", linewidth = 0.3) + 
  scale_fill_viridis_c(name = "Mean score\n10 = Environmental protection\n0 = Economic growth") +
  labs(
    title = "Should economic growth or environmental protection take priority?",
    caption = "Regional MRP modelled on Census 2021 data and British Election Study 2024 data."
  ) +
  theme_void() +
  theme(text = element_text(family = "serif")) +
  theme(plot.title = element_text(hjust = 0.5, size = 16))

#Visualise the plot
choropleth






 
