## Analysis of determinants of primary care quality 
## Jinyi Wang et al. 2024

rm(list = ls())

list.of.packages <- c("stringr","tibble","fingertipsR","httr","jsonlite","ordinal","effects","ggeffects","foreign","MASS","Hmisc","data.table","dplyr","plyr","RColorBrewer","ggplot2","corrplot","rgdal","rgeos","raster","maptools","scales","plotly","latticeExtra","maps","classInt","grid","pals","reshape2","cowplot","sandwich","msm","Cairo")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(stringr)
library(tibble)
library(jsonlite)
library(fingertipsR)
library(httr)
library(data.table)
library(dplyr)
library(plyr)
library(RColorBrewer)
library(ggplot2)
library(corrplot)
library(rgdal)
library(rgeos)
library(raster)
library(maptools)
library(scales)
library(plotly)
library(latticeExtra)
library(maps)
library(classInt)
library(grid)
library(pals)
library(reshape2)
library(cowplot)
library(sandwich)
library(msm)
library(Cairo)
library(foreign)
library(MASS)
library(Hmisc)
library(ordinal)
library(ggeffects)
library(effects)


# Define directories
path_project <- "C:/Users/sirsa/OneDrive/Documents/2024Wang" # active directory


# set working directory 
setwd(path_project) 

# CQC ratings from API
cqc.data <- read.csv("01_July_2024_Latest_ratings.csv") # read original data

# Extract most recent ratings in five domains and overall
cqc.overall <- cqc.data %>% filter(Location.Primary.Inspection.Category == "GP Practices") %>%
  filter(Location.ODS.Code != "") %>%
  filter(Service...Population.Group == "Overall") %>%
  filter(Domain == "Overall") %>%
  dplyr::select(Location.ODS.Code,Location.ONSPD.CCG.Code,Latest.Rating) %>%
  rename(Overall = Latest.Rating)

cqc.safe <- cqc.data %>% filter(Location.Primary.Inspection.Category == "GP Practices") %>%
  filter(Location.ODS.Code != "") %>%
  filter(Service...Population.Group == "Overall") %>%
  filter(Domain == "Safe") %>%
  dplyr::select(Location.ODS.Code,Latest.Rating) %>%
  rename(Safe = Latest.Rating)

cqc.effective <- cqc.data %>% filter(Location.Primary.Inspection.Category == "GP Practices") %>%
  filter(Location.ODS.Code != "") %>%
  filter(Service...Population.Group == "Overall") %>%
  filter(Domain == "Effective") %>%
  dplyr::select(Location.ODS.Code,Latest.Rating) %>%
  rename(Effective = Latest.Rating)

cqc.caring <- cqc.data %>% filter(Location.Primary.Inspection.Category == "GP Practices") %>%
  filter(Location.ODS.Code != "") %>%
  filter(Service...Population.Group == "Overall") %>%
  filter(Domain == "Caring") %>%
  dplyr::select(Location.ODS.Code,Latest.Rating) %>%
  rename(Caring = Latest.Rating)

cqc.responsive <- cqc.data %>% filter(Location.Primary.Inspection.Category == "GP Practices") %>%
  filter(Location.ODS.Code != "") %>%
  filter(Service...Population.Group == "Overall") %>%
  filter(Domain == "Responsive") %>%
  dplyr::select(Location.ODS.Code,Latest.Rating) %>%
  rename(Responsive = Latest.Rating)

cqc.well.led <- cqc.data %>% filter(Location.Primary.Inspection.Category == "GP Practices") %>%
  filter(Location.ODS.Code != "") %>%
  filter(Service...Population.Group == "Overall") %>%
  filter(Domain == "Well-led") %>%
  dplyr::select(Location.ODS.Code,Latest.Rating) %>%
  rename(Well.led = Latest.Rating)

# Merge into single dataset
cqc.all.data <- cqc.overall %>% 
  inner_join(cqc.caring, by="Location.ODS.Code") %>%
  inner_join(cqc.responsive, by="Location.ODS.Code") %>%
  inner_join(cqc.well.led, by="Location.ODS.Code") %>%
  inner_join(cqc.effective, by="Location.ODS.Code") %>%
  inner_join(cqc.safe, by="Location.ODS.Code") %>%
  rename(practice_code = "Location.ODS.Code") %>%
  rename(CCD19CD = "Location.ONSPD.CCG.Code")

# Identify distinct rows
cqc.all.data <- distinct(cqc.all.data) %>% # remove duplicate rows
  group_by(practice_code) %>%
  filter(n() == 1)

print(paste0("There are ", nrow(cqc.all.data), " practices with CQC ratings."))

# IMD
data_imd <- fingertips_data(IndicatorID = 93553, AreaTypeID = 7) %>% # IMD scores by GP practice
  # Keep IMD(2019) scores only
  filter(Timeperiod == "2019") %>% 
  # Remove England value
  filter(AreaType != "England") %>% 
  # Keep required fields only
  dplyr::select(AreaCode, AreaName, ParentName, Value) %>% 
  # Rename columns
  dplyr::rename("practice_code" = "AreaCode", 
                "gp.name" = "AreaName",
                "pcn.name" = "ParentName",
                "imd.score" = "Value") %>%
  # Keep required columns
  dplyr::select(practice_code, imd.score)

data.all <- merge(cqc.all.data,data_imd,by="practice_code") # add IMD score to data

print(paste0("There are ", nrow(data.all), " practices remaining after IMD scores were added"))

# List size - total

list_size.total <- read.csv("gp-reg-pat-prac-quin-age.csv") %>% 
  # Rename practice code column
  rename(practice_code = ORG_CODE)  %>%
  # Filter to keep GP practices only
  filter(ORG_TYPE == "GP") %>%
  # Filter total list size
  filter(AGE_GROUP_5 == "ALL") %>%
  filter(SEX == "ALL") %>%
  # Keep required columns
  dplyr::select(4,9) %>%
  # Rename
  rename(total.listsize = NUMBER_OF_PATIENTS)

# List size - females

list_size.females <- read.csv("gp-reg-pat-prac-quin-age.csv") %>% 
  # Rename practice code column
  rename(practice_code = ORG_CODE)  %>%
  # Filter to keep GP practices only
  filter(ORG_TYPE == "GP") %>%
  # Filter total list size
  filter(AGE_GROUP_5 == "ALL") %>%
  filter(SEX == "FEMALE") %>%
  # Keep required columns
  dplyr::select(4,9) %>%
  # Rename
  rename(total.female = NUMBER_OF_PATIENTS)

# List size - 65+ years

list_size.65plus <- read.csv("gp-reg-pat-prac-quin-age.csv") %>% 
  # Rename practice code column
  rename(practice_code = ORG_CODE)  %>%
  # Filter to keep GP practices only
  filter(ORG_TYPE == "GP") %>%
  # Filter total list size
  filter(AGE_GROUP_5 == "70_74" |AGE_GROUP_5 == "75_79" |AGE_GROUP_5 == "80_84" |AGE_GROUP_5 == "85_89" |AGE_GROUP_5 == "90_94" |AGE_GROUP_5 == "95+" ) %>%
  # Sum total numbers over 65 by GP practice
  group_by(practice_code) %>%
  mutate(total = sum(NUMBER_OF_PATIENTS)) %>%
  # Keep required columns
  dplyr::select(4,10) %>%
  # Rename
  rename(total.over65 = total) %>%
  distinct()

# Add list size data to CQC data
data.all2 <- inner_join(data.all,list_size.total, by="practice_code") %>%
  inner_join(list_size.females, by="practice_code") %>%
  inner_join(list_size.65plus, by="practice_code")

print(paste0("There are ", nrow(data.all2), " practices remaining after list size data were added"))


## Chronic disease
## Indicator ID 351. % with a long-standing health condition. Question 35. People were asked "Do you have any long-term physical or mental health conditions, disabilities or illnesses". The indicator value is the percentage of people who answered this question with a "Yes" from all responses to this question.. From GP patient survey

data_disease <- fingertips_data(IndicatorID = 351, AreaTypeID = 7) %>% # IMD scores by GP practice
  # Keep IMD(2019) scores only
  filter(Timeperiod == "2023") %>% 
  # Remove England value
  filter(AreaType == "GPs") %>% 
  # Keep required fields only
  dplyr::select(AreaCode, AreaName, ParentName, Value) %>% 
  # Rename columns
  dplyr::rename("practice_code" = "AreaCode", 
                "gp.name" = "AreaName",
                "pcn.name" = "ParentName",
                "p.chronic.disease" = "Value") %>%
  # Keep required columns
  dplyr::select(practice_code, p.chronic.disease)

data.all3 <- merge(data.all2,data_disease,by="practice_code") # add IMD score to data

print(paste0("There are ", nrow(data.all3), " practices remaining after chronic disease percentages were added"))


## QOF points achieved
## Indicator 295. % QOF points achieved 2022/23. The percentage of all QOF points achieved across all domains as a proportion of all achievable points.

data_qof <- fingertips_data(IndicatorID = 295, AreaTypeID = 7) %>% # IMD scores by GP practice
  # Keep IMD(2019) scores only
  filter(Timeperiod == "2022/23") %>% 
  # Remove England value
  filter(AreaType == "GPs") %>% 
  # Keep required fields only
  dplyr::select(AreaCode, AreaName, ParentName, Value) %>% 
  # Rename columns
  dplyr::rename("practice_code" = "AreaCode", 
                "gp.name" = "AreaName",
                "pcn.name" = "ParentName",
                "p.qof" = "Value") %>%
  # Keep required columns
  dplyr::select(practice_code, p.qof)

data.all4 <- merge(data.all3,data_qof,by="practice_code") # add IMD score to data

print(paste0("There are ", nrow(data.all4), " practices remaining after qof points percentages were added"))


## Workforce data from https://digital.nhs.uk/data-and-information/publications/statistical/general-and-personal-medical-services (June 2024)

data_workforce <- read.csv("1 General Practice – June 2024 Practice Level - Detailed.csv") %>%
  # Select required rows - FTE of GP and DPCs
  dplyr::select(1,87,533) %>%
  # Rename (note DPC = direct patient care personnel except GPs)
  rename(
    practice_code = PRAC_CODE,
    fte.gp = TOTAL_GP_FTE,
    fte.dpc = TOTAL_DPC_FTE
  )

data.all5 <- merge(data.all4,data_workforce,by="practice_code") # add IMD score to data

print(paste0("There are ", nrow(data.all5), " practices remaining after workforce data were added"))

## Processing - calculate number of patients 
data.all <- data.all5 %>%
  # Calculate number of patients per GP and per DPC
  mutate(n.patients.per.gp = total.listsize / fte.gp) %>%
  mutate(n.patients.per.dpc = total.listsize / fte.dpc) %>%
  # Calculate demographic proportions
  mutate(p.female = 100 * total.female / total.listsize) %>%
  mutate(p.over65 = 100 * total.over65 / total.listsize) %>%
  # Identify practices with a DPC
  mutate(dpc = case_when(
    is.na(n.patients.per.dpc) ~ "No",
    n.patients.per.dpc == "Inf" ~ "No",
    TRUE ~ "Yes")) %>%
  # Remove practices with <1000 patients
  filter(total.listsize >= 1000) %>%
  # Remove unrequired columns 
  dplyr::select(-c(CCD19CD, fte.gp, fte.dpc, n.patients.per.dpc, total.female, total.over65)) %>%
  # Remove rows with missing n.patients.per.gp and chronic disease, or >10k patients per GP (data error?)
  filter(!is.na(n.patients.per.gp)) %>%
  filter(!is.na(p.chronic.disease)) %>%
  filter(n.patients.per.gp < 10000)
  

# Factorise rating data
data.all$Overall <- factor(data.all$Overall, levels = c("Outstanding", "Good", "Requires improvement", "Inadequate", "Insufficient evidence to rate"))
data.all$Safe <- factor(data.all$Safe, levels = c("Outstanding", "Good", "Requires improvement", "Inadequate", "Insufficient evidence to rate"))
data.all$Caring <- factor(data.all$Caring, levels = c("Outstanding", "Good", "Requires improvement", "Inadequate", "Insufficient evidence to rate"))
data.all$Effective <- factor(data.all$Effective, levels = c("Outstanding", "Good", "Requires improvement", "Inadequate", "Insufficient evidence to rate"))
data.all$Well.led <- factor(data.all$Well.led, levels = c("Outstanding", "Good", "Requires improvement", "Inadequate", "Insufficient evidence to rate"))
data.all$Responsive <- factor(data.all$Responsive, levels = c("Outstanding", "Good", "Requires improvement", "Inadequate", "Insufficient evidence to rate"))

# Calculate quintiles of continuous independent variables
data.all <- data.all %>%
  mutate(total.listsize.quintile = ntile(total.listsize,5)) %>%
  mutate(p.female.quintile = ntile(p.female,5)) %>%
  mutate(p.over65.quintile = ntile(p.over65,5)) %>%
  mutate(imd.score.quintile = ntile(imd.score,5)) %>%
  mutate(p.chronic.disease.quintile = ntile(p.chronic.disease,5)) %>%
  mutate(p.qof.quintile = ntile(p.qof, 5)) %>%
  mutate(n.patients.per.gp.quintile = ntile(n.patients.per.gp,5)) %>%
  #dplyr::select(-c(total.listsize, p.female, p.over65, imd.score, p.chronic.disease, p.qof, n.patients.per.gp)) %>%
  filter(!is.na(Overall))


# 1. Distribution of CQC ratings ------------

# Overall

tabledata <- count(data.all$Overall)%>%
  rename(Rating = x, Count = freq)

total <- sum(tabledata$Count)
tabledata <- tabledata %>% mutate (Percentage = round(100*(Count/total),1))

Cairo(file="Figure 1. CQC outcome distribution.png", 
      type="png",
      units="in", 
      width=10, 
      height=4, 
      pointsize=1, 
      dpi=1200)

ggplot(na.omit(tabledata),
       aes(x = Count, y = Rating, fill = Rating)) +
  geom_col() +
  scale_x_continuous(expand = expansion(mult = c(0, 0.3))) +
  #scale_x_log10() +
  scale_y_discrete(limits = rev) +
  scale_fill_manual(breaks = c("Outstanding", "Good", "Requires improvement",
                               "Inadequate", "Insufficient evidence to rate"),
                    values = c("#A8E6CE", "#DCEDC2", "#FFD3B5",
                               "#FFAAA6", "#FF8C94")) +
  geom_text(aes(label=paste0(Count," (",Percentage,"%)")), position=position_dodge(width=0.9), hjust=-0.1) +
  labs(x = "Number of practices",
       y = "Rating",
       title = "Distribution of CQC outcomes: Overall",
       caption = "Data source: Care Quality Commission, July 2024") +
  theme_bw() +
  theme(panel.grid.major.y = element_blank(),
        legend.position = "off")

dev.off()


# 2. IMD distribution by CQC ratings ---------

Cairo(file="Figure 2. IMD distribution by CQC rating.png", 
      type="png",
      units="in", 
      width=4, 
      height=6, 
      pointsize=1, 
      dpi=1200)

ggplot(na.omit(data.all), aes(x = Overall, y = imd.score)) +
  geom_boxplot(size = 0.45, alpha = 1,  aes(color=Overall)) +
  geom_jitter(alpha = .15, size = 0.05, aes(color=Overall)) +
  #facet_grid(pared ~ public, margins = TRUE) +
  scale_color_manual(breaks = c("Outstanding", "Good", "Requires improvement",
                               "Inadequate", "Insufficient evidence to rate"),
                    values = c("#A8E6CE", "#DCEDC2", "#FFD3B5",
                               "#FFAAA6", "#FF8C94")) +
  labs(x = "\nRating",
       y = "IMD (2019) score\n",
       title = "IMD score distribution by CQC rating:\nOverall",
       #caption = "Data source: Care Quality Commission, July 2024"
       ) +
  theme_bw() +
  theme(panel.grid.major.y = element_blank(),
        legend.position = "off") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

dev.off()


# 3. Percent age over 65 years distribution by CQC ratings ---------

Cairo(file="Figure 3. Percent age over 65yr by CQC rating - overall.png", 
      type="png",
      units="in", 
      width=4, 
      height=6, 
      pointsize=1, 
      dpi=1200)

ggplot(na.omit(data.all), aes(x = Overall, y = p.over65)) +
  geom_boxplot(size = 0.45, alpha = 1,  aes(color=Overall)) +
  geom_jitter(alpha = .15, size = 0.05, aes(color=Overall)) +
  #facet_grid(pared ~ public, margins = TRUE) +
  scale_color_manual(breaks = c("Outstanding", "Good", "Requires improvement",
                                "Inadequate", "Insufficient evidence to rate"),
                     values = c("#A8E6CE", "#DCEDC2", "#FFD3B5",
                                "#FFAAA6", "#FF8C94")) +
  labs(x = "\nRating",
       y = "Percentage of patients over 65 years\n",
       title = "Percentage over 65 years by CQC rating:\nOverall",
       #caption = "Data source: Care Quality Commission, July 2024"
  ) +
  theme_bw() +
  theme(panel.grid.major.y = element_blank(),
        legend.position = "off") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

dev.off()


# 4. Percent females distribution by CQC ratings ---------

Cairo(file="Figure 4. Percent females by CQC rating - overall.png", 
      type="png",
      units="in", 
      width=4, 
      height=6, 
      pointsize=1, 
      dpi=1200)

ggplot(na.omit(data.all), aes(x = Overall, y = p.female)) +
  geom_boxplot(size = 0.45, alpha = 1,  aes(color=Overall)) +
  geom_jitter(alpha = .15, size = 0.05, aes(color=Overall)) +
  #facet_grid(pared ~ public, margins = TRUE) +
  scale_color_manual(breaks = c("Outstanding", "Good", "Requires improvement",
                                "Inadequate", "Insufficient evidence to rate"),
                     values = c("#A8E6CE", "#DCEDC2", "#FFD3B5",
                                "#FFAAA6", "#FF8C94")) +
  labs(x = "\nRating",
       y = "Percentage of female patients\n",
       title = "Percentage females by CQC rating:\nOverall",
       #caption = "Data source: Care Quality Commission, July 2024"
  ) +
  theme_bw() +
  theme(panel.grid.major.y = element_blank(),
        legend.position = "off") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

dev.off()


# 5. List size distribution by CQC ratings ---------

Cairo(file="Figure 5. List size distribution by CQC rating - overall.png", 
      type="png",
      units="in", 
      width=4, 
      height=6, 
      pointsize=1, 
      dpi=1200)

ggplot(na.omit(data.all), aes(x = Overall, y = total.listsize)) +
  geom_boxplot(size = 0.45, alpha = 1,  aes(color=Overall)) +
  geom_jitter(alpha = .15, size = 0.05, aes(color=Overall)) +
  #facet_grid(pared ~ public, margins = TRUE) +
  scale_color_manual(breaks = c("Outstanding", "Good", "Requires improvement",
                                "Inadequate", "Insufficient evidence to rate"),
                     values = c("#A8E6CE", "#DCEDC2", "#FFD3B5",
                                "#FFAAA6", "#FF8C94")) +
  labs(x = "\nRating",
       y = "List size\n",
       title = "List size by CQC rating:\nOverall",
       #caption = "Data source: Care Quality Commission, July 2024"
  ) +
  theme_bw() +
  theme(panel.grid.major.y = element_blank(),
        legend.position = "off") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

dev.off()


# 6. Chronic disease distribution by CQC ratings ---------

Cairo(file="Figure 6. Chronic disease proportion by CQC rating - overall.png", 
      type="png",
      units="in", 
      width=4, 
      height=6, 
      pointsize=1, 
      dpi=1200)

ggplot(na.omit(data.all), aes(x = Overall, y = p.chronic.disease)) +
  geom_boxplot(size = 0.45, alpha = 1,  aes(color=Overall)) +
  geom_jitter(alpha = .15, size = 0.05, aes(color=Overall)) +
  #facet_grid(pared ~ public, margins = TRUE) +
  scale_color_manual(breaks = c("Outstanding", "Good", "Requires improvement",
                                "Inadequate", "Insufficient evidence to rate"),
                     values = c("#A8E6CE", "#DCEDC2", "#FFD3B5",
                                "#FFAAA6", "#FF8C94")) +
  labs(x = "\nRating",
       y = "Chronic disease %\n",
       title = "Chronic disease % by CQC rating:\nOverall",
       #caption = "Data source: Care Quality Commission, July 2024"
  ) +
  theme_bw() +
  theme(panel.grid.major.y = element_blank(),
        legend.position = "off") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

dev.off()

# 7. QOF points by CQC ratings ----------------

Cairo(file="Figure 7. Proportion of total QOF points by CQC rating - overall.png", 
      type="png",
      units="in", 
      width=4, 
      height=6, 
      pointsize=1, 
      dpi=1200)

ggplot(na.omit(data.all), aes(x = Overall, y = p.qof)) +
  geom_boxplot(size = 0.45, alpha = 1,  aes(color=Overall)) +
  geom_jitter(alpha = .15, size = 0.05, aes(color=Overall)) +
  #facet_grid(pared ~ public, margins = TRUE) +
  scale_color_manual(breaks = c("Outstanding", "Good", "Requires improvement",
                                "Inadequate", "Insufficient evidence to rate"),
                     values = c("#A8E6CE", "#DCEDC2", "#FFD3B5",
                                "#FFAAA6", "#FF8C94")) +
  labs(x = "\nRating",
       y = "QOF points (% of total)\n",
       title = "QOF points (% of total) by CQC rating:\nOverall",
       #caption = "Data source: Care Quality Commission, July 2024"
  ) +
  theme_bw() +
  theme(panel.grid.major.y = element_blank(),
        legend.position = "off") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

dev.off()

# 8. Patients per GP by CQC ratings ----------------

Cairo(file="Figure 8. Patients per GP by CQC rating - overall.png", 
      type="png",
      units="in", 
      width=4, 
      height=6, 
      pointsize=1, 
      dpi=1200)

ggplot(na.omit(data.all), aes(x = Overall, y = n.patients.per.gp)) +
  geom_boxplot(size = 0.45, alpha = 1,  aes(color=Overall)) +
  geom_jitter(alpha = .15, size = 0.05, aes(color=Overall)) +
  #facet_grid(pared ~ public, margins = TRUE) +
  scale_color_manual(breaks = c("Outstanding", "Good", "Requires improvement",
                                "Inadequate", "Insufficient evidence to rate"),
                     values = c("#A8E6CE", "#DCEDC2", "#FFD3B5",
                                "#FFAAA6", "#FF8C94")) +
  labs(x = "\nRating",
       y = "Patients per GP (FTE)\n",
       title = "Patients per GP (FTE) by CQC rating:\nOverall",
       #caption = "Data source: Care Quality Commission, July 2024"
  ) +
  theme_bw() +
  theme(panel.grid.major.y = element_blank(),
        legend.position = "off") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

dev.off()

# 9. Presence of DCP by CQC rating

# Calculate the proportion of "Yes" per group
proportion_data <- data.all %>%
  group_by(Overall) %>%
  summarise(yes_proportion = 100*mean(dpc == "Yes"))

Cairo(file="Figure 9. Proportion with AHP by CQC rating - overall.png", 
      type="png",
      units="in", 
      width=4, 
      height=6, 
      pointsize=1, 
      dpi=1200)

ggplot(na.omit(proportion_data), aes(x = Overall, y = yes_proportion, fill = Overall)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(breaks = c("Outstanding", "Good", "Requires improvement",
                                "Inadequate", "Insufficient evidence to rate"),
                     values = c("#A8E6CE", "#DCEDC2", "#FFD3B5",
                                "#FFAAA6", "#FF8C94")) +
  labs(x = "\nRating",
       y = "Proportion with Allied Health Professional (%)\n",
       title = "Proportion with AHPs by CQC rating:\nOverall",
       #caption = "Data source: Care Quality Commission, July 2024"
  ) +
  geom_text(aes(label= paste0(round((yes_proportion),1),"%")), vjust=2) +
  theme_bw() +
  theme(panel.grid.major.y = element_blank(),
        legend.position = "off") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))


dev.off()


## ADD QOF POINTS AND PATIENTS PER GP


# 10. Regression for CQC ratings - see https://stats.idre.ucla.edu/r/dae/ordinal-logistic-regression/ -----------
# More info here https://marissabarlaz.github.io/portfolio/ols/

# Remove practice with insufficient evidence to rate
data.all.copy <- data.all # Make copy of data
data.all <- data.all %>%
  filter(Overall != "Insufficient evidence to rate") %>%
  filter(Safe != "Insufficient evidence to rate") %>%
  filter(Caring != "Insufficient evidence to rate") %>%
  filter(Effective != "Insufficient evidence to rate") %>%
  filter(Responsive != "Insufficient evidence to rate") %>%
  filter(Well.led != "Insufficient evidence to rate")

# Factorise rating data to remove "Insufficient evidence" level
data.all$Overall <- factor(data.all$Overall, levels = c("Inadequate","Requires improvement","Good","Outstanding"))
data.all$Safe <- factor(data.all$Safe, levels = c("Inadequate","Requires improvement","Good","Outstanding"))
data.all$Caring <- factor(data.all$Caring, levels = c("Inadequate","Requires improvement","Good","Outstanding"))
data.all$Effective <- factor(data.all$Effective, levels = c("Inadequate","Requires improvement","Good","Outstanding"))
data.all$Well.led <- factor(data.all$Well.led, levels = c("Inadequate","Requires improvement","Good","Outstanding"))
data.all$Responsive <- factor(data.all$Responsive, levels = c("Inadequate","Requires improvement","Good","Outstanding"))

# Fit ordered logit model - UNIVARIATE - ADD EXTRA VARIABLES

# IMD
m <- polr(Overall ~ imd.score.quintile, data = data.all, Hess=TRUE)
#summary(m)
ctable <- coef(summary(m)) # store table of coefficient results
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2 # calculate p values
(ctable <- cbind(ctable, "p value" = p)) # combine tables HERE: Note p value for any independent variables
ci <- confint.default(m) # Alternative CIs, assuming normality
res1A <- exp(cbind(OR = coef(m), ci)) # Table of ORs and CIs HERE: Note OR and 95% CI for any independent variables
res2A <- ctable

# List size
m <- polr(Overall ~ total.listsize.quintile, data = data.all, Hess=TRUE)
ctable <- coef(summary(m)) # store table of coefficient results
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2 # calculate p values
(ctable <- cbind(ctable, "p value" = p)) # combine tables HERE: Note p value for any independent variables
ci <- confint.default(m) # Alternative CIs, assuming normality
exp(cbind(OR = coef(m), ci)) # Table of ORs and CIs HERE: Note OR and 95% CI for any independent variables
res1B <- exp(cbind(OR = coef(m), ci)) # Table of ORs and CIs HERE: Note OR and 95% CI for any independent variables
res2B <- ctable

# % female
m <- polr(Overall ~ p.female.quintile, data = data.all, Hess=TRUE)
ctable <- coef(summary(m)) # store table of coefficient results
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2 # calculate p values
(ctable <- cbind(ctable, "p value" = p)) # combine tables HERE: Note p value for any independent variables
ci <- confint.default(m) # Alternative CIs, assuming normality
exp(cbind(OR = coef(m), ci)) # Table of ORs and CIs HERE: Note OR and 95% CI for any independent variables
res1C <- exp(cbind(OR = coef(m), ci)) # Table of ORs and CIs HERE: Note OR and 95% CI for any independent variables
res2C <- ctable

# % over65
m <- polr(Overall ~ p.over65.quintile, data = data.all, Hess=TRUE)
ctable <- coef(summary(m)) # store table of coefficient results
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2 # calculate p values
(ctable <- cbind(ctable, "p value" = p)) # combine tables HERE: Note p value for any independent variables
ci <- confint.default(m) # Alternative CIs, assuming normality
exp(cbind(OR = coef(m), ci)) # Table of ORs and CIs HERE: Note OR and 95% CI for any independent variables
res1D <- exp(cbind(OR = coef(m), ci)) # Table of ORs and CIs HERE: Note OR and 95% CI for any independent variables
res2D <- ctable

# % chronic disease
m <- polr(Overall ~ p.chronic.disease.quintile, data = data.all, Hess=TRUE)
ctable <- coef(summary(m)) # store table of coefficient results
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2 # calculate p values
(ctable <- cbind(ctable, "p value" = p)) # combine tables HERE: Note p value for any independent variables
ci <- confint.default(m) # Alternative CIs, assuming normality
exp(cbind(OR = coef(m), ci)) # Table of ORs and CIs HERE: Note OR and 95% CI for any independent variables
res1E <- exp(cbind(OR = coef(m), ci)) # Table of ORs and CIs HERE: Note OR and 95% CI for any independent variables
res2E <- ctable

# % QOF points
m <- polr(Overall ~ p.qof.quintile, data = data.all, Hess=TRUE)
ctable <- coef(summary(m)) # store table of coefficient results
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2 # calculate p values
(ctable <- cbind(ctable, "p value" = p)) # combine tables HERE: Note p value for any independent variables
ci <- confint.default(m) # Alternative CIs, assuming normality
exp(cbind(OR = coef(m), ci)) # Table of ORs and CIs HERE: Note OR and 95% CI for any independent variables
res1F <- exp(cbind(OR = coef(m), ci)) # Table of ORs and CIs HERE: Note OR and 95% CI for any independent variables
res2F <- ctable

# patients per GP
m <- polr(Overall ~ n.patients.per.gp.quintile, data = data.all, Hess=TRUE)
ctable <- coef(summary(m)) # store table of coefficient results
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2 # calculate p values
(ctable <- cbind(ctable, "p value" = p)) # combine tables HERE: Note p value for any independent variables
ci <- confint.default(m) # Alternative CIs, assuming normality
exp(cbind(OR = coef(m), ci)) # Table of ORs and CIs HERE: Note OR and 95% CI for any independent variables
res1G <- exp(cbind(OR = coef(m), ci)) # Table of ORs and CIs HERE: Note OR and 95% CI for any independent variables
res2G <- ctable

# AHP present
m <- polr(Overall ~ dpc, data = data.all, Hess=TRUE)
ctable <- coef(summary(m)) # store table of coefficient results
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2 # calculate p values
(ctable <- cbind(ctable, "p value" = p)) # combine tables HERE: Note p value for any independent variables
ci <- confint.default(m) # Alternative CIs, assuming normality
exp(cbind(OR = coef(m), ci)) # Table of ORs and CIs HERE: Note OR and 95% CI for any independent variables
res1H <- exp(cbind(OR = coef(m), ci)) # Table of ORs and CIs HERE: Note OR and 95% CI for any independent variables
res2H <- ctable

# Create univariate results tables

# ORs table
uni.ors <- rbind(res1A,res1B,res1C,res1D,res1E,res1F,res1G,res1H) %>% as.data.frame()
uni.ors <- tibble::rownames_to_column(uni.ors, var = "indicator")
  
# P values table
uni.pvals <- rbind(res2A,res2B,res2C,res2D,res2E,res2F,res2G,res2H) %>% 
  as.data.frame() %>%
  dplyr::select(4) %>%
  rownames_to_column(var = "indicator") 

# Select overall p values only
uni.pvals <- uni.pvals[seq(1, nrow(uni.pvals), by = 4),]
  
# Create single table
uni.results <- uni.ors %>%
  left_join(uni.pvals, by = "indicator") %>%
  mutate("95% CI" = paste0("(",round(`2.5 %`,3)," - ",round(`97.5 %`,3),")")) %>%
  mutate("Odds ratio" = round(OR,3)) %>%
  mutate(`p value` = round(`p value`,3)) %>%
  dplyr::select(indicator, `Odds ratio`, `95% CI`,`p value`) %>%
  mutate(indicator = str_replace(indicator, "imd.score.quintile", "IMD score")) %>%
  mutate(indicator = str_replace(indicator, "total.listsize.quintile", "List size")) %>%
  mutate(indicator = str_replace(indicator, "p.female.quintile", "Female %")) %>%
  mutate(indicator = str_replace(indicator, "p.over65.quintile", "Over-65 %")) %>%
  mutate(indicator = str_replace(indicator, "p.chronic.disease.quintile", "Chronic disease %")) %>%
  mutate(indicator = str_replace(indicator, "p.qof.quintile", "QOF performance")) %>%
  mutate(indicator = str_replace(indicator, "n.patients.per.gp.quintile", "Patients per GP")) %>%
  mutate(indicator = str_replace(indicator, "dpcYes", "Allied health professional")) 
  
write.csv(uni.results,"Table 1. Univariate regression.csv")

# Fit ordered logit model - MULTIVARIATE
m <- polr(Overall ~ imd.score.quintile + total.listsize.quintile + p.female.quintile + p.over65.quintile + p.chronic.disease.quintile + p.qof.quintile + n.patients.per.gp.quintile + dpc, data = data.all, Hess=TRUE)
summary(m)
ctable <- coef(summary(m)) # store table of coefficient results
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2 # calculate p values
ctable <- cbind(ctable, "p value" = p) # combine tables HERE: Note p value for any independent variables
ci <- confint.default(m) # Alternative CIs, assuming normality
res4 <- exp(cbind(OR = coef(m), ci)) # Table of ORs and CIs HERE: Note OR and 95% CI for any independent variables
res3 <- ctable

# ORs table
multi.ors <- res4 %>% 
  as.data.frame() %>%
  tibble::rownames_to_column(var = "indicator")

# P values table
multi.pvals <- res3 %>% 
  as.data.frame() %>%
  dplyr::select(4) %>%
  rownames_to_column(var = "indicator")  %>%
  dplyr::slice(1:8)


# Create single table
multi.results <- multi.ors %>%
  left_join(multi.pvals, by = "indicator") %>%
  mutate("95% CI" = paste0("(",round(`2.5 %`,3)," - ",round(`97.5 %`,3),")")) %>%
  mutate("Odds ratio" = round(OR,3)) %>%
  mutate(`p value` = round(`p value`,3)) %>%
  dplyr::select(indicator, `Odds ratio`, `95% CI`,`p value`) %>%
  mutate(indicator = str_replace(indicator, "imd.score.quintile", "IMD score")) %>%
  mutate(indicator = str_replace(indicator, "total.listsize.quintile", "List size")) %>%
  mutate(indicator = str_replace(indicator, "p.female.quintile", "Female %")) %>%
  mutate(indicator = str_replace(indicator, "p.over65.quintile", "Over-65 %")) %>%
  mutate(indicator = str_replace(indicator, "p.chronic.disease.quintile", "Chronic disease %")) %>%
  mutate(indicator = str_replace(indicator, "p.qof.quintile", "QOF performance")) %>%
  mutate(indicator = str_replace(indicator, "n.patients.per.gp.quintile", "Patients per GP")) %>%
  mutate(indicator = str_replace(indicator, "dpcYes", "Allied health professional")) 

write.csv(multi.results,"Table 2. Multivariate Univariate regression.csv")

