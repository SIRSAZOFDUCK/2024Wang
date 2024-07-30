## Analysis of primary care quality vs deprivation 
## Gursh Hayer, Saran Shantikumar
## saran.shantikumar@warwick.ac.uk
## v1.0 Dec 2021

rm(list = ls())

list.of.packages <- c("ordinal","effects","ggeffects","foreign","MASS","Hmisc","data.table","dplyr","plyr","RColorBrewer","ggplot2","corrplot","rgdal","rgeos","raster","maptools","scales","plotly","latticeExtra","maps","classInt","grid","pals","reshape2","cowplot","sandwich","msm","Cairo")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

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
activedir <- "D:/SSC2_Gursh/Template" # active directory


# set working directory 
setwd(activedir) 

# CQC ratings
cqc.data <- read.csv("01_November_2021_Latest_ratings.csv") # read original data

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
  rename(PRACTICE_CODE = "Location.ODS.Code") %>%
  rename(CCD19CD = "Location.ONSPD.CCG.Code")

# Identify distinct rows
cqc.all.data <- distinct(cqc.all.data) # remove duplicate rows
cqc.all.data <- cqc.all.data[-c(689:695,2804:2806,3011:3017,6373:6375),] # Duplicates: 689-695, 2804-2806, 3011-3017, 6373-6375
 

# IMD
imd <- read.csv("imd2019_practice.csv") # load deprivation data by practice
imd <- imd %>% dplyr::select(Area.Code, Value) %>% 
  rename(PRACTICE_CODE = Area.Code) %>%
  rename(IMD2019 = Value)

data.all <- merge(cqc.all.data,imd,by="PRACTICE_CODE") # add IMD score to data

# List size, age, sex
list_size <- read.csv("practice_listsize.csv") %>% rename(PRACTICE_CODE = PRACTICE) # load list size data by practice
data.all <- merge(data.all,list_size,by="PRACTICE_CODE") # add list size to data

## QOF data
#  chronic disease - can't find data

# Factorise rating data
data.all$Overall <- factor(data.all$Overall, levels = c("Outstanding", "Good", "Requires improvement", "Inadequate", "Insufficient evidence to rate"))
data.all$Safe <- factor(data.all$Safe, levels = c("Outstanding", "Good", "Requires improvement", "Inadequate", "Insufficient evidence to rate"))
data.all$Caring <- factor(data.all$Caring, levels = c("Outstanding", "Good", "Requires improvement", "Inadequate", "Insufficient evidence to rate"))
data.all$Effective <- factor(data.all$Effective, levels = c("Outstanding", "Good", "Requires improvement", "Inadequate", "Insufficient evidence to rate"))
data.all$Well.led <- factor(data.all$Well.led, levels = c("Outstanding", "Good", "Requires improvement", "Inadequate", "Insufficient evidence to rate"))
data.all$Responsive <- factor(data.all$Responsive, levels = c("Outstanding", "Good", "Requires improvement", "Inadequate", "Insufficient evidence to rate"))

# Calculate quintiles of continuous independent variables
data.all <- data.all %>%
  mutate(LIST.SIZE.QUINTILE = ntile(LIST.SIZE,5)) %>%
  mutate(PERCENT.MALE.QUINTILE = ntile(PERCENT.MALE,5)) %>%
  mutate(PERCENT.OVER65.QUINTILE = ntile(PERCENT.OVER65,5)) %>%
  mutate(IMD2019.QUINTILE = ntile(IMD2019,5))


# 1. Distribution of CQC ratings ------------

# Overall

tabledata <- count(data.all$Overall)%>%
  rename(Rating = x, Count = freq)

total <- sum(tabledata$Count)
tabledata <- tabledata %>% mutate (Percentage = round(100*(Count/total),1))

Cairo(file="Figure 1A. CQC outcome distribution - overall.png", 
      type="png",
      units="in", 
      width=10, 
      height=4, 
      pointsize=1, 
      dpi=1200)

ggplot(tabledata,
       aes(x = Count, y = Rating, fill = Rating)) +
  geom_col() +
  scale_x_continuous(expand = expansion(mult = c(0, 0.2))) +
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
       caption = "Data source: Care Quality Commission, Nov 2021") +
  theme_bw() +
  theme(panel.grid.major.y = element_blank(),
        legend.position = "off")

dev.off()

# Safe

tabledata <- count(data.all$Safe)%>%
  rename(Rating = x, Count = freq)

total <- sum(tabledata$Count)
tabledata <- tabledata %>% mutate (Percentage = round(100*(Count/total),1))

Cairo(file="Figure 1B. CQC outcome distribution - safe.png", 
      type="png",
      units="in", 
      width=10, 
      height=4, 
      pointsize=1, 
      dpi=1200)

ggplot(tabledata,
       aes(x = Count, y = Rating, fill = Rating)) +
  geom_col() +
  scale_x_continuous(expand = expansion(mult = c(0, 0.2))) +
  #scale_x_log10() +
  scale_y_discrete(limits = rev) +
  scale_fill_manual(breaks = c("Outstanding", "Good", "Requires improvement",
                               "Inadequate", "Insufficient evidence to rate"),
                    values = c("#A8E6CE", "#DCEDC2", "#FFD3B5",
                               "#FFAAA6", "#FF8C94")) +
  geom_text(aes(label=paste0(Count," (",Percentage,"%)")), position=position_dodge(width=0.9), hjust=-0.1) +
  labs(x = "Number of practices",
       y = "Rating",
       title = "Distribution of CQC outcomes: Safe",
       caption = "Data source: Care Quality Commission, Nov 2021") +
  theme_bw() +
  theme(panel.grid.major.y = element_blank(),
        legend.position = "off")

dev.off()

# Caring

tabledata <- count(data.all$Caring)%>%
  rename(Rating = x, Count = freq)

total <- sum(tabledata$Count)
tabledata <- tabledata %>% mutate (Percentage = round(100*(Count/total),1))

Cairo(file="Figure 1C. CQC outcome distribution - caring.png", 
      type="png",
      units="in", 
      width=10, 
      height=4, 
      pointsize=1, 
      dpi=1200)

ggplot(tabledata,
       aes(x = Count, y = Rating, fill = Rating)) +
  geom_col() +
  scale_x_continuous(expand = expansion(mult = c(0, 0.2))) +
  #scale_x_log10() +
  scale_y_discrete(limits = rev) +
  scale_fill_manual(breaks = c("Outstanding", "Good", "Requires improvement",
                               "Inadequate", "Insufficient evidence to rate"),
                    values = c("#A8E6CE", "#DCEDC2", "#FFD3B5",
                               "#FFAAA6", "#FF8C94")) +
  geom_text(aes(label=paste0(Count," (",Percentage,"%)")), position=position_dodge(width=0.9), hjust=-0.1) +
  labs(x = "Number of practices",
       y = "Rating",
       title = "Distribution of CQC outcomes: Caring",
       caption = "Data source: Care Quality Commission, Nov 2021") +
  theme_bw() +
  theme(panel.grid.major.y = element_blank(),
        legend.position = "off")

dev.off()

# Effective

tabledata <- count(data.all$Effective)%>%
  rename(Rating = x, Count = freq)

total <- sum(tabledata$Count)
tabledata <- tabledata %>% mutate (Percentage = round(100*(Count/total),1))

Cairo(file="Figure 1D. CQC outcome distribution - effective.png", 
      type="png",
      units="in", 
      width=10, 
      height=4, 
      pointsize=1, 
      dpi=1200)

ggplot(tabledata,
       aes(x = Count, y = Rating, fill = Rating)) +
  geom_col() +
  scale_x_continuous(expand = expansion(mult = c(0, 0.2))) +
  #scale_x_log10() +
  scale_y_discrete(limits = rev) +
  scale_fill_manual(breaks = c("Outstanding", "Good", "Requires improvement",
                               "Inadequate", "Insufficient evidence to rate"),
                    values = c("#A8E6CE", "#DCEDC2", "#FFD3B5",
                               "#FFAAA6", "#FF8C94")) +
  geom_text(aes(label=paste0(Count," (",Percentage,"%)")), position=position_dodge(width=0.9), hjust=-0.1) +
  labs(x = "Number of practices",
       y = "Rating",
       title = "Distribution of CQC outcomes: Effective",
       caption = "Data source: Care Quality Commission, Nov 2021") +
  theme_bw() +
  theme(panel.grid.major.y = element_blank(),
        legend.position = "off")

dev.off()

# Well-led

tabledata <- count(data.all$Well.led)%>%
  rename(Rating = x, Count = freq)

total <- sum(tabledata$Count)
tabledata <- tabledata %>% mutate (Percentage = round(100*(Count/total),1))

Cairo(file="Figure 1E. CQC outcome distribution - well-led.png", 
      type="png",
      units="in", 
      width=10, 
      height=4, 
      pointsize=1, 
      dpi=1200)

ggplot(tabledata,
       aes(x = Count, y = Rating, fill = Rating)) +
  geom_col() +
  scale_x_continuous(expand = expansion(mult = c(0, 0.2))) +
  #scale_x_log10() +
  scale_y_discrete(limits = rev) +
  scale_fill_manual(breaks = c("Outstanding", "Good", "Requires improvement",
                               "Inadequate", "Insufficient evidence to rate"),
                    values = c("#A8E6CE", "#DCEDC2", "#FFD3B5",
                               "#FFAAA6", "#FF8C94")) +
  geom_text(aes(label=paste0(Count," (",Percentage,"%)")), position=position_dodge(width=0.9), hjust=-0.1) +
  labs(x = "Number of practices",
       y = "Rating",
       title = "Distribution of CQC outcomes: Well-led",
       caption = "Data source: Care Quality Commission, Nov 2021") +
  theme_bw() +
  theme(panel.grid.major.y = element_blank(),
        legend.position = "off")

dev.off()

# Responsive

tabledata <- count(data.all$Responsive)%>%
  rename(Rating = x, Count = freq)

total <- sum(tabledata$Count)
tabledata <- tabledata %>% mutate (Percentage = round(100*(Count/total),1))

Cairo(file="Figure 1F. CQC outcome distribution - responsive.png", 
      type="png",
      units="in", 
      width=10, 
      height=4, 
      pointsize=1, 
      dpi=1200)

ggplot(tabledata,
       aes(x = Count, y = Rating, fill = Rating)) +
  geom_col() +
  scale_x_continuous(expand = expansion(mult = c(0, 0.2))) +
  #scale_x_log10() +
  scale_y_discrete(limits = rev) +
  scale_fill_manual(breaks = c("Outstanding", "Good", "Requires improvement",
                               "Inadequate", "Insufficient evidence to rate"),
                    values = c("#A8E6CE", "#DCEDC2", "#FFD3B5",
                               "#FFAAA6", "#FF8C94")) +
  geom_text(aes(label=paste0(Count," (",Percentage,"%)")), position=position_dodge(width=0.9), hjust=-0.1) +
  labs(x = "Number of practices",
       y = "Rating",
       title = "Distribution of CQC outcomes: Responsive",
       caption = "Data source: Care Quality Commission, Nov 2021") +
  theme_bw() +
  theme(panel.grid.major.y = element_blank(),
        legend.position = "off")

dev.off()



# 2. IMD distribution by CQC ratings ---------

Cairo(file="Figure 2A. IMD distribution by CQC rating - overall.png", 
      type="png",
      units="in", 
      width=4, 
      height=6, 
      pointsize=1, 
      dpi=1200)

ggplot(data.all, aes(x = Overall, y = IMD2019)) +
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
       #caption = "Data source: Care Quality Commission, Nov 2021"
       ) +
  theme_bw() +
  theme(panel.grid.major.y = element_blank(),
        legend.position = "off") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

dev.off()

Cairo(file="Figure 2B. IMD distribution by CQC rating - safe.png", 
      type="png",
      units="in", 
      width=4, 
      height=6, 
      pointsize=1, 
      dpi=1200)

ggplot(data.all, aes(x = Safe, y = IMD2019)) +
  geom_boxplot(size = 0.45, alpha = 1,  aes(color=Safe)) +
  geom_jitter(alpha = .15, size = 0.05, aes(color=Safe)) +
  #facet_grid(pared ~ public, margins = TRUE) +
  scale_color_manual(breaks = c("Outstanding", "Good", "Requires improvement",
                                "Inadequate", "Insufficient evidence to rate"),
                     values = c("#A8E6CE", "#DCEDC2", "#FFD3B5",
                                "#FFAAA6", "#FF8C94")) +
  labs(x = "\nRating",
       y = "IMD (2019) score\n",
       title = "IMD score distribution by CQC rating:\nSafe",
       #caption = "Data source: Care Quality Commission, Nov 2021"
  ) +
  theme_bw() +
  theme(panel.grid.major.y = element_blank(),
        legend.position = "off") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

dev.off()

Cairo(file="Figure 2C. IMD distribution by CQC rating - caring.png", 
      type="png",
      units="in", 
      width=4, 
      height=6, 
      pointsize=1, 
      dpi=1200)

ggplot(data.all, aes(x = Caring, y = IMD2019)) +
  geom_boxplot(size = 0.45, alpha = 1,  aes(color=Caring)) +
  geom_jitter(alpha = .15, size = 0.05, aes(color=Caring)) +
  #facet_grid(pared ~ public, margins = TRUE) +
  scale_color_manual(breaks = c("Outstanding", "Good", "Requires improvement",
                                "Inadequate", "Insufficient evidence to rate"),
                     values = c("#A8E6CE", "#DCEDC2", "#FFD3B5",
                                "#FFAAA6", "#FF8C94")) +
  labs(x = "\nRating",
       y = "IMD (2019) score\n",
       title = "IMD score distribution by CQC rating:\nCaring",
       #caption = "Data source: Care Quality Commission, Nov 2021"
  ) +
  theme_bw() +
  theme(panel.grid.major.y = element_blank(),
        legend.position = "off") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

dev.off()

Cairo(file="Figure 2D. IMD distribution by CQC rating - effective.png", 
      type="png",
      units="in", 
      width=4, 
      height=6, 
      pointsize=1, 
      dpi=1200)

ggplot(data.all, aes(x = Effective, y = IMD2019)) +
  geom_boxplot(size = 0.45, alpha = 1,  aes(color=Effective)) +
  geom_jitter(alpha = .15, size = 0.05, aes(color=Effective)) +
  #facet_grid(pared ~ public, margins = TRUE) +
  scale_color_manual(breaks = c("Outstanding", "Good", "Requires improvement",
                                "Inadequate", "Insufficient evidence to rate"),
                     values = c("#A8E6CE", "#DCEDC2", "#FFD3B5",
                                "#FFAAA6", "#FF8C94")) +
  labs(x = "\nRating",
       y = "IMD (2019) score\n",
       title = "IMD score distribution by CQC rating:\nEffective",
       #caption = "Data source: Care Quality Commission, Nov 2021"
  ) +
  theme_bw() +
  theme(panel.grid.major.y = element_blank(),
        legend.position = "off") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

dev.off()

Cairo(file="Figure 2E. IMD distribution by CQC rating - well-led.png", 
      type="png",
      units="in", 
      width=4, 
      height=6, 
      pointsize=1, 
      dpi=1200)

ggplot(data.all, aes(x = Well.led, y = IMD2019)) +
  geom_boxplot(size = 0.45, alpha = 1,  aes(color=Well.led)) +
  geom_jitter(alpha = .15, size = 0.05, aes(color=Well.led)) +
  #facet_grid(pared ~ public, margins = TRUE) +
  scale_color_manual(breaks = c("Outstanding", "Good", "Requires improvement",
                                "Inadequate", "Insufficient evidence to rate"),
                     values = c("#A8E6CE", "#DCEDC2", "#FFD3B5",
                                "#FFAAA6", "#FF8C94")) +
  labs(x = "\nRating",
       y = "IMD (2019) score\n",
       title = "IMD score distribution by CQC rating:\nWell-led",
       #caption = "Data source: Care Quality Commission, Nov 2021"
  ) +
  theme_bw() +
  theme(panel.grid.major.y = element_blank(),
        legend.position = "off") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

dev.off()

Cairo(file="Figure 2F. IMD distribution by CQC rating - responsive.png", 
      type="png",
      units="in", 
      width=4, 
      height=6, 
      pointsize=1, 
      dpi=1200)

ggplot(data.all, aes(x = Responsive, y = IMD2019)) +
  geom_boxplot(size = 0.45, alpha = 1,  aes(color=Responsive)) +
  geom_jitter(alpha = .15, size = 0.05, aes(color=Responsive)) +
  #facet_grid(pared ~ public, margins = TRUE) +
  scale_color_manual(breaks = c("Outstanding", "Good", "Requires improvement",
                                "Inadequate", "Insufficient evidence to rate"),
                     values = c("#A8E6CE", "#DCEDC2", "#FFD3B5",
                                "#FFAAA6", "#FF8C94")) +
  labs(x = "\nRating",
       y = "IMD (2019) score\n",
       title = "IMD score distribution by CQC rating:\nResponsive",
       #caption = "Data source: Care Quality Commission, Nov 2021"
  ) +
  theme_bw() +
  theme(panel.grid.major.y = element_blank(),
        legend.position = "off") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

dev.off()

# 3. Percent age over 65 years distribution by CQC ratings ---------

Cairo(file="Figure 3A. Percent age over 65yr by CQC rating - overall.png", 
      type="png",
      units="in", 
      width=4, 
      height=6, 
      pointsize=1, 
      dpi=1200)

ggplot(data.all, aes(x = Overall, y = PERCENT.OVER65)) +
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
       #caption = "Data source: Care Quality Commission, Nov 2021"
  ) +
  theme_bw() +
  theme(panel.grid.major.y = element_blank(),
        legend.position = "off") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

dev.off()

Cairo(file="Figure 3B. Percent age over 65yr by CQC rating - safe.png", 
      type="png",
      units="in", 
      width=4, 
      height=6, 
      pointsize=1, 
      dpi=1200)

ggplot(data.all, aes(x = Safe, y = PERCENT.OVER65)) +
  geom_boxplot(size = 0.45, alpha = 1,  aes(color=Safe)) +
  geom_jitter(alpha = .15, size = 0.05, aes(color=Safe)) +
  #facet_grid(pared ~ public, margins = TRUE) +
  scale_color_manual(breaks = c("Outstanding", "Good", "Requires improvement",
                                "Inadequate", "Insufficient evidence to rate"),
                     values = c("#A8E6CE", "#DCEDC2", "#FFD3B5",
                                "#FFAAA6", "#FF8C94")) +
  labs(x = "\nRating",
       y = "Percentage of patients over 65 years\n",
       title = "Percentage over 65 years by CQC rating:\nSafe",
       #caption = "Data source: Care Quality Commission, Nov 2021"
  ) +
  theme_bw() +
  theme(panel.grid.major.y = element_blank(),
        legend.position = "off") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

dev.off()

Cairo(file="Figure 3C. Percent age over 65yr by CQC rating - caring.png", 
      type="png",
      units="in", 
      width=4, 
      height=6, 
      pointsize=1, 
      dpi=1200)

ggplot(data.all, aes(x = Caring, y = PERCENT.OVER65)) +
  geom_boxplot(size = 0.45, alpha = 1,  aes(color=Caring)) +
  geom_jitter(alpha = .15, size = 0.05, aes(color=Caring)) +
  #facet_grid(pared ~ public, margins = TRUE) +
  scale_color_manual(breaks = c("Outstanding", "Good", "Requires improvement",
                                "Inadequate", "Insufficient evidence to rate"),
                     values = c("#A8E6CE", "#DCEDC2", "#FFD3B5",
                                "#FFAAA6", "#FF8C94")) +
  labs(x = "\nRating",
       y = "Percentage of patients over 65 years\n",
       title = "Percentage over 65 years by CQC rating:\nCaring",
       #caption = "Data source: Care Quality Commission, Nov 2021"
  ) +
  theme_bw() +
  theme(panel.grid.major.y = element_blank(),
        legend.position = "off") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

dev.off()

Cairo(file="Figure 3D. Percent age over 65yr by CQC rating - effective.png", 
      type="png",
      units="in", 
      width=4, 
      height=6, 
      pointsize=1, 
      dpi=1200)

ggplot(data.all, aes(x = Effective, y = PERCENT.OVER65)) +
  geom_boxplot(size = 0.45, alpha = 1,  aes(color=Effective)) +
  geom_jitter(alpha = .15, size = 0.05, aes(color=Effective)) +
  #facet_grid(pared ~ public, margins = TRUE) +
  scale_color_manual(breaks = c("Outstanding", "Good", "Requires improvement",
                                "Inadequate", "Insufficient evidence to rate"),
                     values = c("#A8E6CE", "#DCEDC2", "#FFD3B5",
                                "#FFAAA6", "#FF8C94")) +
  labs(x = "\nRating",
       y = "Percentage of patients over 65 years\n",
       title = "Percentage over 65 years by CQC rating:\nEffective",
       #caption = "Data source: Care Quality Commission, Nov 2021"
  ) +
  theme_bw() +
  theme(panel.grid.major.y = element_blank(),
        legend.position = "off") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

dev.off()

Cairo(file="Figure 3E. Percent age over 65yr by CQC rating - well-led.png", 
      type="png",
      units="in", 
      width=4, 
      height=6, 
      pointsize=1, 
      dpi=1200)

ggplot(data.all, aes(x = Well.led, y = PERCENT.OVER65)) +
  geom_boxplot(size = 0.45, alpha = 1,  aes(color=Well.led)) +
  geom_jitter(alpha = .15, size = 0.05, aes(color=Well.led)) +
  #facet_grid(pared ~ public, margins = TRUE) +
  scale_color_manual(breaks = c("Outstanding", "Good", "Requires improvement",
                                "Inadequate", "Insufficient evidence to rate"),
                     values = c("#A8E6CE", "#DCEDC2", "#FFD3B5",
                                "#FFAAA6", "#FF8C94")) +
  labs(x = "\nRating",
       y = "Percentage of patients over 65 years\n",
       title = "Percentage over 65 years by CQC rating:\nWell-led",
       #caption = "Data source: Care Quality Commission, Nov 2021"
  ) +
  theme_bw() +
  theme(panel.grid.major.y = element_blank(),
        legend.position = "off") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

dev.off()

Cairo(file="Figure 3F. Percent age over 65yr by CQC rating - responsive.png", 
      type="png",
      units="in", 
      width=4, 
      height=6, 
      pointsize=1, 
      dpi=1200)

ggplot(data.all, aes(x = Responsive, y = PERCENT.OVER65)) +
  geom_boxplot(size = 0.45, alpha = 1,  aes(color=Responsive)) +
  geom_jitter(alpha = .15, size = 0.05, aes(color=Responsive)) +
  #facet_grid(pared ~ public, margins = TRUE) +
  scale_color_manual(breaks = c("Outstanding", "Good", "Requires improvement",
                                "Inadequate", "Insufficient evidence to rate"),
                     values = c("#A8E6CE", "#DCEDC2", "#FFD3B5",
                                "#FFAAA6", "#FF8C94")) +
  labs(x = "\nRating",
       y = "Percentage of patients over 65 years\n",
       title = "Percentage over 65 years by CQC rating:\nResponsive",
       #caption = "Data source: Care Quality Commission, Nov 2021"
  ) +
  theme_bw() +
  theme(panel.grid.major.y = element_blank(),
        legend.position = "off") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

dev.off()

# 4. Percent males distribution by CQC ratings ---------

Cairo(file="Figure 4A. Percent males by CQC rating - overall.png", 
      type="png",
      units="in", 
      width=4, 
      height=6, 
      pointsize=1, 
      dpi=1200)

ggplot(data.all, aes(x = Overall, y = PERCENT.MALE)) +
  geom_boxplot(size = 0.45, alpha = 1,  aes(color=Overall)) +
  geom_jitter(alpha = .15, size = 0.05, aes(color=Overall)) +
  #facet_grid(pared ~ public, margins = TRUE) +
  scale_color_manual(breaks = c("Outstanding", "Good", "Requires improvement",
                                "Inadequate", "Insufficient evidence to rate"),
                     values = c("#A8E6CE", "#DCEDC2", "#FFD3B5",
                                "#FFAAA6", "#FF8C94")) +
  labs(x = "\nRating",
       y = "Percentage of male patients\n",
       title = "Percentage males by CQC rating:\nOverall",
       #caption = "Data source: Care Quality Commission, Nov 2021"
  ) +
  theme_bw() +
  theme(panel.grid.major.y = element_blank(),
        legend.position = "off") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

dev.off()

Cairo(file="Figure 4B. Percent males by CQC rating - safe.png", 
      type="png",
      units="in", 
      width=4, 
      height=6, 
      pointsize=1, 
      dpi=1200)

ggplot(data.all, aes(x = Safe, y = PERCENT.MALE)) +
  geom_boxplot(size = 0.45, alpha = 1,  aes(color=Safe)) +
  geom_jitter(alpha = .15, size = 0.05, aes(color=Safe)) +
  #facet_grid(pared ~ public, margins = TRUE) +
  scale_color_manual(breaks = c("Outstanding", "Good", "Requires improvement",
                                "Inadequate", "Insufficient evidence to rate"),
                     values = c("#A8E6CE", "#DCEDC2", "#FFD3B5",
                                "#FFAAA6", "#FF8C94")) +
  labs(x = "\nRating",
       y = "Percentage of male patients\n",
       title = "Percentage males by CQC rating:\nSafe",
       #caption = "Data source: Care Quality Commission, Nov 2021"
  ) +
  theme_bw() +
  theme(panel.grid.major.y = element_blank(),
        legend.position = "off") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

dev.off()

Cairo(file="Figure 4C. Percent males by CQC rating - caring.png", 
      type="png",
      units="in", 
      width=4, 
      height=6, 
      pointsize=1, 
      dpi=1200)

ggplot(data.all, aes(x = Caring, y = PERCENT.MALE)) +
  geom_boxplot(size = 0.45, alpha = 1,  aes(color=Caring)) +
  geom_jitter(alpha = .15, size = 0.05, aes(color=Caring)) +
  #facet_grid(pared ~ public, margins = TRUE) +
  scale_color_manual(breaks = c("Outstanding", "Good", "Requires improvement",
                                "Inadequate", "Insufficient evidence to rate"),
                     values = c("#A8E6CE", "#DCEDC2", "#FFD3B5",
                                "#FFAAA6", "#FF8C94")) +
  labs(x = "\nRating",
       y = "Percentage of male patients\n",
       title = "Percentage males by CQC rating:\nCaring",
       #caption = "Data source: Care Quality Commission, Nov 2021"
  ) +
  theme_bw() +
  theme(panel.grid.major.y = element_blank(),
        legend.position = "off") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

dev.off()

Cairo(file="Figure 4D. Percent males by CQC rating - effective.png", 
      type="png",
      units="in", 
      width=4, 
      height=6, 
      pointsize=1, 
      dpi=1200)

ggplot(data.all, aes(x = Effective, y = PERCENT.MALE)) +
  geom_boxplot(size = 0.45, alpha = 1,  aes(color=Effective)) +
  geom_jitter(alpha = .15, size = 0.05, aes(color=Effective)) +
  #facet_grid(pared ~ public, margins = TRUE) +
  scale_color_manual(breaks = c("Outstanding", "Good", "Requires improvement",
                                "Inadequate", "Insufficient evidence to rate"),
                     values = c("#A8E6CE", "#DCEDC2", "#FFD3B5",
                                "#FFAAA6", "#FF8C94")) +
  labs(x = "\nRating",
       y = "Percentage of male patients\n",
       title = "Percentage males by CQC rating:\nEffective",
       #caption = "Data source: Care Quality Commission, Nov 2021"
  ) +
  theme_bw() +
  theme(panel.grid.major.y = element_blank(),
        legend.position = "off") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

dev.off()

Cairo(file="Figure 4E. Percent males by CQC rating - well-led.png", 
      type="png",
      units="in", 
      width=4, 
      height=6, 
      pointsize=1, 
      dpi=1200)

ggplot(data.all, aes(x = Well.led, y = PERCENT.MALE)) +
  geom_boxplot(size = 0.45, alpha = 1,  aes(color=Well.led)) +
  geom_jitter(alpha = .15, size = 0.05, aes(color=Well.led)) +
  #facet_grid(pared ~ public, margins = TRUE) +
  scale_color_manual(breaks = c("Outstanding", "Good", "Requires improvement",
                                "Inadequate", "Insufficient evidence to rate"),
                     values = c("#A8E6CE", "#DCEDC2", "#FFD3B5",
                                "#FFAAA6", "#FF8C94")) +
  labs(x = "\nRating",
       y = "Percentage of male patients\n",
       title = "Percentage males by CQC rating:\nWell-led",
       #caption = "Data source: Care Quality Commission, Nov 2021"
  ) +
  theme_bw() +
  theme(panel.grid.major.y = element_blank(),
        legend.position = "off") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

dev.off()

Cairo(file="Figure 4F. Percent males by CQC rating - responsive.png", 
      type="png",
      units="in", 
      width=4, 
      height=6, 
      pointsize=1, 
      dpi=1200)

ggplot(data.all, aes(x = Responsive, y = PERCENT.MALE)) +
  geom_boxplot(size = 0.45, alpha = 1,  aes(color=Responsive)) +
  geom_jitter(alpha = .15, size = 0.05, aes(color=Responsive)) +
  #facet_grid(pared ~ public, margins = TRUE) +
  scale_color_manual(breaks = c("Outstanding", "Good", "Requires improvement",
                                "Inadequate", "Insufficient evidence to rate"),
                     values = c("#A8E6CE", "#DCEDC2", "#FFD3B5",
                                "#FFAAA6", "#FF8C94")) +
  labs(x = "\nRating",
       y = "Percentage of male patients\n",
       title = "Percentage males by CQC rating:\nResponsive",
       #caption = "Data source: Care Quality Commission, Nov 2021"
  ) +
  theme_bw() +
  theme(panel.grid.major.y = element_blank(),
        legend.position = "off") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

dev.off()


# 5. List size distribution by CQC ratings ---------

Cairo(file="Figure 5A. List size distribution by CQC rating - overall.png", 
      type="png",
      units="in", 
      width=4, 
      height=6, 
      pointsize=1, 
      dpi=1200)

ggplot(data.all, aes(x = Overall, y = LIST.SIZE)) +
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
       #caption = "Data source: Care Quality Commission, Nov 2021"
  ) +
  theme_bw() +
  theme(panel.grid.major.y = element_blank(),
        legend.position = "off") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

dev.off()

Cairo(file="Figure 5B. List size distribution by CQC rating - safe.png", 
      type="png",
      units="in", 
      width=4, 
      height=6, 
      pointsize=1, 
      dpi=1200)

ggplot(data.all, aes(x = Safe, y = LIST.SIZE)) +
  geom_boxplot(size = 0.45, alpha = 1,  aes(color=Safe)) +
  geom_jitter(alpha = .15, size = 0.05, aes(color=Safe)) +
  #facet_grid(pared ~ public, margins = TRUE) +
  scale_color_manual(breaks = c("Outstanding", "Good", "Requires improvement",
                                "Inadequate", "Insufficient evidence to rate"),
                     values = c("#A8E6CE", "#DCEDC2", "#FFD3B5",
                                "#FFAAA6", "#FF8C94")) +
  labs(x = "\nRating",
       y = "List size\n",
       title = "List size by CQC rating:\nSafe",
       #caption = "Data source: Care Quality Commission, Nov 2021"
  ) +
  theme_bw() +
  theme(panel.grid.major.y = element_blank(),
        legend.position = "off") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

dev.off()

Cairo(file="Figure 5C. List size distribution by CQC rating - caring.png", 
      type="png",
      units="in", 
      width=4, 
      height=6, 
      pointsize=1, 
      dpi=1200)

ggplot(data.all, aes(x = Caring, y = LIST.SIZE)) +
  geom_boxplot(size = 0.45, alpha = 1,  aes(color=Caring)) +
  geom_jitter(alpha = .15, size = 0.05, aes(color=Caring)) +
  #facet_grid(pared ~ public, margins = TRUE) +
  scale_color_manual(breaks = c("Outstanding", "Good", "Requires improvement",
                                "Inadequate", "Insufficient evidence to rate"),
                     values = c("#A8E6CE", "#DCEDC2", "#FFD3B5",
                                "#FFAAA6", "#FF8C94")) +
  labs(x = "\nRating",
       y = "List size\n",
       title = "List size by CQC rating:\nCaring",
       #caption = "Data source: Care Quality Commission, Nov 2021"
  ) +
  theme_bw() +
  theme(panel.grid.major.y = element_blank(),
        legend.position = "off") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

dev.off()

Cairo(file="Figure 5D. List size distribution by CQC rating - effective.png", 
      type="png",
      units="in", 
      width=4, 
      height=6, 
      pointsize=1, 
      dpi=1200)

ggplot(data.all, aes(x = Effective, y = LIST.SIZE)) +
  geom_boxplot(size = 0.45, alpha = 1,  aes(color=Effective)) +
  geom_jitter(alpha = .15, size = 0.05, aes(color=Effective)) +
  #facet_grid(pared ~ public, margins = TRUE) +
  scale_color_manual(breaks = c("Outstanding", "Good", "Requires improvement",
                                "Inadequate", "Insufficient evidence to rate"),
                     values = c("#A8E6CE", "#DCEDC2", "#FFD3B5",
                                "#FFAAA6", "#FF8C94")) +
  labs(x = "\nRating",
       y = "List size\n",
       title = "List size by CQC rating:\nEffective",
       #caption = "Data source: Care Quality Commission, Nov 2021"
  ) +
  theme_bw() +
  theme(panel.grid.major.y = element_blank(),
        legend.position = "off") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

dev.off()

Cairo(file="Figure 5E. List size distribution by CQC rating - well-led.png", 
      type="png",
      units="in", 
      width=4, 
      height=6, 
      pointsize=1, 
      dpi=1200)

ggplot(data.all, aes(x = Well.led, y = LIST.SIZE)) +
  geom_boxplot(size = 0.45, alpha = 1,  aes(color=Well.led)) +
  geom_jitter(alpha = .15, size = 0.05, aes(color=Well.led)) +
  #facet_grid(pared ~ public, margins = TRUE) +
  scale_color_manual(breaks = c("Outstanding", "Good", "Requires improvement",
                                "Inadequate", "Insufficient evidence to rate"),
                     values = c("#A8E6CE", "#DCEDC2", "#FFD3B5",
                                "#FFAAA6", "#FF8C94")) +
  labs(x = "\nRating",
       y = "List size\n",
       title = "List size by CQC rating:\nWell-led",
       #caption = "Data source: Care Quality Commission, Nov 2021"
  ) +
  theme_bw() +
  theme(panel.grid.major.y = element_blank(),
        legend.position = "off") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

dev.off()

Cairo(file="Figure 5F. List size distribution by CQC rating - responsive.png", 
      type="png",
      units="in", 
      width=4, 
      height=6, 
      pointsize=1, 
      dpi=1200)

ggplot(data.all, aes(x = Responsive, y = LIST.SIZE)) +
  geom_boxplot(size = 0.45, alpha = 1,  aes(color=Responsive)) +
  geom_jitter(alpha = .15, size = 0.05, aes(color=Responsive)) +
  #facet_grid(pared ~ public, margins = TRUE) +
  scale_color_manual(breaks = c("Outstanding", "Good", "Requires improvement",
                                "Inadequate", "Insufficient evidence to rate"),
                     values = c("#A8E6CE", "#DCEDC2", "#FFD3B5",
                                "#FFAAA6", "#FF8C94")) +
  labs(x = "\nRating",
       y = "List size\n",
       title = "List size by CQC rating:\nResponsive",
       #caption = "Data source: Care Quality Commission, Nov 2021"
  ) +
  theme_bw() +
  theme(panel.grid.major.y = element_blank(),
        legend.position = "off") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

dev.off()


# 6. Regression for CQC ratings - see https://stats.idre.ucla.edu/r/dae/ordinal-logistic-regression/ -----------
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
# data.all$Overall <- factor(data.all$Overall, levels = c("Outstanding", "Good", "Requires improvement", "Inadequate"))
# data.all$Safe <- factor(data.all$Safe, levels = c("Outstanding", "Good", "Requires improvement", "Inadequate"))
# data.all$Caring <- factor(data.all$Caring, levels = c("Outstanding", "Good", "Requires improvement", "Inadequate"))
# data.all$Effective <- factor(data.all$Effective, levels = c("Outstanding", "Good", "Requires improvement", "Inadequate"))
# data.all$Well.led <- factor(data.all$Well.led, levels = c("Outstanding", "Good", "Requires improvement", "Inadequate"))
# data.all$Responsive <- factor(data.all$Responsive, levels = c("Outstanding", "Good", "Requires improvement", "Inadequate"))

data.all$Overall <- factor(data.all$Overall, levels = c("Inadequate","Requires improvement","Good","Outstanding"))
data.all$Safe <- factor(data.all$Safe, levels = c("Inadequate","Requires improvement","Good","Outstanding"))
data.all$Caring <- factor(data.all$Caring, levels = c("Inadequate","Requires improvement","Good","Outstanding"))
data.all$Effective <- factor(data.all$Effective, levels = c("Inadequate","Requires improvement","Good","Outstanding"))
data.all$Well.led <- factor(data.all$Well.led, levels = c("Inadequate","Requires improvement","Good","Outstanding"))
data.all$Responsive <- factor(data.all$Responsive, levels = c("Inadequate","Requires improvement","Good","Outstanding"))

# Fit ordered logit model - UNIVARIATE

# IMD
m <- polr(Overall ~ IMD2019.QUINTILE, data = data.all, Hess=TRUE)
#summary(m)
ctable <- coef(summary(m)) # store table of coefficient results
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2 # calculate p values
(ctable <- cbind(ctable, "p value" = p)) # combine tables HERE: Note p value for any independent variables
ci <- confint.default(m) # Alternative CIs, assuming normality
res1A <- exp(cbind(OR = coef(m), ci)) # Table of ORs and CIs HERE: Note OR and 95% CI for any independent variables
res2A <- ctable

# List size
m <- polr(Overall ~ LIST.SIZE.QUINTILE, data = data.all, Hess=TRUE)
ctable <- coef(summary(m)) # store table of coefficient results
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2 # calculate p values
(ctable <- cbind(ctable, "p value" = p)) # combine tables HERE: Note p value for any independent variables
ci <- confint.default(m) # Alternative CIs, assuming normality
exp(cbind(OR = coef(m), ci)) # Table of ORs and CIs HERE: Note OR and 95% CI for any independent variables
res1B <- exp(cbind(OR = coef(m), ci)) # Table of ORs and CIs HERE: Note OR and 95% CI for any independent variables
res2B <- ctable

# % male
m <- polr(Overall ~ PERCENT.MALE.QUINTILE, data = data.all, Hess=TRUE)
ctable <- coef(summary(m)) # store table of coefficient results
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2 # calculate p values
(ctable <- cbind(ctable, "p value" = p)) # combine tables HERE: Note p value for any independent variables
ci <- confint.default(m) # Alternative CIs, assuming normality
exp(cbind(OR = coef(m), ci)) # Table of ORs and CIs HERE: Note OR and 95% CI for any independent variables
res1C <- exp(cbind(OR = coef(m), ci)) # Table of ORs and CIs HERE: Note OR and 95% CI for any independent variables
res2C <- ctable

# % over65
m <- polr(Overall ~ PERCENT.OVER65, data = data.all, Hess=TRUE)
ctable <- coef(summary(m)) # store table of coefficient results
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2 # calculate p values
(ctable <- cbind(ctable, "p value" = p)) # combine tables HERE: Note p value for any independent variables
ci <- confint.default(m) # Alternative CIs, assuming normality
exp(cbind(OR = coef(m), ci)) # Table of ORs and CIs HERE: Note OR and 95% CI for any independent variables
res1D <- exp(cbind(OR = coef(m), ci)) # Table of ORs and CIs HERE: Note OR and 95% CI for any independent variables
res2D <- ctable

write.csv(rbind(res2A,res2B,res2C,res2D),"Table 1A. Univariate CQC regression raw values - Overall.csv")
write.csv(rbind(res1A,res1B,res1C,res1D),"Table 2A. Univariate CQC regression ORs and CIs - Overall.csv")

# Fir ordered logit model - MULTIVARIATE
m <- polr(Overall ~ IMD2019.QUINTILE + LIST.SIZE.QUINTILE + PERCENT.MALE.QUINTILE + PERCENT.OVER65.QUINTILE, data = data.all, Hess=TRUE)
summary(m)
ctable <- coef(summary(m)) # store table of coefficient results
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2 # calculate p values
ctable <- cbind(ctable, "p value" = p) # combine tables HERE: Note p value for any independent variables
ci <- confint.default(m) # Alternative CIs, assuming normality
res4 <- exp(cbind(OR = coef(m), ci)) # Table of ORs and CIs HERE: Note OR and 95% CI for any independent variables
res3 <- ctable

write.csv(res3,"Table 3A. Multivariate CQC regression raw values - Overall.csv")
write.csv(res4,"Table 4A. Multivariate CQC regression ORs and CIs - Overall.csv")

# IMD
m <- polr(Safe ~ IMD2019.QUINTILE, data = data.all, Hess=TRUE)
#summary(m)
ctable <- coef(summary(m)) # store table of coefficient results
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2 # calculate p values
(ctable <- cbind(ctable, "p value" = p)) # combine tables HERE: Note p value for any independent variables
ci <- confint.default(m) # Alternative CIs, assuming normality
res1A <- exp(cbind(OR = coef(m), ci)) # Table of ORs and CIs HERE: Note OR and 95% CI for any independent variables
res2A <- ctable

# List size
m <- polr(Safe ~ LIST.SIZE.QUINTILE, data = data.all, Hess=TRUE)
ctable <- coef(summary(m)) # store table of coefficient results
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2 # calculate p values
(ctable <- cbind(ctable, "p value" = p)) # combine tables HERE: Note p value for any independent variables
ci <- confint.default(m) # Alternative CIs, assuming normality
exp(cbind(OR = coef(m), ci)) # Table of ORs and CIs HERE: Note OR and 95% CI for any independent variables
res1B <- exp(cbind(OR = coef(m), ci)) # Table of ORs and CIs HERE: Note OR and 95% CI for any independent variables
res2B <- ctable

# % male
m <- polr(Safe ~ PERCENT.MALE.QUINTILE, data = data.all, Hess=TRUE)
ctable <- coef(summary(m)) # store table of coefficient results
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2 # calculate p values
(ctable <- cbind(ctable, "p value" = p)) # combine tables HERE: Note p value for any independent variables
ci <- confint.default(m) # Alternative CIs, assuming normality
exp(cbind(OR = coef(m), ci)) # Table of ORs and CIs HERE: Note OR and 95% CI for any independent variables
res1C <- exp(cbind(OR = coef(m), ci)) # Table of ORs and CIs HERE: Note OR and 95% CI for any independent variables
res2C <- ctable

# % over65
m <- polr(Safe ~ PERCENT.OVER65, data = data.all, Hess=TRUE)
ctable <- coef(summary(m)) # store table of coefficient results
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2 # calculate p values
(ctable <- cbind(ctable, "p value" = p)) # combine tables HERE: Note p value for any independent variables
ci <- confint.default(m) # Alternative CIs, assuming normality
exp(cbind(OR = coef(m), ci)) # Table of ORs and CIs HERE: Note OR and 95% CI for any independent variables
res1D <- exp(cbind(OR = coef(m), ci)) # Table of ORs and CIs HERE: Note OR and 95% CI for any independent variables
res2D <- ctable

write.csv(rbind(res2A,res2B,res2C,res2D),"Table 1B. Univariate CQC regression raw values - Safe.csv")
write.csv(rbind(res1A,res1B,res1C,res1D),"Table 2B. Univariate CQC regression ORs and CIs - Safe.csv")

# Fir ordered logit model - MULTIVARIATE
m <- polr(Safe ~ IMD2019.QUINTILE + LIST.SIZE.QUINTILE + PERCENT.MALE.QUINTILE + PERCENT.OVER65.QUINTILE, data = data.all, Hess=TRUE)
summary(m)
ctable <- coef(summary(m)) # store table of coefficient results
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2 # calculate p values
ctable <- cbind(ctable, "p value" = p) # combine tables HERE: Note p value for any independent variables
ci <- confint.default(m) # Alternative CIs, assuming normality
res4 <- exp(cbind(OR = coef(m), ci)) # Table of ORs and CIs HERE: Note OR and 95% CI for any independent variables
res3 <- ctable

write.csv(res3,"Table 3B. Multivariate CQC regression raw values - Safe.csv")
write.csv(res4,"Table 4B. Multivariate CQC regression ORs and CIs - Safe.csv")

# IMD
m <- polr(Caring ~ IMD2019.QUINTILE, data = data.all, Hess=TRUE)
#summary(m)
ctable <- coef(summary(m)) # store table of coefficient results
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2 # calculate p values
(ctable <- cbind(ctable, "p value" = p)) # combine tables HERE: Note p value for any independent variables
ci <- confint.default(m) # Alternative CIs, assuming normality
res1A <- exp(cbind(OR = coef(m), ci)) # Table of ORs and CIs HERE: Note OR and 95% CI for any independent variables
res2A <- ctable

# List size
m <- polr(Caring ~ LIST.SIZE.QUINTILE, data = data.all, Hess=TRUE)
ctable <- coef(summary(m)) # store table of coefficient results
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2 # calculate p values
(ctable <- cbind(ctable, "p value" = p)) # combine tables HERE: Note p value for any independent variables
ci <- confint.default(m) # Alternative CIs, assuming normality
exp(cbind(OR = coef(m), ci)) # Table of ORs and CIs HERE: Note OR and 95% CI for any independent variables
res1B <- exp(cbind(OR = coef(m), ci)) # Table of ORs and CIs HERE: Note OR and 95% CI for any independent variables
res2B <- ctable

# % male
m <- polr(Caring ~ PERCENT.MALE.QUINTILE, data = data.all, Hess=TRUE)
ctable <- coef(summary(m)) # store table of coefficient results
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2 # calculate p values
(ctable <- cbind(ctable, "p value" = p)) # combine tables HERE: Note p value for any independent variables
ci <- confint.default(m) # Alternative CIs, assuming normality
exp(cbind(OR = coef(m), ci)) # Table of ORs and CIs HERE: Note OR and 95% CI for any independent variables
res1C <- exp(cbind(OR = coef(m), ci)) # Table of ORs and CIs HERE: Note OR and 95% CI for any independent variables
res2C <- ctable

# % over65
m <- polr(Caring ~ PERCENT.OVER65, data = data.all, Hess=TRUE)
ctable <- coef(summary(m)) # store table of coefficient results
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2 # calculate p values
(ctable <- cbind(ctable, "p value" = p)) # combine tables HERE: Note p value for any independent variables
ci <- confint.default(m) # Alternative CIs, assuming normality
exp(cbind(OR = coef(m), ci)) # Table of ORs and CIs HERE: Note OR and 95% CI for any independent variables
res1D <- exp(cbind(OR = coef(m), ci)) # Table of ORs and CIs HERE: Note OR and 95% CI for any independent variables
res2D <- ctable

write.csv(rbind(res2A,res2B,res2C,res2D),"Table 1C. Univariate CQC regression raw values - Caring.csv")
write.csv(rbind(res1A,res1B,res1C,res1D),"Table 2C. Univariate CQC regression ORs and CIs - Caring.csv")

# Fir ordered logit model - MULTIVARIATE
m <- polr(Caring ~ IMD2019.QUINTILE + LIST.SIZE.QUINTILE + PERCENT.MALE.QUINTILE + PERCENT.OVER65.QUINTILE, data = data.all, Hess=TRUE)
summary(m)
ctable <- coef(summary(m)) # store table of coefficient results
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2 # calculate p values
ctable <- cbind(ctable, "p value" = p) # combine tables HERE: Note p value for any independent variables
ci <- confint.default(m) # Alternative CIs, assuming normality
res4 <- exp(cbind(OR = coef(m), ci)) # Table of ORs and CIs HERE: Note OR and 95% CI for any independent variables
res3 <- ctable

write.csv(res3,"Table 3C. Multivariate CQC regression raw values - Caring.csv")
write.csv(res4,"Table 4C. Multivariate CQC regression ORs and CIs - Caring.csv")

# IMD
m <- polr(Effective ~ IMD2019.QUINTILE, data = data.all, Hess=TRUE)
#summary(m)
ctable <- coef(summary(m)) # store table of coefficient results
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2 # calculate p values
(ctable <- cbind(ctable, "p value" = p)) # combine tables HERE: Note p value for any independent variables
ci <- confint.default(m) # Alternative CIs, assuming normality
res1A <- exp(cbind(OR = coef(m), ci)) # Table of ORs and CIs HERE: Note OR and 95% CI for any independent variables
res2A <- ctable

# List size
m <- polr(Effective ~ LIST.SIZE.QUINTILE, data = data.all, Hess=TRUE)
ctable <- coef(summary(m)) # store table of coefficient results
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2 # calculate p values
(ctable <- cbind(ctable, "p value" = p)) # combine tables HERE: Note p value for any independent variables
ci <- confint.default(m) # Alternative CIs, assuming normality
exp(cbind(OR = coef(m), ci)) # Table of ORs and CIs HERE: Note OR and 95% CI for any independent variables
res1B <- exp(cbind(OR = coef(m), ci)) # Table of ORs and CIs HERE: Note OR and 95% CI for any independent variables
res2B <- ctable

# % male
m <- polr(Effective ~ PERCENT.MALE.QUINTILE, data = data.all, Hess=TRUE)
ctable <- coef(summary(m)) # store table of coefficient results
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2 # calculate p values
(ctable <- cbind(ctable, "p value" = p)) # combine tables HERE: Note p value for any independent variables
ci <- confint.default(m) # Alternative CIs, assuming normality
exp(cbind(OR = coef(m), ci)) # Table of ORs and CIs HERE: Note OR and 95% CI for any independent variables
res1C <- exp(cbind(OR = coef(m), ci)) # Table of ORs and CIs HERE: Note OR and 95% CI for any independent variables
res2C <- ctable

# % over65
m <- polr(Effective ~ PERCENT.OVER65, data = data.all, Hess=TRUE)
ctable <- coef(summary(m)) # store table of coefficient results
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2 # calculate p values
(ctable <- cbind(ctable, "p value" = p)) # combine tables HERE: Note p value for any independent variables
ci <- confint.default(m) # Alternative CIs, assuming normality
exp(cbind(OR = coef(m), ci)) # Table of ORs and CIs HERE: Note OR and 95% CI for any independent variables
res1D <- exp(cbind(OR = coef(m), ci)) # Table of ORs and CIs HERE: Note OR and 95% CI for any independent variables
res2D <- ctable

write.csv(rbind(res2A,res2B,res2C,res2D),"Table 1D. Univariate CQC regression raw values - Effective.csv")
write.csv(rbind(res1A,res1B,res1C,res1D),"Table 2D. Univariate CQC regression ORs and CIs - Effective.csv")

# Fir ordered logit model - MULTIVARIATE
m <- polr(Effective ~ IMD2019.QUINTILE + LIST.SIZE.QUINTILE + PERCENT.MALE.QUINTILE + PERCENT.OVER65.QUINTILE, data = data.all, Hess=TRUE)
summary(m)
ctable <- coef(summary(m)) # store table of coefficient results
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2 # calculate p values
ctable <- cbind(ctable, "p value" = p) # combine tables HERE: Note p value for any independent variables
ci <- confint.default(m) # Alternative CIs, assuming normality
res4 <- exp(cbind(OR = coef(m), ci)) # Table of ORs and CIs HERE: Note OR and 95% CI for any independent variables
res3 <- ctable

write.csv(res3,"Table 3D. Multivariate CQC regression raw values - Effective.csv")
write.csv(res4,"Table 4D. Multivariate CQC regression ORs and CIs - Effective.csv")

# IMD
m <- polr(Well.led ~ IMD2019.QUINTILE, data = data.all, Hess=TRUE)
#summary(m)
ctable <- coef(summary(m)) # store table of coefficient results
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2 # calculate p values
(ctable <- cbind(ctable, "p value" = p)) # combine tables HERE: Note p value for any independent variables
ci <- confint.default(m) # Alternative CIs, assuming normality
res1A <- exp(cbind(OR = coef(m), ci)) # Table of ORs and CIs HERE: Note OR and 95% CI for any independent variables
res2A <- ctable

# List size
m <- polr(Well.led ~ LIST.SIZE.QUINTILE, data = data.all, Hess=TRUE)
ctable <- coef(summary(m)) # store table of coefficient results
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2 # calculate p values
(ctable <- cbind(ctable, "p value" = p)) # combine tables HERE: Note p value for any independent variables
ci <- confint.default(m) # Alternative CIs, assuming normality
exp(cbind(OR = coef(m), ci)) # Table of ORs and CIs HERE: Note OR and 95% CI for any independent variables
res1B <- exp(cbind(OR = coef(m), ci)) # Table of ORs and CIs HERE: Note OR and 95% CI for any independent variables
res2B <- ctable

# % male
m <- polr(Well.led ~ PERCENT.MALE.QUINTILE, data = data.all, Hess=TRUE)
ctable <- coef(summary(m)) # store table of coefficient results
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2 # calculate p values
(ctable <- cbind(ctable, "p value" = p)) # combine tables HERE: Note p value for any independent variables
ci <- confint.default(m) # Alternative CIs, assuming normality
exp(cbind(OR = coef(m), ci)) # Table of ORs and CIs HERE: Note OR and 95% CI for any independent variables
res1C <- exp(cbind(OR = coef(m), ci)) # Table of ORs and CIs HERE: Note OR and 95% CI for any independent variables
res2C <- ctable

# % over65
m <- polr(Well.led ~ PERCENT.OVER65, data = data.all, Hess=TRUE)
ctable <- coef(summary(m)) # store table of coefficient results
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2 # calculate p values
(ctable <- cbind(ctable, "p value" = p)) # combine tables HERE: Note p value for any independent variables
ci <- confint.default(m) # Alternative CIs, assuming normality
exp(cbind(OR = coef(m), ci)) # Table of ORs and CIs HERE: Note OR and 95% CI for any independent variables
res1D <- exp(cbind(OR = coef(m), ci)) # Table of ORs and CIs HERE: Note OR and 95% CI for any independent variables
res2D <- ctable

write.csv(rbind(res2A,res2B,res2C,res2D),"Table 1E. Univariate CQC regression raw values - Well.led.csv")
write.csv(rbind(res1A,res1B,res1C,res1D),"Table 2E. Univariate CQC regression ORs and CIs - Well.led.csv")

# Fir ordered logit model - MULTIVARIATE
m <- polr(Well.led ~ IMD2019.QUINTILE + LIST.SIZE.QUINTILE + PERCENT.MALE.QUINTILE + PERCENT.OVER65.QUINTILE, data = data.all, Hess=TRUE)
summary(m)
ctable <- coef(summary(m)) # store table of coefficient results
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2 # calculate p values
ctable <- cbind(ctable, "p value" = p) # combine tables HERE: Note p value for any independent variables
ci <- confint.default(m) # Alternative CIs, assuming normality
res4 <- exp(cbind(OR = coef(m), ci)) # Table of ORs and CIs HERE: Note OR and 95% CI for any independent variables
res3 <- ctable

write.csv(res3,"Table 3E. Multivariate CQC regression raw values - Well.led.csv")
write.csv(res4,"Table 4E. Multivariate CQC regression ORs and CIs - Well.led.csv")

# IMD
m <- polr(Responsive ~ IMD2019.QUINTILE, data = data.all, Hess=TRUE)
#summary(m)
ctable <- coef(summary(m)) # store table of coefficient results
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2 # calculate p values
(ctable <- cbind(ctable, "p value" = p)) # combine tables HERE: Note p value for any independent variables
ci <- confint.default(m) # Alternative CIs, assuming normality
res1A <- exp(cbind(OR = coef(m), ci)) # Table of ORs and CIs HERE: Note OR and 95% CI for any independent variables
res2A <- ctable

# List size
m <- polr(Responsive ~ LIST.SIZE.QUINTILE, data = data.all, Hess=TRUE)
ctable <- coef(summary(m)) # store table of coefficient results
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2 # calculate p values
(ctable <- cbind(ctable, "p value" = p)) # combine tables HERE: Note p value for any independent variables
ci <- confint.default(m) # Alternative CIs, assuming normality
exp(cbind(OR = coef(m), ci)) # Table of ORs and CIs HERE: Note OR and 95% CI for any independent variables
res1B <- exp(cbind(OR = coef(m), ci)) # Table of ORs and CIs HERE: Note OR and 95% CI for any independent variables
res2B <- ctable

# % male
m <- polr(Responsive ~ PERCENT.MALE.QUINTILE, data = data.all, Hess=TRUE)
ctable <- coef(summary(m)) # store table of coefficient results
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2 # calculate p values
(ctable <- cbind(ctable, "p value" = p)) # combine tables HERE: Note p value for any independent variables
ci <- confint.default(m) # Alternative CIs, assuming normality
exp(cbind(OR = coef(m), ci)) # Table of ORs and CIs HERE: Note OR and 95% CI for any independent variables
res1C <- exp(cbind(OR = coef(m), ci)) # Table of ORs and CIs HERE: Note OR and 95% CI for any independent variables
res2C <- ctable

# % over65
m <- polr(Responsive ~ PERCENT.OVER65, data = data.all, Hess=TRUE)
ctable <- coef(summary(m)) # store table of coefficient results
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2 # calculate p values
(ctable <- cbind(ctable, "p value" = p)) # combine tables HERE: Note p value for any independent variables
ci <- confint.default(m) # Alternative CIs, assuming normality
exp(cbind(OR = coef(m), ci)) # Table of ORs and CIs HERE: Note OR and 95% CI for any independent variables
res1D <- exp(cbind(OR = coef(m), ci)) # Table of ORs and CIs HERE: Note OR and 95% CI for any independent variables
res2D <- ctable

write.csv(rbind(res2A,res2B,res2C,res2D),"Table 1F. Univariate CQC regression raw values - Responsive.csv")
write.csv(rbind(res1A,res1B,res1C,res1D),"Table 2F. Univariate CQC regression ORs and CIs - Responsive.csv")

# Fir ordered logit model - MULTIVARIATE
m <- polr(Responsive ~ IMD2019.QUINTILE + LIST.SIZE.QUINTILE + PERCENT.MALE.QUINTILE + PERCENT.OVER65.QUINTILE, data = data.all, Hess=TRUE)
summary(m)
ctable <- coef(summary(m)) # store table of coefficient results
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2 # calculate p values
ctable <- cbind(ctable, "p value" = p) # combine tables HERE: Note p value for any independent variables
ci <- confint.default(m) # Alternative CIs, assuming normality
res4 <- exp(cbind(OR = coef(m), ci)) # Table of ORs and CIs HERE: Note OR and 95% CI for any independent variables
res3 <- ctable

write.csv(res3,"Table 3F. Multivariate CQC regression raw values - Responsive.csv")
write.csv(res4,"Table 4F. Multivariate CQC regression ORs and CIs - Responsive.csv")
