#Sophia Mucciolo
#data analysis for Aphis nerii EPG
#edited 1.18.2024

#download data
epg<-read.csv("https://raw.githubusercontent.com/smucciolo/A.nerii_choice_2022/main/neriiEPGcsv.csv")
library(dplyr)

#Do A. nerii feed differently between milkweed species?
  #specifically, interested in % time salivating (E1), 
  #duration xylem feeding (G)/salivation/ingestion (E2), time to sustained ingestion (E2)

#want to compare means between 3 groups (milkweed species)

#checking assumptions for one-way ANOVA ####
  #% time E1
hist(epg$percentE1)
shapiro.test(epg$percentE1)
    #not normal (p<0.01)

  #duration E2
hist(epg$durE2)
shapiro.test(epg$durE2)
    #not normal

  #duration G
hist(epg$durG)
shapiro.test(epg$durG)
    #not normal

  #duration E1
hist(epg$durE1)
shapiro.test(epg$durE1)
    #not normal

  #time to sustained E2
hist(epg$timeprobetosustE2)
shapiro.test(epg$timeprobetosustE2)
  #not normal
#None of these variables meet ANOVA assumptions --> use Kruskal-Wallis

#running tests ####

#Does mean % time spent salivating vary between species?
kruskal.test(percentE1~trt,data=epg)
  #p=0.3088 -> accept null hypothesis (no difference)
group_by(epg,trt) %>%
  summarise(median = median(percentE1),
            IQR = IQR(percentE1))
  #summary statistics to report
  
#Does mean duration ingesting vary between species?
kruskal.test(durE2 ~ trt,data=epg)
  #p=0.8912 -> accept null hypothesis (no difference)
group_by(epg,trt) %>%
  summarise(median = median(durE2),
            IQR = IQR(durE2))
  #summary statistics to report

#Does mean duration xylem feeding vary between species?
kruskal.test(durG ~ trt,data=epg)
  #p=0.6103 -> accept null hypothesis (no difference)
group_by(epg,trt) %>%
  summarise(median = median(durG),
            IQR = IQR(durG))
  #summary statistics for reporting

#Does mean duration salivating vary between species?
kruskal.test(durE1 ~ trt,data=epg)
  #p=0.1079 -> accept null hypothesis (no difference) 
group_by(epg,trt) %>%
  summarise(median = median(durE1),
            IQR = IQR(durE1))
  #summary statistics for reporting

#Does mean time spent probing before ingestion >10 minutes vary between species?
kruskal.test(timeprobetosustE2~trt,data=epg)
  #p=0.8782 -> accept null hypothesis (no difference)
group_by(epg,trt) %>%
  summarise(median = median(timeprobetosustE2),
            IQR = IQR(timeprobetosustE2))

#takeaway ####
#no significant differences in feeding styles/behavior between milkweed species
#had 14 replicates per treatment in these analyses

