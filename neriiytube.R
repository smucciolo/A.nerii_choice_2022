#Sophia Mucciolo
#data analysis for Aphis nerii y tube olfactory experiment
#edited 1.18.2024


ytubes<-read.csv("https://raw.githubusercontent.com/smucciolo/A.nerii_choice_2022/main/ytubesFINAL.csv")
#
library(tidyverse)
#
#
#THE QUESTION: does Aphis nerii use odor cues to choose between host plants?

#y tube data comparing proportion of aphids that made one of 2 choices
  #-> chi square

#null hypothesis: no diff in the number of aphids that choose odor cues of each species
#alternative hypothesis: diff number of aphids choose odor cues of each species

#important info about experimental groups:
  #tested alates and apterae separately
  #also wanted to make sure original host plant did not impact choice
  #so, 4 groups: 
    #alates born on A. incarnata 
    #apterae born on A. incarnata
    #alates born on A. syriaca
    #apterae born on A. syriaca

#for each group, also check to see if one side of the tube was preferred or not
  #methodology should have prevented this from happening

#chi squares ####
#alates born on A. incarnata (syr,inc)
alateINCobserved<-c(12,12)
alateINCexpected<-c(0.5,0.5)
chisq.test(x=alateINCobserved,p=alateINCexpected)
  #exact same proportions, p=1
  #accept null hypothesis: no difference in choice
#checking tube side preference (left,right)
alateINCtubeobs<-c(13,11)
alateINCtubeexp<-c(0.5,0.5)
chisq.test(x=alateINCtubeobs,p=alateINCtubeexp)
  #p=0.6831 -> accept null hypothesis of no difference

#apterae born on A. incarnata (syr,inc)
apteraeINCobserved<-c(13,8)
apteraeINCexpected<-c(0.5,0.5)
chisq.test(x=apteraeINCobserved,p=apteraeINCexpected)
  #p=0.2752 -> accept null hypothesis of no significant difference
#checking tube side preference (left,right)
apteraeINCtubeobs<-c(12,9)
apteraeINCtubeexp<-c(0.5,0.5)
chisq.test(x=apteraeINCtubeobs,p=apteraeINCtubeexp)
  #p=0.5127 -> accept null hypothesis of no difference

#alates born on A. syriaca (syr,inc)
alateSYRobserved<-c(11,11)
alateSYRexpected<-c(0.5,0.5)
chisq.test(x=alateSYRobserved,p=alateSYRexpected)
  #exact same proportions, p=1
  #accept null hypothesis of no difference 
#checking tube side preference (left,right)
alateSYRtubeobs<-c(11,11)
alateSYRtubeexp<-c(0.5,0.5)
chisq.test(x=alateSYRtubeobs,p=alateSYRtubeexp)
  #same proportions, p=1
  #accept null hypothesis of no difference 

#apterae born on A. syriaca (syr,inc)
apteraeSYRobserved<-c(7,12)
apteraeSYRexpected<-c(0.5,0.5)
chisq.test(x=apteraeSYRobserved,p=apteraeSYRexpected)
  #p=.2513 -> accept null hypothesis of no difference
#checking tube side preference (left,right)
apteraeSYRtubeobs<-c(10,9)
apteraeSYRtubeexp<-c(0.5,0.5)
chisq.test(x=apteraeSYRtubeobs,p=apteraeSYRtubeexp)
  #p=0.8185 -> accept null hypothesis of no difference

#graphing ####
#GOAL: horizontal mirrored bar chart showing number of aphids between the two treatments

#need to collapse CHOSESYR and CHOSEINC variables into one variable showing choice
#negate all observations for CHOSESYR --> mirrored chart
negsyr<-ytubes
negsyr$CHOSESYR<-negsyr$CHOSESYR*(-1)
#now collapsing into one variable for the y axis
negsyr$choicesum<-negsyr$CHOSESYR+negsyr$CHOSEINC

#CHOICE = fill/color. need to filter out NAs
negsyr<-filter(negsyr,negsyr$CHOICE !="NA")

#plot for poster
ggplot(negsyr,aes(y=INITIAL,x=choicesum,fill=CHOICE))+
  stat_summary(fun=sum,geom="col",colour="black")+
  facet_grid(rows=vars(MORPH))+
  labs(x="Number of Aphids per Treatment Arm",y="Initial host plant")+
  theme_bw()+
  theme(text=element_text(family="Avenir",size=25),legend.position="top",
        axis.title=element_text(size=36),legend.title=element_blank(),
        plot.title=element_text(size=52),
        panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  scale_x_continuous(labels=abs)+
  scale_y_discrete(labels=c("INC"=expression(italic("A. incarnata")),
                            "SYR"=expression(italic("A. syriaca"))))+
  scale_fill_manual(labels=c(expression(italic("A. incarnata")),
                             expression(italic("A. syriaca"))),
                      values=c("darkolivegreen3","orange1"))

#plot for manuscript
ggplot(negsyr,aes(y=INITIAL,x=choicesum,fill=CHOICE))+
  stat_summary(fun=sum,geom="col",colour="black")+
  facet_grid(rows=vars(MORPH))+
  labs(x="Number of Aphids per Treatment Arm",y="Initial host plant")+
  theme_bw()+
  theme(text=element_text(family="Avenir",size=25),legend.position="top",
        axis.title=element_text(size=36),legend.title=element_blank(),
        plot.title=element_text(size=52),
        panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  scale_x_continuous(labels=abs)+
  scale_y_discrete(labels=c("INC"=expression(italic("A. incarnata")),
                            "SYR"=expression(italic("A. syriaca"))))+
  scale_fill_manual(labels=c(expression(italic("A. incarnata")),
                             expression(italic("A. syriaca"))),
                    values=c("darkgray","white"))

















                                                



