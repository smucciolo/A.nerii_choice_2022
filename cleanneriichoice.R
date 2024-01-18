#Sophia Mucciolo
#data analysis for Aphis nerii host selection & impact on reproduction
#edited 1.18.2024

choiceraw<-read.csv("https://raw.githubusercontent.com/smucciolo/A.nerii_choice_2022/main/combined_nerii.csv")

library(MASS)
library(tidyverse)

#cleaning data ####

#changing variable types
choice<-choiceraw
choice$CAGE<-as.character(choice$CAGE)
choice$TIME<-as.character(choice$TIME)

#excluding 168 hour time point (start of second generation data)
firstgen<-filter(choice,choice$TIME !="168")

#MY QUESTIONS:
  #1) Do aphids prefer different milkweed species?
  #2) Does aphid reproduction vary between milkweed species?

#Question 1 ####

#Does the mean number of adults on each milkweed species vary?
  #using this as a measure of preference, because aphids could move between the species

#use One Way ANOVA
  #assumptions: normal residuals, homogeneous variance
  #null hypothesis: mean number of adults on each species is equal

#analyze morphs separately
alate<-filter(choice,choice$MORPH !="APTEROUS")
apterous<-filter(choice,choice$MORPH !="ALATE")

#alates
alate.anova<-aov(ADULT~SPECIES,data=alate)
summary(alate.anova)
  #species does seem to have a significant impact on mean adults (reject null)
#checking assumptions of this model
shapiro.test(resid(alate.anova))
  #p<0.05 --> RESIDUALS NOT NORMAL!!!!!
fligner.test(alate$ADULT~alate$SPECIES)
  #p<0.05 --> VARIANCE NOT HOMOGENEOUS!!!!!
#need to use non-parametric alternative: Kruskal-Wallis
kruskal.test(ADULT~SPECIES,data=alate)
  #species does have a significant impact on number of adults (still reject null)
  #p<0.001
pairwise.wilcox.test(alate$ADULT,alate$SPECIES,paired=FALSE)
  #pairwise comparisons show significance!

#apterae
apterous.anova<-aov(ADULT~SPECIES,data=apterous)
summary(apterous.anova)
  #species seems to have a significant impact on mean adults (reject null)
#checking model assumptions
shapiro.test(resid(apterous.anova))
  #p<0.05 --> RESIDUALS NOT NORMAL!!!!
fligner.test(apterous$ADULT~apterous$SPECIES)
  #p<0.05 --> VARIANCE NOT HOMOGENEOUS!!
#need to use non-parametric alternative: Kruskal-Wallis 
kruskal.test(ADULT~SPECIES,data=apterous)
  #species does have a significant impact on number of adults (still reject null)
  #p<0.001
pairwise.wilcox.test(apterous$ADULT,apterous$SPECIES,paired=FALSE)
  #significant differences between every pair

#Question 2 ####

#Does the mean number of nymphs/adult vary with milkweed species?

#use One Way ANOVA
  #model assumptions: normal residuals, homogeneous variance

#need to make a nymphs per adult column 
firstgen$divide<-firstgen$NYMPH / firstgen$ADULT
#filter out Inf and NaN from that divide
firstgendivideclean<-filter(firstgen,firstgen$divide !="NaN")
firstgendivideclean<-filter(firstgendivideclean,firstgendivideclean$divide !="Inf")

#again, analyzing by morph
alate.nymphs<-filter(firstgendivideclean,firstgendivideclean$MORPH !="APTEROUS")
apterous.nymphs<-filter(firstgendivideclean,firstgendivideclean$MORPH !="ALATE")

#alates
alate.nymph.anova<-aov(divide~SPECIES,data=alate.nymphs)
summary(alate.nymph.anova)
  #p=0.735 -> accept null hypothesis of no difference
shapiro.test(resid(alate.nymph.anova))
  #p<0.01 -> RESIDUALS NOT NORMAL!!!
fligner.test(alate.nymphs$divide~alate.nymphs$SPECIES)
  #p=0.772 -> variance is homogeneous
#non normal residuals -> try non-parametric alternative (Kruskal Wallis)
kruskal.test(divide~SPECIES,data=alate.nymphs)
  #p=0.7338 -> accept null hypothesis of no difference in reproduction between species!

#apterae
apterous.nymph.anova<-aov(divide~SPECIES,data=apterous.nymphs)
summary(apterous.nymph.anova)
  #p=0.622 --> accept null hypothesis of no difference
shapiro.test(resid(apterous.nymph.anova))
  #p<0.01 --> RESIDUALS NOT NORMAL!!!
fligner.test(apterous.nymphs$divide~apterous.nymphs$SPECIES)
  #p=0.241 --> variance is homogeneous
#non normal residuals -> try non-parametric alternative (Kruskal Wallis)
kruskal.test(divide~SPECIES,data=apterous.nymphs)
  #p=0.7654 -> accept null hypothesis of no diff in reproduction between species 


#Does plant height matter for host selection? ####

#Q: Does plant height impact the number of adults on milkweed plants?

#checking alate distribution
var(alate$ADULT)
  #1.124
mean(alate$ADULT)
  #0.936
#var>mean = negative binomial distribution!

#checking apterous distribution
var(apterous$ADULT)
  #1.579
mean(apterous$ADULT)
  #0.965
#var>mean = negative binomial distribution!

#alates
alate.height.glm<-glm.nb(ADULT~HEIGHT*SPECIES+CAGE,data=alate)
anova(alate.height.glm,test="Rao")
#height p=0.04, species p<0.01
#plant height had a significant impact on number of adult aphids on the plants
#the interaction between height and species did not have a significant impact!

#apterae
apterous.height.glm<-glm.nb(ADULT~HEIGHT*SPECIES+CAGE,data=apterous)
anova(apterous.height.glm,test="Rao")
#height does not impact apterae host choice


#graphing ####

#box plot for initial choice
ggplot(firstgen,aes(x=SPECIES,y=ADULT,fill=MORPH))+geom_boxplot()+
  labs(x="Milkweed Species",y="Adult Aphids per Plant",title="Aphid host species choice")+
  scale_fill_manual(name="Morphology",values=c("ALATE"="darkgray","APTEROUS"="white"))+
  scale_color_manual(name="Morphology",values=c("ALATE"="black","APTEROUS"="black"))+theme_bw()+
  theme(text=element_text(family="Avenir",size=25),legend.position="none",axis.title=element_text(size=36),plot.title=element_text(size=52),
        panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  scale_x_discrete(labels=c("INC"=expression(italic("A. incarnata")),
                            "SYR"=expression(italic("A. syriaca")),
                            "TUB"=expression(italic("A. tuberosa"))))+
  facet_grid(cols=vars(MORPH))

#box plot for reproduction
ggplot(firstgendivideclean,aes(x=SPECIES,y=divide,fill=MORPH,color=MORPH))+
  geom_boxplot()+
  labs(x="Milkweed Species",y="Nymphs per Adult",title="Impact of host species on reproduction")+
  scale_fill_manual(name="Morphology",values=c("ALATE"="darkgray","APTEROUS"="white"))+
  scale_color_manual(name="Morphology",values=c("ALATE"="black","APTEROUS"="black"))+theme_bw()+
  theme(text=element_text(family="Avenir",size=25),legend.position="none",
        axis.title=element_text(size=36),plot.title=element_text(size=52),
        legend.title=element_text(size=25),
        panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  facet_grid(cols=vars(MORPH))+
  scale_x_discrete(labels=c("INC"=expression(italic("A. incarnata")),
                            "SYR"=expression(italic("A. syriaca")),
                            "TUB"=expression(italic("A. tuberosa"))))

