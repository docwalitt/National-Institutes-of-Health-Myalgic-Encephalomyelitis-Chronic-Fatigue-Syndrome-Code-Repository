# Gina Norato; gina.norato@nih.gov

library(plyr)
library(dplyr)
library(tidyr) # separate()
library(stringr) # str_pad()
library(ggplot2)

read.csv(file="source.data.supp.fig.9e-9g.csv")


############
## Supp Fig 9E
#############

d<-subset(t2,timepoint!="Cross-sectional",select=c("analysisID","Group","Basal","timepoint"))
d<-d[complete.cases(d),]

ggplot(data=d,aes(x=timepoint,y=Basal,group=analysisID,colour=Group))+geom_point()+geom_line()+scale_colour_manual(values=c("blue","red"),
                                                                                                                   labels=c("HV","PI-ME/CFS"))+
  theme_classic()+ylab("Basal Mitochondrial\nRespiration (pmol/min)")+xlab("Timepoint (post-exercise)")+
  theme(legend.position="bottom",text = element_text(size=18))


############
## Supp Fig 9F
#############

d<-subset(t2,timepoint!="Cross-sectional",select=c("analysisID","Group","Maximal Respiration","timepoint"))
d<-d[complete.cases(d),]


ggplot(data=d,aes(x=timepoint,y=`Maximal Respiration`,group=analysisID,colour=Group))+geom_point()+geom_line()+scale_colour_manual(values=c("blue","red"),
                                                                                                                                   labels=c("HV","PI-ME/CFS"))+
  theme_classic()+ylab("Mitochondrial Maximum \nRespiration (pmol/min)")+xlab("Timepoint (post-exercise)")+
  theme(legend.position="bottom",text = element_text(size=18))


############
## Supp Fig 9G
#############

d<-subset(t2,timepoint!="Cross-sectional",select=c("analysisID","Group","Non-mitochondrial
Oxygen Consumption","timepoint"))
d<-d[complete.cases(d),]

ggplot(data=d,aes(x=timepoint,y=`Non-mitochondrial\nOxygen Consumption`,group=analysisID,colour=Group))+geom_point()+geom_line()+scale_colour_manual(values=c("blue","red"),
                                                                                                                                                     labels=c("HV","PI-ME/CFS"))+
  theme_classic()+ylab("Non-Mitochontrial Oxygen \nConsumption (pmol/min)")+xlab("Timepoint (post-exercise)")+
  theme(legend.position="bottom",text = element_text(size=18))



