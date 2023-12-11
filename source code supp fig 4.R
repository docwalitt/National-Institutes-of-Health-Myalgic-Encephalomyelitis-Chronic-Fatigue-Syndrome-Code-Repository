# Gina Norato; gina.norato@nih.gov

rm(list=ls())
library(plyr)
library(tidyverse)
library(lme4)
library(lmerTest)
library(multcomp)
library(dplyr)
library(tidyr) # separate()
library(stringr) # str_pad()
library(ggplot2)

##### Supplemental Figure 4 #####

dat<-read.csv(file="source.data.supp.fig.4.csv")

longdat<-dat %>% gather(NE.SUPINE:EPI.40,key = "variable",value = "value") %>% 
  separate(variable,into = c("type","minute"))

longdat<-longdat %>% group_by(N,type) %>% mutate(supine=first(value),
                                                 value.new=value/supine)
longdat$minute2<-ifelse(longdat$minute=="SUPINE",0,longdat$minute)

##################
## Supp fig 4A
###################

ggplot(data=subset(longdat,type=="EPI"),
       aes(x=as.numeric(minute2),y=value.new,group=N,colour=Group))+
  geom_point()+geom_line()+
  theme_classic()+xlab("Minute")+ylab("Epinepherine\n(Normalized to Supine)")+
  scale_colour_manual(values=c("red","blue"),label=c("PI-ME/CFS","HV"))+
  theme(legend.position="bottom",panel.grid = element_blank())

##################
## Supp fig 4B
###################

ggplot(data=subset(longdat,type=="NE"),
       aes(x=as.numeric(minute2),y=value.new,group=N,colour=Group))+
  geom_point()+geom_line()+
  theme_classic()+xlab("Minute")+ylab("Norepinepherine\n(Normalized to Supine)")+
  scale_colour_manual(values=c("red","blue"),label=c("PI-ME/CFS","HV"))+
  theme(legend.position="bottom",panel.grid = element_blank())

##################
## Supp fig 4C 
###################

longdat2<-longdat %>% dplyr::select(-c(value,supine))%>% group_by(N,minute)%>%spread(type,value.new)
longdat2<-longdat2 %>% group_by(N) %>% filter(minute!="SUPINE")%>% arrange(N,as.numeric(minute))%>% mutate(ratio.of.diff=(EPI-lag(EPI))/(NE-lag(NE)),
                                                                                                           diff.of.ratios=(EPI/NE) - (lag(EPI)/lag(NE)))
ggplot(data=longdat2,aes(x=as.numeric(minute),y=ratio.of.diff,group=N,colour=Group))+
  geom_point()+geom_line()+theme_classic()+xlab("Minute")+
  ylab(expression(atop(underline(Epi[t]~-Epi[(t-1)]),NE[t]~-NE[(t-1)])))+
  coord_cartesian(ylim=c(-250,250))+
  scale_colour_manual(values=c("red","blue"),labels=c("PI-ME/CFS","HV"))+
  theme(panel.grid = element_blank(),legend.position=c(0.7,0.05),
        legend.direction = "horizontal")+xlim(0,40)


##### Supplemental Figure 4 #####

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




