# Gina Norato; gina.norato@nih.gov

library(plyr)
library(tidyverse)

dat<-read.csv(file="Z://Office of Biostatistics/Analyses/16-N-0058/supp table 7 data.csv")
dat$group<-ifelse(dat$group==1,"HV","CFS")

res.dat<-data.frame(vars=vars,mean.hv=NA,sd.hv=NA,mean.cfs=NA,sd.cfs=NA,p=NA)

vars<-tail(head(names(dat),-1),-1)

for(i in 1:length(vars)){
 d<- dat %>% group_by(group)%>%summarize(mean=mean(get(vars[i]),na.rm=T),
                                      sd=sd(get(vars[i]),na.rm=T)) %>%
    as.data.frame()
 res.dat[i,"mean.hv"]<-d[which(d$group=="HV"),"mean"]
 res.dat[i,"sd.hv"]<-d[which(d$group=="HV"),"sd"]
 res.dat[i,"mean.cfs"]<-d[which(d$group=="CFS"),"mean"]
 res.dat[i,"sd.cfs"]<-d[which(d$group=="CFS"),"sd"]
 
 res.dat[i,"p"]<-wilcox.test(data=dat,get(vars[i])~group)$p.value
}

write.csv(res.dat,file="Z://Office of Biostatistics/Analyses/16-N-0058/supp table 7 results.csv",
          row.names=F)
