###R code that creates the outputs as shown in the 'beans' app
##by Colleen Nell 

##load packages
library(ggplot2)##for plotting
library(dplyr)#used to calculate summary statistics
library(readr)##this is jsut to get the theme
#get theme
source_url("https://raw.githubusercontent.com/collnell/R_vis/master/theme_mooney.R")#ggplot theme

##read in data
raw.data<-read.csv("~/downloads/beans_data.csv")##change this to the location of your downlaoded file
ttable<-read.csv("https://raw.githubusercontent.com/collnell/beans/master/ttable.csv")###read in T table for T statistic in computing Confidence intervals


###produce histogram/density plot
hist.plot<-ggplot(data=raw.data,aes(x=Proportion,fill=Treatment,color=Treatment))+
  geom_density(alpha=.35)+
  theme_mooney(legend.location="bottom")+
  labs(x="Proportion A",y="Density")+
  scale_fill_manual(values=c("#006666","#FF9900"))+
  scale_color_manual(values=c("#006666","#FF9900"))
hist.plot

##functions to calculate SEM  and 95ci
se <- function(x) sqrt(var(x)/length(x))
ci95 <-function(x) se(x)*ttable[ttable$n == length(x),2] 

###calculate sumamry statistics for 'Proportion' variable
summary.data<-raw.data%>%
  na.omit()%>%
  group_by(Treatment)%>%
  summarize(mean_p = mean(Proportion), 
            N = length(Proportion), 
            SEM=se(Proportion), 
            SD = sd(Proportion), 
            CI = ci95(Proportion),
            CIlow = mean_p-CI,
            CIhigh=mean_p+CI,
            t = ttable[ttable$n == length(Proportion),2])

##plot means as bar plot

bar.plot<-ggplot(summary.data,aes(x=Treatment,y=mean_p,fill=Treatment))+#plot
  geom_bar(stat="identity")+
  theme_mooney()+##delete this to use a basic theme for the plot, or replace with 'theme_minimal()+
  geom_errorbar(aes(ymax=mean_p+SEM,ymin=mean_p-SEM),width=.2)+ ##replace 'SEM' with error value of interest
  labs(x="Treatment",y="Proportion A")+
  scale_fill_manual(values=c("#006666","#FF9900"))+
  theme(legend.position="none")
bar.plot

##one-way ANOVA
aov.model<-aov(Proportion~Treatment,data=raw.data)
summary(aov.model)#print results
