setwd(paste(getwd(),'/udacity/data_analysis',sep = ""))
list.files()

#install.packages('ggthemes', dependencies = TRUE) 
library(ggthemes) 
theme_set(theme_minimal()) 

pf <- read.csv('pseudo_facebook.tsv',sep = '\t')
names(pf)

#ggplot
library(ggplot2)

qplot(x= dob_day, data = pf)+
  scale_x_continuous(breaks = 1:31)+
  facet_wrap(~dob_month, ncol = 3)

# same plot, different syntax
ggplot(data = pf, aes(x = dob_day)) + 
  geom_histogram(binwidth = 1) + 
  scale_x_continuous(breaks = 1:31) + 
  facet_wrap(~dob_month)


ggplot(aes(x = dob_day), data = pf) + 
  geom_histogram(binwidth = 1) + 
  scale_x_continuous(breaks = 1:31)

# friend_count

qplot(x = friend_count, data = pf,xlim = c(0,1000))

qplot(x = friend_count, data = subset(pf,!is.na(gender)),binwidth = 25)+
  scale_x_continuous(limits = c(0,1000),breaks = seq(0,1000,50))

ggplot(aes(x = friend_count), data = na.omit(pf)) + 
  geom_histogram() + 
  scale_x_continuous(limits = c(0, 1000))

# split by gender

qplot(x = friend_count, data = pf,binwidth = 25)+
  geom_histogram()+
  facet_wrap(~gender)

ggplot(aes(x = friend_count), data = pf) + 
  geom_histogram(binwidth = 25) + 
  scale_x_continuous(limits = c(0, 1000), breaks = seq(0, 1000, 50))

qplot(x = friend_count, data = pf, binwidth = 25) + 
  scale_x_continuous(limits = c(0, 1000), breaks = seq(0, 1000, 50))

# facet_grid and facet_warp

qplot(x = friend_count, data = pf) + 
  facet_grid(gender ~ .) 

ggplot(aes(x = friend_count), data = pf) + 
  geom_histogram() + 
  scale_x_continuous(limits = c(0, 1000), breaks = seq(0, 1000, 50)) + 
  facet_wrap(~gender)

# omitting NA
ggplot(aes(x = friend_count), data = subset(pf, !is.na(gender))) + 
  geom_histogram() + 
  scale_x_continuous(limits = c(0, 1000), breaks = seq(0, 1000, 50)) + 
  facet_wrap(~gender)

ggplot(aes(x = friend_count), data = na.omit(pf)) + 
  geom_histogram() + 
  scale_x_continuous(limits = c(0, 1000), breaks = seq(0, 1000, 50)) + 
  facet_wrap(~gender)

## Tenure

qplot(x = tenure, data = pf, binwidth = 25,
      color = I('black'), fill = I('#099DD9'))

qplot(x = tenure/365, data = pf, binwidth = 0.25,
      color = I('black'), fill = I('#F79420'))+
  scale_x_continuous(breaks = seq(1,7,1),limits = c(0,7))


## Labeling Plots

ggplot(aes(x = tenure / 365), data = pf) + 
  geom_histogram(color = 'black', fill = '#F79420') + 
  scale_x_continuous(breaks = seq(1, 7, 1), limits = c(0, 7)) + 
  xlab('Number of years using Facebook') + 
  ylab('Number of users in sample')

## User Ages

qplot(x = age , data = pf, binwidth = 1,
      xlab = 'Age of facebook user',
      ylab = 'Number of user in sample',
      color = I('black'), fill = I('#F79420'))+
  scale_x_continuous(breaks = seq(0,113,10))
# how do i know maximum is 113? the answer is to use summary(pf$age)
# solution
ggplot(aes(x = age), data = pf) + 
  geom_histogram(binwidth = 1, fill = '#5760AB') + 
  scale_x_continuous(breaks = seq(0, 113, 5))

## Transforming data

#install.packages("gridExtra")
library(gridExtra)
plot1 = ggplot(aes(x = friend_count),data =pf)+
  geom_histogram(binwidth = 1,fill = '#5760AB')+
  scale_x_continuous()
plot2 = qplot(x = friend_count, data = pf,binwidth = 0.25)+
  scale_x_log10()
plot3 = ggplot(aes(x=friend_count),data = pf)+
  geom_histogram(binwidth=1, fill='#5760AB')+
  scale_x_sqrt()
grid.arrange(plot1,plot2,plot3,ncol=1)

## frequency polygons

# by gender, i wanna compare
plot = qplot(x = www_likes,data = subset(pf,!is.na(gender)),binwidth = 10)+
  scale_x_continuous(lim = c(0,1000),breaks = seq(0,1000,50))+
  facet_wrap(~gender)

# can compare but why don't we compapre in same plot
plot = qplot(x = www_likes,data = subset(pf,!is.na(gender)),geom = 'freqpoly', color = gender)+
  scale_x_log10()
# ggploy solution
ggplot(aes(x = friend_count, y = ..count../sum(..count..)), data = subset(pf, !is.na(gender))) + 
  geom_freqpoly(aes(color = gender), binwidth=10) + 
  scale_x_continuous(limits = c(0, 1000), breaks = seq(0, 1000, 50)) + 
  xlab('Friend Count') + 
  ylab('Percentage of users with that friend count')

ggplot(aes(x = www_likes), data = subset(pf, !is.na(gender))) + 
  geom_freqpoly(aes(color = gender)) + 
  scale_x_log10()

##
aggregate(pf$www_likes,by=list(gender=pf$gender),FUN='sum')
# or
by(pf$www_likes,pf$gender,sum)

## boxplots
qplot(x = gender, y = friend_count, data = subset(pf,!is.na(gender)),geom = 'boxplot',ylim = c(0,1000))
#or
qplot(x = gender, y = friend_count, data = subset(pf,!is.na(gender)),geom = 'boxplot')+
  scale_y_continuous(limits = c(0,1000))
# but above both are the result after removing 2949 rows, so the proportion of this data is changed.
# this is not what we want. we want to just zoom into between 0 and 1000 of y axis. 
qplot(x = gender, y = friend_count, data = subset(pf,!is.na(gender)),geom = 'boxplot')+
  coord_cartesian(ylim = c(0,1000))

## boxplots,quantiles
by(pf$friendships_initiated,pf$gender,summary)
qplot(x = gender,y = friendships_initiated,
      data = subset(pf, !is.na(gender)),geom = 'boxplot')+
  coord_cartesian(ylim = c(0,50))

##getting logical

pf$mobile_check_in <-NA
pf$mobile_check_in <- ifelse(pf$mobile_likes>0,1,0)
pf$mobile_check_in <- factor(pf$mobile_check_in)
summary(pf$mobile_check_in)
prop.table(summary(pf$mobile_check_in))


qplot(x = price, data = subset(diamonds,!is.na(cut)),
      geom = 'freqpoly',binwidth=1,breaks=seq(0,2000,1))

qplot(x = price, data = diamonds,
          geom = 'histogram',binwidth=1)+
  facet_wrap(~cut)

aggregate(diamonds$price,by = list(cut=diamonds$cut),FUN = 'median')
qplot(x = price, data = diamonds) + facet_wrap(~cut)
qplot(x= price/carat,geom = 'histogram', data = diamonds) + facet_grid(~cut,scales = 'free_y')+scale_x_log10()
qplot(x= cut,y = price ,geom = 'boxplot', data = diamonds,fill = cut)
qplot(x= clarity,y = price ,geom = 'boxplot', data = diamonds,fill = clarity)
qplot(x= color,y = price ,geom = 'boxplot', data = diamonds,fill = color)
qplot(x= color,y = price/carat ,geom = 'boxplot', data = diamonds,fill = color)
qplot(x= carat ,binwidth =0.1,geom = 'freqpoly', data = diamonds)+
  scale_x_continuous(limits = c(0, 3), breaks = seq(0, 10, 0.1)) +
  scale_y_continuous(breaks = seq(0,12000,1000))
