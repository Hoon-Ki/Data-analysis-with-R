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
plot1 = ggplot(aes(x = friend_count),data =pf)+geom_histogram(binwidth = 1,fill = '#5760AB')+scale_x_continuous(breaks = seq(0,4923,1000))
plot2 = qplot(x = friend_count, data = pf) + 
plot3 = ggplot(aes(x=scale_x_sqrt(freind_count)),data = pf)
grid.arrange(plot1,plot2,plot3)
