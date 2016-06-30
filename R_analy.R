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
#######################################################
# finished lesson1
#########################################################

########################################################
#lesson2
#######################################################

##scatter plot
qplot(x = age, y = friend_count, data= pf)
#or
ggplot(aes(x = age, y = friend_count), data = pf) + 
  geom_point()

## ggplot syntax

summary(pf$age)
ggplot(aes(x = age, y = friend_count), data = pf) + 
  geom_point()+xlim(13,90)

##overplotting
##coord_trans()
ggplot(aes(x = age, y = friend_count), data = pf) + 
  geom_point(alpha = 0.05, position = position_jitter(h=0))+xlim(13,90) +coord_trans(y='sqrt')

##Alpha and jitter
ggplot(aes(x=age, y=friendships_initiated),data = pf)+geom_point()+
  geom_jitter(alpha=1/10)
# or
ggplot(aes(x=age, y=friendships_initiated),data = pf)+geom_point(alpha =1/10,position = 'jitter')
# transform y-axis
ggplot(aes(x=age, y=friendships_initiated),data = pf)+geom_point(alpha =1/10,position = position_jitter(h=0))+
  coord_trans(y='sqrt')

##conditional means

#install.packages('dplyr')
library(dplyr)
age_groups<-group_by(pf, age)
pf.fc_by_age1 <-summarise(age_groups,
                         friend_count_mean = mean(friend_count),
                         friend_count_median = median(as.numeric(friend_count)),
                         n = n())
head(pf.fc_by_age1)
pf.fc_by_age1<- arrange(pf.fc_by_age1,age)
head(pf.fc_by_age1)

ggplot(aes(x = age, y = friend_count_mean), data = pf.fc_by_age1)+geom_line()
#or

pf.fc_by_age2<-pf %>%
  group_by(age) %>%
  summarise(age_groups,
            friend_count_mean = mean(friend_count), friend_count_median = median(as.numeric(friend_count)),n = n()) %>%
  arrange(age)

head(pf.fc_by_age2,20)


##overlaying summarys with raw data
ggplot(aes(x=age, y=friendships_initiated),data = pf)+
  geom_point(alpha =1/20,position = position_jitter(h=0),color= 'orange')+
  coord_cartesian(xlim=c(13,70),ylim = c(0,1000))+
  geom_line(stat = 'summary', fun.y = mean)+
  geom_line(stat = 'summary', fun.y = quantile, fun.args = list(probs = .9),color = 'blue',linetype =2)+
  geom_line(stat = 'summary', fun.y = quantile, fun.args = list(probs = .1))+
  geom_line(stat = 'summary', fun.y = median,color = 'blue',linetype =2)

##correaltion
cor.test(pf$age,pf$friend_count,method = 'pearson')
#or
with(pf,cor.test(age,friend_count,method = 'pearson'))

##correaltion on subsets
with(subset(pf,age<=70),cor.test(age,friend_count,method = 'pearson'))

##correalation methods
with(subset(pf,age<=70),cor.test(age,friend_count,method = 'spearman'))

##correalation scatter plot
qplot(x = www_likes_received ,data=pf)+scale_x_continuous()
ggplot(aes(x=www_likes_received, y=likes_received),data = pf)+
  geom_point(alpha =1/20,position = position_jitter(h=0),color= 'orange')+
  coord_cartesian(xlim=c(0,30),ylim = c(0,70))+
  geom_line(stat = 'summary', fun.y = mean)+
  geom_line(stat = 'summary', fun.y = quantile, fun.args = list(probs = .9),color = 'blue',linetype =2)+
  geom_line(stat = 'summary', fun.y = quantile, fun.args = list(probs = .1))+
  geom_line(stat = 'summary', fun.y = median,color = 'blue',linetype =2)

with(subset(pf,likes_received<=50 & www_likes_received <=25),cor.test(likes_received,www_likes_received,method = 'kendall'))

##strong correlation
ggplot(aes(x = www_likes_received, y = likes_received),data = pf)+
  geom_point()+
  xlim(0,quantile(pf$www_likes_received,0.95))+
  ylim(0,quantile(pf$likes_received,0.95))+
  geom_smooth(method = 'lm', color = 'red')

with(pf,cor.test(www_likes_received,likes_received))

## More Caution with Correaltion
#install.packages('alr3')
library(alr3)
data(Mitchell)

ggplot(aes(y = Temp, x = Month),data = Mitchell)+
  geom_point()
qplot(data = Mitchell,Month,Temp)

##Noisy scatterplots
with(Mitchell, cor.test(Month,Temp))

## Making Sense of Data
ggplot(aes(y = Temp, x = Month),data = Mitchell)+
  geom_point()+
  scale_x_continuous(breaks = seq(0,203,12))

## A new perspective

ggplot(aes(y = Temp, x = Month%%12),data = Mitchell)+
  geom_point()

#install.packages('energy')
library(energy)
x <- seq(0, 4*pi, pi/20)
y <- cos(x)
qplot(x = x, y = y)
dcor.ttest(x, y)

## Understanding Noise : Age to Age month

ggplot(aes(x = age, y = friend_count_mean), data = pf.fc_by_age1
       )+ geom_line()
head(pf.fc_by_age1,10)
pf.fc_by_age1[17:19,]

pf$age_with_months <- pf$age + (1 - pf$dob_month / 12)
#or
pf$age_with_months <- with(pf, age + (1 - dob_month / 12))

## Age with Months Means

age_groups<-group_by(pf, age_with_months)
pf.fc_by_age_months <-summarise(age_groups,
                          friend_count_mean = mean(friend_count),
                          friend_count_median = median(as.numeric(friend_count)),
                          n = n())
pf.fc_by_age_months<- arrange(pf.fc_by_age_months,age_with_months)


'''
pf.fc_by_age_months<-

  
with(pf, subset(pf,age_with_months,friend_))
pf$friend_count_mean<-by(pf$friend_count,pf$age_with_months,mean)
# or
aggregate(friend_count~age_with_months,data=pf,'mean')
aggregate(friend_count~age_with_months,data=pf,'medain')[2]
aggregate(age_with_months,by)
'''
## Noise in conditional means

ggplot(data = subset(pf.fc_by_age_months,age_with_months<71),aes(x = age_with_months, y = friend_count_mean))+
  geom_line()+
  scale_x_continuous(breaks = seq(13.16667,113.91667,10))

## smoothing conditional means

p1<-ggplot(aes(x = age, y = friend_count_mean), data = subset(pf.fc_by_age1,age<71)
)+ geom_line()+geom_smooth()

p2<-ggplot(data = subset(pf.fc_by_age_months,age_with_months<71),aes(x = age_with_months, y = friend_count_mean))+
  geom_line()+
  scale_x_continuous(breaks = seq(13.16667,113.91667,10))+
  geom_smooth()

p3 <- ggplot(aes(x = round(age/5)*5,y = friend_count),data = subset(pf,age<71))+
  geom_line(stat = 'summary', fun.y = mean)
library(gridExtra)
grid.arrange(p3,p2,p1,ncol=1)

#####################################################

## price vs x


str(diamonds)

#scatter plot
ggplot(aes(x = x, y = price), data = diamonds)+
  geom_point()

ggplot(aes(x = carat, y = price), data = diamonds)+
  geom_point()

ggplot(aes(x = cut, y = price), data = diamonds)+
  geom_point()

ggplot(aes(x = color, y = price), data = diamonds)+
  geom_point()

ggplot(aes(x = clarity, y = price), data = diamonds)+
  geom_point()

ggplot(aes(x = depth, y = price), data = diamonds)+
  geom_point()

ggplot(aes(x = table, y = price), data = diamonds)+
  geom_point()

## correlations

with(diamonds, cor.test(price , x ))
with(diamonds, cor.test(price , y ))
with(diamonds, cor.test(price , z ))

## price vs depth

ggplot(aes(x = depth, y = price), data = diamonds)+
  geom_point(alpha=1/100)+
  scale_x_continuous(breaks = seq(43,79,2))

## correlation - price and depth

with(diamonds, cor.test(price , depth ))

##price vs carat

ggplot(aes(x = carat, y = price), data = diamonds)+
  geom_point()+
  xlim(0,quantile(diamonds$carat,0.99))+
  ylim(0,quantile(diamonds$price,0.99))

## price vs volume(x*y*z) # new variable

diamonds$volume<-with(diamonds,x*y*z)
ggplot(aes(x = volume ,y = price),data = diamonds)+
  geom_point()
  
with(diamonds, cor.test(price , volume ))
detach('package:plyr', unload=TRUE)
library(plyr)

count(diamonds$volume == 0)

## correlations on subsets

ggplot(aes(x = volume ,y = price),data = subset(diamonds,volume!=0 & volume <=800))+
  geom_point()

with(subset(diamonds,volume!=0 & volume <=800), cor.test(price , volume ))

## adjustments- price vs volume
#http://www.ats.ucla.edu/stat/r/faq/smooths.htm

'''
method
smoothing method (function) to use,
eg. lm, glm, gam, loess, rlm.
For datasets with n < 1000 default is loess.
For datasets with 1000 or more observations defaults to gam.
'''
ggplot(aes(x= volume ,y = price),data = subset(diamonds,volume!=0 & volume <=800))+
  geom_point(alpha = 1/100)+
  stat_smooth(method = 'lm')

## mean price by clarity
library(dplyr)
dia_byclar<- group_by(diamonds,clarity)
diamondsByClarity <-dplyr::summarise(dia_byclar,
                              mean_price = mean(price),
                              median_price = median(as.numeric(price)),
                              min_price = min(price),
                              max_price = max(price),
                              n = n())

dia_bycolr<- group_by(diamonds,color)
diamondsByColor <-dplyr::summarise(dia_bycolr,
                                     mean_price = mean(price),
                                     median_price = median(as.numeric(price)),
                                     min_price = min(price),
                                     max_price = max(price),
                                     n = n())
## bar charts of mean price

p1<-ggplot(aes(x = clarity, y = mean_price),data = diamondsByClarity)+
  geom_bar(stat = 'identity') 
# If you want the heights of the bars to represent values in the data, use stat="identity" and map a variable to the y aesthetic.
  
p2<-ggplot(aes(x = color, y = mean_price),data = diamondsByColor)+
  geom_bar(stat = 'identity')

library(gridExtra)
grid.arrange(p1,p2,ncol=1)
