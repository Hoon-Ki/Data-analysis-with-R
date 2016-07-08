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
###########################################################
# finished lesson2
###########################################################

#####################################################
#lesson 3

## Third Qualitative variable

ggplot(aes(x = friend_count),
       data = subset(pf, !is.na(gender))) + geom_bar()+
  facet_wrap(~gender)+xlim(0,1000)

ggplot(aes(x = gender, y= age),
       data = subset(pf, !is.na(gender))) + geom_boxplot()+
  stat_summary(fun.y = mean, geom = 'point', shape = 4)

ggplot(aes(x=age,y=friend_count),
       data = subset(pf,!is.na(gender)))+
  geom_line(aes(color=gender), stat = 'summary', fun.y = median)

# create a new data frame
library(dplyr)

by_age_gender <- group_by(filter(pf,!is.na(gender)),age,gender)
pf.fc_by_age_gender1 <- dplyr::summarise(by_age_gender,
                                 mean_friend_count = mean(friend_count),
                                 median_friend_count = median(friend_count),
                                 n = n())
pf.fc_by_age_gender1<-ungroup(pf.fc_by_age_gender1)
pf.fc_by_age_gender1<-arrange(pf.fc_by_age_gender1,age)

#or

pf.fc_by_age_gender2<-pf %>%
  filter(!is.na(gender))%>%
  group_by(age,gender)%>%
  summarise(mean_friend_count = mean(friend_count),
            median_friend_count = median(friend_count),
            n = n())%>%
  ungroup()%>%
  arrange(age)

## Plotting Conditional Summaries

ggplot(aes(x=age,y=median_friend_count),data = pf.fc_by_age_gender1)+
  geom_line(aes(color=gender))

## wide and long format

#install.packages("tidyr") # only need to run this once 
library(tidyr)
spread(subset(pf.fc_by_age_gender1, select = c('gender', 'age', 'median_friend_count')), gender, median_friend_count)

## Reshaping Data

#install.packages('reshape2')
library(reshape2)

pf.fc_by_age_gender1.wide <- dcast(pf.fc_by_age_gender1,
                                   age~gender,
                                   value.var = 'median_friend_count')
head(pf.fc_by_age_gender1.wide)

## Ratio plot

ggplot(aes(x=age,y = ratio),data = pf.fc_by_age_gender1.wide)+
  geom_line()+
  geom_hline(yintercept = 1, linetype = 2)

## Third quantitative variable

pf$year_joined<- floor(2014-pf$tenure/365)

## cut a variable

pf$year_joined.bucket <- cut(pf$year_joined,breaks = c(2004,2009,2011,2012,2014))

## plotting it all together

ggplot(aes(x=age,y=friend_count), data = subset(pf,!is.na(year_joined.bucket)))+
  geom_line(aes(color=year_joined.bucket),stat = 'summary', fun.y = median)

## plot the grand mean

ggplot(aes(x=age,y=friend_count), data = subset(pf,!is.na(year_joined.bucket)))+
  geom_line(aes(color=year_joined.bucket),stat = 'summary', fun.y = mean)+
  geom_line(aes(x = age, y = friend_count),stat = 'summary',fun.y = mean, linetype = 2)

## friending rate
with(subset(pf,tenure>=1),summary(friend_count/tenure))

## friendships initiated

ggplot(aes(x = tenure, y = friendships_initiated/tenure ),data = subset(pf,tenure>=1))+
  geom_line(aes(color=year_joined.bucket))


## Bias Variance Trade off Revisited

ggplot(aes(x = 7 * round(tenure / 7), y = friendships_initiated / tenure),
       data = subset(pf, tenure > 0)) +
  geom_line(aes(color = year_joined.bucket),
            stat = "summary",
            fun.y = mean)

# or use smooth

ggplot(aes(x=tenure,y=friendships_initiated/tenure),data = subset(pf,tenure>1))+
  geom_smooth(aes(color= year_joined.bucket))

## yogurt dataset
yo<- read.csv('yogurt.csv')
str(yo)

## histogram revisited
yo$id <- factor(yo$id)
str(yo)

library(ggplot2)
p1<-ggplot(aes(x=obs),data = yo)+
  geom_histogram()

p2<-ggplot(aes(x=time),data = yo)+
  geom_histogram()

(p3<-ggplot(aes(x=strawberry),data = yo)+
  geom_histogram())

(p4<-ggplot(aes(x=blueberry),data = yo)+
  geom_histogram())

(p5<-ggplot(aes(x=pina.colada),data = yo)+
  geom_histogram())

(p6<-ggplot(aes(x=plain),data = yo)+
  geom_histogram())

(p7<-ggplot(aes(x=mixed.berry),data = yo)+
  geom_histogram())

(p8<-ggplot(aes(x=price),data = yo)+
  geom_histogram())
library(gridExtra)
grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8,ncol=1)

## number of purchases
yo <- transform(yo, all.purchase = strawberry+blueberry+pina.colada+plain+mixed.berry )

## prices over time

ggplot(aes(x = all.purchase),data = yo)+
  geom_histogram(binwidth = 1)

ggplot(aes(x = time,y = price),data = yo)+
  geom_point(alpha = 0.25,shape = 21, fill = I('#F79420'),position=position_jitter(h=0))
  
## looking at samples of households

set.seed(4321)
sample.ids <- sample(levels(yo$id),16)

ggplot(aes(x = time, y = price),
       data = subset(yo, id %in% sample.ids))+
  facet_wrap(~id)+
  geom_line()+
  geom_point(aes(size = all.purchase),pch=1)

## scatterplot matrices
#install.packages('GGally')
library(GGally)
theme_set(theme_minimal(20))

set.seed(1836)
pf_subset <- pf[ , c(2:7)]
names(pf_subset)
ggpairs(pf_subset[sample.int(nrow(pf_subset),1000),])

## Even More Variables

nci<- read.table('nci.tsv')

colnames(nci)<- c(1:64)

library(reshape2)
nci.long.samp <- melt(as.matrix(nci[1:200,]))
names(nci.long.samp)<- c('gene','case','value')
head(nci.long.samp)

## heat maps

ggplot(aes(y=gene,x = case, fill= value),
       data = nci.long.samp)+
  geom_tile()+
scale_fill_gradientn(colours = colorRampPalette(c('blue','red'))(100))

############################################

# price histograms with Facet 

ggplot(aes(x = price),data = subset(diamonds,!is.na(price)))+
  geom_histogram(aes(fill = cut))+
  scale_x_log10()+
  scale_y_continuous(limits = c(0,600))+
  facet_wrap(~color)#or +scale_fill_brewer(type = 'qual')

##price vs. table colored by cut

ggplot(aes(x = table, y = price),data = diamonds)+
  scale_x_continuous(breaks = seq(40,90,by=10))+
  geom_point(aes(color = cut))

## price vs. volume and diamond clarity
diamonds$volume<-with(diamonds,x*y*z)
ggplot(aes(x = volume ,y = price),data = diamonds)+
  geom_point(aes(color = clarity))+
  ylim(0,quantile(diamonds$volume,0.99))+
  scale_y_log10()+
  coord_cartesian(xlim = c(0,350))
  
## proportion of friendships initiated

pf<-transform(pf, prop_initiated = ifelse(friend_count <= 0, 0, friendships_initiated/friend_count))

## prop_initiated vs tenure

ggplot(aes(x =tenure ,y = prop_initiated),data = subset(pf,!is.na(tenure)&prop_initiated>0))+
         geom_line(aes(color = year_joined.bucket),stat = 'summary',fun.y= median)

##Smoothing prop_initiated vs. tenure

ggplot(aes(x =tenure ,y = prop_initiated),data = subset(pf,!is.na(tenure)&prop_initiated>0))+
  geom_smooth(aes(color = year_joined.bucket))

## Largest Group Mean prop_initiated

with(subset(pf, year_joined.bucket == "(2012,2014]"), mean(prop_initiated))
with(subset(pf, year_joined.bucket == "(2011,2012]"), mean(prop_initiated))
with(subset(pf, year_joined.bucket == "(2009,2011]"), mean(prop_initiated))
with(subset(pf, year_joined.bucket == "(2004,2009]"), mean(prop_initiated))

# Price/Carat Binned, Faceted, & Colored

ggplot(aes(x = cut,y = price/carat),data = diamonds)+
  geom_point(aes(color = color,alpha = 0.25),position = position_jitter(h =0))+
  facet_wrap(~clarity)

ggplot(aes(x = cut,y = price/carat),data = diamonds)+
  geom_point(aes(alpha = 0.25),position = position_jitter(h =0))+
  facet_wrap(~clarity)+
  scale_color_brewer(type = 'div')

##################################################
# finished lesson3
###################################################

#############################################
#lesson4

##scatterplot Review

ggplot(aes(x=carat, y=price), data = diamonds)+
  geom_point(fill = I('#F79420'),color = I('black'), shape = 21)+
  xlim(0,quantile(diamonds$carat,0.99))+
  ylim(0,quantile(diamonds$price,0.99))

## price and carat relationship

ggplot(aes(x=carat, y=price), data = diamonds)+
  geom_point(fill = I('#F79420'),alpha = 1/4, shape = 21)+
  stat_smooth(method = 'lm')+
  xlim(0,quantile(diamonds$carat,0.99))+
  ylim(0,quantile(diamonds$price,0.99))

## ggparis function

library(GGally)
library(scales)
#install.packages('memisc')
library(memisc)

set.seed(2016)
diamond_samp<- diamonds[sample(1:length(diamonds$price),10000),]
ggpairs(diamond_samp, 
        lower = list(continuous = wrap("points", shape = I('.'))), 
        upper = list(combo = wrap("box", outlier.shape = I('.'))))

## The demand of diamonds

library(gridExtra)

plot1 <- qplot(x = price, data = diamonds,binwidth = 100,fill = I('#099DD9')) + 
  ggtitle('Price')

plot2 <- qplot(x=price, data = diamonds,binwidth = 0.01, fill = I('#F79420')) +
  scale_x_log10()+
  ggtitle('Price (log10)')

grid.arrange(plot1,plot2,nrow =1)


## scatterplot transformation
cuberoot_trans = function() trans_new('cuberoot',
                                      transform = function(x) x^(1/3),
                                      inverse = function(x) x^3)

ggplot(aes(carat,price),data = diamonds)+
  geom_point()+
  scale_x_continuous(trans = cuberoot_trans(),limits = c(0.2,3),
                     breaks = c(0.2,0.5,1,2,3))+
  scale_y_continuous(trans = cuberoot_trans(),limits = c(350,15000),
                     breaks = c(350,1000,5000,10000,15000))+
  ggtitle('Price (log10) by Cube-Root of Carat')

## overplotting revisited

ggplot(aes(carat,price),data = diamonds)+
  geom_point(alpha = 0.5,size = 0.75, position = 'jitter')+
  scale_x_continuous(trans = cuberoot_trans(),limits = c(0.2,3),
                     breaks = c(0.2,0.5,1,2,3))+
  scale_y_continuous(trans = cuberoot_trans(),limits = c(350,15000),
                     breaks = c(350,1000,5000,10000,15000))+
  ggtitle('Price (log10) by Cube-Root of Carat')

## price vs carat and clarity

ggplot(aes(carat,price),data = diamonds)+
  geom_point(aes(color = clarity),alpha = 0.5,size = 1, position = 'jitter')+
  scale_x_continuous(trans = cuberoot_trans(),limits = c(0.2,3),
                     breaks = c(0.2,0.5,1,2,3))+
  scale_y_continuous(trans = cuberoot_trans(),limits = c(350,15000),
                     breaks = c(350,1000,5000,10000,15000))+
  ggtitle('Price (log10) by Cube-Root of Carat')

#or
library(RColorBrewer)
ggplot(aes(x = carat, y = price, color = clarity), data = diamonds) + 
  geom_point(alpha = 0.5, size = 1, position = 'jitter') +
  scale_color_brewer(type = 'div',
                     guide = guide_legend(title = 'Clarity', reverse = T,
                                          override.aes = list(alpha = 1, size = 2))) +  
  scale_x_continuous(trans = cuberoot_trans(), limits = c(0.2, 3),
                     breaks = c(0.2, 0.5, 1, 2, 3)) + 
  scale_y_continuous(trans = log10_trans(), limits = c(350, 15000),
                     breaks = c(350, 1000, 5000, 10000, 15000)) +
  ggtitle('Price (log10) by Cube-Root of Carat and Clarity')


## price vs carat and cut

ggplot(aes(x = carat, y = price, color = cut), data = diamonds) + 
  geom_point(alpha = 0.5, size = 1, position = 'jitter') +
  scale_color_brewer(type = 'div',
                     guide = guide_legend(title = 'Cut', reverse = T,
                                          override.aes = list(alpha = 1, size = 2))) +  
  scale_x_continuous(trans = cuberoot_trans(), limits = c(0.2, 3),
                     breaks = c(0.2, 0.5, 1, 2, 3)) + 
  scale_y_continuous(trans = log10_trans(), limits = c(350, 15000),
                     breaks = c(350, 1000, 5000, 10000, 15000)) +
  ggtitle('Price (log10) by Cube-Root of Carat and Cut')

## price vs carat and color

ggplot(aes(x = carat, y = price, color = color), data = diamonds) + 
  geom_point(alpha = 0.5, size = 1, position = 'jitter') +
  scale_color_brewer(type = 'div',
                     guide = guide_legend(title = 'Color',
                                          override.aes = list(alpha = 1, size = 2))) +  
  scale_x_continuous(trans = cuberoot_trans(), limits = c(0.2, 3),
                     breaks = c(0.2, 0.5, 1, 2, 3)) + 
  scale_y_continuous(trans = log10_trans(), limits = c(350, 15000),
                     breaks = c(350, 1000, 5000, 10000, 15000)) +
  ggtitle('Price (log10) by Cube-Root of Carat and Color')


# linear model

lm((log10(diamonds$price))~I(diamonds$carat^1/3))

##A Bigger, Better Data Set
library(data.table)
bigdia<-load('BigDiamonds.Rda')
diamondsbig$price<-log(diamondsbig$price)


#m1<-lm(I(log(price))~I(carat^(1/3)),data = diamondsbig[sample(nrow(diamondsbig),10000,replace = FALSE),])
m1<-lm(price~I(carat^(1/3)),data = diamondsbig[diamondsbig$price<10000 & diamondsbig$cert =='GIA',])
m2<-update(m1,~.+cut)
m3<-update(m2,~.+carat)
m4<-update(m3,~.+color)
m5<-update(m4,~.+clarity)

##predictions

thisDiamonds <- data.frame(carat = 1.00, cut = 'V.Good',
                           color = 'I', clarity = 'VS1')

modelEstimate <- predict(m5, newdata = thisDiamonds,
                         interval= 'prediction', level = .95)

dat = data.frame(m4$model, m4$residuals) 

with(dat, sd(m4.residuals)) 

with(subset(dat, carat > .9 & carat < 1.1), sd(m4.residuals)) 

dat$resid <- as.numeric(dat$m4.residuals)
ggplot(aes(y = resid, x = round(carat, 2)), data = dat) + 
  geom_line(stat = "summary", fun.y = sd) 


