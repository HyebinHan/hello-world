rm(list=ls())
library(fastDummies) # dummy_cols()
library(lubridate) # base package - as.Date: 년월일만!, as.POSIXct: date & time
library(corrplot)


# data --------------------------------------------------------------------

train = read.csv('train.csv')
test = read.csv('test.csv')

head(train);head(test)
names(train);names(test)
summary(train);summary(test)
dim(train) # 10886 12
dim(test) # 6493 9

# NA --- There are no NAs
na.vec = c()
for(i in 1:dim(train)[2]){
  na.vec[i] = sum(is.null(train[,i]))
}
na.vec

# datetime variable transformation
# day(X) --- 20~30일 예측하는거니까 day는 큰 의미가 없을듯. 예측하고자 하는 20~30일 데이터가 없으니까!
# minute(X) --- 모두 0 값이므로 의미 없다고 생각 
# train$day = day(train$datetime)
class(train$datetime) # character

train$year = year(train$datetime)
train$month = month(train$datetime)
train$hour = hour(train$datetime)
train$wday = wday(train$datetime) # start from sunday(sunday=1, saturday=7)
train = train[,-which(names(train)=='datetime')]
names(train)


# correlation
# temp, atemp (select temp)
# casual과 registered는 test에 없는 변수이니까 모델링에서 제거 해야하지 않을까? 
num_varname = c('temp', 'atemp', 'humidity', 'windspeed', 'casual', 'registered', 'hour', 'count')
correlation = cor(train[,num_varname])

corrplot.mixed(correlation, upper = 'square', tl.pos = 'd', tl.cex=0.8)

# caregrocial variable - make dummy
# season, weather, year, month, wday (hour도???)
# workingday, holiday 모두 해당하지 않는 경우는 weekend. 즉 workingday, holiday, weekend를 이미 dummy화 한 것이 workinday, holiday로 생각하면 된다!
train1 = dummy_cols(train,select_columns=c('season','weather','year','month','wday'),remove_first_dummy=TRUE)
train1 = train1[,-which(names(train) %in% c('season','weather','year','month','wday'))]
head(train1);names(train1)

# outlier: count bm값이 outlier일 때와 아닐때 나눠서 모델링? 
# 길때와 아닐때 각 변수의 그래프 비교해봐야 할거같은데?






