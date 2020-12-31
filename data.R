rm(list=ls())
library(fastDummies) # dummy_cols()
library(lubridate) # base package - as.Date: ����ϸ�!, as.POSIXct: date & time
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
# day(X) --- 20~30�� �����ϴ°Ŵϱ� day�� ū �ǹ̰� ������. �����ϰ��� �ϴ� 20~30�� �����Ͱ� �����ϱ�!
# minute(X) --- ��� 0 ���̹Ƿ� �ǹ� ���ٰ� ���� 
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
# casual�� registered�� test�� ���� �����̴ϱ� �𵨸����� ���� �ؾ����� ������? 
num_varname = c('temp', 'atemp', 'humidity', 'windspeed', 'casual', 'registered', 'hour', 'count')
correlation = cor(train[,num_varname])

corrplot.mixed(correlation, upper = 'square', tl.pos = 'd', tl.cex=0.8)

# caregrocial variable - make dummy
# season, weather, year, month, wday (hour��???)
# workingday, holiday ��� �ش����� �ʴ� ���� weekend. �� workingday, holiday, weekend�� �̹� dummyȭ �� ���� workinday, holiday�� �����ϸ� �ȴ�!
train1 = dummy_cols(train,select_columns=c('season','weather','year','month','wday'),remove_first_dummy=TRUE)
train1 = train1[,-which(names(train) %in% c('season','weather','year','month','wday'))]
head(train1);names(train1)

# outlier: count bm���� outlier�� ���� �ƴҶ� ������ �𵨸�? 
# �涧�� �ƴҶ� �� ������ �׷��� ���غ��� �ҰŰ�����?





