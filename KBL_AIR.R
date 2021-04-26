
setwd('C:/Users/samsung/Desktop')

########## install.packages###########
install.packages('readxl')
install.packages('randomForest')
install.packages('caret')
install.packages('xlsx')
install.packages('prediction')  #for using prediction ftn
install.packages('ggplot2')
install.packages('dplyr')
install.packages('hrbrthemes')  # in ggplot2(linear trend)
install.packages('car') # vif ftn
install.packages('MLmetrics') # F1_Score ftn
install.packages('corrplot')





########## Read Data##########
library(readxl)
d1 <- readxl::read_excel("2-3. 2016-2017시즌_경기기록데이터.xlsx",
                         sheet = "PLAYER_DAILY_LIST",
                         col_names = T)
d1
summary(d1)
str(d1)


d2 <- readxl::read_excel(path = "2-3. 2017-2018시즌_경기기록데이터.xlsx",
                         sheet = "PLAYER_DAILY_LIST",
                         col_names = T)
d2
summary(d2)
str(d2)
dim(d2)


sum(d2$fo != "0") # fo 0이 아닌 값들 갯수

sum(d2$fg > d2$fg_a)  # 변수 char형이라서 알맞은 값 X


d3 <- readxl::read_excel(path = "2018-2019시즌_경기기록데이터.xlsx",
                         sheet = "PLAYER_DAILY_LIST",
                         col_names = T)

d3
str(d3)
summary(d3)

dim(d1) ; dim(d2) ; dim(d3)
colnames(d1) ; colnames(d2) ; colnames(d3)


str(d)

##### 필요없는 데이터 삭제
sum(!d$game_code=='01') # game_code 에 저장된 값 모두 '01'로 동일 => 따라서, 해당 변수 제거
delete<-which(colnames(d)=='game_code')
d<-d[,-delete]

sum(!complete.cases(d$back_num))  # 결측치도 많이 포함할 뿐더러 등번호는 무의미한 변수라고 인지하여 제거
delete<-which(colnames(d)=='back_num')
d<-d[,-delete]

sum(which(d$fb!='0')) # fb 변수에 저장된 값이 모두 '0'으로 동일 => 따라서, 해당 변수 제거
delete<-which(colnames(d)=='fb')
d<-d[,-delete]

d$inputtime[1:30] # 입력시간은 무의미한 변수라고 인지하여 제거
delete<-which(colnames(d)=='inputtime')
d<-d[,-delete]

delete=which(colnames(d)=='idf') # idf 변수도 모든 관측값에서 0의 값을 가져서 제거
d<-d[,-delete]

delete=which(colnames(d)=='foul_tot') # foul_tot 변수가 wof변수와 woft변수의 합과 동일함을 인지하여 제거
d<-d[,-delete]

##### playtime==0 인 선수 제거
playtime0<-which(d$play_min=='0' & d$play_sec=='0')
d<-d[-playtime0,]

str(d)

##### 데이터 형태 지정
d$season_code<-as.factor(d$season_code)
d$game_no<-as.factor(d$game_no)
d$team_code<-as.factor(d$team_code)
d$home_away<-as.factor(d$home_away)
d$player_no<-as.factor(d$player_no)
d$pos<-as.factor(d$pos)
d$away_team<-as.factor(d$away_team)
d$start_flag<-as.factor(d$start_flag)
d$play_min<-as.integer(d$play_min)
d$play_sec<-as.integer(d$play_sec)
d$fg<-as.numeric(d$fg)
d$fg_a<-as.numeric(d$fg_a)
d$ft<-as.integer(d$ft)
d$ft_a<-as.integer(d$ft_a)
d$threep<-as.integer(d$threep)
d$threep_a<-as.integer(d$threep_a)
d$dk<-as.integer(d$dk)
d$dk_a<-as.integer(d$dk_a)
d$pp<-as.integer(d$pp)
d$pp_a<-as.integer(d$pp_a)
d$o_r<-as.integer(d$o_r)
d$d_r<-as.integer(d$d_r)
d$a_s<-as.integer(d$a_s)
d$s_t<-as.integer(d$s_t)
d$b_s<-as.integer(d$b_s)
d$gd<-as.integer(d$gd)
d$t_o<-as.integer(d$t_o)
d$wft<-as.integer(d$wft)
d$woft<-as.integer(d$woft)
d$tf<-as.integer(d$tf)
d$ef<-as.integer(d$ef)
d$p_score<-as.integer(d$p_score)
d$score<-as.integer(d$score)
d$inout<-as.integer(d$inout)
d$inout1<-as.integer(d$inout1)
d$fo<-as.integer(d$fo)
d$win <- as.factor(d$win)

str(d)

##### 율 데이터 생성
n<-dim(d)[1] ; p<-dim(d)[2]
fg_r<-rep(0,n) ; ft_r<-rep(0,n) ; threep_r<-rep(0,n) ; dk_r<-rep(0,n) ; pp_r<-rep(0,n)
d<-cbind(d,fg_r,ft_r,threep_r,dk_r,pp_r)  # 5가지의 새로운 율변수 생성

fg_a0<-which(d$fg_a==0)
d$fg_r[-fg_a0]<-d$fg[-fg_a0]/d$fg_a[-fg_a0]
d$fg_r[fg_a0]<-0

ft_a0<-which(d$ft_a==0)
d$ft_r[-ft_a0]<-d$ft[-ft_a0]/d$ft_a[-ft_a0]
d$ft_r[ft_a0]<-0

threep_a0<-which(d$threep_a==0)
d$threep_r[-threep_a0]<-d$threep[-threep_a0]/d$threep_a[-threep_a0]
d$threep_r[threep_a0]<-0

dk_a0<-which(d$dk_a==0)
d$dk_r[-dk_a0]<-d$dk[-dk_a0]/d$dk_a[-dk_a0]
d$dk_r[dk_a0]<-0

pp_a0<-which(d$pp_a==0)
d$pp_r[-pp_a0]<-d$pp[-pp_a0]/d$pp_a[-pp_a0]
d$pp_r[pp_a0]<-0


##### 시간 데이터 생성
d$playtime<-d$play_min*60+d$play_sec
d$playtime<-as.integer(d$playtime)


##### corrplot
par(mfrow=c(1,1))
sum(d$idf!=0)
library(corrplot)
str(d)
cor_d <- d[,-c(1:8,37:48)]
str(cor_d)
corrplot(cor(cor_d))
# fg(2점슛 성공) & pp(페인트존슛 성공) & p_score(페인트존슛 득점) 매우 높은 양의 상관관계 확인
# 성광과 시도 간의 양의 상관관계 존재

str(d)


############################# Analysis ##############################


set.seed(201711505)

library(caret)
n <- dim(d)[1]
train_idx <- createDataPartition(d$pos, times = 1, p=.6, list=FALSE)
idx <- setdiff(1:n, train_idx)
validate_idx<-createDataPartition(d$pos[idx],p=.5,times=1, list=F)
test_idx<-idx[-validate_idx]
train_newdata <- d[train_idx,]
test_newdata <- d[test_idx,]
validation<-d[validate_idx,]

ry_obs<-d$pos[validate_idx]

n1<-length(validate_idx)
#y1<-rep(0,n1) ; y2<-rep(0,n1) ; y3<-rep(0,n1)
#y_obs<-cbind(y1,y2,y3)
#colnames(y_obs)<-c('C','FD','GD')
#for ( i in 1:n1) {
# ifelse(ry_obs[i]=='C',y_obs[i,1]<-1,
#        ifelse(ry_obs[i]=='FD',y_obs[i,2]<-1,
#               y_obs[i,3]<-1))
#}

dim(train_newdata)[1]/dim(d)[1]
dim(test_newdata)[1]/dim(d)[1]
dim(validation)[1]/dim(d)[1]

prop.table(table(d$pos))
prop.table(table(train_newdata$pos))
prop.table(table(test_newdata$pos))
prop.table(table(validation$pos))


mydata<-d

###########################################RF#####################################
########## RF1

library(randomForest)
colname<-colnames(train_newdata)
rfidx1<-which(colname=='game_no'|colname=='player_no'|colname=='away_team'|colname=='play_min'|
                colname=='play_sec'|colname=='fo'|colname=='fg_r'|colname=='ft_r'|colname=='threep_r'|colname=='dk_r'|colname=='pp_r')
ranfo1<-train_newdata[,-rfidx1]
all_rf1<-randomForest(pos~., data=ranfo1, ntree=200)
all_rf1
plot(all_rf1)

varImpPlot(all_rf1)
varImpPlot(all_rf1)[,2]

y_hat_rf1<-predict(all_rf1,validation,type='prob')
y_hat_rf1<-predict(all_rf1,validation,type='response')
sum(y_hat_rf1==ry_obs)/n1   # 예측율 86.38947
confusionMatrix(y_hat_rf1, ry_obs)
#library(prediction)
#pred_rf1<-prediction(y_hat_rf1,y_obs)
#perf_rf1<-performance(pred_rf1,measure='tpr',x.measure='fpr')
#plot(perf_rf1,col='red')
#abline(0,1)
#performance(pred_rf1,'auc')@y.values[[1]]
##AUC:

library(MLmetrics)
F1_Score(ry_obs, y_hat_rf1, positive = NULL)


########## RF2

colname<-colnames(train_newdata)
rfidx2<-which(colname=='game_no'|colname=='player_no'|colname=='away_team'|colname=='play_min'|
                colname=='play_sec'|colname=='fo'|colname=='fg_r'|colname=='ft_r'|colname=='threep_r'|colname=='dk_r'|colname=='pp_r'
              | colname=='team_code')
ranfo2<-train_newdata[,-rfidx2]
all_rf2<-randomForest(pos~., data=ranfo2, ntree=500)
all_rf2
plot(all_rf2)

varImpPlot(all_rf2)
varImpPlot(all_rf2)[,2]

y_hat_rf2<-predict(all_rf2,validation,type='prob')
y_hat_rf2<-predict(all_rf2,validation,type='response')
sum(y_hat_rf2==ry_obs)/n1 
confusionMatrix(y_hat_rf2, ry_obs)




########## RF3

colname<-colnames(train_newdata)
rfidx3<-which(colname=='game_no'|colname=='player_no'|colname=='away_team'|colname=='play_min'|
                colname=='play_sec'|colname=='fo'|colname=='fg' |colname=='ft'|colname=='threep'|colname=='dk'|colname=='pp'
              | colname=='team_code')
ranfo3<-train_newdata[,-rfidx3]
all_rf3<-randomForest(pos~., data=ranfo3, ntree=500)
all_rf3
plot(all_rf3)

varImpPlot(all_rf3)
varImpPlot(all_rf3)[,2]

y_hat_rf3<-predict(all_rf3,validation,type='prob')
y_hat_rf3<-predict(all_rf3,validation,type='response')
confusionMatrix(y_hat_rf3, ry_obs)



########## RF4

colname<-colnames(train_newdata)
rfidx4<-which(colname=='game_no'|colname=='player_no'|colname=='away_team'|colname=='play_min'|
                colname=='play_sec'|colname=='fo'|colname=='fg' |colname=='ft'|colname=='threep'|colname=='dk'|colname=='pp'
              | colname=='team_code' | colname=='fg_a'|colname=='ft_a'|colname=='threep_a'|colname=='dk_a'|colname=='pp_a')
ranfo4<-train_newdata[,-rfidx4]
all_rf4<-randomForest(pos~., data=ranfo4, ntree=500)
all_rf4
plot(all_rf4)

varImpPlot(all_rf4)
varImpPlot(all_rf4)[,2]

y_hat_rf4<-predict(all_rf4,validation,type='prob')
y_hat_rf4<-predict(all_rf4,validation,type='response')
confusionMatrix(y_hat_rf4, ry_obs)


## varImpPlot 비교
par(mfrow=c(2,2))
varImpPlot(all_rf1) ; varImpPlot(all_rf2) ; varImpPlot(all_rf3) ; varImpPlot(all_rf4)

##pos별 Playtime 분포 확인
library(ggplot2)
library(dplyr)
p_p1 <- d %>%
  ggplot(aes(x=pos, y=playtime, fill=pos))+geom_boxplot()
p_p2 <- d %>%
  ggplot(aes(x=pos, y=playtime, fill=pos)) + geom_violin()
par(mfrow=c(1,2))
p_p1 ; p_p2


##pos별 3점슛 시도(threep_a) 분포 확인
t_p1<- d %>%
  ggplot(aes(x=pos, y=threep_a, fill=pos)) + geom_boxplot()
t_p2<- d %>%
  ggplot(aes(x=pos, y=threep_a, fill=pos)) + geom_violin()
par(mfrow=c(1,2))
t_p1 ; t_p2


##pos별 어시스트(a_s) 분포 확인
a_p1<- d %>%
  ggplot(aes(x=pos, y=a_s, fill=pos)) + geom_boxplot()
a_p2<- d %>%
  ggplot(aes(x=pos, y=a_s, fill=pos)) + geom_violin()
par(mfrow=c(1,2))
a_p1 ; a_p2



##pos별 수비리바운드(d_r) 분포 확인
d_p1<- d %>%
  ggplot(aes(x=pos, y=d_r, fill=pos)) + geom_boxplot()
d_p2<- d %>%
  ggplot(aes(x=pos, y=d_r, fill=pos)) + geom_violin()
par(mfrow=c(1,2))
d_p1 ; d_p2


##pos별 공격리바운드(o_r) 분포 확인
o_p1<- d %>%
  ggplot(aes(x=pos, y=o_r, fill=pos)) + geom_boxplot()
o_p2<- d %>%
  ggplot(aes(x=pos, y=o_r, fill=pos)) + geom_violin()
par(mfrow=c(1,2))
o_p1 ; o_p2



##pos별 2점슛시도(fg_a) 분포 확인
f_p1<- d %>%
  ggplot(aes(x=pos, y=fg_a, fill=pos)) + geom_boxplot()
f_p2<- d %>%
  ggplot(aes(x=pos, y=fg_a, fill=pos)) + geom_violin()
par(mfrow=c(1,2))
f_p1 ; f_p2


##pos별 득점 수(score) 분포 확인
s_p1<- d %>%
  ggplot(aes(x=pos, y=score, fill=pos)) + geom_boxplot()
s_p2<- d %>%
  ggplot(aes(x=pos, y=score, fill=pos)) + geom_violin()
par(mfrow=c(1,2))
s_p1 ; s_p2


# C가 playtime과의 연관성이 존재하는가?
library(hrbrthemes)
p_s1<- d %>%
  ggplot(aes(x=playtime, y=score)) + geom_point() + geom_smooth(method=lm , color="red", se=TRUE)


s_p_lm<-lm(score~playtime, data=d)
summary(s_p_lm)
# Adjusted R-squared:  0.5414
# 득점과 playtime 간에 선형성이 존재하지 않는다고 판단한다.
cor(d$score,d$playtime) 



#################fo 값 채워넣기(bY using lm)#######################################
season29<-which(d$season_code==29)
d23<-d[-season29,]

sum(d23$idf!=0)
lmfoidx<-which(colname=='season_code' |colname=='game_no'|colname=='player_no'|colname=='away_team'|colname=='play_min'| colname=='idf' |
                 colname=='play_sec'|colname=='fg_r'|colname=='ft_r'|colname=='threep_r'|colname=='dk_r'|colname=='pp_r' 
               | colname=='foul_tot' | colname=='p_score' | colname=='score')


d23<-d23[,-lmfoidx]
lmfo<-lm(fo~., data=d23)
lmfo
summary(lmfo)




predict(lmfo, d[season29,], type='terms')
fo29lm<-predict(lmfo, d[season29,], type='response')
length(fo29lm)
par(mfrow=c(1,1))
plot(fo29lm)

d$fo[season29]<-fo29lm
# season_code==29 일때의 fo 예측값 채워넣기
str(d)

########## RF5

library(randomForest)
colname<-colnames(train_newdata)
rfidx5<-which(colname=='game_no'|colname=='player_no'|colname=='away_team'|colname=='play_min'|
                colname=='play_sec'|colname=='fg_r'|colname=='ft_r'|colname=='threep_r'|colname=='dk_r'|colname=='pp_r')
ranfo5<-train_newdata[,-rfidx5]
all_rf5<-randomForest(pos~., data=ranfo5, ntree=200)
all_rf5
plot(all_rf5)

varImpPlot(all_rf5)
varImpPlot(all_rf5)[,2]

y_hat_rf5<-predict(all_rf5,validation,type='prob')
y_hat_rf5<-predict(all_rf5,validation,type='response')
sum(y_hat_rf5==ry_obs)/n1   # 예측율 86.38947
confusionMatrix(y_hat_rf5, ry_obs)


library(MLmetrics)
F1_Score(ry_obs, y_hat_rf5, positive = NULL)


########## RF6

colname<-colnames(train_newdata)
rfidx6<-which(colname=='game_no'|colname=='player_no'|colname=='away_team'|colname=='play_min'|
                colname=='play_sec'|colname=='fg_r'|colname=='ft_r'|colname=='threep_r'|colname=='dk_r'|colname=='pp_r'
              | colname=='team_code')
ranfo6<-train_newdata[,-rfidx6]
all_rf6<-randomForest(pos~., data=ranfo6, ntree=500)
all_rf6
plot(all_rf6)

varImpPlot(all_rf6)
varImpPlot(all_rf6)[,2]

y_hat_rf6<-predict(all_rf6,validation,type='prob')
y_hat_rf6<-predict(all_rf6,validation,type='response')
sum(y_hat_rf6==ry_obs)/n1 
confusionMatrix(y_hat_rf6, ry_obs)




########## RF7

colname<-colnames(train_newdata)
rfidx7<-which(colname=='game_no'|colname=='player_no'|colname=='away_team'|colname=='play_min'|
                colname=='play_sec'|colname=='fg' |colname=='ft'|colname=='threep'|colname=='dk'|colname=='pp'
              | colname=='team_code')
ranfo7<-train_newdata[,-rfidx7]
all_rf7<-randomForest(pos~., data=ranfo7, ntree=500)
all_rf7
plot(all_rf7)

varImpPlot(all_rf7)
varImpPlot(all_rf7)[,2]

y_hat_rf7<-predict(all_rf7,validation,type='prob')
y_hat_rf7<-predict(all_rf7,validation,type='response')
confusionMatrix(y_hat_rf7, ry_obs)



########## RF8

colname<-colnames(train_newdata)
rfidx8<-which(colname=='game_no'|colname=='player_no'|colname=='away_team'|colname=='play_min'|
                colname=='play_sec'|colname=='fg' |colname=='ft'|colname=='threep'|colname=='dk'|colname=='pp'
              | colname=='team_code' | colname=='fg_a'|colname=='ft_a'|colname=='threep_a'|colname=='dk_a'|colname=='pp_a')
ranfo8<-train_newdata[,-rfidx8]
all_rf8<-randomForest(pos~., data=ranfo8, ntree=500)
all_rf8
plot(all_rf8)

varImpPlot(all_rf8)
varImpPlot(all_rf8)[,2]

y_hat_rf8<-predict(all_rf8,validation,type='prob')
y_hat_rf8<-predict(all_rf8,validation,type='response')
confusionMatrix(y_hat_rf8, ry_obs)


## varImpPlot 비교
par(mfrow=c(2,2))
varImpPlot(all_rf5) ; varImpPlot(all_rf6) ; varImpPlot(all_rf7) ; varImpPlot(all_rf8)





##pos별 fo(피파울 수) 분포 확인
library(ggplot2)
library(dplyr)
foul_p1<- d %>%
  ggplot(aes(x=pos, y=fo, fill=pos)) + geom_boxplot()
foul_p2<- d %>%
  ggplot(aes(x=pos, y=fo, fill=pos)) + geom_violin()
par(mfrow=c(1,2))
foul_p1 ; foul_p2







#################fo 값 채워넣기(bY using RF)#######################################
season29<-which(d$season_code==29)
d23<-d[-season29,]
d23 <- d23[,-c(2,5,30,39,40,41,42,43,44)]
str(d23)
dim(d23)
library(randomForest)
d23_rf <- randomForest(fo~., data=d23)
plot(d23_rf)
sort(importance(d23_rf)[,1], decreasing=T)  
# 파울과 가장 연관성이 높을것이라 생각되는 변수 자유투에 관한 변수 ft가 가장 중요도가 크게 나온 것을 확인 ==> 잘돌아가고이쑴
par(mfrow=c(1,1))
varImpPlot(d23_rf)
fo29rf <- predict(d23_rf, d23[season29,],type='response')
length(fo29rf)
d$fo[season29] <- fo29rf


###### fo29lm 값과 fo29rf 예측값 비교
library(ggplot2)
library(dplyr)
focompare
# Libraries
library(tidyverse)
library(hrbrthemes)
library(viridis)

# create a dataset
focompare <- data.frame(
  name=c( rep("FO29LM",5477), rep("FO29RF",5477)  ),
  value=c( fo29lm, fo29rf )
)

# 
focompare %>%
  ggplot( aes(x=name, y=value, fill=name)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Compare with prediction b.t LM and RF") +
  xlab("")

## LM 예측값에 음수가 약간 포함되어 있네????




########## RF9

library(randomForest)
colname<-colnames(train_newdata)
rfidx9<-which(colname=='game_no'|colname=='player_no'|colname=='away_team'|colname=='play_min'|
                colname=='play_sec'|colname=='fg_r'|colname=='ft_r'|colname=='threep_r'|colname=='dk_r'|colname=='pp_r')
ranfo9<-train_newdata[,-rfidx9]
all_rf9<-randomForest(pos~., data=ranfo9, ntree=500)
all_rf9
plot(all_rf9)

varImpPlot(all_rf9)
varImpPlot(all_rf9)[,2]

y_hat_rf9<-predict(all_rf9,validation,type='prob')
y_hat_rf9<-predict(all_rf9,validation,type='response')
sum(y_hat_rf9==ry_obs)/n1   # 예측율 87.07748
confusionMatrix(y_hat_rf9, ry_obs)


library(MLmetrics)
F1_Score(ry_obs, y_hat_rf9, positive = NULL)






#################fo 값 채워넣기(bY using lm with constraint)#######################################
season29<-which(d$season_code==29)
d23<-d[-season29,]

sum(d23$idf!=0)
lmfoidx<-which(colname=='season_code' |colname=='game_no'|colname=='player_no'|colname=='away_team'|colname=='play_min'| colname=='idf' |
                 colname=='play_sec'|colname=='fg_r'|colname=='ft_r'|colname=='threep_r'|colname=='dk_r'|colname=='pp_r' 
               | colname=='foul_tot' | colname=='p_score' | colname=='score')


d23<-d23[,-lmfoidx]
lmfo0<-lm(fo~.+0, data=d23)
lmfo0
summary(lmfo0)




predict(lmfo0, d[season29,], type='terms')
fo29lm0<-predict(lmfo0, d[season29,], type='response')
length(fo29lm0)
par(mfrow=c(1,1))
plot(fo29lm0)



# Libraries
library(tidyverse)
library(hrbrthemes)
library(viridis)

# create a dataset
focompare <- data.frame(
  name=c( rep("FO29LM",5477), rep("FO29RF",5477), rep("FO29LM_CONSTRAINTS",5477)  ),
  value=c( fo29lm, fo29rf, fo29lm0 )
)

# 
focompare %>%
  ggplot( aes(x=name, y=value, fill=name)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Compare with prediction b.t LM and RF") +
  xlab("")

# fo29lm0 또한 음수 예측값 존재
sum(fo29lm0<0)

# 정수값에 한해서 동일 예측값 개수 확인
sum(round(fo29lm,0)==round(fo29rf,0))

# 예측값의 차에 대한 분포 확인
lsubr<-fo29lm-fo29rf
plot(lsubr)
abline(h=c(-seq(1:10),0,seq(1:10)), col='red')

sum(-1 <= lsubr & lsubr <=1)/length(lsubr)
sum(-2 <= lsubr & lsubr <=2)/length(lsubr)
sum(-3 <= lsubr & lsubr <=3)/length(lsubr)

sum(d$fo[-season29])/dim(d[-season29])[1]

max(lsubr)
min(lsubr)



#################fo 값 채워넣기(by using RF with Factorizing) ########################

season29<-which(d$season_code==29)
d23<-d[-season29,]

d23$fo <- as.factor(d23$fo)
rffactidx <- which(colname=='season_code' |colname=='game_no'|colname=='player_no'|colname=='away_team'|colname=='playtime'
                   |colname=='fg_r'|colname=='ft_r'|colname=='threep_r'|colname=='dk_r'|colname=='pp_r')

rffact <- d23[,-rffactidx]

set.seed(201711505)
library(randomForest)
rffactrf <- randomForest(fo~., data=rffact)
set.seed(201711505)
rffact_1 <- predict(rffactrf, d[season29,], type='response')
table(rffact_1)
length(rffact_1)
par(mfrow=c(1,1))
plot(rffact_1)
d$fo[season29]<-rffact_1
dim(d)
summary(d23$fo)
summary(rffact_1)
summary(d$fo)
d$fo

varImpPlot(rffactrf)
#layout(matrix(c(1,2),nrow=1),
#       width=c(4,1)) 
#par(mar=c(5,4,4,0)) #No margin on the right side
#plot(rffactrf)
#par(mar=c(5,0,4,2)) #No margin on the left side
#plot(c(0,1),type="n", axes=F, xlab="", ylab="")
#legend("top", colnames(rffactrf$err.rate),col=1:16,cex=0.6,fill=1:16)

d$fo <- as.integer(d$fo)

####################################### PCA ####################################################

pcaidx<-which(colname=='season_code' | colname=='game_no'| colname=='player_no'| colname=='away_team'| colname=='idf'
              |colname=='team_code' | colname=='home_away' | colname=='pos' | colname=='start_flag' | colname=='play_min' 
              | colname=='play_sec' | colname=='fg' | colname=="fg_a" | colname=="ft" | colname=="ft_a"| colname=="threep"
              | colname=="threep_a"| colname=="dk"| colname== "dk_a"| colname=="pp"| colname=="pp_a" | colname=="score")

pcaall<-d[,-pcaidx]
dim(pcaall)
#[Step 2] Covariance Matrix S(or Correlation Matix R)
Rall=round(cor(pcaall),3)
Rall
dim(Rall)


#[Step 3] Spectral Decomposition 
eigen.Rall=eigen(Rall)
round(eigen.Rall$values, 2) # Eigenvalues
Vall=round(eigen.Rall$vectors, 2) # Eigenvectors

#[Step 4] Choice of Eigenvalues and Eigenvectors
gofall=eigen.Rall$values/sum(eigen.Rall$values)*100 # Goodness-of fit
round(gofall, 2)
sum(round(gofall, 2)[1:8])

par(mar=c(5.1, 4.1, 4.1, 2.1), mgp=c(3, 1, 0), las=0)
par(mfrow=c(1,1))
plot(eigen.Rall$values, type="b", main="Scree Graph", xlab="Component Number", ylab="Eigenvalue")

#[Step 5] PCs : liner combination of original variables 
V2all=Vall[,1:8]
colnames(V2all)<-c('P1','P2','P3','P4','P5','P6','P7','P8')
rownames(V2all)<-colname[-pcaidx]
V2all

#[Step 6] PCS, PCs Scores and New Data Matrix P
Zall=scale(pcaall, scale=T) # Standardized Data Matrix
Zall
Pall=Zall%*%V2all            # PCs Scores
round(Pall, 3)

#[Step 7] Plot of PCs Scores
plot(Pall[,1], Pall[, 2], main="Plot of PCs Scores", xlab="1st PC", ylab="2nd PC")
text(Pall[,1], Pall[, 2], labels=rownames, cex=0.8, col="blue", pos=3)
abline(v=0, h=0)

################# 주성분으로 lm돌리기 ####################

class(Pall)
Pall <- as.data.frame(Pall)
dpca <- cbind(Pall, d$home_away, d$pos, d$start_flag, d$score)
colnames(dpca) <- c("P1", "P2", "P3", "P4", "P5", "P6", "P7", "P8", "home_away", "pos", "start_flag", "score")
str(dpca)
str(d)

dcpca <- dpca[dpca$pos=="C",-10]
dgdpca <- dpca[dpca$pos=="GD",-10]
dfdpca <- dpca[dpca$pos=="FD",-10]

dim(dcpca);dim(dgdpca);dim(dfdpca)

dclm <- lm(score~.-home_away-start_flag-P7, data=dcpca)
summary(dclm)
library(car)
vif(dclm)
# C 모델식
# : 7.474 - 2.884P1 - 1.271P2 - 0.555P3 - 0.370P4 -0.594P5 + 0.286P6 - 0.425P8


dgdlm <- lm(score~.-home_away, data=dgdpca)
summary(dgdlm)
vif(dgdlm)
# GD 모델식
# : 8.765 - 2.953P1 - 1.016P2 - 0.425P3 -0.255P4 - 0.092 + 0.485P6 + 0.213P7 - 0.992P8 - 1.062*start_flag1


dfdlm <- lm(score~.-home_away, data=dfdpca)
summary(dfdlm)
vif(dfdlm)
# FD 모델식
# : 8.281 - 2.807P1 - 0.919P2 - 0.562P3 - 0.660P4 -0.351P5 + 0.585P6 + 0.163P7 - 0.675P8 - 0.437*start_flag1



str(dcpca)
dcpcac <- dcpca[,-c(8,9,10)]
str(dcpcac)
library(corrplot)
corrplot(cor(dcpcac), method="number")

str(dgdpca)
dgdpcac <- dgdpca[,-c(8,9,10)]
corrplot(cor(dgdpcac), method='number')

str(dfdpca)
dfdpcac <- dfdpca[,-c(9,10)]
corrplot(cor(dfdpcac), method='number')

################각 선수별 정보를 담은 새로운 데이터 형성 ################################

summary(d)

d$fo<-as.integer(as.character(d$fo))
newidx<-which(colname=='season_code' |colname=='game_no'| colname=='team_code' | colname=='home_away' | colname=='player_no'|
                colname=='pos' |colname=='away_team'|colname=='start_flag' |colname=='play_min'| colname=='idf' |
                colname=='play_sec'|colname=='fg_r'|colname=='ft_r'|colname=='threep_r'|colname=='dk_r'|colname=='pp_r' )
dnew<-d[,-newidx]

player_n<-length( unique( d$player_no ) )
new<-matrix(0, nrow=player_n, ncol=length(colnames(dnew)) )
rownames(new)<-unique(d$player_no)


for( i  in unique(d$player_no) ) {
  idx<-which( d$player_no == i )
  rowidx<-which( rownames(new)== i )
  new[rowidx,] <- colMeans(dnew[idx,])
}
summary(new)
rowMeans(dnew[1:5,])
colnames(new)<-colnames(dnew)
summary(new)


################################## 새로운 데이터에 대한 PCA ################################## 


#[Step 2] Covariance Matrix S(or Correlation Matix R)
Rnew=round(cor(new),3)
Rnew
dim(Rnew)


#[Step 3] Spectral Decomposition 
eigen.Rnew=eigen(Rnew)
round(eigen.Rnew$values, 2) # Eigenvalues
Vnew=round(eigen.Rnew$vectors, 2) # Eigenvectors

#[Step 4] Choice of Eigenvalues and Eigenvectors
gofnew=eigen.Rnew$values/sum(eigen.Rnew$values)*100 # Goodness-of fit
round(gofnew, 2)
sum(round(gofnew, 2)[1:3])

par(mar=c(5.1, 4.1, 4.1, 2.1), mgp=c(3, 1, 0), las=0)
par(mfrow=c(1,1))
plot(eigen.Rnew$values, type="b", main="Scree Graph", xlab="Component Number", ylab="Eigenvalue")

#[Step 5] PCs : liner combination of original variables 
V2new=Vnew[,1:3]
colnames(V2new)<-c('P1','P2','P3')
rownames(V2new)<-colnames(new)
V2new

#[Step 6] PCS, PCs Scores and New Data Matrix P
Znew=scale(new, scale=T) # Standardized Data Matrix
Znew
Pnew=Znew%*%V2new            # PCs Scores
round(Pnew, 3)

#[Step 7] Plot of PCs Scores
plot(Pnew[,1], Pnew[, 2], main="Plot of PCs Scores", xlab="1st PC", ylab="2nd PC")
text(Pnew[,1], Pnew[, 2], labels=rownames(new), cex=0.8, col="blue", pos=3)
abline(v=0, h=0)

plot(Pnew[,2], Pnew[, 3], main="Plot of PCs Scores", xlab="2nd PC", ylab="3rd PC")
text(Pnew[,2], Pnew[, 3], labels=rownames(new), cex=0.8, col="blue", pos=3)
abline(v=0, h=0)

plot(Pnew[,1], Pnew[, 3], main="Plot of PCs Scores", xlab="1st PC", ylab="3rd PC")
text(Pnew[,1], Pnew[, 3], labels=rownames(new), cex=0.8, col="blue", pos=3)
abline(v=0, h=0)

player_pos<-c(rep(0, player_n))

for( i in 1:player_n ) {
  num<-unique(d$player_no)[i] 
  idx<-which(d$player_no==num)
  player_pos[i]<-as.character( d$pos[idx[1]] )
}


player_pos<-as.factor(player_pos)


plot(Pnew[,1], Pnew[, 2], main="Plot of PCs Scores", xlab="1st PC", ylab="2nd PC")
text(Pnew[,1], Pnew[, 2], labels=player_pos, cex=0.8, col="blue", pos=3)
abline(v=0, h=0)



##### PCA 데이터

V2new<-as.matrix(as.data.frame(V2new))
dnew<-as.matrix(dnew)
PCAdata<-dnew %*% V2new
PCAdata<-as.data.frame(PCAdata)
str(PCAdata)


player_n<-length( unique( d$player_no ) )
PCAvar<-matrix(0, nrow=player_n, ncol=3 )
rownames(PCAvar)<-unique(d$player_no)


for( i  in 1:player_n ) {
  num<-unique(d$player_no)[i]
  
  if ( sum(d$player_no == num) == 1 ) {
    PCAvar[i,]<-c(0,0,0)
  }
  else{
    idx<-which( d$player_no == num )
    PCAvar[i,] <- apply(PCAdata[idx,], 2, var)
  }
  
  
}


player_var<-c(rep(0,player_n))
player_var<-as.integer(player_var)
for( i in 1:player_n ) {
  player_var[i]<-gofnew[1]*PCAvar[i,1] + gofnew[2]*PCAvar[i,2] + gofnew[3]*PCAvar[i,3]
}

plot(player_var)
summary(player_var)


###############11/21 판별분석 실행###############3
#우리는 이미 군집이 pos 별로 3개로 나눠져 있기때문에 군집분석 말고 판별분석을 해야함 (다변량2 6장 내용)
#판별분석 잘 알려진 군집으로부터 얻어진 데이터를 가지고 개체들을 군집으로 가장 잘 분리할 수 있는 판별기준을 구해준다. 
#적합정도는 오분류율으로 평가한다. 


# foul_tot : wft + woft 의 값이라서 NAN처리가 되기 때문에 제거 시켰다.

str(d)
daidx<-which(colname=='season_code' |colname=='game_no'| colname=='team_code' | colname=='home_away' | colname=='player_no'
             |colname=='away_team'|colname=='start_flag' |colname=='play_min'| colname=='play_sec'| colname=='idf' 
             |colname=='fg'|colname=='fg_a'|colname=='ft'|colname=='ft_a'|colname=='threep'|colname=='threep_a'|colname=='dk'|colname=='dk_a'
             |colname=='pp'|colname=='pp_a'|colname=='foul_tot')

d31 <- d[d$season_code==31,-daidx]
d30 <- d[d$season_code==30,-daidx]
d29 <- d[d$season_code==29,-daidx]
str(d31)

#범주형 자료 제거 비율만 넣어본다. 
install.packages('MVN')
library(MVN)
install.packages('biotools')
library(biotools)
str(dc)

dc <- d31[d31$pos=="C", -1]
dgd <- d31[d31$pos=="GD", -1]
dfd <- d31[d31$pos=="FD", -1]
dim(dc);dim(dgd);dim(dfd)
resultdc <- mvn(dc)
resultdgd <- mvn(dgd)
resultdfd <- mvn(dfd)
resultdc$multivariateNormality
resultdgd$multivariateNormality
resultdfd$multivariateNormality
#정규성 만족 안하므로 QDA실시

sum(is.na(d31$score))

# stat : 10666, p-value : 2.2e-16 < 0.05 즉, 등분산성의 가설을 기각한다.
boxM(d31[,-1], d31[,1])
S1 <- cov(dc); S2 <- cov(dgd) ; S3 <- cov(dfd)
Sp <- (931*S1 + 2275*S2 + 2408*S3)/(5617-3) 
list(S1, S2, S3, Sp)
d_qda <- d[,-daidx]
QDA <- qda(pos~.,data=d_qda, prior=c(1,1,1)/3)
qcluster <- predict(QDA, d_qda)$class
table(d_qda$pos, qcluster)

mean(d_qda$pos==qcluster)


########### 승률 데이터 만들기 ##########
# d1
str(d1)
d1$score<-as.integer(d1$score)
dim(d1)
unique(d1$game_no)
winner=rep(0,270)
win=rep(0,6480)
for (i in 1:270){
  game = d1[ which(d1[,3]==i) , ]
  team1=unique(game$team_code)[1]
  team2=unique(game$team_code)[2]
  score_team1=sum (game[ which(game$team_code==team1), 38])
  score_team2=sum (game[ which(game$team_code==team2), 38])
  if (score_team1 > score_team2){
    winner[i]=team1
  }else{
    winner[i]=team2
  }
  win[which(game$team_code==winner[i])+24*(i-1)]=1
  win[which(game$team_code!=winner[i])+24*(i-1)]=0
}

table(winner)
length(win)
table(win)
d1 <- cbind(d1,win)


# d2
str(d2)
d2$score<-as.integer(d2$score)
dpos <- which(d2$pos=="AB" | d2$pos == "AA" | d2$pos=="BC" | d2$pos == "AC")
d2 <- d2[-dpos,]
dim(d2)
unique(d2$game_no)
winner=rep(0,270)
win=rep(0,6480)
for (i in 1:270){
  game = d2[ which(d2[,3]==i) , ]
  team1=unique(game$team_code)[1]
  team2=unique(game$team_code)[2]
  score_team1=sum (game[ which(game$team_code==team1), 38])
  score_team2=sum (game[ which(game$team_code==team2), 38])
  if (score_team1 > score_team2){
    winner[i]=team1
  }else{
    winner[i]=team2
  }
  win[which(game$team_code==winner[i])+24*(i-1)]=1
  win[which(game$team_code!=winner[i])+24*(i-1)]=0
}

table(winner)
length(win)
table(win)
d2 <- cbind(d2,win)

# d3
str(d3)
d3$score<-as.integer(d3$score)
seasonidx <- which(d3$season_code==31)
d3 <- d3[-seasonidx,]
gfindex<-which(d3$pos=='GF')
d3$pos[gfindex]<-'GD'
dpos <- which(d3$pos=="AB" | d3$pos == "AA" | d3$pos=="BC" | d3$pos == "AC")
d3 <- d3[-dpos,]
p1 <- d3[1:3743,]
p2 <- d3[3744:6479,]
d[3743,]
a <- as.character( c(33,156,55,1,999999,'C',10,rep(0,35)))
a[38] <- as.integer(a[38])
d3 <- rbind(p1,a,p2)

d3$score <- as.integer(d3$score)
dim(d3)
str(d3)
table(d3$game_no)
unique(d3$game_no)
winner=rep(0,270)
win=rep(0,6480)
for (i in 1:270){
  game = d3[ which(d3[,3]==i) , ]
  team1=unique(game$team_code)[1]
  team2=unique(game$team_code)[2]
  score_team1=sum (game[ which(game$team_code==team1), 38])
  score_team2=sum (game[ which(game$team_code==team2), 38])
  if (score_team1 > score_team2){
    winner[i]=team1
  }else{
    winner[i]=team2
  }
  win[which(game$team_code==winner[i])+24*(i-1)]=1
  win[which(game$team_code!=winner[i])+24*(i-1)]=0
}
table(winner)
length(win)
table(win)
d3 <- cbind(d3,win)

#1125
d<-rbind(d1,d2,d3)
dim(d); dim(d1); dim(d2); dim(d3)
length(winner)
str(d1)

win_r<-rep(0,19440)
player_no<-length( unique(d$player_no) )
for( i in 1:player_no ) {
  player<-unique( d$player_no )[i]
  player<-which( d$player_no==player )
  num<-length(player)
  tot=0
  for( j in 1:num ) {
    a <- d$win[ player[j] ]
    tot = tot+a
    win_r[ player[j] ] <- tot/j
  }
  
}
win_r


d<-cbind(d,win_r)
str(d)

############ RF with 반응변수 = win ############

str(d)

windidx <- which(colnames(d)=='game_no' | colnames(d)=='player_no' | colnames(d)=='away_team' |
                   colnames(d)=='play_min' | colnames(d)=='play_sec' | colnames(d)=='fg' |
                   colnames(d)=='fg_a' | colnames(d)=='ft' | colnames(d)=='ft_a' | colnames(d)=='threep' |
                   colnames(d)=='threep_a' | colnames(d)=='dk' | colnames(d)=='dk_a' | colnames(d)=='pp' |
                   colnames(d)=='pp_a' | colnames(d)=='foul_tot')

str(wind)
wind$win=as.factor(wind$win)
wind <- d[,-windidx]
set.seed(201711505)
windrf <- randomForest(win~., data=wind)
windrf
varImpPlot(windrf)


# C
dc <- d[d$pos=="C",]

windCidx <- which(colnames(dc)=='game_no' | colnames(dc)=='player_no' | colnames(dc)=='away_team' |
                    colnames(dc)=='play_min' | colnames(dc)=='play_sec' | colnames(dc)=='fg' |
                    colnames(dc)=='fg_a' | colnames(dc)=='ft' | colnames(dc)=='ft_a' | colnames(dc)=='threep' |
                    colnames(dc)=='threep_a' | colnames(dc)=='dk' | colnames(dc)=='dk_a' | colnames(dc)=='pp' |
                    colnames(dc)=='pp_a'|colnames(dc)=='win_r'|colnames(dc)=='pos')



windC <- dc[,-windCidx]
str(windC)
set.seed(201711505)
windCrf <- randomForest(win~., data=windC)
windCrf
sort(varImpPlot(windCrf)[,1], decreasing=T)
str(d)
# FD
dfd <- d[d$pos=="FD",]

windFDidx <- which(colnames(dfd)=='game_no' | colnames(dfd)=='player_no' | colnames(dfd)=='away_team' |
                     colnames(dfd)=='play_min' | colnames(dfd)=='play_sec' | colnames(dfd)=='fg' |
                     colnames(dfd)=='fg_a' | colnames(dfd)=='ft' | colnames(dfd)=='ft_a' | colnames(dfd)=='threep' |
                     colnames(dfd)=='threep_a' | colnames(dfd)=='dk' | colnames(dfd)=='dk_a' | colnames(dfd)=='pp' |
                     colnames(dfd)=='pp_a' | colnames(dfd)=='pos' | colnames(dfd)=='win_r')

windFD <- dfd[,-windFDidx]
set.seed(201711505)
windFDrf <- randomForest(win~., data=windFD)
windFDrf
sort(varImpPlot(windFDrf)[,1])

# GD
dgd <- d[d$pos=="GD",]

windGDidx <- which(colnames(dgd)=='game_no' | colnames(dgd)=='player_no' | colnames(dgd)=='away_team' |
                     colnames(dgd)=='play_min' | colnames(dgd)=='play_sec' | colnames(dgd)=='fg' |
                     colnames(dgd)=='fg_a' | colnames(dgd)=='ft' | colnames(dgd)=='ft_a' | colnames(dgd)=='threep' |
                     colnames(dgd)=='threep_a' | colnames(dgd)=='dk' | colnames(dgd)=='dk_a' | colnames(dgd)=='pp' |
                     colnames(dgd)=='pp_a' | colnames(dgd)=='pos' | colnames(dfd)=='win_r')

windGD <- dgd[,-windGDidx]
set.seed(201711505)
windGDrf <- randomForest(win~., data=windGD)
windGDrf
sort(varImpPlot(windGDrf)[,1])










################ 포지션별 선수 능력 평가 지표를 위한 수식 만들기 ##################



## C 수식 : Cgini

Cgini<-sort( varImpPlot(windCrf)[,1], decreasing=T )
sumC<-sum(varImpPlot(windCrf)[,1])
which(names(Cgini)=='woft')
sum(Cgini[1:18])/ sumC


C_idx <-which(names(Cgini)=='inout1' |names(Cgini)=='inout' |names(Cgini)=='woft' |names(Cgini)=='wft' |names(Cgini)=='t_o' )
Cgini[C_idx]<-Cgini[C_idx]*(-1)

Cno_idx <- which(names(Cgini)=='team_code' | names(Cgini)=='home_away' | names(Cgini)=='season_code')
Cgini <- Cgini[-Cno_idx]
Cgini <- Cgini[-c(16:23)]
Cgini


## FD 수식 : FDgini

FDgini<-sort( varImpPlot(windFDrf)[,1], decreasing=T )
sumFD<-sum(varImpPlot(windFDrf)[,1])
which(names(FDgini)=='s_t')
sum(FDgini[1:19])/ sumFD


FD_idx <-which( names(FDgini)=='o_r' |names(FDgini)=='inout1'|names(FDgini)=='inout' |names(FDgini)=='wft' |names(FDgini)=='t_o' )
FDgini[FD_idx]<-FDgini[FD_idx]*(-1)
FDno_idx <- which(names(FDgini)=='team_code' | names(FDgini)=='season_code' | names(FDgini)=='home_away') 
FDgini <- FDgini[-FDno_idx]
FDgini <- FDgini[-c(18:23)]
FDgini


## GD 수식 : GDgini

GDgini<-sort( varImpPlot(windGDrf)[,1], decreasing=T )
sumGD<-sum(varImpPlot(windGDrf)[,1])
which(names(GDgini)=='p_score')
sum(GDgini[1:18])/ sumGD


GD_idx <-which( names(GDgini)=='o_r' |names(GDgini)=='t_o' |names(GDgini)=='wft' |names(GDgini)=='inout' |names(GDgini)=='inout1' )
GDgini[GD_idx]<-GDgini[GD_idx]*(-1)
GDno_idx <- which(names(GDgini)=='team_code' | names(GDgini)=='season_code' | names(GDgini)=='home_away')
GDgini <- GDgini[-GDno_idx]
GDgini <- GDgini[-c(18:23)]
GDgini




################################## 선수들 순위 세우기 #################################


########## Read Data
library(readxl)
player_code <- readxl::read_excel("팀_선수 코드_18-19.xlsx",
                                  sheet = "선수코드",
                                  col_names = T)


dim(player_code)
str(player_code)
player_code<-as.data.frame(player_code)


library(stringr)
str_detect(player_code[1,3], ' ')

notkorea<-rep(TRUE,322)
for( i in 1:322 ) {
  notkorea[i] <- str_detect(player_code[i,3], ' ')
}
notkorea

notkorea_idx<-which(notkorea)
length(notkorea_idx)


length(notkorea_idx)
## 외국인 용병들 확인
table( player_code[notkorea_idx,3] )
## 추가 제거 대상 선수 라건아 290524
which(player_code[,2]==290524)

player_n <- length( unique( d$player_no ))
player_n



# 최종 한국 국적 선수들 271명 존재
koreaplayer<-player_code[-c(notkorea_idx,50),]

koreaplayer_no<-koreaplayer[,2]
koreaplayer_no<-as.data.frame(koreaplayer_no)
koreaplayer_no


str(d3)




sum(!d3$game_code=='01') # game_code 에 저장된 값 모두 '01'로 동일 => 따라서, 해당 변수 제거
delete<-which(colnames(d3)=='game_code')
d3<-d3[,-delete]

sum(!complete.cases(d3$back_num))  # 결측치도 많이 포함할 뿐더러 등번호는 무의미한 변수라고 인지하여 제거
delete<-which(colnames(d3)=='back_num')
d3<-d3[,-delete]

sum(which(d3$fb!='0')) # fb 변수에 저장된 값이 모두 '0'으로 동일 => 따라서, 해당 변수 제거
delete<-which(colnames(d3)=='fb')
d3<-d3[,-delete]

d3$inputtime[1:30] # 입력시간은 무의미한 변수라고 인지하여 제거
delete<-which(colnames(d3)=='inputtime')
d3<-d3[,-delete]

delete=which(colnames(d3)=='idf')
d3<-d3[,-delete]

delete=which(colnames(d3)=='foul_tot')
d3<-d3[,-delete]





##### 데이터 형태 지정
d3$season_code<-as.factor(d3$season_code)
d3$game_no<-as.factor(d3$game_no)
d3$team_code<-as.factor(d3$team_code)
d3$home_away<-as.factor(d3$home_away)
d3$player_no<-as.factor(d3$player_no)
d3$pos<-as.factor(d3$pos)
d3$away_team<-as.factor(d3$away_team)
d3$start_flag<-as.factor(d3$start_flag)
d3$play_min<-as.integer(d3$play_min)
d3$play_sec<-as.integer(d3$play_sec)
d3$fg<-as.numeric(d3$fg)
d3$fg_a<-as.numeric(d3$fg_a)
d3$ft<-as.integer(d3$ft)
d3$ft_a<-as.integer(d3$ft_a)
d3$threep<-as.integer(d3$threep)
d3$threep_a<-as.integer(d3$threep_a)
d3$dk<-as.integer(d3$dk)
d3$dk_a<-as.integer(d3$dk_a)
d3$pp<-as.integer(d3$pp)
d3$pp_a<-as.integer(d3$pp_a)
d3$o_r<-as.integer(d3$o_r)
d3$d_r<-as.integer(d3$d_r)
d3$a_s<-as.integer(d3$a_s)
d3$s_t<-as.integer(d3$s_t)
d3$b_s<-as.integer(d3$b_s)
d3$gd<-as.integer(d3$gd)
d3$t_o<-as.integer(d3$t_o)
d3$wft<-as.integer(d3$wft)
d3$woft<-as.integer(d3$woft)
d3$tf<-as.integer(d3$tf)
d3$ef<-as.integer(d3$ef)
d3$p_score<-as.integer(d3$p_score)
d3$score<-as.integer(d3$score)
d3$inout<-as.integer(d3$inout)
d3$inout1<-as.integer(d3$inout1)
d3$fo<-as.integer(d3$fo)
d3$win <- as.factor(d3$win)


##### 율 데이터 생성

n<-dim(d3)[1] ; p<-dim(d3)[2]
fg_r<-rep(0,n) ; ft_r<-rep(0,n) ; threep_r<-rep(0,n) ; dk_r<-rep(0,n) ; pp_r<-rep(0,n)
d3<-cbind(d3,fg_r,ft_r,threep_r,dk_r,pp_r)  # 5가지의 새로운 율변수 생성
summary(d3)

fg_a0<-which(d3$fg_a==0)
d3$fg_r[-fg_a0]<-d3$fg[-fg_a0]/d3$fg_a[-fg_a0]
d3$fg_r[fg_a0]<-0

ft_a0<-which(d3$ft_a==0)
d3$ft_r[-ft_a0]<-d3$ft[-ft_a0]/d3$ft_a[-ft_a0]
d3$ft_r[ft_a0]<-0

threep_a0<-which(d3$threep_a==0)
d3$threep_r[-threep_a0]<-d3$threep[-threep_a0]/d3$threep_a[-threep_a0]
d3$threep_r[threep_a0]<-0

dk_a0<-which(d3$dk_a==0)
d3$dk_r[-dk_a0]<-d3$dk[-dk_a0]/d3$dk_a[-dk_a0]
d3$dk_r[dk_a0]<-0

pp_a0<-which(d3$pp_a==0)
d3$pp_r[-pp_a0]<-d3$pp[-pp_a0]/d3$pp_a[-pp_a0]
d3$pp_r[pp_a0]<-0


##### 시간 데이터 생성
d3$playtime<-d3$play_min*60+d3$play_sec
d3$playtime<-as.integer(d3$playtime)

##### playtime==0 인 선수 제거
playtime0<-which(d3$play_min=='0' & d3$play_sec=='0')
d3<-d3[-playtime0,]


str(d3)

########## 각 선수들의 변수에 대한 평균값 계산 #########


koreaplayer_no<-as.vector(koreaplayer_no)
koreaplayer_no<-as.factor( t(koreaplayer_no) )

table(koreaplayer_no)

koreaplayer2 <- which( table(koreaplayer_no)==2 )
koreaplayer2_no<-as.factor( names(table(koreaplayer_no))[koreaplayer2] )
koreaplayer2_no


length( unique(koreaplayer_no) )

length(koreaplayer2_no)
sum=0
for( i in 1:24 ){
  if( sum( koreaplayer2_no[i]==unique(d3$player_no) ) >0 ) sum=sum+1
} 
sum
# 18-19년도 선수들 중에 팀을 이전한 선수 수 ( 16명 포지션 변동 X + 2명 포지션 변동 O)


player_code
koreaplayer_no
koreaplayer2_no
length(koreaplayer2_no)

for( i in 1:24 ) {
  no<-koreaplayer2_no[i]
  idx<-which( player_code[,2]==no )
  cat(player_code[idx,6], '\n')
  if( player_code[idx[1],6] != player_code[idx[2],6])  cat( i, '\n' )
}


koreaplayer2_no[c(13,14)]
## 팀 이전 & 포지션 변경 O 선수 번호



length( unique(d3$player_no) )

playercar<-matrix(0, nrow=192, ncol=23)
colnames(playercar)<-c("pos", "o_r", "d_r", "a_s", "s_t", "b_s", "gd", "t_o", "wft", "woft", "tf", "ef", "p_score",
                       "score", "inout", "inout1", "fo",  "fg_r", "ft_r", "threep_r", "dk_r", "pp_r",  "playtime")


rownames(playercar)<-rep(0,192)
ii=1
for( i in 1:192 ){
  player_no<- unique(d3$player_no)[i]
  if( sum( player_no == koreaplayer2_no[c(13,14)] ) >0 ){
    idx1<-which( player_code[,2]==player_no )
    pos<-player_code[idx1,6]
    rownames(playercar)[ii]<-player_code[idx1[1],2]
    playercar[ii,1]<-pos[1]
    idx2<-which( d3$player_no==player_no & d3$pos == pos[1] )
    playercar[ii,2:23]<-colMeans(d3[idx2,-c(1:20,37)])
    ii=ii+1
    
    rownames(playercar)[ii]<-player_code[idx1[1],2]
    playercar[ii,1]<-pos[2]
    idx2<-which( d3$player_no==player_no & d3$pos == pos[2] )
    playercar[ii,2:23]<-colMeans(d3[idx2,-c(1:20,37)])
    ii=ii+1
  } else  if ( sum( player_no == koreaplayer_no ) > 0 & sum( player_no != koreaplayer2_no[c(13,14)] ) ==2  ) {
    idx11<-which( player_code[,2]==player_no )
    pos11<-player_code[idx11[1],6]
    rownames(playercar)[ii]<-player_code[idx11[1],2]
    playercar[ii,1]<-pos11
    idx22<-which( d3$player_no==player_no & d3$pos == pos11 )
    playercar[ii,2:23]<-colMeans(d3[idx22,-c(1:20,37)])
    ii=ii+1
    
  }
}

playercar
dim(playercar)

idx<-which(playercar[,1]=='0')
playercar[idx,]
playercar<-playercar[-idx,]
sum(playercar[,1]=="0")

summary(playercar)
dim(playercar)
playercar<-as.data.frame(playercar)

playercar[1:5,]
sum(playercar[,5]=='NaN')
idx<-which( playercar[,5]=='NaN' )
playercar[idx,]
playercar<-playercar[-idx,]
is.data.frame(playercar)
playercar[,1]<-as.factor(playercar[,1])
playercar[,2]<-as.numeric(as.character(playercar[,2]))
playercar[,3]<-as.numeric(as.character(playercar[,3]))
playercar[,4]<-as.numeric(as.character(playercar[,4]))
playercar[,5]<-as.numeric(as.character(playercar[,5]))
playercar[,6]<-as.numeric(as.character(playercar[,6]))
playercar[,7]<-as.numeric(as.character(playercar[,7]))
playercar[,8]<-as.numeric(as.character(playercar[,8]))
playercar[,9]<-as.numeric(as.character(playercar[,9]))
playercar[,10]<-as.numeric(as.character(playercar[,10]))
playercar[,11]<-as.numeric(as.character(playercar[,11]))
playercar[,12]<-as.numeric(as.character(playercar[,12]))
playercar[,13]<-as.numeric(as.character(playercar[,13]))
playercar[,14]<-as.numeric(as.character(playercar[,14]))
playercar[,15]<-as.numeric(as.character(playercar[,15]))
playercar[,16]<-as.numeric(as.character(playercar[,16]))
playercar[,17]<-as.numeric(as.character(playercar[,17]))
playercar[,18]<-as.numeric(as.character(playercar[,18]))
playercar[,19]<-as.numeric(as.character(playercar[,19]))
playercar[,20]<-as.numeric(as.character(playercar[,20]))
playercar[,21]<-as.numeric(as.character(playercar[,21]))
playercar[,22]<-as.numeric(as.character(playercar[,22]))
playercar[,23]<-as.numeric(as.character(playercar[,23]))


######## playercar 변수명 변경

str(playercar)
colnames(playercar)
nob <- rownames(playercar)
playercar_no <- gsub("X","",nob)
playercar_no[114] <- "291087"
rownames(playercar) <- playercar_no
head(playercar)
dim(playercar)
str(playercar)


##### C 뽑기
dim(playercar)

playerc <- playercar[playercar$pos=="C",]
dim(playercar)
dim(playerc)

playerc <- playerc[,-1]
str(playerc)
playerc <- as.matrix(playerc)

Cgini
Cmat <- c(Cgini[7],Cgini[4],Cgini[8],0,0,0,Cgini[13],Cgini[11],Cgini[15],0,0,Cgini[10],
          Cgini[5],Cgini[12],Cgini[9],Cgini[2],Cgini[3],Cgini[14],0,0,Cgini[6],Cgini[1])
Cmat <- as.matrix(Cmat)

playerCscore <- playerc%*%Cmat
playerCscore
length(playerCscore)

playerCscore_s=sort(playerCscore[,1], decreasing=T)
names(playerCscore_s)[1:10]

round(playerCscore,2)==108037.39

code_C=c('290750','290450','290781','290987','290549','290248','290110','290439','290491',
         '290786')
C_10=rep(0,11)
for (i in 1:length(code_C)){
  C_10=rbind(C_10,player_code[which(player_code$player_no==code_C[i]),])
}
C_10 <- C_10[-1,]

##### FD 뽑기
dim(playercar)

playerfd <- playercar[playercar$pos=="FD",]
dim(playerfd)[1]

playerfd <- playerfd[,-1]
str(playerfd)
playerfd <- as.matrix(playerfd)

FDgini
FDmat <- c(FDgini[11],FDgini[3],FDgini[6],FDgini[17],0,0,FDgini[14],FDgini[12],FDgini[13],0,0,
           FDgini[15],FDgini[2],FDgini[9],FDgini[8],FDgini[4],FDgini[5],FDgini[16],
           FDgini[7],0,FDgini[10],FDgini[1])
FDmat <- as.matrix(FDmat)

playerFDscore <- playerfd%*%FDmat
playerFDscore
length(playerFDscore)
playerFDscore_s <- sort(playerFDscore[,1], decreasing=T)

names(playerFDscore_s)[1:10]

code_FD=c('290787','290897','291091','290417','291084','290993','290280','290119','290661','290284')
FD_10=rep(0,11)
for (i in 1:length(code_FD)){
  FD_10=rbind(FD_10,player_code[which(player_code$player_no==code_FD[i]),])
}
FD_10 <- FD_10[-c(1,5,8),]

##### GD 뽑기

playergd <- playercar[playercar$pos=="GD",]
dim(playergd)[1]

playergd <- playergd[,-1]
str(playergd)
playergd <- as.matrix(playergd)

GDgini
GDmat <- c(GDgini[15],GDgini[5],GDgini[3],GDgini[13],0,0,GDgini[9],GDgini[12],GDgini[11],
           0,0,GDgini[17],GDgini[2],GDgini[10],GDgini[8],GDgini[4],GDgini[7],GDgini[16],
           GDgini[6],0,GDgini[14],GDgini[1])
GDmat <- as.matrix(GDmat)

playerGDscore <- playergd%*%GDmat
playerGDscore
length(playerGDscore)

playerGDscore_s <- sort(playerGDscore[,1], decreasing=T)
names(playerGDscore_s)[1:10]


code_GD=c('290381','290440','290776','291085','290742','290407','290505','210074','290991', 
          '291008')
GD_10=rep(0,11)
for (i in 1:length(code_GD)){
  GD_10=rbind(GD_10,player_code[which(player_code$player_no==code_GD[i]),])
}

GD_10<- GD_10[-c(1,6),]



################################# 각 선수별 정보를 담은 새로운 데이터 형성 ################################

summary(d)
str(d)

d$fo<-as.integer(as.character(d$fo))
# newidx<-which(colnames(d)=='season_code' |colnames(d)=='game_no' | colnames(d)=='team_code' |
#       colnames(d)=='home_away' | colnames(d)=='player_no' | colnames(d)=='pos' |
#      colnames(d)=='away_team' | colnames(d)=='start_flag' |colnames(d)=='play_min' |
#            colnames(d)=='play_sec' | colnames(d)=='fg' | colnames(d)=='fg_a'|colnames(d)=='ft' |
#      colnames(d)=='ft_a' | colnames(d)=='threep' | colnames(d)=='threep_a'||colnames(d)=='dk' |
#      colnames(d)=='dk_r' | colnames(d)=='pp' | colnames(d)=='pp_a')
dnew <- d[,-c(1:20)]
dim(d)
dim(dnew)
d[,c(1:5)]
d$player_no

player_n <- length( unique( d$player_no ) )
new <- matrix(0, nrow=player_n, ncol=length(colnames(dnew)) )
rownames(new) <- unique(d$player_no)
dim(new)

for( i in unique(d$player_no) ) {
  idx <- which( d$player_no == i )
  rowidx <- which( rownames(new)== i )
  new[rowidx,] <- colMeans(dnew[idx,])
}


summary(new)
rowMeans(dnew[1:5,])
colnames(new)<-colnames(dnew)
summary(new)


##### PCA 데이터

V2new<-as.matrix(as.data.frame(V2new))
dnew<-as.matrix(dnew)
PCAdata<-dnew %*% V2new
PCAdata<-as.data.frame(PCAdata)
str(PCAdata)


player_n<-length( unique( d$player_no ) )
PCAvar<-matrix(0, nrow=player_n, ncol=3 )
rownames(PCAvar)<-unique(d$player_no)


for( i  in 1:player_n ) {
  num<-unique(d$player_no)[i]
  
  if ( sum(d$player_no == num) == 1 ) {
    PCAvar[i,]<-c(0,0,0)
  }
  else{
    idx<-which( d$player_no == num )
    PCAvar[i,] <- apply(PCAdata[idx,], 2, var)
  }
}



player_var<-c(rep(0,player_n))
player_var<-as.integer(player_var)
for( i in 1:player_n ) {
  player_var[i]<-gofnew[1]*PCAvar[i,1] + gofnew[2]*PCAvar[i,2] + gofnew[3]*PCAvar[i,3]
}

plot(player_var)
summary(player_var)


####### radar chart #######
# install.packages("doBy") : 범주형 반응변수별 순서형 변수들의 평균을 구해주는 패키지
# 안에 함수로는 summaryBy(설명1+설명2~반, data=데이터, FUN=c(mean))
library(doBy)


library(dplyr)
str(d)
dint <- d[,-c(1:5,7:20)]
sum(!complete.cases(dint))
str(dint)

mean_pos <- summaryBy(o_r+d_r+a_s+s_t+b_s+gd+t_o+wft+woft+tf+ef+p_score+score+inout+inout1
                      +fo+fg_r+ft_r+threep_r+dk_r+pp_r+playtime~pos, data=dint, FUN=c(mean))
mean_pos
# install.packages("fmsb") : 레이더 차트를 그리기 위한 패키
library(fmsb)
# 레이더 차트를 작성하기 위한 데이터 형태는 데이터 프레임형태로
# 1. 첫번째 행은 max , 2. 두번째 행은 min, 세번째 행부터는 원래 관측치여야 한다.


dim(mean_pos)
df_radarchart <- function(dint){
  dint <- data.frame(dint)
  dintmax <- apply(dint, 2, max)
  dintmin <- apply(dint, 2, min)
  as.data.frame(rbind(dintmax, dintmin, dint))
}


mean_pos_trans <- df_radarchart(scale(mean_pos[,c(2:23)]))
windows()
radarchart(df = mean_pos_trans,
           seg = 12,
           pty = 20,
           pcol = 1:3,
           plty = 1:3,
           plwd = 2,
           title=c("radar chart by Position"))
legend("topleft", legend = mean_pos$pos, col = c(1:6), lty = c(1:6), lwd = 2)


########## 포지션 별 radar chart
dc <- d[d$pos=="C",]
dfd <- d[d$pos=="FD",]
dgd <- d[d$pos=="GD",]
dim(dc);dim(dfd);dim(dgd)
str(d)
# C
str(dc)
dcint <- dc[,-c(1:20)]
str(dcint)
mean_c <- summaryBy(o_r+d_r+a_s+s_t+b_s+gd+t_o+wft+woft+tf+ef+p_score+score+inout+inout1
                    +fo+fg_r+ft_r+threep_r+dk_r+pp_r+playtime~win, data=dcint, FUN=c(mean))
mean_c
str(mean_c)
dim(mean_c)
dc_racarchart <- function(dcint){
  dcint <- data.frame(dcint)
  dcintmax <- apply(dcint, 2, max)
  dcintmin <- apply(dcint, 2, min)
  as.data.frame(rbind(dcintmax, dcintmin, dcint))
}

mean_c_trans <- df_radarchart(scale(mean_c[,c(2:23)]))
windows()
radarchart(df=mean_c_trans,
           seq=6,
           pty=20,
           pcol=1:2,
           plty=1:2,
           plwd=2,
           title=c("radar chart in pos C"))
legend("topleft", legend = mean_c$win, col = c(1:6), lty = c(1:6), lwd = 2)

# FD
str(dfd)
dfdint <- dfd[,-c(1:20)]
mean_fd <- summaryBy(o_r+d_r+a_s+s_t+b_s+gd+t_o+wft+woft+tf+ef+p_score+score+inout+inout1
                     +fo+fg_r+ft_r+threep_r+dk_r+pp_r+playtime~win, data=dfdint, FUN=c(mean))
mean_fd
str(mean_fd)
dim(mean_fd)
dfd_racarchart <- function(dfdint){
  dfdint <- data.frame(dfdint)
  dfdintmax <- apply(dfdint, 2, max)
  dfdintmin <- apply(dfdint, 2, min)
  as.data.frame(rbind(dfdintmax, dfdintmin, dfdint))
}

mean_fd_trans <- df_radarchart(scale(mean_fd[,c(2:23)]))
windows()
radarchart(df=mean_fd_trans,
           seq=6,
           pty=20,
           pcol=1:2,
           plty=1:2,
           plwd=2,
           title=c("radar chart in pos FD"))
legend("topleft", legend = mean_fd$win, col = c(1:6), lty = c(1:6), lwd = 2)


# GD
str(dgd)
dgdint <- dgd[,-c(1:20)]
mean_gd <- summaryBy(o_r+d_r+a_s+s_t+b_s+gd+t_o+wft+woft+tf+ef+p_score+score+inout+inout1
                     +fo+fg_r+ft_r+threep_r+dk_r+pp_r+playtime~win, data=dgdint, FUN=c(mean))
mean_gd
str(mean_gd)
dim(mean_gd)
dgd_radarchart <- function(dgdint){
  dgdint <- data.frame(dgdint)
  dgdintmax <- apply(dgdint, 2, max)
  dgdintmin <- apply(dgdint, 2, min)
  as.data.frame(rbind(dgdintmax, dgdintmin, dgdint))
}

mean_gd_trans <- df_radarchart(scale(mean_gd[,c(2:23)]))
windows()
radarchart(df=mean_gd_trans,
           seq=6,
           pty=20,
           pcol=1:2,
           plty=1:2,
           plwd=2,
           title=c("radar chart in pos GD"))
legend("topleft", legend = mean_gd$win, col = c(1:6), lty = c(1:6), lwd = 2)






####################################### PCA로 편차구하기######################

############### ALL
d=mydata
str(d)
colname<-colnames(d)
pcaidx<-which(colname=='season_code' | colname=='game_no'| colname=='player_no'| colname=='away_team'
              | colname=='team_code' | colname=='home_away' | colname=='pos' | colname=='start_flag' | colname=='play_min' 
              | colname=='play_sec' | colname=='fg_r'| colname=="ft_r"|colname=="threep_r"| colname== "dk_r"| colname=="pp_r"| colname=='win')

pcaall<-d[,-pcaidx]
str(pcaall)

dim(pcaall)
str(pcaall)
pcaall$fo=as.integer(pcaall$fo)


#[Step 2] Covariance Matrix S(or Correlation Matix R)
Rall=round(cor(pcaall),3)
Rall
dim(Rall)


#[Step 3] Spectral Decomposition 
eigen.Rall=eigen(Rall)
round(eigen.Rall$values, 2) # Eigenvalues
Vall=round(eigen.Rall$vectors, 2) # Eigenvectors

#[Step 4] Choice of Eigenvalues and Eigenvectors
gofall=eigen.Rall$values/sum(eigen.Rall$values)*100 # Goodness-of fit
round(gofall, 2)
sum(round(gofall, 2)[1:8])

par(mar=c(5.1, 4.1, 4.1, 2.1), mgp=c(3, 1, 0), las=0)
par(mfrow=c(1,1))
plot(eigen.Rall$values, type="b", main="Scree Graph", xlab="Component Number", ylab="Eigenvalue")

#[Step 5] PCs : liner combination of original variables 
V2all=Vall[,1:8]
colnames(V2all)<-c('P1','P2','P3','P4','P5','P6','P7','P8')
rownames(V2all)<-colnames(pcaall)
V2all

#[Step 6] PCS, PCs Scores and New Data Matrix P
Zall=scale(pcaall, scale=T) # Standardized Data Matrix
Zall
Pall=Zall%*%V2all            # PCs Scores
round(Pall, 3)

#[Step 7] Plot of PCs Scores
plot(Pall[,1], Pall[, 2], main="Plot of PCs Scores", xlab="1st PC", ylab="2nd PC")
text(Pall[,1], Pall[, 2], labels=rownames, cex=0.8, col="blue", pos=3)
abline(v=0, h=0)

########## 편차계산
summary(Pall)
is.data.frame(Pall)
Pall<-as.data.frame(Pall)
dim(Pall)
Pall$player_no<-d$player_no
summary(Pall)

n<-length(unique(d$player_no))
player_dev<-matrix(NA, nrow=n , ncol=9 )
for( i in 1:n ) {
  player_dev[i,9]<-as.character(unique(d$player_no)[i])
  playeridx<-which(Pall$player_no==unique(d$player_no)[i])
  player_dev[i,1:8]<-apply(Pall[playeridx,1:8], 2, var)
}

summary(player_dev)
player_dev<-as.data.frame(player_dev)
str(player_dev)


naidx<-which(is.na(player_dev[,1]))
length(naidx)
player_dev[naidx,]
naidx<-rownames(player_dev)[naidx]
naidx<-as.numeric(naidx)
n<-length(naidx)

s<-rep(NA,n)
for( i in 1:n ){
  no<-player_dev[naidx[i],9]
  s[i]<-sum(d$player_no==no)
}
s

playerno<-player_dev[naidx,9]

player_dev[naidx,9]

##########player_dev : 전체 선수(1번만 뛴 선수는 제거)마다 각 변수에 대한 편차 계산 값

summary(player_dev)
is.data.frame(player_dev)
for( i in 1:8){
  player_dev[,i]<-as.numeric(player_dev[,i])
}


player_devsum<-rowSums(player_dev[,1:8])
names(player_devsum)<-player_dev[,9]
player_devsum
##########player_devsum : 전체 선수(1번만 뛴 선수는 제거)마다 각 변수에 대한 편차의 합


sort_devsum<-sort(player_devsum, decreasing=T)
summary(sort_devsum)
plot(sort_devsum)
boxplot(sort_devsum)

which(sort_devsum==1500)
sort_devsum[1:69]
##########sort_devsum : 전체 선수(1번만 뛴 선수는 제거)마다 각 변수에 대한 편차의 합 ==> 정렬


str(ALL_30)
ALL_30=rbind (C_10,GD_10,FD_10)
length(unique(ALL_30$player_no))


remove <- c("291107", "290395", "291180", "291171", "291011", "291108", "290978", "291058", "290367", "290981", "290774",
            "291132", "291139", "291133", "291055", "290861", "291137", "291134", "291014", "291129", "290371", "290982",
            "291140", "290261", "290325", "291138", "291052", "291057", "290993", "290524", "291145", "291233", "291109",
            "290738", "291172", "290984", "290742", "290983", "290412", "290794", "291144", "290381", "290797", "230080",
            "290280", "290542", "290750", "291220", "290505", "290976", "290106", "291056", "215053", "291136", "291234",
            "290790", "290485", "290407", "290862", "290765", "290740", "290558", "290909", "235070", "290450", "290553",
            "220194", "290979")


do=rep(0, length(remove))

for (i in 1:length(remove)){
  do[i]=sum(ALL_30$player_no==remove[i])
}

remove_code=remove[which(do==1)]

for (i in 1:length(remove_code)){
  ALL_30=ALL_30[-which(ALL_30$player_no==remove_code[i]),]
}
length(ALL_30$player_no)
# 30명 중에서 편차큰 9명 제거 후 21명 남음

finalC <- ALL_30[ALL_30$pos=="C",]
finalFD <- ALL_30[ALL_30$pos=="FD",]
finalGD <- ALL_30[ALL_30$pos=="GD",]
finalC

dim(finalC)[1];dim(finalGD)[1];dim(finalFD)[1]

finalC <- finalC[1:2,2:3]
finalFD <- finalFD[1:5,2:3]
finalGD <- finalGD[1:5,2:3]
finalC;finalFD;finalGD


playerCscore <- playerc%*%Cmat

# 33,39,43,45,46,48,59,65,67
remove[67]
#290993 / 290742 / 290381 / 290280 / 290750 / 290505 / 290407 / 290450 / ★290417


########## 편차계산 ###############

summary(Pall)
is.data.frame(Pall)
Pall<-as.data.frame(Pall)
dim(Pall)
Pall$player_no<-d$player_no
summary(Pall)



n<-length(unique(d$player_no))
player_dev<-matrix(NA, nrow=n , ncol=9 )
for( i in 1:n ) {
  player_dev[i,9]<-as.character(unique(d$player_no)[i])
  playeridx<-which(Pall$player_no==unique(d$player_no)[i])
  player_dev[i,1:8]<-apply(Pall[playeridx,1:8], 2, var)
}

summary(player_dev)
player_dev<-as.data.frame(player_dev)
str(player_dev)



naidx<-which(is.na(player_dev[,1]))
length(naidx)
player_dev[naidx,]
naidx<-rownames(player_dev)[naidx]
naidx<-as.numeric(naidx)
n<-length(naidx)


s<-rep(NA,n)
for( i in 1:n ){
  no<-player_dev[naidx[i],9]
  s[i]<-sum(d$player_no==no)
}
s

playerno<-player_dev[naidx,9]

sum(is.na(player_dev[-naidx,]))
player_dev<-player_dev[-naidx,]


##########player_dev : 전체 선수(1번만 뛴 선수는 제거)마다 각 변수에 대한 편차 계산 값

summary(player_dev)
is.data.frame(player_dev)
for( i in 1:8){
  player_dev[,i]<-as.numeric(player_dev[,i])
}


player_devsum<-rowSums(player_dev[,1:8])
names(player_devsum)<-player_dev[,9]
player_devsum
##########player_devsum : 전체 선수(1번만 뛴 선수는 제거)마다 각 변수에 대한 편차의 합


sort_devsum<-sort(player_devsum, decreasing=T)
summary(sort_devsum)
plot(sort_devsum)
boxplot(sort_devsum)

which(sort_devsum==1498)
sort_devsum[1:69]
##########sort_devsum : 전체 선수(1번만 뛴 선수는 제거)마다 각 변수에 대한 편차의 합 ==> 정렬


ALL_42=rbind (C_14,GD_14,FD_14)
ze <- which(ALL_42$player_no==0)
ALL_42 <- ALL_42[-ze,]
ALL_42 <- ALL_42[-c(19,33,36),]
length(ALL_42[,2])

remove=c("291107", "290395", "291180", "291171", "291011", "291108", "290978", "291058",
         "290367", "290981", "290774", "291132", "291133", "291055", "291139", "290861",
         "291137", "291014", "291134", "291129", "290371", "290982", "291140", "290325",
         "290261", "291138", "291052", "291057", "230080", "290524", "291145", "290738",
         "290993", "291233", "291109", "290984", "291172", "290983", "290742", "290412",
         "290794", "291144", "290381", "290797", "290280", "290750", "290542", "290505",
         "291220", "290976", "290106", "291056", "291234", "215053", "291136", "290790",
         "290485", "290862", "290407", "290765", "290558", "290909", "290740", "290450",
         "291015", "290553", "290417", "235070", "290227")


do=rep(0, length(remove))
for (i in 1:length(remove)){
  do[i]=sum(ALL_42$player_no==remove[i])
}
sum(do)
remove_code=remove[which(do==1)]

for (i in 1:length(remove_code)){
  ALL_42=ALL_42[-which(ALL_42$player_no==remove_code[i]),]
}
length(ALL_42$player_no)
ALL_42

finalC0 <- ALL_42[ALL_42$pos=="C",]
finalFD0 <- ALL_42[ALL_42$pos=="FD",]
finalGD0 <- ALL_42[ALL_42$pos=="GD",]


dim(finalC)[1];dim(finalGD)[1];dim(finalFD)[1]

finalC <- finalC0[1:4,2:3]
finalFD <- finalFD0[1:6,2:3]
finalGD <- finalGD0[1:6,2:3]
finalC;finalFD;finalGD

dim(d)
dim(dc)
dim(dfd)
dim(dgd)

############## 성장률 #############
str(d)
fdd <- d[,-1]
str(fdd)

fddidx <- which(colnames(fdd)=="game_no" | colnames(fdd)=="team_code" | colnames(fdd)=="home_away" | colnames(fdd)=="player_no" | 
                  colnames(fdd)=="pos" | colnames(fdd)=="away_team" | colnames(fdd)=="start_flag" | 
                  colnames(fdd)=="play_min" | colnames(fdd)=="play_sec" | colnames(fdd)=="fg" | 
                  colnames(fdd)=="fg_a" | colnames(fdd)=="ft" | colnames(fdd)=="ft_a" | colnames(fdd)=="threep" | 
                  colnames(fdd)=="threep_a" | colnames(fdd)=="dk" | colnames(fdd)=="dk_a" | colnames(fdd)=="pp" |  
                  colnames(fdd)=="pp_a" | colnames(fdd)=="win") 

fdd <- fdd[,-fddidx]
fdd <- fdd[,-c(5,6,10,11,20)]
str(fdd)
fdd <- as.matrix(fdd)

pmat<-matrix(0, nrow=162 , ncol=30 )
FDgini
FDmat <- c(FDgini[10],FDgini[3],FDgini[5],FDgini[16],FDgini[14],FDgini[12],FDgini[13],
           FDgini[15],FDgini[2],FDgini[9],FDgini[7],FDgini[11],FDgini[4],FDgini[17],
           FDgini[6],FDgini[8],FDgini[1])
FDmat <- as.matrix(FDmat)

for(i in 1:length(remove_code)){
  no <- remove_code[i]
  a <- which(d$player_no==no)
  for(j in 1:length(a)){
    resu <- fdd[a[j],]%*%FDmat
    pmat[j,i] <- resu
  }
}

#### 성장률 그래프

c <- pmat[,1]
cidx <- which(c==0)
re1 <- c[-cidx]
c <- pmat[,2]
cidx <- which(c==0)
re2 <- c[-cidx]
c <- pmat[,3]
cidx <- which(c==0)
re3 <- c[-cidx]
c <- pmat[,4]
cidx <- which(c==0)
re4 <- c[-cidx]
c <- pmat[,5]
cidx <- which(c==0)
re5 <- c[-cidx]
c <- pmat[,6]
cidx <- which(c==0)
re6 <- c[-cidx]
c <- pmat[,7]
cidx <- which(c==0)
re7 <- c[-cidx]
c <- pmat[,8]
cidx <- which(c==0)
re8 <- c[-cidx]
c <- pmat[,9]
cidx <- which(c==0)
re9 <- c[-cidx]
c <- pmat[,10]
cidx <- which(c==0)
re10 <- c[-cidx]
c <- pmat[,11]
cidx <- which(c==0)
re11 <- c[-cidx]
c <- pmat[,12]
cidx <- which(c==0)
re12 <- c[-cidx]
c <- pmat[,13]
cidx <- which(c==0)
re13 <- c[-cidx]
c <- pmat[,14]
cidx <- which(c==0)
re14 <- c[-cidx]
c <- pmat[,15]
cidx <- which(c==0)
re15 <- c[-cidx]
c <- pmat[,16]
cidx <- which(c==0)
re16 <- c[-cidx]
c <- pmat[,17]
cidx <- which(c==0)
re17 <- c[-cidx]
c <- pmat[,18]
cidx <- which(c==0)
re18 <- c[-cidx]
c <- pmat[,19]
cidx <- which(c==0)
re19 <- c[-cidx]
c <- pmat[,20]
cidx <- which(c==0)
re20 <- c[-cidx]
c <- pmat[,21]
cidx <- which(c==0)
re21 <- c[-cidx]
c <- pmat[,22]
cidx <- which(c==0)
re22 <- c[-cidx]
c <- pmat[,23]
cidx <- which(c==0)
re23 <- c[-cidx]
c <- pmat[,24]
cidx <- which(c==0)
re24 <- c[-cidx]
c <- pmat[,25]
cidx <- which(c==0)
re25 <- c[-cidx]
c <- pmat[,26]
cidx <- which(c==0)
re26 <- c[-cidx]
c <- pmat[,27]
cidx <- which(c==0)
re27 <- c[-cidx]
c <- pmat[,28]
cidx <- which(c==0)
re28 <- c[-cidx]
c <- pmat[,29]
cidx <- which(c==0)
re29 <- c[-cidx]
c <- pmat[,30]
cidx <- which(c==0)
re30 <- c[-cidx]
plot(pmat)

########## FD ##############



# 33,39,43,45,46,48,59,65,67
remove[67]

# C  : 290750(김종규) / 290450(오세근)
# FD : 290993(최준용) / 290280(문태용) / 
#      290417(최진수)
# GD : 290742(이대성) / 290381(이정현) / 
#      290505(김시래) / 290407(김선형)



####### C 성장률

d29 <- d[d$season_code==29,]
d31 <- d[d$season_code==31,]
d33 <- d[d$season_code==33,]

str(d29)

a <- which(d29$player_no=="290450")
b <- which(d31$player_no=="290450")
c <- which(d33$player_no=="290450")

d29 <- d29[,-c(1,2,3,4,5,6:20,24,25,26,30,31,37,40,41)]
d31 <- d31[,-c(1,2,3,4,5,6:20,24,25,26,30,31,37,40,41)]
d33 <- d33[,-c(1,2,3,4,5,6:20,24,25,26,30,31,37,40,41)]
dim(d29);dim(d31);dim(d33)

d29 <- as.matrix(d29)
d31 <- as.matrix(d31)
d33 <- as.matrix(d33)

Cgini
Cmat <- c(Cgini[6],Cgini[3],Cgini[7],Cgini[13],Cgini[11],Cgini[15],Cgini[9],
          Cgini[4],Cgini[12],Cgini[10],Cgini[8],Cgini[2],Cgini[14],Cgini[5],Cgini[1])
Cmat <- as.matrix(Cmat)


resu29 <- rep(0,length(a))
for(i in 1:length(a)){
  resu29[i] <- d29[a[i],]%*%Cmat
}

resu31 <- rep(0,length(b))
for(i in 1:length(b)){
  resu31[i] <- d31[b[i],]%*%Cmat
}

resu33 <- rep(0,length(c))
for(i in 1:length(c)){
  resu33[i] <- d33[c[i],]%*%Cmat
}

resu29
resu31
resu33
mean(resu29);mean(resu31);mean(resu33)

mi1 <- min(resu29, resu31, resu33)
ma1 <- max(resu29, resu31, resu33)
par(mfrow=c(1,3))
boxplot(resu29, ylim=c(min(resu29, resu31, resu33),max(resu29, resu31, resu33)))
boxplot(resu31, ylim=c(min(resu29, resu31, resu33),max(resu29, resu31, resu33)))
boxplot(resu33, ylim=c(min(resu29, resu31, resu33),max(resu29, resu31, resu33)))






####### FD 성장률

d29 <- d[d$season_code==29,]
d31 <- d[d$season_code==31,]
d33 <- d[d$season_code==33,]

str(d29)

a <- which(d29$player_no=="290417")
b <- which(d31$player_no=="290417")
c <- which(d33$player_no=="290417")

d29 <- d29[,-c(1,2,3,4,5,6:20,25,26,30,31,37,41)]
d31 <- d31[,-c(1,2,3,4,5,6:20,25,26,30,31,37,41)]
d33 <- d33[,-c(1,2,3,4,5,6:20,25,26,30,31,37,41)]
dim(d29);dim(d31);dim(d33)

d29 <- as.matrix(d29)
d31 <- as.matrix(d31)
d33 <- as.matrix(d33)

FDgini
FDmat <- c(FDgini[10],FDgini[3],FDgini[5],FDgini[16],FDgini[14],FDgini[12],FDgini[13],
           FDgini[15],FDgini[2],FDgini[9],FDgini[7],FDgini[11],FDgini[4],FDgini[17],
           FDgini[6],FDgini[8],FDgini[1])
FDmat <- as.matrix(FDmat)

resu29 <- rep(0,length(a))
for(i in 1:length(a)){
  resu29[i] <- d29[a[i],]%*%FDmat
}

resu31 <- rep(0,length(b))
for(i in 1:length(b)){
  resu31[i] <- d31[b[i],]%*%FDmat
}

resu33 <- rep(0,length(c))
for(i in 1:length(c)){
  resu33[i] <- d33[c[i],]%*%FDmat
}

resu29
resu31
resu33
mean(resu29);mean(resu31);mean(resu33)

mi1 <- min(resu29, resu31, resu33)
ma1 <- max(resu29, resu31, resu33)
par(mfrow=c(1,3))
boxplot(resu29, ylim=c(min(resu29, resu31, resu33),max(resu29, resu31, resu33)))
boxplot(resu31, ylim=c(min(resu29, resu31, resu33),max(resu29, resu31, resu33)))
boxplot(resu33, ylim=c(min(resu29, resu31, resu33),max(resu29, resu31, resu33)))


####### GD 성장률


d29 <- d[d$season_code==29,]
d31 <- d[d$season_code==31,]
d33 <- d[d$season_code==33,]

str(d29)

a <- which(d29$player_no=="290381")
b <- which(d31$player_no=="290381")
c <- which(d33$player_no=="290381")

d29 <- d29[,-c(1,2,3,4,5,6:20,25,26,30,31,37,41)]
d31 <- d31[,-c(1,2,3,4,5,6:20,25,26,30,31,37,41)]
d33 <- d33[,-c(1,2,3,4,5,6:20,25,26,30,31,37,41)]
dim(d29);dim(d31);dim(d33)

d29 <- as.matrix(d29)
d31 <- as.matrix(d31)
d33 <- as.matrix(d33)

GDgini
GDmat <- c(GDgini[15],GDgini[4],GDgini[3],GDgini[13],GDgini[9],GDgini[12],GDgini[10],
           GDgini[16],GDgini[2],GDgini[8],GDgini[7],GDgini[11],GDgini[6],GDgini[17],
           GDgini[5],GDgini[14],GDgini[1])
GDmat <- as.matrix(GDmat)

resu29 <- rep(0,length(a))
for(i in 1:length(a)){
  resu29[i] <- d29[a[i],]%*%GDmat
}

resu31 <- rep(0,length(b))
for(i in 1:length(b)){
  resu31[i] <- d31[b[i],]%*%GDmat
}

resu33 <- rep(0,length(c))
for(i in 1:length(c)){
  resu33[i] <- d33[c[i],]%*%GDmat
}

resu29
resu31
resu33
mean(resu29);mean(resu31);mean(resu33)

mi1 <- min(resu29, resu31, resu33)
ma1 <- max(resu29, resu31, resu33)
par(mfrow=c(1,3))
boxplot(resu29, ylim=c(min(resu29, resu31, resu33),max(resu29, resu31, resu33)))
boxplot(resu31, ylim=c(min(resu29, resu31, resu33),max(resu29, resu31, resu33)))
boxplot(resu33, ylim=c(min(resu29, resu31, resu33),max(resu29, resu31, resu33)))



########## 김준일 레전드 경기 뽑기 - 290781 MD ########


d3

str(d3)

a <- which(d3$player_no=="290781")

fd3 <- d3[,-c(1,2,3,4,5,6:20,24,25,26,30,31,37,40,41)]

d3[3995,]
fd3 <- as.matrix(fd3)

Cgini
Cmat <- c(Cgini[7],Cgini[3],Cgini[8],Cgini[14],Cgini[12],Cgini[15],Cgini[10],
          Cgini[4],Cgini[11],Cgini[9],Cgini[6],Cgini[2],Cgini[13],Cgini[5],Cgini[1])
Cmat <- as.matrix(Cmat)

Cmat
str(fd3)
fd3[3995,]%*%Cmat

# 3995, 4071, 4193, 4273, 4419, 4488, 4648
diag(Cmat%*%fd3[3995,])

fd3[3995,]%*%Cmat
fd3[4071,]%*%Cmat
fd3[4193,]%*%Cmat
fd3[4273,]%*%Cmat
fd3[4419,]%*%Cmat
fd3[4488,]%*%Cmat
fd3[4648,]%*%Cmat
