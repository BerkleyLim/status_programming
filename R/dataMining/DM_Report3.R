# 경로 설정
setwd("C:/Users/limhs/Desktop/R_Report")

# 1번 문제
eBay <- read.csv("eBayAuctions.csv", header=TRUE)

#(데이터 전처리) 범주형 예측변수들에 대한 가변수들을 만드시오. 
#가변수에는 물품범주(18개 범주), 통화(USD, GBP, Euro), 경매마감일(월요일-일요일) 
#그리고 경매기간(1, 3, 5, 7 또는 10일)을 포함하시오. 

# 거래 데이터 종류 표시
colnames(eBay)



#1) 통화량 변경 levels가 0 = USD, 1 = GBP, 2 = Euro
currencyD <- factor(eBay$currencyD)
levels(currencyD) # 구성요소 확인

#데이터 전처리 - 통화량
levels(currencyD) <- c("USD", "GBP", "Euro")
levels(currencyD) # 변경된 구성요소 확인



#2) 경매마감일 변경 levels가 0 = 월, 1 = 화, 2 = 수, 3 = 목, 4 = 금, 5 = 토, 6 = 일
endDayD <- factor(eBay$endDayD)
levels(endDayD) # 각각 구성 요소 저장

# 데이터 전처리 -요일
levels(endDayD) <- c("월요일", "화요일", "수요일", "목요일", "금요일", "토요일", "일요일")
levels(endDayD) # 변경된 구성요소 확인




#3) 경매 기간 levels이 0 = 1일, 1 = 3일, 2 = 5일, 3 = 7일, 4 = 10일
DurationD <- factor(eBay$DurationD)
levels(DurationD) # 구성요소 확인 ( 변경전 )

# 변경
levels(DurationD) <- c("1일", "3일", "5일", "7일", "10일")
levels(DurationD) # 구성요소 확인 ( 변경 후 )


#4) 물품 리스트 0~17 여기서 그냥 물품 이름은 물품1~18 로 둔다.
CategoryD <- factor(eBay$CategoryD)
levels(CategoryD)

levels(CategoryD) <- c("물품0", "물품1", "물품2", "물품3",
                       "물품4", "물품5", "물품6", "물품7",
                       "물품8", "물품9", "물품10", "물품11",
                       "물품12", "물품13", "물품14", "물품15",
                       "물품16", "물품17")


# 1-1 문제, 모든 예측변수 사용하여 분류나무 생성,
# 이후 가지치기 하여 최적화된 의사결정나무모델 만들기
library(mlbench) 
library(tree)
library(stats)

#data(eBay), 어떤 것이 경쟁이 있는지의 대해서 먼저 둔다.
Competitive <- eBay$Competitive
T1<-tree(Competitive~., eBay)
summary(T1)
 

#pdf("tree.pdf")
plot(T1, type="uniform") 
text(T1) 
mtext("가격과 화폐의 따른 경쟁률", side=3, line=1, col=2, cex=2) 
dev.off()

prune.tree(T1.1)


#1-2 문제, 학습 데이터 세트 60%, 테스트 데이터 세트 40% 비율
#여기서 나눈다.
partition <- sample(length(eBay[,1]), size=(length(eBay[,1])*0.6) )
train.eBay<-eBay[partition,] # 학습 데이터 셋트
test.eBay<-eBay[-partition,] # 테스트 데이터 셋트

# 학습 데이터를 TReBay로 저장(경쟁율)
TReBay.tr=tree(train.eBay$Competitive~., train.eBay) 
summary(TReBay.tr) 
TReBay.tr

# 학습용 데이터 출력
plot(TReBay.tr) 
text(TReBay.tr, all=T)

# 훈련 데이터의 따른 테스트 데이터 예측하기!!
pred<-predict(TReBay.tr, test.eBay) 
table(round(pred,0)==test.eBay[,8])



# 2번 문제
Flight <- read.csv("FlightDelays.csv")
colnames(Flight)


#다음 중 전처리 할 것!!
#1) 요일
DAY_WEEK <- factor(Flight$DAY_WEEK)
levels(DAY_WEEK) <- c("mon","tue","wed","thus","fry","sat","sun")
levels(DAY_WEEK)

#2) 항공기
TAIL_NUM <- factor(Flight$TAIL_NUM)
levels(TAIL_NUM)

#3) 출발공항
CARRIER <- factor(Flight$CARRIER)
levels(CARRIER)

#4) 도착공항
DEST <- factor(Flight$DEST)
levels(DEST)

#5) 예정 출발 시간 두 시간 단위 구간
#여기서 시간만 표시한다,(시간과 분이 있기 때문에 시간으로 나눔)
CRS_DEP_TIME <- round(Flight$CRS_DEP_TIME / 100, 0)
CRS_DEP_TIME <- factor(CRS_DEP_TIME)
levels(CRS_DEP_TIME) # 6 ~ 21시까지 표시


# 임시 나눌때,
# 그룹화 하기!! 구간화 하기 6~22시까지 2시간 단위로
#levels(Flight$CRS_DEP_TIME) <- c("6~8시","6~8시","8~10시","8~10시",
 #                                "10~12시","10~12시","12~14시","12~14시",
  #                               "14~16시","14~16시","16~18시","16~18시",
   #                              "18~20시","18~20시","20~22시","20~22시")

#levels(Flight$CRS_DEP_TIME) # 구간화 후

library(mlbench) 
library(tree)
library(stats)

# 2.1 문제
# 먼저 연착상태 만들기

Flight.Status <- Flight$Flight.Status
library(rpart)
library(rpart.plot)
ukm.tr.cart <- rpart(Flight.Status ~DAY_WEEK+DEST+CARRIER, Flight) 
ukm.tr.cart

prp(ukm.tr.cart, type=4, extra=1, digits=3, main="항공기 연착 상태")
table(ukm.tr.cart)

#조건없이 종속변수만!!
ukm.tr.cart <- rpart(Flight.Status ~., Flight)
prp(ukm.tr.cart, type=4, extra=1, digits=3, main="항공기 연착 상태")

yhat <- predict(ukm.tr.cart, type="class")
table(yhat)

# 2.2 문제 
# 먼저 월요일 오전 7시 나타내기
test <- Flight[which(CRS_DEP_TIME == "7"),] # 오전 7시
test$CRS_DEP_TIME <- round(test$CRS_DEP_TIME / 100 , 0)
test <- test[which(test$DAY_WEEK == 1),] # 여기서 월요일만 (초기 값 1)



# 작업 시작
f <- test$Flight.Status # 다시 전처리 (연착상태)


ukm.tr <- rpart(f~., test)
yhat <- predict(ukm.tr, type="class")
table(yhat)

prp(ukm.tr, type=4, extra=1, digits=3, main="항공기 연착 상태")

