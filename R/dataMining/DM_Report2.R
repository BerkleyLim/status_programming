# 경로 설정
setwd("C:/Users/limhs/Desktop/R_Report")

# 데이터 불러오기, colleges.XL.zip.xls 파일을 clooeges.XL.csv로 대처
colleges <- read.csv("colleges.XL.csv")

# 데이터 형태 파악
summary(colleges) # 이것으로 NA 잇는 값을 확인한다.
head(colleges)

# 전체 열(column)이름 보기
colnames(colleges) # 이것으로 행의 의미가 무엇인지 본다.
names(colleges)

# 테이블 colleges 포함
attach(colleges)

# 1-1번 - 결측값 데이터 제거 하기 , 각각 제거 (attach한 것은 colleges의 행 이름을 변수로 취급 가능)
# 주의 사항 -which(is.na(colleges$테이블명)) 할때에는, summary(college$테이블명)으로 하여 
# Na's 0일 때 테이블 NA 값의 테이블을 지울려고 할 시, 기존 데이터 모두 지워지므로 항상 summary로 NA값 확인!!
colleges <- colleges[-which(is.na(colleges$Math.SAT) ),] # colleges <- colleges[-which(is.na(Verbal.SAT) ),]도 Na's 0
colleges <- colleges[-which(is.na(colleges$ACT)) , ]
colleges <- colleges[-which(is.na(colleges$X..appli..rec.d)), ] 
                  # colleges <- colleges[-which(is.na(colleges$X..appl..accepted)), ] 지금까지 작업상 Na's 0
                  # colleges <- colleges[-which(is.na(colleges$X..new.stud..enrolled)), ]
colleges <- colleges[-which(is.na(colleges$X..new.stud..from.top.10.)), ]
colleges <- colleges[-which(is.na(colleges$X..new.stud..from.top.25.)), ]
                  # colleges <- colleges[-which(is.na(colleges$X..FT.undergrad)), 
colleges <- colleges[-which(is.na(colleges$X..PT.undergrad)), ]
colleges <- colleges[-which(is.na(colleges$in.state.tuition)), ]
                  # colleges <- colleges[-which(is.na(colleges$out.of.state.tuition)), ]
colleges <- colleges[-which(is.na(colleges$room)), ]
colleges <- colleges[-which(is.na(colleges$board)), ]
colleges <- colleges[-which(is.na(colleges$add..fees)), ]
                  # colleges <- colleges[-which(is.na(colleges$estim..book.costs)), ]
colleges <- colleges[-which(is.na(colleges$estim..personal..)), ]
colleges <- colleges[-which(is.na(colleges$X..fac..w.PHD)), ]
colleges <- colleges[-which(is.na(colleges$Graduation.rate)), ]

# 중간중간에 결측치를 확인하면서 일일히 제거를 하였다.
summary(colleges$Graduation.rate) # 이것으로 NA 잇는 값을 확인한다. <중간중간 검사하다가 결국 밑까지 내려옴>

# 마지막으로 결측치 여부 확인
which(is.na(colleges))

# 1-2번
# 연속형 불려오기
x <- colleges[,18] # 수업료
y <- colleges[,22] # 졸업률

dx <- round(dist(x), 2) # 소숫점 2자리까지
dy <- round(dist(y), 2)

# 덴드로그램 - 복잡하게 나온다. (총 206개)
hc1 <- hclust(dist(x)^2, method="complete") #최장 연결법 - 수업료
hc2 <- hclust(dist(y)^2, method="complete") #최장 연결법 - 졸업률
plot(hc1, labels=names(x) ,hang=1, main="dendrogram : complete - result_stu_fee")
plot(hc2, labels=names(y) ,hang=1 ,main="dendrogram : complete - result_greduation")

# 1-3번 산포도 표현법 및 통계 분석
#수업료 - 6개 군집이 적당
x <- colleges[,18] # 수업료
hc1 <- hclust(dist(x)^2, method="complete") #최장 연결법 - 수업료
c1.num <- 6  # setting number of clusters (군집 여부 결정)
hc1.result <- cutree(hc1,k=c1.num)
plot(x, pch=hc1.result, col=hc1.result, xlab = "전체 학교 수", ylab = "수업료", main="수업료") 

#졸업률 - 5개 군집이 적당
y <- colleges[,22] # 졸업률
hc2 <- hclust(dist(y)^2, method="complete") #최장 연결법 - 졸업률
c2.num <- 5
hc2.result <- cutree(hc2,k=c2.num)
plot(y, pch=hc2.result, col=hc2.result, xlab = "전체 학교 수", ylab = "졸업률", main="졸업률")  



# 1-4 범주형 자료를 활용 <1-3 문제 일부와 일부를 이어서> - 수업료와 state 관계
yy <- table(colleges[,3])  # 공/사립 범주 갯수

plot(x,pch=hc2.result, col=hc2.result,  xlab="전체학교 수", ylab="수업료", main="수업료")
text(x,labels=State ,adj=-0.1, cex=0.8, main="complete")

detach(colleges)

# ---------------------------------------------------- 2장

# 데이터 불러오기
Air <- read.csv("EastWestAirlinesCluster.csv")

# 데이터 내용 표시
colnames(Air)

library(mlbench)
library(stats)

# 데이터 프레임 접근
attach(Air)

# 다음은 2-1) -> Balance는 각각의 마일리지 관계도를 나타낸다.

# 먼저 표준화 작업부터 시작한다.
# Balance 표준화
Bal <- (Balance - mean(Balance) ) / sqrt(var(Balance))
Bal <- round(Bal, 2)
# Qual_mile 표준화
Q_m <- (Qual_miles - mean(Qual_miles) ) / sqrt(var(Qual_miles))
Q_m <- round(Q_m, 2)
#cc1_miles, cc2_miles, cc3_miles 는 summary로 직접 변수 대입해도 크게 영향을 미치지 않음
summary(cc1_miles); summary(cc2_miles); summary(cc3_miles) # 즉 Balance에선 영향이 없다.
#Bonus_miles 표준화
B_m <- (Bonus_miles - mean(Bonus_miles) ) / sqrt(var(Bonus_miles))
B_m <- round(B_m, 2)



# 다음에 표준화 후의 관계도를 그려본다.
#plot Balance, Qual_miles 관계도
x <- hclust( dist(Bal)^2 , method="single")
y <- hclust( dist(Q_m)^2, method = "single")
hcx.result <- cutree(x, k=1)
hcy.result <- cutree(y, k=10)

plot(Bal, Q_m, xlab="Balance", ylab="Qual_mails" ,type="p", main ="plot Balance, Qual_miles 관계도"
     , pch=hcy.result, col=hcy.result)
text(hcx.result, hcy.result, labels = as.character(ID.), cex=0.8, pos=2, col="red")

#plot Balance, Bonus_miles 관계도
z <- hclust(dist(B_m), method = "complete")
hcz.result <- cutree(z, k=2)

plot(Bal, B_m, xlab="Balance", ylab="Bonus_miles", type="p", main = "plot Balance, Bonus_miles 관계도"
     , pch=hcz.result, col=hcz.result)
text(Balance, Bonus_miles, labels = as.character(ID.), cex=0.8, pos=2, col="red")


#2-2 문제
plot(Balance, Qual_miles, xlab="Balance", ylab="Qual_mails" ,type="p", main ="plot Balance, Qual_miles 관계도"
     , pch=hcy.result, col=hcy.result)
text(hcx.result, hcy.result, labels = as.character(ID.), cex=0.8, pos=2, col="red")

# 2-3 문제
#K-means
#3개 군집
# 1) Balance, Qual_miles
Q <- kmeans(Q_m,centers=5)

attributes(Q)
plot(Bal, Q_m, xlab="Balance", ylab="Bonus_miles", type="p", main = "plot Balance, Bonus_miles 관계도"
     , pch=Q$cluster, col=Q$cluster)
text(Bal, Q_m,labels=as.character(ID.),adj=-0.1, cex=0.8)

# 2) Balance, Bonus_miles
B <- kmeans(B_m,centers=3)

attributes(B)
plot(Bal, B_m, xlab="Balance", ylab="Bonus_miles", type="p", main = "plot Balance, Bonus_miles 관계도"
     , pch=B$cluster, col=B$cluster)
text(Bal, B_m,labels=as.character(ID.),adj=-0.1, cex=0.8)


# 2-4 문제
# 따로 복사하고, 95%의 데이터만 사용하기
# 1) Balance
bals <- Bal
set.seed(550)
idx <- sample(length(bals), length(bals)*0.05)
bals <- bals[-idx]


# 2) Qual_miles
quir <- Q_m
set.seed(550)
idx <- sample(length(quir), length(quir)*0.05)
quir <- quir[-idx]

# 3) Bonux_miles
bonus <- B_m
set.seed(550)
idx <- sample(length(bonus), length(bonus)*0.05)
bonus <- bonus[-idx]

# (2-1의 계층적 분석도)
plot(bals, quir, xlab="Balance", ylab="Qual_mails" ,type="p", main ="plot Balance, Qual_miles 관계도"
     , pch=hcy.result, col=hcy.result)
plot(bals, bonus, xlab="Balance", ylab="Bonus_miles", type="p", main = "plot Balance, Bonus_miles 관계도"
     , pch=hcz.result, col=hcz.result)


# 2-5 문제 - k - 평균

# 1) Balance, Qual_miles
Q1 <- kmeans(quir,centers=5)

plot(bals, quir, xlab="Balance", ylab="Qual_miles", type="p", main = "plot Balance, Qual_miles 관계도"
     , pch=Q1$cluster, col=Q1$cluster)
text(bals, quir,labels=as.character(ID.),adj=-0.1, cex=0.8)

# 2) Balance, Bonus_miles
B1 <- kmeans(bonus,centers=3)

plot(bals, bonus, xlab="Balance", ylab="Bonus_miles", type="p", main = "plot Balance, Bonus_miles 관계도"
     , pch=B1$cluster, col=B1$cluster)
text(bals, bonus,labels=as.character(ID.),adj=-0.1, cex=0.8)



# 데이터 프레임 접근 해제
detach(Air)
