# 경로 설정
setwd("C:/Users/limhs/Desktop/R_Report") 
library(MASS)
#생성
sex <- c("M","M","M","M","M","M","M","M","M","M","M","M","M","M","M",
         "F","F","F","F","F","F","F","F","F","F","F","F","F","F","F")
FSIQ <- c(149, 139, 133, 89, 133, 141, 135, 100, 80, 83, 97, 139, 141, 103, 144,
          133, 137, 99, 138, 92, 132, 140, 96, 83, 132,101,135,91,85,77)
weight <- c(166,143,172,134,172,151,155,178,180,166,186,132,171,187,191,
            118,147,146,138,175,118,155,146,135,127,136,122,114,140,106)
height <- c(72.5,73.3,68.8,63.3,68.8,70,69.1,73.5,70,71.4,76.5,68,72,77,67,
            64.5,65,69,64.5,66,64.5,70.5,66,68,68.5,66.3,62,63,68,63)

data <- data.frame(sex, FSIQ, weight, height)

# 판별 분석 Y = 성별, x = FSIQ, weight, height
disc.fit <- lda(factor(sex) ~ FSIQ + weight + height, data = data)

# 분류표
pred_sex <- predict(disc.fit, newdata = data[,-1])
pred_sex
result <- table(data$sex, pred_sex$class)
result

#정분류율
accuracy <- 100* sum(diag(result)) / sum(result)
accuracy

plot()


x1<-c(41, 38, 27, 36, 25, 48, 40, 32, 10, 35, 22, 22, 15, 19, 25)
x2<-c(45, 21, 26, 23, 30, 49, 35, 28, 23, 42, 18, 26, 22, 30, 35)
y<-c(2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1)
cld<-lda(y~x1+x2) 
cld
aa<-apply(cld$means%*%cld$scaling, 2, mean)
aa

predict(cld)

table(y, predict(cld)$class)

plot(cld, dimen=1)


new=data.frame(x1=c(20), x2=c(40)) 
predict(cld, new)


library(caTools)
xx<-data.frame(x1=c(x1), x2=c(x2)) 
colAUC(xx, y, plotROC=T)

plot(x1, x2, pch=paste(y), mean="Linear Disciminant Plot") 
abline(aa/cld$scaling[2], -cld$scaling[1]/cld$scaling[2])
legend("bottomright", "1=무반응, 2=반응")
text(30, 48, "(discriminant line)")

