# ��� ����
setwd("C:/Users/limhs/Desktop/R_Report")

# ������ �ҷ�����, colleges.XL.zip.xls ������ clooeges.XL.csv�� ��ó
colleges <- read.csv("colleges.XL.csv")

# ������ ���� �ľ�
summary(colleges) # �̰����� NA �մ� ���� Ȯ���Ѵ�.
head(colleges)

# ��ü ��(column)�̸� ����
colnames(colleges) # �̰����� ���� �ǹ̰� �������� ����.
names(colleges)

# ���̺� colleges ����
attach(colleges)

# 1-1�� - ������ ������ ���� �ϱ� , ���� ���� (attach�� ���� colleges�� �� �̸��� ������ ��� ����)
# ���� ���� -which(is.na(colleges$���̺���)) �Ҷ�����, summary(college$���̺���)���� �Ͽ� 
# Na's 0�� �� ���̺� NA ���� ���̺��� ������� �� ��, ���� ������ ��� �������Ƿ� �׻� summary�� NA�� Ȯ��!!
colleges <- colleges[-which(is.na(colleges$Math.SAT) ),] # colleges <- colleges[-which(is.na(Verbal.SAT) ),]�� Na's 0
colleges <- colleges[-which(is.na(colleges$ACT)) , ]
colleges <- colleges[-which(is.na(colleges$X..appli..rec.d)), ] 
                  # colleges <- colleges[-which(is.na(colleges$X..appl..accepted)), ] ���ݱ��� �۾��� Na's 0
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

# �߰��߰��� ����ġ�� Ȯ���ϸ鼭 ������ ���Ÿ� �Ͽ���.
summary(colleges$Graduation.rate) # �̰����� NA �մ� ���� Ȯ���Ѵ�. <�߰��߰� �˻��ϴٰ� �ᱹ �ر��� ������>

# ���������� ����ġ ���� Ȯ��
which(is.na(colleges))

# 1-2��
# ������ �ҷ�����
x <- colleges[,18] # ������
y <- colleges[,22] # ������

dx <- round(dist(x), 2) # �Ҽ��� 2�ڸ�����
dy <- round(dist(y), 2)

# ����α׷� - �����ϰ� ���´�. (�� 206��)
hc1 <- hclust(dist(x)^2, method="complete") #���� ����� - ������
hc2 <- hclust(dist(y)^2, method="complete") #���� ����� - ������
plot(hc1, labels=names(x) ,hang=1, main="dendrogram : complete - result_stu_fee")
plot(hc2, labels=names(y) ,hang=1 ,main="dendrogram : complete - result_greduation")

# 1-3�� ������ ǥ���� �� ��� �м�
#������ - 6�� ������ ����
x <- colleges[,18] # ������
hc1 <- hclust(dist(x)^2, method="complete") #���� ����� - ������
c1.num <- 6  # setting number of clusters (���� ���� ����)
hc1.result <- cutree(hc1,k=c1.num)
plot(x, pch=hc1.result, col=hc1.result, xlab = "��ü �б� ��", ylab = "������", main="������") 

#������ - 5�� ������ ����
y <- colleges[,22] # ������
hc2 <- hclust(dist(y)^2, method="complete") #���� ����� - ������
c2.num <- 5
hc2.result <- cutree(hc2,k=c2.num)
plot(y, pch=hc2.result, col=hc2.result, xlab = "��ü �б� ��", ylab = "������", main="������")  



# 1-4 ������ �ڷḦ Ȱ�� <1-3 ���� �Ϻο� �Ϻθ� �̾> - ������� state ����
yy <- table(colleges[,3])  # ��/�縳 ���� ����

plot(x,pch=hc2.result, col=hc2.result,  xlab="��ü�б� ��", ylab="������", main="������")
text(x,labels=State ,adj=-0.1, cex=0.8, main="complete")

detach(colleges)

# ---------------------------------------------------- 2��

# ������ �ҷ�����
Air <- read.csv("EastWestAirlinesCluster.csv")

# ������ ���� ǥ��
colnames(Air)

library(mlbench)
library(stats)

# ������ ������ ����
attach(Air)

# ������ 2-1) -> Balance�� ������ ���ϸ��� ���赵�� ��Ÿ����.

# ���� ǥ��ȭ �۾����� �����Ѵ�.
# Balance ǥ��ȭ
Bal <- (Balance - mean(Balance) ) / sqrt(var(Balance))
Bal <- round(Bal, 2)
# Qual_mile ǥ��ȭ
Q_m <- (Qual_miles - mean(Qual_miles) ) / sqrt(var(Qual_miles))
Q_m <- round(Q_m, 2)
#cc1_miles, cc2_miles, cc3_miles �� summary�� ���� ���� �����ص� ũ�� ������ ��ġ�� ����
summary(cc1_miles); summary(cc2_miles); summary(cc3_miles) # �� Balance���� ������ ����.
#Bonus_miles ǥ��ȭ
B_m <- (Bonus_miles - mean(Bonus_miles) ) / sqrt(var(Bonus_miles))
B_m <- round(B_m, 2)



# ������ ǥ��ȭ ���� ���赵�� �׷�����.
#plot Balance, Qual_miles ���赵
x <- hclust( dist(Bal)^2 , method="single")
y <- hclust( dist(Q_m)^2, method = "single")
hcx.result <- cutree(x, k=1)
hcy.result <- cutree(y, k=10)

plot(Bal, Q_m, xlab="Balance", ylab="Qual_mails" ,type="p", main ="plot Balance, Qual_miles ���赵"
     , pch=hcy.result, col=hcy.result)
text(hcx.result, hcy.result, labels = as.character(ID.), cex=0.8, pos=2, col="red")

#plot Balance, Bonus_miles ���赵
z <- hclust(dist(B_m), method = "complete")
hcz.result <- cutree(z, k=2)

plot(Bal, B_m, xlab="Balance", ylab="Bonus_miles", type="p", main = "plot Balance, Bonus_miles ���赵"
     , pch=hcz.result, col=hcz.result)
text(Balance, Bonus_miles, labels = as.character(ID.), cex=0.8, pos=2, col="red")


#2-2 ����
plot(Balance, Qual_miles, xlab="Balance", ylab="Qual_mails" ,type="p", main ="plot Balance, Qual_miles ���赵"
     , pch=hcy.result, col=hcy.result)
text(hcx.result, hcy.result, labels = as.character(ID.), cex=0.8, pos=2, col="red")

# 2-3 ����
#K-means
#3�� ����
# 1) Balance, Qual_miles
Q <- kmeans(Q_m,centers=5)

attributes(Q)
plot(Bal, Q_m, xlab="Balance", ylab="Bonus_miles", type="p", main = "plot Balance, Bonus_miles ���赵"
     , pch=Q$cluster, col=Q$cluster)
text(Bal, Q_m,labels=as.character(ID.),adj=-0.1, cex=0.8)

# 2) Balance, Bonus_miles
B <- kmeans(B_m,centers=3)

attributes(B)
plot(Bal, B_m, xlab="Balance", ylab="Bonus_miles", type="p", main = "plot Balance, Bonus_miles ���赵"
     , pch=B$cluster, col=B$cluster)
text(Bal, B_m,labels=as.character(ID.),adj=-0.1, cex=0.8)


# 2-4 ����
# ���� �����ϰ�, 95%�� �����͸� ����ϱ�
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

# (2-1�� ������ �м���)
plot(bals, quir, xlab="Balance", ylab="Qual_mails" ,type="p", main ="plot Balance, Qual_miles ���赵"
     , pch=hcy.result, col=hcy.result)
plot(bals, bonus, xlab="Balance", ylab="Bonus_miles", type="p", main = "plot Balance, Bonus_miles ���赵"
     , pch=hcz.result, col=hcz.result)


# 2-5 ���� - k - ���

# 1) Balance, Qual_miles
Q1 <- kmeans(quir,centers=5)

plot(bals, quir, xlab="Balance", ylab="Qual_miles", type="p", main = "plot Balance, Qual_miles ���赵"
     , pch=Q1$cluster, col=Q1$cluster)
text(bals, quir,labels=as.character(ID.),adj=-0.1, cex=0.8)

# 2) Balance, Bonus_miles
B1 <- kmeans(bonus,centers=3)

plot(bals, bonus, xlab="Balance", ylab="Bonus_miles", type="p", main = "plot Balance, Bonus_miles ���赵"
     , pch=B1$cluster, col=B1$cluster)
text(bals, bonus,labels=as.character(ID.),adj=-0.1, cex=0.8)



# ������ ������ ���� ����
detach(Air)