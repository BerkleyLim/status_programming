# ��� ����
setwd("C:/Users/limhs/Desktop/R_Report")

# 1�� ����
eBay <- read.csv("eBayAuctions.csv", header=TRUE)

#(������ ��ó��) ������ ���������鿡 ���� ���������� ����ÿ�. 
#���������� ��ǰ����(18�� ����), ��ȭ(USD, GBP, Euro), ��Ÿ�����(������-�Ͽ���) 
#�׸��� ��űⰣ(1, 3, 5, 7 �Ǵ� 10��)�� �����Ͻÿ�. 

# �ŷ� ������ ���� ǥ��
colnames(eBay)



#1) ��ȭ�� ���� levels�� 0 = USD, 1 = GBP, 2 = Euro
currencyD <- factor(eBay$currencyD)
levels(currencyD) # ������� Ȯ��

#������ ��ó�� - ��ȭ��
levels(currencyD) <- c("USD", "GBP", "Euro")
levels(currencyD) # ����� ������� Ȯ��



#2) ��Ÿ����� ���� levels�� 0 = ��, 1 = ȭ, 2 = ��, 3 = ��, 4 = ��, 5 = ��, 6 = ��
endDayD <- factor(eBay$endDayD)
levels(endDayD) # ���� ���� ��� ����

# ������ ��ó�� -����
levels(endDayD) <- c("������", "ȭ����", "������", "�����", "�ݿ���", "�����", "�Ͽ���")
levels(endDayD) # ����� ������� Ȯ��




#3) ��� �Ⱓ levels�� 0 = 1��, 1 = 3��, 2 = 5��, 3 = 7��, 4 = 10��
DurationD <- factor(eBay$DurationD)
levels(DurationD) # ������� Ȯ�� ( ������ )

# ����
levels(DurationD) <- c("1��", "3��", "5��", "7��", "10��")
levels(DurationD) # ������� Ȯ�� ( ���� �� )


#4) ��ǰ ����Ʈ 0~17 ���⼭ �׳� ��ǰ �̸��� ��ǰ1~18 �� �д�.
CategoryD <- factor(eBay$CategoryD)
levels(CategoryD)

levels(CategoryD) <- c("��ǰ0", "��ǰ1", "��ǰ2", "��ǰ3",
                       "��ǰ4", "��ǰ5", "��ǰ6", "��ǰ7",
                       "��ǰ8", "��ǰ9", "��ǰ10", "��ǰ11",
                       "��ǰ12", "��ǰ13", "��ǰ14", "��ǰ15",
                       "��ǰ16", "��ǰ17")


# 1-1 ����, ��� �������� ����Ͽ� �з����� ����,
# ���� ����ġ�� �Ͽ� ����ȭ�� �ǻ���������� �����
library(mlbench) 
library(tree)
library(stats)

#data(eBay), � ���� ������ �ִ����� ���ؼ� ���� �д�.
Competitive <- eBay$Competitive
T1<-tree(Competitive~., eBay)
summary(T1)
 

#pdf("tree.pdf")
plot(T1, type="uniform") 
text(T1) 
mtext("���ݰ� ȭ���� ���� �����", side=3, line=1, col=2, cex=2) 
dev.off()

prune.tree(T1.1)


#1-2 ����, �н� ������ ��Ʈ 60%, �׽�Ʈ ������ ��Ʈ 40% ����
#���⼭ ������.
partition <- sample(length(eBay[,1]), size=(length(eBay[,1])*0.6) )
train.eBay<-eBay[partition,] # �н� ������ ��Ʈ
test.eBay<-eBay[-partition,] # �׽�Ʈ ������ ��Ʈ

# �н� �����͸� TReBay�� ����(������)
TReBay.tr=tree(train.eBay$Competitive~., train.eBay) 
summary(TReBay.tr) 
TReBay.tr

# �н��� ������ ���
plot(TReBay.tr) 
text(TReBay.tr, all=T)

# �Ʒ� �������� ���� �׽�Ʈ ������ �����ϱ�!!
pred<-predict(TReBay.tr, test.eBay) 
table(round(pred,0)==test.eBay[,8])



# 2�� ����
Flight <- read.csv("FlightDelays.csv")
colnames(Flight)


#���� �� ��ó�� �� ��!!
#1) ����
DAY_WEEK <- factor(Flight$DAY_WEEK)
levels(DAY_WEEK) <- c("mon","tue","wed","thus","fry","sat","sun")
levels(DAY_WEEK)

#2) �װ���
TAIL_NUM <- factor(Flight$TAIL_NUM)
levels(TAIL_NUM)

#3) ��߰���
CARRIER <- factor(Flight$CARRIER)
levels(CARRIER)

#4) ��������
DEST <- factor(Flight$DEST)
levels(DEST)

#5) ���� ��� �ð� �� �ð� ���� ����
#���⼭ �ð��� ǥ���Ѵ�,(�ð��� ���� �ֱ� ������ �ð����� ����)
CRS_DEP_TIME <- round(Flight$CRS_DEP_TIME / 100, 0)
CRS_DEP_TIME <- factor(CRS_DEP_TIME)
levels(CRS_DEP_TIME) # 6 ~ 21�ñ��� ǥ��


# �ӽ� ������,
# �׷�ȭ �ϱ�!! ����ȭ �ϱ� 6~22�ñ��� 2�ð� ������
#levels(Flight$CRS_DEP_TIME) <- c("6~8��","6~8��","8~10��","8~10��",
 #                                "10~12��","10~12��","12~14��","12~14��",
  #                               "14~16��","14~16��","16~18��","16~18��",
   #                              "18~20��","18~20��","20~22��","20~22��")

#levels(Flight$CRS_DEP_TIME) # ����ȭ ��

library(mlbench) 
library(tree)
library(stats)

# 2.1 ����
# ���� �������� �����

Flight.Status <- Flight$Flight.Status
library(rpart)
library(rpart.plot)
ukm.tr.cart <- rpart(Flight.Status ~DAY_WEEK+DEST+CARRIER, Flight) 
ukm.tr.cart

prp(ukm.tr.cart, type=4, extra=1, digits=3, main="�װ��� ���� ����")
table(ukm.tr.cart)

#���Ǿ��� ���Ӻ�����!!
ukm.tr.cart <- rpart(Flight.Status ~., Flight)
prp(ukm.tr.cart, type=4, extra=1, digits=3, main="�װ��� ���� ����")

yhat <- predict(ukm.tr.cart, type="class")
table(yhat)

# 2.2 ���� 
# ���� ������ ���� 7�� ��Ÿ����
test <- Flight[which(CRS_DEP_TIME == "7"),] # ���� 7��
test$CRS_DEP_TIME <- round(test$CRS_DEP_TIME / 100 , 0)
test <- test[which(test$DAY_WEEK == 1),] # ���⼭ �����ϸ� (�ʱ� �� 1)



# �۾� ����
f <- test$Flight.Status # �ٽ� ��ó�� (��������)


ukm.tr <- rpart(f~., test)
yhat <- predict(ukm.tr, type="class")
table(yhat)

prp(ukm.tr, type=4, extra=1, digits=3, main="�װ��� ���� ����")
