library(readr)
a<- read_csv("C:/Users/yonur/Desktop/data_9 (1).csv")
summary(a)
names(a)
head(a,3)

#hedef degiskenimiz Y.

table(a$Y)

a 

#EDA
names(a)[names(a)=="X1"]="AGC" #Amount of Given Credit#

names(a)[names(a)=="X2"]="Gender" #Gender#
a$Male <- as.numeric(a$Gender=="1")
a$Female <- as.numeric(a$Gender=="2")

names(a)[names(a)=="X3"]="Education" #Education#

a$Education[a$Education =="0"]<- 0
a$Education[a$Education =="4"]<- 0
a$Education[a$Education =="5"]<- 0
a$Education[a$Education =="6"]<- 0

a$Gra_Sch <- as.numeric(a$Education=="1")
a$Univer. <- as.numeric(a$Education=="2")
a$High_Sch <- as.numeric(a$Education=="3")
a$Edu_Other<- as.numeric(a$Education=="0")


names(a)[names(a)=="X4"]="MarialS" #MarialS#
#X4: Marital status (1 = married; 2 = single; 3 = divorce; 0=others)#
a$MarialS[a$MarialS =="3"]<- 2
a$MarialS[a$MarialS =="0"]<- 2
a$Married <- as.numeric(a$MarialS=="1")
a$Single <- as.numeric(a$MarialS=="2")


names(a)[names(a)=="X5"]="Age" #Age#

names(a)[names(a)=="X6"]="Pay_0905" #History of past payment. We tracked the past monthly payment records#
names(a)[names(a)=="X7"]="Pay_0805"
names(a)[names(a)=="X8"]="Pay_0705"
names(a)[names(a)=="X9"]="Pay_0605"
names(a)[names(a)=="X10"]="Pay_0505"
names(a)[names(a)=="X11"]="Pay_0405"

a$Septpp1 <- as.numeric(a$Pay_0905=="-2"|a$Pay_0905=="-1"|a$Pay_0905=="0")
a$Septpp2 <- as.numeric(a$Pay_0905=="1"|a$Pay_0905=="2"|a$Pay_0905=="3")
a$Septpp3 <- as.numeric(a$Pay_0905=="4"|a$Pay_0905=="5"|a$Pay_0905=="6")
a$Septpp4 <- as.numeric(a$Pay_0905=="7"|a$Pay_0905=="8"|a$Pay_0905=="9")

a$Augupp1 <- as.numeric(a$Pay_0805=="-2"|a$Pay_0805=="-1"|a$Pay_0805=="0")
a$Augupp2 <- as.numeric(a$Pay_0805=="1"|a$Pay_0805=="2"|a$Pay_0805=="3")
a$Augupp3 <- as.numeric(a$Pay_0805=="4"|a$Pay_0805=="5"|a$Pay_0805=="6")
a$Augupp4 <- as.numeric(a$Pay_0805=="7"|a$Pay_0805=="8"|a$Pay_0805=="9")


a$Julypp1 <- as.numeric(a$Pay_0705=="-2"|a$Pay_0705=="-1"|a$Pay_0705=="0")
a$Julypp2 <- as.numeric(a$Pay_0705=="1"|a$Pay_0705=="2"|a$Pay_0705=="3")
a$Julypp3 <- as.numeric(a$Pay_0705=="4"|a$Pay_0705=="5"|a$Pay_0705=="6")
a$Julypp4 <- as.numeric(a$Pay_0705=="7"|a$Pay_0705=="8"|a$Pay_0705=="9")

a$Junepp1 <- as.numeric(a$Pay_0605=="-2"|a$Pay_0605=="-1"|a$Pay_0605=="0")
a$Junepp2 <- as.numeric(a$Pay_0605=="1"|a$Pay_0605=="2"|a$Pay_0605=="3")
a$Junepp3 <- as.numeric(a$Pay_0605=="4"|a$Pay_0605=="5"|a$Pay_0605=="6")
a$Junepp4 <- as.numeric(a$Pay_0605=="7"|a$Pay_0605=="8"|a$Pay_0605=="9")

a$Maypp1 <- as.numeric(a$Pay_0505=="-2"|a$Pay_0505=="-1"|a$Pay_0505=="0")
a$Maypp2 <- as.numeric(a$Pay_0505=="1"|a$Pay_0505=="2"|a$Pay_0505=="3")
a$Maypp3 <- as.numeric(a$Pay_0505=="4"|a$Pay_0505=="5"|a$Pay_0505=="6")
a$Maypp4 <- as.numeric(a$Pay_0505=="7"|a$Pay_0505=="8"|a$Pay_0505=="9")

a$Aprilpp1 <- as.numeric(a$Pay_0405=="-2"|a$Pay_0405=="-1"|a$Pay_0405=="0")
a$Aprilpp2 <- as.numeric(a$Pay_0405=="1"|a$Pay_0405=="2"|a$Pay_0405=="3")
a$Aprilpp3 <- as.numeric(a$Pay_0405=="4"|a$Pay_0405=="5"|a$Pay_0405=="6")
a$Aprilpp4 <- as.numeric(a$Pay_0405=="7"|a$Pay_0405=="8"|a$Pay_0405=="9")


names(a)[names(a)=="X12"]="Bill_0905"#Amount of bill statement
names(a)[names(a)=="X13"]="Bill_0805"#amount of bill statement
names(a)[names(a)=="X14"]="Bill_0705"
names(a)[names(a)=="X15"]="Bill_0605"
names(a)[names(a)=="X16"]="Bill_0505"
names(a)[names(a)=="X17"]="Bill_0405"

names(a)[names(a)=="X18"]="Paid_0905"#Amount of previous payment 
names(a)[names(a)=="X19"]="Paid_0805"
names(a)[names(a)=="X20"]="Paid_0705"
names(a)[names(a)=="X21"]="Paid_0605"
names(a)[names(a)=="X22"]="Paid_0505"
names(a)[names(a)=="X23"]="Paid_0405"

names(a)[names(a)=="Y"]="Target"

a1 <- a[c('AGC','Male','Age')]
a2 <- a[29:58]
a3 <- a[14:25]
a4 <- a[c('Target')]

a5 <- data.frame(a1,a2,a3,a4)
summary(a5)

#Decision Tree

install.packages("rpart")
install.packages("rpart.plot")
install.packages("caret")
library(rpart)
library(rpart.plot)
library(caret)

set.seed(2167002)

trainind<- createDataPartition(a5$Target, p=0.75, list=F, times=1)
traindata<-a5[trainind,]
testdata<-a5[-trainind,]
summary(traindata);summary(testdata)

tree1<-rpart(Target~., method="class", data=traindata,cp=0.01) 
rpart.plot(tree1, type=4)
rpart.rules(tree1)

node1 <- subset(traindata, Septpp1 == 1 )
prop.table(table(node1$Target))
node1$pred=0 

node2 <- subset(traindata, Septpp1 == 0 & Bill_0905 < 568)
prop.table(table(node2$Target))
node2$pred=0 

node3 <- subset(traindata, Septpp1 == 0 & Bill_0905 >= 568 & Aprilpp1 == 1 & Paid_0905 <  995)
prop.table(table(node3$Target))
node3$pred=0 

node4 <- subset(traindata, Septpp1 == 0 & Bill_0905 >= 568 & Aprilpp1 == 1 & Paid_0905 >= 995)
prop.table(table(node4$Target))
node4$pred=1 

node5 <- subset(traindata,Septpp1 == 0 & Bill_0905 >= 568 & Aprilpp1 == 0 )
prop.table(table(node5$Target))
node5$pred=1 

ntraindata<- rbind(node1, node2, node3, node4,node5)

table_mat1<- table(ntraindata$Target,ntraindata$pred)
rownames(table_mat1)<-paste("Actual", rownames(table_mat1),sep=":")
colnames(table_mat1)<-paste("Predicted",colnames(table_mat1),sep = ":")
print(table_mat1)

DT_acc_test <- sum(diag(table_mat1)/sum(table_mat1))

DT_PC <- paste(sep="","Percent Captured: ",((1408)/(1408+2858)))

DT_RR <- paste(sep="","Response Rate: ",((1408)/(1408+831)))

##REGRESYON

# Linear probability model
set.seed(2167002)

trainind<- createDataPartition(a5$Target, p=0.75, list=F, times=1)
traindata<-a5[trainind,]
testdata<-a5[-trainind,]
summary(traindata);summary(testdata)


lpm <- lm(Target ~ ., data = traindata)
summary(lpm),View(lpm)

f_lpm <- fitted(lpm)

summary(f_lpm)
class <- as.numeric(f_lpm > mean(f_lpm))
TT <- traindata$Target

table_mat <- table(traindata$Target, class)
rownames(table_mat) <-paste("Actual", rownames(table_mat), sep = ":")
colnames(table_mat) <-paste("Predicted", colnames(table_mat), sep = ":")
print(table_mat)

LPM_acc_test <- sum(diag(table_mat / sum(table_mat)))

LPM_PC <- paste(sep="","Percent Captured: ",((2555)/(2555+1711)))

LPM_RR <- paste(sep="","Response Rate: ",((2555)/(2555+ 3002)))

lpm <- lm(Target ~ Septpp1 + Paid_0905, data = traindata)
summary(lpm)

p_lpm <- fitted(lpm)
lpm_class <- as.numeric(p_lpm > mean(p_lpm))
table_mat2 <- table(traindata$Target, lpm_class)

rownames(table_mat2) <-
  paste("Actual", rownames(table_mat2), sep = ":")
colnames(table_mat2) <-
  paste("Predicted", colnames(table_mat2), sep = ":")
print(table_mat2)

LPM1_PC <- paste(sep="","Percent Captured: ",((2183)/(2183+2083)))

LPM1_RR <- paste(sep="","Response Rate: ",((2183)/(2183+ 2230)))

LPM1_acc_test <- sum(diag(table_mat2 / sum(table_mat2)))


##Logit Modeli(logistic regresyon)

logit <- glm(Target ~ ., data = traindata, family = binomial)
bw <- step(logit, trace = 0)
summary(bw)


nomodel <- glm(Target ~ 1, data = traindata, family = binomial)
forward = step(
  nomodel,
  scope = list(lower = formula(nomodel), upper = formula(logit)),
  direction = "forward",
  trace = 0
)
summary(forward)

logit <- glm(
  Target ~ Septpp1 + Julypp1 + Maypp1 + AGC + Paid_0905 +Edu_Other + Married  +Aprilpp1+Augupp2+Paid_0805+Bill_0805    
  + Junepp1 + Male +Paid_0605 + Augupp1  + Age,data = traindata,family = binomial)

traindata$pred <- predict(logit, newdata = traindata, type = "response")
testdata$pred <- predict(logit, newdata = testdata, type = "response")

summary(testdata$pred)

lpm_class <- as.numeric(testdata$pred > mean(traindata$pred))

table_mat3 <- table(testdata$Target, lpm_class)
rownames(table_mat3) <-
  paste("Actual", rownames(table_mat3), sep = ":")
colnames(table_mat3) <-
  paste("Predicted", colnames(table_mat3), sep = ":")
print(table_mat3)
LM_acc_test <- sum(diag(table_mat3 / sum(table_mat3)))
LM_PC <- paste(sep="","Percent Captured: ",((838)/(838+614)))

LM_RR <- paste(sep="","Response Rate: ",((838)/(838+  873)))

mn <- 0
for (i in 1:9) {
  q = i / 10
  quant <- quantile(testdata$pred, probs = c(q))
  seq <-
    subset(testdata, testdata$pred >= quant, select = c(Target, pred))
  percent.captured <- sum(seq$Target) / sum(testdata$Target)
  mn[10 - i] = percent.captured
}

print(mn)
#[1] 0.2863763 0.4819277 0.6144578 0.6904541 0.7599629 0.8266914 0.8730306 0.9230769 0.9675626

plot(mn, type = "l")

install.packages("WVPlots")
library(WVPlots)
GainCurvePlot(testdata, "pred", "Target", title = "pred perf")


##KNN

normal <- function(x) {
  return((x-min(x))/(max(x)-min(x)))
}

a5.altk <- as.data.frame(lapply(a5[,34:45], normal))
k <- a5[2:33]
l <- a5[46]
a6 <- data.frame(k,l)
a5.altk <- data.frame(a5.altk, a6[,1:33])

set.seed(202167002)
#Benzerlik Araniyor
index <- sample(1:nrow(a5.altk ), size = nrow(a5.altk )*0.75,
                 replace=F)
train <- a5.altk[index,]
test <- a5.altk[-index,]


train_l <- a5[index,]
train_l <- train_l[46]
test_l <- a5[-index,]
test_l <- test_l[46]


train <- train[c('Bill_0905' , 'Bill_0805' , 'Bill_0605' , 'Paid_0905' , 'Paid_0805', 'Paid_0605', 
                 'Male' , 'Gra_Sch' ,'Univer.' ,'High_Sch','Married',
                 'Septpp1' , 'Augupp1', 'Augupp2', 'Julypp1' , 'Junepp1' , 'Maypp1' , 'Maypp2' , 'Maypp3' , 'Aprilpp2')]

test <- test[c('Bill_0905' , 'Bill_0805' , 'Bill_0605' , 'Paid_0905' , 'Paid_0805', 'Paid_0605', 
               'Male' , 'Gra_Sch' ,'Univer.' ,'High_Sch','Married',
               'Septpp1' , 'Augupp1', 'Augupp2', 'Julypp1' , 'Junepp1' , 'Maypp1' , 'Maypp2' , 'Maypp3' , 'Aprilpp2')]


install.packages("class")
library(class)

nkk<- sqrt(nrow(train))
knn <- knn(train=train, test=test, cl=train_l$Target, k=round(nkk))
tb <- table(test_l$Target, knn)
view(tb)

accuracy <- sum(diag(tb))/sum(rowSums(tb))*100 #79.67
p_captured <- 100*sum(knn==1 & test_l$Target==1)/sum(test_l$Target==1) #23.26
p_true <- 100*sum(knn==1 & test_l$Target==1)/sum(knn==1) #60.62
p_default <- sum(knn==0 & test_l$Target==1)/sum(knn==0) #0.18
prop.table(table(test_l$Target))
#######################
#       0         1  #
#0.7920307 0.2079693  #
#######################

accuracy;p_captured;p_true;p_default


i=1
k.optm=1
capt=1
true=1
default=1

for(i in 1:120) {
  knn <- knn(train = train, test=test, cl=train_l$Target, k=i)
  tb <- table(test_l$Target, knn)
  k.optm[i] <- sum(diag(tb))/sum(rowSums(tb))*100
  capt[i] <- 100*sum(knn==1 & test_l$Target==1)/sum(test_l$Target==1)
  true[i] <- 100*sum(knn==1 & test_l$Target==1)/sum(knn==1)
  default[i] <- sum(knn==0 & test_l$Target==1)/sum(knn==0)
}

par(mfrow=c(2,2))
plot(k.optm, pch=16, type="l", main="accuracy")
plot(capt, pch=16, type="l", main="% captured")
plot(true, pch=16, type="l", main="% true")
plot(default, pch=16, type="l", main="% default")

knn <- knn(train = train, test=test, cl=train_l$Target, k=20)
table_mat4 <- table(test_l$Target, knn)
KNN_acc_test <- sum(diag(table_mat4))/sum(rowSums(table_mat4))
KNN_PC <- paste(sep="","Percent Captured: ",((487)/(487+870)))
KNN_RR <- paste(sep="","Response Rate: ",((487)/(487+320)))

p_true <- 100*sum(knn==1 & test_l$Target==1)/sum(knn==1)
p_default <- sum(knn==0 & test_l$Target==1)/sum(knn==0)

p_true

p_default

table_mat4



