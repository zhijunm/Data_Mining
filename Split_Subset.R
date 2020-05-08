## 2
data3<- c(2.529,5.417,1081.666,997.805,514.2,491.9,376.1,24.752,21.229,111.463,164,327.389,283.631,268.55,2.136,8.074,421.806,428.26,392.19)
median(data3)
data5<-c(83.727,235.692,1154.597,1281.084,1283.719,2214.2,1487.1,43.906,138.719,2431.992,3708,4412,318.313,1553.831,240.931,6.125,5.742,15.313,488.422,612.87)
median(data5)

## 3.a split data into 10 equal part
ex.df <- read.csv("C:/Users/mazhi/Documents/R/560_data_mining/dataforExamquestion3.csv")
parti<-split(ex.df, sample(rep(1:10, 63),replace=FALSE))
View(parti)
## Another way to split
library(caret)
parti <- createDataPartition(ex.df$HHI, times = 10, p=0.1)
## 3.a # of record with bk=0 in each partition
nrow(subset(parti$'1',bktype==0))
nrow(subset(parti$'2',bktype==0))
nrow(subset(parti$'3',bktype==0))
nrow(subset(parti$'4',bktype==0))
nrow(subset(parti$'5',bktype==0))
nrow(subset(parti$'6',bktype==0))
nrow(subset(parti$'7',bktype==0))
nrow(subset(parti$'8',bktype==0))
nrow(subset(parti$'9',bktype==0))
nrow(subset(parti$'10',bktype==0))

## 3.b subset of bk=0
ex.sub<-subset(ex.df, bktype==0)
nrow(ex.sub)
## 3.b equal number of bk=1
ex.sub1 <- subset(ex.df, bktype==1)
View(ex.sub1)
sub1.rows <- sample(row.names(ex.sub1),79)
ex.sub1<-ex.sub1[sub1.rows,]
View(ex.sub1)
## mean for data 1 for bk=0 and 1
mean(ex.sub$DATA1)
mean(ex.sub1$DATA1)
## t-test for different in the means fo the 2 groups in subset
t.test(ex.sub$DATA1,ex.sub1$DATA1, alt="two.sided", conf=0.95, paired=F)

## confustion matrix for cutoff of 0.6 and 0.8
library(caret)
con.df <- data.frame("Actual Y"=c(0,1,0,1,1,0,1),"Predicted Y"=c(0.5,0.9,0.7,0.7,0.3,0.4,0.5))
con.df
confusionMatrix(as.factor(ifelse(con.df$Predicted.Y>0.6, 1, 0)), 
                as.factor(con.df$Actual.Y))
confusionMatrix(as.factor(ifelse(con.df$Predicted.Y>0.8, 1, 0)), 
                as.factor(con.df$Actual.Y))
2*0.5*0.667/(0.5+0.667)
2*0.25*1/(0.25+1)

## Build a dataframe for question 6
q6.train <- data.frame("Record_ID"=c(1,2,3,4,5,6,7,8,9,10),
             "Age"=c("Young","Young","Young","Young","Young","Young","Young","Young","Pre-presbyopic","Pre-presbyopic"),
             "Spectacle_prescription"=c("Myope","Myope","Myope","Myope","Hypermetrope","Hypermetrope","Hypermetrope","Hypermetrope","Myope","Myope"),
             "Astigmatic"=c("No","No","Yes","Yes","No","No","Yes","Yes","No","No"),
             "Tear_production_rate"=c("Reduced","Normal","Reduced","Normal","Reduced","Normal","Reduced","Normal","Reduced","Normal"),
             "Class_label_lenses"=c("Noncontact","Soft contact","Noncontact","Hard contact","Noncontact","Soft contact","Noncontact","Hard contact","Noncontact","Soft contact"))
q6.train$Age<-ifelse(q6.train$Age=="Young",0,1)
q6.train$Spectacle_prescription<-ifelse(q6.train$Spectacle_prescription=="Myope",0,1)
q6.train$Astigmatic<-ifelse(q6.train$Astigmatic=="No",0,1)
q6.train$Tear_production_rate<-ifelse(q6.train$Tear_production_rate=="Normal",0,1)
q6.train

q6.vali<-data.frame("Record_ID"=c(1,2,3,4,5,6,7,8,9,10),
                    "Age"=c("Pre-presbyopic","Pre-presbyopic","Pre-presbyopic","Pre-presbyopic","Pre-presbyopic","Pre-presbyopic","Presbyopic","Presbyopic","Presbyopic","Presbyopic"),
                    "Spectacle_prescription"=c("Myope","Myope","Hypermetrope","Hypermetrope","Hypermetrope","Hypermetrope","Myope","Myope","Myope","Myope"),
                    "Astigmatic"=c("Yes","Yes","No","No","Yes","Yes","No","No","Yes","Yes"),
                    "Tear_production_rate"=c("Reduced","Normal","Reduced","Normal","Reduced","Normal","Reduced","Normal","Reduced","Normal"),
                    "Class_label_lenses"=c("Noncontact","Hard contact","Noncontact","Soft contact","Noncontact","Noncontact","Noncontact","Noncontact","Noncontact","Hard contact"))
q6.vali$Age<-ifelse(q6.vali$Age=="Pre-presbyopic",1,2)
q6.vali$Spectacle_prescription<-ifelse(q6.vali$Spectacle_prescription=="Myope",0,1)
q6.vali$Astigmatic<-ifelse(q6.vali$Astigmatic=="No",0,1)
q6.vali$Tear_production_rate<-ifelse(q6.vali$Tear_production_rate=="Normal",0,1)
q6.vali

## determine the k from possible values of 3,4,5
library(FNN)
knn.3 <- knn(train = q6.train[,2:5], test = q6.vali[,2:5], cl = q6.train[, 6], k = 3)
confusionMatrix(knn.3, q6.vali[, 6])
