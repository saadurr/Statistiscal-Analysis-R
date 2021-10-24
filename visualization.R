library(readxl)
library(tidyverse)
install.packages("tidyverse")

pdf("visualization.pdf")

df<- read.csv("research_data.csv")

aggregate(df$X0.3.hrs...Night. ~ df$YEAR,FUN = sum)
df1<-aggregate(df$X0.3.hrs...Night. ~ df$YEAR,FUN = sum)
first3hours<-aggregate(df$X0.3.hrs...Night. ~ df$YEAR,FUN =sum)


aggregate(df$X3.6.hrs...Night. ~ df$YEAR,FUN = sum)
df2<-aggregate(df$X3.6.hrs...Night. ~ df$YEAR,FUN = sum)
second3hours<-aggregate(df$X3.6.hrs...Night. ~ df$YEAR,FUN =sum)


aggregate(df$X6.9.hrs..Day. ~ df$YEAR,FUN = sum)
df3<-aggregate(df$X6.9.hrs..Day. ~ df$YEAR,FUN = sum)
third3hours<-aggregate(df$X6.9.hrs..Day. ~ df$YEAR,FUN =sum)

aggregate(df$X9.12.hrs..Day. ~ df$YEAR,FUN = sum)
df4<-aggregate(df$X9.12.hrs..Day. ~ df$YEAR,FUN = sum)
fourth3hours<-aggregate(df$X9.12.hrs..Day. ~ df$YEAR,FUN =sum)


aggregate(df$X12.15.hrs..Day. ~ df$YEAR,FUN = sum)
df5<-aggregate(df$X12.15.hrs..Day. ~ df$YEAR,FUN = sum)
fifth3hours<-aggregate(df$X12.15.hrs..Day. ~ df$YEAR,FUN =sum)


aggregate(df$X15.18.hrs..Day. ~ df$YEAR,FUN = sum)
df6<-aggregate(df$X15.18.hrs..Day. ~ df$YEAR,FUN = sum)
sixth3hours<-aggregate(df$X15.18.hrs..Day. ~ df$YEAR,FUN =sum)


aggregate(df$X18.21.hrs..Night. ~ df$YEAR,FUN = sum)
df7<-aggregate(df$X18.21.hrs..Night. ~ df$YEAR,FUN = sum)
seventh3hours<-aggregate(df$X18.21.hrs..Night. ~ df$YEAR,FUN =sum)


aggregate(df$X21.24.hrs..Night. ~ df$YEAR,FUN = sum)
df8<-aggregate(df$X21.24.hrs..Night. ~ df$YEAR,FUN = sum)
eight3hours<-aggregate(df$X21.24.hrs..Night. ~ df$YEAR,FUN =sum)


total1<-sum(first3hours$`df$X0.3.hrs...Night.`)
total2<-sum(second3hours$`df$X3.6.hrs...Night.`)
total3<-sum(third3hours$`df$X6.9.hrs..Day.`)
total4<-sum(fourth3hours$`df$X9.12.hrs..Day.`)
total5<-sum(fifth3hours$`df$X12.15.hrs..Day.`)
total6<-sum(sixth3hours$`df$X15.18.hrs..Day.`)
total7<-sum(seventh3hours$`df$X18.21.hrs..Night.`)
total8<-sum(eight3hours$`df$X21.24.hrs..Night.`)


total<-data.frame(total1,total2,total3,total4,total5,total6,total7,total8)

colnames(total)<-c("0-3hours","3-6hours","6-9hours","9-12hours","12-15hours","15-18hours","18-21hours","21-24hours")

colnames(first3hours)<-c("Year","0-3H")
colnames(second3hours)<-c("Year","3-6H")
colnames(third3hours)<-c("Year","6-9H")
colnames(fourth3hours)<-c("Year","9-12H")
colnames(fifth3hours)<-c("Year","12-15H")
colnames(sixth3hours)<-c("Year","15-18H")
colnames(seventh3hours)<-c("Year","18-21H")
colnames(eight3hours)<-c("Year","21-24H")


fd1<-merge.data.frame(first3hours,second3hours,by="Year")
fd2<-merge.data.frame(fd1,third3hours,by="Year")
fd3<-merge.data.frame(fd2,fourth3hours,by="Year")
fd4<-merge.data.frame(fd3,fifth3hours,by="Year")
fd5<-merge.data.frame(fd4,sixth3hours,by="Year")
fd6<-merge.data.frame(fd5,seventh3hours,by="Year")
maindata<-merge.data.frame(fd6,eight3hours,by="Year")


finaltotal<-data.frame(t(total))
names(finaltotal)[1]<-"total accidents"

mdt<-maindata[-c(1)]
boxplot(mdt,main = "Number of accidents per given hour", 
        xlab = "24 Hours divided into 8 parts",ylab = "No of accidents", cex.axis=0.75)

scatter.smooth(finaltotal,main = "Number of accidents per given hour", 
                     xlab = "24 Hours divided into 8 parts",ylab = "No of accidents")



dev.off()

