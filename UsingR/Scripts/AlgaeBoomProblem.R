setwd('D:\\Learning\\R\\WorkingDirectory')
algae <- read.table('Analysis.txt',header=F,dec='.',col.name = c('season','size','speed','mxPH','mn02','C1','N03','NH4','oP04','P04','Chla','a1','a2','a3','a4','a5','a6','a7'),na.strings=c('XXXXXXX'))
summary(algae)

#Data Visualization and summerisation
install.packages('Hmisc')
library(Hmisc)
describe(algae)
hist(algae$mxPH,prob = T)

install.packages('car')
library(car)
par(mfrow=c(1,2))
hist(algae$mxPH,prob=T,xlab='',main='Histogram of maximum pH value',ylim=0:1)
lines(density(algae$mxPH,na.rm=T))
rug(jitter(algae$mxPH))
qq.plot(algae$mxPH,main='Normal qq plot of maximum pH')
par(mfrow=c(1,1))

boxplot(algae$oP04,ylab = 'Orthophosphate (oP04)')
rug(jitter(algae$oP04),side=2)
abline(h=mean(algae$oP04,na.rm=T),lty=2)


plot(algae$NH4,xlab="")
abline(h = mean(algae$NH4, na.rm=T),lty=1)
abline(h= mean(algae$NH4,na.rm=T) + sd(algae$NH4,na.rm = T) , lty=2)
abline(h = median(algae$NH4, na.rm=T),lty=3)
identify(algae$NH4)

bwplot(size ~ a1,data = algae,panel=panel.bpplot,probs=seq(.01,.49,by=.01),datadensity=TRUE,ylab='River Size',xlab='Álgae A1')
stripplot(season~ a3|mn02,data=algae[!is.na(algae$mn02),])

#Unknown Values
#Removing the Obervations with unknown values
nrow(algae[!complete.cases(algae),])
na.omit(algae) #removing unknown values

algae[apply(algae,1,function(x) sum(is.na(x)))>4,] #removing rows having na's > 4
#or

install.packages('DMwR')
manyNAs(algae,0.2)
nrow(algae[is.na(algae$Chla),])

#removing unknowns using median
algae[is.na(algae$Chla),"Chla"]<- median(algae$Chla,na.rm=T)
data(algae)  
algae <- algae[-manyNAs(algae), ] 
algae <- centralImputation(algae) #book package

symnum(cor(algae[,4:18],use="complete.obs"))

#Multivariate regression
algae <- algae[-manyNAs(algae),]
clean.algae <- knnImputation(algae,k=10)
nrow(algae)
lm.a1 <- lm(a1~.,data=clean.algae[,1:12])
lm2.a1 <- update(lm.a1,. ~ .,-season)
summary(lm2.a1)
final.a1 <- step(lm.a1)

#Regression trees
data(algae)
nrow(algae)

