install.packages("plm",lib = "D:/Library")
library(plm,lib.loc="D:/Library")
library(Formula,lib.loc="D:/Library")
install.packages("fitdistrplus",lib = "D:/Library")
library(lmtest,lib.loc="D:/Library")
library(sandwich,lib.loc="D:/Library")
library(MASS,lib.loc="D:/Library")
library(fitdistrplus,lib.loc="D:/Library")
##Some descriptive analysis

test2013 <- read.csv("D:/Project/EE/test2013.csv", stringsAsFactors=FALSE) ##balanced data for 2013
str(test2013)
test2013$date<-as.Date(test2013$date,"%d-%m-%Y")
summary(test2013$livebirths)
summary(test2013)
library(scales)

demo("colors")

plot(test2013$livebirths~test2013$SNPregReceived)
##############################################################
test2013$lb<-log(test2013$livebirths)
test2013$sp<-log(test2013$SNPregReceived)
test2013$hcp<-log(test2013$HC.Pregnant)
head(test2013$lb)
test2013$lb

test2013$lb[mapply(is.infinite, test2013$lb)] <- NA
test2013$sp[mapply(is.infinite, test2013$sp)] <- NA
test2013$hcp[mapply(is.infinite, test2013$hcp)] <- NA


ols_logs<-lm(lb ~ I(sp/sqrt(sp)) + 
              I(hcp/sqrt(sp))-1,data=test2013)
summary(ols_logs)
bptest(ols_logs)

c_logs<-lm(lb/2 ~ I(sp/2) 
               ,data=test2013)


bptest(c_logs)
plot(I(lb/sqrt(sp)) ~ I(sp/sqrt(sp)) 
       ,data=test2013)
abline()
###############################################################
cor(test2013$HC.Pregnant,test2013$SNPregReceived)

model3<-lm(test2013$livebirths~test2013$SNPregReceived+test2013$HC.Pregnant,data=test2013)
model4<-lm(test2013$livebirths~test2013$SNPregReceived,data=test2013)
summary(model3)

vcovHC(model3,omega=NULL,type="HC3")

modeln<-lm(formula = I(livebirths/sqrt(SNPregReceived)) ~ I(SNPregReceived/sqrt(SNPregReceived)) + 
             I(HC.Pregnant/sqrt(SNPregReceived))- 1, data = test2013) ##important model *******
summary(modeln)
plot(modeln)


fe1<-plm(formula = livebirths ~ name.of.project +SNPregReceived + HC.Pregnant,index = c("date"),
         data=test2013, model ="within" )
          
fe1<-plm(formula = I(livebirths/sqrt(SNPregReceived)) ~ name.of.project +I(SNPregReceived/sqrt(SNPregReceived)) + 
            I(HC.Pregnant/sqrt(SNPregReceived))-1,index=c("date"),data = test2013, model="within")##fixed effects

         summary(fe1) #
         
         fe2<-plm(formula = I(livebirths/sqrt(SNPregReceived)) ~ I(SNPregReceived/sqrt(SNPregReceived)) + 
                    I(HC.Pregnant/sqrt(SNPregReceived)),data = test2013, model="within",index=c("name.of.project","date"))##fixed effects
         summary(fe2)

fixed_effe<-felm(formula = I(livebirths/sqrt(SNPregReceived)) ~ I(SNPregReceived/sqrt(SNPregReceived)) + 
                   I(HC.Pregnant/sqrt(SNPregReceived)),data=test2013)


summary(fixed_effe)

fixed_effe2<-felm(test2013$livebirths~test2013$SNPregReceived+test2013$HC.Pregnant)
summary(fixed_effe2)
         
m<-VAR(test2013,p=1)
bptest(modeln)

whites.htest(modeln)

modelm<-lm(I(livebirths/sqrt(SNPregReceived))~I(SNPregReceived/sqrt(SNPregReceived)),data=test2013)

modelj<-lm(livebirths~I(sqrt(SNPregReceived))+I(sqrt(HC.Pregnant))-1,data=test2013)

modelk<-lm(I(livebirths/sqrt(SNPregReceived))~I(SNPregReceived/sqrt(SNPregReceived))-1,data=test2013)

summary(modelm)
summary(modelj)

hh<-rlm(I(livebirths/sqrt(SNPregReceived))~I(SNPregReceived/sqrt(SNPregReceived))-1,data=test2013)

bptest(modelm)
bptest(modelj) ## closest to reducing heteroskedasticity
bptest(modelk)

model5<-lm(livebirths~snp,data=test2013)
summary(model5)
summary(model4)

bptest(model3)
bptest(model4)
bptest(model5)

hccm(modelm)
hccm(modelj)
coeftest(modelj,vcov=vcovHC(modelj,omega=NULL,type="HC4"),df = Inf)

lm(livebirths~SNPregReceived+HC.Pregnant,weights=I(1/SNPregReceived),data=test2013)


coeftest(model3,vcov=vcovHC(model3,omega=NULL,type="HC4"),df = Inf)
coeftest(model4,vcov=vcovHC(model3,omega=NULL,type="HC4"),df = Inf)
coeftest(model3,vcov=hccm(model3))

coeftest(modeln,vcov=vcovHC(modeln,omega=NULL,type="HC4"),df = Inf) ##important






######################################live births graphs
g<-ggplot(test20132013,aes(x=date,y=livebirths))+geom_bar(position = "stack",stat = "identity",fill="firebrick")+theme_classic()
g<-g+scale_x_date(labels=date_format("%b"),breaks=date_breaks("months"))+labs(x="Date",y="Live Births",main="Distribution on live births in 2013")

##OLS 1- SN preg
gs<-ggplot(test20132013,aes(x=SNPregReceived,y=livebirths))+geom_point(color="slateblue2")+theme_classic()+labs(x="SN for Pregnant",y="Live Births",main="Distribution on live births in 2013")

plot(test2013$livebirths~test2013$SNPregReceived,data=test2013,ylab = "Live Births",xlab="SN for Pregnant women",col="slateblue3")
abline(model3,lty="solid",col="black")
title(main="Live Births vs Supplimentary Nutrition for Pregnant women") 

##OLS 2-HC preg

plot(test20132013$livebirths~test20132013$HC.Pregnant,data=test20132013,ylab = "Live Births",xlab="HC for Pregnant women",col="slategrey")
abline(model3,lty="solid",col="black")
title(main="Live Births vs Health Checkup for Pregnant women") 


##FE 
plot(test2013$livebirths~test2013$SNPregReceived,data=test2013,ylab = "Live Births",xlab="SN for Pregnant women",col="violetred3")
abline(model3,lty="solid",col="black")
title(main="Live Births vs Supplimentary Nutrition for Pregnant women") 
model7<-plm(test2013$livebirths~test2013$SNPregReceived,data=test2013,index=c("date"),model="within")

##FE hc preg
plot(test20132013$livebirths~test20132013$HC.Pregnant,data=test20132013,ylab = "Live Births",xlab="HC for Pregnant women",col="violetred4")
abline(ht1,lty="solid",col="black")
title(main="Live Births vs Health Checkup for Pregnant women") 
#####################################################################  deaths
test20132013$total.deaths<-(test20132013$deaths.0to1yr+test20132013$deaths.1to5yrs)

g1<-ggplot(test20132013,aes(x=date,y=total.deaths))+geom_bar(position = "stack",stat ="identity",fill="steelblue")
g1<-g1+scale_x_date(labels=date_format("%b"),breaks=date_breaks("months"))+labs(x="Date",y="Deaths(0-5 years)",main="Distribution on live births in 2013")
g1

#OLS
str(test2013)
test2013$sntotal<-(test2013$SN1.3.total+test2013$SN6M.1.total+test2013$SN3.6.total)
model6<-lm(total.deaths~sevr.undr.weight.total+sntotal,data=test2013)
summary(model6)
ht3<-plm(total.deaths~sevr.undr.weight.total+sntotal,data=test2013,index=c("date"),model="within")##fixed effects
summary(ht3) #fail

#########################
str(test2013)
ht1<-plm(livebirths~name.of.project+SNPregReceived+HC.Pregnant,data=test2013,index=c("name.of.project"),model="within")##fixed effects

ht2<-plm(livebirths~SNPregReceived+HC.Pregnant,data=test2013,index=c("date"),model="random")
summary(ht1)
summary(ht2)


View(test2013)


pgmm(livebirths~name.of.project+SNPregReceived+HC.Pregnant, data=test2013,
     collapse=FALSE
     ,index=c("date"))



####################################
#Using package benchmarking for DEA

x<- matrix()
