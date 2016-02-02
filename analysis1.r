
##########################################################
sampledec13 <- read.csv("D:/Data_science/Awc/sampledec2013ex.csv", header=FALSE)
View(sampledec13)
names(sampledec13)<-c("Area","No.ofAWCSanctionedMain","Mini","No.ofAWCFunctioningMain","Mini","NoofAWCReportingMain",
                      "Mini","PopulationMale","Female","Total" 
                      ,"Childrenbelow6M","6M-1","1-3","3-6","Total0-6Yr","Pregnant&LactatingP",
                      "L","Total","LiveBirths","StillBirths","Deaths0-1","1-5Yr","P&L","No.ofAWCsProvidingSNPfor21Days+(Main+Mini)","SNPregEligible","SNPregEnroll","SNPregRecieved","SNNursingEligible","SN Nursing Enroll","SN Nursing Recieved","SN 6M-1Yr Eligible Boys","Girls","Total",
                      "SN6M-1YrEnrolled Boys","Girls","Total","SN6M-1Yr Recieved Boys","Girls","Totals","SN 1-3 Eligible Boys","Girls","Total","SN 1-3 Enrolled Boys","Girls","Total","SN 1-3 Received Boys","Girls","Total","SN 3-6Yr Eligible Boys","Girls","Total","SN 3-6 Enrolled Boys","Girls","Total","SN 3-6 Recieved Boys","Girls","Total","Below 1 Yr Boys","Girls","1-3 Yrs Boys","Girls","3-6Yrs Boys","Girls",
                      "Normal","Below 1Yr Boys","Girls","1-3Yrs Boys","Girls","3-6Yrs Boys",
                      "Girls","Moderately Underweight","Below 1Yr Boys","Girls",
                      "1-3Yrs Boys","Girls","3-6Yrs Boys","Girls","Severely Underweight",
                      "NS Total")
                      
 require(ggplot2)

plot<-qplot(SN Preg Recieved,data=sampledec13)
g<-ggplot(sampledec13,aes(x=Area,y=LiveBirths))
g<-g+scale_y_continuous(limits=c(10,600))+geom_point()
g

g<-ggplot(sampledec13,aes(x=SNPregRecieved,y=LiveBirths))
g<-g+geom_point()
g






str(sampledec13)
samplerural2013<-sampledec13[sampledec13$Area=="Rural",]
View(samplerural2013)
samplerural2013
head(samplerural2013)
g1<-ggplot(samplerural2013,aes(x=SNPregRecieved,y=LiveBirths))
g1<-g1+geom_point()+geom_line()+scale_x_continuous(limits=c(200,4000))
g2
###################################################################################################
#loading packages
require(dplyr)
require(ggplot)
install.packages("corrplot")
require(corrplot)
require(sampling)
###################################################################################################
Data  1
###################################################################################################
setwd("D:\\Data_science\\Awc")
jan2013 <- read.csv("D:/Data_science/Awc/2013/jan2013.csv", header=FALSE,stringsAsFactors=FALSE)
feb2013 <- read.csv("D:/Data_science/Awc/2013/feb2013.csv", header=FALSE,stringsAsFactors=FALSE)
  #View(feb2013)
march <- read.csv("D:/Data_science/Awc/2013/march.csv", header=FALSE,stringsAsFactors=FALSE)
  #View(march)
april <- read.csv("D:/Data_science/Awc/2013/april.csv", header=FALSE, stringsAsFactors=FALSE)
   #View(april)
may <- read.csv("D:/Data_science/Awc/2013/may.csv", header=FALSE, stringsAsFactors=FALSE)

   #View(may)
june<-read.csv("D:/Data_science/Awc/2013/june.csv", header=FALSE, stringsAsFactors=FALSE)
 #View(june)
july<-read.csv("D:/Data_science/Awc/2013/july.csv", header=FALSE, stringsAsFactors=FALSE)
aug<-read.csv("D:/Data_science/Awc/2013/aug2013.csv", header=FALSE, stringsAsFactors=FALSE)
sept<-read.csv("D:/Data_science/Awc/2013/sept.csv", header=FALSE, stringsAsFactors=FALSE)
nov<-read.csv("D:/Data_science/Awc/2013/nov.csv", header=FALSE, stringsAsFactors=FALSE)
View(nov)
dec<-read.csv("D:/Data_science/Awc/2013/December2013.csv", header=FALSE, stringsAsFactors=FALSE)
data2013x<-rbind(jan2013,feb2013,march,april,may,june,july,aug,sept,nov,dec)

data2013<-na.omit(data2013x)
data2013$funcAwc<-(data2013$V8+data2013$V9)


hcpreg <- read.csv("D:/Data_science/Awc/2013/hcpreg.csv", header=FALSE, stringsAsFactors=FALSE)
View(hcpreg)
str(hcpreg)


hcpreg<-hcpreg[-c(1:4),]
hcpreg<-hcpreg[c(1:4217),]
names(hcpreg)<-c("hc.preg","hc.nursing")
hcpreg$hc.preg<-as.numeric(hcpreg$hc.preg)
hcpreg$hc.nursing<-as.numeric(hcpreg$hc.nursing)

data2013<-cbind(data2013,hcpreg) ##final data set
View(data2013)


str(data2013)
data2013$V2<-as.Date(data2013$V2,"%d/%m/%Y") ##changing the variables into numeric values
data2013$V20<-as.numeric(data2013$V20)
data2013$V23<-as.numeric(data2013$V23)
data2013$V24<-as.numeric(data2013$V24)
data2013$V25<-as.numeric(data2013$V25)
data2013$V40<-as.numeric(data2013$V40)
data2013$V14<-as.numeric(data2013$V14)
data2013$V8<-as.numeric(data2013$V8)
data2013$V9<-as.numeric(data2013$V9)
data2013$V62<-as.numeric(data2013$V62)
data2013$V63<-as.numeric(data2013$V63)
data2013$V64<-as.numeric(data2013$V64)
data2013$V65<-as.numeric(data2013$V65)
data2013$V66<-as.numeric(data2013$V66)
data2013$V67<-as.numeric(data2013$V67)
data2013$V68<-as.numeric(data2013$V68)
data2013$V69<-as.numeric(data2013$V69)
data2013$V70<-as.numeric(data2013$V70)
data2013$V71<-as.numeric(data2013$V71)
data2013$V72<-as.numeric(data2013$V72)
data2013$V73<-as.numeric(data2013$V73)
data2013$V74<-as.numeric(data2013$V74)
data2013$V75<-as.numeric(data2013$V75)
data2013$V76<-as.numeric(data2013$V76)
data2013$V77<-as.numeric(data2013$V77)
data2013$V78<-as.numeric(data2013$V78)
data2013$V79<-as.numeric(data2013$V79)
data2013$V80<-as.numeric(data2013$V80)
data2013$V81<-as.numeric(data2013$V81)
data2013$V82<-as.numeric(data2013$V82)
data2013$V83<-as.numeric(data2013$V83)
data2013$w1<-as.numeric(data2013$V84)
###################################################################################################
###################################################################################################
this is srikakulam
#####################################################################################################
sri2013<-data2013[data2013$V3=="SRIKAKULAM",]
chi2013<-filter(data2013$V3=="CHITTOOR")
chi13<-filter(data2013,(V3=="CHITTOOR"))
chi<-data2013[data2013$V3=="CHITTOOR",]
View(sri2013)
str(data2013)
View(chi)
sri2013$V2<-as.Date(sri2013$V2,"%d/%m/%Y")
ts_sri2013<-ts(sri2013,frequency=12)
ts_sri2013
View(ts_sri2013)
plot(ts_sri2013$V23 ~ ts_sri2013$V2,type="l",col="red",axes=F)
plot(sri2013$V23 ~ sri2013$V2,col="red",axes=FALSE)
box()
axis(1,sri2013$V2,format(sri2013$V2,"%m-%y"))

gsri<-ggplot(sri2013,aes(x=sri2013$V2,y=sri2013$V23))
gsri<-gsri+geom_point()
gsri

g<-ggplot(ts_sri2013,aes(ts_sri2013$V2,ts_sri2013$V23))
plot(sri2013$V23~sri2013$V2,,col="red",axes=FALSE)
axis(1,sri2013$V2,format(sri2013$V2,"%m-%y"))

####################################################################################################
using linear regression 
g1<-ggplot(sri2013,aes(x=V31,y=V23))
g1<-g1+geom_point()
g1

str(sri2013)
sri2013$V23<-as.numeric(sri2013$V23)
sri2013$V31<-as.numeric(sri2013$V31)
names(sri2013$V23)<-c("LiveBirths")
mean.livebirths<-mean(sri2013$V23,na.rm=T)
mean.livebirths
plot(sri2013$V23~sri2013$V31,sri2013,col="blue") ##live births vs sn preg recieved
abline(h=mean.livebirths)
model1<-lm(sri2013$V23~sri2013$V31,data=sri2013) ## linear regression
abline(model1)
summary(model1)
plot(model1)
texample<-t.test(sri2013$V23,sri2013$V31)
texample
####################################################################################################
##
sri2013$V25<-as.numeric(sri2013$V25)
sri2013$V26<-as.numeric(sri2013$V26)
sri2013$V31<-as.numeric(sri2013$V31)

automate1<-function(data){
    p1<-ggplot(data,aes(x=V31,y=V23),col="blue")+geom_point()
   
}
automate(sri2013)

automate2<-function(data){
    p2<-plot(data$V23~data$V25,data,col="blue")
}
automate1(data2013)
automate2(data2013)

automate3<-function(data){
    p3<-plot(data$V25~data$V40,data,col="red")
}
sri2013$V14<-as.numeric(sri2013$V14)
data2013$V14<-as.numeric(data2013$V14)
mean.pop<-mean(sri2013$V14,)

############################
require(dplyr)
##############################
grp<-group_by(sri2013,V2,V3)
str(grp)
summary<-summarise(grp,mean=mean(V14))
plot(summary,type="l",col="blue")
axis(1,summary,format(summary,"%m-%y"))
ps<-ggplot(summary)+geom_point()+scale_x_continuous(limits=c(159100,167658))
ps

ts1<-ts(summary)
ts1

ts1plot<-plot(summary$mean~summary$V2,type="l",col="red",axes=F)
box()
axis(1,summary$V2,format(summary$V2,"%m-%Y"))
#####################################################################
################################################################################
#########################################################################################

#for live births vs no of awc , no of preg women,sn for preg women,hc for preg
View(numberofAwc) 
str(numberofAwc)
srinumberofAwc<-numberofAwc[c(19,42,65,88,111,144,167),] ## no of awc's in srikakulam
srinumberofAwc
subsri.numberofpregwomen<-summarise(grp,no.preg=mean(V20)) ## no of pregnant women in srikakulam
subsri.numberofpregwomen
grp$V31<-as.numeric(grp$V31)
subsri.sn.for.preg<-summarise(grp,sn_pregwomen=mean(V31)) ##supplimentary nutrition for pregnant women
subsri.sn.for.preg

subsri.hc.preg<-summarise(grp,hc.preg=mean(hc.preg)) ## preg women who have gone for health checkups
subsri.hc.preg

livebs<-summarise(grp,livebirths=mean(V23))
livebs
smodel<-lm(livebs$livebirths~srinumberofAwc$number.Awc+subsri.numberofpregwomen$no.preg+subsri.sn.for.preg$sn_pregwomen+subsri.hc.preg$hc.preg)
summary(smodel)
plot(smodel)
#####################################################################################################
entire 12 months
######################################################################################################
names(data2013)<-c("S.No","date","district","projectname","area","No.ofAWCsSanctionedmain","sancmini","No.ofAWCfunctioningmain","funcmini",
                   "No.ofAWCreportingmain","reportmini","populationmale","popfemale","totalpop","childrenbelow6mon","child6M-1yr","child1-3yr","child3-6yr",
                   "totalpop0-6yr","poppreg","poplac","pandltotalpop","livebirths","stillbirths","death0to1","death1to5","deathpreglac",
                   "No.ofAWCsProvidingSN21Days(Main+Mini)"," SNPregEligible","SNPregEnroll","SNPregReceived","SNNursingEligible","SNNursingEnroll","SNNursingReceived",
                   "SNeligible6mto1boys","SNeligible6mto1girls","SNeligible6mto1total","SN6Mto1YrEnrolledboys","SN6Mto1YrEnrolledgirls","SN6Mto1YrEnrolledtotal","SN6Mto1YrsReceivedboys",
                   "SN6Mto1YrsReceivedgirls","SN6Mto1YrsReceivedtotal","SN1to3YsEligibleboys","SN1to3YsEligiblegirls","SN1to3YsEligibletotal","SN1to3Enrolledboys","SN1to3Enrolledgirls","SN1to3Enrolledtotal",
                   "SN1to3Receivedboys","SN1to3Receivedgirls","SN1to3Receivedtotal","SN3to6Eligibleboys","SN3to6Eligiblegirls","SN3to6Eligibletotal","SN3to6Enrolledboys","SN3to6Enrolledgirls"
                   "SN3to6Enrolledtotal","SN3to6Receivedboys","SN3to6Receivedgirls","SN3to6Receivedtotal","Below1Yrboys","Below1Yrgirls","N1to3Yrsboys","N1to3Yrgirls",
                   "N3to6Yrsboys","N3to6Yrsgirls","normal","MUBelow1Yrboys","MUBelow1Yrgirls","MU1to3Yrsboys","MU1to3Yrgirls","MU3to6Yrsboys","MU3to6Yrsgirls","moderatetlyunder","SUBelowoneYrboys","SUBelowoneYrgirls","SUonetothreeYrsboys","SUonetotreeYrgirls","SU3to6Yrsboys","SU3to6Yrsgirls","severelyunderweight",
                   "nstotal")

str(data2013)


###############################################################
group<-group_by(data2013,V3,V2)
group
View(group)
sum2013<-summarise(group,mean=mean(V14)) ##mean for population(v14)
warnings()
sum2013
str(sum2013)
warning()
View(sum2013)
Asum<-sum2013[sum2013$V3=="ADILABAD",]
Asum
str(Asum)
Asum$V2<-as.Date(Asum$V2,"%d/%m/%Y")
plot(Asum$mean~Asum$V2,type="l")
lmA<-lm(Asum$mean~Asum$V2,data=Asum)
lmA
plot(lmA)

a<-ggplot(Asum,aes(x=V2,y=mean/10,000,fill=V3))+
    labs(x="Months",y="Number of people",fill=NULL)
b<-a+geom_bar(stat="identity",position="stack")
b

c<-ggplot(dis2013,aes(x=V2,y=mean/10,000,fill=V3))+
    labs(x="Months",y="Number of people",fill=NULL)   ###population variance for 11 months in year 2013
d<-c+geom_bar(stat="identity",position="stack")
d  

e<-ggplot(dist2013,aes(x=V3,y=(mean/1e+00),fill=V2))+
    labs(x="Districts",y="Number of people",fill=NULL)   ###population variance for 11 months in year 2013
f<-e+geom_bar(stat="identity",position="stack")+theme(axis.text.x=element_text(angle=90,hjust=1),
                                                      panel.grid.major = theme_line(colour = "grey90"),
                                                      panel.grid.minor = theme_blank(), panel.background = theme_blank(),
                                                      axis.ticks = theme_blank(), legend.position = "none")
f 



View(dis2013)

View(sum2013)
sum2013<-sum2013[-53,]
sum2013<-sum2013[-2,]
sum2013<-sum2013[-1,]


final2013<-sum2013[54:281,] ##this is final one for pop

dis2013<-arrange(final2013,V2)
str(final2013)
View(final2013)
Ksum<-sum2013[sum2013$V3=="KRISHNA",]
View(jansum)
dist2013<-unique(dis2013[,1:3])
View(dist2013)

#########################################################################################

doing linear and multiple regression now

model2<-lm(V25~V40,data=data2013) ## deaths vs sn for 6m-1
model2
plot(model2)
plot(V25~V40,data=data2013)
ylim(c(0,100))

plot(data2013$V40/10000,data2013$V25,data=data2013,ylim=c(0,150))

data2013$funcAwc<-(data2013$V8+data2013$V9)
data2013$funcAwc
View(data2013)
model3<-lm(V23~V40+funcAwc,data=data2013) ##sn recieved by 0-1 yrs and no of functioning awcs
data2013$funcAwc 
summary(model3)
plot(model3)

##################################################################################################
weight for under 1 year vs no of functioning AWC
#################################################################################################

weight.1<-data.frame(data2013[,62:84])

require(ggplot2)
require(dplyr)
View(weight.1)
names(weight.1)<-c("normal.under1.boys","normal.under1.girls","normal1to3.boys","normal1to3.girls","normal3to6.boys","normal3to6.girls",
                   "normal.total","mod.under1.boys","mod.under1.g","mod.1to3.b","mod.1to3.g","mod.3to6.boys","mod.3to6.girls","moderate.total",
                   "serv.under1.boys","serv.under1.girls","serv.1to3.b","serv.1to3.g","serv.3to6.boys","serv.3to6.girls","serv.total","NStotal","funcAWC")


weight.1<-cbind(weight.1,data2013$V2,data2013$V3) ##final weight matrix

plot(weight.1$normal.under1.boys/10000~weight.1$funcAWC/1e+00) ## ignore

xlim=c(0,1000)
?xlim()

weight.1$w1<-((weight.1$normal.under1.boys*1)+(weight.1$mod.under1.boys*0.75)+(weight.1$serv.under1.boys*0.5))
weight.1$w1 ## dint work


group2<-group_by(weight.1,data2013$V2,data2013$V3)
group2
View(group2)
weight.summary<-summarise(group2,weight.under1.normal=mean(normal.under1.boys))
weight.summary

weight.summary$weight.under1.normal<-trunc(weight.summary$weight.under1.normal)
weight.summary
wsample<-weight.summary[weight.summary$weight.under1.normal<5000,]

group3<-group_by(weight.1,data2013$V2,data2013$V3)
numberofAwc<-summarise(group3,number.Awc=mean(funcAWC))
numberofAwc$number.Awc<-trunc(numberofAwc$number.Awc)
numberofAwc
View(numberofAwc)
numberofAwc<-numberofAwc[-c(47:52),]
numberofAwc<-numberofAwc[-c(70:91),]
numberofAwc<-numberofAwc[-c(93:115),]
numberofAwc<-numberofAwc[-c(122:143),]
View(weight.summary)
weight.summary<-weight.summary[-c(47:52),]
weight.summary<-weight.summary[-c(70:91),]
weight.summary<-weight.summary[-c(93:115),]
## use weight.summary

plot(numberofAwc$number.Awc~weight.summary$weight.under1.normal)
plot(weight.summary$weight.under1.normal~numberofAwc$number.Awc)
model4<-lm(numberofAwc$number.Awc~weight.summary$weight.under1.normal)
abline(model4)
plot(model4)
summary(model4)
model5<-lm(weight.summary$weight.under1.normal~numberofAwc$number.Awc)
abline(model5)
summary(model5)

##############################################################
forr mod underweight

summ2<-summarise(group2,under1.mod.mean=mean(mod.under1.boys))
summ2
plot(numberofAwc$number.Awc~summ2$under1.mod.mean)
model5<-lm(numberofAwc$number.Awc~summ2$under1.mod.mean)
abline(model5)
summary(model5)
######################################################################
data2013$V34<-as.numeric(data2013$V34)
data2013$V16<-as.numeric(data2013$V16)  ## preparing data for multivariation 0-1weight vs hc nm ,sn nm and sn of children(0-1)
SN_nm<-data2013[data2013$V34<1000,34]
SN_nm<-SN_nm[-c(652:669)]
View(SN_nm)
SN_child6M<-data2013[data2013$V16<1150,16]
View(SN_child6M)
SN_child6M<-SN_child6M[-c(652:704)]

weight<-(data2013$V62+data2013$V63)
w<-weight[1:651]


######################################################################## loading hcpreg
hcpreg <- read.csv("D:/Data_science/Awc/2013/hcpreg.csv", header=FALSE, stringsAsFactors=FALSE)
View(hcpreg)
str(hcpreg)

hcpreg<-hcpreg[-c(1:4),]
hcpreg<-hcpreg[c(1:4217),]
names(hcpreg)<-c("hc.preg","hc.nursing")
hcpreg$hc.preg<-as.numeric(hcpreg$hc.preg)
hcpreg$hc.nursing<-as.numeric(hcpreg$hc.nursing)
multi<-data.frame(weight=w,sn_nm=SN_nm,sn_child=SN_child6M)
multi<-cbind(multi,hcpreg)
View(multi)
model6<-lm(weight~sn_nm+sn_child+hc.nursing,data=multi)
##multiple regression
summary(model6)
plot(model6)
############################################################################################
this is chittoor
##################################################################################################
View(chi)
gr_chi<-group_by(chi,V2,V3)
View(gr_chi)


gr_chi$V31<-as.numeric(gr_chi$V31)
chi2013no.of.preg<-summarise(gr_chi,no.preg=mean(V20))
chi2013no.of.preg
chi2013no.of.preg<-chi2013no.of.preg[-c(1),]
View(chi2013no.of.preg)


chi2013.sn_preg<-summarise(gr_chi,sn_preg=mean(V31))
chi2013.sn_preg<-chi2013.sn_preg[-c(1),]
a<-gr_chi$V20 ## number of pregnant women
b<-gr_chi$V23 ## number of live births
c<-gr_chi$V31 ## sn for pregnant women
d<-gr_chi$hc.preg ## hc for pregnant women
cor(a,b)
cor(b,c)
cor(b,d)

str(numberofAwc)
numberofAwc
View(numberofAwc)
chi.numofAwc<-numberofAwc[c(3,26,66,
                            100,124,144,164),]
chi.numofAwc<-chi.numofAwc[-c(1,2,3),]
View(chi.numofAwc)
chi.numofAwc
chi.hc.preg<-summarise(gr_chi,hc.preg=mean(hc.preg))
chi.hc.preg
chi.hc.preg<-chi.hc.preg[-c(1),]
chi.hc.preg
chi.livebirths<-summarise(gr_chi,livebirth=mean(V23))
chi.livebirths<-chi.livebirths[-c(1),]
c
a<-chi.livebirths$livebirth
a
b<-chi.numofAwc$number.Awc
str(a)
cor(a,b)


cmodel<-lm(chi.livebirths$livebirth~chi2013.sn_preg$sn_preg+chi.hc.preg$hc.preg)
summary(cmodel)
plot(cmodel)
########################################################################################GUNTUR
guntur2013<-data2013[data2013$V3=="GUNTUR",]

group_guntur<-group_by(guntur2013,V2,V3)
View(group_guntur)
group_guntur$V31<-as.numeric(group_guntur$V31)
group_guntur$V37<-as.numeric(group_guntur$V37) ## sn for children 6m-1yr



aa<-group_guntur$V23 ## live births
bb<-group_guntur$V31 ## sn for pregnant women
cor(aa,bb)

cor(group_guntur)
cc<-group_guntur$hc.preg ## hc for preg
cor(aa,cc)
dd<-group_guntur$V20 ## no of preg womenn
cor(aa,dd)
ee<-group_guntur$V37
cor(ee,aa)
ff<-group_guntur$V25
cor(ff,ee)
##
guntur<-group_guntur[,6:86]
head(guntur)
guntur<-data.matrix(guntur)

gcor<-cor(guntur)
str(guntur)
corrplot(gcor,method = "circle")

##'
strata.sample.g<-strata(group_guntur,c("V2"),size=c(10),method= "srswor")
sample.g<-group_guntur[sample(1:nrow(group_guntur),50,replace=FALSE),]
strata.sample.g
sample.g
View(sample.g)
cor(sample.g$V25,sample.g$V40)

gsample<-data.frame(sample.g$V23,sample.g$V31,sample.g$hc.preg,sample.g$V34) ##normal sample
names(gsample)<-c("live.births","sn.for.preg","hcpreg","sn.for.nursing")
cor(gsample$live.births,gsample$sn.for.preg)
  str(gsample)
summary(gsample)
mean(gsample$live.births)
View(gsample)
logsample<-log(gsample)                                         ####
str(logsample)


install.packages("gclus")
require(gclus)
my.abs<-abs(cor(gsample))
my.colors<-dmat.color(my.abs)
my.ordered<-order.single(cor(gsample))
cpairs(gsample,my.ordered,panel.colors=gsample)
gcor2<-cor(gsample)
corrplot(gcor2,method="circle")##drawing correlation plot



#standardize values
standardize<-function(x){x-mean(x)}
standard.val<-apply(gsample,2,standardize)
standard.val

##applying priciple component analysis

g.pca<-prcomp(standard.val)
plot(g.pca,type="l")
print(g.pca)
summary(g.pca)

?prcomp()
g.pca2<-prcomp(gsample,center = T,scale. = T)
print(g.pca2)
plot(g.pca2,type="l")
summary(g.pca2)
screeplot(g.pca2,main="Screeplot",xlab="Components")
screeplot(g.pca2,type="l")

logsample
logsample[25,3]<-0
g.pca3<-prcomp(logsample,center=T,scale. = T)
print(g.pca3)
screeplot(g.pca3,xlab="Components")
biplot(g.pca3,cex=c(0,0.7))
##predict PCs
predict(g.pca2,newdata=tail(standard.val,2))
###
gsample
my.var<-varimax(g.pca3$rotation)
my.var
gsample$sn.for.nursing<-as.numeric(gsample$sn.for.nursing)
model10<-lm(live.births~sn.for.preg,data=gsample)
model10
predict(model10,n.ahead = 10)
summary(model10)
View(gsample)
########################################################################
gg1<-lm(sample.g$V40~sample.g$V25,data=sample.g)
sample.g<-sample.g[sample.g$V40 < 10000,]
sample.g
sample.g$V69<-as.numeric(sample.g$V69)
sample.g$V70<-as.numeric(sample.g$V70)
sample.g<-sample.g[sample.g$V69 <10000 & sample.g$V70<10000]
sample.g$total.mod.total<-(sample.g$V69+sample.g$V70)
sample.g$total.undersevere.6m<-(sample.g$V76+sample.g$V77)
sample.g$total.undersevere.6m
cor.a<-sample.g$total.mod.total
cor.b<-sample.g$V25
cor.c<-sample.g$total.undersevere.6m
cor(cor.c,cor.a)
#################################################################
between severe underweight and deaths
#################################################################
plot(sample.g$V25~sample.g$total.undersevere.6m)
model7<-lm(sample.g$V25~sample.g$V43)
abline(model7)
summary(model7)
sample.g$V37<-as.numeric(sample.g$V37)
sample.g$ratio.sn6m<-(sample.g$V43/sample.g$V37)
sample.g$ratio.sn6m
cor(sample.g$V25,sample.g$ratio.sn6m)
plot(sample.g$ratio.sn6m~sample.g$V25,log="xy")
model8<-lm(sample.g$V25~sample.g$ratio.sn6m)
abline(model8)
summary(model8)


k<-ggplot(sample.g,aes(x=V25,y=ratio.sn6m,fill=ratio.sn6m))+
    labs(x="deaths",y="frequency",fill=NULL)   ### frequency of deaths filled with ratio of sn
l<-k+geom_bar(stat="identity",position="stack")+scale_y_continuous(limits=c(0,20),breaks=c(0,2,4,6,8,10,12,14,16,18))

l
#######################################################################
plot(sample.g$V25~sample.g$V43)
sample.g$V43<-as.numeric(sample.g$V43)
gg2<-lm(sample.g$V25~sample.g$V43,data=sample.g,weights = (1/V43))
abline(gg2)
ylab=c(0,100)
plot(gg2)

#######################################################################

new1<-data.frame(sample.g$V25,sample.g$V43) 
names(new1)<-c("death","sn.6M") 
new1$sn.6M<-as.numeric(new1$sn.6M) ## sn for entire 6M-1 children (boys+girls)
gg3<-lm(new1$sn.6M~new1$death,data=new1)
gg3
pre<-predict(gg3,interval="prediction")
pre
str(new1)
plot(new1$death~new1$sn.6M)
plot(gg3)

abline(gg3)
sn.log<-log(new1$sn.6M)
gg4<-lm(sn.log~new1$death)
gg4
p<-predict(gg4,interval="prediction")
p
summary(gg4)
abline(gg4)
summary(gg3)
str(new1)
new1$sn.6M<-as.numeric(new1$sn.6M)


n1<-nls(new1$death~new1$sn,new1,start=list(death=2,sn=1))
?nls()


