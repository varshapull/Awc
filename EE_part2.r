##Master data file - test2013

test2013 <- read.csv("D:/Project/EE/test2013.csv", stringsAsFactors=FALSE) ##balanced data for 2013

str(test2013)

summary(test2013)


required_attri<-data.frame(livebirths=test2013$livebirths,SNPreg=test2013$SNPregReceived,HCPreg=test2013$HC.Pregnant,
                           pregnant_population=test2013$pregnant.pop,pcode = test2013$pcode,date = test2013$date,name=test2013$name.of.project,district=test2013$district) ##filtering out variables 
#which correspond to mortality and creating a data frame.

describe(required_attri)

str(required_attri)
required_attri$date <- as.Date(required_attri$date, "%d-%m-%Y")
required_attri$date <- as.Date(required_attri$date, "%Y-%m-%d")


#######################################################################
# FILTERING OUT BY MONTH
jan<-required_attri[required_attri$date=="2013-01-01",]
f<-required_attri[required_attri$date=="2013-02-01",]
mr<-required_attri[required_attri$date=="2013-03-01",]
a<-required_attri[required_attri$date=="2013-04-01",]
m<-required_attri[required_attri$date=="2013-05-01",]
j<-required_attri[required_attri$date=="2013-06-01",]
jl<-required_attri[required_attri$date=="2013-07-01",]
aug<-required_attri[required_attri$date=="2013-08-01",]
s<-required_attri[required_attri$date=="2013-09-01",]
nov<-required_attri[required_attri$date=="2013-11-01",]
dec<-required_attri[required_attri$date=="2013-12-01",]

View(jan)
write.csv(x=jan,file="D:/Project/EE/filter_jan.csv")
str(jan)

#CREATING MATRICES FOR IV AND DV's BECAUSE DEA FUNCTION TAKES MATRICES AS INPUT

ja1Y<-matrix(c(jan$livebirths,jan$SNPreg,jan$HCPreg),ncol=3)
fY<-matrix(c(f$livebirths,f$SNPreg,f$HCPreg),ncol=3)
mrY<-matrix(c(mr$livebirths,mr$SNPreg,mr$HCPreg),ncol=3)
aY<-matrix(c(a$livebirths,a$SNPreg,a$HCPreg),ncol=3)
mY<-matrix(c(m$livebirths,m$SNPreg,m$HCPreg),ncol=3)
jY<-matrix(c(j$livebirths,j$SNPreg,j$HCPreg),ncol=3)
jlY<-matrix(c(jl$livebirths,jl$SNPreg,jl$HCPreg),ncol=3)
augY<-matrix(c(aug$livebirths,aug$SNPreg,aug$HCPreg),ncol=3)
sepY<-matrix(c(s$livebirths,s$SNPreg,s$HCPreg),ncol=3)
novY<-matrix(c(nov$livebirths,nov$SNPreg,nov$HCPreg),ncol=3)
decY<-matrix(c(dec$livebirths,dec$SNPreg,dec$HCPreg),ncol=3)
#########################################################################
#Performing dea 

dea1 <- dea(ja1Y,noutput=1,orientation=1,rts=2) ##january
View(dea1)
dea1<-cbind(dea1,jan$name,jan$district)
sample_j <- sample(x = ja1Y,size=20)
jan_sample <- ja1Y[sample_j,]
str(jan_sample)
dea.plot(ja1Y[,2],ja1Y[,1],ORIENTATION = "in-out",RTS = "vrs")

write.csv(x=dea1,file="D:/Project/EE/dea_jan.csv")

dea2<- dea(fY,noutput=1,orientation=1,rts=2)  ##febuary
View(dea2)

dea2<-cbind(dea2,jan$name,jan$district)

write.csv(x=dea2,file="D:/Project/EE/dea/dea_feb.csv")

dea3<- dea(mrY,noutput=1,orientation=1,rts=2) ##march
View(dea3)

dea3<-cbind(dea3,jan$name,jan$district)
write.csv(x=dea3,file="D:/Project/EE/dea/dea_mar.csv")

dea4<- dea(aY,noutput=1,orientation=1,rts=2) ##april
View(dea4)

dea4<-cbind(dea4,jan$name,jan$district)
write.csv(x=dea4,file="D:/Project/EE/dea/dea_april.csv")

dea5 <- dea(mY,noutput=1,orientation=1,rts=2) ##may
View(dea5)

dea5<-cbind(dea5,jan$name,jan$district)
write.csv(x=dea5,file="D:/Project/EE/dea/dea_may.csv")

dea6<-  dea(jY,noutput=1,orientation=1,rts=2) ##june
View(dea6)

dea6<-cbind(dea6,jan$name,jan$district)
write.csv(x=dea6,file="D:/Project/EE/dea/dea_june.csv")

dea7<- dea(jlY,noutput=1,orientation=1,rts=2) ##july
View(dea7)

dea7<-cbind(dea7,jan$name,jan$district)
write.csv(x=dea7,file="D:/Project/EE/dea/dea_july.csv")

dea8 <-  dea(augY,noutput=1,orientation=1,rts=2) ##august

dea8<-cbind(dea8,jan$name,jan$district)
write.csv(x=dea8,file="D:/Project/EE/dea/dea_aug.csv")

dea9 <-  dea(sepY,noutput=1,orientation=1,rts=2)##september

dea9<-cbind(dea9,jan$name,jan$district)
write.csv(x=dea9,file="D:/Project/EE/dea/dea_sept.csv")
View(dea9)

dea11<-  dea(novY,noutput=1,orientation=1,rts=2) ##november

dea11<-cbind(dea11,jan$name,jan$district)
write.csv(x=dea11,file="D:/Project/EE/dea/dea_nov.csv")
View(dea11)

dea12<-  dea(decY,noutput=1,orientation=1,rts=2) ##december

dea12<-cbind(dea12,jan$name,jan$district)
write.csv(x=dea12,file="D:/Project/EE/dea/dea_dec.csv")

###################
#Dividing into training and validation sets

sample_size1 <- floor(0.70 * nrow(test2013))
sample_size1

sample_size2 <- floor(0.80 * nrow(test2013))

set.seed(123)
train_70 <- sample(1: nrow(test2013),sample_size1)
train.A <- test2013[train_70,]
test.A <-test2013[-train_70,]

train_80 <- sample(1: nrow(test2013),sample_size2)
train.B <- test2013[train_80,]
test.B <- test2013[-train_80,]

str(train.A)
trainA_attri<-data.frame(livebirths=train.A$livebirths,SNPreg=train.A$SNPregReceived,HCPreg=train.A$HC.Pregnant,
                           pcode = train.A$pcode,date = train.A$date,name=train.A$name.of.project,district=train.A$district) ##filtering out variables 
#which correspond to mortality and creating a data frame.

describe(required_attri)

str(required_attri)
trainA_attri$date <- as.Date(trainA_attri$date, "%d-%m-%Y")
trainA_attri$date <- as.Date(trainA_attri$date, "%Y-%m-%d")


#######################################################################
# FILTERING OUT BY MONTH
jan_train.A<-trainA_attri[trainA_attri$date=="2013-01-01",]
f_train.A<-trainA_attri[trainA_attri$date=="2013-02-01",]
mr_train.A<-trainA_attri[trainA_attri$date=="2013-03-01",]
a_train.A<-trainA_attri[trainA_attri$date=="2013-04-01",]
m_train.A<-trainA_attri[trainA_attri$date=="2013-05-01",]
j_train.A<-trainA_attri[trainA_attri$date=="2013-06-01",]
jl_train.A<-trainA_attri[trainA_attri$date=="2013-07-01",]
aug_train.A<-trainA_attri[trainA_attri$date=="2013-08-01",]
s_train.A<-trainA_attri[trainA_attri$date=="2013-09-01",]
nov_train.A<-trainA_attri[trainA_attri$date=="2013-11-01",]
dec_train.A<-trainA_attri[trainA_attri$date=="2013-12-01",]


ja.A<-matrix(c(jan_train.A$livebirths,jan_train.A$SNPreg,jan_train.A$HCPreg),ncol=3)
fY.A<-matrix(c(f_train.A$livebirths,f_train.A$SNPreg,f_train.A$HCPreg),ncol=3)
mrY.A<-matrix(c(mr_train.A$livebirths,mr_train.A$SNPreg,mr_train.A$HCPreg),ncol=3)
aY.A<-matrix(c(a_train.A$livebirths,a_train.A$SNPreg,a_train.A$HCPreg),ncol=3)
mY.A<-matrix(c(m_train.A$livebirths,m_train.A$SNPreg,m_train.A$HCPreg),ncol=3)
jY.A<-matrix(c(j_train.A$livebirths,j_train.A$SNPreg,j_train.A$HCPreg),ncol=3)
jlY.A<-matrix(c(jl_train.A$livebirths,jl_train.A$SNPreg,jl_train.A$HCPreg),ncol=3)
augY.A<-matrix(c(aug_train.A$livebirths,aug_train.A$SNPreg,aug_train.A$HCPreg),ncol=3)
sepY.A<-matrix(c(s_train.A$livebirths,s_train.A$SNPreg,s_train.A$HCPreg),ncol=3)
novY.A<-matrix(c(nov_train.A$livebirths,nov_train.A$SNPreg,nov_train.A$HCPreg),ncol=3)
decY.A<-matrix(c(dec_train.A$livebirths,dec_train.A$SNPreg,dec_train.A$HCPreg),ncol=3)

dea1A<- dea(ja.A,noutput=1,orientation=1,rts=2)  
View(dea1A)
dea1A <- cbind(dea1A,jan_train.A$name,jan_train.A$district)
write.csv(dea1A,"D:/Project/EE/dea/dea70/dea_janA.csv")

dea2A<- dea(fY.A,noutput=1,orientation=1,rts=2) 
dea2A <- cbind(dea2A,f_train.A$name,f_train.A$district)

dea2A<- dea(fY.A,noutput=1,orientation=1,rts=2) 
dea2A <- cbind(dea2A,f_train.A$name,f_train.A$district)
dea2A<- dea(fY.A,noutput=1,orientation=1,rts=2) 
dea2A <- cbind(dea2A,f_train.A$name,f_train.A$district)

dea3A<-dea(mrY.A,noutput=1,orientation = 1, rts=2)
dea3A <- cbind(dea3A,mr_train.A$name,mr_train.A$district)
write.csv(dea3A,"D:/Project/EE/dea/dea70/marA_dea.csv")

dea4A<-dea(aY.A,noutput=1,orientation = 1, rts=2)
dea4A <- cbind(dea4A,a_train.A$name,a_train.A$district)
write.csv(dea4A,"D:/Project/EE/dea/dea70/aprA_dea.csv")
