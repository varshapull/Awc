test.B <- test2013[-train_80,]

str(test.B)
test.B_attri<-data.frame(livebirths=test.B$livebirths,SNPreg=test.B$SNPregReceived,HCPreg=test.B$HC.Pregnant,
                         pcode = test.B$pcode,date = test.B$date,name=test.B$name.of.project,district=test.B$district) ##filtering out variables 
#which correspond to mortality and creating a data frame.

describe(required_attri)

str(required_attri)
test.B_attri$date <- as.Date(test.B_attri$date, "%d-%m-%Y")
test.B_attri$date <- as.Date(test.B_attri$date, "%Y-%m-%d")


#######################################################################
# FILTERING OUT BY MONTH
jan_test.B<-test.B_attri[test.B_attri$date=="2013-01-01",]
f_test.B<-test.B_attri[test.B_attri$date=="2013-02-01",]
mr_test.B<-test.B_attri[test.B_attri$date=="2013-03-01",]
a_test.B<-test.B_attri[test.B_attri$date=="2013-04-01",]
m_test.B<-test.B_attri[test.B_attri$date=="2013-05-01",]
j_test.B<-test.B_attri[test.B_attri$date=="2013-06-01",]
jl_test.B<-test.B_attri[test.B_attri$date=="2013-07-01",]
aug_test.B<-test.B_attri[test.B_attri$date=="2013-08-01",]
s_test.B<-test.B_attri[test.B_attri$date=="2013-09-01",]
nov_test.B<-test.B_attri[test.B_attri$date=="2013-11-01",]
dec_test.B<-test.B_attri[test.B_attri$date=="2013-12-01",]


tjanB<-matrix(c(jan_test.B$livebirths,jan_test.B$SNPreg,jan_test.B$HCPreg),ncol=3)
tfebB<-matrix(c(f_test.B$livebirths,f_test.B$SNPreg,f_test.B$HCPreg),ncol=3)
tmarB<-matrix(c(mr_test.B$livebirths,mr_test.B$SNPreg,mr_test.B$HCPreg),ncol=3)
taprilB<-matrix(c(a_test.B$livebirths,a_test.B$SNPreg,a_test.B$HCPreg),ncol=3)
tmayB<-matrix(c(m_test.B$livebirths,m_test.B$SNPreg,m_test.B$HCPreg),ncol=3)
tjuneB<-matrix(c(j_test.B$livebirths,j_test.B$SNPreg,j_test.B$HCPreg),ncol=3)
tjulyB<-matrix(c(jl_test.B$livebirths,jl_test.B$SNPreg,jl_test.B$HCPreg),ncol=3)
taugB<-matrix(c(aug_test.B$livebirths,aug_test.B$SNPreg,aug_test.B$HCPreg),ncol=3)
tsepB<-matrix(c(s_test.B$livebirths,s_test.B$SNPreg,s_test.B$HCPreg),ncol=3)
tnovB<-matrix(c(nov_test.B$livebirths,nov_test.B$SNPreg,nov_test.B$HCPreg),ncol=3)
tdecB<-matrix(c(dec_test.B$livebirths,dec_test.B$SNPreg,dec_test.B$HCPreg),ncol=3)

# performing dea

tdea1B <- dea(tjanB,noutput=1,orientation=1,rts=2) ##january
View(dea1)
tdea1B<-cbind(tdea1B,jan_test.B$name,jan_test.B$district)

write.csv(x=tdea1B,file="D:/Project/EE/dea/dea80/test_dea_jan.csv")

tdea2B<- dea(tfebB,noutput=1,orientation=1,rts=2)  ##febuary
View(tdea2B)

tdea2B<-cbind(tdea2B,f_test.B$name,f_test.B$district)

write.csv(x=tdea2B,file="D:/Project/EE/dea/dea80/test_dea_feb.csv")

tdea3B<- dea(tmarB,noutput=1,orientation=1,rts=2) ##march
View(tdea3B)

tdea3B<-cbind(tdea3B,mr_test.B$name,mr_test.B$district)
write.csv(x=tdea3B,file="D:/Project/EE/dea/dea80/test_dea_mar.csv")

tdea4B<- dea(taprilB,noutput=1,orientation=1,rts=2) ##april
View(tdea4B)

tdea4B<-cbind(tdea4B,a_test.B$name,a_test.B$district)
write.csv(x=tdea4B,file="D:/Project/EE/dea/dea80/test_dea_april.csv")

tdea5B <- dea(tmayB,noutput=1,orientation=1,rts=2) ##may
View(tdea5B)

tdea5B<-cbind(tdea5B,m_test.B$name,m_test.B$district)
write.csv(x=tdea5B,file="D:/Project/EE/dea/dea80/test_dea_may.csv")

tdea6B<-  dea(tjuneB,noutput=1,orientation=1,rts=2) ##june
View(tdea6B)

tdea6B<-cbind(tdea6B,j_test.B$name,j_test.B$district)
write.csv(x=tdea6B,file="D:/Project/EE/dea/dea80/test_dea_june.csv")

tdea7B<- dea(tjulyB,noutput=1,orientation=1,rts=2) ##july
View(tdea7B)

tdea7B<-cbind(tdea7B,jl_test.B$name,jl_test.B$district)
write.csv(x=tdea7B,file="D:/Project/EE/dea/dea80/test_dea_july.csv")

tdea8B <-  dea(taugB,noutput=1,orientation=1,rts=2) ##august

tdea8B<-cbind(tdea8B,aug_test.B$name,aug_test.B$district)
write.csv(x=tdea8B,file="D:/Project/EE/dea/dea80/test_dea_aug.csv")

tdea9B <-  dea(tsepB,noutput=1,orientation=1,rts=2)##september

tdea9B<-cbind(tdea9B,s_test.B$name,s_test.B$district)
write.csv(x=tdea9B,file="D:/Project/EE/dea/dea80/test_dea_sept.csv")
View(tdea9B)

tdea11B<-  dea(tnovB,noutput=1,orientation=1,rts=2) ##november

tdea11B<-cbind(tdea11B,nov_test.B$name,nov_test.B$district)
write.csv(x=tdea11B,file="D:/Project/EE/dea/dea80/test_dea_nov.csv")
View(tdea11B)

tdea12B<-  dea(tdecB,noutput=1,orientation=1,rts=2) ##december

tdea12B<-cbind(tdea12B,dec_test.B$name,dec_test.B$district)
write.csv(x=tdea12B,file="D:/Project/EE/dea/dea80/test_dea_dec.csv")
