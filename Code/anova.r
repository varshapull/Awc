GenderDis <- read.csv("D:/Data_science/Statistics/GenderDis.csv", stringsAsFactors=FALSE)
  View(GenderDis)
  
str(GenderDis)
summary(GenderDis)

GenderDis$experience_levels<-cut(GenderDis$Experience,c(0,5,10,15,20,25,30),labels = c(1:6))
str(GenderDis)

anova(lm(Salary~Gender*experience_levels,GenderDis))

