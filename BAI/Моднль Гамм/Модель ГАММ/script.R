#set directory
setwd("C:/Users/Kristina/Desktop/DEP")
#setwd("C:/Users/Usuario/OneDrive - Universidad de Valladolid/Investigaci?n/Articulosenmarcha/AArzacRusia/RGrowth")
dir()
setwd("C:/Users/Kristina/Desktop/DEP")

#call file
north <- read.table("north.txt", sep="\t", header=T)

#create groups by speices
#northPine <- north[which(north$SP=="PISY"),]
#northlasi <- north[which(north$SP=="LASI"),]
northlaca <- north[which(north$SP=="LACA"),]

#install/upload libreries
library(nlme)
library(mgcv)


#exclude year as factor
#northPine$YEAR<-as.numeric(as.character(northPine$YEAR))
#northlasi$YEAR<-as.numeric(as.character(northlasi$YEAR))
northlaca$YEAR<-as.numeric(as.character(northlaca$YEAR))

#GAMM model
modlacaGamm <- gamm(RW ~ SITE+YEAR+s(AGE), random = list(ID = ~ AGE),correlation = corAR1(form =~ YEAR), method="REML",  data=northlaca)

summary(modlacaGamm$gam)
summary(modlacaGamm$lme)

plot.gam(modlacaGamm$gam)

