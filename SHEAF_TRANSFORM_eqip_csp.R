library(rgdal)
library(leaflet)
library(maptools)
library(classInt)
library(leaflet)
library(dplyr)
library(RColorBrewer)
library(raster)
library(plyr)



options(scipen=999)


simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}


#EQIP---

#eqip load via url - best if you are NOT on SESYNC rstudio server
eqip_csp <- read.csv("https://files.sesync.org/index.php/s/DS95J2LaNzjBG9S/download")
eqip_cons_security <- read.csv("https://files.sesync.org/index.php/s/HiA7i59kcqAFDz6/download")
eqip_cons_steward <- read.csv("https://files.sesync.org/index.php/s/QEATC8rFWzyLaT7/download")


eqip_csp <- cbind(eqip_csp[,1], eqip_csp[,4], eqip_csp[,6], eqip_csp[,7:10])
colnames(eqip_csp) <- c("Year", "State", "County", "EQIP_Contracts", "EQIP_CONTRACT_ACRES", "EQIP_FA_OBLIGATIONS", "EQIP_FA_PAYMENTS")

eqip_cons_security <- cbind(eqip_cons_security[,1], eqip_cons_security[,4], eqip_cons_security[,6], eqip_cons_security[,7:10])
colnames(eqip_cons_security) <- c("Year", "State", "County", "CStP_Contracts", "CStP_CONTRACT_ACRES", "CStP_FA_OBLIGATIONS", "CStP_FA_PAYMENTS")

eqip_cons_steward <- cbind(eqip_cons_steward[,1], eqip_cons_steward[,4], eqip_cons_steward[,6], eqip_cons_steward[,7:10])
colnames(eqip_cons_steward) <- c("Year", "State", "County", "CSP_Contracts", "CSP_CONTRACT_ACRES", "CSP_FA_OBLIGATIONS", "CSP_FA_PAYMENTS")


write.csv(eqip_csp, file = "/nfs/soilsesfeedback-data/transformed_data/eqip_csp.csv")
write.csv(eqip_cons_security, file = "/nfs/soilsesfeedback-data/transformed_data/eqip_cons_security.csv")
write.csv(eqip_cons_steward, file = "/nfs/soilsesfeedback-data/transformed_data/eqip_cons_steward.csv")




