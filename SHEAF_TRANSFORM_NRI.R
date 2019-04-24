#ADD NRI



nri_tfact <- read.csv("https://nextcloud.sesync.org/index.php/s/ESranGDWaMcyDNj/download", strip.white=TRUE)
nri_tfact$Year <- c("2015")

nri_prime <- read.csv("https://nextcloud.sesync.org/index.php/s/YQCjJzwztpSfwpe/download", strip.white=TRUE)
nri_lcc <- read.csv("https://nextcloud.sesync.org/index.php/s/RGb2eKkZtLpQ7X9/download", strip.white=TRUE)
nri_irr <- read.csv("https://nextcloud.sesync.org/index.php/s/8EwQkxxsXa6XaRb/download", strip.white=TRUE)
nri_eros <- read.csv("https://nextcloud.sesync.org/index.php/s/R8aASsxtMbiebYr/download", strip.white=TRUE)
nri_dbl <- read.csv("https://nextcloud.sesync.org/index.php/s/tnge8GngoS2ozKg/download", strip.white=TRUE)
nri_crpcov <- read.csv("https://nextcloud.sesync.org/index.php/s/GKroT2c8kRmHBPX/download", strip.white=TRUE)
nri_brd <- read.csv("https://nextcloud.sesync.org/index.php/s/CedCm5X2PR6T37x/download", strip.white=TRUE)

nri_combined <-  Reduce(function(x,y) merge(x = x, y = y, by = c("State", "County", "Year", "Fips"), all = TRUE), 
                        list(nri_tfact, nri_prime, nri_lcc, nri_irr, nri_eros, nri_dbl, nri_crpcov, nri_brd))

nri <- subset(nri_combined, State == "Illinois" | State == "Indiana" | State == "Iowa" | State == "Michigan" | State == "Minnesota" | State == "Missouri" | State == "Ohio" | State == "Wisconsin" | State == "Nebraska"| State == "Kansas" 
              | State == "North Dakota" | State == "South Dakota")

#NRI transform - rangeland divided by total surface area
nri$Brd_Rangeland_SurfaceArea_Ratio <- nri$Brd_Rangeland_Estimate/nri$Brd_Total_Surface_Estimate
#NRI transform - total cropland dividied by developed land
nri$Brd_Crp_Developed_Ratio <- nri$Brd_Crp_Estimate/nri$Brd_Developed_Estimate
#NRI transform - total cropland divided by total surface area
nri$Brd_Crp_SurfaceArea_Ratio <- nri$Brd_Crp_Estimate/nri$Brd_Total_Surface_Estimate
#NRI transform - total pastureland divided by total surface area
nri$Brd_Pasture_SurfaceArea_Ratio <- nri$Brd_Pastureland_Estimate/nri$Brd_Total_Surface_Estimate

#NRI transform - total irrigated crop and pastureland divided by total ag land
#ISSUE:WHAT IS TOTAL AG LAND VARIABLE - ES
#nri$Irr_CrpPasture_AgLandTotal_Ratio <- nri$Irr_Total_irr_Cropland_Pastureland_Estimate/XXXXXX

#write.csv(nri, file = "/nfs/soilsesfeedback-data/transformed_data/NRI_county_combined.csv")


