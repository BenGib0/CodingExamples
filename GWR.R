## SOEE5020 Masters Research Project code
## Ben Gibbons

## Section 1: Packages and Data

# Install and load packages

library(sf)         
library(tidyverse)  
library(tmap)       
library(readxl)     
library(car)        
library(MASS)   
library(GWmodel)
library(readr)
library(hrbrthemes)

setwd("~/Desktop/SOEE5020 Research Project/Data")
library(readr)
BlankReg <- read_csv("BlankReg.csv")
View(BlankReg)
STData <- read_csv("STData.csv")
View(STData)

# Histogram
hist(BlankReg$Emissions)

# Histogram
hist(STData$Emissions)

m = lm(formula = Emissions~Northerlyness+PctEPCc+PctComheatscheme+
         PctElectricity+PctMainsgas+PDense+TotInc+PctWFH+PctAsian+
         PctBlack+PctWhite+PctdDep+PctHomeowner+
         PctSocialRent+PctPrivateRent+PctStudents+
         PctLongtermnowork, data = RegData) 
summary(m)

summary(m$residuals)
s.resids <- rstudent(m)
plot(s.resids,type='h')
abline(h=c(-2,2),col='lightskyblue')

qqPlot(m)

f = lm(formula = Emissions~Northerlyness+PctEPCc+PctComheatscheme+
         PctElectricity+PctMainsgas+PDense+TotInc+PctWFH+PctAsian+
         PctdDep+PctHomeowner+PctPrivateRent
         +PctStudents+PctLongtermnowork, data = BlankReg) 
summary(f)



# Calculate VIF for each predictor variable
vif <- vif(f)

# Print VIF values
print(vif)




n = lm(formula = Emissions~Northerlyness+PctEPCc+PctComheatscheme+
         PctElectricity+PctMainsgas+PDense+PctWFH+PctAsian+
         PctBlack+PctWhite+PctDep+PctHomeowner+
         PctSocialRent+PctPrivateRent+PctStudents+
         PctLongtermnowork, data = STData) 
summary(n)

# Calculate VIF for each predictor variable
vif <- vif(n)

# Print VIF values
print(vif)


ggplot(aes(x = PctAsian, y = Emissions), data = STData) +
  geom_point(color = "black") +  # Add data points
  geom_smooth(method = "lm", se = FALSE, col = "red") +  
  labs(title = "Emissions vs PctAsian", x = "PctAsian", y = "Emissions") +
  theme_grey()  # Adjust theme for better readability

## GWR


data.spdf <- st_as_sf(BlankReg, coords = c("LONG", "LAT"), crs = "OSGB 36")
str(data.spdf)
st_crs(data.spdf)
crs <- CRS("+init=epsg:27700")  # Replace with your actual EPSG code
data.spdf <- st_set_crs(data.spdf, crs)
st_crs(data.spdf)
# determine the kernel bandwidth
bw <- bw.gwr(formula = Emissions~Northerlyness+PctEPCc+PctComheatscheme+
               PctElectricity+PctMainsgas+PDense+PctWFH+PctAsian+
               PctBlack+PctWhite+PctDep+PctHomeowner+
               PctSocialRent+PctPrivateRent+PctStudents+
               PctLongtermnowork,
             approach = "AIC",
             adaptive = T,
             data=data.spdf)

BlankReg = st_transform(BlankReg, "ESRI:27700")



BlankReg_spdf <- st_as_sf(BlankReg, coords = c("LONG", "LAT"))
# convert to sp
BlankReg.sp = as(BlankReg_spdf, "Spatial")
# determine the kernel bandwidth
bw <- bw.gwr(Emissions~Northerlyness+PctEPCc+PctComheatscheme+
               PctElectricity+PctMainsgas+PDense+PctWFH+PctAsian+
               PctBlack+PctWhite+PctdDep+PctHomeowner+
               PctSocialRent+PctPrivateRent+PctStudents+
               PctLongtermnowork,
             approach = "AIC",
             adaptive = T,
             data=BlankReg.sp) 

gwr.mod <- gwr.basic(Emissions~Northerlyness+PctEPCc+PctComheatscheme+
                       PctElectricity+PctMainsgas+PDense+PctWFH+PctAsian+
                       PctBlack+PctWhite+PctdDep+PctHomeowner+
                       PctSocialRent+PctPrivateRent+PctStudents+
                       PctLongtermnowork, 
                     adaptive = T,
                     data = BlankReg.sp, 
                     bw = bw)
bw
gwr.mod
bw
# GWR outouts
names(gwr.mod)

tab.gwr <- rbind(apply(gwr.mod$SDF@data[, 1:23], 2, summary), coef(m))
rownames(tab.gwr)[23] <- "Global"
tab.gwr <- round(tab.gwr, 10)


t(tab.gwr)
write.csv(t(tab.gwr), file = "coef_tab.csv")

gwr_sf = st_as_sf(gwr.mod$SDF)
gwr_sf

gwr_sf = st_as_sf(gwr.mod$SDF)

msoa_polygons <- st_read("MSOAdata")

crs <- CRS("+init=epsg:27700")

gwr_sf <- st_set_crs(gwr_sf, crs)
msoa_polygons <- st_set_crs(msoa_polygons, crs)
summary(msoa_polygons)


library(sp)  # Assuming gwr_sf and msoa_polygons are sf objects
library(sfheaders)
# Specify coefficient names (adjust as needed)
coefficients <- c("Emissions", "Northerlyness", "PctEPCc", "PctComheatscheme", 
                  "PctElectricity", "PctMainsgas", "PDense", "PctWFH", 
                  "PctAsian", "PctBlack", "PctWhite", "PctdDep", "PctHomeowner", 
                  "PctSocialRent", "PctPrivateRent", "PctStudents", 
                  "PctLongtermnowork2")

# Perform nearest neighbor join
joined_data <- nn_join(gwr_sf[, coefficients], msoa_polygons, distance = 1000)

tm_shape(gwr_sf) +
  tm_fill(c("PctEPCc", "PctAsian"), palette = "viridis", style = "kmeans") +
  tm_layout(legend.position = c("right","top"), frame = F)




