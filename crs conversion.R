# Load required libraries
library(sf)
library(rgdal)
library(tidyverse)
library(readxl)

DA_level_demography <- read_excel("C:/Users/ksahu/Desktop/Kirti_Official/Census_CA/DA_level_demography.xlsx")

# Read the shapefile into an sf object
shp <- st_read("C:/Users/ksahu/Desktop/Kirti_Official/lda_000b21a_e/lda_000b21a_e.shp")

shp1 <- st_read('C:/Users/ksahu/Desktop/Kirti_Official/OneDrive_1_7-19-2023/Shelburne County Evacuation Area.shp')
sf_use_s2(FALSE)

# Write the transformed shapefile to a new file
# Replace "output_shapefile.shp" with the desired output filename
shp_transformed<-st_transform(shp,(st_crs(shp1)))
#output_shapefile <- "lda_wgs84.shp"
#st_write(shp_transformed, output_shapefile,append=FALSE)
x<-st_intersection(shp_transformed,shp1)
totallandarea<-sum(x$LANDAREA)
x$share<-x$LANDAREA/totallandarea*100
x$DAUID<-as.double(x$DAUID)
x2 <- inner_join(x, DA_level_demography, by = c("DAUID" = "GEO"))
x2$DAUID<-as.factor(x2$DAUID)

x2$DAUID <- factor(x2$DAUID, levels = x2$DAUID[order(-x2$share)])
ggplot(x2, aes(x = DAUID, y = share)) +
  geom_bar(stat = "identity") +
  labs(title = "Share by DAUID", x = "DAUID", y = "Share (%)") +
  theme_bw()


# Specify the current CRS of the shapefile
# Replace "NAD83_Statistics_Canada_Lambert" with the actual name of the current CRS
current_crs <- "+proj=lcc +lat_1=49 +lat_2=77 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

# Define the target CRS (WGS 84)
target_crs <- "+proj=longlat +datum=WGS84 +no_defs"

# Transform the shapefile to the new CRS
shp_transformed <- st_transform(shp, crs = target_crs)
ggplot() +
  geom_sf(data = x) +
  theme_bw()
