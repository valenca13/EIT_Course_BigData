install.packages("streetscape")

# Extract Mapillary street view images

# Get a Mapillary token

# https://www.mapillary.com/developer/api-documentation?locale=pt_PT 

#Import library

library(streetscape)

# Define location and get coordinates

# a) Open the Mapillary map
# b) Zoom/pan to the exact area you want
# c) Press:
        #F12 or
        #Ctrl+Shift+I (Windows/Linux) or
        #Cmd+Option+I (Mac)


# Bounding box of location to extract images
bbox <- c(-83.751812,42.272984,-83.741255,42.279716)

# Import data

data2 <- strview_search_osm(bbox = bbox, epsg = 2253, token = "")


data <- strview_searchByGeo(bbox = bbox, epsg = 2253, token = "")

streetviewdata <- streetscape::scdataframe
# calculate the percentage of each segmentation
data$decodeDetection()
data$data$segmentation[[1]]
# extract the semantic segmentation of a street view
mask <- streetviewdata$get_mask(1)

map1 <- data$mapPreview('meta')
print(map1)
# assume that one has run data$gvi() and data$decodeDetection()
map2 <- data$mapPreview('seg')
print(map2)
map3 <- data$mapPreview('gvi')
print(map3)
