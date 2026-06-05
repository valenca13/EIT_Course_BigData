
# Load necessary libraries
library(osmdata)
library(tidyverse)
# Define the area of interest (e.g., a bounding box or a specific location)
# sydney_bbox <- c(xmin = 150.0, ymin = -35.0, xmax = 151.0, ymax = -33.0)
# Alternatively, you can specify a location by name
location <- "Sydney, Australia"
# Create an OSM query for amenties data
amenities_query <- opq(location) %>%
  add_osm_feature(key = "amenity", value = c("restaurant", "cafe", "bar", "pub"))
# Fetch the data
amenities_data <- osmdata_sf(amenities_query)
# View the first few rows of the amenities data
head(amenities_data$osm_points)

#plot the amenities data and filter for specific amenities (e.g., restaurants)
library(ggplot2)
ggplot() +
  geom_sf(data = amenities_data$osm_points, aes(color = amenity)) +
  theme_minimal() +
  labs(title = "Amenities in Sydney", color = "Amenity Type") +
  theme(legend.position = "bottom")

# Load necessary libraries
library(osmdata)
library(sf)
library(dplyr)
# Define the area of interest (e.g., a bounding box or a specific location)
location <- "Sydney, Australia"
# Create an OSM query for road network data
road_query <- opq(location) %>%
  add_osm_feature(key = "highway")
# Fetch the road network data
road_data <- osmdata_sf(road_query)
# View the first few rows of the road data
head(road_data$osm_lines)
# perform centrality measures (e.g., degree centrality) on the road network
# For simplicity, let's calculate degree centrality based on the number of connections at each node
# Convert the road network to a graph object
library(igraph)
road_graph <- graph_from_data_frame(road_data$osm_lines, directed = FALSE)
degree_centrality <- degree(road_graph)
# betweenness centralitity 
betweenness_centrality <- betweenness(road_graph)
#closeness centrality
closeness_centrality <- closeness(road_graph)


# Convert pedestrian data to an sf object (assuming it has longitude and latitude columns)
pedestrian_sf <- st_as_sf(pedestrian_data, coords = c("longitude", "latitude"), crs = 4326)
# Create a grid over the area of interest
grid <- st_make_grid(st_as_sf(st_sfc(st_point(c(150.0, -35.0)), st_point(c(151.0, -33.0))), crs = 4326), n = c(10, 10))
# Count the number of pedestrians in each grid cell
pedestrian_counts <- st_join(pedestrian_sf, st_sf(grid)) %>%
  group_by(grid) %>%
  summarise(count = n())
# join the average of the centrality measures in each grid cell
grid_centrality <- st_join(st_sf(grid), st_as_sf(road_graph)) %>%
  group_by(grid) %>%
  summarise(degree_centrality = mean(degree_centrality),
            betweenness_centrality = mean(betweenness_centrality),
            closeness_centrality = mean(closeness_centrality))
#join the total number of amenities in each grid cell by type of amenity
grid_amenities <- st_join(st_sf(grid), st_as_sf(amenities_data$osm_points)) %>%
  group_by(grid, amenity) %>%
  summarise(count = n())

#visualize the relationship between pedestrian counts and centrality measures
ggplot() +
  geom_sf(data = grid_centrality, aes(fill = degree_centrality)) +
  geom_sf(data = pedestrian_counts, aes(size = count), color = "red", alpha = 0.5) +
  theme_minimal() +
  labs(title = "Pedestrian Counts and Degree Centrality in Sydney", fill = "Degree Centrality", size = "Pedestrian Count") +
  theme(legend.position = "bottom")
```


