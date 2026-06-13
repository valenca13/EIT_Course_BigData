 
#' ## About the Data
#' 
#' Hourly pedestrian counts from automatic sensors in our local area.
#' 
#' Data is openly available at: <https://data.cityofsydney.nsw.gov.au/datasets/cityofsydney::automatic-hourly-pedestrian-count>
#' 
#' This dataset contains **hourly pedestrian** counts by location in Sydney, Australia.
#' 
#' **Automated counters** were placed in busy areas of the CBD to assess performance.
#' 
#' Data collection began in *February 2020* and ended in *June 2025*.
#' 
#' ## Install packages
#' 
install.packages(c("tidyverse","skimr","df","DataExplorer", "mapview", "lubridate", "ggplot2"))
#' 
#' ## Import Dataset
#' 
#' The dataset can be downloaded in many formats.
#' 
#' You can also import the data in many formats. Choose the one you prefer
#' 
#' -   **CSV file**
#' 
## --------------------------------------------------------------
data_csv <- read.csv("https://github.com/valenca13/EIT_Course_BigData/releases/download/Datasets/Automatic_Hourly_Pedestrian_Count.csv")

#' ## Convert dataset into *dataframe*
#' 
#' It is good practice to: 
#' i) convert the database into dataframe, as many packages only work with dataframes. 
#' ii) To not work on the raw dataset.

library(tidyverse)
df <- data.frame(data_csv)

#' ## Check the dataset

#' -   **Structure of the dataset**
str(df)

#' -   **Summary of the dataset**

library(skimr)
skim(df)

#' ## Check Missing data

table(is.na(df))

#' -   **Plot missing data**

library(DataExplorer)

plot_missing(df)

#' -   **Listwise deletion**. Removes an entire row (case) if any variable in that row is missing. Only fully complete observations remain.

df_missingListwise = na.omit(df) #removes all rows with at least one NA in any variable

plot_missing(df_missingListwise)

#' -   **Pairwise deletion**. Delete the row of missing value to the presence in a the specific variable involved in a given calculation.

df_missingPairwise = df[!is.na(df$Previous52DayTimeAvg),] #removes all rows with NA in Previous52DayTimeAvg variable

plot_missing(df_missingPairwise)
#' For our case, which treatment would be better?
#' 
#' > **Note:** There are many methods for treating missing data, such as replacing the values by the mean, median or using more complex imputation methods (e.g. k‑Nearest Neighbors (kNN) Imputation).
#' 
#' ## Treat existent variables
#' 
#' -   **Convert the variable "Day" into a factor**

df$Day <- factor(df$Day, labels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"), ordered = TRUE)

str(df$Day)

#' **Check how many automatic counters there are:**

unique(df$Location_code)

unique(df$Location_Name)

#' The location exact location of the automatic counters is not given in the dataset.
#' 
#' We need at least an approximation, to perform spatial analysis.
#' 
#' Check the description in the dashboard: <https://experience.arcgis.com/experience/a0abf612051a4cc79918e6ce5ee7295b/>
#' 
#' Go to Google Maps and retrieve the locations:
#' 
#' -   A001: -33.863518, 151.209968
#' -   A002: -33.871667, 151.210080
#' -   A003: -33.870783, 151.206607
#' -   A004: -33.873041, 151.207565
#' 
#' ## Create variable with latitude and longitude of dataset
#' 
## --------------------------------------------------------------

df$Latitude[df$Location_code == "A001"] <- -33.863518
df$Longitude[df$Location_code == "A001"] <- 151.209968

df$Latitude[df$Location_code == "A002"] <- -33.871667
df$Longitude[df$Location_code == "A002"] <- 151.210080

df$Latitude[df$Location_code == "A003"] <- -33.870783
df$Longitude[df$Location_code == "A003"] <- 151.206607

df$Latitude[df$Location_code == "A004"] <- -33.873041
df$Longitude[df$Location_code == "A004"] <- 151.207565

#' ## Create a *Geometry* variable
library(sf)

df_sf <- st_as_sf(
  df,
  coords = c("Longitude", "Latitude"),
  crs = 4326
)

#' > **Note:** Google Maps uses *WGS84*, which corresponds to EPSG = 4326

#' **Create an iterative map**
#' 
library(mapview)

df_map <- unique(df_sf$geometry)
mapview(df_map)

#' ## Have date and time in separate variables
#' 
#' Parse date-times with **y**ear, **m**onth, and **d**ay, **h**our, **m**inute, and **s**econd components.

library(lubridate)

df_sf$Date <- ymd_hms(df_sf$Date, tz = "UTC")

#' Check the class of the variable ´Date´

class(df$Date)

class(df_sf$Date)

#' Create new variables separating Date, Year, and Time

df_sf$Date_only <- as.Date(df_sf$Date)
df_sf$Year <- format(df_sf$Date, "%Y")
df_sf$Time_only <- format(df_sf$Date, "%H:%M:%S")

#' ## Make some initial plots
#' 
#' -   **Total pedestrian counts over time**

library(ggplot2)

ggplot(df_sf, aes(x = Date, y = TotalCount)) +
  geom_line(linewidth = 1) +
    scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y"
  ) +
  labs(
    title = "Total Nº of Pedestrians Over Time",
    x = "Time",
    y = "Total Nº of Pedestrians"
  ) +
  theme_minimal()

#' -   **Total pedestrian counts per counter over time**

ggplot(df_sf, aes(x = Date, y = TotalCount, colour = Location_Name)) +
  geom_line() +
  facet_wrap(~ Location_code) + # insert "scales = "free_y" to adapt the maximum of Y in each plot
  theme_gray() +
  theme(axis.text.x = element_text(angle = 90, size = 6)) +
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y"
  ) +
  labs(
    title = "Total Nº of Pedestrians by Automatic Counter Over Time",
    x = "Time",
    y = "Total Nº of Pedestrians"
  )

#' *Facet_wrap* divides the plots per automatic counter.
#' 
#' -   **Tendency of total pedestrian counts per counter aggregated by year**

ggplot(df_sf, aes(x = Year, y = TotalCount, colour = Location_Name)) +
  geom_line(linewidth = 4) +
  facet_wrap(~ Location_code) + # insert "scales = "free_y" to adapt the maximum of Y in each plot
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, size = 7)) +
  scale_y_continuous(limits = c(0, max(df_sf$TotalCount) * 1.2)) +
  labs(
    x = "Year",
    y = "Total Nº of Pedestrians"
  )

#' -   **Total number of pedestrians per time**
#' 
ggplot(df_sf, aes(x = Time_only, y = TotalCount, colour = Location_Name)) +
  geom_line(linewidth = 1) +
  facet_wrap(~ Location_code, scales ="free_y") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, size = 5, hjust = 1)) +
  scale_y_continuous(limits = c(0, max(df_sf$TotalCount) * 1.2)) +
  labs(
    x = "Time",
    y = "Total Nº of Pedestrians"
  )

#' Try changing the *themes* for different appearances.

#' > **Note:** Find here the documentation and ´cheatsheet´ of the ggplot2 package: https://ggplot2.tidyverse.org/

 
#' Exercise 1: Try substituting the missing values of one variable to the mean value. 