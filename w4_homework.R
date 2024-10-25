library(dplyr)
library(sf)
library(countrycode)
library(readr)

# read gender inequality csv file
gender_data <- read_csv("HDR23-24_Composite_indices_complete_time_series.csv")

# read country file
world_shapefile <- st_read("World_Countries_(Generalized)_9029012925078512962.geojson")

# change to ISO3 in world_shapefile
world_shapefile <- world_shapefile %>%
  mutate(ISO3 = countrycode(ISO, "iso2c", "iso3c"))

# Extract the required gender inequality data
real_gender_data <- gender_data[, c("iso3","country","gii_2010","gii_2019")]
print(real_gender_data)

# Use na. imit() to remove rows containing NA
real_gender_data2 <- na.omit(real_gender_data)
print(real_gender_data2)

# Calculate the difference in gender inequality index between 2010 and 2019
real_gender_data2 <- real_gender_data2 %>%
  mutate(inequality_diff = gii_2019 - gii_2010)

# Merge using country iso3
world_data <- world_shapefile %>%
  left_join(real_gender_data2, by = c("ISO3" = "iso3"))

# visualization
library(ggplot2)

ggplot(data = world_data) +
  geom_sf(aes(fill = inequality_diff)) +
  scale_fill_viridis_c() +
  theme_minimal() +
  labs(title = "Gender inequality changes between 2010 and 2019")