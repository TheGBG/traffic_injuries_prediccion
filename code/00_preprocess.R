# Script to take a first look into the data

# Libaries --------------------------------------------------------------------
library(dplyr)
library(ggplot2)


# Read and join ---------------------------------------------------------------
# This is what we have. Lets read them all and join them into a dataframe
filenames <- list.files("data/raw/", full.names = TRUE)
list_of_files <- lapply(filenames, read.csv)

# Set all accident number to character so we can join later the tables
list_of_files <- lapply(
  list_of_files, 
  function(x) x %>% mutate(Num_Acc = as.character(Num_Acc))
  )


# Unify in a single dataframe
raw_data <- list_of_files[[1]] %>% 
  full_join(list_of_files[[2]], by = "Num_Acc") %>% 
  full_join(list_of_files[[3]], by = "Num_Acc") %>% 
  full_join(list_of_files[[4]], by = "Num_Acc")

# Save it so we don't have to run this over and over again
dim(raw_data)  # 250k x 56

write.csv(raw_data, "data/raw/full_data.csv", row.names = FALSE)

# Preprocessing ---------------------------------------------------------------

# clean environment except from raw data 
rm(list = setdiff(ls(), "raw_data"))


# Resume from here, reading the full data
raw_data <- read.csv("data/raw/full_data.csv")

# Now the first thin we will do is rename all the colums so that they are
# meaningfull for us. For this,  we will use the metadata file


new_names <- c(
  "accident_id",  # probably useless now that we have everything joint
  "day",
  "month",
  "year",
  "hour",  # fraction of 24h, will need wrangling
  "light_conditions",
  
  # This two might not be too useful
  "departament_code",
  "municipal_code",
  
  "location",
  "intersection",
  
  # This two might be very predictive
  "atmospheric_conditions",
  "collision_type",
  
  "postal_adress",  # probably not
  "latitude",
  "longitude",
  "road_category",
  
  # This 3 are probably useless
  "route_number",
  "index_route_number",
  "alphanum_index_route_number",
  
  "traffic_regime",
  "number_of_lanes",
  "reserved_lane",
  "road_profile",
  "upstream_terminal",
  "distances_to_UT",
  "layout",  # if it is a turn, s... Might be useful
  
  "central_reservation_width",
  "road_width",
  "surface_condition",  # probably predictive
  "infrastructure",
  "situation",
  "max_speed",
  
  "vehicle_id_x",  # useless
  "another_vehicle_id_x",
  "place_in_vehicle",
  "user_category",
  "severity",
  "sex",
  
  "year_of_bith",  # this dataset is froam 2019 so we could get their age
  "reason_of_travel",
  
  # This 3 might be useful when used together
  "saefty_use_1",
  "saefty_use_2",
  "saefty_use_3",
  
  "pedestrian_location",
  "pedestrian_action",
  "alone_pedestrian",
  "vehicle_id_y",
  "another_vehicle_id_y",
  
  
  "flow_direction",
  "vehicle_category",
  "fixed_obstacle_struck",
  "movable_obstacle_struck",
  "initial_shock_point",
  "maneuver_before_accident",
  "engine_type",
  "occupants_public_transport"
  
  )


colnames(raw_data) <- new_names

# Drop all that conains ID

colnames(raw_data)

raw_data <- raw_data %>% 
  select(!contains("id"))


# Now make some features numeric as they should be
raw_data <- raw_data %>% 
  mutate(
    across(
      .cols = c(
        "departament_code", 
        "municipal_code", 
        "route_number", 
        "pedestrian_action"
        ),
      .fns = function(x) as.numeric(x)
      )
    )


# And now check data quality
raw_data %>% 
  is.na() %>% 
  colSums() / nrow(raw_data) * 100

# It's Hard to check the data quality because NA's are encoded as -1. In fact,
# everything in this dataset is encoded as numbers. Let's revert everything and 
# then check again

colnames(raw_data)
head(raw_data)

# Day month and year might be more helpfull if encoded as a same date. And the
# hour is created as a fraction of 24h, so we have to correct that too
raw_data <- raw_data %>% 
  mutate(
    date = lubridate::make_date(year = year,
                                day = day,
                                month = month),
    
    hour = round(hour * 24, 2)
    
    )


# This one might be useful
unique(raw_data$departament_code)

# This one its probably too granular, so we delete it
unique(raw_data$municipal_code)
raw_data <- raw_data %>% 
  select(-municipal_code)


# Now onto the decoding stuff. Let's start with the severity
severity_dict <- c(
  "1" = "unharmed",
  "2" = "killed",
  "3" = "injured_hospitalized",
  "4" = "slightly_injured"
  )

sex_dict <- c(
  "1" = "male",
  "2" = "female"
  )

# Some more dicts
colnames(raw_data)

light_cond_dict <- c(
  "1" = "Full day",
  "2" = "Twilight or dawn",
  "3" = "Night without public lighting",
  "4" = "Night with public lighting not on",
  "5" = "Night with public lighting on"
  )


location_dict <- c(
  "1" = "Outside agglomeration",
  "2" = "In built-up areas"
  )

intersection_dict <- c(
  "1" = "Excluding intersection",
  "2" = "Intersection in X",
  "3" = "T-intersection",
  "4" = "Y intersection",
  "5" = "Intersection with more than 4 branches",
  "6" = "Roundabout",
  "7" = "Place",
  "8" = "Level crossing",
  "9" = "Other intersection", 
  
)

atm_dict <- c(
  
 "-1" = NA,
  "1" = "Normal",
  "2" = "Light rain",
  "3" = "Heavy rain",
  "4" = "Snow. hail",
  "5" = "Fog. smoke",
  "6" = "Strong wind. storm",
  "7" = "Dazzling weather",
  "8" = "Cloudy weather",
  "9" = "Other"
  
)

collision_dict <- c(
 "-1" = NA,
  "1" = "Two vehicles. frontal",
  "2" = "Two vehicles. from the rear",
  "3" = "Two vehicles. from the side",
  "4" = "Three vehicles and more. in a chain",
  "5" = "Three or more vehicles. multiple collisions",
  "6" = "Other collision",
  "7" = "No collision"
)


road_category_dict <- c(
  "1" = "Highway",
  "2" = "National road",
  "3" = "Departmental road",
  "4" = "Communal roads",
  "5" = "Outside the public network",
  "6" = "Parking lot open to public traffic",
  "7" = "Urban metropolis roads",
  "9" = "other"
)


# Decode it
raw_data <- raw_data %>% 
  mutate(severity = severity_dict[severity],
         sex = sex_dict[sex]
         
         ) 



# EDA -------------------------------------------------------------------------
# TODO: create an EDA script and move this
raw_data %>% 
  na.omit() %>% 
  
  ggplot(aes(x = severity, fill = sex)) +
  geom_bar(position = "fill", color = "black") +
  coord_flip() +
  
  theme_bw() +
  
  labs(title = "Injuries severity - Sex comparision",
       subtitle = "Percentaje of injuries severity taken by each sex",
       x = "Percentaje",
       y = "Severity",
       fill = "Sex") +
  
  scale_fill_brewer(palette = "Set3")

# How is the sex in the dataset?
table(raw_data$sex) / nrow(raw_data)


# How is the severity distributed inside each sex?
raw_data %>% 
  na.omit() %>% 
  
  select(severity, sex) %>% 
  group_by(severity, sex) %>% 
  summarize(total = n()) %>%  # count the total obs in each severity-sex pair
  
  group_by(sex) %>%
  mutate(proportion = prop.table(total)) %>%  # gets the percentaje, for each
                                              # severity, twice (2 sexs)
  
  
  ggplot() +
  
  geom_col(
    aes(
      x = severity, 
      y = proportion, 
      fill = severity),
    
    color = "black") + 
  
  coord_flip() +
  
  facet_wrap(~sex, nrow = 2) +
  
  theme_bw() +
  theme(legend.position = "None") +
  
  labs(
    title = "Injuries severity - Sex comparision",
    subtitle = "Percentaje of injuries severity inside each sex",
    x = "Percentaje",
    y = "Severity"
    ) +
  
  scale_fill_brewer(palette = "Set3") +
  ylim(0, 1)
