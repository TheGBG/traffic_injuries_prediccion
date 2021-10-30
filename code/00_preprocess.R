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
  "9" = "Other intersection" 
  
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

traffic_regime_dict <- c(
  
 "-1" =  NA,
  "1" = "One way",
  "2" = "Bidirectional",
  "3" = "A separate carriageway",
  "4" = "With variable assignment channels"
  
)

reserved_lane_dict <- c(

  "-1" = NA,
  "0" = "Not applicable",
  "1" = "Cycle path",
  "2" = "Cycle lane",
  "3" = "Reserved lane"  
  
)

profile_dict <- c(
  
  "-1" =NA,
  "1" = "Flat",
  "2" = "Slope",
  "3" = "hilltop",
  "4" = "Bottom of coast"
  
)

layout_dict <- c(
  
  "-1" = NA,
  "1" = "rectilinear part",
  "2" = "In a curve to the left",
  "3" = "In a curve to the right",
  "4" = "In S"
  
)


surface_dict <- c(
  
  "-1" = NA,
  "1" = "Normal",
  "2" = "Wet",
  "3" = "Puddles",
  "4" = "Flooded",
  "5" = "Snowy",
  "6" = "Mud",
  "7" = "Icy",
  "8" = "Fat. oil",
  "9" = "Other"
  
)

infrastructure_dict <- c(
  
  "-1" = NA,
  "0" = "None",
  "1" = "Underground. tunnel",
  "2" = "Bridge. flyover",
  "3" = "Exchanger or connection sling",
  "4" = "Railroad",
  "5" = "Crossroads",
  "6" = "Pedestrian zone",
  "7" = "Toll zone",
  "8" = "Site",
  "9" = "Others"
  
)

situation_dict <- c(
  
  "-1" = NA,
  "0" = "None",
  "1" = "On the road",
  "2" = "On emergency lane",
  "3" = "On the shoulder",
  "4" = "On the sidewalk",
  "5" = "On a cycle path",
  "6" = "On other special track",
  "8" = "Others"
  
)

# Carefull with this one if we decide to one hot it
vehicle_category_dict <- c(
  
  "00" = "NA,",
  "01" = "Bicycle",
  "02" = "Moped &lt;50cm3",
  "03" = "Cart (Quadricycle with bodywork motor) (formerly cart or motor tricycle)",
  "04" = "Reference not used since 2006 (registered scooter)",
  "05" = "Reference unused since 2006 (motorcycle)",
  "06" = "Reference unused since 2006 (sidecar)",
  "07" = "VL only",
  "08" = "Reference unused since 2006 (VL + caravan)",
  "09" = "Reference not used since 2006 (light vehicles + trailer)",
  "10" = "VU only 1.5T &lt;= PTAC &lt;= 3.5T with or without trailer (formerly VU only 1.5T &lt;= PTAC &lt;= 3.5T)",
  "11" = "Reference not used since 2006 (VU (10) + caravan)",
  "12" = "Reference not used since 2006 (VU (10) + trailer)",
  "13" = "PL only 3.5T <PTCA <= 7,5T ",
  "14" = "PL only > 7.5T",
  "15" = "PL> 3,5T + trailer",
  "16" = "Road tractor only",
  "17" = "Road tractor + semi-trailer",
  "18" = "Reference not used since 2006 (public transport)",
  "19" = "Reference not used since 2006 (tram)",
  "20" = "Special gear",
  "21" = "Farm tractor",
  "30" = "Scooter <50 cm3",
  "31" = "Motorcycle> 50 cm3 and <= 125 cm3",
  "32" = "Scooter> 50 cm3 and <= 125 cm3",
  "33" = "Motorcycle> 125 cm3",
  "34" = "Scooter> 125 cm3",
  "35" = "Light quad <= 50 cm3 (Quadricycle without bodywork engine)",
  "36" = "Heavy quad> 50 cm3 (Quadricycle without bodywork engine)",
  "37" = "Bus",
  "38" = "Coach",
  "39" = "Train",
  "40" = "Tram",
  "41" = "3WD <= 50 cm3",
  "42" = "3WD> 50 cm3 <= 125 cm3",
  "43" = "3WD> 125 cm3",
  "50" = "EDP with motor",
  "60" = "EDP without motor",
  "80" = "VAE",
  "99" = "Other vehicle "
  
)


fixed_obstacle_struck_dict <- c(
  
  "-1" = NA,
  "0" = "Not applicable",
  "1" = "Parked vehicle",
  "2" = "Tree",
  "3" = "Metal slide",
  "4" = "Concrete slide",
  "5" = "Other slide",
  "6" = "Building, wall, bridge pier",
  "7" = "Vertical signage support or emergency call station",
  "8" = "Post",
  "9" = "Street furniture",
  "10" = "Parapet",
  "11" = "Island, refuge, upper terminal",
  "12" = "Sidewalk edge",
  "13" = "Ditch, embankment, rock face",
  "14" = "Other fixed obstacle on the road",
  "15" = "Other fixed obstacle on sidewalk or shoulder",
  "16" = "Clearance of the roadway without obstacle",
  "17" = "Nozzle. aqueduct head"
  
)

movable_obstacle_dict <- list(
  
  "-1" = NA,
  "0" = "None",
  "1" = "Pedestrian",
  "2" = "Vehicle",
  "4" = "Rail vehicle",
  "5" = "Domestic animal",
  "6" = "Wild animal",
  "9" = "Other "
)

reason_travel_dict <- c(
  
  "-1" = NA,
  "0" = "Not specified",
  "1" = "Home. work",
  "2" = "Home. school",
  "3" = "Shopping. shopping",
  "4" = "Professional use",
  "5" = "Walk. leisure",
  "9" = "Other"

  )

safety_dict <- c(
  
  "-1" = NA,
  "0" = "No equipment",
  "1" = "Belt",
  "2" = "Helmet",
  "3" = "Children's device",
	"4" = "reflective vest",
	"5" = "Airbag (2WD / 3WD)",
	"6" = "Gloves (2WD / 3WD)",
	"7" = "Gloves + Airbag (2WD / 3WD)",
	"8" = "Not determinable",
	"9" = "Other"
  
)


user_category_dict <- c(
  
  "1" = "Driver",
  "2" = "Passenger",
  "3" = "Pedestrian" 
  
)


# Columns to delete

raw_data <- raw_data %>% 
  select(
    !contains("id"),
    -c(
      municipal_code,
      postal_adress,
      route_number, 
      index_route_number, 
      alphanum_index_route_number,
      upstream_terminal,
      distances_to_UT,
      flow_direction,
      engine_type,
      occupants_public_transport,
      pedestrian_location,
      pedestrian_action,
      alone_pedestrian,
      saefty_use_2,
      saefty_use_3,
      initial_shock_point,
      place_in_vehicle,
      reserved_lane,
      infrastructure)
    )


# Decode it
raw_data <- raw_data %>% 
  mutate(
    
    severity = severity_dict[severity],
    sex = sex_dict[sex],
    light_conditions = light_cond_dict[light_conditions],
    location = location_dict[location],
    intersection = intersection_dict[intersection],
    #atmospheric_conditions = atm_dict[atmospheric_conditions],
    #collision_type = collision_dict[collision_type],
    road_category = road_category_dict[road_category],
    traffic_regime = traffic_regime_dict[traffic_regime],

    road_profile = profile_dict[road_profile],
    layout = layout_dict[layout],
    surface_condition = surface_dict[surface_condition],
    user_category = user_category_dict[user_category],
    #saefty_use_1 = safety_dict[saefty_use_1],
    #fixed_obstacle_struck = fixed_obstacle_struck_dict[fixed_obstacle_struck],
    #movable_obstacle_struck = movable_obstacle_dict[movable_obstacle_struck]
    
    )


# Apparently R is not happy about us using the -1 key inside vectors :/
raw_data %>% 
  is.na() %>% 
  colSums() / nrow(raw_data) * 100

# Drop columns with over 80% NA
raw_data <- raw_data %>% 
  select(
    -c(
      road_profile,
      layout,
      surface_condition
    ))


# Save it
write.csv(raw_data, "data/raw/decoded_data.csv", row.names = FALSE)


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
