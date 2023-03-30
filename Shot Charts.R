
# Call all necessary packages from Library and Functions for producing graphics
source("~/Desktop/Stat 345/Projects/Midterm Project/LibraryCalls.R")
library(nbastatR)

# Set the System Environment to handle data sizes
Sys.setenv("VROOM_CONNECTION_SIZE" = 131072*100)

## Single Season Graphic:

# Function to take a tibble and returns a new ggplot with the NBA court drawn in on the graph
# Inputs: tibble as variable 'data'
# Outputs: ggplot with NBA court drawn in
NBAcourtWrite <- function(data = tibble()) {
  court <- data %>% ggplot() +
    geom_segment(aes(22, -4, xend=22, yend=10)) +
    geom_segment(aes(-25, -4, xend=25, yend=-4)) + 
    geom_segment(aes(-22, -4, xend=-22, yend=10)) + 
    geom_segment(aes(-6, -4, xend=-6, yend=15)) + 
    geom_segment(aes(6, -4, xend=6, yend=15)) + 
    geom_segment(aes(-25, -4, xend=-25, yend=43)) + 
    geom_segment(aes(25, -4, xend=25, yend=43)) +
    geom_segment(aes(-8, 15, xend=8, yend=15)) + 
    geom_segment(aes(-8, -4, xend=-8, yend=15)) + 
    geom_segment(aes(8, -4, xend=8, yend=15)) + 
    geom_segment(aes(-25, 43, xend=25, yend=43)) + 
    geom_circle(aes(x0=0, y0=15, r=6), inherit.aes = FALSE) + 
    geom_curve(aes(22, 10, xend=0, yend=24.65), curvature=0.3) +
    geom_curve(aes(0, 24.65, xend=-22, yend=10), curvature=0.3) +
    coord_fixed(ratio=1, xlim=c(-25, 25), ylim=c(-4, 43))
  
  return(court)
}

# Gather data for Phoenix Suns team shots for the 2020 regular season
suns.shots <- teams_shots(teams = "Phoenix Suns", seasons = 2020)

# Optional filtering of data to only include the variables necessary
suns.shots <- suns.shots %>% 
  select(c(yearSeason, locationX, locationY, isShotMade))

# Creating shot chart with data and new function 'NBACourtWrite' that writes in the NBA court on the same ggplot
# Shot Charts are binned in hexagonal bins so that there are 75 bins
shot.chart <- suns.shots %>% 
  NBAcourtWrite() +
  geom_hex(aes(locationX/10, locationY/10, fill=isShotMade), bins=75) + 
  scale_fill_discrete(labels=c("Miss", "Make"), type=c("purple", "orange")) + 
  labs(fill="", x="X Distance from the Hoop (ft)", y="Y Distance from the Hoop (ft)", title="Phoenix Suns 2019-2020 Regular Season Shot Chart")

## Multiple Season Graphic:

# Vector of all the years that shot data is wanted 
year <- c(2001:2020)

# Function to gather each year of data
# Inputs: a year
# Outputs: team shot data for Phoenix Suns in the given year (x)
year_function <- function(x) {
  teams_shots(teams = "Phoenix Suns", seasons = x)
}

# Apply the 'year_function' to each year in the 'year' vector
years <- lapply(year, year_function)

# Bind the lists together for the data for all the years
suns.shots.20yrs <- list_rbind(years)

# 20 years worth of shot charts for the Phoenix Suns, faceted by year, 'NBACourtWrite' function used to draw the court
# Shot charts are binned in hexagonal bins so that there are 75 bins on each chart, orange respresenting more shots being made in an area and purple meaning more shots are missed in an area
shot.chart.20yrs <- suns.shots.20yrs %>% 
  select(c(yearSeason, locationX, locationY, isShotMade)) %>%
  NBAcourtWrite() + 
  geom_hex(aes(locationX/10, locationY/10, fill=isShotMade), bins=75) +
  scale_fill_discrete(labels=c("Miss", "Make"), type=c("purple", "orange")) +
  labs(fill="", x="X Distance from the Hoop (ft)", y="Y Distance from the Hoop (ft)", title="Suns Shot Charts Over 20 Seasons") +
  facet_wrap(vars(yearSeason))

## First and Last 5 Years of Suns Shot Data for 10 Shot Charts showing most difference

# Desired years
first.last.5 <- c(2001, 2002, 2003, 2004, 2005, 2016, 2017, 2018, 2019, 2020) 

# Apply to the years
ten.years <- lapply(first.last.5, year_function)

# Bind together ten.years
suns.shots.10yrs <- list_rbind(ten.years)

# First and last 5 years of Shot Charts for Phoenix Suns. Showing the most notable changes
shot.chart.first.last.5yrs <- suns.shots.10yrs %>%
  select(c(yearSeason, locationX, locationY, isShotMade)) %>%
  NBAcourtWrite() + 
  geom_hex(aes(locationX/10, locationY/10, fill=isShotMade), bins=75) +
  scale_fill_discrete(labels=c("Miss", "Make"), type=c("purple", "orange")) +
  labs(fill="", x="X Distance from the Hoop (ft)", y="Y Distance from the Hoop (ft)", title="Phoenix Suns Shot Charts") +
  facet_wrap(vars(yearSeason))

## Extra Graphic

# Use the 2020 shot chart data for the Suns
shots2020 <- teams_shots(teams="Phoenix Suns", seasons=2020)

# Gather extra information from a different function call in 'nbastatR'
extraInfo <- teams_rosters(seasons=2020) 

# Filter so that only the Phoenix Suns roster is in the set
extraInfo <- extraInfo %>% filter(nameTeam=="Phoenix Suns")

# Setting the 'groupPosition' column values to fit into 3 categories: 'G', 'F', and 'C' meaning 'Guard", 'Forward', and 'Center', respectively
# This step cleans up the data so that the graphic doesn't have a lot of colors
extraInfo$groupPosition[which(extraInfo$groupPosition=="G-F")] <- "G"
extraInfo$groupPosition[which(extraInfo$groupPosition=="C-F")] <- "C"
extraInfo$groupPosition[which(extraInfo$groupPosition=="F-C" | extraInfo$groupPosition=="F-G")] <- "F"

# Join the 'extraInfo' with the shot chart data 'shots2020' by the 'namePlayer' column since they both share this
shots2020 <- shots2020 %>% left_join(extraInfo, by="namePlayer")

# A new graphic of the 2019-2020 Phoenix Suns Regular Season shot chart data, this time with the color representing the position of the players that took the shot and the shape (an 'x' for miss and a 'o' for make) representing whether the shot was made
chart2020 <- shots2020 %>% 
  NBAcourtWrite() + 
  geom_point(aes(locationX/10, locationY/10, shape=isShotMade, color=groupPosition, alpha=0.7), size=2) +
  scale_shape_manual(labels=c("Miss", "Make"), values = c(4, 1)) + 
  scale_color_discrete(labels=c("Center", "Forward", "Guard"), type=c("black", "purple", "orange")) +
  labs(color="Position", shape="", alpha="", size="", title="2019-2020 Regular Season Suns Shot Chart", x="X Distance from the Hoop", y="Y Distance from the Hoop")