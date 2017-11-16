library(dplyr)     # Lifeblood of data science
library(rvest)     # Scrape scrape scrape
library(purrr)     # 'map' function
library(zoo)       # 'locf' function for carrying last observation forward
library(lubridate) # Working with dates 
library(stringr)   # Working with strings


### Scrape SNL Episode Airdates

# Helper function for scraping SNL airdate tables from Wikipedia
extract.wiki.table <- function(url){
  
  raw.data <- url %>% 
    read_html %>% 
    html_nodes('.wikiepisodetable') %>% # Tables from wikipedia page are tagged with '.wikiepisodetable'
    map(html_table, header=F, fill=T) %>% # Convert scraped tables to data frames
    bind_rows # Bind the set of data frames into one large data frame
  
  return(raw.data)
}

# Gather and combine raw SNL airdate tables
raw.snl.1 <- extract.wiki.table("https://en.wikipedia.org/wiki/List_of_Saturday_Night_Live_episodes_(seasons_1%E2%80%9315)#Episodes")
raw.snl.2 <- extract.wiki.table("https://en.wikipedia.org/wiki/List_of_Saturday_Night_Live_episodes_(seasons_16%E2%80%9330)#Episodes")
raw.snl.3 <- extract.wiki.table("https://en.wikipedia.org/wiki/List_of_Saturday_Night_Live_episodes#Episodes") %>% select(1:5) # This table will have the number of viewers in an extra column

raw.snl.combined <- rbind(raw.snl.1,raw.snl.2,raw.snl.3) # Bind the three data frames together

head(raw.snl.combined)
#write.csv(raw.snl.combined,"Data/SNLRawEpisodes.csv",row.names=T)

# Helper function for extracting the correct airdate
extract.airdate <- function(airdate){
  
  clean.airdate <- airdate %>% 
    strsplit(split="(", fixed=T) %>% # Split off date string after '('
    unlist %>% # Convert to vector
    `[`(1) %>% # Extract first element
    trimws %>% # Remove whitespace from beginning and end
    mdy # Convert to simple date format
  
  return(clean.airdate)
}

# Clean SNL airdate data
snl.airdates <- raw.snl.combined %>% 
  rename(EpNumber = X1,
         SeasonEpNumber = X2,
         Host = X3,
         MusicalGuest = X4,
         AirDate = X5) %>% # Rename variables
  filter(AirDate != "Original air date") %>% # Remove rows not pertaining to actual episodes
  mutate(EpNumber = as.numeric(EpNumber),
         SeasonEpNumber = as.numeric(SeasonEpNumber)) %>% # Convert numeric columns
  rowwise %>%
  mutate(AirDate = extract.airdate(AirDate)) %>% # Convert airdate to R friendly format
  ungroup

# Extract first episodes from each season and assign a season number
first.episodes <- snl.airdates %>%
  filter(SeasonEpNumber == 1) %>%
  mutate(Season = 1:n()) %>%
  select(EpNumber, Season)

head(first.episodes)
#write.csv(first.episodes,"Data/SNLFirstEpisodes.csv",row.names=F)

# Merge season information to each episode in clean airdate data
snl.airdates.with.season <- snl.airdates %>%
  left_join(first.episodes, by="EpNumber") %>% # Bind season number based on episode number
  mutate(Season = na.locf(Season)) %>% # Carry season number forward to remaining episodes in season
  select(Season, SeasonEpNumber, Host, MusicalGuest, AirDate) %>% # Retain variables of interest
  filter(Season < 43) # Not going to consider the season currently airing

head(snl.airdates.with.season)
#write.csv(snl.airdates.with.season, "Data/SNLAirdates.csv", row.names=F)


### Scrape SNL Episode IMDb ratings

url <- "http://www.imdb.com/title/tt0072562/epdate" #IMDb URL

raw.ratings <- url %>% 
  read_html %>% 
  html_nodes('table') %>% # Ratings table is taggeed as 'table'
  html_table(header=F) %>% # Extract tables
  `[[`(1) %>% # Retain first table with ratings
  data.frame(stringsAsFactors=F) # Convert to data frame

head(raw.ratings)
#write.csv(raw.ratings,"Data/SNLRawRatings.csv",row.names=F)

# Helper function for extracting the season from the full episode number
extract.season <- function(x){
  
  token.list <- x %>%
    as.character %>%
    strsplit(split=".", fixed=T) %>% # Season number appears before the '.'
    unlist
  
  return(as.numeric(token.list[1]))  # Convert to numeric before returning
}

# Clean SNL ratings data
snl.ratings <- raw.ratings %>%
  tail(-1) %>% #  Remove first row
  select(1:4) %>% # Retain first 4 columns
  rename(EpisodeNumber = X1,
         Guests = X2,
         Rating = X3,
         Votes = X4) %>% # Rename variables
  rowwise %>%
  mutate(Season = extract.season(EpisodeNumber)) %>% # Get season number from episode number
  select(Season, Guests, Rating, Votes) %>% # Retain variables
  ungroup %>%
  filter(Season < 43) # Not going to consider episodes from the currently airing season

head(snl.ratings)
#write.csv(snl.ratings,"SNLRatings.csv",row.names=F)


### Check for mismatches between Airdates and Ratings datasets, then combine

#snl.airdates <- read.csv("Data/SNLAirdates.csv", header=T, stringsAsFactors=F)
#snl.ratings <- read.csv("Data/SNLRatings.csv", header=T, stringsAsFactors=F)

# Check if episode counts are the same
snl.airdates.season.counts <- snl.airdates %>%
  group_by(Season) %>%
  summarize(N1=n())

snl.ratings.season.counts <- snl.ratings %>%
  group_by(Season) %>%
  summarize(N2=n()) %>%
  arrange(Season)

snl.airdates.season.counts %>%
  left_join(snl.ratings.season.counts, by="Season") %>%
  mutate(SameCounts = N1==N2) %>%
  filter(!SameCounts) 

# Seasons 2 and 10 have an extra episode in the ratings data set, let's find them!
snl.airdates %>%
  filter(Season == 2) %>%
  .$Host

snl.ratings %>%
  filter(Season == 2) %>%
  .$Guests # Extra episode 'Live from Mardi Gras'

snl.airdates %>%
  filter(Season == 10) %>%
  .$Host

snl.airdates %>%
  filter(Season == 10, SeasonEpNumber == 1) %>%
  .$MusicalGuest

snl.ratings %>%
  filter(Season == 10) %>%
  .$Guests # Extra episode 'SNL Film Festival'

snl.ratings <- snl.ratings %>%
  filter(!(Guests %in% c("Live from Mardi Gras","SNL Film Festival")))

# Combine data frames
snl.data <- cbind(snl.airdates, snl.ratings %>% select(-Season)) %>%
  select(Season,SeasonEpNumber,Guests,Host,MusicalGuest,AirDate,Rating,Votes)

# Quick sanity check, which episodes don't match hosts between data sets?
# Does the 'Host' from the Airdate data appear in the 'Guest' from the Ratings data?
snl.data %>%
  mutate(MatchDetected = str_detect(Guests,fixed(Host))) %>% 
  filter(!MatchDetected) %>%
  select(Season,SeasonEpNumber,Guests,Host,MusicalGuest)

write.csv(snl.data, "Data/SNLData.csv", row.names=F)