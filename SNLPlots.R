library(dplyr)     
library(ggplot2)
library(ggridges) # Funky mountain range plots
library(viridis)  # User-friendly color scheme

# Load SNL data
snl.data <- read.csv("Data/SNLData.csv",header=T,stringsAsFactors=F) %>%
  mutate(AirDate = ymd(AirDate))


### Ratings Distribution by Season

# Extract median episode rating for each season
median.ratings <- snl.data %>% 
  group_by(Season) %>% 
  summarize(MedianRating = median(Rating))

# Merge medians to SNL data
snl.data <- snl.data %>%
  left_join(median.ratings, by="Season")

# Plot rating distributions
ggplot(data=snl.data, aes(x=Rating, y=as.factor(Season), fill=MedianRating)) +
  geom_density_ridges(alpha=0.7, rel_min_height=0.01) +
  scale_fill_viridis(name = "Median Rating") +
  scale_y_discrete(position="right") +
  theme_bw() +
  xlim(0,10) +
  xlab("Ratings Distribution") +
  ylab("Season") +
  annotate("text",x=5,y=20,col="red",label=paste0("@mathieubray ",lubridate::year(lubridate::today())),
           alpha=0.15,cex=15,fontface="bold",angle=30)

snl.data %>% filter(Season==41, Rating < 4.0) # One of the least popular episodes


# Caveat: Number of votes to form rating for episode

mean.votes <- snl.data.enhanced %>% 
  group_by(Season) %>% 
  summarize(MeanVotes = mean(Votes))

snl.data.enhanced <- snl.data.enhanced %>%
  left_join(mean.votes, by="Season")

ggplot(data=snl.data.enhanced, aes(x=Season, y=Votes, color = MeanVotes)) +
  scale_color_viridis(name = "Mean Number of Votes", direction=-1) +
  scale_x_continuous(breaks=1:42) +
  geom_point() +
  theme_bw() +
  coord_flip()


### Plot overall ratings distribution

ggplot(data=snl.data, aes(Rating)) +
  geom_histogram(stat="count",alpha=0.8,fill="blue") +
  theme_bw() +
  xlim(0,10) +
  ylim(0,50) +
  xlab("Ratings Distribution") +
  ylab("Number of Episodes") +
  annotate("text",x=5,y=25,col="red",label=paste0("@mathieubray ",lubridate::year(lubridate::today())),
           alpha=0.15,cex=15,fontface="bold",angle=30)

ggplot(data=snl.data, aes(x=Rating)) +
  geom_density(alpha=0.5, fill="blue") +
  theme_bw() +
  xlim(0,10) +
  xlab("Ratings Distribution") +
  ylab("Density") +
  annotate("text",x=5,y=0.25,col="red",label=paste0("@mathieubray ",lubridate::year(lubridate::today())),
           alpha=0.15,cex=15,fontface="bold",angle=30)


### Ratings based on number of weeks since last episode

# Add in the number of weeks since the last episode
snl.data.enhanced <- snl.data %>%
  mutate(PreviousAirDate = lag(AirDate), # Get airdate of previous episode
         WeeksSinceLastEpisode = as.period(AirDate - PreviousAirDate)@day / 7) %>% # Calculate number of weeks since last episode
  select(-PreviousAirDate)

snl.data.enhanced %>% 
  filter(Season==42) %>%
  select(SeasonEpNumber,Host,AirDate,WeeksSinceLastEpisode) %>%
  head(10)

# Standardize these weeks into categories
snl.data.enhanced %>%
  group_by(WeeksSinceLastEpisode) %>%
  count

snl.data.enhanced <- snl.data.enhanced %>%
  mutate(WeeksCat = case_when(.$WeeksSinceLastEpisode >= 4 ~ "4+",
                            is.na(WeeksSinceLastEpisode) ~ "4+",
                            TRUE ~ as.character(WeeksSinceLastEpisode))) # Truncate at 4+ weeks break
  
snl.data.enhanced %>%
  group_by(WeeksCat) %>%
  count

# Plot distributions based on number of weeks between episodes
ggplot(data=snl.data.enhanced, aes(x=Rating, fill=WeeksCat)) +
  geom_density(data = snl.data.enhanced %>% select(-WeeksCat), fill = "darkgrey") +
  geom_density(alpha = 0.5) +
  facet_wrap(~WeeksCat) +
  scale_fill_viridis(name="Weeks Since Last Episode",discrete=T) +
  theme_bw() +
  xlim(0,10) +
  ylim(0,0.62) +
  xlab("Ratings Distribution") +
  ylab("Density") +
  annotate("text",x=5,y=0.31,col="red",label=paste0("@mathieubray ",lubridate::year(lubridate::today())),
           alpha=0.15,cex=10,fontface="bold",angle=30)


# What if we just looked at episodes written with a break versus those without
snl.data.enhanced <- snl.data.enhanced %>%
  mutate(FreshEpisode = WeeksCat != "1") # Mark whether the episode is 'fresh' (at least 1 week break between episodes)

snl.data.enhanced %>%
  group_by(FreshEpisode) %>%
  count
  
ggplot(data=snl.data.enhanced, aes(x=Rating, fill=FreshEpisode)) +
  geom_density(alpha=0.5) +
  scale_fill_viridis(discrete=T, name = "", labels=c("No Break","Break")) +
  theme_bw() +
  xlim(0,10) +
  ylim(0,0.5) +
  xlab("Ratings Distribution") +
  ylab("Density") +
  annotate("text",x=5,y=0.25,col="red",label=paste0("@mathieubray ",lubridate::year(lubridate::today())),
           alpha=0.15,cex=15,fontface="bold",angle=30) +
  ggtitle("Was There At Least 1 Week Break Before the Episode?")

# What if we just looked at season premieres vs. non-season premieres?
snl.data.enhanced <- snl.data.enhanced %>%
  mutate(Premiere = SeasonEpNumber == 1)

snl.data.enhanced %>%
  group_by(Premiere) %>%
  count

ggplot(data=snl.data.enhanced, aes(x=Rating, fill=Premiere)) +
  geom_density(position="identity",alpha=0.5) +
  scale_fill_viridis(discrete=T, name = "", labels=c("Not Premiere","Premiere")) +
  theme_bw() +
  xlim(0,10) +
  ylim(0,0.5) +
  xlab("Ratings Distribution") +
  ylab("Density") +
  annotate("text",x=5,y=0.25,col="red",label=paste0("@mathieubray ",lubridate::year(lubridate::today())),
           alpha=0.15,cex=15,fontface="bold",angle=30)


### Ratings based on number of consecutive airdates

num.episodes <- length(snl.data.enhanced$FreshEpisode)

consecutive.weeks <- numeric(num.episodes)
counter <- 1

for (i in 1:num.episodes){
  
  if (snl.data.enhanced$FreshEpisode[i]){ # If the episode is fresh
    counter <- 1 # Reset counter
  } else {
    consecutive.weeks[i] <- counter # For episode i, counter represents number of episodes in consecutive weeks
    counter <- counter + 1 # Augment counter
  }
}

snl.data.enhanced$ConsecutiveWeeks <- consecutive.weeks

# Check counts and form groups of episodes basd on how many episodes have aired in a row
snl.data.enhanced %>%
  group_by(ConsecutiveWeeks) %>%
  count

snl.data.enhanced <- snl.data.enhanced %>%
  mutate(ConsecutiveCat = case_when(.$ConsecutiveWeeks >= 2 ~ "2-3",
                          is.na(ConsecutiveWeeks) ~ "2-3",
                          TRUE ~ as.character(ConsecutiveWeeks)))

ggplot(data=snl.data.enhanced, aes(x=Rating, fill=ConsecutiveCat)) +
  geom_density(data = snl.data.enhanced %>% select(-ConsecutiveCat), fill = "darkgrey") +
  geom_density(alpha=0.7) +
  facet_wrap(~ConsecutiveCat) +
  scale_fill_viridis(name = "Number of Consecutive Episodes Prior", discrete=T) +
  theme_bw() +
  xlim(0,10) +
  xlab("Ratings Distribution") +
  ylab("Density") +
  annotate("text",x=5,y=0.31,col="red",label=paste0("@mathieubray ",lubridate::year(lubridate::today())),
           alpha=0.15,cex=10,fontface="bold",angle=30) +
  theme(legend.position = "bottom")


