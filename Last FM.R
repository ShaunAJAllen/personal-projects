# Exploratory Analysis of my LastFM data to recreate my Spotify Wrapped 
# (because my friends doubted that their Wrappeds were accurate)

# Data Wrangling 

# Reading the data
scrobbles <- read.csv('shaunajallen.csv')

# Checking the column names
col_names <- labels(scrobbles)[[2]] ; col_names

# The first observation has been treated as the column names,
# ignore this observation & rename the columns

# Loading tidyverse for data wrangling
library(tidyverse)

# Renaming the columns
scrobbles_2 <- rename(scrobbles,
                      Artist = col_names[1],
                      Album = col_names[2],
                      Song = col_names[3],
                      Timestamp = col_names[4])

# Noticed NAs for two songs so removing for simplicity 
complete_scr <- subset(scrobbles_2, 
                       Song != "After Last Night (with Thundercat & Bootsy Collins)" & Song != "Summer Rain")

# Getting the year, month and day of each scrobble
timestamp_split <- strsplit(complete_scr$Timestamp, 
                            split = " ")

years <- NULL

for (i in 1:length(timestamp_split)) {
  years[i] <- timestamp_split[[i]][3]
}


complete_scr$Year <- years

months <- NULL

for (i in 1:length(timestamp_split)) {
  months[i] <- timestamp_split[[i]][2]
}


complete_scr$Month <- months

days <- NULL

for (i in 1:length(timestamp_split)) {
  days[i] <- timestamp_split[[i]][1]
}

complete_scr$Day <- days

# Removing Nov scrobbles because Wrapped stops counting October 31st
scr_2021 <- complete_scr[complete_scr$Year == "2021" & complete_scr$Month != "Nov", ]


# Data Visualization 

# Getting all the artists scrobbled in 2021
artists <- unique(scr_2021$Artist)
length(artists) # 958 artists scrobbled

# Creating data frame to store total number of scrobbles for each artist
artist_scrobbles <- data.frame(artist = artists, 
                               scrobbles = rep(0, length(artists)))

for(i in 1:nrow(artist_scrobbles)) {
  
  # Add total number of scrobbles for each artist into data frame's second column
  artist_scrobbles[i, 2] <- nrow(filter(scr_2021, Artist == artists[i]))
}

artist_scrobbles_2 <- arrange(artist_scrobbles, desc(scrobbles))

# Top 5 artists of 2021
top_5_artists <- artist_scrobbles_2[1:5, ]

# Loading ggplot2 for better plots
library(ggplot2)

# Bar chart for top 5 artists of 2021
ggplot(data = top_5_artists, 
       mapping = aes(x = reorder(artist, -scrobbles), 
                     y = scrobbles)) + 
  geom_bar(stat = "identity") +
  xlab("Artist") +
  ylab("Total no. scrobbles") +
  labs(title = "Top 5 Artists by Total No. Scrobbles")


# Getting all the songs scrobbled in 2021
songs <- unique(scr_2021$Song)
length(songs) # 4085 songs scrobbled

# Selecting both song and artist because songs 
# by different artists can have the same name
song_artist <- select(scr_2021,
                      Song,
                      Artist)
# Getting unique pairs
unique_song_artist <- unique(song_artist)


# Creating data frame to store total number of scrobbles for each song/artist pair
song_scrobbles <- data.frame(song = unique_song_artist$Song, 
                             artist = unique_song_artist$Artist,
                             scrobbles = rep(0, length(unique_song_artist$Song)))

for(i in 1:nrow(song_scrobbles)) {
  
  # Adding total number of scrobbles for each song into data frame's third column
  song_scrobbles[i, 3] <- nrow(filter(scr_2021, Song == unique_song_artist$Song[i] & Artist == unique_song_artist$Artist[i]))
}

# Ordering songs by scrobbles
song_scrobbles_2 <- arrange(song_scrobbles, desc(scrobbles))

# Top 5 Songs of 2021
top_5_songs <- song_scrobbles_2[1:5, ]

# Bar chart for top 5 songs of 2021
ggplot(data = top_5_songs, mapping = aes(x = reorder(song, -scrobbles), y = scrobbles)) + 
  geom_bar(stat = "identity") +
  xlab("Song") +
  ylab("Total no. scrobbles") +
  labs(title = "Top 5 Songs by Total No. Scrobbles")


# Regression

# Response: Scrobbles/Day
# Predictor: Month

# Need to get scrobbles for every day

scr_2021

# Want to count nrow for unique day and month pairs


day_month <- select(scr_2021,
                      Day,
                      Month)
unique_day_month <- unique(day_month)
unique_day_month
length(unique_day_month$Day)

# create data frame to store total number of scrobbles for each day
day_scrobbles <- data.frame(day = unique_day_month$Day, 
                             month = unique_day_month$Month,
                             scrobbles = rep(0, length(unique_day_month$Day)))

for(i in 1:nrow(day_scrobbles)) {
  
  # add total number of scrobbles for each artist into data frame's second column
  day_scrobbles[i, 3] <- nrow(filter(scr_2021, Day == unique_day_month$Day[i] & Month == unique_day_month$Month[i]))
}

# Fitting a model with month and day as predictors
lm <- lm(scrobbles ~ as.factor(month) + day,
         data = day_scrobbles)

summary(lm)
anova(lm) # Neither month nor day are significant

# Fitting a model with only month as a predictor
lm_month <- lm(scrobbles ~ as.factor(month), 
               data = day_scrobbles)

summary(lm_month)
anova(lm_month) # Month still not significant

# Conclude: Month does not have a significant effect on my daily music listening

ggplot(day_scrobbles, aes(x = scrobbles)) + 
  geom_histogram(binwidth = 10) +
  xlab("Daily Scrobbles") +
  ylab("Frequency")

# Mode = 60-70 daily scrobbles


