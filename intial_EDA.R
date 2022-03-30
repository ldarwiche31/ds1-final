# INITIAL EDA ####

# NOTE: FOCUSED A LOT MORE ON DATA CLEANING AND WRANGLING THAN EDA

# load packages
library(tidyverse)
library(lubridate)


# load data
leilas_music <- read_rds("data/processed/leilas_music.rds")
spotify_subset <- read_rds("data/processed/spotify_subset.rds")

# Let us get some quick summary stats!! I want to get the average for the columns in the dataset
skimr::skim(leilas_music, where(is.numeric))
skimr::skim(spotify_subset, where(is.numeric))

# There are a lot of songs that have multiple versions of themselves which may show up as unique...
# I wonder how many diff versions of the same song show up in my playlists
multiple_versions <- leilas_music %>%
  group_by(artists_name_main, name) %>%
  tally() %>%
  arrange(desc(n))

# while this is interesting I think I should probably exclude such repeats...
# I will do this by choosing the song most popular version of the song
leilas_music <- leilas_music %>%
  group_by(name, artists_name_main) %>%
  mutate(rank = row_number(desc(popularity))) %>%
  filter(rank == 1) %>%
  select(-rank) %>%
  ungroup() %>%
  mutate(popularity = case_when(popularity == 0 & artists_followers > 1000 ~ NA_integer_,
                                TRUE ~ popularity))

# I will do the same cleaning for spotify_subset
spotify_subset <- spotify_subset %>%
  group_by(name, artists_name_main) %>%
  mutate(rank = row_number(desc(popularity))) %>%
  filter(rank == 1) %>%
  select(-rank) %>%
  ungroup() %>%
  mutate(popularity = case_when(popularity == 0 & artists_followers > 1000 ~ NA_integer_,
                                TRUE ~ popularity))

# QUALITATIVE PART OF ANALYSIS ####

# Who are my top artists in terms of playlist presense?
top_artists <- leilas_music %>%
  group_by(artists_name_main) %>%
  tally() %>%
  arrange(desc(n))

# Having access to my previous years spotify wrappeds
# I know that the people I have a lot in spotify playlists
# (especially having removed any duplicate songs) speaks more about
# the artists who I play a wide variety of songs of and who have larger
# discographies. I know artists like Lorde and Phoebe Bridgers I listen
# to in large quantities but the two have relatively small discographies in
# comparison to artists like Taylor Swift or Panic at the Disco!

top_random_artists <- spotify_subset %>%
  group_by(artists_name_main) %>%
  tally() %>%
  arrange(desc(n))

# What about my top genres?
top_genres <- leilas_music %>%
  group_by(broad_genre) %>%
  tally() %>%
  arrange(desc(n))

ggplot(leilas_music, aes(y = broad_genre)) +
  geom_bar()

# How does this compare to the spotify subset?
top_random_genres <- spotify_subset %>%
  group_by(broad_genre) %>%
  tally() %>%
  arrange(desc(n))

# Explicit Comparison between the two

genre_comp <- leilas_music %>% mutate(sample = "Leila's Library") %>%
  dplyr::union_all(spotify_subset %>% mutate(sample = "Random Sample")) %>%
  group_by(sample, broad_genre) %>%
  summarize(count = n()) %>%
  ungroup(broad_genre) %>%
  mutate(percentage = count/sum(count))

# Since it was a random dataset, it somewhat makes sense that it wouldn't really have
# a strong genre presence. While I want to compare my dataset to the patterns of this baseline
# dataset I know that its not necessarily a picture of what the average person listens to but rather
# a picture of what the entire spotify catalog contains.

# Now let us look at how many songs are explicit or not: it appears most are not

ggplot(leilas_music, aes(x = explicit)) +
  geom_bar()

# What mode (minor = 0, major = 1) are my songs in?: most major... but not unsubstantial are minor

ggplot(leilas_music, aes(x = mode)) +
  geom_bar()

# What about how many are from albums vs singles: around a third are singles

ggplot(leilas_music, aes(x = album_type)) +
  geom_bar()

# QUANTITATIVE ####

# BASIC DISTRIBUTIONS

map(names(leilas_music %>% select(where(is.numeric), -mode, -key)),
    function(x)
      ggplot(leilas_music, aes_string(x)) +
      geom_histogram())

map(names(spotify_subset %>% select(where(is.numeric), -mode, -key)),
    function(x)
      ggplot(spotify_subset, aes_string(x)) +
      geom_histogram())

# Some really interesting insights here.
# I think for some of the distributions where mine varies a lot, I think it would be really interesting to look at density plots comparing the two.
      # particular: energy, valence, and acousticness

# CORRELATION; use complete observations for each individual pair

cor_matrix <- as_tibble(cbind(names = names(leilas_music %>% select(where(is.numeric))),
                                    cor(leilas_music %>% select(where(is.numeric)), use = "pairwise.complete.obs")))
# Let us look at what the top 3 correlation pairs are per variable

cor_matrix <- bind_cols(cor_matrix$names, map(cor_matrix %>% select(-names), as.numeric)) %>%
  rename(names = `...1`)

cor_top <- map(names(cor_matrix %>% select(where(is.numeric))), function(x) arrange(as_tibble(list("names" = cor_matrix$names, "value" = cor_matrix[[x]])), desc(abs(value))))
names(cor_top) <- names(cor_matrix %>% select(where(is.numeric)))

cor_matrix_base <- as_tibble(cbind(names = names(spotify_subset %>% select(where(is.numeric))),
                              cor(spotify_subset %>% select(where(is.numeric)), use = "pairwise.complete.obs")))

cor_matrix_base <- bind_cols(cor_matrix_base$names, map(cor_matrix_base %>% select(-names), as.numeric)) %>%
  rename(names = `...1`)

cor_top_base <- map(names(cor_matrix_base %>% select(where(is.numeric))), function(x) arrange(as_tibble(list("names" = cor_matrix_base$names, "value" = cor_matrix_base[[x]])), desc(abs(value))))
names(cor_top_base) <- names(cor_matrix_base %>% select(where(is.numeric)))

# Looking at this here are pairs with really high correlations
cor_top[["popularity"]][1:4,]

cor_top[["album_total_tracks"]][1:2,] # makes sense as longer albums will have higher track numbers be available

cor_top[["danceability"]][1:4,] # valence... happier is more danceable!!

cor_top[["energy"]][1:4,] # valence, loudness, accousticness very highly correlated (ESPECIALLY FOR SOMETHING LIKE THIS)

cor_top[["loudness"]][1:6,] # moderately correlated with a lot... predicative I assume of a lot then

cor_top[["acousticness"]][1:5,] # moderately correlated with a lot

# After looking, I will probably focus the most on these numeric variables:
# danceability, energy, loudness, accousticness, valence, instrumentalness

# select these and make a correlation plot

pairs(leilas_music %>% select(danceability, energy, loudness, acousticness, valence, instrumentalness))

# lets look at instrumentalness... I wonder what genre songs within this category tend to be in

ggplot(leilas_music, aes(x = instrumentalness, y = energy, color = broad_genre)) +
  geom_point(alpha = .5) 

# can kind of see a lot more instrumental and soundtrack songs (things that tend to be study music for me)

# loudness vs energy: danceability

ggplot(leilas_music, aes(x = instrumentalness, y = energy, color = loudness)) +
  geom_point(alpha = .5)

# loudness vs energy: valence 

ggplot(leilas_music,
       aes(x = energy, y = loudness, color = valence)) +
  geom_point()

# lots of strong relationships with loudness, energy and another variable

ggplot(leilas_music,
       aes(x = energy, y = loudness, color = broad_genre)) +
  geom_point() # hard to kind of really see too much pattern but more sound/instrumental things tended to be on one end

ggplot(leilas_music,
       aes(x = energy, y = loudness, color = explicit)) +
  geom_point() # thought this might be related but doesn't seem so

ggplot(leilas_music,
       aes(x = energy, y = loudness, color = mode)) +
  geom_point() # thought this might be related but doesn't seem so

# TIME

  # release_date
    dates <- leilas_music %>%
      mutate(month = month(ymd(leilas_music$album_release_date)),
             year = year(ymd(leilas_music$album_release_date))) %>%
      select(id, broad_genre, popularity, month, year, album_release_date)

    dates %>%
      group_by(month) %>%
      tally()
    
    dates %>%
      group_by(year) %>%
      tally()
    
    dates %>%
      mutate(dow = wday(ymd(leilas_music$album_release_date))) %>%
      group_by(dow) %>%
      tally()
      # a lot of music is released on a Friday so it makes sense that 6 is the most popular by far but interesting that
      # interesting that Tuesday was the second most popular release date
    
# POPULARITY
    leilas_music %>%
      group_by(explicit) %>%
      summarize(average = median(popularity, na.rm = TRUE))
    spotify_subset %>%
      group_by(explicit) %>%
      summarize(average = median(popularity, na.rm = TRUE))
    
    leilas_music %>%
      group_by(album_type) %>%
      summarize(average = mean(popularity, na.rm = TRUE))
    spotify_subset %>%
      group_by(album_type) %>%
      summarize(average = mean(popularity, na.rm = TRUE))
    
    leilas_music  %>%
      group_by(broad_genre) %>%
      summarize(average = median(popularity, na.rm = TRUE),
                count = n())
    spotify_subset  %>%
      group_by(broad_genre) %>%
      summarize(average = median(popularity, na.rm = TRUE),
                count = n())

