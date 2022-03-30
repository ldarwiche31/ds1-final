# CREATING SUBSET FROM SPOTIFY DATASET####

library(spotifyr)
library(tidyverse)

Sys.setenv(SPOTIFY_CLIENT_ID = '1bff3ae8f2bf478cb5e5be684a4d4084')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'ed310531d5cf446cab74c55cc3947ef1')
access_token <- get_spotify_access_token()

spotify_dataset <- read_csv("data/unprocessed/spotify_dataset.csv")

set.seed(3)

spotify_sample <- sample(nrow(spotify_dataset), size = 5000)

spotify_subset <- spotify_dataset[spotify_sample, "id"]

# Rebuild as too mirror format of "leilas_music"

output <- lapply(spotify_subset$id, get_track)

# FLATTENING OUR DATA MANUALLY
    # extracting album nested list
      album <- lapply(output, "[[", "album")
      album_type <- unlist(lapply(album, "[[", "album_type"))
      album_id <- unlist(lapply(album, "[[", "id"))
      album_name <- unlist(lapply(album, "[[", "name"))
      album_id <- unlist(lapply(album, "[[", "id"))
      album_release_date <- unlist(lapply(album, "[[", "release_date"))
      album_total_tracks <- unlist(lapply(album, "[[", "total_tracks"))
      album_info <- as_tibble(list(album_type = album_type, album_id = album_id, album_name = album_name, album_release_date = album_release_date, album_total_tracks = album_total_tracks))
    
    # extracting album nested list
      artist <- lapply(output, "[[", "artists")
      artist <- bind_rows(artist, .id = "Name")
      names(artist) <- str_c("artists_", names(artist))
      artist <- artist %>%
        select(artists_Name, artists_name, artists_id) %>%
        group_by(artists_Name) %>%
        mutate(artist_rank = row_number()) %>%
        mutate(artist_rank = case_when(artist_rank == 1 ~ "main",
                                       TRUE ~ str_c("feature_", artist_rank - 1))
        ) %>%
        pivot_wider(names_from = artist_rank, values_from = c(artists_name, artists_id)) %>%
        ungroup()

    # removing what we did manually/don't need
      output_new <- lapply(output, function(x) within(x, rm(album)))
      output_new <- lapply(output_new, function(x) within(x, rm(artists)))
      output_new <- lapply(output_new, function(x) within(x, rm(external_ids)))
      output_new <- lapply(output_new, function(x) within(x, rm(external_urls)))
      output_new <- lapply(output_new, function(x) within(x, rm(href)))
      output_new <- lapply(output_new, function(x) within(x, rm(is_local)))
      output_new <- lapply(output_new, function(x) within(x, rm(preview_url)))
      output_new <- lapply(output_new, function(x) within(x, rm(uri)))
      output_new <- lapply(output_new, function(x) within(x, rm(disc_number)))
      output_new <- lapply(output_new, function(x) within(x, rm(type)))
      for (i in seq_along(output_new)) {
        output_new[[i]][["available_markets"]] <- length(output[[i]][["available_markets"]])
      }
      
      # combining existing info
      spotify_subset <- bind_rows(output_new)
      spotify_subset <- cbind(spotify_subset, album_info, artist)
      
      # add additional data sources
      
      audio_features <- lapply(spotify_subset$id, get_track_audio_features)
      artist_info <- lapply(unique(spotify_subset$artists_id_main), get_artist)
      bind_rows(audio_features)
      artists_id <- sapply(artist_info, "[[", "id")
      popularity <- sapply(artist_info, "[[", "popularity")
      followers <- lapply(artist_info, "[[", "followers")
      followers <- sapply(followers, "[[", "total")
      genres <- lapply(artist_info, "[[", "genres")
      artist_info_table <- as_tibble(list(artists_id = artists_id, artists_popularity = popularity, artists_followers = followers, genres = genres))
      audio_features_table <- bind_rows(audio_features)
      spotify_subset <- spotify_subset %>%
        left_join(audio_features_table %>% select(danceability:id, time_signature), by = "id") %>%
        left_join(artist_info_table, by = c("artists_id_main" = "artists_id")) %>%
        select(-artists_Name)
      
# genres (own categorization... needed to rank)
      spotify_subset <- spotify_subset %>%
        mutate(broad_genre = case_when(str_detect(as.character(genres), "modern rock") ~ "modern rock",
                                       str_detect(as.character(genres), "pop punk") ~ "pop punk",
                                       str_detect(as.character(genres), "r&b") ~ "r&b",
                                       str_detect(as.character(genres), "\"rap") ~ "rap",
                                       str_detect(as.character(genres), "latin") ~ "latin",
                                       str_detect(as.character(genres), "bollywood") ~ "bollywood",
                                       str_detect(as.character(genres), "classical") ~ "classical",
                                       str_detect(as.character(genres), "focus") ~ "instrumental",
                                       str_detect(as.character(genres), "dance pop") ~ "dance pop",
                                       str_detect(as.character(genres), "alt z") ~ "indie",
                                       str_detect(as.character(genres), "singer-songwriter") ~ "indie",
                                       str_detect(as.character(genres), "indie") ~ "indie",
                                       str_detect(as.character(genres), "show tunes") ~ "show tunes",
                                       str_detect(as.character(genres), "broadway") ~ "show tunes",
                                       str_detect(as.character(genres), "folk") ~ "folk",
                                       str_detect(as.character(genres), "country") ~ "country",
                                       str_detect(as.character(genres), "pop") ~ "pop",
                                       str_detect(as.character(genres), "rock") ~ "rock",
                                       str_detect(as.character(genres), "singer-songwriter") ~ "indie",
                                       str_detect(as.character(genres), "hollywood") ~ "soundtrack",
                                       str_detect(as.character(genres), "ambient") ~ "instrumental",
                                       str_detect(as.character(genres), "jazz") ~ "jazz",
                                       str_detect(as.character(genres), "soundtrack") ~ "soundtrack",
                                       str_detect(as.character(genres), "piano") ~ "instrumental",
                                       str_detect(as.character(genres), "environmental") |
                                         str_detect(as.character(genres), "white noise") |
                                         str_detect(as.character(genres), "water") |
                                         str_detect(as.character(genres), "sound")
                                       ~ "sounds",
                                       str_detect(as.character(genres), "sleep") ~ "instrumental",
                                       str_detect(as.character(genres), "list()") ~ NA_character_,
                                       TRUE ~ "misc"))
      
# WRITE OUT
      
write_rds(spotify_subset, "data/processed/spotify_subset.rds")

