# CREATING SUBSET FROM MY MUSIC ####

library(spotifyr)
library(tidyverse)

Sys.setenv(SPOTIFY_CLIENT_ID = '1bff3ae8f2bf478cb5e5be684a4d4084')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'ed310531d5cf446cab74c55cc3947ef1')
access_token <- get_spotify_access_token()

output <- vector("list", 16) # there are 1500+ tracks... pull from 0-100, 101-200 etc.
for (i in 1:16) {
  output[[i]] <- get_playlist_tracks("2gjTAhIwNf912Y8Mub14cJ", offset = 100 * (i - 1))
}

leilas_music <- bind_rows(output)
names(leilas_music) <- str_replace_all(names(leilas_music), "\\.", "_")

# hard to work with lists in a dataframe so I will extract, manipulate, then reinsert
      markets_count <- unlist(map(leilas_music$track_available_markets, length))
      
      leilas_music <- leilas_music %>%
        mutate(available_markets = markets_count) %>%
        mutate(available_markets = case_when(available_markets == 0 ~ NA_integer_,
                                             TRUE ~ available_markets)) %>%
        filter(track_is_local == FALSE) %>% # local songs do not have the same stored information
        as_tibble() %>%
        unnest(track_artists, keep_empty = TRUE, names_sep = "_") %>%
        select(track_id, track_name, track_artists_name, track_artists_id,
               track_duration_ms, track_explicit, track_popularity, available_markets,
               track_album_album_type, track_album_id, track_album_name, track_album_release_date,
               track_album_total_tracks, track_track_number) %>%
        rename(album_type = track_album_album_type)

# track is repetitive at the beginning... lets get rid of it
      names(leilas_music) <- str_replace_all(names(leilas_music), "^track_", "")

# set up artist info
      leilas_music <- leilas_music %>%
        group_by(id) %>%
        mutate(artist_rank = row_number()) %>%
        mutate(artist_rank = case_when(artist_rank == 1 ~ "main",
                                       TRUE ~ str_c("feature_", artist_rank - 1))
        ) %>%
        pivot_wider(names_from = artist_rank, values_from = c(artists_name, artists_id)) %>%
        ungroup()

# add additional data sources
      audio_features <- map(leilas_music$id, get_track_audio_features)
      artist_info <- map(unique(leilas_music$artists_id_main), get_artist)
      bind_rows(audio_features)
      artists_id <- sapply(artist_info, "[[", "id")
      popularity <- sapply(artist_info, "[[", "popularity")
      followers <- lapply(artist_info, "[[", "followers")
      followers <- sapply(followers, "[[", "total")
      genres <- lapply(artist_info, "[[", "genres")
      artist_info_table <- as_tibble(list(artists_id = artists_id, artists_popularity = popularity, artists_followers = followers, genres = genres))
      audio_features_table <- bind_rows(audio_features)
      leilas_music <- leilas_music %>%
        left_join(audio_features_table %>% select(danceability:id, time_signature), by = "id") %>%
        left_join(artist_info_table, by = c("artists_id_main" = "artists_id"))

# genres (own categorization... needed to rank)
      genres_list <- bind_rows(as_tibble(unlist(leilas_music$genres)), as_tibble(unlist(spotify_subset$genres))) %>%
        group_by(value) %>%
        tally() %>%
        arrange(desc(n))
      
      leilas_music <- leilas_music %>%
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
      
# write out

write_rds(leilas_music, "data/processed/leilas_music.rds")

