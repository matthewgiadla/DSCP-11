# Load necessary packages
library(tidyverse)

library(lubridate)
library(scales)

# =====================
# Load & Clean Data
# =====================

movies <- read_csv("movies.csv") %>%
  rename(movie_year = date) %>%
  select(id, name, rating, movie_year)

releases <- read_csv("releases.csv") %>%
  mutate(release_year = year(ymd(date))) %>%
  select(id, country, release_year)

studios <- read_csv("studios.csv") %>%
  separate_rows(studio, sep = ";|\\n|\\r") %>%
  mutate(studio = str_trim(studio)) %>%
  filter(studio != "")

actors <- read_csv("actors.csv")
crew <- read_csv("crew.csv")

# =====================
# Join Movies + Releases
# =====================

movie_info <- full_join(movies, releases, by = "id") %>%
  mutate(year = coalesce(release_year, movie_year)) %>%
  select(id, name, rating, country, year)

studio_success <- studios %>%
  inner_join(movie_info, by = "id") %>%
  drop_na(studio, country, year, rating) %>%
  select(studio, country, year, rating, movie_id = id, movie_name = name)

write_csv(studio_success, "studio_success.csv")

# =====================
# Metrics
# =====================

# Average Rating
avg_rating <- studio_success %>%
  group_by(studio) %>%
  summarize(avg_rating = mean(rating, na.rm = TRUE), .groups = "drop")

write_csv(avg_rating, "avg_rating.csv")

# Movies per Year + Longevity
studio_movies_per_year <- studio_success %>%
  select(studio, movie_id, year) %>%
  distinct() %>%
  group_by(studio) %>%
  summarize(
    total_movies = n_distinct(movie_id),
    first_year = min(year, na.rm = TRUE),
    last_year = max(year, na.rm = TRUE),
    years_active = last_year - first_year + 1,
    avg_movies_per_year = total_movies / years_active,
    .groups = "drop"
  )

write_csv(studio_movies_per_year, "studio_movies_per_year.csv")

# Countries per Movie
studio_country_reach <- studio_success %>%
  distinct(studio, movie_id, country) %>%
  group_by(studio, movie_id) %>%
  summarize(countries = n(), .groups = "drop") %>%
  group_by(studio) %>%
  summarize(avg_countries_per_movie = mean(countries), .groups = "drop")

write_csv(studio_country_reach, "studio_country_reach.csv")

# Actor Count per Movie
actors_per_movie <- actors %>%
  group_by(id) %>%
  summarize(actor_count = n(), .groups = "drop")

write_csv(actors_per_movie, "actors_per_movie.csv")

studio_actor_avg <- studios %>%
  inner_join(actors_per_movie, by = c("id")) %>%
  group_by(studio) %>%
  summarize(avg_actors = mean(actor_count), .groups = "drop")

# Crew Metrics
crew_roles_per_movie <- crew %>%
  group_by(id) %>%
  summarize(crew_roles = n_distinct(role), .groups = "drop")

crew_total_per_movie <- crew %>%
  group_by(id) %>%
  summarize(crew_total = n(), .groups = "drop")

crew_info <- crew_roles_per_movie %>%
  inner_join(crew_total_per_movie, by = "id")

write_csv(crew_info, "crew_info.csv")

# =====================
# Build Full Metrics
# =====================

top_studios <- avg_rating %>%
  filter(avg_rating > 3)

full_metrics <- top_studios %>%
  inner_join(studio_movies_per_year, by = "studio") %>%
  inner_join(studio_country_reach, by = "studio") %>%
  inner_join(studios, by = "studio") %>%
  inner_join(crew_info, by = "id") %>%
  group_by(studio) %>%
  summarize(
    avg_rating = first(avg_rating),
    total_movies = first(total_movies),
    years_active = first(years_active),
    avg_movies_per_year = first(avg_movies_per_year),
    avg_countries_per_movie = first(avg_countries_per_movie),
    avg_crew_roles = mean(crew_roles),
    avg_crew_total = mean(crew_total),
    .groups = "drop"
  )

write_csv(full_metrics, "full_metrics.csv")

# =====================
# Score Studios
# =====================

scored_studios <- full_metrics %>%
  mutate(
    rating_scaled        = rescale(avg_rating),
    movies_scaled        = rescale(avg_movies_per_year),
    total_movies_scaled  = rescale(total_movies),
    years_scaled         = rescale(years_active),
    countries_scaled     = rescale(avg_countries_per_movie),
    crew_roles_scaled    = rescale(avg_crew_roles),
    crew_total_scaled    = rescale(avg_crew_total),

    success_score = 0.25 * rating_scaled +
                    0.10 * movies_scaled +
                    0.10 * total_movies_scaled +
                    0.15 * years_scaled +
                    0.15 * countries_scaled +
                    0.10 * crew_roles_scaled +
                    0.15 * crew_total_scaled
  ) %>%
  arrange(desc(success_score))

# =====================
# Add Movie Names to Scored Studios
# =====================

studio_movies <- studio_success %>%
  select(studio, movie_name) %>%
  distinct() %>%
  group_by(studio) %>%
  summarize(
    movies = paste(unique(movie_name), collapse = ", "),
    .groups = "drop"
  )

scored_studios_with_movies <- scored_studios %>%
  left_join(studio_movies, by = "studio")

write_csv(scored_studios_with_movies, "scored_studios_with_movies.csv")

