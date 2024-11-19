library(tidyverse)
library(tidytext)
library(ggwordcloud)
library(magick)
library(camcorder)

gg_record(here::here("30daymap-temp/"), width = 8, height = 8.5, dpi = 320)

nba_teams <- tribble(
  ~team_name, ~state, ~subreddit, ~conference, ~division, ~team_color,
  "Golden State Warriors", "California", "warriors", "Western", "Pacific", "#FDB927",
  "Los Angeles Lakers", "California", "lakers", "Western", "Pacific", "#552583",
  "Los Angeles Clippers", "California", "laclippers", "Western", "Pacific", "#C8102E",
  "Sacramento Kings", "California", "kings", "Western", "Pacific", "#121014",
  "Boston Celtics", "Massachusetts", "bostonceltics", "Eastern", "Atlantic", "#007A33",
  "Toronto Raptors", "Ontario", "torontoraptors", "Eastern", "Atlantic", "#CE1141",
  "Philadelphia 76ers", "Pennsylvania", "sixers", "Eastern", "Atlantic", "#006BB6",
  "Chicago Bulls", "Illinois", "chicagobulls", "Eastern", "Central", "#CE1141",
  "Milwaukee Bucks", "Wisconsin", "mkebucks", "Eastern", "Central", "#00471B",
  "Denver Nuggets", "Colorado", "denvernuggets", "Western", "Northwest", "#0E2240",
  "Dallas Mavericks", "Texas", "mavericks", "Western", "Southwest", "#00538C",
  "Houston Rockets", "Texas", "rockets", "Western", "Southwest", "#CE1141",
  "Miami Heat", "Florida", "heat", "Eastern", "Southeast", "#98002E",
  "New York Knicks", "New York", "nyknicks", "Eastern", "Atlantic", "#006BB6",
  "Phoenix Suns", "Arizona", "suns", "Western", "Pacific", "#1D1160",
  "Brooklyn Nets", "New York", "gonets", "Eastern", "Atlantic", "#000000",
  "Atlanta Hawks", "Georgia", "atlantahawks", "Eastern", "Southeast", "#E03A3E",
  "Portland Trailblazers", "Oregon", "ripcity", "Western", "Northwest", "#E03A3E",
  "Oklahoma City Thunder", "Oklahoma", "thunder", "Western", "Northwest", "#007AC1",
  "San Antonio Spurs", "Texas", "nbaspurs", "Western", "Southwest", "#000000",
  "Minnesota Timberwolves", "Minnesota", "timberwolves", "Western", "Northwest", "#0C2340",
  "Washington Wizards", "District of Columbia", "washingtonwizards", "Eastern", "Southeast", "#002B5C",
  "Utah Jazz", "Utah", "utahjazz", "Western", "Northwest", "#002B5C",
  "Memphis Grizzlies", "Tennessee", "memphisgrizzlies", "Western", "Southwest", "#5D76A9",
  "Detroit Pistons", "Michigan", "detroitpistons", "Eastern", "Central", "#C8102E",
  "New Orleans Pelicans", "Louisiana", "nolapelicans", "Western", "Southwest", "#0C2340",
  "Indiana Pacers", "Indiana", "pacers", "Eastern", "Central", "#002D62",
  "Charlotte Hornets", "North Carolina", "charlottehornets", "Eastern", "Southeast", "#1D1160",
  "Orlando Magic", "Florida", "orlandomagic", "Eastern", "Southeast", "#0077C0",
  "Cleveland Cavaliers", "Ohio", "clevelandcavs", "Eastern", "Central", "#860038"
)

# ### Get top posts by year and write to csv
# library(RedditExtractoR)
#
# get_top_posts <- function(subreddit) {
#   threads <- find_thread_urls(subreddit = subreddit, sort_by = "top", period = "year")
#   
#   write_csv(threads, paste0(here::here("2024/data/subreddits/"), subreddit, ".csv"))
#   
#   Sys.sleep(10)
# }
# 
# map(nba_teams$subreddit, get_top_posts) 


# Read top posts by team
team_top_post_csv <- list.files(here::here("2024/data/subreddits/"), full.names = TRUE)

# Get top 100 words
get_top_words <- function(filename) {
  threads <- read_csv(filename)
  
  top_words <- threads %>%
    select(title) %>%
    unnest_tokens(word, title) %>%
    anti_join(stop_words) %>%
    count(word, sort = TRUE) %>%
    head(100) %>% 
    mutate(rank = row_number(), .before = 1)
  
  subreddit <- basename(filename) %>% str_remove(., ".csv")
  
  tibble(subreddit = subreddit, top_words)
}

team_top_words <- map(team_top_post_csv, get_top_words) %>% 
  list_rbind() %>% 
  left_join(nba_teams) %>% 
  group_by(state) %>% 
  mutate(
    team_id = dense_rank(team_name),
    word_lab = paste0(rank, ".", word)
    ) %>% 
  ungroup()

# StateFace mappings
# http://propublica.github.io/stateface/
stateface <- tribble(
  ~character, ~code, ~state,
  "B", "Ala.", "Alabama",
  "A", "Alaska", "Alaska",
  "D", "Ariz.", "Arizona",
  "C", "Ark.", "Arkansas",
  "E", "Calif.", "California",
  "F", "Colo.", "Colorado",
  "G", "Conn.", "Connecticut",
  "H", "Del.", "Delaware",
  "y", "D.C.", "District of Columbia",
  "I", "Fla.", "Florida",
  "J", "Ga.", "Georgia",
  "K", "Hawaii", "Hawaii",
  "M", "Idaho", "Idaho",
  "N", "Ill.", "Illinois",
  "O", "Ind.", "Indiana",
  "L", "Iowa", "Iowa",
  "P", "Kan.", "Kansas",
  "Q", "Ky.", "Kentucky",
  "R", "La.", "Louisiana",
  "U", "Maine", "Maine",
  "T", "Md.", "Maryland",
  "S", "Mass.", "Massachusetts",
  "V", "Mich.", "Michigan",
  "W", "Minn.", "Minnesota",
  "Y", "Miss.", "Mississippi",
  "X", "Mo.", "Missouri",
  "Z", "Mont.", "Montana",
  "c", "Neb.", "Nebraska",
  "g", "Nev.", "Nevada",
  "d", "N.H.", "New Hampshire",
  "e", "N.J.", "New Jersey",
  "f", "N.M.", "New Mexico",
  "h", "N.Y.", "New York",
  "a", "N.C.", "North Carolina",
  "b", "N.D.", "North Dakota",
  "i", "Ohio", "Ohio",
  "j", "Okla.", "Oklahoma",
  "k", "Ore.", "Oregon",
  "l", "Pa.", "Pennsylvania",
  "m", "R.I.", "Rhode Island",
  "n", "S.C.", "South Carolina",
  "o", "S.D.", "South Dakota",
  "p", "Tenn.", "Tennessee",
  "q", "Texas", "Texas",
  "z", "U.S.", "United States",
  "r", "Utah", "Utah",
  "t", "Vt.", "Vermont",
  "s", "Va.", "Virginia",
  "u", "Wash.", "Washington",
  "w", "W.Va.", "West Virginia",
  "v", "Wis.", "Wisconsin",
  "x", "Wyo.", "Wyoming"
)


# Create state silhouette image
text_to_png <- function(char, font = "StateFace") {
  img <- image_blank(width = 300, height = 300, color = "transparent") %>%
    image_annotate(text = char,
                   color = "black",
                   font = font,
                   size = 200,
                   gravity = "center") %>% 
    image_trim()
  
  # Return the image
  return(img)
}

# Fonts
f1 <- "Graphik"
f1b <- "Graphik Compact"

# Plot state word cloud
word_state <- function(state = "California", size = 30) {
  state_teams <- team_top_words %>% 
    filter(state == !!state)
  
  state_char <- stateface %>%
    filter(state == !!state) %>%
    pull(character)
  
  state_png <- text_to_png(state_char)
  
  ggplot(state_teams) +
    geom_text_wordcloud_area(aes(x = team_name, label = word, size = n, color = team_color), mask = state_png, family = f1b, fontface = "bold") +
    scale_size_area(max_size = size) +
    scale_color_identity() +
    labs(
      title = paste("100 most common words in NBA team subreddits:", state),
      subtitle = "Source: Reddit · Graphic: Georgios Karamanis"
    ) +
    theme_void(base_family = f1b) +
    theme(
      plot.background = element_rect(fill = "#F5F3F0", color = NA),
      strip.background = element_rect(fill = "black"),
      strip.text = element_text(color = "white"),
      plot.margin = margin(10, 10, 10, 10),
      plot.title = element_text(hjust = 0.5, size = 18, face = "bold", color = "#FF6B00"),
      plot.subtitle = element_text(hjust = 0.5, color = "#FF6B00", size = 12, margin = margin(6, 0, 10, 0))
    )
}


# Plot state word clouds
word_state(state = "California", size = 20.5)
word_state(state = "New York", size = 34)
word_state(state = "Massachusetts", size = 30)
word_state(state = "Texas", size = 28)
word_state(state = "Illinois", size = 50)
  
