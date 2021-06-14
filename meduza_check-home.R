# Check meduza.io for new articles
require(tidyverse)
require(rvest)

home = rvest::read_html(x = "https://meduza.io/en") # grab page source

# EXTRACTS ----
# grab title-block
# ... contains title, by-line and article path
home.titles =  home %>% html_nodes("div.RichBlock-title") 

# extract links
home.links = home.titles %>% 
  html_nodes("a") %>% 
  html_attrs()

home.links = lapply(home.links, function(x){x[2]}) %>% unlist()
article_positions = grep(pattern = "^/en/feature/", home.links) # pos: which are article links
home.links = home.links[article_positions]

# exract full titles
home.titles.full =  home.titles %>% html_text()
home.titles.full = home.titles.full[article_positions]

# extract by-lines
home.titles.by = home.titles %>%
  html_nodes("[class='BlockTitle-first BlockTitle-isFeatured']") %>% 
  html_text()

# Match by lines and full titles
# ... aligns extracts
assertthat::assert_that(length(home.links) == length(home.titles.full)) 
match_titles = sapply(home.titles.by, function(x){
  # match by-lines to each full title
  # ... take first match
  str_detect(string = home.titles.full, 
             pattern = fixed(x)) %>% 
    which() %>% 
    .[1]
  
}) %>% unlist()

home.titles.by = home.titles.by[-which(is.na(match_titles))] # drop invalid by-lines

match_titles.drop = setdiff(c(1:length(home.titles.full)), # find invalid full titles & links
                            match_titles)
home.links = home.links[-match_titles.drop] # drop invalid entries
home.titles.full = home.titles.full[-match_titles.drop] # drop invalid entries

rm(match_titles, match_titles.drop)

# check extract lengths
assertthat::assert_that(length(home.links) == length(home.titles.full))
assertthat::assert_that(length(home.links) == length(home.titles.by))

# LOG ----
# Generate temp log for downstream processing checks
meduza_check = tibble("link" = home.links,
              "by_line" = home.titles.by,
              "full_title" = home.titles.full,
              "ping_time" = Sys.time(),
              "processed" = FALSE)

write_rds(x = meduza_check, 
          file = "meduza-temp.rds")

# # testing 
# 
# 
# 
# # make sub-titles
# starts = nchar(home.titles.by) + 1
# ends = nchar(home.titles.full)
# home.titles.sub = substr(home.titles.full, 
#                          start = starts, stop = ends) %>% 
#   trimws()
# 
#   rm(starts, ends) 
# 
# trimmed.subtitles = sapply(home.titles.sub, function(x){
#   # trim subtitles to 35 char or less
#   # ... then add elipsis
#   tokens = str_split(x, pattern = "\\s+", simplify = T) # tokenize
#   
#   temp = tokens %>% 
#     nchar() %>% 
#     cumsum()
#   
#   temp = max(which(temp < 35))
#   temp = paste0(tokens[1:temp], collapse = " ") %>% 
#     paste(., "...")
#   
#   return(temp)
#     
# }) %>% unname()
# 
# paste0(home.titles.by, ": ", trimmed.subtitles)


