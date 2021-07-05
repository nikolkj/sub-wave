# Check meduza.io for new articles
require(tidyverse)
require(rvest)
"%nin%" = Negate("%in%")

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
meduza_check = tibble(
              # General Fields 
              "link" = home.links,
              "by_line" = home.titles.by,
              "full_title" = home.titles.full,
              "ping_time" = Sys.time(),
              
              # Final Outcome Flag
              "processed" = FALSE, # make NA if downstream processes fail; ignored by process queue
              
              # googleLanguageR and API Processes
              # ...
              
              # FFMPEG Processes
              # ...
              
              # Podcast API Processes
              "show_id" = as.integer(keyring::key_get("transistor-api.meduza.show_id")), # int
              "episode_id" = 0L, # int
              
              "response_draft" = vector(mode = "list", length = length(home.links)), # api response
              "sucess_draft" = FALSE, # logical, based on api response
              
              "response_uploadurl" = vector(mode = "list", length = length(home.links)), # api response
              "success_uploadurl" = FALSE, #logical, based on api response
              "uploadurl_long" = NA, # string
              "uploadurl_short" = NA, # string
              
              "response_upload" = vector(mode = "list", length = length(home.links)), # api response
              "success_upload" = FALSE, #logical, based on api response
              
              "response_audiolink" = vector(mode = "list", length = length(home.links)), # api response
              "success_audiolink" = FALSE, # logical, based on api response
              
              "response_publish" = vector(mode = "list", length = length(home.links)), # api response
              "success_publish" = FALSE, # logical, based on api response
              
              # Other
              "output_filename" = NA # string
              
              )


# Compare to Existing Processing Queue
if(!file.exists("meduza-processing-queue.rds")){
  
  # initialize if necessary
  saveRDS(object = meduza_check, file = "meduza-processing-queue.rds")
  
}else{
  
  meduza_queue = readRDS(file = "meduza-processing-queue.rds") # read queue
  meduza_check = meduza_check %>% # check for new entries
    filter(link %nin% meduza_queue$link)
  
  if(nrow(meduza_check) > 0){
    meduza_queue = bind_rows(meduza_queue, meduza_check) # add entries to processing queue
    saveRDS(object = meduza_queue, file = "meduza-processing-queue.rds") # save updated queue
  }
  
  
  
}


# write_rds(x = meduza_check, 
#           file = "meduza-temp.rds")



