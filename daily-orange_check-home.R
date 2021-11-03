#!/usr/bin/Rscript

# Check meduza.io for new articles
rm(list = ls())
# source(file = "sec_auth.R")
suppressPackageStartupMessages(require(tidyverse, quietly = TRUE))
require(rvest, quietly = TRUE)
"%nin%" = Negate("%in%")

# Scraping: Prep ---
# Define section names and URLS
home_sections = tribble(~section_name, ~section_url,
                        "NEWS", "https://dailyorange.com/news/", 
                        "CULTURE", "https://dailyorange.com/culture/",
                        "SPORTS", "https://dailyorange.com/sports/",
                        "OPINION", "https://dailyorange.com/opinions/",
                        "MEDIA", "https://dailyorange.com/media/",
                        )

# Scraping: Scape per Section ----
orange_check = vector(mode = "list", length = nrow(home_sections))

for(i in seq_along(home_sections$section_url)){
  home = rvest::read_html(x = home_sections$section_url[i]) # grab page source
  
  home.titles = home %>% html_nodes("[class='tz-h2 feed-h']") %>% html_text2()
  home.links = home %>% html_nodes("[class='tz-h2 feed-h']") %>% 
    html_nodes("a") %>% 
    html_attrs()
  home.links = lapply(home.links, function(x){x[1]}) %>% unlist()
  
  # number of articles must match number of links
  assertthat::are_equal(length(home.titles), length(home.links))
  
  article_check = tibble(
    # General Fields 
    "link" = home.links,
    "by_line" = home_sections$section_name[i],
    "full_title" = home.titles,
    "ping_time" = Sys.time(),
    
    # Final Outcome Flag
    "processed" = FALSE, # make NA if downstream processes fail; ignored by process queue
    
    # googleLanguageR and API Processes
    # ...
    
    # FFMPEG Processes
    # ...
    
    # Podcast API Processes
    "show_id" = as.integer("1234"
      #local.get_key("transistor-api.meduza.show_id")
      ), # int
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
  
  orange_check[[i]] = article_check
}

orange_check = bind_rows(orange_check)

# Compare to Existing Processing Queue
if(!file.exists("daily-orange-processing-queue.rds")){
  
  # initialize if necessary
  saveRDS(object = orange_check, file = "daily-orange-processing-queue.rds")
  
}else{
  
  orange_queue = readRDS(file = "daily-orange-processing-queue.rds") # read queue
  orange_check = orange_check %>% # check for new entries
    filter(link %nin% orange_queue$link)
  
  if(nrow(orange_check) > 0){
    orange_queue = bind_rows(orange_queue, orange_check) # add entries to processing queue
    saveRDS(object = orange_check, file = "daily-orange-processing-queue.rds") # save updated queue
  }
  
  
  
}
