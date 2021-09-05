# ABOUT: Generate synthetic voice encoding for meduza articles

# PRE-REQS:
# 1) api_key = keyring::key_get("transistor-api") must be set. Transitor.fm API key;

# Start-up ----
rm(list = ls())
suppressPackageStartupMessages(require(tidyverse, quietly = TRUE))
require(rvest, quietly = TRUE)
"%nin%" = Negate("%in%")

param_testing = TRUE # set to false

# Prep: General ----

# get processing queue 
meduza_queue = readRDS(file = "meduza-processing-queue.rds")

if(all(meduza_queue$processed)){
  stop("Queue: No new episodes.") # terminate script if all articles are processed
  # else, begin processing
} 

# API authentication
googleLanguageR::gl_auth(json_file = "sub-wave-1342c1e4cfc4.json")

# grab article
# ... ignores where [processed] == TRUE
# ... ignores where [processed] == NA
# .... these entries should be debugged
# .... because some key processing step failed
article_target = meduza_queue %>% 
  filter(!processed) %>% 
  slice(1) # take first available

if(nrow(article_target) > 0L){
  
  # get article
  article = paste0("https://meduza.io", 
                   article_target$link[1]) %>% 
    rvest::read_html()
  
}else{
  # all queue entries correspond to failed processes
  stop("Queue: Only episodes with failed processes. Check logs.") # terminate script
  # else, begin processing
}


# index sections 
# ... limit to accepted types
# ... defined by [accpt_sections] 
# .... which correspond to those having parsing logic
accpt_sections = c("p", "h3", "ul", "ol") # control

article.sections = article %>% 
  html_node("[class='GeneralMaterial-article']") %>% 
  html_children() %>% 
  html_name()

article.sections = article.sections[which(article.sections %in% accpt_sections)] 

# grab sections
raw.p = article %>% html_nodes("p") %>% html_text() %>% trimws()
raw.h3 = article %>% html_nodes("h3") %>% html_children() %>% html_text() %>% trimws()
raw.ul = article %>% html_nodes("ul") %>% html_text() %>% trimws()
raw.ol = article %>% html_nodes("ol") %>% html_text() %>% trimws()


# Prep: Other ----
update_meduza_queue = function(queue_tib = meduza_queue, article_tib = article_target){
  queue_tib = queue_tib %>% 
    filter(link %nin% article_tib$link) %>%
    bind_rows(., article_tib) 
  
  return(queue_tib)
}


# Process: [h3] ----
# Identify selection targets
# ... ignore superfluous (trailer) matches
h3.select = which(raw.h3 != "")
n = sum(article.sections == "h3")
h3.select = h3.select[c(1:n)]
rm(n)

# Update raw-extract
raw.h3 = raw.h3[h3.select]
if(is.na(raw.h3) & length(raw.h3) == 1){raw.h3 = ""}

assertthat::assert_that(!any(is.na(raw.h3))) # check for expected number, length(raw) = n & no NA


# initialize tbl
h3.dat = tibble(raw = raw.h3,
               raw_nchar = nchar(raw),
               wav_order = 0,
               tag = "h3"
)

# update text encoding to ASCII
h3.dat$raw = h3.dat$raw %>% 
  iconv(x = ., from = "UTF-8", to = "ASCII//TRANSLIT")

# assign unique id for each section
h3.dat$rid = sapply(h3.dat$raw, FUN = function(x){ # raw-id
  # make unique id
  digest::digest(object = x, algo = "md5", serialize = T)
})

h3.dat$rid = unname(h3.dat$rid)

# check that all paragraphs are under 5K char, allow 200 char for S
assertthat::assert_that(all(h3.dat$raw_nchar < 4800L)) 

h3.dat = h3.dat %>% 
  mutate(ssml_encode = paste0("<speak>",
                              "<break strength=\"x-strong\"/><emphasis level=\"strong\">",
                              raw,
                              "</emphasis><break strength=\"x-strong\"/>",
                              "</speak>")
  )

for(i in seq(nrow(h3.dat))){
  # write output to "api-out-stage/"
  googleLanguageR::gl_talk(input = h3.dat$ssml_encode[i], 
                           inputType = "ssml",
                           name = "en-GB-Wavenet-B", # preferred
                           output = paste0("api-out-stage/", h3.dat$rid[i],
                                           ".wav"),
                           audioEncoding = "LINEAR16",
                           pitch = -6,
                           sampleRateHertz = 48000
  )
  
  Sys.sleep(2L)
  
}



# Process: [p] ----
# initialize tbl
p.dat = tibble(raw = raw.p,
               raw_nchar = nchar(raw),
               wav_order = 0,
               tag = "p"
               )

# update text encoing to ASCII
p.dat$raw = p.dat$raw %>% 
  iconv(x = ., from = "UTF-8", to = "ASCII//TRANSLIT")

# assign unique id for each section
p.dat$rid = sapply(p.dat$raw, FUN = function(x){ # raw-id
  # make unique id
  digest::digest(object = x, algo = "md5", serialize = T)
  })
 
p.dat$rid = unname(p.dat$rid)

# check that all paragraphs are under 5K char, allow 200 char for S
assertthat::assert_that(all(p.dat$raw_nchar < 4800L)) 

p.dat = p.dat %>% 
  mutate(ssml_encode = paste0("<speak>",
                              raw,
                              "</speak>")
         )

for(i in seq(nrow(p.dat))){
  # write output to "api-out-stage/"
  googleLanguageR::gl_talk(input = p.dat$ssml_encode[i], 
                           inputType = "ssml",
                           name = "en-GB-Wavenet-B", # preferred
                           output = paste0("api-out-stage/", p.dat$rid[i],
                                           ".wav"),
                           audioEncoding = "LINEAR16",
                           speakingRate = 0.96,
                           pitch = -6,
                           sampleRateHertz = 48000
                           
  )
  
  Sys.sleep(2L)
  
}

# Process: [ul] ----

ul.select = which(raw.ul != "")
n = sum(article.sections == "ul")
ul.select = ul.select[c(1:n)]
rm(n)

# Re-make raw extract based on selection targe
raw.ul = article %>% html_nodes("ul") %>% 
  lapply(., function(x) {
    x %>% html_children() %>% html_text()
  }) %>% 
  .[ul.select]

rm(ul.select)

# pre-processing: add breaks
raw.ul = raw.ul %>%
  lapply(., function(x) {
    paste0("<break>", x)
  })

# # pre-processing: concatenate lists
raw.ul = sapply(raw.ul, paste, collapse = "")

# initialize tbl
ul.dat = tibble(raw = raw.ul,
               raw_nchar = nchar(raw),
               wav_order = 0,
               tag = "ul"
)

# update text encoing to ASCII
ul.dat$raw = ul.dat$raw %>% 
  iconv(x = ., from = "UTF-8", to = "ASCII//TRANSLIT")

# assign unique id for each section
ul.dat$rid = sapply(ul.dat$raw, FUN = function(x){ # raw-id
  # make unique id
  digest::digest(object = x, algo = "md5", serialize = T)
})

ul.dat$rid = unname(ul.dat$rid)

# check that all paragraphs are under 5K char, allow 200 char for S
assertthat::assert_that(all(ul.dat$raw_nchar < 4800L)) 

ul.dat = ul.dat %>% 
  mutate(ssml_encode = paste0("<speak>",
                              raw,
                              "</speak>")
  )

for(i in seq(nrow(ul.dat))){
  # write output to "api-out-stage/"
  googleLanguageR::gl_talk(input = ul.dat$ssml_encode[i], 
                           inputType = "ssml",
                           name = "en-GB-Wavenet-B", # preferred
                           output = paste0("api-out-stage/", ul.dat$rid[i],
                                           ".wav"),
                           audioEncoding = "LINEAR16",
                           speakingRate = 1,
                           pitch = -6,
                           sampleRateHertz = 48000
  )
  
  Sys.sleep(2L)
  
}


# Process: [ol] ----

# Identify selection targets
# ... ignore empties and superflous (trailer) matches
ol.select = which(raw.ol != "")
n = sum(article.sections == "ol")
ol.select = ol.select[c(1:n)]
rm(n)

# Re-make raw extract based on selection targe
raw.ol = article %>% html_nodes("ol") %>% 
  lapply(., function(x) {
    x %>% html_children() %>% html_text()
  }) %>% 
  .[ol.select]

raw.ol = lapply(raw.ol, function(x) {
  n = seq(x)
  paste(paste0(n, "."), x)
})

rm(ol.select)

# pre-processing: add breaks
raw.ol = raw.ol %>%
  lapply(., function(x) {
    paste0('<break strength="strong">', x)
  })

# # pre-processing: concatenate lists
raw.ol = sapply(raw.ol, paste, collapse = " ")

# initialize tbl
ol.dat = tibble(raw = raw.ol,
                raw_nchar = nchar(raw),
                wav_order = 0,
                tag = "ol"
)

# update text encoing to ASCII
ol.dat$raw = ol.dat$raw %>% 
  iconv(x = ., from = "UTF-8", to = "ASCII//TRANSLIT")

# assign unique id for each section
ol.dat$rid = sapply(ol.dat$raw, FUN = function(x){ # raw-id
  # make unique id
  digest::digest(object = x, algo = "md5", serialize = T)
})

ol.dat$rid = unname(ol.dat$rid)

# check that all paragraphs are under 5K char, allow 200 char for S
assertthat::assert_that(all(ol.dat$raw_nchar < 4800L)) 

ol.dat = ol.dat %>% 
  mutate(ssml_encode = paste0("<speak>",
                              raw,
                              "</speak>")
  )

for(i in seq(nrow(ol.dat))){
  # write output to "api-out-stage/"
  googleLanguageR::gl_talk(input = ol.dat$ssml_encode[i], 
                           inputType = "ssml",
                           name = "en-GB-Wavenet-B", # preferred
                           output = paste0("api-out-stage/", ol.dat$rid[i],
                                           ".wav"),
                           audioEncoding = "LINEAR16",
                           speakingRate = 1,
                           pitch = -6,
                           sampleRateHertz = 48000
  )
  
  Sys.sleep(2L)
  
}

# Prep: Blobs ----
article.author = article %>% 
  html_node("[class='MaterialNote-module_note_caption__1ezSo']") %>% html_text()
if(is.na(article.author)){article.author = NULL} 

article.translator = article %>% 
  html_node("[class='MaterialNote-module_note_credit__PuFyX']") %>% html_text() 
if(is.na(article.translator)){article.translator = NULL}

article.ts = article %>% 
  html_node("[class='Timestamp-module_root__coOvT']") %>% 
  html_text()

article.date = article.ts %>% 
  as.Date(x = ., "%H:%M %p, %B %d, %Y") %>% 
  format(x = ., "%B %d, %Y") 

article.by = ifelse(is.na(article_target$by_line), # handling for NULL  
                    article_target$full_title,
                    article_target$by_line)

blob.header = paste0("<emphasis level=\"strong\">", 
                     article.by, ". ",
                     "</emphasis>",
                     (if(is.null(article.author)) NULL else {paste0(article.author, ". ")}), # handling for NULL
                     (if(is.null(article.translator)) NULL else {paste0(article.translator, ". ")}) # handling for NULL
                     ) %>% 
  iconv(x = ., from = "UTF-8", to = "ASCII//TRANSLIT") %>% 
  paste0("<speak>", ., "</speak>")

blob.footer = paste0("Published on ", article.date, ".") %>% 
  iconv(x = ., from = "UTF-8", to = "ASCII//TRANSLIT") %>%
  paste0("<speak><break strength=\"x-strong\"/><emphasis level=\"strong\">", 
         ., 
         "</break><emphasis></speak>")

# Prep: Body audio synthesis clean-up ----
# Based on [article.sections]
# ... sections outside "GeneralMaterial-article" block will keep default order of 0
# .... and are dropped downstream
itt.p = 1 
itt.h3 = 1
itt.ul = 1
itt.ol = 1

for(i in seq_along(article.sections)){
  if(article.sections[i] == "h3"){
    h3.dat$wav_order[itt.h3] = i
    itt.h3 = itt.h3 + 1
    
  }else if(article.sections[i] == "p"){
    p.dat$wav_order[itt.p] = i
    itt.p = itt.p + 1
    
  }else if(article.sections[i] == "ul"){
    ul.dat$wav_order[itt.ul] = i
    itt.ul = itt.ul + 1
    
  }else if(article.sections[i] == "ol"){
    ol.dat$wav_order[itt.ol] = i
    itt.ol = itt.ol + 1
    
  }else{
    stop("Set order, unknown section type.")
  }
  
}

# Processing Queue
dat = bind_rows(
  h3.dat,
  p.dat,
  ul.dat,
  ol.dat
) %>% 
  arrange(wav_order)

dat = dat %>% 
  filter(wav_order > 0) %>% 
  mutate(wav_order = str_pad(wav_order, width = 3, side = "left", pad = 0))

# delete files with [wav_order] == 0
temp = dir(path = "api-out-stage/", pattern = ".wav$") %>% 
  str_remove(string = ., pattern = "\\.wav$")

temp = setdiff(temp, dat$rid)
if(length(temp) > 0){
  for(i in seq_along(temp)){
    file.remove(paste0("api-out-stage/", temp[i], ".wav"))
  }
}

rm(temp)

# rename files for downstream processing
# ... name-sort order must match natural order
dat = dat %>% 
  mutate(new_filename = paste0(wav_order, "_", rid, ".wav"))

for(i in seq_along(dat$wav_order)){
   
  file.rename(from = paste0("api-out-stage/",
                            dat$rid[i], ".wav"),
              
              to = paste0("api-out-stage/",
                          dat$new_filename[i])
  )
  
}

# Prep: Blob Encoding ----
blob.header %>% 
googleLanguageR::gl_talk(input = ., 
                         inputType = "ssml",
                         name = "en-GB-Wavenet-B", # preferred
                         # name = "en-US-Wavenet-B", # second-best
                         output = paste0("api-out-stage/", 
                                         "000_intro",
                                         ".wav"),
                         audioEncoding = "LINEAR16", 
                         pitch = -6 ,
                         sampleRateHertz = 48000
                         
)

blob.footer %>% 
  googleLanguageR::gl_talk(input = ., 
                           inputType = "ssml",
                           name = "en-GB-Wavenet-B", # preferred
                           # name = "en-US-Wavenet-B", # second-best
                           output = paste0("api-out-stage/", 
                                           "999_footer",
                                           ".wav"),
                           audioEncoding = "LINEAR16",
                           pitch = -6,
                           sampleRateHertz = 48000 
                           
  )
  
# Encoding conversions and audio concatenation ----
# Convert WAV to m4a
# ... dump to "api-out-stage/" dir

# apply equilizer
# ... e.g. ffmpeg -i input.wav -af "equalizer=f=200:width_type=o:width=2:g=4,equalizer=f=3000:width_type=o:width=2:g=2" output.wav
# ... should be applied in for-loop
# ... e.g. 'cd api-out-stage\\\ && FOR /F "tokens=*" %G IN (\'dir /b *.wav\') DO (ffmpeg eq)'
# .... where input & output ~ "%G"
# .... add overwrite default yes


# convert wav files to m4a 
if(Sys.info()[1] == "Windows"){
  base::shell(cmd = 'cd api-out-stage\\\ && FOR /F "tokens=*" %G IN (\'dir /b *.wav\') DO ffmpeg.exe -i "%G" "%~nG.m4a"',  wait = TRUE)
}else if (Sys.info()[1] == "Linux"){
  # system2 equiv
  base::system(command = 'sh ffmpeg_conv.sh')
  
}else{
  stop("Unrecognized OS details. Can't run FFMPEG.")
}


# merge converted m4a files
dir(path = "api-out-stage/", pattern = ".m4a$") %>% 
  paste0("file '",.,"'") %>% 
  writeLines(text = ., con = "api-out-stage/ref.txt") # make reference file with targets

output_file_name = ifelse(!is.na(article_target$by_line[1]),
                          article_target$by_line[1],
                          article_target$full_title[1]) %>% # prep output file name
  iconv(x = ., from = "UTF-8", to = "ASCII//TRANSLIT") %>% 
  gsub(pattern = "[[:punct:]]", "", .) %>% # NO PUNCT ALLOWED
  stringr::str_to_title() %>%
  gsub(pattern = "\\s+", replacement = "", .) %>% # NO SPACES ALLOWED
  paste0("meduza-", ., ".m4a")

article_target$output_filename[1] = output_file_name # log

if(Sys.info()[1] == "Windows"){
output_file_name %>%
  paste("cd api-out-stage/ && ffmpeg.exe -f concat -safe 0 -i ref.txt -c copy", .) %>%
  base::shell(cmd = .) # finally merge
}else if(Sys.info()[1] == "Linux"){
  output_file_name %>%
    paste("cd api-out-stage/ && ffmpeg -f concat -safe 0 -i ref.txt -c copy", .) %>% 
    system(command = .)
}else{
  stop("error msg.")
}

if(param_testing){
  stop("See files in api-out-stage/")
}

# move
if(Sys.info()[1] == "Windows"){
output_file_name %>% 
  paste("cd api-out-stage/ && move", ., "../landing-meduza/") %>% # move final output file
  shell(cmd = .)
  
  base::shell("cd api-out-stage/ && move *.wav ../exhaust/") # move original API wav output
  base::shell("cd api-out-stage/ && move *.m4a ../exhaust/") # move converted m4a's
  
}else if(Sys.info()[1] == "Linux"){
  # move final output file
  output_file_name %>% 
    paste("cd api-out-stage/ && mv", ., "../landing-meduza/") %>% # move final output file
    system(command = .)
  
  system(command = "cd api-out-stage/ && mv *.wav ../exhaust/") # move original API wav output
  system(command = "cd api-out-stage/ && mv *.m4a ../exhaust/") # move converted m4a's
  
}else{
  stop("error msg.")
}


# dat$new_filename %>%
#   paste0('"',.,'"') %>%
#   paste("cd input-test/ && move", ., "../exhaust/") %>% # move input file
#   base::shell(cmd = .)

# Publish Podcast ----
api_key = keyring::key_get("transistor-api")

# ~~~~~ (1) Create Episode Draft ~~~~~
req.url = "https://api.transistor.fm/v1/episodes" # API URL

post.title = paste0(article_target$by_line[1], ": ", # pass to request
                    trimws(
                      str_sub(
                        string = article_target$full_title[1],
                        start = nchar(article_target$by_line[1]) + 1,
                        end = -1L
                      )
                    ))

if(is.na(article_target$by_line[1])){
  post.title = article_target$full_title[1]
}

post.summary = p.dat$raw[1] # pass to request

post.description = paste(p.dat$raw[1], 
                         paste("Original Article:", paste0("https://meduza.io", article_target$link[1]))
)

resp.draft = httr::POST(
  url = req.url,
  httr::add_headers("x-api-key" = api_key),
  query = list(
    'episode[show_id]' = article_target$show_id[1],
    'episode[title]' = post.title,
    'episode[summary]' = post.summary,
    'episode[description]' = post.description
    
  )
  
)

# status check and error handling
if(resp.draft$status_code %in% c(200L, 201L)){
  
  # success 
  article_target$response_draft[1] = list("response" = resp.draft)
  article_target$episode_id[1] = as.integer(httr::content(resp.draft)$data$id) # store new episode-id value
  article_target$sucess_draft[1] = TRUE
  
}else{
  # failure
  article_target$processed[1] = NA # flag for debugging
  saveRDS(object = update_meduza_queue(), # update processing queue record
          file = "meduza-processing-queue.rds") 
  stop("Tranistor.fm: Failed to create episode draft.")
  
}

rm(resp.draft, post.title, post.summary, req.url) # clean-up 

# ~~~~~ (2) Create Upload URL ~~~~~
req.url = "https://api.transistor.fm/v1/episodes/authorize_upload"
resp.uploadurl = httr::GET(url = req.url, 
                       httr::add_headers("x-api-key" = api_key),
                       query = list(
                         'filename'=output_file_name
                       )
                       )

# status check and error handling
if(resp.uploadurl$status_code %in% c(200L)){
  # success
  article_target$response_uploadurl[1] = list("response" = resp.uploadurl)
  article_target$uploadurl_long[1] = httr::content(resp.uploadurl)[["data"]]$attributes$upload_url
  article_target$uploadurl_short[1] = article_target$uploadurl_long[1] %>% 
    str_locate(string = ., 
               pattern = "=public-read") %>% 
    .[2] %>% 
    str_sub(string = article_target$uploadurl_long[1], end = .)
  article_target$success_uploadurl[1] = TRUE
  
}else{
  
  # failure
  article_target$processed[1] = NA # flag for debugging
  saveRDS(object = update_meduza_queue(), # update processing queue record
          file = "meduza-processing-queue.rds") 
  stop("Tranistor.fm: Failed to create upload url.")
  
}

rm(req.url, resp.uploadurl)

# ~~~~~ (3) Upload File ~~~~~
# dev-note: update to use 'httr' and upload_file()

# put request template
# ... works with base::shell() and base::system()
# .... BUT not base::system2()
# .... why? 
put_request = '
curl -v -X PUT \
 -H \"Content-Type: audio/mpeg\" \
-T {file-path} \
 "{upload-target-long}"
' 
# fill in the blanks
if(Sys.info()[1] == "Windows"){
  
  landing.path = readLines(con = "landing-meduza.path", warn = FALSE) # must be absolute
  
}else if(Sys.info()[1] == "Linux"){
  
  landing.path = system(command = "realpath landing-meduza/", intern = TRUE) %>% 
    paste0(., "/")
  
}else{
  stop("error msg.")
}

put_request = sub(pattern = "\\{file-path\\}",
                  replacement = paste0(landing.path, output_file_name), 
                  x = put_request)

put_request = sub(pattern = "\\{upload-target-long\\}",
                  replacement = article_target$uploadurl_long[1],
                  x = put_request)

put_request = gsub(pattern = "\\\n", "", put_request)

# execute request
req.upload = system(command = put_request, intern = TRUE)

# status check and error handling
# ... checks for <Error> tag in response
# .... no cleaner method since request made through shell and not httr
if(!any(str_detect(req.upload, "<Error>"))){
  
  # success
  article_target$response_upload[1] = list("response" = req.upload)
  article_target$success_upload[1] = TRUE
  
}else{
  
  # failure
  article_target$processed[1] = NA # flag for debugging
  saveRDS(object = update_meduza_queue(), # update processing queue record
          file = "meduza-processing-queue.rds") 
  stop("Tranistor.fm: Failed to upload file.")
  
  
}

# clean-up
rm(landing.path, put_request, req.upload)

# ~~~~~ (4) Link Episode and Audio File ~~~~~~
req.url = "https://api.transistor.fm/v1/episodes/"
resp.link_audio = httr::PATCH(
  url = paste0(req.url, article_target$episode_id[1]),
  httr::add_headers("x-api-key" = api_key),
  query = list('episode[audio_url]' =  article_target$uploadurl_short[1])
)

# status check and error handling
if(resp.link_audio$status_code %in% c(200L)){
  
  # success
  article_target$response_audiolink[1] = list("response" = resp.link_audio)
  article_target$success_audiolink[1] = TRUE
  
}else{
  
  # failure
  article_target$processed[1] = NA # flag for debugging
  saveRDS(object = update_meduza_queue(), # update processing queue record
          file = "meduza-processing-queue.rds") 
  stop("Tranistor.fm: Failed to link audio-file.")
  
}

rm(req.url, resp.link_audio)

# ~~~~~ (5) Publish Episode ~~~~~
req.url = "https://api.transistor.fm/v1/episodes/"
resp.publish = httr::PATCH(
  url = paste0(req.url, article_target$episode_id[1], "/publish"),
  httr::add_headers("x-api-key" = api_key),
  query = list(
    'episode[status]' = 'published',
    'fields[episode][]' = 'status'
  )
)

# status check and error handling
if(resp.publish$status_code %in% c(200L)){
  
  # success
  article_target$response_publish[1] = list("response" = resp.publish)
  article_target$success_publish[1] = TRUE
  
}else{
  
  # failure
  article_target$processed[1] = NA # flag for debugging
  saveRDS(object = update_meduza_queue(), # update processing queue record
          file = "meduza-processing-queue.rds") 
  stop("Tranistor.fm: Failed to publish episode.")
  
}

# ~~~~~ (6) Update Queue Record ~~~~~
article_target$processed[1] = TRUE # final outcome flag
saveRDS(object = update_meduza_queue(), # update processing queue record
        file = "meduza-processing-queue.rds") 
