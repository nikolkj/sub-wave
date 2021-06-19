# Generate synthetic voice encoding for meduza articles
rm(list = ls())
require(tidyverse)
require(rvest)

meduza_check = readRDS(file = "meduza-temp.rds")

if(all(meduza_check$processed)){
  stop() # terminate script if all articles are processed
  # else, begin processing
} 

# Prep: General ----
# API authentication
googleLanguageR::gl_auth(json_file = "sub-wave-1342c1e4cfc4.json")

# grab article
article_target = meduza_check %>% 
  filter(!processed) %>% 
  slice(3) 

article = paste0("https://meduza.io", 
                 article_target$link[1]) %>% 
  rvest::read_html()

# index sections 
# ... limit to accepted types
# ... defined by [accpt_sections] 
# .... which correspond to those having parsing logic
accpt_sections = c("p", "h3", "ul") # control

article.sections = article %>% 
  html_node("[class='GeneralMaterial-article']") %>% 
  html_children() %>% 
  html_name()

article.sections = article.sections[which(article.sections %in% accpt_sections)] 

# grab sections
raw.p = article %>% html_nodes("p") %>% html_text()
raw.h3 = article %>% html_nodes("h3") %>% html_children() %>% html_text() 
raw.ul = article %>% html_nodes("ul") %>% html_text()

# Process: [h3] ----
# Identify selection targets
# ... ignore superflous (trailer) matches
h3.select = which(raw.h3 != "")
n = sum(article.sections == "h3")
h3.select = h3.select[c(1:n)]
rm(n)

# Update raw-extract
raw.h3 = raw.h3[h3.select]
assertthat::assert_that(!any(is.na(raw.h3))) # check for expected number, length(raw) = n & no NA


# initialize tbl
h3.dat = tibble(raw = raw.h3,
               raw_nchar = nchar(raw),
               wav_order = 0,
               tag = "h3"
)

# update text encoing to ASCII
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
                           # name = "en-US-Wavenet-D", # preferred
                           name = "en-US-Wavenet-B", # second-best
                           output = paste0("api-out-stage/", h3.dat$rid[i],
                                           ".wav"),
                           audioEncoding = "LINEAR16",
                           effectsProfileIds = "headphone-class-device"
  )
  
  Sys.sleep(5)
  
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
                           name = "en-US-Wavenet-D", # preferred
                           # name = "en-US-Wavenet-B", # second-best
                           output = paste0("api-out-stage/", p.dat$rid[i],
                                           ".wav"),
                           audioEncoding = "LINEAR16",
                           effectsProfileIds = "headphone-class-device"
  )
  
  Sys.sleep(5)
  
}

# Process: [ul] ----

# Identify selection targets
# ... ignore empties and superflous (trailer) matches
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
                           name = "en-US-Wavenet-D", # preferred
                           # name = "en-US-Wavenet-B", # second-best
                           output = paste0("api-out-stage/", ul.dat$rid[i],
                                           ".wav"),
                           audioEncoding = "LINEAR16",
                           effectsProfileIds = "headphone-class-device"
  )
  
  Sys.sleep(5)
  
}

# Prep: Blobs ----
article.author = article %>% 
  html_node("[class='MaterialNote-module_note_caption__1ezSo']") %>% html_text()

article.translator = article %>% 
  html_node("[class='MaterialNote-module_note_credit__PuFyX']") %>% html_text()

article.ts = article %>% 
  html_node("[class='Timestamp-module_root__coOvT']") %>% 
  html_text()

article.date = article.ts %>% 
  as.Date(x = ., "%H:%M %p, %B %d, %Y") %>% 
  format(x = ., "%B %d, %Y") 

article.by = article_target$by_line

blob.header = paste0("<emphasis level=\"strong\">", 
                     article.by, ". ",
                     "</emphasis>",
                     article.author, ". ",
                     article.translator, ".") %>% 
  iconv(x = ., from = "UTF-8", to = "ASCII//TRANSLIT") %>% 
  paste0("<speak>", ., "</speak>")

blob.footer = paste0("Published on ", article.date, ".") %>% 
  iconv(x = ., from = "UTF-8", to = "ASCII//TRANSLIT") %>%
  paste0("<speak><break strength=\"x-strong\"/><emphasis level=\"strong\">", 
         ., 
         "</break><emphasis></speak>")

# Prep: Body audio syntehsis clean-up ----
# Based on [article.sections]
# ... sections outside "GeneralMaterial-article" block will keep default order of 0
# .... and are dropped downstream
itt.p = 1 
itt.h3 = 1
itt.ul = 1
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
    
  }else{
    stop("Set order, unknown section type.")
  }
  
}

# Processing Queue
dat = bind_rows(
  h3.dat,
  p.dat,
  ul.dat
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

# # special case handing, do not run
# # ... sections outside "GeneralMaterial-article" block will keep default order of 0
# # .... sequence by original order or, where applicable, drop 
# 
# # h3 - leave at zero; will be deleted downstream
# 
# # p - assign negative value, brings to front
# p.dat$wav_order[which(p.dat$wav_order == 0)] = c(-100 : (-100 + sum(p.dat$wav_order == 0))) # bring to front
# 
# 
# 
# # rm(h3.dat, p.dat)
# 
# # rename synethetic audio files for m4a conv and audio concat
# dat = dat %>% 
#   mutate(new_filename = paste0(wav_order, "_", rid, ".wav")) %>% 
#   arrange(new_filename)
# 
# for(i in seq_along(dat$wav_order)){
#   
#   if(dat$wav_order[i] == 0){
#     # remove any files that still have [wav_order] = 0
#     file.remove(paste0("api-out-stage/",
#                        dat$rid[i], ".wav")
#                 )
#   }else{
#     
#   }
#   
  
# Prep: Blob Encoding ----
blob.header %>% 
googleLanguageR::gl_talk(input = ., 
                         inputType = "ssml",
                         # name = "en-US-Wavenet-D", # preferred
                         name = "en-US-Wavenet-B", # second-best
                         output = paste0("api-out-stage/", 
                                         "000_intro",
                                         ".wav"),
                         audioEncoding = "LINEAR16",
                         effectsProfileIds = "headphone-class-device"
)

blob.footer %>% 
  googleLanguageR::gl_talk(input = ., 
                           inputType = "ssml",
                           # name = "en-US-Wavenet-D", # preferred
                           name = "en-US-Wavenet-B", # second-best
                           output = paste0("api-out-stage/", 
                                           "999_footer",
                                           ".wav"),
                           audioEncoding = "LINEAR16",
                           effectsProfileIds = "headphone-class-device"
  )
  
# Encoding conversions and audio concatenation ----
# Convert WAV to m4a
# ... dump to "convert-stage/" dir

# convert wav files to m4a 
base::shell(cmd = 'cd api-out-stage\\\ && FOR /F "tokens=*" %G IN (\'dir /b *.wav\') DO ffmpeg.exe -i "%G" "%~nG.m4a"')

# merge converted m4a files
dir(path = "api-out-stage/", pattern = ".m4a$") %>% 
  paste0("file '",.,"'") %>% 
  writeLines(text = ., con = "api-out-stage/ref.txt") # make reference file with targets

output_file_name = article_target$by_line %>% # prep output file name
  iconv(x = ., from = "UTF-8", to = "ASCII//TRANSLIT") %>% 
  gsub(pattern = "[[:punct:]]", "", .) %>% # NO PUNCT ALLOWED
  stringr::str_to_title() %>%
  gsub(pattern = "\\s+", replacement = "", .) %>% # NO SPACES ALLOWED
  paste0("meduza-", ., ".m4a")

output_file_name %>%
  paste("cd api-out-stage/ && ffmpeg.exe -f concat -safe 0 -i ref.txt -c copy", .) %>%
  base::shell(cmd = .) # finally merge

# move
output_file_name %>% 
  paste("cd api-out-stage/ && move", ., "../landing/") %>% # move final output file
  shell(cmd = .)
base::shell("cd api-out-stage/ && move *.wav ../exhaust/") # move original API wav output
base::shell("cd api-out-stage/ && move *.m4a ../exhaust/") # move converted m4a's

# dat$new_filename %>%
#   paste0('"',.,'"') %>%
#   paste("cd input-test/ && move", ., "../exhaust/") %>% # move input file
#   base::shell(cmd = .)

# # Publish to Castos -----
# post.url = paste0("http://app.castos.com/api/v2/podcasts/",
#                   keyring::key_get(service = "castos.meduza-en-vhf.podcast-id"),
#                   "/episodes"
#                   )
# # Make post content
# # template
# post.content = "<p>Source: <a href = \"POST_LINK\">POST_LINK</a></p><p>\"POST_CONTENT\"</p>" 
# 
# # update
# post.content = post.content %>% 
#   gsub(pattern = "(POST_LINK)", 
#        replacement = paste0("https://meduza.io/", article_target$link[1]), 
#        x = .) %>% 
#   gsub(pattern = "(POST_CONTENT)", 
#        replacement = p.dat$raw[1], 
#        x = .)
# 
# # Make post title
# post.title = paste0(article_target$by_line[1], ": ",
#                     trimws(
#                       str_sub(
#                         string = article_target$full_title[1],
#                         start = nchar(article_target$by_line[1]) + 1,
#                         end = -1L
#                       )
#                     ))
# 
# # Make post file
# # post.file_path = system.file(paste0("landing/",
# #                                    output_file_name))
# # post.file_path = paste0("file=@", getwd(),
# #                         "/landing/",
# #                         output_file_name)
# 
# post.file_path = paste0(getwd(),
#                         "/landing/",
#                         output_file_name)
# 
# # post.file_path = httr::upload_file(post.file_path)
# 
# # post.file_con = file(post.file_path, "rb")
# # 
# # post.file = base::readBin(con = post.file_con, what = raw())
# 
# # Make post request
# post.query = list(token = keyring::key_get(service = "castos-token"))
# # post.body = list('form[post_title]' = post.title,
# #                  'form[post_content]' = post.content,
# #                  'form[episode_file]' = post.file_path
# #                  )
# post.body = list(post_title = post.title,
#                  post_content = post.content,
#                  episode_file = post.file_path,
#                  episode_file = httr::upload_file(post.file_path)
# )
# 
# a = httr::POST(url = post.url, 
#            query = post.query, 
#            body = post.body)
# 
# # a = httr::GET(url = "http://app.castos.com/api/v2/podcasts/27451/episodes", query = request)








