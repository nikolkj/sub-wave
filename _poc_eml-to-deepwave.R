# ABOUT: Parse EML files and convert to SSML
#
# Deepwave API Limits:
# * Total characters per request: 5,000
# * Requests per minute: 300
# * Characters per minute: 150,000
# --
# THEREFORE
# ... input has to chunked into blocks of <5K characters, including SSML 
# .... and input has to throttled to 500 blocks per minute

# Start Up ----
rm(list = ls());
require(tidyverse)
require(magrittr)

# Inventory Input Queue ---- 
# grab input file 
input_file_name = dir(path = "input-test/", pattern = ".eml$")[1]

dat = paste0("input-test/", input_file_name) %>%
  readLines(con = ., warn = FALSE) 

# Generate Intro Blob ----
# get post author from sender tag
post_author = grep("(?i)^\\s*sender:.*@.*", dat, value = TRUE)[1] %>% 
  trimws() %>% 
  gsub("^(?i)sender:\\s*", "", .) %>% 
  gsub('"',"",.) %>% 
  gsub('"',"",.) %>% 
  gsub("\\s*<.*>\\s*$", "", .) %>%
  trimws()

# get post title from <title> tag
post_title = grep("<title>", dat) %>% 
  c((.-5):(.+5)) %>% unique() %>% sort() %>%  # lazy
  dat[.] %>% gsub("=$", "", .) %>%
  paste0(., collapse = "") %>% 
  str_extract(string = ., pattern = "<title>.*</title>") %>%
  gsub("(<title>)|</title>", "", .) %>%
  trimws() 

# get post date
post_date.raw = grep("(?i)^\\s*date:", dat, value = TRUE)[1] %>% 
  trimws() %>%
  str_extract("[A-z]{3}, \\d{1,2} [A-z]{3} \\d{4}") %>%
  str_remove(., "[A-z]{3},\\s+") %>% 
  as.Date(., "%d %b %Y")

assertthat::assert_that(assertthat::is.date(post_date.raw))
post_date.dow = base::weekdays(post_date.raw)
post_date.raw = as.character(post_date.raw)

# get post link
post_link = grep("(?i)^\\s*list-url:", dat, value = TRUE)[1] %>%
    gsub("(?i)^\\s*list-url:\\s*", "", .) %>%
    gsub("(?i)<https://www.",  "", .) %>% 
    gsub("/>.*",  "", .)


assertthat::assert_that(length(post_title) + # both objs w/ len = 1
                          length(post_author) + 
                          length(post_date.dow) + 
                          length(post_date.raw) + 
                          length(post_link) == 5) 

# make intro ssml
intro_blob = paste0("<speak>",
                    "This is a reeding of <break strength=\"strong\"/><emphasis level=\"x-strong\">\"", post_title, 
                    "\"</emphais> by <break strength=\"strong\"/>", post_author, 
                    ". Published through Substack on ", post_date.dow, ", ",
                    "<say-as interpret-as=\"date\" format=\"yyyymmdd\">", post_date.raw,"</say-as>.",
                    "Enjoy this, and other writings at ", post_link,
                    " <break strength=\"x-strong\"/> .",
                    "</speak>"
                    )

assertthat::assert_that(nchar(intro_blob) < 4999L)

# Extract EML Body ----
# pull email body
content_boundary.raw = grep(pattern = "Content-Type: multipart/alternative; boundary=",
                            x = dat, value = TRUE) %>% 
  str_remove(string = ., pattern = "Content-Type: multipart/alternative; boundary=") %>%
  str_remove_all(string = ., pattern = "\"")

dat.body_bounds = grep(pattern = content_boundary.raw, 
                       x = dat, value = FALSE)
  # dat.body_bounds[2:3] plain-text email body
  # dat.body_bounds[3:4] HTML formatted email body


dat.body_raw = dat[dat.body_bounds[2]:dat.body_bounds[3]] # grab everything between boundary lines, plain-text
dat.body_raw = dat.body_raw[-c(1, length(dat.body_raw))] # drop boundary lines

# remove body header
dat.body_raw = dat.body_raw[-grep(pattern = "^Content-Type: text/plain",
                                  x = dat.body_raw)[1]]
dat.body_raw = dat.body_raw[-grep(pattern = "^Mime-Version:",
                                  x = dat.body_raw)[1]]
dat.body_raw = dat.body_raw[-grep(pattern = "^Content-Transfer-Encoding: quoted-printable",
                                  x = dat.body_raw)[1]]
# remove body footer
dat.footer_min1 = which(dat.body_raw == "") # find possible start points
dat.footer_min2 = grep(pattern = "(?i)^unsubscribe", # find confirmation points
                       x = dat.body_raw)
dat.footer_min = max(intersect(dat.footer_min1, # match footer criteria
                               (dat.footer_min2-1)
                               )
                     )

  # did we find the footer? 
  assertthat::assert_that(is.numeric(dat.footer_min), 
                          length(dat.footer_min) == 1
                          )
  
dat.body_raw = dat.body_raw[-c(dat.footer_min:length(dat.body_raw))] # remove 
  
# clean-up
rm(dat, content_boundary.raw, dat.body_bounds,
   dat.footer_min, dat.footer_min1, dat.footer_min2)

# unique body id, used for batch processing
eml_digest = digest::digest(object = dat.body_raw, serialize = TRUE, algo = "md5")

# EML Preprocesing ----

# >>> misc fixes
# "=20=" invalid hexstring handling
# ... causes <break>'s when EOL
dat.body_raw = gsub(pattern = "=20=$", replacement = " =", x = dat.body_raw)

# >>> merge run-on lines
# body_line_max = max(nchar(dat.body_raw)) # doesn't work, exceptions exist
  # ... e.g. one line at 78 characters ending in "=20"
body_line_max = nchar(dat.body_raw) %>% 
  plyr::count() %>% 
  filter(freq == max(freq)) %$% x

# truncate to consensus length
dat.body_raw = substr(x = dat.body_raw, start = 1, stop = body_line_max) 

# find run-on lines
# ...definition:
# .... 1) number of characters equals "body_line_max" (?)
# .... 2) line ends in "="
dat.body_runon = which(nchar(dat.body_raw) == body_line_max) # body message max-length
dat.body_runon = intersect(dat.body_runon, grep("=$", dat.body_raw))
dat.body_runon = grep("=$", trimws(dat.body_raw))

dat.body = rep("", length(dat.body_raw))
j = 1
for(i in seq_along(dat.body_raw)){
  if((i-1) %in% dat.body_runon){
    j = j - 1 # step-backward
    
    # break
    dat.body[j] = paste0(sub("=$", "", dat.body[j]), # lazy, excessive
                        sub("=$", "", dat.body_raw[i])
                        )
    j = j + 1 # step-forward
    
  }else{
    # accept as-is
    dat.body[j] = dat.body_raw[i]
    j = j + 1 # itterate forward
    
  } 
  
}

dat.body = dat.body[dat.body !=  ""] # drop empty elements

# >>> Remove embedded links
dat.body = str_remove_all(string = dat.body, 
                          pattern = "\\[https://.*\\]") 

dat.body = str_replace_all(string = dat.body, 
                          pattern = "https://.*($|\\s)", # lazy, excessive
                          replacement = " ") %>% 
  trimws()

  # writeLines(text = dat.body, con = "test.txt") # test


# >>> Convert hexidecimal to UTF-8
# load punctuation reference table
ref = readxl::read_excel(path = "unicode-hexstring_general-punct.xlsx")
ref = ref %>% janitor::clean_names() %>%
  select(character, utf_8_hex)

dat.body_hex = lapply(dat.body, function(x){
  str_extract_all(string = x, 
                  pattern = "(=[[:alnum:]]{2})+", simplify = TRUE)
})

# create small translation table
body_hex.translate = dat.body_hex %>% unlist() %>% 
  plyr::count() %>% 
  mutate(hex = str_replace_all(string = x, 
                               pattern = "[^[:alnum:]]", 
                               replacement = " ") %>% 
           str_squish() %>%
           tolower()
         ) %>% 
  left_join(x = ., 
            y = ref,
            by = c("hex" = "utf_8_hex")) %>%
  # UNKNOWN to BLANK SPACE, address later
  mutate(character = ifelse(is.na(character), " ", character)) %>%
  
  # manual encodings
  mutate(character = ifelse(x == "=20", "@@", character) # eml uses end-of-line "20" hexstring space to indicate paragraph break 
         # ... add as necessary
         )

# translate
for(i in seq(nrow(body_hex.translate))){
  dat.body = gsub(pattern = body_hex.translate$x[i], 
                       replacement = body_hex.translate$character[i], 
                       x = dat.body)  
}

rm(i, j, dat.body_runon, ref, dat.body_hex, body_hex.translate, body_line_max) # minor clean-up

# >>> misc 
dat.body = iconv(x = dat.body, from = "UTF-8", to = "ASCII//TRANSLIT") # covert utf8 to ASCII
dat.body = str_squish(dat.body) # clean-up whitespace

# lazy removal of superflous string
dat.body = dat.body[-grep("(?i)View this post on the web at", dat.body)[1]] #LaAaAaAaZzzyY 

# tag title-lines for downstream processing
# ... must be done before punctuation repair
# .... because subtitle lines are assumed to be those that 
# .... are not punctuated.

title_lines = dat.body %>% grep("([.?!:]+)|(@@)$", x = .,
                                invert = TRUE, value = FALSE)

dat.body[title_lines] = paste0("#$#", # start-marker
                               dat.body[title_lines], 
                               "#%#") # end-marker

# fix any missing punctuation
missing_punct = grep("[[:punct:]]$", dat.body, invert = TRUE)
dat.body[missing_punct] = paste0(dat.body[missing_punct], ".")

  # writeLines(text = dat.body, con = "test.txt")
rm(missing_punct)

# Create Initial Blocks ----
# ... by sentence, punctuation dependent.
dat.body = dat.body %>% paste0(., collapse = " ") %>% unlist()

dat.body_punct = str_extract_all(string = dat.body, pattern = "[.?!]+\\s+") %>% unlist()
dat.body = str_split(string = dat.body, 
                     pattern = "[.?!]+\\s+", simplify = T) %>%
  unlist()

dat.body = c(paste0(dat.body[-length(dat.body)], dat.body_punct), 
             dat.body[length(dat.body)] # retained final punctuation
             )

dat.body = trimws(dat.body)

rm(dat.body_punct) # clean-up

# Simple SSML Encodings ----
dat.body_ssml = dat.body

# paragraph-breaks to <break>
dat.body_ssml = gsub("(@@)", " <break strength=\"x-strong\"/> ", x = dat.body_ssml)
assertthat::assert_that(max(nchar(dat.body_ssml)) < 5000L,
                        msg = "paragraph-breaks -> <break>: max(nchar(dat.body_ssml)) must be less than 5000L.")

# subtitle-line to <emphasis> with <break>
dat.body_ssml = gsub("(#\\$#)", "<break strength=\"x-strong\"/><emphasis level=\"strong\">", x = dat.body_ssml)
dat.body_ssml = gsub("(#%#)", "</emphasis><break strength=\"x-strong\"/>", x = dat.body_ssml)
assertthat::assert_that(max(nchar(dat.body_ssml)) < 5000L,
                        msg = "title-line -> <emphasis><break>: max(nchar(dat.body_ssml)) must be less than 5000L.")


# # slash to <break strength="medium"> ## !! EXCESSIVE !!
# # ... MUST BE FIRST TO AVOID CLASHES 
# dat.body_ssml = gsub("/", " <break strength=\"weak\"/> ", x = dat.body_ssml)
# assertthat::assert_that(max(nchar(dat.body_ssml)) < 5000L, 
#                         msg = "slash -> <break strength=\"weak\">: max(nchar(dat.body_ssml)) must be less than 5000L.")


# # commas to <break strength="medium"> ## !! EXCESSIVE !!
# dat.body_ssml = gsub(", ", " <break strength=\"medium\"/> ", x = dat.body_ssml)
# assertthat::assert_that(max(nchar(dat.body_ssml)) < 5000L, 
#                         msg = "comma -> <break strength=\"medium\">: max(nchar(dat.body_ssml)) must be less than 5000L.")

# Complex SSML Encodings ----
# ... TBD 


# Optimize Block-lengths ---- 
block_buffer = nchar(paste0("<speak>","</speak>")) # must include all tags include in sec. -"Wrapper SSML Encodings"-
block_target = 4998L - block_buffer - length(dat.body_ssml) # gives (2) char error margin, accounts for wrapping tags and spaces introduced downstream
block_sizes = nchar(dat.body_ssml)

dat.body_ssml.merge = rep(character(), length = length(dat.body_ssml)) # preallocate
j = 0; orig_len = length(dat.body_ssml)
new_start = 1; new_end = 0; # block indices reference to "dat.body_ssml" initial blocks
for(i in seq_along(dat.body_ssml)){
  
  if(new_end >= length(dat.body_ssml)){
    break
  }
  
  block_length = dat.body_ssml[new_start : orig_len] %>% 
    nchar() %>% cumsum()
  
  new_end = (which(block_length < block_target) %>% max()) + (new_start-1)

  # print(c(new_start, new_end)) # check
  
  j = j + 1 # itterate forward on "dat.body_ssml.merge"
  dat.body_ssml.merge[j] = paste(dat.body_ssml[new_start:new_end], collapse = " ") # merge original blocks
  
  # reset search
  new_start = new_end + 1

}

# overwrite original
dat.body_ssml = dat.body_ssml.merge[which(!is.na(nchar(dat.body_ssml.merge)))] # ignores empties

# clean-up
rm(list = c(ls(pattern = "block"), 
            "i", "j", 'new_start', 'new_end', "orig_len")
   )

# Wrapper SSML Encodings ----
# ... any ssml encodings at the beginning or end of a block
# # <break> weak ... end of block ## !! DOESN'T IMPACT CLIP LENGTH !!
# dat.body_ssml = paste0(dat.body_ssml, "<break strength=\"weak\"/>")
# assertthat::assert_that(max(nchar(dat.body_ssml)) < 5000L, 
#                         msg = "<speak>: max(nchar(dat.body_ssml)) must be less than 5000L.")

# <speak>
dat.body_ssml = paste0("<speak>", dat.body_ssml, "</speak>")
assertthat::assert_that(max(nchar(dat.body_ssml)) < 5000L, 
                        msg = "<speak>: max(nchar(dat.body_ssml)) must be less than 5000L.")

# DeepWave Send ----

# input-output tracking table
dat = tibble(ssml_batch = eml_digest,
             ssml_order = seq_along(dat.body_ssml), 
             ssml_input = dat.body_ssml,
             ssml_id = NA
             )

dat$ssml_id = sapply(dat$ssml_input, digest::digest, algo = "md5")

# for linear16
dat$ssml_output_file = stringr::str_pad(string = dat$ssml_order,
                                        width = 4, side = "left", pad = "0") %>%
  paste0(dat$ssml_batch, "_", ., "_", dat$ssml_id, ".wav")

# add intro-blob record
intro_file_name = paste0(dat$ssml_batch[1], "__intro", ".wav") 
dat = dat %>% 
  # add record for final output file
  add_row(ssml_batch = dat$ssml_batch[1], 
          ssml_order = -2, # reserved
          ssml_output_file = intro_file_name,
          ssml_input = intro_blob,
          ssml_id = digest::digest(object = intro_blob, algo = "md5")
          )

# check that all audio files have been removed from staging directory
assertthat::assert_that(length(dir(path = "api-out-stage/", 
                                   pattern = "(wav)$|(mp3)$|(m4a)$")) == 0 )

# API authentication
googleLanguageR::gl_auth(json_file = "sub-wave-1342c1e4cfc4.json")

# process intro blob
googleLanguageR::gl_talk(input = intro_blob, 
                         inputType = "ssml",
                         # name = "en-US-Wavenet-D", # preferred
                         name = "en-US-Wavenet-B", # second-best
                         output = paste0("api-out-stage/",
                                         dat$ssml_output_file[which(dat$ssml_order == -2)]
                                         ),
                         audioEncoding = "LINEAR16",
                         effectsProfileIds = "headphone-class-device"
)

# process main reading
for(i in seq(nrow(dat))){
   if(dat$ssml_order[i] < 1){next} # skip any non-main entries
   googleLanguageR::gl_talk(input = dat$ssml_input[i], 
                           inputType = "ssml",
                           name = "en-US-Wavenet-D", # preferred
                           # name = "en-US-Wavenet-B", # second-best
                           output = paste0("api-out-stage/", dat$ssml_output_file[i]),
                           audioEncoding = "LINEAR16",
                           effectsProfileIds = "headphone-class-device"
                           )
   Sys.sleep(5)
  
}

# Encoding Conversions and File Concatenation ----
# Convert WAV to m4a
# ... dump to "convert-stage/" dir

# convert
base::shell(cmd = 'cd api-out-stage\\\ && FOR /F "tokens=*" %G IN (\'dir /b *.wav\') DO ffmpeg.exe -i "%G" "%~nG.m4a"')

# merge 
dir(path = "api-out-stage/", pattern = ".m4a$") %>% 
  paste0("file '",.,"'") %>% 
  writeLines(text = ., con = "api-out-stage/ref.txt") # make reference file with targets

output_file_name = paste0(dat$ssml_batch[i], ".m4a") 
output_file_name %>%
  paste("cd api-out-stage/ && ffmpeg.exe -f concat -safe 0 -i ref.txt -c copy", .) %>%
  base::shell(cmd = .) # merge

# move
output_file_name %>% 
  paste("cd api-out-stage/ && move", ., "../landing/") %>% # move final output file
  shell(cmd = .)
base::shell("cd api-out-stage/ && move *.wav ../exhaust/") # move original API wav output
base::shell("cd api-out-stage/ && move *.m4a ../exhaust/") # move converted m4a's

input_file_name %>% 
  paste0('"',.,'"') %>% 
  paste("cd input-test/ && move", ., "../exhaust/") %>% # move input file
  base::shell(cmd = .)

# archive processing table
dat %>% 
  # add record for final output file
  add_row(ssml_batch = dat$ssml_batch[1], 
          ssml_order = 0, # reserved
          ssml_output_file = output_file_name) %>% 
  # add record for input file
  add_row(ssml_batch = dat$ssml_batch[1],
          ssml_order = -1, # reserved
          ssml_input = input_file_name) %>%
  writexl::write_xlsx(x = ., 
                      path = paste0("exhaust/",
                                    gsub("m4a$", "xlsx", output_file_name)))

# Write About Files
writeLines(text = paste(toupper(post_link), 
                        stringr::str_to_title(string = post_title), 
                        sep =  " - "), 
           con = paste("landing-info/", dat$ssml_batch[i], "_title.txt"))












