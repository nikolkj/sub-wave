#!/usr/bin/Rscript
setwd(Sys.getenv("MEDUZA_PATH"))
warning(paste("RUN TIME:", Sys.time()))
warning(paste("WORKING DIR:", getwd()))
source(file = "meduza_check-home.R") # check home page, update queue
check_queue = readRDS(file = "meduza-processing-queue.rds") # get updated queue
# View(check_queue)
check_queue = check_queue$processed


while(FALSE %in% check_queue){
  print(paste("Files to process:", sum(!check_queue, na.rm = TRUE))) # print queue length to console
  source(file = "meduza_synthesize-article.R") # process article
  check_queue = readRDS(file = "meduza-processing-queue.rds") # get updated queue
  check_queue = check_queue$processed
  
  
}

