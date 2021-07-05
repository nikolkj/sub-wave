source(file = "meduza_check-home.R") # check home page, update queue

check_queue = readRDS(file = "meduza-processing-queue.rds") # get updated queue
check_queue = check_queue$processed

while(FALSE %in% check_queue){
  
  source(file = "meduza_synthesize-article.R") # process article
  check_queue = readRDS(file = "meduza-processing-queue.rds") # get updated queue
  check_queue = check_queue$processed
  
  
}