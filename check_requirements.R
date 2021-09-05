# ABOUT
# Run to make output directories

# Check Packages ----
# Basic check, does not validate package versions
required_packages = c("tidyverse", "rvest", "googleLanguageR", "assertthat", "digest", "keyring", "httr")
installed_packages = installed.packages()

missing_packages = setdiff(required_packages, rownames(installed_packages))
for(i in seq_along(missing_packages)){
  install.packages(missing_packages[i], dependencies = TRUE)
}


# Check Directories -----
folders = list.dirs()
require_folders = c("landing-meduza", "api-out-stage", "exhaust")
missing_folders = setdiff(paste0("./", require_folders), folders)

for(i in seq_along(missing_folders)){
  dir.create(path = missing_folders[i])
}
  
  
# Check other dependencies ---- 
system_info = Sys.info()

# check for FFMPEG
if(system_info[1] == 'Windows'){
  assertthat::assert_that("ffmpeg.exe" %in% dir(path = "api-out-stage/"), 
                          msg = "You need a copy of ffmpeg.exe in ./api-out-stage/ \n see: https://ffmpeg.org/download.html")
}

if(system_info[i] == 'Linux'){
  # assumes debian-based, e.g. ubuntu, pop-os
  temp = base::system(command = "dpkg -l | grep ffmpeg", intern = TRUE)
  assertthat::assert_that(length(temp) > 0, msg = "You need to install FFMPEG \n see: https://ffmpeg.org/download.html")
  
}


