library(mice)
setwd("/home/gernot/pCloud/University_Graz/Research/Papers/20_Political_Prediction/Data_Analysis/")
# see https://datascienceplus.com/imputing-missing-data-with-r-mice-package/


SOURCE_DATA_FILE_PATH = "/data/base_data/recategorized_4l3c4r/"
DATA_FILE_PATH = paste(".", SOURCE_DATA_FILE_PATH, sep="")
TARGET_DATA_FILE_PATH_CART = "/data/imputed_data/recategorized_4l3c4r/imputation_cart/"
if (file.exists(TARGET_DATA_FILE_PATH_CART)){} else {
  dir.create(paste(getwd(), TARGET_DATA_FILE_PATH_CART, sep=""), recursive = TRUE)
}
TARGET_DATA_FILE_PATH_PMM = "/data/imputed_data/recategorized_4l3c4r/imputation_pmm/"

if (file.exists(TARGET_DATA_FILE_PATH_CART)){} else {
  dir.create(paste(getwd(), TARGET_DATA_FILE_PATH_PMM, sep=""), recursive = TRUE)
}

base_files <- list.files(path=DATA_FILE_PATH, pattern="_X_")

for (x in base_files) {
  data <- read.csv(paste(getwd(), SOURCE_DATA_FILE_PATH, x, sep = "")) # read data
  print(x)
  imputed_data <-  mice(data, method="cart") # method cart imputation
  imputed_data_complete <- complete(imputed_data,1)
  write.csv(imputed_data_complete, paste(getwd(), 
                                         TARGET_DATA_FILE_PATH_CART, x, 
                                         sep = ""), row.names=FALSE)
  rm(imputed_data, imputed_data_complete)
}

for (x in base_files) {
  data <- read.csv(paste(getwd(), SOURCE_DATA_FILE_PATH, x, sep = "")) # read data
  print(x)
  imputed_data <-  mice(data,m=5,maxit=50,meth='pmm',seed=500) # method pmm imputation
  imputed_data_complete <- complete(imputed_data,1)
  write.csv(imputed_data_complete, paste(getwd(), 
                                         TARGET_DATA_FILE_PATH_PMM, x, 
                                         sep = ""), row.names=FALSE)
  rm(imputed_data, imputed_data_complete)
}




