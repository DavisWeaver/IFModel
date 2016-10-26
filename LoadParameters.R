LoadParameters <- function(directory, sex, comorbidity){

  setwd(directory)
#sex <-  "Men"                                       ##Uncomment to debug
#comorbidity <- "No"
start.age <- 66

  file.name <- paste(sex, comorbidity, "ComorbidityLifeTable.csv", sep = "")                           #Define Filename
  life.table <- read.csv(file.name, header = TRUE)                                                     #Load data tables
  
  ### Load tables ###
  
  cancermortality.table <- read.csv("AllCancerIncidenceBasedMortality.csv")
  rccmortality.table <- read.csv("RCCIncidenceBasedMortality.csv")
  nofollowbenefit.range <- c(start.age + 6, start.age + 7, start.age + 8, start.age + 9, start.age + 10, start.age + 11, start.age + 12, start.age +13)
  
  ### Define Parameters ###
  
  pMaligancy <- 0.075
  latencyperiod <- 2
  
  
}