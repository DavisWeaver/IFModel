NoTreatmentLifeExpectancy <- function(directory, sex, comorbidity, start.age = 66){   ###Initialize function###
  if(start.age < 66) {
    return("Please input a start age between 66 and 80")                   ###Return errors if the user inputs unrecognized
  }
  
  if(start.age > 80) {
    return("Please input a start age between 66 and 80")
  }
  
  if(!(sex %in% c("Men", "Women"))) {
    return("Please input either Men or Women")
  }
  if(!(comorbidity %in% c("No", "Low", "Moderate", "Severe"))) {
    return(" Please input No, Low, Moderate, or Severe")
  }
  
  #start.age <- 66
  #datasources.directory <- "C:/Users/DavisWeaver/Documents/R/DataSources"     ####Uncomment these lines to debug 
  #directory <- datasources.directory
  #sex <- "Women"
  #comorbidity <- "Severe"
  setwd(directory)
  file.name <- paste(sex, comorbidity, "ComorbidityLifeTable.csv", sep = "")                           #Define Filename
  life.table <- read.csv(file.name, header = TRUE)                                                     #Load data tables
  cancermortality.table <- read.csv("AllCancerIncidenceBasedMortality.csv")
  rccmortality.table <- read.csv("RCCIncidenceBasedMortality.csv")
  nofollowbenefit.range <- c(start.age + 6, start.age + 7, start.age + 8, start.age + 9, start.age + 10, start.age + 11, start.age + 12, start.age +13)
  
  
  cycle.outputs <- c()                                                                  ##initialize output vector
  startcyclealive.proportion <- 1                                                       ##Set initial proportion
  
  
  for(i in start.age:120) {
    if(sex == "Men") {
      rccmortality.rate <- rccmortality.table[i, 3]                                                   
      cancermortality.rate <- cancermortality.table[i, 2]
    }
    else {
      rccmortality.rate <- rccmortality.table[i, 2]
      cancermortality.rate <- cancermortality.table[i, 3]
    }
    comorbidityadjusteddeath.rate <- life.table[(i - 59), start.age - 64]
    
    if(i < (start.age + 6)) {
      pDie <- 1 - exp(-1*(comorbidityadjusteddeath.rate + (cancermortality.rate - rccmortality.rate)))          #Define pDie for each cycle, patients are spared rccmortality.rate for the duration of the follow-up period
    }
    else if(i %in% nofollowbenefit.range) {                                                                    #Protection ceases from follow-up but cancer must be added back in to account for deficiencies in life tables
      pDie <- 1 - exp(-1*(comorbidityadjusteddeath.rate + cancermortality.rate))
    }
    else {
      pDie <- 1 - exp(-1*(comorbidityadjusteddeath.rate))
    }
    #if(i > (start.age + 13)) {
    # pDie <- 1 - exp(-1*(comorbidityadjusteddeath.rate))
    #}
    
    
    finishcyclealive.proportion <- (startcyclealive.proportion)*(1-pDie)               #Calculate what proportion of the cohort will finish alive in a given cycle
    cycle.reward <- 1 * finishcyclealive.proportion
    cycle.outputs <- c(cycle.outputs, cycle.reward)                                    #Add this proportion to the Cycle_Outputs vector, multiply by 1 to compute the contribution of that cycle to the overall life expectancy calculation. 
    startcyclealive.proportion <- finishcyclealive.proportion                          #Reset Start_Alive to carry over proportion alive from previous cycle.
    
    if(isTRUE(all.equal(finishcyclealive.proportion, 0, tolerance = 0.000001))) {                                             #finish loop when there are no surviving patients
      break 
    }
  }
  
  sum(cycle.outputs) + 0.5 ##half cycle correction
}
