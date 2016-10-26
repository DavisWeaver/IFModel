NoTreatmentLifeExpectancy <- function(directory, sex, comorbidity, start.age = 66, stage = "All", latency.period = 2, malignancy.probability = 0.0803){   ###Initialize function###
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
  if(!(stage %in% c("All", "Local", "Regional", "Advanced"))) {
    return("Please input All, Local, Regional, or Advanced")
  }
  
  #start.age <- 70 
  #datasources.directory <- "C:/Users/DavisWeaver/Documents/R/DataSources"     ####Uncomment these lines to debug 
  #directory <- datasources.directory
  #sex <- "Men"
  #comorbidity <- "Moderate"
  #stage <- "All"
  #malignancy.probability <- 0.0803
  #latency.period <- 2
  
  setwd(directory)
  
  ### Load Tables ###
  
  life.table <- read.csv(paste(sex, comorbidity, "ComorbidityLifeTable.csv", sep = ""))
  cancermortality.table <- read.csv("AllCancerIncidenceBasedMortality.csv")
  rccmortality.table <- read.csv("RCCIncidenceBasedMortality.csv")
  rccsurvival.table <- read.csv(paste(sex, stage, "RCCSurvival.csv", sep = ""))
  
  ### Define Key Parameters ###
  
  
  
  
  ### Initialize Output Vectors for each health state ###
  
  alive.outputs <- c()
  diseasefree.outputs <- c()
  cancer.outputs <- c()
  pDieRCC.outputs <- c()
  pDie.outputs <- c()
  
  
  #diseasefree.outputs <- c()                                                                  
  #incidentalfinding.outputs <- c()
  #cancer.outputs <- c()
  
  ### Set Initial Proportions for each health state ###
  
  startcyclediseasefree.proportion <- 0                                                       
  startcycleincidentalfinding.proportion <- 1
  startcyclecancer.proportion <- 0
  startcycledead.proportion <- 0
  startcycledeadfromcancer.proportion <- 0
  
  
  for(i in 1:120) {
  #i =  3                  ##Uncomment to debug 
     ### Define Rates ###
    if(sex == "Men") {
      rccmortality.rate <- rccmortality.table[(i + start.age), 3]                                                   
      cancermortality.rate <- cancermortality.table[(i + start.age), 2]
    }
    if(sex == "Women") {
      rccmortality.rate <- rccmortality.table[(i + start.age), 2]
      cancermortality.rate <- cancermortality.table[(i + start.age), 3]
    }
    
    comorbidityadjusteddeath.rate <- life.table[(i + start.age - 60), start.age - 64]
    
    if((i - latency.period) %in% c(1:10)) {                                                                          ###Prevent NAing out by setting rccdeath.rate to zero in cases when there is no data
      rccdeath.rate <- rccsurvival.table[(i - latency.period), (start.age + latency.period - 58)]
    } 
    if(!(i - latency.period) %in% c(1:10)) {
      rccdeath.rate <- 0
    }
    
    ### Define Probabilities ###
    
    if(i <=  14) {
                                                                
      pDie <- 1 - exp(-1*(comorbidityadjusteddeath.rate + cancermortality.rate))
    }
    else {
      pDie <- 1 - exp(-1*(comorbidityadjusteddeath.rate))
    }
     
    pDieRCC <- 1 - exp(-1*(rccdeath.rate))                                               ##Calculate annual probability of dying while in the RCC arm
 
    ### Run Markov ###
    
    ### Account for effect of Latency period on 
    
    if(latency.period == 0) {
      if(i == 1)  {cancersurvivors.proportion <- malignancy.probability*(1-pDieRCC)}
      if(i != 1) {cancersurvivors.proportion <- startcyclecancer.proportion*(1-pDieRCC)}
    }
    if(latency.period != 0){
      if(i >= (latency.period + 1)) {
        cancersurvivors.proportion <- startcyclecancer.proportion*(1-pDieRCC)               
      }
      else {
        cancersurvivors.proportion <- startcyclecancer.proportion
      }
    }
  
    ##cancersurvivors.proportion <- CalculateCancerSurvivors(i, latency.period, start.age)
    
    ### Calculate health states for each cycle ###
    

    survivecyclecancer.proportion <- if(latency.period != 0) {startcycleincidentalfinding.proportion*malignancy.probability + cancersurvivors.proportion} else {cancersurvivors.proportion}                             #Calculate proportion of cohort with cancer that doesn't die from cancer
    finishcyclecancer.proportion <- survivecyclecancer.proportion*(1 - pDie)                                                                                                                          #Calculate proportion of cohort that survives cycle overall
    
    if(i == 1) {finishcyclediseasefree.proportion <- (1 - malignancy.probability)*(1 - pDie)}
    else {finishcyclediseasefree.proportion <- startcyclediseasefree.proportion*(1 - pDie)}
    
    finishcyclealive.proportion <- (finishcyclediseasefree.proportion + finishcyclecancer.proportion)                                                                                          #Calculate what proportion of the cohort will finish alive in a given cycle                                                                                         #Add this proportion to the Cycle_Outputs vector, multiply by 1 to compute the contribution of that cycle to the overall life expectancy calculation. 
    finishcycleincidentalfinding.proportion <- 0
    
    ### Populate outcome vectors ###
  
    alive.outputs <- c(alive.outputs, finishcyclealive.proportion)
    diseasefree.outputs <- c(diseasefree.outputs, finishcyclediseasefree.proportion)
    cancer.outputs <- c(cancer.outputs, finishcyclecancer.proportion)
    pDie.outputs <- c(pDie.outputs, pDie)
    pDieRCC.outputs <- c(pDieRCC.outputs, pDieRCC)
    
    ### Reset health states to carry over proportion alive from previous cycle. ###
    startcyclealive.proportion <- finishcyclealive.proportion
    startcyclecancer.proportion <- finishcyclecancer.proportion
    startcyclediseasefree.proportion <- finishcyclediseasefree.proportion
    startcycleincidentalfinding.proportion <- finishcycleincidentalfinding.proportion
    
    if(isTRUE(all.equal(finishcyclealive.proportion, 0, tolerance = 0.00000001))) {                                             #finish loop when there are no surviving patients
      break 
    }
  }
  
  sum(alive.outputs) + 0.5    ## calculate life expectancy w/ half cycle correction
}
