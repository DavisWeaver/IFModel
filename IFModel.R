rm(list = ls())
source('~/R/Projects/IFModel/IFModelTreatmentArm.R')
source('~/R/Projects/IFModel/IFModelNoTreatmentArm.R') 
datasources.directory <- "C:/Users/DavisWeaver/Documents/R/DataSources"   ##uncomment to define target directory in the global environment

age.range <- 66:80
malignancy.range <- seq(0.033415089, 0.116794, 0.0139)
### Initialize Output Vectors ###

nofollowuplifeexpectancy <- c()
followuplifeexpectancy <- c()
treatmentbenefit <- c() 
age <- c()
comorbidity <- c()
comorb <- c("No", "Low", "Moderate", "Severe")

for(i in age.range) {                         ##Run model for ages 66-80

  for(j in 1:length(comorb)) {

    ###Calculate No Treatment Life Expectancy###

    nofollowup.lifeexpectancy <- NoTreatmentLifeExpectancy(directory = datasources.directory,"Men", comorb[j], i)

    ###Calculate Treatment Life Expectancy ###

    followup.lifeexpectancy <- TreatmentLifeExpectancy(directory = datasources.directory, "Men", comorb[j], i)

    ###Calculate Life Expectancy Difference###

    treatment.benefit <- (followup.lifeexpectancy - nofollowup.lifeexpectancy) 

    ###Populate Output Vectors ###

    nofollowuplifeexpectancy <- c(nofollowuplifeexpectancy, nofollowup.lifeexpectancy)
    followuplifeexpectancy <- c(followuplifeexpectancy, followup.lifeexpectancy)
    treatmentbenefit <- c(treatmentbenefit, treatment.benefit)
    age <- c(age, i)
    comorbidity <- c(comorbidity, comorb[j])
    
  } 
}

treatmentbenefit.years <- treatmentbenefit
treatmentbenefit.months <- treatmentbenefit*12
treatmentbenefit.days <- treatmentbenefit*365
model.outputs <- data.frame(age, comorbidity, followuplifeexpectancy, nofollowuplifeexpectancy, treatmentbenefit.years, treatmentbenefit.months, treatmentbenefit.days)