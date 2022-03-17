# Workbook 5: analyze NHANES data

# Set up
#library(foreign)
library(survey)
library(Hmisc)

demo <- sasxport.get('DEMO_I.XPT')
alco <- sasxport.get('ALQ_I.XPT')

nhanes <- merge(x = demo, y = alco, by = 'seqn', all = TRUE)

weightSum <- sum(nhanes$wtint2yr, na.rm =TRUE)

#This number represents the total US population at the time the data was derived

## Analysis
nhanes$alq151[nhanes$alq151 == 2] <- 0
nhanes$alq151[nhanes$alq151 == 7] <- NA
nhanes$alq151[nhanes$alq151 == 9] <- NA

# Create a survey design

nhanes_survey <- svydesign(
  id = ~sdmvpsu,
  nest = TRUE,
  strata = ~sdmvstra,
  weights = ~wtint2yr,
  data = nhanes
)


nhanesMean <- svymean(~alq151, nhanes_survey, na.rm = TRUE)

GenderMean <- svyby(~alq151, ~riagendr, nhanes_survey, svymean, na.rm = TRUE)
