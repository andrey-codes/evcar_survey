#
# This is a script for processing the data from the Electric Car Survey

# Packages:
# =================================================================
library(plyr) 
library(dplyr) 



# Get Data
# =================================================================

# NOTE: don't forget to rename the CSV file
ev_survey_data <- read.csv('electric_car_survey.csv',
                    header = TRUE,
                    stringsAsFactors = FALSE
                    )


# Clean Data
# =================================================================

# Remove the "Preview" Columns
#    Preview columns come from when we made the survey and "previewed" it. 
#    We only want to analyze the responses from the "anonymous" channel.
#    This also cleans out that second row which has some other meta data and stuff.
ev_survey_results <- ev_survey_data %>% filter(ev_survey_data$DistributionChannel == "anonymous")



# Get just the columns we want to analyze:
ev_survey_results <- ev_survey_results[ , c("ResponseId",
                          "Q1_1", "Q1_2", "Q1_3", "Q1_4", "Q1_5", "Q1_6", "Q1_7", 
                          "Q5", "Q6","Q7","Q8","Q9","Q10","Q11","Q12","Q13","Q14"
                          )] 

# Rename Columns
# Just for better readability :)
colnames(ev_survey_results)[colnames(ev_survey_results) == "Q1_1"] <- "attr_Price"           # Q1_1	 |   attr_Price: The Price of electric cars today fits my budget.
colnames(ev_survey_results)[colnames(ev_survey_results) == "Q1_2"] <- "attr_Range"           # Q1_2	 |   attr_Range: The range of today‚Äôs electric cars fit my needs.
colnames(ev_survey_results)[colnames(ev_survey_results) == "Q1_3"] <- "attr_Practicality"    # Q1_3	 |   attr_Practicality: An electric car would be practical for my household and lifestyle.
colnames(ev_survey_results)[colnames(ev_survey_results) == "Q1_4"] <- "attr_Brand"           # Q1_4	 |   attr_Brand: Brand is an important aspect to consider
colnames(ev_survey_results)[colnames(ev_survey_results) == "Q1_5"] <- "attr_CarbonFootprint" # Q1_5	 |   attr_CarbonFootprint: The main reason I would buy an electric car is to reduce my carbon footprint.
# --------------------------
colnames(ev_survey_results)[colnames(ev_survey_results) == "Q1_6"] <- "buy_this_year"	       # Q1_6	 |   buy_this_year: I will buy a car (either gas/petrol or electric) next 24 months
colnames(ev_survey_results)[colnames(ev_survey_results) == "Q1_7"] <- "buy_electric_next"    # Q1_7	 |   buy_electric_next: The next car I buy will be an electric car.
colnames(ev_survey_results)[colnames(ev_survey_results) == "Q5"] <- "attr_Appealing"         # Q5		 |   attr_Appealing: What aspects of electric cars are most appealing to someone like you?
colnames(ev_survey_results)[colnames(ev_survey_results) == "Q6"] <- "attr_Lacking"           # Q6		 |   attr_Lacking: What is still lacking about electric cars today?
# --------------------------
colnames(ev_survey_results)[colnames(ev_survey_results) == "Q7"] <- "Age"                    # Q7		 |   Age
colnames(ev_survey_results)[colnames(ev_survey_results) == "Q8"] <- "Gender"                 # Q8		 |   Gender
colnames(ev_survey_results)[colnames(ev_survey_results) == "Q9"] <- "Education"              # Q9		 |   Education
colnames(ev_survey_results)[colnames(ev_survey_results) == "Q10"] <- "Household_Income"      # Q10	 |   Household_Income
colnames(ev_survey_results)[colnames(ev_survey_results) == "Q11"] <- "Location_Type"         # Q11	 |   Location_Type: How would you describe your location?
colnames(ev_survey_results)[colnames(ev_survey_results) == "Q12"] <- "State"                 # Q12	 |   State: What state/province do you live in?
colnames(ev_survey_results)[colnames(ev_survey_results) == "Q13"] <- "City"                  # Q13	 |   City: What City do you live in?
colnames(ev_survey_results)[colnames(ev_survey_results) == "Q14"] <- "Auto_Worker"           # Q14	 |   Auto_Worker: Do you work in the Automotive Industry?



# Convert Likert Values to numeric ------------- ------------- ------------- -------------

# Creates a numeric scale for our Likert Values
#     Do we want to do 1 - 7 or -3 to + 3?
likert_revalues <- c("Strongly Disagree" = 1,
                     "Disagree" = 2,
                     "Somewhat disagree" = 3,
                     "Neither agree nor disagree" = 4,
                     "Somewhat agree" = 5,
                     "Agree" = 6,
                     "Strongly agree" = 7)
likert_revalues <- as.numeric(likert_revalues)

# Applies this scale to all of our Likert-based columns:
ev_survey_results$attr_Price           <- revalue(ev_survey_results$attr_Price, likert_revalues)
ev_survey_results$attr_Range           <- revalue(ev_survey_results$attr_Range, likert_revalues)
ev_survey_results$attr_Practicality    <- revalue(ev_survey_results$attr_Practicality, likert_revalues)
ev_survey_results$attr_Brand           <- revalue(ev_survey_results$attr_Brand, likert_revalues)
ev_survey_results$attr_CarbonFootprint <- revalue(ev_survey_results$attr_CarbonFootprint, likert_revalues)
ev_survey_results$buy_this_year        <- revalue(ev_survey_results$buy_this_year, likert_revalues)
ev_survey_results$buy_electric_next    <- revalue(ev_survey_results$buy_electric_next, likert_revalues)

  


# See some info about this data:
dim(ev_survey_results)
head(ev_survey_results)



# Get City Population Densities
# =================================================================
# We asked people for their City and State because I have a hypothesis that population density will
# be a predictor for someone's feasibility to own an electric car.
# The dataset for the population densities by city is here: https://www.governing.com/gov-data/population-density-land-area-cities-map.html


# Normalize State Names ----------------------------------------------------------------------
# People didn't really enter their states in a consistent way, so we need to clean it up.

ev_survey_results$State <-toupper(ev_survey_results$State)  # Make Uppercase
ev_survey_results$City <-toupper(ev_survey_results$City)    # Make Uppercase

# Get the State Abreviations from a little CSV I made.
state_abbreviations <- read.csv('misc/state_abbreviations.csv', header = TRUE, stringsAsFactors = FALSE)

# Makes a data frame with the correct number of obsevations as indices:
observations <- c(1:nrow(ev_survey_results))  

# Replaces any two-letter state Abbreviations with full state names:
for ( i in observations ) {
  state_value <- ev_survey_results$State[i]
  length <- nchar(state_value)
  if (length == 2){
    state_index <- match(state_value, state_abbreviations$Abbreviation)
    ev_survey_results$State[i] <- state_abbreviations$Name[state_index]
  }
}






# Plots & Such...
# =================================================================


# Do Rural people find EVs practical? ( DOES NOT WORK)
rural_people <- ev_survey_results %>% filter(ev_survey_results$Location_Type == "Rural")
head(rural_people)
hist(as.numeric(rural_people$attr_Practicality), 
     main = "How practical do rural people find EVs?")


suburban_people <- ev_survey_results %>% filter(ev_survey_results$Location_Type == "Suburban")
hist(as.numeric(suburban_people$attr_Practicality), 
     main = "How practical do Suburban people find EVs?")

# How practical are EVs?
hist(as.numeric(ev_survey_results$attr_Practicality), 
     main = "How practical do people find EVs?")








