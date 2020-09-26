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



# Do people with more education care more about the environment? ------------------------------
# Quick table showing Education Level vs Caring about Carbon Footprint:
edu_and_carbon <- cbind(carbon_important = (ev_survey_results$attr_CarbonFootprint),
                        education_level_num = (ev_survey_results$education_num),
                        education_level = (ev_survey_results$Education))
unique_edu <- unique(ev_survey_results$Education)
# TODO:
# take all of the people with a certain type of degree and
#  Seperate them into a matching column with their carbon opinion
#  Average that column
#  save that average in a df with the name of the education level

plot(ev_survey_results$education_num, ev_survey_results$attr_CarbonFootprint) # Need to think of a more useful way to plot this

