
# Question: Which attributes are most resonant for people that are planning to buy an EV next?

# Packages:
library(dplyr)

# Get Data:
ev_survey_data <- read.csv('ev_survey_data_cleaner.csv',
                           header = TRUE,
                           stringsAsFactors = FALSE
                           )


# GET DATA STRUCTURED
# ==========================================================================================

# Make a DF with just the attributes and the Y/N buyers  ----- ----- ----- ----- ----- -----
attr_for_ev_df <- cbind(buy = ev_survey_data$buy_electric_next,
                        Price = ev_survey_results$attr_Price,
                        Range = ev_survey_results$attr_Range,
                        Practical = ev_survey_results$attr_Practicality,
                        Brand = ev_survey_results$attr_Brand,
                        Carbon = ev_survey_results$attr_CarbonFootprint)
attr_for_ev_df <- ev_survey_data[ , c("buy_electric_next",
                                      "attr_Price",
                                      "attr_Range",
                                      "attr_Practicality",
                                      "attr_Brand",
                                      "attr_CarbonFootprint"
                                      )] 


# Convert the ev_buyers into binary ----- ----- ----- ----- ----- ----- ----- ----- ----- -----
# Revalues the "Buy EV Next" column as binary:
attr_for_ev_df$buy_electric_next <- as.numeric(revalue(as.character(attr_for_ev_df$buy_electric_next), c("-3" = "0","-2" = "0","-1" = "0","0" = "999","1" = "1","2" = "1","3"  = "1")))
#...and takes out the "Neither Agree not Disagree's:
attr_for_ev_df <- attr_for_ev_df %>% filter(attr_for_ev_df$buy_electric_next != 999)


# Convert the attributes to a 1-7 scale
likert_to_positives <- c("-3" = "1","-2" = "2","-1" = "3","0" = "4","1" = "5","2" = "6","3"  = "7")
attr_for_ev_df$attr_Price <- as.numeric(revalue(as.character(attr_for_ev_df$attr_Price), likert_to_positives))
attr_for_ev_df$attr_Range <- as.numeric(revalue(as.character(attr_for_ev_df$attr_Range), likert_to_positives))
attr_for_ev_df$attr_Practicality <- as.numeric(revalue(as.character(attr_for_ev_df$attr_Practicality), likert_to_positives))
attr_for_ev_df$attr_Brand <- as.numeric(revalue(as.character(attr_for_ev_df$attr_Brand), likert_to_positives))
attr_for_ev_df$attr_CarbonFootprint <- as.numeric(revalue(as.character(attr_for_ev_df$attr_CarbonFootprint), likert_to_positives))

#

# ATTRIBUTE PREDICTORS FOR BUYING
# ==========================================================================================

# Get Log of each value:
log_Price = log(attr_for_ev_df$attr_Price)
log_Range = log(attr_for_ev_df$attr_Range)
log_Practicality = log(attr_for_ev_df$attr_Practicality)
log_Brand = log(attr_for_ev_df$attr_Brand)
log_Carbon = log(attr_for_ev_df$attr_CarbonFootprint)


# Log Odds Regression:
lr_model <- glm(buy_electric_next ~ log_Price + log_Range + log_Practicality + log_Brand + log_Carbon,
                family = binomial, 
                attr_for_ev_df)
summary(lr_model)

# Calculate the Linear Regression Equation:
e <- 2.1718281

# Coefficients:
# Only two coefficients are significant.
intercept <- -6.6048
# Coef_Price      (Insignificant)
# Coef_Range      (Insignificant)
Coef_Practicality <- 1.4777
# Coef_Brand      (Insignificant)
Coef_Carbon       <- 1.1081

# PROBABILITY FUNCTION ----------------------------------------------------------------
# Pass in values for a user's Practicality_Score and Carbon_Score to see the liklihood
# that they will say their next car will be an EV:
Probability <- function(Practicality_Score, Carbon_Score){
  # Logit Equation:
  Log_Odds <- intercept + 
    ( Practicality_Score * Coef_Practicality) +
    ( Carbon_Score * Coef_Carbon)
  Odds <- e^Log_Odds # Calculate Odds:
  Probability_num  <- Odds / (1 + Odds) # Calculate Probability
  #return(Probability)
}
# ------------------------------------------------------------------------------------


