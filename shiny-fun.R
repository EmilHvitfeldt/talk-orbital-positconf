orbital_predict <- function(x) {
with(x, {
   species = dplyr::if_else(is.na(species), "unknown", species)
   sex = dplyr::if_else(is.na(sex), "unknown", sex)
   bill_length_mm = dplyr::if_else(is.na(bill_length_mm), 43.3, bill_length_mm)
   species_Chinstrap = as.numeric(species == "Chinstrap")
   sex_male = as.numeric(sex == "male")
   bill_length_mm = bill_length_mm / 5.53764488534842
   species_Chinstrap = species_Chinstrap / 0.39721765160312
   sex_male = sex_male / 0.500841815855869
   bill_length_mm = bill_length_mm - 7.89221731979121
   species_Chinstrap = species_Chinstrap - 0.491701461935901
   sex_male = sex_male - 1.02171730434597
   .pred = case_when(sex_male <= -1.02171730434597 & bill_length_mm <= -0.217474558216316 ~ 3394.140625, sex_male > -1.02171730434597 & bill_length_mm <= -0.217474558216316 ~ 3985.09615384615, species_Chinstrap <= -0.491701461935901 & bill_length_mm > -0.217474558216316 ~ 4980.37634408602, species_Chinstrap > -0.491701461935901 & bill_length_mm > -0.217474558216316 ~ 3757.97872340426)
  .pred
  })
}
