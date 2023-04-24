library(readxl)
library(readr)
library(ggplot2)
library(ggpubr)
library(stringr)
library(tidyverse)
library(dplyr)
library(igraph)
library(multtest)
library(janitor)
library(RColorBrewer)


NAFLDAndNASHMatchedPart1 <- read_excel("University/MSc/NAFLDAndNASHMatchedPart2.xlsx")
nrow(NAFLDAndNASHMatchedPart1)
NAFLDAndNASHMatchedPart1[["Diseases2"]][is.na(NAFLDAndNASHMatchedPart1[["Diseases2"]])] <- "Control"
table(NAFLDAndNASHMatchedPart1$Diseases2)
tabyl(NAFLDAndNASHMatchedPart1$Major_dietary_changes_in_the_last_5_years)
NAFLDAndNASHMatchedPart1[["Major_dietary_changes_in_the_last_5_years"
                          ]][is.na(NAFLDAndNASHMatchedPart1[["Major_dietary_changes_in_the_last_5_years"]])] <- "0"
NAFLDAndNASHMatchedPart1$Cereal_type[NAFLDAndNASHMatchedPart1$Cereal_intake == 0] <- "0"
NAFLDAndNASHMatchedPart1$Bread_type[NAFLDAndNASHMatchedPart1$Bread_intake == 0] <- "0"
NAFLDAndNASHMatchedPart1.1 <- NAFLDAndNASHMatchedPart1 %>% 
  filter(Major_dietary_changes_in_the_last_5_years == "0")  %>%
  filter(`NA Percentage` < "0.3") %>%
  filter(!Diseases2 == "NASH")
table(NAFLDAndNASHMatchedPart1.1$Diseases2)
nrow(NAFLDAndNASHMatchedPart1.1)
NAFLDAndNASHMatchedPart1.2 <- NAFLDAndNASHMatchedPart1.1 %>%
  select(-`NA Count`, - `NA Percentage`)  %>%
  #Spread
  mutate(Butter_spread_intake = ifelse(Spread_type == 1, "1", "0")) %>%
  mutate(Margarine_spread_intake = ifelse(Spread_type == 2, "1", "0")) %>%
  mutate(Olive_oil_spread_intake = ifelse(Spread_type == 3, "1", "0")) %>%
  mutate(Never_use_spread = ifelse(Spread_type == 0, "1", "0")) %>%
  #Cereal
  mutate(Bran_cereal_intake = ifelse(Cereal_type == 1, "1", "0")) %>%
  mutate(Biscuit_cereal_intake = ifelse(Cereal_type == 2, "1", "0")) %>%
  mutate(Oat_cereal_intake = ifelse(Cereal_type == 3, "1", "0")) %>%
  mutate(Museli_intake = ifelse(Cereal_type == 4, "1", "0")) %>%
  mutate(Other_cereal_intake = ifelse(Cereal_type == 5, "1", "0")) %>%
  mutate(Never_eat_cereal = ifelse(Cereal_type == 0, "1", "0")) %>%
  #Milk
  mutate(Full_cream_milk_intake = ifelse(Milk_type_used == 1, "1", "0")) %>%
  mutate(Semi_skimmed_milk_intake = ifelse(Milk_type_used == 2, "1", "0")) %>%
  mutate(Skimmed_milk_intake = ifelse(Milk_type_used == 3, "1", "0")) %>%
  mutate(Soya_milk_intake = ifelse(Milk_type_used == 4, "1", "0")) %>%
  mutate(Other_milk = ifelse(Milk_type_used == 5, "1", "0")) %>%
  mutate(Never_use_milk = ifelse(Milk_type_used == 6, "1", "0")) %>%
  #Bread
  mutate(White_bread_intake = ifelse(Bread_type == 1, "1", "0")) %>%
  mutate(Brown_bread_intake = ifelse(Bread_type == 2, "1", "0")) %>%
  mutate(Wholegrain_bread_intake = ifelse(Bread_type == 3, "1", "0")) %>%
  mutate(Other_bread = ifelse(Bread_type == 4, "1", "0")) %>%
  mutate(Never_eat_bread = ifelse(Bread_type == 0, "1", "0")) %>%
  #Coffee
  mutate(Decaffeinated_coffee_intake = ifelse(Coffee_type == 1, "1", "0")) %>%
  mutate(Instant_coffee_intake = ifelse(Coffee_type == 2, "1", "0")) %>%
  mutate(Ground_coffee_intake = ifelse(Coffee_type == 3, "1", "0")) %>%
  mutate(Other_coffee = ifelse(Coffee_type == 4, "1", "0")) %>%
  #Eat_eggs_dairy_wheat_sugar
  mutate(Eat_eggs = ifelse(`Never_eat_eggs,_dairy,_wheat,_sugar` == 1, "1", ifelse(`Never_eat_eggs,_dairy,_wheat,_sugar` == 5, "1", "0"))) %>%
  mutate(Eat_dairy = ifelse(`Never_eat_eggs,_dairy,_wheat,_sugar` == 2, "1", ifelse(`Never_eat_eggs,_dairy,_wheat,_sugar` == 5, "1", "0"))) %>%
  mutate(Eat_wheat = ifelse(`Never_eat_eggs,_dairy,_wheat,_sugar` == 3, "1", ifelse(`Never_eat_eggs,_dairy,_wheat,_sugar` == 5, "1", "0"))) %>%
  mutate(Eat_sugar = ifelse(`Never_eat_eggs,_dairy,_wheat,_sugar` == 4, "1", ifelse(`Never_eat_eggs,_dairy,_wheat,_sugar` == 5, "1", "0"))) %>%
  mutate(Eat_eggs_dairy_wheat_sugar = ifelse(`Never_eat_eggs,_dairy,_wheat,_sugar` == 5, "1", "0"))
#Urine flags
NAFLDAndNASHMatchedPart1.2[["Microalbumin_in_urine"]][NAFLDAndNASHMatchedPart1.2[["Microalbumin_in_urine_result_flag"]] == "<6.7" ] <- "3.35"
NAFLDAndNASHMatchedPart1.2[["Potassium_in_urine"]][NAFLDAndNASHMatchedPart1.2[["Potassium_in_urine_result_flag"]] == ">200" ] <- "300"
NAFLDAndNASHMatchedPart1.2 <- NAFLDAndNASHMatchedPart1.2  %>% select(-Microalbumin_in_urine_result_flag, - Potassium_in_urine_result_flag)
nrow(NAFLDAndNASHMatchedPart1.2)
names(NAFLDAndNASHMatchedPart1.2)
#Files
NAFLDAndNASHMatchedPart1.2 <- NAFLDAndNASHMatchedPart1.2[,c(1:35, 37:39,46,48, 40:43,49, 36,44,45,47,55,51, 117:146, 56:84, 85:116)]
write.csv(NAFLDAndNASHMatchedPart1.2, "University/MSc/NAFLDAndNASHMatchedEnd.csv")


NAFLDAndNASHMatchedEnd <- read_csv("University/MSc/NAFLDAndNASHMatchedEnd.csv")
NAFLDAndNASHMatchedControl <- NAFLDAndNASHMatchedEnd %>% filter(Diseases2 == "Control")
nrow(NAFLDAndNASHMatchedControl)
NAFLDAndNASHMatched <- NAFLDAndNASHMatchedEnd %>% filter(Diseases2 == "NAFLD")
nrow(NAFLDAndNASHMatched)
NAFLDAndNASHMatchedAfter <- NAFLDAndNASHMatched %>% filter(TimeToDisease > 2)
nrow(NAFLDAndNASHMatchedAfter)
NAFLDAndNASHMatchedBefore <- NAFLDAndNASHMatched %>% filter(TimeToDisease < -2)
nrow(NAFLDAndNASHMatchedBefore)
NAFLDAndNASHMatched_2 <- NAFLDAndNASHMatched %>% filter(TimeToDisease >= -2 & TimeToDisease <= 2)
nrow(NAFLDAndNASHMatched_2)


#After
Diet_NAFLD_LRAF <- NAFLDAndNASHMatchedAfter %>% 
  select(Cooked_vegetable_intake, `Salad_/_raw_vegetable_intake`,  Fresh_fruit_intake, Bread_intake, Cereal_intake) 
Diet_NAFLD_LGAF <- NAFLDAndNASHMatchedAfter %>% 
  select(Butter_spread_intake, Margarine_spread_intake, Olive_oil_spread_intake, Never_use_spread, Bran_cereal_intake, Biscuit_cereal_intake, 
         Oat_cereal_intake, Museli_intake, Other_cereal_intake, Full_cream_milk_intake, Semi_skimmed_milk_intake, Skimmed_milk_intake, Soya_milk_intake, 
         Other_milk, White_bread_intake, Brown_bread_intake, Wholegrain_bread_intake, Other_bread, Decaffeinated_coffee_intake, Instant_coffee_intake, 
         Ground_coffee_intake, Other_coffee, Eat_eggs_dairy_wheat_sugar, Never_eat_cereal, Never_eat_bread, Eat_eggs, Eat_dairy, Eat_wheat, Eat_sugar)
Diet_NAFLD_LMNAF <- NAFLDAndNASHMatchedAfter %>% 
  select(Cereal_type, Milk_type_used, Spread_type, Bread_type, Coffee_type, `Never_eat_eggs,_dairy,_wheat,_sugar`)
Diet_NAFLD_LCAF <- NAFLDAndNASHMatchedAfter %>%
  select(Processed_meat_intake, Non_oily_fish_intake, `Lamb/mutton_intake`, Cheese_intake, Salt_added_to_food)
Bio_NAFLD_LRAF <- NAFLDAndNASHMatchedAfter %>% 
  select(Alanine_aminotransferase,Total_bilirubin, Cholesterol, Alkaline_phosphatase, HDL_cholesterol, Gamma_glutamyltransferase, 
         `Glycated_haemoglobin_(HbA1c)`, LDL_direct, Aspartate_aminotransferase, Triglycerides, Glucose, Total_protein, SHBG, Testosterone, 
         IGF_1, Creatinine, C_reactive_protein, Cystatin_C, Albumin, Apolipoprotein_A,  Apolipoprotein_B, Lipoprotein_A, Urea, Urate, Phosphate, 
         Calcium, Vitamin_D, Microalbumin_in_urine, `Creatinine_(enzymatic)_in_urine`, Potassium_in_urine, Sodium_in_urine, Direct_bilirubin)
Cell_NAFLD_LRAF <- NAFLDAndNASHMatchedAfter %>% 
  select(`White_blood_cell_(leukocyte)_count`, `Red_blood_cell_(erythrocyte)_count`, Haemoglobin_concentration,Haematocrit_percentage, 
         Mean_corpuscular_volume, Mean_corpuscular_haemoglobin, Mean_corpuscular_haemoglobin_concentration, Platelet_count, Platelet_crit,   
         `Red_blood_cell_(erythrocyte)_distribution_width`, `Mean_platelet_(thrombocyte)_volume`,  Platelet_distribution_width, Lymphocyte_count,
         Monocyte_count, Neutrophill_count, Eosinophill_count, Basophill_count, Lymphocyte_percentage, Monocyte_percentage, Neutrophill_percentage,
         Eosinophill_percentage, Basophill_percentage, Reticulocyte_percentage, Reticulocyte_count, Mean_reticulocyte_volume, Mean_sphered_cell_volume,
         Immature_reticulocyte_fraction, High_light_scatter_reticulocyte_percentage, High_light_scatter_reticulocyte_count)
Clin_NAFLD_LRAF <- NAFLDAndNASHMatchedAfter %>%
  select(FirstDiagnosis, Diseases2, TimeToDeathMonths, TimeToDisease, AgeAtDiseaseLast, main_death, date_death, `Pulse_rate,_automated_reading`, 
         `Diastolic_blood_pressure,_automated_reading`, `Systolic_blood_pressure,_automated_reading`, age_recruitment, Sex, body_fat_percentage,
         `Body_mass_index_(BMI)`, Weight, Body_fat_percentage, Whole_body_fat_mass, Whole_body_fat_free_mass, Whole_body_water_mass, Year_of_birth,
         Waist_circumference, Hip_circumference, Standing_height, `Long_standing_illness,_disability_or_infirmity`, Falls_in_the_last_year, 
         Townsend_deprivation_index_at_recruitment, BinaryDeath, CurrentAgeDate, AgeNow, Comorbidities, smoking_status_coding, ethnic_background_coding,
         Ethnic_background)

#Before
Diet_NAFLD_LRBF <- NAFLDAndNASHMatchedBefore %>% 
  select(Cooked_vegetable_intake, `Salad_/_raw_vegetable_intake`,  Fresh_fruit_intake, Bread_intake, Cereal_intake) 
Diet_NAFLD_LGBF <- NAFLDAndNASHMatchedBefore %>% 
  select(Butter_spread_intake, Margarine_spread_intake, Olive_oil_spread_intake, Never_use_spread, Bran_cereal_intake, Biscuit_cereal_intake, 
         Oat_cereal_intake, Museli_intake, Other_cereal_intake, Full_cream_milk_intake, Semi_skimmed_milk_intake, Skimmed_milk_intake, Soya_milk_intake, 
         Other_milk, White_bread_intake, Brown_bread_intake, Wholegrain_bread_intake, Other_bread, Decaffeinated_coffee_intake, Instant_coffee_intake, 
         Ground_coffee_intake, Other_coffee, Eat_eggs_dairy_wheat_sugar, Never_eat_cereal, Never_eat_bread, Eat_eggs, Eat_dairy, Eat_wheat, Eat_sugar)
Diet_NAFLD_LMNBF <- NAFLDAndNASHMatchedBefore %>% 
  select(Cereal_type, Milk_type_used, Spread_type, Bread_type, Coffee_type, `Never_eat_eggs,_dairy,_wheat,_sugar`)
Diet_NAFLD_LCBF <- NAFLDAndNASHMatchedBefore %>%
  select(Processed_meat_intake, Non_oily_fish_intake, `Lamb/mutton_intake`, Cheese_intake, Salt_added_to_food)
Bio_NAFLD_LRBF <- NAFLDAndNASHMatchedBefore %>% 
  select(Alanine_aminotransferase,Total_bilirubin, Cholesterol, Alkaline_phosphatase, HDL_cholesterol, Gamma_glutamyltransferase, 
         `Glycated_haemoglobin_(HbA1c)`, LDL_direct, Aspartate_aminotransferase, Triglycerides, Glucose, Total_protein, SHBG, Testosterone, 
         IGF_1, Creatinine, C_reactive_protein, Cystatin_C, Albumin, Apolipoprotein_A,  Apolipoprotein_B, Lipoprotein_A, Urea, Urate, Phosphate, 
         Calcium, Vitamin_D, Microalbumin_in_urine, `Creatinine_(enzymatic)_in_urine`, Potassium_in_urine, Sodium_in_urine, Direct_bilirubin)
Cell_NAFLD_LRBF <- NAFLDAndNASHMatchedBefore %>% 
  select(`White_blood_cell_(leukocyte)_count`, `Red_blood_cell_(erythrocyte)_count`, Haemoglobin_concentration,Haematocrit_percentage, 
         Mean_corpuscular_volume, Mean_corpuscular_haemoglobin, Mean_corpuscular_haemoglobin_concentration, Platelet_count, Platelet_crit,   
         `Red_blood_cell_(erythrocyte)_distribution_width`, `Mean_platelet_(thrombocyte)_volume`,  Platelet_distribution_width, Lymphocyte_count,
         Monocyte_count, Neutrophill_count, Eosinophill_count, Basophill_count, Lymphocyte_percentage, Monocyte_percentage, Neutrophill_percentage,
         Eosinophill_percentage, Basophill_percentage, Reticulocyte_percentage, Reticulocyte_count, Mean_reticulocyte_volume, Mean_sphered_cell_volume,
         Immature_reticulocyte_fraction, High_light_scatter_reticulocyte_percentage, High_light_scatter_reticulocyte_count)
Clin_NAFLD_LRBF <- NAFLDAndNASHMatchedBefore %>%
  select(FirstDiagnosis, Diseases2, TimeToDeathMonths, TimeToDisease, AgeAtDiseaseLast, main_death, date_death, `Pulse_rate,_automated_reading`, 
         `Diastolic_blood_pressure,_automated_reading`, `Systolic_blood_pressure,_automated_reading`, age_recruitment, Sex, body_fat_percentage,
         `Body_mass_index_(BMI)`, Weight, Body_fat_percentage, Whole_body_fat_mass, Whole_body_fat_free_mass, Whole_body_water_mass, Year_of_birth,
         Waist_circumference, Hip_circumference, Standing_height, `Long_standing_illness,_disability_or_infirmity`, Falls_in_the_last_year, 
         Townsend_deprivation_index_at_recruitment, BinaryDeath, CurrentAgeDate, AgeNow, Comorbidities, smoking_status_coding, ethnic_background_coding,
         Ethnic_background)

#Within 2
Diet_NAFLD_LMN2 <- NAFLDAndNASHMatched_2 %>% 
  select(Cereal_type, Milk_type_used, Spread_type, Bread_type, Coffee_type, `Never_eat_eggs,_dairy,_wheat,_sugar`)
Diet_NAFLD_LR2 <- NAFLDAndNASHMatched_2 %>% 
  select(Cooked_vegetable_intake, `Salad_/_raw_vegetable_intake`,  Fresh_fruit_intake, Bread_intake, Cereal_intake) 
Diet_NAFLD_LG2 <- NAFLDAndNASHMatched_2 %>% 
  select(Butter_spread_intake, Margarine_spread_intake, Olive_oil_spread_intake, Never_use_spread, Bran_cereal_intake, Biscuit_cereal_intake, 
         Oat_cereal_intake, Museli_intake, Other_cereal_intake, Full_cream_milk_intake, Semi_skimmed_milk_intake, Skimmed_milk_intake, Soya_milk_intake, 
         Other_milk, White_bread_intake, Brown_bread_intake, Wholegrain_bread_intake, Other_bread, Decaffeinated_coffee_intake, Instant_coffee_intake, 
         Ground_coffee_intake, Other_coffee, Eat_eggs_dairy_wheat_sugar, Never_eat_cereal, Never_eat_bread, Eat_eggs, Eat_dairy, Eat_wheat, Eat_sugar)
Diet_NAFLD_LC2 <- NAFLDAndNASHMatched_2 %>%
  select(Processed_meat_intake, Non_oily_fish_intake, `Lamb/mutton_intake`, Cheese_intake, Salt_added_to_food)
Bio_NAFLD_LR2 <- NAFLDAndNASHMatched_2 %>% 
  select(Alanine_aminotransferase,Total_bilirubin, Cholesterol, Alkaline_phosphatase, HDL_cholesterol, Gamma_glutamyltransferase, 
         `Glycated_haemoglobin_(HbA1c)`, LDL_direct, Aspartate_aminotransferase, Triglycerides, Glucose, Total_protein, SHBG, Testosterone, 
         IGF_1, Creatinine, C_reactive_protein, Cystatin_C, Albumin, Apolipoprotein_A,  Apolipoprotein_B, Lipoprotein_A, Urea, Urate, Phosphate, 
         Calcium, Vitamin_D, Microalbumin_in_urine, `Creatinine_(enzymatic)_in_urine`, Potassium_in_urine, Sodium_in_urine, Direct_bilirubin)
Cell_NAFLD_LR2 <- NAFLDAndNASHMatched_2 %>% 
  select(`White_blood_cell_(leukocyte)_count`, `Red_blood_cell_(erythrocyte)_count`, Haemoglobin_concentration,Haematocrit_percentage, 
         Mean_corpuscular_volume, Mean_corpuscular_haemoglobin, Mean_corpuscular_haemoglobin_concentration, Platelet_count, Platelet_crit,   
         `Red_blood_cell_(erythrocyte)_distribution_width`, `Mean_platelet_(thrombocyte)_volume`,  Platelet_distribution_width, Lymphocyte_count,
         Monocyte_count, Neutrophill_count, Eosinophill_count, Basophill_count, Lymphocyte_percentage, Monocyte_percentage, Neutrophill_percentage,
         Eosinophill_percentage, Basophill_percentage, Reticulocyte_percentage, Reticulocyte_count, Mean_reticulocyte_volume, Mean_sphered_cell_volume,
         Immature_reticulocyte_fraction, High_light_scatter_reticulocyte_percentage, High_light_scatter_reticulocyte_count)
Clin_NAFLD_LR2 <- NAFLDAndNASHMatched_2 %>%
  select(FirstDiagnosis, Diseases2, TimeToDeathMonths, TimeToDisease, AgeAtDiseaseLast, main_death, date_death, `Pulse_rate,_automated_reading`, 
         `Diastolic_blood_pressure,_automated_reading`, `Systolic_blood_pressure,_automated_reading`, age_recruitment, Sex, body_fat_percentage,
         `Body_mass_index_(BMI)`, Weight, Body_fat_percentage, Whole_body_fat_mass, Whole_body_fat_free_mass, Whole_body_water_mass, Year_of_birth,
         Waist_circumference, Hip_circumference, Standing_height, `Long_standing_illness,_disability_or_infirmity`, Falls_in_the_last_year, 
         Townsend_deprivation_index_at_recruitment, BinaryDeath, CurrentAgeDate, AgeNow, Comorbidities, smoking_status_coding, ethnic_background_coding,
         Ethnic_background)
#Control
Diet_NAFLD_LMNC <- NAFLDAndNASHMatchedControl %>% 
  select(Cereal_type, Milk_type_used, Spread_type, Bread_type, Coffee_type, `Never_eat_eggs,_dairy,_wheat,_sugar`)
Diet_NAFLD_LRC <- NAFLDAndNASHMatchedControl %>% 
  select(Cooked_vegetable_intake, `Salad_/_raw_vegetable_intake`,  Fresh_fruit_intake, Bread_intake, Cereal_intake) 
Diet_NAFLD_LGC <- NAFLDAndNASHMatchedControl %>% 
  select(Butter_spread_intake, Margarine_spread_intake, Olive_oil_spread_intake, Never_use_spread, Bran_cereal_intake, Biscuit_cereal_intake, 
         Oat_cereal_intake, Museli_intake, Other_cereal_intake, Full_cream_milk_intake, Semi_skimmed_milk_intake, Skimmed_milk_intake, Soya_milk_intake, 
         Other_milk, White_bread_intake, Brown_bread_intake, Wholegrain_bread_intake, Other_bread, Decaffeinated_coffee_intake, Instant_coffee_intake, 
         Ground_coffee_intake, Other_coffee, Eat_eggs_dairy_wheat_sugar, Never_eat_cereal, Never_eat_bread, Eat_eggs, Eat_dairy, Eat_wheat, Eat_sugar)
Diet_NAFLD_LCC <- NAFLDAndNASHMatchedControl %>%
  select(Processed_meat_intake, Non_oily_fish_intake, `Lamb/mutton_intake`, Cheese_intake, Salt_added_to_food)
Bio_NAFLD_LRC <- NAFLDAndNASHMatchedControl %>% 
  select(Alanine_aminotransferase,Total_bilirubin, Cholesterol, Alkaline_phosphatase, HDL_cholesterol, Gamma_glutamyltransferase, 
         `Glycated_haemoglobin_(HbA1c)`, LDL_direct, Aspartate_aminotransferase, Triglycerides, Glucose, Total_protein, SHBG, Testosterone, 
         IGF_1, Creatinine, C_reactive_protein, Cystatin_C, Albumin, Apolipoprotein_A,  Apolipoprotein_B, Lipoprotein_A, Urea, Urate, Phosphate, 
         Calcium, Vitamin_D, Microalbumin_in_urine, `Creatinine_(enzymatic)_in_urine`, Potassium_in_urine, Sodium_in_urine, Direct_bilirubin)
Cell_NAFLD_LRC <- NAFLDAndNASHMatchedControl %>% 
  select(`White_blood_cell_(leukocyte)_count`, `Red_blood_cell_(erythrocyte)_count`, Haemoglobin_concentration,Haematocrit_percentage, 
         Mean_corpuscular_volume, Mean_corpuscular_haemoglobin, Mean_corpuscular_haemoglobin_concentration, Platelet_count, Platelet_crit,   
         `Red_blood_cell_(erythrocyte)_distribution_width`, `Mean_platelet_(thrombocyte)_volume`,  Platelet_distribution_width, Lymphocyte_count,
         Monocyte_count, Neutrophill_count, Eosinophill_count, Basophill_count, Lymphocyte_percentage, Monocyte_percentage, Neutrophill_percentage,
         Eosinophill_percentage, Basophill_percentage, Reticulocyte_percentage, Reticulocyte_count, Mean_reticulocyte_volume, Mean_sphered_cell_volume,
         Immature_reticulocyte_fraction, High_light_scatter_reticulocyte_percentage, High_light_scatter_reticulocyte_count)
Clin_NAFLD_LRC <- NAFLDAndNASHMatchedControl %>%
  select(FirstDiagnosis, Diseases2, TimeToDeathMonths, TimeToDisease, AgeAtDiseaseLast, main_death, date_death, `Pulse_rate,_automated_reading`, 
         `Diastolic_blood_pressure,_automated_reading`, `Systolic_blood_pressure,_automated_reading`, age_recruitment, Sex, body_fat_percentage,
         `Body_mass_index_(BMI)`, Weight, Body_fat_percentage, Whole_body_fat_mass, Whole_body_fat_free_mass, Whole_body_water_mass, Year_of_birth,
         Waist_circumference, Hip_circumference, Standing_height, `Long_standing_illness,_disability_or_infirmity`, Falls_in_the_last_year, 
         Townsend_deprivation_index_at_recruitment, BinaryDeath, CurrentAgeDate, AgeNow, Comorbidities, smoking_status_coding, ethnic_background_coding,
         Ethnic_background)

#Outliers
#Diet
Diet_NAFLD_LR2_SD <- Diet_NAFLD_LR2
for(Diet in names(Diet_NAFLD_LR2_SD)){ #lm diet Within
  SD <- sd(Diet_NAFLD_LR2[[Diet]], na.rm = TRUE)
  mean <- mean(Diet_NAFLD_LR2[[Diet]], na.rm = TRUE)
  LB <- as.numeric(mean - 4*SD)
  UB <- as.numeric(mean + 4*SD)
  Diet_NAFLD_LR2_SD[[Diet]]<- as.numeric(Diet_NAFLD_LR2_SD[[Diet]])
  Diet_NAFLD_LR2_SD[[Diet]][which(Diet_NAFLD_LR2_SD[[Diet]] < LB)] = NA
  Diet_NAFLD_LR2_SD[[Diet]][which(Diet_NAFLD_LR2_SD[[Diet]] > UB)] = NA
}
Diet_NAFLD_LRBF_SD <- Diet_NAFLD_LRBF
for(Diet in names(Diet_NAFLD_LRBF_SD)){ #Before lm diet
  SD <- sd(Diet_NAFLD_LRBF[[Diet]], na.rm = TRUE)
  mean <- mean(Diet_NAFLD_LRBF[[Diet]], na.rm = TRUE)
  LB <- as.numeric(mean - 4*SD)
  UB <- as.numeric(mean + 4*SD)
  Diet_NAFLD_LRBF_SD[[Diet]]<- as.numeric(Diet_NAFLD_LRBF_SD[[Diet]])
  Diet_NAFLD_LRBF_SD[[Diet]][which(Diet_NAFLD_LRBF_SD[[Diet]] < LB)] = NA
  Diet_NAFLD_LRBF_SD[[Diet]][which(Diet_NAFLD_LRBF_SD[[Diet]] > UB)] = NA
}
Diet_NAFLD_LRAF_SD <- Diet_NAFLD_LRAF
for(Diet in names(Diet_NAFLD_LRAF_SD)){ #After lm diet
  SD <- sd(Diet_NAFLD_LRAF[[Diet]], na.rm = TRUE)
  mean <- mean(Diet_NAFLD_LRAF[[Diet]], na.rm = TRUE)
  LB <- as.numeric(mean - 4*SD)
  UB <- as.numeric(mean + 4*SD)
  Diet_NAFLD_LRAF_SD[[Diet]]<- as.numeric(Diet_NAFLD_LRAF_SD[[Diet]])
  Diet_NAFLD_LRAF_SD[[Diet]][which(Diet_NAFLD_LRAF_SD[[Diet]] < LB)] = NA
  Diet_NAFLD_LRAF_SD[[Diet]][which(Diet_NAFLD_LRAF_SD[[Diet]] > UB)] = NA
}
Diet_NAFLD_LRC_SD <- Diet_NAFLD_LRC
for(Diet in names(Diet_NAFLD_LRC_SD)){ #Control lm diet
  SD <- sd(Diet_NAFLD_LRC[[Diet]], na.rm = TRUE)
  mean <- mean(Diet_NAFLD_LRC[[Diet]], na.rm = TRUE)
  LB <- as.numeric(mean - 4*SD)
  UB <- as.numeric(mean + 4*SD)
  Diet_NAFLD_LRC_SD[[Diet]]<- as.numeric(Diet_NAFLD_LRC_SD[[Diet]])
  Diet_NAFLD_LRC_SD[[Diet]][which(Diet_NAFLD_LRC_SD[[Diet]] < LB)] = NA
  Diet_NAFLD_LRC_SD[[Diet]][which(Diet_NAFLD_LRC_SD[[Diet]] > UB)] = NA
}
#Bio
Bio_NAFLD_LR2_SD <- Bio_NAFLD_LR2
for(Bio in names(Bio_NAFLD_LR2_SD)){
  SD <- sd(Bio_NAFLD_LR2[[Bio]], na.rm = TRUE)
  mean <- mean(Bio_NAFLD_LR2[[Bio]], na.rm = TRUE)
  LB <- as.numeric(mean - 4*SD)
  UB <- as.numeric(mean + 4*SD)
  Bio_NAFLD_LR2_SD[[Bio]]<- as.numeric(Bio_NAFLD_LR2_SD[[Bio]])
  Bio_NAFLD_LR2_SD[[Bio]][which(Bio_NAFLD_LR2_SD[[Bio]] < LB)] = NA
  Bio_NAFLD_LR2_SD[[Bio]][which(Bio_NAFLD_LR2_SD[[Bio]] > UB)] = NA
}
Bio_NAFLD_LRBF_SD <- Bio_NAFLD_LRBF
for(Bio in names(Bio_NAFLD_LRBF_SD)){
  SD <- sd(Bio_NAFLD_LRBF[[Bio]], na.rm = TRUE)
  mean <- mean(Bio_NAFLD_LRBF[[Bio]], na.rm = TRUE)
  LB <- as.numeric(mean - 4*SD)
  UB <- as.numeric(mean + 4*SD)
  Bio_NAFLD_LRBF_SD[[Bio]]<- as.numeric(Bio_NAFLD_LRBF_SD[[Bio]])
  Bio_NAFLD_LRBF_SD[[Bio]][which(Bio_NAFLD_LRBF_SD[[Bio]] < LB)] = NA
  Bio_NAFLD_LRBF_SD[[Bio]][which(Bio_NAFLD_LRBF_SD[[Bio]] > UB)] = NA
}
Bio_NAFLD_LRAF_SD <- Bio_NAFLD_LRAF
for(Bio in names(Bio_NAFLD_LRAF_SD)){
  SD <- sd(Bio_NAFLD_LRAF[[Bio]], na.rm = TRUE)
  mean <- mean(Bio_NAFLD_LRAF[[Bio]], na.rm = TRUE)
  LB <- as.numeric(mean - 4*SD)
  UB <- as.numeric(mean + 4*SD)
  Bio_NAFLD_LRAF_SD[[Bio]]<- as.numeric(Bio_NAFLD_LRAF_SD[[Bio]])
  Bio_NAFLD_LRAF_SD[[Bio]][which(Bio_NAFLD_LRAF_SD[[Bio]] < LB)] = NA
  Bio_NAFLD_LRAF_SD[[Bio]][which(Bio_NAFLD_LRAF_SD[[Bio]] > UB)] = NA
}
Bio_NAFLD_LRC_SD <- Bio_NAFLD_LRC
for(Bio in names(Bio_NAFLD_LRC_SD)){
  SD <- sd(Bio_NAFLD_LRC[[Bio]], na.rm = TRUE)
  mean <- mean(Bio_NAFLD_LRC[[Bio]], na.rm = TRUE)
  LB <- as.numeric(mean - 4*SD)
  UB <- as.numeric(mean + 4*SD)
  Bio_NAFLD_LRC_SD[[Bio]]<- as.numeric(Bio_NAFLD_LRC_SD[[Bio]])
  Bio_NAFLD_LRC_SD[[Bio]][which(Bio_NAFLD_LRC_SD[[Bio]] < LB)] = NA
  Bio_NAFLD_LRC_SD[[Bio]][which(Bio_NAFLD_LRC_SD[[Bio]] > UB)] = NA
}
#Cells
Cell_NAFLD_LR2_SD <- Cell_NAFLD_LR2
for(cell in names(Cell_NAFLD_LR2_SD)){
  SD <- sd(Cell_NAFLD_LR2[[cell]], na.rm = TRUE)
  mean <- mean(Cell_NAFLD_LR2[[cell]], na.rm = TRUE)
  LB <- as.numeric(mean - 4*SD)
  UB <- as.numeric(mean + 4*SD)
  Cell_NAFLD_LR2_SD[[cell]]<- as.numeric(Cell_NAFLD_LR2_SD[[cell]])
  Cell_NAFLD_LR2_SD[[cell]][which(Cell_NAFLD_LR2_SD[[cell]] < LB)] = NA
  Cell_NAFLD_LR2_SD[[cell]][which(Cell_NAFLD_LR2_SD[[cell]] > UB)] = NA
}
Cell_NAFLD_LRBF_SD <- Cell_NAFLD_LRBF
for(cell in names(Cell_NAFLD_LRBF_SD)){
  SD <- sd(Cell_NAFLD_LRBF[[cell]], na.rm = TRUE)
  mean <- mean(Cell_NAFLD_LRBF[[cell]], na.rm = TRUE)
  LB <- as.numeric(mean - 4*SD)
  UB <- as.numeric(mean + 4*SD)
  Cell_NAFLD_LRBF_SD[[cell]]<- as.numeric(Cell_NAFLD_LRBF_SD[[cell]])
  Cell_NAFLD_LRBF_SD[[cell]][which(Cell_NAFLD_LRBF_SD[[cell]] < LB)] = NA
  Cell_NAFLD_LRBF_SD[[cell]][which(Cell_NAFLD_LRBF_SD[[cell]] > UB)] = NA
}
Cell_NAFLD_LRAF_SD <- Cell_NAFLD_LRAF
for(cell in names(Cell_NAFLD_LRAF_SD)){
  SD <- sd(Cell_NAFLD_LRAF[[cell]], na.rm = TRUE)
  mean <- mean(Cell_NAFLD_LRAF[[cell]], na.rm = TRUE)
  LB <- as.numeric(mean - 4*SD)
  UB <- as.numeric(mean + 4*SD)
  Cell_NAFLD_LRAF_SD[[cell]]<- as.numeric(Cell_NAFLD_LRAF_SD[[cell]])
  Cell_NAFLD_LRAF_SD[[cell]][which(Cell_NAFLD_LRAF_SD[[cell]] < LB)] = NA
  Cell_NAFLD_LRAF_SD[[cell]][which(Cell_NAFLD_LRAF_SD[[cell]] > UB)] = NA
}
Cell_NAFLD_LRC_SD <- Cell_NAFLD_LRC
for(cell in names(Cell_NAFLD_LRC_SD)){
  SD <- sd(Cell_NAFLD_LRC[[cell]], na.rm = TRUE)
  mean <- mean(Cell_NAFLD_LRC[[cell]], na.rm = TRUE)
  LB <- as.numeric(mean - 4*SD)
  UB <- as.numeric(mean + 4*SD)
  Cell_NAFLD_LRC_SD[[cell]]<- as.numeric(Cell_NAFLD_LRC_SD[[cell]])
  Cell_NAFLD_LRC_SD[[cell]][which(Cell_NAFLD_LRC_SD[[cell]] < LB)] = NA
  Cell_NAFLD_LRC_SD[[cell]][which(Cell_NAFLD_LRC_SD[[cell]] > UB)] = NA
}
#Clinical
names(Clin_NAFLD_LRAF[,-c(1:7,11,12,20,23,33:27,24)])
Clin_NAFLD_LRAF_SD <- Clin_NAFLD_LRAF
for(cell in names(Clin_NAFLD_LRAF_SD[,-c(1:7,11,12,20,23,33:27,24)])){
  SD <- sd(Clin_NAFLD_LRAF[[cell]], na.rm = TRUE)
  mean <- mean(Clin_NAFLD_LRAF[[cell]], na.rm = TRUE)
  LB <- as.numeric(mean - 4*SD)
  UB <- as.numeric(mean + 4*SD)
  Clin_NAFLD_LRAF_SD[[cell]]<- as.numeric(Clin_NAFLD_LRAF_SD[[cell]])
  Clin_NAFLD_LRAF_SD[[cell]][which(Clin_NAFLD_LRAF_SD[[cell]] < LB)] = NA
  Clin_NAFLD_LRAF_SD[[cell]][which(Clin_NAFLD_LRAF_SD[[cell]] > UB)] = NA
}
Clin_NAFLD_LR2_SD <- Clin_NAFLD_LR2
for(cell in names(Clin_NAFLD_LR2_SD[,-c(1:7,11,12,20,23,33:27,24)])){
  SD <- sd(Clin_NAFLD_LR2[[cell]], na.rm = TRUE)
  mean <- mean(Clin_NAFLD_LR2[[cell]], na.rm = TRUE)
  LB <- as.numeric(mean - 4*SD)
  UB <- as.numeric(mean + 4*SD)
  Clin_NAFLD_LR2_SD[[cell]]<- as.numeric(Clin_NAFLD_LR2_SD[[cell]])
  Clin_NAFLD_LR2_SD[[cell]][which(Clin_NAFLD_LR2_SD[[cell]] < LB)] = NA
  Clin_NAFLD_LR2_SD[[cell]][which(Clin_NAFLD_LR2_SD[[cell]] > UB)] = NA
}
Clin_NAFLD_LRC_SD <- Clin_NAFLD_LRC
for(cell in names(Clin_NAFLD_LRC_SD[,-c(1:7,11,12,20,23,33:27,24)])){
  SD <- sd(Clin_NAFLD_LRC[[cell]], na.rm = TRUE)
  mean <- mean(Clin_NAFLD_LRC[[cell]], na.rm = TRUE)
  LB <- as.numeric(mean - 4*SD)
  UB <- as.numeric(mean + 4*SD)
  Clin_NAFLD_LRC_SD[[cell]]<- as.numeric(Clin_NAFLD_LRC_SD[[cell]])
  Clin_NAFLD_LRC_SD[[cell]][which(Clin_NAFLD_LRC_SD[[cell]] < LB)] = NA
  Clin_NAFLD_LRC_SD[[cell]][which(Clin_NAFLD_LRC_SD[[cell]] > UB)] = NA
}
Clin_NAFLD_LRBF_SD <- Clin_NAFLD_LRBF
for(cell in names(Clin_NAFLD_LRBF_SD[,-c(1:7,11,12,20,23,33:27,24)])){
  SD <- sd(Clin_NAFLD_LRBF[[cell]], na.rm = TRUE)
  mean <- mean(Clin_NAFLD_LRBF[[cell]], na.rm = TRUE)
  LB <- as.numeric(mean - 4*SD)
  UB <- as.numeric(mean + 4*SD)
  Clin_NAFLD_LRBF_SD[[cell]]<- as.numeric(Clin_NAFLD_LRBF_SD[[cell]])
  Clin_NAFLD_LRBF_SD[[cell]][which(Clin_NAFLD_LRBF_SD[[cell]] < LB)] = NA
  Clin_NAFLD_LRBF_SD[[cell]][which(Clin_NAFLD_LRBF_SD[[cell]] > UB)] = NA
}

#Imputing
#Categorical diet
for (dietc in names(Diet_NAFLD_LMNBF)){ #D2BF
  for (dietd in names(Diet_NAFLD_LMN2)){  #DW2
    for (diete in names(Diet_NAFLD_LMNC)){  #C
      for (dietb in names(Diet_NAFLD_LMNAF)){ #D2AF 
    Diet_NAFLD_LMNAF[[dietb]][is.na(Diet_NAFLD_LMNAF[[dietb]])] <- ((data.frame(tabyl(na.omit(Diet_NAFLD_LMNAF[[dietb]]))) %>% 
                                                                       rename(type = 1) %>% filter(type > 0)) %>%
                                                                      filter(n == max(
                                                                        (data.frame(tabyl(na.omit(Diet_NAFLD_LMNAF[[dietb]]))) %>% 
                                                                           rename(type = 1) %>% filter(type > 0))[,c(2)])))[,c(1)]   }

      Diet_NAFLD_LMNBF[[dietc]][is.na(Diet_NAFLD_LMNBF[[dietc]])] <- ((data.frame(tabyl(na.omit(Diet_NAFLD_LMNBF[[dietc]]))) %>% 
                                                                         rename(type = 1) %>% filter(type > 0)) %>%
                                                                        filter(n == max(
                                                                          (data.frame(tabyl(na.omit(Diet_NAFLD_LMNBF[[dietc]]))) %>% 
                                                                             rename(type = 1) %>% filter(type > 0))[,c(2)])))[,c(1)]
      Diet_NAFLD_LMN2[[dietd]][is.na(Diet_NAFLD_LMN2[[dietd]])] <- ((data.frame(tabyl(na.omit(Diet_NAFLD_LMN2[[dietd]]))) %>% 
                                                                       rename(type = 1) %>% filter(type > 0)) %>%
                                                                      filter(n == max(
                                                                        (data.frame(tabyl(na.omit(Diet_NAFLD_LMN2[[dietd]]))) %>% 
                                                                           rename(type = 1) %>% filter(type > 0))[,c(2)])))[,c(1)]
      Diet_NAFLD_LMNC[[diete]][is.na(Diet_NAFLD_LMNC[[diete]])] <- ((data.frame(tabyl(na.omit(Diet_NAFLD_LMNC[[diete]]))) %>% 
                                                                       rename(type = 1) %>% filter(type > 0)) %>%
                                                                      filter(n == max(
                                                                        (data.frame(tabyl(na.omit(Diet_NAFLD_LMNC[[diete]]))) %>% 
                                                                           rename(type = 1) %>% filter(type > 0))[,c(2)])))[,c(1)]
    }}}
#Control diet + bio
for (diet in names(Diet_NAFLD_LGC)){
  for (bio in names(Bio_NAFLD_LRC_SD)){
    for (dieti in names(Diet_NAFLD_LRC_SD)){
      for (dietc in names(Diet_NAFLD_LCC)){
        Diet_NAFLD_LGC[[diet]][is.na(Diet_NAFLD_LGC[[diet]])] <- median(Diet_NAFLD_LGC[[diet]], na.rm=T)
        Bio_NAFLD_LRC_SD[[bio]][is.na(Bio_NAFLD_LRC_SD[[bio]])] <- mean(Bio_NAFLD_LRC_SD[[bio]], na.rm=T)
        Diet_NAFLD_LRC_SD[[dieti]][is.na(Diet_NAFLD_LRC_SD[[dieti]])] <- median(Diet_NAFLD_LRC_SD[[dieti]], na.rm=T)
        Diet_NAFLD_LCC[[dietc]][is.na(Diet_NAFLD_LCC[[dietc]])] <- median(Diet_NAFLD_LCC[[dietc]], na.rm=T)}}}}
#Within diet + bio
for (diet in names(Diet_NAFLD_LG2)){ 
  for (bio in names(Bio_NAFLD_LR2_SD)){
    for (dieti in names(Diet_NAFLD_LR2_SD)){
      for (dietc in names(Diet_NAFLD_LC2)){
        Diet_NAFLD_LC2[[dietc]][is.na(Diet_NAFLD_LC2[[dietc]])] <- median(Diet_NAFLD_LC2[[dietc]], na.rm=T)
        Diet_NAFLD_LG2[[diet]][is.na(Diet_NAFLD_LG2[[diet]])] <- median(Diet_NAFLD_LG2[[diet]], na.rm=T)
        Bio_NAFLD_LR2_SD[[bio]][is.na(Bio_NAFLD_LR2_SD[[bio]])] <- mean(Bio_NAFLD_LR2_SD[[bio]], na.rm=T)
        Diet_NAFLD_LR2_SD[[dieti]][is.na(Diet_NAFLD_LR2_SD[[dieti]])] <- median(Diet_NAFLD_LR2_SD[[dieti]], na.rm=T)}}}}
#After  diet + bio
for (diet in names(Diet_NAFLD_LGAF)){
  for (bio in names(Bio_NAFLD_LRAF_SD)){
    for (dieti in names(Diet_NAFLD_LRAF_SD)){
      for (dietc in names(Diet_NAFLD_LCAF)){
        Diet_NAFLD_LCAF[[dietc]][is.na(Diet_NAFLD_LCAF[[dietc]])] <- median(Diet_NAFLD_LCAF[[dietc]], na.rm=T)}
      Diet_NAFLD_LGAF[[diet]][is.na(Diet_NAFLD_LGAF[[diet]])] <- median(Diet_NAFLD_LGAF[[diet]], na.rm=T)
      Bio_NAFLD_LRAF_SD[[bio]][is.na(Bio_NAFLD_LRAF_SD[[bio]])] <- mean(Bio_NAFLD_LRAF_SD[[bio]], na.rm=T)
      Diet_NAFLD_LRAF_SD[[dieti]][is.na(Diet_NAFLD_LRAF_SD[[dieti]])] <- median(Diet_NAFLD_LRAF_SD[[dieti]], na.rm=T)}}}
#Before diet + bio
for (diet in names(Diet_NAFLD_LGBF)){ 
  for (bio in names(Bio_NAFLD_LRBF_SD)){
    for (dieti in names(Diet_NAFLD_LRBF_SD)){
      for (dietc in names(Diet_NAFLD_LCBF)){
        Diet_NAFLD_LCBF[[dietc]][is.na(Diet_NAFLD_LCBF[[dietc]])] <- median(Diet_NAFLD_LCBF[[dietc]], na.rm=T)
        Diet_NAFLD_LGBF[[diet]][is.na(Diet_NAFLD_LGBF[[diet]])] <- median(Diet_NAFLD_LGBF[[diet]], na.rm=T)
        Bio_NAFLD_LRBF_SD[[bio]][is.na(Bio_NAFLD_LRBF_SD[[bio]])] <- mean(Bio_NAFLD_LRBF_SD[[bio]], na.rm=T)
        Diet_NAFLD_LRBF_SD[[dieti]][is.na(Diet_NAFLD_LRBF_SD[[dieti]])] <- median(Diet_NAFLD_LRBF_SD[[dieti]], na.rm=T)}}}}
#Cells
for (Cell2 in names(Cell_NAFLD_LR2_SD)){ 
  for (Cellaf in names(Cell_NAFLD_LRAF_SD)){
    for (Cellbf in names(Cell_NAFLD_LRBF_SD)){
      for (Cellc in names(Cell_NAFLD_LRC_SD)){
        Cell_NAFLD_LRC_SD[[Cellc]][is.na(Cell_NAFLD_LRC_SD[[Cellc]])] <- mean(Cell_NAFLD_LRC_SD[[Cellc]], na.rm=T)
        Cell_NAFLD_LRBF_SD[[Cellbf]][is.na(Cell_NAFLD_LRBF_SD[[Cellbf]])] <- mean(Cell_NAFLD_LRBF_SD[[Cellbf]], na.rm=T)
        Cell_NAFLD_LRAF_SD[[Cellaf]][is.na(Cell_NAFLD_LRAF_SD[[Cellaf]])] <- mean(Cell_NAFLD_LRAF_SD[[Cellaf]], na.rm=T)
        Cell_NAFLD_LR2_SD[[Cell2]][is.na(Cell_NAFLD_LR2_SD[[Cell2]])] <- mean(Cell_NAFLD_LR2_SD[[Cell2]], na.rm=T)
    }}}}
#Clinical
names(Clin_NAFLD_LRAF_SD[,c(8:10,12, 24,25,30,31)])
names(Clin_NAFLD_LRAF_SD[,c(13:19,21:22,26)])
for (clina in names(Clin_NAFLD_LRAF_SD[,c(8:10,12, 24,25,30,31)])) { #Clinical
  for(clinb in names(Clin_NAFLD_LRAF_SD[,c(13:19,21:22,26)])){
    Clin_NAFLD_LRC_SD[[clina]][is.na(Clin_NAFLD_LRC_SD[[clina]])] <- median(Clin_NAFLD_LRC_SD[[clina]], na.rm=T)
    Clin_NAFLD_LRC_SD[[clinb]][is.na(Clin_NAFLD_LRC_SD[[clinb]])] <- mean(Clin_NAFLD_LRC_SD[[clinb]], na.rm=T)
    Clin_NAFLD_LRBF_SD[[clina]][is.na(Clin_NAFLD_LRBF_SD[[clina]])] <- median(Clin_NAFLD_LRBF_SD[[clina]], na.rm=T)
    Clin_NAFLD_LRBF_SD[[clinb]][is.na(Clin_NAFLD_LRBF_SD[[clinb]])] <- mean(Clin_NAFLD_LRBF_SD[[clinb]], na.rm=T)
    Clin_NAFLD_LRAF_SD[[clina]][is.na(Clin_NAFLD_LRAF_SD[[clina]])] <- median(Clin_NAFLD_LRAF_SD[[clina]], na.rm=T)
    Clin_NAFLD_LRAF_SD[[clinb]][is.na(Clin_NAFLD_LRAF_SD[[clinb]])] <- mean(Clin_NAFLD_LRAF_SD[[clinb]], na.rm=T)
    Clin_NAFLD_LR2_SD[[clina]][is.na(Clin_NAFLD_LR2_SD[[clina]])] <- median(Clin_NAFLD_LR2_SD[[clina]], na.rm=T)
    Clin_NAFLD_LR2_SD[[clinb]][is.na(Clin_NAFLD_LR2_SD[[clinb]])] <- mean(Clin_NAFLD_LR2_SD[[clinb]], na.rm=T)
  }
}




NAFLD_LRC_SD <- cbind(NAFLDAndNASHMatchedControl$eid, Cell_NAFLD_LRC_SD, Diet_NAFLD_LRC_SD, Diet_NAFLD_LGC, Diet_NAFLD_LCC, Diet_NAFLD_LMNC,
                       Bio_NAFLD_LRC_SD, Clin_NAFLD_LRC_SD)
NAFLD_LRBF_SD <- cbind(NAFLDAndNASHMatchedBefore$eid, Cell_NAFLD_LRBF_SD, Diet_NAFLD_LRBF_SD, Diet_NAFLD_LGBF, Diet_NAFLD_LCBF, Diet_NAFLD_LMNBF,
                       Bio_NAFLD_LRBF_SD, Clin_NAFLD_LRBF_SD)
NAFLD_LRAF_SD <- cbind(NAFLDAndNASHMatchedAfter$eid, Cell_NAFLD_LRAF_SD, Diet_NAFLD_LRAF_SD, Diet_NAFLD_LGAF, Diet_NAFLD_LCAF, Diet_NAFLD_LMNAF,
                       Bio_NAFLD_LRAF_SD, Clin_NAFLD_LRAF_SD)
NAFLD_LR2_SD <- cbind(NAFLDAndNASHMatched_2$eid, Cell_NAFLD_LR2_SD, Diet_NAFLD_LR2_SD, Diet_NAFLD_LG2, Diet_NAFLD_LC2, Diet_NAFLD_LMN2,
                      Bio_NAFLD_LR2_SD, Clin_NAFLD_LR2_SD)


names(NAFLD_LRC_SD)[names(NAFLD_LRC_SD) == "Body_mass_index_(BMI)"] <- "Body_mass_index"
names(NAFLD_LRBF_SD)[names(NAFLD_LRBF_SD) == "Body_mass_index_(BMI)"] <- "Body_mass_index"
names(NAFLD_LRAF_SD)[names(NAFLD_LRAF_SD) == "Body_mass_index_(BMI)"] <- "Body_mass_index"
names(NAFLD_LR2_SD)[names(NAFLD_LR2_SD) == "Body_mass_index_(BMI)"] <- "Body_mass_index"
names(NAFLD_LRC_SD)[names(NAFLD_LRC_SD) == "Sex"] <- "genetic_sex_male"
names(NAFLD_LRBF_SD)[names(NAFLD_LRBF_SD) == "Sex"] <- "genetic_sex_male"
names(NAFLD_LRAF_SD)[names(NAFLD_LRAF_SD) == "Sex"] <- "genetic_sex_male"
names(NAFLD_LR2_SD)[names(NAFLD_LR2_SD) == "Sex"] <- "genetic_sex_male"
NAFLD_LRC_SD <- NAFLD_LRC_SD%>% rename(eid = 1)
NAFLD_LRBF_SD <- NAFLD_LRBF_SD%>% rename(eid = 1)
NAFLD_LRAF_SD <- NAFLD_LRAF_SD%>% rename(eid = 1)
NAFLD_LR2_SD <- NAFLD_LR2_SD%>% rename(eid = 1)



write.csv(NAFLD_LRBF_SD, "University/MSc/Files/BeforeNAFLDdata.csv")
write.csv(NAFLD_LRAF_SD, "University/MSc/Files/AfterNAFLDdata.csv")
write.csv(NAFLD_LR2_SD, "University/MSc/Files/Within2NAFLDdata.csv")
write.csv(NAFLD_LRC_SD, "University/MSc/Files/Controldata.csv")

