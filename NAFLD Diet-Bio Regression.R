
#+-2 years
#Linear Regression
Results_2_lm <- list()
set.seed(132)
for (diet in names(Diet_NAFLD_LR2_SD)){
  for (bio in names(Bio_NAFLD_LR2_SD)){
    Coefficients <- data.frame(summary(lm(Bio_NAFLD_LR2_SD[[bio]] ~ Diet_NAFLD_LR2_SD[[diet]] + NAFLD_LR2_SD$genetic_sex_male + 
                                            NAFLD_LR2_SD$age_recruitment + NAFLD_LR2_SD$Body_mass_index))$coefficients)
    Results_2_lm[[diet]][[bio]] <- Coefficients[2,] %>%
      add_column(Intercept = Coefficients$Estimate[1]) %>%
      add_column(Name = paste0(diet, "&", bio)) }
}
D_B_2.rg1 <- do.call( "rbind", Results_2_lm)
D_B_2.rg2 <- do.call( "rbind", D_B_2.rg1)
rownames(D_B_2.rg2) <- NULL
D_B_2.rg2$Pr...t.. <- as.numeric(D_B_2.rg2$Pr...t..)
D_B_2.rg2$Estimate <- as.numeric(D_B_2.rg2$Estimate)
D_B_2.rg3 <- D_B_2.rg2 %>%
  separate(Name, into = c("Nutrition", "Biomarker"), sep = "&") %>%
  rename( "p_value" = "Pr...t..") %>%
  mutate(Relationship = ifelse(Estimate > 0, "Positive", "Negative")) %>% 
  rename("Coefficient" = "Estimate") %>%
  select(c('p_value',  'Coefficient', 'Relationship', 'Intercept',  'Biomarker','Nutrition'))
D_B_2.rg4 <- D_B_2.rg3[order(D_B_2.rg3$p_value),] %>%
  filter(!Nutrition == "Tea_intake") %>%
  filter(!Nutrition == "Water_intake")
D_B_2.rg4$Bonferroni <- data.frame(mt.rawp2adjp(D_B_2.rg4$p_value, proc=c("Bonferroni"), alpha = 0.05, na.rm = FALSE)$adjp)$Bonferroni
Sig_D_B_2.rg5 <- D_B_2.rg4 %>%
  mutate(Significance = ifelse(Bonferroni < 0.05, "Significant", "Not significant" )) %>%
  filter(Significance == "Significant")
Sig_D_B_2.rg5[,c(1,7,2,4,5,6)]



#After
#Linear Regression
Results_AF_lm <- list()
set.seed(132) #make results reproducible ( same results every time you run)
for (diet in names(Diet_NAFLD_LRAF_SD)){
  for (bio in names(Bio_NAFLD_LRAF_SD)){
    Coefficients <- data.frame(summary(lm(Bio_NAFLD_LRAF_SD[[bio]] ~ Diet_NAFLD_LRAF_SD[[diet]]
                                          + NAFLD_LRAF_SD$genetic_sex_male + 
                                            NAFLD_LRAF_SD$age_recruitment + NAFLD_LRAF_SD$Body_mass_index))$coefficients)
    Results_AF_lm[[diet]][[bio]] <- Coefficients[2,] %>%
      add_column(Intercept = Coefficients$Estimate[1]) %>%
      add_column(Name = paste0(diet, "&", bio)) }
}
D_B_AF.rg1 <- do.call( "rbind", Results_AF_lm) #matrix with 2 relationships
D_B_AF.rg2 <- do.call( "rbind", D_B_AF.rg1) #final dataframe
rownames(D_B_AF.rg2) <- NULL
D_B_AF.rg2$Pr...t.. <- as.numeric(D_B_AF.rg2$Pr...t..)
D_B_AF.rg2$Estimate <- as.numeric(D_B_AF.rg2$Estimate)
D_B_AF.rg3 <- D_B_AF.rg2 %>%
  separate(Name, into = c("Nutrition", "Biomarker"), sep = "&") %>%
  rename( "p_value" = "Pr...t..") %>%
  mutate(Relationship = ifelse(Estimate > 0, "Positive", "Negative")) %>% # very difficult to have a 0 here 
  rename("Coefficient" = "Estimate") %>%
  select(c('p_value',  'Coefficient', 'Relationship', 'Intercept',  'Biomarker','Nutrition'))
D_B_AF.rg4 <- D_B_AF.rg3[order(D_B_AF.rg3$p_value),]%>%
  filter(!Nutrition == "Tea_intake") %>%
  filter(!Nutrition == "Water_intake")
D_B_AF.rg4$Bonferroni <- data.frame(mt.rawp2adjp(D_B_AF.rg4$p_value, proc=c("Bonferroni"), alpha = 0.05, na.rm = FALSE)$adjp)$Bonferroni
Sig_D_B_AF.rg5 <- D_B_AF.rg4 %>%
  mutate(Significance = ifelse(Bonferroni < 0.05, "Significant", "Not significant" )) %>%
  filter(Significance == "Significant")
Sig_D_B_AF.rg5[,c(1,7,2,3,5,6)]


#Before
#Linear Regression
Results_BF_lm <- list()
set.seed(132) #make results reproducible ( same results every time you run)
for (diet in names(Diet_NAFLD_LRBF_SD)){
  for (bio in names(Bio_NAFLD_LRBF_SD)){
    Coefficients <- data.frame(summary(lm(Bio_NAFLD_LRBF_SD[[bio]] ~ Diet_NAFLD_LRBF_SD[[diet]] + NAFLD_LRBF_SD$genetic_sex_male + 
                                            NAFLD_LRBF_SD$age_recruitment + NAFLD_LRBF_SD$Body_mass_index))$coefficients)
    Results_BF_lm[[diet]][[bio]] <- Coefficients[2,] %>%
      add_column(Intercept = Coefficients$Estimate[1]) %>%
      add_column(Name = paste0(diet, "&", bio)) }
}
D_B_BF.rg1 <- do.call( "rbind", Results_BF_lm) #matrix with 2 relationships
D_B_BF.rg2 <- do.call( "rbind", D_B_BF.rg1) #final dataframe
rownames(D_B_BF.rg2) <- NULL
D_B_BF.rg2$Pr...t.. <- as.numeric(D_B_BF.rg2$Pr...t..)
D_B_BF.rg2$Estimate <- as.numeric(D_B_BF.rg2$Estimate)
D_B_BF.rg3 <- D_B_BF.rg2 %>%
  separate(Name, into = c("Nutrition", "Biomarker"), sep = "&") %>%
  rename( "p_value" = "Pr...t..") %>%
  mutate(Relationship = ifelse(Estimate > 0, "Positive", "Negative")) %>% # very difficult to have a 0 here 
  rename("Coefficient" = "Estimate") %>%
  select(c('p_value',  'Coefficient', 'Relationship', 'Intercept',  'Biomarker','Nutrition'))
D_B_BF.rg4 <- D_B_BF.rg3[order(D_B_BF.rg3$p_value),]%>%
  filter(!Nutrition == "Tea_intake") %>%
  filter(!Nutrition == "Water_intake")
D_B_BF.rg4$Bonferroni <- data.frame(mt.rawp2adjp(D_B_BF.rg4$p_value, proc=c("Bonferroni"), alpha = 0.05, na.rm = FALSE)$adjp)$Bonferroni
Sig_D_B_BF.rg5 <- D_B_BF.rg4 %>%
  mutate(Significance = ifelse(Bonferroni < 0.05, "Significant", "Not significant" )) %>%
  filter(Significance == "Significant")
Sig_D_B_BF.rg5[,c(1,7,2,3,5,6)]


#Control
#Linear Regression
Results_C_lm <- list()
set.seed(132) #make results reproducible ( same results every time you run)
for (diet in names(Diet_NAFLD_LRC_SD)){
  for (bio in names(Bio_NAFLD_LRC_SD)){
    Coefficients <- data.frame(summary(lm( Bio_NAFLD_LRC_SD[[bio]] ~ Diet_NAFLD_LRC_SD[[diet]]+ NAFLD_LRC_SD$genetic_sex_male + 
                                             NAFLD_LRC_SD$age_recruitment + NAFLD_LRC_SD$Body_mass_index))$coefficients)
    Results_C_lm[[diet]][[bio]] <- Coefficients[2,] %>%
      add_column(Intercept = Coefficients$Estimate[1]) %>%
      add_column(Name = paste0(diet, "&", bio)) }
}
D_B_C.rg1 <- do.call( "rbind", Results_C_lm) #matrix with 2 relationships
D_B_C.rg2 <- do.call( "rbind", D_B_C.rg1) #final dataframe
rownames(D_B_C.rg2) <- NULL
D_B_C.rg2$Pr...t.. <- as.numeric(D_B_C.rg2$Pr...t..)
D_B_C.rg2$Estimate <- as.numeric(D_B_C.rg2$Estimate)
D_B_C.rg3 <- D_B_C.rg2 %>%
  separate(Name, into = c("Nutrition", "Biomarker"), sep = "&") %>%
  rename( "p_value" = "Pr...t..") %>%
  mutate(Relationship = ifelse(Estimate > 0, "Positive", "Negative")) %>% # very difficult to have a 0 here 
  rename("Coefficient" = "Estimate") %>%
  select(c('p_value',  'Coefficient', 'Relationship', 'Intercept',  'Biomarker','Nutrition'))
D_B_C.rg4 <- D_B_C.rg3[order(D_B_C.rg3$p_value),]%>%
  filter(!Nutrition == "Tea_intake") %>%
  filter(!Nutrition == "Water_intake")
D_B_C.rg4$Bonferroni <- data.frame(mt.rawp2adjp(D_B_C.rg4$p_value, proc=c("Bonferroni"), alpha = 0.05, na.rm = FALSE)$adjp)$Bonferroni
Sig_D_B_C.rg5 <- D_B_C.rg4 %>%
  mutate(Significance = ifelse(Bonferroni < 0.05, "Significant", "Not significant" )) %>%
  filter(Significance == "Significant")
Sig_D_B_C.rg5[,c(1,7,2,3,5,6)]



#+-2 years
#Logistic Regression
Results_2_glm <- list()
set.seed(132)
for (diet in names(Diet_NAFLD_LG2)){
  for (bio in names(Bio_NAFLD_LR2_SD)){
    Coefficients <- data.frame(summary(glm( Diet_NAFLD_LG2[[diet]] ~ Bio_NAFLD_LR2_SD[[bio]] + NAFLD_LR2_SD$genetic_sex_male + 
                                              NAFLD_LR2_SD$age_recruitment + NAFLD_LR2_SD$Body_mass_index, family=binomial(link='logit')))$coefficients)
    Results_2_glm[[diet]][[bio]] <- Coefficients[2,] %>%
      add_column(Intercept = Coefficients$Estimate[1]) %>%
      add_column(Name = paste0(diet, "&", bio)) }
}
D_B_2.lg1 <- do.call( "rbind", Results_2_glm)
D_B_2.lg2 <- do.call( "rbind", D_B_2.lg1)
rownames(D_B_2.lg2) <- NULL
D_B_2.lg2$Pr...z.. <- as.numeric(D_B_2.lg2$Pr...z..)
D_B_2.lg2$Estimate <- as.numeric(D_B_2.lg2$Estimate)
D_B_2.lg3 <- D_B_2.lg2 %>%
  separate(Name, into = c("Nutrition", "Biomarker"), sep = "&") %>%
  rename( "p_value" = "Pr...z..") %>%
  mutate(Relationship = ifelse(Estimate > 0, "Positive", "Negative")) %>%
  rename("Coefficient" = "Estimate") %>%
  select(c('p_value',  'Coefficient', 'Relationship', 'Intercept',  'Biomarker','Nutrition'))
D_B_2.lg4 <- D_B_2.lg3[order(D_B_2.lg3$p_value),]
D_B_2.lg4$Bonferroni <- data.frame(mt.rawp2adjp(D_B_2.lg3$p_value, proc=c("Bonferroni"), alpha = 0.05, na.rm = FALSE)$adjp)$Bonferroni
Sig_D_B_2.lg5 <- D_B_2.lg4 %>%
  mutate(Significance = ifelse(Bonferroni < 0.05, "Significant", "Not significant" )) %>%
  filter(Significance == "Significant")
Sig_D_B_2.lg5[,c(1,7,2,3,5,6)]


#Before
#Logistic Regression
Results_BF_glm <- list()
set.seed(132)
for (diet in names(Diet_NAFLD_LGBF)){
  for (bio in names(Bio_NAFLD_LRBF_SD)){
    Coefficients <- data.frame(summary(glm( Diet_NAFLD_LGBF[[diet]] ~ Bio_NAFLD_LRBF_SD[[bio]] + NAFLD_LRBF_SD$genetic_sex_male + 
                                              NAFLD_LRBF_SD$age_recruitment + NAFLD_LRBF_SD$Body_mass_index, family=binomial(link='logit')))$coefficients)
    Results_BF_glm[[diet]][[bio]] <- Coefficients[2,] %>%
      add_column(Intercept = Coefficients$Estimate[1]) %>%
      add_column(Name = paste0(diet, "&", bio)) }
}
D_B_BF.lg1 <- do.call( "rbind", Results_BF_glm)
D_B_BF.LGBF <- do.call( "rbind", D_B_BF.lg1)
rownames(D_B_BF.LGBF) <- NULL
D_B_BF.LGBF$Pr...z.. <- as.numeric(D_B_BF.LGBF$Pr...z..)
D_B_BF.LGBF$Estimate <- as.numeric(D_B_BF.LGBF$Estimate)
D_B_BF.lg3 <- D_B_BF.LGBF %>%
  separate(Name, into = c("Nutrition", "Biomarker"), sep = "&") %>%
  rename( "p_value" = "Pr...z..") %>%
  mutate(Relationship = ifelse(Estimate > 0, "Positive", "Negative")) %>%
  rename("Coefficient" = "Estimate") %>%
  select(c('p_value',  'Coefficient', 'Relationship', 'Intercept',  'Biomarker','Nutrition'))
D_B_BF.lg4 <- D_B_BF.lg3[order(D_B_BF.lg3$p_value),]
D_B_BF.lg4$Bonferroni <- data.frame(mt.rawp2adjp(D_B_BF.lg3$p_value, proc=c("Bonferroni"), alpha = 0.05, na.rm = FALSE)$adjp)$Bonferroni
Sig_D_B_BF.lg5 <- D_B_BF.lg4 %>%
  mutate(Significance = ifelse(Bonferroni < 0.05, "Significant", "Not significant" )) %>%
  filter(Significance == "Significant")
Sig_D_B_BF.lg5[,c(1,7,2,3,5,6)]


#After
#Logistic Regression
Results_AF_glm <- list()
set.seed(132)
for (diet in names(Diet_NAFLD_LGAF)){
  for (bio in names(Bio_NAFLD_LRAF_SD)){
    Coefficients <- data.frame(summary(glm( Diet_NAFLD_LGAF[[diet]] ~ Bio_NAFLD_LRAF_SD[[bio]] + NAFLD_LRAF_SD$genetic_sex_male + 
                                              NAFLD_LRAF_SD$age_recruitment + NAFLD_LRAF_SD$Body_mass_index, family=binomial(link='logit')))$coefficients)
    Results_AF_glm[[diet]][[bio]] <- Coefficients[2,] %>%
      add_column(Intercept = Coefficients$Estimate[1]) %>%
      add_column(Name = paste0(diet, "&", bio)) }
}
D_B_AF.lg1 <- do.call( "rbind", Results_AF_glm)
D_B_AF.LGAF <- do.call( "rbind", D_B_AF.lg1)
rownames(D_B_AF.LGAF) <- NULL
D_B_AF.LGAF$Pr...z.. <- as.numeric(D_B_AF.LGAF$Pr...z..)
D_B_AF.LGAF$Estimate <- as.numeric(D_B_AF.LGAF$Estimate)
D_B_AF.lg3 <- D_B_AF.LGAF %>%
  separate(Name, into = c("Nutrition", "Biomarker"), sep = "&") %>%
  rename( "p_value" = "Pr...z..") %>%
  mutate(Relationship = ifelse(Estimate > 0, "Positive", "Negative")) %>%
  rename("Coefficient" = "Estimate") %>%
  select(c('p_value',  'Coefficient', 'Relationship', 'Intercept',  'Biomarker','Nutrition'))
D_B_AF.lg4 <- D_B_AF.lg3[order(D_B_AF.lg3$p_value),]
D_B_AF.lg4$Bonferroni <- data.frame(mt.rawp2adjp(D_B_AF.lg3$p_value, proc=c("Bonferroni"), alpha = 0.05, na.rm = FALSE)$adjp)$Bonferroni
Sig_D_B_AF.lg5 <- D_B_AF.lg4 %>%
  mutate(Significance = ifelse(Bonferroni < 0.05, "Significant", "Not significant" )) %>%
  filter(Significance == "Significant")
Sig_D_B_AF.lg5[,c(1,7,2,3,5,6)]



#Control
#Logistic Regression
Results_C_glm <- list()
set.seed(132)
for (diet in names(Diet_NAFLD_LGC)){
  for (bio in names(Bio_NAFLD_LRC_SD)){
    Coefficients <- data.frame(summary(glm( Diet_NAFLD_LGC[[diet]] ~ Bio_NAFLD_LRC_SD[[bio]] + NAFLD_LRC_SD$genetic_sex_male + 
                                              NAFLD_LRC_SD$age_recruitment + NAFLD_LRC_SD$Body_mass_index, family=binomial(link='logit')))$coefficients)
    Results_C_glm[[diet]][[bio]] <- Coefficients[2,] %>%
      add_column(Intercept = Coefficients$Estimate[1]) %>%
      add_column(Name = paste0(diet, "&", bio)) }
}
D_B_C.lg1 <- do.call( "rbind", Results_C_glm)
D_B_C.LGC <- do.call( "rbind", D_B_C.lg1)
rownames(D_B_C.LGC) <- NULL
D_B_C.LGC$Pr...z.. <- as.numeric(D_B_C.LGC$Pr...z..)
D_B_C.LGC$Estimate <- as.numeric(D_B_C.LGC$Estimate)
D_B_C.lg3 <- D_B_C.LGC %>%
  separate(Name, into = c("Nutrition", "Biomarker"), sep = "&") %>%
  rename( "p_value" = "Pr...z..") %>%
  mutate(Relationship = ifelse(Estimate > 0, "Positive", "Negative")) %>%
  rename("Coefficient" = "Estimate") %>%
  select(c('p_value',  'Coefficient', 'Relationship', 'Intercept',  'Biomarker','Nutrition'))
D_B_C.lg4 <- D_B_C.lg3[order(D_B_C.lg3$p_value),]
D_B_C.lg4$Bonferroni <- data.frame(mt.rawp2adjp(D_B_C.lg3$p_value, proc=c("Bonferroni"), alpha = 0.05, na.rm = FALSE)$adjp)$Bonferroni
Sig_D_B_C.lg5 <- D_B_C.lg4 %>%
  mutate(Significance = ifelse(Bonferroni < 0.05, "Significant", "Not significant" )) %>%
  filter(Significance == "Significant")
Sig_D_B_C.lg5[,c(1,7,3,5,6)]



#+-2 years
#Ordinal regression
lapply(Diet_NAFLD_LC2[, c(names(NAFLDAndNASHMatched_2[,c(35:38,40)]))], table)
table(Diet_NAFLD_LC2$Processed_meat_intake)
for (dietc in names(Diet_NAFLD_LC2)){
  Diet_NAFLD_LC2[[dietc]][Diet_NAFLD_LC2[[dietc]]=="0"] <- c("Never")
  Diet_NAFLD_LC2[[dietc]][Diet_NAFLD_LC2[[dietc]]=="1"] <- c("Less than once a week")
  Diet_NAFLD_LC2[[dietc]][Diet_NAFLD_LC2[[dietc]]=="2"] <- c("Once a week")
  Diet_NAFLD_LC2[[dietc]][Diet_NAFLD_LC2[[dietc]]=="3"] <- c("2-4 times a week")
  Diet_NAFLD_LC2[[dietc]][Diet_NAFLD_LC2[[dietc]]=="4"] <- c("5-6 times a week")
  Diet_NAFLD_LC2[[dietc]][Diet_NAFLD_LC2[[dietc]]=="5"] <- c("Once or more daily")
  Diet_NAFLD_LC2[[dietc]] = factor(Diet_NAFLD_LC2[[dietc]], 
                                   levels = c("Never", "Less than once a week", "Once a week", "2-4 times a week", "5-6 times a week", "Once or more daily"), 
                                   ordered = TRUE)
  }
table(Diet_NAFLD_LC2$Processed_meat_intake)
names(Diet_NAFLD_LC2)[names(Diet_NAFLD_LC2) == "Lamb/mutton_intake"] <- "Lamb_mutton_intake"
names(Diet_NAFLD_LC2)
Results_2_clma <- list()
Results_2_clmb <- list()
Results_2_clmc <- list()
Results_2_clmd <- list()
Results_2_clme <- list()
set.seed(132)
library(MASS)
for (bio in names(Bio_NAFLD_LR2_SD)){
    Coefficients <- (data.frame(summary(polr(Diet_NAFLD_LC2[["Processed_meat_intake"]] ~ Bio_NAFLD_LR2_SD[[bio]]
                                             + NAFLD_LR2_SD$genetic_sex_male + 
                                               NAFLD_LR2_SD$age_recruitment + NAFLD_LR2_SD$Body_mass_index, Hess=TRUE))$coefficients))[1,]
    Results_2_clma[["Processed_meat_intake"]][[bio]] <- Coefficients %>%
      add_column(p_value = (data.frame(pnorm(abs(coef(summary(polr(Diet_NAFLD_LC2[["Processed_meat_intake"]] ~ Bio_NAFLD_LR2_SD[[bio]]
                                                                   + NAFLD_LR2_SD$genetic_sex_male + 
                                                                     NAFLD_LR2_SD$age_recruitment + NAFLD_LR2_SD$Body_mass_index, Hess=TRUE)))
                                                 [, "t value"]), lower.tail = FALSE) * 2))[1,]) %>%
      add_column(Name = paste0("Processed_meat_intake", "&", bio))
  }
for (bio in names(Bio_NAFLD_LR2_SD)){
  Coefficients <- (data.frame(summary(polr(Diet_NAFLD_LC2[["Non_oily_fish_intake"]] ~ Bio_NAFLD_LR2_SD[[bio]]
                                           + NAFLD_LR2_SD$genetic_sex_male + 
                                             NAFLD_LR2_SD$age_recruitment + NAFLD_LR2_SD$Body_mass_index, Hess=TRUE))$coefficients))[1,]
  Results_2_clmb[["Non_oily_fish_intake"]][[bio]] <- Coefficients %>%
    add_column(p_value = (data.frame(pnorm(abs(coef(summary(polr(Diet_NAFLD_LC2[["Non_oily_fish_intake"]] ~ Bio_NAFLD_LR2_SD[[bio]]
                                                                 + NAFLD_LR2_SD$genetic_sex_male + 
                                                                   NAFLD_LR2_SD$age_recruitment + NAFLD_LR2_SD$Body_mass_index, Hess=TRUE)))
                                               [, "t value"]), lower.tail = FALSE) * 2))[1,]) %>%
    add_column(Name = paste0("Non_oily_fish_intake", "&", bio))
}
for (bio in names(Bio_NAFLD_LR2_SD)){
  Coefficients <- (data.frame(summary(polr(Diet_NAFLD_LC2[["Lamb_mutton_intake"]] ~ Bio_NAFLD_LR2_SD[[bio]]
                                           + NAFLD_LR2_SD$genetic_sex_male + 
                                             NAFLD_LR2_SD$age_recruitment + NAFLD_LR2_SD$Body_mass_index, Hess=TRUE))$coefficients))[1,]
  Results_2_clmc[["Lamb_mutton_intake"]][[bio]] <- Coefficients %>%
    add_column(p_value = (data.frame(pnorm(abs(coef(summary(polr(Diet_NAFLD_LC2[["Lamb_mutton_intake"]] ~ Bio_NAFLD_LR2_SD[[bio]]
                                                                 + NAFLD_LR2_SD$genetic_sex_male + 
                                                                   NAFLD_LR2_SD$age_recruitment + NAFLD_LR2_SD$Body_mass_index, Hess=TRUE)))
                                               [, "t value"]), lower.tail = FALSE) * 2))[1,]) %>%
    add_column(Name = paste0("Lamb_mutton_intake", "&", bio))
}
for (bio in names(Bio_NAFLD_LR2_SD)){
  Coefficients <- (data.frame(summary(polr(Diet_NAFLD_LC2[["Salt_added_to_food"]] ~ Bio_NAFLD_LR2_SD[[bio]]
                                           + NAFLD_LR2_SD$genetic_sex_male + 
                                             NAFLD_LR2_SD$age_recruitment + NAFLD_LR2_SD$Body_mass_index, Hess=TRUE))$coefficients))[1,]
  Results_2_clmd[["Salt_added_to_food"]][[bio]] <- Coefficients %>%
    add_column(p_value = (data.frame(pnorm(abs(coef(summary(polr(Diet_NAFLD_LC2[["Salt_added_to_food"]] ~ Bio_NAFLD_LR2_SD[[bio]]
                                                                 + NAFLD_LR2_SD$genetic_sex_male + 
                                                                   NAFLD_LR2_SD$age_recruitment + NAFLD_LR2_SD$Body_mass_index, Hess=TRUE)))
                                               [, "t value"]), lower.tail = FALSE) * 2))[1,]) %>%
    add_column(Name = paste0("Salt_added_to_food", "&", bio))
}
for (bio in names(Bio_NAFLD_LR2_SD)){
  Coefficients <- (data.frame(summary(polr(Diet_NAFLD_LC2[["Cheese_intake"]] ~ Bio_NAFLD_LR2_SD[[bio]] + NAFLD_LR2_SD$genetic_sex_male + 
                                             NAFLD_LR2_SD$age_recruitment + NAFLD_LR2_SD$Body_mass_index, Hess=TRUE))$coefficients))[1,]
  Results_2_clme[["Cheese_intake"]][[bio]] <- Coefficients %>%
    add_column(p_value = (data.frame(pnorm(abs(coef(summary(polr(Diet_NAFLD_LC2[["Cheese_intake"]] ~ Bio_NAFLD_LR2_SD[[bio]]
                                                                 + NAFLD_LR2_SD$genetic_sex_male + 
                                                                   NAFLD_LR2_SD$age_recruitment + NAFLD_LR2_SD$Body_mass_index, Hess=TRUE)))
                                               [, "t value"]), lower.tail = FALSE) * 2))[1,]) %>%
    add_column(Name = paste0("Cheese_intake", "&", bio))
}
detach("package:MASS", unload = TRUE)
D_B_2.LC1a <- do.call( "rbind", Results_2_clma)
D_B_2.LC2a <- do.call( "rbind", D_B_2.LC1a)
D_B_2.LC1b <- do.call( "rbind", Results_2_clmb)
D_B_2.LC2b <- do.call( "rbind", D_B_2.LC1b)
D_B_2.LC1c <- do.call( "rbind", Results_2_clmc)
D_B_2.LC2c <- do.call( "rbind", D_B_2.LC1c)
D_B_2.LC1d <- do.call( "rbind", Results_2_clmd)
D_B_2.LC2d <- do.call( "rbind", D_B_2.LC1d)
D_B_2.LC1e <- do.call( "rbind", Results_2_clme)
D_B_2.LC2e <- do.call( "rbind", D_B_2.LC1e)
D_B_2.LC2 <- rbind(D_B_2.LC2a, D_B_2.LC2b, D_B_2.LC2c, D_B_2.LC2d, D_B_2.LC2e)
rownames(D_B_2.LC2) <- NULL
D_B_2.LC2$Estimate <- as.numeric(D_B_2.LC2$Value)
D_B_2.LC2$p_value <- as.numeric(D_B_2.LC2$p_value)
D_B_2.LC3 <- D_B_2.LC2 %>%
  separate(Name, into = c("Nutrition", "Biomarker"), sep = "&") %>%
  mutate(Relationship = ifelse(Estimate > 0, "Positive", "Negative")) %>%
  rename("Coefficient" = "Estimate") %>%
  select(c('p_value',  'Coefficient', 'Relationship', 'Biomarker','Nutrition'))
D_B_2.LC3[["Nutrition"]][D_B_2.LC3[["Nutrition"]]=="Lamb_mutton_intake"] <- "Lamb/mutton_intake"
D_B_2.clm4 <- D_B_2.LC3[order(D_B_2.LC3$p_value),]
D_B_2.clm4$Bonferroni <- data.frame(mt.rawp2adjp(D_B_2.LC3$p_value, proc=c("Bonferroni"), alpha = 0.05, na.rm = FALSE)$adjp)$Bonferroni
Sig_D_B_2.clm5 <- D_B_2.clm4 %>%
  filter(Bonferroni > 0) %>%
  mutate(Significance = ifelse(Bonferroni < 0.05, "Significant", "Not significant" )) %>%
  filter(Significance == "Significant")
Sig_D_B_2.clm5[,c(1,6,2,3,4,5)]


#Before
#Ordinal regression
lapply(Diet_NAFLD_LCBF[, c(names(NAFLDAndNASHMatchedBefore[,c(35:38,40)]))], table)
table(Diet_NAFLD_LCBF$Processed_meat_intake)
for (dietc in names(Diet_NAFLD_LCBF)){
  Diet_NAFLD_LCBF[[dietc]][Diet_NAFLD_LCBF[[dietc]]=="0"] <- c("Never")
  Diet_NAFLD_LCBF[[dietc]][Diet_NAFLD_LCBF[[dietc]]=="1"] <- c("Less than once a week")
  Diet_NAFLD_LCBF[[dietc]][Diet_NAFLD_LCBF[[dietc]]=="2"] <- c("Once a week")
  Diet_NAFLD_LCBF[[dietc]][Diet_NAFLD_LCBF[[dietc]]=="3"] <- c("2-4 times a week")
  Diet_NAFLD_LCBF[[dietc]][Diet_NAFLD_LCBF[[dietc]]=="4"] <- c("5-6 times a week")
  Diet_NAFLD_LCBF[[dietc]][Diet_NAFLD_LCBF[[dietc]]=="5"] <- c("Once or more daily")
  Diet_NAFLD_LCBF[[dietc]] = factor(Diet_NAFLD_LCBF[[dietc]], 
                                   levels = c("Never", "Less than once a week", "Once a week", "2-4 times a week", "5-6 times a week", "Once or more daily"), 
                                   ordered = TRUE)
}
table(Diet_NAFLD_LCBF$Processed_meat_intake)
names(Diet_NAFLD_LCBF)[names(Diet_NAFLD_LCBF) == "Lamb/mutton_intake"] <- "Lamb_mutton_intake"
names(Diet_NAFLD_LCBF)
Results_BF_clma <- list()
Results_BF_clmb <- list()
Results_BF_clmc <- list()
Results_BF_clmd <- list()
Results_BF_clme <- list()
set.seed(132)
library(MASS)
for (bio in names(Bio_NAFLD_LRBF_SD)){
  Coefficients <- (data.frame(summary(polr(Diet_NAFLD_LCBF[["Processed_meat_intake"]] ~ Bio_NAFLD_LRBF_SD[[bio]]
                                           + NAFLD_LRBF_SD$genetic_sex_male + 
                                             NAFLD_LRBF_SD$age_recruitment + NAFLD_LRBF_SD$Body_mass_index, Hess=TRUE))$coefficients))[1,]
  Results_BF_clma[["Processed_meat_intake"]][[bio]] <- Coefficients %>%
    add_column(p_value = (data.frame(pnorm(abs(coef(summary(polr(Diet_NAFLD_LCBF[["Processed_meat_intake"]] ~ Bio_NAFLD_LRBF_SD[[bio]]
                                                                 + NAFLD_LRBF_SD$genetic_sex_male + 
                                                                   NAFLD_LRBF_SD$age_recruitment + NAFLD_LRBF_SD$Body_mass_index, Hess=TRUE)))
                                               [, "t value"]), lower.tail = FALSE) * 2))[1,]) %>%
    add_column(Name = paste0("Processed_meat_intake", "&", bio))
}
for (bio in names(Bio_NAFLD_LRBF_SD)){
  Coefficients <- (data.frame(summary(polr(Diet_NAFLD_LCBF[["Non_oily_fish_intake"]] ~ Bio_NAFLD_LRBF_SD[[bio]]
                                           + NAFLD_LRBF_SD$genetic_sex_male + 
                                             NAFLD_LRBF_SD$age_recruitment + NAFLD_LRBF_SD$Body_mass_index, Hess=TRUE))$coefficients))[1,]
  Results_BF_clmb[["Non_oily_fish_intake"]][[bio]] <- Coefficients %>%
    add_column(p_value = (data.frame(pnorm(abs(coef(summary(polr(Diet_NAFLD_LCBF[["Non_oily_fish_intake"]] ~ Bio_NAFLD_LRBF_SD[[bio]]
                                                                 + NAFLD_LRBF_SD$genetic_sex_male + 
                                                                   NAFLD_LRBF_SD$age_recruitment + NAFLD_LRBF_SD$Body_mass_index, Hess=TRUE)))
                                               [, "t value"]), lower.tail = FALSE) * 2))[1,]) %>%
    add_column(Name = paste0("Non_oily_fish_intake", "&", bio))
}
for (bio in names(Bio_NAFLD_LRBF_SD)){
  Coefficients <- (data.frame(summary(polr(Diet_NAFLD_LCBF[["Lamb_mutton_intake"]] ~ Bio_NAFLD_LRBF_SD[[bio]] + NAFLD_LRBF_SD$genetic_sex_male + 
                                             NAFLD_LRBF_SD$age_recruitment + NAFLD_LRBF_SD$Body_mass_index, Hess=TRUE))$coefficients))[1,]
  Results_BF_clmc[["Lamb_mutton_intake"]][[bio]] <- Coefficients %>%
    add_column(p_value = (data.frame(pnorm(abs(coef(summary(polr(Diet_NAFLD_LCBF[["Lamb_mutton_intake"]] ~ Bio_NAFLD_LRBF_SD[[bio]]
                                                                 + NAFLD_LRBF_SD$genetic_sex_male + 
                                                                   NAFLD_LRBF_SD$age_recruitment + NAFLD_LRBF_SD$Body_mass_index, Hess=TRUE)))
                                               [, "t value"]), lower.tail = FALSE) * 2))[1,]) %>%
    add_column(Name = paste0("Lamb_mutton_intake", "&", bio))
}
for (bio in names(Bio_NAFLD_LRBF_SD)){
  Coefficients <- (data.frame(summary(polr(Diet_NAFLD_LCBF[["Salt_added_to_food"]] ~ Bio_NAFLD_LRBF_SD[[bio]]
                                           + NAFLD_LRBF_SD$genetic_sex_male + 
                                             NAFLD_LRBF_SD$age_recruitment + NAFLD_LRBF_SD$Body_mass_index, Hess=TRUE))$coefficients))[1,]
  Results_BF_clmd[["Salt_added_to_food"]][[bio]] <- Coefficients %>%
    add_column(p_value = (data.frame(pnorm(abs(coef(summary(polr(Diet_NAFLD_LCBF[["Salt_added_to_food"]] ~ Bio_NAFLD_LRBF_SD[[bio]]
                                                                 + NAFLD_LRBF_SD$genetic_sex_male + 
                                                                   NAFLD_LRBF_SD$age_recruitment + NAFLD_LRBF_SD$Body_mass_index, Hess=TRUE)))
                                               [, "t value"]), lower.tail = FALSE) * 2))[1,]) %>%
    add_column(Name = paste0("Salt_added_to_food", "&", bio))
}
for (bio in names(Bio_NAFLD_LRBF_SD)){
  Coefficients <- (data.frame(summary(polr(Diet_NAFLD_LCBF[["Cheese_intake"]] ~ Bio_NAFLD_LRBF_SD[[bio]]
                                           + NAFLD_LRBF_SD$genetic_sex_male + 
                                             NAFLD_LRBF_SD$age_recruitment + NAFLD_LRBF_SD$Body_mass_index, Hess=TRUE))$coefficients))[1,]
  Results_BF_clme[["Cheese_intake"]][[bio]] <- Coefficients %>%
    add_column(p_value = (data.frame(pnorm(abs(coef(summary(polr(Diet_NAFLD_LCBF[["Cheese_intake"]] ~ Bio_NAFLD_LRBF_SD[[bio]]
                                                                 + NAFLD_LRBF_SD$genetic_sex_male + 
                                                                   NAFLD_LRBF_SD$age_recruitment + NAFLD_LRBF_SD$Body_mass_index, Hess=TRUE)))
                                               [, "t value"]), lower.tail = FALSE) * 2))[1,]) %>%
    add_column(Name = paste0("Cheese_intake", "&", bio))
}
detach("package:MASS", unload = TRUE)
D_B_BF.LC1a <- do.call( "rbind", Results_BF_clma)
D_B_BF.LCBFa <- do.call( "rbind", D_B_BF.LC1a)
D_B_BF.LC1b <- do.call( "rbind", Results_BF_clmb)
D_B_BF.LCBFb <- do.call( "rbind", D_B_BF.LC1b)
D_B_BF.LC1c <- do.call( "rbind", Results_BF_clmc)
D_B_BF.LCBFc <- do.call( "rbind", D_B_BF.LC1c)
D_B_BF.LC1d <- do.call( "rbind", Results_BF_clmd)
D_B_BF.LCBFd <- do.call( "rbind", D_B_BF.LC1d)
D_B_BF.LC1e <- do.call( "rbind", Results_BF_clme)
D_B_BF.LCBFe <- do.call( "rbind", D_B_BF.LC1e)
D_B_BF.LCBF <- rbind(D_B_BF.LCBFa, D_B_BF.LCBFb, D_B_BF.LCBFc, D_B_BF.LCBFd, D_B_BF.LCBFe)
rownames(D_B_BF.LCBF) <- NULL
D_B_BF.LCBF$Estimate <- as.numeric(D_B_BF.LCBF$Value)
D_B_BF.LCBF$p_value <- as.numeric(D_B_BF.LCBF$p_value)
D_B_BF.LC3 <- D_B_BF.LCBF %>%
  separate(Name, into = c("Nutrition", "Biomarker"), sep = "&") %>%
  mutate(Relationship = ifelse(Estimate > 0, "Positive", "Negative")) %>%
  rename("Coefficient" = "Estimate") %>%
  select(c('p_value',  'Coefficient', 'Relationship', 'Biomarker','Nutrition'))
D_B_BF.LC3[["Nutrition"]][D_B_BF.LC3[["Nutrition"]]=="Lamb_mutton_intake"] <- "Lamb/mutton_intake"
D_B_BF.clm4 <- D_B_BF.LC3[order(D_B_BF.LC3$p_value),]
D_B_BF.clm4$Bonferroni <- data.frame(mt.rawp2adjp(D_B_BF.LC3$p_value, proc=c("Bonferroni"), alpha = 0.05, na.rm = FALSE)$adjp)$Bonferroni
Sig_D_B_BF.clm5 <- D_B_BF.clm4 %>%
  filter(Bonferroni > 0) %>%
  mutate(Significance = ifelse(Bonferroni < 0.05, "Significant", "Not significant" )) %>%
  filter(Significance == "Significant")
Sig_D_B_BF.clm5[,c(1,6,2,3,4,5)]


#After
#Ordinal regression
lapply(Diet_NAFLD_LCAF[, c(names(NAFLDAndNASHMatchedAfter[,c(35:38,40)]))], table)
table(Diet_NAFLD_LCAF$Processed_meat_intake)
for (dietc in names(Diet_NAFLD_LCAF)){
  Diet_NAFLD_LCAF[[dietc]][Diet_NAFLD_LCAF[[dietc]]=="0"] <- c("Never")
  Diet_NAFLD_LCAF[[dietc]][Diet_NAFLD_LCAF[[dietc]]=="1"] <- c("Less than once a week")
  Diet_NAFLD_LCAF[[dietc]][Diet_NAFLD_LCAF[[dietc]]=="2"] <- c("Once a week")
  Diet_NAFLD_LCAF[[dietc]][Diet_NAFLD_LCAF[[dietc]]=="3"] <- c("2-4 times a week")
  Diet_NAFLD_LCAF[[dietc]][Diet_NAFLD_LCAF[[dietc]]=="4"] <- c("5-6 times a week")
  Diet_NAFLD_LCAF[[dietc]][Diet_NAFLD_LCAF[[dietc]]=="5"] <- c("Once or more daily")
  Diet_NAFLD_LCAF[[dietc]] = factor(Diet_NAFLD_LCAF[[dietc]], 
                                    levels = c("Never", "Less than once a week", "Once a week", "2-4 times a week", "5-6 times a week", "Once or more daily"), 
                                    ordered = TRUE)
}
table(Diet_NAFLD_LCAF$Processed_meat_intake)
names(Diet_NAFLD_LCAF)[names(Diet_NAFLD_LCAF) == "Lamb/mutton_intake"] <- "Lamb_mutton_intake"
names(Diet_NAFLD_LCAF)
Results_AF_clma <- list()
Results_AF_clmb <- list()
Results_AF_clmc <- list()
Results_AF_clmd <- list()
Results_AF_clme <- list()
set.seed(132)
library(MASS)
for (bio in names(Bio_NAFLD_LRAF_SD)){
  Coefficients <- (data.frame(summary(polr(Diet_NAFLD_LCAF[["Processed_meat_intake"]] ~ Bio_NAFLD_LRAF_SD[[bio]]
                                           + NAFLD_LRAF_SD$genetic_sex_male + 
                                             NAFLD_LRAF_SD$age_recruitment + NAFLD_LRAF_SD$Body_mass_index, Hess=TRUE))$coefficients))[1,]
  Results_AF_clma[["Processed_meat_intake"]][[bio]] <- Coefficients %>%
    add_column(p_value = (data.frame(pnorm(abs(coef(summary(polr(Diet_NAFLD_LCAF[["Processed_meat_intake"]] ~ Bio_NAFLD_LRAF_SD[[bio]]
                                                                 + NAFLD_LRAF_SD$genetic_sex_male + 
                                                                   NAFLD_LRAF_SD$age_recruitment + NAFLD_LRAF_SD$Body_mass_index, Hess=TRUE)))
                                               [, "t value"]), lower.tail = FALSE) * 2))[1,]) %>%
    add_column(Name = paste0("Processed_meat_intake", "&", bio))
}
for (bio in names(Bio_NAFLD_LRAF_SD)){
  Coefficients <- (data.frame(summary(polr(Diet_NAFLD_LCAF[["Non_oily_fish_intake"]] ~ Bio_NAFLD_LRAF_SD[[bio]] + NAFLD_LRAF_SD$genetic_sex_male + 
                                             NAFLD_LRAF_SD$age_recruitment + NAFLD_LRAF_SD$Body_mass_index, Hess=TRUE))$coefficients))[1,]
  Results_AF_clmb[["Non_oily_fish_intake"]][[bio]] <- Coefficients %>%
    add_column(p_value = (data.frame(pnorm(abs(coef(summary(polr(Diet_NAFLD_LCAF[["Non_oily_fish_intake"]] ~ Bio_NAFLD_LRAF_SD[[bio]]
                                                                 + NAFLD_LRAF_SD$genetic_sex_male + 
                                                                   NAFLD_LRAF_SD$age_recruitment + NAFLD_LRAF_SD$Body_mass_index, Hess=TRUE)))
                                               [, "t value"]), lower.tail = FALSE) * 2))[1,]) %>%
    add_column(Name = paste0("Non_oily_fish_intake", "&", bio))
}
for (bio in names(Bio_NAFLD_LRAF_SD)){
  Coefficients <- (data.frame(summary(polr(Diet_NAFLD_LCAF[["Lamb_mutton_intake"]] ~ Bio_NAFLD_LRAF_SD[[bio]] + NAFLD_LRAF_SD$genetic_sex_male + 
                                             NAFLD_LRAF_SD$age_recruitment + NAFLD_LRAF_SD$Body_mass_index, Hess=TRUE))$coefficients))[1,]
  Results_AF_clmc[["Lamb_mutton_intake"]][[bio]] <- Coefficients %>%
    add_column(p_value = (data.frame(pnorm(abs(coef(summary(polr(Diet_NAFLD_LCAF[["Lamb_mutton_intake"]] ~ Bio_NAFLD_LRAF_SD[[bio]]
                                                                 + NAFLD_LRAF_SD$genetic_sex_male + 
                                                                   NAFLD_LRAF_SD$age_recruitment + NAFLD_LRAF_SD$Body_mass_index, Hess=TRUE)))
                                               [, "t value"]), lower.tail = FALSE) * 2))[1,]) %>%
    add_column(Name = paste0("Lamb_mutton_intake", "&", bio))
}
for (bio in names(Bio_NAFLD_LRAF_SD)){
  Coefficients <- (data.frame(summary(polr(Diet_NAFLD_LCAF[["Salt_added_to_food"]] ~ Bio_NAFLD_LRAF_SD[[bio]] + NAFLD_LRAF_SD$genetic_sex_male + 
                                             NAFLD_LRAF_SD$age_recruitment + NAFLD_LRAF_SD$Body_mass_index, Hess=TRUE))$coefficients))[1,]
  Results_AF_clmd[["Salt_added_to_food"]][[bio]] <- Coefficients %>%
    add_column(p_value = (data.frame(pnorm(abs(coef(summary(polr(Diet_NAFLD_LCAF[["Salt_added_to_food"]] ~ Bio_NAFLD_LRAF_SD[[bio]] 
                                                                 + NAFLD_LRAF_SD$genetic_sex_male + 
                                                                   NAFLD_LRAF_SD$age_recruitment + NAFLD_LRAF_SD$Body_mass_index, Hess=TRUE)))
                                               [, "t value"]), lower.tail = FALSE) * 2))[1,]) %>%
    add_column(Name = paste0("Salt_added_to_food", "&", bio))
}
for (bio in names(Bio_NAFLD_LRAF_SD)){
  Coefficients <- (data.frame(summary(polr(Diet_NAFLD_LCAF[["Cheese_intake"]] ~ Bio_NAFLD_LRAF_SD[[bio]]+ NAFLD_LRAF_SD$genetic_sex_male + 
                                             NAFLD_LRAF_SD$age_recruitment + NAFLD_LRAF_SD$Body_mass_index, Hess=TRUE))$coefficients))[1,]
  Results_AF_clme[["Cheese_intake"]][[bio]] <- Coefficients %>%
    add_column(p_value = (data.frame(pnorm(abs(coef(summary(polr(Diet_NAFLD_LCAF[["Cheese_intake"]] ~ Bio_NAFLD_LRAF_SD[[bio]]
                                                                 + NAFLD_LRAF_SD$genetic_sex_male + 
                                                                   NAFLD_LRAF_SD$age_recruitment + NAFLD_LRAF_SD$Body_mass_index, Hess=TRUE)))
                                               [, "t value"]), lower.tail = FALSE) * 2))[1,]) %>%
    add_column(Name = paste0("Cheese_intake", "&", bio))
}
detach("package:MASS", unload = TRUE)
D_B_AF.LC1a <- do.call( "rbind", Results_AF_clma)
D_B_AF.LCAFa <- do.call( "rbind", D_B_AF.LC1a)
D_B_AF.LC1b <- do.call( "rbind", Results_AF_clmb)
D_B_AF.LCAFb <- do.call( "rbind", D_B_AF.LC1b)
D_B_AF.LC1c <- do.call( "rbind", Results_AF_clmc)
D_B_AF.LCAFc <- do.call( "rbind", D_B_AF.LC1c)
D_B_AF.LC1d <- do.call( "rbind", Results_AF_clmd)
D_B_AF.LCAFd <- do.call( "rbind", D_B_AF.LC1d)
D_B_AF.LC1e <- do.call( "rbind", Results_AF_clme)
D_B_AF.LCAFe <- do.call( "rbind", D_B_AF.LC1e)
D_B_AF.LCAF <- rbind(D_B_AF.LCAFa, D_B_AF.LCAFb, D_B_AF.LCAFc, D_B_AF.LCAFd, D_B_AF.LCAFe)
rownames(D_B_AF.LCAF) <- NULL
D_B_AF.LCAF$Estimate <- as.numeric(D_B_AF.LCAF$Value)
D_B_AF.LCAF$p_value <- as.numeric(D_B_AF.LCAF$p_value)
D_B_AF.LC3 <- D_B_AF.LCAF %>%
  separate(Name, into = c("Nutrition", "Biomarker"), sep = "&") %>%
  mutate(Relationship = ifelse(Estimate > 0, "Positive", "Negative")) %>%
  rename("Coefficient" = "Estimate") %>%
  select(c('p_value',  'Coefficient', 'Relationship', 'Biomarker','Nutrition'))
D_B_AF.LC3[["Nutrition"]][D_B_AF.LC3[["Nutrition"]]=="Lamb_mutton_intake"] <- "Lamb/mutton_intake"
D_B_AF.clm4 <- D_B_AF.LC3[order(D_B_AF.LC3$p_value),]
D_B_AF.clm4$Bonferroni <- data.frame(mt.rawp2adjp(D_B_AF.LC3$p_value, proc=c("Bonferroni"), alpha = 0.05, na.rm = FALSE)$adjp)$Bonferroni
Sig_D_B_AF.clm5 <- D_B_AF.clm4 %>%
  filter(Bonferroni > 0) %>%
  mutate(Significance = ifelse(Bonferroni < 0.05, "Significant", "Not significant" )) %>%
  filter(Significance == "Significant")
Sig_D_B_AF.clm5[,c(1,6,2,3,4,5)]


#Control
#Ordinal regression
lapply(Diet_NAFLD_LCC[, c(names(NAFLDAndNASHMatchedControl[,c(35:38,40)]))], table)
for (dietc in names(Diet_NAFLD_LCC)){
  Diet_NAFLD_LCC[[dietc]][Diet_NAFLD_LCC[[dietc]]=="0"] <- c("Never")
  Diet_NAFLD_LCC[[dietc]][Diet_NAFLD_LCC[[dietc]]=="1"] <- c("Less than once a week")
  Diet_NAFLD_LCC[[dietc]][Diet_NAFLD_LCC[[dietc]]=="2"] <- c("Once a week")
  Diet_NAFLD_LCC[[dietc]][Diet_NAFLD_LCC[[dietc]]=="3"] <- c("2-4 times a week")
  Diet_NAFLD_LCC[[dietc]][Diet_NAFLD_LCC[[dietc]]=="4"] <- c("5-6 times a week")
  Diet_NAFLD_LCC[[dietc]][Diet_NAFLD_LCC[[dietc]]=="5"] <- c("Once or more daily")
  Diet_NAFLD_LCC[[dietc]] = factor(Diet_NAFLD_LCC[[dietc]], 
                                     levels = c("Never", "Less than once a week", "Once a week", "2-4 times a week", "5-6 times a week", "Once or more daily"), 
                                     ordered = TRUE)
}
table(Diet_NAFLD_LCC$Processed_meat_intake)
names(Diet_NAFLD_LCC)[names(Diet_NAFLD_LCC) == "Lamb/mutton_intake"] <- "Lamb_mutton_intake"
names(Bio_NAFLD_LRC_SD)[names(Bio_NAFLD_LRC_SD) == "Creatinine_(enzymatic)_in_urine"] <- "Creatinine_in_urine"
names(Diet_NAFLD_LCC)
Results_Control_clma <- list()
Results_Control_clmb <- list()
Results_Control_clmc <- list()
Results_Control_clmd <- list()
Results_Control_clme <- list()
set.seed(132)
library(MASS)
for (bio in names(Bio_NAFLD_LRC_SD)){
  Coefficients <- (data.frame(summary(polr(Diet_NAFLD_LCC[["Processed_meat_intake"]] ~ Bio_NAFLD_LRC_SD[[bio]]
                                           + NAFLD_LRC_SD$genetic_sex_male + 
                                             NAFLD_LRC_SD$age_recruitment + NAFLD_LRC_SD$Body_mass_index, Hess=TRUE))$coefficients))[1,]
  Results_Control_clma[["Processed_meat_intake"]][[bio]] <- Coefficients %>%
    add_column(p_value = (data.frame(pnorm(abs(coef(summary(polr(Diet_NAFLD_LCC[["Processed_meat_intake"]] ~ Bio_NAFLD_LRC_SD[[bio]]
                                                                 + NAFLD_LRC_SD$genetic_sex_male + 
                                                                   NAFLD_LRC_SD$age_recruitment + NAFLD_LRC_SD$Body_mass_index, Hess=TRUE)))
                                               [, "t value"]), lower.tail = FALSE) * 2))[1,]) %>%
    add_column(Name = paste0("Processed_meat_intake", "&", bio))
}
for (bio in names(Bio_NAFLD_LRC_SD)){
  Coefficients <- (data.frame(summary(polr(Diet_NAFLD_LCC[["Non_oily_fish_intake"]] ~ Bio_NAFLD_LRC_SD[[bio]]
                                           + NAFLD_LRC_SD$genetic_sex_male + 
                                             NAFLD_LRC_SD$age_recruitment + NAFLD_LRC_SD$Body_mass_index, Hess=TRUE))$coefficients))[1,]
  Results_Control_clmb[["Non_oily_fish_intake"]][[bio]] <- Coefficients %>%
    add_column(p_value = (data.frame(pnorm(abs(coef(summary(polr(Diet_NAFLD_LCC[["Non_oily_fish_intake"]] ~ Bio_NAFLD_LRC_SD[[bio]]
                                                                 + NAFLD_LRC_SD$genetic_sex_male + 
                                                                   NAFLD_LRC_SD$age_recruitment + NAFLD_LRC_SD$Body_mass_index, Hess=TRUE)))
                                               [, "t value"]), lower.tail = FALSE) * 2))[1,]) %>%
    add_column(Name = paste0("Non_oily_fish_intake", "&", bio))
}
for (bio in names(Bio_NAFLD_LRC_SD)){
  Coefficients <- (data.frame(summary(polr(Diet_NAFLD_LCC[["Lamb_mutton_intake"]] ~ Bio_NAFLD_LRC_SD[[bio]] + NAFLD_LRC_SD$genetic_sex_male + 
                                             NAFLD_LRC_SD$age_recruitment + NAFLD_LRC_SD$Body_mass_index, Hess=TRUE))$coefficients))[1,]
  Results_Control_clmc[["Lamb_mutton_intake"]][[bio]] <- Coefficients %>%
    add_column(p_value = (data.frame(pnorm(abs(coef(summary(polr(Diet_NAFLD_LCC[["Lamb_mutton_intake"]] ~ Bio_NAFLD_LRC_SD[[bio]]
                                                                 + NAFLD_LRC_SD$genetic_sex_male + 
                                                                   NAFLD_LRC_SD$age_recruitment + NAFLD_LRC_SD$Body_mass_index, Hess=TRUE)))
                                               [, "t value"]), lower.tail = FALSE) * 2))[1,]) %>%
    add_column(Name = paste0("Lamb_mutton_intake", "&", bio))
}
for (bio in names(Bio_NAFLD_LRC_SD)){
  Coefficients <- (data.frame(summary(polr(Diet_NAFLD_LCC[["Salt_added_to_food"]] ~ Bio_NAFLD_LRC_SD[[bio]] + NAFLD_LRC_SD$genetic_sex_male + 
                                             NAFLD_LRC_SD$age_recruitment + NAFLD_LRC_SD$Body_mass_index, Hess=TRUE))$coefficients))[1,]
  Results_Control_clmd[["Salt_added_to_food"]][[bio]] <- Coefficients %>%
    add_column(p_value = (data.frame(pnorm(abs(coef(summary(polr(Diet_NAFLD_LCC[["Salt_added_to_food"]] ~ Bio_NAFLD_LRC_SD[[bio]]
                                                                 + NAFLD_LRC_SD$genetic_sex_male + 
                                                                   NAFLD_LRC_SD$age_recruitment + NAFLD_LRC_SD$Body_mass_index, Hess=TRUE)))
                                               [, "t value"]), lower.tail = FALSE) * 2))[1,]) %>%
    add_column(Name = paste0("Salt_added_to_food", "&", bio))
}
for (bio in names(Bio_NAFLD_LRC_SD)){
  Coefficients <- (data.frame(summary(polr(Diet_NAFLD_LCC[["Cheese_intake"]] ~ Bio_NAFLD_LRC_SD[[bio]] + NAFLD_LRC_SD$genetic_sex_male + 
                                             NAFLD_LRC_SD$age_recruitment + NAFLD_LRC_SD$Body_mass_index, Hess=TRUE))$coefficients))[1,]
  Results_Control_clme[["Cheese_intake"]][[bio]] <- Coefficients %>%
    add_column(p_value = (data.frame(pnorm(abs(coef(summary(polr(Diet_NAFLD_LCC[["Cheese_intake"]] ~ Bio_NAFLD_LRC_SD[[bio]] 
                                                                 + NAFLD_LRC_SD$genetic_sex_male + 
                                                                   NAFLD_LRC_SD$age_recruitment + NAFLD_LRC_SD$Body_mass_index, Hess=TRUE)))
                                               [, "t value"]), lower.tail = FALSE) * 2))[1,]) %>%
    add_column(Name = paste0("Cheese_intake", "&", bio))
}
detach("package:MASS", unload = TRUE)
D_B_Control.LC1a <- do.call( "rbind", Results_Control_clma)
D_B_Control.LCCa <- do.call( "rbind", D_B_Control.LC1a)
D_B_Control.LC1b <- do.call( "rbind", Results_Control_clmb)
D_B_Control.LCCb <- do.call( "rbind", D_B_Control.LC1b)
D_B_Control.LC1c <- do.call( "rbind", Results_Control_clmc)
D_B_Control.LCCc <- do.call( "rbind", D_B_Control.LC1c)
D_B_Control.LC1d <- do.call( "rbind", Results_Control_clmd)
D_B_Control.LCCd <- do.call( "rbind", D_B_Control.LC1d)
D_B_Control.LC1e <- do.call( "rbind", Results_Control_clme)
D_B_Control.LCCe <- do.call( "rbind", D_B_Control.LC1e)
D_B_Control.LCC <- rbind(D_B_Control.LCCa, D_B_Control.LCCb, D_B_Control.LCCc, D_B_Control.LCCd, D_B_Control.LCCe)
rownames(D_B_Control.LCC) <- NULL
D_B_Control.LCC$Estimate <- as.numeric(D_B_Control.LCC$Value)
D_B_Control.LCC$p_value <- as.numeric(D_B_Control.LCC$p_value)
D_B_Control.LC3 <- D_B_Control.LCC %>%
  separate(Name, into = c("Nutrition", "Biomarker"), sep = "&") %>%
  mutate(Relationship = ifelse(Estimate > 0, "Positive", "Negative")) %>%
  rename("Coefficient" = "Estimate") %>%
  select(c('p_value',  'Coefficient', 'Relationship', 'Biomarker','Nutrition'))
D_B_Control.LC3[["Nutrition"]][D_B_Control.LC3[["Nutrition"]]=="Lamb_mutton_intake"] <- "Lamb/mutton_intake"
D_B_Control.LC3[["Biomarker"]][D_B_Control.LC3[["Biomarker"]]=="Creatinine_in_urine"] <- "Creatinine_(enzymatic)_in_urine"
D_B_Control.clm4 <- D_B_Control.LC3[order(D_B_Control.LC3$p_value),]
D_B_Control.clm4$Bonferroni <- data.frame(mt.rawp2adjp(D_B_Control.LC3$p_value, proc=c("Bonferroni"), alpha = 0.05, na.rm = FALSE)$adjp)$Bonferroni
Sig_D_B_Control.clm5 <- D_B_Control.clm4 %>%
  filter(Bonferroni > 0) %>%
  mutate(Significance = ifelse(Bonferroni < 0.05, "Significant", "Not significant" )) %>%
  filter(Significance == "Significant")
Sig_D_B_Control.clm5[,c(1,6,2,3,4,5)]


#Within 2
Sig_D_B_2 <- rbind(Sig_D_B_2.lg5[,c(1,7,2,3,5,6)], Sig_D_B_2.rg5[,c(1,7,2,3,5,6)], Sig_D_B_2.clm5[,c(1,6,2,3,4,5)])
Sig_D_B_2 <- Sig_D_B_2[order(Sig_D_B_2$p_value),]
head(Sig_D_B_2[,c(-1)])
#After
Sig_D_B_AF <- rbind(Sig_D_B_AF.lg5[,c(1,7,2,3,5,6)], Sig_D_B_AF.rg5[,c(1,7,2,3,5,6)], Sig_D_B_AF.clm5[,c(1,6,2,3,4,5)])
Sig_D_B_AF <- Sig_D_B_AF[order(Sig_D_B_AF$p_value),]
head(Sig_D_B_AF[,c(-1)])
#Before
Sig_D_B_BF <- rbind(Sig_D_B_BF.lg5[,c(1,7,2,3,5,6)], Sig_D_B_BF.rg5[,c(1,7,2,3,5,6)], Sig_D_B_BF.clm5[,c(1,6,2,3,4,5)])
Sig_D_B_BF <- Sig_D_B_BF[order(Sig_D_B_BF$p_value),]
head(Sig_D_B_BF[order(Sig_D_B_BF$p_value),][,c(-1)])
#Control
Sig_D_B_C <- rbind(Sig_D_B_C.lg5[,c(1,7,2,3,5,6)], Sig_D_B_C.rg5[,c(1,7,2,3,5,6)], Sig_D_B_Control.clm5[,c(1,6,2,3,4,5)])
Sig_D_B_C <- Sig_D_B_C[order(Sig_D_B_C$p_value),]
head(Sig_D_B_C[order(Sig_D_B_C$p_value),])


