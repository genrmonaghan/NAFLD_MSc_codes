
#+-2 years
#Bio ~ Cell
Results_2_lm <- list()
set.seed(132)
for (Bio in names(Bio_NAFLD_LR2_SD)){
  for (Cell in names(Cell_NAFLD_LR2_SD)){
    Coefficients <- data.frame(summary(lm(Cell_NAFLD_LR2_SD[[Cell]] ~ Bio_NAFLD_LR2_SD[[Bio]] + NAFLD_LR2_SD$genetic_sex_male + 
                                            NAFLD_LR2_SD$age_recruitment + NAFLD_LR2_SD$Body_mass_index))$coefficients)
    Results_2_lm[[Bio]][[Cell]] <- Coefficients[2,] %>%
      add_column(Intercept = Coefficients$Estimate[1]) %>%
      add_column(Name = paste0(Bio, "&", Cell)) }
}
B_C_2.rg1 <- do.call( "rbind", Results_2_lm)
B_C_2.rg2 <- do.call( "rbind", B_C_2.rg1)
rownames(B_C_2.rg2) <- NULL
B_C_2.rg2$Pr...t.. <- as.numeric(B_C_2.rg2$Pr...t..)
B_C_2.rg2$Estimate <- as.numeric(B_C_2.rg2$Estimate)
B_C_2.rg3 <- B_C_2.rg2 %>%
  separate(Name, into = c("Biomarker", "Cell_Count"), sep = "&") %>%
  rename( "p_value" = "Pr...t..") %>%
  mutate(Relationship = ifelse(Estimate > 0, "Positive", "Negative")) %>% 
  rename("Coefficient" = "Estimate") %>%
  select(c('p_value',  'Coefficient', 'Relationship', 'Intercept',  'Cell_Count','Biomarker'))
B_C_2.rg4 <- B_C_2.rg3[order(B_C_2.rg3$p_value),]
B_C_2.rg4$Bonferroni <- data.frame(mt.rawp2adjp(B_C_2.rg4$p_value, proc=c("Bonferroni"), alpha = 0.05, na.rm = FALSE)$adjp)$Bonferroni
Sig_B_C_2.rg5 <- B_C_2.rg4 %>%
  mutate(Significance = ifelse(Bonferroni < 0.05, "Significant", "Not significant" )) %>%
  filter(Significance == "Significant")
Sig_B_C_2.rg5[,c(1,7,2,4,5,6)]


#After
Results_AF_lm <- list()
set.seed(132)
for (Bio in names(Bio_NAFLD_LRAF_SD)){
  for (Cell in names(Cell_NAFLD_LRAF_SD)){
    Coefficients <- data.frame(summary(lm(Cell_NAFLD_LRAF_SD[[Cell]] ~ Bio_NAFLD_LRAF_SD[[Bio]] + NAFLD_LRAF_SD$genetic_sex_male + 
                                            NAFLD_LRAF_SD$age_recruitment + NAFLD_LRAF_SD$Body_mass_index))$coefficients)
    Results_AF_lm[[Bio]][[Cell]] <- Coefficients[2,] %>%
      add_column(Intercept = Coefficients$Estimate[1]) %>%
      add_column(Name = paste0(Bio, "&", Cell)) }
}
B_C_AF.rg1 <- do.call( "rbind", Results_AF_lm) #matrix with 2 relationships
B_C_AF.rg2 <- do.call( "rbind", B_C_AF.rg1) #final dataframe
rownames(B_C_AF.rg2) <- NULL
B_C_AF.rg2$Pr...t.. <- as.numeric(B_C_AF.rg2$Pr...t..)
B_C_AF.rg2$Estimate <- as.numeric(B_C_AF.rg2$Estimate)
B_C_AF.rg3 <- B_C_AF.rg2 %>%
  separate(Name, into = c("Biomarker", "Cell_Count"), sep = "&") %>%
  rename( "p_value" = "Pr...t..") %>%
  mutate(Relationship = ifelse(Estimate > 0, "Positive", "Negative")) %>% # very difficult to have a 0 here 
  rename("Coefficient" = "Estimate") %>%
  select(c('p_value',  'Coefficient', 'Relationship', 'Intercept',  'Cell_Count','Biomarker'))
B_C_AF.rg4 <- B_C_AF.rg3[order(B_C_AF.rg3$p_value),]
B_C_AF.rg4$Bonferroni <- data.frame(mt.rawp2adjp(B_C_AF.rg3$p_value, proc=c("Bonferroni"), alpha = 0.05, na.rm = FALSE)$adjp)$Bonferroni
Sig_B_C_AF.rg5 <- B_C_AF.rg4 %>%
  mutate(Significance = ifelse(Bonferroni < 0.05, "Significant", "Not significant" )) %>%
  filter(Significance == "Significant")
Sig_B_C_AF.rg5[,c(1,7,2,3,5,6)]


#Before
#Bio ~ Cell
Results_BF_lm <- list()
set.seed(132) #make results reproducible ( same results every time you run)
for (Bio in names(Bio_NAFLD_LRBF_SD)){
  for (Cell in names(Cell_NAFLD_LRBF_SD)){
    Coefficients <- data.frame(summary(lm(Cell_NAFLD_LRBF_SD[[Cell]] ~ Bio_NAFLD_LRBF_SD[[Bio]] + NAFLD_LRBF_SD$genetic_sex_male + 
                                            NAFLD_LRBF_SD$age_recruitment + NAFLD_LRBF_SD$Body_mass_index))$coefficients)
    Results_BF_lm[[Bio]][[Cell]] <- Coefficients[2,] %>%
      add_column(Intercept = Coefficients$Estimate[1]) %>%
      add_column(Name = paste0(Bio, "&", Cell)) }
}
B_C_BF.rg1 <- do.call( "rbind", Results_BF_lm) #matrix with 2 relationships
B_C_BF.rg2 <- do.call( "rbind", B_C_BF.rg1) #final dataframe
rownames(B_C_BF.rg2) <- NULL
B_C_BF.rg2$Pr...t.. <- as.numeric(B_C_BF.rg2$Pr...t..)
B_C_BF.rg2$Estimate <- as.numeric(B_C_BF.rg2$Estimate)
B_C_BF.rg3 <- B_C_BF.rg2 %>%
  separate(Name, into = c("Biomarker", "Cell_Count"), sep = "&") %>%
  rename( "p_value" = "Pr...t..") %>%
  mutate(Relationship = ifelse(Estimate > 0, "Positive", "Negative")) %>% # very difficult to have a 0 here 
  rename("Coefficient" = "Estimate") %>%
  select(c('p_value',  'Coefficient', 'Relationship', 'Intercept',  'Cell_Count','Biomarker'))
B_C_BF.rg4 <- B_C_BF.rg3[order(B_C_BF.rg3$p_value),]
B_C_BF.rg4$Bonferroni <- data.frame(mt.rawp2adjp(B_C_BF.rg4$p_value, proc=c("Bonferroni"), alpha = 0.05, na.rm = FALSE)$adjp)$Bonferroni
Sig_B_C_BF.rg5 <- B_C_BF.rg4 %>%
  mutate(Significance = ifelse(Bonferroni < 0.05, "Significant", "Not significant" )) %>%
  filter(Significance == "Significant")
Sig_B_C_BF.rg5[,c(1,7,2,3,5,6)]



#Control
Results_C_lm <- list()
set.seed(132)
for (Bio in names(Bio_NAFLD_LRC_SD)){
  for (Cell in names(Cell_NAFLD_LRC_SD)){
    Coefficients <- data.frame(summary(lm( Cell_NAFLD_LRC_SD[[Cell]] ~ Bio_NAFLD_LRC_SD[[Bio]] + NAFLD_LRC_SD$genetic_sex_male + 
                                             NAFLD_LRC_SD$age_recruitment + NAFLD_LRC_SD$Body_mass_index))$coefficients)
    Results_C_lm[[Bio]][[Cell]] <- Coefficients[2,] %>%
      add_column(Intercept = Coefficients$Estimate[1]) %>%
      add_column(Name = paste0(Bio, "&", Cell)) }
}
B_C_C.rg1 <- do.call( "rbind", Results_C_lm)
B_C_C.rg2 <- do.call( "rbind", B_C_C.rg1)
rownames(B_C_C.rg2) <- NULL
B_C_C.rg2$Pr...t.. <- as.numeric(B_C_C.rg2$Pr...t..)
B_C_C.rg2$Estimate <- as.numeric(B_C_C.rg2$Estimate)
B_C_C.rg3 <- B_C_C.rg2 %>%
  separate(Name, into = c("Biomarker", "Cell_Count"), sep = "&") %>%
  rename( "p_value" = "Pr...t..") %>%
  mutate(Relationship = ifelse(Estimate > 0, "Positive", "Negative")) %>%
  rename("Coefficient" = "Estimate") %>%
  select(c('p_value',  'Coefficient', 'Relationship', 'Intercept',  'Cell_Count','Biomarker'))
B_C_C.rg4 <- B_C_C.rg3[order(B_C_C.rg3$p_value),]
B_C_C.rg4$Bonferroni <- data.frame(mt.rawp2adjp(B_C_C.rg4$p_value, proc=c("Bonferroni"), alpha = 0.05, na.rm = FALSE)$adjp)$Bonferroni
Sig_B_C_C.rg5 <- B_C_C.rg4 %>%
  mutate(Significance = ifelse(Bonferroni < 0.05, "Significant", "Not significant" )) %>%
  filter(Significance == "Significant")
Sig_B_C_C.rg5[,c(1,7,2,3,5,6)]







Sig_D_B_CC_AF <- rbind(Sig_D_B_C_AF %>% rename(Marker1 = 4, Marker2 =5), 
                       Sig_B_C_AF.rg5[c(7,2,3,5,6)] %>% rename(Marker1 = 4, Marker2 =5)) %>% arrange(Bonferroni)
Sig_D_B_CC_BF <- rbind(Sig_D_B_C_BF %>% rename(Marker1 = 4, Marker2 =5), 
                       Sig_B_C_BF.rg5[c(7,2,3,5,6)] %>% rename(Marker1 = 4, Marker2 =5)) %>% arrange(Bonferroni)
Sig_D_B_CC_2 <- rbind(Sig_D_B_C_2 %>% rename(Marker1 = 4, Marker2 =5), 
                      Sig_B_C_2.rg5[c(7,2,3,5,6)] %>% rename(Marker1 = 4, Marker2 =5)) %>% arrange(Bonferroni)
Sig_D_B_CC_C <- rbind(Sig_D_B_C_C %>% rename(Marker1 = 4, Marker2 =5), 
                      Sig_B_C_C.rg5[c(7,2,3,5,6)] %>% rename(Marker1 = 4, Marker2 =5)) %>% arrange(Bonferroni)

nrow(inner_join(Sig_D_B_CC_AF[c(3,4,5)],Sig_D_B_CC_BF[c(3,4,5)]))/nrow(Sig_D_B_CC_AF) #25 D2AF-D2BF #6.49%
nrow(inner_join(Sig_D_B_CC_AF[c(3,4,5)],Sig_D_B_CC_BF[c(3,4,5)]))/nrow(Sig_D_B_CC_BF)               #92.59%
nrow(inner_join(Sig_D_B_CC_AF[c(3,4,5)],Sig_D_B_CC_C[c(3,4,5)]))/nrow(Sig_D_B_CC_AF)  #268 D2AF-Control highest no. #69.61%
nrow(inner_join(Sig_D_B_CC_AF[c(3,4,5)],Sig_D_B_CC_C[c(3,4,5)]))/nrow(Sig_D_B_CC_C)                                 #53.92%
nrow(inner_join(Sig_D_B_CC_AF[c(3,4,5)],Sig_D_B_CC_2[c(3,4,5)]))/nrow(Sig_D_B_CC_AF)  #68 D2AF-DW2  #17.66%
nrow(inner_join(Sig_D_B_CC_AF[c(3,4,5)],Sig_D_B_CC_2[c(3,4,5)]))/nrow(Sig_D_B_CC_2)                 #83.95%
nrow(inner_join(Sig_D_B_CC_BF[c(3,4,5)],Sig_D_B_CC_2[c(3,4,5)]))/nrow(Sig_D_B_CC_2)   #20 D2BF-DW2  #24.69%
nrow(inner_join(Sig_D_B_CC_BF[c(3,4,5)],Sig_D_B_CC_2[c(3,4,5)]))/nrow(Sig_D_B_CC_BF)                #74.07%
nrow(inner_join(Sig_D_B_CC_BF[c(3,4,5)],Sig_D_B_CC_C[c(3,4,5)]))/nrow(Sig_D_B_CC_BF)  #17 D2BF-Control lowest no. #62.96%
nrow(inner_join(Sig_D_B_CC_BF[c(3,4,5)],Sig_D_B_CC_C[c(3,4,5)]))/nrow(Sig_D_B_CC_C)                               #3.42%
nrow(inner_join(Sig_D_B_CC_2[c(3,4,5)],Sig_D_B_CC_C[c(3,4,5)]))/nrow(Sig_D_B_CC_2)    #52 DW2-Control #64.20%
nrow(inner_join(Sig_D_B_CC_2[c(3,4,5)],Sig_D_B_CC_C[c(3,4,5)]))/nrow(Sig_D_B_CC_C)                    #10.46%
inner_join(inner_join(Sig_D_B_CC_AF[c(3,4,5)],Sig_D_B_CC_BF[c(3,4,5)]),inner_join(Sig_D_B_CC_2[c(3,4,5)],Sig_D_B_CC_C[c(3,4,5)]))


write.csv(Sig_D_B_CC_AF, "University/MSc/Files/Bio_Cell_Diet_RegressionAnalysis_AF.csv")
write.csv(Sig_D_B_CC_BF, "University/MSc/Files/Bio_Cell_Diet_RegressionAnalysis_Before.csv")
write.csv(Sig_D_B_CC_2, "University/MSc/Files/Bio_Cell_Diet_RegressionAnalysis_2.csv")
write.csv(Sig_D_B_CC_C, "University/MSc/Files/Bio_Cell_Diet_RegressionAnalysis_C.csv")

write.csv(rbind(Sig_D_B_CC_C %>% add_column(Cohort = "Control"), Sig_D_B_CC_AF %>% add_column(Cohort = "After 2 years"), 
                Sig_D_B_CC_2 %>% add_column(Cohort = "Within +-2 years"), Sig_D_B_CC_BF %>% add_column(Cohort = "Before 2 years") ), 
          "University/MSc/Files/Bio_Cell_Diet_RegressionAnalysis_Combined.csv")

Sig_D_B_C_AF.1 <- rbind((Sig_D_B_AF[-c(1)] %>% rename(Marker1 = 4, Marker2 =5) %>% add_column(Cohort = "D2AF") 
                         %>% add_column(Class1 = "Biomarker") %>% add_column(Class2 = "Diet") )[c(6,7,4,8,5,2,1)], 
                        (Sig_D_C_AF[-c(1)] %>% rename(Marker1 = 4, Marker2 =5) %>% add_column(Cohort = "D2AF") %>% add_column(Class1 = "Cell count")
                         %>% add_column(Class2 = "Diet") )[c(6,7,4,8,5,2,1)],
                        (Sig_B_C_AF.rg5[-c(1,3,4,8)] %>% rename(Marker1 = 3, Marker2 = 2) %>% add_column(Cohort = "D2AF") 
                         %>% add_column(Class1 = "Biomarker") %>% add_column(Class2 = "Cell count") )[c(5,6,3,7,2,1,4)] )
rownames(Sig_D_B_C_AF.1) <- NULL
Sig_D_B_C_C.1 <- rbind((Sig_D_B_C[-c(1)] %>% rename(Marker1 = 4, Marker2 =5) %>% add_column(Cohort = "Control") 
                        %>% add_column(Class1 = "Biomarker") %>% add_column(Class2 = "Diet") )[c(6,7,4,8,5,2,1)], 
                       (Sig_D_C_C[-c(1)] %>% rename(Marker1 = 4, Marker2 =5) %>% add_column(Cohort = "Control") %>% add_column(Class1 = "Cell count")
                        %>% add_column(Class2 = "Diet") )[c(6,7,4,8,5,2,1)],
                       (Sig_B_C_C.rg5[-c(1,3,4,8)] %>% rename(Marker1 = 3, Marker2 = 2) %>% add_column(Cohort = "Control") 
                        %>% add_column(Class1 = "Biomarker") %>% add_column(Class2 = "Cell count") )[c(5,6,3,7,2,1,4)] )
rownames(Sig_D_B_C_C.1) <- NULL
Sig_D_B_C_BF.1 <- rbind((Sig_D_B_BF[-c(1)] %>% rename(Marker1 = 4, Marker2 =5) %>% add_column(Cohort = "D2BF") 
                         %>% add_column(Class1 = "Biomarker") %>% add_column(Class2 = "Diet") )[c(6,7,4,8,5,2,1)], 
                        (Sig_D_C_BF[-c(1)] %>% rename(Marker1 = 4, Marker2 =5) %>% add_column(Cohort = "D2BF") 
                         %>% add_column(Class1 = "Cell count") %>% add_column(Class2 = "Diet") )[c(6,7,4,8,5,2,1)],
                        (Sig_B_C_BF.rg5[-c(1,3,4,8)] %>% rename(Marker1 = 3, Marker2 = 2) %>% add_column(Cohort = "D2BF") 
                         %>% add_column(Class1 = "Biomarker") %>% add_column(Class2 = "Cell count") )[c(5,6,3,7,2,1,4)] )
rownames(Sig_D_B_C_BF.1) <- NULL
Sig_D_B_C_2.1 <- rbind((Sig_D_B_2[-c(1)] %>% rename(Marker1 = 4, Marker2 =5) %>% add_column(Cohort = "DW2") 
                        %>% add_column(Class1 = "Biomarker") %>% add_column(Class2 = "Diet") )[c(6,7,4,8,5,2,1)], 
                       (Sig_D_C_2[-c(1)] %>% rename(Marker1 = 4, Marker2 =5) %>% add_column(Cohort = "DW2") 
                        %>% add_column(Class1 = "Cell count") %>% add_column(Class2 = "Diet") )[c(6,7,4,8,5,2,1)],
                       (Sig_B_C_2.rg5[-c(1,3,4,8)] %>% rename(Marker1 = 3, Marker2 = 2) %>% add_column(Cohort = "DW2") 
                        %>% add_column(Class1 = "Biomarker") %>% add_column(Class2 = "Cell count") )[c(5,6,3,7,2,1,4)] )
rownames(Sig_D_B_C_2.1) <- NULL

write.csv(rbind(Sig_D_B_C_C.1, Sig_D_B_C_AF.1, Sig_D_B_C_2.1, Sig_D_B_C_BF.1)%>%rename("P value"=7), 
          "University/MSc/Files/Bio_Cell_Diet_RegressionAnalysis_Combined2.csv")
