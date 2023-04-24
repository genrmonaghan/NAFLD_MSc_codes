
library(RColorBrewer)
mycol4 <- c("#2196F3", "#9CCC65", "#FDD835", "#EC407A")
mycol5 <- c("#F06292", "#BA68C8", "#4FC3F7", "#8BC34A", "#FDD835", "#FFB74D")
mycol6 <- c("#F8BBD0", "#E1BEE7", "#BBDEFB", "#DCEDC8", "#FFF9C4", "#FFE0B2")


#Within 2
Sig_D_B_2_coord <- Sig_D_B_CC_2 
for (n in 1:nrow(Sig_D_B_2_coord)) {
  Sig_D_B_2_coord$Intercept[c(n)] <- format(coef(lm(NAFLD_LR2_SD[[Sig_D_B_CC_2$Marker1[c(n)]]] ~ NAFLD_LR2_SD[[Sig_D_B_CC_2$Marker2[c(n)]]] + 
                                                      NAFLD_LR2_SD$Body_mass_index + NAFLD_LR2_SD$genetic_sex_male + NAFLD_LR2_SD$age_recruitment))[1], 
                                            digits = 4)
  Sig_D_B_2_coord$R2[c(n)] <- format(summary(lm(NAFLD_LR2_SD[[Sig_D_B_CC_2$Marker1[c(n)]]] ~ NAFLD_LR2_SD[[Sig_D_B_CC_2$Marker2[c(n)]]]+ 
                                                  NAFLD_LR2_SD$Body_mass_index + NAFLD_LR2_SD$genetic_sex_male + NAFLD_LR2_SD$age_recruitment))$r.squared, 
                                     digits = 3)
}
head(Sig_D_B_2_coord %>% arrange(Bonferroni))
#Scatter plot
ggplot(NAFLD_LR2_SD, aes(Albumin, `Red_blood_cell_(erythrocyte)_count`)) + 
  geom_point(colour = "#FBC02D", size = 3) + 
  geom_smooth(method = "lm", se = T, na.rm = FALSE, colour = "black") +
  labs(x = "RBC count (x10^12 cells/L)", y = "Albumin (g/L)", title = "Diagnosis Within +-2 years Albumin ~ RBC count", 
       subtitle = substitute(paste(italic('p value'), " < 0.0001" %.%  italic('y'), " = 1.156 + 0.0584", italic('x') %.% italic(R)^2, "= 0.259")))
#Overlap
Sig_D_B_CC_C %>% filter(Marker2 == "Albumin") %>% filter(str_detect(Marker1, "Red_blood_cell")) %>% filter(str_detect(Marker1, "count"))
RBCC_ALB <- rbind((data.frame(NAFLD_LRC_SD$`Red_blood_cell_(erythrocyte)_count`, NAFLD_LRC_SD$Albumin, "Control") %>% rename(RBC_C = 1, Albumin = 2, Group = 3)),
                 (data.frame(NAFLD_LRAF_SD$`Red_blood_cell_(erythrocyte)_count`, NAFLD_LRAF_SD$Albumin, "D2AF") %>% rename(RBC_C = 1, Albumin = 2, Group = 3)),
                 (data.frame(NAFLD_LRBF_SD$`Red_blood_cell_(erythrocyte)_count`, NAFLD_LRBF_SD$Albumin, "D2BF") %>% rename(RBC_C = 1, Albumin = 2, Group = 3)),
                 (data.frame(NAFLD_LR2_SD$`Red_blood_cell_(erythrocyte)_count`, NAFLD_LR2_SD$Albumin, "DW2") %>% rename(RBC_C = 1, Albumin = 2, Group = 3)))
RBCC_ALB$Group <- ordered(RBCC_ALB$Group, levels = c("Control", "D2AF", "DW2", "D2BF"))
ggplot(RBCC_ALB, aes( Albumin, RBC_C, color = Group, palette = "Set2", fill = Group)) + geom_smooth(method = "lm", se = T, na.rm = FALSE) +
  labs(x = "RBC count (x10^12 cells/L)", y = "Albumin (g/L)", title = "Albumin ~ RBC Count") + 
  scale_fill_manual(values=mycol4) + scale_color_manual(values=mycol4)



#After
#Scatter graph
Sig_D_B_AF_coord <- Sig_D_B_CC_AF 
for (n in 1:nrow(Sig_D_B_AF_coord)) {
  Sig_D_B_AF_coord$Intercept[c(n)] <- format(coef(lm(NAFLD_LRAF_SD[[Sig_D_B_CC_AF$Marker1[c(n)]]] ~ NAFLD_LRAF_SD[[Sig_D_B_CC_AF$Marker2[c(n)]]] + 
                                                       NAFLD_LRAF_SD$Body_mass_index + NAFLD_LRAF_SD$genetic_sex_male + NAFLD_LRAF_SD$age_recruitment ))[1], 
                                             digits = 4)
  Sig_D_B_AF_coord$R2[c(n)] <- format(summary(lm(NAFLD_LRAF_SD[[Sig_D_B_CC_AF$Marker1[c(n)]]] ~ NAFLD_LRAF_SD[[Sig_D_B_CC_AF$Marker2[c(n)]]]+ 
                                                   NAFLD_LRAF_SD$Body_mass_index + NAFLD_LRAF_SD$genetic_sex_male + NAFLD_LRAF_SD$age_recruitment))$r.squared, 
                                      digits = 3)
}
head(Sig_D_B_AF_coord[c(1,4,5,3,2,6,7)] %>% arrange(Bonferroni)) 
ggplot(NAFLD_LRAF_SD, aes(HDL_cholesterol, Mean_sphered_cell_volume)) + geom_point(colour = "#7CB342", size = 3) +
  geom_smooth(method = "lm", se = T, na.rm = FALSE, colour = "black") +
  labs(x = "HDL-C (mmol/L)", y = "Mean sphered cell volume (fl)", title = "D2AF Mean sphered cell volume ~ HDL-C", 
       subtitle = substitute(paste(italic('p value'), " < 0.0001" %.% 
                                     italic('y'), " = 77.13 + 4.378", italic('x') %.% italic(R)^2, "= 0.0978"))) 
#Overlapping
HDL_MSCV <- rbind((data.frame(NAFLD_LRC_SD$HDL_cholesterol, NAFLD_LRC_SD$Mean_sphered_cell_volume, "Control") %>% rename(HDL = 1, MSCV = 2, Group = 3)),
                 (data.frame(NAFLD_LRAF_SD$HDL_cholesterol, NAFLD_LRAF_SD$Mean_sphered_cell_volume, "D2AF") %>% rename(HDL = 1, MSCV = 2, Group = 3)),
                 (data.frame(NAFLD_LR2_SD$HDL_cholesterol, NAFLD_LR2_SD$Mean_sphered_cell_volume, "DW2") %>% rename(HDL = 1, MSCV = 2, Group = 3)))
HDL_MSCV$Group <- ordered(HDL_MSCV$Group, levels = c("Control", "D2AF", "DW2"))
ggplot(HDL_MSCV, aes(HDL, MSCV, color = Group, palette = "Set2", fill = Group)) + geom_smooth(method = "lm", se = T, na.rm = FALSE) +
  labs(x = "HDL-C (mmol/L)", y = "Mean sphered cell volume (fl)", title = "Mean sphered cell volume ~ HDL-C") + 
  scale_fill_manual(values=mycol4) + scale_color_manual(values=mycol4)


#Before
#Scatter graph
Sig_D_B_BF_coord <- Sig_D_B_CC_BF %>% rename("Gradient" = "Coefficient")
for (n in 1:nrow(Sig_D_B_BF_coord)) {
  Sig_D_B_BF_coord$Intercept[c(n)] <- format(coef(lm(NAFLD_LRBF_SD[[Sig_D_B_CC_BF$Marker1[c(n)]]] ~ NAFLD_LRBF_SD[[Sig_D_B_CC_BF$Marker2[c(n)]]]+ 
                                                       NAFLD_LRBF_SD$Body_mass_index + NAFLD_LRBF_SD$genetic_sex_male + NAFLD_LRBF_SD$age_recruitment))[1], 
                                             digits = 4)
  Sig_D_B_BF_coord$R2[c(n)] <- format(summary(lm(NAFLD_LRBF_SD[[Sig_D_B_CC_BF$Marker1[c(n)]]] ~ NAFLD_LRBF_SD[[Sig_D_B_CC_BF$Marker2[c(n)]]]+ 
                                                   NAFLD_LRBF_SD$Body_mass_index + NAFLD_LRBF_SD$genetic_sex_male + NAFLD_LRBF_SD$age_recruitment))$r.squared, 
                                      digits = 3)
}
head(Sig_D_B_BF_coord %>% arrange(Bonferroni))
ggplot(NAFLD_LRBF_SD, aes(Albumin, `Red_blood_cell_(erythrocyte)_count`)) + geom_point(colour = "#EC407A", size = 3) +
  geom_smooth(method = "lm", se = T, na.rm = FALSE, colour = "black") +
  labs(x = "RBC count (x10^12 cells/L)", y = "Albumin (g/L)", title = "D2BF Albumin ~ RBC Count", 
       subtitle = substitute(paste(italic('p value'), " < 0.0001" %.% italic('y'), " = 2.683 + 0.0378", italic('x') %.% italic(R)^2, "= 0.218")))
#Overlapping - see DW2


#Control
Sig_D_B_C_coord <- Sig_D_B_CC_C %>% rename("Gradient" = "Coefficient")
for (n in 1:nrow(Sig_D_B_C_coord)) {
  Sig_D_B_C_coord$Intercept[c(n)] <- format(coef(lm(NAFLD_LRC_SD[[Sig_D_B_CC_C$Marker1[c(n)]]] ~ NAFLD_LRC_SD[[Sig_D_B_CC_C$Marker2[c(n)]]]+ 
                                                      NAFLD_LRC_SD$Body_mass_index + NAFLD_LRC_SD$genetic_sex_male + NAFLD_LRC_SD$age_recruitment))[1], 
                                             digits = 4)
  Sig_D_B_C_coord$R2[c(n)] <- format(summary(lm(NAFLD_LRC_SD[[Sig_D_B_CC_C$Marker1[c(n)]]] ~ NAFLD_LRC_SD[[Sig_D_B_CC_C$Marker2[c(n)]]]+ 
                                                  NAFLD_LRC_SD$Body_mass_index + NAFLD_LRC_SD$genetic_sex_male + NAFLD_LRC_SD$age_recruitment))$r.squared, 
                                      digits = 3)
}
head(Sig_D_B_C_coord %>% arrange(Bonferroni))
ggplot(NAFLD_LRC_SD, aes(Reticulocyte_count, Triglycerides)) + geom_point(colour = "#64B5F6", size = 3) + 
  geom_smooth(method = "lm", se = T, na.rm = FALSE, colour = "black") +
  labs(x = "Reticulocyte count (x10^12 cells/L)", y = "Triglycerides (mmol/L)", 
       title = "Control Reticulocyte count ~ Triglycerides", 
       subtitle = substitute(paste(italic('p value'), " < 0.0001" %.%  italic('y'), " = 0.0032 + 0.0054", italic('x') %.% italic(R)^2, "= 0.229")))
#Overlapping
Sig_D_B_CC_AF %>% filter(Marker2 == "Triglycerides|Alanine_aminotransferase") %>% filter(str_detect(Marker1, "Reticulocyte")) %>% filter(str_detect(Marker1, "count"))
CER_NAU <- rbind((data.frame(NAFLD_LRC_SD$Reticulocyte_count, NAFLD_LRC_SD$Triglycerides, "Control") %>% rename(RTC = 1, Sodium = 2, Group = 3)),
                 (data.frame(NAFLD_LRAF_SD$Reticulocyte_count, NAFLD_LRAF_SD$Triglycerides, "D2AF") %>% rename(RTC = 1, Sodium = 2, Group = 3)) )
CER_NAU$Group <- ordered(CER_NAU$Group, levels = c("Control", "D2AF"))
ggplot(CER_NAU, aes(RTC, Sodium, color = Group, palette = "Set2", fill = Group)) + geom_smooth(method = "lm", se = T, na.rm = FALSE) +
  labs(x = "Reticulocyte count (x10^12 cells/L)", y = "Triglycerides (mmol/L)", title = "Reticulocyte count ~ Triglycerides") + 
  scale_fill_manual(values=mycol4) + scale_color_manual(values=mycol4)

#Cereal type v intake
mycomparisons_1 <- list( c("0", "1"),  c("0", "2"), c("0", "3"), c("0", "4"),  c("0", "5"))
ggboxplot(NAFLD_LRC_SD, x = "Cereal_type", y = "Cereal_intake", color = mycol5, fill = mycol6,
          ylab = "Cereal bowls per week", xlab = "Cereal type", width = 0.7)  +
  stat_compare_means(comparisons = mycomparisons_1, method = "t.test", paired = FALSE) +
  scale_x_discrete(labels = c(paste0("Never eat cereal \n (n=1461)"), paste0("Bran cereal \n (n=938)"), paste0("Biscuit cereal \n (n=1004)"),
                              paste0("Oat cereal \n (n=1204)"), paste0("Museli \n (n=1493)"), paste0("Other cereal \n (n=1207)")))
tabyl(NAFLD_LRC_SD$Cereal_type)
#Cereal type v sodium in urine
dfCCTS <- data.frame(NAFLD_LRC_SD[["Sodium_in_urine"]], NAFLD_LRC_SD[["Cereal_type"]])
nrow(dfCCTS[complete.cases(dfCCTS),])
dfCCTS <- dfCCTS[complete.cases(dfCCTS),]
table(dfCCTS[c(2)])
ggboxplot(NAFLD_LRC_SD, x = "Cereal_type", y = "Sodium_in_urine", color = mycol5, fill = mycol6, ylab = "Sodium in urine (mmol/L)", 
          xlab = "Type of Cereal Consumed", width = 0.7) +
  stat_compare_means(comparisons = mycomparisons_1, method = "t.test", paired = FALSE) + 
  scale_x_discrete(labels = c(paste0("Never eat cereal \n (n=1460)"), paste0("Bran cereal \n (n=938)"), paste0("Biscuit cereal \n (n=1004)"),
                              paste0("Oat cereal \n (n=1204)"), paste0("Museli \n (n=1493)"), paste0("Other cereal \n (n=1207)")))
Sig_D_B_C_coord %>% filter(Marker == "Sodium_in_urine") %>% filter(str_detect(Nutrition, 'ereal|Museli'))
# BMI ~ Sodium in urine 
BMI_NAU <- rbind((data.frame(NAFLD_LRC_SD$Body_mass_index, NAFLD_LRC_SD$Sodium_in_urine, "Control") %>% rename(BMI = 1, Sodium = 2, Group = 3)),
                 (data.frame(NAFLD_LRAF_SD$Body_mass_index, NAFLD_LRAF_SD$Sodium_in_urine, "After") %>% rename(BMI = 1, Sodium = 2, Group = 3)),
                 (data.frame(NAFLD_LRBF_SD$Body_mass_index, NAFLD_LRBF_SD$Sodium_in_urine, "Before") %>% rename(BMI = 1, Sodium = 2, Group = 3)),
                 (data.frame(NAFLD_LR2_SD$Body_mass_index, NAFLD_LR2_SD$Sodium_in_urine, "Within_2") %>% rename(BMI = 1, Sodium = 2, Group = 3)))
BMI_NAU$Group <- ordered(BMI_NAU$Group, levels = c("Control", "Before", "Within_2", "After"))
ggplot(BMI_NAU, aes(BMI, Sodium, color = Group, palette = "Set2", fill = Group)) + geom_smooth(method = "lm", se = T, na.rm = FALSE) +
  labs(x = "BMI", y = "Sodium in urine (mmol/L)", title = "Sodium in urine ~ BMI") + 
  scale_fill_manual(values=mycol4) + scale_color_manual(values=mycol4)



#Cystatin C
summary(NAFLD_LRC_SD$Cystatin_C)
Sig_D_B_BF_coord %>% filter(str_detect(Marker1, 'Cystatin_C'))
Sig_D_B_AF_coord %>% filter(str_detect(Marker1, 'Cystatin_C')) %>% filter(str_detect(Relationship, 'Neg'))
Sig_D_B_C_coord %>% filter(str_detect(Marker1, 'Cystatin_C')) %>% filter(str_detect(Relationship, 'Neg')) %>% 
  filter(str_detect(Nutrition, 'Wholegrain_bread|Skim|fish|veg|fruit'))
Sig_D_B_AF_coord %>% filter(str_detect(Marker, 'Cystatin_C')) %>% filter(str_detect(Relationship, 'Pos'))
Sig_D_B_C_coord %>% filter(str_detect(Marker, 'Cystatin_C')) %>% filter(str_detect(Relationship, 'Pos')) %>% 
  filter(str_detect(Nutrition, 'White_bread|Full_cream|Salt|Other_cereal'))
ggplot(NAFLD_LRAll_SD, aes(TimeToDisease, Cystatin_C)) + geom_point(colour = "pink", size = 3) + 
  geom_smooth(method = "lm", se = T, na.rm = FALSE, colour = "black") + labs(x = "Time to Disease (years)", y = "Cystatin C (_)")
#Overlapping
#Non oily fish
NOF_CYC <- rbind((data.frame(NAFLD_LRC_SD$`White_blood_cell_(leukocyte)_count`, NAFLD_LRC_SD$Cystatin_C, "Control") %>% rename(Fish = 1, CYC = 2, Group = 3)),
                 (data.frame(NAFLD_LRAF_SD$`White_blood_cell_(leukocyte)_count`, NAFLD_LRAF_SD$Cystatin_C, "After") %>% rename(Fish = 1, CYC = 2, Group = 3)),
                 (data.frame(NAFLD_LRBF_SD$`White_blood_cell_(leukocyte)_count`, NAFLD_LRBF_SD$Cystatin_C, "Before") %>% rename(Fish = 1, CYC = 2, Group = 3)),
                 (data.frame(NAFLD_LR2_SD$`White_blood_cell_(leukocyte)_count`, NAFLD_LR2_SD$Cystatin_C, "Within_2") %>% rename(Fish = 1, CYC = 2, Group = 3)))
NOF_CYC$Group <- ordered(NOF_CYC$Group, levels = c("Control", "Before", "Within_2", "After"))
ggplot(NOF_CYC, aes(Fish, CYC, color = Group, palette = "Set2", fill = Group)) + geom_smooth(method = "lm", se = T, na.rm = FALSE) +
  labs(x = "`White_blood_cell_(leukocyte)_count`", y = "Cystatin C (mg/l)", title = "Cystatin C ~ Non-oily fish intake") + 
  scale_fill_manual(values=mycol4) + scale_color_manual(values=mycol4)
group_by(NAFLD_LRBF_SD, Non_oily_fish_intake) %>%
  summarise( count = n(), mean = mean(Cystatin_C, na.rm = TRUE), 
             L_CI = mean(Cystatin_C, na.rm = TRUE)-qnorm(0.975)*sd(Cystatin_C, na.rm = TRUE)/sqrt(n()),
             U_CI = mean(Cystatin_C, na.rm = TRUE)+qnorm(0.975)*sd(Cystatin_C, na.rm = TRUE)/sqrt(n()), sd = sd(Cystatin_C, na.rm = TRUE),
             median = median(Cystatin_C, na.rm = TRUE), LQ = quantile(Cystatin_C, probs = c(0.25), na.rm = TRUE),
             UQ = quantile(Cystatin_C, probs = c(0.75), na.rm = TRUE))

#Wholegrain bread vs White bread
WHB_CYC <- rbind((NAFLD_LRC_SD %>% filter(White_bread_intake == "1") %>% add_column(Group = paste0("Control_1"))),
                 (NAFLD_LRC_SD %>% filter(Wholegrain_bread_intake == "1") %>% add_column(Group = paste0("Control_0"))),
                 (NAFLD_LRAF_SD %>% filter(White_bread_intake == "1") %>% add_column(Group = paste0("After_1"))),
                 (NAFLD_LRAF_SD %>% filter(Wholegrain_bread_intake == "1") %>% add_column(Group = paste0("After_0"))),
                 (NAFLD_LRBF_SD %>% filter(White_bread_intake == "1") %>% add_column(Group = paste0("Before_1"))),
                 (NAFLD_LRBF_SD %>% filter(Wholegrain_bread_intake == "1") %>% add_column(Group = paste0("Before_0"))),
                 (NAFLD_LR2_SD %>% filter(White_bread_intake == "1") %>% add_column(Group = paste0("Within_1"))),
                 (NAFLD_LR2_SD %>% filter(Wholegrain_bread_intake == "1") %>% add_column(Group = paste0("Within_0"))) )
WHB_CYC$Group <- ordered(WHB_CYC$Group, levels = c("Control_0", "Control_1", "Before_0", "Before_1", "Within_0", "Within_1","After_0", "After_1"))
mycomparisons_4 <- list(c("Before_1","Before_0"), c("Control_1","Control_0"), c("After_1","After_0"), c("Within_1","Within_0"))
mycol7 <- c("#1976D2", "#1976D2", "#689F38", "#689F38", "#F9A825", "#F9A825", "#D81B60", "#D81B60")
mycol7.5 <- c("#BBDEFB", "#BBDEFB", "#DCEDC8", "#DCEDC8", "#FFF9C4", "#FFF9C4", "#F8BBD0", "#F8BBD0")
nrow((NAFLD_LRC_SD %>% filter(White_bread_intake == "1") %>% add_column(Group = paste0("Control_1"))))
ggboxplot(WHB_CYC, x = "Group", y = "Cystatin_C", color = "Group", ylim=c(0,2.5),
          palette = mycol7, fill = mycol7.5, ylab = "Cystatin C (mg/L)", xlab = "Dietary group", width = 0.7) +
  stat_compare_means(comparisons = mycomparisons_4, method = "t.test", paired = FALSE) +
  theme(plot.title = element_text(hjust = 0.5) ) + 
  scale_x_discrete(labels = c(paste0("Eats \n wholegrain \n bread \n (n=4007)"), paste0("Eats \n white bread \n (n=2040)"), 
                              paste0("Eats \n wholegrain \n bread \n (n=133)"), paste0("Eats \n white bread \n (n=126)"),
                              paste0("Eats \n wholegrain \n bread \n (n=191)"), paste0("Eats \n white bread \n (n=166)"),
                              paste0("Eats \n wholegrain \n bread \n (n=1862)"), paste0("Eats \n white bread \n (n=1506)") ))







#Time to disease
TTD_All_NAFLD <- rbind(cbind(Bio_NAFLD_LRBF_SD, Cell_NAFLD_LRBF_SD, Clin_NAFLD_LRBF_SD), 
                       cbind(Bio_NAFLD_LR2_SD, Cell_NAFLD_LR2_SD, Clin_NAFLD_LR2_SD), 
                       cbind(Bio_NAFLD_LRAF_SD, Cell_NAFLD_LRAF_SD, Clin_NAFLD_LRAF_SD))
Results_AF_lm <- list()
set.seed(132)
for (Clin in names(Clin_NAFLD_LRBF_SD[c(4)])){
  for (Bio in names(cbind(Bio_NAFLD_LRBF_SD,Cell_NAFLD_LRBF_SD))){
    Coefficients <- data.frame(summary(lm(TTD_All_NAFLD[[Bio]] ~ TTD_All_NAFLD[[Clin]] + TTD_All_NAFLD$Sex + 
                                            TTD_All_NAFLD$age_recruitment + TTD_All_NAFLD$'Body_mass_index_(BMI)'))$coefficients)
    Results_AF_lm[[Clin]][[Bio]] <- Coefficients[2,] %>%
      add_column(Intercept = Coefficients$Estimate[1]) %>%
      add_column(Name = paste0(Clin, "&", Bio)) }
}
TTD_B_AF.rlg1 <- do.call( "rbind", Results_AF_lm)
TTD_B_AF.rlg2 <- do.call( "rbind", TTD_B_AF.rlg1)
rownames(TTD_B_AF.rlg2) <- NULL
TTD_B_AF.rlg2$Pr...t.. <- as.numeric(TTD_B_AF.rlg2$Pr...t..)
TTD_B_AF.rlg2$Estimate <- as.numeric(TTD_B_AF.rlg2$Estimate)
TTD_B_AF.rlg3 <- TTD_B_AF.rlg2 %>%
  separate(Name, into = c("Marker2", "Marker1"), sep = "&") %>%
  rename( "p_value" = "Pr...t..") %>%
  mutate(Relationship = ifelse(Estimate > 0, "Positive", "Negative")) %>%
  rename("Coefficient" = "Estimate") %>%
  select(c('p_value',  'Coefficient', 'Relationship', 'Intercept',  'Marker1','Marker2'))
TTD_B_AF.rlg4 <- TTD_B_AF.rlg3[order(TTD_B_AF.rlg3$p_value),]
TTD_B_AF.rlg4$Bonferroni <- data.frame(mt.rawp2adjp(TTD_B_AF.rlg4$p_value, proc=c("Bonferroni"), alpha = 0.05, na.rm = FALSE)$adjp)$Bonferroni
Sig_TTD_B_AF.rlg5 <- TTD_B_AF.rlg4 %>%
  mutate(Significance = ifelse(Bonferroni < 0.05, "Significant", "Not significant" )) %>%
  filter(Significance == "Significant")
Sig_TTD_B_AF.rlg5_coord <- Sig_TTD_B_AF.rlg5[-c(1,8)] 
for (n in 1:nrow(Sig_TTD_B_AF.rlg5_coord)) {
  Sig_TTD_B_AF.rlg5_coord$Intercept_2[c(n)] <- format(coef(lm(TTD_All_NAFLD[[Sig_TTD_B_AF.rlg5$Marker1[c(n)]]] ~ TTD_All_NAFLD[[Sig_TTD_B_AF.rlg5$Marker2[c(n)]]]))[1], 
                                            digits = 4)
  Sig_TTD_B_AF.rlg5_coord$Gradient[c(n)] <- format(coef(lm(TTD_All_NAFLD[[Sig_TTD_B_AF.rlg5$Marker1[c(n)]]] ~ TTD_All_NAFLD[[Sig_TTD_B_AF.rlg5$Marker2[c(n)]]]))[2], 
                                           digits = 4)
  Sig_TTD_B_AF.rlg5_coord$R2[c(n)] <- format(summary(lm(TTD_All_NAFLD[[Sig_TTD_B_AF.rlg5$Marker1[c(n)]]] ~ TTD_All_NAFLD[[Sig_TTD_B_AF.rlg5$Marker2[c(n)]]]))$r.squared, 
                                     digits = 3)
}
Sig_TTD_B_AF.rlg5_coord[c(6,4,5,2,1,3,7,8,9)]
ggplot(TTD_All_NAFLD, aes(TimeToDisease, Alkaline_phosphatase)) + geom_point(colour = "#AD1457", size = 3) + 
  geom_smooth(method = "lm", se = T, na.rm = FALSE, colour = "black") +
  labs(x = "Time till NAFLD diagnosis (years)", y = "Alkaline Phosphatase (U/L)", title = "Alkaline Phosphatase ~ Time to Disease",
       subtitle = substitute(paste(italic('p value'), " < 0.0001" %.% 
                                     italic('y'), " = 97.4 - 0.846", italic('x') %.% italic(R)^2, "= 0.0186")))
ggplot(TTD_All_NAFLD, aes(TimeToDisease, Gamma_glutamyltransferase)) + geom_point(colour = "#6A1B9A", size = 3) + 
  geom_smooth(method = "lm", se = T, na.rm = FALSE, colour = "black") +
  labs(x = "Time till NAFLD diagnosis (years)", y = "Gamma Glutamyl Transferase (U/L)", title = "Gamma Glutamyl Transferase ~ Time to Disease",
       subtitle = substitute(paste(italic('p value'), " < 0.0001" %.% 
                                     italic('y'), " = 74.2 - 1.77", italic('x') %.% italic(R)^2, "= 0.0168")))
ggplot(TTD_All_NAFLD, aes(TimeToDisease, Aspartate_aminotransferase)) + geom_point(colour = "#1565C0", size = 3) + 
  geom_smooth(method = "lm", se = T, na.rm = FALSE, colour = "black") +
  labs(x = "Time till NAFLD diagnosis (years)", y = "Aspartate Aminotransferase (U/L)", title = "Aspartate Aminotransferase ~ Time to Disease",
       subtitle = substitute(paste(italic('p value'), " < 0.0001" %.% 
                                     italic('y'), " = 34 - 0.0363", italic('x') %.% italic(R)^2, "= 0.0147")))
ggplot(TTD_All_NAFLD, aes(TimeToDisease, Platelet_crit)) + geom_point(colour = "#2E7D32", size = 3) + 
  geom_smooth(method = "lm", se = T, na.rm = FALSE, colour = "black") +
  labs(x = "Time till NAFLD diagnosis (years)", y = "Platelet crit (%)", title = "Platelet crit ~ Time to Disease",
       subtitle = substitute(paste(italic('p value'), " < 0.0001" %.% 
                                     italic('y'), " = 0.217 - 0.00132", italic('x') %.% italic(R)^2, "= 0.0141")))






#Graphs of participants
#Exclusion bar graphs
NAFLDAndNASHMatchedRaw <- read_excel("University/MSc/NAFLDAndNASHMatched.xlsx")
NAFLDAndNASHMatchedRaw_All <- rbind((NAFLDAndNASHMatchedRaw %>% filter(Diseases2 == "NAFLD")), 
                                    (NAFLDAndNASHMatchedRaw %>% filter(!Diseases2 == "NAFLD")))
NAFLDAndNASHMatchedRaw_BF <- NAFLDAndNASHMatchedRaw_All %>% filter(TimeToDisease < -2)
NAFLDAndNASHMatchedRaw_AF <- NAFLDAndNASHMatchedRaw_All %>% filter(TimeToDisease > 2)
NAFLDAndNASHMatchedRaw_2 <- NAFLDAndNASHMatchedRaw_All %>% filter(TimeToDisease <= 2) %>% filter(TimeToDisease >= -2)
NAFLD_Ngrpah <- data.frame(rep(c("Control","Before", "Within_2", "After"), 2),
                           c(nrow(NAFLDAndNASHMatchedControl), nrow(NAFLDAndNASHMatchedBefore),
                             nrow(NAFLDAndNASHMatched_2), nrow(NAFLDAndNASHMatchedAfter),(nrow(NAFLDAndNASHMatchedRaw)-nrow(NAFLDAndNASHMatchedRaw_All)), 
                             nrow(NAFLDAndNASHMatchedRaw_BF), nrow(NAFLDAndNASHMatchedRaw_2), 
                             nrow(NAFLDAndNASHMatchedRaw_AF)),
                           c(rep("Included", 4), rep("All", 4)))
colnames(NAFLD_Ngrpah) <- c("Participant Group", "Number of Participants", "Data")
NAFLD_Ngrpah$`Participant Group` <- ordered(NAFLD_Ngrpah$`Participant Group`,
                                            levels = c("Control",  "After", "Within_2", "Before"))
ggplot(data=NAFLD_Ngrpah, aes(x=`Participant Group`, y=`Number of Participants`, fill=`Data`)) +
  geom_bar(stat="identity", position=position_dodge()) +
  scale_x_discrete(labels = c(paste0("Control"), paste0("NAFLD diagnosed \n >2 years after"),
                              paste0("NAFLD diagnosed \n within +- 2 years"), paste0("NAFLD diagnosed \n >2 years before"))) +
  geom_text(aes(label=NAFLD_Ngrpah$`Number of Participants`), position=position_dodge(width=0.9), vjust=-0.25)
NAFLD_Ngrpah 

#Percentage exclusions
NAFLD_Ngrpah2 <- data.frame(c("Control","Before", "Within_2", "After"),
                            c(nrow(NAFLDAndNASHMatchedControl), nrow(NAFLDAndNASHMatchedBefore),nrow(NAFLDAndNASHMatched_2), 
                              nrow(NAFLDAndNASHMatchedAfter)), 
                            c((nrow(NAFLDAndNASHMatchedRaw)-nrow(NAFLDAndNASHMatchedRaw_All)), nrow(NAFLDAndNASHMatchedRaw_BF), 
                              nrow(NAFLDAndNASHMatchedRaw_2), nrow(NAFLDAndNASHMatchedRaw_AF)))
colnames(NAFLD_Ngrpah2) <- c("Cohort", "Included", "All")
NAFLD_Ngrpah2$`Percentage of Total Participants` <- as.numeric(format(round(((NAFLD_Ngrpah2$Included / NAFLD_Ngrpah2$All) *100), 2), nsmall = 2))
NAFLD_Ngrpah2$`PercentageofTotalParticipants` <- paste(NAFLD_Ngrpah2$`Percentage of Total Participants`, "%")
NAFLD_Ngrpah2$Cohort <- ordered(NAFLD_Ngrpah2$Cohort, levels = c("Control", "After", "Within_2", "Before"))
NAFLD_Ngrpah2 
mycol8 <- c("#BBDEFB", "#F8BBD0", "#FFF9C4", "#DCEDC8")
mycol9 <- c("#1976D2", "#D81B60", "#F9A825" ,"#689F38" )
ggplot(data=NAFLD_Ngrpah2, aes(x=Cohort, y=`Percentage of Total Participants`)) + 
  ylim(0,100) +
  geom_bar(stat="identity", fill=mycol8, colour=mycol9 ) + 
  theme_bw() + #theme_minimal()
  scale_x_discrete(labels = c(paste0("Control"), paste0("D2AF"), paste0("DW2"), paste0("D2BF"))) +
  geom_text(aes(label = NAFLD_Ngrpah2$PercentageofTotalParticipants), position=position_dodge(width=0.9), vjust=-0.25)

#Graphs of features
#Exclusion bar graphs
NAFLD_Fgraph <- data.frame(rep(c("Total","Biomarkers", "Cell Features", "Nutrition", "Clinical"), 2),
                           c("113", ncol(Bio_NAFLD_LRC_SD), ncol(Cell_NAFLD_LRC_SD), 
                             ncol(cbind(Diet_NAFLD_LRC_SD,Diet_NAFLD_LCC,Diet_NAFLD_LMNC)), ncol(Clin_NAFLD_LRC_SD),
                             ncol(NAFLDAndNASHMatchedRaw_All), "206", "31", "308", "99" ),
                           c(rep("Included", 5), rep("All", 5)))
colnames(NAFLD_Fgraph) <- c("Feature Group", "Number of Features", "Data")
NAFLD_Fgraph$`Number of Features` <- as.numeric(NAFLD_Fgraph$`Number of Features`)
ggplot(data=NAFLD_Fgraph, aes(x=`Feature Group`, y=`Number of Features`, fill=`Data`)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_text(aes(label=NAFLD_Fgraph$`Number of Features`), position=position_dodge(width=0.9), vjust=-0.25)
NAFLD_Fgraph 
#Percentages
NAFLD_Fgraph2 <- data.frame(c("Total","Biomarkers", "Cell Features", "Nutrition", "Clinical"),
                            c(NAFLD_Fgraph[c(1:5),]$`Number of Features`), c(NAFLD_Fgraph[c(6:10),]$`Number of Features`) )
colnames(NAFLD_Fgraph2) <- c("Feature Group", "Included", "All")
NAFLD_Fgraph2$`Percentage of Total Features` <- as.numeric(format(round(((NAFLD_Fgraph2$Included / NAFLD_Fgraph2$All) *100), 2), nsmall = 2))
NAFLD_Fgraph2$`PercentageofTotalFeatures` <- paste(NAFLD_Fgraph2$`Percentage of Total Features`, "%")
NAFLD_Fgraph2 
mycol9 <- c("#BBDEFB", "#DCEDC8", "#FFF9C4", "#F8BBD0")
ggplot(data=NAFLD_Fgraph2, aes(x=`Feature Group`, y=`Percentage of Total Features`)) + 
  ylim(0,100) + geom_bar(stat="identity", fill = "turquoise3") +  theme_bw() + 
  geom_text(aes(label = NAFLD_Fgraph2$PercentageofTotalFeatures), position=position_dodge(width=0.9), vjust=-0.25)





d1 <- data.frame(from="origin", to=paste("group", seq(1,9), sep=""))
d2 <- data.frame( from = c("group1",rep("group3",6),"group4","group4","group3","group4","group4", rep("group1",10), rep("group2",7), rep("group5",39), 
                           "group6", "group9","group7","group6","group7","group6","group9","group7","group6","group7", rep("group9",5),"group8","group9",
                           "group8","group9", rep("group7",3),"group8","group8", rep("group9",3),rep("group8",4),"group9"),
                  to = c(colnames(NAFLD_LR2_SD[c(2:69, 76:107)])) )
edges <- rbind(d1, d2) %>% arrange(from)

#After
list_D_B_C_AF <- data.frame(tabyl(rbind(data.frame(Sig_D_B_CC_AF$Marker1) %>% rename('Marker' =1),data.frame(Sig_D_B_CC_AF$Marker2) %>% 
                         rename('Marker' =1))$Marker)%>% arrange(n))%>% rename('Marker'=1)
list_D_B_C_AF_g <- inner_join(edges, list_D_B_C_AF %>% rename('to'=1), by ='to')%>% arrange(n) %>% select(from, to, n)
list_D_B_C_AF_c <- inner_join(list_D_B_C_AF_g, data.frame(from=paste("group", seq(1,9), sep=""), colour=paste(brewer.pal(9,"Paired"))), by='from')
list_D_B_C_AF_c$to <- factor(list_D_B_C_AF_g$to, levels = list_D_B_C_AF_g$to[order(list_D_B_C_AF_g$n, decreasing = TRUE)] )
list_D_B_C_AF_c$to   <- gsub("_", " ", list_D_B_C_AF_c$to )
list_D_B_C_AF_c$to <- gsub(" direct", "-C", list_D_B_C_AF_c$to)
list_D_B_C_AF_c$to <- gsub(" cholesterol", "-C", list_D_B_C_AF_c$to)
list_D_B_C_AF_c$to   <- gsub("Apolipoprotein ", "Apo-", list_D_B_C_AF_c$to )
list_D_B_C_AF_c[list_D_B_C_AF_c == "C reactive protein"] <- "CRP"
list_D_B_C_AF_c[list_D_B_C_AF_c == "Aspartate aminotransferase"] <- "AST"
list_D_B_C_AF_c[list_D_B_C_AF_c == "Alanine aminotransferase"] <- "ALT"
list_D_B_C_AF_c[list_D_B_C_AF_c == "Gamma glutamyltransferase"] <- "GGT"
list_D_B_C_AF_c[list_D_B_C_AF_c == "Alkaline phosphatase"] <- "ALP"
list_D_B_C_AF_c[list_D_B_C_AF_c == "Glycated haemoglobin (HbA1c)"] <- "HbA1c"
list_D_B_C_AF_c[list_D_B_C_AF_c == "Cholesterol"] <- "TC"
list_D_B_C_AF_c[list_D_B_C_AF_c == "Triglycerides"] <- "TG"
list_D_B_C_AF_c[list_D_B_C_AF_c == "White blood cell (leukocyte) count"] <- "WBC count"
list_D_B_C_AF_c[list_D_B_C_AF_c == "Mean platelet (thrombocyte) volume"] <- "Mean platelet volume"
list_D_B_C_AF_c[list_D_B_C_AF_c == "Mean corpuscular volume"] <- "MCV"
list_D_B_C_AF_c[list_D_B_C_AF_c == "Red blood cell (erythrocyte) count"] <- "RBC count"
list_D_B_C_AF_c[list_D_B_C_AF_c == "Red blood cell (erythrocyte) distribution width"] <- "RDW"
list_D_B_C_AF_c[list_D_B_C_AF_c == "Mean corpuscular haemoglobin concentration"] <- "MCHC"
list_D_B_C_AF_c[list_D_B_C_AF_c == "High light scatter reticulocyte count"] <- "HLSRC"
list_D_B_C_AF_c[list_D_B_C_AF_c == "High light scatter reticulocyte percentage"] <- "HLSRP"
list_D_B_C_AF_c[list_D_B_C_AF_c == "  "] <- " "
list_D_B_C_AF_c$to <- factor(list_D_B_C_AF_c$to, levels = list_D_B_C_AF_c$to[order(list_D_B_C_AF_c$n, decreasing = TRUE)] )
list_D_B_C_AF_c
ggplot(data=list_D_B_C_AF_c, aes(x=`to`, y=`n`)) + ylim(0,29) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_bar(stat="identity", fill = list_D_B_C_AF_c$colour, width = 0.9) + ggtitle("D2AF") + xlab("Feature") + ylab("Number of Associations")

list_DBC_AF_c <- data.frame( from=paste("group", seq(1,9), sep=""),  n=NA)
list_DBC_AF_c[c(1),]$n <- sum(list_D_B_C_AF_c$n[list_D_B_C_AF_c$from == "group1"])
list_DBC_AF_c[c(2),]$n <- sum(list_D_B_C_AF_c$n[list_D_B_C_AF_c$from == "group2"])
list_DBC_AF_c[c(3),]$n <- sum(list_D_B_C_AF_c$n[list_D_B_C_AF_c$from == "group3"])
list_DBC_AF_c[c(4),]$n <- sum(list_D_B_C_AF_c$n[list_D_B_C_AF_c$from == "group4"])
list_DBC_AF_c[c(5),]$n <- sum(list_D_B_C_AF_c$n[list_D_B_C_AF_c$from == "group5"])
list_DBC_AF_c[c(6),]$n <- sum(list_D_B_C_AF_c$n[list_D_B_C_AF_c$from == "group6"])
list_DBC_AF_c[c(7),]$n <- sum(list_D_B_C_AF_c$n[list_D_B_C_AF_c$from == "group7"])
list_DBC_AF_c[c(8),]$n <- sum(list_D_B_C_AF_c$n[list_D_B_C_AF_c$from == "group8"])
list_DBC_AF_c[c(9),]$n <- sum(list_D_B_C_AF_c$n[list_D_B_C_AF_c$from == "group9"])
list_DBC_AF_cg <- inner_join(list_DBC_AF_c, data.frame(from=paste("group", seq(1,9), sep=""), colour=paste(brewer.pal(9,"Paired"))), by='from')
ggplot(data=list_DBC_AF_cg, aes(x=`from`, y=`n`) )  + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_bar(stat="identity", fill = list_DBC_AF_cg$colour, width = 0.9) + ylim(0,150) + ggtitle("D2AF") + xlab("Group") + ylab("Number of Associations")


#DW2
list_D_B_C_2 <- data.frame(tabyl(rbind(data.frame(Sig_D_B_CC_2$Marker1) %>% rename('Marker' =1),data.frame(Sig_D_B_CC_2$Marker2) %>% 
                                          rename('Marker' =1))$Marker)%>% arrange(n))%>% rename('Marker'=1)
list_D_B_C_2_g <- inner_join(edges, list_D_B_C_2 %>% rename('to'=1), by ='to')%>% arrange(n) %>% select(from, to, n)
list_D_B_C_2_c <- inner_join(list_D_B_C_2_g, data.frame(from=paste("group", seq(1,9), sep=""), colour=paste(brewer.pal(9,"Paired"))), by='from')
list_D_B_C_2_c$to   <- gsub("_", " ", list_D_B_C_2_c$to )
list_D_B_C_2_c$to <- gsub(" direct", "-C", list_D_B_C_2_c$to)
list_D_B_C_2_c$to <- gsub(" cholesterol", "-C", list_D_B_C_2_c$to)
list_D_B_C_2_c$to   <- gsub("Apolipoprotein ", "Apo-", list_D_B_C_2_c$to )
list_D_B_C_2_c[list_D_B_C_2_c == "C reactive protein"] <- "CRP"
list_D_B_C_2_c[list_D_B_C_2_c == "Aspartate aminotransferase"] <- "AST"
list_D_B_C_2_c[list_D_B_C_2_c == "Alanine aminotransferase"] <- "ALT"
list_D_B_C_2_c[list_D_B_C_2_c == "Gamma glutamyltransferase"] <- "GGT"
list_D_B_C_2_c[list_D_B_C_2_c == "Alkaline phosphatase"] <- "ALP"
list_D_B_C_2_c[list_D_B_C_2_c == "Glycated haemoglobin (HbA1c)"] <- "HbA1c"
list_D_B_C_2_c[list_D_B_C_2_c == "Cholesterol"] <- "TC"
list_D_B_C_2_c[list_D_B_C_2_c == "Triglycerides"] <- "TG"
list_D_B_C_2_c[list_D_B_C_2_c == "White blood cell (leukocyte) count"] <- "WBC count"
list_D_B_C_2_c[list_D_B_C_2_c == "Mean platelet (thrombocyte) volume"] <- "Mean platelet volume"
list_D_B_C_2_c[list_D_B_C_2_c == "Mean corpuscular volume"] <- "MCV"
list_D_B_C_2_c[list_D_B_C_2_c == "Red blood cell (erythrocyte) count"] <- "RBC count"
list_D_B_C_2_c[list_D_B_C_2_c == "Red blood cell (erythrocyte) distribution width"] <- "RDW"
list_D_B_C_2_c[list_D_B_C_2_c == "Mean corpuscular haemoglobin concentration"] <- "MCHC"
list_D_B_C_2_c[list_D_B_C_2_c == "High light scatter reticulocyte count"] <- "HLSRC"
list_D_B_C_2_c[list_D_B_C_2_c == "High light scatter reticulocyte percentage"] <- "HLSRP"
list_D_B_C_2_c[list_D_B_C_2_c == "  "] <- " "
list_D_B_C_2_c$to <- factor(list_D_B_C_2_c$to, levels = list_D_B_C_2_c$to[order(list_D_B_C_2_c$n, decreasing = TRUE)] )
ggplot(data=list_D_B_C_2_c, aes(x=`to`, y=`n`)) + ylim(0,15) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_bar(stat="identity", fill = list_D_B_C_2_c$colour, width = 0.9) + ggtitle("DW2") + xlab("Feature") + ylab("Number of Associations")

list_DBC_2_c <- data.frame( from=paste("group", seq(1,9), sep=""),  n=NA)
list_DBC_2_c[c(1),]$n <- sum(list_D_B_C_2_c$n[list_D_B_C_2_c$from == "group1"])
list_DBC_2_c[c(2),]$n <- sum(list_D_B_C_2_c$n[list_D_B_C_2_c$from == "group2"])
list_DBC_2_c[c(3),]$n <- sum(list_D_B_C_2_c$n[list_D_B_C_2_c$from == "group3"])
list_DBC_2_c[c(4),]$n <- sum(list_D_B_C_2_c$n[list_D_B_C_2_c$from == "group4"])
list_DBC_2_c[c(5),]$n <- sum(list_D_B_C_2_c$n[list_D_B_C_2_c$from == "group5"])
list_DBC_2_c[c(6),]$n <- sum(list_D_B_C_2_c$n[list_D_B_C_2_c$from == "group6"])
list_DBC_2_c[c(7),]$n <- sum(list_D_B_C_2_c$n[list_D_B_C_2_c$from == "group7"])
list_DBC_2_c[c(8),]$n <- sum(list_D_B_C_2_c$n[list_D_B_C_2_c$from == "group8"])
list_DBC_2_c[c(9),]$n <- sum(list_D_B_C_2_c$n[list_D_B_C_2_c$from == "group9"])
list_DBC_2_cg <- inner_join(list_DBC_2_c, data.frame(from=paste("group", seq(1,9), sep=""), colour=paste(brewer.pal(9,"Paired"))), by='from')
ggplot(data=list_DBC_2_cg, aes(x=`from`, y=`n`))  + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_bar(stat="identity", fill = list_DBC_2_cg$colour, width = 0.9) + ylim(0,40) + ggtitle("DW2") + xlab("Group") + ylab("Number of Associations")



#D2BF
list_D_B_C_BF <- data.frame(tabyl(rbind(data.frame(Sig_D_B_CC_BF$Marker1) %>% rename('Marker' =1),data.frame(Sig_D_B_CC_BF$Marker2) %>% 
                                         rename('Marker' =1))$Marker)%>% arrange(n))%>% rename('Marker'=1)
list_D_B_C_BF_g <- inner_join(edges, list_D_B_C_BF %>% rename('to'=1), by ='to')%>% arrange(n) %>% select(from, to, n)
list_D_B_C_BF_c <- inner_join(list_D_B_C_BF_g, data.frame(from=paste("group", seq(1,9), sep=""), colour=paste(brewer.pal(9,"Paired"))), by='from')
list_D_B_C_BF_c$to <- factor(list_D_B_C_BF_g$to, levels = list_D_B_C_BF_g$to[order(list_D_B_C_BF_g$n, decreasing = TRUE)] )
list_D_B_C_BF_c$to   <- gsub("_", " ", list_D_B_C_BF_c$to )
list_D_B_C_BF_c$to <- gsub(" direct", "-C", list_D_B_C_BF_c$to)
list_D_B_C_BF_c$to <- gsub(" cholesterol", "-C", list_D_B_C_BF_c$to)
list_D_B_C_BF_c$to   <- gsub("Apolipoprotein ", "Apo-", list_D_B_C_BF_c$to )
list_D_B_C_BF_c[list_D_B_C_BF_c == "C reactive protein"] <- "CRP"
list_D_B_C_BF_c[list_D_B_C_BF_c == "Aspartate aminotransferase"] <- "AST"
list_D_B_C_BF_c[list_D_B_C_BF_c == "Alanine aminotransferase"] <- "ALT"
list_D_B_C_BF_c[list_D_B_C_BF_c == "Gamma glutamyltransferase"] <- "GGT"
list_D_B_C_BF_c[list_D_B_C_BF_c == "Alkaline phosphatase"] <- "ALP"
list_D_B_C_BF_c[list_D_B_C_BF_c == "Glycated haemoglobin (HbA1c)"] <- "HbA1c"
list_D_B_C_BF_c[list_D_B_C_BF_c == "Cholesterol"] <- "TC"
list_D_B_C_BF_c[list_D_B_C_BF_c == "Triglycerides"] <- "TG"
list_D_B_C_BF_c[list_D_B_C_BF_c == "White blood cell (leukocyte) count"] <- "WBC count"
list_D_B_C_BF_c[list_D_B_C_BF_c == "Mean platelet (thrombocyte) volume"] <- "Mean platelet volume"
list_D_B_C_BF_c[list_D_B_C_BF_c == "Mean corpuscular volume"] <- "MCV"
list_D_B_C_BF_c[list_D_B_C_BF_c == "Red blood cell (erythrocyte) count"] <- "RBC count"
list_D_B_C_BF_c[list_D_B_C_BF_c == "Red blood cell (erythrocyte) distribution width"] <- "RDW"
list_D_B_C_BF_c[list_D_B_C_BF_c == "Mean corpuscular haemoglobin concentration"] <- "MCHC"
list_D_B_C_BF_c[list_D_B_C_BF_c == "High light scatter reticulocyte count"] <- "HLSRC"
list_D_B_C_BF_c[list_D_B_C_BF_c == "High light scatter reticulocyte percentage"] <- "HLSRP"
list_D_B_C_BF_c[list_D_B_C_BF_c == "  "] <- " "
list_D_B_C_BF_c$to <- factor(list_D_B_C_BF_c$to, levels = list_D_B_C_BF_c$to[order(list_D_B_C_BF_c$n, decreasing = TRUE)] )
ggplot(data=list_D_B_C_BF_c, aes(x=`to`, y=`n`)) + ylim(0,5) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_bar(stat="identity", fill = list_D_B_C_BF_c$colour, width = 0.9) + ggtitle("D2BF") + xlab("Feature") + ylab("Number of Associations")

list_DBC_BF_c <- data.frame( from=paste("group", seq(1,9), sep=""),  n=NA)
list_DBC_BF_c[c(1),]$n <- sum(list_D_B_C_BF_c$n[list_D_B_C_BF_c$from == "group1"])
list_DBC_BF_c[c(2),]$n <- sum(list_D_B_C_BF_c$n[list_D_B_C_BF_c$from == "group2"])
list_DBC_BF_c[c(3),]$n <- sum(list_D_B_C_BF_c$n[list_D_B_C_BF_c$from == "group3"])
list_DBC_BF_c[c(4),]$n <- sum(list_D_B_C_BF_c$n[list_D_B_C_BF_c$from == "group4"])
list_DBC_BF_c[c(5),]$n <- sum(list_D_B_C_BF_c$n[list_D_B_C_BF_c$from == "group5"])
list_DBC_BF_c[c(6),]$n <- sum(list_D_B_C_BF_c$n[list_D_B_C_BF_c$from == "group6"])
list_DBC_BF_c[c(7),]$n <- sum(list_D_B_C_BF_c$n[list_D_B_C_BF_c$from == "group7"])
list_DBC_BF_c[c(8),]$n <- sum(list_D_B_C_BF_c$n[list_D_B_C_BF_c$from == "group8"])
list_DBC_BF_c[c(9),]$n <- sum(list_D_B_C_BF_c$n[list_D_B_C_BF_c$from == "group9"])
list_DBC_BF_cg <- inner_join(list_DBC_BF_c, data.frame(from=paste("group", seq(1,9), sep=""), colour=paste(brewer.pal(9,"Paired"))), by='from')
ggplot(data=list_DBC_BF_cg, aes(x=`from`, y=`n`))  + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_bar(stat="identity", fill = list_DBC_BF_cg$colour, width = 0.9) + ylim(0,20)+ ggtitle("D2BF") + xlab("Group") + ylab("Number of Associations")

#Control
list_D_B_C_C <- data.frame(tabyl(rbind(data.frame(Sig_D_B_CC_C$Marker1) %>% rename('Marker' =1),data.frame(Sig_D_B_CC_C$Marker2) %>% 
                                          rename('Marker' =1))$Marker)%>% arrange(n))%>% rename('Marker'=1)
list_D_B_C_C_g <- inner_join(edges, list_D_B_C_C %>% rename('to'=1), by ='to')%>% arrange(n) %>% select(from, to, n)
list_D_B_C_C_c <- inner_join(list_D_B_C_C_g, data.frame(from=paste("group", seq(1,9), sep=""), colour=paste(brewer.pal(9,"Paired"))), by='from')
list_D_B_C_C_c$to <- factor(list_D_B_C_C_g$to, levels = list_D_B_C_C_g$to[order(list_D_B_C_C_g$n, decreasing = TRUE)] )
list_D_B_C_C_c$to   <- gsub("_", " ", list_D_B_C_C_c$to )
list_D_B_C_C_c$to <- gsub(" direct", "-C", list_D_B_C_C_c$to)
list_D_B_C_C_c$to <- gsub(" cholesterol", "-C", list_D_B_C_C_c$to)
list_D_B_C_C_c$to   <- gsub("Apolipoprotein ", "Apo-", list_D_B_C_C_c$to )
list_D_B_C_C_c[list_D_B_C_C_c == "C reactive protein"] <- "CRP"
list_D_B_C_C_c[list_D_B_C_C_c == "Aspartate aminotransferase"] <- "AST"
list_D_B_C_C_c[list_D_B_C_C_c == "Alanine aminotransferase"] <- "ALT"
list_D_B_C_C_c[list_D_B_C_C_c == "Gamma glutamyltransferase"] <- "GGT"
list_D_B_C_C_c[list_D_B_C_C_c == "Alkaline phosphatase"] <- "ALP"
list_D_B_C_C_c[list_D_B_C_C_c == "Glycated haemoglobin (HbA1c)"] <- "HbA1c"
list_D_B_C_C_c[list_D_B_C_C_c == "Cholesterol"] <- "TC"
list_D_B_C_C_c[list_D_B_C_C_c == "Triglycerides"] <- "TG"
list_D_B_C_C_c[list_D_B_C_C_c == "White blood cell (leukocyte) count"] <- "WBC count"
list_D_B_C_C_c[list_D_B_C_C_c == "Mean platelet (thrombocyte) volume"] <- "Mean platelet volume"
list_D_B_C_C_c[list_D_B_C_C_c == "Mean corpuscular volume"] <- "MCV"
list_D_B_C_C_c[list_D_B_C_C_c == "Red blood cell (erythrocyte) count"] <- "RBC count"
list_D_B_C_C_c[list_D_B_C_C_c == "Red blood cell (erythrocyte) distribution width"] <- "RDW"
list_D_B_C_C_c[list_D_B_C_C_c == "Mean corpuscular haemoglobin concentration"] <- "MCHC"
list_D_B_C_C_c[list_D_B_C_C_c == "High light scatter reticulocyte count"] <- "HLSRC"
list_D_B_C_C_c[list_D_B_C_C_c == "High light scatter reticulocyte percentage"] <- "HLSRP"
list_D_B_C_C_c[list_D_B_C_C_c == "  "] <- " "
list_D_B_C_C_c$to <- factor(list_D_B_C_C_c$to, levels = list_D_B_C_C_c$to[order(list_D_B_C_C_c$n, decreasing = TRUE)] )
list_D_B_C_C_c
ggplot(data=list_D_B_C_C_c, aes(x=`to`, y=`n`)) + ylim(0,30) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_bar(stat="identity", fill = list_D_B_C_C_c$colour, width = 0.9) + ggtitle("Control") + xlab("Feature") + ylab("Number of Associations")

list_DBC_C_c <- data.frame( from=paste("group", seq(1,9), sep=""),  n=NA)
list_DBC_C_c[c(1),]$n <- sum(list_D_B_C_C_c$n[list_D_B_C_C_c$from == "group1"])
list_DBC_C_c[c(2),]$n <- sum(list_D_B_C_C_c$n[list_D_B_C_C_c$from == "group2"])
list_DBC_C_c[c(3),]$n <- sum(list_D_B_C_C_c$n[list_D_B_C_C_c$from == "group3"])
list_DBC_C_c[c(4),]$n <- sum(list_D_B_C_C_c$n[list_D_B_C_C_c$from == "group4"])
list_DBC_C_c[c(5),]$n <- sum(list_D_B_C_C_c$n[list_D_B_C_C_c$from == "group5"])
list_DBC_C_c[c(6),]$n <- sum(list_D_B_C_C_c$n[list_D_B_C_C_c$from == "group6"])
list_DBC_C_c[c(7),]$n <- sum(list_D_B_C_C_c$n[list_D_B_C_C_c$from == "group7"])
list_DBC_C_c[c(8),]$n <- sum(list_D_B_C_C_c$n[list_D_B_C_C_c$from == "group8"])
list_DBC_C_c[c(9),]$n <- sum(list_D_B_C_C_c$n[list_D_B_C_C_c$from == "group9"])
list_DBC_C_cg <- inner_join(list_DBC_C_c, data.frame(from=paste("group", seq(1,9), sep=""), colour=paste(brewer.pal(9,"Paired"))), by='from')
ggplot(data=list_DBC_C_cg, aes(x=`from`, y=`n`))  + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_bar(stat="identity", fill = list_DBC_C_cg$colour, width = 0.9) + ylim(0,200) + ggtitle("Control") + xlab("Group") + 
  ylab("Number of Associations")


#three columns - cohort, marker 1, marker 2
#Supplementary figures - one for each CCA vs Regress for each cohort

library(VennDiagram)
library(RColorBrewer)
myCol <- c( "#FFF9C4", "#F8BBD0","#DCEDC8", "#BBDEFB")
Set_2_Bio_Cell__5 <- Sig_D_B_CC_2 %>% 
  mutate(List = paste(Sig_D_B_CC_2$Relationship, Sig_D_B_CC_2$Marker1, Sig_D_B_CC_2$Marker2, sep="_"))
Set_Bef_Bio_Cell__5 <- Sig_D_B_CC_BF %>% 
  mutate(List = paste(Sig_D_B_CC_BF$Relationship, Sig_D_B_CC_BF$Marker1, Sig_D_B_CC_BF$Marker2, sep="_"))
Set_Aft_Bio_Cell__5 <- Sig_D_B_CC_AF %>% 
  mutate(List = paste(Sig_D_B_CC_AF$Relationship, Sig_D_B_CC_AF$Marker1, Sig_D_B_CC_AF$Marker2, sep="_"))
Set_Con_Bio_Cell__5 <- Sig_D_B_CC_C %>% 
  mutate(List = paste(Sig_D_B_CC_C$Relationship, Sig_D_B_CC_C$Marker1, Sig_D_B_CC_C$Marker2, sep="_"))
venn.diagram(
  x = list( Set_2_Bio_Cell__5$List, Set_Bef_Bio_Cell__5$List, Set_Aft_Bio_Cell__5$List, Set_Con_Bio_Cell__5$List), 
  category.names = c( "DW2" , "D2BF", "D2AF", "Control"), filename = 'University/MSc/Figures/Bio_Cell_Diet_Regression_0.05_venn_diagram.png', 
  output=TRUE, main = "Common Associations Between Cohorts",
  # Output features
  imagetype="png" , height = 900 , width = 900 , resolution = 300, compression = "lzw",
  # Circles
  lwd = 2, lty = 'blank', fill = myCol, cex = .6,
  # Set names
  cat.cex = 0.7, cat.fontface = "bold", cat.default.pos = "outer")



#CCA and univariate overlap
Set_CCA_1 <- CCA_All %>% 
  mutate(List = paste(CCA_All$Cohort, CCA_All$Marker1, CCA_All$Marker2, sep="_")) %>% 
  mutate(List1 = paste(CCA_All$Cohort, CCA_All$Marker2, CCA_All$Marker1, sep="_"))
Sig_Regress_1 <- rbind(data.frame(c("Within"),Sig_D_B_CC_2) %>% rename(Cohort = 1), data.frame(c("Before"),Sig_D_B_CC_BF) %>% rename(Cohort = 1), 
                       data.frame(c("After"),Sig_D_B_CC_AF) %>% rename(Cohort =1) )
Set_Regress_1 <- Sig_Regress_1 %>% 
  mutate(List = paste(Sig_Regress_1$Cohort, Sig_Regress_1$Marker1, Sig_Regress_1$Marker2, sep="_"))
inner_join(Set_Regress_1[c(1,5,6)] %>% rename(Marker1=2,Marker2=3), Set_CCA_1[c(1:3)]%>% rename(Marker1=2,Marker2=3))
inner_join(Set_Regress_1[c(1,5,6)] %>% rename(Marker1=3,Marker2=2), Set_CCA_1[c(1:3)]%>% rename(Marker1=2,Marker2=3))
venn.diagram(
  x = list(Set_CCA_1$List1, Set_Regress_1$List), 
  category.names = c("CCA" , "Univariate Regression "), filename = 'University/MSc/Figures/CCA_Regression_0.05_venn_diagram.png', 
  output=TRUE,
  # Output features
  imagetype="png" , height = 900 , width = 900 , resolution = 300, compression = "lzw",
  # Circles
  lwd = 2, lty = 'blank', fill = c("#B3E2CD", "#F4CAE4"), cex = .5,
  # Set names
  cat.cex = 0.7, cat.fontface = "bold", cat.default.pos = "text", cat.pos = 3)


#Biomarker~ Cell Venn
Set_2_Bio_C_ <- Sig_B_C_2.rg5[-c(1,4,8)] %>% 
  mutate(List = paste(Sig_B_C_2.rg5$Relationship, Sig_B_C_2.rg5$Biomarker, Sig_B_C_2.rg5$Cell_Count, sep="_"))
Set_Bef_Bio_C_ <- Sig_B_C_BF.rg5[-c(1,4,8)] %>% 
  mutate(List = paste(Sig_B_C_BF.rg5$Relationship, Sig_B_C_BF.rg5$Biomarker, Sig_B_C_BF.rg5$Cell_Count, sep="_"))
Set_Aft_Bio_C_ <- Sig_B_C_AF.rg5[-c(1,4,8)] %>% 
  mutate(List = paste(Sig_B_C_AF.rg5$Relationship, Sig_B_C_AF.rg5$Biomarker, Sig_B_C_AF.rg5$Cell_Count, sep="_"))
Set_Con_Bio_C_ <- Sig_B_C_C.rg5[-c(1,4,8)] %>% 
  mutate(List = paste(Sig_B_C_C.rg5$Relationship, Sig_B_C_C.rg5$Biomarker, Sig_B_C_C.rg5$Cell_Count, sep="_"))
venn.diagram(
  x = list( Set_2_Bio_C_$List, Set_Bef_Bio_C_$List, Set_Aft_Bio_C_$List, Set_Con_Bio_C_$List), 
  category.names = c( "DW2" , "D2BF", "D2AF", "Control"), filename = 'University/MSc/Figures/Bio_Cell_Regression_venn_diagram.png', 
  output=TRUE, main = "Common Associations Between Cohorts \n Biomarker ~ Cell",
  # Output features
  imagetype="png" , height = 900 , width = 900 , resolution = 300, compression = "lzw",
  # Circles
  lwd = 2, lty = 'blank', fill = myCol, cex = .6,
  # Set names
  cat.cex = 0.7, cat.fontface = "bold", cat.default.pos = "outer")

#Biomarker~ Diet Venn
Set_Aft_Bio_D_ <- Sig_D_B_AF %>% 
  mutate(List = paste(Sig_D_B_AF$Relationship, Sig_D_B_AF$Marker, Sig_D_B_AF$Nutrition, sep="_"))
Set_Con_Bio_D_ <- Sig_D_B_C %>% 
  mutate(List = paste(Sig_D_B_C$Relationship, Sig_D_B_C$Marker, Sig_D_B_C$Nutrition, sep="_"))
venn.diagram(
  x = list( Set_Aft_Bio_D_$List, Set_Con_Bio_D_$List), 
  category.names = c( "D2AF", "Control"), filename = 'University/MSc/Figures/Bio_Diet_Regression_venn_diagram.png', 
  output=TRUE, main = "Common Associations Between Cohorts \n Biomarker ~ Diet",
  # Output features
  imagetype="png" , height = 900 , width = 900 , resolution = 300, compression = "lzw",
  # Circles
  lwd = 2, lty = 'blank', fill = c("#DCEDC8", "#BBDEFB"), cex = .6,
  # Set names
  cat.cex = 0.5, cat.fontface = "bold", cat.default.pos = "outer")


#Diet ~ Cell Venn
Set_Aft_Cell_D_ <- Sig_D_C_AF %>% 
  mutate(List = paste(Sig_D_C_AF$Relationship, Sig_D_C_AF$Marker, Sig_D_C_AF$Nutrition, sep="_")) %>% select(List)
Set_Bef_Cell_D_ <- Sig_D_C_BF[-c(1)] %>% 
  mutate(List = paste(Sig_D_C_BF$Relationship, Sig_D_C_BF$Marker, Sig_D_C_BF$Nutrition, sep="_")) %>% select(List)
Set_2_Cell_D_ <- Sig_D_C_2 %>% 
  mutate(List = paste(Sig_D_C_2$Relationship, Sig_D_C_2$Marker, Sig_D_C_2$Nutrition, sep="_")) %>% select(List)
Set_C_Cell_D_ <- Sig_D_C_C %>% 
  mutate(List = paste(Sig_D_C_C$Relationship, Sig_D_C_C$Marker, Sig_D_C_C$Nutrition, sep="_")) %>% select(List)
venn.diagram(
  x = list( Set_2_Cell_D_$List, Set_Bef_Cell_D_$List, Set_Aft_Cell_D_$List, Set_C_Cell_D_$List), 
  category.names = c( "DW2" , "D2BF", "D2AF", "Control"), filename = 'University/MSc/Figures/Diet_Cell_Regression_venn_diagram.png', 
  output=TRUE, main = "Common Associations Between Cohorts \n Diet ~ Cell",
  # Output features
  imagetype="png" , height = 900 , width = 900 , resolution = 300, compression = "lzw",
  # Circles
  lwd = 2, lty = 'blank', fill = myCol, cex = .6,
  # Set names
  cat.cex = 0.7, cat.fontface = "bold", cat.default.pos = "outer")


write.csv(colnames(NAFLD_LR2_SD[-c(36:64,139,140,137,122:133,120,115:117)]), "University/MSc/Files/Supp Table 1.csv")
