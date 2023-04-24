
Bio_NAFLD_LRAF_SDP <- Bio_NAFLD_LRAF_SD
Bio_NAFLD_LRBF_SDP <- Bio_NAFLD_LRBF_SD
Bio_NAFLD_LRC_SDP <- Bio_NAFLD_LRC_SD
names(Bio_NAFLD_LRC_SDP)[names(Bio_NAFLD_LRC_SDP) == "Creatinine_in_urine"] <- "Creatinine_(enzymatic)_in_urine"
Bio_NAFLD_LR2_SDP <- Bio_NAFLD_LR2_SD

Bio_NAFLD_LRAF_SDP$Participants <- "After"
Bio_NAFLD_LRBF_SDP$Participants <- "Before"
Bio_NAFLD_LRC_SDP$Participants <- "Control"
Bio_NAFLD_LR2_SDP$Participants <- "Within_2"
Bio_NAFLD <- rbind(Bio_NAFLD_LR2_SDP, Bio_NAFLD_LRC_SDP, Bio_NAFLD_LRBF_SDP, Bio_NAFLD_LRAF_SDP)
Bio_NAFLD$Participants <- ordered(Bio_NAFLD$Participants,levels = c("Control", "After", "Within_2",  "Before"))
Bio_NAFLD$AST_ALT <- Bio_NAFLD$Aspartate_aminotransferase/Bio_NAFLD$Alanine_aminotransferase
names(Bio_NAFLD[,-c(33)])


Results_Bio_1 <- data.frame(c(1), c(1), c(1))
colnames(Results_Bio_1) <- c("p_value","Group","Name")
set.seed(132)
for (i in names(Bio_NAFLD[,c(-33)])) {
  aov <- data.frame(TukeyHSD(aov(Bio_NAFLD[[i]] ~ Bio_NAFLD[["Participants"]]))$`Bio_NAFLD[["Participants"]]`)
  aov1 <- aov %>%
    add_column(Group = rownames(aov)) %>%
    rename("p_value" = "p.adj") %>%
    add_column(Name = paste0(i)) %>%
    select(-c(1,2,3))
  Results_Bio_1 <- rbind(Results_Bio_1, aov1)
}
rownames(Results_Bio_1) <- NULL
Results_Bio_2 <- Results_Bio_1[c(-1),][order(Results_Bio_1$p_value),]
Results_Bio_2$p_value <- as.numeric(Results_Bio_2$p_value)
Results_Bio_2_1_Sig <- Results_Bio_2 %>%
  mutate(Significance = ifelse(p_value < 0.05, "Significant", "Not significant" )) %>%
  filter(Significance == "Significant")
Results_Bio_Sig  <- Results_Bio_2_1_Sig[,c(-4)][order(Results_Bio_2_1_Sig$p_value,  Results_Bio_2_1_Sig$Name),]
rownames(Results_Bio_Sig) <- NULL
Results_Bio_Sig

Results_Bio_Sig %>% filter(str_detect(Name, 'HDL|Asp|Alk|Ala|Gam|HbA1c|Direct|C_')) %>% arrange(Name)
table(Results_Bio_Sig$Name) %>% as.data.frame() %>% arrange(desc(Freq))
table(Results_Bio_Sig$Group) %>% as.data.frame() %>% arrange(desc(Freq))
Results_Bio_Sigb <- Results_Bio_Sig %>% filter(str_detect(Group, 'Control'))
table(Results_Bio_Sigb$Name) %>%  as.data.frame() %>%   arrange(desc(Freq))

#Results by cohort
NMB_Bio_1 <- data.frame(c(1), c(2), c(3), c(4), c(5), c(6), c(7), c(8), c(9) )
colnames(NMB_Bio_1) <- c("Participants","Marker", "mean", "L_CI", "U_CI", "sd", "median", "LQ", "UQ")
set.seed(132)
for (i in unique(Results_Bio_Sig$Name) ) { 
  for(p in unique(Bio_NAFLD$Participants)) {
    NMB_Bio_1 <- rbind(NMB_Bio_1, (
      data.frame( Participants = p, Marker = i,  
                  mean = mean((Bio_NAFLD%>%filter(Participants == p))[[i]], na.rm = TRUE), 
                  L_CI = mean((Bio_NAFLD%>%filter(Participants == p))[[i]], na.rm = TRUE)-(qnorm(0.975)*sd((Bio_NAFLD%>%filter(Participants == p))[[i]], na.rm = TRUE)/sqrt(nrow(data.frame( (Bio_NAFLD %>% filter(Participants == p) )[[i]] ) ) )),
                  U_CI = mean((Bio_NAFLD%>%filter(Participants == p))[[i]], na.rm = TRUE)+qnorm(0.975)*sd((Bio_NAFLD%>%filter(Participants == p))[[i]], na.rm = TRUE)/sqrt(nrow(data.frame( (Bio_NAFLD %>% filter(Participants == p) )[[i]] ) ) ), 
                  sd = sd((Bio_NAFLD%>%filter(Participants == p))[[i]], na.rm = TRUE), 
                  median = median((Bio_NAFLD%>%filter(Participants == p))[[i]], na.rm = TRUE), 
                  LQ = quantile((Bio_NAFLD%>%filter(Participants == p))[[i]], probs = c(0.25), na.rm = TRUE), 
                  UQ = quantile((Bio_NAFLD%>%filter(Participants == p))[[i]], probs = c(0.75), na.rm = TRUE)) 
    ) ) }}
rownames(NMB_Bio_1) <- NULL
NMB_Bio_1$L_CI <- format(as.numeric(NMB_Bio_1$L_CI), scientific = F, digits = 2)
NMB_Bio_1$mean <- format(as.numeric(NMB_Bio_1$mean), scientific = F, digits = 2)
NMB_Bio_1$U_CI <- format(as.numeric(NMB_Bio_1$U_CI), scientific = F, digits = 2)
NMB_Bio_1$median <- format(as.numeric(NMB_Bio_1$median), scientific = F, digits = 2)
NMB_Bio_1$sd <- format(as.numeric(NMB_Bio_1$sd), scientific = F, digits = 2)
NMB_Bio_1$LQ <- format(as.numeric(NMB_Bio_1$LQ), scientific = F, digits = 2)
NMB_Bio_1$UQ <- format(as.numeric(NMB_Bio_1$UQ), scientific = F, digits = 2)
(NMB_Bio <- NMB_Bio_1[-c(1),])

#Normal range
NMR_Bio <- data.frame((table(Results_Bio_Sig$Name) %>% as.data.frame() %>% arrange(desc(Freq)) %>% select(Var1) ),
                      c( 8,  0, 4, 44,0, 0,0.62, 5, 0, 2,0.5,34, 1.1,0, 10,1.71,  0, 1768,3.9, 1,  2, 75,  0,160, 0,0,1.12,60,2.1),
                      c(33,5.1,36,147,10,5,1.15,40,42,32, 35,54,2.05,3,144,20.5,1.2,28884,5.6, 10,125,300,1.7,430,30,1,1.15,83,8.5),
                      c("U/L","µmol/L","U/L","U/L","mg/L","mmol/L","mg/L","U/L","mmol/mol","nmol/L","nmol/L","g/L","g/L","mmol/L","nmol/L","µmol/L","g/L",
                        "µmol/L","mmol/L","mmol/L","mmol/L","mmol/L","mmol/L","µmol/L","µmol/L","ratio","mmol/L","g/L","mmol/L"))
colnames(NMR_Bio) <- c("Marker", "lower", "upper", "units")
NMR_Bio %>% arrange(Marker)

#Choosing results
Stingy <- data.frame(c(1), c(0))
colnames(Stingy) <- c("Marker" ,"Useful")
set.seed(132)
Stingy2 <- Stingy
Stingy3 <- Stingy2
Stingy4 <- Stingy2
for (i in NMR_Bio$Marker){
  if (  as.numeric((NMR_Bio %>% filter(Marker == i)  )$lower ) < 
        as.numeric((NMB_Bio %>% filter(Marker == i) %>% filter(Participants == "Control") ) $mean )    ) { y <- data.frame(i, 1) 
  } else { y <- data.frame(i, 0)} 
  colnames(y)  <- c("Marker" ,"Useful")
  Stingy2 <- rbind(Stingy2, y)
  Stingy2 <- Stingy2 %>% filter(Useful == 1)
}
for (i in NMR_Bio$Marker){
  if (  as.numeric((NMR_Bio %>% filter(Marker == i)  )$upper ) > 
        as.numeric((NMB_Bio %>% filter(Marker == i) %>% filter(Participants == "Control") ) $mean )    ) { y <- data.frame(i, 1) 
  } else { y <- data.frame(i, 0)} 
  colnames(y)  <- c("Marker" ,"Useful")
  Stingy3 <- rbind(Stingy3, y) 
  Stingy3 <- Stingy3 %>% filter(Useful == 1)
}
for (i in NMR_Bio$Marker){
  if (  as.numeric((NMR_Bio %>% filter(Marker == i)  )$upper ) < 
        as.numeric(max( (NMB_Bio %>% filter(Marker == i) %>% filter(!Participants == "Control") ) $mean ))       ) { z <- data.frame(i, 1) 
  } else if( as.numeric((NMR_Bio %>% filter(Marker == i)  )$lower ) > 
             as.numeric(min( (NMB_Bio %>% filter(Marker == i) %>% filter(!Participants == "Control") ) $mean ))  ) { z <- data.frame(i, 1) 
  } else { z <- data.frame(i, 0)} 
  colnames(z)  <- c("Marker" ,"Useful")
  Stingy4 <- rbind(Stingy4, z) 
  Stingy4 <- Stingy4 %>% filter(Useful == 1)
}
for (i in NMR_Bio$Marker) {
  if (min((NMB_Bio %>% filter(str_detect(Marker, i)) %>% filter(!Participants == "Control"))  $L_CI)  >
      max((NMB_Bio %>% filter(str_detect(Marker, i)) %>% filter(Participants == "Control"))  $U_CI ) ) { x <- data.frame(i, TRUE)
  } else if (max((NMB_Bio %>% filter(str_detect(Marker, i)) %>% filter(!Participants == "Control")) $U_CI ) <
             min((NMB_Bio %>% filter(str_detect(Marker, i)) %>% filter(Participants == "Control") ) $L_CI ) ) { x <- data.frame(i, TRUE)
  } else { x <- data.frame(i, FALSE) }
  colnames(x)  <- c("Marker" ,"Useful")
  Stingy <- rbind(Stingy, x)
  Stingy <- Stingy %>% filter(Useful == 1)
}
inner_join(Stingy2, Stingy3)[c(1)]
(Stinginess <- inner_join(inner_join(Stingy2, Stingy3)[c(1)], inner_join(Stingy4[c(1)], Stingy[c(1)])) )
rbind(NMB_Bio %>% filter(Marker == c(Stinginess[c(1),])) %>% arrange(mean), NMB_Bio %>% filter(Marker == c(Stinginess[c(2),])) %>% arrange(mean))




library(RColorBrewer)
myCol2 <- brewer.pal(4, "Set2")
mycol4 <- c("#2196F3", "#9CCC65", "#FDD835", "#EC407A")
mycol3 <- c("#1976D2", "#689F38", "#F9A825", "#D81B60")
mycol3.5 <- c("#BBDEFB", "#DCEDC8", "#FFF9C4", "#F8BBD0")
mycomparisons_2 <- list( c("Control", "Within_2"),  c("Control", "After"), c("Control", "Before"), c("Within_2", "After"),  c("After", "Before"),
                               c("Within_2", "Before"))

#Alkaline_phosphatase
ggboxplot(Bio_NAFLD, x = "Participants", y = "Alkaline_phosphatase", alpha=0.4, color = "Participants", fill = "Participants",
          palette = mycol3,  ylab = "ALP (U/L)", xlab = "Cohort", width = 0.75) +
  stat_compare_means(comparisons = mycomparisons_2, method = "t.test", paired = FALSE) +
  ggtitle("Alkaline Phosphatase")+theme(legend.position="none") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_x_discrete(labels = c(paste0("Control \n (n=7307)"),  paste0("D2AF \n (n=4210)"), paste0("DW2 \n (n=450)"), paste0("D2BF \n (n=326)")))

#Alanine_aminotransferase
ggboxplot(Bio_NAFLD, x = "Participants", y = "Alanine_aminotransferase", color = "Participants", alpha=0.4, fill = "Participants",
          palette = mycol3,  ylab = "ALT (U/L)", xlab = "Cohort", width = 0.75) +
  stat_compare_means(comparisons = mycomparisons_2, method = "t.test", paired = FALSE) +
  ggtitle("Alanine aminotransferase")+ theme(legend.position="none") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_x_discrete(labels = c(paste0("Control \n (n=7307)"), paste0("D2AF \n (n=4210)"),paste0("DW2 \n (n=450)"), paste0("D2BF \n (n=326)")))

#Aspartate_aminotransferase
ggboxplot(Bio_NAFLD, x = "Participants", y = "Aspartate_aminotransferase", color = "Participants", alpha=0.4, fill = "Participants",
                    palette = mycol3,  ylab = "AST (U/L)", xlab = "Cohort", width = 0.75) +
            stat_compare_means(comparisons = mycomparisons_2, method = "t.test", paired = FALSE) +
            ggtitle("Aspartate aminotransferase")+ theme(legend.position="none") +
            theme(plot.title = element_text(hjust = 0.5)) + ylim(0,290) +
            scale_x_discrete(labels = c(paste0("Control \n (n=7307)"), paste0("D2AF \n (n=4210)"),paste0("DW2 \n (n=450)"), paste0("D2BF \n (n=326)")))


#Gamma_glutamyltransferase
ggboxplot(Bio_NAFLD, x = "Participants", y = "Gamma_glutamyltransferase", color = "Participants", alpha=0.4, fill = "Participants",
          palette = mycol3,  ylab = "GGT (U/L)", xlab = "Cohort", width = 0.75) +
  stat_compare_means(comparisons = mycomparisons_2, method = "t.test", paired = FALSE) +
  ggtitle("Gamma glutamyl transferase")+ theme(legend.position="none") +
  theme(plot.title = element_text(hjust = 0.5)) + #ylim(0,290) +
  scale_x_discrete(labels = c(paste0("Control \n (n=7307)"), paste0("D2AF \n (n=4210)"),paste0("DW2 \n (n=450)"), paste0("D2BF \n (n=326)")))


#TG
ggboxplot(Bio_NAFLD, x = "Participants", y = "Triglycerides", color = "Participants", alpha=0.4, fill = "Participants",
          palette = mycol3,  ylab = "TG (mmol/L)", xlab = "Cohort", width = 0.75) +
  stat_compare_means(comparisons = mycomparisons_2, method = "t.test", paired = FALSE) +
  ggtitle("Triglycerides")+ theme(legend.position="none") +
  theme(plot.title = element_text(hjust = 0.5)) + #ylim(0,290) +
  scale_x_discrete(labels = c(paste0("Control \n (n=7307)"), paste0("D2AF \n (n=4210)"),paste0("DW2 \n (n=450)"), paste0("D2BF \n (n=326)")))


#HDL-C
ggboxplot(Bio_NAFLD, x = "Participants", y = "HDL_cholesterol", color = "Participants", alpha=0.4, fill = "Participants",
          palette = mycol3,  ylab = "HDL-C (mmol/L)", xlab = "Cohort", width = 0.75) +
  stat_compare_means(comparisons = mycomparisons_2, method = "t.test", paired = FALSE) +
  ggtitle("HDL Cholesterol")+ theme(legend.position="none") +
  theme(plot.title = element_text(hjust = 0.5)) + #ylim(0,290) +
  scale_x_discrete(labels = c(paste0("Control \n (n=7307)"), paste0("D2AF \n (n=4210)"),paste0("DW2 \n (n=450)"), paste0("D2BF \n (n=326)")))


#CRP
ggboxplot(Bio_NAFLD, x = "Participants", y = "C_reactive_protein", color = "Participants", alpha=0.4, fill = "Participants",
          palette = mycol3,  ylab = "CRP (mg/L)", xlab = "Cohort", width = 0.75) +
  stat_compare_means(comparisons = mycomparisons_2, method = "t.test", paired = FALSE) +
  ggtitle("C reactive Protein")+ theme(legend.position="none") +
  theme(plot.title = element_text(hjust = 0.5)) + #ylim(0,290) +
  scale_x_discrete(labels = c(paste0("Control \n (n=7307)"), paste0("D2AF \n (n=4210)"),paste0("DW2 \n (n=450)"), paste0("D2BF \n (n=326)")))

#HbA1c
ggboxplot(Bio_NAFLD, x = "Participants", y = "Glycated_haemoglobin_(HbA1c)", color = "Participants", alpha=0.4, fill = "Participants",
          palette = mycol3,  ylab = "HbA1c (mmol/mol)", xlab = "Cohort", width = 0.75) +
  stat_compare_means(comparisons = mycomparisons_2, method = "t.test", paired = FALSE) +
  ggtitle("HbA1c")+ theme(legend.position="none") +
  theme(plot.title = element_text(hjust = 0.5)) + #ylim(0,290) +
  scale_x_discrete(labels = c(paste0("Control \n (n=7307)"), paste0("D2AF \n (n=4210)"),paste0("DW2 \n (n=450)"), paste0("D2BF \n (n=326)")))

#Direct_bilirubin
ggboxplot(Bio_NAFLD, x = "Participants", y = "Direct_bilirubin", color = "Participants", alpha=0.4, fill = "Participants",
          palette = mycol3,  ylab = "Direct bilirubin (µmol/L)", xlab = "Cohort", width = 0.75) +
  stat_compare_means(comparisons = mycomparisons_2, method = "t.test", paired = FALSE) +
  ggtitle("Direct bilirubin")+ theme(legend.position="none") +
  theme(plot.title = element_text(hjust = 0.5)) + #ylim(0,290) +
  scale_x_discrete(labels = c(paste0("Control \n (n=7307)"), paste0("D2AF \n (n=4210)"),paste0("DW2 \n (n=450)"), paste0("D2BF \n (n=326)")))


