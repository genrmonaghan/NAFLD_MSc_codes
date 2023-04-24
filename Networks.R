

#+-2 years
Sig_Regress2 <- Sig_D_B_CC_2[c(4,5)]
Sig_Regress2$Marker1 <- gsub("_", " ", Sig_Regress2$Marker1 )
Sig_Regress2$Marker2 <- gsub("_", " ", Sig_Regress2$Marker2 )
Sig_Regress2$Marker1 <- gsub(" direct", "-C", Sig_Regress2$Marker1 )
Sig_Regress2$Marker2 <- gsub(" direct", "-C", Sig_Regress2$Marker2 )
Sig_Regress2$Marker2 <- gsub(" cholesterol", "-C", Sig_Regress2$Marker2 )
Sig_Regress2$Marker1 <- gsub(" cholesterol", "-C", Sig_Regress2$Marker1 )
Sig_Regress2$Marker2 <- gsub("Apolipoprotein ", "Apo-", Sig_Regress2$Marker2 )
Sig_Regress2$Marker1 <- gsub("Apolipoprotein ", "Apo-", Sig_Regress2$Marker1 )
Sig_Regress2[Sig_Regress2 == "C reactive protein"] <- "CRP"
Sig_Regress2[Sig_Regress2 == "Aspartate aminotransferase"] <- "AST"
Sig_Regress2[Sig_Regress2 == "White blood cell (leukocyte) count"] <- "WBC count"
Sig_Regress2[Sig_Regress2 == "Mean platelet (thrombocyte) volume"] <- "Mean platelet volume"
Sig_Regress2[Sig_Regress2 == "Glycated haemoglobin (HbA1c)"] <- "HbA1c"
Sig_Regress2[Sig_Regress2 == "Cholesterol"] <- "TC"
Sig_Regress2[Sig_Regress2 == "Triglycerides"] <- "TG"
Sig_Regress2[Sig_Regress2 == "Gamma glutamyltransferase"] <- "GGT"
Sig_Regress2[Sig_Regress2 == "Alkaline phosphatase"] <- "ALP"
Sig_Regress2[Sig_Regress2 == "Mean corpuscular volume"] <- "MCV"
Sig_Regress2[Sig_Regress2 == "Red blood cell (erythrocyte) count"] <- "RBC count"
Sig_Regress2[Sig_Regress2 == "Red blood cell (erythrocyte) distribution width"] <- "RDW"
Sig_Regress2[Sig_Regress2 == "Mean corpuscular volume"] <- "MCV"
Sig_Regress2[Sig_Regress2 == "  "] <- " "
Sig_Regress2
#  %>% mutate(Marker1 = `str_sub<-`(Marker1, _, value = " "))
#  %>% str_replace_all(c("_" = " "))

#All
netreg2 <- graph_from_data_frame(d=Sig_Regress2, directed=F)
V(netreg2)$Q1_I1 <- c(rep(0, 19), rep(1, 17), 2)
V(netreg2)$color <- c(rep("khaki", 19), rep("turquoise", 17), "palevioletred2")
V(netreg2)$label.cex = 0.6
E(netreg2)$width <- ((log(1/Sig_D_B_CC_2$Bonferroni))/(log(1/max(Sig_D_B_CC_2$Bonferroni))))*1.3
E(netreg2)$color <- ifelse(Sig_D_B_CC_2$Relationship == "Negative", "indianred3", "palegreen3")
circle_2 <- layout_in_circle(netreg2)
plot(netreg2, layout = circle_2, asp = 1, margin = -0.1, 
     vertex.label.color= "grey25", main = "NAFLD Diagnosed Within +-2 years Cohort \n Diet~Biomarkers~Cell p<0.05",
     vertex.label.degree=0, vertex.label.dist=0) 

#Lipids
(Sig_Regress2_lipids <-  Sig_Regress2 %>% filter(str_detect(Marker2, 'LDL|HDL|holest|Apo|Trig|TC|TG|Lipo')) )
Sig_D_B_CC_2_lipids <- Sig_D_B_CC_2 %>% filter(str_detect(Marker2, 'LDL|HDL|holest|Apo|Trig|TC|TG|Lipo'))
netreg2_lipids <- graph_from_data_frame(d=Sig_Regress2_lipids, directed=F)
V(netreg2_lipids)$Q1_I1 <- c(rep(0, 7), rep(1, 5))
V(netreg2_lipids)$color <- c(rep("khaki", 7), rep("turquoise", 5))
V(netreg2_lipids)$label.cex = 0.6
E(netreg2_lipids)$width <- ((log(1/Sig_D_B_CC_2_lipids$Bonferroni))/(log(1/max(Sig_D_B_CC_2_lipids$Bonferroni))))*1.3
E(netreg2_lipids)$color <- ifelse(Sig_D_B_CC_2_lipids$Relationship == "Negative", "indianred2", "lightgreen")
circle_2_lipids <- layout_in_circle(netreg2_lipids)
plot(netreg2_lipids, layout = circle_2_lipids, vertex.label.color= "grey24", vertex.size=15, 
     main = "NAFLD Diagnosed Within +-2 years Cohort \n Lipids p<0.05")

Sig_Regress2_enzy <- Sig_Regress2 %>% filter(str_detect(Marker2, 'Gam|Asp|Ala|Alk|AST|GGT'))
Sig_D_B_CC_2_enzy <- Sig_D_B_CC_2 %>% filter(str_detect(Marker2, 'Gam|Asp|Ala|Alk|AST|GGT'))
netreg2_enzy <- graph_from_data_frame(d=Sig_Regress2_enzy, directed=F)
V(netreg2_enzy)$Q1_I1 <- c(rep(0, 10), rep(1, 3))
V(netreg2_enzy)$color <- c(rep("khaki", 10), rep("turquoise", 3))
V(netreg2_enzy)$label.cex = 0.6
E(netreg2_enzy)$width <- ((log(1/Sig_D_B_CC_2_enzy$Bonferroni))/(log(1/max(Sig_D_B_CC_2_enzy$Bonferroni))))*1.3
E(netreg2_enzy)$color <- ifelse(Sig_D_B_CC_2_enzy$Relationship == "Negative", "indianred2", "lightgreen")
circle_2_enzy <- layout_in_circle(netreg2_enzy)
plot(netreg2_enzy, layout = circle_2_enzy, vertex.label.color= "grey24", main = "NAFLD Diagnosed Within +-2 years Cohort \n Enzymes p<0.05")

(Sig_Regress2_RBC <- Sig_Regress2 %>% filter(str_detect(Marker1, 'Red|RBC|Haemo')) )
(Sig_D_B_CC_2_RBC <- Sig_D_B_CC_2 %>% filter(str_detect(Marker1, 'Red|RBC|Haemo')) )
netreg2_RBC <- graph_from_data_frame(d=Sig_Regress2_RBC, directed=F)
V(netreg2_RBC)$Q1_I1 <- c(rep(0, 3), rep(1, 11))
V(netreg2_RBC)$color <- c(rep("khaki", 3), rep("turquoise", 11))
V(netreg2_RBC)$label.cex = 0.6
E(netreg2_RBC)$width <- ((log(1/Sig_D_B_CC_2_RBC$Bonferroni))/(log(1/max(Sig_D_B_CC_2_RBC$Bonferroni))))*1.3
E(netreg2_RBC)$color <- ifelse(Sig_D_B_CC_2_RBC$Relationship == "Negative", "indianred2", "lightgreen")
circle_2_RBC <- layout_in_circle(netreg2_RBC)
plot(netreg2_RBC, layout = circle_2_RBC, vertex.label.color= "grey24", main = "NAFLD Diagnosed Within +-2 years Cohort \n RBC p<0.05")

Sig_Regress2_plat <- Sig_Regress2 %>% filter(str_detect(Marker1, 'Platelet'))
Sig_D_B_CC_2_plat <- Sig_D_B_CC_2 %>% filter(str_detect(Marker1, 'Platelet'))
netreg2_plat <- graph_from_data_frame(d=Sig_Regress2_plat, directed=F)
V(netreg2_plat)$Q1_I1 <- c(rep(0, 3), rep(1, 7))
V(netreg2_plat)$color <- c(rep("khaki", 3), rep("turquoise", 7))
V(netreg2_plat)$label.cex = 0.6
E(netreg2_plat)$width <- ((log(1/Sig_D_B_CC_2_plat$Bonferroni))/(log(1/max(Sig_D_B_CC_2_plat$Bonferroni))))*1.3
E(netreg2_plat)$color <- ifelse(Sig_D_B_CC_2_plat$Relationship == "Negative", "indianred2", "lightgreen")
circle_2_plat <- layout_in_circle(netreg2_plat)
plot(netreg2_plat, layout = circle_2_plat, vertex.label.color= "grey24", main = "NAFLD Diagnosed Within +-2 years Cohort \n Platelets p<0.05")

(Sig_Regress2_WBC <- Sig_Regress2 %>% filter(str_detect(Marker1, 'White|Neut|Eosin|Mono|Baso')) )
(Sig_Regress2_retic <- rbind(Sig_Regress2 %>% filter(str_detect(Marker1, 'eticulo')), 
                              Sig_Regress2 %>% filter(str_detect(Marker2, 'eticulo')) ))
(Sig_Regress2_urine <- rbind(Sig_Regress2 %>% filter(str_detect(Marker1, 'urine|Cyst|Creat|Ur')), 
                              Sig_Regress2 %>% filter(str_detect(Marker2, 'urine|Cyst|Creat|Ur')) ))
(Sig_Regress2_gluc <- rbind(Sig_Regress2 %>% filter(str_detect(Marker1, 'HbA1c|Gluc')), 
                             Sig_Regress2 %>% filter(str_detect(Marker2, 'HbA1c|Gluc')) ))




#Before
Sig_RegressBF <- (rbind(Sig_D_B_C_BF %>% rename(Marker1 = 4, Marker2 =5), Sig_B_C_BF.rg5[c(7,2,3,5,6)] %>% rename(Marker1 = 4, Marker2 =5)))
rownames(Sig_RegressBF) <- NULL
Sig_RegressBF$Marker1 <- gsub("_", " ", Sig_RegressBF$Marker1 )
Sig_RegressBF$Marker2 <- gsub("_", " ", Sig_RegressBF$Marker2 )
netregBF <- graph_from_data_frame(d=Sig_RegressBF[c(4,5)], directed=F)
V(netregBF)$Q1_I1 <- c(rep(0, 15), 1,  rep(1, 11))
V(netregBF)$color <- c(rep("khaki", 15), "palevioletred2", rep("turquoise", 11))
V(netregBF)$label.cex = 0.6
E(netregBF)$width <- ((log(1/Sig_RegressBF$Bonferroni))/(log(1/max(Sig_RegressBF$Bonferroni))))*1.3
E(netregBF)$color <- ifelse(Sig_RegressBF$Relationship == "Negative", "indianred2", "lightgreen")
circle_BF <- layout_in_circle(netregBF)
plot(netregBF, layout = circle_BF, vertex.label.color= "grey24", main = "NAFLD Diagnosed >2 years Before Cohort \n Diet~Biomarkers~Cell p<0.05")

(Sig_RegressBF_lipids <- Sig_RegressBF[c(3,4,5)] %>% filter(str_detect(Marker2, 'LDL|HDL|holest|Apo|Trig|Lipo')) )

Sig_RegressBF_enzy <- Sig_RegressBF[c(4,5)] %>% filter(str_detect(Marker2, 'Gam|Asp|Ala|Alk'))
Sig_D_B_CC_BF_enzy <- Sig_D_B_CC_BF %>% filter(str_detect(Marker2, 'Gam|Asp|Ala|Alk'))
netregBF_enzy <- graph_from_data_frame(d=Sig_RegressBF_enzy, directed=F)
V(netregBF_enzy)$Q1_I1 <- c(rep(0, 2), rep(1, 1))
V(netregBF_enzy)$color <- c(rep("khaki", 2), rep("turquoise", 1))
V(netregBF_enzy)$label.cex = 0.6
E(netregBF_enzy)$width <- ((log(1/Sig_D_B_CC_BF_enzy$Bonferroni))/(log(1/max(Sig_D_B_CC_BF_enzy$Bonferroni))))*1.3
E(netregBF_enzy)$color <- ifelse(Sig_D_B_CC_BF_enzy$Relationship == "Negative", "indianred2", "lightgreen")
circle_BF_enzy <- layout_in_circle(netregBF_enzy)
plot(netregBF_enzy, layout = circle_BF_enzy, vertex.label.color= "grey24", main = "NAFLD Diagnosed >2 years Before Cohort \n Enzymes p<0.05")


(Sig_RegressBF_RBC <- rbind(Sig_RegressBF %>% filter(str_detect(Marker1, 'Red|RBC|aemo')), 
                            Sig_RegressBF %>% filter(str_detect(Marker2, 'Red|RBC|aemoglobin '))  ) )
Sig_RegressBF_plat <- rbind(Sig_RegressBF %>% filter(str_detect(Marker1, 'Platelet')), 
                           Sig_RegressBF %>% filter(str_detect(Marker2, 'Platelet')) )
(Sig_RegressBF_WBC <- rbind(Sig_RegressBF %>% filter(str_detect(Marker1, 'White|Neut|Eosin|Mono|Baso')), 
                           Sig_RegressBF %>% filter(str_detect(Marker2, 'White|Neut|Eosin|Mono|Baso')) ))
(Sig_RegressBF_retic <- rbind(Sig_RegressBF %>% filter(str_detect(Marker1, 'eticulo')), 
                            Sig_RegressBF %>% filter(str_detect(Marker2, 'eticulo')) ))
(Sig_RegressBF_urine <- rbind(Sig_RegressBF %>% filter(str_detect(Marker1, 'urine|Cyst|Creat')), 
                              Sig_RegressBF %>% filter(str_detect(Marker2, 'urine|Cyst|Creat')) ))

#Add ceofficietns
#TTD graphs


#After
Sig_D_B_CC_AF <-  rbind(Sig_D_B_AF[-c(1)] %>% rename(Marker1 = 4, Marker2 =5), Sig_D_C_AF[-c(1)] %>% rename(Marker1 = 4, Marker2 =5), 
                        Sig_B_C_AF.rg5[c(7,2,3,5,6)] %>% rename(Marker1 = 4, Marker2 = 5) )
Sig_RegressAF <- Sig_D_B_CC_AF[c(4,5)]
Sig_RegressAF$Marker1 <- gsub("_", " ", Sig_RegressAF$Marker1 )
Sig_RegressAF$Marker2 <- gsub("_", " ", Sig_RegressAF$Marker2 )
Sig_RegressAF$Marker1 <- gsub("Gamma glutamyltransferase", "GGT", Sig_RegressAF$Marker1 )
Sig_RegressAF$Marker2 <- gsub("Gamma glutamyltransferase", "GGT", Sig_RegressAF$Marker2 )
Sig_RegressAF$Marker1 <- gsub("Aspartate aminotransferase", "AST", Sig_RegressAF$Marker1 )
Sig_RegressAF$Marker2 <- gsub("Aspartate aminotransferase", "AST", Sig_RegressAF$Marker2 )
Sig_RegressAF$Marker1 <- gsub("Alanine aminotransferase", "ALT", Sig_RegressAF$Marker1 )
Sig_RegressAF$Marker2 <- gsub("Alanine aminotransferase", "ALT", Sig_RegressAF$Marker2 )
Sig_RegressAF$Marker2 <- gsub(" direct", "-C", Sig_RegressAF$Marker2 )
Sig_RegressAF$Marker1 <- gsub(" direct", "-C", Sig_RegressAF$Marker1 )
Sig_RegressAF$Marker2 <- gsub(" cholesterol", "-C", Sig_RegressAF$Marker2 )
Sig_RegressAF$Marker1 <- gsub(" cholesterol", "-C", Sig_RegressAF$Marker1 )
Sig_RegressAF$Marker2 <- gsub("Apolipoprotein ", "Apo-", Sig_RegressAF$Marker2 )
Sig_RegressAF$Marker1 <- gsub("Apolipoprotein ", "Apo-", Sig_RegressAF$Marker1 )
Sig_RegressAF$Marker2 <- gsub("Cholesterol", "TC", Sig_RegressAF$Marker2 )
Sig_RegressAF$Marker1 <- gsub("Cholesterol", "TC", Sig_RegressAF$Marker1 )
Sig_RegressAF$Marker2 <- gsub("Alkaline phosphatase", "Alk-P", Sig_RegressAF$Marker2 )
Sig_RegressAF$Marker1 <- gsub("Alkaline phosphatase", "Alk-P", Sig_RegressAF$Marker1 )


netregAF <- graph_from_data_frame(d=Sig_RegressAF, directed=F)
V(netregAF)$Q1_I1 <- c(rep(0, 20), rep(1, 29), rep(2,25), rep(0,9))
V(netregAF)$color <- c(rep("turquoise", 20), rep("khaki", 29), rep("palevioletred2", 25), rep("turquoise", 9) )
V(netregAF)$label.cex = 0.3
E(netregAF)$width <- ((log(1/Sig_D_B_CC_AF$Bonferroni))/(log(1/max(Sig_D_B_CC_AF$Bonferroni))))*0.2
E(netregAF)$color <- ifelse(Sig_D_B_CC_AF$Relationship == "Negative", "indianred2", "lightgreen")
circle_AF <- layout_in_circle(netregAF)
plot(netregAF, layout = circle_AF, vertex.size=7, vertex.label.color= "grey2",
     main = "NAFLD Diagnosed >2 years After Cohort \n Diet~Biomarkers~Cell p<0.05")


(Sig_RegressAF_enzy <- rbind(Sig_RegressAF %>% filter(str_detect(Marker2, 'Gam|Asp|Ala|Alk|GGT|ALT|AST')), 
                             Sig_RegressAF %>% filter(str_detect(Marker1, 'Gam|Asp|Ala|Alk|GGT|ALT|AST'))) )
netregAF_enzy <- graph_from_data_frame(d=Sig_RegressAF_enzy, directed=F)
V(netregAF_enzy)$color <- c(rep("khaki", 24), rep("turquoise", 4), rep("palevioletred2", 9))
V(netregAF_enzy)$label.cex = 0.5
E(netregAF_enzy)$width <- ((log(1/Sig_D_B_CC_AF_enzy$Bonferroni))/(log(1/max(Sig_D_B_CC_AF_enzy$Bonferroni))))*0.2
E(netregAF_enzy)$color <- ifelse(Sig_D_B_CC_AF_enzy$Relationship == "Negative", "indianred2", "lightgreen")
circle_AF_enzy <- layout_in_circle(netregAF_enzy)
plot(netregAF_enzy, layout = circle_AF_enzy, vertex.size=10, main = "NAFLD Diagnosed >2 years After Cohort \n Enzymes p<0.05", vertex.label.color= "grey2")

(Sig_RegressAF_lipids <- rbind(Sig_RegressAF %>% filter(str_detect(Marker2, 'LDL|HDL|holest|Apo|Trig|Lipo|TC')),  
                               Sig_RegressAF %>% filter(str_detect(Marker1, 'LDL|HDL|holest|Apo|Trig|Lipo|TC'))) )
Sig_D_B_CC_AF_lipids <- rbind(Sig_D_B_CC_AF %>% filter(str_detect(Marker2, 'LDL|HDL|holest|Apo|Trig|TC')), 
                            Sig_D_B_CC_AF %>% filter(str_detect(Marker1, 'LDL|HDL|holest|Apo|Trig|TC')))
netregAF_lipids <- graph_from_data_frame(d=Sig_RegressAF_lipids, directed=F)
nrow(unique(Sig_RegressAF_lipids %>% filter(!str_detect(Marker1, 'LDL|HDL|holest|Apo|Trig|TC')) %>% select(Marker1) ))
nrow(unique(rbind(Sig_RegressAF_lipids %>% filter(str_detect(Marker1, 'LDL|HDL|holest|Apo|Trig|TC')) %>% select(Marker1) %>% rename(Marker2 = 1),  
                  Sig_RegressAF_lipids %>% filter(str_detect(Marker2, 'LDL|HDL|holest|Apo|Trig|TC')) %>% select(Marker2)) ))
nrow(unique(Sig_RegressAF_lipids %>% filter(!str_detect(Marker2, 'LDL|HDL|holest|Apo|Trig|TC')) %>% select(Marker2) ))
V(netregAF_lipids)$Q1_I1 <- c(rep(0, 23), rep(1, 6), rep(2, 6))
V(netregAF_lipids)$color <- c(rep("khaki", 23), rep("turquoise", 6), rep("palevioletred2", 6))
V(netregAF_lipids)$label.cex = 0.5
E(netregAF_lipids)$width <- ((log(1/Sig_D_B_CC_AF_lipids$Bonferroni))/(log(1/max(Sig_D_B_CC_AF_lipids$Bonferroni))))*0.15
E(netregAF_lipids)$color <- ifelse(Sig_D_B_CC_AF_lipids$Relationship == "Negative", "indianred2", "lightgreen")
circle_AF_lipids <- layout_in_circle(netregAF_lipids)
plot(netregAF_lipids, layout = circle_AF_lipids, vertex.size=10, vertex.label.color= "grey2", 
     main = "NAFLD Diagnosed >2 years After Cohort \n Lipids p<0.05")


(Sig_RegressAF_RBC <- Sig_RegressAF %>% filter(str_detect(Marker1, 'Red|aemo|RBC'))   ) 
Sig_D_B_CC_AF_RBC <- Sig_D_B_CC_AF %>% filter(str_detect(Marker1, 'Red|aemoglobin|RBC'))
netregAF_RBC <- graph_from_data_frame(d=Sig_RegressAF_RBC, directed=F)
nrow(unique(Sig_RegressAF_RBC %>% filter(str_detect(Marker1, 'Red|aemoglobin')) %>% select(Marker1) ))
nrow(unique(Sig_RegressAF_RBC %>% select(Marker2) ))
nrow(unique(Sig_RegressAF_RBC %>% filter(str_detect(Marker2, 'Eat|eat|intake')) %>% select(Marker2) ))
V(netregAF_RBC)$Q1_I1 <- c(rep(0, 5), rep(2, 12), rep(1, 24))
V(netregAF_RBC)$color <- c(rep("khaki", 5), rep("palevioletred2", 12), rep("turquoise", 24))
V(netregAF_RBC)$label.cex = 0.5
E(netregAF_RBC)$width <- ((log(1/Sig_D_B_CC_AF_RBC$Bonferroni))/(log(1/max(Sig_D_B_CC_AF_RBC$Bonferroni))))*0.2
E(netregAF_RBC)$color <- ifelse(Sig_D_B_CC_AF_RBC$Relationship == "Negative", "indianred2", "lightgreen")
circle_AF_RBC <- layout_in_circle(netregAF_RBC)
plot(netregAF_RBC, layout = circle_AF_RBC, vertex.size=10, vertex.label.color= "grey2", main = "NAFLD Diagnosed >2 years After Cohort \n RBC p<0.05")


(Sig_RegressAF_urine <- rbind(Sig_RegressAF %>% filter(str_detect(Marker1, 'urine|Cyst|Creat|Urea|Urate')), 
                              Sig_RegressAF %>% filter(str_detect(Marker2, 'urine|Cyst|Creat|Urea|Urate')) ))
Sig_D_B_CC_AF_urine <- rbind(Sig_D_B_CC_AF %>% filter(str_detect(Marker1, 'urine|Cyst|Creat|Urea|Urate')) , 
                             Sig_D_B_CC_AF %>% filter(str_detect(Marker2, 'urine|Cyst|Creat|Urea|Urate')) )
netregAF_urine <- graph_from_data_frame(d=Sig_RegressAF_urine, directed=F)
nrow(unique(Sig_RegressAF_urine %>% filter(!str_detect(Marker1, 'urine|Cyst|Creat|Urea|Urate')) %>% select(Marker1) ))
nrow(unique(rbind(Sig_RegressAF_urine %>% filter(str_detect(Marker1, 'urine|Cyst|Creat|Urea|Urate')) %>% select(Marker1) %>% rename(Marker2 = 1),  
                  Sig_RegressAF_urine %>% filter(str_detect(Marker2, 'urine|Cyst|Creat|Urea|Urate')) %>% select(Marker2)) ))
nrow(unique(Sig_RegressAF_urine %>% filter(!str_detect(Marker2, 'urine|Cyst|Creat|Urea|Urate')) %>% select(Marker2) ))
V(netregAF_urine)$color <- c( rep("turquoise", 6), rep("khaki", 20), rep("palevioletred2", 18), "turquoise")
V(netregAF_urine)$label.cex = 0.5
E(netregAF_urine)$width <- ((log(1/Sig_D_B_CC_AF_urine$Bonferroni))/(log(1/max(Sig_D_B_CC_AF_urine$Bonferroni))))*0.3
E(netregAF_urine)$color <- ifelse(Sig_D_B_CC_AF_urine$Relationship == "Negative", "indianred2", "lightgreen")
circle_AF_urine <- layout_in_circle(netregAF_urine)
plot(netregAF_urine, layout = circle_AF_urine, vertex.size=10, vertex.label.color= "grey2", main = "NAFLD Diagnosed >2 years After Cohort \n urine p<0.05")

(Sig_RegressAF_plat <- rbind(Sig_RegressAF %>% filter(str_detect(Marker1, 'Platelet')), 
                            Sig_RegressAF %>% filter(str_detect(Marker2, 'Platelet')) ) )
(Sig_RegressAF_WBC <- rbind(Sig_RegressAF %>% filter(str_detect(Marker1, 'White|Neut|Eosin|Mono|Baso')), 
                            Sig_RegressAF %>% filter(str_detect(Marker2, 'White|Neut|Eosin|Mono|Baso')) ))
(Sig_RegressAF_retic <- rbind(Sig_RegressAF %>% filter(str_detect(Marker1, 'eticulo')), 
                              Sig_RegressAF %>% filter(str_detect(Marker2, 'eticulo')) ))




#Control
Sig_D_B_CC_C <-  rbind(Sig_D_B_C[-c(1)] %>% rename(Marker1 = 4, Marker2 =5), Sig_D_C_C[-c(1)] %>% rename(Marker1 = 4, Marker2 =5), 
                        Sig_B_C_C.rg5[c(7,2,3,5,6)] %>% rename(Marker1 = 4, Marker2 = 5) )
Sig_RegressC <- Sig_D_B_CC_C[c(4,5)]
netregC <- graph_from_data_frame(d=Sig_RegressC, directed=F)
V(netregC)$Q1_I1 <- c(rep(0, 21), rep(1, 29), rep(2,32), rep(0,9))
V(netregC)$color <- c(rep("turquoise", 21), rep("khaki", 29), rep("palevioletred2", 32), rep("turquoise", 9) )
V(netregC)$label.cex = 0.3
E(netregC)$width <- ((log(1/Sig_D_B_CC_C$Bonferroni))/(log(1/max(Sig_D_B_CC_C$Bonferroni))))*0.3
E(netregC)$color <- ifelse(Sig_D_B_CC_C$Relationship == "Negative", "indianred2", "lightgreen")
circle_C <- layout_in_circle(netregC)
plot(netregC, layout = circle_C, vertex.size=6, main = "NAFLD Diagnosed Control Cohort \n Diet~Biomarkers~Cell p<0.05")

(Sig_RegressAF_lipids <- rbind(Sig_RegressAF %>% filter(str_detect(Marker2, 'LDL|HDL|holest|Apo|Trig')),  
                               Sig_RegressAF %>% filter(str_detect(Marker1, 'LDL|HDL|holest|Apo|Trig'))) )
(Sig_RegressAF_enzy <- rbind(Sig_RegressAF %>% filter(str_detect(Marker2, 'Gam|Asp|Ala|Alk')), 
                             Sig_RegressAF %>% filter(str_detect(Marker1, 'Gam|Asp|Ala|Alk'))) )
(Sig_RegressAF_RBC <- rbind(Sig_RegressAF %>% filter(str_detect(Marker1, 'Red|aemo')), 
                            Sig_RegressAF %>% filter(str_detect(Marker2, 'Red|aemoglobin '))  ) )
(Sig_RegressAF_plat <- rbind(Sig_RegressAF %>% filter(str_detect(Marker1, 'Platelet')), 
                             Sig_RegressAF %>% filter(str_detect(Marker2, 'Platelet')) ) )
(Sig_RegressAF_WBC <- rbind(Sig_RegressAF %>% filter(str_detect(Marker1, 'White|Neut|Eosin|Mono|Baso')), 
                            Sig_RegressAF %>% filter(str_detect(Marker2, 'White|Neut|Eosin|Mono|Baso')) ))
(Sig_RegressAF_retic <- rbind(Sig_RegressAF %>% filter(str_detect(Marker1, 'eticulo')), 
                              Sig_RegressAF %>% filter(str_detect(Marker2, 'eticulo')) ))
(Sig_RegressAF_urine <- rbind(Sig_RegressAF %>% filter(str_detect(Marker1, 'urine|Cyst|Creat')), 
                              Sig_RegressAF %>% filter(str_detect(Marker2, 'urine|Cyst|Creat')) ))
