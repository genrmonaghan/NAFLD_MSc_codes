library(ggraph)
library(igraph)
d1 <- data.frame(from="origin", to=paste("group", seq(1,9), sep=""))
d2 <- data.frame( from = c("group1",rep("group3",6),"group4","group4","group3","group4","group4", rep("group1",10), rep("group2",7), rep("group5",39), 
                           "group6", "group9","group7","group6","group7","group6","group9","group7","group6","group7", rep("group9",5),"group8","group9",
                           "group8","group9", rep("group7",3),"group8","group8", rep("group9",3),rep("group8",4),"group9"),
                  to = c(colnames(NAFLD_LR2_SD[c(2:69, 76:107)])) )
edges <- rbind(d1, d2) %>% arrange(from)

data.frame(tabyl(Sig_D_C_C$Marker) %>% arrange(n)) %>% mutate(group = edges$from[ match( data.frame(tabyl(Sig_D_C_C$Marker) %>% 
                                                                                              arrange(n) %>% rename("Marker"=1))$Marker,edges$to)])
adda <- Sig_D_C_C[-c(1,8,4)] %>% mutate(group = edges$from[ match(Sig_D_C_C$Marker,edges$to)])
tabyl(adda$group) %>% arrange(n)
data.frame(tabyl((Sig_D_C_C %>% filter(str_detect(Marker, 'GGT|AST|ALT|ALP|Alk|Ala|Asp|Gam')))$Nutrition) %>% arrange(n) )
data.frame(tabyl((Sig_D_C_C %>% filter(str_detect(Marker, 'Apo|Lipo|LDL|HDL|holest|TG|TC|Trigl')))$Nutrition) %>% arrange(n) )
acca <- Sig_D_C_C[-c(1,8,4)] %>% mutate(group = edges$from[ match(Sig_D_C_C$Nutrition,edges$to)])
data.frame(tabyl(Sig_D_C_C$Nutrition) %>% arrange(n)) %>% mutate(group = edges$from[ match( data.frame(tabyl(Sig_D_C_C$Nutrition) %>% 
                                                                                                    arrange(n) %>% rename("Marker"=1))$Marker,edges$to)])
tabyl(acca$group) %>% arrange(n)
data.frame(tabyl((Sig_D_C_C %>% filter(str_detect(Nutrition, 'ereal|Muse')))$Marker) %>% arrange(n) )
NOF_CYC <- rbind((data.frame(NAFLD_LRC_SD$Haemoglobin_concentration, NAFLD_LRC_SD$Triglycerides, "Control") %>% rename(Fish = 1, CYC = 2, Group = 3)),
                 (data.frame(NAFLD_LRAF_SD$Haemoglobin_concentration, NAFLD_LRAF_SD$Triglycerides, "After") %>% rename(Fish = 1, CYC = 2, Group = 3)),
                 (data.frame(NAFLD_LRBF_SD$Haemoglobin_concentration, NAFLD_LRBF_SD$Triglycerides, "Before") %>% rename(Fish = 1, CYC = 2, Group = 3)),
                 (data.frame(NAFLD_LR2_SD$Haemoglobin_concentration, NAFLD_LR2_SD$Triglycerides, "Within_2") %>% rename(Fish = 1, CYC = 2, Group = 3)))
NOF_CYC$Group <- ordered(NOF_CYC$Group, levels = c("Control", "Before", "Within_2", "After"))
ggplot(NOF_CYC, aes(Fish, CYC, color = Group, palette = "Set2", fill = Group)) + geom_smooth(method = "lm", se = T, na.rm = FALSE) +
  labs(x = "Haemoglobin_concentration", y = "Triglycerides") + 
  scale_fill_manual(values=mycol4) + scale_color_manual(values=mycol4)

d3 <- data.frame(from="origin", to=paste("group", seq(1,9), sep=""))
d4 <- data.frame( from = c("group1",rep("group3",6),"group4","group4","group3","group4","group4", rep("group1",10), rep("group2",7), rep("group5",39), 
                           "group6", "group9","group7","group6","group7","group6","group9","group7","group6","group7", rep("group9",5),"group8","group9",
                           "group8","group9", rep("group7",3),"group8","group8", rep("group9",3),rep("group8",4),"group9"),
                  to = c(colnames(NAFLD_LR2_SD[c(2:69, 76:107)])) )
edges_1 <- rbind(d3, d4) %>% arrange(from)

set.seed(132)
branches <- Sig_D_B_CC_2 %>% rename("from"="Marker1","to"="Marker2") %>% mutate(value_1 = min(-log(Bonferroni))/-log(Bonferroni)) %>% 
  mutate(value_2 = ifelse(Relationship == "Positive",1,0)  ) %>% select(from, to, Bonferroni, value_1, value_2)
trunk  <-  data.frame( name = unique(c(as.character(edges_1$from), as.character(edges_1$to))) )
trunk$group  <-  edges_1$from[ match( trunk$name, edges_1$to ) ]
trunk[c(11:110),] <- trunk[c(11:110),] %>% arrange(group)
trunk <- trunk %>% mutate(value = 0.1 )
trunk$id <- NA
myleafy <- which(is.na( match(trunk$name, edges_1$from) ))
nleafy <- length(myleafy)
trunk$id[ myleafy ] <- seq(1:nleafy)
trunk$angle <- -30 - 360 * (trunk$id-0.5) / nleafy
trunk$hjust <- ifelse( trunk$angle < -90, 1, 0)
trunk$hjust <- ifelse( trunk$angle < -270, 0, trunk$hjust)
trunk$angle <- ifelse(trunk$angle < -90, trunk$angle+180, trunk$angle)
trunk$angle <- ifelse(trunk$angle < -90, trunk$angle+180, trunk$angle)
trunk

trunk$name   <- gsub("_", " ", trunk$name )
trunk$name   <- gsub(" direct", "-C", trunk$name )
trunk$name <- gsub(" cholesterol", "-C", trunk$name )
trunk$name <- gsub("Apolipoprotein ", "Apo-", trunk$name )
trunk[trunk == "C reactive protein"] <- "CRP"
trunk[trunk == "Aspartate aminotransferase"] <- "AST"
trunk[trunk == "Alanine aminotransferase"] <- "ALT"
trunk[trunk == "Gamma glutamyltransferase"] <- "GGT"
trunk[trunk == "Alkaline phosphatase"] <- "ALP"
trunk[trunk == "Glycated haemoglobin (HbA1c)"] <- "HbA1c"
trunk[trunk == "Cholesterol"] <- "TC"
trunk[trunk == "Triglycerides"] <- "TG"
trunk[trunk == "White blood cell (leukocyte) count"] <- "WBC count"
trunk[trunk == "Mean platelet (thrombocyte) volume"] <- "Mean platelet volume"
trunk[trunk == "Red blood cell (erythrocyte) count"] <- "RBC count"
trunk[trunk == "Red blood cell (erythrocyte) distribution width"] <- "RDW"
trunk[trunk == "Mean corpuscular volume"] <- "MCV"
trunk[trunk == "Red blood cell (erythrocyte) distribution width"] <- "RDW"
trunk[trunk == "Mean corpuscular haemoglobin concentration"] <- "MCHC"
trunk[trunk == "High light scatter reticulocyte count"] <- "HLSRC"
trunk[trunk == "High light scatter reticulocyte percentage"] <- "HLSRP"
trunk[trunk == "Immature reticulocyte fraction"] <- "Immature \nreticulocyte fraction"
trunk[trunk == "  "] <- " "
trunk[trunk == "group1"] <- "1_Immune cells"
trunk[trunk == "group2"] <- "2_Reticulocytes"
trunk[trunk == "group3"] <- "3_RBC"
trunk[trunk == "group4"] <- "4_Platelets"
trunk[trunk == "group5"] <- "5_Diet"
trunk[trunk == "group6"] <- "6_Liver enzymes"
trunk[trunk == "group7"] <- "7_Lipids"
trunk[trunk == "group8"] <- "8_Urine and kidney"
trunk[trunk == "group9"] <- "9_Other biomarkers"


edges_1$to   <- gsub("_", " ", edges_1$to )
edges_1$to <- gsub(" direct", "-C", edges_1$to)
edges_1$to <- gsub(" cholesterol", "-C", edges_1$to)
edges_1$to   <- gsub("Apolipoprotein ", "Apo-", edges_1$to )
edges_1[edges_1 == "C reactive protein"] <- "CRP"
edges_1[edges_1 == "Aspartate aminotransferase"] <- "AST"
edges_1[edges_1 == "Alanine aminotransferase"] <- "ALT"
edges_1[edges_1 == "Gamma glutamyltransferase"] <- "GGT"
edges_1[edges_1 == "Alkaline phosphatase"] <- "ALP"
edges_1[edges_1 == "Glycated haemoglobin (HbA1c)"] <- "HbA1c"
edges_1[edges_1 == "Cholesterol"] <- "TC"
edges_1[edges_1 == "Triglycerides"] <- "TG"
edges_1[edges_1 == "White blood cell (leukocyte) count"] <- "WBC count"
edges_1[edges_1 == "Mean platelet (thrombocyte) volume"] <- "Mean platelet volume"
edges_1[edges_1 == "Mean corpuscular volume"] <- "MCV"
edges_1[edges_1 == "Red blood cell (erythrocyte) count"] <- "RBC count"
edges_1[edges_1 == "Red blood cell (erythrocyte) distribution width"] <- "RDW"
edges_1[edges_1 == "Mean corpuscular haemoglobin concentration"] <- "MCHC"
edges_1[edges_1 == "High light scatter reticulocyte count"] <- "HLSRC"
edges_1[edges_1 == "High light scatter reticulocyte percentage"] <- "HLSRP"
edges_1[edges_1 == "Immature reticulocyte fraction"] <- "Immature \nreticulocyte fraction"
edges_1[edges_1 == "  "] <- " "
edges_1[edges_1 == "group1"] <- "1_Immune cells"
edges_1[edges_1 == "group2"] <- "2_Reticulocytes"
edges_1[edges_1 == "group3"] <- "3_RBC"
edges_1[edges_1 == "group4"] <- "4_Platelets"
edges_1[edges_1 == "group5"] <- "5_Diet"
edges_1[edges_1 == "group6"] <- "6_Liver enzymes"
edges_1[edges_1 == "group7"] <- "7_Lipids"
edges_1[edges_1 == "group8"] <- "8_Urine and kidney"
edges_1[edges_1 == "group9"] <- "9_Other biomarkers"

branches$to   <- gsub("_", " ", branches$to )
branches$to <- gsub(" direct", "-C", branches$to)
branches$to <- gsub(" cholesterol", "-C", branches$to)
branches$to   <- gsub("Apolipoprotein ", "Apo-", branches$to )
branches$from   <- gsub("_", " ", branches$from )
branches$from <- gsub(" direct", "-C", branches$from)
branches$from <- gsub(" cholesterol", "-C", branches$from)
branches$from   <- gsub("Apolipoprotein ", "Apo-", branches$from )
branches[branches == "C reactive protein"] <- "CRP"
branches[branches == "Aspartate aminotransferase"] <- "AST"
branches[branches == "Alanine aminotransferase"] <- "ALT"
branches[branches == "Gamma glutamyltransferase"] <- "GGT"
branches[branches == "Alkaline phosphatase"] <- "ALP"
branches[branches == "Glycated haemoglobin (HbA1c)"] <- "HbA1c"
branches[branches == "Cholesterol"] <- "TC"
branches[branches == "Triglycerides"] <- "TG"
branches[branches == "White blood cell (leukocyte) count"] <- "WBC count"
branches[branches == "Mean platelet (thrombocyte) volume"] <- "Mean platelet volume"
branches[branches == "Mean corpuscular volume"] <- "MCV"
branches[branches == "Red blood cell (erythrocyte) count"] <- "RBC count"
branches[branches == "Red blood cell (erythrocyte) distribution width"] <- "RDW"
branches[branches == "Mean corpuscular haemoglobin concentration"] <- "MCHC"
branches[branches == "High light scatter reticulocyte count"] <- "HLSRC"
branches[branches == "High light scatter reticulocyte percentage"] <- "HLSRP"
branches[branches == "Immature reticulocyte fraction"] <- "Immature \nreticulocyte fraction"
branches[branches == "  "] <- " "
branches$value_3 <- paste(paste('rep'), branches$value_2, paste(',5),'))
branches$value_3   <- gsub("rep", "rep(", branches$value_3)
branches
c(noquote( branches$value_3  ))
relations_1 <- c(rep( 1 ,5), rep( 0 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 0 ,5), 
                  rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 0 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), 
                  rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 0 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), 
                 rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 0 ,5), rep( 0 ,5), 
                 rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5),rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), 
                 rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 0 ,5), 
                 rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), 
                 rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5))
  
mytree <- igraph::graph_from_data_frame( edges_1, vertices=trunk )
(from_1  <-  match( branches$from, trunk$name))
(to_1  <-  match( branches$to, trunk$name))

#Coloured network graph p1
ggraph(mytree, layout = 'dendrogram', circular = TRUE) + 
  geom_conn_bundle(data = get_con(from = from_1, to = to_1), alpha=0.6, width=1.8, tension=0, aes(edge_colour = relations_1)) +
  scale_edge_color_continuous(low="hotpink3", high="lightskyblue") +
  geom_node_text(aes(x = x*1.05, y=y*1.05, filter = leaf, label=name, angle = angle, hjust=hjust, colour=group), size=4.5, alpha=1) +
  geom_node_point(aes(filter = leaf, x = x*1.02, y=y*1.02, colour=group),size=6.75, alpha=0.8) +
  scale_colour_manual(values= rep( brewer.pal(9,"Paired") , 30)) +
  theme_void() +
  theme( legend.position="top", legend.text = element_text(size = 13.1), plot.margin=unit(c(1,1,1,1),"cm"), ) +
  expand_limits(x = c(-1.4, 1.4), y = c(-1.4, 1.4))



#After coloured network
branch_AF <- Sig_D_B_CC_AF %>% rename("from"="Marker1","to"="Marker2") %>% mutate(value_1 = min(-log(Bonferroni))/-log(Bonferroni)) %>% 
  mutate(value_2 = ifelse(Relationship == "Positive",1,0)  ) %>% select(from, to, Bonferroni, value_1, value_2)
branch_AF$to   <- gsub("_", " ", branch_AF$to )
branch_AF$to <- gsub(" direct", "-C", branch_AF$to)
branch_AF$to <- gsub(" cholesterol", "-C", branch_AF$to)
branch_AF$to   <- gsub("Apolipoprotein ", "Apo-", branch_AF$to )
branch_AF$from   <- gsub("_", " ", branch_AF$from )
branch_AF$from <- gsub(" direct", "-C", branch_AF$from)
branch_AF$from <- gsub(" cholesterol", "-C", branch_AF$from)
branch_AF$from   <- gsub("Apolipoprotein ", "Apo-", branch_AF$from )
branch_AF[branch_AF == "C reactive protein"] <- "CRP"
branch_AF[branch_AF == "Aspartate aminotransferase"] <- "AST"
branch_AF[branch_AF == "Alanine aminotransferase"] <- "ALT"
branch_AF[branch_AF == "Gamma glutamyltransferase"] <- "GGT"
branch_AF[branch_AF == "Alkaline phosphatase"] <- "ALP"
branch_AF[branch_AF == "Glycated haemoglobin (HbA1c)"] <- "HbA1c"
branch_AF[branch_AF == "Cholesterol"] <- "TC"
branch_AF[branch_AF == "Triglycerides"] <- "TG"
branch_AF[branch_AF == "White blood cell (leukocyte) count"] <- "WBC count"
branch_AF[branch_AF == "Mean platelet (thrombocyte) volume"] <- "Mean platelet volume"
branch_AF[branch_AF == "Mean corpuscular volume"] <- "MCV"
branch_AF[branch_AF == "Red blood cell (erythrocyte) count"] <- "RBC count"
branch_AF[branch_AF == "Red blood cell (erythrocyte) distribution width"] <- "RDW"
branch_AF[branch_AF == "Mean corpuscular haemoglobin concentration"] <- "MCHC"
branch_AF[branch_AF == "High light scatter reticulocyte count"] <- "HLSRC"
branch_AF[branch_AF == "High light scatter reticulocyte percentage"] <- "HLSRP"
branch_AF[branch_AF == "Immature reticulocyte fraction"] <- "Immature \nreticulocyte fraction"
branch_AF[branch_AF == "  "] <- " "

branch_AF$value_3 <- paste(paste('rep'), branch_AF$value_2, paste(',5),'))
branch_AF$value_3   <- gsub("rep", "rep(", branch_AF$value_3)
branch_AF
noquote(branch_AF$value_3  )
relations_2 <- c(rep( 0 ,5), rep( 0 ,5), rep( 1 ,5), rep( 0 ,5), rep( 0 ,5), rep( 1 ,5), rep( 0 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 0 ,5),
                 rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 0 ,5), rep( 0 ,5), rep( 1 ,5), rep( 0 ,5),
                 rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 0 ,5), rep( 0 ,5), rep( 0 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 0 ,5), rep( 0 ,5),
                rep( 0 ,5), rep( 0 ,5), rep( 0 ,5), rep( 0 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 0 ,5), rep( 0 ,5), rep( 0 ,5),
                 rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 0 ,5), rep( 0 ,5), rep( 1 ,5), rep( 0 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5),
                 rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5),
                 rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 0 ,5), rep( 0 ,5), rep( 0 ,5),
                 rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 0 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5),
                 rep( 1 ,5), rep( 0 ,5), rep( 0 ,5), rep( 0 ,5), rep( 0 ,5), rep( 0 ,5), rep( 0 ,5), rep( 0 ,5), rep( 0 ,5), rep( 0 ,5), rep( 0 ,5), rep( 0 ,5),
                 rep( 0 ,5), rep( 0 ,5), rep( 0 ,5), rep( 0 ,5), rep( 0 ,5), rep( 0 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5),
                 rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5),
                 rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5),
                  rep( 0 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5),
                 rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5),
                 rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5),
                  rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5),
                  rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 0 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5),
                  rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 0 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5),
                 rep( 0 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5),
                 rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5),
                  rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 0 ,5), rep( 0 ,5), rep( 1 ,5), rep( 0 ,5), rep( 0 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5),
                 rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5),
                 rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 0 ,5), rep( 0 ,5),
                 rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 0 ,5), rep( 0 ,5),
                 rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5),
                  rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 0 ,5), rep( 0 ,5), rep( 1 ,5), rep( 0 ,5),
                  rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5),
                 rep( 1 ,5), rep( 0 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5),
                  rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 0 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 0 ,5), rep( 1 ,5),
                 rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 0 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5),
                 rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 0 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5),
                 rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 0 ,5), rep( 0 ,5), rep( 1 ,5),
                 rep( 1 ,5))

(from_2  <-  match( branch_AF$from, trunk$name))
(to_2  <-  match( branch_AF$to, trunk$name))

#Coloured network graph 1400x1400
ggraph(mytree, layout = 'dendrogram', circular = TRUE) + 
  geom_conn_bundle(data = get_con(from = from_2, to = to_2), alpha=0.7, width=1.2, tension=0, aes(edge_colour = relations_2)) +
  scale_edge_color_continuous(low="hotpink3", high="lightskyblue") +
  geom_node_text(aes(x = x*1.04, y=y*1.04, filter = leaf, label=name, angle = angle, hjust=hjust, colour=group), size=4.2, alpha=1) +
  geom_node_point(aes(filter = leaf, x = x*1.02, y=y*1.02, colour=group),size=6.75, alpha=0.8) +
  scale_colour_manual(values= rep( brewer.pal(9,"Paired") , 30)) +
  theme_void() + theme( legend.position="none", plot.margin=unit(c(1,1,1,1),"cm"), ) +
  expand_limits(x = c(-1.3, 1.3), y = c(-1.3, 1.3))




branch_BF <- Sig_D_B_CC_BF %>% rename("from"="Marker1","to"="Marker2") %>% mutate(value_1 = min(-log(Bonferroni))/-log(Bonferroni)) %>% 
  mutate(value_2 = ifelse(Relationship == "Positive",1,0)  ) %>% select(from, to, Bonferroni, value_1, value_2)
branch_BF$to   <- gsub("_", " ", branch_BF$to )
branch_BF$to <- gsub(" direct", "-C", branch_BF$to)
branch_BF$to <- gsub(" cholesterol", "-C", branch_BF$to)
branch_BF$to   <- gsub("Apolipoprotein ", "Apo-", branch_BF$to )
branch_BF$from   <- gsub("_", " ", branch_BF$from )
branch_BF$from <- gsub(" direct", "-C", branch_BF$from)
branch_BF$from <- gsub(" cholesterol", "-C", branch_BF$from)
branch_BF$from   <- gsub("Apolipoprotein ", "Apo-", branch_BF$from )
branch_BF[branch_BF == "C reactive protein"] <- "CRP"
branch_BF[branch_BF == "Aspartate aminotransferase"] <- "AST"
branch_BF[branch_BF == "Alanine aminotransferase"] <- "ALT"
branch_BF[branch_BF == "Gamma glutamyltransferase"] <- "GGT"
branch_BF[branch_BF == "Alkaline phosphatase"] <- "ALP"
branch_BF[branch_BF == "Glycated haemoglobin (HbA1c)"] <- "HbA1c"
branch_BF[branch_BF == "Cholesterol"] <- "TC"
branch_BF[branch_BF == "Triglycerides"] <- "TG"
branch_BF[branch_BF == "White blood cell (leukocyte) count"] <- "WBC count"
branch_BF[branch_BF == "Mean platelet (thrombocyte) volume"] <- "Mean platelet volume"
branch_BF[branch_BF == "Mean corpuscular volume"] <- "MCV"
branch_BF[branch_BF == "Red blood cell (erythrocyte) count"] <- "RBC count"
branch_BF[branch_BF == "Red blood cell (erythrocyte) distribution width"] <- "RDW"
branch_BF[branch_BF == "Mean corpuscular haemoglobin concentration"] <- "MCHC"
branch_BF[branch_BF == "High light scatter reticulocyte count"] <- "HLSRC"
branch_BF[branch_BF == "High light scatter reticulocyte percentage"] <- "HLSRP"
branch_BF[branch_BF == "Immature reticulocyte fraction"] <- "Immature \nreticulocyte fraction"
branch_BF[branch_BF == "  "] <- " "

branch_BF$value_3 <- paste(paste('rep'), branch_BF$value_2, paste(',5),'))
branch_BF$value_3   <- gsub("rep", "rep(", branch_BF$value_3)
branch_BF
#noquote(branch_BF$value_3  )
relations_3 <- c(rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 0 ,5), rep( 0 ,5), rep( 1 ,5), 
                 rep( 0 ,5),rep( 1 ,5), rep( 0 ,5), rep( 0 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), 
                 rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5))

(from_BF  <-  match( branch_BF$from, trunk$name) )
(to_BF  <-  match( branch_BF$to, trunk$name) )

#Coloured Network D2BF 1200x1200
ggraph(mytree, layout = 'dendrogram', circular = TRUE) + 
  geom_conn_bundle(data = get_con(from = from_BF, to = to_BF), alpha=0.6, width=2, tension=0, aes(edge_colour = relations_3 )) +
  scale_edge_color_continuous(low="hotpink3", high="lightskyblue") +
  geom_node_text(aes(x = x*1.05, y=y*1.05, filter = leaf, label=name, angle = angle, hjust=hjust, colour=group), size=4.5, alpha=1) +
  geom_node_point(aes(filter = leaf, x = x*1.02, y=y*1.02, colour=group),size=6.75, alpha=0.8) +
  scale_colour_manual(values= rep( brewer.pal(9,"Paired") , 30)) +
  theme_void() + theme( legend.position="none", plot.margin=unit(c(0.75,0.75,0.75,0.75),"cm"), ) +
  expand_limits(x = c(-1.4, 1.4), y = c(-1.4, 1.4))


branch_C <- Sig_D_B_CC_C %>% rename("from"="Marker1","to"="Marker2") %>% mutate(value_1 = min(-log(Bonferroni))/-log(Bonferroni)) %>% 
  mutate(value_2 = ifelse(Relationship == "Positive",1,0)  ) %>% select(from, to, Bonferroni, value_1, value_2)
branch_C$to   <- gsub("_", " ", branch_C$to )
branch_C$to <- gsub(" direct", "-C", branch_C$to)
branch_C$to <- gsub(" cholesterol", "-C", branch_C$to)
branch_C$to   <- gsub("Apolipoprotein ", "Apo-", branch_C$to )
branch_C$from   <- gsub("_", " ", branch_C$from )
branch_C$from <- gsub(" direct", "-C", branch_C$from)
branch_C$from <- gsub(" cholesterol", "-C", branch_C$from)
branch_C$from   <- gsub("Apolipoprotein ", "Apo-", branch_C$from )
branch_C[branch_C == "C reactive protein"] <- "CRP"
branch_C[branch_C == "Aspartate aminotransferase"] <- "AST"
branch_C[branch_C == "Alanine aminotransferase"] <- "ALT"
branch_C[branch_C == "Gamma glutamyltransferase"] <- "GGT"
branch_C[branch_C == "Alkaline phosphatase"] <- "ALP"
branch_C[branch_C == "Glycated haemoglobin (HbA1c)"] <- "HbA1c"
branch_C[branch_C == "Cholesterol"] <- "TC"
branch_C[branch_C == "Triglycerides"] <- "TG"
branch_C[branch_C == "White blood cell (leukocyte) count"] <- "WBC count"
branch_C[branch_C == "Mean platelet (thrombocyte) volume"] <- "Mean platelet volume"
branch_C[branch_C == "Mean corpuscular volume"] <- "MCV"
branch_C[branch_C == "Red blood cell (erythrocyte) count"] <- "RBC count"
branch_C[branch_C == "Red blood cell (erythrocyte) distribution width"] <- "RDW"
branch_C[branch_C == "Mean corpuscular haemoglobin concentration"] <- "MCHC"
branch_C[branch_C == "High light scatter reticulocyte count"] <- "HLSRC"
branch_C[branch_C == "High light scatter reticulocyte percentage"] <- "HLSRP"
branch_C[branch_C == "Immature reticulocyte fraction"] <- "Immature \nreticulocyte fraction"
branch_C[branch_C == "  "] <- " "

branch_C$value_3 <- paste(paste('rep'), branch_C$value_2, paste(',5),'))
branch_C$value_3   <- gsub("rep", "rep(", branch_C$value_3)
branch_C
noquote(branch_C$value_3  )
relations_3 <- c(rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5),
                 rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5),
                 rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5),
                 rep( 1 ,5), rep( 0 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5),
                 rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5),
                 rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5),
                 rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 0 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5),
                 rep( 0 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5),
                 rep( 1 ,5), rep( 0 ,5), rep( 0 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5),
                 rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 0 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5),
                 rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 0 ,5),
                 rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 0 ,5), rep( 0 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5),
                 rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5),
                 rep( 0 ,5), rep( 0 ,5), rep( 0 ,5), rep( 1 ,5), rep( 0 ,5), rep( 0 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5),
                 rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 0 ,5), rep( 1 ,5), rep( 0 ,5), rep( 0 ,5), rep( 0 ,5), rep( 1 ,5),
                 rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5),
                 rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 0 ,5), rep( 0 ,5), rep( 0 ,5), rep( 1 ,5), rep( 0 ,5),
                 rep( 0 ,5), rep( 1 ,5), rep( 0 ,5), rep( 0 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 0 ,5), rep( 0 ,5), rep( 0 ,5),
                 rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 0 ,5), rep( 1 ,5), rep( 0 ,5),
                 rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 0 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5),
                 rep( 0 ,5), rep( 0 ,5), rep( 1 ,5), rep( 0 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 0 ,5), rep( 1 ,5),
                 rep( 0 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 0 ,5), rep( 1 ,5), rep( 0 ,5), rep( 0 ,5), rep( 1 ,5), rep( 0 ,5),
                 rep( 0 ,5), rep( 0 ,5), rep( 1 ,5), rep( 0 ,5), rep( 0 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5),
                 rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 0 ,5), rep( 1 ,5),
                 rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 0 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5),
                 rep( 1 ,5), rep( 0 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 0 ,5), rep( 0 ,5), rep( 0 ,5), rep( 0 ,5),
                 rep( 0 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5),
                 rep( 0 ,5), rep( 1 ,5), rep( 0 ,5), rep( 0 ,5), rep( 0 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 0 ,5), rep( 0 ,5), rep( 0 ,5), rep( 1 ,5),
                 rep( 0 ,5), rep( 1 ,5), rep( 0 ,5), rep( 0 ,5), rep( 1 ,5), rep( 0 ,5), rep( 0 ,5), rep( 0 ,5), rep( 1 ,5), rep( 0 ,5), rep( 0 ,5), rep( 0 ,5),
                 rep( 0 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 0 ,5), rep( 0 ,5),
                 rep( 1 ,5), rep( 0 ,5), rep( 0 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 0 ,5), rep( 0 ,5), rep( 1 ,5), rep( 0 ,5), rep( 0 ,5), rep( 1 ,5),
                 rep( 0 ,5), rep( 1 ,5), rep( 0 ,5), rep( 0 ,5), rep( 1 ,5), rep( 0 ,5), rep( 0 ,5), rep( 0 ,5), rep( 0 ,5), rep( 0 ,5), rep( 0 ,5), rep( 1 ,5),
                 rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 0 ,5), rep( 0 ,5), rep( 0 ,5),
                 rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 0 ,5), rep( 0 ,5),
                 rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 0 ,5), rep( 0 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 0 ,5),
                 rep( 1 ,5), rep( 0 ,5), rep( 0 ,5), rep( 0 ,5), rep( 1 ,5), rep( 0 ,5), rep( 0 ,5), rep( 0 ,5), rep( 0 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5),
                 rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5),
                  rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 0 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5),
                 rep( 1 ,5), rep( 0 ,5), rep( 0 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5),
                 rep( 0 ,5), rep( 0 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 0 ,5), rep( 0 ,5),
                 rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 0 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5),
                 rep( 0 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5))
(from_C  <-  match( branch_C$from, trunk$name))
(to_C  <-  match( branch_C$to, trunk$name))

#Coloured network graph p1 C
ggraph(mytree, layout = 'dendrogram', circular = TRUE) + 
  geom_conn_bundle(data = get_con(from = from_C, to = to_C), alpha=0.8, width=1, tension=0,
                   aes(edge_colour = relations_3 )) +
  scale_edge_color_continuous(low="hotpink3", high="lightskyblue") +
  #scale_edge_colour_distiller(palette = "BuPu") +
  
  geom_node_text(aes(x = x*1.05, y=y*1.05, filter = leaf, label=name, angle = angle, hjust=hjust, colour=group), size=4.5, alpha=1) +
  
  geom_node_point(aes(filter = leaf, x = x*1.02, y=y*1.02, colour=group),size=6.75, alpha=0.8) +
  scale_colour_manual(values= rep( brewer.pal(9,"Paired") , 30)) +
  
  theme_void() +
  theme(
    legend.position="none", legend.text = element_text(size = 13.1), 
    plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm"),
  ) +
  expand_limits(x = c(-1.3, 1.3), y = c(-1.3, 1.3))
