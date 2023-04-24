
#Inflam BF = 6_Liver_enzymes, 7_Lipids, 8_Urine, 9_Other_Biomarkers(phosphate) (n=5)

d21 <- data.frame(from="origin", to=paste("group", seq(1,5), sep="") )
d22 <- data.frame( from = c(rep("group1",11),"group2","group3","group2","group3","group2","group3","group2",rep("group3",4),rep("group5",2),
                            rep("group4",8)),
                   to = c(colnames(NAFLD_LRBF_SD[c(2,14:23,76,78:81,83:85,95:97,100,92,91,103:106,93,98,99)])) )
(edges_12 <- rbind(d21, d22) %>% arrange(from))
set.seed(132)
(branches_12 <- unique(rbind(Sig_D_B_CC_BF %>% filter(str_detect(Marker1, 'C_|WBC|White_bl|Lymp|Neut|Baso|Mono|Eosin')), 
                            Sig_D_B_CC_BF %>% filter(str_detect(Marker2, 'C_|WBC|White_bl|Lymp|Neut|Baso|Mono|Eosin'))) %>% 
                        rename("from"="Marker1","to"="Marker2") %>% 
                        mutate(value_1 = min(-log(Bonferroni))/-log(Bonferroni)) %>%  mutate(value_BF = ifelse(Relationship == "Positive",1,0)) %>% 
                        select(from, to, Bonferroni, value_1, value_BF)))
trunk_12  <-  data.frame( name = unique(c(as.character(edges_12$from), as.character(edges_12$to))) )
trunk_12$group  <-  edges_12$from[ match( trunk_12$name, edges_12$to ) ]
trunk_12[c(7:38),] <- trunk_12[c(7:38),] %>% arrange(group)
trunk_12 <- trunk_12 %>% mutate(value = 0.1 )
trunk_12$id <- NA
myleafy <- which(is.na( match(trunk_12$name, edges_12$from) ))
nleafy <- length(myleafy)
trunk_12$id[ myleafy ] <- seq(1:nleafy)

trunk_12$name   <- gsub("_", " ", trunk_12$name )
trunk_12$name   <- gsub(" direct", "-C", trunk_12$name )
trunk_12$name <- gsub(" cholesterol", "-C", trunk_12$name )
trunk_12$name <- gsub("Apolipoprotein ", "Apo-", trunk_12$name )
trunk_12[trunk_12 == "C reactive protein"] <- "CRP"
trunk_12[trunk_12 == "Aspartate aminotransferase"] <- "AST"
trunk_12[trunk_12 == "Alanine aminotransferase"] <- "ALT"
trunk_12[trunk_12 == "Gamma glutamyltransferase"] <- "GGT"
trunk_12[trunk_12 == "Alkaline phosphatase"] <- "ALP"
trunk_12[trunk_12 == "Glycated haemoglobin (HbA1c)"] <- "HbA1c"
trunk_12[trunk_12 == "Cholesterol"] <- "TC"
trunk_12[trunk_12 == "Triglycerides"] <- "TG"
trunk_12[trunk_12 == "White blood cell (leukocyte) count"] <- "WBC count"
trunk_12[trunk_12 == "Mean platelet (thrombocyte) volume"] <- "Mean platelet volume"
trunk_12[trunk_12 == "Red blood cell (erythrocyte) count"] <- "RBC count"
trunk_12[trunk_12 == "Red blood cell (erythrocyte) distribution width"] <- "RDW"
trunk_12[trunk_12 == "Mean corpuscular volume"] <- "MCV"
trunk_12[trunk_12 == "Red blood cell (erythrocyte) distribution width"] <- "RDW"
trunk_12[trunk_12 == "Mean corpuscular haemoglobin concentration"] <- "MCHC"
trunk_12[trunk_12 == "High light scatter reticulocyte count"] <- "HLSRC"
trunk_12[trunk_12 == "High light scatter reticulocyte percentage"] <- "HLSRP"
trunk_12[trunk_12 == "Creatinine (enzymatic) in urine"] <- "Creatinine \n(enzymatic) in urine"
trunk_12[trunk_12 == "  "] <- " "
trunk_12[trunk_12 == "group1"] <- "1_Immune cells"
trunk_12[trunk_12 == "group2"] <- "5_Diet"
trunk_12[trunk_12 == "group3"] <- "7_Lipids"
trunk_12[trunk_12 == "group4"] <- "6_Liver enzymes"
trunk_12[trunk_12 == "group6"] <- "8_Urine and kidney"
trunk_12[trunk_12 == "group5"] <- "9_Other biomarkers"
trunk_12
edges_12$to   <- gsub("_", " ", edges_12$to )
edges_12$to <- gsub(" direct", "-C", edges_12$to)
edges_12$to <- gsub(" cholesterol", "-C", edges_12$to)
edges_12$to   <- gsub("Apolipoprotein ", "Apo-", edges_12$to )
edges_12[edges_12 == "C reactive protein"] <- "CRP"
edges_12[edges_12 == "Aspartate aminotransferase"] <- "AST"
edges_12[edges_12 == "Alanine aminotransferase"] <- "ALT"
edges_12[edges_12 == "Gamma glutamyltransferase"] <- "GGT"
edges_12[edges_12 == "Alkaline phosphatase"] <- "ALP"
edges_12[edges_12 == "Glycated haemoglobin (HbA1c)"] <- "HbA1c"
edges_12[edges_12 == "Cholesterol"] <- "TC"
edges_12[edges_12 == "Triglycerides"] <- "TG"
edges_12[edges_12 == "White blood cell (leukocyte) count"] <- "WBC count"
edges_12[edges_12 == "Mean platelet (thrombocyte) volume"] <- "Mean platelet volume"
edges_12[edges_12 == "Mean corpuscular volume"] <- "MCV"
edges_12[edges_12 == "Red blood cell (erythrocyte) count"] <- "RBC count"
edges_12[edges_12 == "Red blood cell (erythrocyte) distribution width"] <- "RDW"
edges_12[edges_12 == "Mean corpuscular haemoglobin concentration"] <- "MCHC"
edges_12[edges_12 == "High light scatter reticulocyte count"] <- "HLSRC"
edges_12[edges_12 == "High light scatter reticulocyte percentage"] <- "HLSRP"
edges_12[edges_12 == "Creatinine (enzymatic) in urine"] <- "Creatinine \n(enzymatic) in urine"
edges_12[edges_12 == "  "] <- " "
edges_12[edges_12 == "group1"] <- "1_Immune cells"
edges_12[edges_12 == "group2"] <- "5_Diet"
edges_12[edges_12 == "group3"] <- "7_Lipids"
edges_12[edges_12 == "group4"] <- "6_Liver enzymes"
edges_12[edges_12 == "group6"] <- "8_Urine and kidney"
edges_12[edges_12 == "group5"] <- "9_Other biomarkers"

branches_12$to   <- gsub("_", " ", branches_12$to )
branches_12$to <- gsub(" direct", "-C", branches_12$to)
branches_12$to <- gsub(" cholesterol", "-C", branches_12$to)
branches_12$to   <- gsub("Apolipoprotein ", "Apo-", branches_12$to )
branches_12$from   <- gsub("_", " ", branches_12$from )
branches_12$from <- gsub(" direct", "-C", branches_12$from)
branches_12$from <- gsub(" cholesterol", "-C", branches_12$from)
branches_12$from   <- gsub("Apolipoprotein ", "Apo-", branches_12$from )
branches_12[branches_12 == "C reactive protein"] <- "CRP"
branches_12[branches_12 == "Creatinine (enzymatic) in urine"] <- "Creatinine \n(enzymatic) in urine"
branches_12[branches_12 == "Aspartate aminotransferase"] <- "AST"
branches_12[branches_12 == "Alanine aminotransferase"] <- "ALT"
branches_12[branches_12 == "Gamma glutamyltransferase"] <- "GGT"
branches_12[branches_12 == "Alkaline phosphatase"] <- "ALP"
branches_12[branches_12 == "Glycated haemoglobin (HbA1c)"] <- "HbA1c"
branches_12[branches_12 == "Cholesterol"] <- "TC"
branches_12[branches_12 == "Triglycerides"] <- "TG"
branches_12[branches_12 == "White blood cell (leukocyte) count"] <- "WBC count"
branches_12[branches_12 == "Mean platelet (thrombocyte) volume"] <- "Mean platelet volume"
branches_12[branches_12 == "Mean corpuscular volume"] <- "MCV"
branches_12[branches_12 == "Red blood cell (erythrocyte) count"] <- "RBC count"
branches_12[branches_12 == "Red blood cell (erythrocyte) distribution width"] <- "RDW"
branches_12[branches_12 == "Mean corpuscular haemoglobin concentration"] <- "MCHC"
branches_12[branches_12 == "High light scatter reticulocyte count"] <- "HLSRC"
branches_12[branches_12 == "High light scatter reticulocyte percentage"] <- "HLSRP"
branches_12[branches_12 == "High light scatter reticulocyte percentage"] <- "HLSRP"
branches_12[branches_12 == "  "] <- " "

trunk_12$angle <- 30 - 360 * (trunk_12$id-0.5) / nleafy
trunk_12$hjust <- ifelse( trunk_12$angle < -90, 1, 0)
#trunk_12$hjust <- ifelse( trunk_12$angle < -270, 0, trunk_12$hjust)
trunk_12$angle <- ifelse(trunk_12$angle < -90, trunk_12$angle+180, trunk_12$angle)
trunk_12$angle <- ifelse(trunk_12$angle < -90, trunk_12$angle+180, trunk_12$angle)
trunk_12[c(34:38),]$hjust <- 0
trunk_12
edges_12
mytree_12 <- igraph::graph_from_data_frame( edges_12, vertices=trunk_12 )
from_12  <-  match( branches_12$from, trunk_12$name)
to_12  <-  match( branches_12$to, trunk_12$name)
branches_12$value_BF_2 <- paste(paste('rep'), branches_12$value_BF, paste(',5),'))
branches_12$value_BF_2   <- gsub("rep", "rep(", branches_12$value_BF_2)
branches_12
noquote( branches_12$value_BF_2  )
relations_12 <- c(rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 0 ,5), rep( 1 ,5))
ggraph(mytree_12, layout = 'dendrogram', circular = TRUE) + 
  geom_conn_bundle(data = get_con(from = from_12, to = to_12), alpha=0.5, width=2.2, tension=0, aes(edge_colour =  relations_12) ) +
  scale_edge_color_continuous(low="hotpink3", high="lightskyblue") +
  geom_node_text(aes(x = x*1.1, y=y*1.1, filter = leaf, label=name, angle = angle, hjust=hjust, colour=group), size=5.1, alpha=1) +
  geom_node_point(aes(filter = leaf, x = x*1.03, y=y*1.03, colour=group),size=6.5, alpha=0.85) +
  scale_colour_manual(values= rep( c("#A6CEE3", "#E31A1C" ,"#FF7F00" ,"#FDBF6F" ,"#CAB2D6") , 30)) +
  theme_void() + theme( legend.position="none")  +
  expand_limits(x = c(-2, 2), y = c(-2, 2))




#Inflam W2 = 6_Liver_enzymes, 9_Other_Biomarkers (n=7)

d23 <- data.frame(from="origin", to=paste("group", seq(1,4), sep="") )
d24 <- data.frame( from = c(rep("group1",11),rep("group2",4),rep("group4",1), rep("group3",8)),
                   to = c(colnames(NAFLD_LR2_SD[c(2,14:23,76,79,81,84,92,91,103:106,93,98,99)])) )
(edges_13 <- rbind(d23, d24) %>% arrange(from))
set.seed(132)
(branches_13 <- unique(rbind(Sig_D_B_CC_2 %>% filter(str_detect(Marker1, 'C_|WBC|White_bl|Lymp|Neut|Baso|Mono|Eosin')), 
                             Sig_D_B_CC_2 %>% filter(str_detect(Marker2, 'WBC|White_bl|Lymp|Neut|Baso|Mono|Eosin'))) %>% 
                         rename("from"="Marker1","to"="Marker2") %>% 
                         mutate(value_1 = min(-log(Bonferroni))/-log(Bonferroni)) %>%  mutate(value_2 = ifelse(Relationship == "Positive",1,0)) %>% 
                         select(from, to, Bonferroni, value_1, value_2)))
trunk_13  <-  data.frame( name = unique(c(as.character(edges_13$from), as.character(edges_13$to))) )
trunk_13$group  <-  edges_13$from[ match( trunk_13$name, edges_13$to ) ]
trunk_13[c(6:29),] <- trunk_13[c(6:29),] %>% arrange(group)
trunk_13 <- trunk_13 %>% mutate(value = 0.1 )
trunk_13$id <- NA
myleafy <- which(is.na( match(trunk_13$name, edges_13$from) ))
nleafy <- length(myleafy)
trunk_13$id[ myleafy ] <- seq(1:nleafy)

trunk_13$name   <- gsub("_", " ", trunk_13$name )
trunk_13$name   <- gsub(" direct", "-C", trunk_13$name )
trunk_13$name <- gsub(" cholesterol", "-C", trunk_13$name )
trunk_13$name <- gsub("Apolipoprotein ", "Apo-", trunk_13$name )
trunk_13[trunk_13 == "C reactive protein"] <- "CRP"
trunk_13[trunk_13 == "Aspartate aminotransferase"] <- "AST"
trunk_13[trunk_13 == "Alanine aminotransferase"] <- "ALT"
trunk_13[trunk_13 == "Gamma glutamyltransferase"] <- "GGT"
trunk_13[trunk_13 == "Alkaline phosphatase"] <- "ALP"
trunk_13[trunk_13 == "Glycated haemoglobin (HbA1c)"] <- "HbA1c"
trunk_13[trunk_13 == "Cholesterol"] <- "TC"
trunk_13[trunk_13 == "Triglycerides"] <- "TG"
trunk_13[trunk_13 == "White blood cell (leukocyte) count"] <- "WBC count"
trunk_13[trunk_13 == "Mean platelet (thrombocyte) volume"] <- "Mean platelet volume"
trunk_13[trunk_13 == "Red blood cell (erythrocyte) count"] <- "RBC count"
trunk_13[trunk_13 == "Red blood cell (erythrocyte) distribution width"] <- "RDW"
trunk_13[trunk_13 == "Mean corpuscular volume"] <- "MCV"
trunk_13[trunk_13 == "Red blood cell (erythrocyte) distribution width"] <- "RDW"
trunk_13[trunk_13 == "Mean corpuscular haemoglobin concentration"] <- "MCHC"
trunk_13[trunk_13 == "High light scatter reticulocyte count"] <- "HLSRC"
trunk_13[trunk_13 == "High light scatter reticulocyte percentage"] <- "HLSRP"
trunk_13[trunk_13 == "Creatinine (enzymatic) in urine"] <- "Creatinine \n(enzymatic) in urine"
trunk_13[trunk_13 == "  "] <- " "
trunk_13[trunk_13 == "group1"] <- "1_Immune cells"
trunk_13[trunk_13 == "group2"] <- "5_Diet"
trunk_13[trunk_13 == "group3"] <- "7_Lipids"
trunk_13[trunk_13 == "group4"] <- "6_Liver enzymes"
trunk_13[trunk_13 == "group6"] <- "8_Urine and kidney"
trunk_13[trunk_13 == "group5"] <- "9_Other biomarkers"
trunk_13
edges_13$to   <- gsub("_", " ", edges_13$to )
edges_13$to <- gsub(" direct", "-C", edges_13$to)
edges_13$to <- gsub(" cholesterol", "-C", edges_13$to)
edges_13$to   <- gsub("Apolipoprotein ", "Apo-", edges_13$to )
edges_13[edges_13 == "C reactive protein"] <- "CRP"
edges_13[edges_13 == "Aspartate aminotransferase"] <- "AST"
edges_13[edges_13 == "Alanine aminotransferase"] <- "ALT"
edges_13[edges_13 == "Gamma glutamyltransferase"] <- "GGT"
edges_13[edges_13 == "Alkaline phosphatase"] <- "ALP"
edges_13[edges_13 == "Glycated haemoglobin (HbA1c)"] <- "HbA1c"
edges_13[edges_13 == "Cholesterol"] <- "TC"
edges_13[edges_13 == "Triglycerides"] <- "TG"
edges_13[edges_13 == "White blood cell (leukocyte) count"] <- "WBC count"
edges_13[edges_13 == "Mean platelet (thrombocyte) volume"] <- "Mean platelet volume"
edges_13[edges_13 == "Mean corpuscular volume"] <- "MCV"
edges_13[edges_13 == "Red blood cell (erythrocyte) count"] <- "RBC count"
edges_13[edges_13 == "Red blood cell (erythrocyte) distribution width"] <- "RDW"
edges_13[edges_13 == "Mean corpuscular haemoglobin concentration"] <- "MCHC"
edges_13[edges_13 == "High light scatter reticulocyte count"] <- "HLSRC"
edges_13[edges_13 == "High light scatter reticulocyte percentage"] <- "HLSRP"
edges_13[edges_13 == "Creatinine (enzymatic) in urine"] <- "Creatinine \n(enzymatic) in urine"
edges_13[edges_13 == "  "] <- " "
edges_13[edges_13 == "group1"] <- "1_Immune cells"
edges_13[edges_13 == "group2"] <- "5_Diet"
edges_13[edges_13 == "group3"] <- "7_Lipids"
edges_13[edges_13 == "group4"] <- "6_Liver enzymes"
edges_13[edges_13 == "group6"] <- "8_Urine and kidney"
edges_13[edges_13 == "group5"] <- "9_Other biomarkers"

branches_13$to   <- gsub("_", " ", branches_13$to )
branches_13$to <- gsub(" direct", "-C", branches_13$to)
branches_13$to <- gsub(" cholesterol", "-C", branches_13$to)
branches_13$to   <- gsub("Apolipoprotein ", "Apo-", branches_13$to )
branches_13$from   <- gsub("_", " ", branches_13$from )
branches_13$from <- gsub(" direct", "-C", branches_13$from)
branches_13$from <- gsub(" cholesterol", "-C", branches_13$from)
branches_13$from   <- gsub("Apolipoprotein ", "Apo-", branches_13$from )
branches_13[branches_13 == "C reactive protein"] <- "CRP"
branches_13[branches_13 == "Creatinine (enzymatic) in urine"] <- "Creatinine \n(enzymatic) in urine"
branches_13[branches_13 == "Aspartate aminotransferase"] <- "AST"
branches_13[branches_13 == "Alanine aminotransferase"] <- "ALT"
branches_13[branches_13 == "Gamma glutamyltransferase"] <- "GGT"
branches_13[branches_13 == "Alkaline phosphatase"] <- "ALP"
branches_13[branches_13 == "Glycated haemoglobin (HbA1c)"] <- "HbA1c"
branches_13[branches_13 == "Cholesterol"] <- "TC"
branches_13[branches_13 == "Triglycerides"] <- "TG"
branches_13[branches_13 == "White blood cell (leukocyte) count"] <- "WBC count"
branches_13[branches_13 == "Mean platelet (thrombocyte) volume"] <- "Mean platelet volume"
branches_13[branches_13 == "Mean corpuscular volume"] <- "MCV"
branches_13[branches_13 == "Red blood cell (erythrocyte) count"] <- "RBC count"
branches_13[branches_13 == "Red blood cell (erythrocyte) distribution width"] <- "RDW"
branches_13[branches_13 == "Mean corpuscular haemoglobin concentration"] <- "MCHC"
branches_13[branches_13 == "High light scatter reticulocyte count"] <- "HLSRC"
branches_13[branches_13 == "High light scatter reticulocyte percentage"] <- "HLSRP"
branches_13[branches_13 == "High light scatter reticulocyte percentage"] <- "HLSRP"
branches_13[branches_13 == "  "] <- " "

trunk_13$angle <- -60 - 360 * (trunk_13$id-0.5) / nleafy
trunk_13$hjust <- ifelse( trunk_13$angle < -90, 1, 0)
#trunk_13$hjust <- ifelse( trunk_13$angle < -270, 0, trunk_13$hjust)
trunk_13$angle <- ifelse(trunk_13$angle < -90, trunk_13$angle+180, trunk_13$angle)
trunk_13$angle <- ifelse(trunk_13$angle < -90, trunk_13$angle+180, trunk_13$angle)
trunk_13[c(20:29),]$hjust <- 0
trunk_13
edges_13
mytree_13 <- igraph::graph_from_data_frame( edges_13, vertices=trunk_13 )
from_13  <-  match( branches_13$from, trunk_13$name)
to_13  <-  match( branches_13$to, trunk_13$name)
branches_13$value_2_2 <- paste(paste('rep'), branches_13$value_2, paste(',5),'))
branches_13$value_2_2   <- gsub("rep", "rep(", branches_13$value_2_2)
branches_13
noquote( branches_13$value_2_2  )
relations_13 <- c(rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5))
ggraph(mytree_13, layout = 'dendrogram', circular = TRUE) + 
  geom_conn_bundle(data = get_con(from = from_13, to = to_13), alpha=0.5, width=2.2, tension=0, aes(edge_colour =  relations_13) ) +
  scale_edge_color_continuous(low="hotpink3", high="lightskyblue") +
  geom_node_text(aes(x = x*1.1, y=y*1.1, filter = leaf, label=name, angle = angle, hjust=hjust, colour=group), size=5.1, alpha=1) +
  geom_node_point(aes(filter = leaf, x = x*1.03, y=y*1.03, colour=group),size=6.5, alpha=0.85) +
  scale_colour_manual(values= rep( c("#A6CEE3", "#E31A1C" ,"#CAB2D6", "#FF7F00" ) , 30)) +
  theme_void() + theme( legend.position="none")  +
  expand_limits(x = c(-2, 2), y = c(-2, 2))





#Inflam C = 5_Diet(cereal,cook v, fresh f, coffee,lamb, p meat, salad,salt,bread), 6_Liver_enzymes, 7_Lipids, 8_Urine, 
#9_Other_Biomarkers(CRP, vit d, t port, t bili, testo, shbg, phosp, igf, hba1c, glucos, calc, alb) (n=117) (n=124)


d25 <- data.frame(from="origin", to=paste("group", seq(1,6), sep="") )
d26 <- data.frame( from = c(rep("group1",11),rep("group2",23),"group4","group3","group5","group4","group5","group4","group3","group5","group4",
                            "group5",rep("group3",5),"group6","group3","group6","group3",rep("group5",3),rep("group6",2),rep("group3",3),
                            rep("group6",4)),
                   to = c(colnames(NAFLD_LRC_SD[c(2,14:23,31:33,35,59,40:44,34,60,50:53,54:57,65,67,69,76:106)])) )
(edges_14 <- rbind(d25, d26) %>% arrange(from))
set.seed(132)
(branches_14 <- unique(rbind(Sig_D_B_CC_C %>% filter(str_detect(Marker1, 'C_|WBC|White_bl|Lymp|Neut|Baso|Mono|Eosin')), 
                             Sig_D_B_CC_C %>% filter(str_detect(Marker2, 'WBC|White_bl|Lymp|Neut|Baso|Mono|Eosin'))) %>% 
                         rename("from"="Marker1","to"="Marker2") %>% 
                         mutate(value_1 = min(-log(Bonferroni))/-log(Bonferroni)) %>%  mutate(value_C = ifelse(Relationship == "Positive",1,0)) %>% 
                         select(from, to, Bonferroni, value_1, value_C)))
trunk_14  <-  data.frame( name = unique(c(as.character(edges_14$from), as.character(edges_14$to))) )
trunk_14$group  <-  edges_14$from[ match( trunk_14$name, edges_14$to ) ]
trunk_14 <- trunk_14 %>% mutate(value = 0.1 )
trunk_14$id <- NA
myleafy <- which(is.na( match(trunk_14$name, edges_14$from) ))
nleafy <- length(myleafy)
trunk_14$id[ myleafy ] <- seq(1:nleafy)

trunk_14$name   <- gsub("_", " ", trunk_14$name )
trunk_14$name   <- gsub(" direct", "-C", trunk_14$name )
trunk_14$name <- gsub(" cholesterol", "-C", trunk_14$name )
trunk_14$name <- gsub("Apolipoprotein ", "Apo-", trunk_14$name )
trunk_14[trunk_14 == "C reactive protein"] <- "CRP"
trunk_14[trunk_14 == "Aspartate aminotransferase"] <- "AST"
trunk_14[trunk_14 == "Alanine aminotransferase"] <- "ALT"
trunk_14[trunk_14 == "Gamma glutamyltransferase"] <- "GGT"
trunk_14[trunk_14 == "Alkaline phosphatase"] <- "ALP"
trunk_14[trunk_14 == "Glycated haemoglobin (HbA1c)"] <- "HbA1c"
trunk_14[trunk_14 == "Cholesterol"] <- "TC"
trunk_14[trunk_14 == "Triglycerides"] <- "TG"
trunk_14[trunk_14 == "White blood cell (leukocyte) count"] <- "WBC count"
trunk_14[trunk_14 == "Mean platelet (thrombocyte) volume"] <- "Mean platelet volume"
trunk_14[trunk_14 == "Red blood cell (erythrocyte) count"] <- "RBC count"
trunk_14[trunk_14 == "Red blood cell (erythrocyte) distribution width"] <- "RDW"
trunk_14[trunk_14 == "Mean corpuscular volume"] <- "MCV"
trunk_14[trunk_14 == "Red blood cell (erythrocyte) distribution width"] <- "RDW"
trunk_14[trunk_14 == "Mean corpuscular haemoglobin concentration"] <- "MCHC"
trunk_14[trunk_14 == "High light scatter reticulocyte count"] <- "HLSRC"
trunk_14[trunk_14 == "High light scatter reticulocyte percentage"] <- "HLSRP"
trunk_14[trunk_14 == "Creatinine (enzymatic) in urine"] <- "Creatinine \n(enzymatic) in urine"
trunk_14[trunk_14 == "  "] <- " "
trunk_14[trunk_14 == "group1"] <- "1_Immune cells"
trunk_14[trunk_14 == "group2"] <- "5_Diet"
trunk_14[trunk_14 == "group3"] <- "7_Lipids"
trunk_14[trunk_14 == "group4"] <- "6_Liver enzymes"
trunk_14[trunk_14 == "group6"] <- "8_Urine and kidney"
trunk_14[trunk_14 == "group5"] <- "9_Other biomarkers"
trunk_14
edges_14$to   <- gsub("_", " ", edges_14$to )
edges_14$to <- gsub(" direct", "-C", edges_14$to)
edges_14$to <- gsub(" cholesterol", "-C", edges_14$to)
edges_14$to   <- gsub("Apolipoprotein ", "Apo-", edges_14$to )
edges_14[edges_14 == "C reactive protein"] <- "CRP"
edges_14[edges_14 == "Aspartate aminotransferase"] <- "AST"
edges_14[edges_14 == "Alanine aminotransferase"] <- "ALT"
edges_14[edges_14 == "Gamma glutamyltransferase"] <- "GGT"
edges_14[edges_14 == "Alkaline phosphatase"] <- "ALP"
edges_14[edges_14 == "Glycated haemoglobin (HbA1c)"] <- "HbA1c"
edges_14[edges_14 == "Cholesterol"] <- "TC"
edges_14[edges_14 == "Triglycerides"] <- "TG"
edges_14[edges_14 == "White blood cell (leukocyte) count"] <- "WBC count"
edges_14[edges_14 == "Mean platelet (thrombocyte) volume"] <- "Mean platelet volume"
edges_14[edges_14 == "Mean corpuscular volume"] <- "MCV"
edges_14[edges_14 == "Red blood cell (erythrocyte) count"] <- "RBC count"
edges_14[edges_14 == "Red blood cell (erythrocyte) distribution width"] <- "RDW"
edges_14[edges_14 == "Mean corpuscular haemoglobin concentration"] <- "MCHC"
edges_14[edges_14 == "High light scatter reticulocyte count"] <- "HLSRC"
edges_14[edges_14 == "High light scatter reticulocyte percentage"] <- "HLSRP"
edges_14[edges_14 == "Creatinine (enzymatic) in urine"] <- "Creatinine \n(enzymatic) in urine"
edges_14[edges_14 == "  "] <- " "
edges_14[edges_14 == "group1"] <- "1_Immune cells"
edges_14[edges_14 == "group2"] <- "5_Diet"
edges_14[edges_14 == "group3"] <- "7_Lipids"
edges_14[edges_14 == "group4"] <- "6_Liver enzymes"
edges_14[edges_14 == "group6"] <- "8_Urine and kidney"
edges_14[edges_14 == "group5"] <- "9_Other biomarkers"

branches_14$to   <- gsub("_", " ", branches_14$to )
branches_14$to <- gsub(" direct", "-C", branches_14$to)
branches_14$to <- gsub(" cholesterol", "-C", branches_14$to)
branches_14$to   <- gsub("Apolipoprotein ", "Apo-", branches_14$to )
branches_14$from   <- gsub("_", " ", branches_14$from )
branches_14$from <- gsub(" direct", "-C", branches_14$from)
branches_14$from <- gsub(" cholesterol", "-C", branches_14$from)
branches_14$from   <- gsub("Apolipoprotein ", "Apo-", branches_14$from )
branches_14[branches_14 == "C reactive protein"] <- "CRP"
branches_14[branches_14 == "Creatinine (enzymatic) in urine"] <- "Creatinine \n(enzymatic) in urine"
branches_14[branches_14 == "Aspartate aminotransferase"] <- "AST"
branches_14[branches_14 == "Alanine aminotransferase"] <- "ALT"
branches_14[branches_14 == "Gamma glutamyltransferase"] <- "GGT"
branches_14[branches_14 == "Alkaline phosphatase"] <- "ALP"
branches_14[branches_14 == "Glycated haemoglobin (HbA1c)"] <- "HbA1c"
branches_14[branches_14 == "Cholesterol"] <- "TC"
branches_14[branches_14 == "Triglycerides"] <- "TG"
branches_14[branches_14 == "White blood cell (leukocyte) count"] <- "WBC count"
branches_14[branches_14 == "Mean platelet (thrombocyte) volume"] <- "Mean platelet volume"
branches_14[branches_14 == "Mean corpuscular volume"] <- "MCV"
branches_14[branches_14 == "Red blood cell (erythrocyte) count"] <- "RBC count"
branches_14[branches_14 == "Red blood cell (erythrocyte) distribution width"] <- "RDW"
branches_14[branches_14 == "Mean corpuscular haemoglobin concentration"] <- "MCHC"
branches_14[branches_14 == "High light scatter reticulocyte count"] <- "HLSRC"
branches_14[branches_14 == "High light scatter reticulocyte percentage"] <- "HLSRP"
branches_14[branches_14 == "High light scatter reticulocyte percentage"] <- "HLSRP"
branches_14[branches_14 == "  "] <- " "
branches_14$value_C_2 <- paste(paste('rep'), branches_14$value_C, paste(',5),'))
branches_14$value_C_2   <- gsub("rep", "rep(", branches_14$value_C_2)
branches_14
noquote( branches_14$value_C_2  )
relations_14 <- c(rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5),
                  rep( 0 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5),
                  rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5),
                  rep( 1 ,5), rep( 0 ,5), rep( 0 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 0 ,5), rep( 0 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5),
                  rep( 0 ,5), rep( 1 ,5), rep( 0 ,5), rep( 0 ,5), rep( 1 ,5), rep( 0 ,5), rep( 0 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5),
                  rep( 0 ,5), rep( 1 ,5), rep( 0 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5),
                  rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5),
                  rep( 1 ,5), rep( 0 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 0 ,5), rep( 0 ,5), rep( 0 ,5),
                  rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 0 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 0 ,5), rep( 0 ,5),
                  rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 0 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5),
                  rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5))

trunk_14$angle <- -60 - 360 * (trunk_14$id-0.5) / nleafy
trunk_14$hjust <- ifelse( trunk_14$angle < -90, 1, 0)
#trunk_14$hjust <- ifelse( trunk_14$angle < -270, 0, trunk_14$hjust)
trunk_14$angle <- ifelse(trunk_14$angle < -90, trunk_14$angle+180, trunk_14$angle)
trunk_14$angle <- ifelse(trunk_14$angle < -90, trunk_14$angle+180, trunk_14$angle)
trunk_14[c(13:45),]$hjust <- 0
trunk_14[c(8:12),]$hjust <- 1
trunk_14
edges_14
mytree_14 <- igraph::graph_from_data_frame( edges_14, vertices=trunk_14 )
from_14  <-  match( branches_14$from, trunk_14$name)
to_14  <-  match( branches_14$to, trunk_14$name)

ggraph(mytree_14, layout = 'dendrogram', circular = TRUE) + 
  geom_conn_bundle(data = get_con(from = from_14, to = to_14), alpha=0.45, width=1.1, tension=0, aes(edge_colour =  relations_14) ) +
  scale_edge_color_continuous(low="hotpink3", high="lightskyblue") +
  geom_node_text(aes(x = x*1.05, y=y*1.05, filter = leaf, label=name, angle = angle, hjust=hjust, colour=group), size=5.1, alpha=1) +
  geom_node_point(aes(filter = leaf, x = x*1.02, y=y*1.02, colour=group),size=8, alpha=0.85) +
  scale_colour_manual(values= rep( c("#A6CEE3", "#FB9A99" ,"#E31A1C", "#CAB2D6",   "#FF7F00", "#FDBF6F" ) , 30)) +
  theme_void() + theme( legend.position="none")  +
  expand_limits(x = c(-1.3, 1.3), y = c(-1.3, 1.3))



