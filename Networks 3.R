
#Lipids W2 = 2_Reticulocytes, 3_RBC, 4_Platelet (n=20)
d5 <- data.frame(from="origin", to=paste("group", seq(1,4), sep=""))
d6 <- data.frame( from = c(rep("group1",6),rep("group3",2),"group1",rep("group3",2),rep("group2",7),rep("group4",7)),
                  to = c(colnames(NAFLD_LR2_SD[c(3:13,24:30,78,80,83,85,95,96,97)])) )
(edges_3 <- rbind(d5, d6) %>% arrange(from))
set.seed(132)
branches_3 <- Sig_D_B_CC_2 %>% rename("from"="Marker1","to"="Marker2") %>% mutate(value_1 = min(-log(Bonferroni))/-log(Bonferroni)) %>% 
  mutate(value_2 = ifelse(Relationship == "Positive",1,0)  ) %>% filter(str_detect(to, 'LDL|HDL|Chol|Trig|Apo|Lipo')) %>% 
  select(from, to, Bonferroni, value_1, value_2)
trunk_3  <-  data.frame( name = unique(c(as.character(edges_3$from), as.character(edges_3$to))) )
trunk_3$group  <-  edges_3$from[ match( trunk_3$name, edges_3$to ) ]
trunk_3[c(6:30),] <- trunk_3[c(6:30),] %>% arrange(group)
trunk_3 <- trunk_3 %>% mutate(value = runif(30) )
trunk_3$id <- NA
myleafy <- which(is.na( match(trunk_3$name, edges_3$from) ))
nleafy <- length(myleafy)
trunk_3$id[ myleafy ] <- seq(1:nleafy)
trunk_3$angle <- -15 - 360 * (trunk_3$id-0.5) / nleafy
trunk_3$hjust <- ifelse( trunk_3$angle < -90, 1, 0)
trunk_3$hjust <- ifelse( trunk_3$angle < -270, 0, trunk_3$hjust)
trunk_3$angle <- ifelse(trunk_3$angle < -90, trunk_3$angle+180, trunk_3$angle)
trunk_3$angle <- ifelse(trunk_3$angle < -90, trunk_3$angle+180, trunk_3$angle)
trunk_3

trunk_3$name   <- gsub("_", " ", trunk_3$name )
trunk_3$name   <- gsub(" direct", "-C", trunk_3$name )
trunk_3$name <- gsub(" cholesterol", "-C", trunk_3$name )
trunk_3$name <- gsub("Apolipoprotein ", "Apo-", trunk_3$name )
trunk_3[trunk_3 == "Cholesterol"] <- "TC"
trunk_3[trunk_3 == "Triglycerides"] <- "TG"
trunk_3[trunk_3 == "Mean platelet (thrombocyte) volume"] <- "Mean platelet volume"
trunk_3[trunk_3 == "Red blood cell (erythrocyte) count"] <- "RBC count"
trunk_3[trunk_3 == "Red blood cell (erythrocyte) distribution width"] <- "RDW"
trunk_3[trunk_3 == "Mean corpuscular volume"] <- "MCV"
trunk_3[trunk_3 == "Red blood cell (erythrocyte) distribution width"] <- "RDW"
trunk_3[trunk_3 == "Mean corpuscular haemoglobin concentration"] <- "MCHC"
trunk_3[trunk_3 == "High light scatter reticulocyte count"] <- "HLSRC"
trunk_3[trunk_3 == "High light scatter reticulocyte percentage"] <- "HLSRP"
trunk_3[trunk_3 == "Platelet distribution width"] <- "Platelet \n distribution width"
trunk_3$name   <- gsub("cyte ", "cyte \n", trunk_3$name )
trunk_3$name   <- gsub(" conc", "\n  conc", trunk_3$name )
trunk_3$name   <- gsub(" haem", "\n  haem", trunk_3$name )
trunk_3$name   <- gsub(" cell", "\n  cell", trunk_3$name )
trunk_3$name   <- gsub("ature ", "ature \n", trunk_3$name )
trunk_3[trunk_3 == "  "] <- " "
trunk_3[trunk_3 == "group2"] <- "2_Reticulocytes"
trunk_3[trunk_3 == "group1"] <- "3_RBC"
trunk_3[trunk_3 == "group3"] <- "4_Platelets"
trunk_3[trunk_3 == "group4"] <- "7_Lipids"
trunk_3

edges_3$to   <- gsub("_", " ", edges_3$to )
edges_3$to <- gsub(" direct", "-C", edges_3$to)
edges_3$to <- gsub(" cholesterol", "-C", edges_3$to)
edges_3$to   <- gsub("Apolipoprotein ", "Apo-", edges_3$to )
edges_3[edges_3 == "Cholesterol"] <- "TC"
edges_3[edges_3 == "Triglycerides"] <- "TG"
edges_3[edges_3 == "White blood cell (leukocyte) count"] <- "WBC count"
edges_3[edges_3 == "Mean platelet (thrombocyte) volume"] <- "Mean platelet volume"
edges_3[edges_3 == "Mean corpuscular volume"] <- "MCV"
edges_3[edges_3 == "Red blood cell (erythrocyte) count"] <- "RBC count"
edges_3[edges_3 == "Red blood cell (erythrocyte) distribution width"] <- "RDW"
edges_3[edges_3 == "Mean corpuscular haemoglobin concentration"] <- "MCHC"
edges_3[edges_3 == "High light scatter reticulocyte count"] <- "HLSRC"
edges_3[edges_3 == "High light scatter reticulocyte percentage"] <- "HLSRP"
edges_3[edges_3 == "Platelet distribution width"] <- "Platelet \n distribution width"
edges_3$to   <- gsub("cyte ", "cyte \n", edges_3$to )
edges_3$to   <- gsub(" haem", "\n  haem", edges_3$to )
edges_3$to   <- gsub(" conc", "\n  conc", edges_3$to )
edges_3$to   <- gsub(" cell", "\n  cell", edges_3$to )
edges_3$to   <- gsub("ature ", "ature \n", edges_3$to )
edges_3[edges_3 == "  "] <- " "
edges_3[edges_3 == "group2"] <- "2_Reticulocytes"
edges_3[edges_3 == "group1"] <- "3_RBC"
edges_3[edges_3 == "group3"] <- "4_Platelets"
edges_3[edges_3 == "group4"] <- "7_Lipids"
edges_3

branches_3$to   <- gsub("_", " ", branches_3$to )
branches_3$to <- gsub(" direct", "-C", branches_3$to)
branches_3$to <- gsub(" cholesterol", "-C", branches_3$to)
branches_3$to   <- gsub("Apolipoprotein ", "Apo-", branches_3$to )
branches_3$from   <- gsub("_", " ", branches_3$from )
branches_3$from <- gsub(" direct", "-C", branches_3$from)
branches_3$from <- gsub(" cholesterol", "-C", branches_3$from)
branches_3$from   <- gsub("Apolipoprotein ", "Apo-", branches_3$from )
branches_3[branches_3 == "Cholesterol"] <- "TC"
branches_3[branches_3 == "Triglycerides"] <- "TG"
branches_3[branches_3 == "White blood cell (leukocyte) count"] <- "WBC count"
branches_3[branches_3 == "Mean platelet (thrombocyte) volume"] <- "Mean platelet volume"
branches_3[branches_3 == "Mean corpuscular volume"] <- "MCV"
branches_3[branches_3 == "Red blood cell (erythrocyte) count"] <- "RBC count"
branches_3[branches_3 == "Red blood cell (erythrocyte) distribution width"] <- "RDW"
branches_3[branches_3 == "Mean corpuscular haemoglobin concentration"] <- "MCHC"
branches_3[branches_3 == "High light scatter reticulocyte count"] <- "HLSRC"
branches_3[branches_3 == "High light scatter reticulocyte percentage"] <- "HLSRP"
branches_3[branches_3 == "Platelet distribution width"] <- "Platelet \n distribution width"
branches_3$from   <- gsub("cyte ", "cyte \n", branches_3$from )
branches_3$from   <- gsub(" conc", "\n  conc", branches_3$from )
branches_3$from   <- gsub(" haem", "\n  haem", branches_3$from )
branches_3$from   <- gsub(" cell", "\n  cell", branches_3$from )
branches_3$from   <- gsub("ature", "ature \n", branches_3$from )
branches_3[branches_3 == "  "] <- " "
branches_3

mytree_3 <- igraph::graph_from_data_frame( edges_3, vertices=trunk_3 )
from_3  <-  match( branches_3$from, trunk_3$name)
to_3  <-  match( branches_3$to, trunk_3$name)
branches_3$value_2[branches_3$value_2 == 0] <- 0.1
branches_3$value_2[branches_3$value_2 == 1] <- 0.9
p_value_3 <- c( rep(1,35), rep(0,5),rep(1,30), rep(0,5), rep(1,25) )

#1300 x 1375
ggraph(mytree_3, layout = 'dendrogram', circular = TRUE) + 
  geom_conn_bundle(data = get_con(from = from_3, to = to_3), alpha=0.4, width=3.5, tension=0, aes(edge_colour =  p_value_3)) +
  scale_edge_color_continuous(low="hotpink3", high="lightskyblue") +
  geom_node_text(aes(x = x*1.1, y=y*1.1, filter = leaf, label=name, angle = angle, hjust=hjust, colour=group), size=5.3, alpha=1) +
  geom_node_point(aes(filter = leaf, x = x*1.02, y=y*1.02, colour=group),size=10, alpha=0.85) +
  scale_colour_manual(values= rep( c("#B2DF8A", "#1F78B4","#33A02C","#FDBF6F") , 30)) +
  theme_void() +
  theme(
    legend.position="bottom", legend.text = element_text(size = 13.1), 
    plot.margin=unit(c(0.75,0.75,0.75,0.75),"cm"), ) +
  expand_limits(x = c(-1.4, 1.4), y = c(-1.4, 1.4))



#Lipids AF = 1_Immune_cells, 2_Reticulocytes, 3_RBC, 4_Platelet, 5_Diet (n=68)
d7 <- data.frame(from="origin", to=paste("group", seq(1,6), sep=""))
d8 <- data.frame( from = c("group1", rep("group2",6),rep("group4",2),"group2",rep("group4",2),rep("group1",10), rep("group3",7),rep("group5",11),
                           rep("group6",7)),
                  to = c(colnames(NAFLD_LRAF_SD[c(2:30, 33,35 ,40:44, 59, 61, 65,66, 78,80,83,85,95,96,97)])) )
(edges_4 <- rbind(d7, d8) %>% arrange(from))
set.seed(132)
branches_4 <- rbind(Sig_D_B_CC_AF %>% filter(str_detect(Marker1, 'LDL|HDL|Chol|Trig|Apo|Lipo')), 
                    Sig_D_B_CC_AF %>% filter(str_detect(Marker2, 'LDL|HDL|Chol|Trig|Apo|Lipo'))) %>% rename("from"="Marker1","to"="Marker2") %>% 
  mutate(value_1 = min(-log(Bonferroni))/-log(Bonferroni)) %>%  mutate(value_AF = ifelse(Relationship == "Positive",1,0)) %>% 
  select(from, to, Bonferroni, value_1, value_AF)
trunk_4  <-  data.frame( name = unique(c(as.character(edges_4$from), as.character(edges_4$to))) )
trunk_4$group  <-  edges_4$from[ match( trunk_4$name, edges_4$to ) ]
trunk_4[c(8:53),] <- trunk_4[c(8:53),] %>% arrange(group)
trunk_4 <- trunk_4 %>% mutate(value = runif(nrow(trunk_4)) )
trunk_4$id <- NA
myleafy <- which(is.na( match(trunk_4$name, edges_4$from) ))
nleafy <- length(myleafy)
trunk_4$id[ myleafy ] <- seq(1:nleafy)
trunk_4$angle <- -15 - 360 * (trunk_4$id-0.5) / nleafy
trunk_4$hjust <- ifelse( trunk_4$angle < -90, 1, 0)
trunk_4$hjust <- ifelse( trunk_4$angle > -270, 0, trunk_4$hjust)
trunk_4$angle <- ifelse(trunk_4$angle < -90, trunk_4$angle+180, trunk_4$angle)
trunk_4$angle <- ifelse(trunk_4$angle < -90, trunk_4$angle+180, trunk_4$angle)
trunk_4$hjust[c(8:17)] <- 1
trunk_4

trunk_4$name   <- gsub("_", " ", trunk_4$name )
trunk_4$name   <- gsub(" direct", "-C", trunk_4$name )
trunk_4$name <- gsub(" cholesterol", "-C", trunk_4$name )
trunk_4$name <- gsub("Apolipoprotein ", "Apo-", trunk_4$name )
trunk_4[trunk_4 == "Cholesterol"] <- "TC"
trunk_4[trunk_4 == "Triglycerides"] <- "TG"
trunk_4[trunk_4 == "White blood cell (leukocyte) count"] <- "WBC count"
trunk_4[trunk_4 == "Mean platelet (thrombocyte) volume"] <- "Mean platelet volume"
trunk_4[trunk_4 == "Red blood cell (erythrocyte) count"] <- "RBC count"
trunk_4[trunk_4 == "Red blood cell (erythrocyte) distribution width"] <- "RDW"
trunk_4[trunk_4 == "Mean corpuscular volume"] <- "MCV"
trunk_4[trunk_4 == "Red blood cell (erythrocyte) distribution width"] <- "RDW"
trunk_4[trunk_4 == "Mean corpuscular haemoglobin concentration"] <- "MCHC"
trunk_4[trunk_4 == "High light scatter reticulocyte count"] <- "HLSRC"
trunk_4[trunk_4 == "High light scatter reticulocyte percentage"] <- "HLSRP"
trunk_4[trunk_4 == "Platelet distribution width"] <- "Platelet \n distribution width"
trunk_4$name   <- gsub(" conc", "\n  conc", trunk_4$name )
trunk_4$name   <- gsub(" haem", "\n  haem", trunk_4$name )
trunk_4$name   <- gsub(" cell", "\n  cell", trunk_4$name )
trunk_4$name   <- gsub("ature ", "ature \n", trunk_4$name )
trunk_4[trunk_4 == "  "] <- " "
trunk_4[trunk_4 == "group3"] <- "2_Reticulocytes"
trunk_4[trunk_4 == "group2"] <- "3_RBC"
trunk_4[trunk_4 == "group4"] <- "4_Platelets"
trunk_4[trunk_4 == "group6"] <- "7_Lipids"
trunk_4[trunk_4 == "group1"] <- "1_Immune cells"
trunk_4[trunk_4 == "group5"] <- "5_Diet"
trunk_4

edges_4$to   <- gsub("_", " ", edges_4$to )
edges_4$to   <- gsub(" direct", "-C", edges_4$to )
edges_4$to <- gsub(" cholesterol", "-C", edges_4$to )
edges_4$to <- gsub("Apolipoprotein ", "Apo-", edges_4$to )
edges_4[edges_4 == "Cholesterol"] <- "TC"
edges_4[edges_4 == "Triglycerides"] <- "TG"
edges_4[edges_4 == "White blood cell (leukocyte) count"] <- "WBC count"
edges_4[edges_4 == "Mean platelet (thrombocyte) volume"] <- "Mean platelet volume"
edges_4[edges_4 == "Red blood cell (erythrocyte) count"] <- "RBC count"
edges_4[edges_4 == "Red blood cell (erythrocyte) distribution width"] <- "RDW"
edges_4[edges_4 == "Mean corpuscular volume"] <- "MCV"
edges_4[edges_4 == "Red blood cell (erythrocyte) distribution width"] <- "RDW"
edges_4[edges_4 == "Mean corpuscular haemoglobin concentration"] <- "MCHC"
edges_4[edges_4 == "High light scatter reticulocyte count"] <- "HLSRC"
edges_4[edges_4 == "High light scatter reticulocyte percentage"] <- "HLSRP"
edges_4[edges_4 == "Platelet distribution width"] <- "Platelet \n distribution width"
edges_4$to   <- gsub(" conc", "\n  conc", edges_4$to )
edges_4$to   <- gsub(" haem", "\n  haem", edges_4$to )
edges_4$to   <- gsub(" cell", "\n  cell", edges_4$to )
edges_4$to   <- gsub("ature ", "ature \n", edges_4$to )
edges_4[edges_4 == "  "] <- " "
edges_4[edges_4 == "group3"] <- "2_Reticulocytes"
edges_4[edges_4 == "group2"] <- "3_RBC"
edges_4[edges_4 == "group4"] <- "4_Platelets"
edges_4[edges_4 == "group6"] <- "7_Lipids"
edges_4[edges_4 == "group1"] <- "1_Immune cells"
edges_4[edges_4 == "group5"] <- "5_Diet"
edges_4

branches_4$from   <- gsub("_", " ", branches_4$from )
branches_4$to   <- gsub("_", " ", branches_4$to )
branches_4$from   <- gsub(" direct", "-C", branches_4$from )
branches_4$from <- gsub(" cholesterol", "-C", branches_4$from )
branches_4$from <- gsub("Apolipoprotein ", "Apo-", branches_4$from )
branches_4$to   <- gsub(" direct", "-C", branches_4$to )
branches_4$to <- gsub(" cholesterol", "-C", branches_4$to )
branches_4$to <- gsub("Apolipoprotein ", "Apo-", branches_4$to )
branches_4[branches_4 == "Cholesterol"] <- "TC"
branches_4[branches_4 == "Triglycerides"] <- "TG"
branches_4[branches_4 == "White blood cell (leukocyte) count"] <- "WBC count"
branches_4[branches_4 == "Mean platelet (thrombocyte) volume"] <- "Mean platelet volume"
branches_4[branches_4 == "Red blood cell (erythrocyte) count"] <- "RBC count"
branches_4[branches_4 == "Red blood cell (erythrocyte) distribution width"] <- "RDW"
branches_4[branches_4 == "Mean corpuscular volume"] <- "MCV"
branches_4[branches_4 == "Red blood cell (erythrocyte) distribution width"] <- "RDW"
branches_4[branches_4 == "Mean corpuscular haemoglobin concentration"] <- "MCHC"
branches_4[branches_4 == "High light scatter reticulocyte count"] <- "HLSRC"
branches_4[branches_4 == "High light scatter reticulocyte percentage"] <- "HLSRP"
branches_4[branches_4 == "Platelet distribution width"] <- "Platelet \n distribution width"
branches_4$to   <- gsub(" conc", "\n  conc", branches_4$to )
branches_4$to   <- gsub(" haem", "\n  haem", branches_4$to )
branches_4$to   <- gsub(" cell", "\n  cell", branches_4$to )
branches_4$to   <- gsub("ature ", "ature \n", branches_4$to )
branches_4$from   <- gsub(" conc", "\n  conc", branches_4$from )
branches_4$from   <- gsub(" haem", "\n  haem", branches_4$from )
branches_4$from   <- gsub(" cell", "\n  cell", branches_4$from )
branches_4$from   <- gsub("ature ", "ature \n", branches_4$from )
branches_4[branches_4 == "  "] <- " "
branches_4

mytree_4 <- igraph::graph_from_data_frame( edges_4, vertices=trunk_4 )
from_4  <-  match( branches_4$from, trunk_4$name)
to_4  <-  match( branches_4$to, trunk_4$name)
branches_4$value_AF_2 <- paste(paste('rep'), branches_4$value_AF, paste(',5),'))
branches_4$value_AF_2   <- gsub("rep", "rep(", branches_4$value_AF_2)
branches_4
noquote( branches_4$value_AF_2  )
p_value_4 <- c(rep( 0 ,5), rep( 0 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 0 ,5), rep( 0 ,5), rep( 0 ,5), rep( 0 ,5), rep( 1 ,5), rep( 0 ,5), 
               rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), 
               rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), 
               rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 0 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), 
               rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5),rep( 0 ,5), rep( 0 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 0 ,5), rep( 0 ,5), 
               rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5),rep( 0 ,5), rep( 0 ,5), rep( 1 ,5), rep( 0 ,5), rep( 0 ,5), rep( 0 ,5), 
               rep( 1 ,5), rep( 0 ,5))
#1300 x 1375
#Mean sphered cell volume = MSCV
ggraph(mytree_4, layout = 'dendrogram', circular = TRUE) + 
  geom_conn_bundle(data = get_con(from = from_4, to = to_4), alpha=0.5, width=1.5, tension=0, aes(edge_colour =  p_value_4) ) +
  scale_edge_color_continuous(low="hotpink3", high="lightskyblue") +
  geom_node_text(aes(x = x*1.06, y=y*1.06, filter = leaf, label=name, angle = angle, hjust=hjust, colour=group), size=4.7, alpha=1) +
  geom_node_point(aes(filter = leaf, x = x*1.02, y=y*1.02, colour=group),size=7.5, alpha=0.92) +
  scale_colour_manual(values= rep( c("#A6CEE3", "#1F78B4" ,"#B2DF8A" ,"#33A02C", "#FB9A99","#FDBF6F") , 30)) +
  theme_void() +
  theme(
    legend.position="bottom", legend.text = element_text(size = 13.1), 
    plot.margin=unit(c(0.75,0.75,0.75,0.75),"cm"), ) +
  expand_limits(x = c(-1.4, 1.4), y = c(-1.4, 1.4))




#Lipids BF = 1_Immune_cells, 2_Reticulocytes, 3_RBC (n=4)
d9 <- data.frame(from="origin", to=paste("group", seq(1,4), sep=""))
d10 <- data.frame( from = c("group1", rep("group2",7), rep("group1",10),rep("group3",7),rep("group4",7)),
                  to = c(colnames(NAFLD_LRBF_SD[c(2:8,11,14:30,78,80,83,85,95,96,97)])) )
(edges_5 <- rbind(d9, d10) %>% arrange(from))
set.seed(132)
(branches_5 <- Sig_D_B_CC_BF %>% rename("from"="Marker1","to"="Marker2") %>% mutate(value_1 = min(-log(Bonferroni))/-log(Bonferroni)) %>% 
  mutate(value_2 = ifelse(Relationship == "Positive",1,0)  ) %>% filter(str_detect(to, 'LDL|HDL|Chol|Trig|Apo|Lipo')) %>% 
  select(from, to, Bonferroni, value_1, value_2) )
trunk_5  <-  data.frame( name = unique(c(as.character(edges_5$from), as.character(edges_5$to))) )
trunk_5$group  <-  edges_5$from[ match( trunk_5$name, edges_5$to ) ]
trunk_5[c(5:36),] <- trunk_5[c(5:36),] %>% arrange(group)
trunk_5 <- trunk_5 %>% mutate(value = runif(nrow(trunk_5)) )
trunk_5$id <- NA
myleafy <- which(is.na( match(trunk_5$name, edges_5$from) ))
nleafy <- length(myleafy)
trunk_5$id[ myleafy ] <- seq(1:nleafy)

trunk_5$name   <- gsub("_", " ", trunk_5$name )
trunk_5$name   <- gsub(" direct", "-C", trunk_5$name )
trunk_5$name <- gsub(" cholesterol", "-C", trunk_5$name )
trunk_5$name <- gsub("Apolipoprotein ", "Apo-", trunk_5$name )
trunk_5[trunk_5 == "Cholesterol"] <- "TC"
trunk_5[trunk_5 == "Triglycerides"] <- "TG"
trunk_5[trunk_5 == "White blood cell (leukocyte) count"] <- "WBC count"
trunk_5[trunk_5 == "Red blood cell (erythrocyte) count"] <- "RBC count"
trunk_5[trunk_5 == "Red blood cell (erythrocyte) distribution width"] <- "RDW"
trunk_5[trunk_5 == "Mean corpuscular volume"] <- "MCV"
trunk_5[trunk_5 == "Mean corpuscular haemoglobin concentration"] <- "MCHC"
trunk_5[trunk_5 == "High light scatter reticulocyte count"] <- "HLSRC"
trunk_5[trunk_5 == "High light scatter reticulocyte percentage"] <- "HLSRP"
trunk_5$name   <- gsub(" cell", "\n  cell", trunk_5$name )
trunk_5$name   <- gsub("ature ", "ature \n", trunk_5$name )
trunk_5$name   <- gsub(" haem", "\n haem", trunk_5$name )
trunk_5[trunk_5 == "  "] <- " "
trunk_5[trunk_5 == "group2"] <- "3_RBC"
trunk_5[trunk_5 == "group4"] <- "7_Lipids"
trunk_5[trunk_5 == "group3"] <- "2_Reticulocytes"
trunk_5[trunk_5 == "group1"] <- "1_Immune cells"
trunk_5
edges_5$to   <- gsub("_", " ", edges_5$to )
edges_5$to   <- gsub(" direct", "-C", edges_5$to )
edges_5$to <- gsub(" cholesterol", "-C", edges_5$to )
edges_5$to <- gsub("Apolipoprotein ", "Apo-", edges_5$to )
edges_5[edges_5 == "Cholesterol"] <- "TC"
edges_5[edges_5 == "Triglycerides"] <- "TG"
edges_5[edges_5 == "White blood cell (leukocyte) count"] <- "WBC count"
edges_5[edges_5 == "Red blood cell (erythrocyte) count"] <- "RBC count"
edges_5[edges_5 == "Red blood cell (erythrocyte) distribution width"] <- "RDW"
edges_5[edges_5 == "Mean corpuscular volume"] <- "MCV"
edges_5[edges_5 == "Mean corpuscular haemoglobin concentration"] <- "MCHC"
edges_5[edges_5 == "High light scatter reticulocyte count"] <- "HLSRC"
edges_5[edges_5 == "High light scatter reticulocyte percentage"] <- "HLSRP"
edges_5$to   <- gsub(" cell", "\n  cell", edges_5$to )
edges_5$to   <- gsub("ature ", "ature \n", edges_5$to )
edges_5$to   <- gsub(" haem", "\n haem", edges_5$to )
edges_5[edges_5 == "  "] <- " "
edges_5[edges_5 == "group2"] <- "3_RBC"
edges_5[edges_5 == "group4"] <- "7_Lipids"
edges_5[edges_5 == "group3"] <- "2_Reticulocytes"
edges_5[edges_5 == "group1"] <- "1_Immune cells"
edges_5
branches_5$from   <- gsub("_", " ", branches_5$from )
branches_5$to   <- gsub("_", " ", branches_5$to )
branches_5$to <- gsub(" cholesterol", "-C", branches_5$to )
branches_5[branches_5 == "Triglycerides"] <- "TG"
branches_5$from   <- gsub(" cell", "\n  cell", branches_5$from )
branches_5[branches_5 == "  "] <- " "
branches_5

trunk_5$angle <- -145 - 360 * (trunk_5$id-0.5) / nleafy
trunk_5$hjust <- ifelse( trunk_5$angle < -90, 1, 0)
trunk_5$hjust <- ifelse( trunk_5$angle > -270, 0, trunk_5$hjust)
trunk_5$angle <- ifelse(trunk_5$angle < -90, trunk_5$angle+180, trunk_5$angle)
trunk_5$angle <- ifelse(trunk_5$angle < -90, trunk_5$angle+180, trunk_5$angle)
trunk_5$angle <- ifelse(trunk_5$angle < -90, trunk_5$angle+180, trunk_5$angle)
trunk_5$hjust[c(32:35,37)] <- 0
trunk_5

mytree_5 <- igraph::graph_from_data_frame( edges_5, vertices=trunk_5 )
from_5  <-  match( branches_5$from, trunk_5$name)
to_5  <-  match( branches_5$to, trunk_5$name)
branches_5$value_AF_2 <- paste(paste('rep'), branches_5$value_2, paste(',5),'))
branches_5$value_AF_2   <- gsub("rep", "rep(", branches_5$value_AF_2)
branches_5
noquote( branches_5$value_AF_2  )
p_value_5 <- c(rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 0 ,5))

#1000 x 1050
ggraph(mytree_5, layout = 'dendrogram', circular = TRUE) + 
  geom_conn_bundle(data = get_con(from = from_5, to = to_5), alpha=0.5, width=2.5, tension=0, aes(edge_colour =  p_value_5) ) +
  scale_edge_color_continuous(low="hotpink3", high="lightskyblue") +
  geom_node_text(aes(x = x*1.08, y=y*1.08, filter = leaf, label=name, angle = angle, hjust=hjust, colour=group), size=4, alpha=1) +
  geom_node_point(aes(filter = leaf, x = x*1.02, y=y*1.02, colour=group),size=7, alpha=0.85) +
  scale_colour_manual(values= rep( c("#A6CEE3", "#1F78B4" ,"#B2DF8A" ,"#FDBF6F") , 30)) +
  theme_void() +
  theme(legend.position="none", legend.text = element_text(size = 13.1), 
    plot.margin=unit(c(0.75,0.75,0.75,0.75),"cm"), ) +
  expand_limits(x = c(-1.8, 1.8), y = c(-1.8, 1.8))




#Lipids C = 1_Immune_cells, 2_Reticulocytes, 3_RBC, 4_Platelet, 5_Diet (cereal,coffee,eat eggs etc., salad,cheese,lamb,bread) (n=101)
d11 <- data.frame(from="origin", to=paste("group", seq(1,6), sep=""))
d12 <- data.frame( from = c("group1", rep("group2",6),rep("group4",2),"group2",rep("group4",2),rep("group1",10), rep("group3",7),rep("group5",25),
                            rep("group6",7)),
                   to = c(colnames(NAFLD_LRC_SD[c(2:30,32,35,40:44,34,50:64,67,68,78,80,83,85,95:97)])) )
(edges_6 <- rbind(d11, d12) %>% arrange(from))
set.seed(132)
(branches_6 <- rbind(Sig_D_B_CC_C %>% filter(str_detect(Marker1, 'LDL|HDL|Chol|Trig|Apo|Lipo')), 
                    Sig_D_B_CC_C %>% filter(str_detect(Marker2, 'LDL|HDL|Chol|Trig|Apo|Lipo'))) %>% rename("from"="Marker1","to"="Marker2") %>% 
  mutate(value_1 = min(-log(Bonferroni))/-log(Bonferroni)) %>%  mutate(value_AF = ifelse(Relationship == "Positive",1,0)) %>% 
  select(from, to, Bonferroni, value_1, value_AF) )

edges_6$to   <- gsub("_", " ", edges_6$to )
edges_6$to <- gsub(" direct", "-C", edges_6$to)
edges_6$to <- gsub(" cholesterol", "-C", edges_6$to)
edges_6$to   <- gsub("Apolipoprotein ", "Apo-", edges_6$to )
edges_6[edges_6 == "C reactive protein"] <- "CRP"
edges_6[edges_6 == "Aspartate aminotransferase"] <- "AST"
edges_6[edges_6 == "Alanine aminotransferase"] <- "ALT"
edges_6[edges_6 == "Gamma glutamyltransferase"] <- "GGT"
edges_6[edges_6 == "Alkaline phosphatase"] <- "ALP"
edges_6[edges_6 == "Glycated haemoglobin (HbA1c)"] <- "HbA1c"
edges_6[edges_6 == "Cholesterol"] <- "TC"
edges_6[edges_6 == "Triglycerides"] <- "TG"
edges_6[edges_6 == "White blood cell (leukocyte) count"] <- "WBC count"
edges_6[edges_6 == "Mean platelet (thrombocyte) volume"] <- "Mean platelet volume"
edges_6[edges_6 == "Mean corpuscular volume"] <- "MCV"
edges_6[edges_6 == "Red blood cell (erythrocyte) count"] <- "RBC count"
edges_6[edges_6 == "Red blood cell (erythrocyte) distribution width"] <- "RDW"
edges_6[edges_6 == "Mean corpuscular haemoglobin concentration"] <- "MCHC"
edges_6[edges_6 == "High light scatter reticulocyte count"] <- "HLSRC"
edges_6[edges_6 == "High light scatter reticulocyte percentage"] <- "HLSRP"
edges_6[edges_6 == "  "] <- " "
edges_6[edges_6 == "group1"] <- "1_Immune cells"
edges_6[edges_6 == "group2"] <- "2_Reticulocytes"
edges_6[edges_6 == "group3"] <- "3_RBC"
edges_6[edges_6 == "group4"] <- "4_Platelets"
edges_6[edges_6 == "group5"] <- "5_Diet"
edges_6[edges_6 == "group6"] <- "6_Liver enzymes"
edges_6[edges_6 == "group7"] <- "7_Lipids"
edges_6[edges_6 == "group8"] <- "8_Urine and kidney"
edges_6[edges_6 == "group9"] <- "9_Other biomarkers"
edges_6
branches_6$to   <- gsub("_", " ", branches_6$to )
branches_6$to <- gsub(" direct", "-C", branches_6$to)
branches_6$to <- gsub(" cholesterol", "-C", branches_6$to)
branches_6$to   <- gsub("Apolipoprotein ", "Apo-", branches_6$to )
branches_6$from   <- gsub("_", " ", branches_6$from )
branches_6$from <- gsub(" direct", "-C", branches_6$from)
branches_6$from <- gsub(" cholesterol", "-C", branches_6$from)
branches_6$from   <- gsub("Apolipoprotein ", "Apo-", branches_6$from )
branches_6[branches_6 == "C reactive protein"] <- "CRP"
branches_6[branches_6 == "Aspartate aminotransferase"] <- "AST"
branches_6[branches_6 == "Alanine aminotransferase"] <- "ALT"
branches_6[branches_6 == "Gamma glutamyltransferase"] <- "GGT"
branches_6[branches_6 == "Alkaline phosphatase"] <- "ALP"
branches_6[branches_6 == "Glycated haemoglobin (HbA1c)"] <- "HbA1c"
branches_6[branches_6 == "Cholesterol"] <- "TC"
branches_6[branches_6 == "Triglycerides"] <- "TG"
branches_6[branches_6 == "White blood cell (leukocyte) count"] <- "WBC count"
branches_6[branches_6 == "Mean platelet (thrombocyte) volume"] <- "Mean platelet volume"
branches_6[branches_6 == "Mean corpuscular volume"] <- "MCV"
branches_6[branches_6 == "Red blood cell (erythrocyte) count"] <- "RBC count"
branches_6[branches_6 == "Red blood cell (erythrocyte) distribution width"] <- "RDW"
branches_6[branches_6 == "Mean corpuscular haemoglobin concentration"] <- "MCHC"
branches_6[branches_6 == "High light scatter reticulocyte count"] <- "HLSRC"
branches_6[branches_6 == "High light scatter reticulocyte percentage"] <- "HLSRP"
branches_6[branches_6 == "  "] <- " "

branches_6[branches_6 == "  "] <- " "
branches_6$value_AF_2 <- paste(paste('rep'), branches_6$value_AF, paste(',5),'))
branches_6$value_AF_2   <- gsub("rep", "rep(", branches_6$value_AF_2)
#noquote( branches_6$value_AF_2  )
p_value_6 <- c(rep( 0 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 0 ,5), rep( 0 ,5), rep( 0 ,5),
               rep( 0 ,5), rep( 0 ,5), rep( 0 ,5), rep( 0 ,5), rep( 0 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 0 ,5),
               rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5),
               rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5),
               rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 0 ,5), rep( 1 ,5), rep( 0 ,5),
               rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 0 ,5), rep( 1 ,5), rep( 0 ,5),
               rep( 1 ,5), rep( 0 ,5), rep( 0 ,5), rep( 0 ,5), rep( 0 ,5), rep( 0 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5),
               rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 0 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5),
               rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 0 ,5))
branches_6

trunk_6  <-  data.frame( name = unique(c(as.character(edges_6$from), as.character(edges_6$to))) )
trunk_6$group  <-  edges_6$from[ match( trunk_6$name, edges_6$to ) ]
trunk_6[c(8:63),] <- trunk_6[c(8:63),] %>% arrange(group)
trunk_6 <- trunk_6 %>% mutate(value = runif(nrow(trunk_6)) )
trunk_6$id <- NA
myleafy <- which(is.na( match(trunk_6$name, edges_6$from) ))
nleafy <- length(myleafy)
trunk_6$id[ myleafy ] <- seq(1:nleafy)
trunk_6$angle <-75 - 360 * (trunk_6$id-0.5) / nleafy
trunk_6$hjust <- ifelse( trunk_6$angle < -90, 1, 0)
trunk_6$hjust <- ifelse( trunk_6$angle > -270, 0, trunk_6$hjust)
trunk_6$angle <- ifelse(trunk_6$angle < -90, trunk_6$angle+180, trunk_6$angle)
trunk_6$angle <- ifelse(trunk_6$angle < -90, trunk_6$angle+180, trunk_6$angle)
trunk_6$hjust[c(8:35)] <- 1
trunk_6
mytree_6 <- igraph::graph_from_data_frame( edges_6, vertices=trunk_6 )
from_6  <-  match( branches_6$from, trunk_6$name)
to_6  <-  match( branches_6$to, trunk_6$name)
ggraph(mytree_6, layout = 'dendrogram', circular = TRUE) + 
  geom_conn_bundle(data = get_con(from = from_6, to = to_6), alpha=0.5, width=1.5, tension=0, aes(edge_colour =  p_value_6) ) +
  scale_edge_color_continuous(low="hotpink3", high="lightskyblue") +
  geom_node_text(aes(x = x*1.07, y=y*1.07, filter = leaf, label=name, angle = angle, hjust=hjust, colour=group), size=4.5, alpha=1) +
  geom_node_point(aes(filter = leaf, x = x*1.02, y=y*1.02, colour=group),size=7, alpha=0.9) +
  scale_colour_manual(values= rep( c("#A6CEE3" ,"#1F78B4", "#B2DF8A" ,"#33A02C" ,"#FB9A99","#FDBF6F") , 30)) +
  theme_void() +
  theme(legend.position="none", legend.text = element_text(size = 13.1), 
    plot.margin=unit(c(0.75,0.75,0.75,0.75),"cm"), ) +
  expand_limits(x = c(-1.45, 1.45), y = c(-1.45, 1.45))





#Liver enzymes AF = 1_Immune_cells, 2_Reticulocytes, 3_RBC, 4_Platelet, 5_Diet(cereal,eat etc., coffee,lamb) (n=69)
d13 <- data.frame(from="origin", to=paste("group", seq(1,6), sep=""))
d14 <- data.frame( from = c("group1", rep("group2",6),rep("group4",2),"group2",rep("group4",2),rep("group1",10), rep("group3",7),rep("group5",17),
                            rep("group6",4)),
                   to = c(colnames(NAFLD_LRAF_SD[c(2:30,35,59,40:44,54:58,61:64,67,76,79,81,84)])) )
(edges_7 <- rbind(d13, d14) %>% arrange(from))
set.seed(132)
(branches_7 <- rbind(Sig_D_B_CC_AF %>% filter(str_detect(Marker1, 'GGT|AST|ALT|ALP|Alk|Gam|Ala|Asp')), 
                    Sig_D_B_CC_AF %>% filter(str_detect(Marker2, 'GGT|AST|ALT|ALP|Alk|Gam|Ala|Asp'))) %>% rename("from"="Marker1","to"="Marker2") %>% 
  mutate(value_1 = min(-log(Bonferroni))/-log(Bonferroni)) %>%  mutate(value_AF = ifelse(Relationship == "Positive",1,0)) %>% 
  select(from, to, Bonferroni, value_1, value_AF))
trunk_7  <-  data.frame( name = unique(c(as.character(edges_7$from), as.character(edges_7$to))) )
trunk_7$group  <-  edges_7$from[ match( trunk_7$name, edges_7$to ) ]
trunk_7[c(8:57),] <- trunk_7[c(8:57),] %>% arrange(group)
trunk_7 <- trunk_7 %>% mutate(value = 0.1 )
trunk_7$id <- NA
myleafy <- which(is.na( match(trunk_7$name, edges_7$from) ))
nleafy <- length(myleafy)
trunk_7$id[ myleafy ] <- seq(1:nleafy)
trunk_7$name   <- gsub("_", " ", trunk_7$name )
trunk_7[trunk_7 == "Aspartate aminotransferase"] <- "AST"
trunk_7[trunk_7 == "Alanine aminotransferase"] <- "ALT"
trunk_7[trunk_7 == "Gamma glutamyltransferase"] <- "GGT"
trunk_7[trunk_7 == "Alkaline phosphatase"] <- "ALP"
trunk_7[trunk_7 == "White blood cell (leukocyte) count"] <- "WBC count"
trunk_7[trunk_7 == "Mean platelet (thrombocyte) volume"] <- "Mean platelet volume"
trunk_7[trunk_7 == "Red blood cell (erythrocyte) count"] <- "RBC count"
trunk_7[trunk_7 == "Red blood cell (erythrocyte) distribution width"] <- "RDW"
trunk_7[trunk_7 == "Mean corpuscular volume"] <- "MCV"
trunk_7[trunk_7 == "Red blood cell (erythrocyte) distribution width"] <- "RDW"
trunk_7[trunk_7 == "Mean corpuscular haemoglobin concentration"] <- "MCHC"
trunk_7[trunk_7 == "High light scatter reticulocyte count"] <- "HLSRC"
trunk_7[trunk_7 == "High light scatter reticulocyte percentage"] <- "HLSRP"
trunk_7[trunk_7 == "  "] <- " "
trunk_7[trunk_7 == "group1"] <- "1_Immune cells"
trunk_7[trunk_7 == "group2"] <- "2_Reticulocytes"
trunk_7[trunk_7 == "group3"] <- "3_RBC"
trunk_7[trunk_7 == "group4"] <- "4_Platelets"
trunk_7[trunk_7 == "group5"] <- "5_Diet"
trunk_7[trunk_7 == "group6"] <- "6_Liver enzymes"
trunk_7
edges_7$to   <- gsub("_", " ", edges_7$to )
edges_7[edges_7 == "Aspartate aminotransferase"] <- "AST"
edges_7[edges_7 == "Alanine aminotransferase"] <- "ALT"
edges_7[edges_7 == "Gamma glutamyltransferase"] <- "GGT"
edges_7[edges_7 == "Alkaline phosphatase"] <- "ALP"
edges_7[edges_7 == "White blood cell (leukocyte) count"] <- "WBC count"
edges_7[edges_7 == "Mean platelet (thrombocyte) volume"] <- "Mean platelet volume"
edges_7[edges_7 == "Red blood cell (erythrocyte) count"] <- "RBC count"
edges_7[edges_7 == "Red blood cell (erythrocyte) distribution width"] <- "RDW"
edges_7[edges_7 == "Mean corpuscular volume"] <- "MCV"
edges_7[edges_7 == "Red blood cell (erythrocyte) distribution width"] <- "RDW"
edges_7[edges_7 == "Mean corpuscular haemoglobin concentration"] <- "MCHC"
edges_7[edges_7 == "High light scatter reticulocyte count"] <- "HLSRC"
edges_7[edges_7 == "High light scatter reticulocyte percentage"] <- "HLSRP"
edges_7[edges_7 == "  "] <- " "
edges_7[edges_7 == "group1"] <- "1_Immune cells"
edges_7[edges_7 == "group2"] <- "2_Reticulocytes"
edges_7[edges_7 == "group3"] <- "3_RBC"
edges_7[edges_7 == "group4"] <- "4_Platelets"
edges_7[edges_7 == "group5"] <- "5_Diet"
edges_7[edges_7 == "group6"] <- "6_Liver enzymes"
edges_7
branches_7$to   <- gsub("_", " ", branches_7$to )
branches_7$from   <- gsub("_", " ", branches_7$from )
branches_7[branches_7 == "Aspartate aminotransferase"] <- "AST"
branches_7[branches_7 == "Alanine aminotransferase"] <- "ALT"
branches_7[branches_7 == "Gamma glutamyltransferase"] <- "GGT"
branches_7[branches_7 == "Alkaline phosphatase"] <- "ALP"
branches_7[branches_7 == "White blood cell (leukocyte) count"] <- "WBC count"
branches_7[branches_7 == "Mean platelet (thrombocyte) volume"] <- "Mean platelet volume"
branches_7[branches_7 == "Red blood cell (erythrocyte) count"] <- "RBC count"
branches_7[branches_7 == "Red blood cell (erythrocyte) distribution width"] <- "RDW"
branches_7[branches_7 == "Mean corpuscular volume"] <- "MCV"
branches_7[branches_7 == "Red blood cell (erythrocyte) distribution width"] <- "RDW"
branches_7[branches_7 == "Mean corpuscular haemoglobin concentration"] <- "MCHC"
branches_7[branches_7 == "High light scatter reticulocyte count"] <- "HLSRC"
branches_7[branches_7 == "High light scatter reticulocyte percentage"] <- "HLSRP"
branches_7[branches_7 == "  "] <- " "
branches_7$value_AF_2 <- paste(paste('rep'), branches_7$value_AF, paste(',5),'))
branches_7$value_AF_2   <- gsub("rep", "rep(", branches_7$value_AF_2)
branches_7
noquote( branches_7$value_AF_2  )
relations_7 <- c(rep( 0 ,5), rep( 0 ,5), rep( 1 ,5), rep( 0 ,5), rep( 0 ,5), rep( 1 ,5), rep( 0 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), 
                 rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), 
                 rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 0 ,5), rep( 0 ,5), rep( 1 ,5), 
                 rep( 1 ,5), rep( 0 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), 
                 rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), 
                 rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 0 ,5), 
                 rep( 1 ,5), rep( 1 ,5), rep( 1 ,5))

trunk_7$angle <- -15 - 360 * (trunk_7$id-0.5) / nleafy
trunk_7$hjust <- ifelse( trunk_7$angle < -90, 1, 0)
trunk_7$hjust <- ifelse( trunk_7$angle < -270, 0, trunk_7$hjust)
trunk_7$angle <- ifelse(trunk_7$angle < -90, trunk_7$angle+180, trunk_7$angle)
#trunk_7$angle <- ifelse(trunk_7$angle < -90, trunk_7$angle+180, trunk_7$angle)
trunk_7[c(43:57),]$hjust <- 0
trunk_7
mytree_7 <- igraph::graph_from_data_frame( edges_7, vertices=trunk_7 )
from_7  <-  match( branches_7$from, trunk_7$name)
to_7  <-  match( branches_7$to, trunk_7$name)
ggraph(mytree_7, layout = 'dendrogram', circular = TRUE) + 
  geom_conn_bundle(data = get_con(from = from_7, to = to_7), alpha=0.5, width=1.2, tension=0, aes(edge_colour =  relations_7) ) +
  scale_edge_color_continuous(low="hotpink3", high="lightskyblue") +
  geom_node_text(aes(x = x*1.04, y=y*1.04, filter = leaf, label=name, angle = angle, hjust=hjust, colour=group), size=4, alpha=1) +
  geom_node_point(aes(filter = leaf, x = x*1.01, y=y*1.01, colour=group),size=6.5, alpha=0.85) +
  scale_colour_manual(values= rep( c(brewer.pal(6,"Paired")) , 30)) +
  theme_void() + theme( legend.position="none", legend.text = element_text(size = 13.1),  plot.margin=unit(c(0.75,0.75,0.75,0.75),"cm"), ) +
  expand_limits(x = c(-1.4, 1.4), y = c(-1.4, 1.4))




#Inflam AF = 5_Diet(cofee,fruit,process, salt,bread), 6_Liver_enzymes, 7_Lipids, 8_Urine, 9_Other_Biomarkers(glucose, vit d, calc, SHBG, protein, igf,crp,hba1c,phosp, total bili) 
#(n=86) (n=87)
d15 <- data.frame(from="origin", to=paste("group", seq(1,6), sep="") )
d16 <- data.frame( from = c(rep("group1",11),rep("group2",13), "group4","group5","group3","group4","group3","group4","group5","group3","group4","group3",
                            rep("group5",4),"group6", "group5", "group6",rep("group3",3),"group6","group6",rep("group5",3),rep("group6",4)),
                   to = c(colnames(NAFLD_LRAF_SD[c(2,14:23,33,35,34,50:57,65,69,76:88,90:93,95:106)])) )
(edges_8 <- rbind(d15, d16) %>% arrange(from))
set.seed(132)
(branches_8 <- unique(rbind(Sig_D_B_CC_AF %>% filter(str_detect(Marker1, 'C_|WBC|White_bl|Lymp|Neut|Baso|Mono|Eosin')), 
                     Sig_D_B_CC_AF %>% filter(str_detect(Marker2, 'WBC|White_bl|Lymp|Neut|Baso|Mono|Eosin'))) %>% 
    rename("from"="Marker1","to"="Marker2") %>% 
    mutate(value_1 = min(-log(Bonferroni))/-log(Bonferroni)) %>%  mutate(value_AF = ifelse(Relationship == "Positive",1,0)) %>% 
    select(from, to, Bonferroni, value_1, value_AF)))
trunk_8  <-  data.frame( name = unique(c(as.character(edges_8$from), as.character(edges_8$to))) )
trunk_8$group  <-  edges_8$from[ match( trunk_8$name, edges_8$to ) ]
trunk_8[c(8:59),] <- trunk_8[c(8:59),] %>% arrange(group)
trunk_8 <- trunk_8 %>% mutate(value = 0.1 )
trunk_8$id <- NA
myleafy <- which(is.na( match(trunk_8$name, edges_8$from) ))
nleafy <- length(myleafy)
trunk_8$id[ myleafy ] <- seq(1:nleafy)

trunk_8$name   <- gsub("_", " ", trunk_8$name )
trunk_8$name   <- gsub(" direct", "-C", trunk_8$name )
trunk_8$name <- gsub(" cholesterol", "-C", trunk_8$name )
trunk_8$name <- gsub("Apolipoprotein ", "Apo-", trunk_8$name )
trunk_8[trunk_8 == "C reactive protein"] <- "CRP"
trunk_8[trunk_8 == "Aspartate aminotransferase"] <- "AST"
trunk_8[trunk_8 == "Alanine aminotransferase"] <- "ALT"
trunk_8[trunk_8 == "Gamma glutamyltransferase"] <- "GGT"
trunk_8[trunk_8 == "Alkaline phosphatase"] <- "ALP"
trunk_8[trunk_8 == "Glycated haemoglobin (HbA1c)"] <- "HbA1c"
trunk_8[trunk_8 == "Cholesterol"] <- "TC"
trunk_8[trunk_8 == "Triglycerides"] <- "TG"
trunk_8[trunk_8 == "White blood cell (leukocyte) count"] <- "WBC count"
trunk_8[trunk_8 == "Mean platelet (thrombocyte) volume"] <- "Mean platelet volume"
trunk_8[trunk_8 == "Red blood cell (erythrocyte) count"] <- "RBC count"
trunk_8[trunk_8 == "Red blood cell (erythrocyte) distribution width"] <- "RDW"
trunk_8[trunk_8 == "Mean corpuscular volume"] <- "MCV"
trunk_8[trunk_8 == "Red blood cell (erythrocyte) distribution width"] <- "RDW"
trunk_8[trunk_8 == "Mean corpuscular haemoglobin concentration"] <- "MCHC"
trunk_8[trunk_8 == "High light scatter reticulocyte count"] <- "HLSRC"
trunk_8[trunk_8 == "High light scatter reticulocyte percentage"] <- "HLSRP"
trunk_8[trunk_8 == "Creatinine (enzymatic) in urine"] <- "Creatinine \n(enzymatic) in urine"
trunk_8[trunk_8 == "  "] <- " "
trunk_8[trunk_8 == "group1"] <- "1_Immune cells"
trunk_8[trunk_8 == "group2"] <- "5_Diet"
trunk_8[trunk_8 == "group3"] <- "7_Lipids"
trunk_8[trunk_8 == "group4"] <- "6_Liver enzymes"
trunk_8[trunk_8 == "group6"] <- "8_Urine and kidney"
trunk_8[trunk_8 == "group5"] <- "9_Other biomarkers"
trunk_8
edges_8$to   <- gsub("_", " ", edges_8$to )
edges_8$to <- gsub(" direct", "-C", edges_8$to)
edges_8$to <- gsub(" cholesterol", "-C", edges_8$to)
edges_8$to   <- gsub("Apolipoprotein ", "Apo-", edges_8$to )
edges_8[edges_8 == "C reactive protein"] <- "CRP"
edges_8[edges_8 == "Aspartate aminotransferase"] <- "AST"
edges_8[edges_8 == "Alanine aminotransferase"] <- "ALT"
edges_8[edges_8 == "Gamma glutamyltransferase"] <- "GGT"
edges_8[edges_8 == "Alkaline phosphatase"] <- "ALP"
edges_8[edges_8 == "Glycated haemoglobin (HbA1c)"] <- "HbA1c"
edges_8[edges_8 == "Cholesterol"] <- "TC"
edges_8[edges_8 == "Triglycerides"] <- "TG"
edges_8[edges_8 == "White blood cell (leukocyte) count"] <- "WBC count"
edges_8[edges_8 == "Mean platelet (thrombocyte) volume"] <- "Mean platelet volume"
edges_8[edges_8 == "Mean corpuscular volume"] <- "MCV"
edges_8[edges_8 == "Red blood cell (erythrocyte) count"] <- "RBC count"
edges_8[edges_8 == "Red blood cell (erythrocyte) distribution width"] <- "RDW"
edges_8[edges_8 == "Mean corpuscular haemoglobin concentration"] <- "MCHC"
edges_8[edges_8 == "High light scatter reticulocyte count"] <- "HLSRC"
edges_8[edges_8 == "High light scatter reticulocyte percentage"] <- "HLSRP"
edges_8[edges_8 == "Creatinine (enzymatic) in urine"] <- "Creatinine \n(enzymatic) in urine"
edges_8[edges_8 == "  "] <- " "
edges_8[edges_8 == "group1"] <- "1_Immune cells"
edges_8[edges_8 == "group2"] <- "5_Diet"
edges_8[edges_8 == "group3"] <- "7_Lipids"
edges_8[edges_8 == "group4"] <- "6_Liver enzymes"
edges_8[edges_8 == "group6"] <- "8_Urine and kidney"
edges_8[edges_8 == "group5"] <- "9_Other biomarkers"

branches_8$to   <- gsub("_", " ", branches_8$to )
branches_8$to <- gsub(" direct", "-C", branches_8$to)
branches_8$to <- gsub(" cholesterol", "-C", branches_8$to)
branches_8$to   <- gsub("Apolipoprotein ", "Apo-", branches_8$to )
branches_8$from   <- gsub("_", " ", branches_8$from )
branches_8$from <- gsub(" direct", "-C", branches_8$from)
branches_8$from <- gsub(" cholesterol", "-C", branches_8$from)
branches_8$from   <- gsub("Apolipoprotein ", "Apo-", branches_8$from )
branches_8[branches_8 == "C reactive protein"] <- "CRP"
branches_8[branches_8 == "Creatinine (enzymatic) in urine"] <- "Creatinine \n(enzymatic) in urine"
branches_8[branches_8 == "Aspartate aminotransferase"] <- "AST"
branches_8[branches_8 == "Alanine aminotransferase"] <- "ALT"
branches_8[branches_8 == "Gamma glutamyltransferase"] <- "GGT"
branches_8[branches_8 == "Alkaline phosphatase"] <- "ALP"
branches_8[branches_8 == "Glycated haemoglobin (HbA1c)"] <- "HbA1c"
branches_8[branches_8 == "Cholesterol"] <- "TC"
branches_8[branches_8 == "Triglycerides"] <- "TG"
branches_8[branches_8 == "White blood cell (leukocyte) count"] <- "WBC count"
branches_8[branches_8 == "Mean platelet (thrombocyte) volume"] <- "Mean platelet volume"
branches_8[branches_8 == "Mean corpuscular volume"] <- "MCV"
branches_8[branches_8 == "Red blood cell (erythrocyte) count"] <- "RBC count"
branches_8[branches_8 == "Red blood cell (erythrocyte) distribution width"] <- "RDW"
branches_8[branches_8 == "Mean corpuscular haemoglobin concentration"] <- "MCHC"
branches_8[branches_8 == "High light scatter reticulocyte count"] <- "HLSRC"
branches_8[branches_8 == "High light scatter reticulocyte percentage"] <- "HLSRP"
branches_8[branches_8 == "High light scatter reticulocyte percentage"] <- "HLSRP"
branches_8[branches_8 == "  "] <- " "

trunk_8$angle <- -30 - 360 * (trunk_8$id-0.5) / nleafy
trunk_8$hjust <- ifelse( trunk_8$angle < -90, 1, 0)
#trunk_8$hjust <- ifelse( trunk_8$angle < -270, 0, trunk_8$hjust)
trunk_8$angle <- ifelse(trunk_8$angle < -90, trunk_8$angle+180, trunk_8$angle)
trunk_8$angle <- ifelse(trunk_8$angle < -90, trunk_8$angle+180, trunk_8$angle)
trunk_8[c(17:42),]$hjust <- 0
trunk_8[c(8:16),]$hjust <- 1
trunk_8
edges_8
mytree_8 <- igraph::graph_from_data_frame( edges_8, vertices=trunk_8 )
from_8  <-  match( branches_8$from, trunk_8$name)
to_8  <-  match( branches_8$to, trunk_8$name)
branches_8$value_AF_2 <- paste(paste('rep'), branches_8$value_AF, paste(',5),'))
branches_8$value_AF_2   <- gsub("rep", "rep(", branches_8$value_AF_2)
branches_8
#noquote( branches_8$value_AF_2  )
relations_8 <- c(rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 0 ,5), rep( 0 ,5), rep( 0 ,5), rep( 1 ,5), rep( 0 ,5), rep( 0 ,5), rep( 0 ,5), 
                 rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), 
                 rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), 
                 rep( 1 ,5), rep( 0 ,5), rep( 0 ,5), rep( 0 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), 
                 rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 0 ,5), rep( 0 ,5), rep( 1 ,5), 
                 rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 0 ,5), rep( 0 ,5), rep( 1 ,5), rep( 0 ,5), 
                 rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), 
                 rep( 0 ,5), rep( 0 ,5), rep( 1 ,5), rep( 0 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5))
ggraph(mytree_8, layout = 'dendrogram', circular = TRUE) + 
  geom_conn_bundle(data = get_con(from = from_8, to = to_8), alpha=0.5, width=1.1, tension=0, aes(edge_colour =  relations_8) ) +
  scale_edge_color_continuous(low="hotpink3", high="lightskyblue") +
  geom_node_text(aes(x = x*1.05, y=y*1.05, filter = leaf, label=name, angle = angle, hjust=hjust, colour=group), size=5, alpha=1) +
  geom_node_point(aes(filter = leaf, x = x*1.01, y=y*1.01, colour=group),size=8, alpha=0.85) +
  scale_colour_manual(values= rep( c("#A6CEE3", "#FB9A99" ,"#E31A1C" ,"#FDBF6F", "#FF7F00", "#CAB2D6") , 30)) +
  theme_void() + theme( legend.position="none")  +
  expand_limits(x = c(-1.4, 1.4), y = c(-1.4, 1.4))


#Liver enzymes BF = 1_Immune_cells, 2_Reticulocytes, 3_RBC, 4_Platelet (n=5)
d17 <- data.frame(from="origin", to=paste("group", seq(1,5), sep="") )
d18 <- data.frame( from = c("group1",rep("group3",6),rep("group4",2),"group3",rep("group4",2),rep("group1",10),rep("group2",7),rep("group5",4)),
                   to = c(colnames(NAFLD_LRBF_SD[c(2:11,13,12,14:30,76,79,81,84)])) )
(edges_9 <- rbind(d17, d18) %>% arrange(from))
set.seed(132)
(branches_9 <- unique(rbind(Sig_D_B_CC_BF %>% filter(str_detect(Marker1, 'GGT|ALP|ALT|AST|Gam|Asp|Alk|Ala')), 
                            Sig_D_B_CC_BF %>% filter(str_detect(Marker2, 'GGT|ALP|ALT|AST|Gam|Asp|Alk|Ala'))) %>% 
                        rename("from"="Marker1","to"="Marker2") %>% 
                        mutate(value_1 = min(-log(Bonferroni))/-log(Bonferroni)) %>%  mutate(value_BF = ifelse(Relationship == "Positive",1,0)) %>% 
                        select(from, to, Bonferroni, value_1, value_BF)))
trunk_9  <-  data.frame( name = unique(c(as.character(edges_9$from), as.character(edges_9$to))) )
trunk_9$group  <-  edges_9$from[ match( trunk_9$name, edges_9$to ) ]
trunk_9[c(7:39),] <- trunk_9[c(7:39),] %>% arrange(group)
trunk_9 <- trunk_9 %>% mutate(value = 0.1 )
trunk_9$id <- NA
myleafy <- which(is.na( match(trunk_9$name, edges_9$from) ))
nleafy <- length(myleafy)
trunk_9$id[ myleafy ] <- seq(1:nleafy)

trunk_9$name   <- gsub("_", " ", trunk_9$name )
trunk_9[trunk_9 == "Aspartate aminotransferase"] <- "AST"
trunk_9[trunk_9 == "Alanine aminotransferase"] <- "ALT"
trunk_9[trunk_9 == "Gamma glutamyltransferase"] <- "GGT"
trunk_9[trunk_9 == "Alkaline phosphatase"] <- "ALP"
trunk_9[trunk_9 == "White blood cell (leukocyte) count"] <- "WBC count"
trunk_9[trunk_9 == "Mean platelet (thrombocyte) volume"] <- "Mean platelet volume"
trunk_9[trunk_9 == "Red blood cell (erythrocyte) count"] <- "RBC count"
trunk_9[trunk_9 == "Red blood cell (erythrocyte) distribution width"] <- "RDW"
trunk_9[trunk_9 == "Mean corpuscular volume"] <- "MCV"
trunk_9[trunk_9 == "Red blood cell (erythrocyte) distribution width"] <- "RDW"
trunk_9[trunk_9 == "Mean corpuscular haemoglobin concentration"] <- "MCHC"
trunk_9[trunk_9 == "High light scatter reticulocyte count"] <- "HLSRC"
trunk_9[trunk_9 == "High light scatter reticulocyte percentage"] <- "HLSRP"
trunk_9[trunk_9 == "Mean corpuscular haemoglobin"] <- "Mean corpuscular\n haemoglobin"
trunk_9[trunk_9 == "Immature reticulocyte fraction"] <- "Immature reticulocyte\n fraction"
trunk_9[trunk_9 == "  "] <- " "
trunk_9[trunk_9 == "group1"] <- "1_Immune cells"
trunk_9[trunk_9 == "group2"] <- "2_Reticulocytes"
trunk_9[trunk_9 == "group3"] <- "3_RBC"
trunk_9[trunk_9 == "group5"] <- "6_Liver enzymes"
trunk_9[trunk_9 == "group4"] <- "4_Platelets"
trunk_9
edges_9$to   <- gsub("_", " ", edges_9$to )
edges_9[edges_9 == "Aspartate aminotransferase"] <- "AST"
edges_9[edges_9 == "Alanine aminotransferase"] <- "ALT"
edges_9[edges_9 == "Gamma glutamyltransferase"] <- "GGT"
edges_9[edges_9 == "Alkaline phosphatase"] <- "ALP"
edges_9[edges_9 == "White blood cell (leukocyte) count"] <- "WBC count"
edges_9[edges_9 == "Mean platelet (thrombocyte) volume"] <- "Mean platelet volume"
edges_9[edges_9 == "Mean corpuscular volume"] <- "MCV"
edges_9[edges_9 == "Red blood cell (erythrocyte) count"] <- "RBC count"
edges_9[edges_9 == "Red blood cell (erythrocyte) distribution width"] <- "RDW"
edges_9[edges_9 == "Mean corpuscular haemoglobin concentration"] <- "MCHC"
edges_9[edges_9 == "High light scatter reticulocyte count"] <- "HLSRC"
edges_9[edges_9 == "High light scatter reticulocyte percentage"] <- "HLSRP"
edges_9[edges_9 == "Mean corpuscular haemoglobin"] <- "Mean corpuscular\n haemoglobin"
edges_9[edges_9 == "Immature reticulocyte fraction"] <- "Immature reticulocyte\n fraction"
edges_9[edges_9 == "  "] <- " "
edges_9[edges_9 == "group1"] <- "1_Immune cells"
edges_9[edges_9 == "group2"] <- "2_Reticulocytes"
edges_9[edges_9 == "group3"] <- "3_RBC"
edges_9[edges_9 == "group5"] <- "6_Liver enzymes"
edges_9[edges_9 == "group4"] <- "4_Platelets"

branches_9$to   <- gsub("_", " ", branches_9$to )
branches_9$from   <- gsub("_", " ", branches_9$from )
branches_9[branches_9 == "Aspartate aminotransferase"] <- "AST"
branches_9[branches_9 == "Alanine aminotransferase"] <- "ALT"
branches_9[branches_9 == "Gamma glutamyltransferase"] <- "GGT"
branches_9[branches_9 == "Alkaline phosphatase"] <- "ALP"
branches_9[branches_9 == "White blood cell (leukocyte) count"] <- "WBC count"
branches_9[branches_9 == "Mean platelet (thrombocyte) volume"] <- "Mean platelet volume"
branches_9[branches_9 == "Mean corpuscular volume"] <- "MCV"
branches_9[branches_9 == "Red blood cell (erythrocyte) count"] <- "RBC count"
branches_9[branches_9 == "Red blood cell (erythrocyte) distribution width"] <- "RDW"
branches_9[branches_9 == "Mean corpuscular haemoglobin concentration"] <- "MCHC"
branches_9[branches_9 == "High light scatter reticulocyte count"] <- "HLSRC"
branches_9[branches_9 == "High light scatter reticulocyte percentage"] <- "HLSRP"
branches_9[branches_9 == "High light scatter reticulocyte percentage"] <- "HLSRP"
branches_9[branches_9 == "Mean corpuscular haemoglobin"] <- "Mean corpuscular\n haemoglobin"
branches_9[branches_9 == "  "] <- " "

trunk_9$angle <- -90 - 360 * (trunk_9$id-0.5) / nleafy
trunk_9$hjust <- ifelse( trunk_9$angle < -90, 1, 0)
#trunk_9$hjust <- ifelse( trunk_9$angle < -270, 0, trunk_9$hjust)
trunk_9$angle <- ifelse(trunk_9$angle < -90, trunk_9$angle+180, trunk_9$angle)
trunk_9$angle <- ifelse(trunk_9$angle < -90, trunk_9$angle+180, trunk_9$angle)
trunk_9[c(7:23),]$hjust <- 0
#trunk_9[c(8:16),]$hjust <- 1
trunk_9
mytree_9 <- igraph::graph_from_data_frame( edges_9, vertices=trunk_9 )
from_9  <-  match( branches_9$from, trunk_9$name)
to_9  <-  match( branches_9$to, trunk_9$name)
branches_9
ggraph(mytree_9, layout = 'dendrogram', circular = TRUE) + 
  geom_conn_bundle(data = get_con(from = from_9, to = to_9), alpha=0.5, width=2.4, tension=0, aes(edge_colour =  1) ) +
  scale_edge_color_continuous(low="lightskyblue", high="lightskyblue") +
  geom_node_text(aes(x = x*1.07, y=y*1.07, filter = leaf, label=name, angle = angle, hjust=hjust, colour=group), size=5.1, alpha=1) +
  geom_node_point(aes(filter = leaf, x = x*1.02, y=y*1.02, colour=group),size=6.9, alpha=0.9) +
  scale_colour_manual(values= rep( c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#E31A1C") , 30)) +
  theme_void() + theme( legend.position="none")  +
  expand_limits(x = c(-1.8, 1.8), y = c(-1.8, 1.8) )





#Liver enzymes W2 = 1_Immune_cells, 2_Reticulocytes, 3_RBC, 4_Platelet (n=16)
(branches_10 <- unique(rbind(Sig_D_B_CC_2 %>% filter(str_detect(Marker1, 'GGT|ALP|ALT|AST|Gam|Asp|Alk|Ala')), 
                            Sig_D_B_CC_2 %>% filter(str_detect(Marker2, 'GGT|ALP|ALT|AST|Gam|Asp|Alk|Ala'))) %>% 
                        rename("from"="Marker1","to"="Marker2") %>% 
                        mutate(value_1 = min(-log(Bonferroni))/-log(Bonferroni)) %>%  mutate(value_2 = ifelse(Relationship == "Positive",1,0)) %>% 
                        select(from, to, Bonferroni, value_1, value_2)))
branches_10$to   <- gsub("_", " ", branches_10$to )
branches_10$from   <- gsub("_", " ", branches_10$from )
branches_10[branches_10 == "Aspartate aminotransferase"] <- "AST"
branches_10[branches_10 == "Alanine aminotransferase"] <- "ALT"
branches_10[branches_10 == "Gamma glutamyltransferase"] <- "GGT"
branches_10[branches_10 == "Alkaline phosphatase"] <- "ALP"
branches_10[branches_10 == "White blood cell (leukocyte) count"] <- "WBC count"
branches_10[branches_10 == "Mean platelet (thrombocyte) volume"] <- "Mean platelet volume"
branches_10[branches_10 == "Mean corpuscular volume"] <- "MCV"
branches_10[branches_10 == "Red blood cell (erythrocyte) count"] <- "RBC count"
branches_10[branches_10 == "Red blood cell (erythrocyte) distribution width"] <- "RDW"
branches_10[branches_10 == "Mean corpuscular haemoglobin concentration"] <- "MCHC"
branches_10[branches_10 == "High light scatter reticulocyte count"] <- "HLSRC"
branches_10[branches_10 == "High light scatter reticulocyte percentage"] <- "HLSRP"
branches_10[branches_10 == "High light scatter reticulocyte percentage"] <- "HLSRP"
branches_10[branches_10 == "Mean corpuscular haemoglobin"] <- "Mean corpuscular\n haemoglobin"
branches_10[branches_10 == "  "] <- " "
from_10  <-  match( branches_10$from, trunk_9$name)
to_10  <-  match( branches_10$to, trunk_9$name)

branches_10$value_2.1 <- paste(paste('rep'), branches_10$value_2, paste(',5),'))
branches_10$value_2.1   <- gsub("rep", "rep(", branches_10$value_2.1)
branches_10
#noquote( branches_10$value_2.1  )
relations_10 <- c(rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), 
                  rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5))
ggraph(mytree_9, layout = 'dendrogram', circular = TRUE) + 
  geom_conn_bundle(data = get_con(from = from_10, to = to_10), alpha=0.5, width=2.7, tension=0, aes(edge_colour = relations_10 ) ) +
  scale_edge_color_continuous(low="hotpink", high="lightskyblue") +
  geom_node_text(aes(x = x*1.04, y=y*1.04, filter = leaf, label=name, angle = angle, hjust=hjust, colour=group), size=5.1, alpha=1) +
  geom_node_point(aes(filter = leaf, x = x*1, y=y*1, colour=group),size=7.75, alpha=0.9) +
  scale_colour_manual(values= rep( c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#E31A1C") , 30)) +
  theme_void() + theme( legend.position="none")  +
  expand_limits(x = c(-1.4, 1.4), y = c(-1.4, 1.4) )


#Liver enzymes C = 1_Immune_cells, 2_Reticulocytes, 3_RBC, 4_Platelet, 5_Diet(bread, coffee, fruit, lamb, cereal) (n=59)
d19 <- data.frame(from="origin", to=paste("group", seq(1,6), sep="") )
d20 <- data.frame( from = c("group1",rep("group3",6),rep("group4",2),"group3",rep("group4",2),rep("group1",10),rep("group2",7),rep("group5",19),
                            rep("group6",4)),
                   to = c(colnames(NAFLD_LRC_SD[c(2:30,33:35,40:44,50:57,59,60, 67,76,79,81,84)])) )
(edges_11 <- rbind(d19, d20) %>% arrange(from))
set.seed(132)
(branches_11 <- unique(rbind(Sig_D_B_CC_C %>% filter(str_detect(Marker1, 'GGT|ALP|ALT|AST|Gam|Asp|Alk|Ala')), 
                            Sig_D_B_CC_C %>% filter(str_detect(Marker2, 'GGT|ALP|ALT|AST|Gam|Asp|Alk|Ala'))) %>% 
                        rename("from"="Marker1","to"="Marker2") %>% 
                        mutate(value_1 = min(-log(Bonferroni))/-log(Bonferroni)) %>%  mutate(value_C = ifelse(Relationship == "Positive",1,0)) %>% 
                        select(from, to, Bonferroni, value_1, value_C)))
trunk_11  <-  data.frame( name = unique(c(as.character(edges_11$from), as.character(edges_11$to))) )
trunk_11$group  <-  edges_11$from[ match( trunk_11$name, edges_11$to ) ]
trunk_11[c(8:59),] <- trunk_11[c(8:59),] %>% arrange(group)
trunk_11 <- trunk_11 %>% mutate(value = 0.1 )
trunk_11$id <- NA
myleafy <- which(is.na( match(trunk_11$name, edges_11$from) ))
nleafy <- length(myleafy)
trunk_11$id[ myleafy ] <- seq(1:nleafy)

trunk_11$name   <- gsub("_", " ", trunk_11$name )
trunk_11$name   <- gsub(" direct", "-C", trunk_11$name )
trunk_11$name <- gsub(" cholesterol", "-C", trunk_11$name )
trunk_11$name <- gsub("Apolipoprotein ", "Apo-", trunk_11$name )
trunk_11[trunk_11 == "C reactive protein"] <- "CRP"
trunk_11[trunk_11 == "Aspartate aminotransferase"] <- "AST"
trunk_11[trunk_11 == "Alanine aminotransferase"] <- "ALT"
trunk_11[trunk_11 == "Gamma glutamyltransferase"] <- "GGT"
trunk_11[trunk_11 == "Alkaline phosphatase"] <- "ALP"
trunk_11[trunk_11 == "Glycated haemoglobin (HbA1c)"] <- "HbA1c"
trunk_11[trunk_11 == "Cholesterol"] <- "TC"
trunk_11[trunk_11 == "Triglycerides"] <- "TG"
trunk_11[trunk_11 == "White blood cell (leukocyte) count"] <- "WBC count"
trunk_11[trunk_11 == "Mean platelet (thrombocyte) volume"] <- "Mean platelet volume"
trunk_11[trunk_11 == "Red blood cell (erythrocyte) count"] <- "RBC count"
trunk_11[trunk_11 == "Red blood cell (erythrocyte) distribution width"] <- "RDW"
trunk_11[trunk_11 == "Mean corpuscular volume"] <- "MCV"
trunk_11[trunk_11 == "Red blood cell (erythrocyte) distribution width"] <- "RDW"
trunk_11[trunk_11 == "Mean corpuscular haemoglobin concentration"] <- "MCHC"
trunk_11[trunk_11 == "High light scatter reticulocyte count"] <- "HLSRC"
trunk_11[trunk_11 == "High light scatter reticulocyte percentage"] <- "HLSRP"
trunk_11[trunk_11 == "  "] <- " "
trunk_11[trunk_11 == "group1"] <- "1_Immune cells"
trunk_11[trunk_11 == "group2"] <- "2_Reticulocytes"
trunk_11[trunk_11 == "group3"] <- "3_RBC"
trunk_11[trunk_11 == "group4"] <- "4_Platelets"
trunk_11[trunk_11 == "group5"] <- "5_Diet"
trunk_11[trunk_11 == "group6"] <- "6_Liver enzymes"
trunk_11[trunk_11 == "group7"] <- "7_Lipids"
trunk_11[trunk_11 == "group8"] <- "8_Urine and kidney"
trunk_11[trunk_11 == "group9"] <- "9_Other biomarkers"


edges_11$to   <- gsub("_", " ", edges_11$to )
edges_11$to <- gsub(" direct", "-C", edges_11$to)
edges_11$to <- gsub(" cholesterol", "-C", edges_11$to)
edges_11$to   <- gsub("Apolipoprotein ", "Apo-", edges_11$to )
edges_11[edges_11 == "C reactive protein"] <- "CRP"
edges_11[edges_11 == "Aspartate aminotransferase"] <- "AST"
edges_11[edges_11 == "Alanine aminotransferase"] <- "ALT"
edges_11[edges_11 == "Gamma glutamyltransferase"] <- "GGT"
edges_11[edges_11 == "Alkaline phosphatase"] <- "ALP"
edges_11[edges_11 == "Glycated haemoglobin (HbA1c)"] <- "HbA1c"
edges_11[edges_11 == "Cholesterol"] <- "TC"
edges_11[edges_11 == "Triglycerides"] <- "TG"
edges_11[edges_11 == "White blood cell (leukocyte) count"] <- "WBC count"
edges_11[edges_11 == "Mean platelet (thrombocyte) volume"] <- "Mean platelet volume"
edges_11[edges_11 == "Mean corpuscular volume"] <- "MCV"
edges_11[edges_11 == "Red blood cell (erythrocyte) count"] <- "RBC count"
edges_11[edges_11 == "Red blood cell (erythrocyte) distribution width"] <- "RDW"
edges_11[edges_11 == "Mean corpuscular haemoglobin concentration"] <- "MCHC"
edges_11[edges_11 == "High light scatter reticulocyte count"] <- "HLSRC"
edges_11[edges_11 == "High light scatter reticulocyte percentage"] <- "HLSRP"
edges_11[edges_11 == "  "] <- " "
edges_11[edges_11 == "group1"] <- "1_Immune cells"
edges_11[edges_11 == "group2"] <- "2_Reticulocytes"
edges_11[edges_11 == "group3"] <- "3_RBC"
edges_11[edges_11 == "group4"] <- "4_Platelets"
edges_11[edges_11 == "group5"] <- "5_Diet"
edges_11[edges_11 == "group6"] <- "6_Liver enzymes"
edges_11[edges_11 == "group7"] <- "7_Lipids"
edges_11[edges_11 == "group8"] <- "8_Urine and kidney"
edges_11[edges_11 == "group9"] <- "9_Other biomarkers"

branches_11$to   <- gsub("_", " ", branches_11$to )
branches_11$to <- gsub(" direct", "-C", branches_11$to)
branches_11$to <- gsub(" cholesterol", "-C", branches_11$to)
branches_11$to   <- gsub("Apolipoprotein ", "Apo-", branches_11$to )
branches_11$from   <- gsub("_", " ", branches_11$from )
branches_11$from <- gsub(" direct", "-C", branches_11$from)
branches_11$from <- gsub(" cholesterol", "-C", branches_11$from)
branches_11$from   <- gsub("Apolipoprotein ", "Apo-", branches_11$from )
branches_11[branches_11 == "C reactive protein"] <- "CRP"
branches_11[branches_11 == "Aspartate aminotransferase"] <- "AST"
branches_11[branches_11 == "Alanine aminotransferase"] <- "ALT"
branches_11[branches_11 == "Gamma glutamyltransferase"] <- "GGT"
branches_11[branches_11 == "Alkaline phosphatase"] <- "ALP"
branches_11[branches_11 == "Glycated haemoglobin (HbA1c)"] <- "HbA1c"
branches_11[branches_11 == "Cholesterol"] <- "TC"
branches_11[branches_11 == "Triglycerides"] <- "TG"
branches_11[branches_11 == "White blood cell (leukocyte) count"] <- "WBC count"
branches_11[branches_11 == "Mean platelet (thrombocyte) volume"] <- "Mean platelet volume"
branches_11[branches_11 == "Mean corpuscular volume"] <- "MCV"
branches_11[branches_11 == "Red blood cell (erythrocyte) count"] <- "RBC count"
branches_11[branches_11 == "Red blood cell (erythrocyte) distribution width"] <- "RDW"
branches_11[branches_11 == "Mean corpuscular haemoglobin concentration"] <- "MCHC"
branches_11[branches_11 == "High light scatter reticulocyte count"] <- "HLSRC"
branches_11[branches_11 == "High light scatter reticulocyte percentage"] <- "HLSRP"
branches_11[branches_11 == "  "] <- " "

trunk_11$angle <- -75 - 360 * (trunk_11$id-0.5) / nleafy
trunk_11$hjust <- ifelse( trunk_11$angle < -90, 1, 0)
#trunk_11$hjust <- ifelse( trunk_11$angle < -270, 0, trunk_11$hjust)
trunk_11$angle <- ifelse(trunk_11$angle < -90, trunk_11$angle+180, trunk_11$angle)
trunk_11$angle <- ifelse(trunk_11$angle < -90, trunk_11$angle+180, trunk_11$angle)
trunk_11[c(10:35),]$hjust <- 0
trunk_11[c(8:9),]$hjust <- 1
trunk_11
edges_11
mytree_11 <- igraph::graph_from_data_frame( edges_11, vertices=trunk_11 )
from_11  <-  match( branches_11$from, trunk_11$name)
to_11  <-  match( branches_11$to, trunk_11$name)
branches_11$value_C.1 <- paste(paste('rep'), branches_11$value_C, paste(',5),'))
branches_11$value_C.1   <- gsub("rep", "rep(", branches_11$value_C.1)
branches_11
noquote( branches_11$value_C.1  )
relations_11 <- c(rep( 0 ,5), rep( 1 ,5), rep( 0 ,5), rep( 0 ,5), rep( 0 ,5), rep( 1 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), 
                  rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), 
                  rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), 
                  rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), 
                  rep( 0 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 1 ,5), rep( 0 ,5), rep( 0 ,5), rep( 1 ,5), 
                  rep( 0 ,5), rep( 0 ,5), rep( 1 ,5), rep( 1 ,5))
ggraph(mytree_11, layout = 'dendrogram', circular = TRUE) + 
  geom_conn_bundle(data = get_con(from = from_11, to = to_11), alpha=0.5, width=1.1, tension=0, aes(edge_colour =  relations_11) ) +
  scale_edge_color_continuous(low="hotpink3", high="lightskyblue") +
  geom_node_text(aes(x = x*1.05, y=y*1.05, filter = leaf, label=name, angle = angle, hjust=hjust, colour=group), size=4.8, alpha=1) +
  geom_node_point(aes(filter = leaf, x = x*1.02, y=y*1.02, colour=group),size=8, alpha=0.85) +
  scale_colour_manual(values= rep( c("#A6CEE3", "#1F78B4", "#B2DF8A" ,"#33A02C", "#FB9A99" ,"#E31A1C" ) , 30)) +
  theme_void() + theme( legend.position="none")  +
  expand_limits(x = c(-1.4, 1.4), y = c(-1.4, 1.4))





