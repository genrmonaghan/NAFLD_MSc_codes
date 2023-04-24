
Cell_NAFLD_LRAF_SDP <- Cell_NAFLD_LRAF_SD
Cell_NAFLD_LRBF_SDP <- Cell_NAFLD_LRBF_SD
Cell_NAFLD_LRC_SDP <- Cell_NAFLD_LRC_SD
Cell_NAFLD_LR2_SDP <- Cell_NAFLD_LR2_SD

Cell_NAFLD_LR2_SDP$Participants <- "Within_2"
Cell_NAFLD_LRAF_SDP$Participants <- "After"
Cell_NAFLD_LRBF_SDP$Participants <- "Before"
Cell_NAFLD_LRC_SDP$Participants <- "Control"
Cell_NAFLD <- rbind(Cell_NAFLD_LR2_SDP, Cell_NAFLD_LRAF_SDP, Cell_NAFLD_LRBF_SDP, Cell_NAFLD_LRC_SDP)
Cell_NAFLD$Participants <- ordered(Cell_NAFLD$Participants,
                                  levels = c("Control", "After", "Within_2", "Before"))

Results_Cell_1 <- data.frame(c(1), c(1), c(1))
colnames(Results_Cell_1) <- c("p_value","Group","Name")
set.seed(132)
for (i in names(Cell_NAFLD[,c(-30)])) {
  aov <- data.frame(TukeyHSD(aov(Cell_NAFLD[[i]] ~ Cell_NAFLD[["Participants"]]))$`Cell_NAFLD[["Participants"]]`)
  aov1 <- aov %>%
    add_column(Group = rownames(aov)) %>%
    rename("p_value" = "p.adj") %>%
    add_column(Name = paste0(i)) %>%
    select(-c(1,2,3))
  Results_Cell_1 <- rbind(Results_Cell_1, aov1)
}
rownames(Results_Cell_1) <- NULL
Results_Cell_2 <- Results_Cell_1[c(-1),][order(Results_Cell_1$p_value),]
Results_Cell_2$p_value <- as.numeric(Results_Cell_2$p_value)
Results_Cell_2_1_Sig <- Results_Cell_2 %>%
  mutate(Significance = ifelse(p_value < 0.05, "Significant", "Not significant" )) %>%
  filter(Significance == "Significant")
Results_Cell_Sig  <- Results_Cell_2_1_Sig[,c(-4)][order(Results_Cell_2_1_Sig$p_value),]
rownames(Results_Cell_Sig) <- NULL
Results_Cell_Sig

Results_Cell_Sig %>% filter(str_detect(Name, 'crit|distribution')) %>% arrange(Name) %>% filter(!str_detect(Name, 'Platelet_d')) 
table(Results_Cell_Sig$Name) %>% as.data.frame() %>% arrange(desc(Freq))
table(Results_Cell_Sig$Group) %>% as.data.frame() %>% arrange(desc(Freq))
Results_Cell_Sigb <- Results_Cell_Sig %>% filter(str_detect(Group, 'Control'))
table(Results_Cell_Sigb$Name) %>%  as.data.frame() %>%   arrange(desc(Freq))
Results_Cell_Sigc <- Results_Cell_Sig %>% filter(p_value < 0.01)
table(Results_Cell_Sigc$Name) %>%  as.data.frame() %>%  arrange(desc(Freq))

Results_Bio_Cell_Sig <- rbind(Results_Cell_Sig, Results_Bio_Sig)
write.csv( Results_Bio_Cell_Sig, "University/MSc/Files/Supp Table 3.csv")


#Results
NMB_Cells_1 <- data.frame(c(1), c(2), c(3), c(4), c(5), c(6), c(7), c(8), c(9) )
colnames(NMB_Cells_1) <- c("Participants","Marker", "mean", "L_CI", "U_CI", "sd", "median", "LQ", "UQ")
set.seed(132)
for (i in unique(Results_Cell_Sig$Name) ) { 
  for(p in unique(Cell_NAFLD$Participants)) {
    NMB_Cells_1 <- rbind(NMB_Cells_1, (
      data.frame( Participants = p, Marker = i,  
                  mean = mean((Cell_NAFLD%>%filter(Participants == p))[[i]], na.rm = TRUE), 
                  L_CI = mean((Cell_NAFLD%>%filter(Participants == p))[[i]], na.rm = TRUE)-(qnorm(0.975)*sd((Cell_NAFLD%>%filter(Participants == p))[[i]], na.rm = TRUE)/sqrt(nrow(data.frame( (Cell_NAFLD %>% filter(Participants == p) )[[i]] ) ) )),
                  U_CI = mean((Cell_NAFLD%>%filter(Participants == p))[[i]], na.rm = TRUE)+qnorm(0.975)*sd((Cell_NAFLD%>%filter(Participants == p))[[i]], na.rm = TRUE)/sqrt(nrow(data.frame( (Cell_NAFLD %>% filter(Participants == p) )[[i]] ) ) ), 
                  sd = sd((Cell_NAFLD%>%filter(Participants == p))[[i]], na.rm = TRUE), 
                  median = median((Cell_NAFLD%>%filter(Participants == p))[[i]], na.rm = TRUE), 
                  LQ = quantile((Cell_NAFLD%>%filter(Participants == p))[[i]], probs = c(0.25), na.rm = TRUE), 
                  UQ = quantile((Cell_NAFLD%>%filter(Participants == p))[[i]], probs = c(0.75), na.rm = TRUE)) 
    ) ) }}
rownames(NMB_Cells_1) <- NULL
(NMB_Cells <- NMB_Cells_1[-c(1),] )
NMB_Cells$Participants <- ordered(NMB_Cells$Participants, levels = c("Control", "After", "Within_2", "Before"))
NMB_Cells

#Normal range
NMR_Cell <- data.frame((table(Results_Cell_Sig$Name) %>% as.data.frame() %>% arrange(desc(Freq)) %>% select(Var1)),
                       c(0.22, 11.8, 0.5, 150, 8.3,0.02,0.11, 0,  0,0.02, 0.5,  0,  0,27.5, 80,0,2,2,3.8,4.5,0.5,0,13.2,1), 
                       c(0.24, 16.1, 2.5, 400,56.6,0.08,0.38, 0,  1,0.08, 2.5,0.2,0.5,33.2,100,0,8,8,5.9, 11,  1,6,16.6,4), 
                       c("%",  "%",  "%","x10^9 cells/L","%","x10^12 cells/L","ratio","fl","x10^9 cells/L","x10^12 cells/L", "%","x10^9 cells/L","x10^9 cells/L",
                         "pg", "fl", "fl", "%","x10^9 cells/L","x10^12 cells/L", "x10^9 cells/L", "%", "%", "g/dL", "x10^9 cells/L")  )
colnames(NMR_Cell) <- c("Marker", "lower", "upper", "units")
NMR_Cell %>% arrange(Marker)

#Worth investigating
Worthy2 <- data.frame(c(1), c(0))
colnames(Worthy2) <- c("Marker" ,"Useful")
set.seed(132)
Worthy <- Worthy2
Worthy3 <- Worthy2
Worthy4 <- Worthy2
for (i in NMR_Cell$Marker){
  #Control more than lower bound of normal range
  if (  as.numeric((NMR_Cell %>% filter(Marker == i)  )$lower ) < 
        as.numeric((NMB_Cells %>% filter(Marker == i) %>% filter(Participants == "Control") ) $mean )    ) { y <- data.frame(i, 1) 
  } else { y <- data.frame(i, 0)} 
  colnames(y)  <- c("Marker" ,"Useful")
  Worthy2 <- rbind(Worthy2, y)
  Worthy2 <- Worthy2 %>% filter(Useful == 1)
}
for (i in NMR_Cell$Marker){
  #Control less than upper bound of normal range
  if (  as.numeric((NMR_Cell %>% filter(Marker == i)  )$upper ) > 
        as.numeric((NMB_Cells %>% filter(Marker == i) %>% filter(Participants == "Control") ) $mean )    ) { y <- data.frame(i, 1) 
  } else { y <- data.frame(i, 0)} 
  colnames(y)  <- c("Marker" ,"Useful")
  Worthy3 <- rbind(Worthy3, y) 
  Worthy3 <- Worthy3 %>% filter(Useful == 1)
}
for (i in NMR_Cell$Marker){
  #no overlap between NAFLD and normal range
  if (  as.numeric((NMR_Cell %>% filter(Marker == i)  )$upper ) < 
        as.numeric(max( (NMB_Cells %>% filter(Marker == i) %>% filter(!Participants == "Control") ) $mean ))       ) { z <- data.frame(i, 1) 
  } else if( as.numeric((NMR_Cell %>% filter(Marker == i)  )$lower ) > 
             as.numeric(min( (NMB_Cells %>% filter(Marker == i) %>% filter(!Participants == "Control") ) $mean ))  ) { z <- data.frame(i, 1) 
  } else { z <- data.frame(i, 0)} 
  colnames(z)  <- c("Marker" ,"Useful")
  Worthy4 <- rbind(Worthy4, z) 
  Worthy4 <- Worthy4 %>% filter(Useful == 1)
}
for (i in unique(Results_Cell_Sig$Name) ) {
  #no overlap between control and NAFLD
  if (min(NMB_Cells %>% filter(str_detect(Marker, i)) %>% filter(!Participants == "Control")  %>% select(L_CI) ) >
      max(NMB_Cells %>% filter(str_detect(Marker, i)) %>% filter(Participants == "Control")  %>% select(U_CI) ) ) { x <- data.frame(i, TRUE)
  } else if (max(NMB_Cells %>% filter(str_detect(Marker, i)) %>% filter(!Participants == "Control")  %>% select(U_CI) ) <
             min(NMB_Cells %>% filter(str_detect(Marker, i)) %>% filter(Participants == "Control")  %>% select(L_CI) ) ) { x <- data.frame(i, TRUE)
  } else { x <- data.frame(i, FALSE) }
  colnames(x)  <- c("Marker" ,"Useful")
  Worthy <- rbind(Worthy, x)
}
Wisely <- Worthy %>% filter(Useful == 1) %>% select(Marker)
Worthiness <- inner_join(inner_join(Worthy2, Worthy3)[c(1)], inner_join(Worthy4[c(1)], Wisely))
NMB_Cells %>% filter(Marker == c(Worthiness) ) %>% arrange(mean)
rbind( NMR_Cell %>% filter(Marker == c(inner_join(Worthy2, Worthy3)[c(7),c(1)]) )  , 
       NMR_Cell %>% filter(Marker == c(inner_join(Worthy2, Worthy3)[c(1),c(1)]) )  ,
       NMR_Cell %>% filter(Marker == c(inner_join(Worthy2, Worthy3)[c(20),c(1)]) ) )
rbind( NMB_Cells %>% filter(Marker == c(inner_join(Worthy2, Worthy3)[c(7),c(1)]) ) %>% arrange(mean) , 
       NMB_Cells %>% filter(Marker == c(inner_join(Worthy2, Worthy3)[c(1),c(1)]) ) %>% arrange(mean) ,
       NMB_Cells %>% filter(Marker == c(inner_join(Worthy2, Worthy3)[c(20),c(1)]) ) %>% arrange(mean) )

NMR_Cell %>% filter(Marker == c(inner_join(Worthy2, Worthy3)[c(7),c(1)]) )
NMB_Cells %>% filter(Marker == c(inner_join(Worthy2, Worthy3)[c(7),c(1)]) ) %>% arrange(mean)


library(RColorBrewer)
myCol <- brewer.pal(5, "Pastel2")
myCol2 <- brewer.pal(4, "Set2")
mycomparisons_1 <- list( c("Control", "Within_2"),  c("Control", "After"), c("Control", "All"), c("Control", "Before"), c("Within_2", "All"),
                         c("Within_2", "After"), c("Before", "All"), c("After", "Before"), c("Within_2", "Before")  )
mycomparisons_2 <- list( c("Control", "Within_2"),  c("Control", "After"), c("Control", "Before"), c("Within_2", "After"),  c("After", "Before"),
                         c("Within_2", "Before"))


# Platelet_crit 
ggboxplot(Cell_NAFLD, x = "Participants", y = "Platelet_crit", color = "Participants", alpha=0.4, fill = "Participants",
          palette = mycol3,  ylab = "Platelet crit volume (%)", xlab = "Cohort", width = 0.75) +
  stat_compare_means(comparisons = mycomparisons_2, method = "t.test", paired = FALSE) +
  ggtitle("Platelet crit")+ theme(legend.position="none") +
  theme(plot.title = element_text(hjust = 0.5)) + #ylim(0,290) +
  scale_x_discrete(labels = c(paste0("Control \n (n=7307)"), paste0("D2AF \n (n=4210)"),paste0("DW2 \n (n=450)"), paste0("D2BF \n (n=326)")))


#Red_blood_cell_(erythrocyte)_distribution_width
ggboxplot(Cell_NAFLD, x = "Participants", y = "Red_blood_cell_(erythrocyte)_distribution_width", color = "Participants", alpha=0.4, fill = "Participants",
          palette = mycol3,  ylab = "RDW (%)", xlab = "Cohort", width = 0.75) +
  stat_compare_means(comparisons = mycomparisons_2, method = "t.test", paired = FALSE) +
  ggtitle("Red blood cell distribution width")+ theme(legend.position="none") +
  theme(plot.title = element_text(hjust = 0.5)) + #ylim(0,290) +
  scale_x_discrete(labels = c(paste0("Control \n (n=7307)"), paste0("D2AF \n (n=4210)"),paste0("DW2 \n (n=450)"), paste0("D2BF \n (n=326)")))



#High_light_scatter_reticulocyte_percentage
ggboxplot(Cell_NAFLD, x = "Participants", y = "High_light_scatter_reticulocyte_percentage", color = "Participants", 
          palette = mycol3, fill = mycol3.5, ylab = "High_light_scatter_reticulocyte_percentage (x 10^9 per liter)", 
          xlab = "Participant group", width = 0.7) +
  stat_compare_means(comparisons = mycomparisons_2, method = "t.test", paired = FALSE) +
  ggtitle("High_light_scatter_reticulocyte_percentage")+
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_discrete(labels = c(paste0("Control \n (n=7307)"), paste0("NAFLD diagnosed \n >2 years after \n (n=4210)"),
                              paste0("NAFLD diagnosed \n within +- 2 years \n (n=450)"), paste0("NAFLD diagnosed \n >2 years before \n (n=326)")))


#Platelet_count
ggboxplot(Cell_NAFLD, x = "Participants", y = "Platelet_count", color = "Participants", 
          palette = mycol3, fill = mycol3.5, ylab = "Platelet count (x 10^9 per liter)", xlab = "Participant group", width = 0.7) +
  stat_compare_means(comparisons = mycomparisons_2, method = "t.test", paired = FALSE) +
  ggtitle("Platelet count")+
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_discrete(labels = c(paste0("Control \n (n=7307)"), paste0("NAFLD diagnosed \n >2 years after \n (n=4210)"),
                              paste0("NAFLD diagnosed \n within +- 2 years \n (n=450)"), paste0("NAFLD diagnosed \n >2 years before \n (n=326)")))


#Monocyte Count
ggboxplot(Cell_NAFLD, x = "Participants", y = "Monocyte_count", color = "Participants", 
          palette = mycol3, fill = mycol3.5, ylab = "Monocyte count (x10^9/L)", xlab = "Participant group", width = 0.7) +
  stat_compare_means(comparisons = mycomparisons_2, method = "t.test", paired = FALSE) +
  ggtitle("Monocyte count") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_x_discrete(labels = c(paste0("Control \n (n=7307)"), paste0("NAFLD diagnosed \n >2 years after \n (n=4210)"),
                              paste0("NAFLD diagnosed \n within +- 2 years \n (450)"), paste0("NAFLD diagnosed \n >2 years before \n (n=326)")))
