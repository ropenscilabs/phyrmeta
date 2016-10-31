### Functions to run meta-analysis in Phylometa
# 1 group in the moderator variable
maketables_1group <- function(output){ #DONE IF/ELSE REFORMATTING

  #Table 1, traditional
  #a1<-unlist(strsplit(output[6]," +"))[-1] #Table header
  a2 <- unlist(strsplit(output[8],"  +"))[-1] #Data row 1, between groups, fixed effects
  a3 <- unlist(strsplit(output[9],"  +"))[-1] #Data row 2, within groups
  a4 <- if(length(unlist(strsplit(output[10]," +"))[-1]) == 6) {c(paste(unlist(strsplit(output[10]," +"))[-1][1],unlist(strsplit(output[10]," +"))[-1][2],unlist(strsplit(output[10]," +"))[-1][3]),unlist(strsplit(output[10]," +"))[-1][4:6])} else
    if(length(unlist(strsplit(output[10]," +"))[-1]) == 8) {c(paste(unlist(strsplit(output[10]," +"))[-1][1],unlist(strsplit(output[10]," +"))[-1][2],unlist(strsplit(output[10]," +"))[-1][3]),unlist(strsplit(output[10]," +"))[-1][4:8])} else
      stats::end
  a7 <- unlist(strsplit(output[11]," +"))[-1] #Data row 6, Total
  a8 <- unlist(strsplit(output[14],"  +"))[-1] #Data row 7, between groups, random effects
  rownaames<-c(a2[1],a3[1],a4[1],a7[1],a8[1]) #make vector of first column names
  numbers<-t(data.frame(as.numeric(a2[2:4]),as.numeric(a3[2:4]),as.numeric(a4[2:4]),append(as.numeric(a7[2]),c(-9999,-9999),after=1),as.numeric(a8[2:4]))) #make data frame of numbers
  numbers<-data.frame(numbers,rownaames) #Make data frame of row names and numbers
  colnames(numbers)<-c("Q","df","P","Source") #Assign column names
  summaryfitstats_table1_trad<-data.frame(Source=numbers$Source,Q=numbers$Q,df=numbers$df,P=numbers$P) #Reorder columns
  #summaryfitstats_table1_trad #See table

  #Table 2, traditional
  #b1<-unlist(strsplit(output[23]," +"))[-1] #Table header
  b2 <- if(length(unlist(strsplit(output[27]," +"))[-1]) == 9) {c(paste((unlist(strsplit(output[27]," +"))[-1])[1],(unlist(strsplit(output[27]," +"))[-1])[2]),unlist(strsplit(output[27]," +"))[-1][3:9])} else
    if(stringr::str_detect(unlist(strsplit(output[27]," +"))[-1][6], ")") == "TRUE") {c(unlist(strsplit(output[27],"  +"))[-1][1:4], paste(stringr::str_split(unlist(strsplit(output[27]," +"))[-1][6], ")")[[1]][1],")", sep=""), stringr::str_split(unlist(strsplit(output[27]," +"))[-1][6], ")")[[1]][2], unlist(strsplit(output[27],"  +"))[-1][6:7])} else
      stats::end
  b3 <- if(length(unlist(strsplit(output[28]," +"))[-1]) == 9) {c(paste((unlist(strsplit(output[28]," +"))[-1])[1],(unlist(strsplit(output[28]," +"))[-1])[2]),unlist(strsplit(output[28]," +"))[-1][3:9])} else
    if(stringr::str_detect(unlist(strsplit(output[28]," +"))[-1][6], ")") == "TRUE") {c(unlist(strsplit(output[28],"  +"))[-1][1:4], paste(stringr::str_split(unlist(strsplit(output[28]," +"))[-1][6], ")")[[1]][1],")", sep=""), stringr::str_split(unlist(strsplit(output[28]," +"))[-1][6], ")")[[1]][2], unlist(strsplit(output[28],"  +"))[-1][6:7])} else
      stats::end
  b6 <- if(length(unlist(strsplit(output[32]," +"))[-1]) == 9) {c(paste((unlist(strsplit(output[32]," +"))[-1])[1],(unlist(strsplit(output[32]," +"))[-1])[2]),unlist(strsplit(output[32]," +"))[-1][3:9])} else
    if(stringr::str_detect(unlist(strsplit(output[32]," +"))[-1][6], ")") == "TRUE") {c(unlist(strsplit(output[32],"  +"))[-1][1:4], paste(stringr::str_split(unlist(strsplit(output[32]," +"))[-1][6], ")")[[1]][1],")", sep=""), stringr::str_split(unlist(strsplit(output[32]," +"))[-1][6], ")")[[1]][2], unlist(strsplit(output[32],"  +"))[-1][6:7])} else
      stats::end
  b7 <- if(length(unlist(strsplit(output[33]," +"))[-1]) == 9) {c(paste((unlist(strsplit(output[33]," +"))[-1])[1],(unlist(strsplit(output[33]," +"))[-1])[2]),unlist(strsplit(output[33]," +"))[-1][3:9])} else
    if(stringr::str_detect(unlist(strsplit(output[33]," +"))[-1][6], ")") == "TRUE") {c(unlist(strsplit(output[33],"  +"))[-1][1:4], paste(stringr::str_split(unlist(strsplit(output[33]," +"))[-1][6], ")")[[1]][1],")", sep=""), stringr::str_split(unlist(strsplit(output[33]," +"))[-1][6], ")")[[1]][2], unlist(strsplit(output[33],"  +"))[-1][6:7])} else
      stats::end
  rownaames1<-c(b2[1],b3[1],b6[1],b7[1]) #make vector of first column names
  b22<-CI_split(b2) #Split confidence interval numbers apart and reinsert to vector of data
  b33<-CI_split(b3)
  b66<-CI_split(b6)
  b77<-CI_split(b7)
  numbers1<-t(data.frame(as.numeric(b22),as.numeric(b33),as.numeric(b66),as.numeric(b77)))  #make data frame of numbers
  numbers1<-data.frame(numbers1,rownaames1) #Make data frame of row names and numbers
  colnames(numbers1)<-c("k","effsize","var","95CI_low","95CI_high","Z","df","P","Group") #Assign column names
  summaryeffsizes_table2_trad<-data.frame(Group=numbers1[,9],k=numbers1[,1],effsize=numbers1[,2],var=numbers1[,3],CI_low=numbers1[,4],CI_high=numbers1[,5],Z=numbers1[,6],df=numbers1[,7],P=numbers1[,8]) #Reorder columns
  #summaryeffsizes_table2_trad #See table

  #Table 1, phylogenetic
  #d1 <- unlist(strsplit(output[47]," +"))[-1] #Table header
  d2 <- unlist(strsplit(output[49],"  +"))[-1] #Data row 1, between groups, fixed effects
  d3 <- unlist(strsplit(output[50],"  +"))[-1] #Data row 2, within groups
  d4 <- if(length(unlist(strsplit(output[51]," +"))[-1]) == 6) {c(paste(unlist(strsplit(output[51]," +"))[-1][1],unlist(strsplit(output[51]," +"))[-1][2],unlist(strsplit(output[51]," +"))[-1][3]),unlist(strsplit(output[51]," +"))[-1][4:6])} else
    if(length(unlist(strsplit(output[51]," +"))[-1]) == 8) {c(paste(unlist(strsplit(output[51]," +"))[-1][1],unlist(strsplit(output[51]," +"))[-1][2],unlist(strsplit(output[51]," +"))[-1][3]),unlist(strsplit(output[51]," +"))[-1][4:8])} else
      stats::end
  d7 <- unlist(strsplit(output[52]," +"))[-1] #Data row 6, Total
  d8 <- unlist(strsplit(output[55],"  +"))[-1] #Data row 7, between groups, random effects
  rownaames2<-c(d2[1],d3[1],d4[1],d7[1],d8[1]) #make vector of first column names
  numbers2<-t(data.frame(append(as.numeric(d2[2:4]),c(-9999,-9999),after=3),as.numeric(d3[2:6]),as.numeric(d4[2:6]),append(as.numeric(d7[2]),c(-9999,-9999,-9999,-9999),after=1),append(as.numeric(d8[2:4]),c(-9999,-9999),after=3))) #make data frame of numbers
  numbers2<-data.frame(numbers2,rownaames2) #Make data frame of row names and numbers
  colnames(numbers2)<-c("Q","df","P","df_polytadj","P_polytadj","Source") #Assign column names
  summaryfitstats_table1_phyl<-data.frame(Source=numbers2[,6],Q=numbers2[,1],df=numbers2[,2],P=numbers2[,3],df_polytadj=numbers2[,4],P_polytadj=numbers2[,5]) #Reorder columns
  #summaryfitstats_table1_phyl #See table

  #Table 2, phylogenetic
  #e1<-unlist(strsplit(output[64]," +"))[-1] #Table header
  e2 <- if(length(unlist(strsplit(output[68]," +"))[-1]) == 9) {c(paste((unlist(strsplit(output[68]," +"))[-1])[1],(unlist(strsplit(output[68]," +"))[-1])[2]),unlist(strsplit(output[68]," +"))[-1][3:9])} else
    if(stringr::str_detect(unlist(strsplit(output[68]," +"))[-1][6], ")") == "TRUE") {c(unlist(strsplit(output[68],"  +"))[-1][1:4], paste(stringr::str_split(unlist(strsplit(output[68]," +"))[-1][6], ")")[[1]][1],")", sep=""), stringr::str_split(unlist(strsplit(output[68]," +"))[-1][6], ")")[[1]][2], unlist(strsplit(output[68],"  +"))[-1][6:7])} else
      stats::end
  e3 <- if(length(unlist(strsplit(output[69]," +"))[-1]) == 9) {c(paste((unlist(strsplit(output[69]," +"))[-1])[1],(unlist(strsplit(output[69]," +"))[-1])[2]),unlist(strsplit(output[69]," +"))[-1][3:9])} else
    if(stringr::str_detect(unlist(strsplit(output[69]," +"))[-1][6], ")") == "TRUE") {c(unlist(strsplit(output[69],"  +"))[-1][1:4], paste(stringr::str_split(unlist(strsplit(output[69]," +"))[-1][6], ")")[[1]][1],")", sep=""), stringr::str_split(unlist(strsplit(output[69]," +"))[-1][6], ")")[[1]][2], unlist(strsplit(output[69],"  +"))[-1][6:7])} else
      stats::end
  e6 <- if(length(unlist(strsplit(output[73]," +"))[-1]) == 9) {c(paste((unlist(strsplit(output[73]," +"))[-1])[1],(unlist(strsplit(output[73]," +"))[-1])[2]),unlist(strsplit(output[73]," +"))[-1][3:9])} else
    if(stringr::str_detect(unlist(strsplit(output[73]," +"))[-1][6], ")") == "TRUE") {c(unlist(strsplit(output[73],"  +"))[-1][1:4], paste(stringr::str_split(unlist(strsplit(output[73]," +"))[-1][6], ")")[[1]][1],")", sep=""), stringr::str_split(unlist(strsplit(output[73]," +"))[-1][6], ")")[[1]][2], unlist(strsplit(output[73],"  +"))[-1][6:7])} else
      stats::end
  e7 <- if(length(unlist(strsplit(output[74]," +"))[-1]) == 9) {c(paste((unlist(strsplit(output[74]," +"))[-1])[1],(unlist(strsplit(output[74]," +"))[-1])[2]),unlist(strsplit(output[74]," +"))[-1][3:9])} else
    if(stringr::str_detect(unlist(strsplit(output[74]," +"))[-1][6], ")") == "TRUE") {c(unlist(strsplit(output[74],"  +"))[-1][1:4], paste(stringr::str_split(unlist(strsplit(output[74]," +"))[-1][6], ")")[[1]][1],")", sep=""), stringr::str_split(unlist(strsplit(output[74]," +"))[-1][6], ")")[[1]][2], unlist(strsplit(output[74],"  +"))[-1][6:7])} else
      stats::end
  rownaames3<-c(e2[1],e3[1],e6[1],e7[1]) #make vector of first column names
  e22<-CI_split(e2) #Split confidence interval numbers apart and reinsert to vector of data
  e33<-CI_split(e3)
  e66<-CI_split(e6)
  e77<-CI_split(e7)
  numbers3<-t(data.frame(as.numeric(e22),as.numeric(e33),as.numeric(e66),as.numeric(e77)))  #make data frame of numbers
  numbers3<-data.frame(numbers3,rownaames3) #Make data frame of row names and numbers
  colnames(numbers3)<-c("k","effsize","var","95CI_low","95CI_high","Z","df","P","Group") #Assign column names
  summaryeffsizes_table2_phyl<-data.frame(Group=numbers3[,9],k=numbers3[,1],effsize=numbers3[,2],var=numbers3[,3],CI_low=numbers3[,4],CI_high=numbers3[,5],Z=numbers3[,6],df=numbers3[,7],P=numbers3[,8]) #Reorder columns
  #summaryeffsizes_table2_phyl #See table

  ### Model fit table, AIC
  f1<-unlist(strsplit(output[89]," +"))[-1] #Table header
  f2<-unlist(strsplit(output[90]," +"))[-1] #Data row 1, fixed effects, all studies
  rownaames4<-c(f1[1],f2[1]) #make vector of first column names
  numbers4<-t(data.frame(as.numeric(f1[2:3]),as.numeric(f2[2:3])))  #make data frame of numbers
  numbers4<-data.frame(numbers4,rownaames4) #Make data frame of row names and numbers
  colnames(numbers4)<-c("AIC_fixed","AIC_random","Analysis") #Assign column names
  summarymodelfits_AIC_table<-data.frame(Analysis=numbers4[,3],AIC_fixed=numbers4[,1],AIC_random=numbers4[,2]) #Reorder columns
  #summarymodelfits_table #See table

  ### Model fit table, -2(likelihood)
  g1 <- unlist(strsplit(output[101]," +"))[-1] #Traditional AIC data
  g2 <- unlist(strsplit(output[102]," +"))[-1] #Phylogenetically controlled AIC data
  rownaames5 <- c(g1[1],g2[1]) #make vector of first column names
  numbers5 <- t(data.frame(as.numeric(g1[2:3]), as.numeric(g2[2:3])))  #make data frame of numbers
  numbers5 <- data.frame(numbers5, rownaames5) #Make data frame of row names and numbers
  colnames(numbers5) <- c("twoln_fixed", "twoln_random", "Analysis") #Assign column names
  summarymodelfits_twoln_table <- data.frame(Analysis = numbers5[,3], twoln_fixed = numbers5[,1], twoln_random = numbers5[,2]) #Reorder columns
  #summarymodelfits_twoln_table #See table

  #Make list of tables
  tables <- list(summaryfitstats_table1_trad, summaryeffsizes_table2_trad, summaryfitstats_table1_phyl, summaryeffsizes_table2_phyl, summarymodelfits_AIC_table, summarymodelfits_twoln_table)
  tables
}

# 2 groups in the moderator variable
maketables_2group <- function(output){ #DONE IF/ELSE REFORMATTING
  #Table 1, Traditional meta-analysis
  a1 <- unlist(strsplit(output[6]," +"))[-1] #Table header
  a2 <- unlist(strsplit(output[8],"  +"))[-1] #Data row 1, between groups, fixed effects
  a3 <- unlist(strsplit(output[9],"  +"))[-1] #Data row 2, within groups
  a4 <- if(length(unlist(strsplit(output[10]," +"))[-1]) == 6) {c(paste(unlist(strsplit(output[10]," +"))[-1][1],unlist(strsplit(output[10]," +"))[-1][2],unlist(strsplit(output[10]," +"))[-1][3]),unlist(strsplit(output[10]," +"))[-1][4:6])} else
    if(length(unlist(strsplit(output[10]," +"))[-1]) == 8) {c(paste(unlist(strsplit(output[10]," +"))[-1][1],unlist(strsplit(output[10]," +"))[-1][2],unlist(strsplit(output[10]," +"))[-1][3]),unlist(strsplit(output[10]," +"))[-1][4:8])} else
      stats::end
  a5 <- if(length(unlist(strsplit(output[11]," +"))[-1]) == 6) {c(paste(unlist(strsplit(output[11]," +"))[-1][1],unlist(strsplit(output[11]," +"))[-1][2],unlist(strsplit(output[11]," +"))[-1][3]),unlist(strsplit(output[11]," +"))[-1][4:6])} else
    if(length(unlist(strsplit(output[11]," +"))[-1]) == 8) {c(paste(unlist(strsplit(output[11]," +"))[-1][1],unlist(strsplit(output[11]," +"))[-1][2],unlist(strsplit(output[11]," +"))[-1][3]),unlist(strsplit(output[11]," +"))[-1][4:8])} else
      stats::end
  a7 <- unlist(strsplit(output[12]," +"))[-1] #Data row 6, Total
  a8 <- unlist(strsplit(output[15]," +"))[-1] #Data row 7, between groups, random effects
  rownaames<-c(a2[1],a3[1],a4[1],a5[1],a7[1],a8[1]) #make vector of first column names
  numbers<-t(data.frame(as.numeric(a2[2:4]),as.numeric(a3[2:4]),as.numeric(a4[2:4]),as.numeric(a5[2:4]),append(as.numeric(a7[2]),c(-9999,-9999),after=1),as.numeric(a8[2:4]))) #make data frame of numbers
  numbers<-data.frame(numbers,rownaames) #Make data frame of row names and numbers
  colnames(numbers)<-c("Q","df","P","Source") #Assign column names
  summaryfitstats_table1_trad<-data.frame(Source=numbers$Source,Q=numbers$Q,df=numbers$df,P=numbers$P) #Reorder columns
  #summaryfitstats_table1_trad #See table

  #Table 2, Traditional meta-analysis
  #b1<-unlist(strsplit(output[24]," +"))[-1] #Table header
  b2 <- if(length(unlist(strsplit(output[28]," +"))[-1]) == 9) {c(paste((unlist(strsplit(output[28]," +"))[-1])[1],(unlist(strsplit(output[28]," +"))[-1])[2]),unlist(strsplit(output[28]," +"))[-1][3:9])} else
    if(stringr::str_detect(unlist(strsplit(output[28]," +"))[-1][6], ")") == "TRUE") {c(unlist(strsplit(output[28],"  +"))[-1][1:4], paste(stringr::str_split(unlist(strsplit(output[28]," +"))[-1][6], ")")[[1]][1],")", sep=""), stringr::str_split(unlist(strsplit(output[28]," +"))[-1][6], ")")[[1]][2], unlist(strsplit(output[28],"  +"))[-1][6:7])} else
      stats::end
  b3 <- if(length(unlist(strsplit(output[29]," +"))[-1]) == 9) {c(paste((unlist(strsplit(output[29]," +"))[-1])[1],(unlist(strsplit(output[29]," +"))[-1])[2]),unlist(strsplit(output[29]," +"))[-1][3:9])} else
    if(stringr::str_detect(unlist(strsplit(output[29]," +"))[-1][6], ")") == "TRUE") {c(unlist(strsplit(output[29],"  +"))[-1][1:4], paste(stringr::str_split(unlist(strsplit(output[29]," +"))[-1][6], ")")[[1]][1],")", sep=""), stringr::str_split(unlist(strsplit(output[29]," +"))[-1][6], ")")[[1]][2], unlist(strsplit(output[29],"  +"))[-1][6:7])} else
      stats::end
  b4 <- if(length(unlist(strsplit(output[30]," +"))[-1]) == 9) {c(paste((unlist(strsplit(output[30]," +"))[-1])[1],(unlist(strsplit(output[30]," +"))[-1])[2]),unlist(strsplit(output[30]," +"))[-1][3:9])} else
    if(stringr::str_detect(unlist(strsplit(output[30]," +"))[-1][6], ")") == "TRUE") {c(unlist(strsplit(output[30],"  +"))[-1][1:4], paste(stringr::str_split(unlist(strsplit(output[30]," +"))[-1][6], ")")[[1]][1],")", sep=""), stringr::str_split(unlist(strsplit(output[30]," +"))[-1][6], ")")[[1]][2], unlist(strsplit(output[30],"  +"))[-1][6:7])} else
      stats::end
  b6 <- if(length(unlist(strsplit(output[34]," +"))[-1]) == 9) {c(paste((unlist(strsplit(output[34]," +"))[-1])[1],(unlist(strsplit(output[34]," +"))[-1])[2]),unlist(strsplit(output[34]," +"))[-1][3:9])} else
    if(stringr::str_detect(unlist(strsplit(output[34]," +"))[-1][6], ")") == "TRUE") {c(unlist(strsplit(output[34],"  +"))[-1][1:4], paste(stringr::str_split(unlist(strsplit(output[34]," +"))[-1][6], ")")[[1]][1],")", sep=""), stringr::str_split(unlist(strsplit(output[34]," +"))[-1][6], ")")[[1]][2], unlist(strsplit(output[34],"  +"))[-1][6:7])} else
      stats::end
  b7 <- if(length(unlist(strsplit(output[35]," +"))[-1]) == 9) {c(paste((unlist(strsplit(output[35]," +"))[-1])[1],(unlist(strsplit(output[35]," +"))[-1])[2]),unlist(strsplit(output[35]," +"))[-1][3:9])} else
    if(stringr::str_detect(unlist(strsplit(output[35]," +"))[-1][6], ")") == "TRUE") {c(unlist(strsplit(output[35],"  +"))[-1][1:4], paste(stringr::str_split(unlist(strsplit(output[35]," +"))[-1][6], ")")[[1]][1],")", sep=""), stringr::str_split(unlist(strsplit(output[35]," +"))[-1][6], ")")[[1]][2], unlist(strsplit(output[35],"  +"))[-1][6:7])} else
      stats::end
  b8 <- if(length(unlist(strsplit(output[36]," +"))[-1]) == 9) {c(paste((unlist(strsplit(output[36]," +"))[-1])[1],(unlist(strsplit(output[36]," +"))[-1])[2]),unlist(strsplit(output[36]," +"))[-1][3:9])} else
    if(stringr::str_detect(unlist(strsplit(output[36]," +"))[-1][6], ")") == "TRUE") {c(unlist(strsplit(output[36],"  +"))[-1][1:4], paste(stringr::str_split(unlist(strsplit(output[36]," +"))[-1][6], ")")[[1]][1],")", sep=""), stringr::str_split(unlist(strsplit(output[36]," +"))[-1][6], ")")[[1]][2], unlist(strsplit(output[36],"  +"))[-1][6:7])} else
      stats::end
  rownaames1<-c(b2[1],b3[1],b4[1],b6[1],b7[1],b8[1]) #make vector of first column names
  b22<-CI_split(b2) #Split confidence interval numbers apart and reinsert to vector of data
  b33<-CI_split(b3)
  b44<-CI_split(b4)
  b66<-CI_split(b6)
  b77<-CI_split(b7)
  b88<-CI_split(b8)
  numbers1<-t(data.frame(as.numeric(b22),as.numeric(b33),as.numeric(b44),as.numeric(b66),as.numeric(b77),as.numeric(b88)))  #make data frame of numbers
  numbers1<-data.frame(numbers1,rownaames1) #Make data frame of row names and numbers
  colnames(numbers1)<-c("k","effsize","var","95CI_low","95CI_high","Z","df","P","Group") #Assign column names
  summaryeffsizes_table2_trad<-data.frame(Group=numbers1[,9],k=numbers1[,1],effsize=numbers1[,2],var=numbers1[,3],CI_low=numbers1[,4],CI_high=numbers1[,5],Z=numbers1[,6],df=numbers1[,7],P=numbers1[,8]) #Reorder columns
  #summaryeffsizes_table2_trad #See table

  ###Phylogenetic meta-analysis
  #Table 1, Phylogenetic meta-analysis
  d1<-unlist(strsplit(output[50]," +"))[-1] #Table header
  d2<-unlist(strsplit(output[52],"  +"))[-1] #Data row 1, between groups, fixed effects
  d3<-unlist(strsplit(output[53],"  +"))[-1] #Data row 2, within groups
  d4 <- if(length(unlist(strsplit(output[54]," +"))[-1]) == 6) {c(paste(unlist(strsplit(output[54]," +"))[-1][1],unlist(strsplit(output[54]," +"))[-1][2],unlist(strsplit(output[54]," +"))[-1][3]),unlist(strsplit(output[54]," +"))[-1][4:6])} else
    if(length(unlist(strsplit(output[54]," +"))[-1]) == 8) {c(paste(unlist(strsplit(output[54]," +"))[-1][1],unlist(strsplit(output[54]," +"))[-1][2],unlist(strsplit(output[54]," +"))[-1][3]),unlist(strsplit(output[54]," +"))[-1][4:8])} else
      stats::end
  d5 <- if(length(unlist(strsplit(output[55]," +"))[-1]) == 6) {c(paste(unlist(strsplit(output[55]," +"))[-1][1],unlist(strsplit(output[55]," +"))[-1][2],unlist(strsplit(output[55]," +"))[-1][3]),unlist(strsplit(output[55]," +"))[-1][4:6])} else
    if(length(unlist(strsplit(output[55]," +"))[-1]) == 8) {c(paste(unlist(strsplit(output[55]," +"))[-1][1],unlist(strsplit(output[55]," +"))[-1][2],unlist(strsplit(output[55]," +"))[-1][3]),unlist(strsplit(output[55]," +"))[-1][4:8])} else
      stats::end
  d7<-unlist(strsplit(output[56]," +"))[-1] #Data row 6, Total
  d8<-unlist(strsplit(output[59],"  +"))[-1] #Data row 7, between groups, random effects
  rownaames2<-c(d2[1],d3[1],d4[1],d5[1],d7[1],d8[1]) #make vector of first column names
  numbers2<-t(data.frame(append(as.numeric(d2[2:4]),c(-9999,-9999),after=3),as.numeric(d3[2:6]),as.numeric(d4[2:6]),as.numeric(d5[2:6]),append(as.numeric(d7[2]),c(-9999,-9999,-9999,-9999),after=1),append(as.numeric(d8[2:4]),c(-9999,-9999),after=3))) #make data frame of numbers
  numbers2<-data.frame(numbers2,rownaames2) #Make data frame of row names and numbers
  colnames(numbers2)<-c("Q","df","P","df_polytadj","P_polytadj","Source") #Assign column names
  summaryfitstats_table1_phyl<-data.frame(Source=numbers2[,6],Q=numbers2[,1],df=numbers2[,2],P=numbers2[,3],df_polytadj=numbers2[,4],P_polytadj=numbers2[,5]) #Reorder columns
  #summaryfitstats_table1_phyl #See table

  #Table 2, Phylogenetic meta-analysis
  #e1<-unlist(strsplit(output[68]," +"))[-1] #Table header
  e2 <- if(length(unlist(strsplit(output[72]," +"))[-1]) == 9) {c(paste((unlist(strsplit(output[72]," +"))[-1])[1],(unlist(strsplit(output[72]," +"))[-1])[2]),unlist(strsplit(output[72]," +"))[-1][3:9])} else
    if(stringr::str_detect(unlist(strsplit(output[72]," +"))[-1][6], ")") == "TRUE") {c(unlist(strsplit(output[72],"  +"))[-1][1:4], paste(stringr::str_split(unlist(strsplit(output[72]," +"))[-1][6], ")")[[1]][1],")", sep=""), stringr::str_split(unlist(strsplit(output[72]," +"))[-1][6], ")")[[1]][2], unlist(strsplit(output[72],"  +"))[-1][6:7])} else
      stats::end
  e3 <- if(length(unlist(strsplit(output[73]," +"))[-1]) == 9) {c(paste((unlist(strsplit(output[73]," +"))[-1])[1],(unlist(strsplit(output[73]," +"))[-1])[2]),unlist(strsplit(output[73]," +"))[-1][3:9])} else
    if(stringr::str_detect(unlist(strsplit(output[73]," +"))[-1][6], ")") == "TRUE") {c(unlist(strsplit(output[73],"  +"))[-1][1:4], paste(stringr::str_split(unlist(strsplit(output[73]," +"))[-1][6], ")")[[1]][1],")", sep=""), stringr::str_split(unlist(strsplit(output[73]," +"))[-1][6], ")")[[1]][2], unlist(strsplit(output[73],"  +"))[-1][6:7])} else
      stats::end
  e4 <- if(length(unlist(strsplit(output[74]," +"))[-1]) == 9) {c(paste((unlist(strsplit(output[74]," +"))[-1])[1],(unlist(strsplit(output[74]," +"))[-1])[2]),unlist(strsplit(output[74]," +"))[-1][3:9])} else
    if(stringr::str_detect(unlist(strsplit(output[74]," +"))[-1][6], ")") == "TRUE") {c(unlist(strsplit(output[74],"  +"))[-1][1:4], paste(stringr::str_split(unlist(strsplit(output[74]," +"))[-1][6], ")")[[1]][1],")", sep=""), stringr::str_split(unlist(strsplit(output[74]," +"))[-1][6], ")")[[1]][2], unlist(strsplit(output[74],"  +"))[-1][6:7])} else
      stats::end
  e6 <- if(length(unlist(strsplit(output[78]," +"))[-1]) == 9) {c(paste((unlist(strsplit(output[78]," +"))[-1])[1],(unlist(strsplit(output[78]," +"))[-1])[2]),unlist(strsplit(output[78]," +"))[-1][3:9])} else
    if(stringr::str_detect(unlist(strsplit(output[78]," +"))[-1][6], ")") == "TRUE") {c(unlist(strsplit(output[78],"  +"))[-1][1:4], paste(stringr::str_split(unlist(strsplit(output[78]," +"))[-1][6], ")")[[1]][1],")", sep=""), stringr::str_split(unlist(strsplit(output[78]," +"))[-1][6], ")")[[1]][2], unlist(strsplit(output[78],"  +"))[-1][6:7])} else
      stats::end
  e7 <- if(length(unlist(strsplit(output[79]," +"))[-1]) == 9) {c(paste((unlist(strsplit(output[79]," +"))[-1])[1],(unlist(strsplit(output[79]," +"))[-1])[2]),unlist(strsplit(output[79]," +"))[-1][3:9])} else
    if(stringr::str_detect(unlist(strsplit(output[79]," +"))[-1][6], ")") == "TRUE") {c(unlist(strsplit(output[79],"  +"))[-1][1:4], paste(stringr::str_split(unlist(strsplit(output[79]," +"))[-1][6], ")")[[1]][1],")", sep=""), stringr::str_split(unlist(strsplit(output[79]," +"))[-1][6], ")")[[1]][2], unlist(strsplit(output[79],"  +"))[-1][6:7])} else
      stats::end
  e8 <- if(length(unlist(strsplit(output[80]," +"))[-1]) == 9) {c(paste((unlist(strsplit(output[80]," +"))[-1])[1],(unlist(strsplit(output[80]," +"))[-1])[2]),unlist(strsplit(output[80]," +"))[-1][3:9])} else
    if(stringr::str_detect(unlist(strsplit(output[80]," +"))[-1][6], ")") == "TRUE") {c(unlist(strsplit(output[80],"  +"))[-1][1:4], paste(stringr::str_split(unlist(strsplit(output[80]," +"))[-1][6], ")")[[1]][1],")", sep=""), stringr::str_split(unlist(strsplit(output[80]," +"))[-1][6], ")")[[1]][2], unlist(strsplit(output[80],"  +"))[-1][6:7])} else
      stats::end
  rownaames3<-c(e2[1],e3[1],e4[1],e6[1],e7[1],e8[1]) #make vector of first column names
  e22<-CI_split(e2) #Split confidence interval numbers apart and reinsert to vector of data
  e33<-CI_split(e3)
  e44<-CI_split(e4)
  e66<-CI_split(e6)
  e77<-CI_split(e7)
  e88<-CI_split(e8)
  numbers3<-t(data.frame(as.numeric(e22),as.numeric(e33),as.numeric(e44),as.numeric(e66),as.numeric(e77),as.numeric(e88)))  #make data frame of numbers
  numbers3<-data.frame(numbers3,rownaames3) #Make data frame of row names and numbers
  colnames(numbers3)<-c("k","effsize","var","95CI_low","95CI_high","Z","df","P","Group") #Assign column names
  summaryeffsizes_table2_phyl<-data.frame(Group=numbers3[,9],k=numbers3[,1],effsize=numbers3[,2],var=numbers3[,3],CI_low=numbers3[,4],CI_high=numbers3[,5],Z=numbers3[,6],df=numbers3[,7],P=numbers3[,8]) #Reorder columns
  #summaryeffsizes_table2_phyl #See table

  ### Model fit table, AIC
  f1<-unlist(strsplit(output[95]," +"))[-1] #Traditional AIC data
  f2<-unlist(strsplit(output[96]," +"))[-1] #Phylogenetically controlled AIC data
  rownaames4<-c(f1[1],f2[1]) #make vector of first column names
  numbers4<-t(data.frame(as.numeric(f1[2:3]),as.numeric(f2[2:3])))  #make data frame of numbers
  numbers4<-data.frame(numbers4,rownaames4) #Make data frame of row names and numbers
  colnames(numbers4)<-c("AIC_fixed","AIC_random","Analysis") #Assign column names
  summarymodelfits_AIC_table<-data.frame(Analysis=numbers4[,3],AIC_fixed=numbers4[,1],AIC_random=numbers4[,2]) #Reorder columns
  #summarymodelfits_AIC_table #See table

  ### Model fit table, -2(likelihood)
  g1 <- unlist(strsplit(output[107]," +"))[-1] #Traditional AIC data
  g2 <- unlist(strsplit(output[108]," +"))[-1] #Phylogenetically controlled AIC data
  rownaames5 <- c(g1[1],g2[1]) #make vector of first column names
  numbers5 <- t(data.frame(as.numeric(g1[2:3]), as.numeric(g2[2:3])))  #make data frame of numbers
  numbers5 <- data.frame(numbers5, rownaames5) #Make data frame of row names and numbers
  colnames(numbers5) <- c("twoln_fixed", "twoln_random", "Analysis") #Assign column names
  summarymodelfits_twoln_table <- data.frame(Analysis = numbers5[,3], twoln_fixed = numbers5[,1], twoln_random = numbers5[,2]) #Reorder columns
  #summarymodelfits_twoln_table #See table

  #Make list of tables
  tables <- list(summaryfitstats_table1_trad, summaryeffsizes_table2_trad, summaryfitstats_table1_phyl, summaryeffsizes_table2_phyl, summarymodelfits_AIC_table, summarymodelfits_twoln_table)
  tables
}

# 3 groups in the moderator variable
maketables_3group <- function(output){ #DONE IF/ELSE REFORMATTING
  #Table 1, Traditional meta-analysis
  #a1<-unlist(strsplit(output[6]," +"))[-1] #Table header
  a2<-unlist(strsplit(output[8],"  +"))[-1] #Data row 1, between groups, fixed effects
  a3<-unlist(strsplit(output[9],"  +"))[-1] #Data row 2, within groups
  a4 <- if(length(unlist(strsplit(output[10]," +"))[-1]) == 6) {c(paste(unlist(strsplit(output[10]," +"))[-1][1],unlist(strsplit(output[10]," +"))[-1][2],unlist(strsplit(output[10]," +"))[-1][3]),unlist(strsplit(output[10]," +"))[-1][4:6])} else
    if(length(unlist(strsplit(output[10]," +"))[-1]) == 8) {c(paste(unlist(strsplit(output[10]," +"))[-1][1],unlist(strsplit(output[10]," +"))[-1][2],unlist(strsplit(output[10]," +"))[-1][3]),unlist(strsplit(output[10]," +"))[-1][4:8])} else
      stats::end
  a5 <- if(length(unlist(strsplit(output[11]," +"))[-1]) == 6) {c(paste(unlist(strsplit(output[11]," +"))[-1][1],unlist(strsplit(output[11]," +"))[-1][2],unlist(strsplit(output[11]," +"))[-1][3]),unlist(strsplit(output[11]," +"))[-1][4:6])} else
    if(length(unlist(strsplit(output[11]," +"))[-1]) == 8) {c(paste(unlist(strsplit(output[11]," +"))[-1][1],unlist(strsplit(output[11]," +"))[-1][2],unlist(strsplit(output[11]," +"))[-1][3]),unlist(strsplit(output[11]," +"))[-1][4:8])} else
      stats::end
  a6 <- if(length(unlist(strsplit(output[12]," +"))[-1]) == 6) {c(paste(unlist(strsplit(output[12]," +"))[-1][1],unlist(strsplit(output[12]," +"))[-1][2],unlist(strsplit(output[12]," +"))[-1][3]),unlist(strsplit(output[12]," +"))[-1][4:6])} else
    if(length(unlist(strsplit(output[12]," +"))[-1]) == 8) {c(paste(unlist(strsplit(output[12]," +"))[-1][1],unlist(strsplit(output[12]," +"))[-1][2],unlist(strsplit(output[12]," +"))[-1][3]),unlist(strsplit(output[12]," +"))[-1][4:8])} else
      stats::end
  a7<-unlist(strsplit(output[13]," +"))[-1] #Data row 6, Total
  a8<-unlist(strsplit(output[16],"  +"))[-1] #Data row 7, between groups, random effects
  rownaames<-c(a2[1],a3[1],a4[1],a5[1],a6[1],a7[1],a8[1]) #make vector of first column names
  numbers<-t(data.frame(as.numeric(a2[2:4]),as.numeric(a3[2:4]),as.numeric(a4[2:4]),as.numeric(a5[2:4]),as.numeric(a6[2:4]),append(as.numeric(a7[2]),c(-9999,-9999),after=1),as.numeric(a8[2:4]))) #make data frame of numbers
  numbers<-data.frame(numbers,rownaames) #Make data frame of row names and numbers
  colnames(numbers)<-c("Q","df","P","Source") #Assign column names
  summaryfitstats_table1_trad<-data.frame(Source=numbers$Source,Q=numbers$Q,df=numbers$df,P=numbers$P) #Reorder columns
  #summaryfitstats_table1_trad #See table

  #Table 2, Traditional meta-analysis
  #b1<-unlist(strsplit(output[25]," +"))[-1] #Table header
  b2 <- if(length(unlist(strsplit(output[29]," +"))[-1]) == 9) {c(paste((unlist(strsplit(output[29]," +"))[-1])[1],(unlist(strsplit(output[29]," +"))[-1])[2]),unlist(strsplit(output[29]," +"))[-1][3:9])} else
    if(stringr::str_detect(unlist(strsplit(output[29]," +"))[-1][6], ")") == "TRUE") {c(unlist(strsplit(output[29],"  +"))[-1][1:4], paste(stringr::str_split(unlist(strsplit(output[29]," +"))[-1][6], ")")[[1]][1],")", sep=""), stringr::str_split(unlist(strsplit(output[29]," +"))[-1][6], ")")[[1]][2], unlist(strsplit(output[29],"  +"))[-1][6:7])} else
      stats::end
  b3 <- if(length(unlist(strsplit(output[30]," +"))[-1]) == 9) {c(paste((unlist(strsplit(output[30]," +"))[-1])[1],(unlist(strsplit(output[30]," +"))[-1])[2]),unlist(strsplit(output[30]," +"))[-1][3:9])} else
    if(stringr::str_detect(unlist(strsplit(output[30]," +"))[-1][6], ")") == "TRUE") {c(unlist(strsplit(output[30],"  +"))[-1][1:4], paste(stringr::str_split(unlist(strsplit(output[30]," +"))[-1][6], ")")[[1]][1],")", sep=""), stringr::str_split(unlist(strsplit(output[30]," +"))[-1][6], ")")[[1]][2], unlist(strsplit(output[30],"  +"))[-1][6:7])} else
      stats::end
  b4 <- if(length(unlist(strsplit(output[31]," +"))[-1]) == 9) {c(paste((unlist(strsplit(output[31]," +"))[-1])[1],(unlist(strsplit(output[31]," +"))[-1])[2]),unlist(strsplit(output[31]," +"))[-1][3:9])} else
    if(stringr::str_detect(unlist(strsplit(output[31]," +"))[-1][6], ")") == "TRUE") {c(unlist(strsplit(output[31],"  +"))[-1][1:4], paste(stringr::str_split(unlist(strsplit(output[31]," +"))[-1][6], ")")[[1]][1],")", sep=""), stringr::str_split(unlist(strsplit(output[31]," +"))[-1][6], ")")[[1]][2], unlist(strsplit(output[31],"  +"))[-1][6:7])} else
      stats::end
  b5 <- if(length(unlist(strsplit(output[32]," +"))[-1]) == 9) {c(paste((unlist(strsplit(output[32]," +"))[-1])[1],(unlist(strsplit(output[32]," +"))[-1])[2]),unlist(strsplit(output[32]," +"))[-1][3:9])} else
    if(stringr::str_detect(unlist(strsplit(output[32]," +"))[-1][6], ")") == "TRUE") {c(unlist(strsplit(output[32],"  +"))[-1][1:4], paste(stringr::str_split(unlist(strsplit(output[32]," +"))[-1][6], ")")[[1]][1],")", sep=""), stringr::str_split(unlist(strsplit(output[32]," +"))[-1][6], ")")[[1]][2], unlist(strsplit(output[32],"  +"))[-1][6:7])} else
      stats::end
  b6 <- if(length(unlist(strsplit(output[36]," +"))[-1]) == 9) {c(paste((unlist(strsplit(output[36]," +"))[-1])[1],(unlist(strsplit(output[36]," +"))[-1])[2]),unlist(strsplit(output[36]," +"))[-1][3:9])} else
    if(stringr::str_detect(unlist(strsplit(output[36]," +"))[-1][6], ")") == "TRUE") {c(unlist(strsplit(output[36],"  +"))[-1][1:4], paste(stringr::str_split(unlist(strsplit(output[36]," +"))[-1][6], ")")[[1]][1],")", sep=""), stringr::str_split(unlist(strsplit(output[36]," +"))[-1][6], ")")[[1]][2], unlist(strsplit(output[36],"  +"))[-1][6:7])} else
      stats::end
  b7 <- if(length(unlist(strsplit(output[37]," +"))[-1]) == 9) {c(paste((unlist(strsplit(output[37]," +"))[-1])[1],(unlist(strsplit(output[37]," +"))[-1])[2]),unlist(strsplit(output[37]," +"))[-1][3:9])} else
    if(stringr::str_detect(unlist(strsplit(output[37]," +"))[-1][6], ")") == "TRUE") {c(unlist(strsplit(output[37],"  +"))[-1][1:4], paste(stringr::str_split(unlist(strsplit(output[37]," +"))[-1][6], ")")[[1]][1],")", sep=""), stringr::str_split(unlist(strsplit(output[37]," +"))[-1][6], ")")[[1]][2], unlist(strsplit(output[37],"  +"))[-1][6:7])} else
      stats::end
  b8 <- if(length(unlist(strsplit(output[38]," +"))[-1]) == 9) {c(paste((unlist(strsplit(output[38]," +"))[-1])[1],(unlist(strsplit(output[38]," +"))[-1])[2]),unlist(strsplit(output[38]," +"))[-1][3:9])} else
    if(stringr::str_detect(unlist(strsplit(output[38]," +"))[-1][6], ")") == "TRUE") {c(unlist(strsplit(output[38],"  +"))[-1][1:4], paste(stringr::str_split(unlist(strsplit(output[38]," +"))[-1][6], ")")[[1]][1],")", sep=""), stringr::str_split(unlist(strsplit(output[38]," +"))[-1][6], ")")[[1]][2], unlist(strsplit(output[38],"  +"))[-1][6:7])} else
      stats::end
  b9 <- if(length(unlist(strsplit(output[39]," +"))[-1]) == 9) {c(paste((unlist(strsplit(output[39]," +"))[-1])[1],(unlist(strsplit(output[39]," +"))[-1])[2]),unlist(strsplit(output[39]," +"))[-1][3:9])} else
    if(stringr::str_detect(unlist(strsplit(output[39]," +"))[-1][6], ")") == "TRUE") {c(unlist(strsplit(output[39],"  +"))[-1][1:4], paste(stringr::str_split(unlist(strsplit(output[39]," +"))[-1][6], ")")[[1]][1],")", sep=""), stringr::str_split(unlist(strsplit(output[39]," +"))[-1][6], ")")[[1]][2], unlist(strsplit(output[39],"  +"))[-1][6:7])} else
      stats::end
  rownaames1<-c(b2[1],b3[1],b4[1],b5[1],b6[1],b7[1],b8[1],b9[1]) #make vector of first column names
  b22<-CI_split(b2) #Split confidence interval numbers apart and reinsert to vector of data
  b33<-CI_split(b3)
  b44<-CI_split(b4)
  b55<-CI_split(b5)
  b66<-CI_split(b6)
  b77<-CI_split(b7)
  b88<-CI_split(b8)
  b99<-CI_split(b9)
  numbers1<-t(data.frame(as.numeric(b22),as.numeric(b33),as.numeric(b44),as.numeric(b55),as.numeric(b66),as.numeric(b77),as.numeric(b88),as.numeric(b99)))  #make data frame of numbers
  numbers1<-data.frame(numbers1,rownaames1) #Make data frame of row names and numbers
  colnames(numbers1)<-c("k","effsize","var","95CI_low","95CI_high","Z","df","P","Group") #Assign column names
  summaryeffsizes_table2_trad<-data.frame(Group=numbers1[,9],k=numbers1[,1],effsize=numbers1[,2],var=numbers1[,3],CI_low=numbers1[,4],CI_high=numbers1[,5],Z=numbers1[,6],df=numbers1[,7],P=numbers1[,8]) #Reorder columns
  #summaryeffsizes_table2_trad #See table

  ###Phylogenetic meta-analysis
  #Table 1, Phylogenetic meta-analysis
  #d1<-unlist(strsplit(output[53]," +"))[-1] #Table header
  d2 <- unlist(strsplit(output[55],"  +"))[-1] #Data row 1, between groups, fixed effects
  d3 <- unlist(strsplit(output[56],"  +"))[-1] #Data row 2, within groups
  d4 <- if(length(unlist(strsplit(output[57]," +"))[-1]) == 6) {c(paste(unlist(strsplit(output[57]," +"))[-1][1],unlist(strsplit(output[57]," +"))[-1][2],unlist(strsplit(output[57]," +"))[-1][3]),unlist(strsplit(output[57]," +"))[-1][4:6])} else
    if(length(unlist(strsplit(output[57]," +"))[-1]) == 8) {c(paste(unlist(strsplit(output[57]," +"))[-1][1],unlist(strsplit(output[57]," +"))[-1][2],unlist(strsplit(output[57]," +"))[-1][3]),unlist(strsplit(output[57]," +"))[-1][4:8])} else
      stats::end
  d5 <- if(length(unlist(strsplit(output[58]," +"))[-1]) == 6) {c(paste(unlist(strsplit(output[58]," +"))[-1][1],unlist(strsplit(output[58]," +"))[-1][2],unlist(strsplit(output[58]," +"))[-1][3]),unlist(strsplit(output[58]," +"))[-1][4:6])} else
    if(length(unlist(strsplit(output[58]," +"))[-1]) == 8) {c(paste(unlist(strsplit(output[58]," +"))[-1][1],unlist(strsplit(output[58]," +"))[-1][2],unlist(strsplit(output[58]," +"))[-1][3]),unlist(strsplit(output[58]," +"))[-1][4:8])} else
      stats::end
  d6 <- if(length(unlist(strsplit(output[59]," +"))[-1]) == 6) {c(paste(unlist(strsplit(output[59]," +"))[-1][1],unlist(strsplit(output[59]," +"))[-1][2],unlist(strsplit(output[59]," +"))[-1][3]),unlist(strsplit(output[59]," +"))[-1][4:6])} else
    if(length(unlist(strsplit(output[59]," +"))[-1]) == 8) {c(paste(unlist(strsplit(output[59]," +"))[-1][1],unlist(strsplit(output[59]," +"))[-1][2],unlist(strsplit(output[59]," +"))[-1][3]),unlist(strsplit(output[59]," +"))[-1][4:8])} else
      stats::end
  d7 <- unlist(strsplit(output[60]," +"))[-1] #Data row 6, Total
  d8 <- unlist(strsplit(output[63],"  +"))[-1] #Data row 7, between groups, random effects
  rownaames2<-c(d2[1],d3[1],d4[1],d5[1],d6[1],d7[1],d8[1]) #make vector of first column names
  numbers2<-t(data.frame(append(as.numeric(d2[2:4]),c(-9999,-9999),after=3),as.numeric(d3[2:6]),as.numeric(d4[2:6]),as.numeric(d5[2:6]),as.numeric(d6[2:6]),append(as.numeric(d7[2]),c(-9999,-9999,-9999,-9999),after=1),append(as.numeric(d8[2:4]),c(-9999,-9999),after=3))) #make data frame of numbers
  numbers2<-data.frame(numbers2,rownaames2) #Make data frame of row names and numbers
  colnames(numbers2)<-c("Q","df","P","df_polytadj","P_polytadj","Source") #Assign column names
  summaryfitstats_table1_phyl<-data.frame(Source=numbers2[,6],Q=numbers2[,1],df=numbers2[,2],P=numbers2[,3],df_polytadj=numbers2[,4],P_polytadj=numbers2[,5]) #Reorder columns
  #summaryfitstats_table1_phyl #See table

  #Table 2, Phylogenetic meta-analysis
  #e1<-unlist(strsplit(output[72]," +"))[-1] #Table header
  e2 <- if(length(unlist(strsplit(output[76]," +"))[-1]) == 9) {c(paste((unlist(strsplit(output[76]," +"))[-1])[1],(unlist(strsplit(output[76]," +"))[-1])[2]),unlist(strsplit(output[76]," +"))[-1][3:9])} else
    if(stringr::str_detect(unlist(strsplit(output[76]," +"))[-1][6], ")") == "TRUE") {c(unlist(strsplit(output[76],"  +"))[-1][1:4], paste(stringr::str_split(unlist(strsplit(output[76]," +"))[-1][6], ")")[[1]][1],")", sep=""), stringr::str_split(unlist(strsplit(output[76]," +"))[-1][6], ")")[[1]][2], unlist(strsplit(output[76],"  +"))[-1][6:7])} else
      stats::end
  e3 <- if(length(unlist(strsplit(output[77]," +"))[-1]) == 9) {c(paste((unlist(strsplit(output[77]," +"))[-1])[1],(unlist(strsplit(output[77]," +"))[-1])[2]),unlist(strsplit(output[77]," +"))[-1][3:9])} else
    if(stringr::str_detect(unlist(strsplit(output[77]," +"))[-1][6], ")") == "TRUE") {c(unlist(strsplit(output[77],"  +"))[-1][1:4], paste(stringr::str_split(unlist(strsplit(output[77]," +"))[-1][6], ")")[[1]][1],")", sep=""), stringr::str_split(unlist(strsplit(output[77]," +"))[-1][6], ")")[[1]][2], unlist(strsplit(output[77],"  +"))[-1][6:7])} else
      stats::end
  e4 <- if(length(unlist(strsplit(output[78]," +"))[-1]) == 9) {c(paste((unlist(strsplit(output[78]," +"))[-1])[1],(unlist(strsplit(output[78]," +"))[-1])[2]),unlist(strsplit(output[78]," +"))[-1][3:9])} else
    if(stringr::str_detect(unlist(strsplit(output[78]," +"))[-1][6], ")") == "TRUE") {c(unlist(strsplit(output[78],"  +"))[-1][1:4], paste(stringr::str_split(unlist(strsplit(output[78]," +"))[-1][6], ")")[[1]][1],")", sep=""), stringr::str_split(unlist(strsplit(output[78]," +"))[-1][6], ")")[[1]][2], unlist(strsplit(output[78],"  +"))[-1][6:7])} else
      stats::end
  e5 <- if(length(unlist(strsplit(output[79]," +"))[-1]) == 9) {c(paste((unlist(strsplit(output[79]," +"))[-1])[1],(unlist(strsplit(output[79]," +"))[-1])[2]),unlist(strsplit(output[79]," +"))[-1][3:9])} else
    if(stringr::str_detect(unlist(strsplit(output[79]," +"))[-1][6], ")") == "TRUE") {c(unlist(strsplit(output[79],"  +"))[-1][1:4], paste(stringr::str_split(unlist(strsplit(output[79]," +"))[-1][6], ")")[[1]][1],")", sep=""), stringr::str_split(unlist(strsplit(output[79]," +"))[-1][6], ")")[[1]][2], unlist(strsplit(output[79],"  +"))[-1][6:7])} else
      stats::end
  e6 <- if(length(unlist(strsplit(output[83]," +"))[-1]) == 9) {c(paste((unlist(strsplit(output[83]," +"))[-1])[1],(unlist(strsplit(output[83]," +"))[-1])[2]),unlist(strsplit(output[83]," +"))[-1][3:9])} else
    if(stringr::str_detect(unlist(strsplit(output[83]," +"))[-1][6], ")") == "TRUE") {c(unlist(strsplit(output[83],"  +"))[-1][1:4], paste(stringr::str_split(unlist(strsplit(output[83]," +"))[-1][6], ")")[[1]][1],")", sep=""), stringr::str_split(unlist(strsplit(output[83]," +"))[-1][6], ")")[[1]][2], unlist(strsplit(output[83],"  +"))[-1][6:7])} else
      stats::end
  e7 <- if(length(unlist(strsplit(output[84]," +"))[-1]) == 9) {c(paste((unlist(strsplit(output[84]," +"))[-1])[1],(unlist(strsplit(output[84]," +"))[-1])[2]),unlist(strsplit(output[84]," +"))[-1][3:9])} else
    if(stringr::str_detect(unlist(strsplit(output[84]," +"))[-1][6], ")") == "TRUE") {c(unlist(strsplit(output[84],"  +"))[-1][1:4], paste(stringr::str_split(unlist(strsplit(output[84]," +"))[-1][6], ")")[[1]][1],")", sep=""), stringr::str_split(unlist(strsplit(output[84]," +"))[-1][6], ")")[[1]][2], unlist(strsplit(output[84],"  +"))[-1][6:7])} else
      stats::end
  e8 <- if(length(unlist(strsplit(output[85]," +"))[-1]) == 9) {c(paste((unlist(strsplit(output[85]," +"))[-1])[1],(unlist(strsplit(output[85]," +"))[-1])[2]),unlist(strsplit(output[85]," +"))[-1][3:9])} else
    if(stringr::str_detect(unlist(strsplit(output[85]," +"))[-1][6], ")") == "TRUE") {c(unlist(strsplit(output[85],"  +"))[-1][1:4], paste(stringr::str_split(unlist(strsplit(output[85]," +"))[-1][6], ")")[[1]][1],")", sep=""), stringr::str_split(unlist(strsplit(output[85]," +"))[-1][6], ")")[[1]][2], unlist(strsplit(output[85],"  +"))[-1][6:7])} else
      stats::end
  e9 <- if(length(unlist(strsplit(output[86]," +"))[-1]) == 9) {c(paste((unlist(strsplit(output[86]," +"))[-1])[1],(unlist(strsplit(output[86]," +"))[-1])[2]),unlist(strsplit(output[86]," +"))[-1][3:9])} else
    if(stringr::str_detect(unlist(strsplit(output[86]," +"))[-1][6], ")") == "TRUE") {c(unlist(strsplit(output[86],"  +"))[-1][1:4], paste(stringr::str_split(unlist(strsplit(output[86]," +"))[-1][6], ")")[[1]][1],")", sep=""), stringr::str_split(unlist(strsplit(output[86]," +"))[-1][6], ")")[[1]][2], unlist(strsplit(output[86],"  +"))[-1][6:7])} else
      stats::end
  rownaames3<-c(e2[1],e3[1],e4[1],e5[1],e6[1],e7[1],e8[1],e9[1]) #make vector of first column names
  e22<-CI_split(e2) #Split confidence interval numbers apart and reinsert to vector of data
  e33<-CI_split(e3)
  e44<-CI_split(e4)
  e55<-CI_split(e5)
  e66<-CI_split(e6)
  e77<-CI_split(e7)
  e88<-CI_split(e8)
  e99<-CI_split(e9)
  numbers3<-t(data.frame(as.numeric(e22),as.numeric(e33),as.numeric(e44),as.numeric(e55),as.numeric(e66),as.numeric(e77),as.numeric(e88),as.numeric(e99)))  #make data frame of numbers
  numbers3<-data.frame(numbers3,rownaames3) #Make data frame of row names and numbers
  colnames(numbers3)<-c("k","effsize","var","95CI_low","95CI_high","Z","df","P","Group") #Assign column names
  summaryeffsizes_table2_phyl<-data.frame(Group=numbers3[,9],k=numbers3[,1],effsize=numbers3[,2],var=numbers3[,3],CI_low=numbers3[,4],CI_high=numbers3[,5],Z=numbers3[,6],df=numbers3[,7],P=numbers3[,8]) #Reorder columns
  #summaryeffsizes_table2_phyl #See table

  ### Model fit table, AIC
  f1<-unlist(strsplit(output[101]," +"))[-1] #Table header
  f2<-unlist(strsplit(output[102]," +"))[-1] #Data row 1, fixed effects, all studies
  rownaames4<-c(f1[1],f2[1]) #make vector of first column names
  numbers4<-t(data.frame(as.numeric(f1[2:3]),as.numeric(f2[2:3])))  #make data frame of numbers
  numbers4<-data.frame(numbers4,rownaames4) #Make data frame of row names and numbers
  colnames(numbers4)<-c("AIC_fixed","AIC_random","Analysis") #Assign column names
  summarymodelfits_AIC_table<-data.frame(Analysis=numbers4[,3],AIC_fixed=numbers4[,1],AIC_random=numbers4[,2]) #Reorder columns
  #summarymodelfits_AIC_table #See table

  ### Model fit table, -2(likelihood)
  g1 <- unlist(strsplit(output[113]," +"))[-1] #Traditional AIC data
  g2 <- unlist(strsplit(output[114]," +"))[-1] #Phylogenetically controlled AIC data
  rownaames5 <- c(g1[1],g2[1]) #make vector of first column names
  numbers5 <- t(data.frame(as.numeric(g1[2:3]), as.numeric(g2[2:3])))  #make data frame of numbers
  numbers5 <- data.frame(numbers5, rownaames5) #Make data frame of row names and numbers
  colnames(numbers5) <- c("twoln_fixed", "twoln_random", "Analysis") #Assign column names
  summarymodelfits_twoln_table <- data.frame(Analysis = numbers5[,3], twoln_fixed = numbers5[,1], twoln_random = numbers5[,2]) #Reorder columns
  #summarymodelfits_twoln_table #See table

  #Make list of tables
  tables <- list(summaryfitstats_table1_trad, summaryeffsizes_table2_trad, summaryfitstats_table1_phyl, summaryeffsizes_table2_phyl, summarymodelfits_AIC_table, summarymodelfits_twoln_table)
  tables
}

# 4 groups in the moderator variable
maketables_4group <- function(output){ #DONE IF/ELSE REFORMATTING
  #Table 1, Traditional meta-analysis
  a1<-unlist(strsplit(output[6],"  +"))[-1] #Table header
  a2<-unlist(strsplit(output[8],"  +"))[-1] #Data row 1, between groups, fixed effects
  a3<-unlist(strsplit(output[9],"  +"))[-1] #Data row 2, within groups
  a4<-unlist(strsplit(output[10],"  +"))[-1] #Data row 3, within goup 0
  a5<-unlist(strsplit(output[11],"  +"))[-1] #Data row 4, within goup 1
  a6<-unlist(strsplit(output[12],"  +"))[-1] #Data row 5, within goup 2
  a7<-unlist(strsplit(output[13],"  +"))[-1] #Data row 6, Total
  a8<-unlist(strsplit(output[14],"  +"))[-1] #Data row 7, between groups, random effects
  a9<-unlist(strsplit(output[17],"  +"))[-1] #Data row 7, between groups, random effects
  rownaames<-c(a2[1],a3[1],a4[1],a5[1],a6[1],a7[1],a8[1],a9[1]) #make vector of first column names
  numbers<-t(data.frame(as.numeric(a2[2:4]),as.numeric(a3[2:4]),as.numeric(a4[2:4]),as.numeric(a5[2:4]),as.numeric(a6[2:4]),as.numeric(a7[2:4]),append(as.numeric(a8[2]),c(-9999,-9999),after=1),as.numeric(a9[2:4]))) #make data frame of numbers
  numbers<-data.frame(numbers,rownaames) #Make data frame of row names and numbers
  colnames(numbers)<-c("Q","df","P","Source") #Assign column names
  summaryfitstats_table1_trad<-data.frame(Source=numbers$Source,Q=numbers$Q,df=numbers$df,P=numbers$P) #Reorder columns
  #summaryfitstats_table1_trad #See table

  #Table 2, Traditional meta-analysis
  b1<-unlist(strsplit(output[26],"  +"))[-1] #Table header
  b2 <- if(length(unlist(strsplit(output[30]," +"))[-1]) == 9) {c(paste((unlist(strsplit(output[30]," +"))[-1])[1],(unlist(strsplit(output[30]," +"))[-1])[2]),unlist(strsplit(output[30]," +"))[-1][3:9])} else
    if(stringr::str_detect(unlist(strsplit(output[30]," +"))[-1][6], ")") == "TRUE") {c(unlist(strsplit(output[30],"  +"))[-1][1:4], paste(stringr::str_split(unlist(strsplit(output[30]," +"))[-1][6], ")")[[1]][1],")", sep=""), stringr::str_split(unlist(strsplit(output[30]," +"))[-1][6], ")")[[1]][2], unlist(strsplit(output[30],"  +"))[-1][6:7])} else
      stats::end
  b3 <- if(length(unlist(strsplit(output[31]," +"))[-1]) == 9) {c(paste((unlist(strsplit(output[31]," +"))[-1])[1],(unlist(strsplit(output[31]," +"))[-1])[2]),unlist(strsplit(output[31]," +"))[-1][3:9])} else
    if(stringr::str_detect(unlist(strsplit(output[31]," +"))[-1][6], ")") == "TRUE") {c(unlist(strsplit(output[31],"  +"))[-1][1:4], paste(stringr::str_split(unlist(strsplit(output[31]," +"))[-1][6], ")")[[1]][1],")", sep=""), stringr::str_split(unlist(strsplit(output[31]," +"))[-1][6], ")")[[1]][2], unlist(strsplit(output[31],"  +"))[-1][6:7])} else
      stats::end
  b4 <- if(length(unlist(strsplit(output[32]," +"))[-1]) == 9) {c(paste((unlist(strsplit(output[32]," +"))[-1])[1],(unlist(strsplit(output[32]," +"))[-1])[2]),unlist(strsplit(output[32]," +"))[-1][3:9])} else
    if(stringr::str_detect(unlist(strsplit(output[32]," +"))[-1][6], ")") == "TRUE") {c(unlist(strsplit(output[32],"  +"))[-1][1:4], paste(stringr::str_split(unlist(strsplit(output[32]," +"))[-1][6], ")")[[1]][1],")", sep=""), stringr::str_split(unlist(strsplit(output[32]," +"))[-1][6], ")")[[1]][2], unlist(strsplit(output[32],"  +"))[-1][6:7])} else
      stats::end
  b5 <- if(length(unlist(strsplit(output[33]," +"))[-1]) == 9) {c(paste((unlist(strsplit(output[33]," +"))[-1])[1],(unlist(strsplit(output[33]," +"))[-1])[2]),unlist(strsplit(output[33]," +"))[-1][3:9])} else
    if(stringr::str_detect(unlist(strsplit(output[33]," +"))[-1][6], ")") == "TRUE") {c(unlist(strsplit(output[33],"  +"))[-1][1:4], paste(stringr::str_split(unlist(strsplit(output[33]," +"))[-1][6], ")")[[1]][1],")", sep=""), stringr::str_split(unlist(strsplit(output[33]," +"))[-1][6], ")")[[1]][2], unlist(strsplit(output[33],"  +"))[-1][6:7])} else
      stats::end
  b6 <- if(length(unlist(strsplit(output[34]," +"))[-1]) == 9) {c(paste((unlist(strsplit(output[34]," +"))[-1])[1],(unlist(strsplit(output[34]," +"))[-1])[2]),unlist(strsplit(output[34]," +"))[-1][3:9])} else
    if(stringr::str_detect(unlist(strsplit(output[34]," +"))[-1][6], ")") == "TRUE") {c(unlist(strsplit(output[34],"  +"))[-1][1:4], paste(stringr::str_split(unlist(strsplit(output[34]," +"))[-1][6], ")")[[1]][1],")", sep=""), stringr::str_split(unlist(strsplit(output[34]," +"))[-1][6], ")")[[1]][2], unlist(strsplit(output[34],"  +"))[-1][6:7])} else
      stats::end
  b7 <- if(length(unlist(strsplit(output[38]," +"))[-1]) == 9) {c(paste((unlist(strsplit(output[38]," +"))[-1])[1],(unlist(strsplit(output[38]," +"))[-1])[2]),unlist(strsplit(output[38]," +"))[-1][3:9])} else
    if(stringr::str_detect(unlist(strsplit(output[38]," +"))[-1][6], ")") == "TRUE") {c(unlist(strsplit(output[38],"  +"))[-1][1:4], paste(stringr::str_split(unlist(strsplit(output[38]," +"))[-1][6], ")")[[1]][1],")", sep=""), stringr::str_split(unlist(strsplit(output[38]," +"))[-1][6], ")")[[1]][2], unlist(strsplit(output[38],"  +"))[-1][6:7])} else
      stats::end
  b8 <- if(length(unlist(strsplit(output[39]," +"))[-1]) == 9) {c(paste((unlist(strsplit(output[39]," +"))[-1])[1],(unlist(strsplit(output[39]," +"))[-1])[2]),unlist(strsplit(output[39]," +"))[-1][3:9])} else
    if(stringr::str_detect(unlist(strsplit(output[39]," +"))[-1][6], ")") == "TRUE") {c(unlist(strsplit(output[39],"  +"))[-1][1:4], paste(stringr::str_split(unlist(strsplit(output[39]," +"))[-1][6], ")")[[1]][1],")", sep=""), stringr::str_split(unlist(strsplit(output[39]," +"))[-1][6], ")")[[1]][2], unlist(strsplit(output[39],"  +"))[-1][6:7])} else
      stats::end
  b9 <- if(length(unlist(strsplit(output[40]," +"))[-1]) == 9) {c(paste((unlist(strsplit(output[40]," +"))[-1])[1],(unlist(strsplit(output[40]," +"))[-1])[2]),unlist(strsplit(output[40]," +"))[-1][3:9])} else
    if(stringr::str_detect(unlist(strsplit(output[40]," +"))[-1][6], ")") == "TRUE") {c(unlist(strsplit(output[40],"  +"))[-1][1:4], paste(stringr::str_split(unlist(strsplit(output[40]," +"))[-1][6], ")")[[1]][1],")", sep=""), stringr::str_split(unlist(strsplit(output[40]," +"))[-1][6], ")")[[1]][2], unlist(strsplit(output[40],"  +"))[-1][6:7])} else
      stats::end
  b10 <- if(length(unlist(strsplit(output[41]," +"))[-1]) == 9) {c(paste((unlist(strsplit(output[41]," +"))[-1])[1],(unlist(strsplit(output[41]," +"))[-1])[2]),unlist(strsplit(output[41]," +"))[-1][3:9])} else
    if(stringr::str_detect(unlist(strsplit(output[41]," +"))[-1][6], ")") == "TRUE") {c(unlist(strsplit(output[41],"  +"))[-1][1:4], paste(stringr::str_split(unlist(strsplit(output[41]," +"))[-1][6], ")")[[1]][1],")", sep=""), stringr::str_split(unlist(strsplit(output[41]," +"))[-1][6], ")")[[1]][2], unlist(strsplit(output[41],"  +"))[-1][6:7])} else
      stats::end
  b11 <- if(length(unlist(strsplit(output[42]," +"))[-1]) == 9) {c(paste((unlist(strsplit(output[42]," +"))[-1])[1],(unlist(strsplit(output[42]," +"))[-1])[2]),unlist(strsplit(output[42]," +"))[-1][3:9])} else
    if(stringr::str_detect(unlist(strsplit(output[42]," +"))[-1][6], ")") == "TRUE") {c(unlist(strsplit(output[42],"  +"))[-1][1:4], paste(stringr::str_split(unlist(strsplit(output[42]," +"))[-1][6], ")")[[1]][1],")", sep=""), stringr::str_split(unlist(strsplit(output[42]," +"))[-1][6], ")")[[1]][2], unlist(strsplit(output[42],"  +"))[-1][6:7])} else
      stats::end
  rownaames1<-c(b2[1],b3[1],b4[1],b5[1],b6[1],b7[1],b8[1],b9[1],b10[1],b11[1]) #make vector of first column names
  b22<-CI_split(b2) #Split confidence interval numbers apart and reinsert to vector of data
  b33<-CI_split(b3)
  b44<-CI_split(b4)
  b55<-CI_split(b5)
  b66<-CI_split(b6)
  b77<-CI_split(b7)
  b88<-CI_split(b8)
  b99<-CI_split(b9)
  b1010<-CI_split(b10)
  b1111<-CI_split(b11)
  numbers1<-t(data.frame(as.numeric(b22),as.numeric(b33),as.numeric(b44),as.numeric(b55),as.numeric(b66),as.numeric(b77),as.numeric(b88),as.numeric(b99),as.numeric(b1010),as.numeric(b1111)))  #make data frame of numbers
  numbers1<-data.frame(numbers1,rownaames1) #Make data frame of row names and numbers
  colnames(numbers1)<-c("k","effsize","var","95CI_low","95CI_high","Z","df","P","Group") #Assign column names
  summaryeffsizes_table2_trad<-data.frame(Group=numbers1[,9],k=numbers1[,1],effsize=numbers1[,2],var=numbers1[,3],CI_low=numbers1[,4],CI_high=numbers1[,5],Z=numbers1[,6],df=numbers1[,7],P=numbers1[,8]) #Reorder columns
  #summaryeffsizes_table2_trad #See table

  ###Phylogenetic meta-analysis
  #Table 1, Phylogenetic meta-analysis
  d1<-unlist(strsplit(output[56],"  +"))[-1] #Table header
  d2<-unlist(strsplit(output[58],"  +"))[-1] #Data row 1, between groups, fixed effects
  d3<-unlist(strsplit(output[59],"  +"))[-1] #Data row 2, within groups
  d4<-unlist(strsplit(output[60],"  +"))[-1] #Data row 3, within goup 0
  d5<-unlist(strsplit(output[61],"  +"))[-1] #Data row 4, within goup 1
  d6<-unlist(strsplit(output[62],"  +"))[-1] #Data row 5, within goup 2
  d7<-unlist(strsplit(output[63],"  +"))[-1] #Data row 6, Total
  d8<-unlist(strsplit(output[64],"  +"))[-1] #Data row 7, between groups, random effects
  d9<-unlist(strsplit(output[67],"  +"))[-1] #Data row 7, between groups, random effects
  rownaames2<-c(d2[1],d3[1],d4[1],d5[1],d6[1],d7[1],d8[1],d9[1]) #make vector of first column names
  numbers2<-t(data.frame(append(as.numeric(d2[2:4]),c(-9999,-9999),after=3),as.numeric(d3[2:6]),as.numeric(d4[2:6]),as.numeric(d5[2:6]),as.numeric(d6[2:6]),as.numeric(d7[2:6]),append(as.numeric(d8[2]),c(-9999,-9999,-9999,-9999),after=3),append(as.numeric(d9[2:4]),c(-9999,-9999),after=3))) #make data frame of numbers
  numbers2<-data.frame(numbers2,rownaames2) #Make data frame of row names and numbers
  colnames(numbers2)<-c("Q","df","P","df_polytadj","P_polytadj","Source") #Assign column names
  summaryfitstats_table1_phyl<-data.frame(Source=numbers2[,6],Q=numbers2[,1],df=numbers2[,2],P=numbers2[,3],df_polytadj=numbers2[,4],P_polytadj=numbers2[,5]) #Reorder columns
  #summaryfitstats_table1_phyl #See table

  #Table 2, Phylogenetic meta-analysis
  e1 <- unlist(strsplit(output[76],"  +"))[-1] #Table header
  e2 <- if(length(unlist(strsplit(output[80]," +"))[-1]) == 9) {c(paste((unlist(strsplit(output[80]," +"))[-1])[1],(unlist(strsplit(output[80]," +"))[-1])[2]),unlist(strsplit(output[80]," +"))[-1][3:9])} else
    if(stringr::str_detect(unlist(strsplit(output[80]," +"))[-1][6], ")") == "TRUE") {c(unlist(strsplit(output[80],"  +"))[-1][1:4], paste(stringr::str_split(unlist(strsplit(output[80]," +"))[-1][6], ")")[[1]][1],")", sep=""), stringr::str_split(unlist(strsplit(output[80]," +"))[-1][6], ")")[[1]][2], unlist(strsplit(output[80],"  +"))[-1][6:7])} else
      stats::end
  e3 <- if(length(unlist(strsplit(output[81]," +"))[-1]) == 9) {c(paste((unlist(strsplit(output[81]," +"))[-1])[1],(unlist(strsplit(output[81]," +"))[-1])[2]),unlist(strsplit(output[81]," +"))[-1][3:9])} else
    if(stringr::str_detect(unlist(strsplit(output[81]," +"))[-1][6], ")") == "TRUE") {c(unlist(strsplit(output[81],"  +"))[-1][1:4], paste(stringr::str_split(unlist(strsplit(output[81]," +"))[-1][6], ")")[[1]][1],")", sep=""), stringr::str_split(unlist(strsplit(output[81]," +"))[-1][6], ")")[[1]][2], unlist(strsplit(output[81],"  +"))[-1][6:7])} else
      stats::end
  e4 <- if(length(unlist(strsplit(output[82]," +"))[-1]) == 9) {c(paste((unlist(strsplit(output[82]," +"))[-1])[1],(unlist(strsplit(output[82]," +"))[-1])[2]),unlist(strsplit(output[82]," +"))[-1][3:9])} else
    if(stringr::str_detect(unlist(strsplit(output[82]," +"))[-1][6], ")") == "TRUE") {c(unlist(strsplit(output[82],"  +"))[-1][1:4], paste(stringr::str_split(unlist(strsplit(output[82]," +"))[-1][6], ")")[[1]][1],")", sep=""), stringr::str_split(unlist(strsplit(output[82]," +"))[-1][6], ")")[[1]][2], unlist(strsplit(output[82],"  +"))[-1][6:7])} else
      stats::end
  e5 <- if(length(unlist(strsplit(output[83]," +"))[-1]) == 9) {c(paste((unlist(strsplit(output[83]," +"))[-1])[1],(unlist(strsplit(output[83]," +"))[-1])[2]),unlist(strsplit(output[83]," +"))[-1][3:9])} else
    if(stringr::str_detect(unlist(strsplit(output[83]," +"))[-1][6], ")") == "TRUE") {c(unlist(strsplit(output[83],"  +"))[-1][1:4], paste(stringr::str_split(unlist(strsplit(output[83]," +"))[-1][6], ")")[[1]][1],")", sep=""), stringr::str_split(unlist(strsplit(output[83]," +"))[-1][6], ")")[[1]][2], unlist(strsplit(output[83],"  +"))[-1][6:7])} else
      stats::end
  e6 <- if(length(unlist(strsplit(output[84]," +"))[-1]) == 9) {c(paste((unlist(strsplit(output[84]," +"))[-1])[1],(unlist(strsplit(output[84]," +"))[-1])[2]),unlist(strsplit(output[84]," +"))[-1][3:9])} else
    if(stringr::str_detect(unlist(strsplit(output[84]," +"))[-1][6], ")") == "TRUE") {c(unlist(strsplit(output[84],"  +"))[-1][1:4], paste(stringr::str_split(unlist(strsplit(output[84]," +"))[-1][6], ")")[[1]][1],")", sep=""), stringr::str_split(unlist(strsplit(output[84]," +"))[-1][6], ")")[[1]][2], unlist(strsplit(output[84],"  +"))[-1][6:7])} else
      stats::end
  e7 <- if(length(unlist(strsplit(output[88]," +"))[-1]) == 9) {c(paste((unlist(strsplit(output[88]," +"))[-1])[1],(unlist(strsplit(output[88]," +"))[-1])[2]),unlist(strsplit(output[88]," +"))[-1][3:9])} else
    if(stringr::str_detect(unlist(strsplit(output[88]," +"))[-1][6], ")") == "TRUE") {c(unlist(strsplit(output[88],"  +"))[-1][1:4], paste(stringr::str_split(unlist(strsplit(output[88]," +"))[-1][6], ")")[[1]][1],")", sep=""), stringr::str_split(unlist(strsplit(output[88]," +"))[-1][6], ")")[[1]][2], unlist(strsplit(output[88],"  +"))[-1][6:7])} else
      stats::end
  e8 <- if(length(unlist(strsplit(output[89]," +"))[-1]) == 9) {c(paste((unlist(strsplit(output[89]," +"))[-1])[1],(unlist(strsplit(output[89]," +"))[-1])[2]),unlist(strsplit(output[89]," +"))[-1][3:9])} else
    if(stringr::str_detect(unlist(strsplit(output[89]," +"))[-1][6], ")") == "TRUE") {c(unlist(strsplit(output[89],"  +"))[-1][1:4], paste(stringr::str_split(unlist(strsplit(output[89]," +"))[-1][6], ")")[[1]][1],")", sep=""), stringr::str_split(unlist(strsplit(output[89]," +"))[-1][6], ")")[[1]][2], unlist(strsplit(output[89],"  +"))[-1][6:7])} else
      stats::end
  e9 <- if(length(unlist(strsplit(output[90]," +"))[-1]) == 9) {c(paste((unlist(strsplit(output[90]," +"))[-1])[1],(unlist(strsplit(output[90]," +"))[-1])[2]),unlist(strsplit(output[90]," +"))[-1][3:9])} else
    if(stringr::str_detect(unlist(strsplit(output[90]," +"))[-1][6], ")") == "TRUE") {c(unlist(strsplit(output[90],"  +"))[-1][1:4], paste(stringr::str_split(unlist(strsplit(output[90]," +"))[-1][6], ")")[[1]][1],")", sep=""), stringr::str_split(unlist(strsplit(output[90]," +"))[-1][6], ")")[[1]][2], unlist(strsplit(output[90],"  +"))[-1][6:7])} else
      stats::end
  e10 <- if(length(unlist(strsplit(output[91]," +"))[-1]) == 9) {c(paste((unlist(strsplit(output[91]," +"))[-1])[1],(unlist(strsplit(output[91]," +"))[-1])[2]),unlist(strsplit(output[91]," +"))[-1][3:9])} else
    if(stringr::str_detect(unlist(strsplit(output[91]," +"))[-1][6], ")") == "TRUE") {c(unlist(strsplit(output[91],"  +"))[-1][1:4], paste(stringr::str_split(unlist(strsplit(output[91]," +"))[-1][6], ")")[[1]][1],")", sep=""), stringr::str_split(unlist(strsplit(output[91]," +"))[-1][6], ")")[[1]][2], unlist(strsplit(output[91],"  +"))[-1][6:7])} else
      stats::end
  e11 <- if(length(unlist(strsplit(output[92]," +"))[-1]) == 9) {c(paste((unlist(strsplit(output[92]," +"))[-1])[1],(unlist(strsplit(output[92]," +"))[-1])[2]),unlist(strsplit(output[92]," +"))[-1][3:9])} else
    if(stringr::str_detect(unlist(strsplit(output[92]," +"))[-1][6], ")") == "TRUE") {c(unlist(strsplit(output[92],"  +"))[-1][1:4], paste(stringr::str_split(unlist(strsplit(output[92]," +"))[-1][6], ")")[[1]][1],")", sep=""), stringr::str_split(unlist(strsplit(output[92]," +"))[-1][6], ")")[[1]][2], unlist(strsplit(output[92],"  +"))[-1][6:7])} else
      stats::end
  rownaames3<-c(e2[1],e3[1],e4[1],e5[1],e6[1],e7[1],e8[1],e9[1],e10[1],e11[1]) #make vector of first column names
  e22<-CI_split(e2) #Split confidence interval numbers apart and reinsert to vector of data
  e33<-CI_split(e3)
  e44<-CI_split(e4)
  e55<-CI_split(e5)
  e66<-CI_split(e6)
  e77<-CI_split(e7)
  e88<-CI_split(e8)
  e99<-CI_split(e9)
  e1010<-CI_split(e10)
  e1111<-CI_split(e11)
  numbers3<-t(data.frame(as.numeric(e22),as.numeric(e33),as.numeric(e44),as.numeric(e55),as.numeric(e66),as.numeric(e77),as.numeric(e88),as.numeric(e99),as.numeric(e1010),as.numeric(e1111)))  #make data frame of numbers
  numbers3<-data.frame(numbers3,rownaames3) #Make data frame of row names and numbers
  colnames(numbers3)<-c("k","effsize","var","95CI_low","95CI_high","Z","df","P","Group") #Assign column names
  summaryeffsizes_table2_phyl<-data.frame(Group=numbers3[,9],k=numbers3[,1],effsize=numbers3[,2],var=numbers3[,3],CI_low=numbers3[,4],CI_high=numbers3[,5],Z=numbers3[,6],df=numbers3[,7],P=numbers3[,8]) #Reorder columns
  #summaryeffsizes_table2_phyl #See table

  ###Model fit table
  f1<-unlist(strsplit(output[107],"  +"))[-1] #Table header
  f2<-unlist(strsplit(output[108],"  +"))[-1] #Data row 1, fixed effects, all studies
  rownaames4<-c(f1[1],f2[1]) #make vector of first column names
  numbers4<-t(data.frame(as.numeric(f1[2:3]),as.numeric(f2[2:3])))  #make data frame of numbers
  numbers4<-data.frame(numbers4,rownaames4) #Make data frame of row names and numbers
  colnames(numbers4)<-c("AIC_fixed","AIC_random","Analysis") #Assign column names
  summarymodelfits_AIC_table<-data.frame(Analysis=numbers4[,3],AIC_fixed=numbers4[,1],AIC_random=numbers4[,2]) #Reorder columns
  #summarymodelfits_table #See table

  ### Model fit table, -2(likelihood)
  g1 <- unlist(strsplit(output[119]," +"))[-1] #Traditional AIC data
  g2 <- unlist(strsplit(output[120]," +"))[-1] #Phylogenetically controlled AIC data
  rownaames5 <- c(g1[1],g2[1]) #make vector of first column names
  numbers5 <- t(data.frame(as.numeric(g1[2:3]), as.numeric(g2[2:3])))  #make data frame of numbers
  numbers5 <- data.frame(numbers5, rownaames5) #Make data frame of row names and numbers
  colnames(numbers5) <- c("twoln_fixed", "twoln_random", "Analysis") #Assign column names
  summarymodelfits_twoln_table <- data.frame(Analysis = numbers5[,3], twoln_fixed = numbers5[,1], twoln_random = numbers5[,2]) #Reorder columns
  #summarymodelfits_twoln_table #See table

  #Make list of tables
  tables <- list(summaryfitstats_table1_trad, summaryeffsizes_table2_trad, summaryfitstats_table1_phyl, summaryeffsizes_table2_phyl, summarymodelfits_AIC_table, summarymodelfits_twoln_table)
  tables
}

