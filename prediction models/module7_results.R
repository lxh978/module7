library(xlsx)
library(party)
library(rpart)
library(rpart.plot)
library(pROC)
library(C50)
library(gmodels)
library(RWeka)

a1 <- read.xlsx("C:/Users/HuaLi/Desktop/¿Î³Ì/module7/Area1_random.xlsm", sheetIndex = 4, header = TRUE)
a2 <- read.xlsx("C:/Users/HuaLi/Desktop/¿Î³Ì/module7/Area2_random.xlsm", sheetIndex = 4, header = TRUE) 
a3 <- rbind(a1,a2)  # read and combine input data from two areas


foldname <- "C:/Users/HuaLi/Desktop/¿Î³Ì/module7/results/replica30" # the folder path of saving all results from the same input dataset
dir.create(foldname)
rules <- c()
for (i in 1:1000){  # repeat 1000 decision tree model training 
  a4 <- a3[sample(1:nrow(a3),nrow(a3),replace = F),]  # randomize the input data to make sure the diffrences in iterations
  a4$Ash[which(a4$Ash=="A2")] <- "A1"
  a4$Ash[which(a4$Ash=="A3")] <- "A1"
  a4$Ash[which(a4$Ash=="A4")] <- "A1"
  a4$Ash[which(a4$Ash=="A8")] <- "A7"     # Decrease the disease categories into A1(A1-A4) A5 A6 A7(A7-A8) based on former research
  
  a_cut <- round(nrow(a4)*0.7)
  a_cut2 <- a_cut+1
  a_train <- a4[1:a_cut,]
  a_test <- a4[a_cut2:nrow(a4),]    # 70% as train dataset, 30% as test dataset
  
  tree_id3 <- rpart(Ash ~ . , data = a_train , method = "class" , parms = list(split="information"))
  printcp(tree_id3)
  tree_id3_prune <- prune(tree_id3,cp=0.001000)   # decision tree
  
  filename <- paste(foldname,"/",i,".pdf",sep = "")
  pdf(filename)
  rpart.plot(tree_id3_prune,branch=1,fallen.leaves=T,cex=0.6,box.palette="Blues")
  dev.off() # save the tree plot
  
  pre_id3 <- predict(tree_id3_prune,data=a_test,type="class")
  obs_id3 <- data.frame(prob=pre_id3[1:(nrow(a4)-a_cut2+1)],obs=a_test$Ash)
  pre_table <- capture.output(table(a_test$Ash,pre_id3[1:(nrow(a4)-a_cut2+1)],dnn=c("true","predict"))) # prediction
 
  filename <- paste(foldname,"/pred_results_id3_",i,".txt",sep = "")
  printer <- file(filename,"w")
  writeLines(pre_table,con=printer)
  close(printer)  # save the prediction performance table
  
  a4_0 <- a4
  a4_0[is.na(a4_0)] <- 0
  train_1r <- a4_0[1:a_cut,]
  test_1r <- a4_0[a_cut2:nrow(a4),-1]
  test_1r[is.na(test_1r)] <- 0
  test_1r.label <- a4_0[a_cut2:nrow(a4),1]
  
  
  #JRip
  m <- JRip(Ash ~ .,data=train_1r)
  pred <- predict(m,test_1r)
#  CrossTable(pred,test_1r.label,prop.r = F,prop.t = F,prop.chisq = F)
  rule <- capture.output(m)
  rule <- rule[-c(1,2,3)]
  
  rules <- c(rules,rule)  # all associated important rules in every cycle
}

filename <- paste(foldname,"/pred_rules_jRip.txt",sep = "")
printer <- file(filename,"w")
writeLines(rules,con=printer)
close(printer)  # save 1000 sets of rules into one file

rules_bad <- grep("Ash=A7",rules,value = T)
rules_good <- grep("Ash=A1",rules,value = T)
rules_mid <- grep("Ash=A5|Ash=A6",rules,value = T) # divided the rules based on their output

filename <- paste(foldname,"/pred_rules_jRip_good.txt",sep = "")
printer <- file(filename,"w")
writeLines(rules_good,con=printer)
close(printer)

filename <- paste(foldname,"/pred_rules_jRip_bad.txt",sep = "")
printer <- file(filename,"w")
writeLines(rules_bad,con=printer)
close(printer)

filename <- paste(foldname,"/pred_rules_jRip_mid.txt",sep = "")
printer <- file(filename,"w")
writeLines(rules_mid,con=printer)
close(printer)          # save different rules with different output

species <- c(colnames(a3[,-1]))
species_rules_num <- data.frame(row.names = c("good","bad","mid"))
for (s in species) {
  s_good_num <- length(grep(paste(s," <=",sep = ""),rules_good))
  s_bad_num <- length(grep(paste(s," <=",sep = ""),rules_bad))
  s_mid_num <- length(grep(paste(s," <=",sep = ""),rules_mid))
  species_rules_num[s]=c(s_good_num,s_bad_num,s_mid_num)
  
}   # count the occurances of each species in different rules

filename <- paste(foldname,"/pred_rules_jRip_species.csv",sep = "")
write.csv(species_rules_num,file = filename)  # save the frequences of each species for t test
