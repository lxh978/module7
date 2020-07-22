i <- 0
folderpath <- paste("C:/Users/HuaLi/Desktop/课程/module7/results/replica",i,"/pred_rules_jRip_species.csv",sep = "")  # read real dataset csv file from folder "replica0"
species_num_0 <- read.csv(folderpath,header = T,row.names = 1)
names <- colnames(species_num_0)
# read real dataset's csv files from folder "replica0"
i <- 1
folderpath <- paste("C:/Users/HuaLi/Desktop/课程/module7/results/replica",i,"/pred_rules_jRip_species.csv",sep = "")
species_num_list <- read.csv(folderpath,header = T,row.names = 1)
for (i in 2:30) {
  folderpath <- paste("C:/Users/HuaLi/Desktop/课程/module7/results/replica",i,"/pred_rules_jRip_species.csv",sep = "")
  species_num_list_new <- read.csv(folderpath,header = T,row.names = 1)
  species_num_list <- rbind(species_num_list,species_num_list_new)
}
# read 30 randomized datasets' csv files from folder "replica1-30"
good <- c()
bad <- c()
mid <- c()
for (i in 1:length(names)) {  # for each species, do 3 t-test
  x <- species_num_list[,i]
  x1 <- x[c(1,4,7,10,13,16,19,22,25,28,31,34,37,40,43,46,49,52,55,58,61,64,67,70,73,76,79,82,85,88)]  # get 30 good replicas
  x1 <- x1[!duplicated(x1)] # remove duplicates in population
  y1 <- species_num_0[1,i]
  if (length(x1)>1) {
    t1 <- t.test(x1,alternative = "two.sided", mu = y1)
    if (t1$p.value < 0.05) {    # here is the threshold of p-values
      good <- c(good,names[i],t1$p.value) # add records with p<0.05 into variable "good"
    }
  }
  x2 <- x[c(2,5,8,11,14,17,20,23,26,29,32,35,38,41,44,47,50,53,56,59,62,65,68,71,74,77,80,83,86,89)]  # get 30 bad replicas
  x2 <- x2[!duplicated(x2)]
  y2 <- species_num_0[2,i]
  if (length(x2)>1) {
    t2 <- t.test(x2,alternative = "two.sided", mu = y2)
    if (t2$p.value < 0.05) {    # here is the threshold of p-values
      bad <- c(bad,names[i],t2$p.value)
    }
  }
  x3 <- x[c(3,6,9,12,15,18,21,24,27,30,33,36,39,42,45,48,51,54,57,60,63,66,69,72,75,78,81,84,87,90)]  # get 30 mid replicas
  x3 <- x3[!duplicated(x3)]
  y3 <- species_num_0[3,i]
  if (length(x3)>1) {
    t3 <- t.test(x3,alternative = "two.sided", mu = y3)
    if (t3$p.value < 0.05) {    # here is the threshold of p-values
      mid <- c(mid,names[i],t3$p.value)
    }
  }
}
filename <- "C:/Users/HuaLi/Desktop/课程/module7/results/species_diff_t_test.txt"
printer <- file(filename,"w")
writeLines(paste(c("good","p-value"),good,sep = "\t"),con=printer)
writeLines(paste(c("bad","p-value"),bad,sep = "\t"),con=printer)
writeLines(paste(c("mid","p-value"),mid,sep = "\t"),con=printer)
close(printer)  # write all significant species with their p values into the results file

# plot the frequencies distributions of significant results
library(ggplot2)

# bad A7
p <- species_num_list[,7]
q <- species_num_0[2,7]
p2 <- p[c(2,5,8,11,14,17,20,23,26,29,32,35,38,41,44,47,50,53,56,59,62,65,68,71,74,77,80,83,86,89)]
p3 <- data.frame(p2)
ggplot(p3,aes(x=1,y=p2))+
  geom_dotplot(binaxis='y', stackdir='center',dotsize=1.2)+
  geom_point(aes(1,q),col="red",size=3)+
  xlab("A7 BAD")

# bad HB
p <- species_num_list[,15]
q <- species_num_0[2,15]
p2 <- p[c(2,5,8,11,14,17,20,23,26,29,32,35,38,41,44,47,50,53,56,59,62,65,68,71,74,77,80,83,86,89)]
p3 <- data.frame(p2)
ggplot(p3,aes(x=1,y=p2))+
  geom_dotplot(binaxis='y', stackdir='center',dotsize=1.2)+
  geom_point(aes(1,q),col="red",size=3)+
  xlab("HB BAD")

# bad L
p <- species_num_list[,18]
q <- species_num_0[2,18]
p2 <- p[c(2,5,8,11,14,17,20,23,26,29,32,35,38,41,44,47,50,53,56,59,62,65,68,71,74,77,80,83,86,89)]
p3 <- data.frame(p2)
ggplot(p3,aes(x=1,y=p2))+
  geom_dotplot(binaxis='y', stackdir='center',dotsize=1.2)+
  geom_point(aes(1,q),col="red",size=3)+
  xlab("L BAD")

# bad missing A
p <- species_num_list[,19]
q <- species_num_0[2,19]
p2 <- p[c(2,5,8,11,14,17,20,23,26,29,32,35,38,41,44,47,50,53,56,59,62,65,68,71,74,77,80,83,86,89)]
p3 <- data.frame(p2)
ggplot(p3,aes(x=1,y=p2))+
  geom_dotplot(binaxis='y', stackdir='center',dotsize=1.2)+
  geom_point(aes(1,q),col="red",size=3)+
  xlab("missing A BAD")

# mid HB
p <- species_num_list[,15]
q <- species_num_0[2,15]
p2 <- p[c(3,6,9,12,15,18,21,24,27,30,33,36,39,42,45,48,51,54,57,60,63,66,69,72,75,78,81,84,87,90)]
p3 <- data.frame(p2)
ggplot(p3,aes(x=1,y=p2))+
  geom_dotplot(binaxis='y', stackdir='center',dotsize=1.2)+
  geom_point(aes(1,q),col="red",size=3)+
  xlab("HB MID")