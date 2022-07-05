library(dplyr)
library(tidyverse)

args <- commandArgs(trailingOnly=TRUE)
if (length(args) < 3) {
  print.usage()
  stop()
}

domains.1 <- args[1]
domains.2 <- args[2]
gap.threshold <- args[3]


table1<-read.table(sprintf("%s",domains.1)
table2<-read.table("%s",domains.2)

num.t1<-nrow(table1)
num.t2<-nrow(table2)

table1$chr<-c('chr')
table2$chr<-c('chr')

table1$V1<-str_c(table1$chr,table1$V1)
table2$V1<-str_c(table2$chr,table2$V1)

table1<-table1[,1:3]
table2<-table2[,1:3]

colnames(table1)<-c('chr','start','end')
colnames(table2)<-c('chr','start','end')

conserved.domains<-c("chr","start","end")

for (o in 1:22){
  t1<-subset(table1,chr==sprintf("chr%s",o))
  t2<-subset(table2,chr==sprintf("chr%s",o))
  num.t1<-nrow(t1)
  for (i in 1:num.t1){
    min.start<-min(abs(t1[i,2]-t2[,2]))
    min.end<-min(abs(t1[i,3]-t2[,3]))
    if (min.start<=gaps.threshold){
      if (min.end<=gaps.threshold){
        conserved.domains<-rbind(conserved.domains,c(sprintf("chr%s",o),t1[i,2],t1[i,3]))
      }
    }  
  }
}

t1<-subset(table1,chr=="chrX")
t2<-subset(table2,chr=="chrX")
num.t1<-nrow(t1)
for (i in 1:num.t1){
  min.start<-min(abs(t1[i,2]-t2[,2]))
  min.end<-min(abs(t1[i,3]-t2[,3]))
  if (min.start<=gaps.threshold){
    if (min.end<=gaps.threshold){
      conserved.domains<-rbind(conserved.domains,c("chrX",t1[i,2],t1[i,3]))
    }
  }
}

write.table(conserved.domains,file="conserved domains.bed",row.names = FALSE,col.names = FALSE,quote = FALSE)

