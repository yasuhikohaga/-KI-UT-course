#1. Use the R internal CO2 dataset (“data(CO2)”).
data("CO2")

#2. Describe briefly the content of the CO2 dataset using the help function.
help("CO2")
#The CO2 data frame has 84 rows and 5 columns of data from an experiment on the cold tolerance of the grass species Echinochloa crus-galli.

#3. What is the average and median CO2 uptake of the plants from Quebec and Mississippi?
avg.Quebec_uptake <- mean(CO2[CO2[,"Type"]=="Quebec","uptake"]) #[1] 33.54286
med.Quebec_uptake <- median(CO2[CO2[,"Type"]=="Quebec","uptake"]) #[1] 37.15
avg.Mississippi_uptake <- mean(CO2[CO2[,"Type"]=="Mississippi","uptake"]) #[1] 20.88333
med.Mississippi_uptake <- median(CO2[CO2[,"Type"]=="Mississippi","uptake"]) #[1] 19.3

#4. In the "airway" example data from Bioconductor, how many genes are expressed in each sample? How many genes are not expressed in any sample?
BiocManager::install("airway")
library(airway)
data("airway") 
airway_data <- airway@assays[["data"]]@listData[["counts"]]#extract count data
colnames(airway_data) <- airway@colData@rownames#rename sample name. data preparation is done
expressed_or_unexpressed_gene_num <- matrix(NA, nrow = 2, ncol = 8)
rownames(expressed_or_unexpressed_gene_num) <- c("expressed gene number", "unexpressed gene number")
colnames(expressed_or_unexpressed_gene_num) <- airway@colData@rownames #create matrix of answer
for (i in colnames(airway_data)) {
  expressed_or_unexpressed_gene_num["expressed gene number", i] <- expressed_gene_num <- sum(airway_data[,i] != 0)
  expressed_or_unexpressed_gene_num["unexpressed gene number", i] <- unexpressed_gene_num <- sum(airway_data[,i] == 0)
}
#expressed_or_unexpressed_gene_num is the answer of this question