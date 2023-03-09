#1 install tidybiology
devtools::install_github("hirscheylab/tidybiology")
library(tidybiology)
library(ggplot2)
#load data sets
data("chromosome")
data("proteins")

#1-a
summary_of_chromosome <- chromosome %>% select(variations, protein_codinggenes, mi_rna) %>% 
                                      summarise(mean_variations = mean(variations),
                                                median_variations = median(variations),
                                                max_variations = max(variations),
                                                mean_protein_codinggenes = mean(protein_codinggenes),
                                                median_protein_codinggenes = median(protein_codinggenes),
                                                max_protein_codinggenes = max(protein_codinggenes),
                                                mean_miRNAs = mean(mi_rna),
                                                median_miRNAs = median(mi_rna),
                                                max_miRNAs = max(mi_rna))

#1-b
ggplot(chromosome, aes(x=id, y=basepairs)) + geom_col()

#1-c
#correlation between length of chromosome and number of protein coding genes.
ggplot(chromosome, aes(x = basepairs, y = protein_codinggenes)) + geom_point() +
  stat_smooth(method = lm, se = FALSE)
#correlation between length of chromosome and number of miRNAs.
ggplot(chromosome, aes(x = basepairs, y = mi_rna)) + geom_point() +
  stat_smooth(method = lm, se = FALSE)

#1-d
summary_of_proteins <- proteins %>% select(length, mass) %>%
                                    summarise(mean_length = mean(length),
                                              median_length = median(length),
                                              max_length = max(length),
                                              mean_mass = mean(mass),
                                              median_mass = median(mass),
                                              max_mass = max(mass))
ggplot(proteins, aes(x = length, y = mass)) + geom_point()
#The mass of protein is correlated with the length.
