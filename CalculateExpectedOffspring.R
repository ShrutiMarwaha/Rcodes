# Calculate expected number of offspring displaying the dominant phenotype http://rosalind.info/problems/iev/

# function to generate probability of dominant offspring (F1) from parent's genotypes
dominant_F1_probability <- function(parent_genotype)
{
  p1 <- strsplit(parent_genotype,"-")[[1]][1] # parent1's genotype
  p2 <- strsplit(parent_genotype,"-")[[1]][2] # parent2's genotype
  
  p1_genotypes <- unlist(strsplit(p1,""))
  p2_genotypes <- unlist(strsplit(p2,""))
  
  p1_genotypes[1] # parent1's allele
  p1_genotypes[2] # parent1's allele
  
  p2_genotypes[1] # parent2's allele
  p2_genotypes[2] # parent2's allele
  
  # 4 possible offspings
  F1 <-  c( paste0(p1_genotypes[1],p2_genotypes[1],collapse=" "), paste0(p1_genotypes[1],p2_genotypes[2],collapse=" "), paste0(p1_genotypes[2],p2_genotypes[1],collapse=" "), paste0(p1_genotypes[2],p2_genotypes[2],collapse=" ") )
  
  dominant_count <- 0
  for(i in seq_along(F1))
  {
    if(F1[i]=="AA" | F1[i]=="Aa" | F1[i]=="aA")
    {
      dominant_count = dominant_count + 1
    }
  }
  Pr_dominant_F1 <- dominant_count/4
  
  return(Pr_dominant_F1)
}

genotypes <- c("AA-AA","AA-Aa","AA-aa","Aa-Aa","Aa-aa","aa-aa")
genotypes_dominant_probability <- sapply(genotypes,dominant_F1_probability)

# function to calculate expected number of offspring displaying the dominant phenotype in the next generation, under the assumption that every couple has exactly same number of offsprings
dominant_F1_population <- function(couples_per_genotype,number_of_offsprings) 
# couples_per_genotype: number of couples in a population possessing each genotype pairing for a given factor
{
  expected_value <- 0
  for(i in seq_along(couples_per_genotype))
  {
    expected_value <- expected_value + (couples_per_genotype[i]*genotypes_dominant_probability[[i]])
  }
  return(expected_value*number_of_offsprings)
}

dominant_F1_population( c(1,0,0,1,0,1), 2)
dominant_F1_population( c(17636, 19549, 19895, 18623, 16520, 19352), 2)



