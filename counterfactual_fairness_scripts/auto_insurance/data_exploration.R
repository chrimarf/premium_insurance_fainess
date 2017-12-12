rm(list=ls())

# Libraries
library(dplyr)
library(ggplot2)

# Data
df = read.csv('../../data/ech_apprentissage.csv',sep=';')

# Convert types and remove var1 = 2016 - annee_permis
df[sapply(df,is.numeric)] = df[sapply(df,is.numeric)] %>% 
  mutate_all(funs(as.numeric))
df = df %>% dplyr::select(-id,-annee_permis) %>%
  dplyr::rename(nb_annee_permis = var1)

# Each variable with less than 15 unique values is considered as a category
less_than_x_unique = function(x){
  length(unique(x))<15
}

df[sapply(df,less_than_x_unique)] = df[sapply(df,less_than_x_unique)] %>%
  mutate_all(funs(as.factor))

# Remove individuals with underrepresentated marque
# Remove factor with two much modalities => Further works on it
df = df %>%
  split(df$marque) %>%
  lapply(function(splitted){
    if(nrow(splitted)<100) return(NULL)
    return(splitted)
  }) %>% bind_rows() %>%
  dplyr::select(-codepostal, -marque)

# reinitialise factor variables
df[sapply(df,is.factor)] = df[sapply(df,is.factor)] %>% 
  mutate_all(funs(factor))

# Explore variables with density plots and histograms
print(summary(df))

for(col in colnames(df[sapply(df,function(col) is.numeric(col))])){
  p = ggplot(df,aes_(x=as.name(col))) +
    geom_density() +
    ggtitle(paste0('Density of ',col))
  print(p)
}

for(col in colnames(df[sapply(df,function(col) is.factor(col))])){
  p = ggplot(df,aes_(x=as.name(col))) +
    geom_bar() +
    ggtitle(paste0('Frequency by modality of ',col))
  print(p)
}

# Transform variables according to the exploration insights

# puis_fiscal = ordinal var.
df$puis_fiscale[df$puis_fiscale<4] = 4
df$puis_fiscale[df$puis_fiscale>15] = '>15'
df$puis_fiscale[df$puis_fiscale=='4'] = '<4'
df$puis_fiscale = as.factor(df$puis_fiscale)

# anc_veh to regroup
df$anc_veh = cut(df$anc_veh,c(min(df$anc_veh)-1,10,25,50,max(df$anc_veh)))

# var11 = anc_veh
df = df %>% dplyr::select(-var11)

# kmage_annuel to regroup
df$kmage_annuel = cut(df$kmage_annuel,
                      c(min(df$kmage_annuel)-1,5000,10000,max(df$kmage_annuel))
                      )

# var 19 to category
df$var19[df$var19>18] = '>18'
df$var19 = as.factor(df$var19)

# var22 to category
df$var22[df$var22>15] = '>15'
df$var22 = as.factor(df$var22)

df[sapply(df,is.factor)] = df[sapply(df,is.factor)] %>%
  mutate_all(funs(factor))

# var5 is identififed as the gender of the individual
df = df %>% dplyr::rename(sex=var5)

saveRDS(df, '../../data/data_explored.RDS')
