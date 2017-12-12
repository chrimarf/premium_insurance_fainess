rm(list=ls())
set.seed(10765)

# Libraries
library(dplyr)
library(bnlearn)
library(visNetwork)
source('../../utils/R/bayesian_network_computing.R')

# Data
df = readRDS('../../data/data_explored.RDS')
df = df %>% na.omit()
df[sapply(df,is.factor)] = df[sapply(df,is.factor)] %>%
  mutate_all(funs(factor)) %>%
  as.data.frame()

# Split train test
smp_size <- floor(0.85 * nrow(df))
train_ind <- sample(seq_len(nrow(df)), size = smp_size)

learning.train <- df[train_ind, ]
learning.test <- df[-train_ind, ]

bn_mod = compute_bayesian_network(learning.train,learning.test,'prime_tot_ttc','sex')
edges = bn_mod$edges
nodes = bn_mod$nodes
learning.test = bn_mod$test

visNetwork(nodes,edges) %>% 
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
  visEdges(arrows = 'to')

# Check performances
learning.test$pred = predict(fitted_bn,'prime_tot_ttc',data = learning.test)
learning.test$mape = abs(learning.test$pred - learning.test$prime_tot_ttc)/learning.test$prime_tot_ttc
learning.test$mae = abs(learning.test$pred - learning.test$prime_tot_ttc)

print(paste0('Median APE : ',as.character(median(learning.test$mape))))
print(paste0('Mean APE : ',as.character(mean(learning.test$mape))))
print(paste0('Median AE : ',as.character(median(learning.test$mae))))
print(paste0('Mean AE : ',as.character(mean(learning.test$mae))))

# To simplify the problem we only take the children in the graph of prime_tot_ttc

df_reduced = df[,append(bn_mod$fit$prime_tot_ttc$parents,"prime_tot_ttc")]

smp_size <- floor(0.85 * nrow(df_reduced))
train_ind <- sample(seq_len(nrow(df_reduced)), size = smp_size)

learning.train <- df_reduced[train_ind, ]
learning.test <- df_reduced[-train_ind, ]

bn_mod = compute_bayesian_network(learning.train,learning.test,'prime_tot_ttc','sex')
edges = bn_mod$edges
nodes = bn_mod$nodes
learning.test = bn_mod$test

visNetwork(nodes,edges) %>% 
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
  visEdges(arrows = 'to',length = 300)

# Check performances
learning.test$pred = predict(fitted_bn,'prime_tot_ttc',data = learning.test)
learning.test$mape = abs(learning.test$pred - learning.test$prime_tot_ttc)/learning.test$prime_tot_ttc
learning.test$mae = abs(learning.test$pred - learning.test$prime_tot_ttc)

print(paste0('Median APE : ',as.character(median(learning.test$mape))))
print(paste0('Mean APE : ',as.character(mean(learning.test$mape))))
print(paste0('Median AE : ',as.character(median(learning.test$mae))))
print(paste0('Mean AE : ',as.character(mean(learning.test$mae))))

# All the variables are descendant of sex. 