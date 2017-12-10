learning = df %>% 
  dplyr::filter(anc_veh<60) %>%
  na.omit() %>%
  as.data.frame() %>%
  dplyr::select(-marque)

learning = learning[,!(colnames(learning) %in% childrens) | colnames(learning)=='prime_tot_ttc']

print(summary(learning))

smp_size <- floor(0.85 * nrow(learning))
train_ind <- sample(seq_len(nrow(learning)), size = smp_size)

learning.train <- learning[train_ind, ]
learning.test <- learning[-train_ind, ]

bl = data.frame(from=rep('prime_tot_ttc',ncol(learning.train)-1),
                to=colnames(learning.train[,colnames(learning.train)!='prime_tot_ttc']))

bl2 = data.frame(from=rep('',ncol(learning.train)-1),
                to=colnames(learning.train[,colnames(learning.train)!='prime_tot_ttc']))

mod1 = hc(learning.train,restart=2,max.iter=20,blacklist = bl)
print(score(mod1,learning.train))
#mod1 = tabu(learning.train,tabu=150,start = mod1,blacklist = bl)
#print(score(mod1,learning.train))
mod1 = tabu(learning.train,tabu=5,start = mod1,blacklist = bl)
print(score(mod1,learning.train))
mod1 = tabu(learning.train,tabu=25,start = mod1,blacklist = bl)
print(score(mod1,learning.train))
mod1 = tabu(learning.train,tabu=5,start = mod1,blacklist = bl)
print(score(mod1,learning.train))

fitted_bn = bn.fit(mod1,learning.train)

edges = data.frame(mod1$arcs)
nodes = data.frame(id = colnames(learning.train), label = colnames(learning.train))

visNetwork(nodes,edges) %>% 
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>% visEdges(arrows ="to")  

learning.test$pred = predict(fitted_bn,'prime_tot_ttc',data = learning.test)
learning.test$mape = abs(learning.test$pred - learning.test$prime_tot_ttc)/learning.test$prime_tot_ttc
learning.test$mae = abs(learning.test$pred - learning.test$prime_tot_ttc)

learning.train$pred = predict(fitted_bn,'prime_tot_ttc',data = learning.train)
learning.train$mape = abs(learning.train$pred - learning.train$prime_tot_ttc)/learning.train$prime_tot_ttc

childrens = c()
explored = c()
unexplored = c()
all = colnames(learning)
stopp = FALSE

vv = function(x,childrens){
  explored = append(explored,x)
  cols = fitted_bn[[x]]$children
  if (all(cols %in% explored)){
    stopp == TRUE
  } else {
    for(col in cols){
      if (!(col %in% childrens)) {
        childrens = append(childrens,col)
      }
      if(!(col %in% unexplored)){
        unexplored = append(unexplored,col)
      }
    }
  }
  return(list(childrens,explored,unexplored,stopp))
}

ha = vv('var5',childrens)
childrens = ha[[1]]
explored = ha[[2]]
unexplored = ha[[3]]
stopp = ha[[4]]

while(length(explored)<=length(all) & !stopp & length(unexplored)>0){
  for(var in unexplored){
    if(!(var %in% explored)){
      print(var)
      ha = vv(var,childrens)
      childrens = ha[[1]]
      explored = ha[[2]]
      unexplored = ha[[3]]
      stopp = ha[[4]]
      unexplored = unexplored[!which(unexplored==var)]
    }
    print(unexplored)
  }
}

childrens = childrens[!which(childrens=='prime_tot_ttc')]

train_matrix = xgb.DMatrix(data.matrix(learning.train %>% dplyr::select(-prime_tot_ttc)),
                           label = learning.train$prime_tot_ttc)
test_matrix = xgb.DMatrix(data.matrix(learning.test %>% dplyr::select(-prime_tot_ttc)),
                          label = learning.test$prime_tot_ttc)

cv_folds <- KFold(learning.train$prime_tot_ttc,
                  stratified = FALSE,nfolds=3)

evalerror <- function(preds, train_matrix) {
  labels <- getinfo(train_matrix, "label")
  err <- as.numeric(median(abs(labels - preds)))
  return(list(metric = "error", value = err))
}

xgb_cv_bayes <- function(max.depth, min_child_weight, eta,nrounds) {
  cv <- xgb.cv(params = list(booster = "gbtree", eta = eta,
                             max_depth = max.depth,
                             min_child_weight = min_child_weight,
                             subsample = 0.8,
                             objective = "reg:linear",
                             eval_metric = evalerror),
               data = train_matrix, nround = 350,
               folds = cv_folds, prediction = TRUE, showsd = TRUE,
               maximize = TRUE, verbose = TRUE)
  saveRDS(cv,'../data/mod.RDS')
  list(Score = -cv$evaluation_log[, min(test_error_mean)],
       Pred = cv$pred)
}
OPT_Res <- BayesianOptimization(xgb_cv_bayes,
                                bounds = list(
                                  eta = c(0.01,0.4),
                                  max.depth = c(2L, 6L),
                                  min_child_weight = c(4L, 10L)),
                                init_grid_dt = NULL, init_points = 20, n_iter = 10,
                                acq = "ei",
                                verbose = TRUE)

mod = xgb.train(params = list(booster = "gbtree", eta = 0.20,
                              max_depth = 6,
                              min_child_weight = 4,
                              subsample = 0.8,
                              eval_metric = evalerror),
                data = train_matrix,
                watchlist=list(train=train_matrix,eval=test_matrix),
                nrounds=400)

learning.train$pred = predict(mod,train_matrix)

learning.test$mape = abs(learning.test$pred - learning.test$prime_tot_ttc)/learning.test$prime_tot_ttc
learning.test$mae = abs(learning.test$pred - learning.test$prime_tot_ttc)
