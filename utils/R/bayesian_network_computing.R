compute_bayesian_network = function(learning.train,
                                    learning.test,
                                    well_name,
                                    source_name,
                                    phase = data.frame(exploration = 25,explotation=5)){

# Blacklist so that well_name is a well and source_name is a source
well = data.frame(from=rep(well_name,ncol(learning.train)-1),
                  to=colnames(learning.train[,colnames(learning.train)!=well_name]))

source = data.frame(to=rep(source_name,ncol(learning.train)-1),
                    from=colnames(learning.train[,colnames(learning.train)!=source_name]))

bl = bind_rows(list(well,source))

dag = hc(learning.train,restart=2,max.iter=20,blacklist = bl)
print('BIC resacled by -2')
print(score(dag,learning.train))

# Alternate exploration and explotation phase as tabu is a meta heuristic
for(i in 1:nrow(phase)){

dag = tabu(learning.train,tabu=phase$exploration[i],start = dag,blacklist = bl)
print('BIC resacled by -2')
print(score(dag,learning.train))
dag = tabu(learning.train,tabu=phase$explotation[i],start = dag,blacklist = bl)
print('BIC resacled by -2')
print(score(dag,learning.train))
}

# Fit coefficients
fitted_bn = bn.fit(dag,learning.train)

# Put in a format to display with visNetwork
edges = data.frame(dag$arcs)
nodes = data.frame(id = colnames(learning.train), label = colnames(learning.train))

# Pred
learning.test$pred = predict(fitted_bn,well_name,data = learning.test)

return(list(fit=fitted_bn,dag=dag,edges=edges,nodes=nodes,test=learning.test))
}