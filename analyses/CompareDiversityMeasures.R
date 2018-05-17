


di.shannon = function(labels){
  # counts for each label
  tx = table(labels)
  # convert to proportions
  tx = tx/sum(tx)
  -sum(tx * log(tx))
}

di.simpson = function(labels){
  # Full formula due to small datasets
  # (Hunter-Gaston index)
  n = table(labels)
  N = length(labels)
  sum(n * (n-1))/(N*(N-1))
}



ranDI = function(n){
  x = sample(1:n,10, replace = T)
  c(shannon=di.shannon(x), simpson=di.simpson(x))
}

par(mfrow=c(2,2))
for(n in c(2,5,10,20)){
  res = as.data.frame(t(replicate(10000,ranDI(n))))
  plot(res$shannon, res$simpson,
       xlab="Shannon",ylab="Simpson")
  title(paste0("Number of categories = ",n))
}


g = expand.grid(rep(list(1:4), 10))
g = t(apply(g,1,function(X){as.numeric(as.factor(X))}))
g = g[!duplicated(g),]
dsi = apply(g,1,di.simpson)
dsh = apply(g,1,di.shannon)

par(mfrow=c(1,1))
plot(dsh,dsi)
ch = chull(dsh,dsi)
ch = c(ch,ch[1])
lines(cbind(dsh,dsi)[ch,])

dsi2 = sapply(2:20, function(X){
  di.simpson(rep(1,X))
})

plot(2:20,dsi2)



