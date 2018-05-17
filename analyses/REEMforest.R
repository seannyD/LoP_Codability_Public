library(REEMtree)
# Some functions to implement a cforest style variable importance 
# measure for REEMtree.

# REEMtree.sample runs the same REEMtree on a sub-sample of the data and variables
# REEMtree.varimp runs many of these and returns the mean variable importance

REEMtree.sample = function(data,dep.var, indep.var,rf, mtry, fraction){
  
  indep.sample = sample(indep.var, mtry)
  
  f.sample = as.formula(paste(dep.var,"~",paste(indep.sample,collapse = '+')))
  
  data.sample = data[sample(1:nrow(data),nrow(data)*fraction),]
  
  rt = REEMtree(f.sample,
                random = rf,
                data = data.sample)
  
  varimp = tree(rt)$variable.importance
  varimp[indep.var]
}


REEMtree.varimp = function(f,rf, mtry = 5, ntree = 100, fraction = 0.632){

  vars = all.vars(f)
  dep.var = vars[1]
  indep.var = vars[2:length(vars)]
  
  varimp = replicate(ntree,REEMtree.sample(data,dep.var,indep.var,rf, mtry, fraction))
  
  varimp = rowMeans(varimp, na.rm = T)
  names(varimp) = indep.var
  return(varimp)
}
