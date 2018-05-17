ISI.method2 <- function(X,names=NULL,tries=100){
  N <- nrow(X)  
  if (is.null(names)) names <- paste('Ind.',c(1:N))  # IF vector names is NULL rename it #
  # Matrices need to be transformed into vectors in order to be passed to a .C routine #
  DminS <- rowSums(sign(X-t(X)))
  ord <- rank(-DminS, ties.method='random')
  Xc <- X[ord,ord]
  namesord<- names[ord]
  vecX<-c(t(Xc))
  out <- .C("ISImethod",as.double(vecX),
            as.integer(N),
            as.character(namesord),
            as.integer(tries),
            matord=double(N*N),
            namord=character(N),
            PACKAGE="DyaDA")
  
  names.ordered <- out$namord
  BestXthusfar <- matrix(out$matord,nrow=nrow(X),ncol=ncol(X),byrow=TRUE,
                         dimnames=list(names.ordered,names.ordered))
  
  res <- list (call=match.call(),dataIni=X,dataFinal=BestXthusfar,initialI=DyaDA:::ISI.comp(X)[[1]],
               initialSI=DyaDA:::ISI.comp(X)[[2]], iniInfo=DyaDA:::ISI.info(X)[[2]],finalI=DyaDA:::ISI.comp(BestXthusfar)[[1]],
               finalSI=DyaDA:::ISI.comp(BestXthusfar)[[2]],finalInfo=NA)
  class(res) <- "isi"
  res 
}