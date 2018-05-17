setwd("~/Documents/Bristol/Codability/LoP_Codability/processing//")

shannon.diversityIndex = function(labels){
  # counts for each label
  tx = table(labels)
  # convert to proportions
  tx = tx/sum(tx)
  -sum(tx * log(tx))
}

simpson.diversityIndex = function(labels){
  # Full formula due to small datasets
  # (Hunter-Gaston index)
  n = table(labels)
  N = length(labels)
  sum(n * (n-1))/(N*(N-1))
}

BnL.diversityIndex = function(labels){
  #CR-DR+20
  #where, DR is the number of different responses a stimulus item receives 
  #and CR is the number of subjects who agree on the most common name
  DR = length(unique(labels))
  CR = max(table(labels))
  return(CR-DR+20)
}

mean.number.of.words = function(labels){
  # group sign language signs together
  labels = gsub("\\[[a-zA-Z -']+\\]"," X ",labels)
  # remove punctuation
  labels = gsub("[-\\?,\\:;\\.]","",labels)
  # remove double spaces
  labels = gsub(" +"," ",labels)
  mean(sapply(strsplit(labels," "), length))
}



d = read.csv("../data/AllData_LoP.csv", stringsAsFactors = F, fileEncoding = 'utf-8', encoding = 'utf-8')


d.first = d[d$Response==1,]
# Remove no descriptions
d.first = d.first[!d.first$head %in% c('n/a'),]
d.first = d.first[!is.na(d.first$head),]

getMeasure = function(dxx, diversityFunction, removeNoDescriptions =T){
  
  if(removeNoDescriptions){
    dxx = dxx[dxx$head !="no description",]
  } else{
    dxx$head[dxx$head=="no description"] = paste0("X",1:sum(dxx$head=="no description"))
  }

  m.simps = tapply(dxx$head, 
                 paste(dxx$Language,
                       dxx$domain, 
                       dxx$Stimulus.code, sep='!'), 
                 diversityFunction)
  
  d.simpson = data.frame(
    matrix(unlist(strsplit(names(m.simps),"!")), 
           nrow=length(m.simps), 
           byrow=T),
    stringsAsFactors=FALSE)
  
  d.simpson = cbind(d.simpson, diversity=m.simps)
  names(d.simpson) = c("Language","domain",'Stimulus.code',deparse(substitute(diversityFunction)))
  return(d.simpson)
}

getN = function(dxx, removeNoDescriptions =T){
  
  if(removeNoDescriptions){
    dxx = dxx[dxx$head !="no description",]
  } else{
    dxx$head[dxx$head=="no description"] = paste0("X",1:sum(dxx$head=="no description"))
  }
  
  nx = tapply(dxx$head, 
                   paste(dxx$Language,
                         dxx$domain, 
                         dxx$Stimulus.code, sep='!'), 
                   function(X){length(unique(X))})
  
  d.nx = data.frame(
    matrix(unlist(strsplit(names(nx),"!")), 
           nrow=length(nx), 
           byrow=T),
    stringsAsFactors=FALSE)
  
  d.nx = cbind(d.nx, N=nx)
  names(d.nx) = c("Language","domain",'Stimulus.code',"N")
  return(d.nx)
}

getNumWords = function(dxx, removeNoDescriptions =T){
  if(removeNoDescriptions){
    dxx = dxx[dxx$head !="no description",]
  } else{
    dxx$full[dxx$head=="no description"] = paste0("X",1:sum(dxx$head=="no description"))
  }
  
  nx = tapply(dxx$full, 
              paste(dxx$Language,
                    dxx$domain, 
                    dxx$Stimulus.code, sep='!'), 
              mean.number.of.words)
  
  d.nx = data.frame(
    matrix(unlist(strsplit(names(nx),"!")), 
           nrow=length(nx), 
           byrow=T),
    stringsAsFactors=FALSE)
  
  d.nx = cbind(d.nx, N=nx)
  names(d.nx) = c("Language","domain",'Stimulus.code',"mean.number.of.words")
  return(d.nx)
}

d.simpson = getMeasure(d.first,simpson.diversityIndex)
d.simpson.nd = getMeasure(d.first,simpson.diversityIndex,F)

d.shannon = getMeasure(d.first,shannon.diversityIndex)
d.shannon.nd = getMeasure(d.first,shannon.diversityIndex,F)

d.BnL= getMeasure(d.first,BnL.diversityIndex)
d.BnL.nd = getMeasure(d.first,BnL.diversityIndex,F)

d.lcount = getN(d.first)
d.nd.lcount = getN(d.first,F)

d.wcount = getNumWords(d.first)
d.nd.wcount = getNumWords(d.first,F)

res = cbind(d.simpson, 
            shannon.diversityIndex = d.shannon$shannon.diversityIndex,
            N = d.lcount$N,
            BnL.diversityIndex = d.BnL$BnL.diversityIndex,
            mean.number.of.words = d.wcount$mean.number.of.words)

res.nd = cbind(d.simpson.nd, 
            shannon.diversityIndex = d.shannon.nd$shannon.diversityIndex,
            N = d.nd.lcount$N,
            BnL.diversityIndex = d.BnL.nd$BnL.diversityIndex,
            mean.number.of.words = d.nd.wcount$mean.number.of.words)


# Check measures match old calculations
res.old = read.csv("~/Dropbox/LoP-article/final_datasheets/simpson.first.agreement.csv")
res.old = res.old[,1:4]
d = res[res$Language=="English",]
d$code = sapply(d$Stimulus.code, function(X){
  strsplit(X,"\\.")[[1]][2]
})
d$simpson.old = res.old[match(d$code,res.old$code),]$English
plot(d$simpson.old, d$simpson.diversityIndex)
abline(0,1)

res.old = read.csv("~/Dropbox/LoP-article/final_datasheets/shannon.first.agreement.csv")
res.old = res.old[,1:4]
d = res[res$Language=="English",]
d$code = sapply(d$Stimulus.code, function(X){
  strsplit(X,"\\.")[[1]][2]
})
d$shannon.old = res.old[match(d$code,res.old$code),]$English
plot(d$shannon.old, d$shannon.diversityIndex)
abline(0,1)

#d[abs(d$shannon.old-d$shannon.diversityIndex)>0.05,]


# With and without no descriptions:

plot(res$shannon.diversityIndex,
     res.nd$shannon.diversityIndex[names(res$shannon.diversityIndex)],
     xlab = "Shannon without ND",
     ylab = "Shannon with ND")

plot(res$simpson.diversityIndex,
     res.nd$simpson.diversityIndex[names(res$simpson.diversityIndex)],
     xlab = "Simpson without ND",
     ylab = "Simpson with ND")


write.csv(res, "../data/DiversityIndices.csv", row.names = F, fileEncoding = 'utf-8')

write.csv(res.nd, "../data/DiversityIndices_ND.csv", row.names = F, fileEncoding = 'utf-8')

#################
# Get data on mean lengths and diversity indices.

library(XLConnect)
library(reshape2)
library(dplyr)

s = read.csv("../data/DiversityIndices_ND.csv", stringsAsFactors = F)

dorig = read.csv("../data/consultant.full.response.length.csv", stringsAsFactors=F)

d = as.data.frame(
  melt(dorig, id.vars = c("domain",'stimulus','code'),
       value.name = "length"),
  stringsAsFactors=F
)
names(d)[4] = "consultant"
d$consultant = as.character(d$consultant)

d$code = gsub(" $","",d$code)


d$Stimulus.code = paste0(d$domain,".",d$code)

# Collapse sound categories
s$Stimulus.code[s$domain=="sound"] = 
  sapply(s$Stimulus.code[s$domain=="sound"],
         function(X){
           x =strsplit(X,"\\.")[[1]]
           paste(x[1:(length(x)-1)],collapse='.')
         })
s$Stimulus.code = gsub("sound\\.sound","sound",s$Stimulus.code)

dc = unique(d$Stimulus.code)
dc[!dc%in% unique(s$Stimulus.code)]



d$Language = sapply(d$consultant, function(X){
  strsplit(X,"_")[[1]][1]
})

sl = sort(unique(s$Language))
names(sl) = unique(d$Language)

d$Language = sl[d$Language]

# Mean length for each domain
d2 = d %>% group_by(Language,domain,Stimulus.code) %>%
  summarise (mean = mean(length, na.rm=T),
             mean.log = mean(log(length+0.5),na.rm=T))

d2 = d2[!is.na(d2$Language),]

d2$simpson.diversityIndex = NA
for(lang in unique(d2$Language)){
  doms = unique(d2[d2$Language==lang,]$domain)
  doms = doms[!is.na(doms)]
  for(dom in doms){
    sel = d2$Language==lang & d2$domain == dom
    sel.s = s$Language==lang & s$domain==dom
    d2[sel,]$simpson.diversityIndex = s[sel.s,][
      match(d2[sel,]$Stimulus.code,s[sel.s,]$Stimulus.code),]$simpson.diversityIndex
  }
}

write.csv(d2,"../data/DiversityIndices_ND_withLengths.csv",
          row.names = F)



