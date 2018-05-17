# Load data from the raw excel sheets and combine into a single data frame (`data/AllData_LoP.csv`)


library(XLConnect)
library(reshape2)
setwd("~/Documents/Bristol/Codability/LoP_Codability/processing/")


getColumn = function(dx, col, row1,consultants){
  stopifnot(col %in% row1)
  dx.head = dx[,c(1:3,which(row1==col))]
  names(dx.head)[4:ncol(dx.head)] = consultants
  dx.head = melt(dx.head,id.vars = c("Response","Stimulus.number",'Stimulus.code'))
  names(dx.head)[4:5] = c("consultant",col)
  return(dx.head)
}

processSheet = function(dx){
  row1 = dx[1,]
  print(sort(as.vector(unique(t(row1)))))
  dx = dx[2:nrow(dx),]
  names(dx)[3] = "Stimulus.code"
  consultantColNames = names(dx)[4:ncol(dx)]
  consultants = unique(gsub("([0-9]+)\\.[0-9]+","\\1",consultantColNames))
  
  # check consultants are named correctly
  x = gsub("([0-9]+)\\.[0-9]+","\\1",consultantColNames)
  x = as.numeric(sapply(x,function(X){strsplit(X,"\\.")[[1]][2]}))
  stopifnot(all(x==cummax(x)))
  
  dx.head = getColumn(dx,'head',row1,consultants)
  dx.full = getColumn(dx,'full response',row1,consultants)
  dx.SAE = getColumn(dx,'SAE',row1,consultants)
  
  stopifnot(nrow(dx.SAE)==nrow(dx.head))
  
  dx.out = cbind(dx.head, full=dx.full$full ,SAE=dx.SAE$SAE)
  
  return(dx.out)
}


domains = c("colour",'shape','sound','touch','taste','smell')

files = list.files("../data/final_raw_datasheets/","*.xls")

toExclude = c("Mian taste",'Semai taste','Benchnon sound','Yurakare sound',"Kata Kolok sound",'ASL sound','BSL sound')

allData = data.frame()
for(f in files){
  lx = strsplit(f,'_')[[1]]
  lang = paste(lx[2:(length(lx)-1)], collapse=" ")
  for(domain in domains){
    if(!paste(lang,domain) %in% toExclude){
      print(paste(f,domain))
      dx = readWorksheetFromFile(paste0("../data/final_raw_datasheets/",f), domain)
      dx = processSheet(dx)
      dx$Language = lang
      dx$domain = domain
      dx$Stimulus.code = paste(dx$domain,dx$Stimulus.code, sep='.')
      if(domain=='sound'){
        dx$Stimulus.code = paste(dx$domain,dx$Stimulus.code, dx$Stimulus.number,sep='.')
      }
      dx$consultant = paste0(lang,".",dx$consultant)
      allData = rbind(allData,dx)
    }
  }
}



checkResponseNumbering = tapply(allData$Response, paste(allData$Language,allData$domain, allData$consultant, allData$Stimulus.code), function(X){all(X==cummax(X))})
all(checkResponseNumbering)


# check if we're missing stimulus types
for(domain in unique(allData$domain)){
  numcats = tapply(allData[allData$domain==domain,]$Stimulus.code, 
         allData[allData$domain==domain,]$Language, 
         function(X){
          length(unique(X))
        })
  print(domain)
  print(length(unique(numcats))==1)
}

facrossLangs = tapply(allData$Language, allData$head, function(X){
  length(unique(X))
})

tail(sort(facrossLangs))

# Do some text cleaning

# Remove leading and trailing spaces
allData$head = gsub(" +$","",allData$head)
allData$head = gsub("^ +","",allData$head)

allData$head[allData$head==""] = NA
allData$head[allData$head==" "] = NA

allData = allData[!is.na(allData$head),]

sel = !allData$Language %in% c("ASL","BSL", "Kata Kolok")
spellingMistakes = tapply(
  allData[sel,]$head, 
  paste(allData[sel,]$Language, allData[sel,]$domain), 
  function(x){
    X = unique(x[!is.na(x)])
    dist = adist(X)
    diag(dist) = Inf
    dist[lower.tri(dist)] = Inf
    c1 = X[which(dist==1,arr.ind = T)[,1]]
    c2 = X[which(dist==1,arr.ind = T)[,2]]
    lowprob = xor(table(x)[c(c1)]==1,table(x)[c(c2)]==1)
    c1 = c1[lowprob]
    c2 = c2[lowprob]
    paste(c1,c2, sep=' // ')
})

spellingMistakes[sapply(spellingMistakes,length)>0]

# Fix a typo in the data (fixed in local data, but might persist in other copies of the raw data)
allData[allData$Stimulus.code == "shape.3 square",]$Stimulus.code = 'shape.3 squares'


write.csv(allData,file="../data/AllData_LoP.csv", row.names = F, fileEncoding = 'utf-8')
