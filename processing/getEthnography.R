library(XLConnect)
library(measurements)
setwd("~/Documents/Bristol/Codability/LoP_Codability/processing/")

# convert ethnography
s = read.csv("../data/DiversityIndices_ND.csv")


ethnography = readWorksheetFromFile("../data/ethnography/LoP-ethnography-20171004.xls",1)

# Flip so variables are rows
ethnography = t(ethnography)
colnames(ethnography) = ethnography[4,]
ethnography = as.data.frame(ethnography)

# Fix language names
ethnography = ethnography[5:nrow(ethnography),]
ethnography$Language = rownames(ethnography)
ethnography$Language = gsub("\\.",' ', ethnography$Language)
ethnography$Language.orig = ethnography$Language

# check langauges are correct
unique(s$Language)[!unique(s$Language) %in% ethnography$Language]
ethnography$Language[!ethnography$Language %in% unique(s$Language)]

# Relabel languages
ethnography$Language[ethnography$Language=="American Sign Language"] = "ASL"
ethnography$Language[ethnography$Language=="British Sign Language"] = "BSL"
ethnography$Language[ethnography$Language=="Yélî Dnye"] = "Yeli Dnye"
ethnography$Language[ethnography$Language=="Yurakaré"] = "Yurakare"

unique(s$Language)[!unique(s$Language) %in% ethnography$Language]
ethnography$Language[!ethnography$Language %in% unique(s$Language)]

# Link population

p = readWorksheetFromFile("../data/ethnography/LoP-sample-20171004.xls",1)
p$Language.name[p$Language.name=="Zapotec, Lachixío"] = "Zapotec"
p$Language.name[!p$Language.name %in% ethnography$Language.orig]

ethnography$pop = p[match(ethnography$Language.orig, p$Language.name),]$Total.Researcher.

# Define "handful" as 100 (lowest pop estimate)

ethnography$pop[ethnography$pop=="handful?"] = 100

# Remove commas from pop numbers
ethnography$pop = as.numeric(gsub(",",'',ethnography$pop))

# Use more resticted subsistance categories
ethnography$subsistance = p[match(ethnography$Language.orig, p$Language.name),]$New.subsistence.mode

ethnography$formal.schooling = p[match(ethnography$Language.orig, p$Language.name),]$formal.schooling

# Add latitude and longitude
ethnography$latitude = p[match(ethnography$Language.orig, p$Language.name),]$Latitude
ethnography$longitude = p[match(ethnography$Language.orig, p$Language.name),]$Longitude

# Make variables labelled "how many" more specific by adding domain
names(ethnography)[which(names(ethnography)=="how many")] = 
  paste(names(ethnography)[which(names(ethnography)=="how many")],
        names(ethnography)[which(names(ethnography)=="how many")-1])
# Disambiguate "what patterns"
names(ethnography)[which(names(ethnography)=="what patterns?")[1]] = "what weave patterns?"
names(ethnography)[which(names(ethnography)=="what patterns?")[1]] = "what leatherware patterns?"
# Disambiguate leatherware decoration"
names(ethnography)[names(ethnography)=="decorated"] = "decorated leatherware"
# Make boat label standard
names(ethnography)[which(names(ethnography)=="make boats")+1] = "boat specialists"
# Make spinning label standard
names(ethnography)[which(names(ethnography)=="spinning thread")+1] = "spinning specialists"
# Make weaving label standard
names(ethnography)[which(names(ethnography)=="weaving")+1] = "weaving specialists"

# Make sure strings are characters, not factors
for(i in 1:ncol(ethnography)){
  ethnography[,i] = as.character(ethnography[,i])
}

# Dyes: Few, none, many
table(ethnography$`how many dyes`)
ethnography$dyes.cat = "few"
ethnography[ethnography$`how many dyes` =="0",]$dyes.cat = "none"
ethnography[ethnography$`how many dyes` %in% c("100s",'many'),]$dyes.cat = "many"


# Paints: 100s, few, none, many
ethnography[ethnography$Language=="Turkish",]$`how many paints` = "100s"
ethnography$paints.cat = "few"
ethnography[ethnography$`how many paints`=="0",]$paints.cat = "none"
ethnography[ethnography$`how many dyes` %in% c("100s",'many'),]$paints.cat = "many"

# Disambiguate environment details label
names(ethnography)[names(ethnography)=="details"] = "environment.details"

# Colour: few, none many
ethnography$ritual.colour.cat = "few"
ethnography[ethnography$`how many ritual colour`=="0",]$ritual.colour.cat = "none"
ethnography[ethnography$`how many ritual colour` %in% c("10s"),]$ritual.colour.cat = "many"

# Remove "cook from scatch" variable
ethnography = ethnography[,names(ethnography)!='cook from scratch']
# Change "nuclear" to "nucleated"
ethnography$settlement[ethnography$settlement=="nuclear"] = "nucleated"

# Count total number of additives
ethnography$num.additives = 
  as.numeric(ethnography$`salt additive` == "yes") +
  as.numeric(ethnography$`umami additive` == "yes") +
               as.numeric(ethnography$`sour additive` == "yes") +
                            as.numeric(ethnography$`sweet additive` == "yes") +
                                         as.numeric(ethnography$`bitter additive` == "yes")


# reduce weave pattern categories
#none
#simple (blotches, linear)
#angular (rectilinear, triangular)
#complex (all, both)
ethnography$what.weave.patterns2 = ethnography$`what weave patterns?`
ethnography$what.weave.patterns2[ethnography$what.weave.patterns2 %in% c("blotches",'linear')] = "simple"
ethnography$what.weave.patterns2[ethnography$what.weave.patterns2 %in% c("rectilinear",'triangular')] = "angular"
ethnography$what.weave.patterns2[ethnography$what.weave.patterns2 %in% c("all",'both')] = "complex"

# Swap order to make standard
ethnography$shape[ethnography$shape=="round+square"] = "square+round"

# Split up shape variable into binary categories.
# Does community have a square category?
ethnography$abstract.square.cat = ethnography$shape %in% c('all','both','square+round','square+round+cone','round+square')
# Does community have a round category
ethnography$abstract.round.cat = ethnography$shape %in% c('all','both','round','square+round','square+round+cone')


# Find pairs of variables that are co-linear
selectedColumns = which(! names(ethnography) %in%
   c("how many dyes","how many paints",'how many ritual colour', 'dyes','paints','ritual colour',"Language","Language.orig",'pop','subsistance','formal.schooling'))
pairs = combn(selectedColumns,2)
identicalPairs = apply(pairs,2, function(p){
  tx = table(ethnography[,p[1]],
        ethnography[,p[2]])
  !any(apply(tx,1,function(z){sum(z>0)>1})) & 
    !any(apply(tx,2,function(z){sum(z>0)>1}))
})
sort(table(pairs[,identicalPairs]))
t(apply(pairs[,identicalPairs],2, function(p){
  paste(names(ethnography)[p])
}))



write.csv(ethnography, "../data/ethnography/LoP_ethnography_processed.csv", row.names = F)

