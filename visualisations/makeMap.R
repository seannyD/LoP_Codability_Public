library(XLConnect)
library(maps)
library(mapproj)
setwd("~/Documents/Bristol/Codability/LoP_Codability/visualisations/")

d = readWorksheetFromFile("../data/ethnography/LoP-sample-20171004.xls",1)

pdf("../results/graphs/map.pdf",
    width=10, height=5)
map(interior=F, ylim=c(-60,90),
    projection = "gall", parameters = 0,
    wrap = T)
points(mapproject(d$Longitude, d$Latitude), pch=16)
dev.off()