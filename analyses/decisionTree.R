library(REEMtree)
library(party)
setwd("~/Documents/Bristol/Codability/LoP_Codability/analyses/")


d = read.csv("../data/AllData_LoP.csv", stringsAsFactors = F)

d = d[!is.na(d$head),]
d = d[!d$head %in% c("n/a","no description"),]
d = d[!is.na(d$SAE),]

d = d[d$Response==1,]

d$SAE = as.factor(d$SAE)
d$Language = substr(d$Language,1,3)
d$Language = as.factor(d$Language)
d$domain = as.factor(d$domain)



ct = ctree(SAE ~ domain, data=d,
           controls = ctree_control(mincriterion = 0.99, maxdepth = 3))
pdf("../results/graphs/SAE_ctree_domain.pdf", width=20, height=12)
plot(ct)
dev.off()


ct = ctree(SAE ~ Language + domain, data=d,
           controls = ctree_control(mincriterion = 0.999, maxdepth = 3))
pdf("../results/graphs/SAE_ctree.pdf", width=20, height=12)
plot(ct)
dev.off()

