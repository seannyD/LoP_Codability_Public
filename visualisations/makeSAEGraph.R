library(ggplot2)
library(reshape2)
library(dplyr)
setwd("~/Documents/Bristol/Codability/LoP_Codability/visualisations/")


# Graph of SAE proportions by language and domain
d = read.csv("../data/AllData_LoP.csv", stringsAsFactors = F)

# Get rid of non-responses
d = d[!is.na(d$head),]
d = d[!d$head %in% c("n/a","no description"),]
d = d[!is.na(d$SAE),]

d = d[d$Response==1,]

domain.order = c("colour","shape",'sound','touch','taste','smell')

d$domain = factor(d$domain, levels=domain.order,
                  labels= c("Color","Shape","Sound","Touch","Taste","Smell"))

d$SAE = factor(d$SAE, levels=c("A","E","S"),
               labels=c("Abstract","Evaluative","Source-based"))

# Note spellingo f Yucatec / Yucatek
d$Language = factor(d$Language, 
                    levels =
                      rev(c("English", "Farsi", "Turkish", "Dogul Dom", "Siwu", "Cantonese", "Lao", "Malay", "Semai", "Kilivila", "Mian", "Yeli Dnye", "Umpila", "Tzeltal", "Yucatec", "Zapotec", "Yurakare", "ASL", "BSL", "Kata Kolok")),
                    labels = 
                      rev(c("English", "Farsi", "Turkish", "Dogul Dom", "Siwu", "Cantonese", "Lao", "Malay", "Semai", "Kilivila", "Mian", "Yélî Dnye", "Umpila", "Tzeltal", "Yucatec", "Zapotec", "Yurakare", "ASL", "BSL", "Kata Kolok")))




prop = d %>% group_by(Language,domain,SAE) %>%
  summarise (n = n()) %>%
  mutate(prop = n / sum(n))

prop$SAE = factor(prop$SAE, levels=rev(c("Abstract","Source-based",'Evaluative')))

#Abstract, Source-Based, Evaluative
AM.colours = c('black','#c24d2b','#2c552c')

gx = ggplot(prop, 
            aes(x=Language, y=prop, fill=SAE)) +
  geom_bar(stat="identity") +
  coord_flip() +
  scale_y_continuous(breaks=c(0,0.5,1.0),
                     limits=c(-0.1,1)) +
  ylab("Proportion of responses")  +
  scale_fill_manual(name = "Description type",
                      values=AM.colours,
                      breaks=c("Abstract","Source-based","Evaluative"))+
  facet_grid(.~domain) +
  theme(strip.background =element_rect(fill="white"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) 

gx

pdf("../results/graphs/SAE_by_Language_by_Domain.pdf", width=7, height = 4.3)
gx
dev.off()  

setEPS()
postscript("../results/graphs/SAE_by_Language_by_Domain.eps", width=7, height = 4.3)
gx
dev.off()


# Summary

propS = prop %>% group_by(domain,SAE) %>% summarise(m = mean(prop))
propS$SAE = factor(propS$SAE, levels=rev(c("Abstract","Source-based",'Evaluative')))
propS$Language = "Mean"
propS$Language = factor(propS$Language)

# For some reason, the orders won't display properly until 
# they're put in reverse order in the table
propS = propS[rev(order(propS$SAE)),]

gxS = ggplot(propS, 
            aes(x=Language, y=m, fill=SAE)) +
  geom_bar(stat="identity") +
  coord_flip() +
  scale_y_continuous(breaks=c(0,0.5,1.0),
                     limits = c(-0.1,1.2)) +
  ylab("Proportion of responses")  +
  scale_fill_manual(name = "Description type",
                    values=AM.colours,
                    breaks=c("Abstract","Source-based","Evaluative"))+
  facet_grid(.~domain) +
  theme(strip.background =element_rect(fill="white")) 

gxS

gxS2 = gxS + theme(axis.title.x=element_blank(),
                   axis.title.y=element_blank(),
                   legend.position = "none"
        )
gxS2
