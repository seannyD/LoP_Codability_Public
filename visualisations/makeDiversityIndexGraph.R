library(ggplot2)
library(grid)
library(gridExtra)
library(cowplot)
setwd("~/Documents/Bristol/Codability/LoP_Codability/visualisations/")




d = read.csv("../data/DiversityIndices_ND.csv", stringsAsFactors = F)

domain.order = c("colour","shape",'sound','touch','taste','smell')

d$domain = factor(d$domain, levels=domain.order, labels=c("Color","Shape","Sound","Touch","Taste","Smell"))

# Note spellingo f Yucatec / Yucatek
d$Language = factor(d$Language, 
                    levels =
                      rev(c("English", "Farsi", "Turkish", "Dogul Dom", "Siwu", "Cantonese", "Lao", "Malay", "Semai", "Kilivila", "Mian", "Yeli Dnye", "Umpila", "Tzeltal", "Yucatec", "Zapotec", "Yurakare", "ASL", "BSL", "Kata Kolok")),
                    labels = 
                      rev(c("English", "Farsi", "Turkish", "Dogul Dom", "Siwu", "Cantonese", "Lao", "Malay", "Semai", "Kilivila", "Mian", "Yélî Dnye", "Umpila", "Tzeltal", "Yucatec", "Zapotec", "Yurakare", "ASL", "BSL", "Kata Kolok")))

labels= data.frame(Language=as.character(levels(d$Language)))
labels$x = 1:nrow(labels)

# Graph of diversity index by language and domain


gx = ggplot(d, aes(x=Language, y=simpson.diversityIndex)) +
  geom_boxplot(width=1, size=1, outlier.size = 0.5, colour='dark gray') +
  stat_summary(fun.y=median, geom="point", shape=16, size=2)+
  coord_flip() +
  scale_y_continuous(breaks=c(0,0.5,1.0),
                     limits=c(0,1)) +
  facet_grid(.~domain) +
  ylab("Simpson's diversity index") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(linetype = 1, colour = 'black', size=1),
        plot.margin=unit(c(0,3,0,0),"cm"),
        panel.spacing = unit(1.05, "lines"),
        strip.background =element_rect(fill=NA),
        axis.ticks.y = element_blank()) 


#plot_grid(gx, axis=c("lr"))
#ggdraw( switch_axis_position( gx, axis="y", keep="y"))

gx2 = plot_grid(gx) + 
  annotation_custom(textGrob(labels$Language, rep(0.89, nrow(labels)),
                             seq(0.13,0.95,length.out=nrow(labels)),
                             gp = gpar(fontsize=11.5),
                             hjust = 0))

pdf("../results/graphs/Diverstiy_by_Language_by_Domain.pdf", width=10, height = 5)
gx2
dev.off()



setEPS()
postscript("../results/graphs/Diverstiy_by_Language_by_Domain.eps", width = 10, height = 5)
gx2
dev.off()


