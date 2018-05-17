library(ggplot2)
library(grid)
library(gridExtra)
#library(cowplot)
library(dplyr)
library(svglite)
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

d2 = d %>% group_by(Language,domain) %>% 
  summarise(m=mean(simpson.diversityIndex),
            upper=mean(simpson.diversityIndex)+sd(simpson.diversityIndex),
            lower=mean(simpson.diversityIndex)-sd(simpson.diversityIndex))
d2$rank = unlist(tapply(d2$m,d2$Language,rank))

stripes = data.frame(
  x1 = seq(0.5,length(unique(d$Language))-0.5,by=2),
  x2 = seq(1.5,length(unique(d$Language)),by=2),
  y1 = rep(-0.2,length(unique(d$Language))),
  y2 = rep(1.2,length(unique(d$Language))))

gx = ggplot(d2, aes(x=Language, y=m, colour=domain)) +
  #geom_errorbar(aes(ymin=lower,ymax=upper))+
  geom_rect(data=stripes, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2, colour=NULL,x=NULL,y=NULL,Language=NULL,group=NULL),fill="light gray")+
  geom_point(position = position_dodge(width = 0.75))+
  coord_flip(ylim=c(0,1)) +
  ylab("Simpson's diversity index")
gx  

pdf("../results/graphs/Diverstiy_by_Language_by_Domain_Hierarchy.pdf", width=10, height = 5)
gx
dev.off()


#########

# Plot with symbols.

library(png)

icon_shape = readPNG("clips/Shape_noBorder.png")
icon_smell = readPNG("clips/Smell_noBorder.png")
icon_sound = readPNG("clips/Sound_noBorder.png")
icon_touch = readPNG("clips/Touch_noBorder.png")
icon_taste = readPNG("clips/Taste_noBorder.png")
icon_colour = readPNG("clips/Colour_noBorder.png")
 
# The coords are flipped, so the width here actually becomes the height
imageWidth = 15
imageMargin = 1
# Half the width and height
iconWidth = 1/2
iconHeight = (1/25)/2

d2$Lang.num = as.numeric(d2$Language)

doms = c("Color","Shape","Touch","Taste","Smell","Sound")
icons = list(icon_colour,
             icon_shape,
             icon_touch,
             icon_taste,
             icon_smell,
             icon_sound)


gx2 = ggplot(d2, aes(x=Language, y=m, colour=domain)) +
  geom_rect(data=stripes, 
            mapping=aes(xmin=x1, xmax=x2, 
                        ymin=y1, ymax=y2, 
                        colour=NULL,
                        x=NULL,y=NULL,
                        Language=NULL,group=NULL),
            fill="light gray")+
  ylab("Simpson's diversity index") +
  geom_point(alpha=0)

for(i in 1:6){
  gx2 = gx2 + mapply(function(xx, yy) 
    annotation_raster(icons[[i]], 
      xmin=xx-iconWidth, 
      xmax=xx+iconWidth, 
      ymin=yy-iconHeight, 
      ymax=yy+iconHeight),
    d2[d2$domain==doms[i],]$Lang.num, 
    d2[d2$domain==doms[i],]$m)
}

gx2 = gx2 + 
  theme(legend.position="none",
        axis.title.y=element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  coord_flip(ylim=c(0,1))  

pdf("../results/graphs/Hierarchy2.pdf",
    width = 15,
    height = 10)
gx2
dev.off()

svg(filename="../results/graphs/Hierarchy2_graph.svg",
    width = 15,
    height = 10)
gx2
dev.off()

ggsave(file="../results/graphs/Hierarchy2.svg", plot=gx2, width=10, height=8)
