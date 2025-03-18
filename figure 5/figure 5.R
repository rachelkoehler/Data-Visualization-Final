library(tidyverse)
library(plotly)
library(ggrepel)
library(patchwork)
library(ggplot2)
library(gridExtra)
library(grid)

### STEP1: data processing

# upload datasets: (age-adjusted) suicide rates from CDC WONDER
s <- read_tsv("suic-race-aadr-1999-2018.txt", skip=1, 
  col_names=c("notes", "gender", "gcode", "race", "rcode", "year", "ycode", 
              "deaths", "pop", "crate", "aadr"), n_max=160,
  col_types = "cccfcdcdddd")
sh <- read_tsv("suic-hisp-aadr-1999-2018.txt", skip=1, 
               col_names=c("notes", "gender", "gcode", "year", "ycode", 
                           "deaths", "pop", "crate", "aadr"), n_max=40,
               col_types = "cccdcdddd")

# keep demographics, deaths, population, create rates
s2 <- select(s, gender, race, year, deaths, pop, aadr) %>%
  mutate(racen = as.numeric(race), rate = deaths / pop * 100000)
sh2 <- select(sh, gender, year, deaths, pop, aadr) %>%
  mutate(racen = 5, rate = deaths / pop * 100000)

# put Hispanics and non-Hispanics together
sr <- bind_rows(s2, sh2) %>%
  select(-race) %>%
  mutate(racen = recode_factor(racen, `1`= "Non-Hispanic\nAIAN", 
        `2`= "Non-Hispanic\nAPI", `3`= "Non-Hispanic\nBlack", 
        `4`= "Non-Hispanic\nWhite", `5`= "Hispanic")) %>%
  filter(racen != "Non-Hispanic\nAIAN")


### STEP2: plot

# modify plot characteristics

##### 2  #####
##### set some plot characteristics
stheme <- theme_classic() + 
  theme(plot.title = element_text(size = 12, face = "bold"), 
        plot.subtitle = element_text(size=12)) + 
  theme(axis.text.x = element_text(size = 12, colour = "black"),
        axis.title.y = element_text(size=12, angle=90, colour="black", face="bold"), 
        axis.text.y = element_text(size = 12, colour="black"), 
        legend.position="none", 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA, linewidth=1)) +
  theme(axis.line.x=element_line(colour="black"), 
        axis.line.y=element_line(colour="black"), 
        axis.ticks = element_line(colour="black"), 
        strip.text = element_text(size = 16), 
        strip.background = element_rect(colour="white")) + 
  theme(axis.ticks.length=unit(-0.25, "cm"))

# 1) plot for men

# Non-Hispanic API men
nhac <- "#e41a1c"
amp <- ggplot(subset(sr, gender=="Male"), 
              aes(x=year, y=aadr, group=racen))  + 
  geom_line(show.legend=F, colour="grey") +
  geom_line(data=subset(sr, gender=="Male" & racen=="Non-Hispanic\nAPI"),
            colour=nhac, size=1.5) +
  geom_point(data=subset(sr, gender=="Male" & (year==1999 | year==2018) & 
                           racen=="Non-Hispanic\nAPI"), shape=21, 
             colour="white", fill=nhac, size=3, stroke=2) +
  scale_y_continuous(limits=c(0,30), breaks=c(0,10,20,30), sec.axis=dup_axis()) +
  scale_x_continuous(breaks=c(2005,2015), sec.axis=dup_axis()) +
  xlab("") + ylab("") + stheme +
  ggtitle("", subtitle="Men") +
  theme(plot.subtitle = element_text(size=16),
        axis.line.y.right = element_blank(), 
        axis.text.y.right = element_blank(),
        axis.line.x.top = element_line(colour = "black"),
        axis.ticks.x.top = element_line(colour = "black"),
        axis.text.x.top = element_blank())

# Non-Hispanic Black men
nhbc <- "#377eb8"
bmp <- ggplot(subset(sr, gender=="Male"), 
       aes(x=year, y=aadr, group=racen))  + 
  geom_line(show.legend=F, colour="grey") +
  geom_line(data=subset(sr, gender=="Male" & racen=="Non-Hispanic\nBlack"),
            colour=nhbc, size=1.5) +
  geom_point(data=subset(sr, gender=="Male" & (year==1999 | year==2018) & 
                           racen=="Non-Hispanic\nBlack"), shape=21, 
             colour="white", fill=nhbc, size=3, stroke=2) +
  scale_y_continuous(limits=c(0,30), breaks=c(0,10,20,30), sec.axis=dup_axis()) +
  scale_x_continuous(breaks=c(2005,2015), sec.axis=dup_axis()) +
  xlab("") + ylab("") + stheme +
  theme(axis.line.y = element_blank(), 
        axis.text.y = element_blank(),
        axis.line.x.top = element_line(colour = "black"),
        axis.ticks.x.top = element_line(colour = "black"),
        axis.text.x.top = element_blank())

# Non-Hispanic White men
nhwc <- "#4daf4a"
wmp <- ggplot(subset(sr, gender=="Male"), 
       aes(x=year, y=aadr, group=racen))  + 
  geom_line(show.legend=F, colour="grey") +
  geom_line(data=subset(sr, gender=="Male" & racen=="Non-Hispanic\nWhite"),
            colour=nhwc, size=1.5) +
  geom_point(data=subset(sr, gender=="Male" & (year==1999 | year==2018) & 
                           racen=="Non-Hispanic\nWhite"), shape=21, 
             colour="white", fill=nhwc, size=3, stroke=2) +
  scale_y_continuous(limits=c(0,30), breaks=c(0,10,20,30), sec.axis=dup_axis()) +
  scale_x_continuous(breaks=c(2005,2015), sec.axis=dup_axis()) +
  xlab("") + ylab("") + stheme +
  theme(axis.line.y = element_blank(), 
        axis.text.y = element_blank(),
        axis.line.x.top = element_line(colour = "black"),
        axis.ticks.x.top = element_line(colour = "black"),
        axis.text.x.top = element_blank())

# Hispanic men
hc <- "#984ea3"
hmp <- ggplot(subset(sr, gender=="Male"), 
       aes(x=year, y=aadr, group=racen))  + 
  geom_line(show.legend=F, colour="grey") +
  geom_line(data=subset(sr, gender=="Male" & racen=="Hispanic"),
            colour=hc, size=1.5) +
  geom_point(data=subset(sr, gender=="Male" & (year==1999 | year==2018) & 
                           racen=="Hispanic"), shape=21, 
             colour="white", fill=hc, size=3, stroke=2) +
  scale_y_continuous(limits=c(0,30), breaks=c(0,10,20,30), sec.axis=dup_axis()) +
  scale_x_continuous(breaks=c(2005,2015), sec.axis=dup_axis()) +
  xlab("") + ylab("") + stheme +
  theme(axis.line.y = element_blank(), 
        axis.text.y = element_blank(),
        axis.line.x.top = element_line(colour = "black"),
        axis.ticks.x.top = element_line(colour = "black"),
        axis.text.x.top = element_blank())

# put plots for men together
mp <- (amp | bmp | wmp | hmp)

# 2) plot for women

# Non-Hispanic API women
nhac <- "#e41a1c"
awp <- ggplot(subset(sr, gender=="Female"), 
       aes(x=year, y=aadr, group=racen))  + 
  geom_line(show.legend=F, colour="grey") +
  geom_line(data=subset(sr, gender=="Female" & racen=="Non-Hispanic\nAPI"),
            colour=nhac, size=1.5) +
  geom_point(data=subset(sr, gender=="Female" & (year==1999 | year==2018) & 
                           racen=="Non-Hispanic\nAPI"), shape=21, 
             colour="white", fill=nhac, size=3, stroke=2) +
  scale_y_continuous(limits=c(0,10), breaks=c(0,5,10), sec.axis=dup_axis()) +
  scale_x_continuous(breaks=c(2005,2015), sec.axis=dup_axis()) +
  xlab("") + ylab("") + stheme +
  ggtitle("Non-Hispanic API", subtitle="Women") +
  theme(plot.title = element_text(colour=nhac),
        plot.subtitle = element_text(size=16), 
        axis.line.y = element_blank(),
        axis.text.x = element_blank(),        
        axis.text.y.right = element_blank())

# Non-Hispanic Black women
nhbc <- "#377eb8"
bwp <- ggplot(subset(sr, gender=="Female"), 
       aes(x=year, y=aadr, group=racen))  + 
  geom_line(show.legend=F, colour="grey") +
  geom_line(data=subset(sr, gender=="Female" & racen=="Non-Hispanic\nBlack"),
            colour=nhbc, size=1.5) +
  geom_point(data=subset(sr, gender=="Female" & (year==1999 | year==2018) & 
                           racen=="Non-Hispanic\nBlack"), shape=21, 
             colour="white", fill=nhbc, size=3, stroke=2) +
  scale_y_continuous(limits=c(0,10), breaks=c(0,5,10), sec.axis=dup_axis()) +
  scale_x_continuous(breaks=c(2005,2015), sec.axis=dup_axis()) +
  xlab("") + ylab("") + stheme +
  ggtitle("Non-Hispanic Black", subtitle="") +
  theme(plot.title = element_text(colour=nhbc),
        axis.text.x = element_blank(),
        axis.line.y = element_blank(),
        axis.text.y = element_blank())

# Non-Hispanic White women
nhwc <- "#4daf4a"
wwp <- ggplot(subset(sr, gender=="Female"), 
       aes(x=year, y=aadr, group=racen))  + 
  geom_line(show.legend=F, colour="grey") +
  geom_line(data=subset(sr, gender=="Female" & racen=="Non-Hispanic\nWhite"),
            colour=nhwc, size=1.5) +
  geom_point(data=subset(sr, gender=="Female" & (year==1999 | year==2018) & 
                           racen=="Non-Hispanic\nWhite"), shape=21, 
             colour="white", fill=nhwc, size=3, stroke=2) +
  scale_y_continuous(limits=c(0,10), breaks=c(0,5,10), sec.axis=dup_axis()) +
  scale_x_continuous(breaks=c(2005,2015), sec.axis=dup_axis()) +
  xlab("") + ylab("") + stheme +
  ggtitle("Non-Hispanic White", subtitle="") +
  theme(plot.title = element_text(colour=nhwc), 
        axis.text.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.y = element_blank())

# Hispanic women
hc <- "#984ea3"
hwp <- ggplot(subset(sr, gender=="Female"), 
       aes(x=year, y=aadr, group=racen))  + 
  geom_line(show.legend=F, colour="grey") +
  geom_line(data=subset(sr, gender=="Female" & racen=="Hispanic"),
            colour=hc, size=1.5) +
  geom_point(data=subset(sr, gender=="Female" & (year==1999 | year==2018) & 
                           racen=="Hispanic"), shape=21, 
             colour="white", fill=hc, size=3, stroke=2) +
  scale_y_continuous(limits=c(0,10), breaks=c(0,5,10), sec.axis=dup_axis()) +
  scale_x_continuous(breaks=c(2005,2015), sec.axis=dup_axis()) +
  xlab("") + ylab("") + stheme +
  ggtitle("Hispanic", subtitle="") +
  theme(plot.title = element_text(colour=hc), 
        axis.text.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.y = element_blank())

# put plots for women together
wp <- (awp | bwp | wwp | hwp)

# put men and women's plots together and annotate
p <- wp / mp + plot_layout(widths = rep(40, 4), heights = rep(4, 2))
y.grob <- textGrob("Age-adjusted death rates for \nsuicide (per 100,000)", 
                   gp=gpar(fontface="bold", fontsize=15), rot=90)
x.grob <- textGrob("Year", 
                   gp=gpar(fontface="bold",  fontsize=15))
grid.arrange(patchworkGrob(p), left = y.grob, bottom = x.grob)

