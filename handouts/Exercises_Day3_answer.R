library(ggplot2)

library(readxl)
titanic <- read_excel("Titanic3.xlsx", na="NA")

#####################################################################################
## First exercise set
## exercise 1
ggplot(titanic, aes(age, fare)) + geom_point() + ylab("fare paid (GBP)")
ggplot(titanic, aes(age, fare)) + geom_point() + ylab("fare paid (GBP)") + coord_polar()
## exercise 2
ggplot(titanic, aes(age, fare)) + geom_point() + ylab("fare paid (GBP)") +
  geom_smooth(method="lm") + theme_minimal()
ggplot(titanic, aes(age, fare)) + geom_point() + ylab("fare paid (GBP)") +
  geom_smooth(method="lm") + theme_bw()
ggsave("SavedPlot.png")
## exercise 3
ggplot(titanic, aes(age, fare)) + geom_point() + geom_smooth(method="lm") +
  theme_bw() + scale_y_log10("fare paid (GBP)")
## exercise 4
ggplot(titanic, aes(age, fare, color=pclass)) + geom_point() + geom_smooth(method="lm") +
  theme_bw() + scale_y_log10("fare paid (GBP)")
## exercise 5
ggplot(titanic, aes(age, fare, color=pclass)) + geom_point()  + geom_smooth(method="lm") +
  theme_bw() + scale_y_log10("fare paid (GBP)") + facet_wrap(~sex) + geom_rug()
## exercise 6
ggplot(titanic, aes(pclass, age)) + geom_boxplot() + xlab("passenger class")
ggplot(titanic, aes(pclass, age)) + geom_boxplot() + xlab("passenger class") + coord_flip()
## exercise 7
ggplot(titanic, aes(pclass, age)) + geom_boxplot() + xlab("passenger class") +
  geom_jitter(color="red",alpha=0.2) + theme_minimal()
## If you don't jitter the points, you get the following:
ggplot(titanic, aes(pclass, age)) + geom_boxplot() + xlab("passenger class") +
  geom_point(color="red",alpha=0.2) +  theme_minimal()
## violin plot instead of boxplot
ggplot(titanic, aes(pclass, age)) + geom_violin() + xlab("passenger class") +
  geom_jitter() +  theme_minimal()
## exercise 8
ggplot(titanic, aes(age,survived)) + geom_point()
ggplot(titanic, aes(age,survived)) + geom_jitter(height=0.05, width=0, alpha=0.5) +
  geom_smooth(method="loess")
ggplot(titanic, aes(age,survived)) + geom_jitter() + geom_smooth(method="loess")
## exercise 9
ggplot(titanic, aes(age,survived,colour=sex)) + geom_jitter(height=0.05, width=0,alpha=0.5) +
  geom_smooth(method="loess") +  theme(legend.position='top')
## extra: we change the label and the colour palette
ggplot(titanic, aes(age,survived,colour=sex)) + geom_jitter(height=0.05, width=0,alpha=0.5) +
  geom_smooth(method="loess") + scale_colour_brewer(name="gender",label=c("F","M"),palette='Set1') +  theme(legend.position='top')
## exercise 10
ggplot(titanic, aes(age,survived,colour=sex)) + geom_jitter(height=0.05, width=0,alpha=0.3) +
  geom_smooth(method="loess") + facet_wrap(~pclass) + ylim(c(-0.1,1.1)) +  theme(legend.position='top')
## truncating the output is prevented by setting the lim its within coord_cartesian
ggplot(titanic, aes(age,survived,colour=sex)) + geom_jitter(height=0.05, width=0,alpha=0.3) +
  geom_smooth(method="loess") + facet_wrap(~pclass) + coord_cartesian(ylim=c(-0.1,1.1))


#####################################################################################
###### Exercises afternoon
#####################################################################################

library(ggplot2)
library(ggthemes)

## exercise 3
NightRose <- read.csv("data/raw/NightingaleRose.csv")
NightRose$Date <- as.Date(NightRose$Date)

## stacked bar chart
ggplot(NightRose, aes(Date,perc,fill=cause)) + geom_col(position="stack") + geom_vline(xintercept=as.Date("1855-03-15"), linetype="dashed", col="grey50")

## dodged bar chart
ggplot(NightRose, aes(Date,perc,fill=cause)) + geom_col(position="dodge") + geom_vline(xintercept=as.Date("1855-03-15"), linetype="dashed", col="grey50")


## change background colour
ggplot(NightRose, aes(Date,perc,fill=cause)) + geom_rect(
  aes(xmin = as.Date("1854-03-15"), xmax = as.Date("1855-03-15"), ymin = -Inf, ymax = Inf),
  alpha = .2, fill="moccasin") + geom_col(position="dodge") +
  geom_vline(xintercept=as.Date("1855-03-15"), linetype="dashed", col="grey50") + scale_x_date(expand=c(0,0))

##
ggplot(NightRose, aes(Date,perc,color=cause)) + geom_line(size=2) + geom_vline(xintercept=as.Date("1855-03-15"), linetype="dashed", col="grey50")

ggplot(NightRose, aes(Date,perc,group=cause,color=cause)) + geom_line(size=0.2,col="grey50") + geom_point(size=5) + geom_vline(xintercept=as.Date("1855-03-15"), linetype="dashed", col="grey50")


## exercise 4:
Cost <- read.csv("data/raw/ICUCost.csv")
library(ggplot2)
library(ggthemes)

## (b)
ggplot(data=Cost,aes(x=Severity,y=Perc,fill=Type))+
 geom_bar(stat='identity')+
 scale_fill_brewer(palette='Set2')+
 coord_flip()+
 ggtitle("Percentage share of the cost in ICU")+
    theme_classic()

## slightly more concise code:
ggplot(data=Cost,aes(Severity,Perc,fill=Type))+
 geom_col()+
 coord_flip()+
 ggtitle("Percentage share of the cost in ICU")+
    theme_classic()

## same figure via reversing map to x-axis and y-axis
ggplot(data=Cost,aes(Perc,Severity,fill=Type))+
 geom_col()+
 ggtitle("Percentage share of the cost in ICU")+
    theme_classic()

## change the theme (ggthemes package needed) and use another colour palette
ggplot(data=Cost,aes(x=Severity,y=Perc,fill=Type))+
 geom_bar(stat='identity')+
 scale_fill_brewer(palette='Set2')+
 coord_flip()+
 ggtitle("Percentage share of the cost in ICU")+
 theme_fivethirtyeight()+
    scale_colour_fivethirtyeight()

## (c)
ggplot(data=Cost,aes(x=Type,y=Perc,fill=Type))+
 geom_bar(stat='identity')+
 scale_fill_brewer(palette='Set2')+
 facet_grid(Ventilated ~ Disease)+
 coord_flip()+
 ggtitle("Percentage share of the cost in ICU")+
 theme_fivethirtyeight()+
    scale_colour_fivethirtyeight()

## without colors and change theme:
ggplot(data=Cost,aes(x=Type,y=Perc))+
 geom_bar(stat='identity')+
 facet_grid(Ventilated ~ Disease)+
 coord_flip()+
 ggtitle("Percentage share of the cost in ICU")+
 theme_bw()

## (d)
ggplot(data=Cost,aes(x=Type,y=Perc,group=Disease,colour=Disease))+
 geom_line(size=1)+
 facet_grid(. ~ Ventilated)+
 ggtitle("Percentage share of the cost in ICU")+
    theme_bw() + coord_flip()

## Here we really need to use coord_flip if we want to reverse the mapping to x-axis and y-axis
ggplot(data=Cost,aes(Perc,Type,group=Disease,colour=Disease))+
 geom_line(size=1)+
 facet_grid(. ~ Ventilated)+
 ggtitle("Percentage share of the cost in ICU")+
 theme_bw()

## Connecting the lines may generate confusion, because it's interpreted as a slope
## Therefore, this alternative may be better:
ggplot(data=Cost,aes(x=Type,y=Perc,group=Disease,colour=Disease))+
 geom_line(size=0.2,col="grey50")+
 geom_point(size=5) +
 facet_grid(. ~ Ventilated)+
 ggtitle("Percentage share of the cost in ICU")+
 theme_bw()  + coord_flip()


#####################################################################################
## we make the same graphs, but first reorder type of cost by average percentage
library(forcats)
Cost$CostType <- fct_reorder(Cost$Type,Cost$Perc,mean)

## (b)
ggplot(data=Cost,aes(x=Severity,y=Perc,fill=CostType))+
 geom_bar(stat='identity')+
 scale_fill_brewer(palette='Set2')+
 coord_flip()+
 ggtitle("Percentage share of the cost in ICU")+
 theme_classic()

## (c)
ggplot(data=Cost,aes(x=CostType,y=Perc,fill=CostType))+
 geom_bar(stat='identity')+
 scale_fill_brewer(palette='Set2')+
 facet_grid(Ventilated ~ Disease)+
 coord_flip()+
 ggtitle("Percentage share of the cost in ICU")+
 theme_fivethirtyeight()+
    scale_colour_fivethirtyeight()

ggplot(data=Cost,aes(x=CostType,y=Perc))+
 geom_bar(stat='identity')+
 facet_grid(Ventilated ~ Disease)+
 coord_flip()+
 ggtitle("Percentage share of the cost in ICU")+
 theme_bw()

## (d)
ggplot(data=Cost,aes(x=CostType,y=Perc,group=Disease,colour=Disease))+
 geom_line(size=1)+
 facet_grid(. ~ Ventilated)+
 ggtitle("Percentage share of the cost in ICU")+
 theme_bw()

ggplot(data=Cost,aes(x=CostType,y=Perc,group=Disease,colour=Disease))+
 geom_line(size=0.2,col="grey50")+
 geom_point(size=5) +
 facet_grid(. ~ Ventilated)+
 ggtitle("Percentage share of the cost in ICU")+
 theme_bw()  + coord_flip()


