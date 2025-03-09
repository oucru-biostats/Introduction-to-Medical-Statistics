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
