library(ggplot2)
head(mpg)
ggplot(mpg,aes(x=displ,y=hwy))+
  geom_point()

ggplot(mpg,aes(displ,cty,color=class)) +
  geom_point()

ggplot(mpg,aes(displ,cty)) +
  geom_point(color="blue")

ggplot(mpg, aes(displ,hwy))+ 
  geom_point() +
  facet_wrap(~class)
#Alternatively, we can use the qplot function & syntax 
qplot(displ,hwy,data=mpg,facets=. ~ class)

ggplot(mpg, aes(displ,hwy)) +
  geom_point() + 
  geom_smooth()

ggplot(mpg, aes(displ,hwy)) +
  geom_point() + 
  geom_smooth(span=0.2)

ggplot(mpg, aes(displ,hwy)) +
  geom_point() + 
  geom_smooth(method="lm")

ggplot(mpg, aes(drv,hwy)) +
  geom_point()

ggplot(mpg, aes(drv,hwy)) +
  geom_jitter()

ggplot(mpg, aes(drv,hwy)) +
  geom_boxplot()

ggplot(mpg, aes(drv,hwy)) +
  geom_violin()

ggplot(mpg,aes(displ,color=drv)) +
  geom_freqpoly(binwidth=0.5)

ggplot(mpg,aes(x=displ,color=drv)) +
  geom_histogram(binwidth=0.5,alpha=0.6) +
  facet_wrap(~drv,ncol=1)
