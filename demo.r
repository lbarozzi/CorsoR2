# install.packages("tidyverse")
library(tidyverse)

dtm<-mpg


ggplot(data=dtm)

ggplot(data=dtm) + geom_point(mapping = aes(x=displ, y = hwy))
ggplot(data=dtm) + geom_point(mapping = aes(x=displ, y = hwy, color=class))
ggplot(data=dtm) + geom_point(mapping = aes(x=year, y = class, color=hwy, size=displ))

ggplot(data=dtm) + geom_point(mapping= aes(x=displ, y=hwy, shape=class, size=cty))

ggplot(data=dtm) + geom_point(mapping= aes(x=displ, y=hwy, color=class)) + facet_wrap(~ class, nrow=2)

ggplot(data=dtm) + geom_point(mapping= aes(x=displ, y=hwy, color=class)) + facet_grid( drv ~ cyl)

ggplot(data=dtm) + geom_point(mapping= aes(x=displ, y=hwy, color=manufacturer)) + facet_wrap(~ class, nrow=2)

ggplot(data=dtm) + geom_point(mapping= aes(x=displ, y=hwy, color=manufacturer)) + geom_smooth(mapping=aes(x=displ,y=hwy))

#Lab 1
#  show data significantly
ggplot(data=mpg) + geom_bar(mapping=aes(x=class,fill=manufacturer, color=manufacturer))

