temp<-seq(0,48,0.5)
modBuchanan<-function(temp,log10N0,log10Nmax,mumax,lag){
  log10N<-log10N0 + (temp >= lag) * (temp <= (lag + (log10Nmax - log10N0) * log(10) / mumax)) *
    mumax * (temp - lag) / log(10) + (temp >= lag) * (temp>(lag + (log10Nmax - log10N0) * log(10) / mumax)) * (log10Nmax - log10N0)
  return(log10N)
}
modBuchanan(temp,1,9,0.5,4)
library(tidyverse)
library(ggplot2)
data<-tibble(temp,modBuchanan(temp,1,9,0.5,4))
data
ggplot(data, aes(x=t,y=modBuchanan(t,1,9,0.5,4)))+
  geom_line()+
  ylab("Log10N")+
  xlab("temps en heures")+
  theme_bw()
