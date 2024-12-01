library(ggplot2)
raw <- read.csv("yagnyeshwaran.csv",header=TRUE)

library(dplyr)
data <- mutate(
 raw,
 ameas.ms2 = 2*x.m/t.s^2
 )

apredicted.ms2 <- function(m2,m1,mc){
  9.81*m2/(m2+m1+mc)
}

fig <- ggplot(data)+geom_point(aes(x=m1.kg,y=ameas.ms2))+
    ylim(0,2)+xlim(0,4)+
    geom_function(fun=apredicted.ms2,args=list(m2=0.23,mc=0.5),color='blue')+
    xlab('$m_1$ (\\unit{\\kilo\\gram})')+
    ylab('$a$ (\\unit{\\meter\\per\\second\\squared})')+
    theme_bw(base_size=8)

library(svglite)
svglite('fig3.svg',width=3,height=2,pointsize=8)
print(fig)
dev.off()

library(xtable)
results <- summarize(
	mean.t = mean(t.s),
	sd.t = sd(t.s),
	mean.a = mean(ameas.ms2),
	sd.a = sd(ameas.ms2),
	group_by(data,m1.kg)
	)
print(xtable(results),include.rownames=FALSE,file='table1raw.tex')

