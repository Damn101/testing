library(magick)	
library(ggplot2)
library(gganimate)
library(gridExtra)
library(grid)
library(ggrepel)
##sharukhan area
Datasource <- read.csv("movies.csv")
mean(Datasource$Rating)
Datasource$Movies <- as.factor(Datasource$Movies)
factoring1 <- factor(Datasource$Movies,levels = c("ZERO","Jab Harry Met Sejal","Raees","Dear Zindagi","Fan","Dilwale"))
factoring1 <- factor(factoring1,levels = rev(levels(factoring1)))
Draw <- ggplot(Datasource,aes(x=factoring1,y=Rating,group=1,label=as.integer(Datasource$Rating)),show.legend=FALSE)+xlab("Last 6 movies")+ylab("IMDB Rating") +scale_y_continuous(limits = c(2,10),breaks = seq(2,10,1) )+geom_point()+geom_line(color="red4",size=1.5)
Draw1 <- Draw+theme(axis.text.x = element_text(angle = 20, hjust = 1))+geom_hline(aes(yintercept=5,color="red"))+theme_test()+theme(legend.position = "none")+geom_hline(aes(yintercept=7,color="red"))
factoring
Draw1
anim <- Draw1+transition_reveal(Datasource$S.no)
second <- anim+enter_fade() +
    exit_shrink() +
    ease_aes('sine-in-out')+ggtitle("Sharukh Khan (Avg=6.23)")+theme(axis.text.x = element_text(angle = 20, hjust = 1,size = 10,face="bold"))+theme(axis.text.y = element_text( hjust = 1,size = 10,face="bold"))+theme(plot.title = element_text(hjust = 0.5,face = "bold",size = 15))+theme(axis.title.x = element_text(face = "bold",size=12))+theme(axis.title.y = element_text(face = "bold",size=12))

second
second_gif <- animate(second, width = 240, height = 240,nframes = 100,fps = 20,type="cairo")
##sharukhan area end
##salman area
Datasource2 <- read.csv("Sallu.csv")
Datasource2$Movies <- as.factor(Datasource2$Movies)
Datasource2$Movies
factoring2 <- factor(Datasource2$Movies,levels = c("Dabbang 3","Bharat","Race 3","Tiger Zinda he ","Tubelight","Sultan"))
factoring2 <- factor(factoring2,levels = rev(levels(factoring2)))
Draw2 <- ggplot(Datasource2,aes(x=factoring2,y=Rating,group=1),show.legend=FALSE)+xlab("Last 6 movies")+ylab("IMDB Rating") +scale_y_continuous(limits = c(2,10),breaks = seq(2,10,1) )+geom_point()+geom_line(color="red",size=1.2)
Draw2
Draw12 <- Draw2+geom_hline(aes(yintercept=7,color="red"))+theme_test()+theme(legend.position = "none")+geom_hline(aes(yintercept=5,color="red"))
Draw12
anim2 <- Draw12+transition_reveal(Datasource$S.no)
first <- anim2 +enter_fade() +  exit_shrink() +
    ease_aes('sine-in-out')+ggtitle("Salman Khan(Avg=4.61)")+theme(axis.text.x = element_text(angle = 20, hjust = 1,size = 10,face="bold"))+theme(axis.text.y = element_text( hjust = 1,size = 10,face="bold"))+theme(plot.title = element_text(hjust = 0.5,face = "bold",size = 15))+theme(axis.title.x = element_text(face = "bold",size=12))+theme(axis.title.y = element_text(face = "bold",size=12))
first
first_gif <- animate(first, width = 240, height = 240,nframes = 100,fps = 20,type="cairo")
first_gif
##salman area end
##akshay part
Datasource3 <- read.csv("Akshay kumar.csv")
mean(Datasource3$Rating)
Datasource3$Movies <- as.factor(Datasource3$Movies)
factoring3 <- factor(Datasource3$Movies,levels = c("Good Newwz","Housefull 4","Mission Mangal","Kesari","Gold","Padman"))
factoring3 <- factor(factoring3,levels = rev(levels(factoring3)))
DrawAKSHAY <- ggplot(Datasource3,aes(x=factoring3,y=Rating,group=1),show.legend=FALSE)+xlab("Last 6 movies")+ylab("IMDB Rating") +scale_y_continuous(limits = c(2,10),breaks = seq(2,10,1) )+geom_point()+geom_line(color="cyan4",size=1.2,)
Draw1AKSHAY <- DrawAKSHAY+theme(axis.text.x = element_text(angle = 20, hjust = 1))+geom_hline(aes(yintercept=5,color="red"))+theme_test()+theme(legend.position = "none")+geom_hline(aes(yintercept=7,color="red"))
animAKSHAY <- Draw1AKSHAY+transition_reveal(Datasource3$S.no)
animAKSHAY
secondAKSHAY <- animAKSHAY+enter_fade() +exit_shrink() + ease_aes('sine-in-out')+ggtitle("Akshay Kumar (Avg=6.6)")+theme(axis.text.x = element_text(angle = 20, hjust = 1,size = 10,face="bold"))+theme(axis.text.y = element_text( hjust = 1,size = 10,face="bold"))+theme(plot.title = element_text(hjust = 0.5,face = "bold",size = 15))+theme(axis.title.x = element_text(face = "bold",size=12))+theme(axis.title.y = element_text(face = "bold",size=12))
secondAKSHAY
second_gifAKSHAY <- animate(secondAKSHAY, width = 240, height = 240,nframes = 100,fps = 20,type="cairo")
second_gifAKSHAY
##Akshay part end
##amir start
Datasource4 <- read.csv("Amir khan.csv")
mean(Datasource4$Rating)
Datasource4$Movies <- as.factor(Datasource4$Movies)
factoring4 <- factor(Datasource4$Movies,levels = c("Thugs of Hindostan","Dangal","PK","Dhoom 3","Talaash","3 Idiots"))
Datasource4$Movies
factoring4 <- factor(factoring4,levels = rev(levels(factoring4)))
DrawAmir <- ggplot(Datasource4,aes(x=factoring4,y=Rating,group=1),show.legend=FALSE)+xlab("Last 6 movies")+ylab("IMDB Rating") +scale_y_continuous(limits = c(2,10),breaks = seq(2,10,1) )+geom_point()+geom_line(color="green",size=1.2,)
Draw1Amir <- DrawAmir+theme(axis.text.x = element_text(angle = 20, hjust = 1))+geom_hline(aes(yintercept=5,color="red"))+theme_test()+theme(legend.position = "none")+geom_hline(aes(yintercept=7,color="red"))
animAmir <- Draw1Amir+transition_reveal(Datasource4$S.no)
secondAmir <- animAmir+enter_fade() +exit_shrink() + ease_aes('sine-in-out')+ggtitle("Amir Khan (Avg=6.9)")+theme(axis.text.x = element_text(angle = 20, hjust = 1,size = 10,face="bold"))+theme(axis.text.y = element_text( hjust = 1,size = 10,face="bold"))+theme(plot.title = element_text(hjust = 0.5,face = "bold",size = 15))+theme(axis.title.x = element_text(face = "bold",size=12))+theme(axis.title.y = element_text(face = "bold",size=12))
second_gifAmir <- animate(secondAmir, width = 240, height = 240,nframes = 100,fps = 20,type="cairo")
##amirend
a_mgik <- image_read(first_gif)
b_mgik <- image_read(second_gif)
c_mgik <- image_read(second_gifAKSHAY)
d_mgik <- image_read(second_gifAmir)
new_gif <- image_append(c(a_mgik[1], b_mgik[1],c_mgik[1],d_mgik[1]))
for(i in 2:100){
    combined <- image_append(c(a_mgik[i], b_mgik[i],c_mgik[i],d_mgik[i]))
    new_gif <- c(new_gif, combined)
}
new_gif

