#Joy Buongiorno
#jbuongior21@gmail.com
#Hydrogen plot

library(ggplot2)
hyd<-read.csv("Hydrogen.csv")
KF_H<-subset(hyd, Fjord %in% c("KF"))
head(KF_H)
VK_H<-hyd[1:27,]
KF_H_plot<-ggplot(KF_H, aes(x=Depth, y=Hydrogen, shape=Station, color=Station)) + 
  geom_point(aes(fill=Station), colour="black", size=4, stroke=2) +
  scale_shape_manual(values = c(23, 22)) +
  theme_bw(base_size = 20) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(text = element_text(size=20)) +
  scale_x_reverse(limits=c(25, 0)) +
  geom_line(aes(color=Station, linetype=Station)) +
  geom_line(size=1) +
  labs(x="Depth (cmbsf)", y= "Hydrogen (nM)") +
  coord_flip()