#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
                            ## CIRCULAR ANALYSES ##
                  # PREPARING, ANALYZING AND PLOTTING DATA #
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

#=========================#
# Install needed packages #
#=========================#

  # Take the hashes out if you already have them installed
#install.packages("circular")
#install.packages("CircStats")
#install.packages("ggplot2")
#install.packages("patchwork")
#install.packages("MASS")
#install.packages("reshape")

#======================#
# Load needed packages #
#======================#

library(circular)
library(CircStats)
library(ggplot2)
library(patchwork)
library(MASS)
library(reshape)

#================#
# Preparing data #
#================#

# Data should be analyzed individually by phenophase
setwd("C:/Users/warri/Desktop/Nova pasta")
data<-read.table("phenophases.txt",h=T)
data

# Separate the angles for each column
leaf_flush<-rep(data$angles,data$leaf_flush)
flower_budding<-rep(data$angles,data$flower_budding)
flowering<-rep(data$angles,data$flowering)
unripe_fruits<-rep(data$angles,data$unripe_fruits)
ripe_fruits<-rep(data$angles,data$ripe_fruits)

# Transform data to the circular scale
  # Radians are the pattern unit. Change it for degrees,
leaf_flush.circ<-as.circular(units = c("degrees"),leaf_flush)
flower_budding.circ<-as.circular(units = c("degrees"),flower_budding)
flowering.circ<-as.circular(units = c("degrees"),flowering)
unripe_fruits.circ<-as.circular(units = c("degrees"),unripe_fruits)
ripe_fruits.circ<-as.circular(units = c("degrees"),ripe_fruits)

#========================#
# Descriptive Statistics #
#========================#

# Mean Angle
  # Corresponds to the central trend of each phenophase
mean.circular(leaf_flush.circ)
mean.circular(flower_budding.circ)
mean.circular(flowering.circ)
mean.circular(unripe_fruits.circ)
mean.circular(ripe_fruits.circ)

# Length of vector r
  # Represents the concentration of data around the mean angle
  # It varies from 0 (uniformly distributed around the year)
  # to 1 (concentrated around the mean angle)
rho.circular(leaf_flush.circ)
rho.circular(flower_budding.circ)
rho.circular(flowering.circ)
rho.circular(unripe_fruits.circ)
rho.circular(ripe_fruits.circ)

# Angular standard deviation 
  # The function sd.circular returns the values in radians.
  # It's necessary to transform these values to degrees.
  # To do so, (180/3.1416) was added to the original formula.
sd.circular(leaf_flush.circ)*(180/3.1416)
sd.circular(flower_budding.circ)*(180/3.1416)
sd.circular(flowering.circ)*(180/3.1416)
sd.circular(unripe_fruits.circ)*(180/3.1416)
sd.circular(ripe_fruits.circ)*(180/3.1416)

#========================#
# Data Distribution Test #
#========================#

# Normality Test (Rayleigh Test)
  # If p is significant, then there is a seasonal pattern
r.test(leaf_flush.circ,degree=TRUE)
r.test(flower_budding.circ,degree=TRUE)
r.test(flowering.circ,degree=TRUE)
r.test(unripe_fruits.circ,degree=TRUE)
r.test(ripe_fruits.circ,degree=TRUE)

#====================#
# Data Visualization #
#====================#

# The graphs will be created from a different dataset

    # Leaf Flushing

# Preparing data to plot it
setwd("C:/Users/warri/Desktop/Nova pasta")
data_graph<-read.table("leaf_flushing.txt",h= TRUE)
data_graph

data_graph=data_graph[1:12,]
data1=data.frame(t(data_graph))
data2=data1[2:5,]
colnames(data2)=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct",
                  "Nov","Dec")
data2$group=row.names(data2)
data3=melt(data2,id="group")
data3$value=as.numeric(data3$value)
head(data3)

# Plot the graph
  # alpha -> controls the transparency of the graph
  # width -> bar width
  # x and xend -> indicate the position of the vector r
    # It must be adjusted according to the mean angle, which have to be
    # adjusted to a month scale. The point 0 is represented by 0.5 in the graph
    # So, add 0.5 to the result you got from the mean angle (transformed)
  # y and yend -> indicate the size of the vector r
    # yend is the vector r*100 to fit in the scale

graph_LF<-
  ggplot(data=data3,aes(x=variable,y=value,fill=group))+
  geom_bar(stat="identity",
           alpha = 0.85,
           width=1,
           position = position_stack(reverse = TRUE))+
  scale_fill_manual(values = c("olivedrab2","chartreuse3",
                               "green3","chartreuse4"),
                    name="Intensity scale",
                    labels=c("1 (1-25%)", "2 (25-50%)",
                             "3 (50-75%)","4 (75-100%)"))+
  theme(legend.position=c(1.05,0.13),
        legend.key.size = unit(0.5, "cm"),
        legend.title = element_text(size=20),
        legend.text = element_text(size=20),
        text = element_text(size = 24),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "grey"))+
  coord_polar()+xlab("")+ylab("")+ylim(0,100)+
  geom_segment(aes(x=11.7, xend=11.7, y=0, yend=39),
               arrow = arrow(length = unit(.2, "cm"),
                             type = "closed"),
               size=0.7,
               colour="black")

graph_LF

# Save the graph
ggsave("leaf_flushing.pdf", graph_LF, dpi=600, units="cm",
       height = 20, width = 30)

  
    # Flower budding

#Preparing data to plot it
setwd("C:/Users/warri/Desktop/Nova pasta")
data_graph2<-read.table("flower_budding.txt",h= TRUE)
data_graph2

data_graph2=data_graph2[1:12,]
data4=data.frame(t(data_graph2))
data5=data4[2:5,]
colnames(data5)=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct",
                  "Nov","Dec")
data5$group=row.names(data5)
data6=melt(data5,id="group")
data6$value=as.numeric(data6$value)
head(data6)

# Plot the graph
graph_FB<-
  ggplot(data=data6,aes(x=variable,y=value,fill=group))+
  geom_bar(stat="identity",
           alpha = 0.85,
           width=1,
           position = position_stack(reverse = TRUE))+
  scale_fill_manual(values = c("darkgoldenrod3","goldenrod2",
                               "goldenrod1","gold"),
                    name="Intensity scale",
                    labels=c("1 (1-25%)", "2 (25-50%)",
                             "3 (50-75%)","4 (75-100%)"))+
  theme(legend.position=c(1.05,0.13),
        legend.key.size = unit(0.5, "cm"),
        legend.title = element_text(size=20),
        legend.text = element_text(size=20),
        text = element_text(size = 24),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "grey"))+
  coord_polar()+xlab("")+ylab("")+ylim(0,100)+
  geom_segment(aes(x=10.8, xend=10.8, y=0, yend=49),
               arrow = arrow(length = unit(.2, "cm"),
                             type = "closed"),
               size=0.7,
               colour="black")

graph_FB

# Save the graph
ggsave("flower_budding.pdf", graph_FB, dpi=600, units="cm",
       height = 20, width = 30)


  # Flowering

#Preparing data to plot it
setwd("C:/Users/warri/Desktop/Nova pasta")
data_graph3<-read.table("flowering.txt",h= TRUE)
data_graph3

data_graph3=data_graph3[1:12,]
data7=data.frame(t(data_graph3))
data8=data7[2:5,]
colnames(data8)=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct",
                  "Nov","Dec")
data8$group=row.names(data8)
data9=melt(data8,id="group")
data9$value=as.numeric(data9$value)
head(data9)

# Plot the graph
graph_FL<-
  ggplot(data=data9,aes(x=variable,y=value,fill=group))+
  geom_bar(stat="identity",
           alpha = 0.85,
           width=1,
           position = position_stack(reverse = TRUE))+
  scale_fill_manual(values = c("yellow1","gold1","gold2", "gold3"),
                    name="Intensity scale",
                    labels=c("1 (1-25%)", "2 (25-50%)",
                             "3 (50-75%)","4 (75-100%)"))+
  theme(legend.position=c(1.05,0.13),
        legend.key.size = unit(0.5, "cm"),
        legend.title = element_text(size=20),
        legend.text = element_text(size=20),
        text = element_text(size = 24),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "grey"))+
  coord_polar()+xlab("")+ylab("")+ylim(0,100)+
  geom_segment(aes(x=11.8, xend=11.8, y=0, yend=45),
               arrow = arrow(length = unit(.2, "cm"),
                             type = "closed"),
               size=0.7,
               colour="black")

graph_FL

# Save the graph
ggsave("flowering.pdf", graph_FL, dpi=600, units="cm",
       height = 20, width = 30)

 
   # Unripe fruits

#Preparing data to plot it
setwd("C:/Users/warri/Desktop/Nova pasta")
data_graph4<-read.table("unripe_fruits.txt",h= TRUE)
data_graph4

data_graph4=data_graph4[1:12,]
data10=data.frame(t(data_graph4))
data11=data10[2:5,]
colnames(data11)=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct",
                  "Nov","Dec")
data11$group=row.names(data11)
data12=melt(data11,id="group")
data12$value=as.numeric(data12$value)
head(data12)

# Plot the graph
graph_UF<-
  ggplot(data=data12,aes(x=variable,y=value,fill=group))+
  geom_bar(stat="identity",
           alpha = 0.85,
           width=1,
           position = position_stack(reverse = TRUE))+
  scale_fill_manual(values = c("mediumorchid1","mediumorchid2",
                               "mediumorchid3", "mediumorchid4"),
                    name="Intensity scale",
                    labels=c("1 (1-25%)", "2 (25-50%)",
                             "3 (50-75%)","4 (75-100%)"))+
  theme(legend.position=c(1.05,0.13),
        legend.key.size = unit(0.5, "cm"),
        legend.title = element_text(size=20),
        legend.text = element_text(size=20),
        text = element_text(size = 24),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "grey"))+
  coord_polar()+xlab("")+ylab("")+ylim(0,100)+
  geom_segment(aes(x=2.14, xend=2.14, y=0, yend=45),
               arrow = arrow(length = unit(.2, "cm"),
                             type = "closed"),
               size=0.7,
               colour="black")

graph_UF

# Save the graph
ggsave("unripe_fruits.pdf", graph_UF, dpi=600, units="cm",
       height = 20, width = 30)

    
    # Ripe fruits

#Preparing data to plot it
setwd("C:/Users/warri/Desktop/Nova pasta")
data_graph5<-read.table("ripe_fruits.txt",h= TRUE)
data_graph5

data_graph5=data_graph5[1:12,]
data13=data.frame(t(data_graph5))
data14=data13[2:5,]
colnames(data14)=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct",
                   "Nov","Dec")
data14$group=row.names(data14)
data15=melt(data14,id="group")
data15$value=as.numeric(data15$value)
head(data15)

# Plot the graph
graph_RF<-
  ggplot(data=data15,aes(x=variable,y=value,fill=group))+
  geom_bar(stat="identity",
           alpha = 0.85,
           width=1,
           position = position_stack(reverse = TRUE))+
  scale_fill_manual(values = c("purple1","purple2","purple3", "purple4"),
                    name="Intensity scale",
                    labels=c("1 (1-25%)", "2 (25-50%)",
                             "3 (50-75%)","4 (75-100%)"))+
  theme(legend.position = c(1.05,0.13),
        legend.key.size = unit(0.5, "cm"),
        legend.title = element_text(size=20),
        legend.text = element_text(size=20),
        text = element_text(size = 24),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "grey"))+
  coord_polar()+xlab("")+ylab("")+ylim(0,100)+
  geom_segment(aes(x=6.4, xend=6.4, y=0, yend=40),
               arrow = arrow(length = unit(.2, "cm"),
                             type = "closed"),
               size=0.7,
               colour="black")

graph_RF

# Save the graph
ggsave("ripe_fruits.pdf", graph_RF, dpi=600, units="cm",
       height = 20, width = 30)

#==============================================================================#