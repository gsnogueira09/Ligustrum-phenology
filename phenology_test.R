#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
                            ## CIRCULAR ANALYSES ##
                   # COMPARISON BETWEEN TWO CIRCULAR SAMPLES #
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
data<-read.table("pheno_comp1.txt",h=T)
data

# Separate the angles for each column
NAF_fl<-rep(data$angles,data$FOM_Fl)
UAF_fl<-rep(data$angles,data$Barigui_Fl)
ligustrum_fl<-rep(data$angles,data$Ligustrum_Fl)
NAF_fr<-rep(data$angles,data$FOM_Fr)
UAF_fr<-rep(data$angles,data$Barigui_Fr)
ligustrum_fr<-rep(data$angles,data$Ligustrum_Fr)


# Transform data to the circular scale
  # Radians are the pattern unit. Change it for degrees,
NAF_fl.circ<-as.circular(units = c("degrees"),NAF_fl)
UAF_fl.circ<-as.circular(units = c("degrees"),UAF_fl)
ligustrum_fl.circ<-as.circular(units = c("degrees"),ligustrum_fl)
NAF_fr.circ<-as.circular(units = c("degrees"),NAF_fr)
UAF_fr.circ<-as.circular(units = c("degrees"),UAF_fr)
ligustrum_fr.circ<-as.circular(units = c("degrees"),ligustrum_fr)

#========================#
# Descriptive Statistics #
#========================#

# Mean Angle
  # Corresponds to the central trend of each phenophase
mean.circular(NAF_fl.circ)
mean.circular(UAF_fl.circ)
mean.circular(ligustrum_fl.circ)
mean.circular(NAF_fr.circ)
mean.circular(UAF_fr.circ)
mean.circular(ligustrum_fr.circ)


# Length of vector r
  # Represents the concentration of data around the mean angle
  # It varies from 0 (uniformly distributed around the year)
  # to 1 (concentrated around the mean angle)
rho.circular(NAF_fl.circ)
rho.circular(UAF_fl.circ)
rho.circular(ligustrum_fl.circ)
rho.circular(NAF_fr.circ)
rho.circular(UAF_fr.circ)
rho.circular(ligustrum_fr.circ)


# Angular standard deviation 
  # The function sd.circular returns the values in radians.
  # It's necessary to transform these values to degrees.
  # To do so, (180/3.1416) was added to the original formula.
sd.circular(NAF_fl.circ)*(180/3.1416)
sd.circular(UAF_fl.circ)*(180/3.1416)
sd.circular(ligustrum_fl.circ)*(180/3.1416)
sd.circular(NAF_fr.circ)*(180/3.1416)
sd.circular(UAF_fr.circ)*(180/3.1416)
sd.circular(ligustrum_fr.circ)*(180/3.1416)


#========================#
# Data Distribution Test #
#========================#

# Normality Test (Rayleigh Test)
  # If p is significant, then there is a seasonal pattern
r.test(NAF_fl.circ,degree=TRUE)
r.test(UAF_fl.circ,degree=TRUE)
r.test(ligustrum_fl.circ,degree=TRUE)
r.test(NAF_fr.circ,degree=TRUE)
r.test(UAF_fr.circ,degree=TRUE)
r.test(ligustrum_fr.circ,degree=TRUE)


#=============================================#
# Data Comparison - Watson-Williams Test #
#=============================================#

# These tests consider as premise the data distribution is unimodal.
# Only groups that show a unimodal distribution according to Rayleigh Test
#   can be submitted to them.

watson.two(NAF_fl.circ, UAF_fl.circ)
watson.two(NAF_fl.circ, ligustrum_fl.circ)
watson.two(ligustrum_fl.circ, UAF_fl.circ)
watson.two(NAF_fr.circ, UAF_fr.circ)
watson.two(NAF_fr.circ, ligustrum_fr.circ)
watson.two(ligustrum_fr.circ, UAF_fr.circ)


#====================#
# Data Visualization #
#====================#

# The graphs will be created from a different dataset

    # Flowering

# Preparing data to plot it
setwd("C:/Users/warri/Desktop/Nova pasta")
data_graph<-read.table("pheno_comp2.txt",h= TRUE)
data_graph

data_graph=data_graph[1:12,]
data1=data.frame(t(data_graph))

# NAF data
data2=data1[3,]
colnames(data2)=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct",
                  "Nov","Dec")
data2$group=row.names(data2)
data3=melt(data2,id="group")
data3$value=as.numeric(data3$value)
head(data3)

# UAF data
data4=data1[4,]
colnames(data4)=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct",
                  "Nov","Dec")
data4$group=row.names(data4)
data5=melt(data4,id="group")
data5$value=as.numeric(data5$value)
head(data5)

# Ligustrum data

data6=data1[5,]
colnames(data6)=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct",
                  "Nov","Dec")
data6$group=row.names(data6)
data7=melt(data6,id="group")
data7$value=as.numeric(data7$value)
head(data7)


# Plot the graphs
  # alpha -> controls the transparency of the graph
  # width -> bar width
  # x and xend -> indicate the position of the vector r
    # It must be adjusted according to the mean angle, which have to be
    # adjusted to a month scale. The point 0 is represented by 0.5 in the graph
    # So, add 0.5 to the result you got from the mean angle (transformed)
  # y and yend -> indicate the size of the vector r
    # yend is the vector r*100 to fit in the scale

graph_NAFfl<-ggplot(data=data3,aes(x=variable,y=value,fill=group))+
  geom_bar(alpha = 0.85,
           stat = "identity",
           width = 1,
           fill = "darkgreen",
           colour = "darkgreen",
           lwd = 0.6)+
  theme(panel.background = element_rect(fill = "white"),
        text = element_text(size = 13),
        panel.grid.major = element_line(color = "grey"))+
  coord_polar()+xlab("")+ylab("")+ylim(0,100)+
  geom_segment(aes(x=10.94, xend=10.94, y=0, yend=51),
               arrow = arrow(length = unit(.2, "cm"),
                             type = "closed"),
               size=0.7,
               colour="green3")
graph_NAFfl


graph_UAFfl<-ggplot(data=data5,aes(x=variable,y=value,fill=group))+
  geom_bar(alpha = 0.85,
           stat = "identity",
           width = 1,
           fill = "darkorange3",
           colour = "darkorange3",
           lwd = 0.6)+
  theme(panel.background = element_rect(fill = "white"),
        text = element_text(size = 13),
        panel.grid.major = element_line(color = "grey"))+
  coord_polar()+xlab("")+ylab("")+ylim(0,100)+
  geom_segment(aes(x=10.13, xend=10.13, y=0, yend=23),
               arrow = arrow(length = unit(.2, "cm"),
                             type = "closed"),
               size=0.7,
               colour="red3")
graph_UAFfl


graph_LIGfl<-ggplot(data=data7,aes(x=variable,y=value,fill=group))+
  geom_bar(alpha = 0.85,
           stat = "identity",
           width = 1,
           fill = "purple4",
           colour = "purple4",
           lwd = 0.6)+
  theme(panel.background = element_rect(fill = "white"),
        text = element_text(size = 13),
        panel.grid.major = element_line(color = "grey"))+
  coord_polar()+xlab("")+ylab("")+ylim(0,100)+
  geom_segment(aes(x=11.8, xend=11.8, y=0, yend=55),
               arrow = arrow(length = unit(.2, "cm"),
                             type = "closed"),
               size=0.7,
               colour="blue1")
graph_LIGfl


# Combining graphs


graph_NxL_fl <- ggplot()+
  geom_bar(data=data3,aes(x=variable,y=value,fill=group),
           alpha = 0.85, stat="identity", width= 1,
           fill = "darkgreen",
           colour="darkgreen",lwd=0.6)+
  geom_bar(data=data7,aes(x=variable,y=value,fill=group),
           alpha = 0.5,stat="identity", width=1,fill="purple4",
           colour="purple4",lwd=0.6)+
  theme(panel.background = element_rect(fill = "white"),
        text = element_text(size = 24),
        panel.grid.major = element_line(color = "grey"))+
  coord_polar()+xlab("")+ylab("")+ylim(0,100)+
  geom_segment(aes(x=11.8, xend=11.8, y=0, yend=55),
               arrow = arrow(length = unit(.2, "cm"), type = "closed"),
               size=0.7,colour="blue1")+
  geom_segment(aes(x=10.94, xend=10.94, y=0, yend=51),
               arrow = arrow(length = unit(.2, "cm"), type = "closed"),
               size=0.7,colour="green3")
graph_NxL_fl


graph_UxL_fl <- ggplot()+
  geom_bar(data=data5,aes(x=variable,y=value,fill=group),
           alpha = 0.85, stat="identity", width= 1,
           fill = "darkorange3",
           colour="darkorange3",lwd=0.6)+
  geom_bar(data=data7,aes(x=variable,y=value,fill=group),
           alpha = 0.5,stat="identity", width=1,fill="purple4",
           colour="purple4",lwd=0.6)+
  theme(panel.background = element_rect(fill = "white"),
        text = element_text(size = 24),
        panel.grid.major = element_line(color = "grey"))+
  coord_polar()+xlab("")+ylab("")+ylim(0,100)+
  geom_segment(aes(x=11.8, xend=11.8, y=0, yend=55),
               arrow = arrow(length = unit(.2, "cm"), type = "closed"),
               size=0.7,colour="blue1")+
  geom_segment(aes(x=10.13, xend=10.13, y=0, yend=23),
               arrow = arrow(length = unit(.2, "cm"), type = "closed"),
               size=0.7,colour="red3")
graph_UxL_fl


# Save the graphs
ggsave("NxL_fl.pdf", graph_NxL_fl, dpi=600, units="cm",
       height = 20, width = 30)

ggsave("UxL_fl.pdf", graph_UxL_fl, dpi=600, units="cm",
       height = 20, width = 30)

  
    # Fruiting

# NAF data
data8=data1[6,]
colnames(data8)=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct",
                  "Nov","Dec")
data8$group=row.names(data8)
data9=melt(data8,id="group")
data9$value=as.numeric(data9$value)
head(data9)

# UAF data
data10=data1[7,]
colnames(data10)=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct",
                  "Nov","Dec")
data10$group=row.names(data10)
data11=melt(data10,id="group")
data11$value=as.numeric(data11$value)
head(data11)

# Ligustrum data

data12=data1[8,]
colnames(data12)=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct",
                  "Nov","Dec")
data12$group=row.names(data12)
data13=melt(data12,id="group")
data13$value=as.numeric(data13$value)
head(data13)

graph_NAFfr<-ggplot(data=data9,aes(x=variable,y=value,fill=group))+
  geom_bar(alpha = 0.85,
           stat = "identity",
           width = 1,
           fill = "darkgreen",
           colour = "darkgreen",
           lwd = 0.6)+
  theme(panel.background = element_rect(fill = "white"),
        text = element_text(size = 13),
        panel.grid.major = element_line(color = "grey"))+
  coord_polar()+xlab("")+ylab("")+ylim(0,100)+
  geom_segment(aes(x=2.27, xend=2.27, y=0, yend=52),
               arrow = arrow(length = unit(.2, "cm"),
                             type = "closed"),
               size=0.7,
               colour="green3")
graph_NAFfr


graph_UAFfr<-ggplot(data=data11,aes(x=variable,y=value,fill=group))+
  geom_bar(alpha = 0.85,
           stat = "identity",
           width = 1,
           fill = "darkorange3",
           colour = "darkorange3",
           lwd = 0.6)+
  theme(panel.background = element_rect(fill = "white"),
        text = element_text(size = 13),
        panel.grid.major = element_line(color = "grey"))+
  coord_polar()+xlab("")+ylab("")+ylim(0,100)+
  geom_segment(aes(x=12.05, xend=12.05, y=0, yend=17),
               arrow = arrow(length = unit(.2, "cm"),
                             type = "closed"),
               size=0.7,
               colour="red3")
graph_UAFfr


graph_LIGfr<-ggplot(data=data13,aes(x=variable,y=value,fill=group))+
  geom_bar(alpha = 0.85,
           stat = "identity",
           width = 1,
           fill = "purple4",
           colour = "purple4",
           lwd = 0.6)+
  theme(panel.background = element_rect(fill = "white"),
        text = element_text(size = 13),
        panel.grid.major = element_line(color = "grey"))+
  coord_polar()+xlab("")+ylab("")+ylim(0,100)+
  geom_segment(aes(x=6.4, xend=6.4, y=0, yend=41),
               arrow = arrow(length = unit(.2, "cm"),
                             type = "closed"),
               size=0.7,
               colour="blue1")
graph_LIGfr


# Combining graphs


graph_NxL_fr <- ggplot()+
  geom_bar(data=data9,aes(x=variable,y=value,fill=group),
           alpha = 0.85, stat="identity", width= 1,
           fill = "darkgreen",
           colour="darkgreen",lwd=0.6)+
  geom_bar(data=data13,aes(x=variable,y=value,fill=group),
           alpha = 0.5,stat="identity", width=1,fill="purple4",
           colour="purple4",lwd=0.6)+
  theme(panel.background = element_rect(fill = "white"),
        text = element_text(size = 24),
        panel.grid.major = element_line(color = "grey"))+
  coord_polar()+xlab("")+ylab("")+ylim(0,100)+
  geom_segment(aes(x=6.4, xend=6.4, y=0, yend=41),
               arrow = arrow(length = unit(.2, "cm"), type = "closed"),
               size=0.7,colour="blue1")+
  geom_segment(aes(x=2.27, xend=2.27, y=0, yend=52),
               arrow = arrow(length = unit(.2, "cm"), type = "closed"),
               size=0.7,colour="green3")
graph_NxL_fr


graph_UxL_fr <- ggplot()+
  geom_bar(data=data11,aes(x=variable,y=value,fill=group),
           alpha = 0.85, stat="identity", width= 1,
           fill = "darkorange3",
           colour="darkorange3",lwd=0.6)+
  geom_bar(data=data13,aes(x=variable,y=value,fill=group),
           alpha = 0.5,stat="identity", width=1,fill="purple4",
           colour="purple4",lwd=0.6)+
  theme(panel.background = element_rect(fill = "white"),
        text = element_text(size = 24),
        panel.grid.major = element_line(color = "grey"))+
  coord_polar()+xlab("")+ylab("")+ylim(0,100)+
  geom_segment(aes(x=6.4, xend=6.4, y=0, yend=41),
               arrow = arrow(length = unit(.2, "cm"), type = "closed"),
               size=0.7,colour="blue1")+
  geom_segment(aes(x=12.05, xend=12.05, y=0, yend=17),
               arrow = arrow(length = unit(.2, "cm"), type = "closed"),
               size=0.7,colour="red3")
graph_UxL_fr


# Save the graphs
ggsave("NxL_fr.pdf", graph_NxL_fr, dpi=600, units="cm",
       height = 20, width = 30)

ggsave("UxL_fr.pdf", graph_UxL_fr, dpi=600, units="cm",
       height = 20, width = 30)

#==============================================================================#