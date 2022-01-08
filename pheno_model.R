#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
                          ## PHENOLOGICAL MODELLING ##
                  # PREPARING, ANALYZING AND MODELLING DATA #
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

#=========================#
# Install needed packages #
#=========================#

  # Take the hashes out if you already have them installed
#install.packages("vegan")
#install.packages("car")
#install.packages("nlme")
#install.packages("MuMIn")
#install.packages("Hmisc")
#install.packages("stringi")
#install.packages("corrplot")

#======================#
# Load needed packages #
#======================#

library("vegan")
library("car")
library("nlme")
library("MuMIn")
library("Hmisc")
library("stringi")
library("corrplot")

#================#
# Preparing data #
#================#

# Data should be analyzed individually by phenophase
setwd("C:/Users/warri/Desktop/Nova pasta")
data<-read.table("data_log.txt",h=T)
data
dim(data)
head(data)
class(data)

# Correlate environmental variables to check collinearity
  # The columns of interest should be placed between the brackets
correl.b<-rcorr(as.matrix(data[,c(6:24)]))
correl.b

# Plot the correlation graph to check out for collinearity
corrplot(correl.b$r, type="upper", order="alphabet", mar = c(0,0,0,0),
         font = 3, tl.cex = 0.8, tl.col = "black",p.mat=correl.b$P,
         sig.level=0.05,insig="blank")

# Structure a temporal autocorrelation
time<-seq(1:24)
time

# Create a structure of temporal autocorrelation taking account of the time
cor<-corCAR1(form=~time)
cor

bb<-Initialize(cor,data=data)
M<-corMatrix(bb)
M

#===============#
# Model Testing #
#===============#

# The models tested were Linear and GLS (Generalized Least Squares)
# Only not collinear variables were used to build each model
# Create a null model to generate the comparisons
# Do not forget to add "corr=cor" in brackets. It indicates the
#   temporal correlation

    # Leaf flush

attach(data)
brot

# Applying models

  # Linear models

model.brot.null<-lm(brot~1)
model1.brot<-lm(brot~DL)
model2.brot<-lm(brot~DL2)
model3.brot<-lm(brot~Rain)
model4.brot<-lm(brot~Rain1)
model5.brot<-lm(brot~RainH)
model6.brot<-lm(brot~SIR)
model7.brot<-lm(brot~SIR1)
model8.brot<-lm(brot~SIR2)
model9.brot<-lm(brot~DL+DL2)
model10.brot<-lm(brot~SIR+SIR1)
model11.brot<-lm(brot~SIR+SIR1+SIR2)
model12.brot<-lm(brot~DL+Rain)
model13.brot<-lm(brot~DL+Rain1)
model14.brot<-lm(brot~DL+RainH)
model15.brot<-lm(brot~DL+Rain+Rain1)
model16.brot<-lm(brot~DL2+Rain)
model17.brot<-lm(brot~DL2+Rain1)
model18.brot<-lm(brot~DL2+RainH)
model19.brot<-lm(brot~DL2+Rain+Rain1)
model20.brot<-lm(brot~DL+DL2+Rain)
model21.brot<-lm(brot~DL+DL2+Rain1)
model22.brot<-lm(brot~DL+DL2+RainH)
model23.brot<-lm(brot~Temp)
model24.brot<-lm(brot~Temp2)
model25.brot<-lm(brot~Temp+Temp2)
model26.brot<-lm(brot~Temp+Rain)
model27.brot<-lm(brot~Temp+Rain1)
model28.brot<-lm(brot~Temp+RainH)
model29.brot<-lm(brot~Temp+Rain+Rain1)
model30.brot<-lm(brot~Temp2+Rain)
model31.brot<-lm(brot~Temp2+Rain1)
model32.brot<-lm(brot~Temp2+RainH)
model33.brot<-lm(brot~Temp2+Rain+Rain1)
model34.brot<-lm(brot~Temp+Temp2+Rain)
model35.brot<-lm(brot~Temp+Temp2+Rain1)
model36.brot<-lm(brot~Temp+Temp2+RainH)
model37.brot<-lm(brot~Temp+Temp2+Rain+Rain1)
model38.brot<-lm(brot~DL+DL2+RainH+SIR)
model39.brot<-lm(brot~DL+DL2+RainH+SIR1)
model40.brot<-lm(brot~DL+DL2+RainH+SIR2)
model41.brot<-lm(brot~DL+DL2+RainH+SIR+SIR1)
model42.brot<-lm(brot~DL+DL2+RainH+SIR+SIR1+SIR2)
model43.brot<-lm(brot~DL+DL2+RainH+SIR1+SIR2)

  # GLS models

model.brot.gls.null<-gls(brot~1,corr=cor)
model1.brot.gls<-gls(brot~DL,corr=cor)
model2.brot.gls<-gls(brot~DL2,corr=cor)
model3.brot.gls<-gls(brot~Rain,corr=cor)
model4.brot.gls<-gls(brot~Rain1,corr=cor)
model5.brot.gls<-gls(brot~RainH,corr=cor)
model6.brot.gls<-gls(brot~SIR,corr=cor)
model7.brot.gls<-gls(brot~SIR1,corr=cor)
model8.brot.gls<-gls(brot~SIR2,corr=cor)
model9.brot.gls<-gls(brot~DL+DL2,corr=cor)
model10.brot.gls<-gls(brot~SIR+SIR1,corr=cor)
model11.brot.gls<-gls(brot~SIR+SIR1+SIR2,corr=cor)
model12.brot.gls<-gls(brot~DL+Rain,corr=cor)
model13.brot.gls<-gls(brot~DL+Rain1,corr=cor)
model14.brot.gls<-gls(brot~DL+RainH,corr=cor)
model15.brot.gls<-gls(brot~DL+Rain+Rain1,corr=cor)
model16.brot.gls<-gls(brot~DL2+Rain,corr=cor)
model17.brot.gls<-gls(brot~DL2+Rain1,corr=cor)
model18.brot.gls<-gls(brot~DL2+RainH,corr=cor)
model19.brot.gls<-gls(brot~DL2+Rain+Rain1,corr=cor)
model20.brot.gls<-gls(brot~DL+DL2+Rain,corr=cor)
model21.brot.gls<-gls(brot~DL+DL2+Rain1,corr=cor)
model22.brot.gls<-gls(brot~DL+DL2+RainH,corr=cor)
model23.brot.gls<-gls(brot~Temp,corr=cor)
model24.brot.gls<-gls(brot~Temp2,corr=cor)
model25.brot.gls<-gls(brot~Temp+Temp2,corr=cor)
model26.brot.gls<-gls(brot~Temp+Rain,corr=cor)
model27.brot.gls<-gls(brot~Temp+Rain1,corr=cor)
model28.brot.gls<-gls(brot~Temp+RainH,corr=cor)
model29.brot.gls<-gls(brot~Temp+Rain+Rain1,corr=cor)
model30.brot.gls<-gls(brot~Temp2+Rain,corr=cor)
model31.brot.gls<-gls(brot~Temp2+Rain1,corr=cor)
model32.brot.gls<-gls(brot~Temp2+RainH,corr=cor)
model33.brot.gls<-gls(brot~Temp2+Rain+Rain1,corr=cor)
model34.brot.gls<-gls(brot~Temp+Temp2+Rain,corr=cor)
model35.brot.gls<-gls(brot~Temp+Temp2+Rain1,corr=cor)
model36.brot.gls<-gls(brot~Temp+Temp2+RainH,corr=cor)
model37.brot.gls<-gls(brot~Temp+Temp2+Rain+Rain1,corr=cor)
model38.brot.gls<-gls(brot~DL+DL2+RainH+SIR,corr=cor)
model39.brot.gls<-gls(brot~DL+DL2+RainH+SIR1,corr=cor)
model40.brot.gls<-gls(brot~DL+DL2+RainH+SIR2,corr=cor)
model41.brot.gls<-gls(brot~DL+DL2+RainH+SIR+SIR1,corr=cor)
model42.brot.gls<-gls(brot~DL+DL2+RainH+SIR+SIR1+SIR2,corr=cor)
model43.brot.gls<-gls(brot~DL+DL2+RainH+SIR1+SIR2,corr=cor)


#=================#
# Model Selection #
#=================#

# Select the models
# Observe the AICc values which indicate the model with the best fit
# Separate the model in which delta <= 2

aicc.brot<-model.sel(model.brot.null,	model1.brot,	model2.brot,	model3.brot,
                     model4.brot,	model5.brot,	model6.brot,	model7.brot,
                     model8.brot,	model9.brot,	model10.brot,	model11.brot,
                     model12.brot,	model13.brot,	model14.brot,	model15.brot,
                     model16.brot,	model17.brot,	model18.brot,	model19.brot,
                     model20.brot,	model21.brot,	model22.brot,	model23.brot,
                     model24.brot,	model25.brot,	model26.brot,	model27.brot,
                     model28.brot,	model29.brot,	model30.brot,	model31.brot,
                     model32.brot,	model33.brot,	model34.brot,	model35.brot,
                     model36.brot,	model37.brot,	model38.brot, model39.brot,
                     model40.brot, model41.brot, model42.brot, model43.brot,
                     model.brot.gls.null,	model1.brot.gls,	model2.brot.gls,
                     model3.brot.gls,	model4.brot.gls,	model5.brot.gls,
                     model6.brot.gls,	model7.brot.gls,	model8.brot.gls,
                     model9.brot.gls,	model10.brot.gls,	model11.brot.gls,
                     model12.brot.gls,	model13.brot.gls,	model14.brot.gls,
                     model15.brot.gls,	model16.brot.gls,	model17.brot.gls,
                     model18.brot.gls,	model19.brot.gls,	model20.brot.gls,
                     model21.brot.gls,	model22.brot.gls,	model23.brot.gls,
                     model24.brot.gls,	model25.brot.gls,	model26.brot.gls,
                     model27.brot.gls,	model28.brot.gls,	model29.brot.gls,
                     model30.brot.gls,	model31.brot.gls,	model32.brot.gls,
                     model33.brot.gls,	model34.brot.gls,	model35.brot.gls,
                     model36.brot.gls,	model37.brot.gls, model38.brot.gls,
                     model39.brot.gls, model40.brot.gls, model41.brot.gls,
                     model42.brot.gls, model43.brot.gls)

aicc.brot

dim(aicc.brot)
sel.brot<-aicc.brot[,14:18]
sel.brot
sel.brot.delta<-subset(sel.brot,delta<=2)
sel.brot.delta

# Until here is enough for simple interpretation
# If more than one model have delta <= 2, it's necessary to apply the
#   the model average
# Take the hashes out if necessary

#sel_average<-model.avg()
#summary(sel_average)

# Take a look at:
#   the importance of each variable
#       *pay attention to the t-value, the strength and the math sign
#   the z value
#   p value


#================#
# Model Analyses #
#================#

# Take a look at:
#   the importance of each variable
#       *pay attention to the t-value, the strength and the math sign
#   p value
#   phi -> optimal correlation structure

summary(model42.brot.gls)

# Calculate the pseudo-r� of GLS
#   It's going to work as a correlation index

r2_gls<-r.squaredLR(model42.brot.gls)
r2_gls

  
    # Flower budding

attach(data)
botao

# Applying models

    # Linear models

model.botao.null<-lm(botao~1)
model1.botao<-lm(botao~DL)
model2.botao<-lm(botao~DL2)
model3.botao<-lm(botao~SIR)
model4.botao<-lm(botao~Temp3)
model5.botao<-lm(botao~DL+DL2)
model6.botao<-lm(botao~DL+Temp3)
model7.botao<-lm(botao~DL+DL2+Temp3)
model8.botao<-lm(botao~DL2+Temp3)
model9.botao<-lm(botao~SIR+Temp3)
model10.botao<-lm(botao~Temp)
model11.botao<-lm(botao~Temp2)
model12.botao<-lm(botao~Temp+Temp2)
model13.botao<-lm(botao~Temp+SIR)
model14.botao<-lm(botao~Temp+Temp3)
model15.botao<-lm(botao~Temp+Temp2+SIR)
model16.botao<-lm(botao~Temp2+SIR)
model17.botao<-lm(botao~DL+DL2+Rain)
model18.botao<-lm(botao~DL+DL2+Rain1)
model19.botao<-lm(botao~DL+DL2+Temp3+Rain)
model20.botao<-lm(botao~DL+DL2+Temp3+Rain1)
model21.botao<-lm(botao~DL+Temp3+Rain)
model22.botao<-lm(botao~DL+Temp3+Rain1)
model23.botao<-lm(botao~Temp+Temp2+Rain)
model24.botao<-lm(botao~Temp+Temp2+Rain1)
model25.botao<-lm(botao~Temp+Temp2+RainH)

    # GLS models

model.botao.gls.null<-gls(botao~1,corr=cor)
model1.botao.gls<-gls(botao~DL,corr=cor)
model2.botao.gls<-gls(botao~DL2,corr=cor)
model3.botao.gls<-gls(botao~SIR,corr=cor)
model4.botao.gls<-gls(botao~Temp3,corr=cor)
model5.botao.gls<-gls(botao~DL+DL2,corr=cor)
model6.botao.gls<-gls(botao~DL+Temp3,corr=cor)
model7.botao.gls<-gls(botao~DL+DL2+Temp3,corr=cor)
model8.botao.gls<-gls(botao~DL2+Temp3,corr=cor)
model9.botao.gls<-gls(botao~SIR+Temp3,corr=cor)
model10.botao.gls<-gls(botao~Temp,corr=cor)
model11.botao.gls<-gls(botao~Temp2,corr=cor)
model12.botao.gls<-gls(botao~Temp+Temp2,corr=cor)
model13.botao.gls<-gls(botao~Temp+SIR,corr=cor)
model14.botao.gls<-gls(botao~Temp+Temp3,corr=cor)
model15.botao.gls<-gls(botao~Temp+Temp2+SIR,corr=cor)
model16.botao.gls<-gls(botao~Temp2+SIR,corr=cor)
model17.botao.gls<-gls(botao~DL+DL2+Rain,corr=cor)
model18.botao.gls<-gls(botao~DL+DL2+Rain1,corr=cor)
model19.botao.gls<-gls(botao~DL+DL2+Temp3+Rain,corr=cor)
model20.botao.gls<-gls(botao~DL+DL2+Temp3+Rain1,corr=cor)
model21.botao.gls<-gls(botao~DL+Temp3+Rain,corr=cor)
model22.botao.gls<-gls(botao~DL+Temp3+Rain1,corr=cor)
model23.botao.gls<-gls(botao~Temp+Temp2+Rain,corr=cor)
model24.botao.gls<-gls(botao~Temp+Temp2+Rain1,corr=cor)
model25.botao.gls<-gls(botao~Temp+Temp2+RainH,corr=cor)


#=================#
# Model Selection #
#=================#

# Select the models
# Observe the AICc values which indicate the model with the best fit
# Separate the model in which delta <= 2

aicc.botao<-model.sel(model.botao.null,	model1.botao,	model2.botao,
                      model3.botao,	model4.botao,	model5.botao,	model6.botao,
                      model7.botao,	model8.botao,	model9.botao,	model10.botao,
                      model11.botao,	model12.botao,	model13.botao,
                      model14.botao,	model15.botao,	model16.botao,
                      model17.botao, model18.botao, model19.botao,
                      model20.botao, model21.botao, model22.botao,
                      model23.botao, model24.botao, model25.botao,
                      model.botao.gls.null,	model1.botao.gls,	model2.botao.gls,
                      model3.botao.gls,	model4.botao.gls,	model5.botao.gls,
                      model6.botao.gls,	model7.botao.gls,	model8.botao.gls,
                      model9.botao.gls,	model10.botao.gls,	model11.botao.gls,
                      model12.botao.gls,	model13.botao.gls,	model14.botao.gls,
                      model15.botao.gls,	model16.botao.gls, model17.botao.gls,
                      model18.botao.gls, model19.botao.gls, model20.botao.gls,
                      model21.botao.gls, model22.botao.gls, model23.botao.gls,
                      model24.botao.gls, model25.botao.gls)

aicc.botao

dim(aicc.botao)
sel.botao<-aicc.botao[,13:17]
sel.botao
sel.botao.delta<-subset(sel.botao,delta<=2)
sel.botao.delta

# Until here is enough for simple interpretation
# If more than one model have delta <= 2, it's necessary to apply the
#   the model average
# Take the hashes out if necessary

sel_average<-model.avg(model20.botao.gls,model19.botao.gls)
summary(sel_average)

# Take a look at:
#   the importance of each variable
#       *pay attention to the t-value, the strength and the math sign
#   the z value
#   p value


#================#
# Model Analyses #
#================#

# Take a look at:
#   the importance of each variable
#       *pay attention to the t-value, the strength and the math sign
#   p value
#   phi -> optimal correlation structure

summary(model20.botao.gls)

# Calculate the pseudo-r� of GLS
#   It's going to work as a correlation index

r2_gls<-r.squaredLR(model20.botao.gls)
r2_gls


    # Flowering

attach(data)
antese

# Applying models

# Linear models

model.antese.null<-lm(antese~1)
model1.antese<-lm(antese~DL)
model2.antese<-lm(antese~DL2)
model3.antese<-lm(antese~SIR)
model4.antese<-lm(antese~SIR1)
model5.antese<-lm(antese~SIR2)
model6.antese<-lm(antese~Temp3)
model7.antese<-lm(antese~DL+DL2)
model8.antese<-lm(antese~DL+Temp3)
model9.antese<-lm(antese~DL+DL2+Temp3)
model10.antese<-lm(antese~DL2+Temp3)
model11.antese<-lm(antese~DL2+SIR+Temp3)
model12.antese<-lm(antese~DL2+SIR2+Temp3)
model13.antese<-lm(antese~SIR+Temp3)
model14.antese<-lm(antese~SIR+SIR1+SIR2)
model15.antese<-lm(antese~SIR+SIR1+Temp3)
model16.antese<-lm(antese~SIR1+SIR2)
model17.antese<-lm(antese~DL+DL2+Rain)
model18.antese<-lm(antese~DL+DL2+Rain1)
model19.antese<-lm(antese~DL+Temp3+Rain)
model20.antese<-lm(antese~DL+Temp3+Rain1)
model21.antese<-lm(antese~DL+DL2+Temp3+Rain)
model22.antese<-lm(antese~DL+DL2+Temp3+Rain1)
model23.antese<-lm(antese~DL2+Temp3+Rain)
model24.antese<-lm(antese~DL2+Temp3+Rain1)
model25.antese<-lm(antese~DL2+SIR2+Temp3+Rain)
model26.antese<-lm(antese~DL2+SIR2+Temp3+Rain1)
model27.antese<-lm(antese~DL2+SIR2+Temp3+RainH)
model28.antese<-lm(antese~DL2+SIR+Temp3+Rain)
model29.antese<-lm(antese~DL2+SIR+Temp3+Rain1)
model30.antese<-lm(antese~DL2+SIR+Temp3+RainH)

# GLS models

model.antese.gls.null<-gls(antese~1,corr=cor)
model1.antese.gls<-gls(antese~DL,corr=cor)
model2.antese.gls<-gls(antese~DL2,corr=cor)
model3.antese.gls<-gls(antese~SIR,corr=cor)
model4.antese.gls<-gls(antese~SIR1,corr=cor)
model5.antese.gls<-gls(antese~SIR2,corr=cor)
model6.antese.gls<-gls(antese~Temp3,corr=cor)
model7.antese.gls<-gls(antese~DL+DL2,corr=cor)
model8.antese.gls<-gls(antese~DL+Temp3,corr=cor)
model9.antese.gls<-gls(antese~DL+DL2+Temp3,corr=cor)
model10.antese.gls<-gls(antese~DL2+Temp3,corr=cor)
model11.antese.gls<-gls(antese~DL2+SIR+Temp3,corr=cor)
model12.antese.gls<-gls(antese~DL2+SIR2+Temp3,corr=cor)
model13.antese.gls<-gls(antese~SIR+Temp3,corr=cor)
model14.antese.gls<-gls(antese~SIR+SIR1+SIR2,corr=cor)
model15.antese.gls<-gls(antese~SIR+SIR1+Temp3,corr=cor)
model16.antese.gls<-gls(antese~SIR1+SIR2,corr=cor)
model17.antese.gls<-gls(antese~DL+DL2+Rain,corr=cor)
model18.antese.gls<-gls(antese~DL+DL2+Rain1,corr=cor)
model19.antese.gls<-gls(antese~DL+Temp3+Rain,corr=cor)
model20.antese.gls<-gls(antese~DL+Temp3+Rain1,corr=cor)
model21.antese.gls<-gls(antese~DL+DL2+Temp3+Rain,corr=cor)
model22.antese.gls<-gls(antese~DL+DL2+Temp3+Rain1,corr=cor)
model23.antese.gls<-gls(antese~DL2+Temp3+Rain,corr=cor)
model24.antese.gls<-gls(antese~DL2+Temp3+Rain1,corr=cor)
model25.antese.gls<-gls(antese~DL2+SIR+Temp3+Rain,corr=cor)
model26.antese.gls<-gls(antese~DL2+SIR+Temp3+Rain1,corr=cor)
model27.antese.gls<-gls(antese~DL2+SIR+Temp3+RainH,corr=cor)
model28.antese.gls<-gls(antese~DL2+SIR2+Temp3+Rain,corr=cor)
model29.antese.gls<-gls(antese~DL2+SIR2+Temp3+Rain1,corr=cor)
model30.antese.gls<-gls(antese~DL2+SIR2+Temp3+RainH,corr=cor)


#=================#
# Model Selection #
#=================#

# Select the models
# Observe the AICc values which indicate the model with the best fit
# Separate the model in which delta <= 2

aicc.antese<-model.sel(model.antese.null,	model1.antese,	model2.antese,
                       model3.antese,	model4.antese,	model5.antese,
                       model6.antese,	model7.antese,	model8.antese,
                       model9.antese,	model10.antese,	model11.antese,
                       model12.antese,	model13.antese,	model14.antese,
                       model15.antese,	model16.antese, model17.antese,
                       model18.antese, model19.antese, model20.antese,
                       model21.antese, model22.antese, model23.antese,
                       model24.antese, model25.antese, model26.antese,
                       model27.antese, model28.antese, model29.antese,
                       model30.antese,	model.antese.gls.null,
                       model1.antese.gls,	model2.antese.gls,
                       model3.antese.gls,	model4.antese.gls,
                       model5.antese.gls,	model6.antese.gls,
                       model7.antese.gls,	model8.antese.gls,
                       model9.antese.gls,	model10.antese.gls,
                       model11.antese.gls,	model12.antese.gls,
                       model13.antese.gls,	model14.antese.gls,
                       model15.antese.gls,	model16.antese.gls,
                       model17.antese.gls, model18.antese.gls,
                       model19.antese.gls, model20.antese.gls,
                       model21.antese.gls, model22.antese.gls,
                       model23.antese.gls, model24.antese.gls,
                       model25.antese.gls, model26.antese.gls,
                       model27.antese.gls, model28.antese.gls,
                       model29.antese.gls, model30.antese.gls)

aicc.antese

dim(aicc.antese)
sel.antese<-aicc.antese[,13:17]
sel.antese
sel.antese.delta<-subset(sel.antese,delta<=2)
sel.antese.delta

# Until here is enough for simple interpretation
# If more than one model have delta <= 2, it's necessary to apply the
#   the model average
# Take the hashes out if necessary

sel_average<-model.avg(model21.antese.gls,model22.antese.gls,model9.antese.gls)
summary(sel_average)

# Take a look at:
#   the importance of each variable
#       *pay attention to the t-value, the strength and the math sign
#   the z value
#   p value


#================#
# Model Analyses #
#================#

# Take a look at:
#   the importance of each variable
#       *pay attention to the t-value, the strength and the math sign
#   p value
#   phi -> optimal correlation structure

summary(model9.antese.gls)

# Calculate the pseudo-r� of GLS
#   It's going to work as a correlation index

r2_gls<-r.squaredLR(model9.antese.gls)
r2_gls


    # Unripe fruits

attach(data)
frutoi

# Applying models

# Linear models

model.frutoi.null<-lm(frutoi~1)
model1.frutoi<-lm(frutoi~DL)
model2.frutoi<-lm(frutoi~DL2)
model3.frutoi<-lm(frutoi~SIR1)
model4.frutoi<-lm(frutoi~SIR2)
model5.frutoi<-lm(frutoi~SIR3)
model6.frutoi<-lm(frutoi~Temp3)
model7.frutoi<-lm(frutoi~DL+DL2)
model8.frutoi<-lm(frutoi~DL+Rain1)
model9.frutoi<-lm(frutoi~DL+Rain2)
model10.frutoi<-lm(frutoi~DL+Rain3)
model11.frutoi<-lm(frutoi~DL+Temp3)
model12.frutoi<-lm(frutoi~DL2+Rain1)
model13.frutoi<-lm(frutoi~DL2+Rain2)
model14.frutoi<-lm(frutoi~DL2+Rain3)
model15.frutoi<-lm(frutoi~DL2+Temp3)
model16.frutoi<-lm(frutoi~SIR1+SIR2)
model17.frutoi<-lm(frutoi~DL+DL2+Rain1)
model18.frutoi<-lm(frutoi~DL+DL2+Rain2)
model19.frutoi<-lm(frutoi~DL+DL2+Rain3)
model20.frutoi<-lm(frutoi~DL+DL2+Temp3)
model21.frutoi<-lm(frutoi~DL2+Rain1+Rain2)
model22.frutoi<-lm(frutoi~DL2+Rain1+Temp3)
model23.frutoi<-lm(frutoi~DL2+Rain2+Rain3)
model24.frutoi<-lm(frutoi~DL2+Rain2+Temp3)
model25.frutoi<-lm(frutoi~DL2+Rain3+Temp3)
model26.frutoi<-lm(frutoi~SIR1+SIR2+SIR3)
model27.frutoi<-lm(frutoi~Temp)
model28.frutoi<-lm(frutoi~Temp2)
model29.frutoi<-lm(frutoi~Temp+Temp2)
model30.frutoi<-lm(frutoi~Temp+Rain1)
model31.frutoi<-lm(frutoi~Temp+Rain2)
model32.frutoi<-lm(frutoi~Temp+Rain3)
model33.frutoi<-lm(frutoi~Temp2+Rain1)
model34.frutoi<-lm(frutoi~Temp2+Rain2)
model35.frutoi<-lm(frutoi~Temp2+Rain3)
model36.frutoi<-lm(frutoi~Temp+Temp2+Rain1)
model37.frutoi<-lm(frutoi~Temp+Temp2+Rain2)
model38.frutoi<-lm(frutoi~Temp+Temp2+Rain3)
model39.frutoi<-lm(frutoi~Temp+Rain1+Rain2)
model40.frutoi<-lm(frutoi~Temp+Rain2+Rain3)
model41.frutoi<-lm(frutoi~Temp2+Rain1+Rain2)
model42.frutoi<-lm(frutoi~Temp2+Rain2+Rain3)

# GLS models

model.frutoi.gls.null<-gls(frutoi~1,corr=cor)
model1.frutoi.gls<-gls(frutoi~DL,corr=cor)
model2.frutoi.gls<-gls(frutoi~DL2,corr=cor)
model3.frutoi.gls<-gls(frutoi~SIR1,corr=cor)
model4.frutoi.gls<-gls(frutoi~SIR2,corr=cor)
model5.frutoi.gls<-gls(frutoi~SIR3,corr=cor)
model6.frutoi.gls<-gls(frutoi~Temp3,corr=cor)
model7.frutoi.gls<-gls(frutoi~DL+DL2,corr=cor)
model8.frutoi.gls<-gls(frutoi~DL+Rain1,corr=cor)
model9.frutoi.gls<-gls(frutoi~DL+Rain2,corr=cor)
model10.frutoi.gls<-gls(frutoi~DL+Rain3,corr=cor)
model11.frutoi.gls<-gls(frutoi~DL+Temp3,corr=cor)
model12.frutoi.gls<-gls(frutoi~DL2+Rain1,corr=cor)
model13.frutoi.gls<-gls(frutoi~DL2+Rain2,corr=cor)
model14.frutoi.gls<-gls(frutoi~DL2+Rain3,corr=cor)
model15.frutoi.gls<-gls(frutoi~DL2+Temp3,corr=cor)
model16.frutoi.gls<-gls(frutoi~SIR1+SIR2,corr=cor)
model17.frutoi.gls<-gls(frutoi~DL+DL2+Rain1,corr=cor)
model18.frutoi.gls<-gls(frutoi~DL+DL2+Rain2,corr=cor)
model19.frutoi.gls<-gls(frutoi~DL+DL2+Rain3,corr=cor)
model20.frutoi.gls<-gls(frutoi~DL+DL2+Temp3,corr=cor)
model21.frutoi.gls<-gls(frutoi~DL2+Rain1+Rain2,corr=cor)
model22.frutoi.gls<-gls(frutoi~DL2+Rain1+Temp3,corr=cor)
model23.frutoi.gls<-gls(frutoi~DL2+Rain2+Rain3,corr=cor)
model24.frutoi.gls<-gls(frutoi~DL2+Rain2+Temp3,corr=cor)
model25.frutoi.gls<-gls(frutoi~DL2+Rain3+Temp3,corr=cor)
model26.frutoi.gls<-gls(frutoi~SIR1+SIR2+SIR3,corr=cor)
model27.frutoi.gls<-gls(frutoi~Temp,corr=cor)
model28.frutoi.gls<-gls(frutoi~Temp2,corr=cor)
model29.frutoi.gls<-gls(frutoi~Temp+Temp2,corr=cor)
model30.frutoi.gls<-gls(frutoi~Temp+Rain1,corr=cor)
model31.frutoi.gls<-gls(frutoi~Temp+Rain2,corr=cor)
model32.frutoi.gls<-gls(frutoi~Temp+Rain3,corr=cor)
model33.frutoi.gls<-gls(frutoi~Temp2+Rain1,corr=cor)
model34.frutoi.gls<-gls(frutoi~Temp2+Rain2,corr=cor)
model35.frutoi.gls<-gls(frutoi~Temp2+Rain3,corr=cor)
model36.frutoi.gls<-gls(frutoi~Temp+Temp2+Rain1,corr=cor)
model37.frutoi.gls<-gls(frutoi~Temp+Temp2+Rain2,corr=cor)
model38.frutoi.gls<-gls(frutoi~Temp+Temp2+Rain3,corr=cor)
model39.frutoi.gls<-gls(frutoi~Temp+Rain1+Rain2,corr=cor)
model40.frutoi.gls<-gls(frutoi~Temp+Rain2+Rain3,corr=cor)
model41.frutoi.gls<-gls(frutoi~Temp2+Rain1+Rain2,corr=cor)
model42.frutoi.gls<-gls(frutoi~Temp2+Rain2+Rain3,corr=cor)


#=================#
# Model Selection #
#=================#

# Select the models
# Observe the AICc values which indicate the model with the best fit
# Separate the model in which delta <= 2

aicc.frutoi<-model.sel(model.frutoi.null,	model1.frutoi,	model2.frutoi,
                       model3.frutoi,	model4.frutoi,	model5.frutoi,
                       model6.frutoi,	model7.frutoi,	model8.frutoi,
                       model9.frutoi,	model10.frutoi,	model11.frutoi,
                       model12.frutoi,	model13.frutoi,	model14.frutoi,
                       model15.frutoi,	model16.frutoi,	model17.frutoi,
                       model18.frutoi,	model19.frutoi,	model20.frutoi,
                       model21.frutoi,	model22.frutoi,	model23.frutoi,
                       model24.frutoi,	model25.frutoi,	model26.frutoi,
                       model27.frutoi,	model28.frutoi,	model29.frutoi,
                       model30.frutoi,	model31.frutoi,	model32.frutoi,
                       model33.frutoi,	model34.frutoi,	model35.frutoi,
                       model36.frutoi,	model37.frutoi,	model38.frutoi,
                       model39.frutoi,	model40.frutoi,	model41.frutoi,
                       model42.frutoi,	model.frutoi.gls.null,
                       model1.frutoi.gls,	model2.frutoi.gls,
                       model3.frutoi.gls,	model4.frutoi.gls,
                       model5.frutoi.gls,	model6.frutoi.gls,
                       model7.frutoi.gls,	model8.frutoi.gls,
                       model9.frutoi.gls,	model10.frutoi.gls,
                       model11.frutoi.gls,	model12.frutoi.gls,
                       model13.frutoi.gls,	model14.frutoi.gls,
                       model15.frutoi.gls,	model16.frutoi.gls,
                       model17.frutoi.gls,	model18.frutoi.gls,
                       model19.frutoi.gls,	model20.frutoi.gls,
                       model21.frutoi.gls,	model22.frutoi.gls,
                       model23.frutoi.gls,	model24.frutoi.gls,
                       model25.frutoi.gls,	model26.frutoi.gls,
                       model27.frutoi.gls,	model28.frutoi.gls,
                       model29.frutoi.gls,	model30.frutoi.gls,
                       model31.frutoi.gls,	model32.frutoi.gls,
                       model33.frutoi.gls,	model34.frutoi.gls,
                       model35.frutoi.gls,	model36.frutoi.gls,
                       model37.frutoi.gls,	model38.frutoi.gls,
                       model39.frutoi.gls,	model40.frutoi.gls,
                       model41.frutoi.gls,	model42.frutoi.gls)

aicc.frutoi

dim(aicc.frutoi)
sel.frutoi<-aicc.frutoi[,15:19]
sel.frutoi
sel.frutoi.delta<-subset(sel.frutoi,delta<=2)
sel.frutoi.delta


#================#
# Model Analyses #
#================#

# Take a look at:
#   the importance of each variable
#       *pay attention to the t-value, the strength and the math sign
#   p value
#   phi -> optimal correlation structure

summary(model20.frutoi.gls)

# Calculate the pseudo-r� of GLS
#   It's going to work as a correlation index

r2_gls<-r.squaredLR(model20.frutoi.gls)
r2_gls


    # Ripe fruits

attach(data)
frutom

# Applying models

# Linear models

model.frutom.null<-lm(frutom~1)
model1.frutom<-lm(frutom~DL)
model2.frutom<-lm(frutom~DL2)
model3.frutom<-lm(frutom~Temp3)
model4.frutom<-lm(frutom~DL+DL2)
model5.frutom<-lm(frutom~DL+Rain)
model6.frutom<-lm(frutom~DL+RainH)
model7.frutom<-lm(frutom~DL+Temp3)
model8.frutom<-lm(frutom~DL2+Rain)
model9.frutom<-lm(frutom~DL2+RainH)
model10.frutom<-lm(frutom~DL2+SIR)
model11.frutom<-lm(frutom~DL2+Temp3)
model12.frutom<-lm(frutom~Rain+Temp3)
model13.frutom<-lm(frutom~RainH+Temp3)
model14.frutom<-lm(frutom~SIR+Temp3)
model15.frutom<-lm(frutom~DL+DL2+Rain)
model16.frutom<-lm(frutom~DL+DL2+RainH)
model17.frutom<-lm(frutom~DL+DL2+Temp3)
model18.frutom<-lm(frutom~DL+Rain+Temp3)
model19.frutom<-lm(frutom~DL+RainH+Temp3)
model20.frutom<-lm(frutom~DL2+Rain+Temp3)
model21.frutom<-lm(frutom~DL2+RainH+Temp3)
model22.frutom<-lm(frutom~Temp)
model23.frutom<-lm(frutom~Temp2)
model24.frutom<-lm(frutom~Temp+Temp2)
model25.frutom<-lm(frutom~Temp+Rain)
model26.frutom<-lm(frutom~Temp+RainH)
model27.frutom<-lm(frutom~Temp+SIR)
model28.frutom<-lm(frutom~Temp2+Rain)
model29.frutom<-lm(frutom~Temp2+RainH)
model30.frutom<-lm(frutom~Temp2+SIR)
model31.frutom<-lm(frutom~Temp+Temp2+Rain)
model32.frutom<-lm(frutom~Temp+Temp2+RainH)
model33.frutom<-lm(frutom~Temp+Temp2+SIR)
model34.frutom<-lm(frutom~Temp+Rain+SIR)
model35.frutom<-lm(frutom~Temp+RainH+SIR)
model36.frutom<-lm(frutom~Temp+SIR+Temp3)
model37.frutom<-lm(frutom~Temp2+Rain+SIR)
model38.frutom<-lm(frutom~Temp2+RainH+SIR)

# GLS models

model.frutom.gls.null<-gls(frutom~1,corr=cor)
model1.frutom.gls<-gls(frutom~DL,corr=cor)
model2.frutom.gls<-gls(frutom~DL2,corr=cor)
model3.frutom.gls<-gls(frutom~Temp3,corr=cor)
model4.frutom.gls<-gls(frutom~DL+DL2,corr=cor)
model5.frutom.gls<-gls(frutom~DL+Rain,corr=cor)
model6.frutom.gls<-gls(frutom~DL+RainH,corr=cor)
model7.frutom.gls<-gls(frutom~DL+Temp3,corr=cor)
model8.frutom.gls<-gls(frutom~DL2+Rain,corr=cor)
model9.frutom.gls<-gls(frutom~DL2+RainH,corr=cor)
model10.frutom.gls<-gls(frutom~DL2+SIR,corr=cor)
model11.frutom.gls<-gls(frutom~DL2+Temp3,corr=cor)
model12.frutom.gls<-gls(frutom~Rain+Temp3,corr=cor)
model13.frutom.gls<-gls(frutom~RainH+Temp3,corr=cor)
model14.frutom.gls<-gls(frutom~SIR+Temp3,corr=cor)
model15.frutom.gls<-gls(frutom~DL+DL2+Rain,corr=cor)
model16.frutom.gls<-gls(frutom~DL+DL2+RainH,corr=cor)
model17.frutom.gls<-gls(frutom~DL+DL2+Temp3,corr=cor)
model18.frutom.gls<-gls(frutom~DL+Rain+Temp3,corr=cor)
model19.frutom.gls<-gls(frutom~DL+RainH+Temp3,corr=cor)
model20.frutom.gls<-gls(frutom~DL2+Rain+Temp3,corr=cor)
model21.frutom.gls<-gls(frutom~DL2+RainH+Temp3,corr=cor)
model22.frutom.gls<-gls(frutom~Temp,corr=cor)
model23.frutom.gls<-gls(frutom~Temp2,corr=cor)
model24.frutom.gls<-gls(frutom~Temp+Temp2,corr=cor)
model25.frutom.gls<-gls(frutom~Temp+Rain,corr=cor)
model26.frutom.gls<-gls(frutom~Temp+RainH,corr=cor)
model27.frutom.gls<-gls(frutom~Temp+SIR,corr=cor)
model28.frutom.gls<-gls(frutom~Temp2+Rain,corr=cor)
model29.frutom.gls<-gls(frutom~Temp2+RainH,corr=cor)
model30.frutom.gls<-gls(frutom~Temp2+SIR,corr=cor)
model31.frutom.gls<-gls(frutom~Temp+Temp2+Rain,corr=cor)
model32.frutom.gls<-gls(frutom~Temp+Temp2+RainH,corr=cor)
model33.frutom.gls<-gls(frutom~Temp+Temp2+SIR,corr=cor)
model34.frutom.gls<-gls(frutom~Temp+Rain+SIR,corr=cor)
model35.frutom.gls<-gls(frutom~Temp+RainH+SIR,corr=cor)
model36.frutom.gls<-gls(frutom~Temp+SIR+Temp3,corr=cor)
model37.frutom.gls<-gls(frutom~Temp2+Rain+SIR,corr=cor)
model38.frutom.gls<-gls(frutom~Temp2+RainH+SIR,corr=cor)


#=================#
# Model Selection #
#=================#

# Select the models
# Observe the AICc values which indicate the model with the best fit
# Separate the model in which delta <= 2

aicc.frutom<-model.sel(model.frutom.null,	model1.frutom,	model2.frutom,
                       model3.frutom,	model4.frutom,	model5.frutom,
                       model6.frutom,	model7.frutom,	model8.frutom,
                       model9.frutom,	model10.frutom,	model11.frutom,
                       model12.frutom,	model13.frutom,	model14.frutom,
                       model15.frutom,	model16.frutom,	model17.frutom,
                       model18.frutom,	model19.frutom,	model20.frutom,
                       model21.frutom,	model22.frutom,	model23.frutom,
                       model24.frutom,	model25.frutom,	model26.frutom,
                       model27.frutom,	model28.frutom,	model29.frutom,
                       model30.frutom,	model31.frutom,	model32.frutom,
                       model33.frutom,	model34.frutom,	model35.frutom,
                       model36.frutom,	model37.frutom,	model38.frutom,
                       model.frutom.gls.null,	model1.frutom.gls,
                       model2.frutom.gls,	model3.frutom.gls,
                       model4.frutom.gls,	model5.frutom.gls,
                       model6.frutom.gls,	model7.frutom.gls,
                       model8.frutom.gls,	model9.frutom.gls,
                       model10.frutom.gls,	model11.frutom.gls,
                       model12.frutom.gls,	model13.frutom.gls,
                       model14.frutom.gls,	model15.frutom.gls,
                       model16.frutom.gls,	model17.frutom.gls,
                       model18.frutom.gls,	model19.frutom.gls,
                       model20.frutom.gls,	model21.frutom.gls,
                       model22.frutom.gls,	model23.frutom.gls,
                       model24.frutom.gls,	model25.frutom.gls,
                       model26.frutom.gls,	model27.frutom.gls,
                       model28.frutom.gls,	model29.frutom.gls,
                       model30.frutom.gls,	model31.frutom.gls,
                       model32.frutom.gls,	model33.frutom.gls,
                       model34.frutom.gls,	model35.frutom.gls,
                       model36.frutom.gls,	model37.frutom.gls,
                       model38.frutom.gls)


aicc.frutom

dim(aicc.frutom)
sel.frutom<-aicc.frutom[,12:16]
sel.frutom
sel.frutom.delta<-subset(sel.frutom,delta<=2)
sel.frutom.delta


#================#
# Model Analyses #
#================#

# Take a look at:
#   the importance of each variable
#       *pay attention to the t-value, the strength and the math sign
#   p value
#   phi -> optimal correlation structure

summary(model17.frutom.gls)

# Calculate the pseudo-r� of GLS
#   It's going to work as a correlation index

r2_gls<-r.squaredLR(model17.frutom.gls)
r2_gls

#=============================================================================#
#=============================================================================#