#setwd and load the data
setwd ("YOUR DIRECTORY PATH")
df <- read.csv("hospit_precip.csv")
head(df)

# Load necessary packages
library(ggplot2)
library(dplyr)
library(lubridate)
library(scales)
library(splines)
library(dlnm)

# Convert dates to date objects
df$Date <- as.Date(df$Date, format = "%d.%m.%Y")
df$Month <- floor_date(df$Date, "month")
df$Year <- year(df$Date)

#calculate dow
df$dow <- weekdays(as.Date(df$Date))

#create cross-basis
#check lag time? especially for precipitation alone; is this correct?
#precipitation
cb_RhiresD <- crossbasis(df$RhiresD, lag = 0, argvar = list(fun = "lin"))
#pp.2
cb_pp.2 <- crossbasis(df$pp.2, lag=3, argvar=list(fun="integer"), arglag=list(fun="strata", breaks=1))
#pep90.2
cb_pep90.2 <- crossbasis(df$pep90.2, lag=3, argvar=list(fun="integer"), arglag=list(fun="strata", breaks=1))
#temp
cb_temp <- crossbasis(df$TabsD, lag=3, argvar=list(fun="lin"), arglag=list(fun="integer"))


#fit model(s)
#note: we use GLM here, as this is what worked (/was specified in the protocol?) however, Lee
#et al uses GNM - is there a different package for this/is this necessary?
#code below is pasted from Lee et al. for reference
#query what IND does & how to apply this to our study?
#model_pp.2 <- gnm(as.formula(paste0(i, " ~ cb_pp.2 + cb_temp + dow")), eliminate=stratum,
             #family=quasipoisson(), data=data, na.action="na.exclude",
             #subset=ind>0)

#model_pep90.2 <- gnm(as.formula(paste0(i, " ~ cb_pep90.2 + cb_temp + dow")), eliminate=stratum,
             #family=quasipoisson(), data=data, na.action="na.exclude",
             #subset=ind>0)

# now loop through all subgroups to apply the model to them 
# define subgroups: these are all, >65y, <65y, men, women
subgroups <- c("all", "ao64y", "a65plusy", "sex1", "sex2")
print(groups)

#MODEL 1: RAINFALL CONTINUOUS
#model with rainfall as continuous exposure
for(i in subgroups){
  
  formula_pop = as.formula(paste0(i, " ~ cb_RhiresD + dow + ns(Date, df = ", 6*9, ")"))
  
  # fit the model with the crossbasis object
  mod_RhiresD <- glm(formula_pop, data = df, family = quasipoisson)
  
  # predict
  pred_RhiresD <- crosspred(cb_RhiresD, mod_RhiresD, cen = 0)
  
  plot(pred_RhiresD, 
       "overall",
       xlab = "Rainfall exposure",
       ylab = "relative risk",
       main = paste0("Model 1: Precipitation", i)
  )
  abline(v = 50, lty = "dashed")
}

#MODEL2: PP.2 (>= 1mm over consecutive 2 days)
#note that values in this column are binary
#loop through subgroups again
for(i in subgroups){
  model_pp.2 <- glm(as.formula(paste0(i, " ~ cb_pp.2 + cb_temp + dow")),
                    family=quasipoisson(), data=df, na.action="na.exclude")
  
  #predict
  pred_pp.2 <- crosspred(cb_pp.2, model_pp.2, cen = 0)
  
  plot(pred_pp.2, 
       "overall",
       xlab = "Rainfall exposure",
       ylab = "relative risk",
       main = paste0("Model 2: PP.2", i)
  )
  abline(v = 50, lty = "dashed")
  
}


#MODEL 3: PEP90.2 (>= 90th percentile over consecutive 2 days)
#note that values in this column are binary
for(i in subgroup){
  
  formula_pop = as.formula(paste0(i, " ~ cb_pep90.2 + dow + ns(Date, df = ", 6*9, ")"))
  
  # fit the model with the crossbasis object
  model_pep90.2 <- glm(formula_pop, data = df, family = quasipoisson)
  
  # predict
  pred_pep90.2 <- crosspred(cb_pep90.2, mod2, cen = 0)
  
  plot(pred_90.2, 
       "overall",
       xlab = "Rainfall exposure",
       ylab = "relative risk",
       main = paste0("Model 3: (PEP90.2): ", i)
  )
  abline(v = 50, lty = "dashed")
}
}