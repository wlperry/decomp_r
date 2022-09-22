# LIBRARIES ----
library(tidyverse)
library(janitor)
library(purrr)
library(broom)
library(car)
library(emmeans)
library(patchwork)

# # READ IN THE DATA ----
# 
# # decomp
# decomp.df <- read_csv("data/decomp_biomass.csv") %>% clean_names()
# 
# # n data 
# n_data.df <- read.csv("Data/Decomp/n with data.csv") %>%
#   clean_names() %>%
#   remove_empty(which = c("cols", "rows"))
# 
# 
# # CLEAN DATA TO JOIN ----
# 
# # rename and make factors
# decomp.df <- decomp.df %>%
#   rename(sample = bag_no) %>%
#   mutate(sample = as.factor(sample))
# 
# # make factor
# n_data.df <- n_data.df %>%
#   mutate(sample = as.factor(sample))
# 
# # JOIN FILES ----
# 
# # full join 
# full.df <- full_join(decomp.df, n_data.df, by = "sample")
# 
# # CLEAN FULL DATAFRAME ----
# 
# # remove na from data we didn't send 
# full.df <- full.df %>%
#   na.omit()
# 
# # make row a number
# full.df <- full.df %>% 
#   mutate(row = case_when(
#     row == "A" ~ 1,
#     row == "B" ~ 2,
#     row == "C" ~ 3,
#     row == "D" ~ 4,
#     row == "E" ~ 5,
#     row == "F" ~ 6,
#     row == "G" ~ 7,
#     row == "H" ~ 8,
#     row == "I" ~ 9,
#     row == "J" ~ 10,
#     TRUE ~ 99999
#   ) )
# 
# # C IN BIOMASS ----
# 
# # first need day 0 porportion of n * initial wt for how much n in initial weight
# # calculate proportion n 
# full.df <- full.df %>% 
#   mutate(prop_c = percent_c / 100)
# 
# # now we need day 0 and by species 
# initial_prop_c.df <- full.df %>%
#   filter(days == 0) %>%
#   group_by(spp) %>%
#   summarize(initial_prop_c = mean(prop_c))
# 
# # we then multiply each sample by the initial prop n to get the initial n for every sample
# full.df <- full.df %>%
#   mutate(initial_prop_c = case_when(
#     spp == "PC" ~ initial_wt_g * 0.4549000,
#     spp == "GM_PC" ~ initial_wt_g * 0.4313636,
#     spp == "AR" ~ initial_wt_g * 0.4131000,
#     spp == "CR" ~ initial_wt_g * 0.4298000
#   ))
# 
# # now we take collected weight and multiply by the collected prop n to get collected n
# full.df <- full.df %>%
#   mutate(collected_prop_c = coll_wt_g * prop_c)
# 
# # finally divide initial by collected to get prop n remaining then * 100 for pct
# full.df <- full.df %>%
#   mutate(prop_c_remain = collected_prop_c / initial_prop_c) %>%
#   mutate(pct_c_remaing = prop_c_remain * 100)
# 
# # graph it to see how it looks
# full.df %>%
#   ggplot(mapping = aes(days, pct_c_remaing, color = spp)) +
#   stat_summary(fun = mean, na.rm = TRUE, geom = "point") +
#   stat_summary(fun.data = mean_se, na.rm = TRUE, geom = "line") +
#   theme_classic()
# 
# # gonna have to get rid of those weird days that got sampled
# unique(full.df$days)
# 
# full.df <- full.df %>%
#   filter(days == 0 | days == 14 | days == 35 | days == 63)
# 
# # plot it again to see if I removed the correct days, yep looks good
# full.df %>%
#   ggplot(mapping = aes(days, pct_c_remaing, color = spp)) +
#   stat_summary(fun = mean, na.rm = TRUE, geom = "point") +
#   stat_summary(fun.data = mean_se, na.rm = TRUE, geom = "line") +
#   theme_classic()

# LIBRARIES ----
library(tidyverse)
library(janitor)
library(purrr)
library(broom)
library(car)
library(emmeans)
library(patchwork)

# READ IN THE DATA ----
full.df <- read_csv("data/cleaned/clean carbon.csv")

# NONLINEAR MODELS ----

# pc ----
pc <- full.df %>% 
  filter(spp=="PC")  %>% 
  select(row, pct_c_remaing, days ) %>% 
  rename(
    rep = row,
    Mt = pct_c_remaing,
    t = days
  ) %>% 
  mutate(Mt = Mt / 100) %>%
  filter(rep != 99999)

# okay, we are missing reps 3 and 5 from t = 63 in this dataframe, 
# we first need to find the average Mt from t = 63 
average.df <- pc %>%
  filter(t == 63) %>%
  group_by(t) %>%
  summarize(average = mean(Mt))

# found the value I need so I use add row to work it into the data frame and get a full set

pc <- pc %>%
  add_row(rep = 3, Mt = 0.5965988, t = 63) %>%
  add_row(rep = 5, Mt = 0.5965988, t = 63)

# so this works!!!
nonlin = nls(Mt ~ 1*exp(-k*t), trace=TRUE, start = list(k = .01), data=pc)
# nonlin regression stuff

rm(nonlin)

#create a vector to store k values estimated for each set of measurements with the same "rep" number:
k=numeric(max(pc$rep)) 	
# k=numeric(10)
#	vector length = number of k values that will be generated.
#	Here, "rep" = the observations that will be included in one 
#	nonlinear model fitting i.e., used to generate one k estimate


#create an array to store predicted mass remaining values:
pred=array(0, dim=c(4, max(pc$rep)))	
#	array(all values = 0,dim=c(no. of rows=number of time steps, columns=number of reps)

#create an array to store the model residuals:
resid=array(0, dim=c(4, max(pc$rep)))	
#	as above

#create an array to store the observed values for making graphs:
obs=array(0, dim=c(4, max(pc$rep)))	

#	  NONLINEAR REGRESSION          #
# estimate k for each replicate (rep) #
#the ?for? loop below subsets the data by replicate (rep) number (rep = 1 to 5), 
#	in order to estimate k nonlinearly for each 
#	replicate (rep).

for(i in 1:max(pc$rep)){
  s1=subset(pc,pc$rep==i)   
  #creates an array "s1" that is a subset of the entire data 
  #set "deco" that only includes rows with the same rep 
  #number, i.e., includes all the data needed to estimate 
  #1 k value.
  #define model independent variable as the column with time (t) data:
  t=s1$t				
  #define the model dependent variable as the column with measured proportion mass remaining (Mt) data:
  Mt=s1$Mt			
  #nonlinear least squares regression (nls) command:
  nonlin = nls(Mt ~ 1 * exp(-k*t), trace=TRUE, start = list(k = .01))
  #nls command explanation:
  #	name to save results under = nls(dependent variable ~ model, 
  #	If trace=TRUE the residual sum-of-squares and the parameter values are 
  #	printed at the conclusion of each iteration, list of initial values)
  
  #command to look at summary of nls regression:
  summary(nonlin)			
  #put predicted values in vector "pr":
  pr=predict(nonlin)		
  
  #put residuals in vector "res":
  res=residuals(nonlin)		
  
  #extract k value from regression "nonlin" and store in previously created vector "k":
  k[i]=coef(nonlin)[1]
  
  #put predicted and residual values in array:
  pred[,i]=pr
  resid[,i]=res
  
  #put observations in array "obs" for plotting later:
  obs[,i]=Mt	
  
}	#end of nls ?for? loop

# #copy vector with k values to clipboard to paste into excel or 
# #	other program of choice. This command raises clipboard 
# #	memory in 32 KB steps; default = 32 KB
# #	To increase memory to 64, replace "clipboard" with 
# #	"clipboard-64":
# write.table(k,"clipboard",sep="\t",row.names=FALSE,col.names=FALSE)
# 
# #copy predicted and residual arrays to clipboard:
# write.table(pred,"clipboard",sep="\t",row.names=FALSE,col.names=FALSE)
# write.table(resid,"clipboard",sep="\t",row.names=FALSE,col.names=FALSE)

#plot predicted values vs residuals for one rep:
plot(pred[,1],resid[,1],xlab="Predictions",ylab="Residuals")
#plot command explanation:
#plot(x data, ydata, x axis label, y axis label)

#plot predictions of Mt from one rep vs time
plot(s1$t,pred[,3],xlab="Days",ylab="Predictions")

#add observed measurements to plot as points
points(s1$t,obs[,1],col="red") 
#points command explanation:
#points(x data, ydata, point color = red)


write_csv(as.data.frame(k), file="output/pc_c_k nonlin values.csv")

pc_c_k_nonlin.df <- as.data.frame(k)

# gm ----
gm <- full.df %>% 
  filter(spp=="GM_PC")  %>% 
  select(row, pct_c_remaing, days ) %>% 
  rename(
    rep = row,
    Mt = pct_c_remaing,
    t = days
  ) %>% 
  mutate(Mt = Mt / 100) %>%
  filter(rep != 99999)


# start by removing the extra 10s
# have to do something silly because filter will not find the values in those rep 10's
working.df <- gm %>%
  filter(t == 0)

working.df <- working.df %>%
  filter(rep != 10)

gm <- gm %>%
  filter(t != 0)

gm <- bind_rows(gm, working.df)

# 3 missing values are t=0 rep=10, t=63, rep=3, 5
# found the value I need so I use add row to work it into the data frame and get a full set

# need the averages first to make the replacements 
average.df <- gm %>%
  filter(t == 63 |t == 0) %>%
  group_by(t) %>%
  summarize(average = mean(Mt))

gm <- gm %>%
  add_row(rep = 3, Mt = 0.7284911, t = 63) %>%
  add_row(rep = 5, Mt = 0.7284911, t = 63) %>%
  add_row(rep = 10, Mt = 1.0305370, t = 0)


# so this works!!!
nonlin = nls(Mt ~ 1*exp(-k*t), trace=TRUE, start = list(k = .01), data=gm)
# nonlin regression stuff

rm(nonlin)

#create a vector to store k values estimated for each set of measurements with the same "rep" number:
k=numeric(max(gm$rep)) 	
# k=numeric(10)
#	vector length = number of k values that will be generated.
#	Here, "rep" = the observations that will be included in one 
#	nonlinear model fitting i.e., used to generate one k estimate


#create an array to store predicted mass remaining values:
pred=array(0, dim=c(4, max(gm$rep)))	
#	array(all values = 0,dim=c(no. of rows=number of time steps, columns=number of reps)

#create an array to store the model residuals:
resid=array(0, dim=c(4, max(gm$rep)))	
#	as above

#create an array to store the observed values for making graphs:
obs=array(0, dim=c(4, max(gm$rep)))	

#	  NONLINEAR REGRESSION          #
# estimate k for each replicate (rep) #
#the ?for? loop below subsets the data by replicate (rep) number (rep = 1 to 5), 
#	in order to estimate k nonlinearly for each 
#	replicate (rep).

for(i in 1:max(gm$rep)){
  s1=subset(gm,gm$rep==i)   
  #creates an array "s1" that is a subset of the entire data 
  #set "deco" that only includes rows with the same rep 
  #number, i.e., includes all the data needed to estimate 
  #1 k value.
  #define model independent variable as the column with time (t) data:
  t=s1$t				
  #define the model dependent variable as the column with measured proportion mass remaining (Mt) data:
  Mt=s1$Mt			
  #nonlinear least squares regression (nls) command:
  nonlin = nls(Mt ~ 1 * exp(-k*t), trace=TRUE, start = list(k = .01))
  #nls command explanation:
  #	name to save results under = nls(dependent variable ~ model, 
  #	If trace=TRUE the residual sum-of-squares and the parameter values are 
  #	printed at the conclusion of each iteration, list of initial values)
  
  #command to look at summary of nls regression:
  summary(nonlin)			
  #put predicted values in vector "pr":
  pr=predict(nonlin)		
  
  #put residuals in vector "res":
  res=residuals(nonlin)		
  
  #extract k value from regression "nonlin" and store in previously created vector "k":
  k[i]=coef(nonlin)[1]
  
  #put predicted and residual values in array:
  pred[,i]=pr
  resid[,i]=res
  
  #put observations in array "obs" for plotting later:
  obs[,i]=Mt	
  
}	#end of nls ?for? loop

# #copy vector with k values to clipboard to paste into excel or 
# #	other program of choice. This command raises clipboard 
# #	memory in 32 KB steps; default = 32 KB
# #	To increase memory to 64, replace "clipboard" with 
# #	"clipboard-64":
# write.table(k,"clipboard",sep="\t",row.names=FALSE,col.names=FALSE)
# 
# #copy predicted and residual arrays to clipboard:
# write.table(pred,"clipboard",sep="\t",row.names=FALSE,col.names=FALSE)
# write.table(resid,"clipboard",sep="\t",row.names=FALSE,col.names=FALSE)

#plot predicted values vs residuals for one rep:
plot(pred[,1],resid[,1],xlab="Predictions",ylab="Residuals")
#plot command explanation:
#plot(x data, ydata, x axis label, y axis label)

#plot predictions of Mt from one rep vs time
plot(s1$t,pred[,3],xlab="Days",ylab="Predictions")

#add observed measurements to plot as points
points(s1$t,obs[,1],col="red") 
#points command explanation:
#points(x data, ydata, point color = red)


write_csv(as.data.frame(k), file="output/gm_c k nonlin values.csv")

gm_c_k_nonlin.df <- as.data.frame(k)

# ar ----
ar <- full.df %>% 
  filter(spp=="AR")  %>% 
  select(row, pct_c_remaing, days ) %>% 
  rename(
    rep = row,
    Mt = pct_c_remaing,
    t = days
  ) %>% 
  mutate(Mt = Mt / 100) %>%
  filter(rep != 99999)

# missing t35 rep6, t63 rep3 | 5

# need the averages first to make the replacements 
average.df <- ar %>%
  filter(t == 63 |t == 35) %>%
  group_by(t) %>%
  summarize(average = mean(Mt))

ar <- ar %>%
  add_row(rep = 6, Mt = 0.4785847, t = 35) %>%
  add_row(rep = 3, Mt = 0.4158301, t = 63) %>%
  add_row(rep = 5, Mt = 0.4158301, t = 63)


# so this works!!!
nonlin = nls(Mt ~ 1*exp(-k*t), trace=TRUE, start = list(k = .01), data=ar)
# nonlin regression stuff

rm(nonlin)

#create a vector to store k values estimated for each set of measurements with the same "rep" number:
k=numeric(max(ar$rep)) 	
# k=numeric(10)
#	vector length = number of k values that will be generated.
#	Here, "rep" = the observations that will be included in one 
#	nonlinear model fitting i.e., used to generate one k estimate


#create an array to store predicted mass remaining values:
pred=array(0, dim=c(4, max(ar$rep)))	
#	array(all values = 0,dim=c(no. of rows=number of time steps, columns=number of reps)

#create an array to store the model residuals:
resid=array(0, dim=c(4, max(ar$rep)))	
#	as above

#create an array to store the observed values for making graphs:
obs=array(0, dim=c(4, max(ar$rep)))	

#	  NONLINEAR REGRESSION          #
# estimate k for each replicate (rep) #
#the ?for? loop below subsets the data by replicate (rep) number (rep = 1 to 5), 
#	in order to estimate k nonlinearly for each 
#	replicate (rep).

for(i in 1:max(ar$rep)){
  s1=subset(ar,ar$rep==i)   
  #creates an array "s1" that is a subset of the entire data 
  #set "deco" that only includes rows with the same rep 
  #number, i.e., includes all the data needed to estimate 
  #1 k value.
  #define model independent variable as the column with time (t) data:
  t=s1$t				
  #define the model dependent variable as the column with measured proportion mass remaining (Mt) data:
  Mt=s1$Mt			
  #nonlinear least squares regression (nls) command:
  nonlin = nls(Mt ~ 1 * exp(-k*t), trace=TRUE, start = list(k = .01))
  #nls command explanation:
  #	name to save results under = nls(dependent variable ~ model, 
  #	If trace=TRUE the residual sum-of-squares and the parameter values are 
  #	printed at the conclusion of each iteration, list of initial values)
  
  #command to look at summary of nls regression:
  summary(nonlin)			
  #put predicted values in vector "pr":
  pr=predict(nonlin)		
  
  #put residuals in vector "res":
  res=residuals(nonlin)		
  
  #extract k value from regression "nonlin" and store in previously created vector "k":
  k[i]=coef(nonlin)[1]
  
  #put predicted and residual values in array:
  pred[,i]=pr
  resid[,i]=res
  
  #put observations in array "obs" for plotting later:
  obs[,i]=Mt	
  
}	#end of nls ?for? loop

# #copy vector with k values to clipboard to paste into excel or 
# #	other program of choice. This command raises clipboard 
# #	memory in 32 KB steps; default = 32 KB
# #	To increase memory to 64, replace "clipboard" with 
# #	"clipboard-64":
# write.table(k,"clipboard",sep="\t",row.names=FALSE,col.names=FALSE)
# 
# #copy predicted and residual arrays to clipboard:
# write.table(pred,"clipboard",sep="\t",row.names=FALSE,col.names=FALSE)
# write.table(resid,"clipboard",sep="\t",row.names=FALSE,col.names=FALSE)

#plot predicted values vs residuals for one rep:
plot(pred[,1],resid[,1],xlab="Predictions",ylab="Residuals")
#plot command explanation:
#plot(x data, ydata, x axis label, y axis label)

#plot predictions of Mt from one rep vs time
plot(s1$t,pred[,3],xlab="Days",ylab="Predictions")

#add observed measurements to plot as points
points(s1$t,obs[,1],col="red") 
#points command explanation:
#points(x data, ydata, point color = red)


write_csv(as.data.frame(k), file="output/ar_c k nonlin values.csv")

ar_c_k_nonlin.df <- as.data.frame(k)

# cr ----
cr <- full.df %>% 
  filter(spp=="CR")  %>% 
  select(row, pct_c_remaing, days ) %>% 
  rename(
    rep = row,
    Mt = pct_c_remaing,
    t = days
  ) %>% 
  mutate(Mt = Mt / 100) %>%
  filter(rep != 99999)

# missing t63 rep 3 | 5 | 6

# need the averages first to make the replacements 
average.df <- cr %>%
  filter(t == 63) %>%
  group_by(t) %>%
  summarize(average = mean(Mt))

cr <- cr %>%
  add_row(rep = 3, Mt = 0.476429, t = 63) %>%
  add_row(rep = 5, Mt = 0.476429, t = 63) %>%
  add_row(rep = 6, Mt = 0.476429, t = 63)


# so this works!!!
nonlin = nls(Mt ~ 1*exp(-k*t), trace=TRUE, start = list(k = .01), data=cr)
# nonlin regression stuff

rm(nonlin)

#create a vector to store k values estimated for each set of measurements with the same "rep" number:
k=numeric(max(cr$rep)) 	
# k=numeric(10)
#	vector length = number of k values that will be generated.
#	Here, "rep" = the observations that will be included in one 
#	nonlinear model fitting i.e., used to generate one k estimate


#create an array to store predicted mass remaining values:
pred=array(0, dim=c(4, max(cr$rep)))	
#	array(all values = 0,dim=c(no. of rows=number of time steps, columns=number of reps)

#create an array to store the model residuals:
resid=array(0, dim=c(4, max(cr$rep)))	
#	as above

#create an array to store the observed values for making graphs:
obs=array(0, dim=c(4, max(cr$rep)))	

#	  NONLINEAR REGRESSION          #
# estimate k for each replicate (rep) #
#the ?for? loop below subsets the data by replicate (rep) number (rep = 1 to 5), 
#	in order to estimate k nonlinearly for each 
#	replicate (rep).

for(i in 1:max(cr$rep)){
  s1=subset(cr,cr$rep==i)   
  #creates an array "s1" that is a subset of the entire data 
  #set "deco" that only includes rows with the same rep 
  #number, i.e., includes all the data needed to estimate 
  #1 k value.
  #define model independent variable as the column with time (t) data:
  t=s1$t				
  #define the model dependent variable as the column with measured proportion mass remaining (Mt) data:
  Mt=s1$Mt			
  #nonlinear least squares regression (nls) command:
  nonlin = nls(Mt ~ 1 * exp(-k*t), trace=TRUE, start = list(k = .01))
  #nls command explanation:
  #	name to save results under = nls(dependent variable ~ model, 
  #	If trace=TRUE the residual sum-of-squares and the parameter values are 
  #	printed at the conclusion of each iteration, list of initial values)
  
  #command to look at summary of nls regression:
  summary(nonlin)			
  #put predicted values in vector "pr":
  pr=predict(nonlin)		
  
  #put residuals in vector "res":
  res=residuals(nonlin)		
  
  #extract k value from regression "nonlin" and store in previously created vector "k":
  k[i]=coef(nonlin)[1]
  
  #put predicted and residual values in array:
  pred[,i]=pr
  resid[,i]=res
  
  #put observations in array "obs" for plotting later:
  obs[,i]=Mt	
  
}	#end of nls ?for? loop

# #copy vector with k values to clipboard to paste into excel or 
# #	other program of choice. This command raises clipboard 
# #	memory in 32 KB steps; default = 32 KB
# #	To increase memory to 64, replace "clipboard" with 
# #	"clipboard-64":
# write.table(k,"clipboard",sep="\t",row.names=FALSE,col.names=FALSE)
# 
# #copy predicted and residual arrays to clipboard:
# write.table(pred,"clipboard",sep="\t",row.names=FALSE,col.names=FALSE)
# write.table(resid,"clipboard",sep="\t",row.names=FALSE,col.names=FALSE)

#plot predicted values vs residuals for one rep:
plot(pred[,1],resid[,1],xlab="Predictions",ylab="Residuals")
#plot command explanation:
#plot(x data, ydata, x axis label, y axis label)

#plot predictions of Mt from one rep vs time
plot(s1$t,pred[,3],xlab="Days",ylab="Predictions")

#add observed measurements to plot as points
points(s1$t,obs[,1],col="red") 
#points command explanation:
#points(x data, ydata, point color = red)


write_csv(as.data.frame(k), file="output/cr_c k nonlin values.csv")

cr_c_k_nonlin.df <- as.data.frame(k)

# clean files ----

cr_c_k_nonlin.df <- cr_c_k_nonlin.df %>% 
  mutate(spp="cr")

ar_c_k_nonlin.df <- ar_c_k_nonlin.df %>% 
  mutate(spp="ar")

pc_c_k_nonlin.df <- pc_c_k_nonlin.df %>% 
  mutate(spp="pc")

gm_c_k_nonlin.df <- gm_c_k_nonlin.df %>% 
  mutate(spp="gm_pc")

# put all of the k values together ----
nonlin_c_k.df <- bind_rows(pc_c_k_nonlin.df,gm_c_k_nonlin.df, 
                           ar_c_k_nonlin.df,cr_c_k_nonlin.df)

# clean joined file ----

nonlin_c_k.df <- nonlin_c_k.df %>% 
  mutate(row_no = rep(1:10, length.out = n())) %>%
  mutate(soil_block = rep(seq_along(1:2), each=5, length.out = n())) 


nonlin_c_k.df <- nonlin_c_k.df %>% 
  mutate(soil_block = as.factor(soil_block))

# PLOTTING K VALUES ----

# plot raw values ----
nonlin_c_k.df %>%
  ggplot(mapping = aes(spp, k)) +
  geom_point() +
  theme_classic()

# mean se plot ----
nonlin_c_k.df %>%
  ggplot(mapping = aes(spp, k, color = soil_block)) +
  stat_summary(fun.data = mean_se, na.rm = TRUE, geom = "errorbar", width = 0.2, 
               position = position_dodge2(.2)) +
  stat_summary(fun = mean, na.rm = TRUE, geom = "point", size = 4,
               position = position_dodge2(.2)) +
  theme_classic()

# pct remaining by days 
full.df %>%
  ggplot(mapping = aes(days, pct_c_remaing, color = spp)) +
  stat_summary(fun.data = mean_se, na.rm = TRUE, geom = "errorbar", width = 0.2, 
               position = position_dodge2(.2)) +
  stat_summary(fun = mean, na.rm = TRUE, geom = "point", size = 4,
               position = position_dodge2(.2)) +
  theme_classic()

# STATISTICAL ANALYSIS ----

# create model ----
one.lm = lm(k ~ spp * soil_block, data = nonlin_c_k.df)

# check assumptions of model ----
residuals <- resid(one.lm)
plot(fitted(one.lm), residuals)
qqnorm(residuals)
qqline(residuals)

# homogeneity of variance looks good, normality could be better but its fine

# run model ----
Anova(one.lm, type = 3)

# RESULTS OF ANOVA ----

# reject Ho: the mean difference in decomposition rate between all groups is not equal to 0 
# no significant interaction between spp and soil block p = 0.32
# no significant interaction of soil block p = 0.40
# significant effect of species p < 0.0001

# POST F TESTS ----

# post f tests to determine where the differences between groups are 

# create emmeans model ----
one.emm <- emmeans(one.lm, ~ spp)

# plot emmeans ----
plot(one.emm, comparisons = TRUE)

# mean separation ----
# where are the differences in groups and what are the emmeans?
multcomp::cld(one.emm, Letters = letters, adjust = "Bonferroni")

# p-values ----
emminteraction = emmeans(one.emm, pairwise ~ spp, adjust = "bonferroni", alpha = 0.5)
emminteraction$contrasts

# PLOTTING ----

# final plot, k by spp with letters ----

# save as a data frame for plotting ----
emmeans.df <- as.data.frame(multcomp::cld(one.emm, Letters = letters, adjust = "Bonferroni"))
emmeans.df

# relevel factors ----
emmeans.df <- emmeans.df %>%
  mutate(spp = as.factor(spp)) 

# final plot ----
emmeans.df %>%
  mutate(spp = fct_relevel(spp, "pc", "gm_pc", "cr", "ar")) %>%
  ggplot(aes(x=spp)) +
  geom_point(aes(y=emmean), size=3) +
  geom_errorbar(aes(ymin = emmean-SE, ymax = emmean+SE), 
                stat="identity", width = 0.2) +
  labs(x="Species", y= "k (% nitrogen loss per day)")  +
  geom_text(aes(x = 1, y = .02, label = "A")) +
  geom_text(aes(x = 2, y = .02, label = "B")) +
  geom_text(aes(x = 3, y = .02, label = "C"))+
  geom_text(aes(x = 4, y = .02, label = "D")) +
  scale_x_discrete(labels = c("pc" = "Pennycress", "gm_pc"= "AOP2 Pennycress",
                              "cr" = "Cereal Rye", "ar" = "Annual Rye")) +
  theme_classic()

# CALCULATING N REMAINING FOR OUTPUT BC THIS IS A CONVIENIENT PLACE TO PUT IT ----
full.df <- full.df %>% 
  mutate(prop_n = percent_n / 100)

# now we need day 0 and by species 
initial_prop_n.df <- full.df %>%
  filter(days == 0) %>%
  group_by(spp) %>%
  summarize(initial_prop_n = mean(prop_n))

# we then multiply each sample by the initial prop n to get the initial n for every sample
full.df <- full.df %>%
  mutate(initial_prop_n = case_when(
    spp == "PC" ~ initial_wt_g * 0.01872000,
    spp == "GM_PC" ~ initial_wt_g * 0.01595455,
    spp == "AR" ~ initial_wt_g * 0.01711000,
    spp == "CR" ~ initial_wt_g * 0.01439000
  ))

# now we take collected weight and multiply by the collected prop n to get collected n
full.df <- full.df %>%
  mutate(collected_prop_n = coll_wt_g * prop_n)

# finally divide initial by collected to get prop n remaining then * 100 for pct
full.df <- full.df %>%
  mutate(prop_n_remain = collected_prop_n / initial_prop_n) %>%
  mutate(pct_n_remain = prop_n_remain * 100)

# graph it to see how it looks
full.df %>%
  ggplot(mapping = aes(days, pct_n_remain, color = spp)) +
  stat_summary(fun = mean, na.rm = TRUE, geom = "point") +
  stat_summary(fun.data = mean_se, na.rm = TRUE, geom = "line") +
  theme_classic()

# some averages ----
average.df <- full.df %>%
  group_by(spp, days) %>%
  filter(days == 63) %>%
  summarize(remain = mean(pct_c_remaing),
            sd = sd(pct_c_remaing))

# SAVE FILES FOR OUTPUT AND PLOTTING ----

# emmeans for carbon data ----
write_csv(emmeans.df, file = "output/final/carbon_k_emmeans.csv")

# full dataset with both pct n and c data remaining ----
write_csv(full.df, file = "output/final/full_data.csv")




