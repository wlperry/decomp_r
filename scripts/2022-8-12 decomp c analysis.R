# LIBRARIES ----
library(tidyverse)
library(janitor)
library(purrr)
library(broom)
library(car)
library(emmeans)
library(patchwork)

# READ IN THE DATA ----

# decomp
decomp.df <- read_csv("data/decomp_biomass.csv") %>% clean_names()

# n data 
n_data.df <- read.csv("Data/Decomp/n with data.csv") %>%
  clean_names() %>%
  remove_empty(which = c("cols", "rows"))


# CLEAN DATA TO JOIN ----

# rename and make factors
decomp.df <- decomp.df %>%
  rename(sample = bag_no) %>%
  mutate(sample = as.factor(sample))

# make factor
n_data.df <- n_data.df %>%
  mutate(sample = as.factor(sample))

# JOIN FILES ----

# full join 
full.df <- full_join(decomp.df, n_data.df, by = "sample")

# CLEAN FULL DATAFRAME ----

# remove na from data we didn't send 
full.df <- full.df %>%
  na.omit()

# make row a number
full.df <- full.df %>% 
  mutate(row = case_when(
    row == "A" ~ 1,
    row == "B" ~ 2,
    row == "C" ~ 3,
    row == "D" ~ 4,
    row == "E" ~ 5,
    row == "F" ~ 6,
    row == "G" ~ 7,
    row == "H" ~ 8,
    row == "I" ~ 9,
    row == "J" ~ 10,
    TRUE ~ 99999
  ) )

# C IN BIOMASS ----

# proportion of n in biomass then get grams of n in sample
full.df <- full.df %>%
  mutate(prop_n = percent_n / 100,
         prop_c = percent_c / 100,
         total_n_g = prop_n * coll_wt_g,
         total_c_g = prop_c * coll_wt_g)


# grams of c in sample
full.df <- full.df %>%
  mutate(total_c_g = prop_c * coll_wt_g)

# QUICK PLOT TO SEE HOW THINGS LOOK ----

full.df %>%
  ggplot(mapping = aes(days, total_c_g, color = spp)) +
  geom_point()

# gonna have to get rid of those weird days that got sampled
unique(full.df$days)

full.df <- full.df %>%
  filter(days == 0 | days == 14 | days == 35 | days == 63)

# plot it again to see if I removed the correct days, yep looks good
full.df %>%
  ggplot(mapping = aes(days, total_c_g, color = spp)) +
  geom_point()

# IMPORTANT NOTE ----
# this might not be a negative exponential, going to do a mean se plot to get a better look

full.df %>%
  ggplot(mapping = aes(days, total_c_g, color = spp)) +
  stat_summary(fun = mean, na.rm = TRUE, geom = "point") +
  stat_summary(fun.data = mean_se, na.rm = TRUE, geom = "line") +
  theme_classic()

# NONLINEAR MODELS ----

# pc ----
pc <- full.df %>% 
  filter(spp=="PC")  %>% 
  select(row, total_c_g, days ) %>% 
  rename(
    rep = row,
    Mt = total_c_g,
    t = days
  ) %>% 
  filter(rep != 99999)

# okay, we are missing reps 3 and 5 from t = 63 in this dataframe, 
# we first need to find the average Mt from t = 63 
average.df <- pc %>%
  filter(t == 63) %>%
  group_by(t) %>%
  summarize(average = mean(Mt))

# found the value I need so I use add row to work it into the data frame and get a full set

pc <- pc %>%
  add_row(rep = 3, Mt = 6.693355, t = 63) %>%
  add_row(rep = 5, Mt = 6.693355, t = 63)

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
  select(row, total_c_g, days ) %>% 
  rename(
    rep = row,
    Mt = total_c_g,
    t = days
  ) %>% 
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
  add_row(rep = 3, Mt = 7.724503, t = 63) %>%
  add_row(rep = 5, Mt = 7.724503, t = 63) %>%
  add_row(rep = 10, Mt = 6.693355, t = 0)


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
  select(row, total_c_g, days ) %>% 
  rename(
    rep = row,
    Mt = total_c_g,
    t = days
  ) %>% 
  filter(rep != 99999)

# missing t35 rep6, t63 rep3 | 5

# need the averages first to make the replacements 
average.df <- ar %>%
  filter(t == 63 |t == 35) %>%
  group_by(t) %>%
  summarize(average = mean(Mt))

ar <- ar %>%
  add_row(rep = 6, Mt = 4.582708, t = 35) %>%
  add_row(rep = 3, Mt = 3.995975, t = 63) %>%
  add_row(rep = 5, Mt = 3.995975, t = 63)


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
  select(row, total_c_g, days ) %>% 
  rename(
    rep = row,
    Mt = total_c_g,
    t = days
  ) %>% 
  filter(rep != 99999)

# missing t63 rep 3 | 5 | 6

# need the averages first to make the replacements 
average.df <- cr %>%
  filter(t == 63) %>%
  group_by(t) %>%
  summarize(average = mean(Mt))

cr <- cr %>%
  add_row(rep = 3, Mt = 4.682803, t = 63) %>%
  add_row(rep = 5, Mt = 4.682803, t = 63) %>%
  add_row(rep = 6, Mt = 4.682803, t = 63)


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
  mutate(sp="cr")

ar_c_k_nonlin.df <- ar_c_k_nonlin.df %>% 
  mutate(sp="ar")

pc_c_k_nonlin.df <- pc_c_k_nonlin.df %>% 
  mutate(sp="pc")

gm_c_k_nonlin.df <- gm_c_k_nonlin.df %>% 
  mutate(sp="gm_pc")

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
  ggplot(mapping = aes(sp, k)) +
  geom_point() +
  theme_classic()

# mean se plot ----
nonlin_c_k.df %>%
  ggplot(mapping = aes(sp, k, color = soil_block)) +
  stat_summary(fun.data = mean_se, na.rm = TRUE, geom = "errorbar", width = 0.2, 
               position = position_dodge2(.2)) +
  stat_summary(fun = mean, na.rm = TRUE, geom = "point", size = 4,
               position = position_dodge2(.2)) +
  theme_classic()

full.df %>%
  ggplot(mapping = aes(days, total_c_g, color = spp)) +
  stat_summary(fun.data = mean_se, na.rm = TRUE, geom = "errorbar", width = 0.2, 
               position = position_dodge2(.2)) +
  stat_summary(fun = mean, na.rm = TRUE, geom = "point", size = 4,
               position = position_dodge2(.2)) +
  theme_classic()

# STATISTICAL ANALYSIS ----

# create model ----
one.lm = lm(k ~ sp * soil_block, data = nonlin_c_k.df)

# check assumptions of model ----
residuals <- resid(one.lm)
plot(fitted(one.lm), residuals)
qqnorm(residuals)
qqline(residuals)

# homogeneity of variance looks good, normality could be better but its fine

# run model ----
Anova(one.lm, type = 3)

# RESULTS OF ANOVA ----

# reject Ho: the mean difference in decomposition rate between all groups is equal to 0 
# no significant interaction between spp and soil block p = 0.23
# no significant interaction of soil block p = 0.78
# significant effect of species p < 0.0001

# POST F TESTS ----

# post f tests to determine where the differences between groups are 

# create emmeans model ----
one.emm <- emmeans(one.lm, ~ sp)

# plot emmeans ----
plot(one.emm, comparisons = TRUE)

# mean separation ----
# where are the differences in groups and what are the emmeans?
multcomp::cld(one.emm, Letters = letters, adjust = "Bonferroni")

# p-values ----
emminteraction = emmeans(one.emm, pairwise ~ sp, adjust = "bonferroni", alpha = 0.5)
emminteraction$contrasts





