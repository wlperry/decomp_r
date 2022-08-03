# LIBRARIES ----
library(tidyverse)
library(janitor)
library(patchwork)
library(readxl)

# READ IN THE DATA ----
# reads in decomp and n data ----
# mass sent
cci.df <- read.csv("Data/Decomp/n no data.csv") %>%
  clean_names() %>%
  remove_empty(which = c("cols", "rows"))

# c and n data
n_data.df <- read.csv("Data/Decomp/n with data.csv") %>%
  clean_names() %>%
  remove_empty(which = c("cols", "rows"))

# biomass data
decomp.df <- read.csv("Data/Decomp/biomass.csv") %>%
  clean_names() %>%
  remove_empty(which = c("cols", "rows"))

# PREP TO JOIN ----

# rename to id
cci.df <- cci.df %>%
  rename(id = bag)

# rename to id
n_data.df <- n_data.df %>%
  rename(id = sample)

# rename to id
decomp.df <- decomp.df %>%
  rename(id = bag_no)

# set biomass id to factor
decomp.df <- decomp.df %>%
  mutate(id = as.factor(id))

# for n data
n_data.df <- n_data.df %>%
  mutate(id = as.factor(id))

# for cci mass sent
cci.df <- cci.df %>% 
  mutate(id = as.factor(id)) 

# JOIN FILES ----

# full data frame for biomass analysis
decomp.df <- full_join(decomp.df, n_data.df, by = "id")

# think i need to add the mass sent data to this
decomp.df <- full_join(decomp.df, cci.df, by = "id")

# remove extra stuff
decomp.df <- decomp.df %>%
  select(-study, -box, -cell, -spp.y, -y_m_d, -plot, -subplot)

# rename spp
decomp.df <- decomp.df %>%
  rename(spp = spp.x)

# full data frame for cci analysis
cci.df <- full_join(cci.df, n_data.df, by = "id")


# CLEANING OF DECOMP DATA AND PREP FOR ANALYSIS ----

# make row a number
decomp.df <- decomp.df %>% 
  mutate(row_no = case_when(
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

# math to get pct mass remaining
decomp.df <- decomp.df %>% 
  mutate(pct_mass_remain = ((coll_wt_g/ initial_wt_g)*100)) %>% 
  mutate(soil_block = as.factor(soil_block),
         spp = as.factor(spp))

# remove missing values
decomp.df <- decomp.df %>% 
  filter(!is.na(initial_wt_g))

# correct for day 0 mass remaining
corr_t0.df <- decomp.df %>%
  filter(days == 0) %>%
  group_by(spp) %>%
  summarize(pct_mass_remain = 100 - mean(pct_mass_remain, na.rm=TRUE))

# correct mass remaining from time 0
decomp.df <- decomp.df %>%
  mutate(pct_mass_remain_corr = case_when(
    spp == "PC"    & days == 0  ~ pct_mass_remain + 2.0582047,
    spp == "GM_PC" & days == 0  ~ pct_mass_remain + -0.2269507,
    spp == "AR"    & days == 0  ~ pct_mass_remain + 7.1806311,
    spp == "CR"    & days == 0  ~ pct_mass_remain + 7.4213231,
    TRUE ~ pct_mass_remain
  ))

# N IN BIOMASS ----
# calcs for the n in biomass ----
# here we work the n in biomass data into the dataframe
# first thing i need to do is divide percents by 100 so they are properly expressed as percent
decomp.df <- decomp.df %>%
  mutate(prop_n = percent_n / 100,
         prop_c = percent_c / 100)

# next i multiply percent n by the final weight of our sample to get the grams of n in the sample
decomp.df <- decomp.df %>%
  mutate(total_n_g = prop_n * coll_wt_g)





decomp.df <- decomp.df %>%
  arrange(spp, days, soil_block, row)
write_csv(decomp.df, "output/decomp_data_with_pct_n.csv")

# %>% 
#   filter(days != 63)

# 
# 
# decomp.df <- decomp.df %>% 
#   filter(days != 7) %>% 
#   filter(days != 21) %>% 
#   filter(days != 28) %>% 
#   filter(days != 49) 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# STARhere::
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

decomp.df <- read_excel("output/decomp_data_with_pct_n FOR ANALYSIS.xlsx")



# SOME QUICK PLOTS TO SEE HOW THINGS LOOK ----
# some odd outliers in here
# total n per g ----
decomp.df %>%
  ggplot(mapping = aes(days, total_n_g, color = spp)) +
  stat_summary(
    fun = mean, na.rm = TRUE, geom = "point", size = 4) +
  stat_summary(
    fun.data = mean_se, na.rm = TRUE, geom = "errorbar", width = .3, size = .7)

# CALCULATING K VALUES FOR DECOMPOSITION ----

# pennycress ----
# create dataframe
pc <- decomp.df %>% 
  filter(!is.na(days)) %>% 
  filter(spp=="PC") %>% 
  select(row_no, pct_mass_remain, days ) %>% 
  rename(
    rep = row_no,
    Mt = pct_mass_remain,
    t = days
  ) %>% 
  mutate(Mt = Mt/100) %>% 
  filter(rep != 99999) 

pc %>%
  ggplot(aes(t, Mt)) +
  stat_summary(
    fun = mean, na.rm = TRUE, geom = "point", size = 4) +
  stat_summary(
    fun.data = mean_se, na.rm = TRUE, geom = "errorbar", width = .3, size = .7)

# set up nonlinear regression
nonlin = nls(Mt ~ 1*exp(-k*t), trace=TRUE, start = list(k = .01), data=pc)

# rm(nonlin)

# create a vector to store k values
# estimated for each set of measurements with the same "rep" number:
k=numeric(max(pc$rep)) 	
# k=numeric(10)
#	vector length = number of k values that will be generated.
#	Here, "rep" = the observations that will be included in one 
#	nonlinear model fitting i.e., used to generate one k estimate


#create an array to store predicted mass remaining values
pred=array(0, dim=c(4, max(pc$rep)))	
#	array(all values = 0,dim=c(no. of rows=number of time steps, columns=number of reps)

#create an array to store the model residuals
resid=array(0, dim=c(4, max(pc$rep)))	
#	as above

#create an array to store the observed values for making graphs
obs=array(0, dim=c(4, max(pc$rep)))	

# nonlinear regression 

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


write_csv(as.data.frame(k), file="output/pc k nonlin values.csv")

pc_k_nonlin.df <- as.data.frame(k)

# gene edited pennycress ----
pc_gm <- decomp.df %>% 
  filter(spp=="GM_PC")  %>% 
  filter(!is.na(days)) %>% 
  filter(days != 84) %>% 
  filter(days != 28) %>% 
  filter(days != 63) %>% 
  select(row_no, pct_mass_remain, days ) %>% 
  rename(
    rep = row_no,
    Mt = pct_mass_remain,
    t = days
  ) %>% 
  mutate(Mt = Mt/100) %>% 
  filter(rep != 99999)

# so this works!!!
nonlin = nls(Mt ~ 1*exp(-k*t), trace=TRUE, start = list(k = .01), data=pc_gm)
# nonlin regression stuff

#create a vector to store k values estimated for each set of measurements with the same "rep" number:
k=numeric(max(pc_gm$rep)) 	
#	vector length = number of k values that will be generated.
#	Here, "rep" = the observations that will be included in one 
#	nonlinear model fitting i.e., used to generate one k estimate

#create an array to store predicted mass remaining values:
pred=array(0, dim=c(6, max(pc_gm$rep)))	 #length(pc_gm$t)
#	array(all values = 0,dim=c(no. of rows=number of time steps, columns=number of reps)

#create an array to store the model residuals:
resid=array(0,dim=c(6, max(pc_gm$rep)))	
#	as above

#create an array to store the observed values for making graphs:
obs=array(0,dim=c(6, max(pc_gm$rep)))

# nonlinear regression 

#the ?for? loop below subsets the data by replicate (rep) number (rep = 1 to 5), 
#	in order to estimate k nonlinearly for each 
#	replicate (rep).

for(i in 1:max(pc_gm$rep)){
  s1=subset(pc_gm,pc_gm$rep==i)   
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


write_csv(as.data.frame(k), file="output/gm_pc k nonlin values.csv")

gm_pc_k_nonlin.df <- as.data.frame(k)

# cereal rye ----
cr <- decomp.df %>% 
  filter(spp=="CR")  %>% 
  filter(days != 63) %>% 
  filter(days != 28) %>% 
  select(row_no, pct_mass_remain, days ) %>% 
  rename(
    rep = row_no,
    Mt = pct_mass_remain,
    t = days
  ) %>% 
  mutate(Mt = Mt/100) %>% 
  filter(rep != 99999)


# so this works!!!
nonlin = nls(Mt ~ 1*exp(-k*t), trace=TRUE, start = list(k = .01), data=cr)
# nonlin regression stuff

#create a vector to store k values estimated for each set of measurements with the same "rep" number:
k=numeric(max(cr$rep)) 	
#	vector length = number of k values that will be generated.
#	Here, "rep" = the observations that will be included in one 
#	nonlinear model fitting i.e., used to generate one k estimate


#create an array to store predicted mass remaining values:
pred=array(0, dim=c(7, max(cr$rep)))	 #length(cr$t)
#	array(all values = 0,dim=c(no. of rows=number of time steps, columns=number of reps)

#create an array to store the model residuals:
resid=array(0,dim=c(7, max(cr$rep)))	
#	as above

#create an array to store the observed values for making graphs:
obs=array(0,dim=c(7, max(cr$rep)))	

# nonlinear regression 

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


write_csv(as.data.frame(k), file="output/cr k nonlin values.csv")

cr_k_nonlin.df <- as.data.frame(k)

# annual rye ----
ar <- decomp.df %>% 
  filter(spp=="AR")  %>% 
  filter(days != 28) %>% 
  filter(days != 35) %>% 
  filter(days != 49) %>%
  filter(days != 63) %>% 
  filter(!is.na(days)) %>% 
  select(row_no, pct_mass_remain, days ) %>% 
  rename(
    rep = row_no,
    Mt = pct_mass_remain,
    t = days
  ) %>% 
  mutate(Mt = Mt/100) %>% 
  filter(rep != 99999)

# so this works!!!
nonlin = nls(Mt ~ 1*exp(-k*t), trace=TRUE, start = list(k = .01), data=ar)
# nonlin regression stuff

#create a vector to store k values estimated for each set of measurements with the same "rep" number:
k=numeric(max(ar$rep)) 	
#	vector length = number of k values that will be generated.
#	Here, "rep" = the observations that will be included in one 
#	nonlinear model fitting i.e., used to generate one k estimate


#create an array to store predicted mass remaining values:
pred=array(0, dim=c(5, max(ar$rep)))	 #length(ar$t)
#	array(all values = 0,dim=c(no. of rows=number of time steps, columns=number of reps)

#create an array to store the model residuals:
resid=array(0,dim=c(5, max(ar$rep)))	
#	as above

#create an array to store the observed values for making graphs:
obs=array(0,dim=c(5, max(ar$rep)))	

# nonlinear regression 

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


write_csv(as.data.frame(k), file="output/ar k nonlin values.csv")

ar_k_nonlin.df <- as.data.frame(k)

# put all of the k values together ----

cr_k_nonlin.df <- cr_k_nonlin.df %>% 
  mutate(sp="cr")

ar_k_nonlin.df <- ar_k_nonlin.df %>% 
  mutate(sp="ar")

pc_k_nonlin.df <- pc_k_nonlin.df %>% 
  mutate(sp="pc")

gm_pc_k_nonlin.df <- gm_pc_k_nonlin.df %>% 
  mutate(sp="gm_pc")


nonlin_k.df <- bind_rows(pc_k_nonlin.df,gm_pc_k_nonlin.df, ar_k_nonlin.df,cr_k_nonlin.df)



nonlin_k.df <- nonlin_k.df %>% 
  mutate(row_no = rep(1:10, length.out = n())) %>%
  mutate(soil_block = rep(seq_along(1:2), each=5, length.out = n())) 


nonlin_k.df <- nonlin_k.df %>% 
  mutate(soil_block = as.factor(soil_block))


# graph of decomp k values ----
nonlin_k.plot<- nonlin_k.df %>% 
  ggplot(aes(x=sp, y=k))+
  stat_summary(fun.data = mean_se, na.rm = TRUE, 
               geom = "errorbar",
               width = 0.2,
               position = position_dodge(.2)) +
  stat_summary(fun = mean, na.rm = TRUE,
               geom = "point",
               size = 4,
               position = position_dodge(.2)) 


nonlin_k_soil.plot<- nonlin_k.df %>% 
  ggplot(aes(x=sp, y=k, color=soil_block))+
  stat_summary(fun.data = mean_se, na.rm = TRUE, 
               geom = "errorbar",
               width = 0.2,
               position = position_dodge(.2)) +
  stat_summary(fun = mean, na.rm = TRUE,
               geom = "point",
               size = 4,
               position = position_dodge(.2)) 

nonlin_k.plot
nonlin_k_soil.plot

# STATISTICAL ANALYSIS ----

# create model ----
k_nonlin.model = lm(k~sp*soil_block, data=nonlin_k.df)

# check assumptions of the model ----
residuals <- resid(k_nonlin.model)
plot(fitted(k_nonlin.model), residuals)
qqnorm(residuals)
qqline(residuals)

# statistical tests for assumptions ----
shapiro.test((k_nonlin.model$residuals))
leveneTest(k ~ sp, data=nonlin_k.df)

# run model ----
Anova(k_nonlin.model,type=3)

# interactsions are significant, need post f tests through emmeans ----

# post f tests ----

# crate emmean model 
k_nonlin.emm <- emmeans(k_nonlin.model, ~ sp)

# Plot means
plot(k_nonlin.emm, comparisons = TRUE)

# Mean separation
multcomp::cld(k_nonlin.emm, Letters = letters, adjust="bonferroni")

# Now to obtain the emmeans
# pairwise
anova_emminteraction = emmeans(k_nonlin.model, 
                               pairwise ~ sp,
                               adjust = "bonferroni")

anova_emminteraction$emmeans

k_nonlin.emmeans <- as.data.frame(anova_emminteraction$emmeans)

levels(k_nonlin.emmeans$sp)
k_nonlin.emmeans <- k_nonlin.emmeans %>% 
  mutate(sp = fct_relevel(sp,"pc", "gm_pc","cr", "ar"))

k_nonlin.plot <-
  
  +
  geom_text(aes(x = .8,  y = .021, label = "ABC"),  size=4)  + # PC 1
  geom_text(aes(x = 1.15, y = .021, label = "BCD"),  size=4) + # PC 2
  geom_text(aes(x = 1.8, y = .021, label = "A"),   size=4) + # GM_PC 1 
  geom_text(aes(x = 2.15, y = .021, label = "AB"),   size=4) + # GM_PC 2
  geom_text(aes(x = 2.8, y = .021 , label = "CD"), size=4) + # CR 1
  geom_text(aes(x = 3.15, y = .021 , label = "E"),  size=4) + # CR 2
  geom_text(aes(x = 3.8, y = .021, label = "DE"),  size=4) + # AR 1
  geom_text(aes(x = 4.15, y = .021, label = "F"),   size=4) + # AR 2
  scale_x_discrete(labels = c("PC", "AOP2 PC", "CR", "AR"))+
  scale_color_manual(
    name = "Species",
    labels = c( "WT Pennycress",  "AOP2 Pennycress", "Cereal Rye", "Annual Rye"  ),
    values = c( "blue4","orange2", "red3",  "green4")) +
  scale_shape_manual(
    name = "Soil Type",
    labels = c("Saybrook Loam", "Drummer and ElPaso Loam"),
    values = c(21, 22))+
  scale_fill_manual(
    name = "Soil Type",
    labels = c("Saybrook Loam", "Drummer and ElPaso Loam"),
    values = c("black", "gray50"))  + 
  
  theme(axis.line = element_line(size = 0.4, linetype = "solid"), 
        axis.title = element_text(size = 14, face = "bold", color="black"), 
        # axis.text =   element_text(size = 14,                                                                                                 colour = "black"), axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 14, face = "bold", color="black"),
        axis.text.x = element_text(size = 14, face = "bold", color="black"),
        legend.text = element_text(size = 12, face = "bold", color="black"), 
        legend.title = element_text(size = 12,face = "bold", color="black"), 
        legend.key = element_rect(fill = NA),
        legend.background = element_rect(fill = NA))
k_nonlin.plot

# CALCULATING K VALUES FOR NITROGEN LOSS ----

# pennycress ----

# so I don't really have the best understanding of the following code so what I'm doing is not
# changing anything major and just selecting the column in want in the data frame and 
# i have everything else the same 

# create dataframe
# got rid of /100 here because already did that above
pc <- decomp.df %>% 
  filter(!is.na(days)) %>% 
  filter(spp=="PC")  %>% 
  filter(days != 63) %>% 
  filter(days != 28) %>% 
  select(row_no, total_n_g, days ) %>% 
  rename(
    rep = row_no,
    Mt = total_n_g,
    t = days
  ) %>% 
  filter(rep != 99999)

# going to try na.omit here to see if that helps
pc <- pc %>%
  na.omit()

# set up nonlinear regression
nonlin = nls(Mt ~ 1*exp(-k*t), trace=TRUE, start = list(k = .01), data=pc)

rm(nonlin)

# create a vector to store k values
# estimated for each set of measurements with the same "rep" number:
k=numeric(max(pc$rep)) 	
# k=numeric(10)
#	vector length = number of k values that will be generated.
#	Here, "rep" = the observations that will be included in one 
#	nonlinear model fitting i.e., used to generate one k estimate


#create an array to store predicted mass remaining values
pred=array(0, dim=c(7, max(pc$rep)))	
#	array(all values = 0,dim=c(no. of rows=number of time steps, columns=number of reps)

#create an array to store the model residuals
resid=array(0, dim=c(7, max(pc$rep)))	
#	as above

#create an array to store the observed values for making graphs
obs=array(0, dim=c(7, max(pc$rep)))	

# nonlinear regression 

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


write_csv(as.data.frame(k), file="output/pc k nonlin values.csv")

pc_k_nonlin_n.df <- as.data.frame(k)



# FINAL PLOTS ----

# Graph of decomposition 
decomp.plot <- decomp.df %>% 
  # filter(spp %in% c("AR", "CR")) %>% 
  ggplot(aes(x=days, y= pct_mass_remain_corr, fill=spp, shape=spp, color = spp)) +
  stat_summary(fun.data = mean_se, na.rm = TRUE, 
               geom = "errorbar",
               width = 4,
               position = position_dodge(2)) +
  stat_summary(fun = mean, na.rm = TRUE,
               geom = "line",
               size = 1,
               position = position_dodge(2)) +
  stat_summary(fun = mean, na.rm = TRUE,
               geom = "point",
               size = 4,
               position = position_dodge(2)) +
  labs(y="Mean Percent Biomass Remaining", x="Days") +
  scale_x_continuous(breaks=seq( 0, 85, 7))+ # this neeeds to change to full series
  scale_color_manual(
    name = "Species",
    labels = c("Annual Rye", "Cereal Rye", "AOP2 Pennycress", "WT Pennycress"),
    values = c( "blue4", "orange2", "red3", "green4")) +
  scale_shape_manual(
    name = "Species",
    labels = c("Annual Rye", "Cereal Rye", "AOP2 Pennycress", "WT Pennycress"),
    values = c(21, 22, 24, 25))+
  scale_fill_manual(
    name = "Species",
    labels = c("Annual Rye", "Cereal Rye", "AOP2 Pennycress", "WT Pennycress"),
    values = c( "blue4", "orange2", "red3", "green4"))  + 
  theme(axis.line = element_line(size = 0.4, linetype = "solid"), 
        axis.title = element_text(size = 14, face = "bold"), 
        # axis.text =   element_text(size = 14,                                                                                                 colour = "black"), axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 14, face = "bold"), 
        legend.title = element_text(size = 14,face = "bold"), 
        legend.key = element_rect(fill = NA),
        legend.background = element_rect(fill = NA)) 

decomp.plot

# decomposition on different soils
decomp_soil.plot <- decomp.df %>% 
  ggplot(aes(x=days, y= pct_mass_remain_corr, shape=soil_block, color = spp, fill = soil_block)) +
  stat_summary(fun.data = mean_se, na.rm = TRUE, 
               geom = "errorbar",
               width = 4,
               position = position_dodge(2)) +
  stat_summary(fun = mean, na.rm = TRUE,
               geom = "line",
               size = 1,
               position = position_dodge(2)) +
  stat_summary(fun = mean, na.rm = TRUE,
               geom = "point",
               size = 4,
               position = position_dodge(2)) +
  labs(y="Mean Percent Biomass Remaining", x="Days") +
  scale_x_continuous(breaks=seq(0,85,7))+
  scale_color_manual(
    name = "Species",
    labels = c("Annual Rye", "Cereal Rye", "AOP2 Pennycress", "WT Pennycress"),
    values = c( "blue4", "orange2", "red3", "green4")) +
  scale_shape_manual(
    name = "Soil Type",
    labels = c("Saybrook Loam", "Drummer and ElPaso Loam"),
    values = c(21, 22))+
  scale_fill_manual(
    name = "Soil Type",
    labels = c("Saybrook Loam", "Drummer and ElPaso Loam"),
    values = c("black", "gray50"))  +
  theme(axis.line = element_line(size = 0.4,
                                 linetype = "solid"), axis.title = element_text(size = 21,
                                                                                face = "bold"), axis.text = element_text(size = 18,
                                                                                                                         colour = "black"), axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        legend.text = element_text(size = 14,
                                   face = "bold"), legend.title = element_text(size = 18,
                                                                               face = "bold"), legend.key = element_rect(fill = NA),
        legend.background = element_rect(fill = NA))

decomp_soil.plot

