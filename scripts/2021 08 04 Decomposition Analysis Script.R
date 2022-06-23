# load libraries ----
library(tidyverse)
library(janitor)
library(purrr)
library(broom)
library(car)
library(emmeans)

# read in file -----
decomp.df <- read_csv("data/decomp_biomass.csv") %>% clean_names()

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

# math modifications to get pct mass remaining  -----
decomp.df <- decomp.df %>% 
  mutate(pct_mass_remain = ((coll_wt_g/ initial_wt_g)*100)) %>% 
  mutate(soil_block = as.factor(soil_block),
         spp = as.factor(spp))

# remove missing values -----
decomp.df <- decomp.df %>% 
  filter(!is.na(initial_wt_g))

# # data to look at  ---
# decomp_check.df <- decomp.df %>% 
#   filter(!is.na(days)) %>% 
#   filter(days==49 & soil_block==2) %>% 
#   filter(spp == "PC" | spp == "GM_PC") %>% 
#   arrange(bag_no)
# 
# write_csv(decomp.df, "output/numbers to check.csv")
#  

# correct for day 0 mass remaining  -----
corr_t0.df <- decomp.df %>%
  filter(days == 0) %>%
  group_by(spp) %>%
  summarize(pct_mass_remain = 100 - mean(pct_mass_remain, na.rm=TRUE))

# correct mass remaining from time 0 -----
# Do we adjust all masses to this correction ----
decomp.df <- decomp.df %>%
  mutate(pct_mass_remain_corr = case_when(
    spp == "PC"    & days == 0  ~ pct_mass_remain + 2.0582047,
    spp == "GM_PC" & days == 0  ~ pct_mass_remain + -0.2269507,
    spp == "AR"    & days == 0  ~ pct_mass_remain + 7.1806311,
    spp == "CR"    & days == 0  ~ pct_mass_remain + 7.4213231,
    TRUE ~ pct_mass_remain
  ))


# Graph of decomposition  -----
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

ggsave(decomp.plot, file="figures/decomposition with time graph.pdf",
       height=7, width=7, units="in")


# decomposition on different soils -----
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

ggsave(decomp_soil.plot, file="figures/decomposition soil graph.pdf",
       width=8, height =7, units="in")
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# PC Decomp nonlin
pc <- decomp.df %>% 
  filter(!is.na(days)) %>% 
  filter(spp=="PC")  %>% 
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
pred=array(0, dim=c(7, max(pc$rep)))	
#	array(all values = 0,dim=c(no. of rows=number of time steps, columns=number of reps)

#create an array to store the model residuals:
resid=array(0, dim=c(7, max(pc$rep)))	
#	as above

#create an array to store the observed values for making graphs:
obs=array(0, dim=c(7, max(pc$rep)))	

#######################################
#	  NONLINEAR REGRESSION          #
# estimate k for each replicate (rep) #
#######################################
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
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# PC Decomp nonlin
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

#######################################
#	  NONLINEAR REGRESSION          #
# estimate k for each replicate (rep) #
#######################################
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
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# cr Decomp nonlin
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

#######################################
#	  NONLINEAR REGRESSION          #
# estimate k for each replicate (rep) #
#######################################
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
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


# PC Decomp nonlin
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

#######################################
#	  NONLINEAR REGRESSION          #
# estimate k for each replicate (rep) #
#######################################
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
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

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


### GRAPHING K VALUES
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
nonlin_k.plot
ggsave(nonlin_k.plot, file="figures/nonlin_k_plot.pdf",
       height=7, width=7, units="in")

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
nonlin_k_soil.plot
ggsave(nonlin_k_soil.plot, file="figures/nonlin_k_soil_plot.pdf",
       height=7, width=7, units="in")

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 


k_nonlin.model = lm(k~sp*soil_block, data=nonlin_k.df)
Anova(k_nonlin.model,type=3)
leveneTest(k ~ sp, data=nonlin_k.df)

# Check assumptions------
# Normality Assumption graphical QQ PLOT ----
# the base way
plot(k_nonlin.model, 2)
# Test for normaly distributed residuals ----
shapiro.test((k_nonlin.model$residuals))


# POST F TESTS ------
# Post F Tests run if there is a significant effect----
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



ggsave(k_nonlin.plot, file="figures/nonlin_k_soils_plot.pdf",
       height=3.2, width=7.8, units="in")




# simple version
k.plot <- k.emmeans %>% 
  ggplot(aes(x=sp)) +
  geom_point(aes(y=emmean), size=3) +
  geom_errorbar(aes(ymin = emmean-SE, ymax = emmean+SE), 
                stat="identity", width = 0.2) +
  labs(x="Species", y= "k (% per day loss)")  +
  geom_text(aes(x = 1, y = .997, label = "A")) +
  geom_text(aes(x = 2, y = .997, label = "B")) +
  geom_text(aes(x = 3, y = .997 , label = "B"))+
  geom_text(aes(x = 4, y = .997, label = "C"))
k.plot












# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# how to get linear estimates
decomp.df <- decomp.df %>% 
  mutate(ln_pct_remain = log(pct_mass_remain))

pc_k.df <- 
  decomp.df %>% 
  filter(spp=="PC")  %>% 
  filter(row_no != 99999)%>% 
  split(.$row_no) %>% 
  map(~lm(ln_pct_remain ~  days, data = .x)) %>% 
  map_df(tidy) %>% 
  filter(term == 'days') %>% 
  mutate(k=exp(estimate),
         sp="PC")

pcgm_k.df <- 
  decomp.df %>% 
  filter(spp=="GM_PC")  %>% 
  split(.$row_no) %>% 
  map(~lm(ln_pct_remain ~  days, data = .x)) %>% 
  map_df(tidy) %>% 
  filter(term == 'days') %>% 
  mutate(k=exp(estimate),
         sp="GM_PC")

ar_k.df <- 
  decomp.df %>% 
  filter(spp=="AR")  %>% 
  split(.$row_no) %>% 
  map(~lm(ln_pct_remain ~  days, data = .x)) %>% 
  map_df(tidy) %>% 
  filter(term == 'days') %>% 
  mutate(k=exp(estimate),
         sp="AR")

cr_k.df <- 
  decomp.df %>% 
  filter(spp=="CR")  %>% 
  split(.$row_no) %>% 
  map(~lm(ln_pct_remain ~  days, data = .x)) %>% 
  map_df(tidy) %>% 
  filter(term == 'days') %>% 
  mutate(k=exp(estimate),
         sp="CR")


k.df <- bind_rows(pc_k.df, pcgm_k.df, ar_k.df, cr_k.df )

k.df <- k.df %>%  mutate(sp=as.factor(sp))
k.df %>% 
  ggplot(aes(sp, 1-k, color=sp)) +
  stat_summary(fun.data = mean_se, na.rm = TRUE, 
               geom = "errorbar",
               width = 0.2,
               position = position_dodge(.2)) +
  stat_summary(fun = mean, na.rm = TRUE,
               geom = "point",
               size = 4,
               position = position_dodge(.2)) 


k.model = lm(k~sp, data=k.df)
Anova(k.model,type=3)
leveneTest(k ~ sp, data=k.df)

# Check assumptions------
# Normality Assumption graphical QQ PLOT ----
# the base way
plot(k.model, 2)
# Test for normaly distributed residuals ----
shapiro.test((k.model$residuals))


# POST F TESTS ------
# Post F Tests run if there is a significant effect----
k.emm <- emmeans(k.model, ~ sp)

# Plot means
plot(k.emm, comparisons = TRUE)

# Mean separation
multcomp::cld(k.emm, Letters = letters, adjust="bonferroni")

# Now to obtain the emmeans
# pairwise
anova_emminteraction = emmeans(k.model, 
                               pairwise ~ sp,
                               adjust = "bonferroni")

anova_emminteraction$emmeans

k.emmeans <- as.data.frame(anova_emminteraction$emmeans)

k.plot <- k.emmeans %>% 
  ggplot(aes(x=sp)) +
  geom_point(aes(y=emmean), size=3) +
  geom_errorbar(aes(ymin = emmean-SE, ymax = emmean+SE), 
                stat="identity", width = 0.2) +
  labs(x="Species", y= "k (% per day loss)")  +
  geom_text(aes(x = 1, y = .997, label = "A")) +
  geom_text(aes(x = 2, y = .997, label = "B")) +
  geom_text(aes(x = 3, y = .997 , label = "B"))+
  geom_text(aes(x = 4, y = .997, label = "C"))
k.plot


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# or this
decomp.df %>% 
  ggplot(aes(days, pct_mass_remain, color=spp)) +
  geom_point( position = position_dodge2(.1)) +
  geom_smooth(method = "lm") + 
  facet_wrap(~spp)

# we want to make sub datasets --
pc.df <- decomp.df %>% 
  filter(spp == "PC" & soil_block %in% c(1,2)) %>%
  select(days, row_no, pct_mass_remain) 
  
  # plot again
pc.df  %>% 
  ggplot(aes(days, pct_mass_remain, color=as.factor(row_no))) +
  geom_point() +
  geom_smooth(method = "lm")


deco <- decomp.df %>% 
  filter(spp=="AR") %>% 
  select(row_no, pct_mass_remain, days ) %>% 
  rename(
    rep = row_no,
    Mt = pct_mass_remain,
    t = days
  )

# nonlin = nls(pct_mass_remain ~ 1*exp(-k*time), trace=TRUE, start = list(k = .01), data=pc_b1.df)
# nonlin regression stuff

#create a vector to store k values estimated for each set of measurements with the same "rep" number:
k=numeric(max(deco$rep)) 	
#	vector length = number of k values that will be generated.
#	Here, "rep" = the observations that will be included in one 
#	nonlinear model fitting i.e., used to generate one k estimate




#create an array to store predicted mass remaining values:
pred=array(0, dim=c(4, max(deco$rep)))	
#	array(all values = 0,dim=c(no. of rows=number of time steps, columns=number of reps)

#create an array to store the model residuals:
resid=array(0,dim=c(4, max(deco$rep)))	
#	as above

#create an array to store the observed values for making graphs:
obs=array(0,dim=c(4,max(deco$rep)))	

#######################################
#	  NONLINEAR REGRESSION          #
# estimate k for each replicate (rep) #
#######################################
#the ?for? loop below subsets the data by replicate (rep) number (rep = 1 to 5), 
#	in order to estimate k nonlinearly for each 
#	replicate (rep).

for(i in 1:max(deco$rep)){
  s1=subset(deco,deco$rep==i)   
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
