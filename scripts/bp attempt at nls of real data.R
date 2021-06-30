library(tidyverse)
library(janitor)

# read in file
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

# lets do hard math
decomp.df <- decomp.df %>% 
  mutate(pct_mass_remain = ((coll_wt_g)/ initial_wt_g)*100)

# # summary of mass remaining
# corr_t0.df <- decomp.df %>% 
#   filter(time ==0) %>% 
#   group_by(spp) %>% 
#   summarize(pct_mass_remain = 100-mean(pct_mass_remain, na.rm=TRUE))
# 
# # correct mass remaining
# decomp.df <- decomp.df %>%
#   mutate(pct_mass_remain = case_when(
#     spp == "PC"   & time == 1  ~ pct_mass_remain + 2.0582047,
#     spp == "GM_PC"& time == 1 ~ pct_mass_remain + -0.2269507,
#     spp == "AR"   & time == 1 ~ pct_mass_remain + 7.1806311,
#     spp == "CR"   & time == 1 ~ pct_mass_remain + 7.4213231,
#     TRUE ~ pct_mass_remain
# 
#   ))



# look at the graph
decomp.df %>% 
  ggplot(aes(time, pct_mass_remain, color=spp)) +
  stat_summary(fun = mean, na.rm = TRUE,
               geom = "point",
               size = 3,
               position = position_dodge(.2)) +
  stat_summary(fun.data = mean_se, na.rm = TRUE, 
               geom = "errorbar",
               width = 0.2,
               position = position_dodge(.2)) 


# or this
decomp.df %>% 
  ggplot(aes(time, pct_mass_remain, color=spp)) +
  geom_point( position = position_dodge2(.1)) +
  geom_smooth(method = "lm") + 
  facet_wrap(~spp)

# we want to make sub datasets --
pc.df <- decomp.df %>% 
  filter(spp == "PC" & soil_block %in% c(1,2)) %>%
  select(time, row_no, pct_mass_remain) 
  
  # plot again
pc.df  %>% 
  ggplot(aes(time, pct_mass_remain, color=as.factor(row_no))) +
  geom_point() +
  geom_smooth(method = "lm")


deco <- pc.df %>% 
  rename(
    rep = row_no,
    Mt = pct_mass_remain,
    t = time
  )

# nonlin = nls(pct_mass_remain ~ 1*exp(-k*time), trace=TRUE, start = list(k = .01), data=pc_b1.df)
# nonlin regression stuff

#create a vector to store k values estimated for each set of measurements with the same "rep" number:
k=numeric(max(deco$rep)) 	
#	vector length = number of k values that will be generated.
#	Here, "rep" = the observations that will be included in one 
#	nonlinear model fitting i.e., used to generate one k estimate




#create an array to store predicted mass remaining values:
pred=array(0,dim=c(2, max(deco$rep)))	
#	array(all values = 0,dim=c(no. of rows=number of time steps, columns=number of reps)

#create an array to store the model residuals:
resid=array(0,dim=c(2, max(deco$rep)))	
#	as above

#create an array to store the observed values for making graphs:
obs=array(0,dim=c(2,max(deco$rep)))	

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
  nonlin = nls(Mt ~ 1*exp(-k*t), trace=TRUE, start = list(k = .01))
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
