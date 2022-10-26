#descriptions and/or explanations are preceded by a "#" symbol
#	and will not be read by R

rm(list=ls())	#clears memory

#DATA:
t=c(30,60,100,150,200,250,300,350,400,500,600,700,1000,1500,30,60,100,150,200,250,300,350,400,500,600,700,1000,1500,30,60,100,150,200,250,300,350,400,500,600,700,1000,1500,30,60,100,150,200,250,300,350,400,500,600,700,1000,1500,30,60,100,150,200,250,300,350,400,500,600,700,1000,1500)
rep=c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,3,3,3,3,3,3,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4,4,4,4,4,4,4,5,5,5,5,5,5,5,5,5,5,5,5,5,5)
Mt=c(0.8348,0.7269,0.273,0.6786,0.5165,0.3807,0.2635,0.4139,0.2480,0,0.0347,0.0602,0.0135,0.0561,0.9327,0.7803,0.825,0.4272,0.3188,0.3545,0.4112,0.1906,0.2957,0.1460,0.0909,0,0.1159,0.0699,1,0.7401,1,0.2981,0.4296,0.5585,0.1113,0.176,0.1191,0.2580,0.0417,0.1013,0.1932,0.2583,0.9110,0.8791,0.6680,0.8651,0.4715,0.3278,0,0,0.1273,0.0304,0,0,0,0.2644,0.6649,0.7160,0.764,0.7455,0.4342,0.6268,0.4569,0.1907,0,0.496,0,0,0.2446,0)

#bind above data vectors together as columns in a data frame:
deco=data.frame(cbind(t,rep,Mt)) 

#look at row headings and first few lines of data in data frame:
head(deco) 	

#NOTE: data may also be read in from a .csv file as follows: 
#	file.choose()	#command to browse to your .csv data file
#	deco=read.csv("C:\\DecompositionData.csv", header=TRUE) #command to read in data from .csv file

#create a vector to store k values estimated for each set of measurements with the same "rep" number:
k=numeric(max(deco$rep)) 	
#	vector length = number of k values that will be generated.
#	Here, "rep" = the observations that will be included in one 
#	nonlinear model fitting i.e., used to generate one k 
#	estimate

#create an array to store predicted mass remaining values:
pred=array(0,dim=c(14,max(deco$rep)))	
#	array(all values = 0,dim=c(no. of rows=number of time steps, columns=number of reps)

#create an array to store the model residuals:
resid=array(0,dim=c(14,max(deco$rep)))	
#	as above

#create an array to store the observed values for making graphs:
obs=array(0,dim=c(14,max(deco$rep)))	

#######################################
#	  NONLINEAR REGRESSION          #
# estimate k for each replicate (rep) #
#######################################
#the “for” loop below subsets the data by replicate (rep) number (rep = 1 to 5), 
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
	
}	#end of nls “for” loop

#copy vector with k values to clipboard to paste into excel or 
#	other program of choice. This command raises clipboard 
#	memory in 32 KB steps; default = 32 KB
#	To increase memory to 64, replace "clipboard" with 
#	"clipboard-64":
write.table(k,"clipboard",sep="\t",row.names=FALSE,col.names=FALSE)

#copy predicted and residual arrays to clipboard:
write.table(pred,"clipboard",sep="\t",row.names=FALSE,col.names=FALSE)
write.table(resid,"clipboard",sep="\t",row.names=FALSE,col.names=FALSE)

#plot predicted values vs residuals for one rep:
plot(pred[,1],resid[,1],xlab="Predictions",ylab="Residuals")
	#plot command explanation:
	#plot(x data, ydata, x axis label, y axis label)

#plot predictions of Mt from one rep vs time
plot(s1$t,pred[,1],xlab="Days",ylab="Predictions")

#add observed measurements to plot as points
points(s1$t,obs[,1],col="red") 
	#points command explanation:
	#points(x data, ydata, point color = red)



