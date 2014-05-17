

#-------------------------
# load saved model data
#-------------------------
rm(list=ls())
folder = 'W:\\Research\\Energy Efficiency\\EE Finance toy model\\'
scenario = 'risk.range.base'#what set of non-intervention params do you want?

run.name='' #if there are multiple runs using this scenario

load(file=paste(folder,"model-runs\\",scenario,run.name,".R",sep=''))
# contains list=c('inlist','README','inputs','results','scenario')

# need to re-create indexing variables
LLR = results[,"LSR"]>0 & results[,"upfront.rebate"]==0 & results[,"interest.buydown"]==0
IRB = results[,"LSR"]==0 & results[,"upfront.rebate"]==0 & results[,"interest.buydown"]>0
rebate = results[,"LSR"]==0 & results[,"interest.buydown"]==0
nothing = results[,"LSR"]==0 & results[,"upfront.rebate"]==0 & results[,"interest.buydown"]==0
no.loss=results[,"chance.full.loss"]==0
no.recovery = results[,"recovery"]==0


#-------------------------
# Try to make Jeff's graph showing under what conditions
# an LLR is less expensive than an IRB, for the same user terms
#
#!# TODO: Update this description
# Axes:  (ratio of LLR size/expected loss) vs 'break even' gvt cost of capital
# plotted: line showing where IRB cost = LLR cost
# Held constant: user terms, riskiness, discount/hurdle rates
#--------------------------

# tapply: finding the measn/sd of a given column for each unique combination of the factors in INDEX
gvt.means = tapply(results[,"gvt.cost.NPV"],INDEX=results[,c("LSR","LPCR","interest.buydown","chance.full.loss")], FUN=mean)
interest.means = tapply(results[,"interest.user"],INDEX=results[,c("LSR","LPCR","interest.buydown","chance.full.loss")], FUN=mean)

# dim 1 is LSR
# dim 2 is LPCR
# dim 3 is interest.buydown
# dim 4 is risk.adjust
# interest.means[1,,,] is the sheet with 0 LSR (i.e. no LLR)
# interest.means[,,1,] is the sheet with no IRB
# interest.means[2:dim(interest.means)[1],,1,] is the sheet with an active LLR & no IRB

# to extract the data from dimnames, which are the values of the
# INDEX variables used in tapply
# change the # in double brackets to access a different INDEX variable
# as.numeric(dimnames(interest.means)[[2]]))

#--------------------------
# There are different loss rates available to plot
# let's pick one
#--------------------------

loss.index=4
#--------------------------
# Make vectors of data to fit lines to
#--------------------------
##################Begin Function ###################
#make.regressions= function(){
####################################################
llr<-list()
llr$x<--c(gvt.means[2:dim(interest.means)[1],,1,loss.index])
llr$y<-c(interest.means[2:dim(interest.means)[1],,1,loss.index])
irb<-list()
irb$x<--c(gvt.means[1,1,1:dim(interest.means)[3],loss.index])
irb$y<-c(interest.means[1,1,1:dim(interest.means)[3],loss.index])
#--------------------------
# fit a polynomial model to IRB
#--------------------------
#use nls()
# starting params
irby<-irb$y[1:9]
irbx<-irb$x[1:9]
p0 <- -100
p1 <- 1
p2<- 1
irb.model<-nls(irby ~ p0*irbx + p1*(irbx^2) + p2 , start=list(p0=p0,p1=p1,p2=p2))
#summary(irb.model)
# plot(irbx,irby)
# lines(irbx, predict(irb.model, irbx),col="blue")
#--------------------------
# Make a two-part linear fit to LLR
#--------------------------
#--------------------------
# Find top half of line end
h <- min(llr$y)
top <- which(llr$y != h)
bottom <- which(llr$y == h)
#--------------------------
# linear fit to top half of line
topy<-llr$y[top]
topx<-llr$x[top]
toplm<-lm(topy~topx)#lm(formula=rev(llr$y[top])~rev(-llr$x[top]))
#abline(toplm)
#--------------------------
# bottom half of line (horizontal)
#need to find x coords of beginning/end
bottom.start<-c(x=llr$x[bottom][1], y=llr$y[bottom][1]) #this is ~ the corner
bottom.end<-c(x=llr$x[bottom][length(bottom)], y=llr$y[bottom][length(bottom)])
#-------------------------
# find where the llr lines intersect
topcoef<-coef(toplm)
#topexpr <- expression(topcoef[2]*x + topcoef[1])
top.inv.expr <- expression((y-topcoef[1])/topcoef[2])
corner<-c(x=eval(top.inv.expr, list(y=h))[1], y=h)
#--------------------------
# Find the difference between the fits
#--------------------------
#these are dummy points along which to evaluate the difference
bottom.x <- seq(corner[1], bottom.end[1], length.out=50)
top.x<-seq(0,corner[1],length.out=30)
# using assign to set the values of these two variables will allow us to access these variables from the global envir
# without having to return them
assign("topDiff", predict(irb.model,newdata=data.frame(irbx=top.x)) - predict(toplm,newdata=data.frame(topx=top.x)), envir = .GlobalEnv)
assign("bottomDiff", predict(irb.model, newdata=data.frame(irbx=bottom.x))- h, envir = .GlobalEnv)
###################################################
#} ##### End Function make.regressions #############
###################################################
#stop()
#-------------------------
# Call the function
#-------------------------
#make.regressions()
#--------------------------
# Plot the difference
#--------------------------
#load CPI colors
load(file=paste(folder,"CPIcolors.R",sep=''))
#------------------
# make the real plot
#------------------
# may require function-ing the calculations done above"
plot(bottom.x, bottomDiff, col='black',type='l',xlim=c(0,4000), ylim=c(-4,4),xlab="Cost to Government",
     ylab="User interest rate difference (LLR - IRB)",lwd=2)
title(main="When does an LLR reduce user interest rates \n more efficiently than an IRB?", sub = "Positive y-values indicate an LLR is more cost-effective")
legend('bottomleft', fill=c('black', CPIcolors$DarkGrey, CPIcolors$Red), legend=c("25% chance of default","10% chance of default", "40% chance of default"),bty='n')
lines(top.x,topDiff,col='black',lwd=2)
abline(h=0)



loss.index=1
#--------------------------
# Make vectors of data to fit lines to
#--------------------------
##################Begin Function ###################
#make.regressions= function(){
####################################################
llr<-list()
llr$x<--c(gvt.means[2:dim(interest.means)[1],,1,loss.index])
llr$y<-c(interest.means[2:dim(interest.means)[1],,1,loss.index])
irb<-list()
irb$x<--c(gvt.means[1,1,1:dim(interest.means)[3],loss.index])
irb$y<-c(interest.means[1,1,1:dim(interest.means)[3],loss.index])
#--------------------------
# fit a polynomial model to IRB
#--------------------------
#use nls()
# starting params
irby<-irb$y[1:9]
irbx<-irb$x[1:9]
p0 <- -100
p1 <- 1
p2<- 1
irb.model<-nls(irby ~ p0*irbx + p1*(irbx^2) + p2 , start=list(p0=p0,p1=p1,p2=p2))
#summary(irb.model)
# plot(irbx,irby)
# lines(irbx, predict(irb.model, irbx),col="blue")
#--------------------------
# Make a two-part linear fit to LLR
#--------------------------
#--------------------------
# Find top half of line end
h <- min(llr$y)
top <- which(llr$y != h)
bottom <- which(llr$y == h)
#--------------------------
# linear fit to top half of line
topy<-llr$y[top]
topx<-llr$x[top]
toplm<-lm(topy~topx)#lm(formula=rev(llr$y[top])~rev(-llr$x[top]))
#abline(toplm)
#--------------------------
# bottom half of line (horizontal)
#need to find x coords of beginning/end
bottom.start<-c(x=llr$x[bottom][1], y=llr$y[bottom][1]) #this is ~ the corner
bottom.end<-c(x=llr$x[bottom][length(bottom)], y=llr$y[bottom][length(bottom)])
#-------------------------
# find where the llr lines intersect
topcoef<-coef(toplm)
#topexpr <- expression(topcoef[2]*x + topcoef[1])
top.inv.expr <- expression((y-topcoef[1])/topcoef[2])
corner<-c(x=eval(top.inv.expr, list(y=h))[1], y=h)
#--------------------------
# Find the difference between the fits
#--------------------------
#these are dummy points along which to evaluate the difference
bottom.x <- seq(corner[1], bottom.end[1], length.out=50)
top.x<-seq(0,corner[1],length.out=30)
# using assign to set the values of these two variables will allow us to access these variables from the global envir
# without having to return them
assign("topDiff", predict(irb.model,newdata=data.frame(irbx=top.x)) - predict(toplm,newdata=data.frame(topx=top.x)), envir = .GlobalEnv)
assign("bottomDiff", predict(irb.model, newdata=data.frame(irbx=bottom.x))- h, envir = .GlobalEnv)
###################################################
#} ##### End Function make.regressions #############
###################################################
#stop()
#-------------------------
# Call the function
#-------------------------
#make.regressions()
#--------------------------
# Plot the difference
#--------------------------
#load CPI colors
load(file=paste(folder,"CPIcolors.R",sep=''))
lines(bottom.x, bottomDiff, col=CPIcolors$DarkGrey,type='l',xlim=c(0,4000), ylim=c(-4,4),lwd=2)
lines(top.x,topDiff,col=CPIcolors$DarkGrey,lwd=2)
dimnames(gvt.means)



loss.index=7
#--------------------------
# Make vectors of data to fit lines to
#--------------------------
##################Begin Function ###################
#make.regressions= function(){
####################################################
llr<-list()
llr$x<--c(gvt.means[2:dim(interest.means)[1],,1,loss.index])
llr$y<-c(interest.means[2:dim(interest.means)[1],,1,loss.index])
irb<-list()
irb$x<--c(gvt.means[1,1,1:dim(interest.means)[3],loss.index])
irb$y<-c(interest.means[1,1,1:dim(interest.means)[3],loss.index])
#--------------------------
# fit a polynomial model to IRB
#--------------------------
#use nls()
# starting params
irby<-irb$y[1:9]
irbx<-irb$x[1:9]
p0 <- -100
p1 <- 1
p2<- 1
irb.model<-nls(irby ~ p0*irbx + p1*(irbx^2) + p2 , start=list(p0=p0,p1=p1,p2=p2))
#summary(irb.model)
# plot(irbx,irby)
# lines(irbx, predict(irb.model, irbx),col="blue")
#--------------------------
# Make a two-part linear fit to LLR
#--------------------------
#--------------------------
# Find top half of line end
h <- min(llr$y)
top <- which(llr$y != h)
bottom <- which(llr$y == h)
#--------------------------
# linear fit to top half of line
topy<-llr$y[top]
topx<-llr$x[top]
toplm<-lm(topy~topx)#lm(formula=rev(llr$y[top])~rev(-llr$x[top]))
#abline(toplm)
#--------------------------
# bottom half of line (horizontal)
#need to find x coords of beginning/end
bottom.start<-c(x=llr$x[bottom][1], y=llr$y[bottom][1]) #this is ~ the corner
bottom.end<-c(x=llr$x[bottom][length(bottom)], y=llr$y[bottom][length(bottom)])
#-------------------------
# find where the llr lines intersect
topcoef<-coef(toplm)
#topexpr <- expression(topcoef[2]*x + topcoef[1])
top.inv.expr <- expression((y-topcoef[1])/topcoef[2])
corner<-c(x=eval(top.inv.expr, list(y=h))[1], y=h)
#--------------------------
# Find the difference between the fits
#--------------------------
#these are dummy points along which to evaluate the difference
bottom.x <- seq(corner[1], bottom.end[1], length.out=50)
top.x<-seq(0,corner[1],length.out=30)
# using assign to set the values of these two variables will allow us to access these variables from the global envir
# without having to return them
assign("topDiff", predict(irb.model,newdata=data.frame(irbx=top.x)) - predict(toplm,newdata=data.frame(topx=top.x)), envir = .GlobalEnv)
assign("bottomDiff", predict(irb.model, newdata=data.frame(irbx=bottom.x))- h, envir = .GlobalEnv)
lines(bottom.x, bottomDiff, col=CPIcolors$Red,type='l',xlim=c(0,4000), ylim=c(-4,4),lwd=2)
lines(top.x,topDiff,col=CPIcolors$Red,lwd=2)
