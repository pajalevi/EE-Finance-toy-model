#----------------------------------------------
# llr-v-irb-plot2.R
# Making graps #2 for model
# showing the difference in consumer interest rate (y)
# for a given level of gvt expenditure (x)
# over a range of loss levels.
# Patricia Levi 05/2014
#
# draws heavily from llr-v-irb-plots.R & it's close relative 
# plot-llr-irb-AlltheLines.R - essentially
# plots the difference between the two lines in llr-v-irb
#----------------------------------------------


#-------------------------
# load saved model data
#-------------------------
  rm(list=ls())
  folder = 'W:\\Research\\Energy Efficiency\\EE Finance toy model\\'
  scenario = 'risky.base'#what set of non-intervention params do you want?
  
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
  gvt.means = tapply(results[,"gvt.cost.NPV"],INDEX=results[,c("LSR","LPCR","interest.buydown","risk.adjust")], FUN=mean)
  interest.means = tapply(results[,"interest.user"],INDEX=results[,c("LSR","LPCR","interest.buydown","risk.adjust")], FUN=mean)
  
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
  loss.index=1
#--------------------------
# Make vectors of data to fit lines to
#--------------------------

###FLIP FLOR IRB AND LLR###
  llr=list()
  llr$x=c(gvt.means[1,1,1,loss.index], gvt.means[2:dim(interest.means)[1],,1,loss.index])
  llr$y=c(interest.means[1,1,1,loss.index],interest.means[2:dim(interest.means)[1],,1,loss.index])

  irb=list()
  irb$x=c(gvt.means[1,1,1,loss.index],gvt.means[1,1,1:dim(interest.means)[3],loss.index])
  irb$y=c(interest.means[1,1,1,loss.index], interest.means[1,1,1:dim(interest.means)[3],loss.index])

#--------------------------
# fit a polynomial model to IRB
#--------------------------

#use nls()

#--------------------------
# Make a two-part linear fit to LLR
#--------------------------

  #--------------------------
  # Find top half of line end
  h = min(llr$y)
  top = which(llr$y != h)
  bottom = which(llr$y == h)

  #--------------------------
  # linear fit to top half of line
    toplm=lm(formula=rev(llr$y[top])~rev(-llr$x[top]))
    #abline(toplm)
  
  #--------------------------
  # bottom half of line (horizontal)
    #need to find x coords of beginning/end
  
#--------------------------
# Find the difference between the fits
#--------------------------


#--------------------------
# Plot the difference
#--------------------------
#load CPI colors
load(file=paste(folder,"CPIcolors.R",sep=''))
