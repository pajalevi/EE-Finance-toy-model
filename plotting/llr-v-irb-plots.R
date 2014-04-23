#----------------------------------------------
# LLR-v-IRB-plots.R
# contains various ways/attempts to plot up the results
# from modelinit_bank_hurdle.R
# That compare LLR to IRB
# 
# draws on experiments in plotting_bank_hurdle.R
#
# Patricia Levi 04/2014
#----------------------------------------------

# shortcuts:
# run the model
  rm(list=ls())
  source('W:/Research/Energy Efficiency/EE Finance toy model/modelinit_bank_hurdle-runA.R')

# or, load saved model data
  rm(list=ls())
  folder = 'W:\\Research\\Energy Efficiency\\EE Finance toy model\\'
  scenario = 'sensitivityTesting'#what set of non-intervention params do you want?
  run.name='risk-premia' #used to save this set of params for later use/reference, should you desire.
  load(file=paste(folder,"model-runs\\",scenario,"-run-",run.name,sep=''))
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
# Axes:  (ratio of LLR size/expected loss) vs 'break even' gvt cost of capital
# plotted: line showing where IRB cost = LLR cost
# Held constant: user terms, riskiness, discount/hurdle rates
#--------------------------

# show only entries where IRB bought the rate down to zero
#results[IRB & results[,"interest.user"]==0,]

#---------------------------------------------
# Axes: cost to gvt vs consumer's monthly payments
# Plotted: two lines: IRB & LLR
# held constant: all dat other stuff
#---------------------------------------------


# tapply: finding the measn/sd of a given column for each unique combination of the factors in INDEX
gvt.means = tapply(results[,"gvt.cost.NPV"],INDEX=results[,c("LSR","LPCR","interest.buydown","chance.full.loss")], FUN=mean)
userpmt.means = tapply(results[,"loan.payment.user"],INDEX=results[,c("LSR","LPCR","interest.buydown","chance.full.loss")], FUN=mean)
#userpmt.sd = tapply(result[,"loan.payment.user"],INDEX=result[,c("LSR","LPCR","interest.buydown")], FUN=sd)

#!# TODO: Make alternate script that plots consumer interest rate instead of userpmt, in a nicer way than this:
interest.means = tapply(results[,"interest.user"],INDEX=results[,c("LSR","LPCR","interest.buydown","chance.full.loss")], FUN=mean)
userpmt.means=interest.means

#!# For some reason the tapply results return NA for mean loan.payment.user when interest.buydown=interest.bank AND LSR !=0
#!#! fortunately we are not interested in these results, but it is still an issue that would be nice to resolve

# dim 1 is LSR
# dim 2 is LPCR
# dim 3 is interest.buydown
# dim 4 is chance.full.loss
# userpmt.means[1,,,] is the sheet with 0 LSR (i.e. no LLR)
# userpmt.means[,,1,] is the sheet with no IRB
# userpmt.means[2:dim(userpmt.means)[1],,1,] is the sheet with an active LLR

# to extract the data from dimnames, which are the values of the 
# INDEX variables used in tapply
# change the # in double brackets to access a different INDEX variable
#as.numeric(dimnames(userpmt.means)[[2]])) 

##############################################
#plot up interest rate/monthly payment vs cost to gvt
# for irb and llr
#############################################
xl = range(gvt.means[],na.rm=T)
yl = range(userpmt.means[],na.rm=T)


## first plot low range ##
loss.index=1

plot(y= c(userpmt.means[1,1,1,loss.index],userpmt.means[2:dim(userpmt.means)[1],,1,loss.index]), # c() concatenates a singular zero point
     x= c(gvt.means[1,1,1,loss.index], gvt.means[2:dim(userpmt.means)[1],,1,loss.index]), #start with the LLR (IRB = 0 & LSR != 0)
     xlab= "NPV, cost to government ($)",
     ylab= "Consumer's monthly payment ($)",
     pch=1,
     col=1,
     xlim=xl, ylim=yl,
     type='o')

# now plot points with an LLR and without an IRB
points(y= c(userpmt.means[1,1,1,loss.index], userpmt.means[1,1,1:dim(userpmt.means)[3],loss.index]), #doesn't matter what sheet we use in 2nd dim since LPCR is irrelevant
       x= c(gvt.means[1,1,1,loss.index],gvt.means[1,1,1:dim(userpmt.means)[3],loss.index]), 
       pch=4,
       col=2,
       type='o')

#!# somehow not all the lines are connected for IRB line

## now plot high range ##
loss.index=dim(userpmt.means)[[4]]

points(y= c(userpmt.means[1,1,1,loss.index],userpmt.means[2:dim(userpmt.means)[1],,1,loss.index]), # c() concatenates a singular zero point
     x= c(gvt.means[1,1,1,loss.index], gvt.means[2:dim(userpmt.means)[1],,1,loss.index]), #start with the LLR (IRB = 0 & LSR != 0)
     pch=1,
     col=1,
     type='o')

# now plot points with an LLR and without an IRB
points(y= c(userpmt.means[1,1,1,loss.index], userpmt.means[1,1,1:dim(userpmt.means)[3],loss.index]), #doesn't matter what sheet we use in 2nd dim since LPCR is irrelevant
       x= c(gvt.means[1,1,1,loss.index],gvt.means[1,1,1:dim(userpmt.means)[3],loss.index]), 
       pch=4,
       col=2,
       type='o')


## plot acoutrements ##
#legend('topleft', legend = c("Loan Loss Reserve","Interest rate buydowns"), pch=c(1,4),col=c(1,2))
legend('topleft', 
       legend = c(paste("Loan Loss Reserve:\n",min(results[LLR,"LPCR"])*100,"% - ",max(results[LLR,"LPCR"])*100,"% LPCR",sep=''),
                  paste("Interest rate buydown:\n",min(results[,"interest.buydown"])*100,"% - ",
                        round(max(results[,"interest.buydown"])*100,digits=6),"%",sep='')),
       pch=c(1,4),
       col=c(1,2),
       bty='n')

title(main="Comparing costs and effects of IRB and LLR",sub=paste("Tenor=10, Project cost=",results[,"eecost"],sep=''))

#!# TODO: add a subtitle showing the range of expected losses.
#!# TODO: figureout how to plot the midpoint and range (solid line in center, shaded area / dashed lines around it) to show range
          # ofpossibility for interest rage / monthly payment values. may have to use ggplot?

#        axes=F,
#        pch=1)
#   axis(2,at=1:nrow(sel.sit),labels=paste(sel.sit[,"tenor"],"yrs, ",round(sel.sit[,"interest.user"]*100,3),"%",sep=''))
#   axis(1)

##################################################################
# plot the delta in interest rate (LLR-IRB)
# vs cost to gvt
#   NONFUNCTIONAL


## first plot low range of expected loss##
loss.index=1
diff = userpmt.means




xl = range(gvt.means[],na.rm=T)
yl = range(userpmt.means[],na.rm=T)

plot(y= c(userpmt.means[1,1,1,loss.index],userpmt.means[2:dim(userpmt.means)[1],,1,loss.index]), # c() concatenates a singular zero point
     x= c(gvt.means[1,1,1,loss.index], gvt.means[2:dim(userpmt.means)[1],,1,loss.index]), #start with the LLR (IRB = 0 & LSR != 0)
     xlab= "NPV, cost to government ($)",
     ylab= "Consumer's monthly payment ($)",
     pch=1,
     col=1,
     xlim=xl, ylim=yl,
     type='o')

# now plot points with an LLR and without an IRB
points(y= c(userpmt.means[1,1,1,loss.index], userpmt.means[1,1,1:dim(userpmt.means)[3],loss.index]), #doesn't matter what sheet we use in 2nd dim since LPCR is irrelevant
       x= c(gvt.means[1,1,1,loss.index],gvt.means[1,1,1:dim(userpmt.means)[3],loss.index]), 
       pch=4,
       col=2,
       type='o')


####################################################################
# Trying to do the same thing but with ggplot 2 so that it is 
# BEAUTIFUL!
#
# CURRENTLY NONFUNCTIONAL
####################################################################
library(ggplot2)

resultdf = as.data.frame(results)
resultdf.llr=as.data.frame(results[LLR,])
resultdf.irb=as.data.frame(results[IRB,])
p <- qplot(gvt.cost.NPV,interest.user, data=resultdf.llr, color=chance.full.loss) #geom ='line' doesnt work here
q <- qplot(gvt.cost.NPV,interest.user, data=resultdf.irb, color=chance.full.loss)

 ## work in progress ##



abline(lm(userpmt.means[1,1,1:dim(userpmt.means)[3],loss.index] ~ gvt.means[1,1,1:dim(userpmt.means)[3],loss.index]), col='blue')

