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

# v3: adapted from plot2, attempting to show the merits of the combination of IRB and LLR.



# load saved model data
  rm(list=ls())
  folder = 'W:\\Research\\Energy Efficiency\\EE Finance toy model\\'
  scenario = 'risky.base.newPremia'#what set of non-intervention params do you want?
  
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

    no.rebate = results[,"upfront.rebate"]==0


#-------------------------
# Try to make Jeff's graph showing under what conditions
# an LLR is less expensive than an IRB, for the same user terms
#
# Axes:  (ratio of LLR size/expected loss) vs 'break even' gvt cost of capital
# plotted: line showing where IRB cost = LLR cost
# Held constant: user terms, riskiness, discount/hurdle rates
#--------------------------
#load CPI colors
load(file=paste(folder,"CPIcolors.R",sep=''))

# show only entries where IRB bought the rate down to zero
#results[IRB & results[,"interest.user"]==0,]

#---------------------------------------------
# Axes: cost to gvt vs consumer's monthly payments
# Plotted: two lines: IRB & LLR
# held constant: all dat other stuff
#---------------------------------------------

# tapply: finding the measn/sd of a given column for each unique combination of the factors in INDEX
gvt.means = tapply(results[,"gvt.cost.NPV"],INDEX=results[,c("LSR","LPCR","interest.buydown")], FUN=mean)
userpmt.means = tapply(results[,"loan.payment.user"],INDEX=results[,c("LSR","LPCR","interest.buydown")], FUN=mean)
#userpmt.sd = tapply(result[,"loan.payment.user"],INDEX=result[,c(" LSR","LPCR","interest.buydown")], FUN=sd)

#!# TODO: Make alternate script that plots consumer interest rate instead of userpmt, in a nicer way than this:
interest.means = tapply(results[,"interest.user"],INDEX=results[,c("LSR","LPCR","interest.buydown")], FUN=mean)
userpmt.means=interest.means

#!# For some reason the tapply results return NA for mean loan.payment.user when interest.buydown=interest.bank AND LSR !=0
#!#! fortunately we are not interested in these results, but it is still an issue that would be nice to resolve

# dim 1 is LSR
# dim 2 is LPCR
# dim 3 is interest.buydown
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
xl = c(0,-range(gvt.means[],na.rm=T)[1]-4000) #flip-flop x axis direction
yl = c(0,range(userpmt.means[],na.rm=T)[2]+1)

par(mar=c(8,4,4,2)+0.1, xaxs="i", yaxs="i")

plot(y= c(userpmt.means[1,1,1],userpmt.means[2:dim(userpmt.means)[1],,1]), # c() concatenates a singular zero point
     x= -c(gvt.means[1,1,1], gvt.means[2:dim(userpmt.means)[1],,1]), #starts with the IRB
     xlab= "Cost to Government ($)",
     ylab= "Consumer's interest rate (%)",
     pch=1,
     col=CPIcolors$Orange,
     xlim=xl, ylim=yl,
     type='o',
     lwd=2,
     axes=F)

# now plot points with an IRB and without an LLR
points(y= c(userpmt.means[1,1,1], userpmt.means[1,1,1:dim(userpmt.means)[3]]), #doesn't matter what sheet we use in 2nd dim since LPCR is irrelevant
       x= -c(gvt.means[1,1,1],gvt.means[1,1,1:dim(userpmt.means)[3]]), 
       pch=4,
       col=CPIcolors$Red,
       type='o',
       lwd=2)

# now plot points with an combo of IRB and LLR -- somehow.
###############################
lpcr.dim=15 #pick this to be the LPCR at which the LLR achieves max interest rate reductions
###############################
points(y= c(userpmt.means[2,1:lpcr.dim,1],userpmt.means[2,lpcr.dim,1:dim(userpmt.means)[3]]), #doesn't matter what sheet we use in 2nd dim since LPCR is irrelevant
       x= -c(gvt.means[2,1:lpcr.dim,1],gvt.means[2,lpcr.dim,1:dim(userpmt.means)[3]]), 
       pch=4,
       col=CPIcolors$Green,
       type='o',
       lwd=2)

axis(side=1, at=seq(from=xl[1],to=xl[2],by=1000))
axis(side=2, at=seq(from=yl[1],to=yl[2],by=2))

#!# somehow not all the lines are connected for IRB line


## plot acoutrements ##
#legend('topleft', legend = c("Loan Loss Reserve","Interest rate buydowns"), pch=c(1,4),col=c(1,2))
legend('topright', 
       legend = c(paste("Loan Loss Reserve:\n",min(results[LLR,"LPCR"])*100,"% - ",max(results[LLR,"LPCR"])*100,"% LPCR",sep=''),
#                  paste("Interest rate buydown:\n",min(results[,"interest.buydown"])*100,"% - ",
                  "Interest Rate Buydown","Combined Policies"),
#                  round(max(results[,"interest.buydown"])*100,digits=6),"%",sep='')),
#       pch=c(1,4),
        lwd=2,
       col=c(CPIcolors$Orange,CPIcolors$Red,CPIcolors$Green),
       bty='n')

title(main="Impact of Gov't Spending via IRB and LLR")
 title(sub=paste("Tenor=",results[,"tenor"],", Project cost=",results[,"eecost"],". Expected loss:",round(max(results[,"expected.loss.pct"]),digits=4)*100,"%",
                 ".\n RunID:",scenario,run.name,sep=''),
       line=5)

#!# TODO: add a subtitle showing the range of expected losses.
#!# TODO: figureout how to plot the midpoint and range (solid line in center, shaded area / dashed lines around it) to show range
          # ofpossibility for interest rage / monthly payment values. may have to use ggplot?

#        axes=F,
#        pch=1)
#   axis(2,at=1:nrow(sel.sit),labels=paste(sel.sit[,"tenor"],"yrs, ",round(sel.sit[,"interest.user"]*100,3),"%",sep=''))
#   axis(1)

############################
# where do the lines intersect?



stop("worked!")

# for base.case
text(labels="-- LPCR = 31.5 % \n        IRB = 7 %",x=(3300)+70,y=6.8791+.05, cex=.7, adj = 0, srt = 45)
# for current graph, intersects at ~ LPCR=13.5%
#LLR
userpmt.means[2:dim(userpmt.means)[1],,1]
gvt.means[2:dim(userpmt.means)[1],,1]

#IRB - only visible up to 9% buydown
userpmt.means[1,1,1:dim(userpmt.means)[3]]
gvt.means[1,1,1:dim(userpmt.means)[3]]

     
