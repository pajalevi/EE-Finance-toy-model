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
rm(list=ls())
source('W:/Research/Energy Efficiency/EE Finance toy model/modelinit_bank_hurdle.R')


#-------------------------
# Try to make Jeff's graph showing under what conditions
# an LLR is less expensive than an IRB, for the same user terms
#
# Axes:  (ratio of LLR size/expected loss) vs 'break even' gvt cost of capital
# plotted: line showing where IRB cost = LLR cost
# Held constant: user terms, riskiness, discount/hurdle rates
#--------------------------

# show only entries where IRB bought the rate down to zero
results[IRB & results[,"interest.user"]==0,]

#---------------------------------------------
# Axes: cost to gvt vs consumer's monthly payments
# Plotted: two lines: IRB & LLR
# held constant: all dat other stuff
#---------------------------------------------

# tapply: finding the measn/sd of a given column for each unique combination of the factors in INDEX
gvt.means = tapply(results[,"gvt.cost.NPV"],INDEX=results[,c("LSR","LPCR","interest.buydown")], FUN=mean)
userpmt.means = tapply(results[,"loan.payment.user"],INDEX=results[,c("LSR","LPCR","interest.buydown")], FUN=mean)
#userpmt.sd = tapply(result[,"loan.payment.user"],INDEX=result[,c("LSR","LPCR","interest.buydown")], FUN=sd)

#!# For some reason the tapply results return NA for mean loan.payment.user when interest.buydown=interest.bank AND LSR !=0
#!#! fortunately we are not interested in these results, but it is still an issue that would be nice to resolve

# dim 1 is LSR
# dim 2 is LPCR
# dim 3 is interest.buydown
# userpmt.means[1,,] is the sheet with 0 LSR (i.e. no LLR)
# userpmt.means[,,1] is the sheet with no IRB
# userpmt.means[2:dim(userpmt.means)[1],,1] is the sheet with an active LLR

# to extract the data from dimnames, which are the values of the 
# INDEX variables used in tapply
# change the # in double brackets to access a different INDEX variable
#as.numeric(dimnames(userpmt.means)[[2]])) 

#plot up this information somehow
xl = range(gvt.means[],na.rm=T)
yl = range(userpmt.means[],na.rm=T)

plot(y= userpmt.means[2:dim(userpmt.means)[1],,1], 
     x= gvt.means[2:dim(userpmt.means)[1],,1], #start with the LLR (IRB = 0 & LSR != 0)
     xlab= "NPV, cost to government ($)",
     ylab= "Consumer's monthly payment ($)",
     pch=1,
     col=1,
     xlim=xl, ylim=yl,
     type='p')
# now plot points with an LLR and without an IRB
points(y= userpmt.means[1,1,2:dim(userpmt.means)[3]], #doesn't matter what sheet we use in 2nd dim since LPCR is irrelevant
       x= gvt.means[1,1,2:dim(userpmt.means)[3]], 
       pch=4,
       col=2,
       type='p')
#legend('topleft', legend = c("Loan Loss Reserve","Interest rate buydowns"), pch=c(1,4),col=c(1,2))
legend('topleft', 
       legend = c(paste("Loan Loss Reserve:\n",min(results[LLR,"LPCR"])*100,"% - ",max(results[LLR,"LPCR"])*100,"% LPCR",sep=''),
                  paste("Interest rate buydown:\n",min(results[,"interest.buydown"])*100,"% - ",
                        round(max(results[,"interest.buydown"])*100,digits=6),"%",sep='')),
       pch=c(1,4),
       col=c(1,2),
       bty='n')

title(main="Comparing costs and effects of IRB and LLR",sub=paste("Tenor=10, Project cost=",results[,"eecost"],sep=''))

#        axes=F,
#        pch=1)
#   axis(2,at=1:nrow(sel.sit),labels=paste(sel.sit[,"tenor"],"yrs, ",round(sel.sit[,"interest.user"]*100,3),"%",sep=''))
#   axis(1)
