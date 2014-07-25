#-------------------------------------#
# premiaSensitivity.R
# makes plots showing the sensitivity
# of our results to changes in risk.adjust
# drawn from llr-v-irb-plots.R
# Patricia Levi 7/2014
#-------------------------------------#

# load saved model data
rm(list=ls())
folder = 'W:\\Research\\Energy Efficiency\\EE Finance toy model\\'
scenario = 'highRisk.newPremia'#what set of non-intervention params do you want?

run.name='-premiaSensitivity' #if there are multiple runs using this scenario

load(file=paste(folder,"model-runs\\",scenario,run.name,".R",sep=''))
# contains list=c('inlist','README','inputs','results','scenario')

# need to re-create indexing variables
LLR = results[,"LSR"]>0 & results[,"upfront.rebate"]==0 & results[,"interest.buydown"]==0
IRB = results[,"LSR"]==0 & results[,"upfront.rebate"]==0 & results[,"interest.buydown"]>0
rebate = results[,"LSR"]==0 & results[,"interest.buydown"]==0
nothing = results[,"LSR"]==0 & results[,"upfront.rebate"]==0 & results[,"interest.buydown"]==0
no.loss=results[,"chance.full.loss"]==0
no.recovery = results[,"recovery"]==0


#load CPI colors
load(file=paste(folder,"CPIcolors.R",sep=''))
CPIcolors$lightRed="#a07675"
CPIcolors$midRed="#a05c59"
reds=c(CPIcolors$lightRed,CPIcolors$midRed,CPIcolors$Red,CPIcolors$Red,CPIcolors$midRed)
CPIcolors$lightOrange= "#d6a68b"
CPIcolors$midOrange= "#d68e64"

oranges=c(CPIcolors$lightOrange,CPIcolors$midOrange,CPIcolors$Orange,CPIcolors$Orange,CPIcolors$midOrange)


# show only entries where IRB bought the rate down to zero
#results[IRB & results[,"interest.user"]==0,]

#---------------------------------------------
# Axes: cost to gvt vs consumer's monthly payments
# Plotted: two lines: IRB & LLR
# held constant: all dat other stuff
#---------------------------------------------

# tapply: finding the measn/sd of a given column for each unique combination of the factors in INDEX
gvt.means = tapply(results[,"gvt.cost.NPV"],INDEX=results[,c("LSR","LPCR","interest.buydown","risk.adjust")], FUN=mean)
userpmt.means = tapply(results[,"loan.payment.user"],INDEX=results[,c("LSR","LPCR","interest.buydown","risk.adjust")], FUN=mean)
#userpmt.sd = tapply(result[,"loan.payment.user"],INDEX=result[,c(" LSR","LPCR","interest.buydown")], FUN=sd)

#!# TODO: Make alternate script that plots consumer interest rate instead of userpmt, in a nicer way than this:
interest.means = tapply(results[,"interest.user"],INDEX=results[,c("LSR","LPCR","interest.buydown","risk.adjust")], FUN=mean)
userpmt.means=interest.means

##############################################
#plot up interest rate/monthly payment vs cost to gvt
# for irb and llr
#############################################

xl = c(0,-range(gvt.means[],na.rm=T)[1]-4000) #flip-flop x axis direction
yl = c(0,range(userpmt.means[],na.rm=T)[2]+1)
par(mar=c(8,4,4,2)+0.1, xaxs="i", yaxs="i")
plot(y=NA,x=NA,
     xlab= "Cost to Government ($)",
     ylab= "Consumer's interest rate (%)",
     pch=1,
     xlim=xl, ylim=yl,
     axes=F)

for(i in c(1,4,5)){
  points(y= c(userpmt.means[1,1,1,i],userpmt.means[2:dim(userpmt.means)[1],,1,i]), # c() concatenates a singular zero point
       x= -c(gvt.means[1,1,1,i], gvt.means[2:dim(userpmt.means)[1],,1,i]), #starts with the IRB
       pch=i,
       col=oranges[i],
       type='o',
       lwd=2,
       cex=.5)
  
  # now plot points with an IRB and without an LLR
  points(y= c(userpmt.means[1,1,1,i], userpmt.means[1,1,1:dim(userpmt.means)[3],i]), #doesn't matter what sheet we use in 2nd dim since LPCR is irrelevant
         x= -c(gvt.means[1,1,1,i],gvt.means[1,1,1:dim(userpmt.means)[3],i]), 
         pch=i,
         col=reds[i],
         type='o',
         lwd=2,
         cex=.5)
  print(i)
}

axis(side=1, at=seq(from=xl[1],to=xl[2],by=1000))
axis(side=2, at=seq(from=yl[1],to=yl[2],by=2))

#!# somehow not all the lines are connected for IRB line


## plot acoutrements ##
#legend('topleft', legend = c("Loan Loss Reserve","Interest rate buydowns"), pch=c(1,4),col=c(1,2))
legend('topright', 
       legend = c(paste("Loan Loss Reserve:\n",min(results[LLR,"LPCR"])*100,"% - ",max(results[LLR,"LPCR"])*100,"% LPCR",sep=''),
                  #                  paste("Interest rate buydown:\n",min(results[,"interest.buydown"])*100,"% - ",
                  "Interest Rate Buydown",
                  "Risk adjustment values: \n.5, 1.4, and 2"),
       #                  round(max(results[,"interest.buydown"])*100,digits=6),"%",sep='')),
       #       pch=c(1,4),
       lwd=2,
       col=c(CPIcolors$Orange,CPIcolors$Red,"white"),
       bty='n')

title(main="Higher risk adjustment differentiates LLR and IRB")
title(sub=paste("Tenor=",results[,"tenor"],", Project cost=",results[,"eecost"],". Expected loss:",round(max(results[,"expected.loss.pct"]),digits=4)*100,"%",
                ".\n RunID:",scenario,run.name,sep=''),
      line=5)
