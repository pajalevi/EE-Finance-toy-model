
#----------------------------------------------
# plotting_bank_hurdle.R
# contains various ways/attempts to plot up the results
# from modelinit_bank_hurdle.R
#
#
# Patricia Levi 03/2014
#----------------------------------------------
rm(list=ls())
source('W:/Research/Energy Efficiency/EE Finance toy model/modelinit_bank_hurdle.R')
results


#---------------------
### cost to gvt (x) vs NPV to user for interest rate buydown and upfront rebate
#---------------------
### NB: this will only be a straight line / meaningful if upfront.rebate and interest.buydown are the ONLY variables that vary.
#library(calibrate)
if(length(upfront.rebate)>1 | length(interest.buydown)>1){
  xl=range(result[,"gvt.cost.NPV"])
  yl=range(result[,"user.NPV"])
  sel.buydown=which(result[,"upfront.rebate"]==0)
  sel.rebate=which(result[,"interest.buydown"]==0)
  plot(x=result[sel.buydown,"gvt.cost.NPV"],y=result[sel.buydown,"user.NPV"],type='p',xlim=xl,ylim=yl,col=4,xlab="Gvt NPV",ylab="User NPV")
  #!#label individual points with library(calibrate) textxy()
  #names=result[sel.buydown,"interest.buydown"]
  points(x=result[sel.rebate,"gvt.cost.NPV"],y=result[sel.rebate,"user.NPV"],type='p',col=2)
  abline(h=0)
  abline(v=0)
  legend('topright',legend=c("interest rate buydown","upfront rebate"),fill=c(4,2))
}


### cost to user discount rate vs cost to gvt -- all for a loan with the same 
if(length(upfront.rebate)>1 | length(interest.buydown)>1){
  yl=range(result[,"gvt.cost.NPV"])
  xl=range(result[,"user.discount"])
  sel.buydown=which(result[,"upfront.rebate"]==0)
  sel.rebate=which(result[,"interest.buydown"]==0)
  
  plot(y=result[sel.buydown,"gvt.cost.NPV"],x=result[sel.buydown,"user.discount"],type='p',xlim=xl,ylim=yl,col=4,
       ylab="Net Present Value of Cost to Gvt",xlab="User Discount Rate")
  
  points(y=result[sel.rebate,"gvt.cost.NPV"],x=result[sel.rebate,"user.discount"],type='p',col=2)
  
  abline(h=0)
  abline(v=0)
  legend('topright',legend=c("interest rate buydown","upfront rebate"),fill=c(4,2))
  
}

#-----------------------
# ATTEMPT 1:compare costs of different interventions
#------------------------


#select out rows with only buydown OR rebate
sel.buydown=which(result[,"upfront.rebate"]==0)
sel.rebate=which(result[,"interest.buydown"]==0)

combo.rebate=unique(result[sel.rebate,c("interest.user","tenor")])
combo.buydown=unique(result[sel.buydown,c("interest.user","tenor")])

#create a list of lists of model runs that have the same user interest/tenor
# for the rebate intervention and the buydown intervention
rebate.index=list()
for(i in 1:dim(combo.rebate)[[1]]){
  rebate.index[[length(rebate.index)+1]]=(which(result[,"interest.user"]==combo.rebate[i,"interest.user"] & result[,"tenor"]==combo.rebate[i,"tenor"]))
}

buydown.index=list()
for(i in 1:dim(combo.buydown)[[1]]){
  buydown.index[[length(buydown.index)+1]]=(which(result[,"interest.user"]==combo.buydown[i,"interest.user"] & result[,"tenor"]==combo.buydown[i,"tenor"]))
}

# for each set of interventions, compile some summary stats on the 
# cost to gvt, discount rates, etc, that result in the user terms

# find average (and sd) of cost to government
new = numeric(dim(combo.buydown)[[1]])
combo.buydown=cbind(combo.buydown,new,new)
dimnames(combo.buydown)[[2]]=c(dimnames(combo.buydown)[[2]][1:2],"cost.gvt.avg","cost.gvt.sd")
for(i in 1:dim(combo.buydown)[[1]]){
  combo.buydown[i,"cost.gvt.avg"]=mean(result[buydown.index[[i]],"gvt.cost.NPV"])
  combo.buydown[i,"cost.gvt.sd"]=sd(result[buydown.index[[i]],"gvt.cost.NPV"])
}
#-------------------------------------------------------------
# ATTEMPT 2
# for each unique situation, compile some summary stats on the 
# cost to gvt, discount rates, etc, that result in the user terms

sel.sit=unique(result[,c(1:12)]) # holding everying except intervention and NPV constant
# 1:10 are inputs except for buydown or rebates (i.e. gvt interventions)
vary=which(dim(table(sel.sit))!=1) #which columns of sel.sit vary?
print(dimnames(result)[[2]][vary]) # get the names of the varying columns

# use tapply to aggregate using multiple factors identified by vary 
gvt.means = tapply(result[,"gvt.cost.NPV"],INDEX=result[,vary], FUN=mean)
userpmt.means = tapply(result[,"loan.payment.user"],INDEX=result[,vary], FUN=mean)
userpmt.sd = tapply(result[,"loan.payment.user"],INDEX=result[,vary], FUN=sd)

###############################################
# could in theory develop a plotting system
# that would respond dynamically to any 
# set of varying factors
# but thats not going to be worth my time right now
##############################################

#---------
# for varying rebate, buydown, tenor, and gvt discount rate
gvt.means = tapply(result[,"gvt.cost.NPV"],INDEX=result[,c(4,8,11,12)], FUN=mean)
userpmt.means = tapply(result[,"loan.payment.user"],INDEX=result[,c(4,8,11,12)], FUN=mean)

yl=range(userpmt.means[3,,,])
xl=range(gvt.means[3,,,])
plot(y= userpmt.means[3,,,1], #tenor=15, no upfront rebate
     x= gvt.means[3,,,1], #start with the upfront rebate
     xlab= "NPV, cost to government ($)",
     ylab= "Consumer's monthly payment ($)",
#     xlim= xl, ylim=yl,
     pch=1,
     col=1)
points(y= userpmt.means[3,,1,], #tenor=15, no interest buydown
       x= gvt.means[3,,1,], 
       pch=4,
       col=2)
par(cex.sub=.6)
title(main="Upfront rebates reduce monthly \n payments more efficiently",sub=paste("Tenor=15 yrs, Project cost=$",result[,"eecost"],
                                                                                   ". G'vt discount rate range:",min(result[,"gvt.discount"]),
                                                                                   "-",max(result[,"gvt.discount"]),sep=''))
legend('topleft', 
       legend = c(paste("Interest rate buydown:\n",min(result[,"interest.buydown"])*100,"% - ",max(result[,"interest.buydown"])*100,"%",sep='')
                             ,paste("Upfront rebate:\n",min(result[,"upfront.rebate"]/result[,"eecost"])*100,"% - ",max(result[,"upfront.rebate"]/result[,"eecost"])*100,"% of cost",sep='')), 
       pch=c(1,4),
       col=c(1,2),
       bty='n')



#----------------------------------------------------------------
# ATTEMPT 2.5: For variance in tenor, interest rate buydown, and upfront rebate
#----------------------------------------------------------------
gvt.means = tapply(result[,"gvt.cost.NPV"],INDEX=result[,c("tenor","interest.user","upfront.rebate")], FUN=mean)
userpmt.means = tapply(result[,"loan.payment.user"],INDEX=result[,c("tenor","interest.user","upfront.rebate")], FUN=mean)
userpmt.sd = tapply(result[,"loan.payment.user"],INDEX=result[,c("tenor","interest.user","upfront.rebate")], FUN=sd)

#userpmt.means[,,1] is the sheet with no upfront rebate
#userpmt.means[,dim(userpmt.means)[2],] is the sheet with no interest rate buydown
# dim 1 is tenor
# dim 2 is interest.user
# dim 3 is upfront.rebate
#as.numeric(dimnames(userpmt.means)[[2]])) # to extract the data from dimnames

#plot up this information somehow
plot(y= userpmt.means[3,,1], #tenor=15, no upfront rebate
     x= gvt.means[3,,1], #start with the upfront rebate
     xlab= "NPV, cost to government ($)",
     ylab= "Consumer's monthly payment ($)",
     pch=1,
     col=1,
     type='o')
points(y= userpmt.means[3,dim(userpmt.means)[2],], #tenor=15, no interest buydown
       x= gvt.means[3,dim(userpmt.means)[2],], 
       pch=4,
       col=2,
       type='o')
legend('topleft', legend = c("Interest rate buydowns","Upfront rebates"), pch=c(1,4),col=c(1,2))
title(main="Upfront rebates reduce monthly \n payments more efficiently",sub=paste("Tenor=15, Project cost=",result[,"eecost"],sep=''))

#        axes=F,
#        pch=1)
#   axis(2,at=1:nrow(sel.sit),labels=paste(sel.sit[,"tenor"],"yrs, ",round(sel.sit[,"interest.user"]*100,3),"%",sep=''))
#   axis(1)

#----------------------------------------------------------------
# VERSION 4: different axes!! gvt.NPV v consumer NPV
# varying USER discount rate (result[,7])
#----------------------------------------------------------------
gvt.means = tapply(result[,"gvt.cost.NPV"],INDEX=result[,c(4,7,11,12)], FUN=mean)
usernpv.means = tapply(result[,"user.NPV"],INDEX=result[,c(4,7,11,12)], FUN=mean)

yl=range(c(usernpv.means[3,,1,],usernpv.means[3,,,1]))
xl=range(c(gvt.means[3,,,1],gvt.means[3,,1,]))
plot(y= usernpv.means[3,,,1], #tenor=15, no upfront rebate
     x= gvt.means[3,,,1], #start with the upfront rebate
     xlab= "NPV, cost to government ($)",
     ylab= "Consumer's NPV ($)",
     xlim= xl, ylim=yl,
     pch=1,
     col=1)
points(y= usernpv.means[3,,1,], #tenor=15, no interest buydown
       x= gvt.means[3,,1,], 
       xlim= xl, ylim=yl,
       pch=4,
       col=2)
par(cex.sub=.6)
title(main="Upfront rebates increase user \n value more efficiently",sub=paste("Tenor=15 yrs, Project cost=$",result[,"eecost"],
                                                                                   ". User discount rate range:",min(result[,"user.discount"]),
                                                                                   "-",max(result[,"user.discount"]),sep=''))
legend('topright', 
       legend = c(paste("Interest rate buydown:\n",min(result[,"interest.buydown"])*100,"% - ",max(result[,"interest.buydown"])*100,"%",sep='')
                  ,paste("Upfront rebate:\n",min(result[,"upfront.rebate"]/result[,"eecost"])*100,"% - ",max(result[,"upfront.rebate"]/result[,"eecost"])*100,"% of cost",sep='')), 
       pch=c(1,4),
       col=c(1,2),
       bty='o')

#---------------------
### tenor (x) vs NPV to user & bank
#---------------------
### NB: this will only be meaningful if tenor is the ONLY variable that varies.
if(length(tenor)>1 & F){
  xl=range(result[,"tenor"])
  yl=range(result[,c("user.NPV","bank.NPV")])
  plot(x=result[,"tenor"],y=result[,"user.NPV"],type='o',xlim=xl,ylim=yl,col=4)
  #!#label individual points with library(calibrate) textxy()
  #names=result[sel.buydown,"interest.buydown"]
  points(x=result[,"tenor"],y=result[,"bank.NPV"],type='o',col=2)
  abline(h=0)
  abline(v=0)
  abline(v=(eecost/savings.yr),col="dark green")
  legend('bottomright',legend=c("user NPV","bank NPV","simple payback"),fill=c(4,2,"dark green"))
}
