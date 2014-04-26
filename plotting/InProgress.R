# plotting scripts in progress


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
