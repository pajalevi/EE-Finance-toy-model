# toy model setup

#source model contents
source('W:\\Research/Energy Efficiency/EE Finance toy model/model1.R')


#----------------------------------------------
# Initialize
# NB: any of the initial settings can be single numbers
#     OR sets of testable numbers (eg. c(1,2,3) or seq(1,3,1))
#----------------------------------------------
### load some pre-made R-copies of excel's finance functions. Thanks, the Internet! ###
### included: NPV(rate, values), 
#             IRR(x, start=0.1), 
#             FV(rate, nper, pmt, pv = 0.0, type = 0), 
#             PV(rate, nper, pmt, fv = 0.0, type = 0), 
#             PMT(rate, nper, pv, fv=0, type=0)
source('W:\\Research/Energy Efficiency/EE Finance toy model/excel_finance_functions.R')

#---------------------
### retrofit info ###
#---------------------
eecost=10000  #cost of energy upgrade ($)

#savings.kWh= 2666 #estimated energy savings per year (kWh/yr)
#cost.kWh=.30  #cost of energy ($/kWh) -- assume third tier is being offset
#savings.yr=savings_kWh*cost_kWh
savings.yr=1000 #yearly savings ($)
ee.lifetime = 15

#savings.stdev=200 #standard dev of expected savings ($)

#---------------------
### financing info ###
#---------------------
tenor =10 #seq(5,15,by=1)#15 #loan tenor
#!# Also need to consider useful lifetime of equiptment (you wouldn't finance it for longer than it is useful)

interest.yr=seq(.02,.10, by=.01) #annual interest rate
loan.frac = 1 #fraction of eecost covered by loan

#---------------------
### other info ###
#---------------------
bank.discount = 0.0515 #cost of capital http://pages.stern.nyu.edu/~adamodar/New_Home_Page/datafile/wacc.htm #.0075 
user.discount = seq(.05,0.30,by=.05) 
## incorporate sensitivity testing
#default.frac=.05 #fraction of consumers expected to default

##Gvt discount rate
gvt.discount = .03 #seq(0.02,0.07,by=.01)

### intervention info ###
interest.buydown = seq(0,0.02,by=0.01) #amount that the gvt will buydown the interest rate
upfront.rebate = eecost * seq(0,0.2,by=0.05)# .20 #20 percent buydown

loan.loss = F
chance.full.loss = 0.05
# cisco devries; "$10m gives about $200m of financing" for PACE.
# frac.guaranteed = .75


#---------------------
### prepare inputs ###
### for multiple values
#---------------------

# use expand.grid, which accepts a list of vectors
inlist = list(eecost = eecost, 
              savings.yr=savings.yr, 
              ee.lifetime = ee.lifetime, 
              tenor=tenor, 
              interest.yr=interest.yr, 
              loan.frac=loan.frac, 
              bank.discount = bank.discount, 
              user.discount = user.discount, 
              gvt.discount = gvt.discount, 
              interest.buydown = interest.buydown, 
              upfront.rebate = upfront.rebate, 
              loan.loss = loan.loss, 
              chance.full.loss = chance.full.loss)
inputs = expand.grid (inlist)

#---------------------
### call the model ###
#---------------------

result = toymodel(inputs)

#---------------------
### to be implemented in the future ###
#---------------------
# partition bit of script into functions
#guarantee.frac = #fraction of loan that is guaranteed
# interest rate buydown
# upfront rebate
#     showing the cost comparison between the above two
# loan-loss reserve
# incorporate risk/cost of customer default

# set-up to create a dataset for x vs y comparison
# ex - how cost to government changes as interest rate buydown changes
#     -** plot cost to gvt on x axis, NPV to user on Y, compare interest rate and upfront rebate
#    - how user NPV changes as user discount changes
#    - how costs change as function of loan tenor
#    - how risk of default changes banks' costs (i.e. importance of knowing if ee is notably less risky)
# output kWh (or $) saved per $ spent by gvt

#change model output to output a dataset, not print it

#---------------------
### print out results ###
#---------------------

#print(as.matrix(result))
#result

#---------------------
### graphing ###
#---------------------
#library(calibrate)
  #---------------------
  ### cost to gvt (x) vs NPV to user for interest rate buydown and upfront rebate
  #---------------------
  ### NB: this will only be meaningful if upfront.rebate and interest.buydown are the ONLY variables that vary.
  if(length(upfront.rebate)>1 | length(interest.buydown)>1){
    xl=range(result[,"gvt.cost.NPV"])
    yl=range(result[,"user.NPV"])
    sel.buydown=which(result[,"upfront.rebate"]==0)
    sel.rebate=which(result[,"interest.buydown"]==0)
    plot(x=result[sel.buydown,"gvt.cost.NPV"],y=result[sel.buydown,"user.NPV"],type='o',xlim=xl,ylim=yl,col=4,xlab="Gvt NPV",ylab="user.NPV")
#!#label individual points with library(calibrate) textxy()
#names=result[sel.buydown,"interest.buydown"]
    points(x=result[sel.rebate,"gvt.cost.NPV"],y=result[sel.rebate,"user.NPV"],type='o',col=2)
    abline(h=0)
    abline(v=0)
    legend('topright',legend=c("interest rate buydown","upfront rebate"),fill=c(4,2))
  }

#--------------------------------
# pair up rebate/buydown pairs by 
# user interest rate & tenor
# compare cost to government
# graph the difference in cost
#---------------------------------
  user.interest.rates=unique(result[,"user.interest.yr"])
  locs=which(result[,"user.interest.yr"]==user.interest.rates)

  #---------------------
  ### tenor (x) vs NPV to user & bank
  #---------------------
  ### NB: this will only be meaningful if tenor is the ONLY variable that varies.
  if(length(tenor)>1){
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
