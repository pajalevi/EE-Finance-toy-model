#----------------------------------------------
# modelinit_bank_hurdle.R
# Output of this model is loan terms given by a 
# bank to meet their hurdle rate given a
# certain type of loan and incentive structure
#
# this script initializes the model inputs and
# calls bank_hurdle.R
#
# This script is modified from model_initialize.R
#
# Patricia Levi 03/2014
#----------------------------------------------


#source model contents
source('W:\\Research\\Energy Efficiency\\EE Finance toy model\\bank_hurdle.R')

# source newtons method solver
source('W:/Research/Energy Efficiency/EE Finance toy model/newton.solve.R')

source('W:\\Research\\Energy Efficiency\\EE Finance toy model\\excel_finance_functions.R')
### loads some pre-made R-copies of excel's finance functions. Thanks, the Internet! ###
### included: NPV(rate, values), 
#             IRR(x, start=0.1), 
#             FV(rate, nper, pmt, pv = 0.0, type = 0), 
#             PV(rate, nper, pmt, fv = 0.0, type = 0), 
#             PMT(rate, nper, pv, fv=0, type=0)


#-------------------#
# Initialize Inputs #
#-------------------#
#----------------------------------------------
# NB: any of the initial settings can be single numbers
#     OR sets of testable numbers (eg. c(1,2,3) or seq(1,3,1))
#----------------------------------------------
  
  #---------------#
  # retrofit info #
  #---------------#
    eecost=13000  #cost of energy upgrade ($) - energy upgrade CA agv according to Zimring
    savings.yr=2000 #yearly savings ($)
    ee.lifetime = 15
    
  #----------------#
  # financing info #
  #----------------#
    tenor = 10# seq(5,15,by=5)#15 #loan tenor
    # not really in use currently
#    loan.frac = 1# fraction of eecost covered by loan     

  #---------------#
  # discount info #
  #---------------#
    bank.hurdle = .1499#cost of capital http://pages.stern.nyu.edu/~adamodar/New_Home_Page/datafile/wacc.htm #.0075 
    user.discount = .15 #seq(.05,.25,by=.02)
    gvt.discount = .05 #seq(.01,.03,by=.01) 

  #------------#
  # risky info #
  #------------#
    risk.adjust =c(.1,.4)#c(0,.4) # as in (Amato, J., 2005) this number is used as follows
    #!#                   # additional return (basis pts) required to compensate for risk = expected loss(i.e. 1-ev.pmt) * risk.adjust
    # same source gives the .4 number
    chance.full.loss = c(.13,.40) # i.e. chance of defaulting by end of loan tenor
    recovery = c(.9,.4) #pct of loan that is recovered on default
    
  #-------------------#
  # intervention info #
  #-------------------#
    # different types of interventions are separated by a "\n # \n"
    #
    interest.buydown = 0#c(0,(14.99-9.7)/100)#seq(0,0.03,by=0.01) #amount that the gvt will buydown the interest rate
    #
    upfront.rebate = 0#eecost * c(0,.05)#seq(0,0.2,by=0.05)# .20 #20 percent buydown
    #
    #TO TURN OFF LOAN LOSS RESERVE, SET LSR=0
    LPCR = .1908 # loan pool coverage ratio. usually around 5-10%
    # cisco devries; "$10m gives about $200m of financing" for PACE.
    LSR = c(0,.8) #loss-share ratio, usually ~90%

#--------------------#
# prepare inputs     #
# for multiple values#
#--------------------#

# use expand.grid, which accepts a list of vectors
inlist = list(eecost = eecost, 
              savings.yr=savings.yr, 
              ee.lifetime = ee.lifetime, 
              tenor=tenor, 
#              loan.frac=loan.frac, 
              bank.hurdle = bank.hurdle, 
              user.discount = user.discount, 
              gvt.discount = gvt.discount, 
              chance.full.loss = chance.full.loss,
              recovery=recovery,
              LPCR = LPCR,
              LSR = LSR,
              interest.buydown = interest.buydown, 
              upfront.rebate = upfront.rebate,
              risk.adjust=risk.adjust)

inputs = expand.grid (inlist)

#--------------------#
### call the model ###
#--------------------#

results = bankmodel(inputs)

# create some indices for viewing different subsets of the results
LLR = results[,"LSR"]>0 & results[,"upfront.rebate"]==0 & results[,"interest.buydown"]==0
IRB = results[,"LSR"]==0 & results[,"upfront.rebate"]==0
rebate = results[,"LSR"]==0 & results[,"interest.buydown"]==0
nothing = results[,"LSR"]==0 & results[,"upfront.rebate"]==0 & results[,"interest.buydown"]==0
no.loss=results[,"chance.full.loss"]==0
no.recovery = results[,"recovery"]==0


#-------------------------------------#
### to be implemented in the future ###
#-------------------------------------#
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

#!# Figure out how hurdle rate is some combo of riskiness and discount rate...
#!# so it would sort of account for part of the change_full_loss stuff??

# Tenor may be a model output?

# incorporate uncertainty in energy savings

