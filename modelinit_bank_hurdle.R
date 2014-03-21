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
    savings.yr=1300 #yearly savings ($)
    #ee.lifetime = 15
    
  #----------------#
  # financing info #
  #----------------#
    tenor = seq(5,15,by=5)#15 #loan tenor
    loan.frac = 1# fraction of eecost covered by loan     
    chance.full.loss = 0.05 # i.e. default chance

  #------------#
  # other info #
  #------------#
    bank.hurdle = 0.0515 #cost of capital http://pages.stern.nyu.edu/~adamodar/New_Home_Page/datafile/wacc.htm #.0075 
    user.discount = seq(.05,.25,by=.02)
    gvt.discount = seq(.01,.03,by=.01) 

  #-------------------#
  # intervention info #
  #-------------------#
    # different types of interventions are separated by a "\n # \n"
    #
    interest.buydown = seq(0,0.03,by=0.005) #amount that the gvt will buydown the interest rate
    #
    upfront.rebate = eecost * seq(0,0.2,by=0.02)# .20 #20 percent buydown
    #
    loan.loss = c(T,F) #c(F,T)
    LPCR = .05 # loan pool coverage ratio. usually around 5-10%
    # cisco devries; "$10m gives about $200m of financing" for PACE.
    LSR = .90 #loss-share ratio, usually ~90%

#--------------------#
# prepare inputs     #
# for multiple values#
#--------------------#

# use expand.grid, which accepts a list of vectors
inlist = list(eecost = eecost, 
              savings.yr=savings.yr, 
              ee.lifetime = ee.lifetime, 
              tenor=tenor, 
              loan.frac=loan.frac, 
              bank.hurdle = bank.hurdle, 
              user.discount = user.discount, 
              gvt.discount = gvt.discount, 
              loan.loss = loan.loss,
              LPCR = LPCR,
              LSR = LSR,
              chance.full.loss = chance.full.loss,
              interest.buydown = interest.buydown, 
              upfront.rebate = upfront.rebate)

inputs = expand.grid (inlist)

#--------------------#
### call the model ###
#--------------------#

result = bankmodel(inputs)

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

