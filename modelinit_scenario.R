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
# This script sources an R object ('scenario' variable, object name is 'inlist') that contains the 
# information concerning the retrofit, tenor, discount rates, & risk-related information - i.e. 
# everything EXCEPT the governmental intervention
# 
#
# Patricia Levi 03/2014
#----------------------------------------------

#-----------------------------------#
# To allow for re-testing, you can
# load an existing scenario, or
# you can make a new scenario
# using scenario-maker.R
#---------------------------------- #

folder = 'W:\\Research\\Energy Efficiency\\EE Finance toy model\\'
scenario = 'risk.range.base' #what set of params do you want?

run='' #used to uniquely identify this run, if the scenario is used multiple times.

#source model code
source(paste(folder,'bank_hurdle.R',sep=''))

# source newtons method solver & excel function
source(paste(folder,'newton.solve.R',sep=''))
source(paste(folder,'excel_finance_functions.R',sep=''))
### loads some pre-made R-copies of excel's finance functions. Thanks, the Internet! ###
### included: NPV(rate, values), 
#             IRR(x, start=0.1), 
#             FV(rate, nper, pmt, pv = 0.0, type = 0), 
#             PV(rate, nper, pmt, fv = 0.0, type = 0), 
#             PMT(rate, nper, pv, fv=0, type=0)


#-------------------#
# Initialize Inputs #
#-------------------#
# load scenario values - ie. everything 
# scenarios created by scenario-maker.R
README='NA' # in case the scenario does not have a README
load(paste(folder,'scenarios\\',scenario,".R",sep=''))


#----------------------------------------------
# after loading the scenario, you could edit any part of it by uncommenting the relevant line
# and changing the input. Make sure 
#
# NB: any of the initial settings can be single numbers
#     OR sets of testable numbers (eg. c(1,2,3) or seq(1,3,1))
#----------------------------------------------
  #---------------#
  # retrofit info #
  #---------------#
#     inlist$eecost=13000  #cost of energy upgrade ($) - energy upgrade CA agv according to Zimring
#     inlist$savings.yr=2000 #yearly savings ($)
#     inlist$ee.lifetime = 15
  
  #----------------#
  # financing info #
  #----------------#
#     inlist$tenor = 100# seq(5,15,by=5)#15 #loan tenor
  
  #---------------#
  # discount info #
  #---------------#
#     inlist$bank.hurdle =seq(.04,.12,by=.01)#cost of capital http://pages.stern.nyu.edu/~adamodar/New_Home_Page/datafile/wacc.htm #.0075 
#     inlist$user.discount = .15 #seq(.05,.25,by=.02)
#     inlist$gvt.discount = .04#seq(.01,.05,by=.01) 
      
  #------------#
  # risky info #
  #------------#
#      inlist$risk.adjust =.4#c(.3,.4,.5) # as in (Amato, J., 2005) this number is used as follows
#!#                 # additional return (basis pts) required to compensate for risk = expected loss(i.e. 1-ev.pmt) * risk.adjust
    # same source gives the .4 number
#     inlist$chance.full.loss = .15#seq(.05,.3,by=.05) # i.e. chance of defaulting by end of loan tenor. 13% ~= 4% expected loss
#   inlist$recovery = .4 #pct of loan that is recovered on default
  
  #-------------------#
  # intervention info #
  #-------------------#
    # different types of interventions are separated by a "\n # \n"
    #
#     inlist$interest.buydown = c(0,seq(0.01,0.15,by=0.01)) #amount that the gvt will buydown the interest rate
    #
#     inlist$upfront.rebate = 0#eecost * c(0,.05)#seq(0,0.2,by=0.05)# .20 #20 percent buydown
    #
    #TO TURN OFF LOAN LOSS RESERVE, SET LSR=0
#       inlist$LPCR = seq(0,.2,by=.01)#c(.02,.03,.04,.05,.06,.07) # loan pool coverage ratio. usually around 5-10%
    # cisco devries; "$10m gives about $200m of financing" for PACE.
#      inlist$LSR = c(0,.9) #loss-share ratio, usually ~90%. 0% --> no LLR

  #-------------------#
  # if you change the scenario
  # add to README
  # and change the scenario name or 
  # the run ID
  #-------------------#
#   README=c(README,"text")
#   scenario= ''
#   run = ''


#--------------------#
# prepare inputs     #
# for multiple values#
#--------------------#

inputs = expand.grid (inlist)


#--------------------#
### call the model ###
#--------------------#

results = bankmodel(inputs)

#--------------------#
# save model run params
# and outputs
#--------------------#
run.name=paste(scenario,run,sep='') 
save(list=c('inlist','README','inputs','results','scenario'), file=paste(folder,"model-runs\\",run.name,".R",sep=''))


# create some indices for viewing different subsets of the results
LLR = results[,"LSR"]>0 & results[,"upfront.rebate"]==0 & results[,"interest.buydown"]==0
IRB = results[,"LSR"]==0 & results[,"upfront.rebate"]==0 & results[,"interest.buydown"]>0
rebate = results[,"LSR"]==0 & results[,"interest.buydown"]==0
nothing = results[,"LSR"]==0 & results[,"upfront.rebate"]==0 & results[,"interest.buydown"]==0
no.loss=results[,"chance.full.loss"]==0
no.recovery = results[,"recovery"]==0


