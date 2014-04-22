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

folder = 'W:\\Research\\Energy Efficiency\\EE Finance toy model\\'
scenario = 'mass.save'#what set of non-intervention params do you want?
run.name='A' #used to save this set of params for later use/reference, should you desire.

# load scenario values - ie. everything except gvt intervention specifics
# scenarios created by scenario-maker.R
load(paste(folder,'scenarios\\',scenario,".R",sep=''))

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
#----------------------------------------------
# NB: any of the initial settings can be single numbers
#     OR sets of testable numbers (eg. c(1,2,3) or seq(1,3,1))
#----------------------------------------------
# NB: retrofit, financing, discount, and risk info are set in scenario-maker.R
    
  #-------------------#
  # intervention info #
  #-------------------#
    # different types of interventions are separated by a "\n # \n"
    #
    interest.buydown = c(.05,.06)#c(0,seq(0.01,0.15,by=0.01)) #amount that the gvt will buydown the interest rate
    #
    upfront.rebate = 0#eecost * c(0,.05)#seq(0,0.2,by=0.05)# .20 #20 percent buydown
    #
    #TO TURN OFF LOAN LOSS RESERVE, SET LSR=0
    LPCR = .10#c(.02,.03,.04,.05,.06,.07) # loan pool coverage ratio. usually around 5-10%
    # cisco devries; "$10m gives about $200m of financing" for PACE.
    LSR = c(0,.9) #loss-share ratio, usually ~90%. 0% --> no LLR

#--------------------#
# prepare inputs     #
# for multiple values#
#--------------------#
# attach intervention information to the scenario inlist
inlist$LPCR = LPCR
inlist$LSR = LSR
inlist$interest.buydown = interest.buydown
inlist$upfront.rebate = upfront.rebate

# use expand.grid, which accepts a list of vectors

inputs = expand.grid (inlist)


#--------------------#
### call the model ###
#--------------------#

results = bankmodel(inputs)

#--------------------#
# save model run params
# and outputs
#--------------------#
save(list=c('inlist','README','inputs','results','scenario'), file=paste(folder,"model-runs\\",scenario,"-run-",run.name,sep=''))


# create some indices for viewing different subsets of the results
LLR = results[,"LSR"]>0 & results[,"upfront.rebate"]==0 & results[,"interest.buydown"]==0
IRB = results[,"LSR"]==0 & results[,"upfront.rebate"]==0 & results[,"interest.buydown"]>0
rebate = results[,"LSR"]==0 & results[,"interest.buydown"]==0
nothing = results[,"LSR"]==0 & results[,"upfront.rebate"]==0 & results[,"interest.buydown"]==0
no.loss=results[,"chance.full.loss"]==0
no.recovery = results[,"recovery"]==0


