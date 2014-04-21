#--------------------------------------------------
# scenario-maker.R
# creates an object 'inlist' and saves it
# for later use by modelinit_bank_hurdle.R
# specifying everything about a run except the 
# governmental intervention.
#--------------------------------------------------

#-------------------------------
# where to save this scenario? 
#-------------------------------
folder = 'W:\\Research\\Energy Efficiency\\EE Finance toy model\\'
scenario = 'base.case.R'

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
bank.hurdle = .0515#cost of capital http://pages.stern.nyu.edu/~adamodar/New_Home_Page/datafile/wacc.htm #.0075 
user.discount = .15 #seq(.05,.25,by=.02)
gvt.discount = .04#seq(.01,.05,by=.01) 

#------------#
# risky info #
#------------#
risk.adjust =.4 # as in (Amato, J., 2005) this number is used as follows
#!#                 # additional return (basis pts) required to compensate for risk = expected loss(i.e. 1-ev.pmt) * risk.adjust
# same source gives the .4 number
chance.full.loss = .13 # i.e. chance of defaulting by end of loan tenor. 13% ~= 4% expected loss
recovery = .4 #pct of loan that is recovered on default

#-------------------#
# intervention info #
#-------------------#
# different types of interventions are separated by a "\n # \n"
#
interest.buydown = c(0,seq(0.01,0.15,by=0.01)) #amount that the gvt will buydown the interest rate
#
upfront.rebate = 0#eecost * c(0,.05)#seq(0,0.2,by=0.05)# .20 #20 percent buydown
#
#TO TURN OFF LOAN LOSS RESERVE, SET LSR=0
LPCR = c(.02,.03,.04,.05,.06,.07) # loan pool coverage ratio. usually around 5-10%
# cisco devries; "$10m gives about $200m of financing" for PACE.
LSR = c(0,.8) #loss-share ratio, usually ~90%. 0% --> no LLR


#---------------------------#
# save inputs so that the   #
# scenario can be re-used   #
#---------------------------#
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
              risk.adjust=risk.adjust)

save(list=inlist, file=paste(folder,scenario,sep=''))
