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
folder = 'W:\\Research\\Energy Efficiency\\EE Finance toy model\\scenarios\\'
scenario = 'mass.save'
#describe what this scenario is for
README= "Taken from Brown 2011 description of Mass Save Residential HEAT program, 
 which buys down interest rates from mkt rate (Prime + 1%, w/ floor of 5%) to 0%.
 Given: Avg loan size, max tenor (7 yrs), IRB cost, 
 default rate (<1% after 5 yrs, <.5% according to Hayes 2011a),
 and bank hurdle rate is given a floor of 5%"

#----------------------------------------------
# NB: any of the initial settings can be single numbers
#     OR sets of testable numbers (eg. c(1,2,3) or seq(1,3,1))
#----------------------------------------------

#---------------#
# retrofit info #
#---------------#
eecost=8080  #cost of energy upgrade ($) - energy upgrade CA agv according to Zimring
savings.yr=2000 #yearly savings ($)
ee.lifetime = 15

#----------------#
# financing info #
#----------------#
tenor = 7# seq(5,15,by=5)#15 #loan tenor
# not really in use currently
#    loan.frac = 1# fraction of eecost covered by loan     

#---------------#
# discount info #
#---------------#
bank.hurdle = .05#.0503#cost of capital http://pages.stern.nyu.edu/~adamodar/New_Home_Page/datafile/wacc.htm, January 2014
user.discount = .15 #seq(.05,.25,by=.02)
gvt.discount = .04#seq(.01,.05,by=.01) 

#------------#
# risky info #
#------------#
risk.adjust =.4 # as in (Amato, J., 2005) this number is used as follows
#!#                 # additional return (basis pts) required to compensate for risk = expected loss(i.e. 1-ev.pmt) * risk.adjust
# same source gives the .4 number
chance.full.loss = .01 # i.e. chance of defaulting by end of loan tenor. 13% ~= 4% expected loss
recovery = .4 #pct of loan that is recovered on default


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

save(list=c('inlist','README'), file=paste(folder,scenario,".R",sep=''))

