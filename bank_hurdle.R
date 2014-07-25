#----------------------------------------------
# bank_hurdle.R
# Output of this model is loan terms given by a 
# bank to meet their hurdle rate given a
# certain type of loan and incentive structure
#
# this script is initialized by
# modelinit_bank_hurdle.R
#
# This script is modified from model1.R
#
# Patricia Levi 03/2014
#----------------------------------------------

bankmodel = function (input) {
  output = matrix(nrow=nrow(input), ncol = 13, dimnames=list(c(), c("expected.loss.pct","expected.loss","interest.user","interest.bank","loan.payment.user",
                                                                   "loan.payment.bank","simple.payback.yrs","user.NPV","bank.NPV","gvt.cost.NPV",
                                                                   "gvt.reserve.size","gvt.llr.oppcost","buydown.cost")))
  
  #----------------------------------------------#
  # Iterate through all the rows of input matrix #
  #----------------------------------------------#
  for(i in 1:nrow(input)){
    ## check for reasonable values ##
    if (input$tenor[i] > input$ee.lifetime[i]) print("WARNING: loan tenor is longer than the lifetime of the EE project!")
    
    
    # Modify loan terms according to gvt intervention
    loan.amt = input$eecost[i] - input$upfront.rebate[i]
    
    #----------------------------------#
    # assume monthly loan payment &    #
    # calculate monthly rates          #
    #----------------------------------#
    bank.hurdle.mo = mo.rate(input$bank.hurdle[i])
    user.discount.mo = mo.rate(input$user.discount[i])
    gvt.discount.mo = mo.rate(input$gvt.discount[i]) # used for LLR opportunity cost
    npmt = input$tenor[i]*12 # number of payments over tenor of loan    
    
    #--------------------------#
    # Back out Interest Rate   #
    # to bank and then to user #
    # given bank NPV = 0       #
    #--------------------------#
      
      #-------------------------------------------------------------#
      # find P(default) and then the probability that the bank gets # 
      # paid by either the bank or the LLR (= ev.pmt)               #
      # since this is expected value, I can treat LSR               #
      # (the fraction the bank must pay) as a proability            #
      #-------------------------------------------------------------#
      k = seq(1,npmt,by=1)
    
      ## calclate p(d) ##
      # chance of default in a given month
      # assume a uniform chance of default over the whole loan tenor
      default.chance.mo = sub.prob(input$chance.full.loss[i],npmt) #ie chance of default in a given payment cycle (month)
      # cumulative chance of no default by a given month
      no.default.chance = (1-default.chance.mo) ^ k #this makes a list with length 12*tenor=npmt
    
    # for bookkeeping, what % of total value is expected to be lost?    
    expected.loss = sum((1-no.default.chance)*((1-input$recovery[i]) *(1-input$LSR[i])))/(npmt)
    expected.loss.noLLR = sum((1-no.default.chance)*(1-input$recovery[i]))/(npmt)
      
      # loan pool coverage ratio (LPCR) - if expected.loss > LPCR --> problems!
      # Attempt to factor this in the the small.LLR adjustment
#!# small.LLR is the fraction of the expected loss that the LLR will cover. it is used to adjust LSR
#!# this is a kind of clunky way to represent undersized LLR
        small.LLR = 1
        if (input$LPCR[i] < expected.loss.noLLR){ 
          #warning("LPCR is less than expected loss! The pool will be exhausted!") 
          small.LLR = input$LPCR[i] / expected.loss.noLLR
        }
      
      # expected value of payment = P(no defaults) + P(LLR OR (Loan Security/Recovery) pays bank AND the user defaults)
     ev.pmt = no.default.chance + ((1-no.default.chance) * (1-(1-input$recovery[i]) *(1-(input$LSR[i]* small.LLR))))
#!#      # ASSUMPTION: if recovery is 40%, and LSR is 80%, gvt makes bank whole for 80% of outstanding balance [leaving the bank with 12% loss] (pari passu)
              # Credit Enhancement Overview Guide wording: 
              # LSR = "the percentage of a lender's loss on each customer default that they can recover." 
          
      #---------------------------#
      # find Loan Payment to bank #
      #---------------------------#
      # calculate conversion factor from PV to loan payment 
      # from k=1 to =n, Sum of (1-(1-p(d))^k)(1+bank hurdle rate)^-k
#!# Add, to hurdle rate in this case, (1-ev.pmt)* risk.premium.factor to account for risk
#!# but you wouldn't use that risky discount rate to calculate IRB payments, for example.
#!# need to think carefully about when to use the risk-free and risky rate
      bank.risk.premium = input$risk.adjust[i]*(1-ev.pmt)

      discount.stream = (1+mo.rate(input$bank.hurdle[i] + bank.risk.premium))^-k
      EV.NPV.factor = sum(ev.pmt * discount.stream) # i.e. expected value & NPV conversion

      loan.payment = loan.amt / EV.NPV.factor
  

      #---------------------------------------#
      # solve for interest rate given to bank
      # given loan.payment & PV=loan.amt
      # loan.payment = 
      #     (interest.rate * loan.amt)/(1-(1+interest.rate)^-n)
      # use newton.solve.R   
      #---------------------------------------#
      fx = expression(((x * loan.amt)/(1-(1+x)^-npmt))-loan.payment)
      interest.rate.mo = tail(newton.solve(f=fx,loan.amt=loan.amt, loan.payment=loan.payment,npmt=npmt),n=1)
      interest.rate = yr.rate(interest.rate.mo)
      
      
    #-------------------------------------------#
    # Calculate user interest rate,             #
    # loan payment, (would differ w/ interest   #
    # rate buydown), and net present value,     #
    # account for interest-rate buydown,        #
    # upfront rebate                            #
    #-------------------------------------------#

    # disallow interest rate buydowns that would result in a negative interest rate for the user
#!# This might mess with the linearity of our graphs...
#!# comment out the else block (and the if/else lines) to allow negative interest rates
#    if(interest.rate - input$interest.buydown[i] >= 0){
      interest.user = interest.rate - input$interest.buydown[i]
#    } else {
#       interest.user=0
#       # uncomment if you'd like to display the ACTUAL buydown, 
#       # not the buydown that was initially set
#       input$interest.buydown[i] = interest.rate
#     }

    interest.user.mo = mo.rate(interest.user)
    loan.payment.user = - pmt(interest.user.mo,nper=npmt,pv=loan.amt)
    # pmt returns NaN if interest = 0; catch that & replace
    if(is.nan(loan.payment.user)){ 
      loan.payment.user=loan.amt/npmt
    }
      
    loan.NPV.user = loan.amt - loan.payment.user/user.discount.mo * (1 - (1/((1+user.discount.mo)^(npmt)))) 
    #print(paste("Loan NPV to user: $", loan.NPV.user,". Montly payments: $", loan.payment))
    
    savings.NPV.user = input$savings.yr[i]/input$user.discount[i] * (1 - (1/((1+input$user.discount[i])^(input$tenor[i])))) 
    #print(paste("savings NPV to user:",savings.NPV.user))
    
    user.NPV = savings.NPV.user + loan.NPV.user + input$upfront.rebate[i] - input$eecost[i]

    #calculate years for a simple payback period
    simple.payback = loan.amt/input$savings.yr[i]
    
    #------------------------------#
    # Calculate cost to government #
    #------------------------------#

      #-----------------------------#
      # find loan.loss reserve size #
      #-----------------------------#
      reserve.size = loan.amt * input$LSR[i] * input$LPCR[i]
      
      #-------------------
      # INTEREST RATE BUYDOWN COST
      # appropriate discount rate is the bank's required interest rate (i.e. discount rate)
      # b/c IRB is typically done as an upfront lump sum.
      #-------------------
      gvt.contribution = loan.payment-loan.payment.user
      buydown.NPV.gvt = -1* gvt.contribution/interest.rate.mo * (1 - (1/((1+interest.rate.mo)^(npmt))))

      #-------------------
      # LOAN LOSS RESERVE COST
      # incorporate opportunity cost of LLR into gvt NPV
      # assume that alternative would be to invest the LLR
      # in a set of projects that pay interest at rate r, monthly
#      pmts = reserve.size*gvt.discount.mo
#      llr.opp.cost = pmts * (1-(1+gvt.discount.mo)^-npmt)*gvt.discount.mo #discounted stream of interest on the llr
      
    
    gvt.cost.NPV = buydown.NPV.gvt - input$upfront.rebate[i] - reserve.size #- llr.opp.cost 
     
    
    #---------------------------------#
    # SANITY CHECK                    #
    # Calculate bank NPV, should = 0  #
    #---------------------------------#
    #loan.NPV.bank = loan.payment*EV.NPV.factor  # guaranteed to result in 0 bank NPV
    
    loan.NPV.bank = loan.payment.user*EV.NPV.factor - buydown.NPV.gvt # accounts for zero risk of IRB payment

    bank.NPV = loan.NPV.bank - loan.amt
        
    
    #------------------------------#
    # Fill out  output matrix      #
    #------------------------------#    
    d=4
    output[i,"expected.loss.pct"]=expected.loss
    output[i,"expected.loss"]=.01*expected.loss*loan.amt
    output[i,"user.NPV"]=round(user.NPV,digits=d)
    output[i,"bank.NPV"]=round(bank.NPV,digits=d)
    output[i,"gvt.cost.NPV"]=round(gvt.cost.NPV,digits=d)
    output[i,"gvt.reserve.size"]=round(reserve.size,digits=d)
 #   output[i,"gvt.llr.oppcost"]=llr.opp.cost
    output[i,"interest.user"]=round(interest.user*100,digits=d)
    output[i,"interest.bank"]=round(interest.rate*100,digits=d)
    output[i,"loan.payment.user"]=round(loan.payment.user,digits=d)
    output[i,"loan.payment.bank"]=round(loan.payment,digits=d)
    output[i,"simple.payback.yrs"]=simple.payback
    output[i,"buydown.cost"]=round(buydown.NPV.gvt,digits=d)

  }

#-----------------#
# Prepare output  #
#-----------------#
  output = cbind(input,output)
  # cost to government
  
  # chance of user default 
  
  # expected return for bank
  
  #cat(paste("Net present value to user is \n $", round(user.NPV,2),"\n"))
  #cat(paste("Net present value to bank is \n $", round(bank.NPV,2),"\n"))
  #cat(paste("Net present value of cost to gvt is \n $", round(gvt.cost.NPV,2),"\n"))
  return(output)
  
  # print out initialization info
  # provide year 1 payment & bill savings. (?)
}

#--------------------------------#
# Helper functions for the model #
#--------------------------------#
  #--------------------------------#
  # calculate probability of an event in a sub-period, 
  # given overall rate & number of sub-periods
  #--------------------------------#
  sub.prob = function (prob, n){
    if(prob>1) {stop("prob is greater than 1, which is breaking the math")}
    r_p =1-((1-prob)^(1/n))
    return(r_p)    
  }

  #--------------------------------#
  # deal with compounding interest #
  #--------------------------------#
  # calculate the monthly rate from yearly 
   mo.rate = function (yr.rate,n=12){
#     #if(yr.rate>1) {stop("yearly rate is greater than 100%, which is breaking the math")}
#     monthly = ((1+yr.rate)^(1/n))-1
#     return(monthly)
      return(yr.rate/n)
   }
#   
#   #calculate compounded yearly rate from nominal monthly rate
  yr.rate = function (mo.rate, n=12){
#     if(mo.rate>1) {stop("monthly rate is greater than 100%, which is breaking the math")}
#     yearly =((1+mo.rate)^n)-1
#     return(yearly)    
    return(mo.rate*n)
  }
#   
