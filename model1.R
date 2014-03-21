#----------------------------------------------
#Toy Model for Energy Efficiency Finance Loans
#----------------------------------------------

toymodel = function (input) {
  output = matrix(nrow=nrow(input), ncol = 6, dimnames=list(c(), c("user.NPV","bank.NPV","gvt.cost.NPV","gvt.reserve.size","loan.payment.user","user.interest.yr")))
  
  for(i in 1:nrow(input)){
    #----------------------------------------------
    # Calculate
    #----------------------------------------------
    ## check for reasonable values ##
    if (input$tenor[i] > input$ee.lifetime[i]) print("WARNING: loan tenor is longer than the lifetime of the EE project!")
    
    # account for gvt intervention
    interest.user.yr = input$interest.yr[i] - input$interest.buydown[i]
    loan.amt = input$eecost[i] - input$upfront.rebate[i]
    if(input$loan.loss[i]){
      reserve.size = input$chance.full.loss[i] * loan.amt
    } else {
      reserve.size = 0
    }
    
    #assume monthly loan payment
      interest.mo=input$interest.yr[i]/12
      interest.user.mo = interest.user.yr/12
      bank.discount.mo = input$bank.discount[i]/12
      user.discount.mo = input$user.discount[i]/12
      gvt.discount.mo = input$gvt.discount[i]/12
      npmt = input$tenor[i]*12 # number of payments over tenor of loan
    
    #find present value of loan
    ## bank present value ##
      loan.payment = -pmt(interest.mo, npmt, loan.amt)
      
    
    # account for risk of default
    # assume a uniform chance of default over the whole loan tenor
    # if loan defaults after year one, bank still gets some value in it (in the form of loan payments)
    if(input$loan.loss[i]){
      
      loan.NPV.bank = loan.payment/bank.discount.mo * (1 - (1/((1+bank.discount.mo)^(npmt))))
      # agrees with pv() function. i.e same as
      # loan.NPV.bank = - pv(rate=bank.discount.mo,nper=input$tenor*12, pmt = loan.payment)
      
    }  else {
      
      # chance of default in a given month - assumes uniform distribution
      default.chance.mo = sub.prob(input$chance.full.loss[i],npmt) #ie chance of default in a given payment cycle (month)
      # cumulative chance of default by a given month
      cum.default.chance = (1-(1-default.chance.mo) ^ seq(1,npmt,by=1)) #this makes a list with length 12*tenor=npmt
      
      # PV of each payment made in a given month
      PV.payment  = loan.payment / (1+bank.discount.mo)^seq(1,npmt,by=1) #this is a list with length 12*tenor
      
      # total NPV of expected value of loan repayments
      loan.NPV.bank = sum((1-cum.default.chance) * PV.payment)
      
    }
    
      bank.NPV = loan.NPV.bank - loan.amt
      
    ## user present value ##
      loan.payment.user = loan.amt * interest.user.mo / (1-(1+interest.mo)^-(npmt))
      loan.NPV.user = -1* loan.payment.user/user.discount.mo * (1 - (1/((1+user.discount.mo)^(npmt)))) 
      #print(paste("Loan NPV to user: $", loan.NPV.user,". Montly payments: $", loan.payment))
    
      savings.NPV.user = input$savings.yr[i]/input$user.discount[i] * (1 - (1/((1+input$user.discount[i])^(input$tenor[i])))) 
      #print(paste("savings NPV to user:",savings.NPV.user))
      
      user.NPV = savings.NPV.user + loan.NPV.user + input$upfront.rebate[i]
#!# how does user NPV change if default occurs???     
    
    ## cost to government ##
      gvt.contribution = loan.payment-loan.payment.user #accounts for interest buydown
      buydown.NPV.gvt = -1* gvt.contribution/gvt.discount.mo * (1 - (1/((1+gvt.discount.mo)^(npmt))))
    
      gvt.cost.NPV = buydown.NPV.gvt - input$upfront.rebate[i]
    
    #input to output matrix
    output[i,"user.NPV"]=user.NPV
    output[i,"bank.NPV"]=bank.NPV
    output[i,"gvt.cost.NPV"]=gvt.cost.NPV
    output[i,"gvt.reserve.size"]=reserve.size
    output[i,"loan.payment.user"]=loan.payment.user
    output[i,"user.interest.yr"]=interest.user.yr

  }
#----------------------------------------------
# Output
#----------------------------------------------
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
# calculate probability of an event in a sub-period, 
# given overall rate & number of sub-periods
sub.prob = function (prob, n){
  if(prob>1) {stop("prob is greater than 1, which is breaking the math")}
  r_p =1-((1-prob)^(1/n))
  return(r_p)    
}
