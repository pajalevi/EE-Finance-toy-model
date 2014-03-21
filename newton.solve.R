#-------------------------------------------
# newton.solve.R
# uses newton's method to find the zeros of a function
# created for use in bank_hurdle.R to solve
# for interest rate given a bank hurdle rate
#
# Patricia Levi 3/2014
#
# Created with help from 
# http://www.theresearchkitchen.com/archives/642
#-------------------------------------------

newton.solve = function (f,err.tol=1E-12,x0=.05,N=10,h=.00001){
  # f       for symbolic differentiation is an expression (f=expression(x^3 + 4*x^2 - 10))
  #         IMPORTANT: THE UNKNOWN VARIABLE MUST BE GIVEN AS 'x'
  # err.tol is the error tolerance at which
  #         iteration will stop
  # x0      is the initial guess
  # N       maximum number of iterations
  # h       step size for determining slope (crude 1st derivative)
  #         R also has some symbolic differentiation capabilities...
  
  #initialize step counter 
  i=1 
  # create list of length N to store steps
  p=numeric(N)
  
  # run a max of N iterations
  while(i<=N){
    
    # with symbolic differentiation of f
    # for numerical differentiation, replace with code in comments at bottom
    dydx = D(f,"x")
    df.dx =  eval(dydx, list(x=x0))
    fx0 = eval(f, list(x=x0))
    x1 = (x0 - (fx0/df.dx))
    
    p[i]=x1
    
    # test to see if err.tol has been reached
    if(abs(x1-x0)<err.tol) break #break ends the while loop but not the function
    
    # prepare for next iteration
    x0=x1   
    #print(paste("completed iteration #",i))
    i=i+1
  }
  
  # return the full list of steps
  return(p[1:(i-1)])
  
}


# For numerical differentiation, make sure f is a function, not an expression
# and replace differentiation paragraph with the following:
#
#    df.dx=(f(x0+h)-f(x0))/h
#    x1 = (x0 - (f(x0)/df.dx))
