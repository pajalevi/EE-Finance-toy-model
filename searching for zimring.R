# testing for the right params (chance.full.loss, bank.hurdle, risk.adjust) to give Zimring outputs

# which combinations give the correct buydown cost?
sel =which(abs(results[IRB,"buydown.cost"]+2500) == min(abs(results[IRB,"buydown.cost"]+2500)))
results[which(IRB)[sel],]
# bank hurdle = .07, chance.full.loss=.1, risk.adjust = .3
# > which(IRB)[sel]
# [1] 284 319 354 389
# buydown.cost = -2499.167
# BUT bank interest = 8%, user interest = 3%.

# which combination gives the correct buydown cost?
sel =which(abs(results[LLR,"gvt.reserve.size"]-2500) == min(abs(results[LLR,"gvt.reserve.size"]-2500)))
unique(results[which(LLR)[sel],"gvt.reserve.size"])
# > which(LLR)[sel]
# [1]  246  247  248  249  250  251  252  253  254  255  256  257  258  259  260  261  262  263  264  265  266  267  268  269  270  271  272  273  274  275
# [31]  276  277  278  279  280 1366 1367 1368 1369 1370 1371 1372 1373 1374 1375 1376 1377 1378 1379 1380 1381 1382 1383 1384 1385 1386 1387 1388 1389 1390
# [61] 1391 1392 1393 1394 1395 1396 1397 1398 1399 1400 2486 2487 2488 2489 2490 2491 2492 2493 2494 2495 2496 2497 2498 2499 2500 2501 2502 2503 2504 2505
# [91] 2506 2507 2508 2509 2510 2511 2512 2513 2514 2515 2516 2517 2518 2519 2520
# LPCR = .2
# > unique(results[which(LLR)[sel],"gvt.reserve.size"])
# [1] 2080
#!# not big enough!

# only LPCR&LSR affects the size of the LLR


# which combination gives the correct interest rate (14.99%) w.o intervention?
sel =which(abs(results[nothing,"interest.bank"]-14.99) == min(abs(results[nothing,"interest.bank"]-14.99)))
results[which(nothing)[sel],]
# interest.user = 14.9132
# > which(nothing)[sel]
# [1] 1149 1184 1219 1254
# bank.hurdle = .07, chance.full.loss = 0.35,  risk.adjust = .4

# which combination gives the correct interest rate (9.99) w/ IRB? Should be the same as above by definition
sel =which(abs(results[IRB,"interest.user"]-9.99) == min(abs(results[IRB,"interest.user"]-9.99)))
results[which(IRB)[sel],]
# bank.hurdle = .07, chance.full.loss = .35, risk.adjust = .4
# buydown cost is 2163.023

# which combinations get close to correct interest rate
sel =which(abs(results[IRB,"interest.user"]-9.99)<2)
unique(results[which(IRB)[sel],"interest.user"])

unique(results[IRB,"interest.user"])

x=which(results[IRB,"interest.user"]==unique(results[IRB,"interest.user"])[5])
results[which(IRB)[x],]
results[which(IRB)[which(results[IRB,"interest.user"]==unique(results[IRB,"interest.user"])[4])],]


#-------------------------------------
#re-creating interest rate calculations
interest.user=.097
interest.rate=.1499

interest.user.mo = mo.rate(interest.user)
interest.rate.mo=mo.rate(interest.rate)


loan.payment.user = - pmt(interest.user.mo,nper=npmt,pv=loan.amt)
gvt.contribution = loan.payment-loan.payment.user
buydown.NPV.gvt = -1* gvt.contribution/interest.rate.mo * (1 - (1/((1+interest.rate.mo)^(npmt))))


loan.payment = -pmt(interest.mo, nper=npmt, pv=13000)

plot(x=results[IRB,"interest.user"],y=results[IRB,"gvt.cost.NPV"])
abline(h=-2280)
abline(v=9.7)
