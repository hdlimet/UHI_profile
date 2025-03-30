atc_fit=function (x,...) {
  if (all(is.na(x))) {
    f1=NA
    f2=NA
    f3=NA
  }else{
    nyear=length(x)/365
    sel=which(x>(-100) & x<100)  ## remove outline
    if (!length(sel) | length(sel)<10) {
      f1=NA
      f2=NA
      f3=NA
    } else {
      data_sum=data.frame(doy=rep(1:365,nyear)[sel],lst=x[sel])  
      start <- list(cm=20,ca=20,cp=200)
      lower=c(0,0,150)
      upper=c(40,40,250)
      atc <- nlsLM(lst~cm+ca*sin(2*pi*doy/365-2*pi/365*cp+0.5*pi),data=data_sum,start=start,
                   lower=lower,upper=upper,algorithm = "LM",control = nls.lm.control(maxiter = 50))
      f1=coef(atc)[1]
      f2=coef(atc)[2]
      f3=coef(atc)[3]
    }
  }
  return(c(f1,f2,f3))
}