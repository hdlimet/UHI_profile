regress_cal=function (x,...) {
  if (all(is.na(x))) {
    f1=NA
    f2=NA
    f3=NA
  } else if (length(which(!is.na(x)))<5) {
    f1=NA
    f2=NA
    f3=NA
  } else {
    f1=cor(x[which(!is.na(x))],c(2003:2018)[which(!is.na(x))],method="pearson") 
    f2=cor.test(x[which(!is.na(x))],c(2003:2018)[which(!is.na(x))])$p.value
    f3=coef(lm(formula=var~year,data=data.frame(var=x[which(!is.na(x))],year=c(2003:2018)[which(!is.na(x))])))[2]
  }
  return(c(f1,f2,f3))
}