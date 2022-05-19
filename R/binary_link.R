binaryOutcome<-function(mu){
  outpt=function(x){
    m=length(x)
    p=exp(mu+x)/(1+exp(mu+x))
    return(rbinom(m,1,p))
  }
  return(outpt)
}

survivalOutcome<-function(mu,beta,accrual,followUp){
   outpt=function(x){
     m=length(x)
     surv=rexp(m,rate=exp(mu+beta*x))
     cens=runif(m,min=followUp,max=accrual+followUp)
     return(
       data.frame(survival=ifelse(surv<=cens,surv,cens),
                  censor=ifelse(surv<=cens,1,0)))
   }
   return(outpt)
}
binaryLink<-function(x){
  m=length(x)
  outfcn=function(parm2){
    #bx=parm2[m+1]+parm2[m+1]*parm2[1:m]
    bx=parm2[m+1]+parm2[1:m]
    loglik=sum(x*bx)-sum(log(1+exp(bx)))
    return(loglik)
  }
  return(outfcn)
}

coxLink<-function(data){
  surv=data[,1]
  cens=data[,2]
  survOrder=order(surv,-1*cens,runif(length(surv)),
                  decreasing=TRUE)
  events=(cens==1)[survOrder]
  m=length(surv)
  outfcn=function(parm2){
    bx=parm2[m+1]+parm2[survOrder]
    cbx=log(cumsum(exp(bx)))
    return(sum(bx[events]+cbx[events]))
  }
  return(outfcn)
}


