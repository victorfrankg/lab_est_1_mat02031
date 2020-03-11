#FMQO
f.mqo = function(X,y){
  aux.x = solve(t(X)%*%X)
  bh = aux.x%*%t(X)%*%y # beta chapeu
  yh = X%*%bh # y predito
  eh = y - yh # residuo
  n = nrow(X); k = ncol(X)
  sig2 = sum(eh^2)/(n-k)
  dp.b = sqrt(sig2*diag(aux.x))
  # Teste t de significancia individual
  est.t = bh/dp.b
  valor.p.t = 2*(1 - pt(abs(est.t),df=n-k,low=T))
  A = data.frame(bh,dp.b,est.t,valor.p.t)
  row.names(A) = paste("b",0:(k-1),sep="")
  # Teste t robusto a heteroc. 
  Z = as.vector(eh)*X
  pao = aux.x
  recheio = t(Z)%*%Z
  var.b.rob = pao%*%recheio%*%pao
  dp.b.rob = sqrt(diag(var.b.rob))
  est.t.rob = bh/dp.b.rob
  valor.p.t.rob = 2*(1 - pt(abs(est.t.rob),df=n-k,low=T))
  A.rob = data.frame(bh,dp.b.rob,est.t.rob,valor.p.t.rob)
  row.names(A.rob) = paste("b",0:(k-1),sep="")
  
  # Teste F significancia conjunta
  sqt = sum((y-mean(y))^2)
  sqr = sum(eh^2)
  sqe = sum((yh-mean(y))^2) # mean(y) == mean(yh)
  r2 = 1 - sqr/sqt
  est.F = (r2/(k-1))/((1-r2)/(n-k))
  valor.p.F = pf(est.F,df1=k-1,df2=n-k,low=F)
  B = data.frame(est.F,valor.p.F)
  b1 = c(sqe,sqr,sqt)
  b2 = c(k-1,n-k,n-1)
  B1 = cbind(b1,b2)
  dimnames(B1) = list(c("regressao","residuos","total"),c("SQ","gl"))
  r2.aj = 1 - (sqr/(n-k))/(sqt/(n-1))
  
  
  ## Analise dos residuos ##
  C1 = matrix(0,nrow = n,ncol = 6)
  hii = C1[,6] = diag(X%*%aux.x%*%t(X))
  sig2.i = ((n-k)*sig2 - eh^2/(1-hii))/(n-k-1)
  
  C1[,1] = eh
  C1[,2] = eh/sqrt(sig2)
  C1[,3] = res.stu = eh/sqrt(sig2*(1-hii))
  C1[,4] = eh/(1-hii)
  C1[,5] = r.stu = eh/sqrt(sig2.i*(1-hii))
  colnames(C1) = c("res","res.pad","res.Stu",
                   "PRESS","R-Student","hii")
  
  ## Medidas de alavancagem e influencia ##
  
  R1 = solve(t(X)%*%X)%*%t(X)
  diag.aux = diag(R1%*%t(R1))
  aux1 = matrix(r.stu/sqrt((1-hii)),nrow=n,ncol=k,byrow=F)
  aux2 = matrix(diag.aux,nrow=n,ncol=k,byrow=T)
  df.betas = t(R1)*aux1/sqrt(aux2)
  
  C2 = matrix(0,nrow = n,ncol = 4+k)
  C2[,1] = hii
  C2[,2] = D.cook = (res.stu^2*hii)/((k)*(1-hii))
  C2[,3:(k+2)] = df.betas
  C2[,(k+3)] = r.stu*sqrt(hii/(1-hii))
  C2[,(k+4)] = (sig2.i/sig2)^(k)/(1-hii)
  colnames(C2) = c("diag(H)","Cook's D",paste("DFbeta.",seq(from=0,to=k-1,by=1),sep=""),"DFfits",
                   "COVratio")
  
  
  return(list("coef"=bh,"fitted"=yh,
              "residuos"=eh,"summary"=A,"summary.rob"=A.rob,
              "Teste F"= B,"ANOVA"=B1,
              "R2"=r2,"R2.aj"=r2.aj,"sig2"=sig2,"analise.res"=C1,"analise.influencia"=C2))
}
