# funzione per calcolare il p-value


z_test<-function(media,mi0,dev_std,n,tailed){
  
  z<-(media-mi0)/(dev_std/sqrt(n))
  
  if(tailed==1 | tailed==2) return(cat('Z-Score',z,'P-value',tailed*pnorm(-abs(z))))
  if(tailed!=1 | tailed!=2) return('error')
}


z_test(11.2,10,sqrt(8),1000,1)
