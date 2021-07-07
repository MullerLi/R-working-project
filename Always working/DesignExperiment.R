DesignR<-function(treatment,rep){
  Design<-design.rcbd(trt=as.factor(1:treatment),r=rep,seed=1651651,first=FALSE,continue=TRUE,randomization=TRUE )
  OUT<-function(x){
  out<-capture.output(x)
  write.csv(out,file= paste0( "RCBD" , format(Sys.time() ,"%d-%S"),".csv"))
  Sys.sleep(1)}
  lapply(Design, function(x) OUT(x))
  print("done!")}
  
DesignR(12,5)

