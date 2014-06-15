library(shiny)

transform<-function(dataset){ 

data<-NULL   
  
if(is.data.frame(dataset)){  
data<-read.csv(file=as.character(dataset$datapath),header=TRUE)}
             
return (data)
}

constans<-function (type="xr",n) {
  #order A2 A3 B3 B4 d2 D3 D4
  cons<-read.csv(file="data/constans.csv",header=TRUE)
  
  const<-vector()
  
  if (type=="xr"){  names<-c("A2","D3","D4") 
                    const<-cons[cons$n==n,names]}
  
  if (type=="xs"){names<-c("A3","B3","B4") 
                  const<-cons[cons$n==n,names]}
  
  if (type=="imr"){names<-c("D3","D4") 
                   const<-c(cons[cons$n==n,"d2"],cons[cons$n==2,names])}
    
  return(const)
}

parameters<-function(dataset,type="xr"){
  
  c<-constans(type=type,n=nrow(dataset))
   
  if (type=="xr"){ names<-c("X","LCL_X","UCL_X","R","LCL_R","UCL_R")
    mean_x<-apply(dataset,1,mean)
    global_mean<-mean(mean_x)  
    
    range_x<-apply(dataset,1,max)-apply(dataset,1,min)
    global_range<-mean(range_x)
    
    lcl_x<-global_mean-c[[1]]*global_range
    ucl_x<-global_mean+c[[1]]*global_range
    
    lcl_r<-global_range*c[[2]]
    ucl_r<-global_range*c[[3]]
    
    coef<-c(global_mean,lcl_x,ucl_x,global_range,lcl_r, ucl_r)
    
    coef[coef<0]<-0
    values<-cbind("x"=mean_x,"sd"=range_x)
    return(list(coef,values)) 
  }
  
  if (type=="xs"){ names<-c("X","LCL_X","UCL_X","S","LCL_S","UCL_S")
                   mean_x<-apply(dataset,1,mean)
                   global_mean<-mean(mean_x)  
                   
                   s<-mean(apply(dataset,1,sd))
                   sval<-apply(dataset,1,sd)                   
                   lcl_x<-global_mean-c[[1]]*s
                   ucl_x<-global_mean+c[[1]]*s
                   
                   lcl_s<-s*c[[2]]
                   ucl_s<-s*c[[3]]
                   
                   coef<-c(global_mean,lcl_x,ucl_x,s,lcl_s, ucl_s)
                   coef[coef<0]<-0
                   values<-cbind("x"=mean_x,"sd"=sval)
                   return(list(coef,values))
  
  }
  
  if (type=="imr"){ names<-c("X","LCL_X","UCL_X","IMR","LCL_IMR","UCL_IMR")
                   
                  global_mean<-apply(dataset,2,mean)
                   
                   mr<-vector()
                  
                  for (i in 1:nrow(dataset))
                  { 
                    if(i==1){mr[i]<-0}
                    else {
                    mr[i]<-abs(dataset[i,1]-dataset[i-1,1])}
                                        
                  }
                  
                   mr_mean<-mean(mr)
                   lcl_x<-global_mean - (3*mr_mean)/c[[1]]
                   ucl_x<-global_mean + (3*mr_mean)/c[[1]]
                   
                   lcl_mr<-mr_mean*c[[2]]
                   ucl_mr<-mr_mean*c[[3]]
                   
                    coef<-c(global_mean,lcl_x,ucl_x,mr_mean,lcl_mr, ucl_mr)
                    coef[coef<0]<-0
                    values<-cbind("x"=dataset,"sd"=mr)
                    return(list(coef,values))
  }

}

chart<-function(dataset,type="xr"){
  
 dataset<-transform(dataset)
 
  library(ggplot2)
  library(stringr)
  library(grid)
  coef<-parameters(dataset=dataset,type=type)
  
  data_graphic_x<-data.frame("Mean_X"=rep(coef[[1]][[1]],nrow(dataset)),
                             "LCL_X"=rep(coef[[1]][[2]],nrow(dataset)),
                             "UCL_X"=rep(coef[[1]][[3]],nrow(dataset)),
                             "x"=coef[[2]][,1],
                             "Sample"=c(1:nrow(dataset)))
  
  data_graphic2<-data.frame("Mean"=rep(coef[[1]][[4]],nrow(dataset)),
                             "LCL"=rep(coef[[1]][[5]],nrow(dataset)),
                             "UCL"=rep(coef[[1]][[6]],nrow(dataset)),
                            "sd"=coef[[2]][,2],
                             "Sample"=c(1:nrow(dataset)))
  
  #graphic for the variable X
  gg_x<-ggplot(data=data_graphic_x,aes(Sample,Mean_X))
  gg_mean<-gg_x+geom_point(colour="green")+geom_line(colour="green") + geom_line(aes(Sample,LCL_X),colour="red") +geom_point(aes(Sample,LCL_X),colour="red")
  
  chart_x<-gg_mean+ geom_line(aes(Sample,UCL_X),colour="red") +
          geom_point(aes(Sample,UCL_X),colour="red")+ labs(title="X-bar Chart") + geom_point(aes(Sample,x),colour="skyblue") + 
          geom_line(aes(Sample,x),colour="skyblue")
     
  #graphic for the dispersion variable s,r or imr
  
  gg_sd<-ggplot(data=data_graphic2,aes(Sample,Mean))
  gg_mean_sd<-gg_sd+geom_point(colour="green")+geom_line(colour="green")+ geom_line(aes(Sample,LCL),colour="red") +geom_point(aes(Sample,LCL),colour="red")
  
  chart_sd<-gg_mean_sd + geom_line(aes(Sample,UCL),colour="red") +geom_point(aes(Sample,UCL),colour="red") + 
            labs(title=paste(str_sub(type,2),"Chart"))+ geom_point(aes(Sample,sd),colour="orange")+
            geom_line(aes(Sample,sd),colour="orange")
     
  #create the viewport

pushViewport(viewport(layout = grid.layout(2, 1)))
  vplayout<-function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
  
  print(chart_x,vp=vplayout(1,1))
  print(chart_sd,vp=vplayout(2,1))

}

Index<-function (dataset,nominal,LSL,USL){
  dataset<-transform(dataset)
  
  if(length(dataset)==1) { mu<-mean(dataset[[1]]) 
  std<-sd(dataset[[1]])}
  else { mu<-mean(apply(dataset,1,mean)) 
         std<-sd(apply(dataset,1,mean))}
        
  cpu<-(USL-mu)/(3*std)
  cpl<-(mu-LSL)/(3*std)
  
  cpk<-min(cpl,cpu)
  cp<-(USL-LSL)/(3*std)
  
  table<-data.frame("Mean"=mu, "Standard deviation"=std,"Cpk"=cpk,"CP"=cp,"CPL"=cpl,"CPU"=cpu)
  return(table)
}

shinyServer(function(input,output){

  text<-reactive({  

     
data.frame("Instructions"=c("Insert the nominal value of the specification",
                                                             "Insert the lower  specification limit (the value for default is for demonstration)",
                                                             "Insert the upper specification limit (the value for default is for demonstration)",
                                                             "Select the kind of chart, for datasets with subsamples (colums) of size greater than 1 you should select the (x,R) or (X,S) chart, 
                                                             for subsamples (colums) of size 1 you should select the IMR Chart",
                                                             "Upload the file with the subsamples, a datasets with names: data_xr_xs and data_imr
                                                              are available in: https://github.com/Stevensct02/Application-of-data-products, for 
                                                              see a demonstration"))})
    
  graphic<-reactive({  
    
    if(is.data.frame(input$dataset) & (input$type=="xr"|input$type=="xs") & length(transform(input$dataset))!=1)  {
    chart(dataset=input$dataset,type=input$type)}
    
    if(is.data.frame(input$dataset) & input$type=="imr" & length(transform(input$dataset))==1)  {
      chart(dataset=input$dataset,type="imr")}
      })  
  
  Cps<-reactive({  
    if(is.data.frame(input$dataset))  {
      Index(dataset=input$dataset,nominal=input$nominal,LSL=input$LSL,USL=input$USL)}})   
  
  output$message<-renderTable({text()})
  output$chart<-renderPlot({  graphic()  })
  output$stat<-renderTable({Cps()})
 
}
)