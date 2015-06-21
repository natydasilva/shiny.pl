library(shiny)
library(plyr)
#library(dplyr)
library(MASS)
library(ggplot2)
library(rrcov)
library(gridExtra)


simu <- function(mux1,mux2,muy1,muy2,cor){
  bivn <- mvrnorm(50, mu = c(mux1,mux2), Sigma = matrix(c(1, cor, cor, 1), 2))
  bivn2 <- mvrnorm(50, mu = c(muy1,muy2), Sigma = matrix(c(1, cor, cor, 1), 2))
  
  d1 <- data.frame(Sim="sim1",bivn)
  d2 <- data.frame(Sim="sim2",bivn2)
  rbind(d1,d2) 
}

theta<-0:359
ilda.aov <- function(theta,data){
  dattr <- data.frame(Sim=data$Sim,X1=data[,2]*cos(theta*pi/180),X2=sin(theta*pi/180)*data[,3])
  dattr$suma <- dattr$X1+dattr$X2
  anova(lm(suma~Sim,data=dattr))[[2]][[1]]/sum(anova(lm(suma~Sim,data=dattr))[[2]])
}



# Define server logic required to summarize and view the 
# selected dataset
shinyServer(function(input, output) {
  
  dat.sim<- reactive({simu(input$meanx1,input$meanx2,input$meany1,input$meany2,input$cor)})
  #ilda.aux <- reactive({ilda.aov(theta,data.sim())})
  ilda <- reactive({mdply(data.frame(theta=0:359),ilda.aov,data=dat.sim()) })
  wmi <- reactive({as.numeric(which.min(ilda()[,2]-1))})
  wma <- reactive({as.numeric(which.max(ilda()[,2]-1))})
  lin  <- reactive({ data.frame(v1=sin(wma()*pi/180)/cos(wma()*pi/180),v2=sin(wmi()*pi/180)/cos(wmi()*pi/180))} )
  dat.pl <-reactive({data.frame(ilda=ilda()[,2],theta=theta)})
  v1<-reactive({as.numeric(sin(wma()*pi/180)/cos(wma()*pi/180))})
  int <- reactive({ilda()[1,2]})
  range <- reactive({max(ilda()[,2])-min(ilda()[,2])})
  ilda2<- reactive({(ilda()[,2]-min(ilda()[,2]))/range()*2 + 3 })
  df1 <- reactive({data.frame(x=cos(theta*pi/180)*4,y=sin(theta*pi/180)*4)})
  df2 <- reactive({data.frame(x=cos(theta*pi/180)*ilda2(),y=sin(theta*pi/180)*ilda2())})
  
  ilda.best <- reactive({ilda()[,2][wma()]})
  proj.data.best <- reactive({data.frame(Sim=dat.sim()[,1],proj=cos(wma()*pi/180)*dat.sim()[,2]+sin(wma()*pi/180)*dat.sim()[,3])})
  proj.data.nobest <- reactive({data.frame(Sim=dat.sim()[,1],proj=cos(wmi()*pi/180)*dat.sim()[,2]+sin(wmi()*pi/180)*dat.sim()[,3])})
  
  
  #output$table1<-renderTable({data.frame(v1())})
  output$plot1<-renderPlot({
    print(ggplot(dat.sim(),aes(x=X1,y=X2))+geom_point(aes(shape=Sim))+
            scale_y_continuous(limits=c(-5,5)) + scale_x_continuous(limits=c(-5,5))+
            geom_abline(data=lin(),aes(intercept=0,slope=v1))+
            geom_abline(data=lin(),aes(intercept=0,slope=v2))
          )
  })


 output$plot2<-renderPlot({
   
print(qplot(data=dat.pl()[1:180,],x=theta, y=ilda,geom='line',size=I(1.5)) +
  geom_vline(xintercept=wma(),colour=I("red")))
#+
   # geom_hline(yintercept=int(),linetype="dashed"))
# print(ggplot(data=dat.pl()[1:180,],aes(x=theta, y= ilda) )+geom_line()+
#   geom_vline(xintercept=v1(),colour=I("red")) +
#   geom_hline(yintercept=int(),linetype="dashed"))


})
output$plot3<-renderPlot({
  p1 <- ggplot(data=dat.sim(),aes(x=X1,y=X2))+geom_point(aes(shape=Sim))+
          scale_y_continuous(limits=c(-5,5)) + scale_x_continuous(limits=c(-5,5))+ 
          geom_abline(data=lin(),aes(intercept=0,slope=v1))+
          geom_abline(data=lin(),aes(intercept=0,slope=v2))
 print(p1+ geom_point(data=df1(),aes(x=x,y=y))+geom_point(data=df2(),aes(x=x,y=y)) )
  
 
  })
output$plot4<-renderPlot({
p3<-ggplot(proj.data.best(),aes(x=proj))+geom_histogram(aes(fill = Sim))+theme(legend.position = "none")
p4<-ggplot(proj.data.nobest(),aes(x=proj))+geom_histogram(aes(fill=Sim))+theme(legend.position = "none")
print( grid.arrange(p3,p4,ncol=2) )

 })
})
