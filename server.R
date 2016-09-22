library(shiny)
library(ggplot2)
library(rhandsontable)
library(dplyr)

ttable<-read.csv("/Users/colleennell/Dropbox/Projects/beans/ttable.csv")
se <- function(x,n) sqrt(var(x)/length(x))
ci95 <-function(x) se(x)*ttable[ttable$n == length(x),2]

shinyServer(function(input,output){
  
  ###reactive variable names & variables
  output$tr1<-renderText({input$treat1})
  output$tr2<-renderText({input$treat2})
  output$var1<-renderText({input$var1})
  output$var2<-renderText({input$var2})
  
  ##make blank dfs
  treat1.df = data.frame(variable1 =as.numeric(rep(NA,3)), ##eventually make column names reactive
                   variable2 = as.numeric(rep(NA,3)),
                   prop = as.numeric(rep(NA,3))
                  )
  testdf1 = data.frame(variable1 = as.numeric(runif(20,0,20)),
                      variable2 = as.numeric(runif(20,0,30)))
  testdf1$propvar1<-testdf1$variable1/(testdf1$variable1+testdf1$variable2)
  
  testdf2 = data.frame(variable1 = as.numeric(runif(20,0,20)),
                       variable2 = as.numeric(runif(20,0,30)))
  testdf2$propvar1<-testdf2$variable1/(testdf2$variable1+testdf2$variable2)
  
  

  
  ##generate talbes
  output$table1 = renderRHandsontable({
    pvar1<-paste0("Prop(",input$var1,")")
    rhandsontable(testdf1,colHeaders =c(input$var1,input$var2,pvar1))
  })
  output$table2 = renderRHandsontable({
    pvar1<-paste0("Prop(",input$var1,")")
    rhandsontable(testdf2,colHeaders =c(input$var1,input$var2,pvar1))
  })
  
  ##plot means
  output$plot1 <- renderPlot({
    testdf1$treat<-rep(input$treat1,length(testdf1$propvar1))#combine datatables for plot
    testdf2$treat<-rep(input$treat2,length(testdf2$propvar1))
    alldata<-rbind(testdf1,testdf2)
    
    errorb<-switch(input$errortype,##reactive error bars
                   se=se,
                   sd=sd,
                   ci=ci95)
    
    all.summ<-alldata%>%
      group_by(treat)%>%
      summarize(mean_prop = mean(propvar1), n_df = length(propvar1), errorr=errorb(propvar1))
    
    
    yvar.lab<-paste("Proportion",input$var1)#reactive y axis label based on variable inputs
    ##eventually want the plotted variable to be a drop-down so that it could be proportion var1 or var2
    
    ggplot(all.summ,aes(x=treat,y=mean_prop,fill=treat))+#plot
      geom_bar(stat="identity")+
      geom_errorbar(aes(ymax=mean_prop+errorr,ymin=mean_prop-errorr),width=.2)+
      theme_minimal()+
      labs(x="Treatment",y=yvar.lab)+
      scale_fill_manual(values=c("#006666","#FF9900"))+
      theme(legend.position="none")
  })
  
  output$significance <- renderInfoBox({
    infoBox(
      "Significance:",br(),"P = ",icon = icon("thumbs-up",lib="glyphicon"),
      color="purple", fill=TRUE
    )
  })
  
})

