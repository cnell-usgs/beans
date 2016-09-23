library(shiny)
library(ggplot2)
library(rhandsontable)
library(dplyr)
library(devtools)
library(readr)

ttable<-read.csv("https://raw.githubusercontent.com/collnell/beans/master/ttable.csv")#ttable
source_url("https://raw.githubusercontent.com/collnell/R_vis/master/theme_mooney.R")#ggplot theme

se <- function(x) sqrt(var(x)/length(x))
ci95 <-function(x) se(x)*ttable[ttable$n == length(x),2]

shinyServer(function(input,output){
  
  ###reactive variable names & variables
  output$tr1<-renderText({input$treat1})
  output$tr2<-renderText({input$treat2})
  output$var1<-renderText({input$var1})
  output$var2<-renderText({input$var2})
  
  ##make blank dfs
  treat1.df = data.frame(variable1 =as.numeric(rep(NA,3)), 
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
    
    bar.plot<-ggplot(all.summ,aes(x=treat,y=mean_prop,fill=treat))+#plot
      geom_bar(stat="identity")+
      theme_mooney()+
      geom_errorbar(aes(ymax=mean_prop+errorr,ymin=mean_prop-errorr),width=.2)+
      labs(x="Treatment",y=yvar.lab)+
      scale_fill_manual(values=c("#006666","#FF9900"))+
      theme(legend.position="none")
    bar.plot
  })
  
  output$histo<- renderPlot({
    testdf1$Treatment<-rep(input$treat1,length(testdf1$propvar1))
    testdf2$Treatment<-rep(input$treat2,length(testdf2$propvar1))
    alldata<-rbind(testdf1,testdf2)
    all.summ<-alldata%>%
      group_by(Treatment)%>%
      summarize(mean_prop = mean(propvar1))
    
    xvar.lab<-paste("Proportion",input$var1)
    
    hist.plot<-ggplot(data=alldata,aes(x=propvar1,fill=Treatment,color=Treatment))+
      geom_density(alpha=.35)+
      theme_mooney(legend.location="bottom")+
      labs(x=xvar.lab,y="Frequency")+
      scale_fill_manual(values=c("#006666","#FF9900"))+
      scale_color_manual(values=c("#006666","#FF9900"))
    
    if(input$showmean == FALSE){
      hist.plot
    } else{
      hist.plot+geom_vline(data=all.summ,aes(xintercept=mean_prop,color=Treatment),
                           linetype="dotted",size=2)+
        geom_text(data=all.summ,aes(x=mean_prop, label=round(mean_prop,2), y=0), color="black", angle=90, hjust=0,vjust=0)
    }
  })
  
  
  output$summary_table<- renderRHandsontable({
    testdf1$treat<-rep(input$treat1,length(testdf1$propvar1))#combine datatables for plot
    testdf2$treat<-rep(input$treat2,length(testdf2$propvar1))
    alldata<-rbind(testdf1,testdf2)
    
    stat.df<-alldata%>%
      group_by(treat)%>%
      summarize(N = length(propvar1),
                SE = se(propvar1),
                S = sd(propvar1),
                mean = mean(propvar1),
                t = ttable[ttable$n == length(propvar1),2],
                CI = ci95(propvar1),
                CIlow = mean(propvar1)-ci95(propvar1),
                CIhi = mean(propvar1)-ci95(propvar1))
    rhandsontable(stat.df[,-1], rowHeaders=c(input$treat1,input$treat2),
                  colHeaders=c("N","SE","S","Mean","T","95% CI","Low CI","High CI"),readOnly=TRUE,
                  rowHeaderWidth=150)%>%
      hot_cols(colWidths = 60)
  })
  
  output$significance <- renderInfoBox({
    infoBox(
      "Test:",br(),"P = ",icon = icon("thumbs-up",lib="glyphicon"),
      color="blue", fill=TRUE
    )
  })

  ##download csv of data
  output$downloadData <-downloadHandler(
    filename = function() {
      paste("data-",Sys.Date(),".csv", sep="")
    },
    content = function(file){
      write.csv(testdf1,file)
    }
  )
  output$downloadplotr <-downloadHandler(##this should be redone
    filename=function(){
      paste("ggplot_bar.r")
    },
    content =function(file){
      write_rds(bar.plot,file,compress="none")
    }
  )

  
})

