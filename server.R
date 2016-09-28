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
  
  values <- reactiveValues(df_data1 = data.frame(variable1 = as.numeric(rep(NA,50)),
                                                variable2 = as.numeric(rep(NA,50))),
                           df_data2 =data.frame(variable1 = as.numeric(rep(NA,50)),
                                                variable2 = as.numeric(rep(NA,50))))
  values$df_data = data.frame(variable1= as.numeric(rep(0,100)),
                                  variable2=as.numeric(rep(0,100)),
                                  Treatment = as.factor(rep("treat1",100)),
                                  Proportion = as.numeric(rep(0,100)))
  values$summ_data = data.frame(Treatment = c("treat1","treat2"),
                                mean_prop = c(0,0),
                                n_df = c(0,0),
                                se_prop = c(0,0),
                                sd_prop = c(0,0),
                                ci_prop = c(0,0))
  
  ##generate talbes
  output$table1 = renderRHandsontable({
    pvar1<-paste0("Proportion(",input$var1,")")
    rhandsontable(values$df_data1,width=300,height=475,colHeaders =c(input$var1,input$var2,pvar1))
  })
  output$table2 = renderRHandsontable({
    pvar1<-paste0("Proportion(",input$var1,")")
    rhandsontable(values$df_data2,width=300,height=475,colHeaders =c(input$var1,input$var2,pvar1))
  })
  
  observeEvent(input$getdata, { #an error occurs if there are empty cells ' must be vector type was null'
    df_data1<-hot_to_r(input$table1)##needs some kind of remove NA
    df_data2<-hot_to_r(input$table2)
    
    df_data1$Treatment<-rep(input$treat1,length(values$df_data1$variable1))
    df_data2$Treatment<-rep(input$treat2,length(values$df_data2$variable1))
    
    df_data_all<-rbind(df_data1,df_data2)
    df_data_all$Proportion<-df_data_all$variable1/(df_data_all$variable1+df_data_all$variable2)
    
    summdata<-df_data_all%>%#summarize data
      na.omit()%>%
      group_by(Treatment)%>%
      summarize(mean_prop = mean(Proportion), 
                n_df = length(Proportion), 
                se_prop=se(Proportion), 
                sd_prop = sd(Proportion), 
                ci_prop = ci95(Proportion))
    
    values$df_data<-df_data_all
    values$summ_data<-summdata
  })

 #plot means
  output$plot1 <- renderPlot({
    error<-switch(input$errortype,##reactive error bars
                   se=values$summ_data$se_prop,
                   sd=values$summ_data$sd_prop,
                   ci=values$summ_data$ci_prop)
    
    yvar.lab<-paste("Proportion",input$var1)#reactive y axis label based on variable inputs
    ##eventually want the plotted variable to be a drop-down so that it could be proportion var1 or var2
    
    bar.plot<-ggplot(values$summ_data,aes(x=Treatment,y=mean_prop,fill=Treatment))+#plot
      geom_bar(stat="identity")+
      theme_mooney()+
      geom_errorbar(aes(ymax=mean_prop+error,ymin=mean_prop-error),width=.2)+
      labs(x="Treatment",y=yvar.lab)+
      scale_fill_manual(values=c("#006666","#FF9900"))+
      theme(legend.position="none")
    bar.plot
  })
  
  output$histo<- renderPlot({
    xvar.lab<-paste("Proportion",input$var1)
    
    hist.plot<-ggplot(data=values$df_data,aes(x=Proportion,fill=Treatment,color=Treatment))+
      geom_density(alpha=.35)+
      theme_mooney(legend.location="bottom")+
      labs(x=xvar.lab,y="Frequency")+
      scale_fill_manual(values=c("#006666","#FF9900"))+
      scale_color_manual(values=c("#006666","#FF9900"))
    
    if(input$showmean == FALSE){
      hist.plot
    } else{
      hist.plot+geom_vline(data=values$summ_data,aes(xintercept=mean_prop,color=Treatment),
                           linetype="dotted",size=2)+
        geom_text(data=values$summ_data,aes(x=mean_prop, label=round(mean_prop,2), y=0), color="black", angle=90, hjust=0,vjust=0)
    }
  })
  
  
  output$summary_table<- renderRHandsontable({
    validate(
      need(input$getdata, "Enter values and press 'Run Data' for summary statistics")
    )
    stat.df<-values$df_data%>%
      na.omit()%>%
      group_by(Treatment)%>%
      summarize(N = length(Proportion),
                SE = se(Proportion),
                S = sd(Proportion),
                mean = mean(Proportion),
                t = ttable[ttable$n == length(Proportion),2],
                CI = ci95(Proportion),
                CIlow = mean(Proportion)-ci95(Proportion),
                CIhi = mean(Proportion)-ci95(Proportion))
    rhandsontable(stat.df[,-1], rowHeaders=c(input$treat1,input$treat2),
                  colHeaders=c("N","SE","S","Mean","T","95% CI","Low CI","High CI"),readOnly=TRUE,
                  rowHeaderWidth=130)%>%
      hot_cols(colWidths = 60)
  })

  
  output$anovatable<-renderPrint({
    validate(
      need(input$getdata, "Enter values and press 'Run Data' for ANOVA test results")
    )
    aov.model<-aov(Proportion~Treatment,data=values$df_data)
    print(aov.model)
    br()
    br()
    print(summary(aov.model))
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

