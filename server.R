library(shiny)
library(ggplot2)
library(rhandsontable)
library(dplyr)
library(devtools)
library(readr)
library(RCurl)

##proportion column
##3replicates

ttable<-read.csv("https://raw.githubusercontent.com/collnell/beans/master/ttable.csv")#ttable
source_url("https://raw.githubusercontent.com/collnell/R_vis/master/theme_mooney.R")#ggplot theme
DIY_code<-source_url("https://raw.githubusercontent.com/collnell/beans/master/beans_DIY")##DIY code
DIY_code2<-getURL("https://raw.githubusercontent.com/collnell/beans/master/beans_DIY", ssl.verifypeer = FALSE)
eval(parse(text=DIY_code2))

se <- function(x) sqrt(var(x)/length(x))
ci95 <-function(x) se(x)*ttable[ttable$n == length(x),2]

shinyServer(function(input,output){
  
  ###reactive variable names & variables
  output$tr1<-renderText({input$treat1})
  output$tr2<-renderText({input$treat2})
  output$var1<-renderText({input$var1})
  output$var2<-renderText({input$var2})
  ##make blank dfs
<<<<<<< Updated upstream
  
=======
>>>>>>> Stashed changes
  values <- reactiveValues(df_data1 = data.frame(variable1 = as.numeric(rep(NA,50)),
                                                variable2 = as.numeric(rep(NA,50))),
                           df_data2 =data.frame(variable1 = as.numeric(rep(NA,50)),
                                                variable2 = as.numeric(rep(NA,50))))
  values$df_data = data.frame(variable1= as.numeric(rep(0,100)),
                                  variable2=as.numeric(rep(0,100)),
                                  Treatment = as.factor(rep("treat1",100)),
<<<<<<< Updated upstream
                                  Proportion = as.numeric(rep(0,100)))
=======
                                  Proportion1 = as.numeric(rep(0,100)))
>>>>>>> Stashed changes
  values$summ_data = data.frame(Treatment = c("treat1","treat2"),
                                mean_var1 =c(0,0),
                                mean_var2=c(0,0),
                                mean_prop = c(0,0),
                                n_df = c(0,0),
                                se_prop = c(0,0),
                                sd_prop = c(0,0),
                                ci_prop = c(0,0))
  
  ##generate talbes
  output$table1 = renderRHandsontable({
    pvar1<-paste0("Proportion(",input$var1,")")
<<<<<<< Updated upstream
    rhandsontable(values$df_data1,width=300,height=475,colHeaders =c(input$var1,input$var2,pvar1))
  })
  output$table2 = renderRHandsontable({
    pvar1<-paste0("Proportion(",input$var1,")")
    rhandsontable(values$df_data2,width=300,height=475,colHeaders =c(input$var1,input$var2,pvar1))
=======
    rhandsontable(values$df_data1,width=300,height=485,colHeaders =c(input$var1,input$var2,pvar1))
  })
  output$table2 = renderRHandsontable({
    pvar1<-paste0("Proportion(",input$var1,")")
    rhandsontable(values$df_data2,width=300,height=485,colHeaders =c(input$var1,input$var2,pvar1))
>>>>>>> Stashed changes
  })

  observeEvent(input$getdata, { 
    df_data1<-hot_to_r(input$table1)##update df
    df_data2<-hot_to_r(input$table2)
    df_data1$Treatment<-rep(input$treat1,length(values$df_data1$variable1))#make treatment var
    df_data2$Treatment<-rep(input$treat2,length(values$df_data2$variable1))
    df_data_all<-rbind(df_data1,df_data2)#bind
    df_data_all$Proportion1<-df_data_all$variable1/(df_data_all$variable1+df_data_all$variable2)#calc proportions
    df_data_all$Proportion2<-df_data_all$variable2/(df_data_all$variable1+df_data_all$variable2)
    
    summdata<-df_data_all%>%#summarize data
      na.omit()%>%###need to fix data summary box to show all variables?
      group_by(Treatment)%>%
      summarize(mean_prop1 = mean(Proportion1), #col2
                mean_prop2 = mean(Proportion2),#col3
                mean_var1 = mean(variable1),#col4
                mean_var2 = mean(variable2),#col5
                n_v1 = length(variable1), 
                n_v2 =length(variable2),
                se_prop=se(Proportion1), 
                sd_prop = sd(Proportion1), 
                ci_prop = ci95(Proportion1))
    
    values$df_data<-df_data_all%>%na.omit()
    values$summ_data<-summdata
    values$inputcols<-summdata
  })
 
  output$selectUI<-renderUI({
    validate(need(input$getdata,"Treatment and variable names will update when data is run")
    )
    v1<-paste0(input$var1)
    v2<-paste0(input$var2)
    p1<-paste0("Proportion (",input$var1,")")
    p2<-paste0("Proportion (",input$var2,")")
    colnames(values$inputcols)<-c("Treatment",p1,p2,v1,v2)#rename variables based on textinput
    selectInput("figval","Y-axis value:",colnames(values$inputcols[,2:5]))#dynamic ui that changes with inputs
    ##the problem here is that it is impossible to connect with the plot then...
  })

 #plot means
  output$plot1 <- renderPlot({
    v1<-paste0(input$var1)
    v2<-paste0(input$var2)
    p1<-paste0("Proportion (",input$var1,")")
    p2<-paste0("Proportion (",input$var2,")")
    
    validate(need(input$getdata,"Plot will appear here"))
    if (input$figval == p1){##what i could do is have a selection for which proportion? and then calcualte proportion, and viola!
      yval<-values$summ_data$mean_prop1
    } else if (input$figval == p2) {
      yval<-values$summ_data$mean_prop2
    } else if (input$figval == v1){
      yval<-values$summ_data$mean_var1
    } else {
      yval<-values$summ_data$mean_var2
    }
    
    error<-switch(input$errortype,##reactive error bars
                   se=values$summ_data$se_prop,
                   sd=values$summ_data$sd_prop,
                   ci=values$summ_data$ci_prop)
    
    yvar.lab<-paste("Proportion",input$var1)#reactive y axis label based on variable inputs
    ##eventually want the plotted variable to be a drop-down so that it could be proportion var1 or var2
    
    bar.plot<-ggplot(values$summ_data,aes_string(x='Treatment',y=yval,fill='Treatment'))+#plot
      geom_bar(stat="identity")+
      theme_mooney()+
      geom_errorbar(aes(ymax=yval+error,ymin=yval-error),width=.2)+
      labs(x="Treatment",y=yvar.lab)+
      scale_fill_manual(values=c("#006666","#FF9900"))+
      theme(legend.position="none")
    bar.plot
  })
  
  output$histo<- renderPlot({
    validate(need(input$getdata,"Histogram will appear here"))
    
    v1<-paste0(input$var1)
    v2<-paste0(input$var2)
    p1<-paste0("Proportion (",input$var1,")")
    p2<-paste0("Proportion (",input$var2,")")
    
    if (input$figval == p1){
      yval<-values$summ_data$mean_prop1
      meanval<-values$df_data$Proportion1##these names are backwards
      xval<-p1
    } else if (input$figval == p2) {
      yval<-values$summ_data$mean_prop2
      meanval<-values$df_data$Proportion2
      xval<-p2
    } else if (input$figval == v1){
      yval<-values$summ_data$mean_var1
      meanval<-values$df_data$variable1
      xval<-v1
    } else {
      yval<-values$summ_data$mean_var2
      meanval<-values$df_data$variable2
      xval<-v2
    }
    
    
    hist.plot<-ggplot(data=values$df_data,aes_string(x=meanval,fill='Treatment',color='Treatment'))+
      geom_density(alpha=.35)+
      theme_mooney(legend.location="bottom")+
<<<<<<< Updated upstream
      labs(x=xvar.lab,y="Density")+
=======
      labs(x=yval,y="Frequency")+
>>>>>>> Stashed changes
      scale_fill_manual(values=c("#006666","#FF9900"))+
      scale_color_manual(values=c("#006666","#FF9900"))
    
    if(input$showmean == FALSE){
      hist.plot
    } else{
      hist.plot+geom_vline(data=values$summ_data,aes_string(xintercept=yval,color='Treatment'),
                           linetype="dotted",size=2)+
        geom_text(data=values$summ_data,aes_string
                  (x=yval, label=round(yval,2), y=0), color="black", angle=90, hjust=0,vjust=0)
    }
  })
  
  
  output$summary_table<- renderRHandsontable({
    validate(
      need(input$getdata, "Enter values and press 'Run Data' for summary statistics")
    )
    stat.df<-values$df_data%>%
      na.omit()%>%
      group_by(Treatment)%>%
      summarize(N = length(Proportion1),
                SE = se(Proportion1),
                S = sd(Proportion1),
                mean = mean(Proportion1),
                t = ttable[ttable$n == length(Proportion1),2],
                CI = ci95(Proportion1),
                CIlow = mean(Proportion1)-ci95(Proportion1),
                CIhi = mean(Proportion1)-ci95(Proportion1))
    rhandsontable(stat.df[,-1], rowHeaders=c(input$treat1,input$treat2),
                  colHeaders=c("N","SE","S","Mean","T","95% CI","Low CI","High CI"),readOnly=TRUE,
                  rowHeaderWidth=130)%>%
      hot_cols(colWidths = 60)
  })

  
  output$anovatable<-renderPrint({
    validate(
      need(input$getdata, "Enter values and press 'Run Data' for ANOVA test results")
    )
<<<<<<< Updated upstream
    aov.model<-aov(Proportion~Treatment,data=values$df_data)
=======
    aov.model<-aov(Proportion1~Treatment,data=values$df_data)
>>>>>>> Stashed changes
    print(aov.model)
    br()
    br()
    print(summary(aov.model))
  })
  

  ##download csv of data
  output$downloadData <-downloadHandler(
    filename = function() {
      paste("beans_data.csv", sep="")
    },
    content = function(file){
      write.csv(values$df_data,file)
    }
  )

  
})

