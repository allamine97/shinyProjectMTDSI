library(shiny)
library(ggplot2)
library(knitr)
library(pander)
library(FactoMineR)
library(rsconnect)
library("factoextra")
function(input, output,session) {
  
  contentsrea <- reactive({
    inFile <- input$datafile
    if (is.null(inFile))
      return("Please upload a csv file")
    read.csv(inFile$datapath,header = TRUE)
  })
  
  
  observe({
    updateSelectInput(session, "variable1", choices = names(contentsrea()))
    updateSelectInput(session, "variable2", choices = names(contentsrea()))
  })
  
  
  
  observeEvent(input$update,{
    df <- read.csv(input$datafile$datapath, header = TRUE, sep =input$var)
    variable1 <- df[, which(colnames(df) == input$variable1)]
    variable2 <- df[, which(colnames(df) == input$variable2)]
    dat <- data.frame(variable1 = variable1, variable2 = variable2)
    # Plot
    fit <- lm(variable2 ~ variable1, data = dat)
    dat$predicted <- predict(fit)
    ggObj <- ggplot(data = dat, aes(x = variable1, y = variable2)) +
      geom_point(color='black', size = 3, stroke = 2, fill = "gray60", shape = 21) +
      xlab(input$variable1) +
      ylab(input$variable2) +
      theme_classic() +
      theme(axis.line = element_line(size = 1.5),
            axis.title = element_text(size = 17),
            axis.text = element_text(size = 17,
                                     color = "black"),
            axis.ticks = element_line(size = 1.5),
            axis.ticks.length=unit(.25, "cm"))
    #ggObj1 <- ggObj + geom_segment(aes(xend = variable1, yend = predicted), size = 1)
    ggObj <- ggObj + geom_smooth(method = "lm", se = FALSE, color = "darkred", size = 1.5)
    
    
    output$plot <- renderPlot(ggObj)
    output$summary<-renderPrint({
      summary(fit)
    })
  
    output$results <- renderUI({
      withMathJax(
        paste0(
          "\\( R^2 = \\) ", round(summary(fit)$adj.r.squared, 3),
          ", \\( \\beta_0 = \\) ", round(fit$coef[[1]], 3),
          ", \\( \\beta_1 = \\) ", round(fit$coef[[2]], 3)
        )
      )
    })
    
  })
  
  observeEvent(input$am,{
    db <- read.csv(input$datafile$datapath, header = TRUE, sep =input$var)
    output$pc<-renderPlot({
      #res.ca <- CA (db, graph = FALSE)
      #print(res.ca)
      acp =PCA(db, scale.unit =TRUE,graph=FALSE)
      plot(acp,choix='ind')
      #plot(acp,choix='var',habillage = 'cos2')
    })
  })
  
  output$table <- renderDataTable({
    file <- input$datafile
    ext <- tools::file_ext(file$datapath)
    
    req(file)
    validate(need(ext == "csv", "Please upload a csv file"))
    
    switch (input$dist,
      "summary" = return(summary(read.csv(file$datapath,sep=input$var))),
      "head"= return(head(read.csv(file$datapath,sep=input$var))),
      #"str"=return(str(read.csv(file$datapath,sep=input$var))),
      #"View"=return(View(read.csv(file$datapath,sep=input$var))),
      
    )
  })
  
 
  
  output$strfile <- renderPrint({
    f<-input$datafile
    
    if (is.null(f)) {
      
      return()
      
    }
    str(read.csv(f$datapath,sep=input$var))
    #fact<-read.csv(f$datapath,sep=input$var)
    #res.ca <- CA (fact, graph = FALSE)
    #print(res.ca)
    
  })
  
  #--------
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste('my-report', sep = '.', switch(
        input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
      ))
    },
    
    content = function(file) {
      src <- normalizePath('report.Rmd')
      
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(getwd())
      on.exit(setwd(owd))
      file.copy(src, 'report.Rmd', overwrite = TRUE)
      
      library(rmarkdown)
      out <- render('report.Rmd', switch(
        input$format,
        PDF = pdf_document(), HTML = html_document(), Word = word_document()
      ))
      file.rename(out, file)
    }
  )
  

  
}