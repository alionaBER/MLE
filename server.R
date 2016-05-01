shinyServer(function(input, output) {
        
        output$var <- renderUI({
                switch(input$data,
                               'applicants' = selectInput('var', 'Choose a variable', 
                                                        choices = c("admitted.to.summerschool", "applicants.height", "times.contacted.hotline")),
                               'calls' = selectInput('var', 'Choose a variable', 
                                                     choices = c("employee", "waiting.time.for.call"))
                        )
        })

        
        output$pmf <- renderPlot({
                dat <- get(input$data)
                v <- dat[,input$var]
                ddisunif <- function(x,k) ifelse(x>=min(v) & x<=k & round(x)==x,1/k,0) 
                fitType <- function(x, type) {
                        switch(type,
                               exp = curve(dexp(x, input$p.exp), add = TRUE, col=rgb(red=1, green=0, blue=0, alpha=0.5), lwd = 6),
                               norm = curve(dnorm(x, mean = input$mean.norm, sd = input$sd.norm), add = TRUE, col=rgb(red=1, green=0, blue=0, alpha=0.5), lwd = 6),
                               bern = plot(suppressWarnings(dbinom(x, 1, input$p.bern)), col=rgb(red=1, green=0, blue=0, alpha=0.5), lwd = 6, type = "h", ylim=c(0,1), ylab = "", xlab = "", xaxt='n', yaxt='n'),
                               pois = plot(suppressWarnings(dpois(x, input$p.pois)), col=rgb(red=1, green=0, blue=0, alpha=0.5), lwd = 6, type = "h", ylim=c(0,1), ylab = "", xlab = "", xaxt='n', yaxt='n'),
                               disunif = plot(ddisunif(x,length(unique(v))), col=rgb(red=1, green=0, blue=0, alpha=0.5), lwd = 6, type = "h", ylim=c(0,1), ylab = "", xlab = "", xaxt='n', yaxt='n')
                               )
                }
                if (is.null(input$var) ){
                        return(NULL)
                } else if (input$var == "admitted.to.summerschool"){
                        plot(table(v)/S, ylim=c(0,1), main = "", xlab = "", ylab = "")
                        if (is.null(input$dist)){
                                return(NULL)
                        }else{
                                x <- seq(min(v), max(v), length.out=100)
                                par(new=TRUE)
                                fitType(x, input$dist)   
                        }
                } else if (input$var == "employee") {
                        plot(table(v)/C,  ylim=c(0,1), main = "", xlab = "", ylab = "")
                        if (is.null(input$dist)){
                                return(NULL)
                        }else{
                                x <- round(seq(min(v), max(v), length.out=100), 1)
                                par(new=TRUE)
                                fitType(x, input$dist)   
                        }
                } else if (input$var == "times.contacted.hotline") {
                        plot(table(dat[,input$var])/S, ylim=c(0,1), main = "", xlab = "", ylab = "")
                        if (is.null(input$dist)){
                                return(NULL)
                        }else{
                                x <- round(seq(min(v), max(v), length.out=100),1)
                                par(new=TRUE)
                                fitType(x, input$dist)   
                        }
                } else if (input$var == "applicants.height") {
                        hist(v, prob = TRUE, main = "", xlab = "", col = "grey")
                        rug(v)
                        if (is.null(input$dist)){
                                return(NULL)
                        }else{
                                x <- seq(min(v), max(v), length.out=100)
                                par(new=TRUE)
                                fitType(x, input$dist)   
                        }

                } else if (input$var == "waiting.time.for.call") {
                        hist(v, prob = TRUE, ylim=c(0,1), main = "", xlab = "", col = "grey")
                        rug(v)
                        if (is.null(input$dist)){
                                return(NULL)
                        }else{
                                x <- seq(min(v), max(v), length.out=100)
                                par(new=TRUE)
                                fitType(x, input$dist)   
                        }

                        
                }
        })

        output$cdf <- renderPlot({
                dat <- get(input$data)
                v <- dat[,input$var]
                fitType <- function(x, type) {
                        switch(type,
                               exp = curve(pexp(x, input$p.exp), add = TRUE, col=rgb(red=1, green=0, blue=0, alpha=0.5), lwd = 6),
                               norm = curve(pnorm(x, mean = input$mean.norm, sd = input$sd.norm), add = TRUE, col=rgb(red=1, green=0, blue=0, alpha=0.5), lwd = 6),
                               pois = curve(ppois(x, input$p.pois), add = TRUE, col=rgb(red=1, green=0, blue=0, alpha=0.5), lwd = 6),
                               bern = curve(pbinom(x, 1, input$p.bern), add = TRUE, col=rgb(red=1, green=0, blue=0, alpha=0.5), lwd = 6),
                               disunif = curve(pdisunif(x, length(unique(v))), add = TRUE, col=rgb(red=1, green=0, blue=0, alpha=0.5), lwd = 6)
                               )
                }
                if (is.null(input$var)){
                        return(NULL)
                } else if (input$var == "admitted.to.summerschool" | input$var == "times.contacted.hotline"){
                        plot(ecdf(v), ylab="F(x)", xlab = "", main = "")
                        if (is.null(input$dist)){
                                return(NULL)
                        }else{
                                x <- seq(min(v), max(v), length.out=100)
                                pdisunif <- function(x,k) ifelse(x<min(v),0,ifelse(x<=max(v), floor(x+1)/k, 1))
                                fitType(x, input$dist)   
                        }
                } else if (input$var == "employee"){
                        plot(ecdf(v), ylab="F(x)", xlab = "", main = "")
                        if (is.null(input$dist)){
                                return(NULL)
                        }else{
                                x <- seq(min(v), max(v), length.out=100)
                                pdisunif <- function(x,k) ifelse(x<min(v),0,ifelse(x<=max(v), floor(x)/k, 1))
                                fitType(x, input$dist)   
                        }
                } else if (input$var == "applicants.height") {
                        plot(ecdf(v), ylab="F(x)", xlab = "", main = "")
                        if (is.null(input$dist)){
                                return(NULL)
                        }else{
                                x <- seq(min(v), max(v), length.out=100)
                                pdisunif <- function(x,k) ifelse(x<min(v),0,ifelse(x<=max(v), floor(x)/k, 1))
                                fitType(x, input$dist)   
                        }
                        
                } else if (input$var == "waiting.time.for.call") {
                        plot(ecdf(v), ylab="F(x)", xlab = "", main = "")
                        if (is.null(input$dist)){
                                return(NULL)
                        }else{
                                x <- seq(min(v), max(v), length.out=100)
                                pdisunif <- function(x,k) ifelse(x<min(v),0,ifelse(x<=max(v), max(x)/k, 1))
                                fitType(x, input$dist)   
                        }
                        
                        
                }
        })                
        
})