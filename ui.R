shinyUI(pageWithSidebar(
        headerPanel("Maximul likelihood estimation"),
        sidebarPanel(
                radioButtons('data', 'Data set:', 
                             choices = list("Applicants" = "applicants", 
                                            "Calls to a hotline" = "calls")),
                
                uiOutput('var'),
                radioButtons("dist", "Distribution:", 
                             c("Discrete uniform" = "disunif",
                               "Bernoulli trials" = "bern",
                               "Poisson" = "pois",
                               "Exponential" = "exp",
                               "Normal" = "norm"),
                             selected = character(0)),
                conditionalPanel(
                        condition = "input.dist == 'bern'",
                        sliderInput('p.bern', "Parameter", 
                                    min=0, max=1, value=0.5, step=0.1)
                        ),
                conditionalPanel(
                        condition = "input.dist == 'pois'",
                        sliderInput('p.pois', "Parameter", 
                                    min=0, max=10, value=2, step=0.1)                        
                ),
                conditionalPanel(
                        condition = "input.dist == 'exp'",
                        sliderInput('p.exp', "Parameter", 
                                    min=0, max=10, value=2, step=0.1)                        
                ),
                conditionalPanel(
                        condition = "input.dist == 'norm'",
                        sliderInput('mean.norm', "Mean", 
                                    min=0, max=300, value=150, step=0.1),
                        sliderInput('sd.norm', "Standard deviation", 
                                    min=0, max=100, value=50, step=0.1)
                )
        ),

        
        
        mainPanel(
                tags$style(type="text/css",
                           ".shiny-output-error { visibility: hidden; }",
                           ".shiny-output-error:before { visibility: hidden; }"
                ),
                p("The datasets represent applicants to a summer school and their calls to the admission hotline. 
                  There are five employees and each call is randomly assign to one of them."), 
                fluidRow(
                        column(6,
                        h4("Probability mass function"), plotOutput("pmf")
                        ),
                        column(6,
                        h4("Cumulative distribution function"), plotOutput("cdf")  
                        )
                )
        )

)
)
