library(shiny)
library(tidyverse)
library(ggrepel)
library(DT)
update_geom_defaults("label", list(family = "Fira Sans Condensed"))
update_geom_defaults("text", list(family = "Fira Sans Condensed"))

# Define UI for app that draws a histogram ----
ui <- fluidPage(
    
    # App title ----
    titlePanel("Monopoly vs. Perfect Competition"),
    withMathJax(),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(
            
            h4("Demand Function"),
            helpText(),
            
            p("Set the parameters for the market inverse demand functions below:"),
            helpText("Market Inverse Demand Function: $$p=a-bQ$$"),
            # Input: Slider for a ----
            sliderInput(inputId = "a",
                        label = "Demand intercept, a",
                        min = 8,
                        max = 12,
                        value = 10, step=2),
            
            # Input: Slider for b ----
            sliderInput(inputId = "b",
                        label = "Demand slope, b",
                        min = 0.5,
                        max = 1.5,
                        value = 1, step=0.5),
            h4("Firm's Cost Function"),
            
            # Input: Slider for c ----
            p("Set the parameters for the firm's total cost function below:"),
            helpText("Total Cost Function: $$C(q)=cq^{e_1}+dq^{e_2}+f$$"),
            sliderInput(inputId = "c",
                        label = "First cost parameter, c",
                        min = 0,
                        max = 3,
                        value = 1, step=1),
            # Input: Slider for e1 ----
            sliderInput(inputId = "e_1",
                        label = "First exponent, \\(e_1\\)",
                        min = 0,
                        max = 2,
                        value = 1, step=1),
            # Input: Slider for d ----
            sliderInput(inputId = "d",
                        label = "Second Cost Parameter, d",
                        min = 0,
                        max = 2,
                        value = 1, step=1),
            # Input: Slider for e2 ----
            sliderInput(inputId = "e_2",
                        label = "Second exponent, \\(e_2\\)",
                        min = 0,
                        max = 1,
                        value = 1, step=1),
            # Input: Slider for d ----
            sliderInput(inputId = "f",
                        label = "Fixed cost, f",
                        min = 0,
                        max = 5,
                        value = 1, step=1),
            # Input: Breakeven ----
            checkboxInput(inputId = "breakeven",
                          label = "Show Break Even Price",
                          value = FALSE),
            checkboxInput(inputId = "shutdown",
                          label = "Show Shut Down Price",
                          value = FALSE),
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
            
            # Output: Plot ----
            plotOutput(outputId = "graph"),
            dataTableOutput(outputId = "table"),
            hr(),
            #h3("Takeaways:", align="center"),
            #h4("1. As market demand increases (decreases) (change in a), firms will produce more (less)."),
            #h4("2. As market demand rotates and becomes more elastic/flat (less elastic/steep) (change in b), firms will produce more (less)."),
            #h4("3. As costs increase (decrease), firms will produce less (more)."),
            br(),
            hr(),
            br(),
            p("This model is coded with", a("R and Shiny",href="https://shiny.rstudio.com/"), "by", a("Ryan Safner",href="http://ryansafner.com"))
        )
    )
)

# Define server logic required to draw the graph ----
server <- function(input, output) {
    
    output$graph <- renderPlot({
        
        # gather input parameters ----
        a<-input$a
        b<-input$b
        c<-input$c
        d<-input$d
        e_1<-input$e_1
        e_2<-input$e_2
        f<-input$f
        shutdown<-input$shutdown
        breakeven<-input$breakeven
        
        # create functions from input parameters ----
        Demand=function(q){a-b*q}
        MR=function(q){a-2*b*q}
        MC=function(q){e_1*c*q^(e_1-1)+d*e_2*q^(e_2-1)}
        TC=function(q){c*q^(e_1)+d*q^(e_2)+f}
        AC=function(q){TC(q)/q}
        VC=function(q){c*q^(e_1)+d*q^(e_2)}
        AVC=function(q){VC(q)/q}
        
        # create solutions algebraically ----
        
        ## monopoly optimal quantity, where MR = MC
        opt<-(a-d)/(e_1*c+2)
        
        ## monopoly break-even price, where AC = MC
        q_ACmin<-optimize(
            f = AC,
            lower = 0,
            upper = a/b) %>%
            as.tibble() %>%
            pull(minimum)
        
        ## monopoly shut-down price, where AVC = MC
        q_AVCmin<-optimize(
            f = AVC,
            lower = 0,
            upper = a/b) %>%
            as.tibble() %>%
            pull(minimum)
        
        ## competitive quantity, where p = MC 
        ### (a root of a quadratic function)
        
        comp_root = function(q){e_1*c*q^(e_1-1)+d*e_2*q^(e_2-1)+b*q-a}
        
        q_pc<-uniroot(f = comp_root,
                      interval = c(-a/b,a/b),
                      extendInt = "yes"
        ) %>%
            as.tibble() %>%
            pull(root)
        
        # create labels on graph ----

        points<-tribble(
            ~x, ~y, ~lab,
            q_pc, Demand(q_pc), "PC",
            opt, Demand(opt), "M"
        )
        
        ## surpluses
        
        cs_pc<-tribble(
            ~x, ~y,
            0, a,
            0, Demand(q_pc),
            q_pc, Demand(q_pc)
        )
        
        cs_m<-tribble(
            ~x, ~y,
            0, a,
            0, Demand(opt),
            opt, Demand(opt)
        )
        
        dwl<-tribble(
            ~x, ~y,
            opt, Demand(opt),
            opt, Demand(q_pc),
            q_pc, Demand(q_pc)
        )
        
        # the graph ----
        
        plot<-ggplot(data.frame(x=c(0,12)), aes(x=x))+
            # Surpluses
            geom_polygon(data = cs_m,
                         aes(x = x, y = y, fill = "CS"),
                         alpha = 0.4)+
            geom_polygon(data = dwl,
                         aes(x = x, y = y, fill = "DWL"),
                         alpha = 0.4)+
            geom_rect(xmin = 0, xmax = opt, ymin = AC(opt), ymax = Demand(opt), aes(fill = "Profit"), alpha = 0.4)+
            
            # Costs
            stat_function(fun=MC, geom="line", aes(color="MC"), size=2)+
            #geom_label(x=0.8*(a/b),y=MC(0.8*(a/b)), color = "red", label="MC(q)")+
            stat_function(fun=AC, geom="line", aes(color="AC"), size=2)+
            #geom_label(x=0.8*(a/b),y=AC(0.8*(a/b)), color = "orange", label="AC(q)")+
            stat_function(fun=AVC, geom="line", aes(color="AVC"), size=2)+
            #geom_label(x=0.8*(a/b),y=AVC(0.8*(a/b)), color = "brown", label="AVC(q)")+
            
            # Revenues
            stat_function(fun=Demand, geom="line", aes(color="Demand"), size=2)+
            #geom_label(x=0.8*(a/b),y=Demand(0.8*(a/b)), color = "blue", label="Market Demand")+
            stat_function(fun=MR, geom="line", aes(color="MR"), size=2)+
            #geom_label(x=0.4*(a/b),y=MR(0.4*(a/b)), color = "purple", label="MR(q)")+
            
            # perfect competition lines
            #geom_segment(x = 0, xend = q_pc, y = Demand(q_pc), yend = Demand(q_pc), linetype = "dotted", size = 1)+
            geom_segment(x = q_pc, xend = q_pc, y = Demand(q_pc), yend = 0, linetype = "dotted", size = 1)+
            
            # monopoly lines
            geom_segment(aes(x=opt,y=0),xend=opt,yend=Demand(opt),linetype="dashed", size=1)+
            geom_segment(aes(x=0,y=Demand(opt)),xend=opt,yend=Demand(opt), linetype="dashed", size=1)+
            
            # points
            geom_label(data = points,
                       aes(x = x, y = y, label = lab),
                       #box.padding = 0.5,
                       #seed = 1
            )+
            labs(x = "Quantity",
                 y = "Price",
                 caption = "PC: competitive outcome \n M: Monopoly outcome")+
            scale_colour_manual("Curves",
                                breaks = c("Demand", "MR", "MC", "AC", "AVC"),
                                values = c("Blue", "Purple", "Red", "Orange", "Brown"))+
            scale_fill_manual("Surpluses",
                              breaks = c("CS", "Profit", "DWL"),
                              values = c("Blue", "Green", "Black"),
                              guide = guide_legend(override.aes = list(alpha = 0.3)))+
            scale_x_continuous(breaks = seq(0,a,(a/10)),
                               limits = c(0,1.1*a),
                               expand = c(0,0))+
            scale_y_continuous(breaks = seq(0,(a/b),0.1*(a/b)),
                               limits = c(0,1.1*(a/b)),
                               labels = scales::dollar, #function(x){paste0("$",x)},
                               expand = c(0,0))+
            theme_bw(base_family = "Fira Sans Condensed", base_size=16)
        
        # show breakeven? ----
        if (breakeven == TRUE & shutdown == FALSE) {
            print(plot+
                      geom_hline(yintercept = AC(q_ACmin), linetype = "dashed", size = 1)+
                      geom_label(x=1.0*(a/b),y=AC(q_ACmin), label = "Break even price")
            )
            }
        else if (breakeven == FALSE & shutdown == TRUE) {
            print(plot+
                      geom_hline(yintercept = AVC(q_AVCmin), linetype = "dashed", size = 1)+
                      geom_label(x=1.0*(a/b),y=AVC(q_AVCmin), label = "Shut down price")
            )
        }
        else if (breakeven == TRUE & shutdown == TRUE) {
            print(plot+
                      geom_hline(yintercept = AC(q_ACmin), linetype = "dashed", size = 1)+
                      geom_label(x=1.0*(a/b),y=AC(q_ACmin), label = "Break even price")+
                      geom_hline(yintercept = AVC(q_AVCmin), linetype = "dashed", size = 1)+
                      geom_label(x=1.0*(a/b),y=AVC(q_AVCmin), label = "Shut down price")
            )
        }
        else {print(plot)}
        })
    
    output$table<-DT::renderDataTable({
        
        # gather input parameters ----
        a<-input$a
        b<-input$b
        c<-input$c
        d<-input$d
        e_1<-input$e_1
        e_2<-input$e_2
        f<-input$f
        
        # create functions from input parameters ----
        Demand=function(q){a-b*q}
        MR=function(q){a-2*b*q}
        MC=function(q){e_1*c*q^(e_1-1)+d*e_2*q^(e_2-1)}
        TC=function(q){c*q^(e_1)+d*q^(e_2)+f}
        AC=function(q){TC(q)/q}
        VC=function(q){c*q^(e_1)+d*q^(e_2)}
        AVC=function(q){VC(q)/q}
        
        # create solutions algebraically ----
        
        ## monopoly optimal quantity, where MR = MC
        opt<-(a-d)/(e_1*c+2)
        
        ## monopoly break-even price, where AC = MC
        q_ACmin<-optimize(
            f = AC,
            lower = 0,
            upper = a/b) %>%
            as.tibble() %>%
            pull(minimum)
        
        ## monopoly shut-down price, where AVC = MC
        q_AVCmin<-optimize(
            f = AVC,
            lower = 0,
            upper = a/b) %>%
            as.tibble() %>%
            pull(minimum)
        
        ## competitive quantity, where p = MC 
        ### (a root of a quadratic function)
        
        comp_root = function(q){e_1*c*q^(e_1-1)+d*e_2*q^(e_2-1)+b*q-a}
        
        q_pc<-uniroot(f = comp_root,
                      interval = c(-a/b,a/b),
                      extendInt = "yes"
        ) %>%
            as.tibble() %>%
            pull(root)
        
        # calculate surpluses ----
        cs_pc<-0.5*(a-Demand(q_pc))*q_pc
        cs_m<-0.5*(a-Demand(opt))*opt
        dwl<-0.5*(q_pc-opt)*(Demand(opt)-Demand(q_pc))
        pi<-(Demand(opt)-AC(opt))*opt
        lerner = (Demand(opt) - MC(opt))/Demand(opt)
        elasticity = -1*1/lerner
        elasticity_pc<-(1/(-1*b)*c/q_pc)
        
        # table ----
        tribble(
            ~"Market", ~"Output", ~"Price", ~"Markup", ~"Price Elasticity", ~"CS", ~"Profit", ~"DWL",
            "Perfect Competition", round(q_pc,2), paste0("$",round(c,2)), paste0(0,"%"), round(elasticity_pc, 2), paste0("$",round(cs_pc,2)), paste0("$",round(0,2)), paste0("$", round(0,2)),
            "Monopoly", round(opt,2), paste0("$",round(Demand(opt), 2)), paste0(round(100*lerner,2),"%"), round(elasticity,2), paste0("$",round(cs_m,2)), paste0("$",round(pi,2)), paste0("$",round(dwl,2)),
        )
        
        #library(kableExtra)
        #table #%>% knitr::kable(., type="html")
        
    })
}
shinyApp(ui = ui, server = server)
