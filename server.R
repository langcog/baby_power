library(shiny)
library(shinyapps)
library(ggplot2)
library(dplyr)
library(tidyr)
library(pwr)
theme_set(theme_bw())

################################################################################
## HELPER FUNCTIONS

sem <- function(x) {
  sd(x) / sqrt(length(x)) 
}

ci95.t <- function(x) {
  qt(.975, length(x)-1) * sem(x)
} 

pretty.p <- function(x) {
  as.character(signif(x, digits = 3))
}

################################################################################
## SHINY SERVER

shinyServer(function(input, output, session) {
  
  ########### CONDITIONS #############
  conds <- reactive({
    if (input$control) {
      groups = factor(c("Experimental","Control", 
                        levels = c("Experimental", "Control")))
    } else {
      groups = factor("Experimental")
    }
    expand.grid(group = groups,
                condition = factor(c("Longer looking predicted",
                                     "Shorter looking predicted"))) %>%
      group_by(group, condition) 
  })
  
  ########### GENERATE DATA #############
  data <- reactive({
    if (input$go | !input$go) {
    conds() %>%
      do(data.frame(looking.time = ifelse(rep(.$group == "Experimental" & 
                                                .$condition == "Longer looking predicted", input$N), 
                                          rnorm(n = input$N, 
                                                mean = input$m + (input$d * input$sd)/2, 
                                                sd = input$sd),
                                          rnorm(n = input$N, 
                                                mean = input$m - (input$d * input$sd)/2, 
                                                sd = input$sd))))
    }
  })
  
  ########### MEANS FOR PLOTTING #############
  ms <- reactive({
    data() %>%
      group_by(group, condition) %>%
      summarise(mean = mean(looking.time), 
                ci = ci95.t(looking.time), 
                sem = sem(looking.time)) %>%
    rename_("interval" = input$interval)
  })
  
  ########### BAR GRAPH #############
  output$bar <- renderPlot({
    pos <- position_dodge(width=.25)
    qplot(group, mean, fill = condition, 
          colour = condition,
          position = pos,
          stat = "identity", 
          geom="bar", width = .25,
          data = ms()) + 
      geom_linerange(data = ms(), 
                     aes(x = group, y = mean, 
                         fill = condition, 
                         ymin = mean - interval, 
                         ymax = mean + interval), 
                     position = pos,                     
                     colour = "black") + 
      xlab("Group") +
      ylab("Simulated Looking Time")  })
  
  ########### SCATTER PLOT #############
  output$scatter <- renderPlot({
    print(ms())
    print(data())
    pos <- position_jitterdodge(jitter.width = .1,
                                dodge.width = .25)
    qplot(group, looking.time, fill = condition, 
          colour = condition, 
          group = condition, 
          position = pos, 
          geom="point", 
          data = data()) + 
      geom_linerange(data = ms(), 
                     aes(x = group, y = mean, 
                         fill = condition, 
                         ymin = mean - interval, 
                         ymax = mean + interval), 
                     position = pos, 
                     size = 2, 
                     colour = "black") + 
      xlab("Group") +
      ylab("Looking Time") + 
      ylim(c(0, ceiling(max(data()$looking.time)/5)*5))
  })
  
  ########### STATISTICAL TEST OUTPUTS #############
  output$stat <- renderText({
    p.e <- t.test(data()$looking.time[data()$condition == "Longer looking predicted" & data()$group == "Experimental"],
                  data()$looking.time[data()$condition == "Shorter looking predicted" & data()$group == "Experimental"], 
                  paired = TRUE)$p.value
    
    stat.text <- paste("A t.test of the experimental condition is ",
          ifelse(p.e > .05, "non", ""),
          "significant at p = ", 
          pretty.p(p.e),
          ". ", 
          sep = "")
    
    if (input$control) {      
      p.c <- t.test(data()$looking.time[data()$condition == "Longer looking predicted" & data()$group == "Control"],
                    data()$looking.time[data()$condition == "Shorter looking predicted" & data()$group == "Control"], 
                    paired = TRUE)$p.value
          
      a <- anova(lm(looking.time ~ group * condition, data = data()))
      
      return(paste(stat.text,
                   "A t.test of the control condition is ",
                   ifelse(p.c > .05, "non", ""),
                   "significant at p = ",
                   pretty.p(p.c),
                   ". An ANOVA ",
                   ifelse(a$"Pr(>F)"[3] < .05,
                          "shows a", "does not show a"),
                   " significant interaction at p = ",
                   pretty.p(a$"Pr(>F)"[3]),
                   ".",
                   sep = ""))    
    } 
    
    return(stat.text)
  })
  
  ########### POWER COMPUTATIONS #############
  output$power <- renderPlot({
    if (input$control) {
      ns <- seq(5, 120, 5)
      pwrs <- data.frame(ns = ns,
                         Experimental = pwr.p.test(h = input$d, 
                                         n = ns, 
                                         sig.level = .05)$power,
                         Control = pwr.p.test(h = 0, 
                                              n = ns, 
                                              sig.level = .05)$power,
                         Interaction = pwr.2p.test(h = input$d, 
                                                   n = ns,                                                   
                                                   sig.level = .05)$power) %>%
        gather(condition, ps, Experimental, Interaction, Control) 
  
      
      this.pwr <- data.frame(ns = rep(input$N, 3),  
                             ps = c(pwr.p.test(h = input$d,
                                             n = input$N, 
                                             sig.level = .05)$power,
                                    pwr.p.test(h = 0,
                                               n = input$N, 
                                               sig.level = .05)$power,
                                    pwr.2p.test(h = input$d,
                                               n = input$N, 
                                               sig.level = .05)$power),
                             condition = c("Experimental", "Interaction",
                                           "Control"))
      qplot(ns, ps, col = condition, 
            geom=c("point","line"),
            data=pwrs) + 
        geom_point(data = this.pwr,
                   col = "red", size = 6) + 
        geom_hline(yintercept=.8, lty=2) + 
        geom_vline(xintercept=pwr.p.test(h = input$d, 
                                         sig.level = .05, 
                                         power = .8)$n, lty=2) + 
        geom_vline(xintercept=pwr.2p.test(h = input$d, 
                                         sig.level = .05, 
                                         power = .8)$n, lty=2) +         
        ylim(c(0,1)) + 
        ylab("Power to reject the null at p < .05") 
  
    } else { 
      ns <- seq(5, 120, 5)
      pwrs <- data.frame(ns = ns,
                         ps = pwr.p.test(h = input$d, 
                                         n = ns, 
                                         sig.level = .05)$power)
      
      this.pwr <- data.frame(ns = input$N, 
                             ps = pwr.p.test(h = input$d,
                                             n = input$N, 
                                             sig.level = .05)$power)
      qplot(ns, ps, geom=c("point","line"),
            data=pwrs) + 
        geom_point(data = this.pwr,
                   col = "red", size = 6) + 
        geom_hline(yintercept=.8, lty=2) + 
        geom_vline(xintercept=pwr.p.test(h = input$d, 
                                         sig.level = .05, 
                                         power = .8)$n, lty=2) + 
        ylim(c(0,1)) + 
        ylab("Power to reject the null at p < .05") + 
        xlab("Number of participants (N)")
    }
  })
})