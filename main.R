
library(ggplot2)
library(tidyr)
library(dplyr)
library(RColorBrewer)
library(scales)
library(gridExtra)

calculate_netto <- function(income, term){
  
 income <-  income
 term <- term
 incomeMonthly <- income/term
 health_soc_base <- ifelse(income < (441.5 * term), 441.5 * term, income) 
 soc_upper <- ifelse(health_soc_base/term <= 6181, health_soc_base, 6181 * term) # upper base limit
 
 
 medicare <- 0.044 * soc_upper
 oldAge <- 0.18 * soc_upper
 disability <- 0.06 * soc_upper
 unemployment <- 0.02 * soc_upper
 solidarityFund <- 0.0475 * soc_upper
 socAll <- medicare + oldAge + disability + unemployment + solidarityFund
 soc <- c(medicare, oldAge, disability, unemployment, solidarityFund)
 
 health <- 0.14 * health_soc_base
 
 fixedExpenses <- 0.6 * income
 fixedExpenses_cond <- fixedExpenses < 20000
 fixedExpenses <- ifelse(fixedExpenses_cond, fixedExpenses, 20000)
 
 taxBase1 <- income - fixedExpenses - sum(soc) - health
 taxBase1 <- ifelse(taxBase1 <= 0, 0, taxBase1)
 nonTaxable <- ifelse(taxBase1/term <= 1650.75, 316.94 * term, 8755.578 - (taxBase1/4))
 nonTaxable <- ifelse(nonTaxable <= 0, 0, nonTaxable)
 taxBase <- taxBase1 - nonTaxable
 taxBase <- ifelse(taxBase <= 0, 0, taxBase)
 
 incomeTax19 <- ifelse(taxBase <= 35022.31, 0.19 * taxBase, 0.19 * 35022.31)
 incomeTax25 <- ifelse(taxBase > 35022.31, 0.25 * (taxBase - 35022.31), 0)

 netto <- income - sum(soc) - health - incomeTax19 - incomeTax25
 nettoMonthly <- netto/term
 
 result <- c(income, incomeMonthly, term, medicare, oldAge, disability, unemployment, solidarityFund, socAll, health,
                fixedExpenses, nonTaxable, taxBase1, taxBase, incomeTax19, incomeTax25, netto, nettoMonthly)
 
 
 
 return(result)
 
  
}

main <- function(){
  
 brutto <- seq(10000, 100000, 500)
 term <- 12
 toPlot <- sapply(brutto, calculate_netto, term = term)
 toPlot <- as.data.frame(t(toPlot))
 colnames(toPlot) <-c('brutto', 'bruttoMonthly', 'term', 'medicare', 'oldAge', 'disability', 
                      'unemployment', 'solidarityFund', 'socAll', 'health', 'fixedExpenses',
                      'nonTaxable', 'taxBase1', 'taxBase',
                      'incomeTax19', 'incomeTax25', 'netto', 'nettoMonthly')
 
 annotations <- toPlot[seq(1,181, 10), c('netto', 'nettoMonthly', 'brutto', 'bruttoMonthly')]

 incomeTax19_min <- min(toPlot[toPlot$incomeTax19 > 0, ,]$brutto)
 incomeTax25_min <- min(toPlot[toPlot$incomeTax25 > 0, ,]$brutto)
 fixedExpenses_upperLimit <- min(toPlot[toPlot$fixedExpenses == 20000,]$brutto)

 socBase_upperLimit <- 6181 * 12
 nonTaxable_change <- 77000
 
 v_lines <- c(incomeTax19_min, incomeTax25_min, fixedExpenses_upperLimit, socBase_upperLimit, nonTaxable_change)

 toPlot1 <- gather(toPlot, deduction, value, c(-brutto, -netto))

 finalPlot1 <- ggplot() + 
   geom_area(aes(fill = deduction, x = brutto, y = value), 
             data = toPlot1[!(toPlot1$deduction %in% c('taxBase', 'taxBase1', 'term', 'nettoMonthly', 'fixedExpenses', 'bruttoMonthly', 'socAll', 'nonTaxable', 'taxBase1', 'taxBase')),], 
             position = 'stack', 
             alpha = 0.7) + 
   geom_line(aes(x = brutto, y = netto), 
             data = toPlot1, 
             color = 'red', 
             linetype = 2, 
             size = 1) +
   geom_vline(xintercept = v_lines, linetype = 'dotted', size = 1, alpha = 0.5) +
   theme(panel.background = element_rect(fill =NA),
         panel.grid.major = element_line(colour = '#e5e5e5'),
         axis.line = element_line(colour = '#BDBDBD'), 
         legend.background = element_rect(colour = "black"), 
         panel.grid.minor = element_line(linetype = 'dotted', colour = '#e5e5e5'), 
         legend.position = 'bottom', 
         legend.title = element_blank(),
         plot.title = element_text(hjust = 0.5),
         plot.margin = unit(c(1,1,2,1), 'lines')) + 
   ggtitle('Deductions and income tax with fixed expenses 60% from income') + 
   ylab('EUR') + 
   xlab('Brutto income [EUR]') + 
   scale_fill_brewer(palette = 'Set3') + 
   scale_y_continuous(breaks = seq(0, 50000, 5000), labels = comma) + 
   scale_x_continuous(breaks = seq(0, 100000, 10000), labels = comma) +
   annotate('text', x = annotations$brutto, 
            y = annotations$netto + 3000, 
            label = round(annotations$nettoMonthly, 1), 
            colour = 'darkblue')  +
   annotate('text', x = annotations$brutto, 
            y = annotations$netto - 3000, 
            label = round(annotations$bruttoMonthly, 1), 
            colour = 'darkred') +
   annotate('text', x = 14000, y = 52000, label = '- - - netto income', colour = 'red') +
   annotate('text', x = 14000, y = 50000, label = 'netto monthly income', colour = 'darkblue') +
   annotate('text', x = 14000, y = 48000, label = 'brutto monthly income', colour = 'darkred') +
   annotate('text', x = 14000, y = 36000, label = 'medicare 4,4% \n old age 18% \n disability 6% \n unemployment 2% \n solidarity fund 4,75% \n health insurane 14% \n income tax 19/25%') +
   annotate('text', x = 30000, y = 54000, label = 'fixed expenses\n limit', size = 3) +
   annotate('text', x = 43500, y = 54000, label = '19% income tax\n start', size = 3) +
   annotate('text', x = 70500, y = 54000, label = 'social security\n base limit', size = 3) +
   annotate('text', x = 81000, y = 54000, label = 'non-taxable part\n calc. change', size = 3) +
   annotate('text', x = 91000, y = 24000, label = '25% income tax\n start', size = 3) 
 
 toPlot2 <- toPlot %>%
   mutate(ratio = netto/brutto)
 
 finalPlot2 <- ggplot(aes(x = brutto, y = ratio), data = toPlot2) +
   geom_line(color = 'red', linetype = 2, size = 1)+
   theme(panel.background = element_rect(fill =NA),
         panel.grid.major = element_line(colour = '#e5e5e5'),
         axis.line = element_line(colour = '#BDBDBD'), 
         legend.background = element_rect(colour = "black"), 
         panel.grid.minor = element_line(linetype = 'dotted', colour = '#e5e5e5'), 
         legend.position = 'bottom', 
         legend.title = element_blank(),
         plot.title = element_text(hjust = 0.5)) +
   ggtitle('Percentage of netto income from brutto') +
   geom_hline(yintercept = 0.5) + 
   scale_x_continuous(breaks = seq(0, 100000, 10000), labels = comma) + 
   scale_y_continuous(limits = c(0.46, 0.52), labels = percent)
 
 plots <- grid.arrange(finalPlot1, finalPlot2, nrow = 4, ncol = 1,
                       layout_matrix = matrix(c(1, 1, 1, 2)))
 
 print(plots)

return(toPlot)
  
}

main()