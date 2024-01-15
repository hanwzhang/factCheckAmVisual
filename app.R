#library(shiny)
library(tidyverse)
library(haven)
library(ggplot2)
library(plotly)
library(ggthemes)
library(weights)
library(shinythemes)
library(sjlabelled)

gss87_21 <- read_dta('gssalt.dta') |> mutate(born = factor(born, levels = c(1,2), labels = c('native','immigrant'))) |>
  mutate(immyear = ifelse(born == 'native', 'native',
                          ifelse(yearsusa <=20, '<=20yrs','>20yrs'))) |>
  mutate(immyear = factor(immyear, levels = c('native','>20yrs','<=20yrs'))) |>
  mutate(immage = age - yearsusa) |>
  mutate(immage = ifelse(born == 'native', 'native',
                          ifelse(immage <=24, '<=24','>24'))) |>
  mutate(immage = factor(immage, levels = c('native','<=24','>24'))) |>
  mutate(gen = factor(gen, levels = c('first','second','third','native'), labels = c('1st','2nd','3rd','other native')))
  
gss21 <- gss87_21 |> filter(year == 2021)

opnames <- c('hard work' = 'ophrdwrk_essential',
             'a good education' = 'opeduc_essential',
             'wealth' = 'opwlth_vimpEssential',
             "parents' education" = 'oppared_vimpEssential',
             'political connections' = 'opclout_vimpEssential',
             'sex' = 'opsex_vimpEssential',
             'race' = 'oprace_vimpEssential',
             'religion' = 'oprelig_vimpEssential',
             'knowing the right people' = 'opknow_vimpEssential',
             'immigrant gen.' = 'gen',
             'years in US' = 'immyear',
             'age at migration' = 'immage',
             'birth' = 'born')

# DEFINE UI
ui <- fluidPage(
  theme = shinytheme('journal'),
  titlePanel("Fact-Checking the American Dream: Do Immigrants Believe in Meritocracy More?"),
  withTags({div(class = 'header', checked = NA,
                #h3(style = 'text-align:center','Welcome'),
                p("Popular theories of migration suggest that immigrants are attracted to the US by better wages, opportunities, and the meritocratic idea that anyone can get ahead through hard work - 'the American Dream'. But do immigrants really see the American society as meritocratic?"),
                hr()
  )}),
  withTags({div(h3("1. What do people see as important for getting ahead?"))}),
  fluidRow(column(8,
                  withTags({div(p("Believers of meritocracy see success as a result of ",b('Hard Work'), " and ",b('Education')," (as a measure of hard work and a selection mechanism for success)."))}),
                  fluidRow(column(6,plotlyOutput("hrdwrkOY")),
                           column(6,plotlyOutput('educOY'))),
                  withTags({div(class = 'header', checked = NA,
                                p("You can hover over a data point to see its description. Over the past 4 decades, immigrants' belief in education outgrew that of native-borns, but their belief in hard work has fallen behind. Being more educated may cause one to be more invested in its importance for success:")
                  )}),
                  plotlyOutput("immEDU"),
                  withTags({div(class = 'header', checked = NA,
                                p('A higher proportion of immigrants are highly educated today - thanks to policies that facilitate "brain drain" and education as a migration pathway while thwarting the migration of less educated populations.
                                In other words, the image of a "desirable" immigrant enforced by the receiving nation manifests itself as what immigrants perceive as the path of least resistance to social mobility.
                                  This could explain why immigrants attribute more importance to education than native-borns. But why has their belief in hard work stagnated - ',b("don't they see hard work as necessary for attaining education?")),
                  )})
                  ),
           column(4,
                  withTags({div(p("You can view how other beliefs have changed over the 40+ years by changing the variables here."))}),
                  selectInput('leftBelief','Belief 1 (left)',
                              choices = list('hard work' = 'ophrdwrk_essential',
                                             'a good education' = 'opeduc_essential',
                                             'wealth' = 'opwlth_vimpEssential',
                                             "parents' education" = 'oppared_vimpEssential',
                                             'knowing the right people' = 'opknow_vimpEssential'),
                              selected = 'ophrdwrk_essential'),
                  selectInput('rightBelief','Belief 2 (right)',
                              choices = list('hard work' = 'ophrdwrk_essential',
                                             'a good education' = 'opeduc_essential',
                                             'wealth' = 'opwlth_vimpEssential',
                                             "parents' education" = 'oppared_vimpEssential',
                                             'knowing the right people' = 'opknow_vimpEssential'),
                              selected = 'opeduc_essential'),
                  withTags({div(p("You can change how we break down the population here."))}),
                  selectInput('opCat',NULL,
                              choices = list('immigrant vs. native-born*' = 'born',
                                             '1st, 2nd, 3rd generation immigrant**' = 'gen',
                                             'years immigrants spent in the US' = 'immyear',
                                             'age when immigrated to the US' = 'immage'),
                              selected = 'born'),
                  )),
  withTags({div(hr(),h3("2. Education: A result of hard work or wealth?"))}),
  fluidRow(column(8,
                  fluidRow(column(6,plotlyOutput('HwByEdu')),
                           column(6,plotlyOutput('WlthByEdu'))),
                  withTags({div(p("(Responses are from the 2021 wave.) Believing education to be essential has a much bigger positive effect on native-borns' belief in hard work than on immigrants'.
                                  On the other hand, it also has a bigger positive effect on immigrants' belief in wealth than native-borns'."),
                                p("This means ",b("immigrants are more likely to see wealth as necessary for attaining education and thus success, while native-borns are more likely to see education as dependent on hard work."),
                                  "Immigrant families may be more aware of the structures in our education system that privilege the wealthy and exclude those without capital, and/or they more than native-borns see education as requiring investment.
                                  Immigrants not only practice agency by navigating systems of oppression with resilience. They also practice ", b("an intellectual form of agency,"),
                                  "where they resist ideals that naturalize inequality as a result of personal hard work and instead acknowledge the roles of ascribed/inherited status in social mobility.")
                  )})
                  ),
           column(4,
                  withTags({div(p("You can change the parameters here to see how other beliefs are affected by the interaction of belief in education and characteristics of the population."))}),
                  selectInput('interactionY1','How the probability of Belief A (left)',
                              choices = list('viewing hard work as essential' = 'ophrdwrk_essential',
                                             'viewing wealth as very important / essential' = 'opwlth_vimpEssential',
                                             'viewing parent education as very important / essential' = 'oppared_vimpEssential',
                                             'viewing political connection as very important / essential' = 'opclout_vimpEssential',
                                             'viewing knowing the right people as very important / essential' = 'opknow_vimpEssential',
                                             'viewing race as very important / essential' = 'oprace_vimpEssential',
                                             'viewing religion as very important / essential' = 'oprelig_vimpEssential',
                                             'viewing sex as very important / essential' = 'opsex_vimpEssential'),
                              selected = 'ophrdwrk_essential'),
                  selectInput('interactionY2','versus Belief B (right)',
                              choices = list('viewing hard work as essential' = 'ophrdwrk_essential',
                                             'viewing wealth as very important / essential' = 'opwlth_vimpEssential',
                                             'viewing parent education as very important / essential' = 'oppared_vimpEssential',
                                             'viewing political connection as very important / essential' = 'opclout_vimpEssential',
                                             'viewing knowing the right people as very important / essential' = 'opknow_vimpEssential',
                                             'viewing race as very important / essential' = 'oprace_vimpEssential',
                                             'viewing religion as very important / essential' = 'oprelig_vimpEssential',
                                             'viewing sex as very important / essential' = 'opsex_vimpEssential'),
                              selected = 'opwlth_vimpEssential'),
                  # selectInput('interactionX','vary in relation to',
                  #             choices = list('respondent class'='classbinary',
                  #                            'respondent race'='racebinary',
                  #                            'education level'='degree',
                  #                            'family income at age 16'='incom16',
                  #                            'whether hard work viewed essential' = 'ophrdwrk_essential',
                  #                            'whether education viewed essential' = 'opeduc_essential',
                  #                            'whether wealth viewed very important / essential' = 'opwealth_vimpEssential',
                  #                            'whether parent education viewed very important / essential' = 'oppared_vimpEssential',
                  #                            'whether political connection viewed very important / essential' = 'opclout_vimpEssential',
                  #                            'whether knowing the right people viewed very important / essential' = 'opknow_vimpEssential',
                  #                            'whether race viewed very important / essential' = 'oprace_vimpEssential',
                  #                            'whether religion viewed very important / essential' = 'oprelig_vimpEssential',
                  #                            'whether sex viewed very important / essential' = 'opsex_vimpEssential'),
                  #             selected = 'opwealth_vimpEssential'),
                  selectInput('interactionGroup','vary with belief in education, across',
                              choices = list('immigrant vs. native-born*' = 'born',
                                             '1st, 2nd, 3rd generation immigrant**' = 'gen',
                                             'years spent in the US' = 'immyear',
                                             'age when immigrated' = 'immage'),
                              selected = 'born')
                  )),
    withTags({div(hr(),h3('3. Immigrant mobility across generations'))}),
    fluidRow(column(8,
                    withTags({div(p("Looking at college attendance among children of immigrants vs. native-borns reveals how immigrants' association of education with wealth amounts to mobility in their second generation.
                                    The accumulation of wealth and its subsequent investment in children's education by first-generation immigrants made possible the education mobility among their children."))}),
                    plotlyOutput('gen'),
                    withTags({div(p("In both groups, there is a positive correlation between ",b("household income when one is 16 years old")," and ",b("college attendance."),
                    "But the correlation is stronger among immigrants than among native-borns. This might be due to immigrant families' tendency to invest a higher proportion of their income in children's education."),
                    p("To end on a positive note, immigrants' orientation towards the next generation is reflected in their relative optimism in their children's happiness, too:")
                    )})
                    ),
             column(4,)),
    fluidRow(column(8,
                    plotlyOutput('kid'),
                    withTags({div(p("What we found cautions us against drawing conclusions about social mobility from the trajectories of individuals;
                                    For many, mobility is achieved in the form of families and in timeframes that span multiple generations."),
                                  p("It should also alert us that immigrants increasingly experience the American society and education system as one rigged by wealth and privilege.
                                    This complicates the conventional understanding of migration pull factors, which assumes that migrants are attracted by the promise of improving living standards through their own labor.
                                    Instead, migrants may increasingly recognize the U.S. society as non-meritocratic. While this may appeal to immigrants with wealth and connections more than those with few resources, immigration remains on the rise on both ends of this spectrum.
                                    The awareness of non-meritocracy determinants of success, then, becomes evidence of immigrant agency - their staying vigilant of simplistic narratives about success and their efforts to overcome structural barriers."),
                    )})),
             column(4,
                    withTags({div(p("You can choose other ways to compare perceived prospects for one's children here."))}),
                    selectInput('prospectsGroup','compare across',
                                choices = list('immigrant vs. native-born*' = 'born',
                                               '1st, 2nd, 3rd generation immigrant**' = 'gen',
                                               'years spent in the US' = 'immyear',
                                               'age when immigrated' = 'immage'),
                                selected = 'born')
                    )
             ),
  withTags({div(hr(),
                h4('Endnotes'),
                ol(
                li("This project uses data from the 1987 and 2021 waves of the ",a("General Social Survey.",href = 'https://gss.norc.org/')),
                li('*Immigrant status is constructed from the question, "Are you born in the U.S.?" Only respondents who responded to this question is included in our sample.
                   Respondents who answered "yes" are categorized as native-borns and those who answered "no" as immigrants.'),
                li('**Immigrant generation is constructed from the questions: "Are you born in the U.S.?", "Are your parents born in the U.S.?", and "Are your grandparents born in the U.S.?".
                  Respondents born outside the U.S. are categorized as 1st-generation; Respondents born in the U.S. whose parents are born outside of the U.S. as 2nd-generation;
                  And respondents whose parents and themselves are born in the U.S. but have grandparents born outside of the U.S. as 3rd-generation.
                   Therefore, "other native" includes native-borns whose family has been in the U.S. for >=3 generations but also native-borns who did not report the birth status of their parents or grandparents.
                   This method may underestimate the actual number of immigrants, but ensures that those categorized as 1st-, 2nd-, and 3rd-generation immigrants are defined by the shared awareness of an immigration story in their family.'))
                )})
)

# SERVER LOGIC
server <- function(input, output) {
    output$hrdwrkOY <- renderPlotly({
      hrdwrkOY <- gss87_21 |>
        group_by(year, .data[[input$opCat]]) |> summarise(mean = mean(.data[[input$leftBelief]], na.rm = TRUE)) |>
        filter(!is.na(.data[[input$opCat]])) |>
        ggplot(aes(x = factor(year), y = mean, group = .data[[input$opCat]], color = .data[[input$opCat]],
                   text = paste0('In ',year,', ',100*round(mean,3),'% of ',.data[[input$opCat]],'\nsee ',
                                 names(opnames)[opnames == input$leftBelief], ' as ',
                                 ifelse(str_detect(input$leftBelief,'vimp') == FALSE, 'essential', 'very important or essential')))) +
        geom_line() + geom_point()+
        labs(title = paste('Probability of emphasizing',names(opnames)[opnames == input$leftBelief]),
             x= NULL, y = NULL, color = names(opnames)[opnames == input$opCat]) +
        ylim(0.2,0.8) +
        theme(legend.position = 'bottom') +
        scale_color_brewer(palette = 'Set1')
      hrdwrkOY_plotly <- hrdwrkOY |> ggplotly(tooltip = 'text') |>layout(legend = list(orientation = "h")) #, x = 0.25, y = -0.1
    })
    
    output$educOY <- renderPlotly({
      educOY <- gss87_21 |>
        group_by(year, .data[[input$opCat]]) |> summarise(mean = mean(.data[[input$rightBelief]], na.rm = TRUE)) |>
        filter(!is.na(.data[[input$opCat]])) |>
        ggplot(aes(x = factor(year), y = mean, group = .data[[input$opCat]], color = .data[[input$opCat]],
                   text = paste0('In ',year,', ',100*round(mean,3),'% of ',.data[[input$opCat]], '\nsee ',
                                 names(opnames)[opnames == input$rightBelief],' as ',
                   ifelse(str_detect(input$rightBelief,'vimp') == FALSE, 'essential', 'very important or essential')))) +
        geom_line() + geom_point()+
        labs(title = paste('Probability of emphasizing',names(opnames)[opnames == input$rightBelief]),
             x= NULL, y = NULL, color = names(opnames)[opnames == input$opCat]) +
        ylim(0.2,0.8) +
        theme(legend.position = 'bottom') +
        scale_color_brewer(palette = 'Set1')
      educOY_plotly <- educOY |> ggplotly(tooltip = 'text') |>layout(legend = list(orientation = "h"))
    })
    
    output$immEDU <- renderPlotly({
      immEDU <- gss87_21 |> #filter(year == 2021) |>
        mutate(degreelb = sjlabelled::as_label(degree)) |>
        group_by(year, born, degreelb) |> summarise(num = n()) |> group_by(year, born) |> mutate(prop = num / sum(num)) |>
        ggplot(aes(x = born, y = num, fill = degreelb,
                   text = paste0(100*round(prop,3),'% of ',sjlabelled::as_label(born),' completed ',degreelb))) +
        geom_col(position = 'fill') + 
        labs(title = 'Immigrants excelling in education attainment',
             x = NULL,y = NULL,fill = NULL) +
        theme(legend.position = 'bottom') +
        scale_fill_brewer(palette = 'Spectral') + facet_wrap(~factor(year), nrow = 1)
      immEDU_plotly <- immEDU |> ggplotly(tooltip = 'text')
    })
    
    output$HwByEdu <- renderPlotly({
      HwByEdu <- gss21 |> filter(year == 2021) |> filter(!is.na(opeduc_essential)) |>
        mutate(opeduc_essential = as.character(opeduc_essential)) |>
        mutate(opeduc_essential = dplyr::recode(opeduc_essential, '1' = 'education essential', '0'='education not essential')) |>
        mutate(opeduc_essential = factor(opeduc_essential, levels = c('education not essential', 'education essential'))) |>
        group_by(.data[[input$interactionGroup]],opeduc_essential) |> summarise(mean = mean(.data[[input$interactionY1]], na.rm = TRUE)) |>
        #filter(!is.na(.data[[input$interactionGroup]])) |>
        ggplot(aes(x =factor(opeduc_essential), y = mean, group = .data[[input$interactionGroup]], color = .data[[input$interactionGroup]],
                   text = paste0(100*round(mean, 3),'% of ',.data[[input$interactionGroup]],' see ',
                                 names(opnames)[opnames == input$interactionY1], ' as ',
                                 ifelse(str_detect(input$interactionY1,'vimp') == FALSE, 'essential', 'very important or essential')))) +
        geom_line() + geom_point() +
        theme(legend.position = 'bottom') +
        ylim(0,0.8) +
        labs(title = paste0('Probability of emphasizing ',
                            names(opnames)[opnames == input$interactionY1]),
             x = NULL,y = NULL, color = names(opnames)[opnames == input$interactionGroup]) +
        scale_color_brewer(palette = 'Set1')
      HwByEdu_plotly <- HwByEdu |> ggplotly(tooltip = 'text') |> layout(legend = list(orientation = "h", x = 0, y = -0.1))
    })
    
    output$WlthByEdu <- renderPlotly({
      WlthByEdu <- gss21 |> filter(year == 2021) |> filter(!is.na(opeduc_essential)) |>
        mutate(opeduc_essential = as.character(opeduc_essential)) |>
        mutate(opeduc_essential = dplyr::recode(opeduc_essential, '1' = 'education essential', '0'='education not essential')) |>
        mutate(opeduc_essential = factor(opeduc_essential, levels = c('education not essential', 'education essential'))) |>
        group_by(.data[[input$interactionGroup]],opeduc_essential) |> summarise(mean = mean(.data[[input$interactionY2]], na.rm = TRUE)) |>
        #filter(!is.na(.data[[input$interactionGroup]])) |>
        ggplot(aes(x =factor(opeduc_essential), y = mean, group = .data[[input$interactionGroup]], color = .data[[input$interactionGroup]],
                   text = paste0(100*round(mean, 3),'% of ',.data[[input$interactionGroup]],' see ',
                                 names(opnames)[opnames == input$interactionY2], ' as ',
                                 ifelse(str_detect(input$interactionY2,'vimp') == FALSE, 'essential', 'very important or essential')))) + 
        geom_line() + geom_point() +
        theme(legend.position = 'bottom') +
        ylim(0,0.8) +
        labs(title = paste0('Probability of emphasizing ',
                            names(opnames)[opnames == input$interactionY2]),
             x = NULL,y = NULL, color = names(opnames)[opnames == input$interactionGroup]) +
        scale_color_brewer(palette = 'Set1')
      WlthByEdu_plotly <- WlthByEdu |> ggplotly(tooltip = 'text')|> layout(legend = list(orientation = "h", x = 0, y = -0.1))
    })
    
    output$kid <- renderPlotly({
      kid <- gss21 |> filter(!is.na(kidssolv))|>
        mutate(kid = factor(kidssolv, 
                            levels = c('1','2','3','4','5','6'),
                            labels = c('much better','somewhat better','about the same','somewhat worse','much worse','no children')))|>
        group_by(.data[[input$prospectsGroup]], kid) |>
        summarise(num = n()) |> mutate(prop = num / sum(num)) |>
        filter(!is.na(.data[[input$prospectsGroup]])) |>
        ggplot(aes(x =.data[[input$prospectsGroup]], y = num, fill = kid, text = round(prop,3))) + geom_col(position = 'fill') +
        labs(title = "One's children's living standards compared to oneself",
             x = NULL,y = NULL, fill = NULL) +scale_fill_brewer(palette = 'Spectral')
      kid_plotly <- kid |> ggplotly(tooltip = 'text')
    })
    output$gen <- renderPlotly({
      gen <- gss21 |>
        mutate(college = ifelse(degree >=3, 1, 0))|>
        filter(gen %in% c('2nd','other native')) |>
        filter(as.numeric(incom16) <=5) |> mutate(incom16 = as_label(incom16))|>
        mutate(gen = dplyr::recode(gen,'2nd' = '2nd-gen immigrant','other native' = 'other native-born')) |>
        group_by(incom16, gen) |> 
        summarise(mean = mean(college, na.rm = TRUE), num = n()) |>
        ggplot(aes(x = incom16, y = mean, group = gen, color = gen,
                   text = paste0(round(mean,3)*100,'% (n = ',num,')'))) +
        geom_line() + 
        geom_point() +
        labs(title = "College attendance rate by family income at the age of 16",
             x = NULL,y = NULL, color = NULL) +
        scale_color_brewer(palette = 'Set1')
      #scale_x_discrete(labels=c('far below', 'below average', 'average', 'above average', 'far above')) +
      gen_plotly <- gen |> ggplotly(tooltip = 'text') |>layout(legend = list(orientation = "h", x = 0, y = -0.1))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
