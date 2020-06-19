# load requirements
library(shiny)
library(shinydashboard)
library(janitor)
library(glue)
library(tidyverse)
library(data.table)
# library(DT)

# set random seed
set.seed(1965)

# global data
metrics <- readRDS('data/metrics.RDS') %>%
  as.data.table()

stress <- as.data.table(readRDS('data/stress.RDS') %>% janitor::clean_names(.))

institutions <- stress[order(institution_name, state)][
  , .(unitid, institution_name, control, state, entry = glue("{institution_name} ({state})"))]

metric_names <-
  function(sector) {
    # metric names for sector of institution
    str_sort(c(ifelse(sector == 'Public', 'State Appropriations', 'Endowment/Expenses'),
               c('UG First-time First Year Enrollment', 'Retention', 'Market Price')))
  }

set_parameters <-
  function(unitid) {
    # display parameters
    list(unitid = unitid,
         sector = institutions$control[institutions$unitid == unitid],
         institution_name = institutions$institution_name[institutions$unitid == unitid],
         metric_names = metric_names(institutions$control[institutions$unitid == unitid]))
  }

run_model <- function(df) {
  lm(formula = Value ~ year_key, 
     data = df)
}

# function to create standard-formatted box plots with reference institution
box_me <- function(metric_name, parameters)
  {
    df <- metrics[Metric == metric_name & Control == parameters$sector]
  
    df2 <- df[Unitid == parameters$unitid]
  
    ggplot(data = df,
           aes(x = `Fiscal Year`, y = Value)) +
  
      # box plot for reference group
      geom_boxplot(color = '#1f78b4', fill='#a6cee3', alpha = 1/5) +
  
      # line plot for reference institution
      geom_line(data = df2,
                aes(x = `Fiscal Year`, y = Value, group = 1),
                color = 'black',
                size = 1.5) +
  
      # points for reference institution
      geom_point(data = df2,
                 aes(x = `Fiscal Year`, y = Value),
                 color = 'black',
                 shape = 18,
                 size = 6)

}


plot_me <-
  function(metric_name, parameters) {
    df <- 
      metrics %>%
      filter(Metric == metric_name,
             Unitid == parameters$unitid)
    
    # first_fy <- metrics$`Fiscal Year`[metrics$year_key == min(metrics$year_key)]
    first_year <- min(df$year_key)
    
    df <-
      df %>%
      select(Unitid, `Institution Name`, State, Control, Metric, Alert, Warning) %>%
      distinct() %>%
      expand_grid(year_key = seq(max(metrics$year_key) + 1, max(metrics$year_key) + 3)) %>%
      mutate(`Fiscal Year` = str_c(year_key - 1, year_key, sep='-')) %>%
      bind_rows(df, .) %>%
      group_by(Unitid, Metric) %>%
      nest() %>%
      ungroup() %>%
      mutate(fit = map(data, run_model),
             Trend = map2(fit, data, predict)) %>%
      select(-fit) %>%
      unnest(cols = c('data', 'Trend'))
    
    labels <- 
      df %>%
      select(Alert, Warning) %>%
      distinct() %>%
      pivot_longer(1:2, names_to = 'label', values_to = 'Value') %>%
      mutate(`Fiscal Year` = str_c(first_year - 1, first_year, sep='-'))
      
    
    df %>%
      ggplot(aes(x = `Fiscal Year`,
                 y = Value,
                 group = 1)) +
      geom_line(size = 1.5) +
      
      # add linear trend line
      geom_line(aes(y = Trend),
                linetype = 'longdash',
                size = 1,
                color = '#7570b3') +
      
      # add alert and warning lines
      geom_hline(aes(yintercept = Alert),
                 linetype = 'dotted',
                 color = '#1b9e77',
                 size = 1) +
      geom_hline(aes(yintercept = Warning),
                 linetype = 'dotted',
                 color = '#d95f02',
                 size = 1) +
      
      # Add line labels
      geom_text(aes(label = label,
                    y = Value),
                data = labels,
                hjust = "left")
  }


