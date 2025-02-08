# pvhc_functions_for_charts.r
# functions to run for PVHC monthly report

# these are used for the summary charts, not used for table

# df for line chart

chtDf <- function(data) {
  arrange(data , Year , Month) %>% 
    group_by(Year) %>%
    mutate(amt_cum = cumsum(amt_sum)) %>%
    ungroup()
}

# create chart
crCht <- function(data) {
  ggplot(data , aes(x = Month , y = amt_cum , shape = Year , 
                    group = Year , color = Year)) + 
    geom_line() +
    geom_point(size = 3) +
    scale_shape_discrete(name = NULL) +
    scale_color_manual(values = clrs , name = NULL) 
}

# create base file for account totals
accts <- function(df , startyr , endyr) {
  df %>%
    filter(acct1 == 'Income' | acct1 == 'Expense' | acct1 == 'Owners draw') %>%
    mutate(Month = lubridate::month(date , label = TRUE , abbr = TRUE)) %>%
    mutate(Year = lubridate::year(date)) %>%
    mutate(amount_num = ifelse(
      acct1=='Income' , -amount_num , amount_num)) %>%
    mutate(amount_num = ifelse(
      is.na(amount_num) , 0 , amount_num)) %>%
    filter(between(Year , startyr , endyr)) %>%
    group_by(acct1 , acct2 , Year , Month) %>%
    summarise(amount_ttl = sum(amount_num)) %>%
    ungroup() 
}


# income, expense and owners draw subtotals
subs <- function(data) {
  group_by(data , acct1 , Year , Month) %>%
    summarise(amount_ttl = sum(amount_ttl)) %>%
    mutate(acct2 = ifelse(
      acct1 == 'Expense' , 'Total expense' ,
      ifelse(acct1 == 'Income' , 'Total income' ,
             ifelse(acct1 == 'Owners draw' , 'Total owners draw' , 
                    NA))))
}


# income less expense totals
ile <- function(data) {
  filter(data , acct1 == 'Income' | acct1 == 'Expense') %>%
    mutate(amount_ttl = ifelse(
      acct1 == 'Expense' , -amount_ttl , amount_ttl)) %>%
    group_by(Year , Month) %>%
    summarise(amount_ttl = sum(amount_ttl)) %>%
    mutate(acct2 = 'Income less expense')
}

# net after owners draw totals
naod <- function(data) {
  filter(data , acct1 == 'Income' | acct1 == 'Expense' | acct1 == 'Owners draw') %>%
    mutate(amount_ttl = ifelse(
      (acct1 == 'Expense' | acct1 == 'Owners draw') , -amount_ttl , amount_ttl)) %>%
    group_by(Year , Month) %>%
    summarise(amount_ttl = sum(amount_ttl)) %>%
    mutate(acct2 = 'Net after owners draw')
}

# filtering out PPP, or any acct2
filtA2 <- function(data , x) {
  filter(data , acct2 != x) %>%
    mutate(Month = lubridate::month(date , label = TRUE , abbr = TRUE)) %>%
    mutate(Year = lubridate::year(date)) %>%
    mutate(amount_num = ifelse(
      acct1=='Income' , -amount_num , amount_num)) %>%
    arrange(Year) %>%
    group_by(Year , Month , acct1 ) %>%
    summarise(amt_sum = sum(amount_num)) %>%
    mutate(amt_sum = ifelse(
      acct1 == 'Expense' , -amt_sum , amt_sum)) %>%
    group_by(Year , Month) %>%
    summarise(acct1 = 'IncLessExp' , 
              amt_sum = sum(amt_sum)) %>%
    ungroup() %>% 
    mutate(Month = fct_relevel(Month , 
                               'Jan' , 'Feb' , 'Mar' , 'Apr' , 'May' , 'Jun' ,
                               'Jul' , 'Aug' , 'Sep' , 'Oct' , 'Nov' , 'Dec')) 
}

yXyCum <- function(data) {
  mutate(data , Year = as.factor(Year)) %>%
    ggplot(aes(x = Month , y = amt_cum ,  
               group = Year , color = Year)) +
    geom_line() +
    geom_point(size = 3) +
    scale_shape_discrete(name = NULL) +
    #  scale_color_manual(values = clrs , name = NULL) 
    scale_y_continuous(labels=scales::dollar_format())
  # limits = c(0 , 110000),
  # breaks = c(0 , 20000 , 40000 ,
  #            60000 , 80000 , 100000))
}

yXyByMo <- function(data) {
  filter(data , Year >= 2020) %>%
    mutate(Year = as.factor(Year)) %>%
    ggplot(aes(x = Month , y = amt_sum ,  
               fill = Year)) +
    geom_col(position = 'dodge2') +
    #  scale_fill_manual(values = clrs , name = NULL) +
    #  scale_color_manual(values = clrs , name = NULL) 
    scale_y_continuous(labels=scales::dollar_format())
}

# end user-defined chart functions

