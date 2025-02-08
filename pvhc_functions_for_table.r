# pvhc_functions_for_table.r

makType <- function(t) {
  filter(pvhcAllTrans , acct1 == t) %>%
    filter(date <= filterDte) %>%
    mutate(Month = lubridate::month(date , label = TRUE , abbr = TRUE)) %>%
    mutate(Year = lubridate::year(date)) %>%
    mutate(amount_num = ifelse(
      acct1=='Owners draw' & acct2 != 'Salary' , -amount_num , amount_num)) %>%
    mutate(amount_num = ifelse(
      is.na(amount_num) , 0 , amount_num)) %>%
    filter(between(Year , 2019 , year(fy_start))) %>%
    group_by(acct1 , acct2 , Year , Month) %>%
    summarise(amount_ttl = sum(amount_num)) %>%
    ungroup() %>%
    pivot_wider(names_from = Month , 
                values_from = amount_ttl) %>%
    mutate(ytd = rowSums(.[, c(4:15)] , na.rm = TRUE)) %>%
    filter(Year == year(fy_start)) %>%
    mutate(Year = as.character(Year)) %>%
    arrange(acct2) %>%
    select(acct2 , acct1 , everything()) %>%
    #  adorn_totals(na.rm = T , fill = t , name = sprintf('Total %s' , t)) %>%
    mutate(across(Jan:ytd , ~na_if(., 0))) %>%
    mutate_at(vars(c('Jan':'ytd')) , 
              ~ifelse(acct2 != 'Salary' & acct2 != 'Total Owners draw', 
                      -. , .))
}

# for net less owners draw
makType_NLOD <- function(t) {
  filter(pvhcAllTrans , acct1 == t) %>%
    filter(date <= filterDte) %>%
    mutate(Month = lubridate::month(date , label = TRUE , abbr = TRUE)) %>%
    mutate(Year = lubridate::year(date)) %>%
    mutate(amount_num = -amount_num) %>%
    # mutate(amount_num = ifelse(
    #   acct1=='Owners draw' & acct2 != 'Salary' , -amount_num , amount_num)) %>%
    mutate(amount_num = ifelse(
      is.na(amount_num) , 0 , amount_num)) %>%
    filter(between(Year , 2019 , year(fy_start))) %>%
    group_by(acct1 , acct2 , Year , Month) %>%
    summarise(amount_ttl = sum(amount_num)) %>%
    arrange(Month) %>%
    ungroup() %>%
    pivot_wider(names_from = Month , 
                values_from = amount_ttl) %>%
    arrange(match(acct2 , c(
      'Salary' ,
      'Charitable donations' ,
      'HSA contribution' , 
      'Health insurance' ,
      'IRA contribution' ,
      'Tax payments' ,
      "Ownerś Draw Other"))) %>%
    mutate(ytd = rowSums(.[, c(4:15)] , na.rm = TRUE)) %>%
    filter(Year == year(fy_start)) %>%
    mutate(Year = as.character(Year)) %>%
    #  arrange(acct2) %>%
    select(acct2 , acct1 , everything()) %>%
    {if (currM != 'Dec') {
      mutate(. , across(nextM:Dec, ~as.character(.))) 
    } else {.}} %>%
    bind_rows(. , 
              netInc %>%
                # mutate(across(nextM:Dec, ~as.character(.))) %>%
                # adorn_totals(name = 'Net Income') %>%
                filter(acct2 == 'Net Income')) %>%
    adorn_totals(na.rm = T , fill = NA , name = 'Net Less Draw') %>%
    mutate_at(vars(c('Jan':'ytd')) ,
              ~ifelse(acct2 != 'Net Income' & 
                        acct2 != 'Net Less Draw' & 
                        is.numeric(.),
                      -. , .))
}

