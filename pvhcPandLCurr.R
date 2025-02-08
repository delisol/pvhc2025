# pvhcPandLCurr.R

# current month

pvhcCurrTrans <- 
  pvhcAllTrans %>%
  filter(date >= curr_mo_start &
           date <= fy_to_date)

# create pre-table
incCurr <- makTab(x = quote('Income') , df = pvhcCurrTrans)
expCurr <- makTab(x = quote('Expense') , df = pvhcCurrTrans)

preTabCurr <- 
  bind_rows(
    incCurr %>%
      add_row(), 
    expCurr , 
    pvhcCurrTrans %>%
      ed_trans() %>%
      group_by() %>%
      summarise(amt_acct1 = sum(amount_num)) %>%
      mutate(type = 'NET_INCOME' ,
             acct1 = 'Net income for period')) %>%
  mutate(acct1 =
           ifelse(
             is.na(acct1) , acct2 , acct1)) %>%
  mutate(across(amt_acct1:amt_acct2 ,
                ~ifelse(type == 'INCOME' |
                          type == 'NET_INCOME', 
                        -. , .))) %>%
  select(type , acct1 , acct2 , amt_acct2 , amt_acct1 , subhead)

ftCurr <- 
  flextable(preTabCurr) %>%
  line_spacing(space = 0.5 ,
               i = ~ acct1 == acct2)  %>%
  hline(i = ~ is.na(acct2) & acct1 != 'Income' & acct1 != 'Expense'
        & is.na(subhead)) %>%
  #  hline(i = ~ str_detect(acct1 , 'Total')) %>%
  hline(i = ~ acct1 == 'Total Income' , 
        border = no_border) %>%
  hline(i = ~ acct1 == 'Total Expense'  ,
        border = fp_border(width = 2)) %>%
  # hline(i = ~ str_detect(acct1 , 'Ending') , border = big_border) %>%
  bold(i = ~ acct1 == 'Income' | 
         acct1 == 'Total Income' | 
         acct1 == 'Expense' | 
         acct1 == 'Total Expense' | 
         str_detect(acct1, 'Net')) %>%
  # italic(i = ~ str_detect(acct1 , 'Beginning') |
  #          str_detect(acct1 , 'Ending')) %>%
  fontsize(size = 12 , 
           i = ~ acct1 == 'Income' | 
             acct1 == 'Expense' |
             acct1 == 'Total Income' | 
             acct1 == 'Total Expense') %>%
  color(i = ~ acct1 == 'Income' | acct1 == 'Expense' , 
        color = 'blue') %>%
  color(
    i = ~ amt_acct1 < 0 | amt_acct2 < '0' , 
    j = 4:5 ,color = 'red') %>%
  padding(i = ~ acct1 == acct2 , 
          j = 2 , padding.left = 30) %>%
  colformat_double(j = 4:5 ,
                   prefix = '$ ' , 
                   digits = 0) %>%
  delete_columns(c('acct2' , 'type' , 'subhead')) %>%
  delete_part(part = 'header') %>%
  border(
    i = 1,
    j = 1:3 ,
    border.top = big_border,
    part = "body") %>%
  border(
    i = ~ acct1 == 'Income' | acct1 == 'Expense' , 
    j = 1:3 , border.top = blu_border) %>%
  width(j = 3 , width = "1") %>%
  width(j = 1 , width = "3.5") %>%
  set_caption(
    caption = as_paragraph(
      as_image(
        'pvhc.png' ,
        width = 1.1 ,
        height = 1.1),
      as_chunk(
        'Portland Vet House Calls Profit & Loss ' ,
        fp_text(font.size = 14)) , 
      as_image(
        'pvhc.png' ,
        width = 1.1 ,
        height = 1.1), 
      as_chunk(sprintf('for the period %s to %s' , 
                       str_remove(format(as.Date(curr_mo_start) , '%B %d, %Y') , '0') , 
                       format(as.Date(fy_to_date) , '%B %d, %Y')))))



save_as_html(ftCurr , path= 'test.html')

# save_as_html(ftCurr , path= sprintf('PVHC P & L for %s.html' , format(as.Date(fy_to_date) , format = '%b %Y')))

