# pvhcPandL.R

# creates P&L with no Excel!

# packages needed
library(tidyverse)
library(janitor)
library(flextable)
library(here)
library(officer)
# end packages needed

# user-defined functions
ed_trans <- function(data) {
  filter(data , date >= fy_start & 
           date <= fy_to_date) %>%
    filter(type != 'BANK') %>%
    filter(acct1 == 'Income' | 
             acct1 == 'Expenses') %>%
    select(-acct1) %>%
    rename(acct1 = acct2) %>%
    rename(acct2 = acct3)
}


makTab <- function(x , df) {
  bind_rows(
    df %>%
      ed_trans() %>%
      group_by(acct2) %>%
      summarise(amt_acct2 = sum(amount_num , na.rm = TRUE) ,
                type = first(type) , 
                acct1 = first(acct1)) %>%
      na.exclude(acct2) , 
    df %>% 
      ed_trans() %>%
      group_by(acct1) %>%
      summarise(amt_acct1 = sum(amount_num, na.rm = TRUE) , 
                type = first(type)) , 
    df %>%
      ed_trans() %>%
      group_by(type) %>%
      summarise(amt_acct1 = sum(amount_num))) %>% 
    filter(type == str_to_upper(x)) %>%
    bind_rows(. , 
              group_by(. , acct1) %>%
                summarise(count = n()) %>%
                filter(count > 1) %>%
                mutate(subhead = 'y')) %>%
    arrange(acct1 , subhead , acct2) %>%
    mutate(acct1 = 
             ifelse(
               is.na(acct1) , 
               str_to_title(type) , 
               acct1)) %>%
    mutate(acct1 = 
             ifelse(
               is.na(acct2) & !is.na(lag(acct2)) & is.na(subhead) | 
                 (acct1 == 'Income' | acct1 == 'Expense'), 
               sprintf('Total %s' , str_to_title(acct1)) , 
               acct1)) %>%
    mutate(acct1 = 
             ifelse(
               subhead == 'y' & !is.na(subhead) , 
               str_to_title(acct1) , acct1)) %>%
    mutate(acct1 = 
             ifelse(
               !is.na(acct2) , NA , acct1)) %>%
    select(type , acct1 , acct2 , amt_acct1 , amt_acct2 , subhead) %>%
    add_row(type = str_to_upper(x) ,  
                                acct1 = str_to_title(x) , 
                                .before = 1) %>%
              ungroup()
}

# end user-defined functions

# read in source data
pvhcAccountTree <- read_csv("C:/Users/dsole/OneDrive/Personal Vault/data/pvhc/pvhcAccountTree.csv") %>%
  clean_names()

pvhcAllTrans <- read_csv("C:/Users/dsole/OneDrive/Personal Vault/data/pvhc/pvhcAllTrans.csv" , 
                         col_types = cols(Date = col_date(format = "%m/%d/%Y"))) %>%
  clean_names() %>%
  left_join(. ,
            pvhcAccountTree %>% select(type, full_account_name) ,
            by = 'full_account_name') %>%
  fill(date) %>%
  mutate(amount_num = str_remove_all(amount_num , '\\)')) %>%
  mutate(amount_num = str_replace_all(amount_num , '\\(' , '-')) %>%
  mutate(amount_num = str_remove_all(amount_num , ',')) %>%
  mutate(amount_num = as.numeric(amount_num)) %>%
  separate(full_account_name , into = c('acct1' , 'acct2' , 'acct3') , 
           sep = ':' , remove = FALSE)


# create pre-table
inc <- makTab(x = quote('Income') , df = pvhcAllTrans)
exp <- makTab(x = quote('Expense') , df = pvhcAllTrans)

preTab <- 
  bind_rows(
    inc %>%
      add_row(), 
    exp , 
    pvhcAllTrans %>%
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

# create flextable
big_border <- fp_border(color = "blue", width = 3)
blu_border <- fp_border(color = "blue", style = 'dotted' , width = 2)
no_border <- fp_border(color = 'white' , width = 0)

ft <- 
  flextable(preTab) %>%
  line_spacing(space = 0.2 ,
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
                       str_remove(format(as.Date(fy_start) , '%B %d, %Y') , '0') , 
                       format(as.Date(fy_to_date) , '%B %d, %Y'))))) %>%
  line_spacing(. , space = 0.9 , part = 'body')


ft
#

save_as_html(ft , path= sprintf('PVHC Profit & Loss for %s.html' , reptDte))
# 
# # convert html outut from render to pdf
# # see https://search.r-project.org/CRAN/refmans/psycModel/html/html_to_pdf.html
# psycModel::html_to_pdf(file_path = 
#                          sprintf('C:\\junk\\github\\pvhc\\pvhc2023\\PVHC Profit & Loss for %s.html' ,
#                                  reptDte) , 
#                       scale = 0.90)

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


ftCurr  

# save_as_html(ftCurr , path= sprintf('PVHC P & L for %s.html' , format(as.Date(fy_to_date) , format = '%b %Y')))


