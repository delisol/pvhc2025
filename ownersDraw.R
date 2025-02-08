# pvhcPandL.R

# creates P&L with no Excel!

# packages needed
library(tidyverse)
library(janitor)
library(flextable)
library(here)
library(officer)
# end packages needed

# filterDte <- '2024-01-31'
# fy_start <- '2024-01-01'
# curr_mo_start <- '2024-01-01'
# fy_to_date <- filterDte
# reptDte <- format(as.Date(fy_to_date) , '%B %d, %Y')
# currM <- 'Jan' # month of report
# nextM <- 'Feb' # change to next month after report date

# user-defined functions
ed_trans <- function(data) {
   filter(data , date >= fy_start & 
           date <= fy_to_date) %>%
#    filter(type != 'BANK') %>%
    filter(str_detect(acct1 , 'draw')) %>%
    mutate(type = "Owner's draw") %>%
    select(-acct1) %>%
    rename(acct1 = acct2) %>%
    rename(acct2 = acct3)
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

# for convenience
df <- pvhcAllTrans

# no function for this
od <-   bind_rows(
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
      summarise(amt_acct1 = sum(amount_num)))  %>% 
    # filter(type == str_to_upper(x))  %>%
    bind_rows(. , 
              group_by(. , acct1) %>%
                summarise(count = n()) %>%
                filter(count > 1) %>%
                mutate(subhead = 'y')) %>%
    arrange(acct1 , subhead , acct2) %>%
    mutate(acct1 = 
             ifelse(
               is.na(acct1) , 
               type , 
               acct1)) %>%
  mutate(acct1 =
             ifelse(
               !is.na(acct2) , acct2 , acct1)) %>%
  mutate(acct1 = 
           ifelse(acct1 == "Owner's draw" , 
                  "Total Owner's draw" , acct1)) %>%
  select(type , acct1 , acct2 , amt_acct1 , amt_acct2 , subhead) %>%
              ungroup()

# no pre-table

# create flextable
big_border <- fp_border(color = "blue", width = 3)
blu_border <- fp_border(color = "blue", style = 'dotted' , width = 2)
no_border <- fp_border(color = 'white' , width = 0)

ft_od <-
  flextable(od) %>%
  line_spacing(space = 0.5 ,
               i = ~ acct1 == acct2) %>%
  fontsize(size = 12 , 
           i = ~ acct1 == "Total Owner's draw") %>%
  color(i = ~ acct1 == "Total Owner's draw" , 
        color = 'blue') %>%
  color(
    i = ~ amt_acct1 < 0 | amt_acct2 < '0' , 
    j = 4:5 ,color = 'red') %>%
      width(j = 2 , width = "2") %>%
      width(j = 3 , width = "3") %>%
    padding(i = ~ acct1 == acct2 , 
            j = 2 , padding.left = 30) %>%
    colformat_double(j = 4:5 ,
                   prefix = '$ ' , 
                   digits = 0) %>%
  delete_columns(c('type' , 'subhead' , 'acct2')) %>%
  delete_part(part = 'header') %>%
  border(
    i = 1,
    j = 1:3 ,
    border.top = big_border,
    part = "body") %>%
  border(
    i = ~ acct1 == "Owner's draw" , 
    j = 1:3 , border.top = blu_border) %>%
    width(j = 1 , width = 3.5) %>%
    width(j = 2 , width = 1) %>%
    set_caption(
    caption = as_paragraph(
      as_image(
        'pvhc.png' ,
        width = 1 ,
        height = 1),
      as_chunk(
        "Portland Vet House Calls Owner's Draw " ,
        fp_text(font.size = 14)) , 
      as_image(
        'pvhc.png' ,
        width = 1 ,
        height = 1), 
      as_chunk('<br>') , 
      as_chunk(sprintf('for the period %s to %s' , 
                       str_remove(format(as.Date(fy_start) , '%B %d, %Y') , '0') , 
                       format(as.Date(fy_to_date) , '%B %d, %Y')))))

ft_od  

