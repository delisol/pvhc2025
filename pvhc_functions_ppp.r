# pvhc_functions_ppp.r
# pvhc functions for yes/no on PPP dollars

mak_cumTtl <- function(data , exclude) {  
  filter(pvhcAllTrans , (acct1 == 'Income' | acct1 == 'Expense') &
           (date >= '2019-01-01' & date <= filterDte)) %>%
    filtA2(x = exclude)
}

mak_subtitel <- function(exclude) {
  ifelse(
    exclude == 'PPP' ,  'PPP dollars EXCLUDED (2020, 2021)' ,
    ifelse(
      exclude == '' , 'PPP dollars INCLUDED (2020, 2021)' ,
      NA))
}
