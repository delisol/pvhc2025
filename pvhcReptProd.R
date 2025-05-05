# pvhcReptProd.R
# renders rmd file and converts html output to pdf
# stores in appropriate Drive directory

# pass report date values
filterDte <- '2025-04-30' #last day of report period
fy_start <- '2025-01-01'
curr_mo_start <- '2025-04-01'
fy_to_date <- filterDte
reptDte <- format(as.Date(fy_to_date), '%B %d, %Y')
currM <- 'Apr' # month of report
nextM <- 'May' # change to next month after report date

rmarkdown::render(
  'PvhcMonthlyReport.Rmd',
  output_format = 'html_document',
  output_dir = 'C:\\Users\\dsole\\OneDrive\\Documents\\FinancialPlanning\\PVHC financials\\2025',
  output_file = sprintf('PVHC Monthly Report for %s', reptDte)
)

# convert html outut from render to pdf
# see https://search.r-project.org/CRAN/refmans/psycModel/html/html_to_pdf.html
psycModel::html_to_pdf(
  file_path = sprintf(
    'C:\\Users\\dsole\\OneDrive\\Documents\\FinancialPlanning\\PVHC financials\\2025\\PVHC Monthly Report for %s.html',
    reptDte
  ),
  scale = .88
)

# zip::zip(sprintf('C:\\Users\\dsole\\OneDrive\\Documents\\FinancialPlanning\\PVHC financials\\2023\\%s.zip' , reptDte) ,
#          files = c(
#            sprintf('C:\\Users\\dsole\\OneDrive\\Documents\\FinancialPlanning\\PVHC financials\\2024\\PVHC Monthly Report for %s.html' , reptDte)) ,
#          mode = 'cherry-pick' ,
#          include_directories = FALSE)

# zip::zip(sprintf('C:\\Users\\dsole\\OneDrive\\Documents\\FinancialPlanning\\PVHC financials\\2023\\%s.zip' , reptDte) ,
#             files = c(
#               sprintf('C:\\Users\\dsole\\OneDrive\\Documents\\FinancialPlanning\\PVHC financials\\2023\\PVHC Monthly Report for %s.html' , reptDte) ,
#               sprintf('C:\\Users\\dsole\\OneDrive\\Documents\\FinancialPlanning\\PVHC financials\\2023\\PVHC Monthly Report for %s.pdf' , reptDte)) ,
#          mode = 'cherry-pick' ,
#          include_directories = FALSE)
