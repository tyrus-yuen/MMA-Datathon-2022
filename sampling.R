library(xlsx)

data_transactions<-read.csv("/Users/tyrusyuen/Library/CloudStorage/OneDrive-TheChineseUniversityofHongKong/2022 H2/UoT Datathon/UoT Datathon 2022 Shared Drive/transactions.csv")
data_demographics<-read.csv("/Users/tyrusyuen/Library/CloudStorage/OneDrive-TheChineseUniversityofHongKong/2022 H2/UoT Datathon/UoT Datathon 2022 Shared Drive/demographics.csv")

merge<-merge(x = data_transactions, y = data_demographics, by = "household_key", all.x = TRUE)
merge1<-subset(merge,BRAND == 'National')
dim(data_transactions)
dim(merge)

merge_noNA<-merge1[rowSums(is.na(merge1['INCOME_DESC'])) == 0, ]

wb = createWorkbook()
for (i in 1:20) {

set.seed(sample(1:1e9, 1))
index <- sample(1:nrow(merge_noNA), 35e3)
index

sample<-merge_noNA[index, ]

writeDataTable(wb, sheet=addWorksheet(wb, paste("Sample",i)),sample, startCol=1)

}

saveWorkbook(wb, "/Users/tyrusyuen/Library/CloudStorage/OneDrive-TheChineseUniversityofHongKong/2022 H2/UoT Datathon/filename.xlsx")
