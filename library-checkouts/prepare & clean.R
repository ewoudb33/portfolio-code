library(dplyr)
library(data.table)
library(fastDummies)

# read in datatables
library_checkouts <- fread('Checkouts_By_Title_Data_Lens_2017.csv')
dictionary <- fread('Integrated_Library_System__ILS__Data_Dictionary.csv')

# create final dataset
final_df <- library_checkouts[, c('BibNumber', 'ItemType', 'Collection', 
                                  'CheckoutDateTime')]
# make data table
final_df <- as.data.table(final_df)

# add dependent variable
final_df <- final_df[, number_of_checkouts := .N, by = .(BibNumber)]
  
# remove duplicated values
final_df <- final_df[!duplicated(final_df[,BibNumber]),]

# prepare inventory dataset to merge
sub_inventory <- inventory[, c('BibNum', 'Author', 'PublicationYear', 'Publisher')]

# make data table
sub_inventory <- as.data.table(sub_inventory)

# change variable to able to merge
setnames(sub_inventory, "BibNum", "BibNumber")

# Merge inventory and final dataset
final_merged <- merge(final_df, sub_inventory, by="BibNumber", allow.cartesian=TRUE)

# remove duplicated values
final_merged <- final_merged[!duplicated(final_merged[,BibNumber]),]

# creating factor variables for the item type and collection type
final_merged$ItemType <- factor(final_merged$ItemType)
final_merged$Collection <- factor(final_merged$Collection)

class(final_merged$CheckoutDateTime)

library(lubridate)

# Change time variable to numerical month
final_merged$CheckoutDateTime <- mdy_hms(final_merged$CheckoutDateTime)

final_merged$CheckoutDateTime <- format(final_merged$CheckoutDateTime, "%m")

final_merged$CheckoutDateTime <- as.numeric(final_merged$CheckoutDateTime)

# create factor variable for author
final_merged$Author <- factor(final_merged$Author)

final_merged$PublicationYear <- gsub(pattern = "[", replacement = "", x = final_merged$PublicationYear , 
             fixed = TRUE)

final_merged$PublicationYear <- gsub(pattern = "]", replacement = "", x = final_merged$PublicationYear , 
     fixed = TRUE)

final_merged$PublicationYear <- gsub(pattern = "c", replacement = "", x = final_merged$PublicationYear , 
                     fixed = TRUE)

final_merged$PublicationYear <- gsub(pattern = ",", replacement = "", x = final_merged$PublicationYear , 
                     fixed = TRUE)

final_merged$PublicationYear <- gsub(pattern = ".", replacement = "", x = final_merged$PublicationYear , 
                     fixed = TRUE)

final_merged$PublicationYear <- substr(final_merged$PublicationYear, start = 1, stop = 4)

final_merged$PublicationYear <- as.numeric(final_merged$PublicationYear)

final_merged$Publisher <- gsub(pattern = "[", replacement = "", x = final_merged$Publisher , 
                                     fixed = TRUE)

final_merged$Publisher <- gsub(pattern = "]", replacement = "", x = final_merged$Publisher , 
                               fixed = TRUE)

final_merged$Publisher <- gsub(pattern = ",", replacement = "", x = final_merged$Publisher , 
                               fixed = TRUE)
 
final_clean <- final_merged[!(is.na(final_merged$Author) | final_merged$Author==""), ]

final_clean <- final_clean[!(is.na(final_clean$Publisher) | final_clean$Publisher==""), ]

final_clean <- final_clean[!(is.na(final_clean$PublicationYear) | final_clean$PublicationYear==""), ]

final_clean$PublicationYear <- as.numeric(final_clean$PublicationYear)

final_clean$Publisher <- as.factor(final_clean$Publisher)

# Create dummy variables for month with October as a reference

final_clean <- dummy_cols(final_clean, select_columns = 'CheckoutDateTime')
final_clean <- final_clean[, !("CheckoutDateTime_10")]

final_clean <- dummy_cols(final_clean, select_columns = 'ItemType')
final_clean <- final_clean[, !("ItemType_ucunknj")]

