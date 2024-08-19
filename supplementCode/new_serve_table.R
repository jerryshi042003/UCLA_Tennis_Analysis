#By Rushil Shah
install.packages("scales")
library(scales)
library(gt)


#Load CSV Data
data <- read.csv("/Users/rushilshah/Documents/BSA Visualization/exampleData.csv")

#Create new Data Frame for Sorted Data
df_CLEANED = data.frame()


#Delete all the rows where UCLA is Serving
for (i in 1:nrow(data)){
  row <- data[i,]
  if (row["serverName"] == "Tyler Zink") #replace 'Tyler Zink' with the opponent of the UCLA Player. Make sure it matches name in CSV file.
  {
    df_CLEANED <- rbind(df_CLEANED, row)
  }
}

#Create new Data Frame with one column for number of attempted returns second for number of successful returns
column_headers <- c("tServesAd","tServesDeuce", "bodyServesAd", "bodyServesDeuce", "wideServesAd", "wideServesDeuce", "totals")
row_names <- c("pointWon","returnRate","successfulReturns", "attemptedReturns")

df_SERVES <- data.frame(matrix(0, nrow = 4, ncol = length(column_headers)))
colnames(df_SERVES) <- column_headers
row.names(df_SERVES) <- row_names

new_row <- c(0,0,0,0,0,0,0)
df_SERVES <- rbind(df_SERVES, new_row)

#function to check point Won #check logic
pointWon <- function(row_index, dataFrame){
  
  while(TRUE)
  {
    row <- dataFrame[row_index,]
    if( !is.na(row["isPointEnd"]) )
    {
      break;
    }
    row_index = row_index + 1
  }
  
  row <- dataFrame[row_index,]
  if (row["shotInRally"] %% 2 == 1)
  {
    if (is.na(row["isWinner"]))
    {
      return(1)
    }
    
    else
    {
      return(0)
    }
  }
  
  else if (row["shotInRally"] %% 2 == 0)
  {
    if (!is.na(row["isWinner"]))
    {
      return(1)
    }
    
    else
    {
      return(0)
    }
  }
}


#Get the Attempted Returns & Successful Returns on First Serve
for (i in 1:nrow(df_CLEANED)){
  
  row <- df_CLEANED[i,] #this row gets data for opponents serve type
  next_row = df_CLEANED[i+1,] #this row gets data for UCLA player's return
  
  if (!is.na(row["isPointStart"]) && is.na(row["isPointEnd"])){
    
    serveZone <- df_CLEANED[i,"firstServeZone"]
    side <- df_CLEANED[i, "side"]
    
    #classify the zones:
    if(serveZone == "T"){
      serveZone <- "tServes"
    }
    else if(serveZone == "Body"){
      serveZone <- "bodyServes"
    }
    else if(serveZone == "Wide"){
      serveZone <- "wideServes"
    }
    else{
      #skip data collection if faulty data
      next
    }
    
    category = paste(serveZone, side, sep = "")
    
    if(is.na(next_row["isErrorNet"]) && is.na(next_row["isErrorLong"]) && is.na(next_row["isErrorLong"]) && is.na(next_row["isErrorWideR"]) && is.na(next_row["isErrorWideL"]))
    {
      df_SERVES["successfulReturns",category] <- df_SERVES["successfulReturns",category] + 1
      df_SERVES["pointWon",category] <- df_SERVES["pointWon",category] + pointWon(i,df_CLEANED)
      
    }
    
    df_SERVES["attemptedReturns",category] <- df_SERVES["attemptedReturns",category] + 1
  }
}

#Get the Sums and Set to Total
successful_sum <- rowSums(df_SERVES["successfulReturns",])
attempt_sum <- rowSums(df_SERVES["attemptedReturns",])
point_sums <- rowSums(df_SERVES["pointWon",])
df_SERVES["successfulReturns","totals"] <- successful_sum
df_SERVES["attemptedReturns","totals"] <- attempt_sum
df_SERVES["pointWon","totals"] <- point_sums

#set the values in Return Rate Row
for (i in 1:ncol(df_SERVES))
{
  val1 = df_SERVES["successfulReturns",i]
  val2 = df_SERVES["attemptedReturns",i]
  val3 = df_SERVES["pointWon",i]
  return_percent = (val1/val2)
  point_percent = (val3/val1)
  df_SERVES["returnRate",i] = sprintf("%.1f%%", return_percent * 100)
  df_SERVES["pointWon",i] = sprintf("%.1f%%", point_percent * 100)
}

#Delete last three rows
df_SERVES <- head(df_SERVES,-3)

#Change row names
rownames(df_SERVES) <- c("Point Won Rate", "Serve Return Rate")

#Create a Table for the Results
gt_tbl <- gt(df_SERVES, rownames_to_stub = TRUE)
gt_tbl <- tab_header(gt_tbl, title=md("**Returns on Serves**"), subtitle = NULL, preheader = NULL)
gt_tbl <- tab_source_note(gt_tbl, source_note="*Point Won Rate calculated as number of points won divided by number of succesful returns") #change caption depending on match
gt_tbl <- cols_label(gt_tbl, tServesAd = 'Ad', tServesDeuce = 'Deuce', bodyServesAd = 'Ad', bodyServesDeuce = 'Deuce', wideServesAd = 'Ad', wideServesDeuce = 'Deuce', totals = 'Totals')
gt_tbl <- tab_options(gt_tbl, data_row.padding = px(15), column_labels.padding = px(7), column_labels.padding.horizontal = px(15), container.padding.y = px(15), heading.padding = px(7))
gt_tbl <- cols_align(gt_tbl,align=c("center"),columns = everything())

#Create Spanners
gt_tbl <- tab_spanner(gt_tbl,label = md('**T-Serves**'),columns = 2:3)
gt_tbl <- tab_spanner(gt_tbl,label = md('**Body Serves**'),columns = 4:5)
gt_tbl <- tab_spanner(gt_tbl,label = md('**Wide Serves**'),columns = 6:7)

# Show the gt Table
gt_tbl