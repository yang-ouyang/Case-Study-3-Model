library(splus2R) #1.2-2

# Main Data is given at 4Hz
data = read.csv("data.csv")
data1 = subset(data, id == 1)
nrow(data)

# ECG Data is given at 100Hz
data_ECG = read.csv("data_ECG.csv")
data1_ECG = subset(data_ECG, id == 1)

# BVP Data is given at 16Hz
data_BVP = read.csv("data_BVP.csv")
data1_BVP = subset(data_BVP, id == 1)

# Diagnostic Checks
# plot(data1_ECG$chest_ECG[c(1:1000)])
# sum(peaks(data1_ECG$chest_ECG[c(1:1000)], span = 59))
# plot(data1_BVP$wrist_BVP[c(1:200)])
# sum(peaks(data1_BVP$wrist_BVP[c(1:200)], span = 9))
# 
# #60 seconds
# sum(peaks(data1_ECG$chest_ECG[c(1:6000)], span = 59))
# sum(peaks(data1_ECG$chest_ECG[c(6001:12000)], span = 59))
# sum(peaks(data1_BVP$wrist_BVP[c(1:(16*60))], span = 9))
# sum(peaks(data1_BVP$wrist_BVP[c((16*60+1):(16*60*2))], span = 9))
# #Good agreement between ECG and BVP HR analysis

# Get subject specific BVP data

data1_BVP = subset(data_BVP, id == 1)
data2_BVP = subset(data_BVP, id == 2)
data3_BVP = subset(data_BVP, id == 3)
data4_BVP = subset(data_BVP, id == 4)
data5_BVP = subset(data_BVP, id == 5)
data6_BVP = subset(data_BVP, id == 6)
data7_BVP = subset(data_BVP, id == 7)
data8_BVP = subset(data_BVP, id == 8)
data9_BVP = subset(data_BVP, id == 9)
data10_BVP = subset(data_BVP, id == 10)
data11_BVP = subset(data_BVP, id == 11)
data12_BVP = subset(data_BVP, id == 12)
data13_BVP = subset(data_BVP, id == 13)
data14_BVP = subset(data_BVP, id == 14)
data15_BVP = subset(data_BVP, id == 15)

# Accumulate number of heart beats at a certain point, from the beginning of experiment

add_HB_BVP <- function(data){
  BVP_peaks = as.numeric(peaks(data$wrist_BVP, span = 9))
  for (i in 2:length(BVP_peaks)){
    BVP_peaks[i] = BVP_peaks[i]+BVP_peaks[i-1]
  }
  data$HB_BVP <- BVP_peaks
  return(data)
}

# Add Heart Beat data to all dataframes

data1_BVP = add_HB_BVP(data1_BVP)
data2_BVP = add_HB_BVP(data2_BVP)
data3_BVP = add_HB_BVP(data3_BVP)
data4_BVP = add_HB_BVP(data4_BVP)
data5_BVP = add_HB_BVP(data5_BVP)
data6_BVP = add_HB_BVP(data6_BVP)
data7_BVP = add_HB_BVP(data7_BVP)
data8_BVP = add_HB_BVP(data8_BVP)
data9_BVP = add_HB_BVP(data9_BVP)
data10_BVP = add_HB_BVP(data10_BVP)
data11_BVP = add_HB_BVP(data11_BVP)
data12_BVP = add_HB_BVP(data12_BVP)
data13_BVP = add_HB_BVP(data13_BVP)
data14_BVP = add_HB_BVP(data14_BVP)
data15_BVP = add_HB_BVP(data15_BVP)

combined_BVP = rbind(data1_BVP, data2_BVP, data3_BVP, data4_BVP, data5_BVP, 
                     data6_BVP, data7_BVP, data8_BVP, data9_BVP, data10_BVP,
                     data11_BVP, data12_BVP, data13_BVP, data14_BVP, data15_BVP)

# Downsample Heart Beat data from 16Hz to 4Hz

indices = seq(1,nrow(combined_BVP), 4)
downsampled_BVP = combined_BVP[indices,]

# Add Heart Beat data to main dataset

data$HB_BVP <- downsampled_BVP$HB_BVP

# Get subject specific ECG data

data1_ECG = subset(data_ECG, id == 1)
data2_ECG = subset(data_ECG, id == 2)
data3_ECG = subset(data_ECG, id == 3)
data4_ECG = subset(data_ECG, id == 4)
data5_ECG = subset(data_ECG, id == 5)
data6_ECG = subset(data_ECG, id == 6)
data7_ECG = subset(data_ECG, id == 7)
data8_ECG = subset(data_ECG, id == 8)
data9_ECG = subset(data_ECG, id == 9)
data10_ECG = subset(data_ECG, id == 10)
data11_ECG = subset(data_ECG, id == 11)
data12_ECG = subset(data_ECG, id == 12)
data13_ECG = subset(data_ECG, id == 13)
data14_ECG = subset(data_ECG, id == 14)
data15_ECG = subset(data_ECG, id == 15)

# Accumulate number of heart beats at a certain point, from the beginning of experiment

add_HB_ECG <- function(data){
  ECG_peaks = as.numeric(peaks(data$chest_ECG, span = 59))
  for (i in 2:length(ECG_peaks)){
    ECG_peaks[i] = ECG_peaks[i]+ECG_peaks[i-1]
  }
  data$HB_ECG <- ECG_peaks
  return(data)
}

# Add Heart Beat data to all dataframes

data1_ECG = add_HB_ECG(data1_ECG)
data2_ECG = add_HB_ECG(data2_ECG)
data3_ECG = add_HB_ECG(data3_ECG)
data4_ECG = add_HB_ECG(data4_ECG)
data5_ECG = add_HB_ECG(data5_ECG)
data6_ECG = add_HB_ECG(data6_ECG)
data7_ECG = add_HB_ECG(data7_ECG)
data8_ECG = add_HB_ECG(data8_ECG)
data9_ECG = add_HB_ECG(data9_ECG)
data10_ECG = add_HB_ECG(data10_ECG)
data11_ECG = add_HB_ECG(data11_ECG)
data12_ECG = add_HB_ECG(data12_ECG)
data13_ECG = add_HB_ECG(data13_ECG)
data14_ECG = add_HB_ECG(data14_ECG)
data15_ECG = add_HB_ECG(data15_ECG)

combined_ECG = rbind(data1_ECG, data2_ECG, data3_ECG, data4_ECG, data5_ECG, 
                     data6_ECG, data7_ECG, data8_ECG, data9_ECG, data10_ECG,
                     data11_ECG, data12_ECG, data13_ECG, data14_ECG, data15_ECG)

# Downsample ECG HB data from 100Hz to 4Hz

indices = seq(1,nrow(combined_ECG), 25)
downsampled_ECG = combined_ECG[indices,]

# Add Heart Beat data to main dataset

data$HB_ECG <- downsampled_ECG$HB_ECG

# Convert ACC to magnitude

data$chest_ACC = sqrt((data$chest_ACC_x^2 + data$chest_ACC_y^2 + data$chest_ACC_z^2))
data$wrist_ACC = sqrt((data$wrist_ACC_x^2 + data$wrist_ACC_y^2 + data$wrist_ACC_z^2))

# Remove ACC components and save data

data = data[,-c(3,4,5,11,12,13)]
save(data, file = "data.rda")












