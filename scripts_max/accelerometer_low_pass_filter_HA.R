# Author: Hoda Allahbakhshi
# Based on Bayat, Akram, Marc Pomplun, and Duc A. Tran. "A study on human activity recognition using accelerometer data from smartphones." Procedia Computer Science 34 (2014): 450-457.

# Seperate linear acceleration from gravity component using a 3D accelerometer
Gravity_motion<- function(raw_data)
{
  fs = 50.0 ### 3D accelerometer sampling rate, Hz
  
  nyq = 0.5 * fs
  
  cutoff = 0.35 ###paper suggests 0.1Hz - 0.5Hz to cutoff gravity
  
  normal_cutoff = cutoff / nyq
  
  order = 1 ### paper use order of 1. 
  
  timeStep = 1.0/fs
  
  library(signal)
  
  bf<- butter(order, normal_cutoff,type=c("low"), plane=c("z"))
  
  gravityX=filtfilt(bf, raw_data$Acc_X)
  
  gravityY=filtfilt(bf, raw_data$Acc_Y)
  
  gravityZ=filtfilt(bf, raw_data$Acc_Z)
  
  Totalgravity=filtfilt(bf, raw_data$SVM)
  
  MotionX=raw_data$Acc_X-gravityX
  
  MotionY=raw_data$Acc_Y-gravityY
  
  MotionZ=raw_data$Acc_Z-gravityZ
  
  TotalMotion=raw_data$SVM-Totalgravity
  
  Result<- list(gravityX,gravityY,gravityZ,Totalgravity,MotionX,MotionY,MotionZ,TotalMotion)
  
  names(Result)<-c("gravityX","gravityY","gravityZ","Totalgravity","MotionX","MotionY","MotionZ","TotalMotion")
  
  return(Result)}