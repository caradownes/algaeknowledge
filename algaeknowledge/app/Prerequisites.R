#Author: Zachary Lowery
#Date: 10/15/2018
#Script Name: Prerequisites
#Description: This is an R script intended to check for the libraries most necessary to run the app.
#             If they are not found, they are installed to the host machine, and then loaded so the 
#             app can be run.

#Thanks to Shane on Stack Overflow for this code snippet:
   #https://stackoverflow.com/questions/4090169/elegant-way-to-check-for-missing-packages-and-install-them

#Create a list of packages we need.
packageList <- c("shiny", "shinydashboard", "shinyjs", "V8", "dplyr", "tidyverse", "datasets", "dygraphs",
                 "ggplot2", "DT", "corrplot", "psych", "plot3D", "scales", "stargazer", "xts", "data.table",
                 "RColorBrewer", "Hmisc", "readxl", "tidyxl", "unpivotr", "rdrop2", "httpuv")

#Determine what packages need to be installed.
uninstalledPackages <- packageList[!(packageList %in% installed.packages()[,"Package"])]

#Install the necessary packages.
if(length(uninstalledPackages)) install.packages(uninstalledPackages)

#This code snippet is adapted from daroczig's answer on Stack Overflow:
   #https://stackoverflow.com/questions/8175912/load-multiple-packages-at-once
lapply(packageList, library, character.only = TRUE)
