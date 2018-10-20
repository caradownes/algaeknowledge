#Author: Zachary Lowery
#Date: 10/15/2018
#Script Name: GUI.R
#Description: This script is to be a collection of functions, variables, constants, which relate
#             to and define the graphical user interface (GUI) of the app.

#Constants
defaultHeaderName <- "EASyR"

#Function Definitions

GUI.CreateDashboardHeader <- function(title = defaultHeaderName){
   dashboardHeader(title = defaultHeaderName)
}
