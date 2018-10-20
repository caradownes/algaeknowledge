#Author: Zachary Lowery
#Date: 10/15/2018
#Script Name: dropBox.R
#Description: This script is intended to define all of the functions meant to operate on DropBox, whether that
#             be file operations or initialization.

#DEBUG CONSTANTS
dropBoxAuthenticationError <- TRUE


#Constants
dropBoxDirectory <- "algae"
dropBoxTokenName <- "droptoken.rds"
dropBoxOutputDir <- dropBoxDirectory
dropBoxExportOutputDir <- "algaeoutput"
dropBoxDashboardOutputDir <- "algae"
dropBoxFilenames <- 0
dropBoxExcelFiles <- 0
dropBoxDashFilenames <- 0



#Function Definitions

dropBox.InitToken <- function(){ 
    
   if(dropBoxAuthenticationError){
     token <- drop_auth(rdstoken = dropBoxTokenName)
   }
   else{
      token <- drop_auth()
   }
   
   saveRDS(token, dropBoxTokenName)
  
     #Comment Author: Zachary Lowery
     #Date: 10/15/2018
     #If this is never to be run EVER AGAIN, why is this documentation here?
     #Please provide more details.
  
     #--------------------------------------
     #This is run once to generate the oAuth
     #      --DO NOT RUN--
     #token <- drop_auth(new_user = TRUE,
     #           key = "mmhfsybffdom42w",
     #           secret = "l8zeqqqgm1ne5z0",
     #           cache = TRUE, rdstoken = NA)
     #saveRDS(token, "droptoken.rds")
     #--------------------------------------
}

dropBox.InitFileIO <- function(){
  #Comment Author: Zachary Lowery
  #Date: I suspect this segment of code is responsible for a cryptic error I am
  #      getting when I try to build this code. I am getting a 409 error, most likely
  #      because my local machine does not have this directory structure in place or 
  #      because my DropBox does not currently have this directory structure.
  #      I am using an if switch to cancel this for the time being.
  
  #-----------------------Set up the directory structure for the app--------
  
  #This is set up for DropBox as the storage for the data
  #setwd(dropBoxDirectory)
  dropBoxFilenames  <- drop_dir(path = dropBoxDirectory) %>%
    select("path_lower")
  dropBoxExcelFiles <- drop_dir(path = dropBoxDirectory) %>%
    select("name")
  dropBoxDashFilenames <- drop_dir(path = "algaedash") %>%
    select("path_lower")
}

