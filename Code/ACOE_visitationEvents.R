
# total time that an animal was detected on a given day --------
  ### does not exclude time in between detections where an animal was clearly not detected

test <- mergedDetectionsTags %>%
  filter(common_name == "Atlantic sturgeon") %>%
  group_by(tag_id, dateEST) %>%
  summarise(count = n(), 
            minTime = min(EST), 
            maxTime = max(EST)) %>% 
  mutate(timeDiffS = maxTime - minTime,
         timeDiffM = timeDiffS/60, 
         timeDiffH = timeDiffM/60) %>% 
  mutate(timeDiffS = as.numeric(timeDiffS), 
         timeDiffM = as.numeric(timeDiffM), 
         timeDiffH = as.numeric(timeDiffH)) %>% 
  as.data.frame() %>% 
  ggplot() + 
  geom_point(aes(x = timeDiffH, y = count))
test

summary(test)
sd(test$timeDiffH)


# creating own residence extraction ---------------------------------------

  ### for loop that runs through each individual on each day detected
    ### organizes all detections in order regardless of receiver within the array
    ### creates a new activity df that includes columns: individual id, event number, startTime, endTime, totalTime
    ### create a column for event number in detections df (this will go in order of 1 to whatever the last number is)
    ### looks at time difference between subsequent detections
    ### if time difference is less then 15 minutes, then jump down to next detection and check if difference is greater than 15 minutes
    ### when time difference is greater than 15 minutes, then populate event information into the activity dataframe
    ### move to next row to look for next event
    ### once this has cycled through all events on a given date, more to the next date and continue
    ### once this has cycled through all events for a given individual, move to the next individual until complete

#message to chatgpt: Hello, I want to create a for loop in R to do the following. The data will be animal movement data whereby datetime is formatted as POSIXct. The for loop will run through each individual, then organize datetime in ascending order. The event numbers will start with 1 and then increase consecutively. For each individual, it will look at the time difference between subsequent detections (e.g., time difference between detection 1 and detection 2), if that time difference is less than 15 minutes, then it will look at the next detection (e.g., time difference between detection 2 and detection 3) and continue on. If the time difference is greater than 15 minutes, it will create a new event number in the event column and continue moving down through the detections. Once all of the detections have been assessed and the events established, it will move onto the next individual tallying the time differences. The ideal output would be an activity dataframe that includes animal ID, event number, start time, end time, and time difference for the event. Thanks!

write.csv(test2, paste0(owd, "/", "testData.csv"))#fo steve
  
test2 <- mergedDetectionsTags %>% 
  #arrange(EST) %>% 
  #group_by(tag_id) %>% 
  filter(tag_id == "A69-1602-49371" | tag_id == "A69-1602-49382") %>% 
  mutate(timeDiff = EST - lag(EST, 1), #time difference calculated in seconds, NAs present
         timeDiff = as.numeric(timeDiff)) %>% #remove character string and convert to numeric
  as.data.frame() %>% 
  #replace(is.na(.), 0) %>% #this step might not be necessary
  mutate(timeOutFilter = case_when(timeDiff <=  900 ~ "Continue", 
                                   timeDiff > 900 ~ "Break",
                                   TRUE ~ "Break")) %>% 
  summary()
  group_by(tag_id) %>% 
  mutate(fullTimeDiff = EST[timeOutFilter, timeOutFilter == "Break"] - lag(EST[timeOutFilter, timeOutFilter == "Break"], 1))


  

  
  
  
  
  
  
  