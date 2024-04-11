all_to_underscore <- function(string)
{
    string <- gsub(pattern = "\\^|\\°|\\!|\"|\\§|\\%|\\&|\\$|\\(|\\)|\\=|\\?|\\´|\\`|\\+|\\*|\\#|\\'|\\,|\\;|\\.|\\:|\\-|<|>|\\²|\\³|\\{|\\[|\\]|\\]|\\}|\\|\\\\|\\~|\\@|\\€| |\\|", "_", string)
    
    return(string)
}



prepare_everything_1 <- function(main_path, group_names, paths_to_groups)
{
    filenames <- NULL
    files <- NULL
    files_save <- NULL
    
    mouse_number <- NULL
    group_name_save <- NULL
    sub_group_name_save <- NULL
    trial_number_save <- NULL
    
    #print(gsub(pattern = "/", "\\", list.dirs(path = main_path), fixed = TRUE))
    
    subfolder_names <- list()
    maybe_subfolder_names <- list()
    
    variable_name <- NULL
    variable_name_save <<- NULL
    
    files_counter_lower <- 1L
    files_counter_upper <- 0L
    
    count_up <- 1L
    
    all_data_frames <<- NULL
    
    for(i in 1:length(paths_to_groups))
    {
        #gsub(pattern = "//", "/", paths_to_groups[i], fixed = TRUE) -> changes path to contain all \\ so that there is no mix ups with / or \
        #, recursive = TRUE, pattern = "corrected_.*?csv") returns only files which start with corrected_ and end on csv
        #gsub(pattern = ".*?/", "", -> removes the folder path list.files() returns before the filename
        #gsub(pattern = "^corrected_|\\.csv$", "", -> removes the corrected_ at the beginning and the .csv at the end so that only the filename is left
        
        if(identical(list.files(path = gsub(pattern = "//", "/", paths_to_groups[i], fixed = TRUE), recursive = TRUE, pattern = "corrected_.*?csv"), character(0)))
        {
            #If no fitting file exists, character(0) will be returned, so these folders can be skipped, although since the user manually chose the folder, a notification will be printed.
            print(paste("No corrected .csv files could be found in the path:", paths_to_groups[i]))
            next #i = i + 1
        }
        
        maybe_subfolder_names[i] <- list(gsub(pattern = "/", "\\", list.files(path = gsub(pattern = "/", "\\", paths_to_groups[i], fixed = TRUE)), fixed = TRUE))
        
        subfolder_names[i] <- list(gsub(pattern = "/", "\\", list.dirs(path = gsub(pattern = "/", "\\", paths_to_groups[i], fixed = TRUE), full.names = FALSE, recursive = FALSE), fixed = TRUE))
        
        progress_counter <- 0L
        
        #print(group_names[i])
        #print(subfolder_names[i])
        
        print(paste0(progress_counter,"/",length(subfolder_names[[i]]), " subfolders of group ", group_names[i], " are currently processed."))
        
        if(identical(subfolder_names[[i]], character(0)))
        {
            #no sub_groups present, group_name is data frame name and entire group data frame
            
            #print("Case 1")
            #actual data frames in a list
            list_files <- list.files(path = gsub(pattern = "//", "/", paths_to_groups[i], fixed = TRUE), recursive = TRUE, pattern = "corrected_.*?csv")
            
            files_counter_upper <- files_counter_upper + length(list_files)
            
            files[files_counter_lower:files_counter_upper] <- lapply(paste0(paths_to_groups[i], "\\" , gsub(pattern = "//", "/", list_files, fixed = TRUE)), read.csv)
            filenames[files_counter_lower:files_counter_upper] <- c(gsub(pattern = "^corrected_|\\.csv$", "", gsub(pattern = ".*?/", "", list_files)))
            
            mouse_number[files_counter_lower:files_counter_upper] <- c(gsub(pattern = "_\\d.*", "", gsub(pattern = "^_", "", gsub(pattern = "[a-z]||[A-Z]", "", filenames[files_counter_lower:files_counter_upper]))))
            trial_number_save[files_counter_lower:files_counter_upper] <- c(gsub(pattern = "^.*?_\\(|\\)", "", filenames[files_counter_lower:files_counter_upper]))
            variable_name <- paste0("data_frames_of_group_", tolower(all_to_underscore(group_names[i])))
            variable_name_save[count_up] <<- variable_name
            sub_group_name_save[files_counter_lower:files_counter_upper] <- c(rep("None", diff(c(files_counter_lower, files_counter_upper))+1))
            
            names(files)[files_counter_lower:files_counter_upper] <- filenames[files_counter_lower:files_counter_upper]
            
            #Creates a new variable in the global environment containing, at the same time creates a non global variable that will be used to create a holistic data frame
            assign(substitute(variable_name), files[files_counter_lower:files_counter_upper], envir = globalenv())
            
            all_data_frames[files_counter_lower:files_counter_upper] <<- files[files_counter_lower:files_counter_upper]
            
            files_counter_lower <- files_counter_lower + length(list_files)
            #count_up <- count_up + 1
            #progress_counter <- progress_counter + 1
            
            print(paste0(progress_counter,"/",length(subfolder_names[[i]]), " subfolders of group ", group_names[i], " are currently processed."))
            
        
        } else if(length(maybe_subfolder_names[[i]]) == length(subfolder_names[[i]]) || (length(maybe_subfolder_names[[i]]) != length(subfolder_names[[i]]) && identical(list.files(path = gsub(pattern = "//", "/", paths_to_groups[i], fixed = TRUE), recursive = FALSE, pattern = "corrected_.*?csv"), character(0))))
        {
            #Cases where no additional files other than the subfolders are present in the folder, OR where additional files are present, but are not corrected_ .csv files and therefore irrelevant
            
            #only sub_group data frames, as well as entire group data frame
            
            if((length(subfolder_names[[i]]) == 1 && subfolder_names[[i]] == "Corrected_Data") || (length(subfolder_names[[i]]) == 2 && any(subfolder_names[[i]] == "Corrected_Data") && any(subfolder_names[[i]] == "Meta_Data")))
            {
                #If the only subfolder present is the Corrected_Data folder, OR if the Corrected_Data folder and the Meta_Data folder are present
                #Same as no sub_groups present, group_name is data frame name and entire group data frame
                #print("Case 2")
                list_files <- list.files(path = gsub(pattern = "//", "/", paths_to_groups[i], fixed = TRUE), recursive = TRUE, pattern = "corrected_.*?csv")
                
                files_counter_upper <- files_counter_upper + length(list_files)
                
                files[files_counter_lower:files_counter_upper] <- lapply(paste0(paths_to_groups[i], "\\" , gsub(pattern = "//", "/", list_files, fixed = TRUE)), read.csv)
                
                filenames[files_counter_lower:files_counter_upper] <- c(gsub(pattern = "^corrected_|\\.csv$", "", gsub(pattern = ".*?/", "", list_files)))
                
                mouse_number[files_counter_lower:files_counter_upper] <- c(gsub(pattern = "_\\d.*", "", gsub(pattern = "^_", "", gsub(pattern = "[a-z]||[A-Z]", "", filenames[files_counter_lower:files_counter_upper]))))
                
                trial_number_save[files_counter_lower:files_counter_upper] <- c(gsub(pattern = "^.*?_\\(|\\)", "", filenames[files_counter_lower:files_counter_upper]))
                
                variable_name <- paste0("data_frames_of_group_", tolower(all_to_underscore(group_names[i])))
                
                variable_name_save[count_up] <<- variable_name
                
                sub_group_name_save[files_counter_lower:files_counter_upper] <- c(rep("None", diff(c(files_counter_lower, files_counter_upper))+1))
                
                names(files)[files_counter_lower:files_counter_upper] <- filenames[files_counter_lower:files_counter_upper]
                
                #Creates a new variable in the global environment containing, at the same time creates a non global variable that will be used to create a holistic data frame
                assign(substitute(variable_name), files[files_counter_lower:files_counter_upper], envir = globalenv())
                all_data_frames[files_counter_lower:files_counter_upper] <<- files[files_counter_lower:files_counter_upper]
                
                files_counter_lower <- files_counter_lower + length(list_files)
                count_up <- count_up + 1
                
                
                if(length(subfolder_names[[i]]) == 1)
                {
                    progress_counter <- progress_counter + 1
                    
                } else {
                    
                    progress_counter <- progress_counter + 1
                    
                    print(paste0(progress_counter,"/",length(subfolder_names[[i]]), " subfolders of group ", group_names[i], " are currently processed."))
                    
                    progress_counter <- progress_counter + 1
                }
                
                print(paste0(progress_counter,"/",length(subfolder_names[[i]]), " subfolders of group ", group_names[i], " are currently processed."))
                
            } else {
                
                #Cases where only subfolders are present which may or may not include the Corrected_Data and Meta_Data folder directly
                #print("Case 3")
                
                for(j in 1: length(subfolder_names[[i]]))
                {
                    if(subfolder_names[[i]][j] == "Corrected_Data")
                    {
                        #print("Case 3.1")
                        
                        #This instant will be treated as no_sub_group data
                        
                        #print(paste0(paths_to_groups[i], "\\Corrected_Data\\"))
                        #print(paste0(gsub(pattern = "//", "/", paths_to_groups[i], fixed = TRUE), "\\Corrected_Data"))
                        
                        if(identical(list.files(path = paste0(gsub(pattern = "//", "/", paths_to_groups[i], fixed = TRUE), "\\Corrected_Data"), recursive = TRUE, pattern = "corrected_.*?csv"), character(0)))
                        {
                            #If no fitting file exists, character(0) will be returned, so these folders can be skipped, although since the user manually chose the folder, a notification will be printed.
                            print(paste0("No corrected .csv files could be found in the path: ", paths_to_groups[i], "\\Corrected_Data"))
                            
                            progress_counter <- progress_counter + 1
                            
                            next #j = j + 1
                        }
                        
                        list_files <- list.files(path = paste0(gsub(pattern = "//", "/", paths_to_groups[i], fixed = TRUE), "\\", subfolder_names[[i]][j]), recursive = TRUE, pattern = "corrected_.*?csv")
                        
                        files_counter_upper <- files_counter_upper + length(list_files)
                        
                        files[files_counter_lower:files_counter_upper] <- lapply(paste0(paths_to_groups[i], "\\Corrected_Data\\" , gsub(pattern = "//", "/", list_files, fixed = TRUE)), read.csv)
                        
                        filenames[files_counter_lower:files_counter_upper] <- c(gsub(pattern = "^corrected_|\\.csv$", "", gsub(pattern = ".*?/", "", list_files)))
                        
                        mouse_number[files_counter_lower:files_counter_upper] <- c(gsub(pattern = "_\\d.*", "", gsub(pattern = "^_", "", gsub(pattern = "[a-z]||[A-Z]", "", filenames[files_counter_lower:files_counter_upper]))))
                        
                        trial_number_save[files_counter_lower:files_counter_upper] <- c(gsub(pattern = "^.*?_\\(|\\)", "", filenames[files_counter_lower:files_counter_upper]))
                        
                        variable_name <- paste0("data_frames_of_group_", tolower(all_to_underscore(group_names[i])), "_no_sub_group")
                        variable_name_save[count_up] <<- variable_name
                        
                        sub_group_name_save[files_counter_lower:files_counter_upper] <- c(rep("None", diff(c(files_counter_lower, files_counter_upper))+1))
                        
                        names(files)[files_counter_lower:files_counter_upper] <- filenames[files_counter_lower:files_counter_upper]
                        files_save <- append(files_save, files[files_counter_lower:files_counter_upper])
                        
                        #Creates a new variable in the global environment containing, at the same time creates a non global variable that will be used to create a holistic data frame
                        assign(substitute(variable_name), files[files_counter_lower:files_counter_upper], envir = globalenv())
                        all_data_frames[files_counter_lower:files_counter_upper] <<- files[files_counter_lower:files_counter_upper]
                        
                        files_counter_lower <- files_counter_lower + length(list_files)
                        count_up <- count_up + 1
                        
                        progress_counter <- progress_counter + 1
                        
                        print(paste0(progress_counter,"/",length(subfolder_names[[i]]), " subfolders of group ", group_names[i], " are currently processed."))
                        
                    } else 
                    {
                        
                        if(subfolder_names[[i]][j] == "Meta_Data")
                        {
                            #print("Case 3.2")
                            progress_counter <- progress_counter + 1
                            
                            print(paste0(progress_counter,"/",length(subfolder_names[[i]]), " subfolders of group ", group_names[i], " are currently processed."))
                            next #j = j + 1
                        }
                        
                        #print("Case 3.3")
                        
                        #Normal sub_groups
                        
                        #print(paste0(paths_to_groups[i], "\\", subfolder_names[[i]][j], "\\"))
                        #print(paste0(gsub(pattern = "//", "/", paths_to_groups[i], fixed = TRUE), "\\", subfolder_names[[i]][j]))
                        
                        if(identical(list.files(path = paste0(gsub(pattern = "//", "/", paths_to_groups[i], fixed = TRUE), "\\", subfolder_names[[i]][j]), recursive = TRUE, pattern = "corrected_.*?csv"), character(0)))
                        {
                            #If no fitting file exists, character(0) will be returned, so these folders can be skipped, although since the user manually chose the folder, a notification will be printed.
                            print(paste0("No corrected .csv files could be found in the path: ", paths_to_groups[i], "\\", subfolder_names[[i]][j]))
                            
                            progress_counter <- progress_counter + 1
                            
                            next #j = j + 1
                        }
                        
                        list_files <- list.files(path = paste0(gsub(pattern = "//", "/", paths_to_groups[i], fixed = TRUE), "\\", subfolder_names[[i]][j]), recursive = TRUE, pattern = "corrected_.*?csv")
                        
                        files_counter_upper <- files_counter_upper + length(list_files)
                        
                        files[files_counter_lower:files_counter_upper] <- lapply(paste0(paths_to_groups[i], "\\", subfolder_names[[i]][j], "\\", gsub(pattern = "//", "/", list_files, fixed = TRUE)), read.csv)
                        
                        filenames[files_counter_lower:files_counter_upper] <- c(gsub(pattern = "^corrected_|\\.csv$", "", gsub(pattern = ".*?/", "", list_files)))
                        
                        mouse_number[files_counter_lower:files_counter_upper] <- c(gsub(pattern = "_\\d.*", "", gsub(pattern = "^_", "", gsub(pattern = "[a-z]||[A-Z]", "", filenames[files_counter_lower:files_counter_upper]))))
                        
                        trial_number_save[files_counter_lower:files_counter_upper] <- c(gsub(pattern = "^.*?_\\(|\\)", "", filenames[files_counter_lower:files_counter_upper]))
                        
                        variable_name <- paste0("data_frames_of_group_", tolower(all_to_underscore(group_names[i])), "_", tolower(all_to_underscore(subfolder_names[[i]][j])))
                        variable_name_save[count_up] <<- variable_name
                        
                        sub_group_name_save[files_counter_lower:files_counter_upper] <- c(rep(subfolder_names[[i]][j], diff(c(files_counter_lower, files_counter_upper))+1))
                        
                        names(files)[files_counter_lower:files_counter_upper] <- filenames[files_counter_lower:files_counter_upper]
                        files_save <- append(files_save, files[files_counter_lower:files_counter_upper])
                        
                        #Creates a new variable in the global environment containing, at the same time creates a non global variable that will be used to create a holistic data frame
                        assign(substitute(variable_name), files[files_counter_lower:files_counter_upper], envir = globalenv())
                        all_data_frames[files_counter_lower:files_counter_upper] <<- files[files_counter_lower:files_counter_upper]
                        
                        files_counter_lower <- files_counter_lower + length(list_files)
                        
                        count_up <- count_up + 1
                        
                        progress_counter <- progress_counter + 1
                        
                        print(paste0(progress_counter,"/",length(subfolder_names[[i]]), " subfolders of group ", group_names[i], " are currently processed."))
                    }
                    
                    next #j = j + 1
                }
                
                variable_name <- paste0("data_frames_of_group_", tolower(all_to_underscore(group_names[i])))
                
                assign(substitute(variable_name), files_save, envir = globalenv())
                
                files_save <- NULL
                files_save <- list()
            }
            
        } else {
            #Cases where both subfolders and corrected_ .csv files are present in the given group folder
            #no_sub_group data frame in addtion to other sub group data frames, as well as entire group data frame
            #print("Case 4")
            #I don't know how to properly integrate this one right now
            
            print(paste0("Error in: ", paths_to_groups[i], ". This directory will be skipped. Please look at the text right after this one."))
            print("Either you have no subfolders in a chosen group directory or all corrected_ .csv files are within subfolders. You are allowed to have other files (again: not corrected_ .csv files) in the group directory containing subfolders.")
            
            next #i = i + 1
        }
        
        #print(" ")
        
        print(paste0("Done with subfolders of group ", group_names[i]))
        
        if(i == 1)
        {
            group_name_save[1:files_counter_upper] <- rep(group_names[i], files_counter_upper)
            
            bottom_number <- files_counter_upper + 1
            
        } else {
            
            group_name_save[bottom_number:files_counter_upper] <- rep(group_names[i], abs(diff(c(bottom_number, files_counter_upper)))+1)
            
            bottom_number <- files_counter_upper + 1
        }
        
        next #i = i + 1
    } #end of for-loop i
    
    names(all_data_frames) <<- filenames
    
    print("The following variables have been created:")
    print(variable_name_save)
    print("all_data_frames")
    
    dates <<- NULL
    
    #Getting the proper dates for all data frames
    for (ii in 1:length(all_data_frames))
    {
        if(is.na(names(all_data_frames[ii])) || is.null(names(all_data_frames[ii])))
        {
            next #ii = ii + 1
        }
        #From name, remove everything except mouse number and then count how many digits there are
        ncharlength <- nchar(gsub(pattern = "_.*" ,"", gsub(pattern = "\\_\\(.*?\\d\\)", "", names(all_data_frames[ii]))))
        
        #Convert the count into an integer
        ncharlength <- as.integer(ncharlength)
        
        #Repeat \\d as many times as counted 
        digitrepeat <- paste0(rep("\\d", ncharlength), collapse = "")
        
        #Use the \\d repeats to get rid of mouse number to extract the dates
        dates[ii] <<- gsub(pattern = "\\_", ".", gsub(pattern = paste0(digitrepeat, "_") ,"", gsub(pattern = "\\_\\(.*?\\d\\)", "", names(all_data_frames[ii]))))
        
        next #ii = ii + 1
    }
    
    dates_of_recording <- unique(dates)
    
    
    #main_table creation
    main_table <<- data.frame(cbind(names(all_data_frames), mouse_number, dates, trial_number_save, NA, NA, group_name_save, sub_group_name_save, NA, NA, NA))
    colnames(main_table) <<- c("Trial", "Mouse Number", "Date of Recording", "Trial Number", "Trials on that Date", "Total Trials", "Group", "Subgroup", "Frames per Second", "Horizontal Resolution", "Vertical Resolution")
    
    #write.csv(main_table, file = paste0(main_path, "\\", "main_table", ".csv"))
    
    #Mouse Number, Date of Recording, Trial Number, Trials on that Date, Total Trials, Group, Subgroup, Frames per Second, Horizontal Resolution, Vertical Resolution
    
    
    print("A main_table has been created and can be called by that name.")
    
    #Determines the column names and assigns variables based on the names. Also drops the X column at the beginning, if present
    column_names <- colnames(all_data_frames[[1]])
    
    #Create simple variables that can be used to call the following columns
    for(j in 1:length(column_names))
    {
        if(column_names[j] == "frame"){
            frame_c <<- j
        }
        if(column_names[j] == "left_eye_x"){
            left_eye_x <<- j
        }
        if(column_names[j] == "left_eye_y"){
            left_eye_y <<- j
        }
        if(column_names[j] == "right_eye_x"){
            right_eye_x <<- j
        }
        if(column_names[j] == "right_eye_y"){
            right_eye_y <<- j
        }
        if(column_names[j] == "snout_x"){
            snout_x <<- j
        }
        if(column_names[j] == "snout_y"){
            snout_y <<- j
        }
        if(column_names[j] == "belly_x"){
            belly_x <<- j
        }
        if(column_names[j] == "belly_y"){
            belly_y <<- j
        }
        if(column_names[j] == "back_x"){
            back_x <<- j
        }
        if(column_names[j] == "back_y"){
            back_y <<- j
        }
        if(column_names[j] == "front_left_paw_x"){
            front_left_paw_x <<- j
        }
        if(column_names[j] == "front_left_paw_y"){
            front_left_paw_y <<- j
        }
        if(column_names[j] == "front_right_paw_x"){
            front_right_paw_x <<- j
        }
        if(column_names[j] == "front_right_paw_y"){
            front_right_paw_y <<- j
        }
        if(column_names[j] == "back_left_paw_x"){
            back_left_paw_x <<- j
        }
        if(column_names[j] == "back_left_paw_y"){
            back_left_paw_y <<- j
        }
        if(column_names[j] == "back_right_paw_x"){
            back_right_paw_x <<- j
        }
        if(column_names[j] == "back_right_paw_y"){
            back_right_paw_y <<- j
        }
        if(column_names[j] == "tail_base_x"){
            tail_base_x <<- j
        }
        if(column_names[j] == "tail_base_y"){
            tail_base_y <<- j
        }
        if(column_names[j] == "tail_center_x"){
            tail_center_x <<- j
        }
        if(column_names[j] == "tail_center_y"){
            tail_center_y <<- j
        }
        if(column_names[j] == "tail_tip_x"){
            tail_tip_x <<- j
        }
        if(column_names[j] == "tail_tip_y"){
            tail_tip_y <<- j
        }
        if(column_names[j] == "mark_far_left_x"){
            mark_far_left_x <<- j
        }
        if(column_names[j] == "mark_far_left_y"){
            mark_far_left_y <<- j
        }
        if(column_names[j] == "mark_left_x"){
            mark_left_x <<- j
        }
        if(column_names[j] == "mark_left_y"){
            mark_left_y <<- j
        }
        if(column_names[j] == "mark_right_x"){
            mark_right_x <<- j
        }
        if(column_names[j] == "mark_right_y"){
            mark_right_y <<- j
        }
        if(column_names[j] == "mark_far_right_x"){
            mark_far_right_x <<- j
        }
        if(column_names[j] == "mark_far_right_y"){
            mark_far_right_y <<- j
        }
        if(column_names[j] == "top_beam_left_x"){
            top_beam_left_x<<- j
        }
        if(column_names[j] == "top_beam_left_y"){
            top_beam_left_y <<- j
        }
        if(column_names[j] == "top_beam_middle_x"){
            top_beam_middle_x <<- j
        }
        if(column_names[j] == "top_beam_middle_y"){
            top_beam_middle_y <<- j
        }
        if(column_names[j] == "top_beam_right_x"){
            top_beam_right_x <<- j
        }
        if(column_names[j] == "top_beam_right_y"){
            top_beam_right_y <<- j
        }
        if(column_names[j] == "bottom_beam_left_x"){
            bottom_beam_left_x <<- j
        }
        if(column_names[j] == "bottom_beam_left_y"){
            bottom_beam_left_y <<- j
        }
        if(column_names[j] == "bottom_beam_middle_x"){
            bottom_beam_middle_x <<- j
        }
        if(column_names[j] == "bottom_beam_middle_y"){
            bottom_beam_middle_y <<- j
        }
        if(column_names[j] == "bottom_beam_right_x"){
            bottom_beam_right_x <<- j
        }
        if(column_names[j] == "bottom_beam_right_y"){
            bottom_beam_right_y <<- j
        }
        
        next #j = j + 1
    }
} #End of function prepare_everything_1



prepare_everything_2 <- function(main_path, all_df_list, var_names, table)
{
    num_mice_total <<- length(unique(table$`Mouse Number`))
    
    for(j in 1:length(var_names))
    {
        if(is.na(var_names[j]) || is.null(var_names[j]))
        {
            next #j = j + 1
        }
        
        num_mice <- 0L
        
        filenames <- names(get(var_names[j]))
        
        assign(filenames[1], get(variable_name_save[[j]])[1], envir = environment())
        
        if(length(filenames) == 1)
        {
            #take care of it and skip to next j
            which_row <- which(gsub(pattern = "\\_\\(.*\\d\\)", "", names(all_df_list)) == paste0(gsub(pattern = "\\_\\(.*\\d\\)", "", filenames[1])))
            table$`Trials on that Date`[which_row] <- 1
            assign(paste0(gsub("\\_\\(.*\\d\\)", "", filenames[1])), all_df_list[which_row])
            num_mice <- 1
            print(paste("Done with", gsub(pattern = "\\_\\(.*?\\d\\)", "" , filenames[1])))
            assign(paste0("num_mice_", gsub(pattern = "data_frames_of_group_", "", var_names[j])), num_mice, envir = .GlobalEnv)
            next # j = j + 1
        }
        
        
        for(i in 2:length(filenames))
        {
           if(gsub("\\_\\(.*\\d\\)", "", filenames[i-1]) == gsub("\\_\\(.*\\d\\)", "", filenames[i]))
           {
               #skip because it's the same mouse
               #except if it's the last one
               if(i == length(filenames))
               {
                   which_row <- which(gsub(pattern = "\\_\\(.*\\d\\)", "", names(all_df_list)) == paste0(gsub(pattern = "\\_\\(.*\\d\\)", "", filenames[i])))
                   table$`Trials on that Date`[which_row] <- length(which_row)
                   assign(paste0(gsub("\\_\\(.*\\d\\)", "", filenames[i])), all_df_list[which_row], envir = .GlobalEnv)
                   num_mice = num_mice + 1
                   print(paste("Done with", gsub(pattern = "\\_\\(.*?\\d\\)", "" , filenames[i])))
                   
               }
           } else {
               if(i == length(filenames))
               {
                    
                    #Last trial is not the same mouse as the trial before
                    #take care of i-1 and i
                    which_row <- which(gsub(pattern = "\\_\\(.*\\d\\)", "", names(all_df_list)) == paste0(gsub(pattern = "\\_\\(.*\\d\\)", "", filenames[i-1])))
                    table$`Trials on that Date`[which_row] <- length(which_row)
                    assign(paste0(gsub("\\_\\(.*\\d\\)", "", filenames[i-1])), all_df_list[which_row], envir = .GlobalEnv)
                    num_mice = num_mice + 1
                    print(paste("Done with", gsub(pattern = "\\_\\(.*?\\d\\)", "" , filenames[i-1])))
                     
                    which_row <- which(gsub(pattern = "\\_\\(.*\\d\\)", "", names(all_df_list)) == paste0(gsub(pattern = "\\_\\(.*\\d\\)", "", filenames[i])))
                    table$`Trials on that Date`[which_row] <- length(which_row)
                    assign(paste0(gsub("\\_\\(.*\\d\\)", "", filenames[i])), all_df_list[which_row], envir = .GlobalEnv)
                    num_mice = num_mice + 1
                    print(paste("Done with", gsub(pattern = "\\_\\(.*?\\d\\)", "" , filenames[i])))
           
               } else {
                   
                    #All other entries and also if the last trials mouse matches with the previous one
                    #new mouse in filenames[i]
                    #take care of i-1
                    #trouble with last entry in subgroup
                    which_row <- which(gsub(pattern = "\\_\\(.*\\d\\)", "", names(all_df_list)) == paste0(gsub(pattern = "\\_\\(.*\\d\\)", "", filenames[i-1])))
                    table$`Trials on that Date`[which_row] <- length(which_row)
                    assign(paste0(gsub("\\_\\(.*\\d\\)", "", filenames[i-1])), all_df_list[which_row], envir = .GlobalEnv)
                    num_mice = num_mice + 1
                    print(paste("Done with", gsub(pattern = "\\_\\(.*?\\d\\)", "" , filenames[i-1])))
               }
           }
           next #i = i + 1
        }#end of for-loop i
        
        assign(paste0("num_mice_", gsub(pattern = "data_frames_of_group_", "", var_names[j])), num_mice, envir = .GlobalEnv)
        
        next #j = j + 1
    }
    
    
    #Put same mouse tested at different dates in one additional data frame and total trials number
    
    unique_mouse_number <- unique(table$`Mouse Number`)
    
    for(k in 1:length(unique_mouse_number))
    {
        
        which_ones <- which(gsub(pattern = "_\\d.*", "", gsub(pattern = "^_", "", gsub(pattern = "[a-z]||[A-Z]", "", names(all_df_list)))) == unique_mouse_number[k])
        
        assign(unique_mouse_number[k], all_df_list[which_ones], envir = .GlobalEnv)
        
        table$`Total Trials`[which_ones] <- length(get(unique_mouse_number[k]))
    }
    
    #tabl <- table
    
    #rm(dates)
    
    assign("main_table", table, envir = .GlobalEnv)
    write.csv(main_table, file = paste0(main_path, "\\", "main_table", ".csv"))
    wd <- getwd()
    setwd(main_path)
    saveRDS(all_data_frames, file = "all_data_frames.RDS", compress = FALSE)
    setwd(wd)
    
} #End of function prepare_everything_2



search_dataframe <- function(df_list, df)
{
    found <- FALSE
    
    for(i in 1:length(df_list))
    {
        if(isTRUE(all.equal(df_list[[i]], df)))
        {
            print(paste("The list entry is", i))
            found <- TRUE
        }
    }
    
    if(!isTRUE(found))
    {
        print("could not find data frame in list.")
    }
} #End of function search_dataframe



random_data_frames <- function(df_list, percentage = NULL)
{
    #set.seed(69)
    numbers_of_dataframes <- c()
    frame_names <- c()
    
    if(is.null(percentage)){
        percentage <- 0.2
    } 
    if(percentage <= 0 || percentage > 1){
        percentage <- 0.2
        print(paste("Unfeasable percentage given, percentage is set to:", percentage))
    }
    
    numbers_of_dataframes <- sample(1:length(df_list), round(length(df_list)*percentage), replace = FALSE)
    
    #cat(random_dataframes)
    #print(length(random_dataframes))
    
    for(i in 1:length(numbers_of_dataframes))
    {
        frame_names[i] <- names(df_list[numbers_of_dataframes[i]])
    }
    
    #cat(frame_names)
    #print(length(frame_names))
    
    random_dataframes <<- data.frame(numbers_of_dataframes, frame_names)
    colnames(random_dataframes) <<-  c("Number of Data Frame", "Data Frame Name")
    print("A random assortment of data frames sampled. These can be accessed using: random_dataframes")
} #End of function random_data_frames



read_all_files_into_environment <- function(directory = NULL)
{
    wd <- getwd()
    wd <- gsub(pattern = "/","\\", wd, fixed = TRUE)
    
    if(is.null(directory))
    {
        print(paste0("No directory was given. An additional window should have opened. Please choose a directory in that opened window to read the results from, simply close that window if the result should be read from the current working directory, which is: ", wd))
        directory <- choose.dir(caption = "No directory was given. Please choose a directory to read the results from. Just close this window, if the result should be read from the current working directory.")
        
        if(is.null(directory) || is.na(directory))
        {
            directory <- wd
        } else {
            directory <- gsub(pattern = "/","\\", directory, fixed = TRUE)
        }
    }
    
    setwd(directory)
    
    #if(is.null(name) || name != "slip_detection_list")
    #{
    #    print("name must be \"slip_detection_list\" for this function to work. Look at the previously created files and choose the right name.")
    #    stop()
    #}
    
    #full_detection_list <<- read.csv(paste0(directory, "\\", name, ".csv"), header = TRUE, sep = ",") #Maybe the separator needs to be changed for other csvs.
    #full_detection_list <<- subset(full_detection_list, select = -c(1))
    
    slip_detection_list <<- read.csv(paste0(directory, "\\", "slip_detection_list", ".csv"), header = TRUE, sep = ",", colClasses = c("character")) #Maybe the separator needs to be changed for other csvs.
    slip_detection_list <<- subset(slip_detection_list, select = -c(1))
    colnames(slip_detection_list) <<- gsub(pattern = ".", " ",colnames(slip_detection_list), fixed = TRUE)
    slip_detection_list$`Mouse Number` <<- as.character(slip_detection_list$`Mouse Number`)
    slip_detection_list$`Trial Number` <<- as.integer(slip_detection_list$`Trial Number`)
    slip_detection_list$`Trials on that Date` <<- as.integer(slip_detection_list$`Trials on that Date`)
    slip_detection_list$`Total Trials` <<- as.integer(slip_detection_list$`Total Trials`)
    slip_detection_list$`Number of Minor Slips` <<- as.integer(slip_detection_list$`Number of Minor Slips`)
    slip_detection_list$`Number of Major Slips` <<- as.integer(slip_detection_list$`Number of Major Slips`)
    slip_detection_list$`Length of Trial in Frames` <<- as.integer(slip_detection_list$`Length of Trial in Frames`)
    slip_detection_list$`Total Frames below Top of Beam` <<- as.integer(slip_detection_list$`Total Frames below Top of Beam`)
    
    #slip_detection_list$`Number_of_Minor_Slips` <<- as.integer(slip_detection_list$`Number_of_Minor_Slips`)
    #slip_detection_list$`Number_of_Major_Slips` <<- as.integer(slip_detection_list$`Number_of_Major_Slips`)
    #slip_detection_list$`Length_of_Trial_in_Frames` <<- as.integer(slip_detection_list$`Length_of_Trial_in_Frames`)
    #slip_detection_list$`Total_Frames_below_Top_of_Beam` <<- as.integer(slip_detection_list$`Total_Frames_below_Top_of_Beam`)
    #colnames(slip_detection_list) <<- gsub(pattern = "_", " ",colnames(slip_detection_list), fixed = TRUE)
    
    main_table <<- read.csv(paste0(directory, "\\", "main_table", ".csv"), header = TRUE, sep = ",", colClasses = c("character"))
    main_table <<- subset(main_table, select = -c(1))
    colnames(main_table) <<- gsub(pattern = ".", " ",colnames(main_table), fixed = TRUE)
    main_table$`Mouse Number` <<- as.character(main_table$`Mouse Number`)
    main_table$`Trial Number` <<- as.integer(main_table$`Trial Number`)
    main_table$`Trials on that Date` <<- as.integer(main_table$`Trials on that Date`)
    main_table$`Total Trials` <<- as.integer(main_table$`Total Trials`)
    main_table$`Frames per Second` <<- as.integer(main_table$`Frames per Second`)
    main_table$`Horizontal Resolution` <<- as.integer(main_table$`Horizontal Resolution`)
    main_table$`Vertical Resolution` <<- as.integer(main_table$`Vertical Resolution`)
    
    mean_list <<- read.csv(paste0(directory, "\\", "mean_list", ".csv"), header = TRUE, sep = ",")
    mean_list <<- subset(mean_list, select = -c(1))
    colnames(mean_list) <<- gsub(pattern = ".", " ",colnames(mean_list), fixed = TRUE)
    
    mean_list_cm <<- read.csv(paste0(directory, "\\", "mean_list_cm", ".csv"), header = TRUE, sep = ",")
    mean_list_cm <<- subset(mean_list_cm, select = -c(1))
    colnames(mean_list_cm) <<- gsub(pattern = ".", " ",colnames(mean_list_cm), fixed = TRUE)
    
    area_of_mouse_table <<- read.csv(paste0(directory, "\\", "area_of_mouse_table", ".csv"), header = TRUE, sep = ",")
    area_of_mouse_table <<- subset(area_of_mouse_table, select = -c(1))
    colnames(area_of_mouse_table) <<- gsub(pattern = ".", " ",colnames(area_of_mouse_table), fixed = TRUE)
    
    all_data_frames <<- readRDS(file = "all_data_frames.RDS")
    
    reverse_ratio                    <<- readRDS(file = "revese_ratio.RDS")
    
    reverse_ratio_blp                <<- readRDS(file = "revese_ratio_blp.RDS")
    reverse_ratio_flp                <<- readRDS(file = "revese_ratio_flp.RDS")
    reverse_ratio_bel                <<- readRDS(file = "revese_ratio_bel.RDS")
    reverse_ratio_bac                <<- readRDS(file = "revese_ratio_bac.RDS")
    reverse_ratio_snt                <<- readRDS(file = "revese_ratio_snt.RDS")
    reverse_ratio_le                 <<- readRDS(file = "revese_ratio_le.RDS")
    reverse_ratio_tb                 <<- readRDS(file = "revese_ratio_tb.RDS")
    reverse_ratio_tc                 <<- readRDS(file = "revese_ratio_tc.RDS")
    reverse_ratio_tt                 <<- readRDS(file = "revese_ratio_tt.RDS")
    
    instantaneous_velocity_belly     <<- readRDS(file = "instantaneous_velocity_belly.RDS")
    instantaneous_velocity_back      <<- readRDS(file = "instantaneous_velocity_back.RDS")
    instantaneous_velocity_tail_base <<- readRDS(file = "instantaneous_velocity_tail_base.RDS")
    
    stationary_belly                 <<- readRDS(file = "stationary_belly.RDS")
    stationary_back                  <<- readRDS(file = "stationary_back.RDS")
    stationary_tail_base             <<- readRDS(file = "stationary_tail_base.RDS")
    
    stationary                       <<- readRDS(file = "stationary.RDS")
    
    tb_to_tt_angle                   <<- readRDS(file = "tb_to_tt_angle.RDS")
    
    segment_duration_dataframes      <<- readRDS(file = "segment_duration_dataframes.RDS")
    slip_position_dataframes         <<- readRDS(file = "slip_position_dataframes.RDS")
    
    
    print(paste("Directly read the csv file. You can call them using:", "all_data_frames, slip_detection_list, mean_list, mean_list_cm, area_of_mouse_table, reverse_ratio, reverse_ratio_blp, reverse_ratio_flp, reverse_ratio_bel, reverse_ratio_bac, reverse_ratio_snt, reverse_ratio_le, reverse_ratio_tb, reverse_ratio_tc, reverse_ratio_tt, instantaneous_velocity_belly, instantaneous_velocity_back, instantaneous_velocity_tail_base, stationary_belly, stationary_back, stationary_tail_base, stationary, tb_to_tt_angle, segment_duration_dataframes, slip_position_dataframes"))
    
    setwd(wd)
    
} #End of function read_all_files_into_environment



slip_detection <- function(table, df_list, body_part_x = NULL, body_part_y = NULL, columns_to_remove = NULL, visualize_results = NULL, visualize_process = NULL, visualize_process_minor = NULL, visualize_process_major = NULL, col_names = NULL, save_files = NULL, directory = NULL, directly_read_files = NULL, be_vocal = TRUE, exception_list = NULL, try_reverse_ratio_plotting = NULL, minor_slip_threshold = NULL, major_slip_threshold = NULL)
{
    
    ###Initiation###
    
    #Needed for valleys() and peaks() function
    if(!isTRUE("photobiology" %in% (.packages())))
    {
        if(!require(photobiology))
        {
            install.packages(photobiology, dep=TRUE, quiet = TRUE) #repos='http://star-www.st-andrews.ac.uk/cran/')
        }
        suppressPackageStartupMessages(library(photobiology))  #suppressPackageStartupMessages() because the package keeps telling you that it was made for a different R Version
    }
    
    i <- 0L #used for main df_list loop
    #az <- 0L #used for date separation
    ay <- 0L #used for date separation
    ax <- 0L #used for instantaneous velocity calculation
    
    
    if(is.null(minor_slip_threshold)){
        minor_slip_threshold <- 1.02
    } #1.02 represents 20% below the beam. Which is the line where points below will be counted as minor slips.
    if(is.null(major_slip_threshold)){
        major_slip_threshold <- 1.05
    } #1.05 represents 50% below the beam. Which is the line where points below will be counted as major slips.
    
    
    if(is.null(visualize_results)){
        visualize_results <- FALSE
    } #FALSE
    if(is.null(visualize_process)){
        visualize_process <- FALSE
    } #FALSE
    
    if(is.null(visualize_process_minor)){
        visualize_process_minor <- FALSE
    } #FALSE
    if(is.null(visualize_process_major)){
        visualize_process_major <- FALSE
    } #FALSE
    
    if(is.null(body_part_y) || is.null(body_part_x)) 
    {
        body_part_y <- back_left_paw_y
        body_part_x <- back_left_paw_x
        
        print(paste0("No body_part_y or body_part_x was given. These are essential for this function to work. If none or given the default body part will be used and plotted which is the back/hind left paws x and y position. Which are column number ", back_left_paw_x, " for the x-position and ", back_left_paw_y, " for the y-position."))
    } #Defaults to hind left paw
    
    if(is.null(try_reverse_ratio_plotting))
    {
        try_reverse_ratio_plotting <- FALSE
    } #FALSE
    
    if(is.null(save_files)){
        save_files <- FALSE
    } #FALSE
    if(is.null(directly_read_files)){
        directly_read_files <- FALSE
    } #FALSE
    if(is.null(columns_to_remove)){
        columns_to_remove <- c(9999) #Unreachable column number to exclude. c(), c(0), NULL, NA, NaN, etc. do not work. Maybe Inf would work but I have not tested that.
    } #Unreachable column number
    if(is.null(col_names)){
        #Column Names for the full_detection_list, not_full_detection_list and another_data_frame
        
        #"Mouse_Line", "Date_of_Birth", "Gender", "Date_of_Recording_BW", "Age_during_Recording_BW",..., "Age_Group", -> remove
        col_names <- c("Trial","Mouse Number", "Date of Recording", "Trial Number", "Trials on that Date", "Total Trials", "Group", "Subgroup", "Number of Minor Slips", "Number of Major Slips", "Length of Trial in Frames", "Total Frames below Top of Beam")
        
        print(paste("No column names were given, the default column names are taken, which are:"))
        cat(col_names)
        print("")
    } #If TRUE, then use default column names
    
    
    #Inquire path to save file
    if(save_files == TRUE)
    {
        wd <- getwd()
        wd <- gsub(pattern = "/","\\", wd, fixed = TRUE)
        
        if(is.null(directory))
        {
            print(paste0("No directory was given. An additional window should have opened. Please choose a directory in that opened window to save the results, simply close that window if the result should be saved in the current working directory, which is: ", wd))
            directory <- choose.dir(caption = "No directory was given. Please choose a directory to save the results. Just close this window, if the result should be saved in the current working directory.")
            
            if(is.null(directory) || is.na(directory))
            {
                directory <- wd
            } else {
                directory <- gsub(pattern = "/","\\", directory, fixed = TRUE)
            }
        }
        
        setwd(directory)
    }
    
    minor_slip_list <- rep(NA, times = length(df_list))
    major_slip_list <- rep(NA, times = length(df_list))
    
    #list of list with strings inside
    where_is_this_point_analysed <- vector(mode = "list", length(df_list)) #Each frame labelled if labelled in minor, major slip analysis, etc. Used to get segment_duration_dataframes
    
    #list of 2 column data frames (different lengths) containing strings and numbers
    what_segment_is_it           <- vector(mode = "list", length(df_list)) #labels from where_is_this_point_analysed segmented
    how_long_is_this_segment     <- vector(mode = "list", length(df_list)) #duration of segment
    start_frame                  <- vector(mode = "list", length(df_list)) #start frame of segment
    end_frame                    <- vector(mode = "list", length(df_list)) #end frame of segment
    
    #global list of the above data frames
    segment_duration_dataframes <- vector(mode = "list", length(df_list))
    
    #list of 4 column data frames (different lengths) containing numbers
    at_what_frames_are_the_slips <- vector(mode = "list", length(df_list)) #the exact frames defined as slips
    what_point_is_it_x           <- vector(mode = "list", length(df_list)) #x coordinates of these slips
    what_point_is_it_y           <- vector(mode = "list", length(df_list)) #y coordinates of these slips
    type_of_slip                 <- vector(mode = "list", length(df_list)) #major or minor
    
    #global list of the above data frames
    slip_position_dataframes <- vector(mode = "list", length(df_list))
    
    
    reverse_ratio <- vector(mode = "list", length(df_list)) #This vector() function fixed the "*tmp*[[k]] subscript out of bounds" error; no continous memory allocation required due to a set length of lists in a vector.
    
    reverse_ratio_blp <- vector(mode = "list", length(df_list))
    reverse_ratio_flp <- vector(mode = "list", length(df_list))
    reverse_ratio_bel <- vector(mode = "list", length(df_list))
    reverse_ratio_bac <- vector(mode = "list", length(df_list))
    reverse_ratio_snt <- vector(mode = "list", length(df_list))
    reverse_ratio_le  <- vector(mode = "list", length(df_list))
    reverse_ratio_tb  <- vector(mode = "list", length(df_list))
    reverse_ratio_tc  <- vector(mode = "list", length(df_list))
    reverse_ratio_tt  <- vector(mode = "list", length(df_list))
    
    instantaneous_velocity_belly <- vector(mode = "list", length(df_list))
    stationary_belly  <- vector(mode = "list", length(df_list))
    instantaneous_velocity_back <- vector(mode = "list", length(df_list))
    stationary_back  <- vector(mode = "list", length(df_list))
    instantaneous_velocity_tail_base <- vector(mode = "list", length(df_list))
    stationary_tail_base  <- vector(mode = "list", length(df_list))
    
    #For saying that the mouse is "truly" stationary
    stationary  <- vector(mode = "list", length(df_list))
    
    
    #All for area calculation of an irregular rectangle
    snt_bel_dist <- vector(mode = "list", length(df_list)) #Side a for area calculation
    bel_tb_dist  <- vector(mode = "list", length(df_list)) #Side b for area calculation
    tb_bac_dist  <- vector(mode = "list", length(df_list)) #Side c for area calculation
    bac_snt_dist <- vector(mode = "list", length(df_list)) #Side d for area calculation
    
    snt_tb_dist  <- vector(mode = "list", length(df_list)) #Side e for area calculation
    bel_bac_dist <- vector(mode = "list", length(df_list)) #Side f for area calculation
    
    area_of_mouse <- list()  #Please note that this will be square pixel
    area_of_mouse_cm <- list()  #Please note that this will be square cm
    
    area_of_mouse_table <- list() #Combined data into one data frame
    
    #Angle calculation
    tb_tt_dist <- vector(mode = "list", length(df_list)) #Distance from tail base to tail tip
    snt_tt_dist <- vector(mode = "list", length(df_list)) #Distance from snout to tail tip
    tb_to_tt_angle <- vector(mode = "list", length(df_list)) #Angle between two sides/vectors, in this case between tb_tt_dist and snt_tb_dist
    
    #Needed, if tail center is lower than tail tip (also for angle calculation)
    tb_tc_dist     <- vector(mode = "list", length(df_list))
    snt_tc_dist    <- vector(mode = "list", length(df_list))
    tb_to_tc_angle <- vector(mode = "list", length(df_list))
    
    
    #These subset lists are used if the tracking of the edge of the beam is not sufficient enough for any given trial. In these cases the mean of all videos recorded on that date will be taken. This is just very precautious, because this case will most likely will not occur.
    #df_list_subset <-  list()
    #
    #df_list_subset_mean_top_left_y <-  list()
    #df_list_subset_mean_top_right_y <-  list()
    #df_list_subset_mean_bottom_left_y <-  list()
    #df_list_subset_mean_bottom_right_y <-  list()
    #
    #df_list_subset_mean_top_left_x <-  list()
    #df_list_subset_mean_top_right_x <-  list()
    #df_list_subset_mean_bottom_left_x <-  list()
    #df_list_subset_mean_bottom_right_x <-  list()
    #
    #subset_var <- list()
    
    df_list_subset <-  NULL
    
    df_list_subset_mean_top_left_y <-  NULL
    df_list_subset_mean_top_right_y <-  NULL
    df_list_subset_mean_bottom_left_y <-  NULL
    df_list_subset_mean_bottom_right_y <-  NULL
    
    df_list_subset_mean_top_left_x <-  NULL
    df_list_subset_mean_top_right_x <-  NULL
    df_list_subset_mean_bottom_left_x <-  NULL
    df_list_subset_mean_bottom_right_x <-  NULL
    
    subset_var <- NULL
    
    #Also needed for the following subsetting
    #not_full_dataframe_list <- FALSE
    
    dates_of_recording <- unique(table$`Dates of Recording`)
    
    #Subsetting different dates of recordings to get a mean beam position of any given date, if the tracking for any given trial is not good enough
    for(ay in 1:length(dates_of_recording))
    {
        
        if(identical(unlist(names(df_list)), character(0)))
        {
            print("Something is not right with the names of the given data frames. The function will continue normally, but will break/fail if any data frame is encountered, where the beam is not well tracked.")
            break
        }
        
        if(identical(which(dates_of_recording[ay] == table$`Date of Recording`), integer(0)))
        {
            #print(paste("Date:", dates_of_recording[ay,], "is not present in current list of data frames."))
            #not_full_dataframe_list <- TRUE
            next #ay = ay + 1
        }
        
        subset_var <- which(dates_of_recording[ay] == table$`Date of Recording`)
        
        df_list_subset[[ay]] <- df_list[subset_var]
        
        #lapply allows for access of a certain column(s) present in a list of dataframes. Afterwards the selected column-list will be unlisted and then saved again as a dataframe from which then the first column is omitted (because this column only contains names) by subsetting with [[ay]][,1] and from that one can calculate a mean containing all values of that subset.
        df_list_subset_mean_top_left_y[[ay]] <- data.frame(unlist(lapply(df_list_subset[[ay]], "[", "top_beam_left_y"))) #Could also be done in one line like this: mean(data.frame(unlist(lapply(df_list_subset[[ay]], "[", "top_beam_left_y")))[,1], na.rm =TRUE)
        
        
        if(length(df_list_subset_mean_top_left_y[[ay]]) == 0L || sum(dim(df_list_subset_mean_top_left_y[[ay]])) == 0L)
        {
            #not_full_dataframe_list <- TRUE
            df_list_subset_mean_top_left_y[[ay]] <- NULL
            next
        }
        
        df_list_subset_mean_top_left_y[[ay]] <- df_list_subset_mean_top_left_y[[ay]][,1]
        df_list_subset_mean_top_left_y[[ay]] <- mean(df_list_subset_mean_top_left_y[[ay]], na.rm =TRUE)
        
        
        df_list_subset_mean_top_left_x[[ay]] <- data.frame(unlist(lapply(df_list_subset[[ay]], "[", "top_beam_left_x")))
        df_list_subset_mean_top_left_x[[ay]] <- df_list_subset_mean_top_left_x[[ay]][,1]
        df_list_subset_mean_top_left_x[[ay]] <- mean(df_list_subset_mean_top_left_x[[ay]], na.rm =TRUE)
        
        
        df_list_subset_mean_top_right_y[[ay]] <- data.frame(unlist(lapply(df_list_subset[[ay]], "[", "top_beam_right_y")))
        df_list_subset_mean_top_right_y[[ay]] <- df_list_subset_mean_top_right_y[[ay]][,1]
        df_list_subset_mean_top_right_y[[ay]] <- mean(df_list_subset_mean_top_right_y[[ay]], na.rm =TRUE)
        
        df_list_subset_mean_top_right_x[[ay]] <- data.frame(unlist(lapply(df_list_subset[[ay]], "[", "top_beam_right_x")))
        df_list_subset_mean_top_right_x[[ay]] <- df_list_subset_mean_top_right_x[[ay]][,1]
        df_list_subset_mean_top_right_x[[ay]] <- mean(df_list_subset_mean_top_right_x[[ay]], na.rm =TRUE)
        
        
        df_list_subset_mean_bottom_left_y[[ay]] <- data.frame(unlist(lapply(df_list_subset[[ay]], "[", "bottom_beam_left_y")))
        df_list_subset_mean_bottom_left_y[[ay]] <- df_list_subset_mean_bottom_left_y[[ay]][,1]
        df_list_subset_mean_bottom_left_y[[ay]] <- mean(df_list_subset_mean_bottom_left_y[[ay]], na.rm =TRUE)
        
        df_list_subset_mean_bottom_left_x[[ay]] <- data.frame(unlist(lapply(df_list_subset[[ay]], "[", "bottom_beam_left_x")))
        df_list_subset_mean_bottom_left_x[[ay]] <- df_list_subset_mean_bottom_left_x[[ay]][,1]
        df_list_subset_mean_bottom_left_x[[ay]] <- mean(df_list_subset_mean_bottom_left_x[[ay]], na.rm =TRUE)
        
        
        df_list_subset_mean_bottom_right_y[[ay]] <- data.frame(unlist(lapply(df_list_subset[[ay]], "[", "bottom_beam_right_y")))
        df_list_subset_mean_bottom_right_y[[ay]] <- df_list_subset_mean_bottom_right_y[[ay]][,1]
        df_list_subset_mean_bottom_right_y[[ay]] <- mean(df_list_subset_mean_bottom_right_y[[ay]], na.rm =TRUE)
        
        df_list_subset_mean_bottom_right_x[[ay]] <- data.frame(unlist(lapply(df_list_subset[[ay]], "[", "bottom_beam_right_x")))
        df_list_subset_mean_bottom_right_x[[ay]] <- df_list_subset_mean_bottom_right_x[[ay]][,1]
        df_list_subset_mean_bottom_right_x[[ay]] <- mean(df_list_subset_mean_bottom_right_x[[ay]], na.rm =TRUE)
        
        next #ay = ay +1
    }
    
    #Counting Frames over all data frames
    length_of_trial <- cbind(rep(0,times = length(df_list))) #The time (in frames) that body parts x-coordinates were between the two inner markings.
    frames_bbt <- cbind(rep(0,times = length(df_list))) #The time (in frames) that body parts y-coordinates were below the top of the beam.
    
    #For creating csv files
    #table_subset to exclude FPS and Resolution Columns
    table_subset <- subset(table, select = -c(which(colnames(table) == c("Frames per Second")), which(colnames(table) == c("Horizontal Resolution")), which(colnames(table) == c("Vertical Resolution"))))
    table_subset <- subset(table_subset, select = -columns_to_remove)
    
    attributes_list <- invisible(split(table_subset, seq(from = 1, to = nrow(table)))) #ncol(table[match(gsub(pattern = "_.*" ,"", gsub(pattern = "\\_\\(.*?\\d\\)", "", names(df_list[1]))), table[[mouse_number_column]]), ]). Invisible so it does not print the split into the command box.
    
    full_detection_list <- NULL #Will not be a list later on. Will be the data frame containing everything necessary for analysis.
    not_full_detection_list <- NULL  #Will not be a list later on. Will contain Taps and Slips.
    
    #Means to compare
    mean_list <- c()
    
    left_eye_mean <- c()
    snout_mean <- c()
    back_mean <- c()
    belly_mean <- c()
    tail_base_mean <- c()
    tail_center_mean <- c()
    
    tail_tip_mean <- c()
    
    mean_list_cm <- c()
    
    left_eye_mean_cm <- c()
    snout_mean_cm <- c()
    back_mean_cm <- c()
    belly_mean_cm <- c()
    tail_base_mean_cm <- c()
    tail_center_mean_cm <- c()
    tail_tip_mean_cm <- c()
    
    
    for(i in 1:length(df_list)) #Main for-loop
    {
        #a to h are in the minor and major slip detection
        #i is for the data_frame
        l <- 0L #Frames below the beam and reverse ratio
        n <- 0L #Points below the beam and trial length
        q <- 0L #Minor Slip points
        p <- 0L #Major Slip points
        #x and y are unused to avoid confusion
        
        
        #This is done to get the correct length for these values so that they are properly plotted
        if(length(df_list[[i]][,frame_c]) >= length(resolution[[2]]) && length(df_list[[i]][,frame_c]) >= length(resolution[[1]]))
        {
            
            mfl_longitudinal <- rep(mean(df_list[[i]][,mark_far_left_x], na.rm = TRUE), times = length(df_list[[i]][,frame_c])) #It is the vertical line, but the horizontal (longitudinal) position of the markings
            ml_longitudinal <- rep(mean(df_list[[i]][,mark_left_x], na.rm = TRUE), times = length(df_list[[i]][,frame_c]))
            mr_longitudinal <- rep(mean(df_list[[i]][,mark_right_x], na.rm = TRUE), times = length(df_list[[i]][,frame_c]))
            mfr_longitudinal <- rep(mean(df_list[[i]][,mark_far_right_x], na.rm = TRUE), times = length(df_list[[i]][,frame_c]))
            
            mfl_latitudinal <- rep(mean(df_list[[i]][,mark_far_left_y], na.rm = TRUE), times = length(df_list[[i]][,frame_c])) #It is the horizontal line, but the vertical (latitudinal) position (height) of the markings
            ml_latitudinal <- rep(mean(df_list[[i]][,mark_left_y], na.rm = TRUE), times = length(df_list[[i]][,frame_c]))
            mr_latitudinal <- rep(mean(df_list[[i]][,mark_right_y], na.rm = TRUE), times = length(df_list[[i]][,frame_c]))
            mfr_latitudinal <- rep(mean(df_list[[i]][,mark_far_right_y], na.rm = TRUE), times = length(df_list[[i]][,frame_c]))
            
        } else if(length(df_list[[i]][,frame_c]) >= length(resolution[[2]]))
        {
            mfl_longitudinal <- rep(mean(df_list[[i]][,mark_far_left_x], na.rm = TRUE), times = length(df_list[[i]][,frame_c])) #It is the vertical line, but the horizontal (longitudinal) position of the markings
            ml_longitudinal <- rep(mean(df_list[[i]][,mark_left_x], na.rm = TRUE), times = length(df_list[[i]][,frame_c]))
            mr_longitudinal <- rep(mean(df_list[[i]][,mark_right_x], na.rm = TRUE), times = length(df_list[[i]][,frame_c]))
            mfr_longitudinal <- rep(mean(df_list[[i]][,mark_far_right_x], na.rm = TRUE), times = length(df_list[[i]][,frame_c]))
            
            mfl_latitudinal <- rep(mean(df_list[[i]][,mark_far_left_y], na.rm = TRUE), times = length(resolution[[1]])) #It is the horizontal line, but the vertical (latitudinal) position (height) of the markings
            ml_latitudinal <- rep(mean(df_list[[i]][,mark_left_y], na.rm = TRUE), times = length(resolution[[1]]))
            mr_latitudinal <- rep(mean(df_list[[i]][,mark_right_y], na.rm = TRUE), times = length(resolution[[1]]))
            mfr_latitudinal <- rep(mean(df_list[[i]][,mark_far_right_y], na.rm = TRUE), times = length(resolution[[1]]))
            
        } else if(length(df_list[[i]][,frame_c]) >= length(resolution[[1]]))
        {
            
            mfl_longitudinal <- rep(mean(df_list[[i]][,mark_far_left_x], na.rm = TRUE), times = length(resolution[[2]])) #It is the vertical line, but the horizontal (longitudinal) position of the markings
            ml_longitudinal <- rep(mean(df_list[[i]][,mark_left_x], na.rm = TRUE), times = length(resolution[[2]]))
            mr_longitudinal <- rep(mean(df_list[[i]][,mark_right_x], na.rm = TRUE), times = length(resolution[[2]]))
            mfr_longitudinal <- rep(mean(df_list[[i]][,mark_far_right_x], na.rm = TRUE), times = length(resolution[[2]]))
               
            mfl_latitudinal <- rep(mean(df_list[[i]][,mark_far_left_y], na.rm = TRUE), times = length(df_list[[i]][,frame_c])) #It is the horizontal line, but the vertical (latitudinal) position (height) of the markings
            ml_latitudinal <- rep(mean(df_list[[i]][,mark_left_y], na.rm = TRUE), times = length(df_list[[i]][,frame_c]))
            mr_latitudinal <- rep(mean(df_list[[i]][,mark_right_y], na.rm = TRUE), times = length(df_list[[i]][,frame_c]))
            mfr_latitudinal <- rep(mean(df_list[[i]][,mark_far_right_y], na.rm = TRUE), times = length(df_list[[i]][,frame_c]))
            
        } else {
            mfl_longitudinal <- rep(mean(df_list[[i]][,mark_far_left_x], na.rm = TRUE), times = length(resolution[[2]])) #It is the vertical line, but the horizontal (longitudinal) position of the markings
            ml_longitudinal <- rep(mean(df_list[[i]][,mark_left_x], na.rm = TRUE), times = length(resolution[[2]]))
            mr_longitudinal <- rep(mean(df_list[[i]][,mark_right_x], na.rm = TRUE), times = length(resolution[[2]]))
            mfr_longitudinal <- rep(mean(df_list[[i]][,mark_far_right_x], na.rm = TRUE), times = length(resolution[[2]]))
            
            mfl_latitudinal <- rep(mean(df_list[[i]][,mark_far_left_y], na.rm = TRUE), times = length(resolution[[1]])) #It is the horizontal line, but the vertical (latitudinal) position (height) of the markings
            ml_latitudinal <- rep(mean(df_list[[i]][,mark_left_y], na.rm = TRUE), times = length(resolution[[1]]))
            mr_latitudinal <- rep(mean(df_list[[i]][,mark_right_y], na.rm = TRUE), times = length(resolution[[1]]))
            mfr_latitudinal <- rep(mean(df_list[[i]][,mark_far_right_y], na.rm = TRUE), times = length(resolution[[1]]))
        }
        
        top_bm_left_y <- mean(df_list[[i]][,top_beam_left_y], na.rm=TRUE)
        #top_bm_middle_y <- mean(df_list[[i]][,top_beam_middle_y], na.rm=TRUE)
        top_bm_right_y <- mean(df_list[[i]][,top_beam_right_y], na.rm=TRUE)
        bottom_bm_left_y <- mean(df_list[[i]][,bottom_beam_left_y], na.rm=TRUE)
        #bottom_bm_middle_y <- mean(df_list[[i]][,bottom_beam_middle_y], na.rm=TRUE)
        bottom_bm_right_y <- mean(df_list[[i]][,bottom_beam_right_y], na.rm=TRUE)
        
        top_bm_left_x <- mean(df_list[[i]][,top_beam_left_x], na.rm=TRUE)
        #top_bm_middle_x <- mean(df_list[[i]][,top_beam_middle_x], na.rm=TRUE)
        top_bm_right_x <- mean(df_list[[i]][,top_beam_right_x], na.rm=TRUE)
        bottom_bm_left_x <- mean(df_list[[i]][,bottom_beam_left_x], na.rm=TRUE)
        #bottom_bm_middle_x <- mean(df_list[[i]][,bottom_beam_middle_x], na.rm=TRUE)
        bottom_bm_right_x <- mean(df_list[[i]][,bottom_beam_right_x], na.rm=TRUE)
        
        if(all(is.na(top_bm_left_y)) || all(is.na(top_bm_right_y)) || all(is.na(top_bm_left_x)) || all(is.na(top_bm_right_x)))
        {
            slope_using_top_beam <- (df_list_subset_mean_top_left_y[[which(dates_of_recording == table$`Date of Recording`[i])]]-df_list_subset_mean_top_right_y[[which(dates_of_recording == table$`Date of Recording`[i])]])/(df_list_subset_mean_top_left_x[[which(dates_of_recording == table$`Date of Recording`[i])]]-df_list_subset_mean_top_right_x[[which(dates_of_recording == table$`Date of Recording`[i])]])
            
            print(paste0("The top edge of the beam seems not to be tracked correctly in dataframe ", paste0(gsub(pattern = "`|L", "", deparse(substitute(df_list[[i]], env = environment()))), ","), "which is", names(df_list[i]), ". Hence the mean beam position of all videos recorded on the same day (", dates_of_recording[which(dates_of_recording == table$`Date of Recording`[i])], ") is taken to calculate the beams position and slope. Please check that trial if the result is satisfactory. If not, use the exception_list variable when calling the function."))
        }else
        {
            slope_using_top_beam <- ((top_bm_left_y-top_bm_right_y)/(top_bm_left_x-top_bm_right_x))
        }
        
        slope_using_top_beam <- slope_using_top_beam*(-1)
        
        if(all(is.na(bottom_bm_left_y)) || all(is.na(bottom_bm_right_y)) || all(is.na(bottom_bm_left_x)) || all(is.na(bottom_bm_right_x)))
        {
            slope_using_bottom_beam <- (df_list_subset_mean_bottom_left_y[[which(dates_of_recording == table$`Date of Recording`[i])]]-df_list_subset_mean_bottom_right_y[[which(dates_of_recording == table$`Date of Recording`[i])]])/(df_list_subset_mean_bottom_left_x[[which(dates_of_recording == table$`Date of Recording`[i])]]-df_list_subset_mean_bottom_right_x[[which(dates_of_recording == table$`Date of Recording`[i])]])
            print(paste0("The bottom edge of the beam seems not to be tracked correctly in dataframe ", paste0(gsub(pattern = "`|L", "", deparse(substitute(df_list[[i]], env = environment()))), ","), "which is", names(df_list[i]), ". Hence the mean beam position of all videos recorded on the same day (", dates_of_recording[which(dates_of_recording == table$`Date of Recording`[i])], ") is taken to calculate the beams position and slope. Please check that trial if the result is satisfactory. If not, use the exception_list variable when calling the function."))
            
        }else
        {
            slope_using_bottom_beam <- ((bottom_bm_left_y-bottom_bm_right_y)/(bottom_bm_left_x-bottom_bm_right_x))
        }
        
        slope_using_bottom_beam <- slope_using_bottom_beam*(-1)
        
        if(all(is.na(top_bm_right_y)))
        {
            function_top_beam <- slope_using_top_beam*resolution[[1]]+df_list_subset_mean_top_right_y[[which(dates_of_recording == dates[i])]]
        }else
        {
            function_top_beam <- slope_using_top_beam*resolution[[1]]+mean(top_bm_right_y,na.rm = TRUE)
        }
        
        if(all(is.na(bottom_bm_right_y)))
        {
            function_bottom_beam <- slope_using_bottom_beam*resolution[[1]]+df_list_subset_mean_bottom_right_y[[which(dates_of_recording == dates[i])]]
        }else
        {
            function_bottom_beam <- slope_using_bottom_beam*resolution[[1]]+mean(bottom_bm_right_y,na.rm = TRUE)
        }
        
        function_top_beam <- rev(function_top_beam)
        function_bottom_beam <- rev(function_bottom_beam)
        
        new_beam_height <- abs(mean(mean(top_bm_right_y-bottom_bm_right_y,na.rm=TRUE, trim = 0),mean(top_bm_left_y-bottom_bm_left_y,na.rm=TRUE, trim = 0),na.rm=TRUE, trim = 0)) #New Beam height in pixel
        #Without adding trim = 0 this breaking message appears: Error in if (trim > 0 && n) { : missing value where TRUE/FALSE needed
        
        if(is.na(new_beam_height))
        {
            new_beam_height <- abs(mean(mean(df_list_subset_mean_top_right_y[[which(dates_of_recording == dates[i])]] - df_list_subset_mean_bottom_right_y[[which(dates_of_recording == dates[i])]]),mean(df_list_subset_mean_top_left_y[[which(dates_of_recording == dates[i])]]-df_list_subset_mean_bottom_left_y[[which(dates_of_recording == dates[i])]]),na.rm=TRUE)) #New Beam height in pixel
        }
        
        line_of_top_beam <- cbind(function_top_beam,resolution[[1]])
        line_of_bottom_beam <- cbind(function_bottom_beam,resolution[[1]])
        
        points_below_top_beam <- cbind(rep(NA, times = length(df_list[[i]][,frame_c])),rep(NA, times = length(df_list[[i]][,frame_c])))
        frames_below_top_beam <- 0L
        trial_length <- 0L
        
        minor_slip_points <- cbind(rep(NA, times = length(df_list[[i]][,frame_c])),rep(NA, times = length(df_list[[i]][,frame_c])))
        major_slip_points <- cbind(rep(NA, times = length(df_list[[i]][,frame_c])),rep(NA, times = length(df_list[[i]][,frame_c])))
        
        minor_slip_count <- 0L
        major_slip_count <- 0L
        
        iterations_to_skip_minor_slip <- 0L
        iterations_to_skip_major_slip <- 0L
        
        graph_number_minor <- 0L
        graph_number_major <- 0L
        
        approx_fun_minor <- NULL
        approx_fun_major <- NULL
        
        #There are also minima and maxima variables inside the slip detection for both the graph itself, as well as the approximated functions
        
        
        ###Calculations###
        
        #beam_height_ratio <- beam_height_cm/inner_marking_distance_cm #Will always be (way) smaller than 1.
        
        pixel_to_cm_scale <- new_beam_height/beam_height_cm #Scale for coverting pixel into cm and vice versa
        
        for(n in 1:length(df_list[[i]][,frame_c]))
        {
            #Check which points are below the beam
            if(any(df_list[[i]][n,body_part_y] >= function_top_beam) && !is.na(any(df_list[[i]][n,body_part_y] >= function_top_beam)))
            {
                points_below_top_beam[n,1] <- df_list[[i]][n,body_part_y]
                points_below_top_beam[n,2] <- df_list[[i]][n,body_part_x]
                
            } else {
                points_below_top_beam[n,1] <- NA
                points_below_top_beam[n,2] <- NA
            }
            
            #Check which frames are between the two inner markings
            if(isTRUE(df_list[[i]][n,body_part_x] >= ml_longitudinal[n] && df_list[[i]][n,body_part_x] <= mr_longitudinal[n]) && !is.na(df_list[[i]][n,body_part_x]) && isTRUE(isTRUE(!is.na(ml_longitudinal[n]) && !is.na(ml_longitudinal[n])) || isTRUE(ml_longitudinal[n] != 0 && mr_longitudinal[n] != 0)))
            {
                trial_length <- trial_length + 1
            }
            
            next #n = n + 1
        }
        
        #For some weird and not really explainable reason occurs a change in list length in several reverse ratios seemingly at random (like the example case below). The following lines are written to accommodate for that.
        #The length of reverse_ratio_tt changes WITHOUT any reason (values are fine and stuff) at an i of 117 (or at the end of an i of 116) from 131 (same length like all others) to 124... if I run it from an i of 120 it will run just fine with a length of 131. There is not even a single command that shortens the length of these variables... just WTF... 
        
        if(length(df_list) != length(reverse_ratio) && ((i-1) == length(reverse_ratio) || i == length(df_list))){reverse_ratio[[i]] <- list()}
        if(length(df_list) != length(reverse_ratio_blp) && ((i-1) == length(reverse_ratio_blp) || i == length(df_list))){reverse_ratio_blp[[i]] <- list()}
        if(length(df_list) != length(reverse_ratio_flp) && ((i-1) == length(reverse_ratio_flp) || i == length(df_list))){reverse_ratio_flp[[i]] <- list()}
        if(length(df_list) != length(reverse_ratio_bel) && ((i-1) == length(reverse_ratio_bel) || i == length(df_list))){reverse_ratio_bel[[i]] <- list()}
        if(length(df_list) != length(reverse_ratio_bac) && ((i-1) == length(reverse_ratio_bac) || i == length(df_list))){reverse_ratio_bac[[i]] <- list()}
        if(length(df_list) != length(reverse_ratio_snt) && ((i-1) == length(reverse_ratio_snt) || i == length(df_list))){reverse_ratio_snt[[i]] <- list()}
        if(length(df_list) != length(reverse_ratio_le) && ((i-1) == length(reverse_ratio_le) || i == length(df_list))){reverse_ratio_le[[i]] <- list()}
        if(length(df_list) != length(reverse_ratio_tb) && ((i-1) == length(reverse_ratio_tb) || i == length(df_list))){reverse_ratio_tb[[i]] <- list()}
        if(length(df_list) != length(reverse_ratio_tc) && ((i-1) == length(reverse_ratio_tc) || i == length(df_list))){reverse_ratio_tc[[i]] <- list()}
        if(length(df_list) != length(reverse_ratio_tt) && ((i-1) == length(reverse_ratio_tt) || i == length(df_list))){reverse_ratio_tt[[i]] <- list()}
        
        if(length(df_list) != length(instantaneous_velocity_belly) && ((i-1) == length(instantaneous_velocity_belly) || i == length(df_list))){instantaneous_velocity_belly[[i]] <- list()}
        if(length(df_list) != length(stationary_belly) && ((i-1) == length(stationary_belly) || i == length(df_list))){stationary_belly[[i]] <- list()}
        if(length(df_list) != length(instantaneous_velocity_back) && ((i-1) == length(instantaneous_velocity_back) || i == length(df_list))){instantaneous_velocity_back[[i]] <- list()}
        if(length(df_list) != length(stationary_back) && ((i-1) == length(stationary_back) || i == length(df_list))){stationary_back[[i]] <- list()}
        if(length(df_list) != length(instantaneous_velocity_tail_base) && ((i-1) == length(instantaneous_velocity_tail_base) || i == length(df_list))){instantaneous_velocity_tail_base[[i]] <- list()}
        if(length(df_list) != length(stationary_tail_base) && ((i-1) == length(stationary_tail_base) || i == length(df_list))){stationary_tail_base[[i]] <- list()}
        
        if(length(df_list) != length(stationary) && ((i-1) == length(stationary) || i == length(df_list))){stationary[[i]] <- list()}
        
        if(length(df_list) != length(snt_bel_dist) && ((i-1) == length(snt_bel_dist) || i == length(df_list))){snt_bel_dist[[i]] <- list()}
        if(length(df_list) != length(bel_tb_dist) && ((i-1) == length(bel_tb_dist) || i == length(df_list))){bel_tb_dist[[i]] <- list()}
        if(length(df_list) != length(tb_bac_dist) && ((i-1) == length(tb_bac_dist) || i == length(df_list))){tb_bac_dist[[i]] <- list()}
        if(length(df_list) != length(bac_snt_dist) && ((i-1) == length(bac_snt_dist) || i == length(df_list))){bac_snt_dist[[i]] <- list()}
        
        if(length(df_list) != length(snt_tb_dist) && ((i-1) == length(snt_tb_dist) || i == length(df_list))){snt_tb_dist[[i]] <- list()}
        if(length(df_list) != length(bel_bac_dist) && ((i-1) == length(bel_bac_dist) || i == length(df_list))){bel_bac_dist[[i]] <- list()}
        
        if(length(df_list) != length(tb_tt_dist) && ((i-1) == length(tb_tt_dist) || i == length(df_list))){tb_tt_dist[[i]] <- list()}
        if(length(df_list) != length(snt_tt_dist) && ((i-1) == length(snt_tt_dist) || i == length(df_list))){snt_tt_dist[[i]] <- list()}
        
        if(length(df_list) != length(tb_to_tt_angle) && ((i-1) == length(tb_to_tt_angle) || i == length(df_list))){tb_to_tt_angle[[i]] <- list()}
        
        if(length(df_list) != length(tb_tc_dist) && ((i-1) == length(tb_tc_dist) || i == length(df_list))){tb_tc_dist[[i]] <- list()}
        if(length(df_list) != length(snt_tc_dist) && ((i-1) == length(snt_tc_dist) || i == length(df_list))){snt_tc_dist[[i]] <- list()}
        if(length(df_list) != length(tb_to_tc_angle) && ((i-1) == length(tb_to_tc_angle) || i == length(df_list))){tb_to_tc_angle[[i]] <- list()}
        
        
        #Slip separation into major and minor slips (To get local minima from points below the top of the beam)

        for(l in 1:length(points_below_top_beam[,2]))
        {
                
            #Separation of points into minor slips and major slips
            
            if(isTRUE(is.na(points_below_top_beam[l,1]) || is.na(points_below_top_beam[l,2])) || isTRUE(is.na(ml_longitudinal[l]) && is.na(mr_longitudinal[l])) || isTRUE(ml_longitudinal[l] == 0 && mr_longitudinal[l] == 0))
            {
                
                #These NAs are added so that the reverse ratio has the same length as the recorded frames
                reverse_ratio[[i]][l] <- NA
                
                reverse_ratio_blp[[i]][l] <- NA
                reverse_ratio_flp[[i]][l] <- NA
                reverse_ratio_bel[[i]][l] <- NA
                reverse_ratio_bac[[i]][l] <- NA
                reverse_ratio_snt[[i]][l] <- NA
                reverse_ratio_le[[i]][l]  <- NA
                reverse_ratio_tb[[i]][l]  <- NA
                reverse_ratio_tc[[i]][l]  <- NA
                reverse_ratio_tb[[i]][l]  <- NA
                
                where_is_this_point_analysed[[i]][l] <- "inaccurate tracking"
                
                next
                
            } else if(points_below_top_beam[l,2] >= ml_longitudinal[l] && points_below_top_beam[l,2] <= mr_longitudinal[l])
            {
                #Point that is between the two inner markings
                
                if(is.na(function_top_beam[match(round(points_below_top_beam[l,2]), line_of_top_beam[,2])]))
                {
                    function_top_beam[match(round(points_below_top_beam[l,2]), function_top_beam[l])] <- function_top_beam[match(round(points_below_top_beam[l-1,2]), line_of_top_beam[,2])]
                }
                
                try(reverse_ratio[[i]][l] <- ((100/mean(new_beam_height, na.rm = TRUE)) * (mean(new_beam_height, na.rm = TRUE) + (function_top_beam[match(round(df_list[[i]][l,body_part_x]), line_of_top_beam[,2])] - df_list[[i]][match(which(line_of_top_beam[,2] == round(df_list[[i]][l,body_part_x])), round(df_list[[i]][,body_part_x])),body_part_y]))) - 100, silent = TRUE)
                
                try(reverse_ratio_blp[[i]][l] <- ((100/mean(new_beam_height, na.rm = TRUE)) * (mean(new_beam_height, na.rm = TRUE) + (function_top_beam[match(round(df_list[[i]][l,back_left_paw_x]), line_of_top_beam[,2])] - df_list[[i]][match(which(line_of_top_beam[,2] == round(df_list[[i]][l,back_left_paw_x])), round(df_list[[i]][,back_left_paw_x])),back_left_paw_y]))) - 100, silent = TRUE)
                try(reverse_ratio_flp[[i]][l] <- ((100/mean(new_beam_height, na.rm = TRUE)) * (mean(new_beam_height, na.rm = TRUE) + (function_top_beam[match(round(df_list[[i]][l,front_left_paw_x]), line_of_top_beam[,2])] - df_list[[i]][match(which(line_of_top_beam[,2] == round(df_list[[i]][l,front_left_paw_x])), round(df_list[[i]][,front_left_paw_x])),front_left_paw_y]))) - 100, silent = TRUE)
                try(reverse_ratio_bel[[i]][l] <- ((100/mean(new_beam_height, na.rm = TRUE)) * (mean(new_beam_height, na.rm = TRUE) + (function_top_beam[match(round(df_list[[i]][l,belly_x]), line_of_top_beam[,2])] - df_list[[i]][match(which(line_of_top_beam[,2] == round(df_list[[i]][l,belly_x])), round(df_list[[i]][,belly_x])),belly_y]))) - 100, silent = TRUE)
                try(reverse_ratio_bac[[i]][l] <- ((100/mean(new_beam_height, na.rm = TRUE)) * (mean(new_beam_height, na.rm = TRUE) + (function_top_beam[match(round(df_list[[i]][l,back_x]), line_of_top_beam[,2])] - df_list[[i]][match(which(line_of_top_beam[,2] == round(df_list[[i]][l,back_x])), round(df_list[[i]][,back_x])),back_y]))) - 100, silent = TRUE)
                try(reverse_ratio_snt[[i]][l] <- ((100/mean(new_beam_height, na.rm = TRUE)) * (mean(new_beam_height, na.rm = TRUE) + (function_top_beam[match(round(df_list[[i]][l,snout_x]), line_of_top_beam[,2])] - df_list[[i]][match(which(line_of_top_beam[,2] == round(df_list[[i]][l,snout_x])), round(df_list[[i]][,snout_x])),snout_y]))) - 100, silent = TRUE)
                try(reverse_ratio_le[[i]][l]  <- ((100/mean(new_beam_height, na.rm = TRUE)) * (mean(new_beam_height, na.rm = TRUE) + (function_top_beam[match(round(df_list[[i]][l,left_eye_x]), line_of_top_beam[,2])] - df_list[[i]][match(which(line_of_top_beam[,2] == round(df_list[[i]][l,left_eye_x])), round(df_list[[i]][,left_eye_x])),left_eye_y]))) - 100, silent = TRUE)
                try(reverse_ratio_tb[[i]][l]  <- ((100/mean(new_beam_height, na.rm = TRUE)) * (mean(new_beam_height, na.rm = TRUE) + (function_top_beam[match(round(df_list[[i]][l,tail_base_x]), line_of_top_beam[,2])] - df_list[[i]][match(which(line_of_top_beam[,2] == round(df_list[[i]][l,tail_base_x])), round(df_list[[i]][,tail_base_x])),tail_base_y]))) - 100, silent = TRUE)
                try(reverse_ratio_tc[[i]][l]  <- ((100/mean(new_beam_height, na.rm = TRUE)) * (mean(new_beam_height, na.rm = TRUE) + (function_top_beam[match(round(df_list[[i]][l,tail_center_x]), line_of_top_beam[,2])] - df_list[[i]][match(which(line_of_top_beam[,2] == round(df_list[[i]][l,tail_center_x])), round(df_list[[i]][,tail_center_x])),tail_center_y]))) - 100, silent = TRUE)
                try(reverse_ratio_tt[[i]][l]  <- ((100/mean(new_beam_height, na.rm = TRUE)) * (mean(new_beam_height, na.rm = TRUE) + (function_top_beam[match(round(df_list[[i]][l,tail_tip_x]), line_of_top_beam[,2])] - df_list[[i]][match(which(line_of_top_beam[,2] == round(df_list[[i]][l,tail_tip_x])), round(df_list[[i]][,tail_tip_x])),tail_tip_y]))) - 100, silent = TRUE)
                
                if(points_below_top_beam[l,1] >= function_top_beam[match(round(points_below_top_beam[l,2]), line_of_top_beam[,2])] && points_below_top_beam[l,1] <= function_top_beam[match(round(points_below_top_beam[l,2]), line_of_top_beam[,2])]*minor_slip_threshold)
                {
                    #This is basically done as a buffer, if the beam or body part is not accurately tracked.
                    
                    minor_slip_points[l,1] <- NA
                    minor_slip_points[l,2] <- NA
                    major_slip_points[l,1] <- NA
                    major_slip_points[l,2] <- NA
                    
                    #where_is_this_point_analysed[[i]][l] <- "between no slip and minor slip" #Which is essentially the same as no slip in our case
                    where_is_this_point_analysed[[i]][l] <- "no slip"
                    
                    #frames_below_top_beam = frames_below_top_beam + 1
                    
                } else if(isTRUE(points_below_top_beam[l,1] >= function_top_beam[match(round(points_below_top_beam[l,2]), line_of_top_beam[,2])])*minor_slip_threshold && points_below_top_beam[l,1] <= function_top_beam[match(round(points_below_top_beam[l,2]), line_of_top_beam[,2])]*major_slip_threshold)
                {
                    minor_slip_points[l,1] <- points_below_top_beam[l,1]
                    minor_slip_points[l,2] <- points_below_top_beam[l,2]
                    major_slip_points[l,1] <- NA
                    major_slip_points[l,2] <- NA
                    
                    frames_below_top_beam = frames_below_top_beam + 1
                    
                    where_is_this_point_analysed[[i]][l] <- "minor"
                    
                } else if(points_below_top_beam[l,1] >= function_top_beam[match(round(points_below_top_beam[l,2]), line_of_top_beam[,2])]*major_slip_threshold)
                {
                    minor_slip_points[l,1] <- NA
                    minor_slip_points[l,2] <- NA
                    
                    major_slip_points[l,1] <- points_below_top_beam[l,1]
                    major_slip_points[l,2] <- points_below_top_beam[l,2]
                    
                    frames_below_top_beam = frames_below_top_beam + 1
                    
                    where_is_this_point_analysed[[i]][l] <- "major"
                    
                } else {
                    
                    minor_slip_points[l,1] <- NA
                    minor_slip_points[l,2] <- NA
                    major_slip_points[l,1] <- NA
                    major_slip_points[l,2] <- NA
                    
                    where_is_this_point_analysed[[i]][l] <- "no slip"
                }
                
            } else {
                
                #Point that is not between the two inner markings
                
                minor_slip_points[l,1] <- NA
                minor_slip_points[l,2] <- NA
                major_slip_points[l,1] <- NA
                major_slip_points[l,2] <- NA
                
                #These NAs are added so that the reverse ratio has the same length as the recorded frames
                reverse_ratio[[i]][l] <- NA
                
                reverse_ratio_blp[[i]][l] <- NA
                reverse_ratio_flp[[i]][l] <- NA
                reverse_ratio_bel[[i]][l] <- NA
                reverse_ratio_bac[[i]][l] <- NA
                reverse_ratio_snt[[i]][l] <- NA
                reverse_ratio_le[[i]][l]  <- NA
                reverse_ratio_tb[[i]][l]  <- NA
                reverse_ratio_tc[[i]][l]  <- NA
                reverse_ratio_tb[[i]][l]  <- NA
                
                where_is_this_point_analysed[[i]][l] <- "point not between trial markings"
                next #l = l + 1
            }
        }
        
        
        for(an in 1:length(where_is_this_point_analysed[[i]]))
        {
            if(an == 1)
            {
                #init
                name_of_label <- where_is_this_point_analysed[[i]][an]
                duration_of_label <- 1L
                segment_counter <- 1L
                
                start_frame_index <- 1L
                end_frame_index <- 1L
            }
             
            if(identical(where_is_this_point_analysed[[i]][an+1],  where_is_this_point_analysed[[i]][an]))
            {
                #count
                duration_of_label <- duration_of_label + 1
                end_frame_index <- end_frame_index + 1
                
            } else 
            {
                #assign
                how_long_is_this_segment[[i]][segment_counter] <- duration_of_label
                what_segment_is_it[[i]][segment_counter] <- name_of_label
                start_frame[[i]][segment_counter] <- start_frame_index
                end_frame[[i]][segment_counter] <- end_frame_index
                
                #reinit
                name_of_label <- where_is_this_point_analysed[[i]][an+1]
                duration_of_label <- 1L
                start_frame_index <- end_frame_index + 1
                end_frame_index <- start_frame_index
                segment_counter <- segment_counter + 1
            }
            
            next #an = an + 1
        }
        
        segment_duration_dataframes[[i]] <- cbind(unlist(what_segment_is_it[[i]]), as.numeric(unlist(how_long_is_this_segment[[i]])), as.numeric(unlist(start_frame[[i]])), as.numeric(unlist(end_frame[[i]])))
        segment_duration_dataframes[[i]] <- data.frame(segment_duration_dataframes[[i]])
        colnames(segment_duration_dataframes[[i]]) <- c("Segment Name", "Duration", "Starting at Frame", "Ending at Frame")
        
        #Velocity calculation and check if stationary and angle calculation
        for(ax in 2:length(df_list[[i]][,frame_c]))
        {
            if(ax == 2)
            {
                stationary_belly[[i]][ax-1] <- NaN
                stationary_back[[i]][ax-1] <- NaN
                stationary_tail_base[[i]][ax-1] <- NaN
                
                stationary[[i]][ax-1] <- NaN
            }
            
            
            #Since the distance between two points is equal to the hypothenuse of the distances on both axes and the time scale is one frame, the instantaneous velocity is that hypothenuse distance in pixels per frame.
            
            instantaneous_velocity_belly[[i]][ax] <- sqrt(((df_list[[i]][ax,belly_x]-df_list[[i]][ax-1,belly_x])^2)+((df_list[[i]][ax,belly_y]-df_list[[i]][ax-1,belly_y])^2))
            instantaneous_velocity_back[[i]][ax] <- sqrt(((df_list[[i]][ax,back_x]-df_list[[i]][ax-1,back_x])^2)+((df_list[[i]][ax,back_y]-df_list[[i]][ax-1,back_y])^2))
            instantaneous_velocity_tail_base[[i]][ax] <- sqrt(((df_list[[i]][ax,tail_base_x]-df_list[[i]][ax-1,tail_base_x])^2)+((df_list[[i]][ax,tail_base_y]-df_list[[i]][ax-1,tail_base_y])^2))
            
            
            #Belly
            if(!is.na(instantaneous_velocity_belly[[i]][ax]) && (instantaneous_velocity_belly[[i]][ax] > 1.5 || instantaneous_velocity_belly[[i]][ax] < -1.5)) #Yes the values can't be negative i know
            {
                stationary_belly[[i]][ax] <- FALSE
                
            } else if(is.na(instantaneous_velocity_belly[[i]][ax]))
            {
                stationary_belly[[i]][ax] <- NaN #NULL does not work due to the "replacement has a length of zero" error
                
            } else 
            {
                stationary_belly[[i]][ax] <- TRUE
            }
            
            #Back
            if(!is.na(instantaneous_velocity_back[[i]][ax]) && (instantaneous_velocity_back[[i]][ax] > 1.5 || instantaneous_velocity_back[[i]][ax] < -1.5)) #Yes the values can't be negative i know
            {
                stationary_back[[i]][ax] <- FALSE
                
            } else if(is.na(instantaneous_velocity_back[[i]][ax]))
            {
                stationary_back[[i]][ax] <- NaN #NULL
                
            } else 
            {
                stationary_back[[i]][ax] <- TRUE
            }
            
            #Tail base
            if(!is.na(instantaneous_velocity_tail_base[[i]][ax]) && (instantaneous_velocity_tail_base[[i]][ax] > 1.5 || instantaneous_velocity_tail_base[[i]][ax] < -1.5)) #Yes the values can't be negative i know
            {
                stationary_tail_base[[i]][ax] <- FALSE
                
            } else if(is.na(instantaneous_velocity_tail_base[[i]][ax]))
            {
                stationary_tail_base[[i]][ax] <- NaN #NULL
                
            } else 
            {
                stationary_tail_base[[i]][ax] <- TRUE
            }
            
            
            #Is stationary? For some weird reason isTRUE() does not work and will always be FALSE. (Maybe because the values will be displayed as 0 and 1? But then again, == TRUE and == 1 work abd will deliver TRUEs.)
            if(is.nan(stationary_belly[[i]][ax]) || is.nan(stationary_back[[i]][ax]) || is.nan(stationary_tail_base[[i]][ax]))
            {
                stationary[[i]][ax] <- NaN
                
            } else if(stationary_belly[[i]][ax] == TRUE && stationary_back[[i]][ax] == TRUE && stationary_tail_base[[i]][ax] == TRUE)
            {
                stationary[[i]][ax] <- TRUE
                
                #Maybe remove here counts from frames counted as trial length and/or being below the beam? If prompted of course and not automatically.
                
            } else 
            {
                stationary[[i]][ax] <- FALSE
            }
            
            #Body size (getting a, b, c, d and e, f to calculate the area
            snt_bel_dist[[i]][ax] <- sqrt((df_list[[i]][ax,snout_x]-df_list[[i]][ax,belly_x])^2+(df_list[[i]][ax,snout_y]-df_list[[i]][ax,belly_y])^2)
            bel_tb_dist[[i]][ax] <- sqrt((df_list[[i]][ax,belly_x]-df_list[[i]][ax,tail_base_x])^2+(df_list[[i]][ax,belly_y]-df_list[[i]][ax,tail_base_y])^2)
            tb_bac_dist[[i]][ax] <- sqrt((df_list[[i]][ax,tail_base_x]-df_list[[i]][ax,back_x])^2+(df_list[[i]][ax,tail_base_y]-df_list[[i]][ax,back_y])^2)
            bac_snt_dist[[i]][ax] <- sqrt((df_list[[i]][ax,back_x]-df_list[[i]][ax,snout_x])^2+(df_list[[i]][ax,back_y]-df_list[[i]][ax,snout_y])^2)
            
            snt_tb_dist[[i]][ax] <- sqrt((df_list[[i]][ax,snout_x]-df_list[[i]][ax,tail_base_x])^2+(df_list[[i]][ax,snout_y]-df_list[[i]][ax,tail_base_y])^2)
            bel_bac_dist[[i]][ax] <- sqrt((df_list[[i]][ax,belly_x]-df_list[[i]][ax,back_x])^2+(df_list[[i]][ax,belly_y]-df_list[[i]][ax,back_y])^2)

            #Tail angle
            tb_tt_dist[[i]][ax] <- sqrt((df_list[[i]][ax,tail_base_x]-df_list[[i]][ax,tail_tip_x])^2+(df_list[[i]][ax,tail_base_y]-df_list[[i]][ax,tail_tip_y])^2) #b
            
            snt_tt_dist[[i]][ax] <- sqrt((df_list[[i]][ax,snout_x]-df_list[[i]][ax,tail_tip_x])^2+(df_list[[i]][ax,snout_y]-df_list[[i]][ax,tail_tip_y])^2) #c
            
            #Angle Calculation based on the following formula
            #tb_to_tt_angle = acos((a^2+b^2-c^2)/(2*a*b))
            #NaNs could be produced here due to dividing by 0. The term that is divided by 0 will become Inf and the acos(Inf) is NaN. The term divided by can become 0 due to e.g. the tail tip beign exactly at the location of the tail base, which would make the distance 0.
            
            if(!is.na(snt_tb_dist[[i]][ax]) && !is.na(tb_tt_dist[[i]][ax]) && (2*snt_tb_dist[[i]][ax]*tb_tt_dist[[i]][ax]) == 0)
            {
                tb_to_tt_angle[[i]][ax] <- NA
            } else {
                tb_to_tt_angle[[i]][ax] <- acos(((snt_tb_dist[[i]][ax]^2)+(tb_tt_dist[[i]][ax]^2)-(snt_tt_dist[[i]][ax]^2))/(2*snt_tb_dist[[i]][ax]*tb_tt_dist[[i]][ax])) #Geez, this is given in radiant and I was wondering why it is so messed up...
            }

            tail_center_lower <- NULL
            
            if(is.na(df_list[[i]][ax,tail_center_y]) || is.na(df_list[[i]][ax,tail_tip_y]))
            {
                #to avoid missing argument where TRUE/FALSE is needed
                tail_center_lower <- FALSE
                
            } else if(df_list[[i]][ax,tail_tip_y] <= df_list[[i]][ax,tail_center_y])
            {
                #tail center is lower than tail tip
                tail_center_lower <- TRUE
                
                tb_tc_dist[[i]][ax]     <- sqrt((df_list[[i]][ax,tail_base_x]-df_list[[i]][ax,tail_center_x])^2+(df_list[[i]][ax,tail_base_y]-df_list[[i]][ax,tail_center_y])^2) #b
                snt_tc_dist[[i]][ax]    <- sqrt((df_list[[i]][ax,snout_x]-df_list[[i]][ax,tail_center_x])^2+(df_list[[i]][ax,snout_y]-df_list[[i]][ax,tail_center_y])^2) #c
                
                #NaNs could be produced here due to dividing by 0. The term that is divided by 0 will become Inf and the acos(Inf) is NaN. The term divided by can become 0 due to e.g. the tail tip beign exactly at the location of the tail base, which would make the distance 0.
                
                if(!is.na(snt_tc_dist[[i]][ax]) && !is.na(tb_tc_dist[[i]][ax]) && (2*snt_tc_dist[[i]][ax]*tb_tc_dist[[i]][ax]) == 0)
                {
                    tb_to_tc_angle[[i]][ax] <- NA
                } else {
                    tb_to_tc_angle[[i]][ax] <- acos(((snt_tc_dist[[i]][ax]^2)+(tb_tc_dist[[i]][ax]^2)-(snt_tc_dist[[i]][ax]^2))/(2*snt_tc_dist[[i]][ax]*tb_tc_dist[[i]][ax]))
                }

            } else {
                
                tail_center_lower <- FALSE
            }
            
            
            if(is.na(tb_to_tt_angle[[i]][ax]))
            {
                
                #skip
                
            } else if(df_list[[i]][ax,tail_tip_y] <= df_list[[i]][ax,tail_base_y] && !isTRUE(tail_center_lower))
            {
                #tail tip is above tail base
                tb_to_tt_angle[[i]][ax] <- tb_to_tt_angle[[i]][ax] * (180/pi) #To convert the ffin' radiants into degree, because why not have the option in the acos() function or have a rad2deg() function in base R
                
            } else if(isTRUE(tail_center_lower)){
                
                #No need to check for NAs since the isTRUE and is.na before have ruled out all missing coordinates
                
                #Take tail center angle instead, because it is lower
                if(df_list[[i]][ax,tail_center_y] <= df_list[[i]][ax,tail_base_y])
                {
                    #tail center is above the tail base
                    tb_to_tt_angle[[i]][ax] <- tb_to_tc_angle[[i]][ax] * (180/pi) 
                    
                } else {
                    
                    #tail center is below the tail base
                    tb_to_tt_angle[[i]][ax] <- 180 - (180 - (tb_to_tc_angle[[i]][ax] * (180/pi))) + 180 #The 180 - (180 - (tb_to_tc_angle[[i]][ax] * (180/pi))) term gives me the angle outside the calculated triangle which is then added to the full 180°
                }

            } else {
                
                #tail tip is below tail base
                tb_to_tt_angle[[i]][ax] <- 180 - (180 - (tb_to_tt_angle[[i]][ax] * (180/pi))) + 180 #To get angles that are greater than 180 degrees
            }
        }
        
        #Area calculation based on the following formula
        #area_of_mouse = 0.25 * sqrt((4*(e^2)*(f^2))-(((b^2)+(d^2)-(a^2)-(c^2))^2))
        area_of_mouse[i] <- 0.25 * sqrt((4*(mean(snt_tb_dist[[i]], na.rm = TRUE)^2)*(mean(bel_bac_dist[[i]], na.rm = TRUE)^2))-(((mean(bel_tb_dist[[i]], na.rm = TRUE)^2)+(mean(bac_snt_dist[[i]], na.rm = TRUE)^2)-(mean(snt_bel_dist[[i]], na.rm = TRUE)^2)-(mean(tb_bac_dist[[i]], na.rm = TRUE)^2))^2)) #please note that this will be square pixel
        area_of_mouse_cm[i] <- as.numeric(area_of_mouse[i])/(pixel_to_cm_scale^2) #please note that this will be square cm. The scale also needs to be squared.
        
        area_of_mouse_table[[i]] <- data.frame(cbind(names(df_list)[i], as.numeric(area_of_mouse[i]),as.numeric(area_of_mouse_cm[i]),pixel_to_cm_scale, mean(snt_bel_dist[[i]], na.rm = TRUE), mean(bel_tb_dist[[i]], na.rm = TRUE), mean(tb_bac_dist[[i]], na.rm = TRUE), mean(bac_snt_dist[[i]], na.rm = TRUE), mean(snt_tb_dist[[i]], na.rm = TRUE), mean(bel_bac_dist[[i]], na.rm = TRUE)))
        names(area_of_mouse_table)[i] <- names(df_list)[i]
        colnames(area_of_mouse_table[[i]]) <- c("Trial", "Area of Mouse in square Pixels", "Area of Mouse in square Centimeters", "Pixel to Centimeter Scale", "a (snt_bel_dist)", "b (bel_tb_dist)", "c (tb_bac_dist)", "d (bac_snt_dist)", "e (snt_tb_dist)", "f (bel_bac_dist)")
        

        first <- TRUE #Needed for minor slip separation
        wpi_c <- 1L #For coordinates and frame number of slip points, what point is it counter
        
        #Counting minor slips
        for(q in 2:length(minor_slip_points[,2]))
        {
            
            if(is.na(iterations_to_skip_minor_slip))
            {
                iterations_to_skip_minor_slip <- 0L
            }
            
            
            if(iterations_to_skip_minor_slip != 0  && !is.na(iterations_to_skip_minor_slip))
            {
                #Simply to skip iterations due to not being able to modify the for-loop variable directly
                
                iterations_to_skip_minor_slip = iterations_to_skip_minor_slip - 1
                #print(paste("skipped",q,"with value", minor_slip_points[q,1], "and", iterations_to_skip_minor_slip, "skips left"))
                next #q = q + 1
                
            }else if(is.na(minor_slip_points[q,1]) && is.na(minor_slip_points[q,2])) 
            {
                #print(paste("is.na", minor_slip_points[q,1], minor_slip_points[q,2], "with q", q))
                next #q = q + 1
                
            } else if(is.na(minor_slip_points[q-1, 1]) && !is.na(minor_slip_points[q, 1]) && !is.na(minor_slip_points[q, 2]) && !is.na(match(NA, minor_slip_points[q:length(minor_slip_points[,1]),1])-2))
            {
                a <- 0L
                b <- 0L
                c <- 0L
                d <- 0L
                
                if((isTRUE(all(diff(minor_slip_points[q:(q+match(NA, minor_slip_points[q:length(minor_slip_points[,1]),1])-2)]) < 0)) || isTRUE(all(diff(minor_slip_points[q:(q+match(NA, minor_slip_points[q:length(minor_slip_points[,1]),1])-2)]) > 0))) && length(diff(minor_slip_points[q:(q+match(NA, minor_slip_points[q:length(minor_slip_points[,1]),1])-2)])) > 2)
                {
                    iterations_to_skip_minor_slip <- match(NA, minor_slip_points[q:length(minor_slip_points[,1]),1]) - 2
                    next
                }
                
                #To check if after the minor slip points follows a major slip so that it would be counted as a major slip instead
                if(!is.na(major_slip_points[(q+match(NA, minor_slip_points[q:length(minor_slip_points[,1]),1])-1),2]))
                {
                    if(first == TRUE)
                    {
                        #If it is the first time the code runs through here it is better not to put the minor_slip_count in the negative
                    } else {
                        
                        if(isTRUE(any((major_slip_points[(q+match(NA, minor_slip_points[q:length(minor_slip_points[,1]),1])-1):(q+match(NA, major_slip_points[q:length(major_slip_points[,1]),1])-2),2]) - 10 < ml_longitudinal[q])))
                        {
                            #If the trial ends in the middle of a major slip it will erease any one minor slip that has happend during the trial
                            #print(paste("edge case in dataframe", names(df_list)[i]))
                            
                            #something may not be right here, might take a look at this after publication
                            
                        } else {
                            
                            if(length(major_slip_points[(q+match(NA, minor_slip_points[q:length(minor_slip_points[,1]),1])-1):(q+match(NA, major_slip_points[q:length(major_slip_points[,1]),1])-2),2]) < 3)
                            {
                                #If only 2 points follow after the dip it will not be considered as a major slip, so you rather do not subtract one
                                
                            } else if(minor_slip_count > 0) {
                                
                                minor_slip_count <- minor_slip_count - 1
                                
                            } else {
                                #No subtraction
                            }
                        }
                    }
                }
                
                first <- FALSE
                
                #Initial check with strict check
                
                if(!is.na(match(NA, minor_slip_points[q:length(minor_slip_points[,1]),1]))){
                    minima_minor <- cbind(rep(NA, times = length(valleys(minor_slip_points[q:(q+match(NA, minor_slip_points[q:length(minor_slip_points[,1]),1])-2)], span = 7, strict = FALSE, na.rm=TRUE))), rep(NA, times = length(valleys(minor_slip_points[q:(q+match(NA, minor_slip_points[q:length(minor_slip_points[,1]),1])-2)], span = 7, strict = FALSE, na.rm=TRUE))))
                    maxima_minor <- cbind(rep(NA, times = length(peaks(minor_slip_points[q:(q+match(NA, minor_slip_points[q:length(minor_slip_points[,1]),1])-2)], span = 7, strict = FALSE, na.rm=TRUE))), rep(NA, times = length(peaks(minor_slip_points[q:(q+match(NA, minor_slip_points[q:length(minor_slip_points[,1]),1])-2)], span = 7, strict = FALSE, na.rm=TRUE))))
                    
                    #Assigning y-values
                    minima_minor[,1] <- valleys(minor_slip_points[q:(q+match(NA, minor_slip_points[q:length(minor_slip_points[,1]),1])-2)], span = 7, strict = FALSE, na.rm=TRUE)
                    
                    #Assigning y-values
                    maxima_minor[,1] <- peaks(minor_slip_points[q:(q+match(NA, minor_slip_points[q:length(minor_slip_points[,1]),1])-2)], span = 7, strict = FALSE, na.rm=TRUE)
                } else {
                    minima_minor <- cbind(0,0)
                    maxima_minor <- cbind(0,0)
                }
                
                #Too check for cases were no slip was found, basically trying again with lower thresholds
                
                if(all(is.na(maxima_minor[,1])))# && all(!is.na(maxima_minor[,2])))
                {
                    #print("low")
                    minima_minor <- NULL
                    maxima_minor <- NULL
                    
                    minima_minor <- cbind(rep(NA, times = length(valleys(minor_slip_points[q:(q+match(NA, minor_slip_points[q:length(minor_slip_points[,1]),1])-2)], span = 5, strict = FALSE, na.rm=TRUE))), rep(NA, times = length(valleys(minor_slip_points[q:(q+match(NA, minor_slip_points[q:length(minor_slip_points[,1]),1])-2)], span = 5, strict = FALSE, na.rm=TRUE))))
                    maxima_minor <- cbind(rep(NA, times = length(peaks(minor_slip_points[q:(q+match(NA, minor_slip_points[q:length(minor_slip_points[,1]),1])-2)], span = 5, strict = FALSE, na.rm=TRUE))), rep(NA, times = length(peaks(minor_slip_points[q:(q+match(NA, minor_slip_points[q:length(minor_slip_points[,1]),1])-2)], span = 5, strict = FALSE, na.rm=TRUE))))
                    
                    #Assigning y-values
                    minima_minor[,1] <- valleys(minor_slip_points[q:(q+match(NA, minor_slip_points[q:length(minor_slip_points[,1]),1])-2)], span = 5, strict = FALSE, na.rm=TRUE)
                    
                    #Assigning y-values
                    maxima_minor[,1] <- peaks(minor_slip_points[q:(q+match(NA, minor_slip_points[q:length(minor_slip_points[,1]),1])-2)], span = 5, strict = FALSE, na.rm=TRUE)
                }
                
                #Too check for cases were no slip was found, basically trying again with even lower thresholds
                
                if(all(is.na(maxima_minor[,1])))# && all(!is.na(maxima_minor[,2])))
                {
                    #print("lower")
                    minima_minor <- NULL
                    maxima_minor <- NULL
                    
                    minima_minor <- cbind(rep(NA, times = length(valleys(minor_slip_points[q:(q+match(NA, minor_slip_points[q:length(minor_slip_points[,1]),1])-2)], span = 3, strict = FALSE, na.rm=TRUE))), rep(NA, times = length(valleys(minor_slip_points[q:(q+match(NA, minor_slip_points[q:length(minor_slip_points[,1]),1])-2)], span = 3, strict = FALSE, na.rm=TRUE))))
                    maxima_minor <- cbind(rep(NA, times = length(peaks(minor_slip_points[q:(q+match(NA, minor_slip_points[q:length(minor_slip_points[,1]),1])-2)], span = 3, strict = FALSE, na.rm=TRUE))), rep(NA, times = length(peaks(minor_slip_points[q:(q+match(NA, minor_slip_points[q:length(minor_slip_points[,1]),1])-2)], span = 3, strict = FALSE, na.rm=TRUE))))
                    
                    #Assigning y-values
                    minima_minor[,1] <- valleys(minor_slip_points[q:(q+match(NA, minor_slip_points[q:length(minor_slip_points[,1]),1])-2)], span = 3, strict = FALSE, na.rm=TRUE)
                    
                    #Assigning y-values
                    maxima_minor[,1] <- peaks(minor_slip_points[q:(q+match(NA, minor_slip_points[q:length(minor_slip_points[,1]),1])-2)], span = 3, strict = FALSE, na.rm=TRUE)
                }
                
                
                #Assigning x-values
                if(length(minima_minor[,1]) != 0  && all(!is.na(minima_minor[,1])))
                {
                    for(a in 1:length(minima_minor[,1])){
                        minima_minor[a,2] <- minor_slip_points[q+match(minima_minor[a,1], minor_slip_points[q:length(minor_slip_points[,1]),1]),2]
                        next #a = a + 1
                    }
                } else {
                    #Minima are not that needed as I initially intended
                }
                
                
                
                if(length(maxima_minor[,1]) != 0 && all(!is.na(maxima_minor[,1])))# && all(!is.na(maxima_minor[,2])))
                {
                    #Assigning x-values
                    for(b in 1:length(maxima_minor[,1])){
                        maxima_minor[b,2] <- minor_slip_points[q+match(maxima_minor[b,1], minor_slip_points[q:length(minor_slip_points[,1]),1]),2]
                        #print(maxima_minor[b,2])
                        #print(paste("match", match(maxima_minor[b,1])))
                        next #b = b + 1
                    }
                    
                    #Separating slips
                    if(length(which(!is.na(maxima_minor[,1]))) >= 3 && (max(maxima_minor[,2]) - min(maxima_minor[,2])) <= 10)
                    {
                        
                        #Cases where the slips are not spread apart enough but you can't also count all of them without getting too many
                        if(length(minor_slip_points[q:(q+match(NA, minor_slip_points[q:length(minor_slip_points[,1]),1])-2)]) >= 25)
                        {
                            #supressWarnings() because otherwise it will always tell you that x values will be reduced to unique values. Removing them is not an option due to then having unequal lengths of vectors.
                            suppressWarnings(approx_fun_new_minor <- approx(as.vector(minor_slip_points[q:(q+match(NA, minor_slip_points[q:length(minor_slip_points[,1]),1])-2),2]),as.vector(minor_slip_points[q:(q+match(NA, minor_slip_points[q:length(minor_slip_points[,1]),1])-2),1]), na.rm =TRUE))
                            
                            
                            minima_minor_fun_new <- NULL
                            maxima_minor_fun_new <- NULL
                            
                            #Assigning y-values
                            minima_minor_fun_new$y <- valleys(approx_fun_new_minor$y, span = 7, strict = TRUE, na.rm=TRUE)
                            
                            #Assigning x-values
                            for(c in 1:length(minima_minor_fun_new$y)){
                                minima_minor_fun_new$x[c] <- approx_fun_new_minor$x[match(minima_minor_fun_new$y[c], approx_fun_new_minor$y)]
                                next #c = c + 1
                            }
                            
                            #Assigning y-values
                            maxima_minor_fun_new$y <- peaks(approx_fun_new_minor$y, span = 7, strict = TRUE, na.rm=TRUE)
                            
                            #Assigning x-values
                            for(d in 1:length(maxima_minor_fun_new$y)){
                                maxima_minor_fun_new$x[d] <- approx_fun_new_minor$x[match(maxima_minor_fun_new$y[d], approx_fun_new_minor$y)]
                                next #d = d + 1
                            }
                            
                            #Stricter detection if too many global maxima
                            
                            if(length(maxima_minor_fun_new$y) >= 3)
                            {
                                #print("Stricter minor slip detection")
                                suppressWarnings(approx_fun_new_minor <- approx(as.vector(minor_slip_points[q:(q+match(NA, minor_slip_points[q:length(minor_slip_points[,1]),1])-2),2]),as.vector(minor_slip_points[q:(q+match(NA, minor_slip_points[q:length(minor_slip_points[,1]),1])-2),1]), na.rm =TRUE))
                                
                                
                                minima_minor_fun_new <- NULL
                                maxima_minor_fun_new <- NULL
                                
                                #Assigning y-values
                                minima_minor_fun_new$y <- valleys(approx_fun_new_minor$y, span = 11, strict = TRUE, na.rm=TRUE)
                                
                                #Assigning x-values
                                for(c in 1:length(minima_minor_fun_new$y)){
                                    minima_minor_fun_new$x[c] <- approx_fun_new_minor$x[match(minima_minor_fun_new$y[c], approx_fun_new_minor$y)]
                                    next #c = c + 1
                                }
                                
                                #Assigning y-values
                                maxima_minor_fun_new$y <- peaks(approx_fun_new_minor$y, span = 11, strict = TRUE, na.rm=TRUE)
                                
                                #Assigning x-values
                                for(d in 1:length(maxima_minor_fun_new$y)){
                                    maxima_minor_fun_new$x[d] <- approx_fun_new_minor$x[match(maxima_minor_fun_new$y[d], approx_fun_new_minor$y)]
                                    next #d = d + 1
                                }
                                
                                #print(paste("maxima_minor_fun$y 1", maxima_minor_fun_new$y))
                                
                                if(all(is.na(maxima_minor_fun_new$y)))
                                {
                                    #print(paste("length1",length(maxima_minor_fun_new$y)))
                                    if(median(reverse_ratio[[i]][q:(q+match(NA, minor_slip_points[q:length(minor_slip_points[,1]),1])-2)]) <= -50 || median(reverse_ratio[[i]][q:(q+match(NA, minor_slip_points[q:length(minor_slip_points[,1]),1])-2)]) >= -20)
                                    {
                                        #print(paste(median(reverse_ratio[[i]][q:(q+match(NA, minor_slip_points[q:length(minor_slip_points[,1]),1])-2)]) , names(df_list)[i], "would have added 1"))
                                    } else 
                                    {
                                        minor_slip_count <- minor_slip_count + 1 #Maybe separate here further.
                                    }
                                }
                            }    
                            
                            #Even stricter if too many maxima
                            if(length(maxima_minor_fun_new$y) >= 2)
                            {
                                #print("Stricter minor slip detection")
                                suppressWarnings(approx_fun_new_minor <- approx(as.vector(minor_slip_points[q:(q+match(NA, minor_slip_points[q:length(minor_slip_points[,1]),1])-2),2]),as.vector(minor_slip_points[q:(q+match(NA, minor_slip_points[q:length(minor_slip_points[,1]),1])-2),1]), na.rm =TRUE))
                                
                                
                                minima_minor_fun_new <- NULL
                                maxima_minor_fun_new <- NULL
                                
                                #Assigning y-values
                                minima_minor_fun_new$y <- valleys(approx_fun_new_minor$y, span = 21, strict = TRUE, na.rm=TRUE)
                                
                                #Assigning x-values
                                for(c in 1:length(minima_minor_fun_new$y)){
                                    minima_minor_fun_new$x[c] <- approx_fun_new_minor$x[match(minima_minor_fun_new$y[c], approx_fun_new_minor$y)]
                                    next #c = c + 1
                                }
                                
                                #Assigning y-values
                                maxima_minor_fun_new$y <- peaks(approx_fun_new_minor$y, span = 21, strict = TRUE, na.rm=TRUE)
                                
                                #Assigning x-values
                                for(d in 1:length(maxima_minor_fun_new$y)){
                                    maxima_minor_fun_new$x[d] <- approx_fun_new_minor$x[match(maxima_minor_fun_new$y[d], approx_fun_new_minor$y)]
                                    next #d = d + 1
                                }
                                
                                #print(paste("maxima_minor_fun$y 1", maxima_minor_fun_new$y))
                                
                                if(all(is.na(maxima_minor_fun_new$y)))
                                {
                                    #print(paste("length1",length(maxima_minor_fun_new$y)))
                                    if(median(reverse_ratio[[i]][q:(q+match(NA, minor_slip_points[q:length(minor_slip_points[,1]),1])-2)]) <= -50 || median(reverse_ratio[[i]][q:(q+match(NA, minor_slip_points[q:length(minor_slip_points[,1]),1])-2)]) >= -20)
                                    {
                                        #print(paste(median(reverse_ratio[[i]][q:(q+match(NA, minor_slip_points[q:length(minor_slip_points[,1]),1])-2)]) , names(df_list)[i], "would have added 1"))
                                    } else 
                                    {
                                        minor_slip_count <- minor_slip_count + 1 #Maybe separate here further.
                                    }
                                }
                                
                            }
                            
                            if(length(maxima_minor_fun_new$y) >= 3)
                            {
                                if(!is.na(major_slip_points[(q+match(NA, minor_slip_points[q:length(minor_slip_points[,1]),1])-1),2]) && minor_slip_count > 0)
                                {
                                    minor_slip_count <- minor_slip_count - 1 #It is very likely that it will be overcounted without this
                                }
                            }
                            
                            #Each local maxima will be counted as a slip.
                            
                            if(mean(reverse_ratio[[i]][q:(q+match(NA, minor_slip_points[q:length(minor_slip_points[,1]),1])-2)]) <= -50 || mean(reverse_ratio[[i]][q:(q+match(NA, minor_slip_points[q:length(minor_slip_points[,1]),1])-2)]) >= -20)
                            {
                                #print(paste("mean", mean(reverse_ratio[[i]][q:(q+match(NA, minor_slip_points[q:length(minor_slip_points[,1]),1])-2)]) , names(df_list)[i], "would have added", length(maxima_minor_fun_new$y)))
                            } else 
                            {
                                minor_slip_count <- minor_slip_count + length(maxima_minor_fun_new$y)
                            }
                            
                            if(visualize_process == TRUE || visualize_process_minor == TRUE)
                            {
                                #Graphical representation
                                try(plot(approx_fun_new_minor, xlab = "Body part x-position", ylab = "Body part y-position", main = paste("approx_fun_new_minor", graph_number_minor+1, "for", names(df_list[i])), type = "l"), silent = TRUE)
                                try(points(minima_minor_fun_new$x, minima_minor_fun_new$y, col = "red"), silent = TRUE)
                                try(points(maxima_minor_fun_new$x, maxima_minor_fun_new$y, col = "blue"), silent = TRUE)
                                try(legend("bottomleft", paste("minor Slips:", minor_slip_count), bty = "n", horiz = TRUE),silent = TRUE)
                            }
                            
                            
                            ###New Block Start 1###
                            
                            if(identical(maxima_minor_fun_new$x, numeric(0)))
                            {
                                maxima_minor_fun_new$x <- NA
                            }
                            if(identical(maxima_minor_fun_new$y, numeric(0)))
                            {
                                maxima_minor_fun_new$y <- NA
                            }
                            
                            maxima_minor_fun_new <- data.frame(maxima_minor_fun_new)
                            
                            for(row in 1:nrow(maxima_minor_fun_new))
                            {
                                #print(paste("y", match(round(maxima_minor_fun_new[row,1], digits = 0), round(df_list[[i]][q:(q+match(NA, minor_slip_points[q:length(minor_slip_points[,1]),1])-2),body_part_y], digits = 0))))
                                #print(paste("x", match(round(maxima_minor_fun_new[row,2], digits = 0), round(df_list[[i]][q:(q+match(NA, minor_slip_points[q:length(minor_slip_points[,1]),1])-2),body_part_x], digits = 0))))
                                
                                #these
                                #print(paste("y", match(maxima_minor_fun_new[row,1], approx_fun_new_minor$y)))
                                #print(paste("x", match(maxima_minor_fun_new[row,2], approx_fun_new_minor$x)))
                                
                                if(is.na(match(maxima_minor_fun_new[row,2], approx_fun_new_minor$x))||is.na(match(maxima_minor_fun_new[row,1], approx_fun_new_minor$y)))
                                {
                                    #print(paste("length divided by maximas", round(length(q:(q+match(NA, minor_slip_points[q:length(minor_slip_points[,1]),1])-2))/length(maxima_minor_fun_new$y), digits = 0)))
                                    #print(paste("round y", round(median(match(maxima_minor_fun_new[row,1], approx_fun_new_minor$y), na.rm = TRUE), digits = 0)))
                                    
                                    median_length <- round((length(q:(q+match(NA, minor_slip_points[q:length(minor_slip_points[,1]),1])-2))/length(maxima_minor_fun_new$y))*row, digits = 0)
                                    
                                    what_point_is_it_x[[i]][wpi_c]           <- df_list[[i]][q+median_length, body_part_x]
                                    what_point_is_it_y[[i]][wpi_c]           <- df_list[[i]][q+median_length, body_part_y]
                                    at_what_frames_are_the_slips[[i]][wpi_c] <- df_list[[i]][q+median_length, frame_c]
                                    
                                } else if(!identical(match(maxima_minor_fun_new[row,2], approx_fun_new_minor$x), match(maxima_minor_fun_new[row,1], approx_fun_new_minor$y)))
                                {
                                    #print("unequal")
                                    
                                    median_number <- round(median(match(maxima_minor_fun_new[row,2], approx_fun_new_minor$x), match(maxima_minor_fun_new[row,1], approx_fun_new_minor$y), na.rm = TRUE), digits = 0)
                                    
                                    what_point_is_it_x[[i]][wpi_c]           <- df_list[[i]][q+median_number, body_part_x]
                                    what_point_is_it_y[[i]][wpi_c]           <- df_list[[i]][q+median_number, body_part_y]
                                    at_what_frames_are_the_slips[[i]][wpi_c] <- df_list[[i]][q+median_number, frame_c]
                                    
                                } else 
                                {
                                    #the points obviously don't match up, because it is an approximated function against the real whacky coordinates
                                    #print(paste("x of point", df_list[[i]][q+match(maxima_minor_fun_new[row,2], approx_fun_new_minor$x), body_part_x], "y of point", df_list[[i]][q+match(maxima_minor_fun_new[row,1], approx_fun_new_minor$y), body_part_y], "frame of point", df_list[[i]][q+match(maxima_minor_fun_new[row,1], approx_fun_new_minor$y), frame_c]))
                                    
                                    what_point_is_it_x[[i]][wpi_c]           <- df_list[[i]][q+match(maxima_minor_fun_new[row,2], approx_fun_new_minor$x), body_part_x]
                                    what_point_is_it_y[[i]][wpi_c]           <- df_list[[i]][q+match(maxima_minor_fun_new[row,1], approx_fun_new_minor$y), body_part_y]
                                    at_what_frames_are_the_slips[[i]][wpi_c] <- df_list[[i]][q+match(maxima_minor_fun_new[row,1], approx_fun_new_minor$y), frame_c]
                                }
                                
                                type_of_slip[[i]][wpi_c] <- "minor"
                                wpi_c <- wpi_c + 1
                            }
                            
                            ###New Block End 1###
                            
                            
                            ##print(where_is_this_point_analysed[[i]][q:(q+match(NA, minor_slip_points[q:length(minor_slip_points[,1]),1])-2)])
                            #print(paste("minor length", length(q:(q+match(NA, minor_slip_points[q:length(minor_slip_points[,1]),1])-2)))) #Essentially the duration of the segment
                            
                            #print(paste("new interpolation slip count", minor_slip_count))
                            
                        } else {
                            #Cases with too many maxima in a way to small area will only be counted as one slip.
                            if(median(reverse_ratio[[i]][q:(q+match(NA, minor_slip_points[q:length(minor_slip_points[,1]),1])-2)]) <= -50 || median(reverse_ratio[[i]][q:(q+match(NA, minor_slip_points[q:length(minor_slip_points[,1]),1])-2)]) >= -20)
                            {
                                #print(paste(median(reverse_ratio[[i]][q:(q+match(NA, minor_slip_points[q:length(minor_slip_points[,1]),1])-2)]) , names(df_list)[i], "would have added 1"))
                            } else 
                            {
                                minor_slip_count <- minor_slip_count + 1 #Maybe separate here further.
                                
                                ###New Block Minor###
                                #median_x_position <- median(df_list[[i]][q:(q+match(NA, minor_slip_points[q:length(minor_slip_points[,1]),1])-2), body_part_x], na.rm = TRUE)
                                #median_y_position <- median(df_list[[i]][q:(q+match(NA, minor_slip_points[q:length(minor_slip_points[,1]),1])-2), body_part_y], na.rm = TRUE)
                                #median_frame      <- round(median(df_list[[i]][q:(q+match(NA, minor_slip_points[q:length(minor_slip_points[,1]),1])-2), frame_c], na.rm = TRUE), digits = 0)
                                #
                                #what_point_is_it_x[[i]][wpi_c]           <- median_x_position
                                #what_point_is_it_y[[i]][wpi_c]           <- median_y_position
                                #at_what_frames_are_the_slips[[i]][wpi_c] <- median_frame
                                #
                                #type_of_slip[[i]][wpi_c] <- "minor"
                                #wpi_c <- wpi_c + 1
                                ###New Block Minor###
                            }
                            #print(paste("too many slips found", minor_slip_count))
                        }
                        
                        #No next here for visualisation
                        
                    } else if(length(which(!is.na(maxima_minor[,1]))) >= 3 && (max(maxima_minor[,2]) - min(maxima_minor[,2])) > 10)
                    {
                        
                        #Cases with too many maxima that are farther than 10 pixels apart in width will have a function approximated/interpolated from which then minima and maxima are taken
                        
                        #supressWarnings() because otherwise it will always tell you that x values will be reduced to unique values. Removing them is not an option due to then having unequal lengths of vectors.
                        suppressWarnings(approx_fun_minor <- approx(as.vector(minor_slip_points[q:(q+match(NA, minor_slip_points[q:length(minor_slip_points[,1]),1])-2),2]),as.vector(minor_slip_points[q:(q+match(NA, minor_slip_points[q:length(minor_slip_points[,1]),1])-2),1]), na.rm =TRUE))
                        
                        
                        minima_minor_fun <- NULL
                        maxima_minor_fun <- NULL
                        
                        #Assigning y-values
                        minima_minor_fun$y <- valleys(approx_fun_minor$y, span = 5, strict = TRUE, na.rm=TRUE)
                        
                        #Assigning x-values
                        for(c in 1:length(minima_minor_fun$y)){
                            minima_minor_fun$x[c] <- approx_fun_minor$x[match(minima_minor_fun$y[c], approx_fun_minor$y)]
                            next #c = c + 1
                        }
                        
                        #Assigning y-values
                        maxima_minor_fun$y <- peaks(approx_fun_minor$y, span = 5, strict = TRUE, na.rm=TRUE)
                        
                        #Assigning x-values
                        for(d in 1:length(maxima_minor_fun$y)){
                            maxima_minor_fun$x[d] <- approx_fun_minor$x[match(maxima_minor_fun$y[d], approx_fun_minor$y)]
                            next #d = d + 1
                        }
                        
                        
                        if(length(maxima_minor_fun$y) > 3)
                        {
                            suppressWarnings(approx_fun_minor <- approx(as.vector(minor_slip_points[q:(q+match(NA, minor_slip_points[q:length(minor_slip_points[,1]),1])-2),2]),as.vector(minor_slip_points[q:(q+match(NA, minor_slip_points[q:length(minor_slip_points[,1]),1])-2),1]), na.rm =TRUE))
                            
                            
                            minima_minor_fun <- NULL
                            maxima_minor_fun <- NULL
                            
                            #Assigning y-values
                            minima_minor_fun$y <- valleys(approx_fun_minor$y, span = 7, strict = TRUE, na.rm=TRUE)
                            
                            #Assigning x-values
                            for(c in 1:length(minima_minor_fun$y)){
                                minima_minor_fun$x[c] <- approx_fun_minor$x[match(minima_minor_fun$y[c], approx_fun_minor$y)]
                                next #c = c + 1
                            }
                            
                            #Assigning y-values
                            maxima_minor_fun$y <- peaks(approx_fun_minor$y, span = 7, strict = TRUE, na.rm=TRUE)
                            
                            #Assigning x-values
                            for(d in 1:length(maxima_minor_fun$y)){
                                maxima_minor_fun$x[d] <- approx_fun_minor$x[match(maxima_minor_fun$y[d], approx_fun_minor$y)]
                                next #d = d + 1
                            }
                        }
                        
                        #print(paste("maxima_minor_fun$y 2", maxima_minor_fun$y))
                        
                        #At least one slip must be here
                        if(all(is.na(maxima_minor_fun$y)))
                        {
                            #print(paste("length2",length(maxima_minor_fun$y)))
                            if(median(reverse_ratio[[i]][q:(q+match(NA, minor_slip_points[q:length(minor_slip_points[,1]),1])-2)]) <= -50 || median(reverse_ratio[[i]][q:(q+match(NA, minor_slip_points[q:length(minor_slip_points[,1]),1])-2)]) >= -20)
                            {
                                #print(paste(median(reverse_ratio[[i]][q:(q+match(NA, minor_slip_points[q:length(minor_slip_points[,1]),1])-2)]) , names(df_list)[i], "would have added 1"))
                            } else 
                            {
                                minor_slip_count <- minor_slip_count + 1 #Maybe separate here further.
                            }
                        }
                        
                        #Each local maxima will be counted as a slip.
                        
                        if(mean(reverse_ratio[[i]][q:(q+match(NA, minor_slip_points[q:length(minor_slip_points[,1]),1])-2)]) <= -50 || mean(reverse_ratio[[i]][q:(q+match(NA, minor_slip_points[q:length(minor_slip_points[,1]),1])-2)]) >= -20)
                        {
                            #print(paste("mean", mean(reverse_ratio[[i]][q:(q+match(NA, minor_slip_points[q:length(minor_slip_points[,1]),1])-2)]) , names(df_list)[i], "would have added", length(maxima_minor_fun$y)))
                        } else 
                        {
                            minor_slip_count <- minor_slip_count + length(maxima_minor_fun$y)
                        }
                        
                        if(visualize_process == TRUE || visualize_process_minor == TRUE)
                        {
                            #Graphical representation
                            try(plot(approx_fun_minor, xlab = "Body part x-position", ylab = "Body part y-position", main = paste("approx_fun_minor", graph_number_minor+1, "for", names(df_list[i])), type = "l"), silent = TRUE)
                            try(points(minima_minor_fun$x, minima_minor_fun$y, col = "red"), silent = TRUE)
                            try(points(maxima_minor_fun$x, maxima_minor_fun$y, col = "blue"), silent = TRUE)
                            try(legend("bottomleft", paste("minor Slips:", minor_slip_count), bty = "n", horiz = TRUE),silent = TRUE)
                            
                            #print(where_is_this_point_analysed[[i]][q:(q+match(NA, minor_slip_points[q:length(minor_slip_points[,1]),1])-2)])
                            #print(paste("minor length", length(q:(q+match(NA, minor_slip_points[q:length(minor_slip_points[,1]),1])-2)))) #Essentially the duration of the segment
                        }
                        
                        #print(paste("interpolation slip count", minor_slip_count))
                        
                        
                        ###New Block Start 2###
                        
                        if(identical(maxima_minor_fun$x, numeric(0)))
                        {
                            maxima_minor_fun$x <- NA
                        }
                        if(identical(maxima_minor_fun$y, numeric(0)))
                        {
                            maxima_minor_fun$y <- NA
                        }
                        
                        maxima_minor_fun <- data.frame(maxima_minor_fun)
                        
                        for(row in 1:nrow(maxima_minor_fun))
                        {
                            #these
                            #print(paste("y", match(maxima_minor_fun[row,1], approx_fun_minor$y)))
                            #print(paste("x", match(maxima_minor_fun[row,2], approx_fun_minor$x)))
                            
                            if(is.na(match(maxima_minor_fun[row,2], approx_fun_minor$x))||is.na(match(maxima_minor_fun[row,1], approx_fun_minor$y)))
                            {
                                #print(paste("length divided by maximas", round(length(q:(q+match(NA, minor_slip_points[q:length(minor_slip_points[,1]),1])-2))/length(maxima_minor_fun$y), digits = 0)))
                                #print(paste("round y", round(median(match(maxima_minor_fun[row,1], approx_fun_minor$y), na.rm = TRUE), digits = 0)))
                                
                                median_length <- round((length(q:(q+match(NA, minor_slip_points[q:length(minor_slip_points[,1]),1])-2))/length(maxima_minor_fun$y))*row, digits = 0)
                                
                                what_point_is_it_x[[i]][wpi_c]           <- df_list[[i]][q+median_length, body_part_x]
                                what_point_is_it_y[[i]][wpi_c]           <- df_list[[i]][q+median_length, body_part_y]
                                at_what_frames_are_the_slips[[i]][wpi_c] <- df_list[[i]][q+median_length, frame_c]
                                
                            } else if(!identical(match(maxima_minor_fun[row,2], approx_fun_minor$x), match(maxima_minor_fun[row,1], approx_fun_minor$y)))
                            {
                                #print("unequal")
                                
                                median_number <- round(median(match(maxima_minor_fun[row,2], approx_fun_minor$x), match(maxima_minor_fun[row,1], approx_fun_minor$y), na.rm = TRUE), digits = 0)
                                
                                what_point_is_it_x[[i]][wpi_c]           <- df_list[[i]][q+median_number, body_part_x]
                                what_point_is_it_y[[i]][wpi_c]           <- df_list[[i]][q+median_number, body_part_y]
                                at_what_frames_are_the_slips[[i]][wpi_c] <- df_list[[i]][q+median_number, frame_c]
                                
                            } else 
                            {
                                #the points obviously don't match up, because it is an approximated function against the real whacky coordinates
                                #print(paste("x of point", df_list[[i]][q+match(maxima_minor_fun[row,2], approx_fun_minor$x), body_part_x], "y of point", df_list[[i]][q+match(maxima_minor_fun[row,1], approx_fun_minor$y), body_part_y], "frame of point", df_list[[i]][q+match(maxima_minor_fun[row,1], approx_fun_minor$y), frame_c]))
                                
                                what_point_is_it_x[[i]][wpi_c]           <- df_list[[i]][q+match(maxima_minor_fun[row,2], approx_fun_minor$x), body_part_x]
                                what_point_is_it_y[[i]][wpi_c]           <- df_list[[i]][q+match(maxima_minor_fun[row,1], approx_fun_minor$y), body_part_y]
                                at_what_frames_are_the_slips[[i]][wpi_c] <- df_list[[i]][q+match(maxima_minor_fun[row,1], approx_fun_minor$y), frame_c]
                            }
                            
                            type_of_slip[[i]][wpi_c] <- "minor"
                            wpi_c <- wpi_c + 1
                        }
                        
                        ###New Block End 2###
                        
                    } else {
                        
                        #Cases that have only one or two maxima
                        
                        if(median(reverse_ratio[[i]][q:(q+match(NA, minor_slip_points[q:length(minor_slip_points[,1]),1])-2)]) <= -50 || median(reverse_ratio[[i]][q:(q+match(NA, minor_slip_points[q:length(minor_slip_points[,1]),1])-2)]) >= -20)
                        {
                            #print(paste(median(reverse_ratio[[i]][q:(q+match(NA, minor_slip_points[q:length(minor_slip_points[,1]),1])-2)]) , names(df_list)[i], "would have added 1"))
                        } else 
                        {
                            minor_slip_count <- minor_slip_count + 1 #Maybe separate here further.
                            
                            ###New Block Minor###
                            #median_x_position <- median(df_list[[i]][q:(q+match(NA, minor_slip_points[q:length(minor_slip_points[,1]),1])-2), body_part_x], na.rm = TRUE)
                            #median_y_position <- median(df_list[[i]][q:(q+match(NA, minor_slip_points[q:length(minor_slip_points[,1]),1])-2), body_part_y], na.rm = TRUE)
                            #median_frame      <- round(median(df_list[[i]][q:(q+match(NA, minor_slip_points[q:length(minor_slip_points[,1]),1])-2), frame_c], na.rm = TRUE), digits = 0)
                            #
                            #what_point_is_it_x[[i]][wpi_c]           <- median_x_position
                            #what_point_is_it_y[[i]][wpi_c]           <- median_y_position
                            #at_what_frames_are_the_slips[[i]][wpi_c] <- median_frame
                            #
                            #type_of_slip[[i]][wpi_c] <- "minor"
                            #wpi_c <- wpi_c + 1
                            ###New Block Minor###
                        }
                        
                        #No next here for visualisation
                    }
                    
                } else 
                {
                    #print("else, were no slips could be found")
                    
                    next #q = q + 1 #If no further separation is done
                }
                
                #For naming the graphs
                graph_number_minor <- graph_number_minor + 1
                
                if(visualize_process == TRUE || visualize_process_minor == TRUE){
                    #Graphical representation of all minor slips
                    try(plot(minor_slip_points[q:(q+match(NA, minor_slip_points[q:length(minor_slip_points[,1]),1])-2),2], minor_slip_points[q:(q+match(NA, minor_slip_points[q:length(minor_slip_points[,1]),1])-2),1], xlab = "Body part x-position", ylab = "Body part y-position", main = paste("minor slip detection",graph_number_minor, "for", names(df_list[i])), type = "l"), silent = TRUE)
                    try(points(minima_minor[,2],minima_minor[,1], col = "dark red"),silent = TRUE)
                    try(points(maxima_minor[,2],maxima_minor[,1], col = "dark blue"),silent = TRUE)
                    try(legend("bottomleft", paste("minor Slips:", minor_slip_count), bty = "n", horiz = TRUE),silent = TRUE)
                    
                    #print(where_is_this_point_analysed[[i]][q:(q+match(NA, minor_slip_points[q:length(minor_slip_points[,1]),1])-2)])
                    #print(paste("minor length", length(q:(q+match(NA, minor_slip_points[q:length(minor_slip_points[,1]),1])-2)))) #Essentially the duration of the segment
                }
                
                #q = q + match(NA, minor_slip_points[q:length(minor_slip_points[,1]),1]) #for example: match(NA, minor_slip_points[393:length(minor_slip_points[,1]),1]) returns 6, because this is the next NA value at entry 398, so q will skip to 399 by addition
                iterations_to_skip_minor_slip <- match(NA, minor_slip_points[q:length(minor_slip_points[,1]),1]) - 2 #Workaround, because you can't modify for-loop integers like this. -2 because e.g.: entry 393 is counted by match as well as the step to 398. So to skip the accurate number 2 needs to be removed. 
                next #q = q + 1
                
            } else if(!is.na(minor_slip_points[q-1, 1]) && !is.na(minor_slip_points[q, 1]) && !is.na(minor_slip_points[q, 2]) && !is.na(match(NA, minor_slip_points[q:length(minor_slip_points[,1]),1])-2)) #In case it directly starts with a minor slip
            {
                #In case it directly starts with a minor slip
                
                #Honestly I would just like to use labels here to jump to the correct parts of the script, but that's not possible in R.
                
                #minor_slip_count <- minor_slip_count + 1
                #iterations_to_skip_minor_slip <- match(NA, minor_slip_points[q-1:length(minor_slip_points[,1]),1]) - 2
                
                minor_slip_points[q, 1] <- NA
                #print(paste("There seems to be the special case in minor slip detection.", names(df_list[i]), "with q", q))
                next #q = q + 1
                
            } else if(is.na(match(NA, minor_slip_points[q:length(minor_slip_points[,1]),1])-2))
            {
                iterations_to_skip_minor_slip <- iterations_to_skip_minor_slip + (length(minor_slip_points[,1]) - q)
                #print(paste("Special case: match() returns NA at q", q, "for minor slip detection in data frame", paste0(gsub(pattern = "`|L", "", deparse(substitute(df_list[[i]], env = environment()))), ","), "which is", names(df_list[i]), "- This case with it's", iterations_to_skip_minor_slip, "iterations will be skipped."))
                next #q = q + 1
                
            } else
            {
                
                #All other cases that should not be, but still can be.
                
                print(paste("Something is not quite right at minor slip detection: q =", q, "in data frame", paste0(gsub(pattern = "`|L", "", deparse(substitute(df_list[[i]], env = environment()))), ","), "which is", paste0(names(df_list[i]), ","), "at graph number", graph_number_minor + 1))
                next #q = q + 1
            }
                
            next #q = q + 1
        } #End of the minor slip detection for-loop
        
               
        #If something went unexcpectly weird
        if(minor_slip_count < 0)
        {
            minor_slip_count <- 0
        }
                
        minor_slip_list[i] <- minor_slip_count
        
           
        #Counting major slips
        for(p in 2:length(major_slip_points[,2]))
        {
            
            if(is.na(iterations_to_skip_major_slip))
            {
                iterations_to_skip_major_slip <- 0L
            }
            
            if(iterations_to_skip_major_slip != 0  && !is.na(iterations_to_skip_major_slip))
            {
                #Simply to skip iterations due to not being able to modify the for-loop variable directly
                
                iterations_to_skip_major_slip = iterations_to_skip_major_slip - 1
                #print(paste("skipped",p,"with value", major_slip_points[p,1], "and", iterations_to_skip_major_slip, "skips left"))
                next #p = p + 1
                
            }else if(is.na(major_slip_points[p,1]) && is.na(major_slip_points[p,2])) 
            {
                #print(paste("is.na", major_slip_points[p,1], major_slip_points[p,2], "with p", p))
                next #p = p + 1
                
            }else if(!is.na(match(NA, major_slip_points[p:length(major_slip_points[,1]), 1])) && length(major_slip_points[p:(p+match(NA, major_slip_points[p:length(major_slip_points[,1]),1])-2)]) < 3) 
            {
                #If the length is less than 3 frames
                iterations_to_skip_major_slip <- length(major_slip_points[p:(p+match(NA, major_slip_points[p:length(major_slip_points[,1]),1])-2)])
                
            }else if(is.na(major_slip_points[p-1, 1]) && !is.na(major_slip_points[p, 1]) && !is.na(major_slip_points[p, 2]) && !is.na(match(NA, major_slip_points[p:length(major_slip_points[,1]),1])-2))
            {
                a <- 0L
                b <- 0L
                c <- 0L
                d <- 0L
                
                #Initial check with strict check
                
                if(!is.na(match(NA, major_slip_points[p:length(major_slip_points[,1]),1]))){
                    minima_major <- cbind(rep(NA, times = length(valleys(major_slip_points[p:(p+match(NA, major_slip_points[p:length(major_slip_points[,1]),1])-2)], span = 7, strict = FALSE, na.rm=TRUE))), rep(NA, times = length(valleys(major_slip_points[p:(p+match(NA, major_slip_points[p:length(major_slip_points[,1]),1])-2)], span = 7, strict = FALSE, na.rm=TRUE))))
                    maxima_major <- cbind(rep(NA, times = length(peaks(major_slip_points[p:(p+match(NA, major_slip_points[p:length(major_slip_points[,1]),1])-2)], span = 7, strict = FALSE, na.rm=TRUE))), rep(NA, times = length(peaks(major_slip_points[p:(p+match(NA, major_slip_points[p:length(major_slip_points[,1]),1])-2)], span = 7, strict = FALSE, na.rm=TRUE))))
                    
                    #Assigning y-values
                    minima_major[,1] <- valleys(major_slip_points[p:(p+match(NA, major_slip_points[p:length(major_slip_points[,1]),1])-2)], span = 7, strict = FALSE, na.rm=TRUE)
                    
                    #Assigning y-values
                    maxima_major[,1] <- peaks(major_slip_points[p:(p+match(NA, major_slip_points[p:length(major_slip_points[,1]),1])-2)], span = 7, strict = FALSE, na.rm=TRUE)
                } else {
                    minima_major <- cbind(0,0)
                    maxima_major <- cbind(0,0)
                }
                
                #Too check for cases were no slip was found, basically trying again with lower thresholds
                
                if(all(is.na(maxima_major[,1])))# && all(!is.na(maxima_major[,2])))
                {
                    #print("low")
                    minima_major <- NULL
                    maxima_major <- NULL
                    
                    minima_major <- cbind(rep(NA, times = length(valleys(major_slip_points[p:(p+match(NA, major_slip_points[p:length(major_slip_points[,1]),1])-2)], span = 5, strict = FALSE, na.rm=TRUE))), rep(NA, times = length(valleys(major_slip_points[p:(p+match(NA, major_slip_points[p:length(major_slip_points[,1]),1])-2)], span = 5, strict = FALSE, na.rm=TRUE))))
                    maxima_major <- cbind(rep(NA, times = length(peaks(major_slip_points[p:(p+match(NA, major_slip_points[p:length(major_slip_points[,1]),1])-2)], span = 5, strict = FALSE, na.rm=TRUE))), rep(NA, times = length(peaks(major_slip_points[p:(p+match(NA, major_slip_points[p:length(major_slip_points[,1]),1])-2)], span = 5, strict = FALSE, na.rm=TRUE))))
                    
                    #Assigning y-values
                    minima_major[,1] <- valleys(major_slip_points[p:(p+match(NA, major_slip_points[p:length(major_slip_points[,1]),1])-2)], span = 5, strict = FALSE, na.rm=TRUE)
                    
                    #Assigning y-values
                    maxima_major[,1] <- peaks(major_slip_points[p:(p+match(NA, major_slip_points[p:length(major_slip_points[,1]),1])-2)], span = 5, strict = FALSE, na.rm=TRUE)
                }
                
                
                #Assigning x-values
                if(length(minima_major[,1]) != 0  && all(!is.na(minima_major[,1])))
                {
                    for(a in 1:length(minima_major[,1])){
                        minima_major[a,2] <- major_slip_points[p+match(minima_major[a,1], major_slip_points[p:length(major_slip_points[,1]),1]),2]
                        next #a = a + 1
                    }
                } else {
                    #Minima are not that needed as I initially intended
                }
                
                
                
                if(length(maxima_major[,1]) != 0 && all(!is.na(maxima_major[,1])))# && all(!is.na(maxima_major[,2])))
                {
                    #Assigning x-values
                    for(b in 1:length(maxima_major[,1])){
                        maxima_major[b,2] <- major_slip_points[p+match(maxima_major[b,1], major_slip_points[p:length(major_slip_points[,1]),1]),2]
                        #print(maxima_major[b,2])
                        #print(paste("match", match(maxima_major[b,1])))
                        next #b = b + 1
                    }
                    
                    #Separating slips
                    if(length(which(!is.na(maxima_major[,1]))) >= 3 && (max(maxima_major[,2]) - min(maxima_major[,2])) <= 10)
                    {
                        
                        #Cases where the slips are not spread apart enough but you can't also count all of them without getting too many
                        if(length(major_slip_points[p:(p+match(NA, major_slip_points[p:length(major_slip_points[,1]),1])-2)]) >= 25)
                        {
                            #print(paste("length",length(major_slip_points[p:(p+match(NA, major_slip_points[p:length(major_slip_points[,1]),1])-2)])))
                            #supressWarnings() because otherwise it will always tell you that x values will be reduced to unique values. Removing them is not an option due to then having unequal lengths of vectors.
                            suppressWarnings(approx_fun_new_major <- approx(as.vector(major_slip_points[p:(p+match(NA, major_slip_points[p:length(major_slip_points[,1]),1])-2),2]),as.vector(major_slip_points[p:(p+match(NA, major_slip_points[p:length(major_slip_points[,1]),1])-2),1]), na.rm =TRUE))
                            
                            
                            minima_major_fun_new <- NULL
                            maxima_major_fun_new <- NULL
                            
                            #Assigning y-values
                            minima_major_fun_new$y <- valleys(approx_fun_new_major$y, span = 7, strict = TRUE, na.rm=TRUE)
                            
                            #Assigning x-values
                            for(c in 1:length(minima_major_fun_new$y)){
                                minima_major_fun_new$x[c] <- approx_fun_new_major$x[match(minima_major_fun_new$y[c], approx_fun_new_major$y)]
                                next #c = c + 1
                            }
                            
                            #Assigning y-values
                            maxima_major_fun_new$y <- peaks(approx_fun_new_major$y, span = 7, strict = TRUE, na.rm=TRUE)
                            
                            #Assigning x-values
                            for(d in 1:length(maxima_major_fun_new$y)){
                                maxima_major_fun_new$x[d] <- approx_fun_new_major$x[match(maxima_major_fun_new$y[d], approx_fun_new_major$y)]
                                next #d = d + 1
                            }
                            
                            #Stricter detection if too many local maxima
                            #print(paste("length(maxima_major_fun_new$y)", length(maxima_major_fun_new$y)))
                            
                            if(length(maxima_major_fun_new$y) >= 3)
                            {
                                #print("Stricter major slip detection")
                                suppressWarnings(approx_fun_new_major <- approx(as.vector(major_slip_points[p:(p+match(NA, major_slip_points[p:length(major_slip_points[,1]),1])-2),2]),as.vector(major_slip_points[p:(p+match(NA, major_slip_points[p:length(major_slip_points[,1]),1])-2),1]), na.rm =TRUE))
                                
                                
                                minima_major_fun_new <- NULL
                                maxima_major_fun_new <- NULL
                                
                                #Assigning y-values
                                minima_major_fun_new$y <- valleys(approx_fun_new_major$y, span = 11, strict = TRUE, na.rm=TRUE)
                                
                                #Assigning x-values
                                for(c in 1:length(minima_major_fun_new$y)){
                                    minima_major_fun_new$x[c] <- approx_fun_new_major$x[match(minima_major_fun_new$y[c], approx_fun_new_major$y)]
                                    next #c = c + 1
                                }
                                
                                #Assigning y-values
                                maxima_major_fun_new$y <- peaks(approx_fun_new_major$y, span = 11, strict = TRUE, na.rm=TRUE)
                                
                                #Assigning x-values
                                for(d in 1:length(maxima_major_fun_new$y)){
                                    maxima_major_fun_new$x[d] <- approx_fun_new_major$x[match(maxima_major_fun_new$y[d], approx_fun_new_major$y)]
                                    next #d = d + 1
                                }
                                
                                #print(paste("maxima_major_fun$y 1", maxima_major_fun_new$y))
                                
                                if(all(is.na(maxima_major_fun_new$y)))
                                {
                                    #print(paste("length1",length(maxima_major_fun_new$y))) #is 0
                                    major_slip_count <- major_slip_count + 1
                                }
                                
                            }
                            
                            #Each local maxima will be counted as a slip.
                            major_slip_count <- major_slip_count + length(maxima_major_fun_new$y)
                            
                            if(visualize_process == TRUE || visualize_process_major == TRUE)
                            {
                                #Graphical representation
                                try(plot(approx_fun_new_major, xlab = "Body part x-position", ylab = "Body part y-position", main = paste("approx_fun_new_major", graph_number_major+1, "for", names(df_list[i])), type = "l"), silent = TRUE)
                                try(points(minima_major_fun_new$x, minima_major_fun_new$y, col = "red"), silent = TRUE)
                                try(points(maxima_major_fun_new$x, maxima_major_fun_new$y, col = "blue"), silent = TRUE)
                                try(legend("bottomleft", paste("Major Slips:", major_slip_count), bty = "n", horiz = TRUE),silent = TRUE)
                            }
                            
                            #print(paste("new interpolation slip count", major_slip_count))
                            
                            ###New Block Start 3###
                            
                            if(identical(maxima_major_fun_new$x, numeric(0)))
                            {
                                maxima_major_fun_new$x <- NA
                            }
                            if(identical(maxima_major_fun_new$y, numeric(0)))
                            {
                                maxima_major_fun_new$y <- NA
                            }
                            
                            maxima_major_fun_new <- data.frame(maxima_major_fun_new)
                            
                            for(row in 1:nrow(maxima_major_fun_new))
                            {
                                #print(paste("y", match(round(maxima_major_fun_new[row,1], digits = 0), round(df_list[[i]][p:(p+match(NA, major_slip_points[p:length(major_slip_points[,1]),1])-2),body_part_y], digits = 0))))
                                #print(paste("x", match(round(maxima_major_fun_new[row,2], digits = 0), round(df_list[[i]][p:(p+match(NA, major_slip_points[p:length(major_slip_points[,1]),1])-2),body_part_x], digits = 0))))
                                
                                #these
                                #print(paste("y", match(maxima_major_fun_new[row,1], approx_fun_new_major$y)))
                                #print(paste("x", match(maxima_major_fun_new[row,2], approx_fun_new_major$x)))
                                
                                if(is.na(match(maxima_major_fun_new[row,2], approx_fun_new_major$x))||is.na(match(maxima_major_fun_new[row,1], approx_fun_new_major$y)))
                                {
                                    #print(paste("length divided by maximas", round(length(p:(p+match(NA, major_slip_points[p:length(major_slip_points[,1]),1])-2))/length(maxima_major_fun_new$y), digits = 0)))
                                    #print(paste("round y", round(median(match(maxima_major_fun_new[row,1], approx_fun_new_major$y), na.rm = TRUE), digits = 0)))
                                    
                                    median_length <- round((length(p:(p+match(NA, major_slip_points[p:length(major_slip_points[,1]),1])-2))/length(maxima_major_fun_new$y))*row, digits = 0)
                                    
                                    what_point_is_it_x[[i]][wpi_c]           <- df_list[[i]][p+median_length, body_part_x]
                                    what_point_is_it_y[[i]][wpi_c]           <- df_list[[i]][p+median_length, body_part_y]
                                    at_what_frames_are_the_slips[[i]][wpi_c] <- df_list[[i]][p+median_length, frame_c]
                                    
                                } else if(!identical(match(maxima_major_fun_new[row,2], approx_fun_new_major$x), match(maxima_major_fun_new[row,1], approx_fun_new_major$y)))
                                {
                                    #print("unequal")
                                    
                                    median_number <- round(median(match(maxima_major_fun_new[row,2], approx_fun_new_major$x), match(maxima_major_fun_new[row,1], approx_fun_new_major$y), na.rm = TRUE), digits = 0)
                                    
                                    what_point_is_it_x[[i]][wpi_c]           <- df_list[[i]][p+median_number, body_part_x]
                                    what_point_is_it_y[[i]][wpi_c]           <- df_list[[i]][p+median_number, body_part_y]
                                    at_what_frames_are_the_slips[[i]][wpi_c] <- df_list[[i]][p+median_number, frame_c]
                                    
                                } else 
                                {
                                    #the points obviously don't match up, because it is an approximated function against the real whacky coordinates
                                    #print(paste("x of point", df_list[[i]][p+match(maxima_major_fun_new[row,2], approx_fun_new_major$x), body_part_x], "y of point", df_list[[i]][p+match(maxima_major_fun_new[row,1], approx_fun_new_major$y), body_part_y], "frame of point", df_list[[i]][p+match(maxima_major_fun_new[row,1], approx_fun_new_major$y), frame_c]))
                                    
                                    what_point_is_it_x[[i]][wpi_c]           <- df_list[[i]][p+match(maxima_major_fun_new[row,2], approx_fun_new_major$x), body_part_x]
                                    what_point_is_it_y[[i]][wpi_c]           <- df_list[[i]][p+match(maxima_major_fun_new[row,1], approx_fun_new_major$y), body_part_y]
                                    at_what_frames_are_the_slips[[i]][wpi_c] <- df_list[[i]][p+match(maxima_major_fun_new[row,1], approx_fun_new_major$y), frame_c]
                                }
                                
                                type_of_slip[[i]][wpi_c] <- "major"
                                wpi_c <- wpi_c + 1
                            }
                            
                            ###New Block End 3###
                            
                        } else {
                            #Cases with too many maxima in a way to small area will only be counted as one slip.
                            
                            major_slip_count <- major_slip_count + 1
                            #print(paste("too many slips found", major_slip_count))
                            
                            ###New Block Major###
                            median_x_position <- median(df_list[[i]][p:(p+match(NA, major_slip_points[p:length(major_slip_points[,1]),1])-2), body_part_x], na.rm = TRUE)
                            median_y_position <- median(df_list[[i]][p:(p+match(NA, major_slip_points[p:length(major_slip_points[,1]),1])-2), body_part_y], na.rm = TRUE)
                            median_frame      <- round(median(df_list[[i]][p:(p+match(NA, major_slip_points[p:length(major_slip_points[,1]),1])-2), frame_c], na.rm = TRUE), digits = 0)
                            
                            what_point_is_it_x[[i]][wpi_c]           <- median_x_position
                            what_point_is_it_y[[i]][wpi_c]           <- median_y_position
                            at_what_frames_are_the_slips[[i]][wpi_c] <- median_frame
                            
                            type_of_slip[[i]][wpi_c] <- "major"
                            wpi_c <- wpi_c + 1
                            ###New Block Major###
                        }
                        
                        #No next here for visualisation
                        
                    } else if(length(which(!is.na(maxima_major[,1]))) >= 3 && (max(maxima_major[,2]) - min(maxima_major[,2])) > 10)
                    {
                        
                        #Cases with too many maxima that are farther than 10 pixels apart in width will have a function approximated/interpolated from which then minima and maxima are taken
                        
                        #supressWarnings() because otherwise it will always tell you that x values will be reduced to unique values. Removing them is not an option due to then having unequal lengths of vectors.
                        suppressWarnings(approx_fun_major <- approx(as.vector(major_slip_points[p:(p+match(NA, major_slip_points[p:length(major_slip_points[,1]),1])-2),2]),as.vector(major_slip_points[p:(p+match(NA, major_slip_points[p:length(major_slip_points[,1]),1])-2),1]), na.rm =TRUE))
                        
                        
                        minima_major_fun <- NULL
                        maxima_major_fun <- NULL
                        
                        #Assigning y-values
                        minima_major_fun$y <- valleys(approx_fun_major$y, span = 5, strict = TRUE, na.rm=TRUE)
                        
                        #Assigning x-values
                        for(c in 1:length(minima_major_fun$y)){
                            minima_major_fun$x[c] <- approx_fun_major$x[match(minima_major_fun$y[c], approx_fun_major$y)]
                            next #c = c + 1
                        }
                        
                        #Assigning y-values
                        maxima_major_fun$y <- peaks(approx_fun_major$y, span = 5, strict = TRUE, na.rm=TRUE)
                        
                        #Assigning x-values
                        for(d in 1:length(maxima_major_fun$y)){
                            maxima_major_fun$x[d] <- approx_fun_major$x[match(maxima_major_fun$y[d], approx_fun_major$y)]
                            next #d = d + 1
                        }
                        
                        #print(paste("maxima_major_fun$y 2", maxima_major_fun$y))
                        
                        #At least one slip must be here
                        if(all(is.na(maxima_major_fun$y)))
                        {
                            #print(paste("length2",length(maxima_major_fun$y))) # is 0
                            major_slip_count <- major_slip_count + 1
                        }
                        
                        #Each local maxima will be counted as a slip.
                        major_slip_count <- major_slip_count + length(maxima_major_fun$y)
                        
                        if(visualize_process == TRUE || visualize_process_major == TRUE)
                        {
                            #Graphical representation
                            try(plot(approx_fun_major, xlab = "Body part x-position", ylab = "Body part y-position", main = paste("approx_fun_major", graph_number_major+1, "for", names(df_list[i])), type = "l"), silent = TRUE)
                            try(points(minima_major_fun$x, minima_major_fun$y, col = "red"), silent = TRUE)
                            try(points(maxima_major_fun$x, maxima_major_fun$y, col = "blue"), silent = TRUE)
                            try(legend("bottomleft", paste("Major Slips:", major_slip_count), bty = "n", horiz = TRUE),silent = TRUE)
                        }
                        
                        #print(paste("interpolation slip count", major_slip_count))
                        
                        
                        ###New Block Start 4###
                        
                        if(identical(maxima_major_fun$x, numeric(0)))
                        {
                            maxima_major_fun$x <- NA
                        }
                        if(identical(maxima_major_fun$y, numeric(0)))
                        {
                            maxima_major_fun$y <- NA
                        }
                        
                        maxima_major_fun <- data.frame(maxima_major_fun)
                        
                        for(row in 1:nrow(maxima_major_fun))
                        {
                            #print(paste("y", match(round(maxima_major_fun[row,1], digits = 0), round(df_list[[i]][p:(p+match(NA, major_slip_points[p:length(major_slip_points[,1]),1])-2),body_part_y], digits = 0))))
                            #print(paste("x", match(round(maxima_major_fun[row,2], digits = 0), round(df_list[[i]][p:(p+match(NA, major_slip_points[p:length(major_slip_points[,1]),1])-2),body_part_x], digits = 0))))
                            
                            #these
                            #print(paste("y", match(maxima_major_fun[row,1], approx_fun_major$y)))
                            #print(paste("x", match(maxima_major_fun[row,2], approx_fun_major$x)))
                            
                            if(is.na(match(maxima_major_fun[row,2], approx_fun_major$x))||is.na(match(maxima_major_fun[row,1], approx_fun_major$y)))
                            {
                                #print(paste("length divided by maximas", round(length(p:(p+match(NA, major_slip_points[p:length(major_slip_points[,1]),1])-2))/length(maxima_major_fun$y), digits = 0)))
                                #print(paste("round y", round(median(match(maxima_major_fun[row,1], approx_fun_major$y), na.rm = TRUE), digits = 0)))
                                
                                median_length <- round((length(p:(p+match(NA, major_slip_points[p:length(major_slip_points[,1]),1])-2))/length(maxima_major_fun$y))*row, digits = 0)
                                
                                what_point_is_it_x[[i]][wpi_c]           <- df_list[[i]][p+median_length, body_part_x]
                                what_point_is_it_y[[i]][wpi_c]           <- df_list[[i]][p+median_length, body_part_y]
                                at_what_frames_are_the_slips[[i]][wpi_c] <- df_list[[i]][p+median_length, frame_c]
                                
                            } else if(!identical(match(maxima_major_fun[row,2], approx_fun_major$x), match(maxima_major_fun[row,1], approx_fun_major$y)))
                            {
                                #print("unequal")
                                
                                median_number <- round(median(match(maxima_major_fun[row,2], approx_fun_major$x), match(maxima_major_fun[row,1], approx_fun_major$y), na.rm = TRUE), digits = 0)
                                
                                what_point_is_it_x[[i]][wpi_c]           <- df_list[[i]][p+median_number, body_part_x]
                                what_point_is_it_y[[i]][wpi_c]           <- df_list[[i]][p+median_number, body_part_y]
                                at_what_frames_are_the_slips[[i]][wpi_c] <- df_list[[i]][p+median_number, frame_c]
                                
                            } else 
                            {
                                #the points obviously don't match up, because it is an approximated function against the real whacky coordinates
                                #print(paste("x of point", df_list[[i]][p+match(maxima_major_fun[row,2], approx_fun_major$x), body_part_x], "y of point", df_list[[i]][p+match(maxima_major_fun[row,1], approx_fun_major$y), body_part_y], "frame of point", df_list[[i]][p+match(maxima_major_fun[row,1], approx_fun_major$y), frame_c]))
                                
                                what_point_is_it_x[[i]][wpi_c]           <- df_list[[i]][p+match(maxima_major_fun[row,2], approx_fun_major$x), body_part_x]
                                what_point_is_it_y[[i]][wpi_c]           <- df_list[[i]][p+match(maxima_major_fun[row,1], approx_fun_major$y), body_part_y]
                                at_what_frames_are_the_slips[[i]][wpi_c] <- df_list[[i]][p+match(maxima_major_fun[row,1], approx_fun_major$y), frame_c]
                            }
                            
                            type_of_slip[[i]][wpi_c] <- "major"
                            wpi_c <- wpi_c + 1
                        }
                        
                        ###New Block End 4###
                        
                    } else {
                        #Cases that have only one or two maxima
                        major_slip_count <- major_slip_count + 1 #Maybe separate here further.
                        #print(paste("just slip count", major_slip_count))
                        
                        ###New Block Major###
                        median_x_position <- median(df_list[[i]][p:(p+match(NA, major_slip_points[p:length(major_slip_points[,1]),1])-2), body_part_x], na.rm = TRUE)
                        median_y_position <- median(df_list[[i]][p:(p+match(NA, major_slip_points[p:length(major_slip_points[,1]),1])-2), body_part_y], na.rm = TRUE)
                        median_frame      <- round(median(df_list[[i]][p:(p+match(NA, major_slip_points[p:length(major_slip_points[,1]),1])-2), frame_c], na.rm = TRUE), digits = 0)
                        
                        what_point_is_it_x[[i]][wpi_c]           <- median_x_position
                        what_point_is_it_y[[i]][wpi_c]           <- median_y_position
                        at_what_frames_are_the_slips[[i]][wpi_c] <- median_frame
                        
                        type_of_slip[[i]][wpi_c] <- "major"
                        wpi_c <- wpi_c + 1
                        ###New Block Major###
                        
                        #No next here for visualisation
                    }
                    
                } else 
                {
                    #print("else, were no slips could be found")
                    
                    major_slip_count <- major_slip_count + 1
                    #Cases were no slip points could be found (but may be, so further separation might be needed)
                    #Cases were no slip points could be found, try a looser search and then maybe count
                    
                    
                    ###New Block Major###
                    median_x_position <- median(df_list[[i]][p:(p+match(NA, major_slip_points[p:length(major_slip_points[,1]),1])-2), body_part_x], na.rm = TRUE)
                    median_y_position <- median(df_list[[i]][p:(p+match(NA, major_slip_points[p:length(major_slip_points[,1]),1])-2), body_part_y], na.rm = TRUE)
                    median_frame      <- round(median(df_list[[i]][p:(p+match(NA, major_slip_points[p:length(major_slip_points[,1]),1])-2), frame_c], na.rm = TRUE), digits = 0)
                    
                    what_point_is_it_x[[i]][wpi_c]           <- median_x_position
                    what_point_is_it_y[[i]][wpi_c]           <- median_y_position
                    at_what_frames_are_the_slips[[i]][wpi_c] <- median_frame
                    
                    type_of_slip[[i]][wpi_c] <- "major"
                    wpi_c <- wpi_c + 1
                    ###New Block Major###
                    
                    #next #p = p + 1 #If no further separation is done
                }
                
                #For naming the graphs
                graph_number_major <- graph_number_major + 1
                
                if(visualize_process == TRUE || visualize_process_major == TRUE){
                    #Graphical representation of all major slips
                    try(plot(major_slip_points[p:(p+match(NA, major_slip_points[p:length(major_slip_points[,1]),1])-2),2], major_slip_points[p:(p+match(NA, major_slip_points[p:length(major_slip_points[,1]),1])-2),1], xlab = "Body part x-position", ylab = "Body part y-position", main = paste("Major slip detection",graph_number_major, "for", names(df_list[i])), type = "l"), silent = TRUE)
                    try(points(minima_major[,2],minima_major[,1], col = "dark red"),silent = TRUE)
                    try(points(maxima_major[,2],maxima_major[,1], col = "dark blue"),silent = TRUE)
                    try(legend("bottomleft", paste("Major Slips:", major_slip_count), bty = "n", horiz = TRUE),silent = TRUE)
                }
                
                #p = p + match(NA, major_slip_points[p:length(major_slip_points[,1]),1]) #for example: match(NA, major_slip_points[393:length(major_slip_points[,1]),1]) returns 6, because this is the next NA value at entry 398, so p will skip to 399 by addition
                iterations_to_skip_major_slip <- match(NA, major_slip_points[p:length(major_slip_points[,1]),1]) - 2 #Workaround, because you can't modify for-loop integers like this. -2 because e.g.: entry 393 is counted by match as well as the step to 398. So to skip the accurate number 2 needs to be removed. 
                #print(paste("here",p, "with", iterations_to_skip_major_slip,"skips"))
                next #p = p + 1
                
            } else if(!is.na(major_slip_points[p-1, 1]) && !is.na(major_slip_points[p, 1]) && !is.na(major_slip_points[p, 2]) && !is.na(match(NA, major_slip_points[p:length(major_slip_points[,1]),1])-2)) #In case it directly starts with a major slip
            {
                #In case it directly starts with a major slip
                
                #Honestly I would just like to use labels here to jump to the correct parts of the script, but that's not possible in R.
                
                #major_slip_count <- major_slip_count + 1
                #iterations_to_skip_major_slip <- match(NA, major_slip_points[p-1:length(major_slip_points[,1]),1]) - 2
                
                major_slip_points[p, 1] <- NA
                #print(paste("There seems to be the special case in major slip detection.", names(df_list[i]), "with p", p))
                next #p = p + 1
                
            } else if(is.na(match(NA, major_slip_points[p:length(major_slip_points[,1]),1])-2))
            {
                iterations_to_skip_major_slip <- iterations_to_skip_major_slip + (length(minor_slip_points[,1]) - p)
                #print(paste("Special case: match() returns NA at p", p, "for major slip detection in data frame", paste0(gsub(pattern = "`|L", "", deparse(substitute(df_list[[i]], env = environment()))), ","), "which is", names(df_list[i]), "- This case with it's", iterations_to_skip_major_slip, "iterations will be skipped."))
                #These special case messages can be quite common, idk why the match is NA at some points, but it does not seem to effect accuracy.
                next #p = p + 1
                
            } else
            {
                
                #All other cases that should not be, but still can be.
                
                print(paste("Something is not quite right at major slip detection: p =", p, "in data frame", paste0(gsub(pattern = "`|L", "", deparse(substitute(df_list[[i]], env = environment()))), ","), "which is", paste0(names(df_list[i]), ","), "at graph number", graph_number_major + 1))
                next #p = p + 1
                
            }
            
            next #p = p + 1
        } #End of the major slip detection for-loop
            
        major_slip_list[i] <- major_slip_count
        
        
        #Create result graphics
        if(visualize_results == TRUE)
        {
            #Plot for new detection
            try(plot(df_list[[i]][,body_part_x], df_list[[i]][,body_part_y], xlab = "Body part x-position", ylab = "Body part y-position", main = names(df_list[i]), type = "l", ylim = rev(range(df_list[[i]][,body_part_y], na.rm = TRUE)),xlim = c(0, length(resolution[[1]]))), silent = TRUE)
            
            
            points(minor_slip_points[,2],minor_slip_points[,1], col = "orange")
            points(major_slip_points[,2],major_slip_points[,1], col = "dark red")
            
            lines(line_of_top_beam[,2], line_of_top_beam[,1], col = "grey")
            lines(line_of_bottom_beam[,2], line_of_bottom_beam[,1], col = "light grey")
            
            #Draw inner markings
            if(length(df_list[[i]][,frame_c]) >= length(resolution[[2]]) && length(df_list[[i]][,frame_c]) >= length(resolution[[1]]))
            {
                lines(ml_longitudinal,df_list[[i]][,frame_c], col = "green")
                lines(mr_longitudinal,df_list[[i]][,frame_c], col = "yellow")
                
                #lines(df_list[[i]][,frame_c], ml_latitudinal, col = "green")
                #lines(df_list[[i]][,frame_c], mr_latitudinal, col = "yellow")
                
            } else if(length(df_list[[i]][,frame_c]) >= length(resolution[[2]])) 
            {
                lines(ml_longitudinal,df_list[[i]][,frame_c], col = "green")
                lines(mr_longitudinal,df_list[[i]][,frame_c], col = "yellow")
                
                #lines(resolution[[1]], ml_latitudinal, col = "green")
                #lines(resolution[[1]], mr_latitudinal, col = "yellow")
                
            } else if(length(df_list[[i]][,frame_c]) >= length(resolution[[1]])) 
            {
                lines(ml_longitudinal,resolution[[2]], col = "green")
                lines(mr_longitudinal,resolution[[2]], col = "yellow")
                
                #lines(df_list[[i]][,frame_c], ml_latitudinal, col = "green")
                #lines(df_list[[i]][,frame_c], mr_latitudinal, col = "yellow")
                
            } else 
            {
                lines(ml_longitudinal,resolution[[2]], col = "green")
                lines(mr_longitudinal,resolution[[2]], col = "yellow")
                
                #lines(resolution[[1]], ml_latitudinal, col = "green")
                #lines(resolution[[1]], mr_latitudinal, col = "yellow")
            }
            
            legend("bottomleft", paste("Minor Slips:", minor_slip_list[i], " Major Slips:", major_slip_list[i]), bty = "n", horiz = TRUE)
            
            
            if(try_reverse_ratio_plotting == TRUE)
            {
                try(plot(rev(reverse_ratio[[i]]),type = "l", main = paste("reverse ratio body part", names(df_list)[i]), xlab = "x-position", ylab = "percentile above/below beam"), silent = TRUE) #df_list[[i]][,frame_c], 
                abline(h = 1)
                abline(h = -50, col = "red")
                abline(h = -100, col = "grey")
                
                try(plot(rev(reverse_ratio_blp[[i]]),type = "l", main = paste("reverse ratio back left paw", names(df_list)[i]), xlab = "x-position", ylab = "percentile above/below beam"), silent = TRUE) #df_list[[i]][,frame_c], 
                abline(h = 1)
                abline(h = -50, col = "red")
                abline(h = -100, col = "grey")
                try(plot(rev(reverse_ratio_flp[[i]]),type = "l", main = paste("reverse ratio front left paw", names(df_list)[i]), xlab = "x-position", ylab = "percentile above/below beam"), silent = TRUE) #df_list[[i]][,frame_c], 
                abline(h = 1)
                abline(h = -50, col = "red")
                abline(h = -100, col = "grey")
                try(plot(rev(reverse_ratio_bel[[i]]),type = "l", main = paste("reverse ratio belly", names(df_list)[i]), xlab = "x-position", ylab = "percentile above/below beam"), silent = TRUE) #df_list[[i]][,frame_c], 
                abline(h = 1)
                abline(h = -50, col = "red")
                abline(h = -100, col = "grey")
                try(plot(rev(reverse_ratio_bac[[i]]),type = "l", main = paste("reverse ratio back", names(df_list)[i]), xlab = "x-position", ylab = "percentile above/below beam"), silent = TRUE) #df_list[[i]][,frame_c], 
                abline(h = 1)
                abline(h = -50, col = "red")
                abline(h = -100, col = "grey")
                try(plot(rev(reverse_ratio_snt[[i]]),type = "l", main = paste("reverse ratio snout", names(df_list)[i]), xlab = "x-position", ylab = "percentile above/below beam"), silent = TRUE) #df_list[[i]][,frame_c], 
                abline(h = 1)
                abline(h = -50, col = "red")
                abline(h = -100, col = "grey")
                try(plot(rev(reverse_ratio_le[[i]]),type = "l", main = paste("reverse ratio left eye", names(df_list)[i]), xlab = "x-position", ylab = "percentile above/below beam"), silent = TRUE) #df_list[[i]][,frame_c], 
                abline(h = 1)
                abline(h = -50, col = "red")
                abline(h = -100, col = "grey")
                try(plot(rev(reverse_ratio_tb[[i]]),type = "l", main = paste("reverse ratio tail base", names(df_list)[i]), xlab = "x-position", ylab = "percentile above/below beam"), silent = TRUE) #df_list[[i]][,frame_c], 
                abline(h = 1)
                abline(h = -50, col = "red")
                abline(h = -100, col = "grey")
                try(plot(rev(reverse_ratio_tc[[i]]),type = "l", main = paste("reverse ratio tail center", names(df_list)[i]), xlab = "x-position", ylab = "percentile above/below beam"), silent = TRUE) #df_list[[i]][,frame_c], 
                abline(h = 1)
                abline(h = -50, col = "red")
                abline(h = -100, col = "grey")
                try(plot(rev(reverse_ratio_tt[[i]]),type = "l", main = paste("reverse ratio tail tip", names(df_list)[i]), xlab = "x-position", ylab = "percentile above/below beam"), silent = TRUE) #df_list[[i]][,frame_c], 
                abline(h = 1)
                abline(h = -50, col = "red")
                abline(h = -100, col = "grey")
            }
        }
            
        #mean_list values
        left_eye_mean[i] <- mean(df_list[[i]][,left_eye_y], na.rm = TRUE)
        snout_mean[i] <- mean(df_list[[i]][,snout_y], na.rm = TRUE)
        back_mean[i] <- mean(df_list[[i]][,back_y], na.rm = TRUE)
        belly_mean[i] <- mean(df_list[[i]][,belly_y], na.rm = TRUE)
        tail_base_mean[i] <- mean(df_list[[i]][,tail_base_y], na.rm = TRUE)
        tail_center_mean[i] <- mean(df_list[[i]][,tail_center_y], na.rm = TRUE)
        
        tail_tip_mean[i] <- mean(df_list[[i]][,tail_center_y], na.rm = TRUE)
        
        left_eye_mean_cm[i] <- left_eye_mean[i]/pixel_to_cm_scale
        snout_mean_cm[i] <- snout_mean[i]/pixel_to_cm_scale
        back_mean_cm[i] <- back_mean[i]/pixel_to_cm_scale
        belly_mean_cm[i] <- belly_mean[i]/pixel_to_cm_scale
        tail_base_mean_cm[i] <- tail_base_mean[i]/pixel_to_cm_scale
        tail_center_mean_cm[i] <- tail_center_mean[i]/pixel_to_cm_scale
        tail_tip_mean_cm[i] <- tail_tip_mean[i]/pixel_to_cm_scale
        
        
        slip_position_dataframes[[i]] <- cbind(unlist(type_of_slip[[i]]) , as.numeric(unlist(at_what_frames_are_the_slips[[i]])), as.numeric(unlist(what_point_is_it_x[[i]])), as.numeric(unlist(what_point_is_it_y[[i]] )))
        slip_position_dataframes[[i]] <- data.frame(slip_position_dataframes[[i]])
        colnames(slip_position_dataframes[[i]]) <- c("type of slip", "frame of slip", "x position of slip", "y position of slip")
        
        
        #Be vocal about the results
        if(be_vocal == TRUE)
        {
            print(paste(paste0(gsub(pattern = "`|L", "", deparse(substitute(df_list[[i]], env = environment()))), ","), "which is", paste0(names(df_list[i]), ","), "was calculated and has:", minor_slip_count, "minor slip(s), and", major_slip_count, "major slips."))
        }
        
        
        length_of_trial[i] <- trial_length
        frames_bbt[i] <- frames_below_top_beam
        
        next #i = i + 1
    } #End of the df_list for-loop
        
    aa <- 0L #Used to create the full_detection_list
    ab <- 0L #Used for reverse_ratio list
        

    area_of_mouse_table_sub <- data.frame(unlist(lapply(area_of_mouse_table, "[", "Area of Mouse in square Centimeters")))
    
    area_of_mouse_table <- data.frame(cbind(names(df_list), area_of_mouse_table_sub, unlist(lapply(area_of_mouse_table, "[", "Area of Mouse in square Pixels")), unlist(lapply(area_of_mouse_table, "[", "Pixel to Centimeter Scale")), unlist(lapply(area_of_mouse_table, "[", "a (snt_bel_dist)")), unlist(lapply(area_of_mouse_table, "[", "b (bel_tb_dist)")), unlist(lapply(area_of_mouse_table, "[", "c (tb_bac_dist)")), unlist(lapply(area_of_mouse_table, "[", "d (bac_snt_dist)")), unlist(lapply(area_of_mouse_table, "[", "e (snt_tb_dist)")), unlist(lapply(area_of_mouse_table, "[", "f (bel_bac_dist)"))))
    
    rownames(area_of_mouse_table) <- NULL
    
    colnames(area_of_mouse_table) <- c("Trial", "Area of Mouse in square Centimeters", "Area of Mouse in square Pixels", "Pixel to Centimeter Scale", "a (snt_bel_dist)", "b (bel_tb_dist)", "c (tb_bac_dist)", "d (bac_snt_dist)", "e (snt_tb_dist)", "f (bel_bac_dist)")
    
    not_full_detection_list <- data.frame(cbind(as.numeric(minor_slip_list), as.numeric(major_slip_list), as.numeric(length_of_trial), as.numeric(frames_bbt))) #Yeah, I know it is not a list, but leave me be
    colnames(not_full_detection_list) <- c("Number of Minor Slips", "Number of Major Slips", "Length of Trial [in frames]", "Total Frames below Top of Beam")
    
    
    for(aa in 1:length(df_list))
    {
        if(!isTRUE(gsub(pattern = "_.*" ,"", gsub(pattern = "\\_\\(.*?\\d\\)", "", names(df_list[aa]))) %in% table$`Mouse Number`))
        {
            print(paste("No matching mouse number inside the table could be found for the mouse with the number:", gsub(pattern = "_.*" ,"", gsub(pattern = "\\_\\(.*?\\d\\)", "", names(df_list[aa]))), "- Please check this number, because it will be skipped here."))
            next #aa = aa + 1
        }
        
        if(aa == 1){
            
            #names(df_list[aa]),
            full_detection_list <- data.frame(cbind(data.frame(attributes_list[aa]), data.frame(not_full_detection_list[aa,])))
            colnames(full_detection_list) <- col_names
            
        } else {
            why_1 <- data.frame(attributes_list[aa])
            colnames(why_1) <- col_names[1:ncol(why_1)]
            
            why_2 <- data.frame(not_full_detection_list[aa,])
            colnames(why_2) <- col_names[(ncol(why_1)+1):sum(ncol(why_1),ncol(why_2))]
            
            full_detection_list <- rbind(full_detection_list, cbind(why_1, why_2))
        }
        next #aa = aa + 1
    }
    
    
    mean_list <- cbind(cbind(names(df_list)), full_detection_list$Group, full_detection_list$Subgroup, left_eye_mean,snout_mean,back_mean,belly_mean,tail_center_mean,tail_base_mean, tail_tip_mean)
    colnames(mean_list)[1:3] <- c("Trial", "Group", "Subgroup")
    mean_list <- as.data.frame(mean_list)
    mean_list_cm <- cbind(cbind(names(df_list)), full_detection_list$Group, full_detection_list$Subgroup, left_eye_mean_cm,snout_mean_cm,back_mean_cm,belly_mean_cm,tail_center_mean_cm,tail_base_mean_cm, tail_tip_mean_cm)
    colnames(mean_list_cm)[1:3] <- c("Trial", "Group", "Subgroup")
    
    #Initialize names (not entirely necessary and only done just in case)
    try(names(reverse_ratio) <- names(df_list), silent = TRUE)
    
    try(names(reverse_ratio_blp) <- names(df_list), silent = TRUE)
    try(names(reverse_ratio_flp) <- names(df_list), silent = TRUE)
    try(names(reverse_ratio_bel) <- names(df_list), silent = TRUE)
    try(names(reverse_ratio_bac) <- names(df_list), silent = TRUE)
    try(names(reverse_ratio_snt) <- names(df_list), silent = TRUE)
    try(names(reverse_ratio_le) <- names(df_list), silent = TRUE)
    try(names(reverse_ratio_tb) <- names(df_list), silent = TRUE)
    try(names(reverse_ratio_tc) <- names(df_list), silent = TRUE)
    try(names(reverse_ratio_tt) <- names(df_list), silent = TRUE)
    
    try(names(tb_to_tt_angle) <- names(df_list), silent = TRUE)
    
    #Give the variables their proper names
    for(ab in 1:length(df_list))
    {
        try(names(reverse_ratio)[ab] <- paste0("Trial_", names(df_list)[ab], " Group_", table$Group[ab]," Subgroup_",table$Subgroup[ab]), silent = TRUE)
        
        try(names(reverse_ratio_blp)[ab] <- paste0("Trial_", names(df_list)[ab], " Group_", table$Group[ab]," Subgroup_", table$Subgroup[ab]), silent = TRUE)
        try(names(reverse_ratio_flp)[ab] <- paste0("Trial_", names(df_list)[ab], " Group_", table$Group[ab]," Subgroup_", table$Subgroup[ab]), silent = TRUE)
        try(names(reverse_ratio_bel)[ab] <- paste0("Trial_", names(df_list)[ab], " Group_", table$Group[ab]," Subgroup_", table$Subgroup[ab]), silent = TRUE)
        try(names(reverse_ratio_bac)[ab] <- paste0("Trial_", names(df_list)[ab], " Group_", table$Group[ab]," Subgroup_", table$Subgroup[ab]), silent = TRUE)
        try(names(reverse_ratio_snt)[ab] <- paste0("Trial_", names(df_list)[ab], " Group_", table$Group[ab]," Subgroup_", table$Subgroup[ab]), silent = TRUE)
        try(names(reverse_ratio_le)[ab]  <- paste0("Trial_", names(df_list)[ab], " Group_", table$Group[ab]," Subgroup_", table$Subgroup[ab]), silent = TRUE)
        try(names(reverse_ratio_tb)[ab]  <- paste0("Trial_", names(df_list)[ab], " Group_", table$Group[ab]," Subgroup_", table$Subgroup[ab]), silent = TRUE)
        try(names(reverse_ratio_tc)[ab]  <- paste0("Trial_", names(df_list)[ab], " Group_", table$Group[ab]," Subgroup_", table$Subgroup[ab]), silent = TRUE)
        try(names(reverse_ratio_tt)[ab]  <- paste0("Trial_", names(df_list)[ab], " Group_", table$Group[ab]," Subgroup_", table$Subgroup[ab]), silent = TRUE)
        
        try(names(tb_to_tt_angle)[ab]  <- paste0("Trial_", names(df_list)[ab], " Group_", table$Group[ab]," Subgroup_",table$Subgroup[ab]), silent = TRUE)
        
        try(names(segment_duration_dataframes)[ab]  <- paste0("Trial_", names(df_list)[ab], " Group_", table$Group[ab]," Subgroup_",table$Subgroup[ab]), silent = TRUE)
        try(names(slip_position_dataframes)[ab]    <- paste0("Trial_", names(df_list)[ab], " Group_", table$Group[ab]," Subgroup_",table$Subgroup[ab]), silent = TRUE)
        
        next #ab = ab+1
        
        #" Date_of_Recording_", dates[ab], -> This was between Trial_ and Group_ and was removed because dates does not exist anymore as a global variable because it gets removed during initialization in the rm() line.
    }

    if(save_files == TRUE)
    {
        write.csv(full_detection_list, file = paste0(directory, "\\", "slip_detection_list", ".csv"))
        
        
        write.csv(mean_list, file = paste0(directory, "\\", "mean_list", ".csv"))
        write.csv(mean_list_cm, file = paste0(directory, "\\", "mean_list_cm", ".csv"))
        
        write.csv(area_of_mouse_table, file = paste0(directory, "\\", "area_of_mouse_table", ".csv"))
        
        saveRDS(reverse_ratio, file = "revese_ratio.RDS", compress = FALSE)
        saveRDS(reverse_ratio_blp, file = "revese_ratio_blp.RDS", compress = FALSE)
        saveRDS(reverse_ratio_flp, file = "revese_ratio_flp.RDS", compress = FALSE)
        saveRDS(reverse_ratio_bel, file = "revese_ratio_bel.RDS", compress = FALSE)
        saveRDS(reverse_ratio_bac, file = "revese_ratio_bac.RDS", compress = FALSE)
        saveRDS(reverse_ratio_snt, file = "revese_ratio_snt.RDS", compress = FALSE)
        saveRDS(reverse_ratio_le, file = "revese_ratio_le.RDS", compress = FALSE)
        saveRDS(reverse_ratio_tb, file = "revese_ratio_tb.RDS", compress = FALSE)
        saveRDS(reverse_ratio_tc, file = "revese_ratio_tc.RDS", compress = FALSE)
        saveRDS(reverse_ratio_tt, file = "revese_ratio_tt.RDS", compress = FALSE)
        
        saveRDS(instantaneous_velocity_belly, file = "instantaneous_velocity_belly.RDS", compress = FALSE)
        saveRDS(instantaneous_velocity_back, file = "instantaneous_velocity_back.RDS", compress = FALSE)
        saveRDS(instantaneous_velocity_tail_base, file = "instantaneous_velocity_tail_base.RDS", compress = FALSE)
        
        saveRDS(stationary_belly, file = "stationary_belly.RDS", compress = FALSE)
        saveRDS(stationary_back, file = "stationary_back.RDS", compress = FALSE)
        saveRDS(stationary_tail_base, file = "stationary_tail_base.RDS", compress = FALSE)
        
        saveRDS(stationary, file = "stationary.RDS", compress = FALSE)
        
        saveRDS(tb_to_tt_angle, file = "tb_to_tt_angle.RDS", compress = FALSE)
        
        saveRDS(segment_duration_dataframes, file = "segment_duration_dataframes.RDS", compress = FALSE)
        saveRDS(slip_position_dataframes, file = "slip_position_dataframes.RDS", compress = FALSE)
        
        
        #All saved
        print(paste("Saved the results successfully in directory:", directory))
        
        
        if(directly_read_files == TRUE)
        {
            setwd(wd)
            read_all_files_into_environment(directory)
            setwd(wd)
        }
        
    } else {
        
        if(directly_read_files == TRUE)
        {
            print("Cannot directly read files if none are created. Please set: save_files = TRUE")
        }
    }
} #End of function slip_detection



gg_box_plots_after_slip_detection <- function(slip_table, table, take_mean_of_mice, paired_data = NULL, normal_distribution = NULL, save_as_filetype = NULL, directory = NULL, reverse_y_axis = FALSE, plot_group_comparison = NULL, list_all_groups = NULL, custom_breaks_y_axis = NULL, multiple_correction_method = NULL, only_plot_comparing_boxplots = NULL)
{

    if(!isTRUE("ggplot2" %in% (.packages())))
    {
        if(!require(ggplot2))
        {
            install.packages(ggplot2, dep=TRUE, quiet = TRUE)
        }
        
        suppressPackageStartupMessages(library(ggplot2))
    }
    
    if(!isTRUE("ggpubr" %in% (.packages())))
    {
        if(!require(ggpubr))
        {
            install.packages(ggpubr, dep=TRUE, quiet = TRUE)
        }
        
        suppressPackageStartupMessages(library(ggpubr))
    }
    
    if(identical(as.integer(which(lapply(slip_table, is.numeric) == TRUE)), integer(0)))
    {
        print("No columns in the given dataframe is numeric. Please change that. This function will do nothing otherwise. Aborting function call.")
        stop()
    }
    
    if(!identical(take_mean_of_mice, TRUE) && !identical(take_mean_of_mice, FALSE))
    {
        print("take_mean_of_mice must be either TRUE or FALSE. It determines if the N or the n should be plotted. By default this will be TRUE which mean the N will be plotted.")
        take_mean_of_mice <- TRUE
    }
    if(is.null(paired_data))
    {
        paired_data <- FALSE
    }
    if(is.null(normal_distribution))
    {
        normal_distribution <- TRUE
    }
    if(is.null(only_plot_comparing_boxplots))
    {
        only_plot_comparing_boxplots <- TRUE
    }
    
    if(!is.null(custom_breaks_y_axis))
    {
        if(typeof(custom_breaks_y_axis ) == "double" || typeof(custom_breaks_y_axis ) == "integer")
        {
            custom_breaks_y_axis  <- custom_breaks_y_axis [!sapply(custom_breaks_y_axis ,is.null)]
            custom_breaks_y_axis  <- custom_breaks_y_axis [!sapply(custom_breaks_y_axis ,is.na)]
            custom_breaks_y_axis  <- custom_breaks_y_axis [!sapply(custom_breaks_y_axis ,is.nan)]
            custom_breaks_y_axis  <- custom_breaks_y_axis [!sapply(custom_breaks_y_axis ,is.infinite)]
            
            if(identical(custom_breaks_y_axis , numeric(0)))
            {
                print("custom_breaks_y_axis  is numeric(0) and is therefore set to NULL. Please make sure it is a vector containing double or integer values like c(0, 2, 4, 8, 10) or seq(0, 10, 2)")
                custom_breaks_on_y_axis  <- NULL
                custom_breaks_y_axis  <- NULL
                
                pretty <- scale_y_continuous(breaks = ~round(unique(pretty(.))))
                
            } else {
                custom_breaks_on_y_axis  <- scale_y_continuous(breaks = custom_breaks_y_axis)
                
                pretty <- NULL
            }
            
        } else {
            print("Unrecognized type of breaks, setting custom_breaks_y_axis  to NULL. Expected a vector containing double or integer values like c(0, 2, 4, 8, 10) or seq(0, 10, 2)")
            custom_breaks_on_y_axis  <- NULL
            custom_breaks_y_axis <- NULL
            
            pretty <- scale_y_continuous(breaks = ~round(unique(pretty(.))))
        }
    } else {
        custom_breaks_on_y_axis <- NULL
        
        pretty <- scale_y_continuous(breaks = ~round(unique(pretty(.))))
    }
    
    if(!is.null(save_as_filetype))
    {
        if(is.character(save_as_filetype))
        {
            if(!grepl(pattern = "^\\.", save_as_filetype))
            {
                print("save_as_filetype must start with a . before the file type to be save like save_as_filetype = \".png\"")
                
                save_as_filetype <- gsub(pattern = "^", ".", save_as_filetype)
                
                print(paste("Hence it is now changed to:", save_as_filetype))
            }
            
            if(identical(save_as_filetype, ".svg"))
            {
                if(!isTRUE("svglite" %in% (.packages())))
                {
                    if(!require(svglite))
                    {
                        install.packages(svglite, dep=TRUE, quiet = TRUE)
                    }
                    suppressPackageStartupMessages(library(svglite))
                }
            }
            
            file_type <- save_as_filetype
            
            save_as_filetype <- TRUE
            
        } else {
            print(paste("save_as_filetype must be a string of characters with a dot in front indicating the file type saved like save_as_filetype = \".png\""))
            stop()
        }
        
    } else {
        
        save_as_filetype <- FALSE
    }
    
    if(is.null(plot_group_comparison))
    {
        plot_group_comparison <- TRUE
    }
    
    if(is.null(list_all_groups))
    {
        list_all_groups <- FALSE
    }
    
    if(normal_distribution == TRUE)
    {
        test_method <- "t.test"
    } else {
        test_method <- "wilcox.test"
    }
    
    if(isTRUE(reverse_y_axis))
    {
        reversed <- scale_y_reverse()
    } else {
        reversed <- NULL
    }

    if(is.null(multiple_correction_method))
    {
        multiple_correction_method <- "none"
    } else if(any(multiple_correction_method == c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr")))
    {
        #correct
    } else {
        print("multiple_correction_method must be one of the following methods: \"holm\", \"hochberg\", \"hommel\", \"bonferroni\", \"BH\", \"BY\",\"fdr\". Otherwise it will be set to \"none\".")
        multiple_correction_method <- "none"
    }
    
    
    if(isTRUE(list_all_groups))
    {
        count <- 0L
        
        all_groups_list <- list()
    }
    
    if(isTRUE(save_as_filetype))
    {
        wd <- getwd()
        wd <- gsub(pattern = "/","\\", wd, fixed = TRUE)
        
        if(is.null(directory))
        {
            directory <- choose.dir( caption = "No directory was given. Please choose a directory to save the results. Just close this window, if the result should be saved in the current working directory.")
            
            if(is.null(directory) || is.na(directory))
            {
                directory <- wd
            } else {
                directory <- gsub(pattern = "/","\\", directory, fixed = TRUE)
            }
        }
        
        setwd(directory)
        
        ifelse(!dir.exists("Boxplots"), dir.create("Boxplots"), "Folder exists already")
        
        setwd(paste0(directory, "\\Boxplots"))
        #ifelse(!dir.exists("Subgroup_Comparison"), dir.create("Subgroup_Comparison"), "Folder exists already")
    } else {
        wd <- getwd()
    }
    
    #theme <- theme_bw() %+replace% theme(axis.title = element_text(size = rel(1.1), face = "bold"), plot.title = element_text(size = rel(1.5), face = "bold", margin = margin(0,0,10,0), hjust = 0.5), axis.text = element_text(size = rel(1.10), face = "bold"), legend.title = element_text(size = rel(1.05), face = "bold"), legend.text = element_text(size = rel(0.95), face = "bold"))
    
    #sans is TT Arial, serif is TT Times New Roman, mono is TT Courier New, these are the default windowsFonts without additional library
    theme <- theme_classic() %+replace% theme(text = element_text(family = "sans"), panel.grid.minor = element_blank(), panel.grid.major = element_blank(), axis.title = element_text(size = rel(1.3), face = "bold"), plot.title = element_text(size = rel(1.8), face = "bold", margin = margin(0,0,10,0), hjust = 0.5), axis.text = element_text(size = rel(1.2), face = "bold"), legend.title = element_text(size = rel(1.3), face = "bold"), legend.text = element_text(size = rel(1), face = "bold"))
    
    
    subgroups <- unique(slip_table$Subgroup)
    groups <- unique(slip_table$`Group`)
    
    counter_box_plots <- 0L
    subgroup_counter <- 0L
    group_counter <- 0L
    
    unique_mouse_numbers <- unique(slip_table$`Mouse Number`)
    
    for(check_mouse in length(unique_mouse_numbers))
    {
        if(identical(which(unique_mouse_numbers[check_mouse] == table$`Mouse Number`), integer(0)))
        {
            print(paste("Mouse Number", unique_mouse_numbers[check_mouse], "was not found in the main_table. Be aware of that. This function will continue using it's data."))
        }
    }
    
    value_groups <- NULL
    
    group_save <- vector(mode = "list", length = length(groups))
    
    longer_df <- NULL #Makes plot from all subgroups from all groups (is usd to generate longest_df)
    
    p_value_list <<- NULL

    
    ###Boxplot creation
    
    #Defines plotting area as one row and columns based on the length of subgroups. There is also the mar argument to specify the (bottom, left, top, right) margins for the plotting area. Default: mar = c(5.1, 4.1, 4.1, 2.1)
    for(counter_box_plots in 1:ncol(slip_table))
    {
        subgroup_counter <- 0L
        
        if(is.na(colnames(slip_table)[counter_box_plots]) || is.null(colnames(slip_table)[counter_box_plots]))
        {
            print(paste("Columns must be labelled something other than NA or NULL, because it would error out the function. Hence column", counter_box_plots, "will be skipped."))
            next #counter_box_plots = counter_box_plots + 1
        }
        
        if(is.numeric(unlist(slip_table[[counter_box_plots]])) && colnames(slip_table)[counter_box_plots] != "Mouse Number" && colnames(slip_table)[counter_box_plots] != "Trial Number" && colnames(slip_table)[counter_box_plots] != "Trials on that Date" && colnames(slip_table)[counter_box_plots] != "Total Trials" && colnames(slip_table)[counter_box_plots] != "Date of Recording")
        {
            
            par(mfrow = c(1,length(subgroups)), mar = c(5.1, 4.1, 4.1/2, 2.1/2))
            
            if(grepl("Frames", colnames(slip_table)[counter_box_plots]))
            {
                slip_table[counter_box_plots] <- sapply(slip_table[counter_box_plots], function(x){x/fps})
                
                colnames(slip_table)[counter_box_plots] <- c(gsub(pattern = "*Frames*", "Seconds", colnames(slip_table)[counter_box_plots]))
                
                y_axis_title <- c(gsub(pattern = "*Frames*", "Seconds", gsub(pattern = "_", " ", colnames(slip_table)[counter_box_plots])))
                
                plot_title <- c(gsub(pattern = "*Frames*", "Seconds", gsub(pattern = "_", " ", colnames(slip_table)[counter_box_plots])))
                
            } else {
                
                y_axis_title <- gsub(pattern = "_", " ", colnames(slip_table)[counter_box_plots])
                
                plot_title <- gsub(pattern = "_", " ", colnames(slip_table)[counter_box_plots])
            }
            
            #from here it is basically copied from the gg_reverse_ratio function, just slightly modified
            for(group_counter in 1:length(groups))
            {
                if(is.na(groups[group_counter]))
                {
                    group_save[[group_counter]] <- NULL
                    next #group_counter = group_counter +1
                }
                
                #The only way to get an "all is FALSE"
                if(!isTRUE(any(slip_table$Group == groups[group_counter])))
                {
                    group_save[[group_counter]] <- NULL
                    next #group_counter = group_counter +1
                }
                
                subgroup_save <- vector(mode = "list", length = length(subgroups))
                
                if(isTRUE(take_mean_of_mice))
                {
                    value_groups <- data.frame(cbind(slip_table[[counter_box_plots]][which(slip_table$Group == groups[group_counter])], slip_table$Group[which(slip_table$Group == groups[group_counter])], slip_table$Subgroup[which(slip_table$Group == groups[group_counter])], slip_table$`Mouse Number`[which(slip_table$Group == groups[group_counter])]))
                    
                    colnames(value_groups) <- c("y", "Group", "Subgroup", "Mouse Number")
                    
                    value_groups$y <- as.numeric(value_groups$y)
                } else {
                    value_groups <- data.frame(cbind(slip_table[[counter_box_plots]][which(slip_table$Group == groups[group_counter])], slip_table$Group[which(slip_table$Group == groups[group_counter])], slip_table$Subgroup[which(slip_table$Group == groups[group_counter])]))
                
                    colnames(value_groups) <- c(colnames(slip_table)[counter_box_plots], "Group", "Subgroup")
                }
                
                for(subgroup_counter in 1:length(subgroups))
                {
                    if(is.na(subgroups[subgroup_counter]))
                    {
                        subgroup_save[[subgroup_counter]] <- NULL
                        next #subgroup_counter = subgroup_counter +1
                    }
                    
                    #colours <- NULL
                    #colours2 <- NULL
                    
                    colour_coding_1 <- NULL
                    colour_coding_2 <- NULL
                    
                    value_subgroups <- subset(value_groups, value_groups$Subgroup == subgroups[subgroup_counter])
                    
                                        
                    if(is.null(value_subgroups) || sum(dim(value_subgroups)) == 0 || identical(value_subgroups, data.frame()) || nrow(value_subgroups) == 0)
                    {
                        #print("no matching subgroup")
                        #next #subgroup_counter = subgroup_counter +1
                        
                    } else {
                        
                        if(isTRUE(take_mean_of_mice))
                        {
                            subgroup_means <- NULL
                            mouse_numbers <- unique(value_subgroups$`Mouse Number`)
                            
                            for(mouse in 1:length(mouse_numbers))
                            {
                                subgroup_means[mouse] <- mean(value_subgroups$y[which(value_subgroups$`Mouse Number` == mouse_numbers[mouse])], na.rm =TRUE)
                            }
                            
                            value_subgroups <- value_subgroups[!duplicated(value_subgroups$`Mouse Number`),]
                            
                            value_subgroups$y <- as.numeric(subgroup_means)
                            
                            value_subgroups$`Mouse Number` <- NULL 
                        }
                        
                        value_subgroups$Group <- NULL 
                        
                        value_subgroups <- data.frame(value_subgroups)
                        
                        colnames(value_subgroups) <- c(colnames(slip_table)[counter_box_plots], "Subgroup")
                        
                        value_subgroups <- na.omit(value_subgroups)
                        
                        colnames(value_subgroups)[1] <- c("y")
                        
                        value_subgroups$y <- as.numeric(value_subgroups$y)
                        
                        value_subgroups$Subgroup <- as.factor(value_subgroups$Subgroup)
                        
                        subgroup_save[[subgroup_counter]] <- value_subgroups
                        
                        
                        if(!isTRUE(only_plot_comparing_boxplots))
                        {
                            dotplot_binwidth <- abs(max(value_subgroups$y)-min(value_subgroups$y))/30
                            
                            
                            plot_subgroup <- suppressWarnings(ggplot(data = value_subgroups, mapping = aes(x= `Subgroup`, y = `y`, group = `Subgroup`, fill = `Subgroup`)) + geom_boxplot(outlier.size = -1, width = 0.5) + theme + pretty + geom_dotplot(aes(fill = `Subgroup`), binaxis = "y", stackdir = "center", shape = 21, color = "black", drop = TRUE, na.rm = TRUE, method = "dotdensity", stroke = 1.5, dotsize = 0.8, binwidth = dotplot_binwidth) + labs(title = paste(plot_title, groups[group_counter]), x = NULL, y = y_axis_title) + reversed + custom_breaks_on_y_axis)
                            
                            suppressWarnings(print(plot_subgroup))
                            
                            
                            if(isTRUE(save_as_filetype))
                            {
                                try(suppressWarnings(ggsave(paste0(all_to_underscore(groups[group_counter]), "_", all_to_underscore(subgroups[subgroup_counter]), "_", all_to_underscore(colnames(slip_table)[counter_box_plots]), file_type), plot = plot_subgroup)), silent =  TRUE)
                            }
                        }
                    }
                    
                    if(subgroup_counter == length(subgroups))
                    {
                        subgroup_save <- subset(subgroup_save, lapply(subgroup_save, is.null) == FALSE)
                        
                        colours <- NULL
                        
                        if(!is.null(colours))
                        {
                            scale_fill <- scale_fill_manual(values = colours, na.translate = FALSE)   
                        } else {
                            scale_fill <- NULL
                        }
                        
                        
                        long_df <- NULL
                        
                        for(i in 1:length(subgroup_save))
                        {
                            if(i == 1)
                            {
                                long_df <- data.frame(subgroup_save[[i]])
                            } else {
                                long_df <- rbind(long_df, data.frame(subgroup_save[[i]]))
                            }
                        }
                        
                        colnames(long_df) <- c(colnames(slip_table)[counter_box_plots], "Subgroup")
                        
                        longer_df[[group_counter]] <- long_df
                        
                        longer_df[[group_counter]]$Subgroup <- cbind(gsub(pattern = "$", paste0("_", groups[group_counter]), unlist(longer_df[[group_counter]]$Subgroup)))
                        
                        #Needed  in the if-else down below
                        comparison_list <- list()
                        temp_comparison_list <- list()
                        ad <- 0L
                        
                        #idk sometimes R randomly recognizes it as a factor automatically, hence this line is needed
                        long_df$`Subgroup` <- as.character(long_df$`Subgroup`)
                        
                        #stat_compare_mean() needs reference groups in it's t-test to function in form of c("Name of Group", "Name of other Group"). This is done in this if-else
                        if(length(unique(long_df$`Subgroup`)) == 1)
                        {a
                            comparison_list <- c(unique(long_df$`Subgroup`))
                            
                        } else {
                            for(ad in 2:length(unique(long_df$`Subgroup`)))
                            {
                                if(ad >= 3){
                                    
                                    comparison_list[[ad]] <- c(unique(long_df$`Subgroup`)[ad-1], unique(long_df$`Subgroup`)[ad])
                                    
                                    #print(comparison_list[[ad]])
                                    
                                    temp_comparison_list[[ad]] <- c(unique(long_df$`Subgroup`)[1], unique(long_df$`Subgroup`)[ad])
                                } else {
                                    comparison_list[[ad]] <- c(unique(long_df$`Subgroup`)[ad-1], unique(long_df$`Subgroup`)[ad])
                                }
                                
                                #print(paste("c()", c(unique(long_df$`Subgroup`)[ad-1], unique(long_df$`Subgroup`)[ad])))
                                next #ad = ad + 1
                            }
                            comparison_list <- comparison_list[which(!sapply(comparison_list, is.null))] #Removes NULL in list
                            temp_comparison_list <- temp_comparison_list[which(!sapply(temp_comparison_list, is.null))]  #Removes NULL in list
                            
                            
                            comparison_list <- append(comparison_list, temp_comparison_list)
                            
                            #print(paste("comparison_list (full)", comparison_list))
                            #print(comparison_list)
                        }
                        
                        colnames(long_df)[1] <- c("y")
                        
                        long_df$y <- as.numeric(long_df$y)
                        
                        long_df$Subgroup <- as.factor(long_df$Subgroup)
                        
                        dotplot_binwidth <- abs(max(long_df$y)-min(long_df$y))/30
                        
                        
                        plot_subgroup <- suppressWarnings(ggplot(data = long_df, mapping = aes(x= `Subgroup`, y = `y`, group = `Subgroup`, fill = `Subgroup`)) + geom_boxplot(outlier.size = -1, width = 0.5) + stat_compare_means(label = "p.signif", method = test_method, paired = paired_data, comparisons = comparison_list, p.adjust.methods = multiple_correction_method) + stat_compare_means(label = "p.format", method = test_method, paired = paired_data, comparisons = comparison_list, vjust = 1.5, p.adjust.methods = multiple_correction_method) + theme + pretty + geom_dotplot(aes(fill = `Subgroup`), binaxis = "y", stackdir = "center", shape = 21, color = "black", drop = TRUE, na.rm = TRUE, method = "dotdensity", stroke = 1.5, dotsize = 0.8, binwidth = dotplot_binwidth) + labs(title = paste(plot_title, groups[group_counter], "all Subgroups"), x = NULL, y = y_axis_title) + reversed + custom_breaks_on_y_axis)
                        
                        suppressWarnings(print(plot_subgroup))
                        
                        if(isTRUE(save_as_filetype))
                        {
                            try(suppressWarnings(ggsave(paste0(all_to_underscore(groups[group_counter]), "_", all_to_underscore(colnames(slip_table)[counter_box_plots]), "_", "all_subgroups", file_type), plot = plot_subgroup)), silent =  TRUE)
                        }
                    }
                    
                    next #subgroup_counter = subgroup_counter + 1
                }#end of for-loop subgroup_counter
                
                
                if(isTRUE(take_mean_of_mice))
                {
                    group_means <- NULL
                    mouse_numbers <- unique(value_groups$`Mouse Number`)
                    
                    for(mouse in 1:length(mouse_numbers))
                    {
                        group_means[mouse] <- mean(value_groups$y[which(value_groups$`Mouse Number` == mouse_numbers[mouse])], na.rm =TRUE)
                    }
                    
                    value_groups <- value_groups[!duplicated(value_groups$`Mouse Number`),]
                    
                    #print(value_groups)
                    
                    value_groups$y <- as.numeric(group_means)
                    
                    #print(value_groups)
                    
                    value_groups$`Mouse Number` <- NULL 
                }
                
                value_groups$Subgroup <- NULL
                
                value_groups <- data.frame(value_groups) 
                
                value_groups <- na.omit(value_groups)
                
                colnames(value_groups)[1] <- c("y")
                
                value_groups$y <- as.numeric(value_groups$y)
                
                value_groups$Group <- as.factor(value_groups$Group)
                
                group_save[[group_counter]] <- value_groups
                
                if(!isTRUE(only_plot_comparing_boxplots))
                {
                    
                    dotplot_binwidth <- abs(max(value_groups$y)-min(value_groups$y))/30
                    
                    plot_group <- suppressWarnings(ggplot(data = value_groups, mapping = aes(x= `Group`, y = `y`, group = `Group`, fill = `Group`)) + geom_boxplot(outlier.size = -1, width = 0.5) + theme + pretty + geom_dotplot(aes(fill = `Group`), binaxis = "y", stackdir = "center", shape = 21, color = "black", drop = TRUE, na.rm = TRUE, method = "dotdensity", stroke = 1.5, dotsize = 0.8, binwidth = dotplot_binwidth) + labs(title = plot_title, x = NULL, y = y_axis_title) + reversed + custom_breaks_on_y_axis)
                    
                    suppressWarnings(print(plot_group))
                    
                    if(isTRUE(save_as_filetype))
                    {
                        try(suppressWarnings(ggsave(paste0(all_to_underscore(groups[group_counter]), "_", all_to_underscore(colnames(slip_table)[counter_box_plots]), file_type), plot = plot_group)), silent =  TRUE)
                    }   
                }
                
                if(group_counter == length(groups))
                {
                    group_save <- subset(group_save, lapply(group_save, is.null) == FALSE)
                    longer_df <- subset(longer_df, lapply(longer_df, is.null) == FALSE)
                    
                    long_df <- NULL
                    longest_df <- NULL #Uses longer_df from subgroups to plot all subgroups from all groups
                    
                    #print(longer_df)
                    
                    for(i in 1:length(group_save))
                    {
                        if(i == 1)
                        {
                            long_df <- data.frame(group_save[[i]])
                            longest_df <- data.frame(longer_df[[i]])
                        } else {
                            
                            long_df <- rbind(long_df, data.frame(group_save[[i]]))
                            longest_df <- rbind(longest_df, data.frame(longer_df[[i]]))   
                        }
                    }
                    
                    colnames(long_df) <- c(colnames(slip_table)[counter_box_plots], "Group")
                    colnames(longest_df) <- c(colnames(slip_table)[counter_box_plots], "Subgroup of Group")
                    
                    #print(long_df)
                    #print(longest_df)
                    
                    #Needed  in the if-else down below
                    comparison_list <- list()
                    temp_comparison_list <- list()
                    ad <- 0L
                    
                    #idk sometimes R randomly recognizes it as a factor automatically, hence this line is needed
                    long_df$`Group` <- as.character(long_df$`Group`)
                    
                    #stat_compare_mean() needs reference groups in it's t-test to function in form of c("Name of Group", "Name of other Group"). This is done in this if-else
                    if(length(unique(long_df$`Group`)) == 1)
                    {
                        comparison_list <- c(unique(long_df$`Group`))
                        
                    } else {
                        for(ad in 2:length(unique(long_df$`Group`)))
                        {
                            if(ad >= 3){
                                
                                comparison_list[[ad]] <- c(unique(long_df$`Group`)[ad-1], unique(long_df$`Group`)[ad])
                                
                                #print(comparison_list[[ad]])
                                
                                temp_comparison_list[[ad]] <- c(unique(long_df$`Group`)[1], unique(long_df$`Group`)[ad])
                            } else {
                                comparison_list[[ad]] <- c(unique(long_df$`Group`)[ad-1], unique(long_df$`Group`)[ad])
                            }
                            
                            #print(paste("c()", c(unique(long_df$`Group`)[ad-1], unique(long_df$`Group`)[ad])))
                            next #ad = ad + 1
                        }
                        comparison_list <- comparison_list[which(!sapply(comparison_list, is.null))] #Removes NULL in list
                        temp_comparison_list <- temp_comparison_list[which(!sapply(temp_comparison_list, is.null))]  #Removes NULL in list
                        
                        
                        comparison_list <- append(comparison_list, temp_comparison_list)
                        
                        #print(paste("comparison_list (full)", comparison_list))
                        #print(comparison_list)
                    }
                    
                    colnames(long_df)[1] <- c("y")
                    
                    long_df$y <- as.numeric(long_df$y)
                    
                    long_df$Group <- as.factor(long_df$Group)
                    
                    dotplot_binwidth <- abs(max(long_df$y)-min(long_df$y))/30
                    
                    #Needed  in the if-else down below
                    comparison_list_2 <- list()
                    temp_comparison_list_2 <- list()
                    ad <- 0L
                    
                    
                    #idk sometimes R randomly recognizes it as a factor automatically, hence this line is needed
                    longest_df$`Subgroup of Group` <- as.character(longest_df$`Subgroup of Group`)
                    
                    #stat_compare_mean() needs reference groups in it's t-test to function in form of c("Name of Group", "Name of other Group"). This is done in this if-else
                    if(length(unique(longest_df$`Subgroup of Group`)) == 1)
                    {
                        comparison_list_2 <- c(unique(longest_df$`Subgroup of Group`))
                        
                    } else {
                        for(ad in 2:length(unique(longest_df$`Subgroup of Group`)))
                        {
                            if(ad >= 3){
                                
                                comparison_list_2[[ad]] <- c(unique(longest_df$`Subgroup of Group`)[ad-1], unique(longest_df$`Subgroup of Group`)[ad])
                                
                                #print(comparison_list_2[[ad]])
                                
                                temp_comparison_list_2[[ad]] <- c(unique(longest_df$`Subgroup of Group`)[1], unique(longest_df$`Subgroup of Group`)[ad])
                            } else {
                                comparison_list_2[[ad]] <- c(unique(longest_df$`Subgroup of Group`)[ad-1], unique(longest_df$`Subgroup of Group`)[ad])
                            }
                            
                            #print(paste("c()", c(unique(longest_df$`Subgroup of Group`)[ad-1], unique(longest_df$`Subgroup of Group`)[ad])))
                            next #ad = ad + 1
                        }
                        comparison_list_2 <- comparison_list_2[which(!sapply(comparison_list_2, is.null))] #Removes NULL in list
                        temp_comparison_list_2 <- temp_comparison_list_2[which(!sapply(temp_comparison_list_2, is.null))]  #Removes NULL in list
                        
                        
                        comparison_list_2 <- append(comparison_list_2, temp_comparison_list_2)
                        
                        #print(paste("comparison_list_2 (full)", comparison_list_2))
                        #print(comparison_list_2)
                    }
                    
                    colnames(longest_df)[1] <- c("y")
                    
                    longest_df$y <- as.numeric(longest_df$y)
                    
                    longest_df$`Subgroup of Group` <- as.factor(longest_df$`Subgroup of Group`)
                    
                    dotplot_binwidth_2 <- abs(max(longest_df$y)-min(longest_df$y))/30
                    
                    
                    plot_group   <- suppressWarnings(ggplot(data = long_df, mapping = aes(x= `Group`, y = `y`, group = `Group`, fill = `Group`)) + geom_boxplot(outlier.size = -1, width = 0.5) + stat_compare_means(label = "p.signif", method = test_method, paired = paired_data, comparisons = comparison_list, p.adjust.methods = multiple_correction_method) + stat_compare_means(label = "p.format", method = test_method, paired = paired_data, comparisons = comparison_list, vjust = 1.5, p.adjust.methods = multiple_correction_method) + theme + pretty + geom_dotplot(aes(fill = `Group`), binaxis = "y", stackdir = "center", shape = 21, color = "black", drop = TRUE, na.rm = TRUE, method = "dotdensity", stroke = 1.5, dotsize = 0.8, binwidth = dotplot_binwidth) + labs(title = paste(plot_title, "all Groups"), x = NULL, y = y_axis_title) + reversed + custom_breaks_on_y_axis)
                    plot_group_2 <- suppressWarnings(ggplot(data = longest_df, mapping = aes(x= `Subgroup of Group`, y = `y`, group = `Subgroup of Group`, fill = `Subgroup of Group`)) + geom_boxplot(outlier.size = -1, width = 0.5) + stat_compare_means(label = "p.signif", method = test_method, paired = paired_data, comparisons = comparison_list_2, p.adjust.methods = multiple_correction_method) + stat_compare_means(label = "p.format", method = test_method, paired = paired_data, comparisons = comparison_list_2, vjust = 1.5, p.adjust.methods = multiple_correction_method) + theme + pretty + geom_dotplot(aes(fill = `Subgroup of Group`), binaxis = "y", stackdir = "center", shape = 21, color = "black", drop = TRUE, na.rm = TRUE, method = "dotdensity", stroke = 1.5, dotsize = 0.8, binwidth = dotplot_binwidth_2) + labs(title = paste(plot_title, "all Subgroups from all Groups"), x = NULL, y = y_axis_title) + reversed + custom_breaks_on_y_axis)
                    
                    suppressWarnings(print(plot_group))
                    suppressWarnings(print(plot_group_2))
                    
                    if(isTRUE(save_as_filetype))
                    {
                        try(suppressWarnings(ggsave(paste0(all_to_underscore(colnames(slip_table)[counter_box_plots]), "_", "all_groups", ".svg"), plot = file_type)), silent =  TRUE)
                        try(suppressWarnings(ggsave(paste0(all_to_underscore(colnames(slip_table)[counter_box_plots]), "_", "all_subgroups_from_all_groups", ".svg"), plot = file_type)), silent =  TRUE)
                    }
                    
                    
                    plot_group_build <- suppressWarnings(ggplot_build(plot_group))
                    plot_group_build_2 <- suppressWarnings(ggplot_build(plot_group_2))
                    
                    if(is.null(p_value_list))
                    {
                        p_value_list <<- data.frame(cbind(plot_group_build$data[[3]]$group, plot_group_build$data[[3]]$annotation, plot_group_build$data[[2]]$annotation, rep(test_method, length(plot_group_build$data[[3]]$group)), rep(plot_title, length(plot_group_build$data[[3]]$group))))
                        colnames(p_value_list) <<- c("Comparison", "p-Value", "Significance", "Test Method", "Column")
                        
                        temp_p_value_list <- data.frame(cbind(plot_group_build_2$data[[3]]$group, plot_group_build_2$data[[3]]$annotation, plot_group_build_2$data[[2]]$annotation, rep(test_method, length(plot_group_build_2$data[[3]]$group)), rep(plot_title, length(plot_group_build$data[[3]]$group))))
                        colnames(temp_p_value_list) <- c("Comparison", "p-Value", "Significance", "Test Method", "Column")
                        
                        p_value_list <<- rbind(p_value_list, temp_p_value_list)
                    
                    } else {
                        temp_p_value_list <- data.frame(cbind(plot_group_build$data[[3]]$group, plot_group_build$data[[3]]$annotation, plot_group_build$data[[2]]$annotation, rep(test_method, length(plot_group_build$data[[3]]$group)), rep(plot_title, length(plot_group_build$data[[3]]$group))))
                        colnames(temp_p_value_list) <- c("Comparison", "p-Value", "Significance", "Test Method", "Column")
                        
                        temp_p_value_list_2 <- data.frame(cbind(plot_group_build_2$data[[3]]$group, plot_group_build_2$data[[3]]$annotation, plot_group_build_2$data[[2]]$annotation, rep(test_method, length(plot_group_build_2$data[[3]]$group)), rep(plot_title, length(plot_group_build$data[[3]]$group))))
                        colnames(temp_p_value_list_2) <- c("Comparison", "p-Value", "Significance", "Test Method", "Column")
                        
                        p_value_list <<- rbind(p_value_list, temp_p_value_list)
                        
                        p_value_list <<- rbind(p_value_list, temp_p_value_list_2)
                    }
                } #end of if group_counter is it's own length
                next #group_counter = group_counter + 1
            } #end of for-loop group_counter
        } #end of if column is.numeric
        next #counter_box_plots = counter_box_plots + 1
    }
    
    p_value_list$`p-Value` <<- as.numeric(p_value_list$`p-Value`)
    
    p_value_list$Comparison <<- gsub(pattern = "\\-*.\\d$","",p_value_list$Comparison)
    
    p_value_list <<- p_value_list[which(!duplicated(p_value_list)),]
    
    print("A p_value_list was created containing all comparisons with their according p-values. You can call it using: p_value_list")
    
    setwd(wd)
} #End of function gg_box_plots_after_slip_detection



gg_reverse_ratio_density <- function(rev_rat, table, plot_title = NULL, x_axis_title = NULL, y_axis_title = NULL, xlim = NULL, paired_data = NULL, normal_distribution = NULL, save_as_svg = NULL, directory = NULL, reverse_y_axis = FALSE, plot_group_comparison = NULL, plot_age_comparison = NULL, remove_stationary_phases = NULL, plot_vertical_zero_line = TRUE, save_for_publication = NULL, custom_breaks_x_axis = NULL, custom_breaks_y_axis = NULL, only_plot_comparing_boxplots = NULL, coord_xlim = NULL, coord_ylim = NULL)
{
    #Initialize
    if(!isTRUE("ggplot2" %in% (.packages())))
    {
        if(!require(ggplot2))
        {
            install.packages(ggplot2, dep=TRUE, quiet = TRUE)
        }
        
        suppressPackageStartupMessages(library(ggplot2))
    }
    
    if(!isTRUE("ggpubr" %in% (.packages())))
    {
        if(!require(ggpubr))
        {
            install.packages(ggpubr, dep=TRUE, quiet = TRUE)
        }
        
        suppressPackageStartupMessages(library(ggpubr))
    }
    
    if(is.null(remove_stationary_phases)){remove_stationary_phases <- FALSE}
    
    if(is.null(save_for_publication)){save_for_publication <- FALSE}
    
    if(!isTRUE(plot_vertical_zero_line)){plot_vertical_zero_line <- NULL}
    if(isTRUE(plot_vertical_zero_line))
    {
        if(isTRUE(save_for_publication))
        {
            plot_vertical_zero_line <- geom_vline(aes(xintercept = 0), color="black", linetype = "dashed", linewidth = 1.5)
        } else {
            plot_vertical_zero_line <- geom_vline(aes(xintercept = 0), color="black", linetype = "dashed", linewidth = 1)
        }
    }
    
    if(is.null(plot_title)){plot_title <- deparse(substitute(rev_rat))}
    if(is.null(x_axis_title)){x_axis_title <- "Relative to Beam (%)"}
    if(is.null(y_axis_title)){y_axis_title <- "Distribution of Frames (a.u.)"}
    
    
    if(is.null(save_as_svg)){save_as_svg <- FALSE}
    
    if(is.null(only_plot_comparing_boxplots))
    {
        only_plot_comparing_boxplots <- TRUE
    }
    
    if(!is.null(custom_breaks_x_axis))
    {
        if(typeof(custom_breaks_x_axis) == "double" || typeof(custom_breaks_x_axis) == "integer")
        {
            custom_breaks_x_axis <- custom_breaks_x_axis[!sapply(custom_breaks_x_axis,is.null)]
            custom_breaks_x_axis <- custom_breaks_x_axis[!sapply(custom_breaks_x_axis,is.na)]
            custom_breaks_x_axis <- custom_breaks_x_axis[!sapply(custom_breaks_x_axis,is.nan)]
            custom_breaks_x_axis <- custom_breaks_x_axis[!sapply(custom_breaks_x_axis,is.infinite)]
            
            if(identical(custom_breaks_x_axis, numeric(0)))
            {
                print("custom_breaks_x_axis is numeric(0) and is therefore set to NULL. Please make sure it is a vector containing double or integer values like c(0, 2, 4, 8, 10) or seq(0, 10, 2)")
                custom_breaks_on_x_axis  <- NULL
                custom_breaks_x_axis <- NULL
                
            } else {
                
                custom_breaks_on_x_axis <- scale_x_continuous(breaks = custom_breaks_x_axis)
            }
            
        } else {
            print("Unrecognized type of breaks, setting custom_breaks_x_axis to NULL. Expected a vector containing double or integer values like c(0, 2, 4, 8, 10) or seq(0, 10, 2)")
            custom_breaks_on_x_axis <- NULL
            custom_breaks_x_axis <- NULL
        }
    } else {
        custom_breaks_on_x_axis <- NULL
    }
    
    
    
    if(!is.null(custom_breaks_y_axis))
    {
        if(typeof(custom_breaks_y_axis ) == "double" || typeof(custom_breaks_y_axis ) == "integer")
        {
            custom_breaks_y_axis  <- custom_breaks_y_axis [!sapply(custom_breaks_y_axis ,is.null)]
            custom_breaks_y_axis  <- custom_breaks_y_axis [!sapply(custom_breaks_y_axis ,is.na)]
            custom_breaks_y_axis  <- custom_breaks_y_axis [!sapply(custom_breaks_y_axis ,is.nan)]
            custom_breaks_y_axis  <- custom_breaks_y_axis [!sapply(custom_breaks_y_axis ,is.infinite)]
            
            if(identical(custom_breaks_y_axis , numeric(0)))
            {
                print("custom_breaks_y_axis  is numeric(0) and is therefore set to NULL. Please make sure it is a vector containing double or integer values like c(0, 2, 4, 8, 10) or seq(0, 10, 2)")
                custom_breaks_on_y_axis  <- NULL
                custom_breaks_y_axis  <- NULL
                
                pretty <- scale_y_continuous(breaks = ~round(unique(pretty(.))))
                
            } else {
                custom_breaks_on_y_axis  <- scale_y_continuous(breaks = custom_breaks_y_axis)
                
                pretty <- NULL
            }
            
        } else {
            print("Unrecognized type of breaks, setting custom_breaks_y_axis  to NULL. Expected a vector containing double or integer values like c(0, 2, 4, 8, 10) or seq(0, 10, 2)")
            custom_breaks_on_y_axis  <- NULL
            custom_breaks_y_axis <- NULL
            
            pretty <- scale_y_continuous(breaks = ~round(unique(pretty(.))))
        }
    } else {
        custom_breaks_on_y_axis <- NULL
        
        pretty <- scale_y_continuous(breaks = ~round(unique(pretty(.))))
    }
    
    
    if(!is.null(coord_xlim) || !is.null(coord_ylim))
    {
        if(is.null(coord_xlim))
        {
            coord <- coord_cartesian(xlim = NULL, ylim = coord_ylim, expand = FALSE)
            
        } else if(is.null(coord_ylim))
        {
            
            coord <- coord_cartesian(xlim = coord_xlim, ylim = c(0, NA), expand = FALSE)
            
            
        } else {
            
            coord <- coord_cartesian(xlim = coord_xlim, ylim = coord_ylim, expand = FALSE)
        }
    } else {
        
        coord <- coord_cartesian(xlim = NULL, ylim = c(0, NA), expand = FALSE)
    }
    
    
    if(save_as_svg == TRUE)
    {
        
        if(!isTRUE("svglite" %in% (.packages())))
        {
            if(!require(svglite))
            {
                install.packages(svglite, dep=TRUE, quiet = TRUE)
            }
            
            suppressPackageStartupMessages(library(svglite))
        }
        
        wd <- getwd()
        wd <- gsub(pattern = "/","\\", wd, fixed = TRUE)
        
        if(is.null(directory))
        {
            directory <- choose.dir(caption = "No directory was given. Please choose a directory to save the results. Just close this window, if the result should be saved in the current working directory.")
            
            if(is.null(directory) || is.na(directory))
            {
                directory <- wd
            } else {
                directory <- gsub(pattern = "/","\\", directory, fixed = TRUE)
            }
            
            setwd(directory)
        }
        
        ifelse(!dir.exists("Density_plots"), dir.create("Density_plots"), "Folder exists already")
        
        setwd(paste0(directory, "\\Density_plots"))
        
        if(isTRUE(save_for_publication))
        {
            ifelse(!dir.exists("For_Publication"), dir.create("For_Publication"), "Folder exists already")
            
            setwd(paste0(directory, "\\Density_plots\\For_Publication"))
        }
    }
    
    #Subset reverse_ratio to plot Histograms (You this ratio by running the slip_detection function as a global variable)
    j <- 0L
    i <- 0L
    
    rev_rat_col <- NULL
    
    #family = in element_text() for a different font, but font loader needs to be used first
    
    #Setting ggplot theme
    #theme <- theme_bw() %+replace% theme(panel.border = element_blank(), panel.grid.minor = element_blank(), panel.grid.major = element_blank(), axis.line.y = element_line(colour = "black"), axis.line.x = element_blank(), axis.title = element_text(size = rel(1.3), face = "bold"), plot.title = element_text(size = rel(1.8), face = "bold", margin = margin(0,0,10,0), hjust = 0.5), axis.text = element_text(size = rel(1.2), face = "bold"), legend.title = element_text(size = rel(1.3), face = "bold"), legend.text = element_text(size = rel(1), face = "bold") + geom_vline(xintercept = 0))
    
    #sans is TT Arial, serif is TT Times New Roman, mono is TT Courier New, these are the default windowsFonts without additional library
    
    if(isTRUE(save_for_publication))
    {
        theme <- theme_classic() %+replace% theme(text = element_text(family = "sans"), panel.grid.minor = element_blank(), panel.grid.major = element_blank(), axis.title = element_text(size = 20, face = "bold"), plot.title = element_text(size = 20, face = "bold", margin = margin(0,0,10,0), hjust = 0.5), axis.text = element_text(size = 20, face = "bold"), legend.title = element_blank(), legend.text = element_text(size = 20, face = "bold"), legend.position = c(0.3, 0.9), axis.line = element_line(colour = 'black'), axis.ticks = element_line(colour = 'black', linewidth = 1.5), axis.ticks.length = unit(6, units = "points"))
    } else {
        theme <- theme_classic() %+replace% theme(text = element_text(family = "sans"), panel.grid.minor = element_blank(), panel.grid.major = element_blank(), axis.title = element_text(size = rel(1.3), face = "bold"), plot.title = element_text(size = rel(1.8), face = "bold", margin = margin(0,0,10,0), hjust = 0.5), axis.text = element_text(size = rel(1.2), face = "bold"), legend.title = element_text(size = rel(1.3), face = "bold"), legend.text = element_text(size = rel(1), face = "bold"))
    }
    
    if(isTRUE(remove_stationary_phases))
    {
        for(i in 1:length(rev_rat))
        {
            rev_rat_col[[i]] <- data.frame(cbind(rev_rat[[i]], stationary[[i]]))
            
            if(length(rev_rat[[i]]) != length(stationary[[i]])) 
            { 
                #print(paste("rev_rat[[i]]", length(rev_rat[[i]])))
                #print(paste("stationary[[i]]", length(stationary[[i]])))
                
                #print(rev_rat[[i]])
                #print(stationary[[i]])
                #print(rev_rat_col[[i]]) 
            }
        }
        
        #print(rev_rat_col)
    }
    
    #colours <- c("#FC8D59", "#FFFFBF", "#91BFDB") #original colourblind friendly colours
    
    groups <- unique(table$Group)
    subgroups <- unique(table$Subgroup)
    
    value_groups <- NULL
    
    value_subgroups_1 <- NULL
    value_subgroups_2 <- NULL
    
    group_save <- vector(mode = "list", length = length(groups))
    
    longer_df <- NULL #Makes plot from all subgroups from all groups 
    
    for(group_counter in 1:length(groups))
    {
        if(is.na(groups[group_counter]))
        {
            group_save[[group_counter]] <- NULL
            next #group_counter = group_counter +1
        }
        
        #The only way to get an "all is FALSE"
        if(!isTRUE(any(grepl(paste0("Group_", groups[group_counter]), names(rev_rat)))))
        {
            group_save[[group_counter]] <- NULL
            next #group_counter = group_counter +1
        }
        
        subgroup_save <- vector(mode = "list", length = length(subgroups))
        
        value_groups <- cbind(rev_rat[which(grepl(paste0("Group_", groups[group_counter]), names(rev_rat)))], stationary[which(grepl(paste0("Group_", groups[group_counter]), names(rev_rat)))])
        
        names(value_groups) <- names(rev_rat)[which(grepl(paste0("Group_", groups[group_counter]), names(rev_rat)))]
        
        for(subgroup_counter in 1:length(subgroups))
        {
            if(is.na(subgroups[subgroup_counter]))
            {
                subgroup_save[[subgroup_counter]] <- NULL
                next #subgroup_counter = subgroup_counter +1
            }
            
            #colours <- NULL
            #colours2 <- NULL
            
            colour_coding_1 <- NULL
            colour_coding_2 <- NULL
            
            value_subgroups_1<- rev_rat[which(grepl(paste0("Group_", groups[group_counter]), names(rev_rat)))]
            value_subgroups_2 <- stationary[which(grepl(paste0("Group_", groups[group_counter]), names(rev_rat)))]
            
            if(!isTRUE(any(grepl(paste0("Subgroup_", subgroups[subgroup_counter]), names(value_groups)))))
            {
                #print("no matching subgroup")
                #next #subgroup_counter = subgroup_counter +1
                
            } else {
                
                value_subgroups_1 <- value_subgroups_1[which(grepl(paste0("Subgroup_", subgroups[subgroup_counter]), names(value_groups)))]
                value_subgroups_2 <- value_subgroups_2[which(grepl(paste0("Subgroup_", subgroups[subgroup_counter]), names(value_groups)))]
                
                value_subgroups <- value_groups[which(grepl(paste0("Subgroup_", subgroups[subgroup_counter]), names(value_groups)))]
                
                names(value_subgroups_1) <- NULL
                names(value_subgroups_2) <- NULL
                
                value_subgroups <- data.frame(cbind(unlist(value_subgroups_1), unlist(value_subgroups_2)))
                
                #attr(value_subgroups, "names") <- NULL
                #rownames(value_subgroups) <- NULL
                
                if(ncol(value_subgroups) < 2 || is.null(value_subgroups) || sum(dim(value_subgroups)) == 0 || identical(value_subgroups, data.frame()) || nrow(value_subgroups) == 0)
                {
                    print(paste("The given data set", substitute(rev_rat), "of subgroup", subgroups[subgroup_counter], "from group", groups[group_counter], "is not tracked well enough and will be skipped."))
                    subgroup_save[[subgroup_counter]] <- NULL
                    next #subgroup_counter = subgroup_counter +1
                }
                
                colnames(value_subgroups) <- c("value", "boolean")
                
                value_subgroups <- na.omit(value_subgroups)
                
                if(isTRUE(remove_stationary_phases))
                {
                    value_subgroups <- subset(value_subgroups, select = !isTRUE(is.nan(value_subgroups[,2])))
                    #value_subgroups <- subset(value_subgroups, select = isTRUE(value_subgroups[,2]))
                }
                
                #value_subgroups  <- lapply(value_subgroups,`[`,-2)
                value_subgroups  <- data.frame(value_subgroups[,1])
                
                value_subgroups <- data.frame(cbind(value_subgroups, rep(subgroups[subgroup_counter], times = nrow(value_subgroups))))
                
                colnames(value_subgroups) <- c("Percentage Below Beam", "Subgroup")
                
                subgroup_save[[subgroup_counter]] <- value_subgroups
                
                #colours <- c("#91BFDB", "#F9F981", "#FC8D59") #a slightly darker shade of yellow for better visibility
                #colours2 <- c("#0000FF", "#FFFF00", "#FF0000") #darker outlines
                #
                #colour_coding_1 <- scale_fill_manual(values = colours, na.translate = FALSE)
                #colour_coding_2 <- scale_colour_manual(values = colours2, na.translate = FALSE)
                
                #coord <- coord_cartesian(xlim = c(min(value_groups$`Percentage Below Beam`) -10, max(value_groups$`Percentage Below Beam`) + 10), ylim = c(0, NA), expand = FALSE)
                #coord <- coord_cartesian(xlim = c(-5,55), ylim = c(0, NA), expand = FALSE) #was used for zooming in on the x-axis in the snout plots
                #coord <- coord_cartesian(xlim = c(85,215), ylim = c(0, NA), expand = FALSE) #was used for zooming out on the x-axis in the back plots
                #coord <- coord_cartesian(xlim = c(-200,400), ylim = c(0, NA), expand = FALSE) #was used for zooming out on the x-axis in the tail center plots
                #coord <- coord_cartesian(xlim = c(-450,550), ylim = c(0, NA), expand = FALSE) #was used for zooming out on the x-axis in the tail tip plots
                #coord <- coord_cartesian(xlim = c(-55,55), ylim = c(0, NA), expand = FALSE) #was used for zooming out on the x-axis in the left forepaw plots
                
                if(!isTRUE(only_plot_comparing_boxplots))
                {
                    plot_subgroup <- ggplot(value_subgroups, aes(x = `Percentage Below Beam`, color = `Subgroup`, fill = `Subgroup`)) + geom_density(alpha = 0.6, position="identity") + coord + colour_coding_1 + colour_coding_2 + theme + labs(title = paste(plot_title, groups[group_counter]), x = x_axis_title, y = y_axis_title) + plot_vertical_zero_line + custom_breaks_on_x_axis + custom_breaks_on_y_axis
                    
                    #p1_build <- ggplot_build(plot)
                    
                    print(plot_subgroup)
                    
                    if(isTRUE(save_as_svg))
                    {
                        try(suppressWarnings(ggsave(paste0(all_to_underscore(groups[group_counter]), "_", all_to_underscore(subgroups[subgroup_counter]), "_", all_to_underscore(deparse(substitute(rev_rat))), ".svg"), plot = plot_subgroup)), silent =  TRUE)
                    }
                }
            }
            
            if(subgroup_counter == length(subgroups))
            {
                subgroup_save <- subset(subgroup_save, lapply(subgroup_save, is.null) == FALSE)
                
                if(identical(subgroup_save, list()))
                {
                    next #subgroup_counter = subgroup_counter + 1
                }
                
                long_df <- NULL
                
                for(i in 1:length(subgroup_save))
                {
                    if(i == 1)
                    {
                        long_df <- data.frame(subgroup_save[[i]])
                    }
                    long_df <- rbind(long_df, data.frame(subgroup_save[[i]]))
                }
                
                colnames(long_df) <- c("Percentage Below Beam", "Subgroup")
                
                longer_df[[group_counter]] <- long_df
                
                longer_df[[group_counter]]$Subgroup <- cbind(gsub(pattern = "$", paste0("_", groups[group_counter]), unlist(longer_df[[group_counter]]$Subgroup)))
                
                plot_subgroup <- ggplot(long_df, aes(x = `Percentage Below Beam`, color = `Subgroup`, fill = `Subgroup`)) + geom_density(alpha = 0.6, position="identity") + coord + colour_coding_1 + colour_coding_2 + theme + labs(title = paste(plot_title, groups[group_counter], "all Subgroups"), x = x_axis_title, y = y_axis_title) + plot_vertical_zero_line + custom_breaks_on_x_axis + custom_breaks_on_y_axis + pretty
                
                print(plot_subgroup)
                
                
                if(isTRUE(save_as_svg))
                {
                    try(suppressWarnings(ggsave(paste0(all_to_underscore(groups[group_counter]), "_", all_to_underscore(deparse(substitute(rev_rat))), "_", "all_subgroups", ".svg"), plot = plot_subgroup)), silent =  TRUE)
                }
            }
            
            #assign(paste0(tolower(all_to_underscore(groups[group_counter])), "_", tolower(all_to_underscore(subgroups[subgroup_counter]))), value_subgroups, envir = globalenv())
            next #subgroup_counter = subgroup_counter + 1
        }
        
        names(value_groups) <- NULL
        
        attr(value_groups, "names") <- NULL
        rownames(value_groups) <- NULL
        
        value_groups <- data.frame(cbind(unlist(value_groups[,1]), unlist(value_groups[,2])))
        
        #Why was it so fucking difficult to get a stupid dataframe out of the named list, like WTF this took so long to figure out and the solution looks so ugly
        #Like literally nothing worked (stack, do.call, rbind, cbind, names() <- NULL, unlists, data.frame in like any combination, even though sometimes the correct structure was achieved, I always got the same stupid wrong unworkable list)
        #value_groups <- data.frame(cbind(value_groups_1, value_groups_2))
        
        if(ncol(value_groups) < 2 || is.null(value_groups) || sum(dim(value_groups)) == 0 || identical(value_groups, data.frame()) || nrow(value_groups) == 0)
        {
            print(paste("The given data set", substitute(rev_rat), "of group", groups[group_counter], "is not tracked well enough and will be skipped."))
            group_save[[group_counter]] <- NULL
            next #group_counter = group_counter +1
        }
        
        colnames(value_groups) <- c("value", "boolean")
        
        value_groups <- na.omit(value_groups)
        
        if(isTRUE(remove_stationary_phases))
        {
            value_groups <- subset(value_groups, select = !isTRUE(is.nan(value_groups[,2])))
            #value_groups <- subset(value_groups, select = isTRUE(value_groups[,2]))
        }
        
        #value_groups <- lapply(value_groups,`[`,-2)
        value_groups  <- data.frame(value_groups[,1])
        
        value_groups <- data.frame(cbind(value_groups, rep(groups[group_counter], times = nrow(value_groups))))
        
        colnames(value_groups) <- c("Percentage Below Beam", "Group")
        
        group_save[[group_counter]] <- value_groups
        
        #colours <- c("#91BFDB", "#F9F981", "#FC8D59") #a slightly darker shade of yellow for better visibility
        #colours2 <- c("#0000FF", "#FFFF00", "#FF0000") #darker outlines
        #
        #colour_coding_1 <- scale_fill_manual(values = colours, na.translate = FALSE)
        #colour_coding_2 <- scale_colour_manual(values = colours2, na.translate = FALSE)
        
        #coord <- coord_cartesian(xlim = c(min(value_groups$`Percentage Below Beam`) -10, max(value_groups$`Percentage Below Beam`) + 10), ylim = c(0, NA), expand = FALSE)
        #coord <- coord_cartesian(xlim = c(-5,55), ylim = c(0, NA), expand = FALSE) #was used for zooming in on the x-axis in the snout plots
        #coord <- coord_cartesian(xlim = c(85,215), ylim = c(0, NA), expand = FALSE) #was used for zooming out on the x-axis in the back plots
        #coord <- coord_cartesian(xlim = c(-200,400), ylim = c(0, NA), expand = FALSE) #was used for zooming out on the x-axis in the tail center plots
        #coord <- coord_cartesian(xlim = c(-450,550), ylim = c(0, NA), expand = FALSE) #was used for zooming out on the x-axis in the tail tip plots
        #coord <- coord_cartesian(xlim = c(-55,55), ylim = c(0, NA), expand = FALSE) #was used for zooming out on the x-axis in the left forepaw plots
        
        if(!isTRUE(only_plot_comparing_boxplots))
        {
            plot_group <- ggplot(value_groups, aes(x = `Percentage Below Beam`, color = `Group`, fill = `Group`)) + geom_density(alpha = 0.6, position="identity") + coord + colour_coding_1 + colour_coding_2 + theme + labs(title = plot_title, x = x_axis_title, y = y_axis_title) + plot_vertical_zero_line + custom_breaks_on_x_axis + custom_breaks_on_y_axis + pretty
            
            #p1_build <- ggplot_build(plot)
            
            print(plot_group)
            
            if(isTRUE(save_as_svg))
            {
                try(suppressWarnings(ggsave(paste0(all_to_underscore(groups[group_counter]), "_", all_to_underscore(deparse(substitute(rev_rat))), ".svg"), plot = plot_group)), silent =  TRUE)
            }
        }
        
        if(group_counter == length(groups))
        {
            group_save <- subset(group_save, lapply(group_save, is.null) == FALSE)
            longer_df <- subset(longer_df, lapply(longer_df, is.null) == FALSE)
            
            long_df <- NULL
            longest_df <- NULL #Uses longer_df from subgroups to plot all subgroups from all groups
            
            #print(longer_df)
            if(identical(group_save, list()))
            {
                next #subgroup_counter = subgroup_counter + 1
            }
            
            
            for(i in 1:length(group_save))
            {
                if(i == 1)
                {
                    long_df <- data.frame(group_save[[i]])
                    longest_df <- data.frame(longer_df[[i]])
                }
                
                long_df <- rbind(long_df, data.frame(group_save[[i]]))
                longest_df <- rbind(longest_df, data.frame(longer_df[[i]]))
            }
            
            #print(long_df)
            #print(longest_df)
            
            colnames(long_df) <- c("Percentage Below Beam", "Group")
            colnames(longest_df) <- c("Percentage Below Beam", "Subgroup of Group")
            
            plot_group <- ggplot(long_df, aes(x = `Percentage Below Beam`, color = `Group`, fill = `Group`)) + geom_density(alpha = 0.6, position="identity") + coord + colour_coding_1 + colour_coding_2 + theme + labs(title = paste(plot_title, "all Groups"), x = x_axis_title, y = y_axis_title) + plot_vertical_zero_line + custom_breaks_on_x_axis + custom_breaks_on_y_axis + pretty
            plot_group_2 <- ggplot(longest_df, aes(x = `Percentage Below Beam`, color = `Subgroup of Group`, fill = `Subgroup of Group`)) + geom_density(alpha = 0.6, position="identity") + coord + colour_coding_1 + colour_coding_2 + theme + labs(title = paste(plot_title, "all Subgroups from all Groups"), x = x_axis_title, y = y_axis_title) + plot_vertical_zero_line + custom_breaks_on_x_axis + custom_breaks_on_y_axis + pretty
            
            
            print(plot_group)
            print(plot_group_2)
            
            if(isTRUE(save_as_svg))
            {
                try(suppressWarnings(ggsave(paste0(all_to_underscore(deparse(substitute(rev_rat))), "_", "all_groups", ".svg"), plot = plot_group)), silent =  TRUE)
                try(suppressWarnings(ggsave(paste0(all_to_underscore(deparse(substitute(rev_rat))), "_", "all_subgroups_from_all_groups", ".svg"), plot = plot_group_2)), silent =  TRUE)
            }
        }
        #assign(paste0(tolower(all_to_underscore(groups[group_counter]))), value_groups, envir = globalenv())
        next #group_counter = group_counter + 1
    }
} #End of function gg_reverse_ratio_histograms



#put in subgroup
mean_of_mice_function <- function(table_to_mean, table, column_x, env = NULL)
{
    mean_of_trials <- list()
    
    mouse_num <- list()
    
    group_of_mouse <- list()
    
    trial_num <- list()
    
    subgroup_of_mouse <- list()
    
    mean_of_trials_group <- data.frame()
    
    subgroups <- unique(table$Subgroup)
    
    for(mouse in 1:length(table$`Mouse Number`))
    {
        if(identical(which(table$`Mouse Number`[mouse] == table_to_mean$`Mouse Number`), integer(0)))
        {
            next #mouse = mouse + 1
        }
        
        sum_of_rows <- sum(table_to_mean[[column_x]][which(table$`Mouse Number`[mouse] == table_to_mean$`Mouse Number`)], na.rm = TRUE)
        
        mean_of_trials[mouse] <- sum_of_rows/as.numeric(table$`Total Trials`[mouse])
        
        mouse_num[mouse] <- table$`Mouse Number`[mouse]
        
        trial_num[mouse] <- as.numeric(table$`Table Number`[mouse])
        
        group_of_mouse[mouse] <- unique(table_to_mean$Group[which(table$`Mouse Number`[mouse] == table_to_mean$`Mouse Number`)])
        
        subgroup_of_mouse[mouse] <- table$Subgroup[mouse]
    }
    
    mean_of_trials <- data.frame(cbind(unlist(mean_of_trials)))
    
    mouse_num <- data.frame(cbind(unlist(mouse_num)))
    
    group_of_mouse <- data.frame(cbind(unlist(group_of_mouse)))
    
    subgroup_of_mouse <- data.frame(cbind(unlist(subgroup_of_mouse)))
    
    trial_num <- data.frame(cbind(unlist(trial_num)))
    
    #df_for_plotting <- cbind(mean_of_trials, group_of_mouse)
    #colnames(df_for_plotting) <- c("Mean of Trials", "Group")
    
    
    mean_of_trials_group <- data.frame(cbind(mean_of_trials, group_of_mouse, subgroup_of_mouse, mouse_num, trial_num))
    
    colnames(mean_of_trials_group) <- c("Mean of Trials", "Group", "Subgroup", "Mouse Number", "Number of Trials")
    
    print(paste("n of", unique(group_of_mouse), unique(subgroup_of_mouse), "is:", sum(as.numeric(unlist(trial_num)), na.rm = TRUE)))
    
    #dotplot_binwidth <- abs(max(mean_of_trials)-min(mean_of_trials))/30
    
    if(is.null(env))
    {
        assign("mean_of_trials_group", mean_of_trials_group, globalenv())
    } else {
        assign("mean_of_trials_group", mean_of_trials_group, env)
    }
} #End of function mean_of_mice_function



#functions?
researcher_vs_BAS_function <- function(data_frame, compare_criteria_subgroup, compare_criteria_group, column_1, column_2, column_1_name, column_2_name, title_addition, take_mean_of_mice)
{
    #data_frame <- major_comparison
    #compare_criteria_subgroup <- 6
    #compare_criteria_group <- "Control"
    #column_1 <- 1
    #column_2 <- 2
    #column_1_name <- "Researcher"
    #column_2_name <- "Computer"
    #title_addition <- "Major Slip"
    
    env <- environment()
    
    compare_1 <- data.frame(cbind(data_frame[[column_1]][which(data_frame$Subgroup == compare_criteria_subgroup)], data_frame$Group[which(data_frame$Subgroup == compare_criteria_subgroup)],  data_frame$Subgroup[which(data_frame$Subgroup == compare_criteria_subgroup)], data_frame$`Mouse Number`[which(data_frame$Subgroup == compare_criteria_subgroup)])) 
    colnames(compare_1) <- c("Placeholder", "Group", "Subgroup", "Mouse Number")
    
    compare_1 <- data.frame(cbind(compare_1[[1]][which(compare_1$Group == compare_criteria_group)], compare_1$Group[which(compare_1$Group == compare_criteria_group)],  compare_1$Subgroup[which(compare_1$Group == compare_criteria_group)], compare_1$`Mouse Number`[which(compare_1$Group == compare_criteria_group)])) 
    
    colnames(compare_1) <- c("Placeholder", "Group", "Subgroup", "Mouse Number")
    compare_1[1] <- as.numeric(unlist(compare_1[1]))
    
    compare_1$Group <- rep(column_1_name, nrow(compare_1))
    
    compare_2 <- data.frame(cbind(data_frame[[column_2]][which(data_frame$Subgroup == compare_criteria_subgroup)], data_frame$Group[which(data_frame$Subgroup == compare_criteria_subgroup)],  data_frame$Subgroup[which(data_frame$Subgroup == compare_criteria_subgroup)], data_frame$`Mouse Number`[which(data_frame$Subgroup == compare_criteria_subgroup)])) 
    colnames(compare_2) <- c("Placeholder", "Group", "Subgroup", "Mouse Number")
    
    compare_2 <- data.frame(cbind(compare_2[[1]][which(compare_2$Group == compare_criteria_group)], compare_2$Group[which(compare_2$Group == compare_criteria_group)],  compare_2$Subgroup[which(compare_2$Group == compare_criteria_group)], compare_2$`Mouse Number`[which(compare_2$Group == compare_criteria_group)])) 
    
    colnames(compare_2) <- c("Placeholder", "Group", "Subgroup", "Mouse Number")
    
    compare_2[1] <- as.numeric(unlist(compare_2[1]))
    compare_2$Group <- rep(column_2_name, nrow(compare_2))
    
    #print(compare_1)
    #print(compare_2)
    
    #title_ggplot <- paste(compare_criteria_group, compare_criteria_subgroup, title_addition)
    
    if(!isTRUE(take_mean_of_mice))
    {
        df_ggplot <- rbind(compare_1, compare_2)
        
    } else 
    {
        mean_of_mice_function(compare_1, main_table, 1, env)
        df_ggplot <- mean_of_trials_group
        
        #print(df_ggplot)
        
        #rm(mean_of_trials_group)
        
        mean_of_mice_function(compare_2, main_table, 1, env)
        df_ggplot <- rbind(df_ggplot, mean_of_trials_group)
        
        #print(df_ggplot)
        
        rm(mean_of_trials_group)
        
        df_ggplot <- subset(df_ggplot, select = -c(ncol(df_ggplot))) #Remove the last Trials column because it is not needed by the next function and would throw an error
    }

    
    colnames(df_ggplot) <- c(title_addition, "Group", "Subgroup", "Mouse Number")
    df_ggplot[1] <- as.numeric(unlist(df_ggplot[1]))
    
    #print(df_ggplot)
    
    mean <- mean(df_ggplot[[1]][which(df_ggplot$Group == column_1_name)], na.rm = TRUE)
    big_n <- length(df_ggplot[[1]][which(df_ggplot$Group == column_1_name)])
    
    SEM <- sqrt(var(df_ggplot[[1]][which(df_ggplot$Group == column_1_name)], na.rm = TRUE))/sqrt(big_n)
    
    
    print(paste("Mean of", title_addition, compare_criteria_subgroup, compare_criteria_group, column_1_name, "Equals:", mean))
    print(paste("N of", title_addition, compare_criteria_subgroup, compare_criteria_group, column_1_name, "is:", big_n))
    print(paste("SEM of", title_addition, compare_criteria_subgroup, compare_criteria_group, column_1_name, "Equals:", SEM))
    
    print("")
    
    mean <- mean(df_ggplot[[1]][which(df_ggplot$Group == column_2_name)], na.rm = TRUE)
    big_n <- length(df_ggplot[[1]][which(df_ggplot$Group == column_2_name)])
    
    SEM <- sqrt(var(df_ggplot[[1]][which(df_ggplot$Group == column_2_name)], na.rm = TRUE))/sqrt(big_n)
    
    
    print(paste("Mean of", title_addition, compare_criteria_subgroup, compare_criteria_group, column_2_name, "Equals:", mean))
    print(paste("N of", title_addition, compare_criteria_subgroup, compare_criteria_group, column_2_name, "is:", big_n))
    print(paste("SEM of", title_addition, compare_criteria_subgroup, compare_criteria_group, column_2_name, "Equals:", SEM))
    
    print("")
    print("")
    
    gg_box_plots_after_slip_detection(df_ggplot, main_table, take_mean_of_mice = FALSE)
} #End of function researcher_vs_computer_function



#put in subgroup
mean_of_reverse_ratio <- function(rev_rat, table, remove_stationary_phases = NULL, visualize_removal_of_stationary_phases = NULL)
{
    
    if(is.null(remove_stationary_phases)){remove_stationary_phases <- FALSE}
    
    if(is.null(visualize_removal_of_stationary_phases)){visualize_removal_of_stationary_phases <- FALSE}
    
    
    if(isTRUE(visualize_removal_of_stationary_phases) && !isTRUE(remove_stationary_phases)){ print("Please set remove_stationary_phases to TRUE, because visualize_removal_of_stationary_phases will do nothing on its own.") }
    
    groups <- unique(table$Group)
    
    mean_of_trials <- list()
    
    mouse_num <- list()
    
    group_of_mouse <- list()
    
    #subgroup_of_mouse <- list()
    
    #trial_num <- list()
    
    mean_of_trials_group <<- data.frame()
    
    comparison <- integer(0)
    
    if(isTRUE(remove_stationary_phases))
    {
        for(in_list in 1:length(rev_rat))
        {
            if(isTRUE(visualize_removal_of_stationary_phases))
            {
                plot(unlist(rev_rat[in_list]), ylab = "before removal", xlab = paste("index of", deparse(substitute(rev_rat[in_list]))), type = "l")
            }
            
            for(remove in 1:length(rev_rat[[in_list]]))
            {
                if(isTRUE(stationary[in_list][[1]][remove] == 1) || is.nan(stationary[in_list][[1]][remove]))
                {
                    rev_rat[in_list][[1]][remove] <- NA
                }
            }
            
            if(isTRUE(visualize_removal_of_stationary_phases))
            {
                plot(unlist(rev_rat[in_list]), ylab = "after removal", xlab = paste("index of", deparse(substitute(rev_rat[in_list]))), type = "l")
            }
        }
    }
    
    mouse <- 1L
    
    comparison <- which(gsub(pattern = "_\\d.*", "", gsub(pattern = "^_", "", gsub(pattern = "[a-z]||[A-Z]", "", names(rev_rat)[mouse]))) == gsub(pattern = "_\\d.*", "", gsub(pattern = "^_", "", gsub(pattern = "[a-z]||[A-Z]", "", names(rev_rat)))))
    
    
    for(mouse in 1:length(rev_rat))
    {
        #comparison_mouse_number <- gsub(pattern = "_\\d.*", "", gsub(pattern = "^_", "", gsub(pattern = "[a-z]||[A-Z]", "", names(rev_rat)[mouse]))) #Mouse Number
        
        if(mouse != 1)
        {
            if(!identical(which(gsub(pattern = "_\\d.*", "", gsub(pattern = "^_", "", gsub(pattern = "[a-z]||[A-Z]", "", names(rev_rat)[mouse]))) != gsub(pattern = "_\\d.*", "", gsub(pattern = "^_", "", gsub(pattern = "[a-z]||[A-Z]", "", names(rev_rat)[mouse-1])))), integer(0)))
            {
                #which indeces fit to the mouse number given in a list of all mouse numbers
                comparison <- which(gsub(pattern = "_\\d.*", "", gsub(pattern = "^_", "", gsub(pattern = "[a-z]||[A-Z]", "", names(rev_rat)[mouse]))) == gsub(pattern = "_\\d.*", "", gsub(pattern = "^_", "", gsub(pattern = "[a-z]||[A-Z]", "", names(rev_rat)))))
                #num_of_trial <- 1L
            } else {
                #num_of_trial <- num_of_trial + 1
            }
        }
        
        
        if(identical(comparison, integer(0)))
        {
            next #mouse = mouse + 1
        }
        

        
        #print(comparison)
        
        mean_list <- rev_rat[comparison]
        mean_1 <- c()
        
        
        for(i in 1:length(mean_list))
        {
            if(is.na(suppressWarnings(mean(mean_list[[i]], na.rm = TRUE))))
            {
                #I have no idea why the code worked so well until I tried out the tail tip values, then suddenly I needed this whole section and separation and unlisting to get it to work properly
                mean_list[[i]] <- mean_list[[i]][!sapply(mean_list[[i]], is.null)]
                mean_list[[i]] <- mean_list[[i]][!sapply(mean_list[[i]], is.na)]
                
                #mean_1[i] <- NA
                mean_1[i] <- mean(unlist(mean_list[[i]]), na.rm = TRUE)
                
            } else {
                mean_1[i] <- mean(mean_list[[i]], na.rm = TRUE)
            }
            
            
            if(i == length(mean_list))
            {
                mean_of_trials[mouse] <- mean(mean_1, na.rm = TRUE)
            }
        }
        
        
        
        mouse_num[mouse] <- gsub(pattern = "_\\d.*", "", gsub(pattern = "^_", "", gsub(pattern = "[a-z]||[A-Z]", "", names(rev_rat)[mouse]))) #Mouse Number
        
        group_of_mouse[mouse] <- groups[which(groups == gsub(pattern = "^Trial_.*Group\\_|\\ Subgroup_.*$", "", names(rev_rat)[mouse]))]
        
        #subgroup_of_mouse <- 
        
        #trial_num <- 
        
        next #mouse = mouse + 1
    }
    
    mean_of_trials <- data.frame(cbind(unlist(mean_of_trials)))
    
    mouse_num <- data.frame(cbind(unlist(mouse_num)))
    
    group_of_mouse <- data.frame(cbind(unlist(group_of_mouse)))
    
    #subgroup_of_mouse <- data.frame(cbind(unlist(group_of_mouse)))
    
    #trial_num <- data.frame(cbind(unlist(trial_num)))
    
    # subgroup_of_mouse,
    mean_of_trials_group <<- data.frame(cbind(mean_of_trials, group_of_mouse, mouse_num))
    # "Subgroup",
    colnames(mean_of_trials_group) <<- c("Mean of Trials", "Group", "Mouse Number")
    
    mean_of_trials_group <<- mean_of_trials_group[!duplicated(mean_of_trials_group),]
    
    #rownames(mean_of_trials_group) <<- NULL
} #End of function mean_of_reverse_ratio



#put in subgroup
mean_of_area <- function(area_names, area_values)
{
    mean_of_trials_group <<- list()
    
    for(c_1 in 1:length(names(area_names)))
    {
        if(c_1 == 1)
        {
            comparison <- which(gsub(pattern = "_\\d.*", "", gsub(pattern = "^_", "", gsub(pattern = "[a-z]||[A-Z]", "", names(area_names)[c_1]))) == gsub(pattern = "_\\d.*", "", gsub(pattern = "^_", "", gsub(pattern = "[a-z]||[A-Z]", "", names(area_names)))))
        } else {
            
            if(identical(comparison, which(gsub(pattern = "_\\d.*", "", gsub(pattern = "^_", "", gsub(pattern = "[a-z]||[A-Z]", "", names(area_names)[c_1]))) == gsub(pattern = "_\\d.*", "", gsub(pattern = "^_", "", gsub(pattern = "[a-z]||[A-Z]", "", names(area_names)))))))
            {
                next #c_1 = c_1 + 1
            } else {
                comparison <- which(gsub(pattern = "_\\d.*", "", gsub(pattern = "^_", "", gsub(pattern = "[a-z]||[A-Z]", "", names(area_names)[c_1]))) == gsub(pattern = "_\\d.*", "", gsub(pattern = "^_", "", gsub(pattern = "[a-z]||[A-Z]", "", names(area_names)))))
            }
        }
        
        if(identical(comparison, integer(0)))
        {
            next #c_1 = c_1 + 1
        }
        
        #print(comparison)
        
        mean_of_trials <- list()
        
        mean_list <- area_values[comparison]
        mean_1 <- c()
        
        for(i in 1:length(mean_list))
        {
            mean_1[i] <- mean(mean_list[[i]], na.rm = TRUE)
            
            if(i == length(mean_list))
            {
                mean_of_trials[c_1] <- mean(mean_1, na.rm = TRUE)
            }
        }
        
        trial_num <- length(mean_list)
        
        group_of_mouse <- gsub(pattern = "_\\d.*", "", gsub(pattern = "^_", "", gsub(pattern = "[a-z]||[A-Z]", "", names(area_names)[c_1])))
        
        #subgroup_of_mouse <- gsub()
        
        mouse_num <- gsub(pattern = "_\\d.*", "", gsub(pattern = "^_", "", gsub(pattern = "[a-z]||[A-Z]", "", names(area_names)[c_1]))) #Mouse Number
        
        mean_of_trials <- data.frame(cbind(unlist(mean_of_trials)))
        
        mouse_num <- data.frame(cbind(unlist(mouse_num)))
        
        group_of_mouse <- data.frame(cbind(unlist(group_of_mouse)))
        
        
        #subgroup_of_mouse <- data.frame(cbind(unlist(subgroup_of_mouse)))
        
        trial_num <- data.frame(cbind(unlist(trial_num)))
        
        # subgroup_of_mouse,
        mean_of_trials_group[[c_1]] <<- data.frame(cbind(mean_of_trials, group_of_mouse, mouse_num, trial_num))
        # "Subgroup",
        colnames(mean_of_trials_group[[c_1]]) <<- c("Mean of Trials", "Group", "Mouse Number", "Number of Trials")
    }
    
    boolean <- c()
    
    for(l in 1:length(mean_of_trials_group)){boolean[l] <- !is.null(mean_of_trials_group[[l]])}
    
    mean_of_trials_group <<- subset(mean_of_trials_group, boolean)
} #End of function mean_of_area



convert_RDS_into_csv <- function(rds_file, directory = NULL, column_names = NULL)
{
    wd <- getwd()
    wd <- gsub(pattern = "/","\\", wd, fixed = TRUE)
    
    if(is.null(directory))
    {
        print(paste0("No directory was given. An additional window should have opened. Please choose a directory in that opened window to save the results, simply close that window if the result should be saved in the current working directory, which is: ", wd))
        directory <- choose.dir(caption = "No directory was given. Please choose a directory to save the results. Just close this window, if the result should be saved in the current working directory.")
        
        if(is.null(directory) || is.na(directory))
        {
            directory <- wd
        } else {
            directory <- gsub(pattern = "/","\\", directory, fixed = TRUE)
        }
    }
    
    if(is.null(column_names))
    {
        column_names <- FALSE
        print("You can give custom column names using column_names = c(\"\")")
    }
    
    setwd(directory)
    
    dir_name <- paste0(gsub(pattern = "`|L", "", deparse(substitute(rds_file, env = environment()))))

    ifelse(!dir.exists(dir_name), dir.create(dir_name), "Folder exists already")
    
    setwd(paste0(directory, "\\", dir_name))
    
    for (length_num in 1:length(rds_file))
    {
        file_name <- names(rds_file)[length_num]
        
        #Remove some special characters
        file_name <- paste0(gsub(pattern = "`|L|/", "", names(rds_file)[length_num]))
        
        #print(rds_file[length_num])
        #print(paste0(directory, "\\", dir_name, "\\", file_name, ".csv"))
        
        write.table(rds_file[length_num], file = paste0(directory, "\\", dir_name, "\\", file_name, ".csv"), col.names = column_names, row.names = FALSE, sep = ",", dec = ".", quote = FALSE)
        
        next #length_num = length_num + 1
    }
    
    print("Converted .RDS file into .csv files in directory named after the .RDS file")
    
    setwd(wd)
} #End of function convert_RDS_into_csv