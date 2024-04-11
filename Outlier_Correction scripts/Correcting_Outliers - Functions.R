load_dependencies <- function (){
    ###Added after M.Sc. thesis###
    if(!isTRUE("raster" %in% (.packages())))
    {
        if(!require(raster))
        {
            install.packages(raster, dep=TRUE, quiet = TRUE) #repos='http://star-www.st-andrews.ac.uk/cran/')
        }
        suppressPackageStartupMessages(library(raster)) #needed for example for the pointDistance() function
    }
    ###End of Addtion after M.Sc. thesis###
}




define_everything <- function(data_dir, filename, skipped_rows){
    
    setwd(data_dir)

    data_file <- paste(data_dir, "\\", filename, ".csv", sep = "")
    
    #print(data_file)
    
    if(isTRUE(grepl(pattern = "^corrected\\_", filename)))
    {
        data <- read.csv(data_file, header = TRUE)
    } else {
        data <- read.csv(data_file, header = FALSE, col.names = c("frame","left_eye_x","left_eye_y","left_eye_like","right_eye_x","right_eye_y","right_eye_like","snout_x","snout_y","snout_like","belly_x","belly_y","belly_like","back_x","back_y","back_like","front_left_paw_x","front_left_paw_y","front_left_paw_like","front_right_paw_x","front_right_paw_y","front_right_paw_like","back_left_paw_x","back_left_paw_y","back_left_paw_like","back_right_paw_x","back_right_paw_y","back_right_paw_like","tail_base_x","tail_base_y","tail_base_like","tail_center_x","tail_center_y","tail_center_like","tail_tip_x","tail_tip_y","tail_tip_like","mark_far_left_x","mark_far_left_y","mark_far_left_like","mark_left_x","mark_left_y","mark_left_like","mark_right_x","mark_right_y","mark_right_like","mark_far_right_x","mark_far_right_y","mark_far_right_like","top_beam_left_x","top_beam_left_y","top_beam_left_like","top_beam_middle_x","top_beam_middle_y","top_beam_middle_like","top_beam_right_x","top_beam_right_y","top_beam_right_like","bottom_beam_left_x","bottom_beam_left_y","bottom_beam_left_like","bottom_beam_middle_x","bottom_beam_middle_y","bottom_beam_middle_like","bottom_beam_right_x","bottom_beam_right_y","bottom_beam_right_like"), skip = skipped_rows)    
    }
    
    p_cut_off <- 0.2 #Will be modified for certain bodyparts, like the tail tip & tail center
    
    #Mouse body
    left_eye <- cbind(data$left_eye_x,data$left_eye_y)
    right_eye <- cbind(data$right_eye_x,data$right_eye_y)
    snout <- cbind(data$snout_x,data$snout_y)
    belly <- cbind(data$belly_x,data$belly_y)
    back <- cbind(data$back_x,data$back_y)
    front_left_paw <- cbind(data$front_left_paw_x,data$front_left_paw_y)
    front_right_paw <- cbind(data$front_right_paw_x,data$front_right_paw_y)
    back_left_paw <- cbind(data$back_left_paw_x,data$back_left_paw_y)
    back_right_paw <- cbind(data$back_right_paw_x,data$back_right_paw_y)
    tail_base <- cbind(data$tail_base_x,data$tail_base_y)
    tail_center <- cbind(data$tail_center_x,data$tail_center_y)
    tail_tip <- cbind(data$tail_tip_x,data$tail_tip_y)
    
    #Beam walk
    mark_far_left <- cbind(data$mark_far_left_x, data$mark_far_left_y)
    mark_left <- cbind(data$mark_left_x, data$mark_left_y)
    mark_far_right <- cbind(data$mark_far_right_x, data$mark_far_right_y)
    mark_right <- cbind(data$mark_right_x, data$mark_right_y)
    
    ###Added after M.Sc. thesis###

    left_beam_top <- cbind(data$top_beam_left_x,data$top_beam_left_y)
    middle_beam_top <- cbind(data$top_beam_middle_x,data$top_beam_middle_y)
    right_beam_top <- cbind(data$top_beam_right_x,data$top_beam_right_y)
    
    left_beam_bottom <- cbind(data$bottom_beam_left_x,data$bottom_beam_left_y)
    middle_beam_bottom <- cbind(data$bottom_beam_middle_x,data$bottom_beam_middle_y)
    right_beam_bottom <- cbind(data$bottom_beam_right_x,data$bottom_beam_right_y)
    
    ###End of Addtion after M.Sc. thesis###
    
    
    #Creating vectors and such for measuring distances between two points either over time (the vec_ ones) or between two different sets of points (the distance_ ones)
    #Mouse body (vector length)
    vec_len_le <- c()
    vec_len_re <- c()
    vec_len_snout <- c()
    vec_len_belly <- c()
    vec_len_back <- c()
    vec_len_flp <- c()
    vec_len_frp <- c()
    vec_len_blp <- c()
    vec_len_brp <- c()
    vec_len_tb <- c()
    vec_len_tc <- c()
    vec_len_tt <- c()
    
    #Beam walk (vector length)
    vec_len_mfl <- c()
    vec_len_ml <- c()
    vec_len_mr <- c()
    vec_len_mfr <- c()
    
    ###Added after M.Sc. thesis###
    vec_len_lbt <- c()
    vec_len_mbt <- c()
    vec_len_rbt <- c()
    vec_len_lbb <- c()
    vec_len_mbb <- c()
    vec_len_rbb <- c()
    ###End of Addtion after M.Sc. thesis###
    
    test3 <- list(vec_len_le,vec_len_re,vec_len_snout,vec_len_belly,vec_len_back,vec_len_flp,vec_len_frp,vec_len_blp,vec_len_brp,vec_len_tb,vec_len_tc,vec_len_tt,vec_len_mfl,vec_len_ml,vec_len_mr,vec_len_mfr,vec_len_lbt,vec_len_mbt,vec_len_rbt,vec_len_lbb,vec_len_mbb,vec_len_rbb)
    names(test3) <- c("vec_len_le","vec_len_re","vec_len_snout","vec_len_belly","vec_len_back","vec_len_flp","vec_len_frp","vec_len_blp","vec_len_brp","vec_len_tb","vec_len_tc","vec_len_tt","vec_len_mfl","vec_len_ml","vec_len_mr","vec_len_mfr","vec_len_lbt","vec_len_mbt","vec_len_rbt","vec_len_lbb","vec_len_mbb","vec_len_rbb")
    list2env(test3, envir=.GlobalEnv) #Instead of putting every variable in a named list and then submitting it to the global environment for actual access, one could also use <- when changing and creating variables for the same effect.
    #The tip above, does NOT work here
    
    test <- list(left_eye, right_eye, snout, belly, back, front_left_paw, front_right_paw, back_left_paw, back_right_paw, tail_base, tail_center, tail_tip, mark_far_left, mark_left, mark_right, mark_far_right,left_beam_top,middle_beam_top,right_beam_top,left_beam_bottom,middle_beam_bottom,right_beam_bottom, p_cut_off, data)
    names(test) <- c("left_eye", "right_eye", "snout", "belly", "back", "front_left_paw", "front_right_paw", "back_left_paw", "back_right_paw", "tail_base", "tail_center", "tail_tip", "mark_far_left", "mark_left", "mark_right", "mark_far_right","left_beam_top","middle_beam_top","right_beam_top","left_beam_bottom","middle_beam_bottom","right_beam_bottom","p_cut_off", "data")
    list2env(test, envir=.GlobalEnv)
    
    #print("Done with define_everything")
}




calculate_distances <- function(){
    
    setwd(data_dir)
    
    #Mouse body (distance)
    distance_body <- pointDistance(snout,tail_base,lonlat=FALSE)
    distance_bodyheight <- pointDistance(belly,back,lonlat=FALSE)
    distance_paws <- pointDistance(front_left_paw,back_left_paw,lonlat = FALSE)
    distance_tail <- pointDistance(tail_tip,tail_base, lonlat = FALSE)
    
    #Beam walk (distance)
    distance_left <- pointDistance(mark_far_left, mark_left, lonlat =FALSE)
    distance_right <- pointDistance(mark_far_right, mark_right, lonlat =FALSE)
    
    ###Added after M.Sc. thesis###
    distance_beam_top <- pointDistance(left_beam_top, right_beam_top, lonlat =FALSE)
    distance_beam_bottom <- pointDistance(left_beam_bottom, right_beam_bottom, lonlat =FALSE)
    ###End of Addtion after M.Sc. thesis###
    
    test2 <- list(distance_body,distance_bodyheight,distance_paws,distance_tail, distance_left,distance_right,distance_beam_top,distance_beam_bottom)
    names(test2) <- c("distance_body","distance_bodyheight","distance_tail","distance_paws","distance_left","distance_right","distance_beam_top","distance_beam_bottom")
    list2env(test2, envir=.GlobalEnv)
    
    #print("Done with calculate_distances")
}




calculate_vectors <- function(){
    #Calculating the length of each vector between the previous and present point in time

    setwd(data_dir)
    
    j <- 0

    
    for(j in 2:length(data$frame))
    {
        #print("Here 1")
        #Mouse body (vector length)
        vec_len_le[j] <- sqrt(((left_eye[j,1]-left_eye[j-1,1])^2)+((left_eye[j,2]-left_eye[j-1,2])^2))
        vec_len_re[j] <- sqrt(((right_eye[j,1]-right_eye[j-1,1])^2)+((right_eye[j,2]-right_eye[j-1,2])^2))
        vec_len_snout[j] <- sqrt(((snout[j,1]-snout[j-1,1])^2)+((snout[j,2]-snout[j-1,2])^2))
        vec_len_belly[j] <- sqrt(((belly[j,1]-belly[j-1,1])^2)+((belly[j,2]-belly[j-1,2])^2))
        vec_len_back[j] <- sqrt(((back[j,1]-back[j-1,1])^2)+((back[j,2]-back[j-1,2])^2))
        vec_len_flp[j] <- sqrt(((front_left_paw[j,1]-front_left_paw[j-1,1])^2)+((front_left_paw[j,2]-front_left_paw[j-1,2])^2))
        vec_len_frp[j] <- sqrt(((front_right_paw[j,1]-front_right_paw[j-1,1])^2)+((front_right_paw[j,2]-front_right_paw[j-1,2])^2))
        vec_len_blp[j] <- sqrt(((back_left_paw[j,1]-back_left_paw[j-1,1])^2)+((back_left_paw[j,2]-back_left_paw[j-1,2])^2))
        vec_len_brp[j] <- sqrt(((back_right_paw[j,1]-back_right_paw[j-1,1])^2)+((back_right_paw[j,2]-back_right_paw[j-1,2])^2))
        vec_len_tb[j] <- sqrt(((tail_base[j,1]-tail_base[j-1,1])^2)+((tail_base[j,2]-tail_base[j-1,2])^2))
        vec_len_tc[j] <- sqrt(((tail_center[j,1]-tail_center[j-1,1])^2)+((tail_center[j,2]-tail_center[j-1,2])^2))
        vec_len_tt[j] <- sqrt(((tail_tip[j,1]-tail_tip[j-1,1])^2)+((tail_tip[j,2]-tail_tip[j-1,2])^2))
        
        
        #Beam walk (vector length)
        vec_len_mfl[j] <- sqrt(((mark_far_left[j,1]-mark_far_left[j-1,1])^2)+((mark_far_left[j,2]-mark_far_left[j-1,2])^2))
        vec_len_ml[j] <- sqrt(((mark_left[j,1]-mark_left[j-1,1])^2)+((mark_left[j,2]-mark_left[j-1,2])^2))
        vec_len_mr[j] <- sqrt(((mark_right[j,1]-mark_right[j-1,1])^2)+((mark_right[j,2]-mark_right[j-1,2])^2))
        vec_len_mfr[j] <- sqrt(((mark_far_right[j,1]-mark_far_right[j-1,1])^2)+((mark_far_right[j,2]-mark_far_right[j-1,2])^2))
        
        
        ###Added after M.Sc. thesis###
        vec_len_lbt[j] <- sqrt(((left_beam_top[j,1]-left_beam_top[j-1,1])^2)+((left_beam_top[j,2]-left_beam_top[j-1,2])^2))
        vec_len_mbt[j] <- sqrt(((middle_beam_top[j,1]-middle_beam_top[j-1,1])^2)+((middle_beam_top[j,2]-middle_beam_top[j-1,2])^2))
        vec_len_rbt[j] <- sqrt(((right_beam_top[j,1]-right_beam_top[j-1,1])^2)+((right_beam_top[j,2]-right_beam_top[j-1,2])^2))
        vec_len_lbb[j] <- sqrt(((left_beam_bottom[j,1]-left_beam_bottom[j-1,1])^2)+((left_beam_bottom[j,2]-left_beam_bottom[j-1,2])^2))
        vec_len_mbb[j] <- sqrt(((middle_beam_bottom[j,1]-middle_beam_bottom[j-1,1])^2)+((middle_beam_bottom[j,2]-middle_beam_bottom[j-1,2])^2))
        vec_len_rbb[j] <- sqrt(((right_beam_bottom[j,1]-right_beam_bottom[j-1,1])^2)+((right_beam_bottom[j,2]-right_beam_bottom[j-1,2])^2))
        ###End of Addtion after M.Sc. thesis###
        
        next #j = j+1
    }
    
    test3 <- list(vec_len_le,vec_len_re,vec_len_snout,vec_len_belly,vec_len_back,vec_len_flp,vec_len_frp,vec_len_blp,vec_len_brp,vec_len_tb,vec_len_tc,vec_len_tt,vec_len_mfl,vec_len_ml,vec_len_mr,vec_len_mfr,vec_len_lbt,vec_len_mbt,vec_len_rbt,vec_len_lbb,vec_len_mbb,vec_len_rbb)
    names(test3) <- c("vec_len_le","vec_len_re","vec_len_snout","vec_len_belly","vec_len_back","vec_len_flp","vec_len_frp","vec_len_blp","vec_len_brp","vec_len_tb","vec_len_tc","vec_len_tt","vec_len_mfl","vec_len_ml","vec_len_mr","vec_len_mfr","vec_len_lbt","vec_len_mbt","vec_len_rbt","vec_len_lbb","vec_len_mbb","vec_len_rbb")
    list2env(test3, envir=.GlobalEnv) #Instead of putting every variable in a named list and then submitting it to the global environment for actual access, one could also use <- when changing and creating variables for the same effect.
    
    #print("Done with calculate_vectors")
}




define_limits <- function(){
    #Setting limits for each body part, or marking. The median was choosen because it is more robust against outliers which can really skew the average due to poor tracking.
    setwd(data_dir)
    
    #Mouse body (vector length)
    up_lim_vec_len_le <- median(vec_len_le,na.rm = TRUE)+50
    low_lim_vec_len_le <- 0
    
    up_lim_vec_len_re <- median(vec_len_re,na.rm = TRUE)+50
    low_lim_vec_len_re <- 0
    
    up_lim_vec_len_snout <- median(vec_len_snout,na.rm = TRUE)+50
    low_lim_vec_len_snout <- 0
    
    up_lim_vec_len_belly  <- median(vec_len_belly,na.rm = TRUE)+50
    low_lim_vec_len_belly <- 0
    
    up_lim_vec_len_back <- median(vec_len_back,na.rm = TRUE)+50
    low_lim_vec_len_back <- 0
    
    up_lim_vec_len_flp <- median(vec_len_flp,na.rm = TRUE)+100
    low_lim_vec_len_flp <- 0
    
    up_lim_vec_len_frp <- median(vec_len_frp,na.rm = TRUE)+100
    low_lim_vec_len_frp <- 0
    
    up_lim_vec_len_blp <- median(vec_len_blp,na.rm = TRUE)+100
    low_lim_vec_len_blp <- 0
    
    up_lim_vec_len_brp <- median(vec_len_brp,na.rm = TRUE)+100
    low_lim_vec_len_brp <- 0
    
    up_lim_vec_len_tb <- median(vec_len_tb,na.rm = TRUE)+50
    low_lim_vec_len_tb <- 0
    
    up_lim_vec_len_tc <- median(vec_len_tc,na.rm = TRUE)+150
    low_lim_vec_len_tc <- 0
    
    up_lim_vec_len_tt <- median(vec_len_tt,na.rm = TRUE)+150
    low_lim_vec_len_tt <- 0
    
    
    #low_lim <- 0 #For easier use (although I didn't use it, lol)
    
    
    #Beam walk (vector length)
    up_lim_mark_vec <- ((median(vec_len_mfl,na.rm = TRUE)+median(vec_len_ml,na.rm = TRUE)+median(vec_len_mr,na.rm = TRUE)+median(vec_len_mfr,na.rm = TRUE))/4)+3
    low_lim_mark_vec <- ((median(vec_len_mfl,na.rm = TRUE)+median(vec_len_ml,na.rm = TRUE)+median(vec_len_mr,na.rm = TRUE)+median(vec_len_mfr,na.rm = TRUE))/4)-3
    
    
    #Mouse body (distance)
    up_lim_bod <- median(distance_body,na.rm = TRUE)*1.8
    low_lim_bod <- median(distance_body,na.rm = TRUE)/2
    
    up_lim_bodh <- median(distance_bodyheight,na.rm = TRUE)*1.8
    low_lim_bodh <- median(distance_bodyheight,na.rm = TRUE)/2
    
    up_lim_paws <- median(distance_paws,na.rm = TRUE)*3
    low_lim_paws <- 0
    
    up_lim_tail <- median(distance_tail,na.rm = TRUE)*3
    low_lim_tail <- 0
    
    #Beam walk (distance)
    up_lim_mark <-  ((median(distance_left,na.rm = TRUE)+median(distance_right,na.rm = TRUE))/2)*1.2
    low_lim_mark <- ((median(distance_left,na.rm = TRUE)+median(distance_right,na.rm = TRUE))/2)*0.8
    
    ###Added after M.Sc. thesis###
    up_lim_vec_len_lbt <- median(vec_len_lbt,na.rm = TRUE)+10
    up_lim_vec_len_mbt <- median(vec_len_mbt,na.rm = TRUE)+10
    up_lim_vec_len_rbt <- median(vec_len_rbt,na.rm = TRUE)+10
    up_lim_vec_len_lbb <- median(vec_len_lbb,na.rm = TRUE)+10
    up_lim_vec_len_mbb <- median(vec_len_mbb,na.rm = TRUE)+10
    up_lim_vec_len_rbb <- median(vec_len_rbb,na.rm = TRUE)+10
    
    low_lim_beam <- 0
    
    up_lim_distance_beam_top <- median(distance_beam_top,na.rm = TRUE)*1.2
    low_lim_distance_beam_top <- median(distance_beam_top,na.rm = TRUE)*0.8
    
    up_lim_distance_beam_bottom <- median(distance_beam_bottom,na.rm = TRUE)*1.2
    low_lim_distance_beam_bottom <- median(distance_beam_bottom,na.rm = TRUE)*0.8
    ###End of Addtion after M.Sc. thesis###
    
    
    test4 <- list(up_lim_vec_len_le,low_lim_vec_len_le,up_lim_vec_len_re,low_lim_vec_len_re,up_lim_vec_len_snout,low_lim_vec_len_snout,up_lim_vec_len_belly,low_lim_vec_len_belly,up_lim_vec_len_back,low_lim_vec_len_back,up_lim_vec_len_flp,low_lim_vec_len_flp,up_lim_vec_len_frp,low_lim_vec_len_frp,up_lim_vec_len_blp,low_lim_vec_len_blp,up_lim_vec_len_brp,low_lim_vec_len_brp,up_lim_vec_len_tb,low_lim_vec_len_tb,up_lim_vec_len_tc,low_lim_vec_len_tc,up_lim_vec_len_tt,low_lim_vec_len_tt,up_lim_mark_vec,low_lim_mark_vec,up_lim_bod,low_lim_bod,up_lim_bodh,low_lim_bodh,up_lim_paws,low_lim_paws,up_lim_tail,low_lim_tail,up_lim_mark,low_lim_mark,up_lim_vec_len_lbt,up_lim_vec_len_mbt,up_lim_vec_len_rbt,up_lim_vec_len_lbb,up_lim_vec_len_mbb,up_lim_vec_len_rbb,low_lim_beam,up_lim_distance_beam_top,low_lim_distance_beam_top,up_lim_distance_beam_bottom,low_lim_distance_beam_bottom)
    names(test4) <- c("up_lim_vec_len_le","low_lim_vec_len_le","up_lim_vec_len_re","low_lim_vec_len_re","up_lim_vec_len_snout","low_lim_vec_len_snout","up_lim_vec_len_belly","low_lim_vec_len_belly","up_lim_vec_len_back","low_lim_vec_len_back","up_lim_vec_len_flp","low_lim_vec_len_flp","up_lim_vec_len_frp","low_lim_vec_len_frp","up_lim_vec_len_blp","low_lim_vec_len_blp","up_lim_vec_len_brp","low_lim_vec_len_brp","up_lim_vec_len_tb","low_lim_vec_len_tb","up_lim_vec_len_tc","low_lim_vec_len_tc","up_lim_vec_len_tt","low_lim_vec_len_tt","up_lim_mark_vec","low_lim_mark_vec","up_lim_bod","low_lim_bod","up_lim_bodh","low_lim_bodh","up_lim_paws","low_lim_paws","up_lim_tail","low_lim_tail","up_lim_mark","low_lim_mark","up_lim_vec_len_lbt","up_lim_vec_len_mbt","up_lim_vec_len_rbt","up_lim_vec_len_lbb","up_lim_vec_len_mbb","up_lim_vec_len_rbb","low_lim_beam","up_lim_distance_beam_top","low_lim_distance_beam_top","up_lim_distance_beam_bottom","low_lim_distance_beam_bottom")
    list2env(test4, envir=.GlobalEnv)
    
    #print("Done with define_limits")
}



remove_outliers <- function() {
    setwd(data_dir)
    
    k <- 0
    #Removing tracking errors based on distance over time and distance between two points. Also removing based on likelihood for certain bodyparts like the right eye, hence it is very unlikely to be seen. Removing based on likelihood does for example not work for the tip of the tail, hence the whole distance calculations.
    for(k in 1:length(data$frame))
    {
        #Mouse body (vector length)
        if(vec_len_le[k] > up_lim_vec_len_le || vec_len_le[k] < low_lim_vec_len_le || is.na(vec_len_le[k]) || data$left_eye_like[k] < p_cut_off) {
            vec_len_le[k] <- NA
            data$left_eye_x[k] <- NA
            data$left_eye_y[k] <- NA
        }
        if(vec_len_re[k] > up_lim_vec_len_re || vec_len_re[k] < low_lim_vec_len_re || is.na(vec_len_re[k]) || data$right_eye_like[k] < p_cut_off) {
            vec_len_re[k] <- NA
            data$right_eye_x[k] <- NA
            data$right_eye_y[k] <- NA
        }
        if(vec_len_snout[k] > up_lim_vec_len_snout || vec_len_snout[k] < low_lim_vec_len_snout || is.na(vec_len_snout[k]) || data$snout_like[k] < p_cut_off) {
            vec_len_snout[k] <- NA
            data$snout_x[k] <- NA
            data$snout_y[k] <- NA
        }
        if(vec_len_belly[k] > up_lim_vec_len_belly || vec_len_belly[k] < low_lim_vec_len_belly || is.na(vec_len_belly[k]) || data$belly_like[k] < p_cut_off) {
            vec_len_belly[k] <- NA
            data$belly_x[k] <- NA
            data$belly_y[k] <- NA
        }
        if(vec_len_back[k] > up_lim_vec_len_back || vec_len_back[k] < low_lim_vec_len_back || is.na(vec_len_back[k]) || data$back_like[k] < p_cut_off) {
            vec_len_back[k] <- NA
            data$back_x[k] <- NA
            data$back_y[k] <- NA
        }
        if(vec_len_flp[k] > up_lim_vec_len_flp || vec_len_flp[k] < low_lim_vec_len_flp || is.na(vec_len_flp[k]) || data$front_left_paw_like[k] < p_cut_off) {
            vec_len_flp[k] <- NA
            data$front_left_paw_x[k] <- NA
            data$front_left_paw_y[k] <- NA
        }
        if(vec_len_frp[k] > up_lim_vec_len_frp || vec_len_frp[k] < low_lim_vec_len_frp || is.na(vec_len_frp[k]) || data$front_right_paw_like[k] < p_cut_off) {
            vec_len_frp[k] <- NA
            data$front_right_paw_x[k] <- NA
            data$front_right_paw_y[k] <- NA
        }
        if(vec_len_blp[k] > up_lim_vec_len_blp || vec_len_blp[k] < low_lim_vec_len_blp || is.na(vec_len_blp[k]) || data$back_left_paw_like[k] < p_cut_off) {
            vec_len_blp[k] <- NA
            data$back_left_paw_x[k] <- NA
            data$back_left_paw_y[k] <- NA
        }
        if(vec_len_brp[k] > up_lim_vec_len_brp || vec_len_brp[k] < low_lim_vec_len_brp || is.na(vec_len_brp[k]) || data$back_right_paw_like[k] < p_cut_off) {
            vec_len_brp[k] <- NA
            data$back_right_paw_x[k] <- NA
            data$back_right_paw_y[k] <- NA
        }
        if(vec_len_tb[k] > up_lim_vec_len_tb || vec_len_tb[k] < low_lim_vec_len_tb || is.na(vec_len_tb[k]) || data$tail_base_like[k] < p_cut_off) {
            vec_len_tb[k] <- NA
            data$tail_base_x[k] <- NA
            data$tail_base_y[k] <- NA
        }
        if(vec_len_tc[k] > up_lim_vec_len_tc || vec_len_tc[k] < low_lim_vec_len_tc || is.na(vec_len_tc[k]) || data$tail_center_like[k] < p_cut_off-(p_cut_off*0.75)) {
            vec_len_tc[k] <- NA
            data$tail_center_x[k] <- NA
            data$tail_center_y[k] <- NA
        }
        if(vec_len_tt[k] > up_lim_vec_len_tt || vec_len_tt[k] < low_lim_vec_len_tt || is.na(vec_len_tt[k]) || data$tail_tip_like[k] < p_cut_off-(p_cut_off*0.75)) {
            vec_len_tt[k] <- NA
            data$tail_tip_x[k] <- NA
            data$tail_tip_y[k] <- NA
        }
        
        
        #Mouse body (distance)
        if(distance_body[k] > up_lim_bod || distance_body[k] < low_lim_bod || is.na(distance_body[k])) {
            distance_body[k] <- NA
        }
        if(distance_bodyheight[k] > up_lim_bodh || distance_bodyheight[k] < low_lim_bodh || is.na(distance_bodyheight[k])) {
            distance_bodyheight[k] <- NA
        }
        if(distance_paws[k] > up_lim_paws || distance_paws[k] < low_lim_paws || is.na(distance_paws[k])) {
            distance_paws[k] <- NA
        }
        if(distance_tail[k] > up_lim_tail || distance_tail[k] < low_lim_tail || is.na(distance_tail[k])) {
            distance_tail[k] <- NA
        }
        
        
        #Beam walk (vector length)
        if(vec_len_mfl[k] > up_lim_mark_vec || vec_len_mfl[k] < low_lim_mark_vec || is.na(vec_len_mfl[k]) || data$mark_far_left_like[k] < p_cut_off) {
            vec_len_mfl[k] <- NA
            data$mark_far_left_x[k] <- NA
            data$mark_far_left_y[k] <- NA
        }
        if(vec_len_ml[k] > up_lim_mark_vec || vec_len_ml[k] < low_lim_mark_vec || is.na(vec_len_ml[k]) || data$mark_left_like[k] < p_cut_off) {
            vec_len_ml[k] <- NA
            data$mark_left_x[k] <- NA
            data$mark_left_y[k] <- NA
        }
        if(vec_len_mr[k] > up_lim_mark_vec || vec_len_mr[k] < low_lim_mark_vec || is.na(vec_len_mr[k]) || data$mark_right_like[k] < p_cut_off) {
            vec_len_mr[k] <- NA
            data$mark_right_x[k] <- NA
            data$mark_right_y[k] <- NA
        }
        if(vec_len_mfr[k] > up_lim_mark_vec || vec_len_mfr[k] < low_lim_mark_vec || is.na(vec_len_mfr[k]) || data$mark_far_right_like[k] < p_cut_off) {
            vec_len_mfr[k] <- NA
            data$mark_far_right_x[k] <- NA
            data$mark_far_right_y[k] <- NA
        }
        
        #print(paste0("vec_len_lbt[k] ", vec_len_lbt[k], " up_lim_vec_len_lbt ", up_lim_vec_len_lbt, " low_lim_beam ", low_lim_beam, " data$top_beam_left_like[k] ", data$top_beam_left_like[k], " p_cut_off ", p_cut_off))
        
        ###Added after M.Sc. thesis###
        if(vec_len_lbt[k] > up_lim_vec_len_lbt || vec_len_lbt[k] < low_lim_beam || is.na(vec_len_lbt[k]) || data$top_beam_left_like[k] < p_cut_off) {
            vec_len_lbt[k] <- NA
            data$top_beam_left_x[k] <- NA
            data$top_beam_left_y[k] <- NA
        }
        if(vec_len_mbt[k] > up_lim_vec_len_mbt || vec_len_mbt[k] < low_lim_beam || is.na(vec_len_mbt[k]) || data$top_beam_middle_like[k] < p_cut_off) {
            vec_len_mbt[k] <- NA
            data$top_beam_middle_x[k] <- NA
            data$top_beam_middle_y[k] <- NA
        }
        if(vec_len_rbt[k] > up_lim_vec_len_rbt || vec_len_rbt[k] < low_lim_beam || is.na(vec_len_rbt[k]) || data$top_beam_right_like[k] < p_cut_off) {
            vec_len_rbt[k] <- NA
            data$top_beam_right_x[k] <- NA
            data$top_beam_right_y[k] <- NA
        }
        if(vec_len_lbb[k] > up_lim_vec_len_lbb || vec_len_lbb[k] < low_lim_beam || is.na(vec_len_lbb[k]) || data$bottom_beam_left_like[k] < p_cut_off) {
            vec_len_lbb[k] <- NA
            data$bottom_beam_left_x[k] <- NA
            data$bottom_beam_left_y[k] <- NA
        }
        if(vec_len_mbb[k] > up_lim_vec_len_mbb || vec_len_mbb[k] < low_lim_beam || is.na(vec_len_mbb[k]) || data$bottom_beam_middle_like[k] < p_cut_off) {
            vec_len_mbb[k] <- NA
            data$bottom_beam_middle_x[k] <- NA
            data$bottom_beam_middle_y[k] <- NA
        }
        if(vec_len_rbb[k] > up_lim_vec_len_rbb || vec_len_rbb[k] < low_lim_beam || is.na(vec_len_rbb[k]) || data$bottom_beam_right_like[k] < p_cut_off) {
            vec_len_rbb[k] <- NA
            data$bottom_beam_right_x[k] <- NA
            data$bottom_beam_right_y[k] <- NA
        }
        ###End of Addtion after M.Sc. thesis###
        
        
        #Beam walk (distance)
        if(distance_left[k] > up_lim_mark || distance_left[k] < low_lim_mark || is.na(distance_left[k]) || data$mark_far_left_like[k] < p_cut_off || data$mark_left_like[k] < p_cut_off) {
            distance_left[k] <- NA
        }
        if(distance_right[k] > up_lim_mark || distance_right[k] < low_lim_mark || is.na(distance_right[k]) || data$mark_far_right_like[k] < p_cut_off || data$mark_right_like[k] < p_cut_off) {
            distance_right[k] <- NA
        }
        
        ###Added after M.Sc. thesis###
        if(distance_beam_top[k] > up_lim_distance_beam_top || distance_beam_top[k] < low_lim_beam || is.na(distance_beam_top[k]) || data$top_beam_left_like[k] < p_cut_off || data$top_beam_right_like[k] < p_cut_off) {
            distance_beam_top[k] <- NA
        }
        if(distance_beam_bottom[k] > up_lim_distance_beam_bottom || distance_beam_bottom[k] < low_lim_beam || is.na(distance_beam_bottom[k]) || data$bottom_beam_right_like[k] < p_cut_off || data$bottom_beam_right_like[k] < p_cut_off) {
            distance_beam_bottom[k] <- NA
        }
        ###End of Addtion after M.Sc. thesis###
        
        #End of checking
        next #k = k+1
    }
    
    test3 <- list(vec_len_le,vec_len_re,vec_len_snout,vec_len_belly,vec_len_back,vec_len_flp,vec_len_frp,vec_len_blp,vec_len_brp,vec_len_tb,vec_len_tc,vec_len_tt,vec_len_mfl,vec_len_ml,vec_len_mr,vec_len_mfr,vec_len_lbt,vec_len_mbt,vec_len_rbt,vec_len_lbb,vec_len_mbb,vec_len_rbb)
    names(test3) <- c("vec_len_le","vec_len_re","vec_len_snout","vec_len_belly","vec_len_back","vec_len_flp","vec_len_frp","vec_len_blp","vec_len_brp","vec_len_tb","vec_len_tc","vec_len_tt","vec_len_mfl","vec_len_ml","vec_len_mr","vec_len_mfr","vec_len_lbt","vec_len_mbt","vec_len_rbt","vec_len_lbb","vec_len_mbb","vec_len_rbb")
    list2env(test3, envir=.GlobalEnv) #Instead of putting every variable in a named list and then submitting it to the global environment for actual access, one could also use <- when changing and creating variables for the same effect.
    
    test <- list(left_eye, right_eye, snout, belly, back, front_left_paw, front_right_paw, back_left_paw, back_right_paw, tail_base, tail_center, tail_tip, mark_far_left, mark_left, mark_right, mark_far_right,left_beam_top,middle_beam_top,right_beam_top,left_beam_bottom,middle_beam_bottom,right_beam_bottom, p_cut_off, data)
    names(test) <- c("left_eye", "right_eye", "snout", "belly", "back", "front_left_paw", "front_right_paw", "back_left_paw", "back_right_paw", "tail_base", "tail_center", "tail_tip", "mark_far_left", "mark_left", "mark_right", "mark_far_right","left_beam_top","middle_beam_top","right_beam_top","left_beam_bottom","middle_beam_bottom","right_beam_bottom","p_cut_off", "data")
    list2env(test, envir=.GlobalEnv)
    
    test2 <- list(distance_body,distance_bodyheight,distance_paws,distance_tail, distance_left,distance_right,distance_beam_top,distance_beam_bottom)
    names(test2) <- c("distance_body","distance_bodyheight","distance_tail","distance_paws","distance_left","distance_right","distance_beam_top","distance_beam_bottom")
    list2env(test2, envir=.GlobalEnv)
    
    #print("Done with remove_outliers")
}




try_plotting_corrected_data <- function(){
    
    #Checking corrected data
    
    #Mouse body (vector length)
    try(plot(vec_len_le~data$frame),silent=TRUE)
    try(plot(vec_len_re~data$frame),silent=TRUE)
    try(plot(vec_len_snout~data$frame),silent=TRUE)
    try(plot(vec_len_belly~data$frame),silent=TRUE)
    try(plot(vec_len_back~data$frame),silent=TRUE)
    try(plot(vec_len_flp~data$frame),silent=TRUE)
    try(plot(vec_len_frp~data$frame),silent=TRUE)
    try(plot(vec_len_blp~data$frame),silent=TRUE)
    try(plot(vec_len_brp~data$frame),silent=TRUE)
    try(plot(vec_len_tb~data$frame),silent=TRUE)
    try(plot(vec_len_tc~data$frame),silent=TRUE)
    try(plot(vec_len_tt~data$frame),silent=TRUE)
    
    #Beam walk (vector length)
    try(plot(vec_len_mfl~data$frame),silent=TRUE)
    try(plot(vec_len_ml~data$frame),silent=TRUE)
    try(plot(vec_len_mr~data$frame),silent=TRUE)
    try(plot(vec_len_mfr~data$frame),silent=TRUE)
    
    #Mouse body (distance)
    try(plot(distance_body~data$frame),silent=TRUE)
    try(plot(distance_bodyheight~data$frame),silent=TRUE)
    try(plot(distance_paws~data$frame),silent=TRUE)
    try(plot(distance_tail~data$frame),silent=TRUE)
    
    #Beam walk (distance)
    try(plot(distance_left~data$frame),silent=TRUE)
    try(plot(distance_right~data$frame),silent=TRUE)
    
    #print("Done with try_plotting_corrected_data")
}



#names, data
create_csvs <- function(name) {
    
    meta_data <- cbind(data$frame,vec_len_le,vec_len_re,vec_len_snout,vec_len_belly,vec_len_back,vec_len_flp,vec_len_frp,vec_len_blp,vec_len_brp,vec_len_tb,vec_len_tc,vec_len_tt,vec_len_mfl,vec_len_ml,vec_len_mr,vec_len_mfr,vec_len_lbt,vec_len_mbt,vec_len_rbt,vec_len_lbb,vec_len_mbb,vec_len_rbb,distance_body,distance_bodyheight,distance_paws,distance_left,distance_right,distance_beam_top,distance_beam_bottom)
    #meta_data
    
    ifelse(!dir.exists("Corrected_Data"), dir.create("Corrected_Data"), "Folder exists already")
    
    setwd(paste0(data_dir, "\\Corrected_Data"))
    
    #Creating new .csv files for the corrected tracking data, as well as for the meta data.
    write.csv(data,  paste("corrected_", name, ".csv", sep=""), row.names = FALSE)#, row.names = FALSE, col.names = FALSE)
    
    setwd(data_dir)
    
    ifelse(!dir.exists("Meta_Data"), dir.create("Meta_Data"), "Folder exists already")
    
    setwd(paste0(data_dir, "\\Meta_Data"))
    
    write.csv(meta_data, paste("meta_data_",name, ".csv", sep=""), row.names = FALSE)#, row.names = FALSE, col.names = FALSE)
    
    setwd(data_dir)
    
    meta <- list(meta_data)
    names(meta) <- c("meta_data")
    list2env(meta, envir=.GlobalEnv)
    
    #print("Done with create_csvs")
}