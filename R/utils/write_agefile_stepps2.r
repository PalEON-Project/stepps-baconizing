write_agefile_stepps2 <- function (download, survey.year, chronology = 1, path, corename, site.id, cal.prog = "Bacon") 
{
  if (!file.exists(paste0(path, "/Cores"))) {
    stop(paste0("Core directory must exist.  There is no directory at ", 
                path, "/Cores"))
  }
  if (!"download" %in% class(download) | !(c("chronologies") %in% 
                                             names(download) | c("chronologies") %in% names(download[[1]]))) {
    stop(paste0("write_agefile can only operate on valid download ", 
                "objects with valid chronologies"))
  }
  if ("download" %in% class(download) & (c("chronologies") %in% 
                                           names(download) | c("chronologies") %in% names(download[[1]])))  {
    #     chron.controls <- get_chroncontrol(chronologyid = download$chronologies[[chronology]]$chronology.id[1], 
    #                                        verbose = FALSE)
    
    # use uncalibrated RC dates; sometimes not default
    if (site.id == 12001){
      #       chron.controls <- get_chroncontrol(chronologyid = 6346, 
      #                                          verbose = FALSE)
      chron.controls <- get_chroncontrol(6346, 
                                         verbose = FALSE)
    } else if (site.id == 3131) {
      chron.controls <- get_chroncontrol(1407, verbose = FALSE)
      chron.controls$chron.control$control.type = as.vector(chron.controls$chron.control$control.type)
      chron.controls$chron.control$control.type[1] = 'Core top'
    } else {
      chron.controls <- get_chroncontrol(download$sample.meta$chronology.id[1], 
                                         verbose = FALSE)
    }
    if (nrow(chron.controls$chron.control) < 2) {
      stop("Chronology must have more than a single date for proper analysis.")
    }
    
    # why do some markers have NA ages? not a useful marker!
    chron.controls$chron.control <- chron.controls$chron.control[!is.na(chron.controls$chron.control$age),]
    
    uncal <- c("Radiocarbon", "Radiocarbon, reservoir correction", 
               "Radiocarbon, average of two or more dates")
    #     stepps_controls = c("Radiocarbon", "Radiocarbon, reservoir correction", 
    #                         "Radiocarbon, average of two or more dates", "Core top", 
    #                         "Lead-210", "Radiocarbon, calibrated")
    
    # no lead for now
    stepps_controls = c("Radiocarbon", "Radiocarbon, reservoir correction", 
                        "Radiocarbon, average of two or more dates", "Core top", 
                        "Radiocarbon, calibrated", "Annual laminations (varves)",
                        "Radiocarbon, calibrated from calendar years")
    
    pre_depths = read.csv(file='data/cal_data_mid_depth_v4.csv', stringsAsFactors=FALSE)
    cal_ids    = read.csv(file='data/pollen_meta_2014-05-01.csv', stringsAsFactors=FALSE)
    
    amb_rise = FALSE
    if (!(site.id %in% pre_depths$id)) {
      stepps_controls = c(stepps_controls, "Ambrosia rise", "European settlement horizon")
      
      if (any(tolower(substr(chron.controls$chron.control$control.type, 1, 3)) == "amb")){
        amb_rise = TRUE
        print("Keeping Ambrosia rise")
        print(chron.controls$chron.control$control.type[which(tolower(substr(chron.controls$chron.control$control.type, 1, 3)) == "amb")])
      }
      if (any(tolower(substr(chron.controls$chron.control$control.type, 1, 3)) == "eur")){
        amb_rise = TRUE
        print("Keeping European settlement horizon")
        print(chron.controls$chron.control$control.type[which(tolower(substr(chron.controls$chron.control$control.type, 1, 3)) == "eur")])
        
        if (site.id == 14270) {
          chron.controls$chron.control$depth[which(tolower(substr(chron.controls$chron.control$control.type, 1, 3)) == "eur")] = 43
        }
        
      }
      if (any(chron.controls$chron.control$control.type == 'Biostratigraphic')){
        print("Dropping biostratigraphic marker")
      }
    }
    
    
    #     # split clh out because of numeric vs string id issue
    #     pre_depths_clh = pre_depths[(substr(pre_depths$id,1,1)=='C'),]
    #     pre_depths = pre_depths[!(substr(pre_depths$id,1,1)=='C'),]
    
    if (!tolower(cal.prog) %in% c("bacon", "clam")) {
      stop("You must define either Bacon or Clam as your model output.")
    }
    if (cal.prog == "Bacon") {
      run_flag = NA
      idx.keep  = which(chron.controls$chron.control$control.type %in% stepps_controls)
      lead.cond = chron.controls$chron.control$control.type == "Lead-210"
      if (length(idx.keep)>0){
        
        types = as.vector(chron.controls$chron.control$control.type[idx.keep])
        types[types == 'Radiocarbon, reservoir correction'] = 'Radiocarbon_rc'
        types[types == 'Radiocarbon, calibrated'] = 'Radiocarbon_cal'
        types[types == 'Radiocarbon, calibrated from calendar years'] = 'Radiocarbon_cal'
        types[types == 'European settlement horizon'] = 'Settlement'
        
        chron <- data.frame(labid = paste0(types, 
                                           "_", chron.controls$chron.control$chron.control.id[idx.keep]), 
                            #         chron <- data.frame(labid = paste0(chron.controls$chron.control$ControlType[idx.keep], 
                            #                                            "_", chron.controls$chron.control$ChronControlID[idx.keep]), 
                            age = as.numeric(chron.controls$chron.control$age[idx.keep]), 
                            error = abs(as.numeric(chron.controls$chron.control$age[idx.keep]) - as.numeric(chron.controls$chron.control$age.young[idx.keep])), 
                            depth = as.numeric(chron.controls$chron.control$depth[idx.keep]), 
                            cc = ifelse(chron.controls$chron.control$control.type[idx.keep] %in% uncal, 1, 0), stringsAsFactors=FALSE)
        
        # deal with zero error lead-210 dates
        lead.cond <- types == 'Lead-210'
        if ( (sum(lead.cond)>0) & (any(chron[lead.cond, 'error'] == 0)) ) {
          chron = set_lead_error(chron, types)
        }
        
        top.cond <- types == "Core top" 
        
        if (sum(top.cond) > 0){
          if (is.na(chron$age[top.cond])){
            chron = chron[!top.cond,]
            types = types[!top.cond]
          } else if ((any(chron[top.cond, "error"] == 0)) | any(is.na(chron[top.cond, "error"]))) {
            chron[top.cond, "error"] = 5
          }
          
          # fix core tops
          if (site.id == 1548){         #kotiranta
            #             chron = chron[!top.cond,]
          } else if (site.id == 982) { # green lake
            chron[top.cond, 'depth'] = 0
            chron[top.cond, 'age']   = -22
            chron[top.cond, 'error'] = 5          
          } else if (site.id == 3032) { # wolverine
            chron[top.cond, 'depth'] = 0
            chron[top.cond, 'age']   = -30
            chron[top.cond, 'error'] = 5 
          }
          
        }
        
        
        radio.cond <- types %in% c("Radiocarbon", "Radiocarbon_rc", "Radiocarbon, average of two or more dates")
        if ( any(radio.cond) ) {
          radio.exact <- radio.cond & ((chron$error == 0) | (is.na(chron$error)))
          if ( any(radio.exact) ){
            chron[radio.exact,'error'] = round(mean(chron$error[radio.cond & (chron$error != 0)], na.rm=TRUE))
          } 
        } 
        if (any(is.na(chron$error))) {
          chron$error[which(is.na(chron$error))] = 50
        }
        
        depths <- unique(download$sample.meta$depth)
        
#         # needs to have a pre-settlement sample defined, and not be Green Lake, Wolverine, or Kotiranta
#         clh.ids     <- c('CLH1', 'CLH7', 'CLH8', 'CLH9', 'CLH3', 'CLH2', 'CLH6')
#         clh.neo.ids <- c(13069, 13073, 13060, 13071, 14446, 15489, 15882)
#         #         clh.ids     <- c('CLH1', 'CLH4', 'CLH7', 'CLH8', 'CLH9')
#         #         clh.neo.ids <- c(13069, 1633, 13073, 13060, 13071)
#         if (site.id %in% clh.neo.ids){
#           site.id = clh.ids[which(clh.neo.ids == site.id)]
#         }
        
        # dataset ID 1548: Kotiranta; no core top  
        # dataset ID 982: Green Lake; top 10cm collapsed upon extrution
        # dataset ID 369: Chippewa Bog; no core top
        # dataset ID 3473: Lake Mary; no surface sample
        if ( ( as.character(site.id) %in% pre_depths$id) & !(site.id %in% c(1548, 982, 369, 3473)) ){
          labid = 'Presettlement_paleon'
          #         age   = 1850
          
          # McNearney Lake is in a State Forest
          if (site.id %in% c(1725, 3485, 3486)){
            age = 60
          } else {
            age   = 1950 - survey.year # want to make sure we get the presettlement date!   
          }
          
          error = 50
          depth = pre_depths$depth[which(pre_depths$id == as.character(site.id))]
          cc    = 0   
          chron <- rbind(chron, data.frame(labid, age, error, depth, cc))
        }
        
        chron <- chron[with(chron, order(depth)),]
        
        nchar_ub = 21
        if (any(nchar(as.vector(chron$labid)) > nchar_ub)) {
          print('Why can BACON only write a limited number of characters?!?!')
          #           chron$labid[nchar(as.vector(chron$labid)) > 4] = substr(chron$labid[nchar(as.vector(chron$labid)) > 4], 1, 20)
          chron$labid[nchar(as.vector(chron$labid)) > nchar_ub ] = substr(chron$labid[nchar(as.vector(chron$labid)) > nchar_ub ], 1, nchar_ub)
        }
        
        if ((length(chron$age[!is.na(chron$age)]) > 1) & (sum(lead.cond)==0)) {
          run_flag = TRUE
          ndates   = length(chron$age[!is.na(chron$age)])
          gc_age_max = max(chron$age[!is.na(chron$age)])
          gc_age_min = min(chron$age[!is.na(chron$age)])
          dates    = "RC, CT, or presett"
        } else if ((length(chron$age[!is.na(chron$age)]) > 1) & sum(lead.cond)>0) {
          run_flag = FALSE
          ndates   = length(chron$age[!is.na(chron$age)])
          gc_age_max = max(chron$age[!is.na(chron$age)])
          gc_age_min = min(chron$age[!is.na(chron$age)])
          dates    = "Lead"
          print("Lead dates, run flag set to FALSE.")
        } else {
          run_flag = FALSE
          ndates   = 0
          gc_age_max = NA
          gc_age_min = NA
          dates    = "None"
        }
        
        if (!corename %in% list.files(paste0(path, "/Cores"))) {
          works <- dir.create(path = paste0(path, "/Cores/", 
                                            corename))
          if (!works) {
            stop("Could not create the directory.  Check the path, corename and your permissions.")
          }
        }
        write.csv(chron, paste0(path, "/Cores/", corename, "/", 
                                corename, ".csv"), row.names = FALSE, quote=TRUE)
        
      } else {
        run_flag = FALSE
        ndates   = 0
        gc_age_min = NA
        gc_age_max = NA
        if (sum(lead.cond)>0) {
          dates = "Lead"
        } else {
          dates = 'None'
        }
      }

      depths <- download$sample.meta$depth
      if (!corename %in% list.files(paste0(path, "/Cores"))) {
        works <- dir.create(path = paste0(path, "/Cores/", 
                                          corename))
        if (!works) {
          stop("Could not create the directory.  Check the path, corename and your permissions.")
        }
      }

      write.table(depths, paste0(path, "/Cores/", corename, "/", 
                                 corename, "_depths.txt"), col.names = FALSE, row.names = FALSE, quote=TRUE)
    }
    
    
  }
  
  return(list(run_flag=run_flag, ndates=ndates, dates=dates, gc_age_min=gc_age_min, gc_age_max=gc_age_max, amb_rise=amb_rise))
}
  