write_agefile_stepps <- function (download, survey.year, chronology = 1, path, corename, site.id, cal.prog = "Bacon") 
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
      chron.controls <- get_chroncontrol(chronologyid = 6346, 
                                         verbose = FALSE)
#     } else if {
#       
    } else {
      chron.controls <- get_chroncontrol(chronologyid = download$sample.meta$chronology.id[1], 
                                         verbose = FALSE)
    }
    if (nrow(chron.controls$chron.control) < 2) {
      stop("Chronology must have more than a single date for proper analysis.")
    }
    uncal <- c("Radiocarbon", "Radiocarbon, reservoir correction", 
               "Radiocarbon, average of two or more dates")
    stepps_controls = c("Radiocarbon", "Radiocarbon, reservoir correction", 
                        "Radiocarbon, average of two or more dates", "Core top", 
                        "Lead-210", "Radiocarbon, calibrated")
    pre_depths = read.csv(file='data/cal_data_mid_depth_2014-07-28.csv', stringsAsFactors=FALSE)
    
    #     # split clh out because of numeric vs string id issue
    #     pre_depths_clh = pre_depths[(substr(pre_depths$id,1,1)=='C'),]
    #     pre_depths = pre_depths[!(substr(pre_depths$id,1,1)=='C'),]
    
    if (!tolower(cal.prog) %in% c("bacon", "clam")) {
      stop("You must define either Bacon or Clam as your model output.")
    }
    if (cal.prog == "Bacon") {
      idx.keep  <- which(chron.controls$chron.control$control.type %in% stepps_controls)
      if (length(idx.keep)>0){
        
        types = as.vector(chron.controls$chron.control$control.type[idx.keep])
        types[types == 'Radiocarbon, reservoir correction'] = 'Radiocarbon_rc'
        types[types == 'Radiocarbon, calibrated'] = 'Radiocarbon_cal'
        
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
          } else if (any(chron[top.cond, "error"] == 0)) {
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
          radio.exact <- radio.cond & (chron$error == 0)
          if ( any(radio.exact) ){
            chron[radio.exact,'error'] = round(mean(chron$error[radio.cond & (chron$error != 0)]))
          } 
        }
        
        depths <- unique(download$sample.meta$depth)

        # needs to have a pre-settlement sample defined, and not be Green Lake, Wolverine, or Kotiranta
        clh.ids     <- c('CLH1', 'CLH7', 'CLH8', 'CLH9')
        clh.neo.ids <- c(13069, 13073, 13060, 13071)
#         clh.ids     <- c('CLH1', 'CLH4', 'CLH7', 'CLH8', 'CLH9')
#         clh.neo.ids <- c(13069, 1633, 13073, 13060, 13071)
        if (site.id %in% clh.neo.ids){
          site.id = clh.ids[which(clh.neo.ids == site.id)]
        }
        
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
        
        if (any(nchar(as.vector(chron$labid)) > 20)) {
          print('Why can BACON only write a limited number of characters?!?!')
          #           chron$labid[nchar(as.vector(chron$labid)) > 4] = substr(chron$labid[nchar(as.vector(chron$labid)) > 4], 1, 20)
          chron$labid[nchar(as.vector(chron$labid)) > 20 ] = substr(chron$labid[nchar(as.vector(chron$labid)) > 4], 1, 20)
        }
        
        run_flag = FALSE
        
        if (length(chron$age[!is.na(chron$age)]) > 1) {
          run_flag = TRUE
        } 
        
        ndates = length(chron$age[!is.na(chron$age)])
        
      } else {
        run_flag = FALSE
        ndates   = 0
      }
      
      
      # #      suit <- cbind(suit, c(site.id, corename, run_flag))
      
    }
    #     if (cal.prog == "Clam") {
    #       chron <- data.frame(ID = paste0(chron.controls$chron.control$ControlType, 
    #                                       "_", chron.controls$chron.control$ChronControlID), 
    #                           C14_age = chron.controls$chron.control$Age, cal_BP = chron.controls$chron.control$Age, 
    #                           error = abs(chron.controls$chron.control$Age - 
    #                                         chron.controls$chron.control$AgeYoungest), 
    #                           reservoir = NA, depth = chron.controls$chron.control$Depth)
    #       chron$cal_BP[chron.controls$chron.control$ControlType %in% 
    #                      uncal] <- NA
    #       chron$C14_age[!chron.controls$chron.control$ControlType %in% 
    #                       uncal] <- NA
    #     }
    depths <- download$sample.meta$depth
    if (!corename %in% list.files(paste0(path, "/Cores"))) {
      works <- dir.create(path = paste0(path, "/Cores/", 
                                        corename))
      if (!works) {
        stop("Could not create the directory.  Check the path, corename and your permissions.")
      }
    }
    
    write.csv(chron, paste0(path, "/Cores/", corename, "/", 
                            corename, ".csv"), row.names = FALSE, quote=TRUE)
    #     write.csv(chron, paste0(path, "/Cores/", corename, "/", 
    #                             corename, "_depths.txt"), row.names = FALSE)
    write.table(depths, paste0(path, "/Cores/", corename, "/", 
                               corename, "_depths.txt"), col.names = FALSE, row.names = FALSE, quote=TRUE)
  }
  
  return(list(run_flag=run_flag, ndates=ndates))
}


