
#### LISTS AND LOOKUPS ####
  
    
    ## DATA COLLECTORS ##
    rawCollectors <- sort(unique(c(rawFences$Collector_, 
                                   rawFeatures$Collector_, 
                                   rawWildlife$Collector_,
                                   rawRange$Collector_)))

    # rawCollectors # map text to collector names 
    collector_lookup <- tibble(
      pattern = c("aa",
                  "lliso|bald|basin|Soff",
                  "eth full",
                  "lemi|tristen|tristn|flemi",
                  "hayden",
                  "ean jeff",
                  "bha",
                  "^er$",
                  "brut",
                  "kimi"),
      clean_name = c("Amy Anderson",
                     "Allison Stoff",
                     "Beth Fuller",
                     "Tristen Fleming",
                     "Hayden Campbell",
                     "Dean Jeffers",
                     "Andrew Hahne",
                     "Emily Reed",
                     "Jaden Brutsman",
                     "Kimi Zamuda"))
    
    # map names to organizations
    org_lookup <- tibble(
      name = c("Amy Anderson",
               "Allison Stoff", "Tristen Fleming", "Hayden Campbell", 
               "Jaden Brutsman", "Kaylee Cook", "Kristin Barker", 
               "Kimi Zamuda", "Emily Reed", 
               "Andrew Hahne", "Madison Clarke", "Dean Jeffers"),
      org  = c("WGFD", 
               "BYLL", "BYLL", "BYLL", 
               "BYLL", "BYLL", "BYLL", 
               "AFI", "AFI", 
               "BHA", "BHA", "BHA"))
    
    # map organizations to abbrevs
    abbrev_lookup <- tibble(
      orgname = c("Absaroka Fence Initiative",
                  "Beyond Yellowstone Living Lab",
                  "The Nature Conservancy",
                  "Backcountry Hunters and Anglers",
                  "Wyoming Game and Fish Department",
                  "United States Forest Service",
                  "Bureau of Land Management"),
      abbrev = c("AFI",
                 "BYLL",
                 "TNC",
                 "BHA",
                 "WGFD",
                 "USFS",
                 "BLM"))
    


####  FUNCTIONS ####
    

    
        # clean names 
        clean_collector <- function(name) {
          matched <- collector_lookup %>% 
            filter(str_detect(name, pattern)) %>% 
            pull(clean_name)
          
          if (length(matched) > 0) {
            return(matched[1])
          } else {
            return(str_to_title(name))
          }
        }
        
        # assign organizations
        clean_org <- function(name_input) {
          org <- org_lookup %>% 
            filter(name == name_input) %>% 
            pull(org)
          
          if (length(org) > 0) {
            return(org[1])
          } else {
            return("UnkOther")
          }
        }
        
        # abbreviate organizations
        abbrev_org <- function(org_name) {
          abbrev <- abbrev_lookup %>% 
            filter(orgname == org_name) %>% 
            pull(abbrev)
          
          if (length(abbrev) > 0) {
            return(abbrev[1])
          } else {
            return("Other")
          }
        }
        
        
    # fence condition cleanup
        condition_cleanup <- function(fcc) {
          case_when(
            grepl("down|poor|old|needs|abandoned", fcc, ignore.case = TRUE) ~ "Poor",
            grepl("fa|ional", fcc, ignore.case = TRUE) ~ "Fair",
            grepl("goo|new", fcc, ignore.case = TRUE) ~ "Good",
            is.na(fcc) ~ NA
          )
        }

      # # COLUMN NAMES #
      # 
      # # names(rawFences)
      # newnames <- c(
      #   FenceID = "AFI_FenceI",
      #   Railtop = "Railtop",
      #   Type = "Fence_Mate",
      #   Material = "Material_T",
      #   Condition = "Fence_Cond",
      #   Source = "Source",
      #   landowner = "Land_Agenc",
      #   WGFDregion = "WGFD_Regio",
      #   comments = "Comments",
      #   dateMapped = "Date_of_Co",
      #   dateAdded = "CreationDa",
      #   creator = "Creator",
      #   dateEdited = "EditDate",
      #   arcAccount = "Editor",
      #   access = "Project_Ac",
      #   urgency = "Project_Ur",
      #   status = "Inventory_",
      #   numberWires = "Num_WireOr",
      #   stays = "Wire_Stays")     