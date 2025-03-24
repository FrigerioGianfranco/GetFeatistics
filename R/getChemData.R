# packages needed:
#library(httr)
#library(jsonlite)
#library(utils)
#library(classyfireR)

#' Get Chemical data from PuhChem.
#'
#' Given a set of molecules with at least a known identifier code for each one, it first retrieve the PubChem CID (unless that was the passed identifier), then it gets from PubChem the desired proprierties and other identifier, and can also classify the compounds based on ClassiFire classification. This function needs a stable internet connection and might take several time (depending on the number of compounds and information to retrieve).
#'
#' @param id vector with identifier codes.
#' @param idtype character of length 1 or of the same length as id. The elements of this vector must be one of the following: "CID", "SMILES", "InChI", "InChIKey". If only one is provided, it is assumed that all the id are of that type. Otherwise, the type of each element of id can be specified here.
#' @param properties NULL or a character. If "all" is passed, all the PubChem properties will be fetched. The most wanted properties you might want to get are: "Title", "SMILES", "InChI", "InChIKey", "IUPACName", "MolecularWeight", "ExactMass", "MonoisotopicMass". To know all the PubChem properties run all_PubChem_properties(), and you can also read the full description here: https://pubchem.ncbi.nlm.nih.gov/docs/pug-rest#section=Compound-Property-Tables.
#' @param otheridentifiers NULL or a character. If "all" is passed, all the other identifiers available in PubChem will be fetched. The most common other identifiers you might want to pass here are: "CAS", "HMDB", "KEGG", "ChEBI", "ChEMBL", "DrugBank", "DSSTox".
#' @param synonyms NULL or an integer. If Inf is passed, all the synonyms available in PubChem will be fetched (with a maximum of 999). Otherwise, pass the number of top synonyms reported in PubChem to retrieve.
#' @param ClassiFire NULL or a character. If "all" is passed, all the 7 levels of the ClassiFire classification will be retrieved; otherwise you can pass here only the desired ones. The levels are: "kingdom", "superclass", "class", "subclass", "level 5", "level 6", "level 7", "level 8".
#'
#' @return A data frame (tibble) in which each row is a compound as provided in id. The first column is the id provided, the second column is the PubChem CID, all other columns contain the desired information retrieved. On the console it will be also printed the status of the retrival.
#'
#' @export
getChemData <- function(id, idtype, properties = "all", otheridentifiers = "all", synonyms = 10, ClassiFire = "all") {
  
  if (length(id) < 1) {stop("id must contain at least a value!")}
  if (!is.character(id) & !is.integer(id) & !is.numeric(id) & !is.factor(id)) {stop("id must be a valid vector with compound identifier codes")}
  if (any(is.na(id))) {stop("id must not contain NAs")}
  
  change_messy_case_and_check <- function(the_char_vector, the_names_with_wanted_case) {
    tibble_output <- tibble(char_initial = the_char_vector,
                            char_output = as.character(rep(NA, length(the_char_vector))),
                            is_invalid = as.logical(rep(NA, length(the_char_vector))))
    for (i in 1:length(the_char_vector)) {
      if (tolower(the_char_vector[i]) %in% tolower(the_names_with_wanted_case)) {
        tibble_output$char_output[i] <- the_names_with_wanted_case[which(tolower(the_names_with_wanted_case) == tolower(the_char_vector[i]))]
        tibble_output$is_invalid[i] <- FALSE
      } else {
        tibble_output$is_invalid[i] <- TRUE
      }
    }
    return(tibble_output)
  }
  
  if (!is.character(idtype) & !is.factor(idtype)) {stop('idtype must be a vector containing one or more of these elements: "CID", "SMILES", "InChI", "InChIKey"')}
  if (any(is.na(idtype))) {stop("idtype must not contain NAs")}
  idtype_tibble_check <- change_messy_case_and_check(idtype , c("CID", "SMILES", "InChI", "InChIKey"))
  if (any(idtype_tibble_check$is_invalid)) {
    if (length(unique(idtype_tibble_check$char_initial[which(idtype_tibble_check$is_invalid)])) == 1) {
      stop('"', paste0(unique(idtype_tibble_check$char_initial[which(idtype_tibble_check$is_invalid)]), '" was passed to idtype but it is not valid. The possible idtype are: "CID", "SMILES", "InChI", "InChIKey"'))
    } else {
      stop('"', paste0(paste0(unique(idtype_tibble_check$char_initial[which(idtype_tibble_check$is_invalid)]), collapse = '", "'), '" were passed to idtype but they are not valid. The possible idtype are: "CID", "SMILES", "InChI", "InChIKey"'))
    }
  }
  idtype <- idtype_tibble_check$char_output
  
  if (length(idtype) == 1) {
    idtype <- rep(idtype, length(id))
  } else if (length(idtype) != length(id)) {
    stop("idtype must be of length 1 or of the same length as id")
  }
  
  if (!is.null(properties)) {
    all_PubChem_prop <- all_PubChem_properties()
    if (!is.character(properties) & !is.factor(properties)) {stop('properties must be NULL, "all", or a vector containing valid PubChem properties. To see all the available properties run all_PubChem_properties()')}
    if (length(properties) < 1) {stop('properties must be NULL, "all", or a vector containing valid PubChem properties. To see all the available properties run all_PubChem_properties()')}
    if (any(is.na(properties))) {stop("properties must not contain NAs")}
    if (length(properties) == 1) {
      if (tolower(properties) == "all") {
        properties <- all_PubChem_prop
      }
    }
    properties_tibble_check <- change_messy_case_and_check(properties , all_PubChem_prop)
    if (any(properties_tibble_check$is_invalid)) {
      if (length(unique(properties_tibble_check$char_initial[which(properties_tibble_check$is_invalid)])) == 1) {
        stop('"', paste0(unique(properties_tibble_check$char_initial[which(properties_tibble_check$is_invalid)]), '" was passed to properties but it is not a valid PubChem property. To see all the available properties run all_PubChem_properties()'))
      } else {
        stop('"', paste0(paste0(unique(properties_tibble_check$char_initial[which(properties_tibble_check$is_invalid)]), collapse = '", "'), '" were passed to properties but they are not a valid PubChem properties. To see all the available properties run all_PubChem_properties()'))
      }
    }
    properties <- properties_tibble_check$char_output
    properties <- unique(properties)
  }
  
  if (!is.null(otheridentifiers)) {
    noteworthy_otheridentifiers <- c("CAS", "HMDB", "KEGG", "ChEBI", "ChEMBL", "DrugBank", "DSSTox")
    if (!is.character(otheridentifiers) & !is.factor(otheridentifiers)) {stop(paste0('otheridentifiers must NULL, "all", or a vector containing valid other identifiers available in PubChem, such as: "', paste0(noteworthy_otheridentifiers, collapse = '", "') ,'"'))}
    if (length(otheridentifiers) < 1) {stop(paste0('otheridentifiers must NULL, "all", or a vector containing valid other identifiers available in PubChem, such as: "', paste0(noteworthy_otheridentifiers, collapse = '", "') ,'"'))}
    if (any(is.na(otheridentifiers))) {stop("otheridentifiers must not contain NAs")}
    
    if (!identical(tolower(otheridentifiers), "all")) {
      otheridentifiers_tibble_check <- change_messy_case_and_check(otheridentifiers, noteworthy_otheridentifiers)
      otheridentifiers[which(!otheridentifiers_tibble_check$is_invalid)] <- otheridentifiers_tibble_check$char_output[which(!otheridentifiers_tibble_check$is_invalid)]
    }
    
    otheridentifiers <- unique(otheridentifiers)
  }
  
  if (!is.null(synonyms)) {
    if (length(synonyms) != 1) {stop('synonyms must be NULL, Inf, or an integer number, indicating the number of top synonims from PubChem you want to retreive')}
    if (is.na(synonyms)) {stop("synonyms must not be NA")}
    if (!is.infinite(synonyms)) {
      if (!is.integer(synonyms) & as.integer(synonyms) != synonyms) {stop('synonyms must be NULL, Inf, or an integer number, indicating the number of top synonims from PubChem you want to retreive')}
      synonyms <- as.integer(synonyms)
      if (synonyms > 999) stop("synonyms must be NULL, Inf, or an integer number not greater than 999. If you pass Inf, all synonyms will be retrieved with a maximum of 999")
    }
  }
  
  if (!is.null(ClassiFire)) {
    all_Classes <- c("kingdom", "superclass", "class", "subclass", "level 5", "level 6", "level 7", "level 8")
    if (!is.character(ClassiFire) & !is.factor(ClassiFire)) {stop('ClassiFire must be NULL, "all", or a vector containing valid ClassiFire classes; i.e.: one or more of the following: "kingdom", "superclass", "class", "subclass", "level 5", "level 6", "level 7", "level 8"')}
    if (length(ClassiFire) < 1) {stop('ClassiFire must be NULL, "all", or a vector containing valid ClassiFire classes; i.e.: one or more of the following: "kingdom", "superclass", "class", "subclass", "level 5", "level 6", "level 7", "level 8"')}
    if (any(is.na(ClassiFire))) {stop("ClassiFire must not contain NAs")}
    if (length(ClassiFire) == 1) {
      if (tolower(ClassiFire) == "all") {
        ClassiFire <- all_Classes
      }
    }
    ClassiFire_tibble_check <- change_messy_case_and_check(ClassiFire , c(all_Classes, paste0("ClassiFire_", all_Classes)))
    if (any(ClassiFire_tibble_check$is_invalid)) {
      if (length(unique(ClassiFire_tibble_check$char_initial[which(ClassiFire_tibble_check$is_invalid)])) == 1) {
        stop('"', paste0(unique(ClassiFire_tibble_check$char_initial[which(ClassiFire_tibble_check$is_invalid)]), '" was passed to ClassiFire but it is not a valid class. The valid ClassiFire classes are: "kingdom", "superclass", "class", "subclass", "level 5", "level 6", "level 7", "level 8"'))
      } else {
        stop('"', paste0(paste0(unique(ClassiFire_tibble_check$char_initial[which(ClassiFire_tibble_check$is_invalid)]), collapse = '", "'), '" were passed to ClassiFire but they are not a valid classes. The valid ClassiFire classes are: "kingdom", "superclass", "class", "subclass", "level 5", "level 6", "level 7", "level 8"'))
      }
    }
    ClassiFire <- ClassiFire_tibble_check$char_output
    if (any(grepl("ClassiFire_", ClassiFire))) {
      ClassiFire <- str_remove_all(ClassiFire, "ClassiFire_")
    }
    ClassiFire <- unique(ClassiFire)
  }
  
  
  asking_PubChem <- function(the_url) {
    
    the_response <- httr::GET(the_url)
    the_status <- httr::status_code(the_response)
    the_message <- httr::http_status(the_response)$message
    the_data <- jsonlite::fromJSON(httr::content(x = the_response,
                                                 as = "text",
                                                 encoding = "UTF-8"))
    
    if (the_status == 200) {
      cat("Success!")
      the_output <- the_data
    } else if (the_status == 202) {
      cat("Accepted (asynchronous operation pending)!")
      the_output <- the_data
    } else {                                                 ######### forse qua aggiungere opzione che se server time put, aspetta tipo 2 secondi e poi riprova?!
      cat(paste0(the_message, ". ", the_data$Fault$Message))
      the_output <- NULL
    } 
    
    return(the_output)
  }
  
  fix_special_char <- function(ch) {
    ch_out <- ch %>%
      utils::URLencode(reserved = TRUE)
    return(ch_out)
  }
  
  
  
  output_tibble <- tibble(id = id,
                          idtype = idtype,
                          CID = as.integer(NA))
  lastcolumn_before_otheridentif <- "CID"
  if (!is.null(properties)) {
    attaching_tibble <- as_tibble(matrix(as.character(NA), nrow = nrow(output_tibble), ncol = length(properties), dimnames = list(row = NULL, col = properties)))
    
    output_tibble <- bind_cols(output_tibble, attaching_tibble)
    lastcolumn_before_otheridentif <- colnames(output_tibble)[length(colnames(output_tibble))]
  }
  if (!is.null(otheridentifiers)) {
    if (identical(tolower(otheridentifiers), "all")) {
      otheridentifiers_others <- character()
    } else {
      list_for_otheridentifiers <- vector(mode = "list", length = nrow(output_tibble))
    }
  }
  if (!is.null(synonyms)) {
    if (is.infinite(synonyms)) {
      attaching_tibble <- as_tibble(matrix(as.character(NA), nrow = nrow(output_tibble), ncol = 1, dimnames = list(row = NULL, col = "synonym001")))
      
      output_tibble <- bind_cols(output_tibble, attaching_tibble)
      lastcolumn_synonim <- "synonym001"
    } else {
      attaching_tibble <- as_tibble(matrix(as.character(NA), nrow = nrow(output_tibble), ncol = synonyms, dimnames = list(row = NULL, col = paste0("synonym", zero_prefixing(numbers = 1:synonyms, highest = 999)))))
      
      output_tibble <- bind_cols(output_tibble, attaching_tibble)
    }
  }
  if (!is.null(ClassiFire)) {
    attaching_tibble <- as_tibble(matrix(as.character(NA), nrow = nrow(output_tibble), ncol = length(ClassiFire), dimnames = list(row = NULL, col = ClassiFire)))
    
    output_tibble <- bind_cols(output_tibble, attaching_tibble)
  }
  
  
  for (i in 1:nrow(output_tibble)) {
    cat("\n -- compound ", i, "/", nrow(output_tibble), ":")
    
    if (output_tibble$idtype[i] == "CID") {
      this_CID <- as.integer(output_tibble$id[i])
    } else if (output_tibble$idtype[i] == "SMILES") {
      
      this_SMILES <- output_tibble$id[i]
      
      cat(paste0("\n - Getting CID from SMILE ", this_SMILES, " :\n"))
      
      retr_resul <- asking_PubChem(paste0("https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/smiles/cids/JSON?smiles=", fix_special_char(this_SMILES)))
      
      if (is.null(retr_resul)) {
        this_CID <- NA
      } else {
        this_CID <- as.integer(retr_resul$IdentifierList$CID)
      }
      
    } else if (output_tibble$idtype[i] == "InChI") {
      
      this_InChI <- output_tibble$id[i]
      
      cat(paste0("\n - Getting CID from InChI ", this_InChI, " :\n"))
      
      retr_resul <- asking_PubChem(paste0("https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/inchi/cids/JSON?inchi=", fix_special_char(this_InChI)))
      
      if (is.null(retr_resul)) {
        this_CID <- NA
      } else {
        this_CID <- as.integer(retr_resul$IdentifierList$CID)
      }
      
    } else if (output_tibble$idtype[i] == "InChIKey") {
      
      this_InChIKey <- output_tibble$id[i]
      
      cat(paste0("\n - Getting CID from InChIKey ", this_InChIKey, " :\n"))
      
      retr_resul <- asking_PubChem(paste0("https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/inchikey/cids/JSON?inchikey=", fix_special_char(this_InChIKey)))
      
      if (is.null(retr_resul)) {
        this_CID <- NA
      } else {
        this_CID <- as.integer(retr_resul$IdentifierList$CID)
      }
    }
    
    output_tibble$CID[i] <- this_CID
    
    if (!is.na(this_CID)) {
      
      if (!is.null(properties)) {
        
        cat(paste0("\n - Getting properties from CID ", this_CID, " :\n"))
        
        propr_result <- asking_PubChem(paste0("https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/cid/", this_CID, "/property/", paste0(properties, collapse = ","), "/JSON"))
        
        if (is.null(propr_result)) {
          output_tibble[i, properties] <- as.character(NA)
        } else {
          
          properties_table <- as_tibble(propr_result$PropertyTable$Properties)
          
          if ("CID" %in% colnames(properties_table)) {
            if (this_CID != properties_table$CID[1]) {cat("\nIt's wired: CID retrieved from proprierties is ", properties_table$CID[1])}
            properties_table <- properties_table[, colnames(properties_table)[which(colnames(properties_table) != "CID")]]
          }
          properties_table <- mutate_all(properties_table, as.character)
          
          output_tibble[i, colnames(properties_table)] <- properties_table[1,]
        }
      }
      
      if (!is.null(otheridentifiers)) {
        
        if (!identical(tolower(otheridentifiers), "all")) {
          list_for_otheridentifiers[[i]] <- vector(mode = "list", length = length(otheridentifiers))
          names(list_for_otheridentifiers[[i]]) <- otheridentifiers
        }
        
        cat(paste0("\n - Getting other identifiers from CID ", this_CID, " :\n"))
        
        otheridentif_result <- asking_PubChem(paste0("https://pubchem.ncbi.nlm.nih.gov/rest/pug_view/data/compound/", this_CID, "/JSON/?heading=Other+Identifiers"))
        
        if (!is.null(otheridentif_result)) {
          
          otheridentif_table <- as_tibble(otheridentif_result$Record$Reference)
          
          if (identical(tolower(otheridentifiers), "all")) {
            
            if (any(duplicated(otheridentif_table$SourceName))) {
              otheridentif_table$SourceName <- fix_duplicated(otheridentif_table$SourceName, zeros = TRUE, define_highest_for_zeros = 99, start_with_zero = FALSE, exclude_the_first = TRUE, NA_as_character = FALSE)
            }
            
            otheridentifiers_new <- otheridentif_table$SourceName[which(!otheridentif_table$SourceName %in% otheridentifiers_others)]
            
            if (length(otheridentifiers_new) > 1) {
              output_tibble_before <- output_tibble[,1:which(colnames(output_tibble)==lastcolumn_before_otheridentif)]
              output_tibble_after <- output_tibble[,(which(colnames(output_tibble)==lastcolumn_before_otheridentif)+1):ncol(output_tibble)]
              
              attaching_tibble <- as_tibble(matrix(as.character(NA), nrow = nrow(output_tibble), ncol = length(otheridentifiers_new), dimnames = list(row = NULL, col = otheridentifiers_new)))
              
              output_tibble <- bind_cols(output_tibble_before, attaching_tibble)
              output_tibble <- bind_cols(output_tibble, output_tibble_after)
              
              lastcolumn_before_otheridentif <- otheridentifiers_new[length(otheridentifiers_new)]
              
              otheridentifiers_others <- c(otheridentifiers_others, otheridentifiers_new)
            }
            
            output_tibble[i, otheridentif_table$SourceName] <- as_tibble(matrix(as.character(otheridentif_table$SourceID), nrow = 1, ncol = length(otheridentif_table$SourceName), dimnames = list(row = NULL, col = otheridentif_table$SourceName)))
            
            if (i == nrow(output_tibble)) {
              output_tibble <- output_tibble[, c(colnames(output_tibble)[1:((which(colnames(output_tibble)==otheridentifiers_others[1]))-1)],
                                                 sort(otheridentifiers_others),
                                                 colnames(output_tibble)[((which(colnames(output_tibble)==otheridentifiers_others[length(otheridentifiers_others)]))+1):length(colnames(output_tibble))])]
            }
            
          } else {
            
            for (a in otheridentifiers) {
              
              list_for_otheridentifiers[[i]][[a]] <- otheridentif_table$SourceID[which(grepl(a, otheridentif_table$SourceName))]
              names(list_for_otheridentifiers[[i]][[a]]) <- fix_duplicated(otheridentif_table$SourceName[which(grepl(a, otheridentif_table$SourceName))],
                                                                           zeros = TRUE, define_highest_for_zeros = 99, start_with_zero = FALSE, exclude_the_first = TRUE, NA_as_character = FALSE)
              
              
            }
          }
        } else {
          if (!identical(tolower(otheridentifiers), "all")) {
            for (a in otheridentifiers) {
              list_for_otheridentifiers[[i]][[a]] <- character()
              names(list_for_otheridentifiers[[i]][[a]]) <- character()
            }
          }
        }
      }
      
      
      if (!is.null(synonyms)) {
        
        cat(paste0("\n - Getting synonyms from CID ", this_CID, " :\n"))
        
        synonyms_result <- asking_PubChem(paste0("https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/cid/", this_CID, "/synonyms/JSON"))
        
        if (!is.null(synonyms_result)) {
          
          all_these_synonyms <- as.character(synonyms_result$InformationList$Information$Synonym[[1]])
          
          if (length(all_these_synonyms) > 999) {
            all_these_synonyms <- all_these_synonyms[1:999]
          }
          
          names(all_these_synonyms) <- paste0("synonym", zero_prefixing(1:length(all_these_synonyms), highest = 999))
          
          if (is.infinite(synonyms)) {
            if (all(names(all_these_synonyms) %in% colnames(output_tibble))) {
              output_tibble[i, names(all_these_synonyms)] <- as_tibble(matrix(all_these_synonyms, nrow = 1, ncol = length(all_these_synonyms), dimnames = list(row = NULL, col = names(all_these_synonyms))))
            } else {
              synonyms_to_add <- names(all_these_synonyms)[which(!names(all_these_synonyms) %in% colnames(output_tibble))]
              
              output_tibble_untill_lastsyn <- output_tibble[, 1:max(which(grepl("synonym", colnames(output_tibble))))]
              output_tibble_new_syn <- as_tibble(matrix(as.character(NA), nrow = nrow(output_tibble), ncol = length(synonyms_to_add), dimnames = list(row = NULL, col = synonyms_to_add)))
              output_tibble_after_lastsyn <- output_tibble[, (max(which(grepl("synonym", colnames(output_tibble))))+1):length(colnames(output_tibble))]
              
              output_tibble <- bind_cols(output_tibble_untill_lastsyn, output_tibble_new_syn)
              output_tibble <- bind_cols(output_tibble, output_tibble_after_lastsyn)
              
              output_tibble[i, names(all_these_synonyms)] <- as_tibble(matrix(all_these_synonyms, nrow = 1, ncol = length(all_these_synonyms), dimnames = list(row = NULL, col = names(all_these_synonyms))))
            }
          } else {
            
            if (length(all_these_synonyms) > synonyms) {
              all_these_synonyms <- all_these_synonyms[1:synonyms]
            }
            
            output_tibble[i, names(all_these_synonyms)] <- as_tibble(matrix(all_these_synonyms, nrow = 1, ncol = length(all_these_synonyms), dimnames = list(row = NULL, col = names(all_these_synonyms))))
          }
        }
      }
    }
    
    
    
    
    if (!is.null(ClassiFire)) {
      
      if (output_tibble$idtype[i] == "InChIKey") {
        InChIKey_for_ClassF <- output_tibble$id[i]
      } else if ("InChIKey" %in% colnames(output_tibble)) {
        InChIKey_for_ClassF <- output_tibble$InChIKey[i]
      } else {
        cat("\n - Cannot retreive ClassiFire because InChIKey is not the id and it has not even be tretrieved among the properties")
        InChIKey_for_ClassF <- NULL
      }
      
      
      if (!is.na(InChIKey_for_ClassF) & !is.null(InChIKey_for_ClassF)) {
        
        cat(paste0("\n - Getting ClassiFire from InChIKey ", InChIKey_for_ClassF, " :"))
        
        The_classification <- suppressMessages(classyfireR::get_classification(InChIKey_for_ClassF))
        
        if (is.null(The_classification)) {
          cat("\nFailed...")
        } else {
          cat("\nSuccess!")
          
          ClassiF_tibble <- as_tibble(The_classification@classification)
          
          ClassiF_vector <- ClassiF_tibble$Classification
          names(ClassiF_vector) <- ClassiF_tibble$Level
          
          ClassiF_vector <- ClassiF_vector[which(names(ClassiF_vector) %in% ClassiFire)]
          
          output_tibble[i, names(ClassiF_vector)] <- as_tibble(matrix(as.character(ClassiF_vector), nrow = 1, ncol = length(ClassiF_vector), dimnames = list(row = NULL, col = names(ClassiF_vector))))
        }
      }
    }
    
    cat("\n")
  }
  
  if (!is.null(otheridentifiers) & !identical(tolower(otheridentifiers), "all")) {
    tibble_for_otheridentifiers <- as_tibble(matrix(as.character(NA), nrow = nrow(output_tibble), ncol = 0))
    
    for (a in otheridentifiers) {
      this_combined_names <- character()
      for (i in 1:length(list_for_otheridentifiers)) {
        this_combined_names <- c(this_combined_names, names(list_for_otheridentifiers[[i]][[a]]))
      }
      this_combined_names <- unique(this_combined_names)
      
      if (length(this_combined_names) > 0 ) {
        tibble_for_otheridentifiers[, this_combined_names] <- as.character(NA)
      } else {
        tibble_for_otheridentifiers[, a] <- as.character(NA)
      }
    }
    
    for (i in 1:length(list_for_otheridentifiers)) {
      this_total_vector_of_identifier <- character()
      names(this_total_vector_of_identifier) <- character()
      
      for (a in otheridentifiers) {
        this_total_vector_of_identifier <- c(this_total_vector_of_identifier, list_for_otheridentifiers[[i]][[a]])
      }
      
      this_total_vector_of_identifier <- this_total_vector_of_identifier[which(!duplicated(names(this_total_vector_of_identifier)))]
      
      tibble_for_otheridentifiers[i, names(this_total_vector_of_identifier)] <- as_tibble(matrix(this_total_vector_of_identifier, nrow = 1, ncol = length(this_total_vector_of_identifier), dimnames = list(row = NULL, col = names(this_total_vector_of_identifier))))
    }
    
    output_tibble_before <- output_tibble[,1:which(colnames(output_tibble)==lastcolumn_before_otheridentif)]
    output_tibble_after <- output_tibble[,(which(colnames(output_tibble)==lastcolumn_before_otheridentif)+1):ncol(output_tibble)]
    
    output_tibble <- bind_cols(output_tibble_before, tibble_for_otheridentifiers)
    output_tibble <- bind_cols(output_tibble, output_tibble_after)
  }
  
  return(output_tibble)
}
