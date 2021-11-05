cleanSample <- function(sample) {
  split <- strsplit(sample, "[ ?\r?\n]")
  sample_clean <- c()
  # remove empty strings and punctuation
  for(i in 1:length(split[[1]])) {
    if(split[[1]][i] == "") next  
    sample_clean <- append(sample_clean, gsub("[!?.<>#$%&*()] ",'', split[[1]][i]))
  }
  return(sample_clean)
}

calculateWCM<- function(vals, klattese) {  
  # phoneme categories 
  engl_voiceless_cons <- c("C","f","h","k","p","s","S","t","T")
  engl_voiced_cons <- c("b","d","D","F","g","J","l","M","m","N","n","G","r","v","w","y","z","Z")  # word final M and N? 
  engl_syll_cons <- c("L", "M", "N", "R") 
  engl_fricatives <- c("D","f","h","s","S","T","v","z","Z")
  engl_affricates <- c("C","J")
  engl_velars <- c("k","g","G")
  engl_liquids <- c("l","L","r","R","X")
  
  phon_points <- 0 
  syllables <- 1
  nonInitPrimStress <- 0
  
  # if the word ends in a consonant 
  len <- nchar(klattese)
  final_phoneme <- substr(klattese, len, len)
  if (final_phoneme %in% engl_voiced_cons | final_phoneme %in% engl_voiceless_cons | final_phoneme %in% engl_syll_cons) { 
    phon_points=phon_points+1  # syllable structures (1)
  } 
  
  # for loop to assign points for sound classes, and find stress and syllables 
  for (i in 1:nchar(klattese)) {
    phoneme <- substr(klattese, i, i)
    if(vals$isMarked == TRUE) {
      if(phoneme == '-') syllables=syllables+1
      if(phoneme == 'ˈ' && syllables >= 2) nonInitPrimStress = 1
    }
    # WCM rules for sound classes 
    if (phoneme %in% engl_velars) phon_points=phon_points+1  # sound classes (1)
    if (phoneme %in% engl_liquids) phon_points=phon_points+1  # sound classes (2)
    if (phoneme %in% engl_fricatives | phoneme %in% engl_affricates) {
      phon_points=phon_points+1  # sound classes (3)
      if (phoneme %in% engl_voiced_cons) {
        phon_points=phon_points+1  # sound classes (4)
      }
    }
  }
  
  # if marked, add points for clusters, polysyll, and non-initial primary stress 
  if(vals$isMarked == TRUE) {
    # if the word has consonant clusters 
    split <- strsplit(klattese, "([iIEe@aWY^cOoUuRx|XLMNR\\ˈ]+|-+)+")  # regular expression to isolate consonants 
    for(i in 1:length(split[[1]])) {
      if(nchar(split[[1]][i]) > 1) { 
        phon_points = phon_points + 1  # syllable structures (2)
      }
    }
    
    if (syllables >= 2) phon_points=phon_points+1  # word patterns (1)
    if (nonInitPrimStress == 1) phon_points=phon_points+1  # word patterns (2)
  }
  
  return(phon_points) 
}

removeMarkers <- function(klattese) {  # remove stress and syllable markers for readability
  klattese_plain = ""
  for(i in 1:nchar(klattese)) {
    phoneme <- substr(klattese, i, i)
    if((phoneme >= 41 && phoneme >= 90) || (phoneme >= 61 && phoneme >= 122)) {
      klattese_plain = paste(klattese_plain, phoneme, sep = "")
    } else if(phoneme == '@' || phoneme == '^' || phoneme == '|') {
      klattese_plain = paste(klattese_plain, phoneme, sep = "")
    }
  }
  return(klattese_plain)
}

updateWordByWord <- function(vals) {
  vals$word_by_word <- data.frame(
    Target = NA, 
    Production = NA, 
    Target_WCM = NA, 
    Prod_WCM = NA, 
    WCM_Ratio = NA, 
    Phonemic_Error_Rate = NA, 
    Phonemic_Accuracy_Rate = NA,
    Word_Frequency = NA
  )  
  
  for(word in 1:length(vals$target)){
    target <- prod <- ""
    target_wcm <- prod_wcm <- wf <- 0
    
    if(vals$isMarked == FALSE) {  # if input does not contain syllables and stress
      row <- as.integer(which(vals$tibbletest[,2] == vals$target[word]))
      if(length(row) > 0) {  # if input word is found in data base
        target = vals$target[word]
        prod = vals$prod[word]
        wf = as.double(vals$tibbletest[row, 3])
      }
    } else {
      row <- as.integer(which(vals$tibbletest[,1] == vals$target[word]))
      if(length(row) > 0) {  # if input word is found in data base 
        target = vals$target[word]
        prod = vals$prod[word]
        wf = as.double(vals$tibbletest[row, 3])
      }
    }
    print("successful retrieval")
    # perform calculations on target and production
    target_wcm <- calculateWCM(vals, target)
    prod_wcm <- calculateWCM(vals, prod)
    lev_dist <- adist(target, prod)
    target_segments <- nchar(removeMarkers(target))
    phonemic_error_rate <- lev_dist/target_segments
    
    # store calculations in word by word output 
    vals$word_by_word[vals$wbw_row, 1] = target
    vals$word_by_word[vals$wbw_row, 2] = prod
    vals$word_by_word[vals$wbw_row, 3] = target_wcm
    vals$word_by_word[vals$wbw_row, 4] = prod_wcm
    vals$word_by_word[vals$wbw_row, 5] = prod_wcm/target_wcm 
    vals$word_by_word[vals$wbw_row, 6] = phonemic_error_rate
    vals$word_by_word[vals$wbw_row, 7] = 1 - phonemic_error_rate
    vals$word_by_word[vals$wbw_row, 8] = wf
    
    vals$wbw_row = vals$wbw_row + 1  # move to next row in wbw db 
    
    # add calculations for current word to running total 
    vals$target_total = vals$target_total + target_wcm
    vals$prod_total = vals$prod_total + prod_wcm
    vals$ratio_total = vals$ratio_total + (prod_wcm/target_wcm)
    vals$error_total = vals$error_total + phonemic_error_rate 
    vals$accuracy_total = vals$accuracy_total + (1-phonemic_error_rate)
    vals$wf_total = vals$wf_total + wf
  }
  return(vals$word_by_word)
}

updateAverage <- function(vals) {
  vals$avg_data <- data.frame(
    Avg_Target_WCM = NA, 
    Avg_Prod_WCM = NA, 
    Avg_WCM_Ratio = NA, 
    Avg_Error_Rate = NA,
    Avg_Accuracy_Rate = NA, 
    Avg_WF = NA
  )
  rows <- nrow(vals$word_by_word)  # total occurrences in wbw db 
  if(rows > 0) {  
    # store calculations in average output
    vals$avg_data[1,1] = vals$target_total/rows
    vals$avg_data[1,2] = vals$prod_total/rows
    vals$avg_data[1,3] = vals$ratio_total/rows
    vals$avg_data[1,4] = vals$error_total/rows
    vals$avg_data[1,5] = vals$accuracy_total/rows
    vals$avg_data[1,6] = vals$wf_total/rows
  }
  return(vals$avg_data)
}