cleanSample <- function(sample) {  
 split <- strsplit(sample, "[ ?\r?\n]")  # split on spaces or newlines
  sample_clean <- c()
  # remove empty strings and punctuation, format stress markers
  for(i in 1:length(split[[1]])) {
    word = split[[1]][i]
    if(word == "") next  # skip any strings that are empty due to splitting on space
    if(grepl("'", word, fixed=TRUE) == TRUE) {  # If apostrophe was used as stress marker in input
      word <- gsub("'", "ˈ", word)  # Replace it with true klattese stress marker 
    }
    sample_clean <- append(sample_clean, gsub("[!?.<>#$%&*()] ",'', word))
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
  engl_vowels <- c("i","I","E","e","@","a","W","Y","^","c","O","o","U","u","R","x","|","X","L","M","N","R")
  
  vowels <- 0
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

    # added this to allow syllable count without syllable marking
    else  { # if not marked
      if(phoneme %in% engl_vowels) vowels = vowels + 1
      
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
  
    # add points for if the word has consonant clusters 
    split <- strsplit(klattese, "([iIEe@aWY^cOoUuRx|XLMNR\\ˈ]+|-+)+")  # regular expression to isolate consonants 
    for(i in 1:length(split[[1]])) {
      if(nchar(split[[1]][i]) > 1) { 
        phon_points = phon_points + 1  # syllable structures (2)
      }
    }

    # if marked, add points for polysyll, and non-initial primary stress 
    
  if(vals$isMarked == TRUE) {
        
    if (syllables > 2) phon_points=phon_points+1  # word patterns (1)
    if (nonInitPrimStress == 1) phon_points=phon_points+1  # word patterns (2)
  } else { #if not marked, add points for syllables based on numvowels >2
    
    if(vowels > 2) phon_points = phon_points + 1  # word patterns (1)
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
    target <- vals$target[word]
    prod <- vals$prod[word]
    target_wcm <- prod_wcm <- wf <- 0
    str <- vals$target[word]
    
    if(vals$isMarked == FALSE) {  # if input does not contain syllables and stress
      print(target)
      target <- removeMarkers(target)  # remove markers for error forgiveness
      print(target)
      row <- as.integer(which(vals$tibbletest[,2] == target))
      if(length(row) > 0) {  # if input word is found in data base
        wf <- as.double(max(vals$tibbletest[row, 3]))  # use max word frequency found
      } else wf <- 0  # if input not found in db, wf = NA
    } else {  # input is marked for syllables and stress
      row <- as.integer(which(vals$tibbletest[,1] == vals$target[word]))
      if(length(row) > 0) {  # if input word is found in data base 
        wf = as.double(max(vals$tibbletest[row, 3]))  # use max word frequency found
      }  else wf <- 0  # if input not found in db, wf = NA
    }
    # perform calculations on target and production
    target_wcm <- calculateWCM(vals, target)
    prod_wcm <- calculateWCM(vals, prod)
    wcm_ratio <- 0
    if(target_wcm > 0) wcm_ratio = prod_wcm/target_wcm
    if(target_wcm == 0 && prod_wcm == 0) wcm_ratio = 1
    lev_dist <- adist(target, prod)
    target_segments <- nchar(removeMarkers(target))
    phonemic_error_rate <- lev_dist/target_segments
    
    # store calculations in word by word output 
    vals$word_by_word[vals$wbw_row, 1] = target
    vals$word_by_word[vals$wbw_row, 2] = prod
    vals$word_by_word[vals$wbw_row, 3] = toString(target_wcm)
    vals$word_by_word[vals$wbw_row, 4] = toString(prod_wcm)
    vals$word_by_word[vals$wbw_row, 5] = toString(round(wcm_ratio, 3)) 
    vals$word_by_word[vals$wbw_row, 6] = toString(round(phonemic_error_rate, 3))
    vals$word_by_word[vals$wbw_row, 7] = toString(round(1 - phonemic_error_rate, 3))
    vals$word_by_word[vals$wbw_row, 8] = toString(round(wf, 3))
    
    vals$wbw_row = vals$wbw_row + 1  # move to next row in wbw db 
    
    # add calculations for current word to running total 
    vals$target_total = vals$target_total + target_wcm
    vals$prod_total = vals$prod_total + prod_wcm
    vals$ratio_total = vals$ratio_total + wcm_ratio
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
    vals$avg_data[1,1] = toString(round(vals$target_total/rows, 3))
    vals$avg_data[1,2] = toString(round(vals$prod_total/rows, 3))
    vals$avg_data[1,3] = toString(round(vals$ratio_total/rows, 3))
    vals$avg_data[1,4] = toString(round(vals$error_total/rows, 3))
    vals$avg_data[1,5] = toString(round(vals$accuracy_total/rows, 3))
    vals$avg_data[1,6] = toString(round(vals$wf_total/rows, 3))
  }
  return(vals$avg_data)
}
