#' get_symbols %% ~~function to do ... ~~
#' 
#' Gets phonetic symbols
#' 
#' Output:
#' 
#' Vector list containing the segments specified
#' 
#' @aliases get_symbols
#' @param symbol_format transcription type
#' @param consonant_set consonant subset specification (based on features)
#' @param vowel_set vowel subset specification (based on features)
#' @param consonant_set_manual manual list of consonants
#' @param vowel_set_manual manual list of vowels
#' @return %% ~Describe the value returned %% If it is a LIST, use %%
#' \item{comp1 }{Description of 'comp1'} %% \item{comp2 }{Description of
#' 'comp2'} %% ...
#' @note %% ~~further notes~~
#' @author Simon Gonzalez - simon.gonzalez@@anu.edu.au
#' @seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
#' @references %% ~put references to the literature/web site here ~
#' @keywords ~kwd1 ~kwd2
#' @examples
#' @export
#' 
#' MAUS_segments = get_symbols('maus')
#' 
#' 
#' 
get_symbols <- function(symbol_format = 'ipa', consonant_set = 'all', vowel_set = 'all',
                   consonant_set_manual = NULL, vowel_set_manual = NULL){

  set_ids = sort(c('celex', 'arpabet', 'disc', 'hex', 'ipa', 'maus', 'sampa', 'lexset'))

  #CELEX = celex
  vowels_celex = c('eI', 'aI', '3:', 'oI',
                   '@U', 'aU', 'I@', 'E@',
                   'U@', 'i:', 'I', 'E',
                   '&', 'u:', '@','V',
                   'A:', 'U', 'O:', 'Q')
  consonants_celex = c('p', 'b', 'm', 'f',
                       'v', 'T', 'D', 's',
                       'z', 't', 'd', 'r',
                       'l', 'n', 'S', 'Z',
                       'tS', 'dZ', 'j', 'w',
                       'k', 'g', 'N', 'h')

  #arpabet = arpabet
  vowels_arpabet = c('EY', 'AY', 'ER', 'OY',
                   'OW', 'AW', 'ɪə', 'eː',
                   'ʊə', 'IY', 'IH', 'EH',
                   'AE', 'UW', 'AX','AH',
                   'AA', 'UH', 'AO', 'AA')
  consonants_arpabet = c('P', 'B', 'M', 'F',
                       'V', 'TH', 'DH', 'S',
                       'Z', 'T', 'D', 'R',
                       'L', 'N', 'SH', 'ZH',
                       'CH', 'JH', 'Y', 'W',
                       'K', 'G', 'NG', 'HH')

  #DISC = disc
  vowels_disc = c('1', '2', '3', '4',
                  '5', '6', '7', '8',
                  '9', 'i', 'I', 'E',
                  '{', 'u', '@','V',
                  '#', 'U', '$', 'Q')
  consonants_disc = c('p', 'b', 'm', 'f',
                      'v', 'T', 'D', 's',
                      'z', 't', 'd', 'r',
                      'l', 'n', 'S', 'Z',
                      'J', '-', 'j', 'w',
                      'k', 'g', 'N', 'h')

  #HEX = hex
  vowels_hex = c('\u00E6\u026A', '\u0251e', '\u025C\u02D0', 'o\u026A',
                 '\u0259\u0289', '\u00E6\u0254', '\u026A\u0259', 'e\u02D0',
                 '\u028A\u0259', 'i\u02D0', '\u026A', 'e',
                 '\u00E6', '\u0289\u02D0', '\u0259','\u0250',
                 '\u0250\u02D0', '\u028A', 'o\u02D0', '\u0254')
  consonants_hex = c('p', 'b', 'm', 'f',
                     'v', '\u03B8', '\u00F0', 's',
                     'z', 't', 'd', '\u0279',
                     'l', 'n', '\u0283', '\u0292',
                     '\u02A7', '\u02A4', 'j', 'w',
                     'k', 'g', '\u014B', 'h')

  #IPA = ipa
  vowels_ipa = c('æɪ', 'ɑe', 'ɜː', 'oɪ',
                 'əʉ', 'æɔ', 'ɪə', 'eː',
                 'ʊə', 'iː', 'ɪ', 'e',
                 'æ', 'ʉː', 'ə','ɐ',
                 'ɐː', 'ʊ', 'oː', 'ɔ')

  consonants_ipa = c('p', 'b', 'm', 'f',
                     'v', 'θ', 'ð', 's',
                     'z', 't', 'd', 'ɹ',
                     'l', 'n', 'ʃ', 'ʒ',
                     'ʧ', 'ʤ', 'j', 'w',
                     'k', 'g', 'ŋ', 'h')

  #MAUS = maus
  vowels_maus = c('{I', 'Ae', '3:', 'oI',
                  '@}', '{O', 'I@', 'e:',
                  'U@', 'i:', 'I', 'e',
                  '{', '}:', '@','6',
                  '6:', 'U', 'o:', 'O')
  consonants_maus = c('p', 'b', 'm', 'f',
                      'v', 'T', 'D', 's',
                      'z', 't', 'd', 'r',
                      'l', 'n', 'S', 'Z',
                      'tS', 'dZ', 'j', 'w',
                      'k', 'g', 'N', 'h')

  #SAMPA = sampa
  vowels_sampa = c('eI', 'aI', '3:', 'oI',
                   '@U', 'aU', 'I@', 'E@',
                   'U@', 'i:', 'I', 'E',
                   '{', 'u:', '@','V',
                   'A:', 'U', 'O:', 'Q')
  consonants_sampa = c('p', 'b', 'm', 'f',
                       'v', 'T', 'D', 's',
                       'z', 't', 'd', 'r',
                       'l', 'n','S', 'Z',
                       'tS', 'dZ', 'j', 'w',
                       'k', 'g', 'N', 'h')

  #Lexical Set = lexset
  vowels_lexset = c('FACE', 'PRICE', 'NURSE', 'CHOICE',
                    'GOAT', 'MOUTH', 'NEAR', 'SQUARE',
                    'TOUR', 'FLEECE', 'KIT', 'DRESS',
                    'TRAP', 'GOOSE', 'SCHWA','STRUT',
                    'BATH', 'FOOT', 'THOUGHT', 'LOT')
  consonants_lexset = c('p', 'b', 'm', 'f',
                        'v', 'T', 'D', 's',
                        'z', 't', 'd', 'r',
                        'l', 'n','S', 'Z',
                        'tS', 'dZ', 'j', 'w',
                        'k', 'g', 'N', 'h')

  #phnological features
  consonantal = c(1:18, 21:24)
  sonorant = c(3, 12, 13, 14, 19, 20, 23)
  continuant = c(4, 5, 6, 7, 8, 9, 12, 13, 14, 15, 16, 19, 20, 23, 24)
  #place
  labial = c(1, 2, 3, 4, 5, 20)
  bilabial = c(1,2,3,20)
  labiodental = c(4,5)
  anterior = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 13, 14)
  coronal = c(6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19)
  distributed = c(6, 7, 15, 16, 17, 18)
  dorsal = c(20,21,22,23)
  #manner
  sibilant = c(8, 9, 15, 16, 17, 18)
  nasal = c(3,14,23)
  lateral = c(13)
  delayed_release = c(17, 18)
  #voicing
  voiced = c(2,3,5,7,9,11,12,13,14,16,18,19,20,22,23)
  #vowels
  high = c(10, 11, 14, 18, 19)
  low = c(13, 16, 17)
  front = c(10, 11, 12, 13)
  back = c(18, 19, 20)
  long = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 14, 17, 19)
  round = c(14, 18, 19, 20)
  tense = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 14, 17, 19)

  #phonetic features
  #voicing
  voiceless = c(1,4,6,8,10,15,17,21,24)
  #place
  dental = c(6,7)
  alveolar = c(8,9,10,11,12,13,14)
  palatoalveolar = c(15,16,17,18)
  palatal = c(19)
  velar = c(20,21,22,23)
  labiovelar = c(20)
  glottal= c(24)
  #manner
  stop = c(1,2,3,10,11,14,21,22,23)
  plosive = c(1,2,10,11,21,22)
  fricative = c(4,5,6,7,8,9,15,16,24)
  affricate = c(17,18)
  approximant = c(12,13,19,20)
  liquid = c(12,13)
  rhotic = c(12)
  #vowels
  mid = c(3, 12, 15, 20)
  central = c(3, 14, 15, 16, 17)
  monoph = c(3, 8, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20)
  diph = c(1, 2, 4, 5, 6, 7, 9)

  if(symbol_format == 'celex'){
    consonant_vector = consonants_celex
    vowel_vector = vowels_celex
  }else if(symbol_format == 'disc'){
    consonant_vector = consonants_disc
    vowel_vector = vowels_disc
  }else if(symbol_format == 'hex'){
    consonant_vector = consonants_hex
    vowel_vector = vowels_hex
  }else if(symbol_format == 'ipa'){
    consonant_vector = consonants_ipa
    vowel_vector = vowels_ipa
  }else if(symbol_format == 'maus'){
    consonant_vector = consonants_maus
    vowel_vector = vowels_maus
  }else if(symbol_format == 'sampa'){
    consonant_vector = consonants_sampa
    vowel_vector = vowels_sampa
  }else if(symbol_format == 'lexset'){
    consonant_vector = consonants_lexset
    vowel_vector = vowels_lexset
  }else if(symbol_format == 'arpabet'){
    consonant_vector = consonants_arpabet
    vowel_vector = vowels_arpabet
  }

  if(consonant_set != 'all' & is.null(consonant_set_manual)){
    if(length(consonant_set) == 1){
      consonant_vector = consonant_vector[eval(parse(text=consonant_set))]
    }else{
      if(class(sapply(consonant_set , function(x) eval(parse(text=x)))) == 'list'){
        tmp_consonant_vector = consonant_vector[Reduce(intersect,
                                                   as.list(sapply(consonant_set , function(x) eval(parse(text=x)))))]
      }else{
        tmp_consonant_vector = consonant_vector[Reduce(intersect,
                                                   as.list(as.data.frame(sapply(consonant_set , function(x) eval(parse(text=x))))))]

      }

      if(length(tmp_consonant_vector) == 0){
        consonant_vector = consonant_vector[as.vector(unlist(sapply(consonant_set , function(x) eval(parse(text=x)))))]
      }else{
        consonant_vector = tmp_consonant_vector
      }

    }
  }else if(consonant_set == 'all' & !is.null(consonant_set_manual)){
    consonant_vector = consonat_vector[which(consonat_vector %in% consonant_set_manual)]
  }

  if(vowel_set != 'all' & is.null(vowel_set_manual)){
    if(length(vowel_set) == 1){
      vowel_vector = vowel_vector[eval(parse(text=vowel_set))]
    }else{
      if(class(sapply(vowel_set , function(x) eval(parse(text=x)))) == 'list'){
        tmp_vowel_vector = vowel_vector[Reduce(intersect,
                                           as.list(sapply(vowel_set , function(x) eval(parse(text=x)))))]
      }else{
        tmp_vowel_vector = vowel_vector[Reduce(intersect,
                                           as.list(as.data.frame(sapply(vowel_set , function(x) eval(parse(text=x))))))]
      }

      if(length(tmp_vowel_vector) == 0){
        vowel_vector = vowel_vector[as.vector(unlist(sapply(vowel_set , function(x) eval(parse(text=x)))))]
      }else{
        vowel_vector = tmp_vowel_vector
      }
    }
  }else if(vowel_set == 'all' & !is.null(vowel_set_manual)){
    vowel_vector = vowel_vector[which(vowel_vector %in% vowel_set_manual)]
  }

  return(segment_list = list(consonants = consonant_vector, vowels = vowel_vector))

}
