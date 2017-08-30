#' classify_words %% ~~function to do ... ~~
#' 
#' Classifies words into Parts of Speech, syllable, and characater number
#' 
#' Output:
#' 
#' Exports the same dataset with new columns containing the word
#' classifications
#' 
#' @aliases classify_words
#' @param data_set dataset (data.frame) containing the words to be classified
#' @param column_header character vector containing the header of the column
#' containing the words to be classified
#' @param POS_tag boolean for showing the generalised POS tag, e.g.
#' 'CC','CD','DT','EX'
#' @param POS_tag_long boolean for showing the long POS tag, e.g. 'Coordinating
#' conjunction','Cardinal number','Determiner','Existential there'
#' @param POS_tag_simplified boolean for showing the simplified POS tag, e.g.
#' 'conjunction','number', 'determiner','there'
#' @param syllable_number boolean for showing the number of syllables per word
#' @param character_number boolean for showing the number of characters per
#' word
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
#' new_classififcaion =  classify_words(data_set = input_data,
#'                       column_header = 'word',
#'                       POS_tag = T,
#'                       POS_tag_long = T,
#'                       POS_tag_simplified = T,
#'                       syllable_number = T,
#'                       character_number = T)
#' 
classify_words <- function(data_set = NULL, column_header = NULL,
                          POS_tag = T, POS_tag_long = F, POS_tag_simplified = F,
                          syllable_number = T, character_number = F){

  if(POS_tag == T || POS_tag_long == T || POS_tag_simplified == T){
    long_POS = c('Coordinating conjunction','Cardinal number','Determiner','Existential there',
                 'Foreign word','Preposition or subordinating conjunction','Adjective','Adjective, comparative','Adjective, superlative',
                 'List item marker','Modal','Noun, singular or mass','Noun, plural','Proper noun, singular',
                 'Proper noun, plural','Predeterminer','Possessive ending','Personal pronoun',
                 'Possessive pronoun','Adverb','Adverb, comparative','Adverb, superlative',
                 'Particle','Symbol','to','Interjection','Verb, base form','Verb, past tense',
                 'Verb, gerund or present participle','Verb, past participle',
                 'Verb, non-3rd person singular present','Verb, 3rd person singular present',
                 'Wh-determiner','Wh-pronoun','Possessive wh-pronoun','Wh-adverb')

    long_POS_simplified = c('conjunction','number', 'determiner','there','word',
                            'preposition','adjective','adjective','adjective','listmarker',
                            'modal','noun','noun','noun','noun',
                            'predeterminer','possessive','name','possessivepronoun','adverb',
                            'adverb','adverb','particle','symbol', 'to',
                            'interjection','verb','verb','verb','verb',
                            'verb','verb', 'wh', 'wh','wh','wh')

    short_POS = c('CC','CD','DT','EX','FW','IN','JJ','JJR','JJS','LS','MD','NN','NNS','NNP',
                  'NNPS','PDT','POS','PRP','PRP$','RB','RBR','RBS','RP','SYM','TO','UH','VB',
                  'VBD','VBG','VBN','VBP','VBZ','WDT','WP','WP$','WRB')

    POS_mapping = setNames(as.character(long_POS), as.character(short_POS))

    POS_mapping_simplified = setNames(as.character(long_POS_simplified), as.character(short_POS))

    #extract the column to be tagged
    tmp_tags = data.frame(unique(data_set[[column_header]]),
                          as.character(pos(unique(data_set[[column_header]]), progress.bar = F)$POStagged$POStags))

    names(tmp_tags) = c('words', 'tags')

    mapping = setNames(tmp_tags$tags, tmp_tags$words)

    POS_tags = as.character(mapping[as.character(data_set[[column_header]])])

    #POS_tags = as.character(pos(data_set[[column_header]], progress.bar = F)$POStagged$POStags)

    if(POS_tag == T){
      data_set$POS_tag = POS_tags
    }

    if(POS_tag_long == T){
      data_set$POS_tag_long = POS_mapping[as.character(POS_tags)]
    }

    if(POS_tag_simplified == T){
      data_set$POS_tag_simplified = POS_mapping_simplified[as.character(POS_tags)]
    }
  }

  if(syllable_number == T){
    data_set$word_syllable_number = syllable_sum(data_set[[column_header]])
  }

  if(character_number == T){
    data_set$word_character_number = character_count(data_set[[column_header]])
  }

  return(data_set)
}
