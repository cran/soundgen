#' Formants in American vowels
#'
#' Typical relative frequencies of the first four formants measured in dF units
#' (average spacing between formants, or formant dispersion) above or below
#' schwa based on estimated VTL in American English, from Hillenbrand (1995),
#' who measured F1-F4 in ~1.5K recordings (139 speakers, 12 vowels from each).
#' Audio and formant measurements are freely available online:
#' https://homepages.wmich.edu/~hillenbr/voweldata.html. The dataset below is
#' the result of modeling Hillenbrand's data with brms: mvbind(F1rel, F2rel) ~
#' vowel + (vowel|speaker). It shows the most credible location of each vowel
#' centroid in the F1Rel-F2Rel space.
#'
#' A dataframe of 12 observations and 5 columns: "vowel" = vowel (American
#' English), "F1Rel" to "F4Rel" = formant frequencies in dF relative to their
#' neutral, equidistant positions in a perfectly cylindrical vocal tract. See
#' \code{\link{schwa}} - this is what schwa() returns as $ff_relative_dF
#'
#' @references Hillenbrand, J., Getty, L. A., Clark, M. J., & Wheeler, K.
#'   (1995). Acoustic characteristics of American English vowels. The Journal of
#'   the Acoustical society of America, 97(5), 3099-3111.
#' @examples
#' plot(hillenbrand$F1Rel, hillenbrand$F2Rel, type = 'n')
#' text(hillenbrand$F1Rel, hillenbrand$F2Rel, labels = hillenbrand$vowel)
"hillenbrand"


#' Training datasets for NLP detection
#' @description
#' The results of running \code{\link{naiveBayes_train}} on acoustically
#' analyzed 969 human nonverbal vocalizations (detectNLP_training_nonv) and on
#' 5000 synthetic sounds with or without NLP created with soundgen
#' (detectNLP_training_synth). Used by \code{\link{detectNLP}}.
"detectNLP_training_nonv"

#' @rdname detectNLP_training_nonv
"detectNLP_training_synth"


#' Speech example
#' @description
#' A few seconds of speech
"speechEx"

#' Impulse response
#' @description
#' A finger snap in a room as an approximation to an impulse response
"IR"
