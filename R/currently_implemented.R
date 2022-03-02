#' Get information about implementation progress of
#' \code{NEPSscaling::plausible_values()}
#'
#' @export

currently_implemented <- function() {
cat(
"Plausible_values() is implemented for the following
starting cohorts, domains and waves;
all competence scores are available for
cross-sectional and longitudinal research
(if multiple waves are implemented).

== SC1 ==
Math: waves 5 and 7
Science: waves 6 and 8
Cognitive Development: wave 1

== SC2 ==
Grammar: waves 1 and 3
Math: waves 2, 3, 4, 6 and 9
Science: waves 1, 3, 5 and 9
Native Russian: wave 4
Native Turkish: wave 4
Reading: waves 6 and 9
ICT: wave 5
Orthography: wave 6
Vocabulary: waves 1, 3 and 5

== SC3 ==
Reading: waves 1, 3, 6 and 9
Math: waves 1, 3, 5 and 9
ICT: waves 2, 5 and 9
Science: waves 2, 5 and 8
Orthography (A - whole word, B - structural unit): waves 1, 3 and 5
Native Russian: waves 3 and 6
Native Turkish: waves 3 and 6
English as a foreign language: waves 7 and 9
Scientific Thinking: wave 9
Listening comprehension (German): wave 6

== SC4 ==
Reading: waves 2, 7 and 10
Math: waves 1, 7 and 10
ICT: waves 1 and 7
Science: waves 1 and 5
Native Russian: wave 2
Native Turkish: wave 2
English as a foreign language: waves 3 and 7
Scientific Thinking: wave 7

== SC5 ==
Reading: waves 1 and 12
Math: waves 1 and 12
ICT: wave 5
Science: wave 5
Business Administration: wave 7
English as a foreign language: wave 12

== SC6 ==
Reading: waves 3/5 and 9
Math: waves 3 and 9
ICT: wave 5
Science: wave 5\n"
)
}
