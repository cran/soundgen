# .onLoad = function(libname, pkgname) {
# }

.onAttach = function(libname, pkgname) {
  mes = paste0(
    "Soundgen ", packageVersion('soundgen'),
    "\nTips & demos on project's homepage: http://cogsci.se/soundgen.html",
    "\nPlease cite as: ",
    "Anikin, A. (2019). Soundgen: an open-source tool for synthesizing ",
    "nonverbal vocalizations. Behavior Research Methods, 51(2), 778-792."
  )
  packageStartupMessage(mes)
}
