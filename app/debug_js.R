
suppressPackageStartupMessages({
  library(lubridate)
})


# lubridate:::wday.default()
cat("-------------------------------------------\n")

sessionInfo() %>% print()


#dommage, system n'est pas supporté 
# cat("locales")
# locales <- system("locale -a", intern=TRUE)
# unique(stringr::str_split_fixed(locales, "\\.", 2)[, 1])  %>% print()


print(Sys.getlocale("LC_TIME"))

cat("wday(today x5")
wday(today(), label=TRUE, abbr=FALSE, locale="French_France.1252") %>% as.character() %>% print()
wday(today(), label=TRUE, abbr=FALSE, locale="French_France") %>% as.character() %>% print()
wday(today(), label=TRUE, abbr=FALSE, locale="French") %>% as.character() %>% print()
wday(today(), label=TRUE, abbr=FALSE, locale="fr_FR.UTF-8") %>% as.character() %>% print()
wday(today(), label=TRUE, abbr=FALSE, locale="fr") %>% as.character() %>% print()
# 
# 
# lubridate:::.get_locale_regs(Sys.getlocale("LC_TIME"))$wday_names %>% print()

print(htmltools::HTML("juju <br> jijij"))
wday(today(), label=TRUE, abbr=FALSE) %>% print()
locale="French_France.1252"

cat("exists(locale")
exists(locale, envir=lubridate:::.locale_reg_cache, inherits = FALSE)  %>% print()
cat("Sys.setlocale")
Sys.setlocale("LC_TIME", locale=locale)
cat("Sys.getlocale")
print(Sys.getlocale("LC_TIME"))
cat("lubridate:::.locale_reg_cache")
ls(lubridate:::.locale_reg_cache) %>% print() 
cat("get(locale")
get(locale, envir=lubridate:::.locale_reg_cache) %>% length() %>% print()

cat("lubridate:::.get_locale_regs French_France.1252")
lubridate:::.get_locale_regs("French_France.1252")$wday_names %>% print()


cat("wday(today")
wday(today(), label=TRUE) %>% as.character() %>% print()


cat("-------------------------------------------\n")