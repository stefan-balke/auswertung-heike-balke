# master-heike

## Installation

um die notwendigen Pakete zu installieren:

```
source("install_requirements.r")
```

## Ausführen des Rmarkdown scripts

```r
library(rmarkdown)
rrender("Heike.Rmd", html_document())
render("Heike.Rmd", pdf_document())
```

