
.pkgenv <- new.env(parent=emptyenv())

.repo <- "https://eddelbuettel.github.io/tiledb-soma-repo"

.getPackagesData <- function(package="tiledbsoma", repo) {
    if (is.null(.pkgenv$packages)) {
        if (missing(repo)) repo <- .repo
        url <- file.path(repo, "packages.csv")
        .pkgenv$packages <- read.csv(url)
    }
    if (is.null(.pkgenv$preferred)) {
        descfile <- system.file("DESCRIPTION", package="lorede")
        flds <- c("Config/tiledbsoma/preferred", "Config/tiledbsoma/minimum")
        prefs <- read.dcf(descfile, fields=flds)
        .pkgenv$preferred <- prefs[1,1]
        .pkgenv$minimum <- prefs[1,2]
    }
    if (is.null(.pkgenv$installed)) {
        IP <- as.data.frame(installed.packages())
        .pkgenv$installed <- subset(IP, Package=="tiledbsoma", Version)[1,1]
    }
}

.checkSoma <- function() {
    cat("Installed: ", .pkgenv$installed, "\n",
        "Preferred: ", .pkgenv$preferred, " ",
        ifelse(.pkgenv$installed >= .pkgenv$installed, "[good]", "[bad]"), "\n",
        "Minimum:   ", .pkgenv$minimum, " ",
        ifelse(.pkgenv$installed >= .pkgenv$minimum, "[good]", "[bad]"), "\n",
        "\n", sep="")
}

.onLoad <- function(libname, pkgname) {
    .getPackagesData()
}

.onAttach <- function(libname, pkgname) {
    .getPackagesData()
    nm <- packageName()
    packageStartupMessage("Welcome to '", nm, "' version ", packageVersion(nm), ".")
    if (interactive())
        checkSoma()
}

installVersion <- function(version, type = c("bin", "src")) {
    if (missing(version))
        stop("Argument 'version' is mandatory.", call. = FALSE)
    type <- match.arg(type)

    si <- Sys.info()
    os <- "unset"
    if (si[["sysname"]] == "Linux") {
        os <- "linux"
    } else if (si[["sysname"]] == "Darwin" && si[["machine"]] == "arm64") {
        os <- "big-sur-arm64"
    } else if (type != "src") {
        stop(r"(Only 'type="src"' supported on )", si[["sysname"]], "/", si[["machine"]], call. = FALSE)
    }

    P <- .pkgenv$packages
    colnames(P) <- toupper(colnames(P)) 	# make colnames ALLUPPERCASE

    if (os == "unset" || type == "src") {
        pkg <- subset(P, VERSION==version & TYPE==type, select=c(FILES, BINFILE))
    } else {
        pkg <- subset(P, VERSION==version & TYPE==type & OS==os, select=c(FILES, BINFILE))
    }
    ##print(pkg)

    if (nrow(pkg) < 1) stop("No matching package found.", call. = FALSE)

    setwd(tempdir())
    pkgfile <- file.path(.repo, pkg[1,1])
    instfile <- pkg[1,2]
    download.file(pkgfile, instfile)
    cat("Downloaded into", instfile, "\n")
    install.packages(instfile, repos=NULL)
    cat("Installed ", instfile, "\n")
}
