@echo off
TITLE DARK-ART - R Shiny App
echo ----------------------------------------
echo           SUMMONING DARK-ART.....       
echo ----------------------------------------


:: Try to use the Rscript found in the standard location
SET "RSCRIPT=C:\Program Files\R\R-4.4.3\bin\Rscript.exe"

IF NOT EXIST "%RSCRIPT%" (
    ECHO Rscript not found at default location.
    ECHO Trying global 'Rscript' command...
    SET "RSCRIPT=Rscript"
)

"%RSCRIPT%" -e "shiny::runApp('app.R', launch.browser=TRUE)"

IF %ERRORLEVEL% NEQ 0 (
    ECHO.
    ECHO An error occurred. Please ensure R is installed and required packages are available.
    PAUSE
)
