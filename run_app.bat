@echo off
TITLE DARK-ART - R Shiny App
echo ----------------------------------------
echo           SUMMONING DARK-ART.....       
echo ----------------------------------------


:: Use the system Rscript (ensures compatibility with 'rig' or manual updates)
SET "RSCRIPT=Rscript"

"%RSCRIPT%" -e "shiny::runApp('app.R', launch.browser=TRUE)"

IF %ERRORLEVEL% NEQ 0 (
    ECHO.
    ECHO An error occurred. Please ensure R is installed and required packages are available.
    PAUSE
)
