REM Chemin vers R (ajuste en fonction de ta machine)
set R_HOME=C:\Users\edgar.matter\AppData\Local\Programs\R\R-4.5.1

"%R_HOME%\bin\Rscript.exe" -e "renv::restore(); shiny::runApp('C:/Users/edgar.matter/Desktop/HydrobioHdF_TEST')"

pause