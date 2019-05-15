@echo off 
setlocal 
path=C:\Program Files\R\R-3.2.5\bin\i386;%path%


R CMD INSTALL --build --no-multiarch --force darwintools_066

endlocal