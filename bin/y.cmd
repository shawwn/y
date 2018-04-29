@echo off
setlocal
set home=%~dp0

@echo on
"%~dp0..\..\emacs\bin\emacs.exe" -Q --batch -L %~dp0 -l y.el %*

