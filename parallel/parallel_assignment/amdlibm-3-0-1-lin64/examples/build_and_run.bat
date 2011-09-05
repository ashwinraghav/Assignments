@echo off

set ST_PATH=%PATH%
set PATH=%PATH%;..\lib\dynamic

cl /nologo /EHa /I..\include /Fesample main.c use_cbrt.c /link /LIBPATH:..\lib\dynamic amdlibm.lib
sample.exe
del /F /Q *.obj *.exe
echo.
echo --------------------------------------------
echo.

cl /nologo /EHa /I..\include /Fesample main.cpp use_cbrt.cpp /link /LIBPATH:..\lib\dynamic amdlibm.lib
sample.exe
del /F /Q *.obj *.exe
echo.
echo --------------------------------------------
echo.

cl /nologo /EHa /I..\include /Fesample replace_compiler_sin.c /link /LIBPATH:..\lib\dynamic amdlibm.lib
sample.exe
del /F /Q *.obj *.exe
echo.
echo --------------------------------------------
echo.

cl /nologo /EHa /I..\include /Fesample replace_compiler_math_fns.c /link /LIBPATH:..\lib\dynamic amdlibm.lib
sample.exe
del /F /Q *.obj *.exe
echo.
echo --------------------------------------------
echo.

set PATH=%ST_PATH%

