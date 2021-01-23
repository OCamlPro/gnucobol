:: Batch for preparing distribution folder

@echo off
setlocal

:: Set distribution folder
set COB_DIST_PATH="%~dp0\dist\"

:: Set clean source directory
set COB_SOURCE_PATH="%~dp0..\"

:: Set directory with necessary header files
set COB_HEADER_PATH="%~dp0"

:: Set directory with generated release files
set COB_RELEASE_PATH="%~dp0"

:: Set directory with necessary library files
set COB_LIB_PATH="COB_RELEASE_PATH..\"

:: clean dist and copy all files
%~d0

if exist "%1COB_DIST_PATH%" (
   rmdir /S /Q "%COB_DIST_PATH%"
)

mkdir "%COB_DIST_PATH%"
cd "%COB_DIST_PATH%"



copy "%COB_SOURCE_PATH%AUTHORS"			.
copy "%COB_SOURCE_PATH%COPYING*"		.
copy "%COB_SOURCE_PATH%NEWS"			.
copy "%COB_SOURCE_PATH%README"			.
copy "%COB_SOURCE_PATH%THANKS"			.
copy "%COB_SOURCE_PATH%TODO"			.

copy "%COB_RELEASE_PATH%set_env_vs_dist.bat"	set_env_vs.bat
copy "%COB_RELEASE_PATH%set_env_vs_dist_x64.bat"	set_env_vs_x64.bat

call :copyrel
call :copyrel x64

mkdir config
copy "%COB_SOURCE_PATH%config\*.conf"		config\

mkdir copy
copy "%COB_SOURCE_PATH%copy\*.cpy"		copy\

mkdir doc
copy "%COB_SOURCE_PATH%doc\*.pdf"		doc\

mkdir include
mkdir include\libcob
copy "%COB_SOURCE_PATH%libcob.h"		include\
copy "%COB_SOURCE_PATH%libcob\common.h"		include\libcob\
copy "%COB_SOURCE_PATH%libcob\exception.def"	include\libcob\
copy "%COB_HEADER_PATH%gmp.h"			include\
rem erase /S include\_*

mkdir po
for %%f in ("%COB_SOURCE_PATH%po\*.gmo") do (
   copy "%%~ff"			po\%%~nf.mo
)
copy "%COB_SOURCE_PATH%po\*.po"			po\
copy "%COB_SOURCE_PATH%po\*.pot"		po\
erase /Q po\*@*

goto :end

:copyrel
if NOT "%1"=="x64" (
   set copyfrom="%COB_RELEASE_PATH%win32\release"
   set  copytobin=bin
   set  copytolib=lib
) else (
   set  copyfrom="%COB_RELEASE_PATH%x64\release"
   set  copytobin=bin_x64
   set  copytolib=lib_x64
)
mkdir %copytobin%
copy "%copyfrom%\cobc.exe"			%copytobin%\
copy "%copyfrom%\cobc.pdb"			%copytobin%\
copy "%copyfrom%\cobcrun.exe"			%copytobin%\
copy "%copyfrom%\cobcrun.pdb"			%copytobin%\
copy "%copyfrom%\libcob.dll"			%copytobin%\
copy "%copyfrom%\libcob.pdb"			%copytobin%\

copy "%copyfrom%\libvbisam.dll"			%copytobin%\
copy ""%copyfrom%\mpir.dll"			%copytobin%\
copy ""%copyfrom%\pdcurses.dll"			%copytobin%\

mkdir %copytolib%"
copy "%copyfrom%\libcob.lib"			%copytolib%\

goto :eof

:end
:: must be last as we compile with the dist itself
mkdir extras
copy "%COB_SOURCE_PATH%extras\*.cob"		extras\
copy "%COB_SOURCE_PATH%extras\README"		extras\

echo.
echo.
echo Using created GNU Cobol distribution (Win32) to compile extras
cd "%COB_DIST_PATH%bin"
call ..\set_env_vs.bat
cobc -m -Wall -std=mf ..\extras\CBL_OC_DUMP.cob -v

echo.
echo.
echo Using created GNU Cobol distribution (x64) to compile extras
cd "%COB_DIST_PATH%bin_x64"
call ..\set_env_vs_x64.bat
cobc -m -Wall -std=mf ..\extras\CBL_OC_DUMP.cob -v

cd ..

echo.
echo.

if exist "%ProgramFiles%\7-Zip\7z.exe" (
   erase "..\GNU Cobol.7z"
   "%ProgramFiles%\7-Zip\7z.exe" a -r -mx=9 "..\GNU Cobol.7z" *
) else if exist "%ProgramFiles(x86)%\7-Zip\7z.exe" (
   erase "..\GNU Cobol.7z"
   "%ProgramFiles(x86)%\7-Zip\7z.exe" a -r -mx=9 "..\GNU Cobol.7z" *
) else (
   echo 7-zip not found, "GNU Cobol.7z" not created
)

pause
endlocal