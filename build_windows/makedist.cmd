:: Copyright (C) 2014-2020 Free Software Foundation, Inc.
:: Written by Simon Sobisch, Edward Hart
::
:: This file is part of GnuCOBOL.
::
:: The GnuCOBOL compiler is free software: you can redistribute it
:: and/or modify it under the terms of the GNU General Public License
:: as published by the Free Software Foundation, either version 3 of the
:: License, or (at your option) any later version.
::
:: GnuCOBOL is distributed in the hope that it will be useful,
:: but WITHOUT ANY WARRANTY; without even the implied warranty of
:: MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
:: GNU General Public License for more details.
::
:: You should have received a copy of the GNU General Public License
:: along with GnuCOBOL.  If not, see <https://www.gnu.org/licenses/>.

:: Batch for preparing windows binary distribution folder
:: By default, binaries use Release executable. To distribute a debug
:: distributable (y tho), provide DEBUG as an argument.

@echo off

setlocal enabledelayedexpansion
set cb_errorlevel=0

:: Get the main dir from the batch's position
set "COB_MAIN_DIR=%~dp0"

:: Set (temporary) distribution folder
set "cob_dist_path=%COB_MAIN_DIR%distnew\"

:: Set clean source directory
set "cob_source_path=%COB_MAIN_DIR%..\"

:: Set directory with necessary header files
set "cob_header_path=%COB_MAIN_DIR%"

:: Set directory with generated release files
set "cob_build_path=%COB_MAIN_DIR%"

if exist "%cob_build_path%config.h" (
   pushd "%cob_build_path%"
   call :setver
   popd
) else if exist "%cob_source_path%config.h" (
   echo WARNING: config.h not found as "%cob_build_path%config.h" but as "%cob_source_path%config.h"!
   pushd "%cob_source_path%"
   call :setver
   popd
) else (
   set PACKAGE_DIRECTORY=GnuCOBOL
   set PACKAGE_TARNAME=gnucobol
)
echo Creating binary distribution for %PACKAGE_DIRECTORY%
set DIST_PACKAGE=%PACKAGE_DIRECTORY%_vs_bin

:: check for existing binaries
if /i "%1%"=="DEBUG" (
   set config=Debug
) else (
   set config=Release
)

rem TODO: read this from config.h
set "ltversion=-5"

if exist "%cob_build_path%Win32\%config%\cobc.exe" (
   set have_32=1
   echo 32-bit %config% binaries: found
) else (
   set have_32=0
   echo 32-bit %config% binaries: not found
)

if exist "%cob_build_path%x64\%config%\cobc.exe" (
   set have_64=1
   echo 64-bit %config% binaries: found
) else (
   set have_64=0
   echo 64-bit %config% binaries: not found
)

if "%have_32%%have_64%"=="00" (
   echo No %config% binaries available.
   goto :abort
)

:: clean dist
if exist "%cob_dist_path%" (
   rmdir /S /Q "%cob_dist_path%" 1>nul
)
mkdir "%cob_dist_path%"
pushd "%cob_dist_path%"

echo.

echo Copying docs...
set "txt_doc_list=AUTHORS COPYING COPYING.LESSER COPYING.DOC NEWS README THANKS DEPENDENCIES HACKING TODO"
for %%f in (%txt_doc_list%) do (
    copy  %cob_source_path%%%f .\%%f.TXT 1>nul
)
mkdir doc
if exist "%cob_source_path%doc\*.pdf" (
   copy "%cob_source_path%doc\*.pdf"		doc\	1>nul
)
if exist "%cob_source_path%doc\*.html" (
   copy "%cob_source_path%doc\*.html"		doc\	1>nul
)


echo Copying configuration files...
mkdir config
set "config_ext_list=conf conf-inc words cfg"
for %%f in (%config_ext_list%) do (
    copy "%cob_source_path%config\*.%%f"	config\	1>nul
)

echo Copying copybooks...
mkdir copy
copy "%cob_source_path%copy\*.cpy"		copy\	1>nul

echo Copying header files...
mkdir include
mkdir include\libcob
copy "%cob_source_path%libcob.h"		include\	1>nul
copy "%cob_source_path%libcob\common.h"		include\libcob\	1>nul
copy "%cob_source_path%libcob\exception*.def"	include\libcob\	1>nul

echo Copying translations...
mkdir po
copy "%cob_source_path%po\*.po"			po\	1>nul
copy "%cob_source_path%po\*.pot"		po\	1>nul
if exist "po\*@*" (
   erase /Q po\*@* 1>nul
)
dir "%cob_source_path%po\*.gmo" /b /a-d >nul 2>&1
if "%errorlevel%"=="1" (
   dir "%cob_source_path%po\*.mo" /b /a-d >nul 2>&1
)
if "%errorlevel%"=="0" (
   echo Copying message catalogs...
   mkdir locale
   for %%f in ("%cob_source_path%po\*.gmo") do (
     call :copy_locale %%f
   )
   for %%f in ("%cob_source_path%po\*.mo")  do (
     call :copy_locale %%f
   )
) else (
   echo no message catalogs found, consider compiling them
)

echo Copying extras...
mkdir extras
copy "%cob_source_path%extras\*.cob"		extras\			1>nul
copy "%cob_source_path%extras\README"		extras\README.txt	1>nul

echo.

if "%have_32%"=="1" (
   call :copy_exes_and_libs "Win32"
   if !cb_errorlevel! neq 0 (
      goto :abort
   )
)
if "%have_64%"=="1" (
   if "%have_32%"=="1" (
       echo.
   )
   call :copy_exes_and_libs "x64"
   if !cb_errorlevel! neq 0 (
      goto :abort
   )
)

:: must be last as we compile with the dist itself
echo Compiling extras...
echo.

if "%have_32%"=="1" (
   call :compile_extras "Win32" x86-windows
   if !cb_errorlevel! neq 0 (
      goto :abort
   )
)
if "%have_64%"=="1" (
   if "%have_32%"=="1" (
       echo.
   )
   call :compile_extras "x64"   x64-windows
   if !cb_errorlevel! neq 0 (
      goto :abort
   )
)

echo.

echo Compressing dist package...
if exist "%ProgramFiles%\7-Zip\7z.exe" (
   erase "..\%DIST_PACKAGE%.7z" 1>nul 2>nul
   "%ProgramFiles%\7-Zip\7z.exe" a -r -mx=9 "..\%DIST_PACKAGE%.7z" *
) else if exist "%ProgramFiles(x86)%\7-Zip\7z.exe" (
   erase "..\%DIST_PACKAGE%.7z" 1>nul
   "%ProgramFiles(x86)%\7-Zip\7z.exe" a -r -mx=9 "..\%DIST_PACKAGE%.7z" *
) else (
   echo.
   echo 7-zip not found, "%DIST_PACKAGE%.7z" not created
   rem cd ..
   rem move dist PACKAGE 1>nul
   rem echo.
   echo %cob_build_path%%DIST_PACKAGE% ready for distribution; manual compression needed.
   goto :end
)
echo.
echo %cob_build_path%%DIST_PACKAGE%.7z ready for distribution.

goto :end

:abort
echo Abort^^!

:end
popd

call :pause_if_interactive
exit /b %cb_errorlevel%


:: pause if not started directly
:pause_if_interactive
echo %cmdcmdline% | find /i "%~0" >nul
if %errorlevel% equ 0 (
   echo.
   pause
)
goto :eof


:copy_exes_and_libs
call :set_platform_and_ext %1%

copy "%cob_build_path%set_env_vs_dist%platform_ext%.bat"	set_env_vs%platform_ext%.bat	1>nul

set copy_to_bin=bin%platform_ext%
set copy_to_lib=lib%platform_ext%

set "copy_from=%cob_build_path%%platform%\%config%"

echo Copying binaries for %platform%...
mkdir %copy_to_bin%
set "exe_lib_list=cobc.exe cobc.pdb cobcrun.exe cobcrun.pdb libcob%ltversion%.dll libcob%ltversion%.pdb"
for %%f in (%exe_lib_list%) do (
    copy "%copy_from%\%%f"	%copy_to_bin%\	1>nul
)

if "%VCPKG_EXPORT_DIR%"=="" (
   call :copy_deps
) else (
   echo no dependencies copied as VCPKG_EXPORT_DIR is set
)

mkdir %copy_to_lib%
copy "%copy_from%\libcob%ltversion%.lib"			%copy_to_lib%\	1>nul

goto :eof


:copy_deps
:: Copy math library.
if exist "%copy_from%\mpir.dll" (
   copy "%copy_from%\mpir.dll"			%copy_to_bin%\	1>nul
) else if exist "%copy_from%\libgmp.dll" (
   copy "%copy_from%\libgmp.dll"			%copy_to_bin%\	1>nul
) else if exist "%copy_from%\gmp.dll" (
   copy "%copy_from%\gmp.dll"			%copy_to_bin%\	1>nul
) else (
   echo No math library found.
   set cb_errorlevel=1
   goto :eof
)

:: Copy the ISAM-handler library, guessing the name if necessary.
:: Note: Not handling C-ISAM as there's no known Windows version of this library.
if exist "%copy_from%\libvbisam.dll" (
   copy "%copy_from%\libvbisam.dll"		%copy_to_bin%\	1>nul
) else if exist "%cob_header_path%db.h" (
   for /f "tokens=3" %%a in ('find "DB_VERSION_MAJOR" "%cob_header_path%db.h"') do (
      set major=%%a
   )
   for /f "tokens=3" %%a in ('find "DB_VERSION_MINOR" "%cob_header_path%db.h"') do (
      set minor=%%a
   )
   echo Guessing from db.h... libdb!major!!minor!
   if exist "%copy_from%\libdb!major!!minor!.dll" (
      copy "%copy_from%\libdb!major!!minor!.dll"	%copy_to_bin%\	1>nul
   ) else if exist "%copy_from%\libdb!major!!minor!d.dll" (
      copy "%copy_from%\libdb!major!!minor!d.dll"	%copy_to_bin%\	1>nul
   ) else (
      echo No ISAM handler found.
   )
) else if exist "%cob_header_path%disam.h" (
   for /f "tokens=3,4 delims=. " %%a in ('find "Version" "%cob_header_path%disam.h"') do (
      set major=%%a
      set minor=%%b
   )
   echo Guessing from disam.h... libdisam!major!!minor!
   if exist "%copy_from%\libdisam!major!!minor!.dll" (
      copy "%copy_from%\libdisam!major!!minor!.dll"	%copy_to_bin%\	1>nul
   ) else if "%PLATFORM%"=="Win32" (
      if exist "%copy_from%\libdisam!major!!minor!_win32.dll" (
         copy "%copy_from%\libdisam!major!!minor!_win32.dll"	%copy_to_bin%\	1>nul
      ) else (
         echo No ISAM handler found.
      )
   ) else if exist "%copy_from%\libdisam!major!!minor!_win64.dll" (
         copy "%copy_from%\libdisam!major!!minor!_win64.dll"	%copy_to_bin%\	1>nul
   ) else (
      echo No ISAM handler found.
   )
) else (
   echo No ISAM handler found.
)

:: Copy the intl library.
call :copy_lib_if_exists "libintl.dll" %copy_to_bin% "intl"

:: Copy the cJSON library.
call :copy_lib_if_exists "cjson.dll" %copy_to_bin% "cJSON"

:: Copy the curses library.
call :copy_lib_if_exists "pdcurses.dll" %copy_to_bin% "curses"

:: Copy the XML libary (and its dependencies)
call :copy_lib_if_exists "libxml2.dll" %copy_to_bin% "XML"
call :copy_lib_if_exists "zlib1.dll" %copy_to_bin% "zlib"
call :copy_lib_if_exists "libcharset.dll" %copy_to_bin% "charset"
call :copy_lib_if_exists "lzma.dll" %copy_to_bin% "lzma"

:: Copy the iconv library.
call :copy_lib_if_exists "libiconv.dll" %copy_to_bin% "iconv"

goto :eof


:compile_extras
call :set_platform_and_ext %1%
echo Using created GnuCOBOL distribution -%platform%- to compile extras...
pushd "%cob_dist_path%bin%platform_ext%"
call ..\set_env_vs%platform_ext%.bat
if not [%VCPKG_EXPORT_DIR%]==[] (
   echo using vcpgk binaries...
   set "PATH=%VCPKG_EXPORT_DIR%\installed\%2\bin;%PATH%"
   set "extra_include=%VCPKG_EXPORT_DIR%\installed\%2\include"
) else (
   set "extra_include=."
 )
cobc -m -Wall -O2 -I "%extra_include%" ..\extras\CBL_OC_DUMP.cob
if %errorlevel% neq 0 (
   echo.
   echo cobc had unexpected return value %errorlevel%, running verbose again...
   call :pause_if_interactive
   where cobc.exe
   cobc -vv -m -Wall -O2 -I "%extra_include%" ..\extras\CBL_OC_DUMP.cob
   set cb_errorlevel=!errorlevel!
)
popd
goto :eof


:copy_locale
mkdir "locale\%~n1"
mkdir "locale\%~n1\LC_MESSAGES"
copy "%~f1" "locale\%~n1\LC_MESSAGES\%PACKAGE_TARNAME%.mo"	1>nul
goto :eof


:setver
for /f "tokens=3 delims= " %%a in ('find "PACKAGE_NAME"    "config.h"') do (
   set PVTEMP=%%a
)
set PACKAGE_NAME=!PVTEMP:"=!
for /f "tokens=3 delims= " %%a in ('find "PACKAGE_VERSION" "config.h"') do (
   set PVTEMP=%%a
)
set PACKAGE_VERSION=!PVTEMP:"=!
for /f "tokens=3 delims= " %%a in ('find "PACKAGE_TARNAME" "config.h"') do (
   set PVTEMP=%%a
)
set PACKAGE_TARNAME=!PVTEMP:"=!
set PACKAGE_DIRECTORY=!PACKAGE_NAME!_!PACKAGE_VERSION!
goto :eof


:set_platform_and_ext
if %1%=="Win32" (
   set platform=Win32
   set platform_ext=
) else (
   set platform=x64
   set platform_ext=_x64
)
goto :eof

:copy_lib_if_exists
if exist "%copy_from%\%~1%" (
   copy "%copy_from%\%~1"	"%2%\"	1>nul
) else (
   echo %~3 library not found.
)
echo off
goto :eof
