:: Copyright (C) 2014-2020,2022 Free Software Foundation, Inc.
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

:: General purpose cmd script for setting GnuCOBOL environment in Windows
:: with MSC compiler; version to setup specified by environment variables
:: %arch%, %arch_full%, %source_build%, %source_config%
:: %CONFIGURATION% may be used to setup the defaults for MSVC environment
:: works "interactive" if either %stay_open% or %CI% is defined

@echo off

:: restore old PATH if called multiple times to not expand it endlessly
if not "%COB_OLD_PATH%" == "" (
   set "PATH=%COB_OLD_PATH%"
   set "COB_OLD_PATH=%PATH%"
)
if "%CONFIGURATION%" == "" (
   set CONFIGURATION=Release
)


echo Setup Visual Studio (%arch%/%arch_full%)...
echo.

:: check if cl.exe with matching architecture is already in path
where cl.exe 1>nul 2>nul
if not "%errorlevel%" == "0" (
   goto :vsvars
)
cl.exe 2>&1 | findstr %arch% > nul
if "%errorlevel%" == "0" (
   echo cl.exe already in PATH
   echo no further initialization is done for the C compiler
   echo.
   goto :setup_gc
)

:vsvars

:: Check for valid MSC Environment and let it do it's work.
:: If not found try Windows SDKs in standard installation folders

:: Visual Studio 2017 and newer: no VS150COMNTOOLS globally or vsvars any more...
:: check if available, otherwise check on
set "found="
for %%v in (2022 2019 2017) do (
   if not "%found%" == ""  goto :eof
   call :vsvars_current %%v
)
if not "%found%" == "" (
   call "%found%" -arch=%arch_full%
   goto :setup_gc
)

:: Visual Studio 2015, 2013, 2012, 2010, 2008
for %%v in ("%VS140COMNTOOLS%" "%VS120COMNTOOLS%" "%VS110COMNTOOLS%" "%VS100COMNTOOLS%" "%VS90COMNTOOLS%") do (
   if not "%found%" == ""  goto :eof
   call :vsvars_old "%%v"
)
if not "%found%" == "" (
   call "%found%vsvars32.bat"
   goto :setup_gc
)

goto :sdk_setup

:vsvars_current
for %%v in (BuildTools Community Professional Enterprise) do (
  if not "%found%" == ""  goto :eof
  if exist "%ProgramFiles(x86)%\Microsoft Visual Studio\%1\%%v\Common7\Tools\VsDevCmd.bat" (
     set "found=%ProgramFiles(x86)%\Microsoft Visual Studio\%1\%%v\Common7\Tools\VsDevCmd.bat"
  ) else if exist "%ProgramFiles%\Microsoft Visual Studio\%1\%%v\Common7\Tools\VsDevCmd.bat" (
     set "found=%ProgramFiles%\Microsoft Visual Studio\%1\%%v\Common7\Tools\VsDevCmd.bat"
  ) 
)
goto :eof

:vsvars_old
set param=%1
set param=%param:"=%
if exist "%param%vsvars32.bat" (
   set "found=%1"
   set param=""
   goto :eof
)
if exist "%param%VCVarsQueryRegistry.bat" (
   call "%param%VCVarsQueryRegistry.bat"
)
if %errorlevel% equ 0 if exist "%VCINSTALLDIR%vcvarsall.bat" (
   set param=""
   call "%VCINSTALLDIR%vcvarsall.bat" %arch%
   goto :setup_gc
)
set param=""
goto :eof

:sdk_entry
if exist "%ProgramFiles(x86)%\Microsoft SDKs\Windows\%1\Bin\SetEnv.Cmd" (
   set "found=%ProgramFiles(x86)%\Microsoft SDKs\Windows\%1\Bin\SetEnv.Cmd"
) else if exist "%ProgramFiles%\Microsoft SDKs\Windows\%1\Bin\SetEnv.Cmd" (
   set "found=%ProgramFiles%\Microsoft SDKs\Windows\%1\Bin\SetEnv.Cmd"
)
goto :eof

:prefix_path
set param=%1
set param=%param:"=%
set "PATH=%param%;%PATH%"
set param=""
goto :eof

:: WinSDK variants
:sdk_setup
echo Warning: Not possible to set %arch_full% environment for Microsoft Visual Studio!

:: Windows SDK 10 (Windows 10 / VS 2015 compiler) - untested
:: Windows SDK 8.1 (Windows 8.1 and .NET 4.5.1 / VS 2013 compiler)
:: Windows SDK 8.0 (Windows 8 and .NET 4.5 / VS 2012 compiler)
:: Windows SDK 7.1 (Windows 7 and .NET 4 / VS 2010 SP1 compiler)
:: Windows SDK 7.0 (Windows 7 and .NET 3.5 SP1 / VS 2008 compiler)
:: Windows SDK 6.1 (Windows 2008 Server and .NET 3.5 / VS 2008 compiler)
for %%v in (v10 v8.1 v8.0 v7.1 v7.0 v6.1) do (
   if not "%found%" == ""  goto :eof
   call :sdk_entry %%v
)
if not "%found%" == "" (
   call "%found%" /%arch% /%CONFIGURATION%
   goto :gcc
)

color 0C
echo Warning: Not possible to set %arch_full% environment for Microsoft Windows SDK!

:gcc
color 07

:setup_gc

:: check if cl.exe is already in path
echo cl.exe now in PATH:
where cl.exe 2>nul
if "%errorlevel%" == "0" (
   cl.exe 1>nul
) else (
   echo ERROR: cl.exe not found!
)

echo.
:: Now the stuff for GnuCOBOL
echo Setting environment for GnuCOBOL.

:: Get the main dir from the batch's position
if not [%source_config%] == [] (
  set "COB_MAIN_DIR=%~dp0..\"
) else (
  set "COB_MAIN_DIR=%~dp0"
)

:: Set the necessary folders for cobc
set "COB_CONFIG_DIR=%COB_MAIN_DIR%config"
set "COB_COPY_DIR=%COB_MAIN_DIR%copy"

set "LOCALEDIR=%COB_MAIN_DIR%locale"

set "COB_DEV_DIR=%~dp0%source_build%"

:: Set the necessary options for MSC compiler
if not [%source_config%] == [] (
  set "COB_CFLAGS=/I "%COB_MAIN_DIR%." -I "%~dp0.""
  set "COB_LIB_PATHS=/LIBPATH:"%COB_DEV_DIR%""
) else (
  set "COB_CFLAGS=/I "%COB_MAIN_DIR%include""
  set "COB_LIB_PATHS=/LIBPATH:"%COB_MAIN_DIR%lib_%arch%""
)

:: save current PATH and add the bin path of GnuCOBOL to PATH for further references
if "%COB_OLD_PATH%" == "" (
  set "COB_OLD_PATH=%PATH%"
)
if not [%source_config%] == [] (
  call :prefix_path "%COB_DEV_DIR%\%source_config%"
) else (
  call :prefix_path "%COB_MAIN_DIR%bin_%arch%"
)

set "COB_DEV_DIR="

:: check if we find cobcrun, otherwise abort with message
where cobcrun.exe 1>nul 2>nul
if not "%errorlevel%" == "0" (
   echo cobcrun not found
   if not [%stay_open%] == [] (
      pause
   )
   goto :eof
)
:: some info to output
setlocal
(cobcrun -v --version | findstr /b /c:"GnuCOBOL")>"%TEMP%\gcvars.tmp"
set /p info1=<"%TEMP%\gcvars.tmp"
(cobcrun -v --version | findstr GMP)>"%TEMP%\gcvars.tmp"
set /p info2a=<"%TEMP%\gcvars.tmp"
(cobcrun -v --version | findstr MPIR)>"%TEMP%\gcvars.tmp"
set /p info2b=<"%TEMP%\gcvars.tmp"
del "%TEMP%\gcvars.tmp"
endlocal & set "COB_INFO1=%info1%"& set "COB_INFO2=%info2a%%info2b%"

:: start executable as requested
:call_if_needed
if not [%1] == [] (
  echo environment is prepared:
  call :cobcver
  echo now starting the requested %1
  call %*
  goto :eof
)

:: new cmd to stay open if not started directly from cmd.exe window 
if not [%stay_open%] == [] (
  cmd /k "cobc --version && echo. && echo %COB_INFO1% && echo %COB_INFO2%"
  goto :eof
)

:: Compiler and package version output
:cobcver
cobc --version && echo. && echo %COB_INFO1% && echo %COB_INFO2%
