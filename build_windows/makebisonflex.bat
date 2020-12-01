:: Copyright (C) 2014-2017,2019 Free Software Foundation, Inc.
:: Written by Simon Sobisch
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

:: Batch for generating the Bison / Flex C sources
:: Pass argument filebasename to generate only a single file,
:: oterhwise eall files will be generated.

@echo off
setlocal

:: change to cobc directory
pushd "%~dp0..\cobc"

:: file prefix used for temporary files
set tmp_prf=mbfbat

:: limit on one file, must be specified as "ppparse", "pplex",
:: or "parser.y", "scanner.l"
set single_file=%*

:: check executables
call :exe_check "%BISON%" bison "GNU Bison" BISON
call :exe_check "%FLEX%" flex "flex" FLEX

call :print_separator

:: bison invocation
if not "%BISON%"=="" (
   call :bisoncall ppparse
   call :bisoncall parser
) else (
   echo ERROR: invocation of bison skipped!
)

:: flex invocation
if not "%FLEX%"=="" (
   call :flexcall pplex
   call :flexcall scanner
) else (
   echo ERROR: invocation of flex skipped!
)
goto :end


:exe_check
:: set local variables, remove quotes for command name and package
set "command_name=%1"
set "command_name=%command_name:"=%"
set "exe_name=%2"
set "exe_package=%3"
set "exe_package=%exe_package:"=%"
set "env_name=%4"

if not "%command_name%"=="" (
   set command_extern=1
) else (
   set command_extern=0
   set "command_name="
   rem echo.
   rem echo Searching for %exe_package%...
   where /q win_%exe_name%.exe
   if errorlevel 1 (
      where /q %exe_name%.exe
      if errorlevel 1 (
         where /q %exe_name%.bat
         if errorlevel 1 (
            echo.
            echo ERROR: No %exe_name% executable found.
            echo Make sure %exe_package% is in PATH or set %env_name% environment variable accordingly.
         ) else (
            set "command_name=%exe_name%.bat"
         )
      ) else (
         set "command_name=%exe_name%.exe"
      )
   ) else (
      set "command_name=win_%exe_name%.exe"
   )
)
if not "%command_name%"=="" (
   echo.
   echo Testing %exe_package%:
   "%command_name%" --version
   if errorlevel 1 (
      echo.
      if %command_extern%==0 (
         echo ERROR: "%command_name%" is not usable.
         echo Make sure a working %exe_package% is in PATH or set %env_name% environment variable accordingly.
      ) else (
         echo ERROR: %env_name% environment doesn't point to a working %exe_package% executable: "%command_name%"
         echo Unset this variable and make sure %exe_package% is in PATH or set %env_name% environment variable accordingly.
      )
      set "command_name="
   )
)
set "%env_name%=%command_name%"
goto :eof


:print_separator
echo.
echo -----------------------------------------------
echo.
goto :eof


:bisoncall
if not "%single_file%"=="" ^
if not "%single_file%"=="%1" ^
if not "%single_file%"=="%1.y" (
  goto :eof
)
echo generating %1.c, %1.h ...
call :store_old %1.c
call :store_old %1.h
%BISON% -o "%1.c"   "%1.y"
if %ERRORLEVEL% equ 0 (
  call :waiter
  call :compare_generated %1.c
  call :compare_generated %1.h
  if exist "%1.output" erase "%1.output" >NUL
) else (
  call :delete_generated %1.c 
  call :delete_generated %1.h
  echo.   %1.c, %1.h were not changed
)
call :print_separator
goto :eof

:flexcall
if not "%single_file%"=="" ^
if not "%single_file%"=="%1" ^
if not "%single_file%"=="%1.l" (
  goto :eof
)
echo generating %1.c ...
call :store_old %1.c
%FLEX%  -o "%1.c"   "%1.l"
if %ERRORLEVEL% equ 0 (
  call :waiter
  call :compare_generated %1.c
) else (
  call :delete_generated %1.c
  echo.   %1.c was not changed
)
call :print_separator
goto :eof


:compare_generated
rem echo %1 was generated
fc "%1" "%tmp_prf%_%1" 1>NUL 2>NUL
if %ERRORLEVEL% equ 0 (
  call :delete_generated %1
  echo.   %1 is unchanged
) else (
  call :use_generated %1
)
goto :eof

:waiter
ping -w 250 -n 2 localhost 1>NUL 2>NUL
goto :eof

:store_old
if exist "%1" (
  move  "%1" "%tmp_prf%_%1" >NUL
) else if exist "%tmp_prf%_%1" (
  erase      "%tmp_prf%_%1" >NUL
)
goto :eof

:use_generated
if exist "%tmp_prf%_%1" (
  erase  "%tmp_prf%_%1" >NUL
)
echo.   %1 is changed
goto :eof

:delete_generated
if exist "%tmp_prf%_%1" move "%tmp_prf%_%1" "%1" >NUL
goto :eof


:end

:: pause if not started directly
echo %cmdcmdline% | find /i "%~0" >nul
if %errorlevel% equ 0 (
   echo.
   pause
)
endlocal
