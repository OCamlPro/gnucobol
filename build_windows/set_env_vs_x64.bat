:: Batch for setting GNU Cobol Environment in Windows with MSC compiler
:: _x64 version

@echo off

:: Check for valid MSC Environment and let it do it's work.
:: If not found try Windows SDKs in standard installation folders
if exist "%VS120COMNTOOLS%vsvars64.bat" (
   call "%VS120COMNTOOLS%vsvars64.bat"
   goto :gc
)
if exist "%VS110COMNTOOLS%vsvars64.bat" (
   call "%VS110COMNTOOLS%vsvars64.bat"
   goto :gc
)
if exist "%VS100COMNTOOLS%vsvars64.bat" (
   call "%VS100COMNTOOLS%vsvars64.bat"
   goto :gc
)
if exist "%VS90COMNTOOLS%vsvars64.bat" (
   call "%VS90COMNTOOLS%vsvars64.bat"
   goto :gc
)
if exist "%VS80COMNTOOLS%vsvars64.bat" (
   call "%VS80COMNTOOLS%vsvars64.bat"
   goto :gc
)

echo Warning: Not possible to set 64 bit environment for Microsoft Visual Studio!
if exist "%ProgramFiles(x86)%\Microsoft SDKs\Windows\v8.1\Bin\SetEnv.Cmd" (
   call "%ProgramFiles(x86)%\Microsoft SDKs\Windows\v8.1\Bin\SetEnv.Cmd" /x64 /release
   goto :gcc
)
if exist "%ProgramFiles%\Microsoft SDKs\Windows\v8.1\Bin\SetEnv.Cmd" (
   call "%ProgramFiles%\Microsoft SDKs\Windows\v8.1\Bin\SetEnv.Cmd" /x64 /release
   goto :gcc
)
if exist "%ProgramFiles(x86)%\Microsoft SDKs\Windows\v8.0\Bin\SetEnv.Cmd" (
   call "%ProgramFiles(x86)%\Microsoft SDKs\Windows\v8.0\Bin\SetEnv.Cmd" /x64 /release
   goto :gcc
)
if exist "%ProgramFiles%\Microsoft SDKs\Windows\v8.0\Bin\SetEnv.Cmd" (
   call "%ProgramFiles%\Microsoft SDKs\Windows\v8.0\Bin\SetEnv.Cmd" /x64 /release
   goto :gcc
)
if exist "%ProgramFiles(x86)%\Microsoft SDKs\Windows\v7.1\Bin\SetEnv.Cmd" (
   call "%ProgramFiles(x86)%\Microsoft SDKs\Windows\v7.1\Bin\SetEnv.Cmd" /x64 /release
   goto :gcc
)
if exist "%ProgramFiles%\Microsoft SDKs\Windows\v7.1\Bin\SetEnv.Cmd" (
   call "%ProgramFiles%\Microsoft SDKs\Windows\v7.1\Bin\SetEnv.Cmd" /x64 /release
   goto :gcc
)
if exist "%ProgramFiles(x86)%\Microsoft SDKs\Windows\v7.0\Bin\SetEnv.Cmd" (
   call "%ProgramFiles(x86)%\Microsoft SDKs\Windows\v7.0\Bin\SetEnv.Cmd" /x64 /release
   goto :gcc
)
if exist "%ProgramFiles%\Microsoft SDKs\Windows\v7.0\Bin\SetEnv.Cmd" (
   call "%ProgramFiles%\Microsoft SDKs\Windows\v7.0\Bin\SetEnv.Cmd" /x64 /release
   goto :gcc
)
if exist "%ProgramFiles(x86)%\Microsoft SDKs\Windows\v6.1\Bin\SetEnv.Cmd" (
   call "%ProgramFiles(x86)%\Microsoft SDKs\Windows\v6.1\Bin\SetEnv.Cmd" /x64 /release
   goto :gcc
)
if exist "%ProgramFiles%\Microsoft SDKs\Windows\v6.1\Bin\SetEnv.Cmd" (
   call "%ProgramFiles%\Microsoft SDKs\Windows\v6.1\Bin\SetEnv.Cmd" /x64 /release
   goto :gcc
)

color 0C
echo Warning: Not possible to set environment for Microsoft Windows SDK!

:gcc
color 07

:gc
