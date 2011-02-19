@echo off

REM Defaults (also used to clear up variable values)

SET SERVER=yes
SET CLIENT=no
SET NAME=refactorerl@localhost
SET BASE="%CD%"
SET ERL=erl
SET ARGS=


REM Interpret arguments

:argloop
if x%1 == x goto endarg
if %1 == -erl goto erl
if %1 == -base goto base
if %1 == -wrangler goto wrangler
if %1 == -name goto name
if %1 == -server goto server
if %1 == -emacs goto emacs
if %1 == -client goto client
if %1 == -help goto help
shift
goto argloop

:erl
shift
set ERL=%1
shift
goto argloop

:base
shift
SET BASE=%1
shift
goto argloop

:wrangler
shift
SET ARGS=%ARGS% -pa %1
shift
goto argloop

:name
shift
SET NAME=%1
shift
goto argloop

:server
set SERVER=yes
set CLIENT=server
shift
goto argloop

:emacs
set CLIENT=emacs
shift
goto argloop

:client
set SERVER=no
shift
goto argloop

:help
echo Usage: referl [Option]...
echo Starts RefactorErl, using the current working directory as the data directory.

echo Recognised options:
echo   -erl PATH        Path to the Erlang executable to use
echo   -base PATH       Path to the RefactorErl base directory
echo   -wrangler PATH   Path to a Wrangler installation
echo   -name NAME       Erlang node name
echo   -server          Start in server mode (no shell is started)
echo   -client          Start in client mode (no server is started)
echo   -emacs           Start as an Emacs client
echo   -help            Print this help text

goto exit

:endarg

REM Set extra arguments
if %CLIENT%==server set ARGS=%ARGS% -noinput
if %CLIENT%==emacs set ARGS=%ARGS% -noshell -run referl_emacs

:start
if %SERVER%==yes goto server
goto client

:server
%ERL% ^
  -sname  %NAME% ^
  -pa     %BASE%\lib\refactorerl\ebin ^
  -pa     %BASE%\lib\clustering\ebin ^
  -pa     %BASE%\lib\test\ebin ^
  -pa     %BASE%\build ^
  -boot   %BASE%\refactorerl ^
  -config %BASE%\sys.config ^
  +W "w" ^
  %ARGS%
goto exit

:client
%ERL% ^
  -sname  %NAME% ^
  -pa     %BASE%\lib\refactorerl\ebin ^
  %ARGS%

:exit
