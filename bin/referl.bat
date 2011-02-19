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
if %1 == -build goto build
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

:build
set CLIENT=build
set SERVER=no
set NAME=build@localhost
shift
set TARGET=%1
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
echo   -name NAME       Erlang node name
echo   -server          Start in server mode (no shell is started)
echo   -client          Start in client mode (no server is started)
echo   -build TARGET    Build TARGET (e.g. tool, doc, clean)
echo   -emacs           Start as an Emacs client
echo   -help            Print this help text

goto exit
REM echo   -wrangler PATH   Path to a Wrangler installation

:endarg

REM Set extra arguments
if %CLIENT%==server set ARGS=%ARGS% -noinput
if %CLIENT%==emacs set ARGS=%ARGS% -noshell -run referl_emacs
if %CLIENT%==build set ARGS=%ARGS% -noshell -run referl_gen_build start %TARGET%

:start

SET ERL_LIBS=%BASE:"=%\lib

if %SERVER%==yes goto server
goto client

:server
%ERL% ^
  -sname  %NAME% ^
  -config %BASE%\sys.config ^
  -boot   %BASE%\refactorerl ^
  +W "w" ^
  %ARGS%
goto exit

:client

if %CLIENT%==build %ERL% -make

%ERL% ^
  -sname  %NAME% ^
  %ARGS%

:exit
