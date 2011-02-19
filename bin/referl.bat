@echo off

SET SERVER=yes
SET CLIENT=no
SET NAME=refactorerl@localhost
SET BASE=
SET ERL=
SET ARGS=

REM Interpret arguments

:argloop
if "%1" == "" goto endarg
if %1 == -erl goto erl
if %1 == -base goto base
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
echo Use:
echo "referl [-erl path] [-base dir] [-name node] [-server|-client|-emacs]"
shift
goto exit

:endarg

REM Set defaults
if x%ERL% == x SET ERL=erl
if x%BASE% == x set BASE="%CD%"

REM Set extra arguments
if %CLIENT%==server set ARGS=-noinput
if %CLIENT%==emacs set ARGS=-noshell -run referl_emacs

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