@ECHO off

SET CONFIG=Release
SET BUILD_DIR=builds\Argentum

rm -r "%BUILD_DIR%"

rem dotnet clean --configuration %CONFIG%

ECHO.
ECHO BUILDING THE CODE...

dotnet build --configuration %CONFIG% ^
    --verbosity minimal --no-incremental || GOTO :error

ECHO.
ECHO RUNNING THE TESTS...

dotnet test --configuration %CONFIG% --verbosity minimal ^
	--filter Category!=acceptance || GOTO :error

ECHO RUNNING THE F# LINT...

dotnet tool install --tool-path tools\fsharplint ^
    --version 0.13.3 dotnet-fsharplint
tools\fsharplint\dotnet-fsharplint.exe lint ^
    --file-type solution Argentum.sln || GOTO :error

ECHO.
ECHO MAKING THE CONSOLE PACKAGE...

dotnet publish Argentum -c %CONFIG% --output "%cd%\%BUILD_DIR%" || GOTO :error

ECHO.
ECHO BUILD SUCCESSFUL

GOTO :EOF

:error
ECHO Failed with error #%errorlevel%.
EXIT /b %errorlevel%
