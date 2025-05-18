@echo off

set ARGS=test
rem :: run only integration test "test"
set ARGS=%ARGS% --test test
rem :: following args are ignored by cargo and passed to the test binary
set ARGS=%ARGS% --
rem :: my print and eprint are visible
set ARGS=%ARGS% --nocapture
rem :: test name filter, otherwise my params are interpreted as filters 
set ARGS=%ARGS% ""
rem :: following args are ignored by the test binary and are handled by me
set ARGS=%ARGS% --
rem :: args passed on call
set ARGS=%ARGS% %*

echo cargo %ARGS%
cargo %ARGS%