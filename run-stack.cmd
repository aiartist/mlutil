@echo off
setlocal

if "%1" equ "" (
  echo Usage: run-stack ^<stack-command-arg-0^> .. ^<stack-command-arg-N-1^>
  exit /b 1
)

set COUNT=0
for /f %%d in ('dir /b /a:d') do (
  if exist %%d\stack.yaml (
    set /a COUNT=COUNT+1
    echo Run stack in %%d
    pushd %%d
    call :RunStack %*
    if errorlevel 1 (
      echo Failed to run stack command
      exit /b 1
    )
    popd
  )
)

if %COUNT% == 0 (
  call :RunStack %*
  if errorlevel 1 (
    echo Failed to run stack command
    exit /b 1
  )
)

goto :eof

:RunStack
echo dir: %cd%
call stack %*
if errorlevel 1 exit /b 1
goto :eof
