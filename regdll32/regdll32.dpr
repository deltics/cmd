
  program regdll32;

{$APPTYPE CONSOLE}

uses
// TODO: Improve/use Deltics.CommandLine to parse command line more effectively
//
//  Deltics.CommandLine in '..\rtl\Deltics.CommandLine.pas',
//  Deltics.Types in '..\rtl\Deltics.Types.pas',
  SysUtils,
  Windows;

type
  TCallType = (ftBooleanFunction,
               ftIntegerFunction,
               ftProcedure);

  TProcedure = procedure; stdcall;
  TIntegerFunction = function: Integer; stdcall;
  TBooleanFunction = function: Boolean; stdcall;


var
  dllfilename: String;
  dllfunc: String = 'register';
  callType: TCallType = ftProcedure;

  hDLL: THandle;
  entrypoint: Pointer;

begin
  try
    if (ParamCount = 0) then
    begin
      WriteLn('REGDLL32 - DLL registration/deregistration utility');
      WriteLn('©2011 Jolyon Smith (deltics)');
      WriteLn('');
      WriteLn('');
      WriteLn('  Calls a simple procedure or boolean function in a specified DLL, using the');
      WriteLn('   stdcall calling convention.');
      WriteLn('');
      WriteLn('');
      WriteLn('  USAGE:   regdll32 filename[.ext] [fnname] [/proc | /bool | /errorlevel]');
      WriteLn('');
      WriteLn('');
      WriteLn('     If the library filename extension [.ext] is omitted then .dll is ');
      WriteLn('      assumed.');
      WriteLn('');
      WriteLn('     Include a fnname to identify the case sensitive name of the ');
      WriteLn('      procedure or function to be called (default: "Register").');
      WriteLn('');
      WriteLn('');
      WriteLn('     Specify: /proc        to indicate that the registration procedure is a ');
      WriteLn('                            procedure with no return value (default).');
      WriteLn('');
      WriteLn('              /bool        to indicate that the registration procedure is a ');
      WriteLn('                            function returning TRUE/FALSE.');
      WriteLn('');
      WriteLn('              /errorlevel  to indicate that the registration procedure is a ');
      WriteLn('                           function returning an integer error level.');
      WriteLn('');
      EXIT;
    end;

    if (ParamCount > 0) then
    begin
      dllfilename := ParamStr(1);
      if ExtractFileExt(dllfilename) = '' then
        dllfilename := dllfilename + '.dll';
    end;

    if (ParamCount > 1) then
      dllfunc := ParamStr(2);

    if FindCmdLineSwitch('proc', ['/'], TRUE) then
      callType := ftProcedure
    else if FindCmdLineSwitch('bool', ['/'], TRUE) then
      callType := ftBooleanFunction
    else if FindCmdLineSwitch('errorlevel', ['/'], TRUE) then
      callType := ftIntegerFunction;


    hDLL := LoadLibrary(PChar(dllfilename));
    if (hDLL = 0) then
      RaiseLastOSError;

    try
      entrypoint := GetProcAddress(hDLL, PChar(dllfunc));
      if (entrypoint = NIL) then
        raise EArgumentException.CreateFmt('Function or procedure ''%s'' not found in %s (check case of function name in library)', [dllfunc, dllfilename]);

      case callType of
        ftProcedure       : begin
                              TProcedure(entrypoint);
                              ExitCode := 0;
                            end;

        ftBooleanFunction : begin
                              if TBooleanFunction(entrypoint) then
                                ExitCode := 0
                              else
                                ExitCode := 255;
                            end;

        ftIntegerFunction : begin
                              ExitCode := TIntegerFunction(entrypoint);
                            end;
      end;

    finally
      FreeLibrary(hDLL);
    end;

  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
