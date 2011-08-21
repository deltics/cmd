unit EnvVars;

interface

uses
  // Delphi
  Classes;

function GetEnvVarValue(const VarName: string): string;
  {Returns the value for the given environment variable or '' if the variable
  does not exist}
function SetEnvVarValue(const VarName, VarValue: string): Integer;
  {Sets the given environment variable to the given value. Creates the
  environment variable if it doesn't already exist. Setting a variable to the
  empty string deletes it. Returns 0 on success or a Windows error code on
  error}
function DeleteEnvVar(const VarName: string): Integer;
  {Deletes the given environment variable. Returns 0 on success of a Windows
  error code on error}
function CreateEnvBlock(const NewEnv: TStrings; const IncludeCurrent: Boolean;
  const Buffer: Pointer; const BufSize: Integer): Integer;
  {Creates a new environment block containing the given strings. If
  IncludeCurrent is true then the variable defined in the current process's
  environment block are included. The new block is stored in the memory pointed
  to by Buffer, which is at least BufSize bytes. The size of the block is
  returned. If the provided buffer is nil or is too small then no buffer is
  created. The return value gives the required buffer size}
function ExpandEnvVars(const Str: string): string;
  {Replaces any environment variables in the given string with their values and
  returns the string. Environment variables should be delimited by % characters
  thus: %ENVVAR%}
function GetAllEnvVars(const Vars: TStrings): Integer;
  {Copies all the environment variables available to the current process in the
  given string list, with each item in the string list representing one
  environment variable in the form NAME=VALUE. Returns the size of the
  environment block}


implementation

uses
  // Delphi
  Windows, SysUtils;

function GetEnvVarValue(const VarName: string): string;
  {Returns the value for the given environment variable or '' if the variable
  does not exist}
var
  BufSize: Integer;  // buffer size required for value (including terminal #0)
begin
  // Get required buffer size (includes space for terminal #0)
  BufSize := GetEnvironmentVariable(PChar(VarName), nil, 0);
  if BufSize > 0 then
  begin
    // Env var exists: read value into result string
    SetLength(Result, BufSize - 1); // space for #0 automatically added
    GetEnvironmentVariable(PChar(VarName), PChar(Result), BufSize);
  end
  else
    // Env var does not exist
    Result := '';
end;

function SetEnvVarValue(const VarName, VarValue: string): Integer;
  {Sets the given environment variable to the given value. Creates the
  environment variable if it doesn't already exist. Setting a variable to the
  empty string deletes it. Returns 0 on success or a Windows error code on
  error}
begin
  // Simply call SetEnvironmentVariable API function
  if SetEnvironmentVariable(PChar(VarName), PChar(VarValue)) then
    Result := 0
  else
    Result := GetLastError;
end;

function DeleteEnvVar(const VarName: string): Integer;
  {Deletes the given environment variable. Returns 0 on success of a Windows
  error code on error}
begin
  // Call SetEnvironmentVariable API function with nil value to delete var
  if SetEnvironmentVariable(PChar(VarName), nil) then
    Result := 0
  else
    Result := GetLastError;
end;

function CreateEnvBlock(const NewEnv: TStrings; const IncludeCurrent: Boolean;
  const Buffer: Pointer; const BufSize: Integer): Integer;
  {Creates a new environment block containing the given strings. If
  IncludeCurrent is true then the variable defined in the current process's
  environment block are included. The new block is stored in the memory pointed
  to by Buffer, which is at least BufSize bytes. The size of the block is
  returned. If the provided buffer is nil or is too small then no buffer is
  created. The return value gives the required buffer size}
var
  EnvVars: TStringList; // list of env vars in new block
  Idx: Integer;         // loops through all env vars in new block
  PBuf: PChar;          // points to start of each env var entry in block
begin
  // Create string list to hold all new environment vars
  EnvVars := TStringList.Create;
  try
    // include copy of current environment block if required
    if IncludeCurrent then
      GetAllEnvVars(EnvVars);
    // store given environment vars in list
    EnvVars.AddStrings(NewEnv);
    // Calculate size of new environment block
    Result := 0;
    for Idx := 0 to Pred(EnvVars.Count) do
      Inc(Result, Length(EnvVars[Idx]) + 1);
    Inc(Result);
    // Check if provided buffer is large enough and create block in it if so
    if (Buffer <> nil) and (BufSize >= Result) then
    begin
      // new environment blocks are always sorted
      EnvVars.Sorted := True;
      // do the copying
      PBuf := Buffer;
      for Idx := 0 to Pred(EnvVars.Count) do
      begin
        StrPCopy(PBuf, EnvVars[Idx]);
        Inc(PBuf, Length(EnvVars[Idx]) + 1);
      end;
      // terminate block with additional #0
      PBuf^ := #0;
    end;
  finally
    EnvVars.Free;
  end;
end;

function ExpandEnvVars(const Str: string): string;
  {Replaces any environment variables in the given string with their values and
  returns the string. Environment variables should be delimited by % characters
  thus: %ENVVAR%}
var
  BufSize: Integer; // size required for expanded string (excluding #0)
begin
  // Get required buffer size (excludes space for terminal #0)
  BufSize := ExpandEnvironmentStrings(PChar(Str), nil, 0);
  // Read expanded string into result string
  SetLength(Result, BufSize); // space for #0 automatically added
  ExpandEnvironmentStrings(PChar(Str), PChar(Result), BufSize);
end;

function GetAllEnvVars(const Vars: TStrings): Integer;
  {Copies all the environment variables available to the current process in the
  given string list, with each item in the string list representing one
  environment variable in the form NAME=VALUE. Returns the size of the
  environment block}
var
  PEnvVars: PChar;    // pointer to start of environment block
  PEnvEntry: PChar;   // pointer to an environment string in block
begin
  // Clear the list
  Vars.Clear;
  // Get reference to environment block for this process
  PEnvVars := GetEnvironmentStrings;
  if PEnvVars <> nil then
  begin
    // We have a block: extract strings from it
    // Env strings are #0 separated and list ends with #0#0
    PEnvEntry := PEnvVars;
    try
      while PEnvEntry^ <> #0 do
      begin
        Vars.Add(PEnvEntry);
        Inc(PEnvEntry, StrLen(PEnvEntry) + 1);
      end;
      // Calculate length of block
      Result := (PEnvEntry - PEnvVars) + 1;
    finally
      // Dispose of the memory block
      Windows.FreeEnvironmentStrings(PEnvEntry);
    end;
  end
  else
    // No block => zero length
    Result := 0;
end;

end.
