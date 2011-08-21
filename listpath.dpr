program listpath;

{$APPTYPE CONSOLE}

uses
  EnvVars,
  SysUtils;

var
  sPath: String;
  sEntry: String;
begin
  sPath := GetEnvironmentVariable('PATH');

  while sPath <> '' do
  begin
    if Pos(';', sPath) <> 0 then
      sEntry := Copy(sPath, 1, Pos(';', sPath) - 1)
    else
      sEntry := sPath;

    Delete(sPath, 1, Length(sEntry) + 1);

    WriteLn(sEntry);
  end;
end.
