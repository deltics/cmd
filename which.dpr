program which;

{$APPTYPE CONSOLE}

uses
  EnvVars,
  SysUtils;

  function FindFile(const aFile: String): Boolean;
  var
    sPath: String;
    sEntry: String;
  begin
    result := FileExists(aFile);
    if result then
    begin
      WriteLn(GetCurrentDir + '\' + aFile);
      EXIT;
    end;

    sPath := GetEnvironmentVariable('PATH');

    while sPath <> '' do
    begin
      if Pos(';', sPath) <> 0 then
        sEntry := Copy(sPath, 1, Pos(';', sPath) - 1)
      else
        sEntry := sPath;

      Delete(sPath, 1, Length(sEntry) + 1);

      result := FileExists(sEntry + '\' + aFile);
      if result then
      begin
        WriteLn(sEntry + '\' + aFile);
        EXIT;
      end;
    end;
  end;

var
  sTarget: String;
begin
  sTarget := ParamStr(1);

  if ExtractFileExt(sTarget) = '' then
  begin
    if NOT FindFile(sTarget + '.exe') then
      FindFile(sTarget + '.bat');
  end
  else
    FindFile(sTarget);

end.
