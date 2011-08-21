program svnclean;

{$APPTYPE CONSOLE}

  uses
    Classes,
    SysUtils,
    Windows;


  procedure DeleteDir(const aPath: String);
  var
    rec: TSearchRec;
  begin
    if FindFirst(aPath + '\*.*', faAnyFile, rec) = 0 then
    try
      WriteLn('DELETING FILES in ' + aPath + '...');
      repeat
        if (rec.Name = '.') or (rec.Name = '..') then
          CONTINUE;

        if (rec.Attr and faDirectory) = faDirectory then
          DeleteDir(aPath + '\' + rec.Name)
        else
        begin
          WriteLn('DELETING ' + aPath + '\' + rec.Name);
          SetFileAttributes(PChar(aPath + '\' + rec.Name), rec.Attr xor (faReadOnly or faSysFile or faHidden));
          DeleteFile(PChar(aPath + '\' + rec.Name));
        end;
      until FindNext(rec) <> 0;
    finally
      SysUtils.FindClose(rec);
    end;

    WriteLn('REMOVING FOLDER: ' + aPath);
    SetFileAttributes(PChar(aPath), faDirectory);
    RemoveDirectory(PChar(aPath));
  end;


  procedure CleanFolder(const aPath: String);
  var
    rec: TSearchRec;
  begin
    if FindFirst(aPath + '\*.*', faAnyFile, rec) = 0 then
    try
      repeat
        // WriteLn('scanning ' + aPath + '\' + rec.Name + '...');
        if (rec.Name = '.') or (rec.Name = '..') then
          CONTINUE;

        if (rec.Attr and faDirectory) = faDirectory then
        begin
          if (rec.Name = '.svn') then
            DeleteDir(aPath + '\' + rec.Name)
          else
            CleanFolder(aPath + '\' + rec.Name);
        end;
      until FindNext(rec) <> 0;
    finally
      SysUtils.FindClose(rec);
    end;
  end;


begin
  CleanFolder(GetCurrentDir);
end.
