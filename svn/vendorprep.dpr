program vendorprep;

{$APPTYPE CONSOLE}

  uses
  Classes,
  SysUtils,
  Windows,
  CommandLineUtils in 'CommandLineUtils.pas';

var
    iFiles        : Integer = 0;
    iFolders      : Integer = 0;
    optInfo       : Boolean = FALSE;
    optVerbose    : Boolean = FALSE;
    optSimulated  : Boolean = FALSE;


  procedure CleanFolder(const aPath: String);
  var
    rec: TSearchRec;
    filename: String;
  begin
    if FindFirst(aPath + '\*.*', faAnyFile, rec) = 0 then
    try
      repeat
        if (rec.Name = '.') or (rec.Name = '..') or (rec.Name = '.svn') then
          CONTINUE;

        filename := aPath + '\' + rec.Name;

        if (rec.Attr and faDirectory) = faDirectory then
        begin
          Inc(iFolders);
          CleanFolder(filename)
        end
        else
        begin
          Inc(iFiles);

          if NOT optSimulated then
            DeleteFile(PChar(filename));

          if optVerbose or optSimulated then
          begin
            Delete(filename, 1, 2); // Remove leading '.\'
            WriteLn(filename);
          end;
        end;
      until FindNext(rec) <> 0;
    finally
      SysUtils.FindClose(rec);
    end;
  end;


begin
  WriteLn('VendorPrep');
  WriteLn('(c) Copyright 2009 Flow Software');

  optInfo       := CommandLineContains('/?') or CommandLineContains('-?')
                    or CommandLineContains('-h');
  optSimulated  := CommandLineContains('-s');
  optVerbose    := CommandLineContains('-v');

  if optInfo or ((ParamCount > 1) and NOT (optSimulated or optVerbose)) then
  begin
    WriteLn;
    WriteLn('Prepares a folder to receive a new drop of a vendor library by deleting');
    WriteLn('all files and folders except those in any .svn folders.');
    WriteLn;
    WriteLn('  USAGE:   vendorprep [-v] | [-s] | [-h | -? | /?]');
    WriteLn;
    WriteLn('           -h, -?, /?  Displays this help/usage information');
    WriteLn;
    WriteLn('           -v          V(erbose) mode.  Emit names of deleted files');
    WriteLn;
    WriteLn('           -s          S(imulated) mode.  Equivalent to verbose but filenames');
    WriteLn('                        are output only, no files are actually deleted.');
    WriteLn;
  end
  else
  begin
    CleanFolder('.');

    WriteLn;
    
    if optSimulated then
      WriteLn(Format('%d file(s) would be deleted, in %d folder(s)', [iFiles, iFolders]))
    else
      WriteLn(Format('%d file(s) deleted in %d folder(s)', [iFiles, iFolders]));
  end;
end.
