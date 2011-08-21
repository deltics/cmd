program vendordrop;

{$APPTYPE CONSOLE}

  uses
    Classes,
    SysUtils,
    Windows;


  procedure CMDCapture(aCommand:String; aOut: TStringList) ;
  const
    ReadBuffer = 2400;
  var
    Security : TSecurityAttributes;
    ReadPipe,WritePipe : THandle;
    start : TStartUpInfo;
    ProcessInfo : TProcessInformation;
    Buffer : Pchar;
    BytesRead : DWord;
    Apprunning : DWord;
  begin
    with Security do
    begin
      nlength := SizeOf(TSecurityAttributes) ;
      binherithandle := true;
      lpsecuritydescriptor := nil;
    end;

    if Createpipe (ReadPipe, WritePipe, @Security, 0) then
    begin
      Buffer := AllocMem(ReadBuffer + 1) ;
      try
        FillChar(Start,Sizeof(Start),#0) ;
        start.cb := SizeOf(start) ;
        start.hStdOutput := WritePipe;
        start.hStdInput := ReadPipe;
        start.dwFlags := STARTF_USESTDHANDLES +
                             STARTF_USESHOWWINDOW;
        start.wShowWindow := SW_HIDE;

        if CreateProcess(nil, PChar(aCommand),
                              @Security,
                              @Security,
                              true,
                              NORMAL_PRIORITY_CLASS,
                              nil,
                              nil,
                              start,
                              ProcessInfo) then
        begin
          while (WaitForSingleObject(ProcessInfo.hProcess, 100) = WAIT_TIMEOUT) do;

          repeat
            BytesRead := 0;
            ReadFile(ReadPipe, Buffer[0], ReadBuffer, BytesRead, nil) ;
            Buffer[BytesRead]:= #0;
            OemToAnsi(Buffer,Buffer) ;
            aOut.Text := aOut.Text + String(Buffer) ;
          until (BytesRead < ReadBuffer) ;
        end;
      finally
        FreeMem(Buffer) ;
      end;
      CloseHandle(ProcessInfo.hProcess) ;
      CloseHandle(ProcessInfo.hThread) ;
      CloseHandle(ReadPipe) ;
      CloseHandle(WritePipe) ;
    end;
  end;

var
  i: Integer;
  status: Char;
  filename: String;
  result: TStringList;
begin
  result := TStringList.Create;

  CMDCapture('svn status', result);

  for i := 0 to Pred(result.Count) do
  begin
    status    := result[i][1];
    filename  := Copy(result[i], 9, Length(result[i]) - 8);

    if Pos(' ', filename) <> 0 then
      filename := '"' + filename + '"';

    case status of
      '?' : WriteLn('svn add ' + filename);
      '!' : WriteLn('svn delete ' + filename);
//      'M' : WriteLn('modified: ' + filename);
    end;
  end;

  result.Free;
end.
