
  program makemsg;

{$APPTYPE CONSOLE}

{
      1 CAT_Category_1            Message Category One
        CAT_Category_2            Second Message Category
        CAT_Category_3            A Third Category of Message
  .
    100 MSG_General                 %s

   1000 MSG_CommandLineIncomplete   Invalid command line.  %s (%s) has not been specified.
}

uses
{ vcl: }
  Classes,
  SysUtils,
{ deltics: }
  Deltics.StrUtils in '..\..\rtl\Deltics.StrUtils.pas';

var
  filename: String = '';
  mcroot: String = '';
  pasroot: String = '';
  src: TStringList = NIL;
  cats: TStringList = NIL;
  msgs: TStringList = NIL;
  output: TStringList = NIL;
  maxCatNameLen: Integer = 0;
  maxMsgNameLen: Integer = 0;

  procedure Parse;
  var
    i: Integer;
    id: Integer;
    s: String;
    name: String;
    value: String;
    list: TStringList;
    part: TStringArray;
  begin
    id    := 1;
    list  := cats;

    for i := 0 to Pred(src.Count) do
    begin
      s := Trim(src[i]);

      if (s = '') or (Copy(s, 1, 2) = '//') then
        CONTINUE;

      if (s = '.') then
        if list = cats then
        begin
          id    := 1;
          list  := msgs;
          CONTINUE;
        end
        else
          BREAK;

      Split(s, ' ', part);

      if ANSIChar(s[1]) in ['0'..'9'] then
        id  := StrToInt(StrPop(s, ' '))
      else
        Inc(id);

      name  := StrPop(s, ' ');
      value := Trim(s);

      if (list = cats) and (Length(name) > maxCatNameLen) then
        maxCatNameLen := Length(name);

      if (list = msgs) and (Length(name) > maxMsgNameLen) then
        maxMsgNameLen := Length(name);

      list.AddObject(name + '=' + value, TObject(id));
    end;
  end;


  procedure CreateMC;
  var
    i: Integer;
  begin
    output.Clear;
    output.Add(';#ifndef __MESSAGES_H__');
    output.Add(';#define __MESSAGES_H__');
    output.Add(';');
    output.Add('');
    output.Add('LanguageNames =');
    output.Add('(');
    output.Add('    English = 0x0409:' + mcroot + '_ENU');
    output.Add(')');
    output.Add('');

    output.Add(';// CATEGORIES ///////////////////////////////////////////////////////////////');
    output.Add('');
    for i := 0 to Pred(cats.Count) do
    begin
      output.Add('MessageId    = ' + IntToStr(Integer(cats.Objects[i])));
      output.Add('SymbolicName = ' + cats.Names[i]);
      output.Add('Severity     = Success');
      output.Add('Language     = English');
      output.Add(cats.ValueFromIndex[i]);
      output.Add('.');
      output.Add('');
    end;
    output.Add('');

    output.Add(';// MESSAGES /////////////////////////////////////////////////////////////////');
    output.Add('');
    for i := 0 to Pred(msgs.Count) do
    begin
      output.Add('MessageId    = ' + IntToStr(Integer(msgs.Objects[i])));
      output.Add('SymbolicName = ' + msgs.Names[i]);
      output.Add('Severity     = Success');
      output.Add('Language     = English');
      output.Add(msgs.ValueFromIndex[i]);
      output.Add('.');
      output.Add('');
    end;
    output.Add('');

    output.Add(';');
    output.Add(';#endif  //__MESSAGES_H__');
    output.Add(';');

    output.SaveToFile(mcroot + '.mc');
  end;


  procedure CreatePAS;
  var
    i: Integer;
    s: String;
  begin
    output.Clear;
    output.Add('');
    output.Add('');
    output.Add('  unit ' + pasroot + ';');
    output.Add('');
    output.Add('');
    output.Add('interface');
    output.Add('');
    output.Add('  uses');
    output.Add('    Deltics.EventLog;');
    output.Add('');
    output.Add('');
    output.Add('  const');

    output.Add('    CAT_COUNT = ' + IntToStr(cats.Count) + ';');
    output.Add('');
    for i := 0 to Pred(cats.Count) do
    begin
      s := cats.Names[i];
      s := s + StringOfChar(' ', maxCatNameLen - Length(s));
      output.Add('    ' + s + ' : TEventCategoryID = ' + IntToStr(Integer(cats.Objects[i])) + ';');
    end;
    output.Add('');
    output.Add('');
    for i := 0 to Pred(msgs.Count) do
    begin
      s := msgs.Names[i];
      s := s + StringOfChar(' ', maxMsgNameLen - Length(s));
      output.Add('    ' + s + ' : TEventID = ' + IntToStr(Integer(msgs.Objects[i])) + ';');
    end;
    output.Add('');
    output.Add('');

    output.Add('implementation');
    output.Add('');
    output.Add('');

    output.Add('end.');

    if (ParamCount > 1) then
      output.SaveToFile(ParamStr(2) + '\' + pasroot + '.pas')
    else
      output.SaveToFile(pasroot + '.pas');
  end;


  procedure CreateUtils;

    function EscapeStr(const aStr: String): String;
    var
      i: Integer;
      sv: String;
    begin
      result := '';

      i := 1;
      while i <= Length(aStr) do
      begin
        result := result + aStr[i];
        case ANSIChar(aStr[i]) of
          ''''  : result := result + '''';
          '%'   : begin
                    Inc(i);
                    sv := '';
                    while (i <= Length(aStr)) and (ANSIChar(aStr[i]) in ['0'..'9']) do
                    begin
                      sv := sv + aStr[i];
                      Inc(i);
                    end;
                    result := result + IntToStr(StrToInt(sv) - 1) + ':s';
                    Dec(i);
                  end;
        end;
        Inc(i);
      end;
    end;

  var
    i: Integer;
    s: String;
  begin
    output.Clear;
    output.Add('');
    output.Add('');
    output.Add('  unit ' + pasroot + '.Utils;');
    output.Add('');
    output.Add('');
    output.Add('interface');
    output.Add('');
    output.Add('  uses');
    output.Add('    Deltics.EventLog,');
    output.Add('    ' + pasroot + ';');
    output.Add('');
    output.Add('');
    output.Add('  function MessageCategoryFromID(const aID: TEventCategoryID): String;');
    output.Add('  function MessageTextFromID(const aID: TEventID): String;');
    output.Add('');
    output.Add('');

    output.Add('implementation');
    output.Add('');
    output.Add('  uses');
    output.Add('    SysUtils;');
    output.Add('');
    output.Add('');

    output.Add('  function MessageCategoryFromID(const aID: TEventCategoryID): String;');
    output.Add('  const');
    for i := 0 to Pred(cats.Count) do
    begin
      s := cats.Names[i];
      s := s + StringOfChar(' ', maxCatNameLen - Length(s));
      output.Add('    _' + s + ' = ' + IntToStr(Integer(cats.Objects[i])) + ';');
    end;
    output.Add('  begin');
    output.Add('    case Integer(aID) of');
    for i := 0 to Pred(cats.Count) do
    begin
      s := cats.Names[i];
      s := s + StringOfChar(' ', maxCatNameLen - Length(s));
      output.Add('      _' + s + ' : result := ''' + EscapeStr(cats.ValueFromIndex[i]) + ''';');
    end;
    output.Add('    else');
    output.Add('      result := ''Unknown category ('' + IntToStr(Ord(aID)) + '')'';');
    output.Add('    end;');
    output.Add('  end;');
    output.Add('');
    output.Add('');

    output.Add('  function MessageTextFromID(const aID: TEventID): String;');
    output.Add('  const');
    for i := 0 to Pred(msgs.Count) do
    begin
      s := msgs.Names[i];
      s := s + StringOfChar(' ', maxMsgNameLen - Length(s));
      output.Add('      _' + s + ' = ' + IntToStr(Integer(msgs.Objects[i])) + ';');
    end;
    output.Add('  begin');
    output.Add('    case Integer(aID) of');
    for i := 0 to Pred(msgs.Count) do
    begin
      s := msgs.Names[i];
      s := s + StringOfChar(' ', maxMsgNameLen - Length(s));
      output.Add('      _' + s + ' : result := ''' + EscapeStr(msgs.ValueFromIndex[i]) + ''';');
    end;
    output.Add('    else');
    output.Add('      result := ''Unknown event ID ('' + IntToStr(Ord(aID)) + '')'';');
    output.Add('    end;');
    output.Add('  end;');
    output.Add('');
    output.Add('');
    output.Add('');
    output.Add('');

    output.Add('end.');

    if (ParamCount > 1) then
      output.SaveToFile(ParamStr(2) + '\' + pasroot + '.Utils.pas')
    else
      output.SaveToFile(pasroot + '.Utils.pas');
  end;


begin
  if (ParamCount = 0) then
  begin
    WriteLn('MAKEMSG - EventLog Message File Make Utility');
    WriteLn('©2011 Jolyon Smith (deltics)');
    WriteLn('');
    WriteLn('');
    WriteLn('  Creates a Message Constant (MC) file and corresponding Pascal const declarations');
    WriteLn('   unit (PAS) along with a Pascal utility routines unit to convert ID''s to message');
    WriteLn('   text, based on the contents of a message definition file.');
    WriteLn('');
    WriteLn('');
    WriteLn('  USAGE:   makemsg filename.ext [outputfilename]');
    WriteLn('');
    WriteLn('     Creates files called  :   filename.mc');
    WriteLn('                               outputfilename.pas');
    WriteLn('                               outputfilename.Utils.pas');
    WriteLn('');
    WriteLn('     If the filename contains periods (dots) other than that which separates the');
    WriteLn('      filename and the extension, then those dots are removed on the output MC file');
    WriteLn('      but preserved in the output PAS files.');
    WriteLn('');
    WriteLn('     If the outputfilename is omitted then the outputfilename is the specified');
    WriteLn('      filename with any dots removed but without any specified extension.');
    WriteLn('');
    WriteLn('');
    WriteLn('     Expects a file called :   filename.ext');
    WriteLn('');
    WriteLn('     Which must conform to this layout template:');
    WriteLn('');
    WriteLn('   |// Comment');
    WriteLn('   |     <id> <category_name>    <category text>');
    WriteLn('   |     <__> <category_name>    <category text>');
    WriteLn('   |     <__> <category_name>    <category text>');
    WriteLn('   |.');
    WriteLn('   |     <id> <message_name>     <message text');
    WriteLn('   |     <__> <message_name>     <message text');
    WriteLn('   |     <__> <message_name>     <message text');
    WriteLn('   :');
    WriteLn('  [^ indicates column 0 - file does not itself contain pipe characters]');
    WriteLn('');
    WriteLn('   <__> indicates an omitted <id> value.  Where an <id> is omitted the <id> ');
    WriteLn('         for that item is calculated by incrementing the previous <id> value.');
    WriteLn('');
    WriteLn('   Comment lines (beginning with //) are ignored.');
    WriteLn('');
    WriteLn('   A single line containing ONLY a dot (period) indicates the separation between');
    WriteLn('    category entries and message entries.');
    WriteLn('');
    WriteLn('   Whitespace before <id>, between <id> and <name> and between <name> and <text>');
    WriteLn('    is ignored.  Whitespace in the <text> value is preserved without the need ');
    WriteLn('    for explicit delimiters');
    WriteLn('');
    WriteLn('');
    WriteLn('   Example:');
    WriteLn('');
    WriteLn('   |     1 CAT_Category_1            Message Category One');
    WriteLn('   |       CAT_Category_2            Second Message Category');
    WriteLn('   |       CAT_Category_3            A Third Category of Message');
    WriteLn('   |.');
    WriteLn('   |   100 MSG_General                 %s');
    WriteLn('   |');
    WriteLn('   |  1000 MSG_CommandLineIncomplete   Invalid command line.  %s (%s) has not been specified.');
    WriteLn('');
    WriteLn('');
    EXIT;
  end;

  cats    := TStringList.Create;
  msgs    := TStringList.Create;
  output  := TStringList.Create;

  src := TStringList.Create;
  try
    try
      filename := ParamStr(1);

      mcroot  := ChangeFileExt(filename, '');
      mcroot  := StringReplace(mcroot, '.', '', [rfReplaceAll]);

      pasroot := ChangeFileExt(filename, '');

      src.LoadFromFile(filename);

      Parse;
      CreateMC;
      CreatePAS;
      CreateUtils;

    finally
      src.Free;
      msgs.Free;
      cats.Free;
      output.Free;
    end;

    ExitCode := 0;

  except
    on e: Exception do
    begin
      WriteLn('ERROR: ' + e.Message);
      ExitCode := 255;
    end;
  end;
end.
