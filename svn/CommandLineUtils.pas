
  unit CommandLineUtils;


interface

  function CommandLineContains(const aParam: String): Boolean;



implementation

  uses
    SysUtils;
    

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function CommandLineContains(const aParam: String): Boolean;
  var
    i: Integer;
  begin
    result := FALSE;

    for i := 0 to ParamCount do
    begin
      result := SameText(ParamStr(i), aParam);
      if result then
        EXIT;
    end;
  end;


end.
