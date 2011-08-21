

  unit test.dot.Utils;


interface

  uses
    Deltics.EventLog,
    test.dot;


  function MessageCategoryFromID(const aID: TEventCategoryID): String;
  function MessageTextFromID(const aID: TEventID): String;


implementation


  function MessageCategoryFromID(const aID: TEventCategoryID): String;
  const
    _CAT_Installation    = 1;
    _CAT_Server_Startup  = 2;
    _CAT_Server_Shutdown = 3;
  begin
    case aID of
      _CAT_Installation    : result := 'Installation';
      _CAT_Server_Startup  : result := 'Server Startup';
      _CAT_Server_Shutdown : result := 'Server Shutdown';
    else
      result := 'Unknown category (' + Ord(aID) + ')';
    end;
  end;


  function MessageTextFromID(const aID: TEventID): String;
  const
      _MSG_General               = 100;
      _MSG_CommandLineIncomplete = 1000;
  begin
    case aID of
      _MSG_General               : result := '%0:s';
      _MSG_CommandLineIncomplete : result := 'Invalid command line.  %0:s (%1:s) has not been specified.';
    else
      result := 'Unknown event ID (' + Ord(aID) + ')';
    end;
  end;




end.
