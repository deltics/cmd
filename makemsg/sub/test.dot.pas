

  unit test.dot;


interface

  uses
    Deltics.EventLog;


  const
    CAT_COUNT = 3;

    CAT_Installation    : TEventCategoryID = 1;
    CAT_Server_Startup  : TEventCategoryID = 2;
    CAT_Server_Shutdown : TEventCategoryID = 3;


    MSG_General               : TEventID = 100;
    MSG_CommandLineIncomplete : TEventID = 1000;


implementation


end.
