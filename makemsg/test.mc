;#ifndef __MESSAGES_H__
;#define __MESSAGES_H__
;

LanguageNames =
(
    English = 0x0409:test_ENU
)

;// CATEGORIES ///////////////////////////////////////////////////////////////

MessageId    = 1
SymbolicName = CAT_Installation
Severity     = Success
Language     = English
Installation
.

MessageId    = 2
SymbolicName = CAT_Server_Startup
Severity     = Success
Language     = English
Server Startup
.

MessageId    = 3
SymbolicName = CAT_Server_Shutdown
Severity     = Success
Language     = English
Server Shutdown
.


;// MESSAGES /////////////////////////////////////////////////////////////////

MessageId    = 100
SymbolicName = MSG_General
Severity     = Success
Language     = English
%s
.

MessageId    = 1000
SymbolicName = MSG_CommandLineIncomplete
Severity     = Success
Language     = English
Invalid command line.  %s (%s) has not been specified.
.


;
;#endif  //__MESSAGES_H__
;
