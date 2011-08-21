;#ifndef __MESSAGES_H__
;#define __MESSAGES_H__
;

LanguageNames =
(
    English = 0x0409:testdot_ENU
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
%1
.

MessageId    = 1000
SymbolicName = MSG_CommandLineIncomplete
Severity     = Success
Language     = English
Invalid command line.  %1 (%2) has not been specified.
.


;
;#endif  //__MESSAGES_H__
;
