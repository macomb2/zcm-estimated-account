*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZCMESTFEEBUNDLE.................................*
DATA:  BEGIN OF STATUS_ZCMESTFEEBUNDLE               .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZCMESTFEEBUNDLE               .
CONTROLS: TCTRL_ZCMESTFEEBUNDLE
            TYPE TABLEVIEW USING SCREEN '0100'.
*...processing: ZCMESTFEEPERCENT................................*
DATA:  BEGIN OF STATUS_ZCMESTFEEPERCENT              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZCMESTFEEPERCENT              .
CONTROLS: TCTRL_ZCMESTFEEPERCENT
            TYPE TABLEVIEW USING SCREEN '0200'.
*.........table declarations:.................................*
TABLES: *ZCMESTFEEBUNDLE               .
TABLES: *ZCMESTFEEPERCENT              .
TABLES: ZCMESTFEEBUNDLE                .
TABLES: ZCMESTFEEPERCENT               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
