*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZCM_EST_ACCT
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZCM_EST_ACCT       .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
