*----------------------------------------------------------------------*
***INCLUDE LZCM_EST_ACCTF01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  CHECK_PRICE_SWITCH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LV_SWITCH_AD  text
*----------------------------------------------------------------------*
FORM check_price_switch CHANGING pv_switch_ad TYPE boole_d.

  DATA: lv_switch_value TYPE piqvalue.

  CALL FUNCTION 'HRIQ_READ_T7PIQSWITCHVALUE'
    EXPORTING
      grpid           = co_grpid_studacct
      valid           = co_valid_price
    IMPORTING
      value           = lv_switch_value
    EXCEPTIONS
      entry_not_found = 1
      OTHERS          = 2.
  IF sy-subrc NE 0.
*    MESSAGE e895(hrpiq000) WITH    co_grpid_studacct
*                                   co_valid_price.
  ELSE.
    MOVE lv_switch_value TO pv_switch_ad.
  ENDIF.

ENDFORM.                    " check_price_switch
*&---------------------------------------------------------------------*
*&      Form  get_period_range
*&---------------------------------------------------------------------*
*       This form reads the periods which are involved in the
*       fee calacuation
*----------------------------------------------------------------------*
*      -->PT_REG_PROGRAM_OF_STUDY  Table contains a group of programs
*      -->PT_PERIOD_RANGE          Periods involved
*----------------------------------------------------------------------*
FORM get_period_range   TABLES
             pt_reg_program_of_study STRUCTURE piq_reg_program_of_study
             pt_period_range         STRUCTURE piq_period_key.

  DATA: lv_tabix       TYPE  sytabix,
        ls_reg_program TYPE piq_reg_program_of_study,
        lt_reg_program TYPE TABLE OF piq_reg_program_of_study.

  DATA: lv_min_year  TYPE piqperyr,
        lv_min_perid TYPE piqperid,
        lv_max_year  TYPE piqperyr VALUE '9999',
        lv_max_perid TYPE piqperid VALUE '999'.

  CLEAR: lv_min_year, lv_min_perid.

  SORT pt_reg_program_of_study BY stobjid
                                  scobjid
                                  peryr
                                  perid.

  LOOP AT pt_reg_program_of_study.

    lv_tabix = sy-tabix.

    AT NEW scobjid.
      CLEAR lt_reg_program[].
      READ TABLE pt_reg_program_of_study INTO ls_reg_program
                                              INDEX lv_tabix.
      LOOP AT pt_reg_program_of_study
                     WHERE scobjid = ls_reg_program-scobjid.
        APPEND pt_reg_program_of_study TO lt_reg_program.
      ENDLOOP.

*     find the new start year+session for this period.
      SORT lt_reg_program BY peryr perid.
      READ TABLE lt_reg_program INDEX 1 INTO ls_reg_program.
      IF ls_reg_program-peryr > lv_min_year.
        lv_min_year  = ls_reg_program-peryr.
        lv_min_perid = ls_reg_program-perid.
      ELSEIF ls_reg_program-peryr = lv_min_year.
        IF ls_reg_program-perid > lv_min_perid.
          lv_min_perid = ls_reg_program-perid.
        ENDIF.
      ENDIF.

*     find the new end year+session for this period.
      SORT lt_reg_program BY peryr DESCENDING perid DESCENDING.
      READ TABLE lt_reg_program INDEX 1 INTO ls_reg_program.
      IF ls_reg_program-peryr < lv_max_year.
        lv_max_year  = ls_reg_program-peryr.
        lv_max_perid = ls_reg_program-perid.
      ELSEIF ls_reg_program-peryr = lv_max_year.
        IF ls_reg_program-perid < lv_max_perid.
          lv_max_perid = ls_reg_program-perid.
        ENDIF.
      ENDIF.

    ENDAT.

  ENDLOOP.

  CLEAR: pt_period_range, pt_period_range[].

  pt_period_range-ayear = lv_min_year.
  pt_period_range-perid = lv_min_perid.

  APPEND pt_period_range.
  CLEAR: pt_period_range.

  pt_period_range-ayear = lv_max_year.
  pt_period_range-perid = lv_max_perid.

  APPEND pt_period_range.

ENDFORM.                               " get_period_range
*&---------------------------------------------------------------------*
*&      Form  CONVERT_ST_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->IV_AYEAR  text
*      -->IV_PERID  text
*      -->IS_ST_INFO  text
*      <--ES_ST_INFO  text
*----------------------------------------------------------------------*
FORM convert_st_data
     TABLES
       pt_error    STRUCTURE  piq_error_structure
     USING
       iv_persl    LIKE  cmac_st-cmpersl
       iv_keydate  LIKE  piq_objtype_keydate_category-keydate
       is_st_info  LIKE  piq_student_info
       is_feectrl  LIKE  t7piqfeectrl
     CHANGING
       es_st_info LIKE cmac_st.

  MOVE: is_st_info-stobjid    TO es_st_info-cmstid,
        is_st_info-stnumber   TO es_st_info-cmstnum,
*       iv_ayear              TO es_st_info-cmperyr,
*       iv_perid              TO es_st_info-cmperid,
        iv_persl              TO es_st_info-cmpersl,
        is_st_info-stgrp      TO es_st_info-cmstgrp,
        is_st_info-prevhe     TO es_st_info-cmprevhe,
        is_st_info-stfeecat   TO es_st_info-cmstcat,
        is_st_info-bencat     TO es_st_info-cmbencat,
        is_st_info-pdisct1    TO es_st_info-cmpdisct1,
        is_st_info-pdisct2    TO es_st_info-cmpdisct2,
        is_st_info-pdisct3    TO es_st_info-cmpdisct3,
        is_st_info-pdisct4    TO es_st_info-cmpdisct4,
        is_st_info-pdisct5    TO es_st_info-cmpdisct5,
        is_st_info-pdisct6    TO es_st_info-cmpdisct6,
        is_st_info-natio      TO es_st_info-cmnatio,
        is_st_info-o_objid    TO es_st_info-cmstorg,
        is_st_info-f_objid    TO es_st_info-cmstcampus,
        is_st_info-bukrs      TO es_st_info-cmcompcode,
        is_st_info-opbuk      TO es_st_info-cmcocdgrp,
        is_st_info-visatype   TO es_st_info-cmvisatype,
        iv_keydate            TO es_st_info-cmstkeydat.

* BADI for student information
  IF exit IS INITIAL.
*   Get the BADI instance
    CALL METHOD cl_exithandler=>get_instance
      EXPORTING
        exit_name              = 'CMAC_BADI_PRICING'
        null_instance_accepted = 'X'
      CHANGING
        instance               = exit
      EXCEPTIONS
        OTHERS                 = 0.

  ENDIF.
  IF NOT exit IS INITIAL.
    CALL METHOD exit->set_stinfo
      EXPORTING
        is_stinfo  = is_st_info
      CHANGING
        cs_stinfo  = es_st_info
        ct_log_tab = pt_error[].
  ENDIF.

ENDFORM.                               " CONVERT_ST_DATA
*&---------------------------------------------------------------------*
*&      Form  read_progression_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LV_KEYDATE  text
*      -->P_<LFS_REG_PROGRAM>  text
*      -->P_LS_PROGRAM_OF_STUDY  text
*      <--P_LS_SC_INFO  text
*      <--P_ET_ERROR  text
*----------------------------------------------------------------------*
FORM read_progression_data
  TABLES   pt_error  STRUCTURE piq_error_structure
  USING    pv_plvar TYPE plvar
           pv_keydate TYPE piqkeydate
           ps_reg_program TYPE piq_reg_program_of_study
           ps_program_of_study TYPE piq_program_of_study
  CHANGING ps_sc_info TYPE cmac_sc.


  DATA:
*        lt_return      TYPE bapiret2_t,
*        ls_return      TYPE bapiret2,
    lt_progresults TYPE TABLE OF bapiprog_group_result,
    ls_progresult  TYPE bapiprog_group_result.
*        ls_error       TYPE piq_error_structure.


* only of program type is filled
  CHECK NOT ps_program_of_study-progcvar IS INITIAL.

* get progression results for program type of SC object
  CALL FUNCTION 'BAPI_STUDENTPROGRESULTS_GET'
    EXPORTING
      objectid            = ps_reg_program-stobjid
      planversion         = pv_plvar
      program_type        = ps_program_of_study-progcvar
      keydate             = pv_keydate
*     READ_TEXTS          =
*     LANGUAGE_ISO        =
*     LANGUAGE            =
    TABLES
      progression_results = lt_progresults.
*     PROGRESSION_RESULTS_TXT       =
*     return                        = lt_return.


** return messages to caller, if any
*  LOOP AT lt_return INTO ls_return.
*    ls_error-objid = ps_reg_program-stobjid.
*    ls_error-otype = c_student.
*    ls_error-subobject = c_log_sub_cmadmin.
*    ls_error-msgty = ls_return-type.
*    ls_error-msgid = ls_return-id.
*    ls_error-msgno = ls_return-number.
*    ls_error-msgv1 = ls_return-message_v1.
*    ls_error-msgv2 = ls_return-message_v2.
*    ls_error-msgv3 = ls_return-message_v3.
*    ls_error-msgv4 = ls_return-message_v4.
*    APPEND ls_error TO pt_error.
*  ENDLOOP.


* move values to structure
  ps_sc_info-cmprogcvar = ps_program_of_study-progcvar.

  LOOP AT lt_progresults INTO ls_progresult.
    CASE ls_progresult-progression_category.
      WHEN '1'. "Academic Honors
        ps_sc_info-cmaward = ls_progresult-progression_result.
        ps_sc_info-cmaward_status = ls_progresult-progression_result_status.

      WHEN '2'. "Academic Standing
        ps_sc_info-cmacst = ls_progresult-progression_result.
        ps_sc_info-cmacst_status = ls_progresult-progression_result_status.

      WHEN '3'. "Academic Standing for Financial Aid
        ps_sc_info-cmacstfa = ls_progresult-progression_result.
        ps_sc_info-cmacstfa_status = ls_progresult-progression_result_status.

      WHEN '4'. "Progress Classification
        ps_sc_info-cmprcl = ls_progresult-progression_result.
        ps_sc_info-cmprcl_status = ls_progresult-progression_result_status.

      WHEN '5'. "Progress Classification for Financial Aid
        ps_sc_info-cmprclfa = ls_progresult-progression_result.
        ps_sc_info-cmprclfa_status = ls_progresult-progression_result_status.

    ENDCASE.

  ENDLOOP.


ENDFORM.                    " read_progression_data
*&---------------------------------------------------------------------*
*&      Form  CONVERT_REG_SC_DATA
*&---------------------------------------------------------------------*
*       This form maps the Program data from the HR structure to the
*       accounting  structure, customer can use the BADI to add their
*       mapping rule
*----------------------------------------------------------------------*
*      -->P_IT_ERROR      Message log table
*      -->IV_KEYDATE      Keydate for the Program of study
*      -->IS_REG_SC_INFO  Registration data for program of study
*      <--ES_SC_INFO      The Program of study data used in pricing
*----------------------------------------------------------------------*
FORM convert_reg_sc_data TABLES
                           pt_error  STRUCTURE piq_error_structure
                         USING
                           iv_keydate     TYPE piqkeydate
                           is_reg_sc_info TYPE piq_reg_program_of_study
                         CHANGING
                           es_sc_info     TYPE cmac_sc.

  MOVE: is_reg_sc_info-peryr        TO es_sc_info-cmperyr,
        is_reg_sc_info-perid        TO es_sc_info-cmperid,
        is_reg_sc_info-enteryear    TO es_sc_info-cmentyear,
        is_reg_sc_info-aclevel      TO es_sc_info-cmstage,
        is_reg_sc_info-leave_reason TO es_sc_info-cmsclabsr,
        is_reg_sc_info-repeatflag   TO es_sc_info-cmrepstage,
        is_reg_sc_info-mainprog     TO es_sc_info-cmscmain,
        is_reg_sc_info-regdate      TO es_sc_info-cmscregdat,
        is_reg_sc_info-deregtype    TO es_sc_info-cmscwdtyp,
        is_reg_sc_info-deregdate    TO es_sc_info-cmscwddat,
        is_reg_sc_info-derreason    TO es_sc_info-cmscdregr,
        is_reg_sc_info-partt        TO es_sc_info-cmptime,
        is_reg_sc_info-progclass    TO es_sc_info-cmprogcls,
        is_reg_sc_info-prs_state    TO es_sc_info-cmstate,
        is_reg_sc_info-pr_status    TO es_sc_info-cmstatus,
        is_reg_sc_info-perct        TO es_sc_info-cmperct,
        is_reg_sc_info-enrcateg     TO es_sc_info-cmenrcateg,
        is_reg_sc_info-progclass    TO es_sc_info-cmprogcls,
        is_reg_sc_info-regclass     TO es_sc_info-cmregcls,
        is_reg_sc_info-cancprocess  TO es_sc_info-cmcanctype,
        is_reg_sc_info-cancreason   TO es_sc_info-cmcancrean,
        is_reg_sc_info-cancdate     TO es_sc_info-cmcancdate,
        is_reg_sc_info-categ        TO es_sc_info-cmcateg,
        is_reg_sc_info-choice_no    TO es_sc_info-cmchoice,
        iv_keydate                  TO es_sc_info-cmsckeydat.

* BADI for registered program of study information
  IF exit IS INITIAL.
*   Get the BADI instance
    CALL METHOD cl_exithandler=>get_instance
      EXPORTING
        exit_name              = 'CMAC_BADI_PRICING'
        null_instance_accepted = 'X'
      CHANGING
        instance               = exit
      EXCEPTIONS
        OTHERS                 = 0.
  ENDIF.
  IF NOT exit IS INITIAL.
    CALL METHOD exit->set_reg_scinfo
      EXPORTING
        is_reg_scinfo = is_reg_sc_info
      CHANGING
        cs_reg_scinfo = es_sc_info
        ct_log_tab    = pt_error[].
  ENDIF.

ENDFORM.                               " CONVERT_REG_SC_DATA
*&---------------------------------------------------------------------*
*&      Form  CONVERT_SC_DATA
*&---------------------------------------------------------------------*
*       This form maps the Program data from the HR structure to the
*       accounting  structure, customer can use the BADI to add their
*       mapping rule
*----------------------------------------------------------------------*
*      -->P_IT_ERROR      Message log table
*      -->IS_SC_INFO      Program of study data
*      <--ES_SC_INFO      The Program of study data used in pricing
*----------------------------------------------------------------------*
FORM convert_sc_data TABLES
                       pt_error STRUCTURE  piq_error_structure
                     USING
                       is_sc_info  LIKE piq_program_of_study
                     CHANGING
                       es_sc_info  LIKE cmac_sc.

  MOVE: is_sc_info-scobjid    TO es_sc_info-cmscid,
        is_sc_info-o_objid    TO es_sc_info-cmscorg,
        is_sc_info-o_objid    TO es_sc_info-cmscorg,
        is_sc_info-bukrs      TO es_sc_info-cmsccocd,
*       is_sc_info-varid      to es_sc_info-cmvarid,
        is_sc_info-scfeecat   TO es_sc_info-cmsccat,
        is_sc_info-optlength  TO es_sc_info-cmsclength,
        is_sc_info-timeunit   TO es_sc_info-cmsctunit,
*       is_sc_info-fulltpartt TO es_sc_info-cmptime,
        is_sc_info-cpthd      TO es_sc_info-cmcpthd,
        is_sc_info-cpunit     TO es_sc_info-cmsccpunit,
        is_sc_info-feevalthd  TO es_sc_info-cmfvalthd,
        is_sc_info-mvunit     TO es_sc_info-cmscmvunit,
        is_sc_info-ddsched    TO es_sc_info-cmscddshed.

* BADI for Program of study information
  IF exit IS INITIAL.
*   Get the BADI instance
    CALL METHOD cl_exithandler=>get_instance
      EXPORTING
        exit_name              = 'CMAC_BADI_PRICING'
        null_instance_accepted = 'X'
      CHANGING
        instance               = exit
      EXCEPTIONS
        OTHERS                 = 0.
  ENDIF.
  IF NOT exit IS INITIAL.
    CALL METHOD exit->set_scinfo
      EXPORTING
        is_scinfo  = is_sc_info
      CHANGING
        cs_scinfo  = es_sc_info
        ct_log_tab = pt_error[].
  ENDIF.

ENDFORM.                               " CONVERT_SC_DATA
*&---------------------------------------------------------------------*
*&      Form  get_specialisation_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ET_ERROR  text
*      -->P_LV_KEYDATE  text
*      -->P_<LFS_REG_PROGRAM>  text
*      <--P_LS_SC_INFO  text
*----------------------------------------------------------------------*
FORM get_specialisation_data
                              USING    p_lv_keydate     TYPE piqkeydate
                                       p_lv_plvar       TYPE plvar
                                       p_ls_reg_program TYPE piq_reg_program_of_study
                              CHANGING p_ls_sc_info     TYPE cmac_sc.



*--- add module group booking related data to registration table
  CALL FUNCTION 'HRIQ_READ_REGISTERED_MODULEGRP'
    EXPORTING
      iv_st_objid     = p_ls_reg_program-stobjid
      iv_sc_objid     = p_ls_reg_program-scobjid
      iv_plvar        = p_lv_plvar
      iv_begda        = p_ls_reg_program-begda
      iv_endda        = p_ls_reg_program-endda
      iv_aclevel      = p_ls_reg_program-aclevel
      iv_sc_keydate   = p_lv_keydate
    IMPORTING
      ev_modgrpobjid  = p_ls_sc_info-cmcgobjid
      ev_modgrpvar    = p_ls_sc_info-cmscgrpvar
      ev_modgrpcat    = p_ls_sc_info-cmcgcateg
      ev_modgrpfeecat = p_ls_sc_info-cmcgfeecat.


ENDFORM.                    " get_specialisation_data
*&---------------------------------------------------------------------*
*&      Form  get_progtype_usage
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ET_ERROR  text
*      -->P_LS_REG_COURSES  text
*      -->P_LT_REG_PROGRAM_OF_STUDY  text
*      <--P_LS_SM_INFO  text
*----------------------------------------------------------------------*
FORM get_progtype_usage
                         TABLES   pt_error STRUCTURE piq_error_structure
                         USING    pv_plvar TYPE plvar
                                  ps_reg_courses TYPE piq_reg_courses
                                  pt_reg_program_of_study TYPE hriq_reg_program_of_study
                                  pt_keydate TYPE piq_objtype_keydate_cate_tab
                         CHANGING ps_sm_info TYPE cmac_sm.

  DATA: lt_usedlist  TYPE piqprog_gr_modulebooking_usedt,
        ls_used      TYPE piqprog_gr_modulebooking_used,
        lv_no        TYPE i,
        ls_reg       TYPE piq_reg_program_of_study,
        lv_keydate   TYPE sy-datum,
        lt_p1730     TYPE TABLE OF p1730,
        lt_hrobjects TYPE TABLE OF hrobject,
        ls_hrobject  TYPE hrobject,
        lv_progcvar  TYPE piqprogc_var.

  FIELD-SYMBOLS: <fs_p1730> TYPE p1730.



* read all program type usages for module booking
  CALL METHOD cl_hrpiq00prog_gr_modullist=>get_progtypes4modbooking
    EXPORTING
      iv_plvar      = pv_plvar
      iv_modreg_id  = ps_reg_courses-id
    IMPORTING
      et_usedlist   = lt_usedlist
    EXCEPTIONS
      not_found     = 1
      invalid_plvar = 2
      OTHERS        = 3.

  IF sy-subrc <> 0 OR lt_usedlist IS INITIAL.
    CLEAR: ps_sm_info-cmprogcvar, ps_sm_info-cmmultprogcvar.
    EXIT.
  ENDIF.

  DESCRIBE TABLE lt_usedlist LINES lv_no.

* if only one isage, take it
  IF lv_no EQ 1.
    READ TABLE lt_usedlist INTO ls_used INDEX 1.
    ps_sm_info-cmprogcvar = ls_used-progc_var.
    CLEAR ps_sm_info-cmmultprogcvar.
    EXIT.
  ENDIF.

* if more than one, take program type of leading program (if found)
  LOOP AT pt_reg_program_of_study INTO ls_reg
    WHERE peryr = ps_reg_courses-peryr AND perid = ps_reg_courses-perid AND
          NOT mainprog IS INITIAL.

*   Get key date for SC
    CALL FUNCTION 'CMAC_KEYDATE_CATEGORY_READ'
      EXPORTING
        iv_otype       = c_program
        iv_ayear       = ls_reg-peryr
        iv_perid       = ls_reg-perid
        iv_begda       = ls_reg-begda
        iv_endda       = ls_reg-endda
      TABLES
        it_keydate_tab = pt_keydate
      CHANGING
        ev_keydate     = lv_keydate.

    ls_hrobject-otype = c_program.
    ls_hrobject-plvar = pv_plvar.
    ls_hrobject-objid = ls_reg-scobjid.
    APPEND ls_hrobject TO lt_hrobjects.

    CALL FUNCTION 'HRIQ_READ_INFTY_NNNN'
      EXPORTING
        authority      = 'DISP'
        with_stru_auth = 'X'
        infty          = '1730'
        istat          = '1'
        begda          = lv_keydate
        endda          = lv_keydate
      TABLES
        innnn          = lt_p1730
        objects        = lt_hrobjects
      EXCEPTIONS
        nothing_found  = 1
        OTHERS         = 2.

    IF sy-subrc EQ 0.
      READ TABLE lt_p1730 ASSIGNING <fs_p1730> INDEX 1.
      lv_progcvar = <fs_p1730>-progcvar.
    ENDIF.

    IF NOT lv_progcvar IS INITIAL.
      READ TABLE lt_usedlist INTO ls_used WITH KEY progc_var = lv_progcvar.
    ENDIF.

    IF NOT ls_used-progc_var IS INITIAL.
      EXIT.
    ENDIF.

  ENDLOOP.

  ps_sm_info-cmprogcvar = ls_used-progc_var.
  ps_sm_info-cmmultprogcvar = 'X'.



ENDFORM.                    " get_progtype_usage
*&---------------------------------------------------------------------*
*&      Form  CONVERT_REG_SM_DATA
*&---------------------------------------------------------------------*
*       This form maps the Module data from the HR structure to the
*       accounting  structure, customer can use the BADI to add their
*       mapping rule
*----------------------------------------------------------------------*
*      -->PT_ERROR        Message log table
*      -->IS_REG_SM_INFO  Registration data for the module
*      <--ES_SC_INFO      The Module data used in pricing
*----------------------------------------------------------------------*
FORM convert_reg_sm_data TABLES   pt_error STRUCTURE piq_error_structure
                         USING    is_reg_sm_info TYPE piq_reg_courses
                         CHANGING es_sm_info     TYPE cmac_sm.

  MOVE: is_reg_sm_info-smobjid      TO es_sm_info-cmsmid,
        is_reg_sm_info-smstatus     TO es_sm_info-cmsmstats,
        is_reg_sm_info-bookdate     TO es_sm_info-cmsmregdat,
        is_reg_sm_info-bookreason   TO es_sm_info-cmsmregrea,
        is_reg_sm_info-stordate     TO es_sm_info-cmsmwddate,
        is_reg_sm_info-storreason   TO es_sm_info-cmsmwdrea,
        is_reg_sm_info-chargefree   TO es_sm_info-cmsmexmpt,
        is_reg_sm_info-transferflag TO es_sm_info-cmsmtransf,
        is_reg_sm_info-smrating     TO es_sm_info-cmsmrating,
        is_reg_sm_info-packnumber   TO es_sm_info-cmseobjid,
        is_reg_sm_info-alt_scaleid  TO es_sm_info-cmsmscalid,
        is_reg_sm_info-cpattemp     TO es_sm_info-cmsmcptemp,
        is_reg_sm_info-cpunit       TO es_sm_info-cmsmcpunit,
        is_reg_sm_info-sc_vs_d      TO es_sm_info-cmoutprog,
*        is_reg_sm_info-varid        to es_sm_info-cmvarid,
        is_reg_sm_info-peryr        TO es_sm_info-cmperyr,
        is_reg_sm_info-perid        TO es_sm_info-cmperid,
        is_reg_sm_info-waitlist     TO es_sm_info-cmsmwaitlist.

* BADI for registered module information
  IF exit IS INITIAL.
*   Get the BADI instance
    CALL METHOD cl_exithandler=>get_instance
      EXPORTING
        exit_name              = 'CMAC_BADI_PRICING'
        null_instance_accepted = 'X'
      CHANGING
        instance               = exit
      EXCEPTIONS
        OTHERS                 = 0.
  ENDIF.
  IF NOT exit IS INITIAL.
    CALL METHOD exit->set_reg_sminfo
      EXPORTING
        is_reg_sminfo = is_reg_sm_info
      CHANGING
        cs_reg_sminfo = es_sm_info
        ct_log_tab    = pt_error[].
  ENDIF.

ENDFORM.                               " CONVERT_REG_SM_DATA
*&---------------------------------------------------------------------*
*&      Form  CONVERT_SM_DATA
*&---------------------------------------------------------------------*
*       This form maps the Module data from the HR structure to the
*       accounting  structure, customer can use the BADI to add their
*       mapping rule
*----------------------------------------------------------------------*
*      -->PT_ERROR        Message log table
*      -->IS_SM_INFO      The module data from HR side
*      <--ES_SM_INFO      The Module data used in pricing
*----------------------------------------------------------------------*
FORM convert_sm_data TABLES   pt_error STRUCTURE piq_error_structure
                     USING    is_sm_info TYPE piq_student_module
                     CHANGING es_sm_info TYPE cmac_sm.

  MOVE: is_sm_info-caobjid    TO es_sm_info-cmsmcaid,
        is_sm_info-smfeecat   TO es_sm_info-cmsmcat,
        is_sm_info-o_objid    TO es_sm_info-cmsmorg,
        is_sm_info-bukrs      TO es_sm_info-cmsmcocd,
        is_sm_info-refundtyp  TO es_sm_info-cmreftype,
        is_sm_info-smfeeval   TO es_sm_info-cmsmvalue,
        is_sm_info-mvunit     TO es_sm_info-cmmvunit,
        is_sm_info-cpopt      TO es_sm_info-cmcrhrs,
        is_sm_info-cpunit     TO es_sm_info-cmcpunit,
        is_sm_info-keydate    TO es_sm_info-cmsmkeydat,
        is_sm_info-packnumber TO es_sm_info-cmseobjid,
        is_sm_info-seccat1    TO es_sm_info-cmsmsecat1,
        is_sm_info-seccat2    TO es_sm_info-cmlevcat,
        is_sm_info-smddsched  TO es_sm_info-cmsmddshed,
        is_sm_info-seddsched  TO es_sm_info-cmseddshed,
        is_sm_info-f_objid    TO es_sm_info-cmsmcampus,
        is_sm_info-delmode    TO es_sm_info-cmsmdelmod,
        is_sm_info-cpmax      TO es_sm_info-cmcrhmax,
        is_sm_info-cpmin      TO es_sm_info-cmcrhmin.

* BADI for module information
  IF exit IS INITIAL.
*   Get the BADI instance
    CALL METHOD cl_exithandler=>get_instance
      EXPORTING
        exit_name              = 'CMAC_BADI_PRICING'
        null_instance_accepted = 'X'
      CHANGING
        instance               = exit
      EXCEPTIONS
        OTHERS                 = 0.
  ENDIF.
  IF NOT exit IS INITIAL.
    CALL METHOD exit->set_sminfo
      EXPORTING
        is_sminfo  = is_sm_info
      CHANGING
        cs_sminfo  = es_sm_info
        ct_log_tab = pt_error[].
  ENDIF.

ENDFORM.                               " CONVERT_SM_DATA
*&---------------------------------------------------------------------*
*&      Form  get_fee_calc_result
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_ET_CMAC_POST[]  text
*----------------------------------------------------------------------*
FORM get_fee_calc_result  TABLES  ct_mpost STRUCTURE cmac_fee_simulate_result
                  USING  pt_fee_tree     TYPE cmac_t_fee_tree.

  DATA: ls_fee_tree   TYPE cmac_s_fee_tree,
        ls_fee_type   TYPE cmac_s_fee_type,
        ls_fee_result TYPE cmac_s_fee_result.

  READ TABLE pt_fee_tree INTO ls_fee_tree INDEX 1.
  IF sy-subrc = 0.
    READ TABLE ls_fee_tree-t_fee_result INTO ls_fee_result INDEX 1.
    IF sy-subrc = 0.
      LOOP AT ls_fee_result-t_fee_type INTO ls_fee_type.
        ct_mpost-bukrs      = ls_fee_type-bukrs.
        ct_mpost-actkey     = ls_fee_type-actkey.
        ct_mpost-doccurr    = ls_fee_type-doccuky.
        ct_mpost-autoamt    = ls_fee_type-docamt.
        IF ls_fee_type-actkey2 = 'FON'.
          ct_mpost-actkey = 'FON'.
          ct_mpost-actkeyt = 'Undergraduate Online Learning'.
        ENDIF.

        FIELD-SYMBOLS: <fs1> TYPE cmac_fee_simulate_result,
                       <fs2> TYPE cmac_s_fee_acc_obj.

        READ TABLE ct_mpost ASSIGNING <fs1>
                            WITH KEY  bukrs   = ct_mpost-bukrs
                                      actkey  = ct_mpost-actkey
                                      doccurr = ct_mpost-doccurr.
        IF sy-subrc = 0.
          <fs1>-autoamt = <fs1>-autoamt + ct_mpost-autoamt.
        ELSE.
          DATA: ls_t001 TYPE t001.
*          get the account key description.

          SELECT SINGLE vtext FROM t687t INTO  ct_mpost-actkeyt
                                 WHERE spras = sy-langu
                                 AND   kappl = 'CM'
                                 AND   kvsl1 = ct_mpost-actkey.

*         get the account description.

          CALL FUNCTION 'FI_COMPANY_CODE_DATA'
            EXPORTING
              i_bukrs = ct_mpost-bukrs
            IMPORTING
              e_t001  = ls_t001
            EXCEPTIONS
              OTHERS  = 0.

          ct_mpost-butxt = ls_t001-butxt.
          ct_mpost-totalamt = ct_mpost-autoamt.
          APPEND ct_mpost.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.

ENDFORM.                    " get_fee_calc_result

*&---------------------------------------------------------------------*
*&      Form  FILL_ERROR_RETURN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fill_error_return TABLES pt_error     TYPE hriq_error_structure
                       USING  pv_objectid  TYPE hrobjid
                              pv_otype     TYPE otype
                              pv_subobject TYPE balsubobj.

  DATA: lv_short TYPE short_d,
        wa_error TYPE piq_error_structure.

  IF pv_objectid NE c_dummy_objid.
    wa_error-objid   = pv_objectid.
  ENDIF.
  wa_error-otype     = pv_otype.
  wa_error-subobject = pv_subobject.
  wa_error-msgty     = sy-msgty.
  wa_error-msgid     = sy-msgid.
  wa_error-msgno     = sy-msgno.
  wa_error-msgv1     = sy-msgv1.
  wa_error-msgv2     = sy-msgv2.
  wa_error-msgv3     = sy-msgv3.
  wa_error-msgv4     = sy-msgv4.

  IF ( NOT pv_otype IS INITIAL ) AND
     ( NOT pv_objectid IS INITIAL ).
    CALL FUNCTION 'HRIQ_READ_OBJECT'
      EXPORTING
        plvar     = '01'
        otype     = pv_otype
        objid     = pv_objectid
      IMPORTING
        short     = lv_short
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.
    IF sy-subrc = 0.
      wa_error-objid = lv_short.
    ENDIF.
  ENDIF.

  APPEND wa_error TO pt_error.

ENDFORM.                               " FILL_ERROR_RETURN
