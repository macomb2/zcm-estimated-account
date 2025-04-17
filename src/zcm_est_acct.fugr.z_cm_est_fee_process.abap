FUNCTION z_cm_est_fee_process.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_STUDENT) TYPE  PIQSTUDENT
*"     VALUE(IV_PLVAR) TYPE  PLVAR DEFAULT '01'
*"     VALUE(IV_DATE) TYPE  SYDATUM
*"     VALUE(IV_FEE_GROUP) TYPE  PIQ_FEE_GROUP DEFAULT '2'
*"     VALUE(IV_FEEMODE) TYPE  PIQFEECALCMODE DEFAULT 'D'
*"     VALUE(IS_PERIOD_HEADER) TYPE  PIQ_PKEY_CTRL
*"     VALUE(IS_FEECTRL) TYPE  PIQ_FEECTRL OPTIONAL
*"  EXPORTING
*"     VALUE(ET_FEEDOC_NR) TYPE  CMAC_FEE_DOCNR_T
*"     VALUE(ET_FEE_RESULT) TYPE  PIQAD_FEE_SIMULATE_RESULT_T
*"     VALUE(EV_TUITION) TYPE  CMAC_TOTAL_AMT
*"     VALUE(EV_ONLINE) TYPE  CMAC_TOTAL_AMT
*"     VALUE(EV_FEE) TYPE  CMAC_TOTAL_AMT
*"     VALUE(ET_COURSES) TYPE  PIQ_REG_COURSES_TAB
*"  TABLES
*"      ET_ERROR STRUCTURE  PIQ_ERROR_STRUCTURE
*"      ET_FIKEY STRUCTURE  CMAC_FIKEY
*"----------------------------------------------------------------------
  DATA: ls_program_of_study     TYPE piq_program_of_study,
        ls_reg_courses          TYPE piq_reg_courses,
        ls_course_info          TYPE piq_student_module,
        ls_st_info              TYPE cmac_st,
        ls_sc_info              TYPE cmac_sc,
        ls_sm_info              TYPE cmac_sm,
        ls_fee_control          TYPE cmac_fee_control,
        ls_feectrl              TYPE piq_feectrl,
        ls_feectrle             TYPE t7piqfeectrle,
        lt_pkey_info            TYPE TABLE OF piq_period_key,
        lt_fee_tree             TYPE cmac_t_fee_tree,
        lt_fee_tree_exc         TYPE cmac_t_fee_tree,
        lt_reg_course_info      TYPE piq_reg_courses_tab,
        lt_reg_courses          TYPE piq_reg_courses_tab,
        lt_course_info          TYPE piq_student_module_tab,
        lt_reg_program_of_study TYPE TABLE OF piq_reg_program_of_study,
        lt_program              TYPE TABLE OF piq_reg_program_of_study,
        ls_program              TYPE piq_reg_program_of_study,
        lt_program_of_study     TYPE TABLE OF piq_program_of_study,
        lt_period_range         TYPE TABLE OF piq_period_key,
        lt_sc_info              TYPE TABLE OF cmac_sc,
        lt_sm_info              TYPE TABLE OF cmac_sm,
        lt_sc_info2             TYPE TABLE OF cmac_sc,
        lt_sc_objects           TYPE hrobject_t,
        ls_sc_object            TYPE hrobject,
        ls_sc_info2             TYPE cmac_sc,
        lt_auto_post_fee        TYPE cmac_t_fee_type,
        lt_manl_post_fee        TYPE cmac_t_fee_type,
        lt_header_single        TYPE TABLE OF cmac_fkkko,
        lt_bp_items_single      TYPE TABLE OF cmac_fkkop,
        lt_gl_items_single      TYPE TABLE OF cmac_fkkopk,
        lt_cl_items_single      TYPE TABLE OF cmac_fkkcl,
        lt_return_single        TYPE bapiret2_t,
        ls_fikey                TYPE cmac_fikey,
        wa_reg_program          TYPE piq_reg_program_of_study,
        ls_student_info         TYPE piq_student_info,
        lv_tabix                TYPE sytabix,
        lv_indx                 TYPE cmac_index_number,
        lv_continue             TYPE boole_d,
        lv_switch_ad            TYPE boole_d,
        lv_resob                TYPE resob_kk,
        lv_resky                TYPE resky_kk,
        lv_mass_start           TYPE flag,
        lv_student              TYPE piqstudent,
        lv_fee_group            TYPE piq_fee_group,
        lv_plvar                TYPE plvar,
        lv_feetrigger           TYPE piqfeetrigger,
        lv_open_pkey            TYPE persl_kk,
        lv_fee_pkey             TYPE persl_kk,
        lt_student              TYPE TABLE OF piqstudent,
        ls_period               TYPE piq_period_key,
        lt_period_key           TYPE TABLE OF piq_period_key,
        lt_period_key2          TYPE TABLE OF piq_period_key,
        ls_period_key2          TYPE piq_period_key,
        lt_timelimits2          TYPE TABLE OF piqtimelimits,
        ls_timelimits2          TYPE piqtimelimits,
        lt_hold_key             TYPE TABLE OF piq_period_key,
        lt_calc_fees            TYPE cmac_t_fee_duedates,
        lt_cmac_post            TYPE TABLE OF cmac_fee_simulate_result,
        ls_cmac_post            TYPE cmac_fee_simulate_result,
        lv_length               TYPE i,
        lv_last_char            TYPE c,
        lt_a913                 TYPE TABLE OF a913,
        lv_accttype             TYPE zcmestfeetype,
        lv_prevyear             TYPE piqperyr,
        lv_percent              TYPE zcmestpercent,
        lt_trig_info            TYPE TABLE OF piq_trigger_info,
        lv_optxt                TYPE OPTXT_KK,
        lv_cmac_idx             TYPE sy-tabix.

* Data for fee calculation audit
  DATA: lv_feedoc_nr TYPE cmac_fee_docnr,
        lt_feedoc_ad TYPE cmac_itemres_t,
        lt_cmficadoc TYPE cmac_feefica_t.

* Data for the posting
  FIELD-SYMBOLS: <lfs_reg_program>  TYPE piq_reg_program_of_study,
                 <fs_open_pkey>     TYPE t7piqpkey,
                 <lfs_reg_course>   TYPE piq_reg_courses,
                 <fee_tree>         TYPE cmac_s_fee_tree,
                 <fee_result>       TYPE cmac_s_fee_result,
                 <fs_return_single> TYPE bapiret2,
                 <fs_header_single> TYPE cmac_fkkko,
                 <fs_trig_info>     TYPE piq_trigger_info,
                 <fs_ficadoc>       TYPE cmacdb_feefica.

* The keydate category for each object type
  DATA: lv_keydate   TYPE piqkeydate,
        lv_persl     TYPE persl_kk,
        lv_fee_persl TYPE persl_kk,
        lv_dummy     TYPE cmac_msg_handler.
  DATA: lt_period_range2 TYPE TABLE OF piq_period_key,
        ls_period_key    TYPE piq_period_key,
        ls_hold_key      TYPE piq_period_key,
        lt_timelimits    TYPE TABLE OF piqtimelimits,
        ls_timelimit     TYPE piqtimelimits,
        lt_keydate       TYPE TABLE OF piq_objtype_keydate_category,
        ls_keydate       TYPE piq_objtype_keydate_category,
        lt_error         TYPE TABLE OF piq_error_structure.
  DATA : lv_aktiv TYPE piq_opton.

  "remove structural Authorization for RFC
  SET PARAMETER ID 'ZNOAUTHCHECK' FIELD 'X'.

  is_feectrl-mandt = '300'.
  is_feectrl-pmode = '1'.
  "calling code
  IF cl_hrpiq00_switch_check=>ishercm_sfws_hana_opt_1( ) = abap_true.
    CALL METHOD cl_hrpiq00_optimize=>read_switch_for_process
      EXPORTING
        iv_optid = '0001' "To be hardcoded
      RECEIVING
        rv_aktiv = lv_aktiv.

  ENDIF.

  CALL FUNCTION 'HRIQ_READ_PERIOD_KEY_INFO'
    EXPORTING
      iv_period_key = is_period_header-persl
    IMPORTING
      et_period_key = lt_period_key2
    TABLES
      et_error      = lt_error.

  IF et_error IS INITIAL.
    READ TABLE lt_period_key2 INDEX 1 INTO ls_period_key2.
    CALL FUNCTION 'HRIQ_ACAD_GET_PERIOD_DATES'
      EXPORTING
        period                   = ls_period_key2-perid
        year                     = ls_period_key2-ayear
*       SEARCH_DATE_BEG          = SY-DATUM
*       SEARCH_DATE_END          = SY-DATUM
      TABLES
        timelimits               = lt_timelimits2
      EXCEPTIONS
        no_data_found            = 1
        year_not_existent        = 2
        period_not_existent      = 3
        objecttype_not_supported = 4
        no_active_plvar          = 5
        no_periods_found         = 6
        OTHERS                   = 7.
    IF sy-subrc <> 0.

    ENDIF.
    READ TABLE lt_timelimits2 WITH KEY ca_timelimit = '0100' INTO ls_timelimits2.
    IF sy-subrc = 0.
      iv_date = ls_timelimits2-ca_lbegda.
    ENDIF.
  ENDIF.

  lv_fee_persl = is_period_header-persl.
  IF is_period_header-persl+2(2) = '10'.
    lv_fee_persl(2)  = is_period_header-persl(2) - 1.
    lv_fee_persl+2(2) = is_period_header-persl+2(2).
    IF iv_date IS NOT INITIAL.
      CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
        EXPORTING
          date      = iv_date
          days      = 0
          months    = 0
          signum    = '-'
          years     = 1
        IMPORTING
          calc_date = iv_date.
    ENDIF.
  ENDIF.

************************************************************************
*               Get the instance if BADI handler                       *
************************************************************************
  IF exit IS INITIAL.
    CALL METHOD cl_exithandler=>get_instance
      EXPORTING
        exit_name              = 'CMAC_BADI_PRICING'
        null_instance_accepted = 'X'
      CHANGING
        instance               = exit
      EXCEPTIONS
        OTHERS                 = 0.
  ENDIF.

************************************************************************
*                    Get the fee control data                          *
************************************************************************

* Get the fee control data
  CALL FUNCTION 'CMAC_FEE_CONTROL_GET'
    EXPORTING
      iv_period_key  = is_period_header-persl
      iv_fee_group   = iv_fee_group
      iv_calcdate    = iv_date
      iv_pmode       = is_feectrl-pmode
      iv_feemode     = iv_feemode
    CHANGING
      cs_fee_control = ls_fee_control
    EXCEPTIONS
      no_data_found  = 1
      OTHERS         = 2.
  IF sy-subrc <> 0.
    CALL FUNCTION 'CMAC_ERROR_HANDLE'
      EXPORTING
        iv_otype     = c_student
        iv_subobject = c_log_sub_feecalc
      TABLES
        et_error     = et_error.
  ENDIF.

* get indicator if price per position should be stored.
  PERFORM check_price_switch CHANGING lv_switch_ad.

  CASE iv_fee_group.
    WHEN '1'.
      CALL FUNCTION 'HRIQ_READ_PERIOD_KEY_INFO'
        EXPORTING
          iv_period_key = is_period_header-persl
        IMPORTING
          et_period_key = lt_pkey_info
        TABLES
          et_error      = lt_error.
      APPEND LINES OF lt_pkey_info TO lt_period_key.
    WHEN '2'.
*     1. Ext.Trigger => Int. Trigger
      SELECT SINGLE * INTO  ls_feectrle
                      FROM  t7piqfeectrle
                      WHERE feecalcmode = iv_feemode.
      MOVE-CORRESPONDING ls_feectrle TO ls_feectrl.
*     2. Get Int. Trigger setting
      IF NOT ls_feectrle-trigger1 IS INITIAL.
        SELECT * APPENDING CORRESPONDING FIELDS OF
                 TABLE lt_trig_info
                 FROM  t7piqfeetrig
                 WHERE int_trig = ls_feectrle-trigger1.
        LOOP AT lt_trig_info ASSIGNING <fs_trig_info>.
          <fs_trig_info>-pmode    = ls_feectrle-pmode.
          <fs_trig_info>-ext_trig = iv_feemode.
        ENDLOOP.
      ENDIF.
      IF NOT ls_feectrle-trigger2 IS INITIAL.
        SELECT * APPENDING CORRESPONDING FIELDS OF
                 TABLE lt_trig_info
                 FROM  t7piqfeetrig
                 WHERE int_trig = ls_feectrle-trigger2.
        LOOP AT lt_trig_info ASSIGNING <fs_trig_info>.
          <fs_trig_info>-pmode    = ls_feectrle-pmode.
          <fs_trig_info>-ext_trig = iv_feemode.
        ENDLOOP.
      ENDIF.
*     3. Get Period Key information
      IF lt_trig_info[] IS INITIAL.
        MESSAGE e135(hrpiq00accounting) INTO lv_dummy.
        PERFORM fill_error_return TABLES lt_error
                                  USING  c_dummy_objid
                                         space
                                         c_log_sub_general.
        APPEND LINES OF lt_error TO et_error.
        CLEAR: lt_error[].
        EXIT.
      ENDIF.
      SORT lt_trig_info BY smode.
      LOOP AT lt_trig_info ASSIGNING <fs_trig_info>.
        AT NEW smode.
          CLEAR: lt_pkey_info[].
          CASE <fs_trig_info>-smode.
            WHEN '1'.
              CALL FUNCTION 'HRIQ_READ_PERIOD_KEY_INFO'
                EXPORTING
                  iv_period_key = is_period_header-persl
                IMPORTING
                  et_period_key = lt_pkey_info
                TABLES
                  et_error      = lt_error.
              APPEND LINES OF lt_pkey_info TO lt_period_key.
            WHEN '2'.
              CALL FUNCTION 'HRIQ_READ_PERIOD_KEY_INFO'
                EXPORTING
                  iv_period_key = is_period_header-prev_persl
                IMPORTING
                  et_period_key = lt_pkey_info
                TABLES
                  et_error      = lt_error.
              APPEND LINES OF lt_pkey_info TO lt_period_key.
          ENDCASE.
        ENDAT.
      ENDLOOP.
  ENDCASE.

* Get 'Key for period allocation'
  IF NOT lt_period_key[] IS INITIAL.
    lt_hold_key = lt_period_key.
    READ TABLE lt_period_key INDEX 1 INTO ls_period_key.
    lv_persl = ls_period_key-persl.
    ls_hold_key = ls_period_key.
  ENDIF.

  APPEND iv_student TO lt_student.
  zcl_est_account=>get_reg_info(
        EXPORTING
            it_student      = lt_student
            it_period_key   = lt_period_key
            iv_fee_grp      = iv_fee_group
            iv_plvar        = iv_plvar
            iv_feetrigger   = iv_feemode
            iv_open_pkey    = lv_persl
            it_trig_info    = lt_trig_info
        IMPORTING
            et_program      = lt_program
            et_error        = lt_error ).

  IF lv_aktiv = abap_true." if the switch is on, then optimized logic is executed
    DATA : lt_student_info TYPE STANDARD TABLE OF student_info .

    SELECT * FROM student_info INTO CORRESPONDING FIELDS OF TABLE lt_student_info
      FOR ALL ENTRIES IN lt_program WHERE stobjid = lt_program-stobjid.

  ENDIF.

* Main loop, the fee calculation will be done student by student
  SORT lt_program BY stobjid.
  IF lt_program IS INITIAL.
    MESSAGE e001(zcm_est_acct) WITH is_period_header-persl INTO lv_dummy.
    PERFORM fill_error_return TABLES lt_error
                              USING  c_dummy_objid
                                     space
                                     c_log_sub_general.
    APPEND LINES OF lt_error TO et_error.
    EXIT.
  ENDIF.

  LOOP AT lt_program INTO ls_program.

    lv_tabix = sy-tabix.

*   Group internal table by Student
    AT NEW stobjid.

      CLEAR: wa_reg_program, ls_st_info, gt_komk[], ls_student_info,
             lt_reg_courses[], lt_sc_info[], lt_sm_info[],
             lt_reg_program_of_study[], lt_reg_course_info[].

      CLEAR: lt_header_single[],   lt_bp_items_single[],
             lt_gl_items_single[], lt_cl_items_single[],
             lt_return_single[].

      CLEAR: lv_feedoc_nr, lt_feedoc_ad[], lt_cmficadoc[].

      CLEAR: ls_fee_control-feeproc.

      READ TABLE lt_program INTO wa_reg_program INDEX lv_tabix.
*     Save programs of studies
      LOOP AT lt_program ASSIGNING <lfs_reg_program>
                          WHERE stobjid = wa_reg_program-stobjid.
        IF <lfs_reg_program> IS ASSIGNED.
          APPEND <lfs_reg_program> TO lt_reg_program_of_study.
        ENDIF.
      ENDLOOP.

*****************************************************************
*     Read information for the current student
*****************************************************************
*     1. Get key date for student
*        The desing is student should use standard academic session,
*        which means the category will be 03 or 04 only.
*        So there is only one 'student information' for each student.
      PERFORM get_period_range TABLES lt_reg_program_of_study
                                      lt_period_range.

*      CALL FUNCTION 'CMAC_KEYDATE_CATEGORY_READ'
*        EXPORTING
*          iv_otype       = c_student
*          iv_ayear       = wa_reg_program-peryr
*          iv_perid       = wa_reg_program-perid
*          iv_begda       = wa_reg_program-begda
*          iv_endda       = wa_reg_program-endda
*        TABLES
*          it_keydate_tab = it_keydate
*          it_period_key  = lt_period_range
*        CHANGING
*          ev_keydate     = lv_keydate.
* Note 1522107
      IF ls_fee_control-pmode = '2' AND
        ls_fee_control-cmfeegroup = '2'.
        "Read student data for current semester
        CALL FUNCTION 'HRIQ_READ_PERIOD_KEY_INFO'
          EXPORTING
            iv_period_key = is_period_header-persl
          IMPORTING
            et_period_key = lt_period_range2
          TABLES
            et_error      = lt_error[].
        IF lt_period_range2 IS NOT INITIAL.
          SORT lt_period_range2 BY ayear perid.
          READ TABLE lt_period_range2 INTO ls_period_key INDEX 1.
          CALL FUNCTION 'HRIQ_ACAD_GET_PERIOD_DATES_NEW'
            EXPORTING
              period                   = ls_period_key-perid
              year                     = ls_period_key-ayear
*             SEARCH_DATE_BEG          = SY-DATUM
*             SEARCH_DATE_END          = SY-DATUM
*             TIMEPOINT                =
            TABLES
*             OBJECT                   =
*             YEARPERIOD               =
              timelimits               = lt_timelimits
            EXCEPTIONS
              no_data_found            = 1
              year_not_existent        = 2
              period_not_existent      = 3
              objecttype_not_supported = 4
              no_active_plvar          = 5
              no_periods_found         = 6
              customizing_error        = 7
              OTHERS                   = 8.
          IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
          ENDIF.
          CALL FUNCTION 'HRIQ_READ_KEYDATE_CATEGORY'
            EXPORTING
              it_period_key = lt_period_range2[]
            IMPORTING
              et_keydate    = lt_keydate
            EXCEPTIONS
              nothing_found = 1
              only_sm       = 2
              no_sc         = 3
              no_st         = 4.
          READ TABLE lt_timelimits INTO ls_timelimit INDEX 1.
          CALL FUNCTION 'CMAC_KEYDATE_CATEGORY_READ'
            EXPORTING
              iv_otype       = c_student
              iv_ayear       = ls_period_key-ayear
              iv_perid       = ls_period_key-perid
              iv_begda       = ls_timelimit-ca_lbegda
              iv_endda       = ls_timelimit-ca_lendda
            TABLES
              it_keydate_tab = lt_keydate[]
              it_period_key  = lt_period_range2
            CHANGING
              ev_keydate     = lv_keydate.
        ENDIF.
      ELSE.
        "Read student data for given semester
        CALL FUNCTION 'HRIQ_READ_KEYDATE_CATEGORY'
          EXPORTING
            it_period_key = lt_period_key
          IMPORTING
            et_keydate    = lt_keydate
          EXCEPTIONS
            nothing_found = 1
            only_sm       = 2
            no_sc         = 3
            no_st         = 4.

        CALL FUNCTION 'CMAC_KEYDATE_CATEGORY_READ'
          EXPORTING
            iv_otype       = c_student
            iv_ayear       = wa_reg_program-peryr
            iv_perid       = wa_reg_program-perid
            iv_begda       = wa_reg_program-begda
            iv_endda       = wa_reg_program-endda
          TABLES
            it_keydate_tab = lt_keydate[]
            it_period_key  = lt_period_range
          CHANGING
            ev_keydate     = lv_keydate.
      ENDIF.

*     2. Read student information
      IF lv_aktiv EQ abap_false.

        CALL FUNCTION 'CMAC_STUDENT_INFORMATION_READ'
          EXPORTING
            iv_plvar        = iv_plvar
            iv_student_id   = iv_student
            iv_keydate      = lv_keydate
          IMPORTING
            es_student_info = ls_student_info
          TABLES
            et_error        = et_error.
        IF et_error IS INITIAL.
          lv_prevyear = ls_period_key-ayear - 1.
*          SELECT * FROM a913 INTO TABLE lt_a913
*            WHERE kappl = 'CM' AND
*                  kschl = 'ZTUA' AND
*                  cmperyr = lv_prevyear AND
*                  cmperid = ls_period_key-perid AND
*                  cmstgrp = ls_student_info-stgrp AND
*                  cmstcat = ls_student_info-stfeecat.
*          IF sy-subrc <> 0.
*            lv_length = strlen( ls_student_info-stfeecat ).
*            lv_length = lv_length - 2.
*            lv_last_char = ls_student_info-stfeecat+lv_length(1).
*            IF lv_last_char = 'L'.
*              ls_student_info-stfeecat = 'UGL'.
*            ELSEIF lv_last_char = 'U'.
*              ls_student_info-stfeecat = 'UGU'.
*            ELSE.
*              ls_student_info-stfeecat = 'GR'.
*            ENDIF.
*          ENDIF.
        ENDIF.

      ELSE.

        FIELD-SYMBOLS : <ls_stud_temp> TYPE student_info.

        "filling general and 1702 infotype data
        LOOP AT lt_student_info ASSIGNING <ls_stud_temp>
          WHERE plvar = iv_plvar AND
                stobjid = wa_reg_program-stobjid AND
               ( ( begda_1702 <= lv_keydate OR begda_1702 IS INITIAL ) AND ( endda_1702 >= lv_keydate OR endda_1702 IS INITIAL ) ).
          ls_student_info-keydate = lv_keydate.
          ls_student_info-stobjid = <ls_stud_temp>-stobjid.
          ls_student_info-stnumber = <ls_stud_temp>-stnumber.
          ls_student_info-opbuk   = <ls_stud_temp>-opbuk.
          ls_student_info-bukrs   = <ls_stud_temp>-bukrs.
          ls_student_info-natio   = <ls_stud_temp>-natio.
          ls_student_info-f_objid = <ls_stud_temp>-f_objid.
          ls_student_info-o_objid = <ls_stud_temp>-o_objid.
          EXIT.
        ENDLOOP.

        "filling 1705 infotype data
        LOOP AT lt_student_info ASSIGNING <ls_stud_temp>
          WHERE plvar = iv_plvar AND
                stobjid = wa_reg_program-stobjid AND
               ( begda_1705 <= lv_keydate AND endda_1705 >= lv_keydate ).
          ls_student_info-prevhe = <ls_stud_temp>-prevhe.
          ls_student_info-stgrp = <ls_stud_temp>-stgrp.
          EXIT.
        ENDLOOP.

        "filling 1706 infotype data
        LOOP AT lt_student_info ASSIGNING <ls_stud_temp>
          WHERE plvar = iv_plvar AND
                stobjid = wa_reg_program-stobjid AND
               ( begda_1706 <= lv_keydate AND endda_1706 >= lv_keydate  ).
          ls_student_info-stfeecat = <ls_stud_temp>-stfeecat.
          ls_student_info-bencat   = <ls_stud_temp>-bencat.
          ls_student_info-pdisct1  = <ls_stud_temp>-pdisct1.
          ls_student_info-pdisct2  = <ls_stud_temp>-pdisct2.
          ls_student_info-pdisct3  = <ls_stud_temp>-pdisct3.
          ls_student_info-pdisct4  = <ls_stud_temp>-pdisct4.
          ls_student_info-pdisct5  = <ls_stud_temp>-pdisct5.
          ls_student_info-pdisct6  = <ls_stud_temp>-pdisct6.
          EXIT.
        ENDLOOP.

        "filling 1712 infotype data
        LOOP AT lt_student_info ASSIGNING <ls_stud_temp>
          WHERE plvar = iv_plvar AND
                stobjid = wa_reg_program-stobjid AND
               ( begda_1712 <= lv_keydate AND endda_1712 >= lv_keydate  ).
          ls_student_info-visatype = <ls_stud_temp>-visatype.
          EXIT.
        ENDLOOP.

      ENDIF.

*     3. Prepare ST data for reading fee tree and fee calculation
      PERFORM convert_st_data TABLES   et_error
                              USING    lv_persl
                                       lv_keydate
                                       ls_student_info
                                       is_feectrl
                              CHANGING ls_st_info.

*     Read fee tree to get the condition
      CALL FUNCTION 'CMAC_FEE_TREE_INIT'
        EXPORTING
          is_cmac_st          = ls_st_info
        IMPORTING
          et_fee_tree         = lt_fee_tree
        CHANGING
          cs_fee_ctrl         = ls_fee_control
        EXCEPTIONS
          procedure_not_found = 1
          tree_read_error     = 2
          OTHERS              = 3.
*     Error handle
      IF sy-subrc <> 0.
        CALL FUNCTION 'CMAC_ERROR_HANDLE'
          EXPORTING
            iv_otype     = c_student
            iv_objid     = ls_st_info-cmstid
            iv_subobject = c_log_sub_feecalc
          TABLES
            et_error     = et_error.
        CONTINUE.
      ENDIF.

************************************************************************
*            Determine which information we want to know               *
************************************************************************
*     1. If infomation of SC is required
      READ TABLE lt_fee_tree TRANSPORTING NO FIELDS
                             WITH KEY otype = c_program.
      IF sy-subrc EQ 0.
        LOOP AT lt_reg_program_of_study ASSIGNING <lfs_reg_program>
                          WHERE stobjid = ls_student_info-stobjid.
*         Get key date for SC
*         Note 1522107
          CLEAR lv_keydate.
          IF ls_fee_control-pmode = '2' AND
              ls_fee_control-cmfeegroup = '2'.
            CALL FUNCTION 'CMAC_KEYDATE_CATEGORY_READ'
              EXPORTING
                iv_otype       = c_program
                iv_ayear       = ls_period_key-ayear
                iv_perid       = ls_period_key-perid
                iv_begda       = ls_timelimit-ca_lbegda
                iv_endda       = ls_timelimit-ca_lendda
              TABLES
                it_keydate_tab = lt_keydate[]
              CHANGING
                ev_keydate     = lv_keydate.
          ELSE.
            CALL FUNCTION 'CMAC_KEYDATE_CATEGORY_READ'
              EXPORTING
                iv_otype       = c_program
                iv_ayear       = <lfs_reg_program>-peryr
                iv_perid       = <lfs_reg_program>-perid
                iv_begda       = <lfs_reg_program>-begda
                iv_endda       = <lfs_reg_program>-endda
              TABLES
                it_keydate_tab = lt_keydate[]
              CHANGING
                ev_keydate     = lv_keydate.
          ENDIF.

*         Read informatoion of SC
          CALL FUNCTION 'CMAC_PROGRAM_INFORMATION_READ'
            EXPORTING
              iv_plvar            = iv_plvar
              iv_scobjid          = <lfs_reg_program>-scobjid
              iv_keydate          = lv_keydate
              iv_stobjid          = ls_student_info-stobjid
            IMPORTING
              es_program_of_study = ls_program_of_study
            TABLES
              et_error            = et_error.

*         Read program type progression results << 4.73 >>
          PERFORM read_progression_data
                                      TABLES   et_error
                                      USING    iv_plvar
                                               lv_keydate
                                               <lfs_reg_program>
                ls_program_of_study
                                      CHANGING ls_sc_info.

*         Fill relevant fields
          ls_sc_info-cmsccat = ls_program_of_study-scfeecat.

*         Prepare SC data for fee calculation(Registered)
          PERFORM convert_reg_sc_data TABLES   et_error
                                      USING    lv_keydate

                                               <lfs_reg_program>
                                      CHANGING ls_sc_info.

*         Prepare SC data for fee calculation
          PERFORM convert_sc_data
                  TABLES   et_error
                  USING    ls_program_of_study
                  CHANGING ls_sc_info.

*         Read Specialisation Data
          PERFORM get_specialisation_data  USING    lv_keydate
                                                     iv_plvar
                                                    <lfs_reg_program>
                                           CHANGING ls_sc_info.
          IF ls_fee_control-pmode = '2' AND
              ls_fee_control-cmfeegroup = '2'.
            ls_sc_info-cmperyr = ls_period_key-ayear.
            ls_sc_info-cmperid = ls_period_key-perid.
          ENDIF.

          APPEND ls_sc_info TO lt_sc_info.
          APPEND ls_program_of_study TO lt_program_of_study.
        ENDLOOP.
      ENDIF.

*     2. If infomation of SM is required
      READ TABLE lt_fee_tree TRANSPORTING NO FIELDS
                                 WITH KEY otype = c_module.
      IF sy-subrc EQ 0.
*       Check if key date category has been assigned to module
        READ TABLE lt_keydate WITH KEY otype = c_module INTO ls_keydate.
        IF sy-subrc <> 0 OR ls_keydate-category IS INITIAL.
          MESSAGE e137(hrpiq00accounting) INTO lv_dummy.
          CALL FUNCTION 'CMAC_ERROR_HANDLE'
            EXPORTING
              iv_otype     = c_student
              iv_objabbr   = ls_st_info-cmstnum
              iv_subobject = c_log_sub_feecalc
            TABLES
              et_error     = et_error.
          CONTINUE.
        ENDIF.

        LOOP AT lt_reg_program_of_study ASSIGNING <lfs_reg_program>
                          WHERE stobjid = ls_student_info-stobjid.
          CLEAR: lt_reg_course_info[].

*         Read registered SM for current student
          CALL FUNCTION 'CMAC_REGISTERED_COURSE_READ'
            EXPORTING
              iv_plvar      = iv_plvar
              iv_stobjid    = <lfs_reg_program>-stobjid
            TABLES
              et_reg_course = lt_reg_course_info
              et_error      = et_error.

          CLEAR: lt_reg_courses[].

          IF NOT lt_reg_course_info[] IS INITIAL.
            LOOP AT lt_reg_course_info ASSIGNING <lfs_reg_course>.
              READ TABLE lt_period_key TRANSPORTING NO FIELDS
                          WITH KEY ayear = <lfs_reg_course>-peryr
                                   perid = <lfs_reg_course>-perid.
              IF sy-subrc = 0.
                APPEND <lfs_reg_course> TO lt_reg_courses.
              ENDIF.
            ENDLOOP.
          ENDIF.

          IF lt_reg_courses IS INITIAL.
            MESSAGE e001(zcm_est_acct) WITH is_period_header-persl INTO lv_dummy.
            PERFORM fill_error_return TABLES lt_error
                                      USING  c_dummy_objid
                                             space
                                             c_log_sub_general.
            APPEND LINES OF lt_error TO et_error.
            EXIT.
          ELSE.
            et_courses = lt_reg_courses.
          ENDIF.
          CLEAR: lt_course_info[].
          IF ls_fee_control-pmode = '2' AND
              ls_fee_control-cmfeegroup = '2'.
*         Read information for every SM (with key date)
            CALL FUNCTION 'CMAC_COURSE_INFORMATION_READ'
              EXPORTING
                iv_plvar       = iv_plvar
                it_reg_courses = lt_reg_courses
                iv_stobjid     = ls_student_info-stobjid
              IMPORTING
                et_course_info = lt_course_info
              TABLES
                it_keydate     = lt_keydate[]
                et_error       = et_error.
          ELSE.
            CALL FUNCTION 'CMAC_COURSE_INFORMATION_READ'
              EXPORTING
                iv_plvar       = iv_plvar
                it_reg_courses = lt_reg_courses
                iv_stobjid     = ls_student_info-stobjid
              IMPORTING
                et_course_info = lt_course_info
              TABLES
                it_keydate     = lt_keydate
                et_error       = et_error.
          ENDIF.

*         Delete duplicate module information
          DELETE ADJACENT DUPLICATES FROM lt_course_info
                                     COMPARING ALL FIELDS.

          LOOP AT lt_reg_courses INTO ls_reg_courses.

* Get program type usage of module registration (SM booking) << 4.73 >>
            PERFORM get_progtype_usage
                    TABLES   et_error
                    USING    iv_plvar
                             ls_reg_courses
                             lt_reg_program_of_study
                             lt_keydate
                    CHANGING ls_sm_info.

*           Prepare SM data for fee calculation (Registered)
            PERFORM convert_reg_sm_data
                    TABLES   et_error
                    USING    ls_reg_courses
                    CHANGING ls_sm_info.

            READ TABLE lt_course_info INTO ls_course_info
                       WITH KEY smobjid    = ls_sm_info-cmsmid
                                packnumber = ls_sm_info-cmseobjid.
            IF sy-subrc = 0.
*             Prepare SM data for fee calculation
              PERFORM convert_sm_data TABLES   et_error
                                      USING    ls_course_info
                                      CHANGING ls_sm_info.
              IF ls_fee_control-pmode = '2' AND
                ls_fee_control-cmfeegroup = '2'..
                ls_sm_info-cmperyr = ls_period_key-ayear.
                ls_sm_info-cmperid = ls_period_key-perid.
              ENDIF.
              APPEND ls_sm_info TO lt_sm_info.
            ENDIF.
          ENDLOOP.
        ENDLOOP.
      ENDIF.

*     Escape the repeated registration information and errors
      SORT: et_error, lt_sc_info, lt_sm_info.
      DELETE ADJACENT DUPLICATES FROM et_error
                                 COMPARING ALL FIELDS.
      DELETE ADJACENT DUPLICATES FROM lt_sc_info
                                 COMPARING ALL FIELDS.
      DELETE ADJACENT DUPLICATES FROM lt_sm_info
                                 COMPARING ALL FIELDS.

*     BADI for all informaiton(ST, SC and SM)
      IF exit IS INITIAL.
*       Get the BADI instance
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
        CALL METHOD exit->set_all
          CHANGING
            cs_stinfo  = ls_st_info
            ct_scinfo  = lt_sc_info[]
            ct_sminfo  = lt_sm_info[]
            ct_log_tab = et_error[].
      ENDIF.

      IF is_period_header-persl+2(2) = '10'.
        ls_st_info-cmpersl = lv_fee_persl.
        IF ls_st_info-cmstkeydat IS NOT INITIAL.
          CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
            EXPORTING
              date      = ls_st_info-cmstkeydat
              days      = 0
              months    = 0
              signum    = '-'
              years     = 1
            IMPORTING
              calc_date = ls_st_info-cmstkeydat.
        ENDIF.
        ls_fee_control-persl = lv_fee_persl.
        LOOP AT lt_period_key INTO ls_period_key.
          IF ls_period_key-ayear IS NOT INITIAL.
            ls_period_key-ayear = ls_period_key-ayear - 1.
            ls_period_key-persl = lv_fee_persl.
            MODIFY lt_period_key FROM ls_period_key.
          ENDIF.
        ENDLOOP.

        LOOP AT lt_sm_info INTO ls_sm_info.
          ls_sm_info-cmperyr = ls_sm_info-cmperyr - 1.
*          IF ls_sm_info-zzyearent > 0.
*            ls_sm_info-zzyearent = ls_sm_info-zzyearent - 1.
*          ENDIF.
*          IF ls_sm_info-cmsmkeydat IS NOT INITIAL.
*            CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
*              EXPORTING
*                date      = ls_sm_info-cmsmkeydat
*                days      = 0
*                months    = 0
*                signum    = '-'
*                years     = 1
*              IMPORTING
*                calc_date = ls_sm_info-cmsmkeydat.
*          ENDIF.
          IF ls_sm_info-cmsmregdat IS NOT INITIAL.
            CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
              EXPORTING
                date      = ls_sm_info-cmsmregdat
                days      = 0
                months    = 0
                signum    = '-'
                years     = 1
              IMPORTING
                calc_date = ls_sm_info-cmsmregdat.
          ENDIF.
          IF ls_sm_info-cmsmwddate IS NOT INITIAL.
            CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
              EXPORTING
                date      = ls_sm_info-cmsmwddate
                days      = 0
                months    = 0
                signum    = '-'
                years     = 1
              IMPORTING
                calc_date = ls_sm_info-cmsmwddate.
          ENDIF.
          MODIFY lt_sm_info FROM ls_sm_info.
        ENDLOOP.

        LOOP AT lt_sc_info INTO ls_sc_info.
          ls_sc_info-cmperyr = ls_sc_info-cmperyr - 1.
          IF ls_sc_info-cmscregdat IS NOT INITIAL.
            CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
              EXPORTING
                date      = ls_sc_info-cmscregdat
                days      = 0
                months    = 0
                signum    = '-'
                years     = 1
              IMPORTING
                calc_date = ls_sc_info-cmscregdat.
          ENDIF.
          IF ls_sc_info-cmsckeydat IS NOT INITIAL.
            CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
              EXPORTING
                date      = ls_sc_info-cmsckeydat
                days      = 0
                months    = 0
                signum    = '-'
                years     = 1
              IMPORTING
                calc_date = ls_sc_info-cmsckeydat.
          ENDIF.
          MODIFY lt_sc_info FROM ls_sc_info.
        ENDLOOP.
      ENDIF.
*     ----Prepare the fee calculation data for ST, SC and SM--------
      CALL FUNCTION 'CMAC_FEE_CALC_INIT'
        TABLES
          ct_scinfo          = lt_sc_info
          ct_sminfo          = lt_sm_info
        CHANGING
          cs_stinfo          = ls_st_info
        EXCEPTIONS
          student_data_error = 1
          program_data_error = 2
          module_data_error  = 3.
      IF sy-subrc <> 0.
        CALL FUNCTION 'CMAC_ERROR_HANDLE'
          EXPORTING
            iv_otype     = c_student
            iv_objabbr   = ls_st_info-cmstnum
            iv_subobject = c_log_sub_feecalc
          TABLES
            et_error     = et_error.
        CONTINUE.
      ENDIF.

*     ----Start the Fee Calculation----------------------------------
      lt_fee_tree_exc = lt_fee_tree.
      CALL FUNCTION 'CMAC_FEE_CALCULATE'
        EXPORTING
          iv_switch_ad         = lv_switch_ad
          is_cmac_st           = ls_st_info
          is_fee_ctrl          = ls_fee_control
        IMPORTING
          et_feedoc_ad         = lt_feedoc_ad
        TABLES
          it_cmac_sc           = lt_sc_info
          it_cmac_sm           = lt_sm_info
          it_period            = lt_period_key
          ct_log_tab           = et_error
        CHANGING
          ct_cmac_fee_tree     = lt_fee_tree
          ct_cmac_fee_tree_exc = lt_fee_tree_exc
        EXCEPTIONS
          fee_procedure_error  = 1
          calendar_read_error  = 2
          pricing_error        = 3
          OTHERS               = 4.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

*     ----Get and check the calculated result-------------------------

      READ TABLE lt_fee_tree ASSIGNING <fee_tree> INDEX 1.
      IF <fee_tree> IS ASSIGNED.
        READ TABLE <fee_tree>-t_fee_result ASSIGNING <fee_result>
                                           INDEX 1.
        IF <fee_result> IS ASSIGNED.
          lt_auto_post_fee[] = <fee_result>-t_fee_type.
*         Warning: no pricing result for the student
          IF lt_auto_post_fee[] IS INITIAL.
            MESSAGE w025 WITH ls_st_info-cmstnum is_period_header-persl
                         INTO lv_dummy.
            CALL FUNCTION 'CMAC_ERROR_HANDLE'
              EXPORTING
                iv_otype     = c_student
                iv_objabbr   = ls_st_info-cmstnum
                iv_subobject = c_log_sub_feecalc
              TABLES
                et_error     = et_error.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDAT.
  ENDLOOP.

  PERFORM get_fee_calc_result TABLES lt_cmac_post
      USING lt_fee_tree[].

  READ TABLE lt_cmac_post WITH KEY
    actkey = 'TU' TRANSPORTING NO FIELDS.

  IF is_period_header-persl+2(2) = '10'.
    IF sy-subrc <> 0.
      SELECT * FROM a913 INTO TABLE lt_a913
        WHERE kappl = 'CM' AND
              kschl = 'ZTUA' AND
              cmperyr = lv_prevyear AND
              cmperid = ls_period_key-perid AND
              cmstgrp = ls_st_info-cmstgrp AND
              cmstcat = ls_st_info-cmstcat.
      IF sy-subrc <> 0.
        lv_length = strlen( ls_st_info-cmstcat ).
        lv_length = lv_length - 2.
        lv_last_char = ls_st_info-cmstcat+lv_length(1).
        IF lv_last_char = 'L'.
          ls_st_info-cmstcat = 'UGL'.
        ELSEIF lv_last_char = 'U'.
          ls_st_info-cmstcat = 'UGU'.
        ELSE.
          ls_st_info-cmstcat = 'GR'.
        ENDIF.
      ENDIF.
      CALL FUNCTION 'CMAC_FEE_CALCULATE'
        EXPORTING
          iv_switch_ad         = lv_switch_ad
          is_cmac_st           = ls_st_info
          is_fee_ctrl          = ls_fee_control
        IMPORTING
          et_feedoc_ad         = lt_feedoc_ad
        TABLES
          it_cmac_sc           = lt_sc_info
          it_cmac_sm           = lt_sm_info
          it_period            = lt_period_key
          ct_log_tab           = et_error
        CHANGING
          ct_cmac_fee_tree     = lt_fee_tree
          ct_cmac_fee_tree_exc = lt_fee_tree_exc
        EXCEPTIONS
          fee_procedure_error  = 1
          calendar_read_error  = 2
          pricing_error        = 3
          OTHERS               = 4.

      PERFORM get_fee_calc_result TABLES lt_cmac_post
          USING lt_fee_tree[].

    ENDIF.
  ENDIF.

  LOOP AT lt_cmac_post INTO ls_cmac_post.
    lv_cmac_idx = sy-tabix.
    CLEAR lv_accttype.
    SELECT accttype FROM zcmestfeebundle INTO lv_accttype
      WHERE acctkey = ls_cmac_post-actkey AND
            validdate <= sy-datum.
    ENDSELECT.

    IF ls_cmac_post-actkey = 'TU'.
      IF ls_cmac_post-optxt IS NOT INITIAL.
       SELECT SINGLE low FROM tvarvc INTO lv_optxt "#EC CI_NOORDER
       WHERE name = 'ZCM_FEECALC_ONLINE_TEXT'
         AND type = 'P'.
         IF ls_cmac_post-optxt = lv_optxt.
           lv_accttype = 'ONLINE'.
         ENDIF.
      ENDIF.
    ENDIF.
    IF is_period_header-persl+2(2) = '10'.
      CLEAR lv_percent.
      SELECT estpercent FROM zcmestfeepercent INTO lv_percent
        WHERE estyear = ls_hold_key-ayear AND
              estsession = ls_hold_key-perid AND
              estcategory = lv_accttype AND
              estresidency = ls_student_info-stgrp.
      ENDSELECT.
      IF lv_percent IS NOT INITIAL.
        ls_cmac_post-totalamt = ls_cmac_post-totalamt + ( ls_cmac_post-totalamt * lv_percent ).
        MODIFY lt_cmac_post index lv_cmac_idx FROM ls_cmac_post.
      ENDIF.
      IF lv_accttype IS NOT INITIAL.
        IF lv_accttype = 'TUITION' OR
           lv_accttype = 'MANDATORY'.
          ev_tuition = ev_tuition + ls_cmac_post-totalamt.
        ELSEIF lv_accttype = 'COURSE' OR
               lv_accttype = 'PROGRAM'.
          ev_fee = ev_fee + ls_cmac_post-totalamt.
        ELSEIF lv_accttype = 'ONLINE'.
          ev_online = ev_online + ls_cmac_post-totalamt.
        ENDIF.
      ENDIF.
    ELSE.
      IF lv_accttype IS NOT INITIAL.
        IF lv_accttype = 'TUITION' OR
           lv_accttype = 'MANDATORY'.
          ev_tuition = ev_tuition + ls_cmac_post-totalamt.
        ELSEIF lv_accttype = 'COURSE' OR
               lv_accttype = 'PROGRAM'.
          ev_fee = ev_fee + ls_cmac_post-totalamt.
        ELSEIF lv_accttype = 'ONLINE'.
          ev_online = ev_online + ls_cmac_post-totalamt.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.

  et_fee_result[] = lt_cmac_post[].
  "re-instate structural Authorization
  SET PARAMETER ID 'ZNOAUTHCHECK' FIELD space.


ENDFUNCTION.
