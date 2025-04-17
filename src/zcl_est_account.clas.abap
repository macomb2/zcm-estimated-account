class ZCL_EST_ACCOUNT definition
  public
  final
  create public .

public section.

  class-methods GET_REG_INFO
    importing
      value(IT_STUDENT) type HRIQ_STUDENT_LIST_TAB
      value(IT_PERIOD_KEY) type PIQ_PERIOD_KEY_TAB
      value(IV_FEE_GRP) type PIQ_FEE_GROUP
      value(IV_PLVAR) type PLVAR
      value(IV_FEETRIGGER) type PIQFEETRIGGER
      value(IV_OPEN_PKEY) type PERSL_KK
      value(IT_TRIG_INFO) type PIQ_TRIGGER_INFO_TAB
    exporting
      value(ET_PROGRAM) type HRIQ_REG_PROGRAM_OF_STUDY
      value(ET_ERROR) type HRIQ_ERROR_STRUCTURE .
protected section.
private section.
ENDCLASS.



CLASS ZCL_EST_ACCOUNT IMPLEMENTATION.


  METHOD get_reg_info.
    DATA: lt_program_info  TYPE hriq_reg_program_of_study.

    FIELD-SYMBOLS: <fs_program_info> TYPE piq_reg_program_of_study.

    CLEAR: et_program[].

    DATA : lv_aktiv TYPE piq_opton.
    "calling code
    IF cl_hrpiq00_switch_check=>ishercm_sfws_hana_opt_1( ) = abap_true.
      CALL METHOD cl_hrpiq00_optimize=>read_switch_for_process
        EXPORTING
          iv_optid = '0001' "To be hardcoded
        RECEIVING
          rv_aktiv = lv_aktiv.

    ENDIF.

* Read student information: registration information.
    CASE iv_fee_grp.
*   For admission fee
      WHEN '1'.
        IF lv_aktiv = abap_true.
          CALL FUNCTION 'HRIQ_READ_REGISTERED_PROG_OPT'
            EXPORTING
              iv_plvar            = iv_plvar
              iv_istat            = '2'
              it_student          = it_student[]
              it_period_key       = it_period_key[]
            IMPORTING
              et_reg_program_info = lt_program_info
            TABLES
              et_error            = et_error.

        ELSE.
          CALL FUNCTION 'HRIQ_READ_REGISTERED_PROGRAM'
            EXPORTING
              iv_plvar            = iv_plvar
              iv_istat            = '2'
              it_student          = it_student[]
              it_period_key       = it_period_key[]
            IMPORTING
              et_reg_program_info = lt_program_info
            TABLES
              et_error            = et_error.
        ENDIF.
*   For study related(with Fee Trigger)
      WHEN '2'.
        IF lv_aktiv = abap_true.
          CALL FUNCTION 'HRIQ_READ_REGISTERED_PROG_OPT'
            EXPORTING
              iv_plvar            = iv_plvar
              iv_istat            = '1'
              iv_feetrigger       = iv_feetrigger
              iv_open_pkey        = iv_open_pkey
              it_student          = it_student[]
              it_period_key       = it_period_key[]
              it_trig_info        = it_trig_info[]
            IMPORTING
              et_reg_program_info = lt_program_info
            TABLES
              et_error            = et_error.
        ELSE.
          CALL FUNCTION 'HRIQ_READ_REGISTERED_PROGRAM'
            EXPORTING
              iv_plvar            = iv_plvar
              iv_istat            = '1'
              iv_feetrigger       = iv_feetrigger
              iv_open_pkey        = iv_open_pkey
              it_student          = it_student[]
              it_period_key       = it_period_key[]
              it_trig_info        = it_trig_info[]
            IMPORTING
              et_reg_program_info = lt_program_info
            TABLES
              et_error            = et_error.
        ENDIF.

    ENDCASE.
    CHECK NOT lt_program_info[] IS INITIAL.

    LOOP AT lt_program_info ASSIGNING <fs_program_info>.
      APPEND <fs_program_info> TO et_program.
    ENDLOOP.

    SORT et_program BY peryr perid scobjid.
  ENDMETHOD.
ENDCLASS.
