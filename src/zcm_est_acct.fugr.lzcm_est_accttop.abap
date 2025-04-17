FUNCTION-POOL ZCM_EST_ACCT MESSAGE-ID cmac_fee_calc. "MESSAGE-ID ..

* INCLUDE LZCM_EST_ACCTD...                  " Local class definition
* Include the constants
INCLUDE cmac_fee_const.

TYPES:
  BEGIN OF s_rule_elements,
    corule   LIKE tcmac06r-corule,
    elements TYPE cmac_t_rule_elements,
  END OF s_rule_elements,

  BEGIN OF ty_sc_acad_cal ,
    cmperyr       TYPE cmac_sc-cmperyr,
    cmperid       TYPE cmac_sc-cmperid,
    cmscid        TYPE cmac_sc-cmscid,
    objtyp        TYPE cmac_fee_display-otype,
    cmsccaid      TYPE cmac_sc-cmsccaid,
    timelimitup   TYPE tcmac04d-timelimitup,
    cmsckeydat    TYPE cmac_sc-cmsckeydat,
    tt_acad_calen TYPE piq_academic_calendar_tab,
  END OF ty_sc_acad_cal,

  BEGIN OF ty_sm_acad_cal ,
    cmperyr       TYPE cmac_sm-cmperyr,
    cmperid       TYPE cmac_sm-cmperid,
    cmsmid        TYPE cmac_sm-cmsmid,
    objtyp        TYPE cmac_fee_display-otype,
    cmsmcaid      TYPE cmac_sm-cmsmcaid,
    timelimitup   TYPE tcmac04d-timelimitup,
    cmsmkeydat    TYPE cmac_sm-cmsmkeydat,
    tt_acad_calen TYPE piq_academic_calendar_tab,
  END OF ty_sm_acad_cal.
.

DATA : gt_sc_acad_cal TYPE STANDARD TABLE OF ty_sc_acad_cal,
       gt_sm_acad_cal TYPE STANDARD TABLE OF ty_sm_acad_cal.


DATA:
  gt_komk          TYPE komk OCCURS 0 WITH HEADER LINE,
  gt_rule_elements TYPE STANDARD TABLE OF s_rule_elements.

CONSTANTS: co_grpid_studacct TYPE piqgrpid VALUE 'STUDACCT',
           c_dummy_objid TYPE hrobjid VALUE '',
           co_valid_price    TYPE piqvalid VALUE 'PRICE'.

********
* BADI *
********
CLASS cl_exithandler DEFINITION LOAD.
DATA: exit      TYPE REF TO if_ex_cmac_badi_pricing,
      delta_fee TYPE REF TO if_ex_cmac_delta_fee.
  INCLUDE LSVIMDAT                                . "general data decl.
  INCLUDE LZCM_EST_ACCTT00                        . "view rel. data dcl.
