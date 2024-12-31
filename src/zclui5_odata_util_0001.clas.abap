CLASS zclui5_odata_util_0001 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS modifyorder
      CHANGING ls_input TYPE zc_pp003
               flag     TYPE bapi_mtype
               msg      TYPE bapi_msg.

PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCLUI5_ODATA_UTIL_0001 IMPLEMENTATION.


  METHOD modifyorder.
    DATA:lv_plannedorder TYPE zc_pp003-plannedorder.
    lv_plannedorder = ls_input-plannedorder.
    lv_plannedorder = |{ lv_plannedorder ALPHA = IN }|.
    SELECT SINGLE *
             FROM i_plannedorder WITH PRIVILEGED ACCESS
            WHERE plannedorder = @lv_plannedorder
             INTO @DATA(ls_plannedorder).


    MODIFY ENTITIES OF i_plannedordertp PRIVILEGED
     ENTITY plannedorder
     UPDATE FIELDS (
     totalquantity
     baseunit
     plndorderplannedstartdate
     plndorderplannedenddate
     yy1_plannebatch_pla
     plannedorderisfirm
     )
     WITH VALUE #(
     (
     %key-plannedorder = lv_plannedorder
     %data-totalquantity = ls_input-totalquantity
     %data-baseunit = ls_plannedorder-baseunit
     %data-plndorderplannedstartdate = ls_input-plndorderplannedstartdate
     %data-plndorderplannedenddate = ls_input-plndorderplannedenddate
     %data-yy1_plannebatch_pla = ls_input-yy1_plannebatch_pla
     %data-plannedorderisfirm = ls_input-plannedorderisfirm
     %control-totalquantity = cl_abap_behv=>flag_changed
     %control-baseunit = cl_abap_behv=>flag_changed
     %control-plndorderplannedstartdate = cl_abap_behv=>flag_changed
     %control-plndorderplannedenddate = cl_abap_behv=>flag_changed
     %control-yy1_plannebatch_pla = cl_abap_behv=>flag_changed
     %control-plannedorderisfirm = cl_abap_behv=>flag_changed
     )
     )
     FAILED DATA(failed)
     REPORTED DATA(reported)
     MAPPED DATA(mapped).

    IF failed IS INITIAL.
      flag = 'S'.
      msg = '修改成功'.
      ls_input-lastchangedbyuser = cl_abap_context_info=>get_user_technical_name( ).
      ls_input-lastchangedate = cl_abap_context_info=>get_system_date( ).
    ELSE.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO FINAL(mtext).
      flag = 'E'.
      zcl_com_util=>get_eml_msg( EXPORTING  ls_failed = failed
                                            ls_reported = reported
                                            lv_component = 'PLANNEDORDER'
                                 IMPORTING msg = msg     ).
      msg = |{ msg }'/'{ mtext }|.
      msg = |修改失败:{ msg }|.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
