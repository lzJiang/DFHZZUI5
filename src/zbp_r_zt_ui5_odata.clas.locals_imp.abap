CLASS lsc_zr_zt_ui5_odata DEFINITION INHERITING FROM cl_abap_behavior_saver.

  PROTECTED SECTION.

    METHODS save_modified REDEFINITION.

ENDCLASS.

CLASS lsc_zr_zt_ui5_odata IMPLEMENTATION.

  METHOD save_modified.
    DATA:ls_zzt_ui5_odata TYPE zzt_ui5_odata.
    IF create-zrztui5odata IS NOT INITIAL.

      READ TABLE create-zrztui5odata[] INTO DATA(ls_req) INDEX 1.
      IF sy-subrc = 0.
*      CALL FUNCTION 'ZZUI5_FG_ODATA_0001'
*        EXPORTING
*          request_code      = ls_req-requestcode
*          request_parameter = ls_req-requestparameter
*        IMPORTING
*          return_result     = ls_req-returnresult
*          return_code       = ls_req-returncode
*          return_message    = ls_req-returnmessage.
        MOVE-CORRESPONDING ls_req TO ls_zzt_ui5_odata.
        MODIFY zzt_ui5_odata FROM @ls_zzt_ui5_odata.
        zclui5_odata_util=>update_zzt_pp_001( EXPORTING ls_input = ls_zzt_ui5_odata ).
        zclui5_odata_util_0002=>save_zztfi_0003( EXPORTING ls_input = ls_zzt_ui5_odata ).
        zclui5_odata_util_0003=>save_lld( EXPORTING ls_input = ls_zzt_ui5_odata ).
      ENDIF.

      ENDIF.

      IF update-zrztui5odata IS NOT INITIAL.

        " Provide table of instance data of all instances that have been updated during current transaction

        " Use %CONTROL to get information on what entity fields have been updated

      ENDIF.

      IF delete-zrztui5odata IS NOT INITIAL.

        " Provide table with keys of all instances that have been deleted during current transaction

        " NOTE: There is no information on fields when deleting instances
      ENDIF.
    ENDMETHOD.

*  METHOD adjust_numbers.
*    "get all records from buffer
*    READ ENTITY IN LOCAL MODE zr_zt_ui5_odata
*     ALL FIELDS WITH VALUE #( FOR ls_zrztui5odata IN mapped-zrztui5odata ( %pky = ls_zrztui5odata-%pre ) )
*     RESULT DATA(lt_zrztui5odata).
*
*    "prepare the create requests
*    LOOP AT mapped-zrztui5odata ASSIGNING FIELD-SYMBOL(<ls_bonus_key>).
*      "get the record from transaction buffer for this key
*      DATA(lr_bonus) = REF #( lt_zrztui5odata[ KEY pid COMPONENTS %pid = <ls_bonus_key>-%pid
*                                                              %key = <ls_bonus_key>-%tmp ] ).
*      ASSIGN lr_bonus->* TO FIELD-SYMBOL(<fs_data>).
*      ASSIGN COMPONENT 'Requestcode' OF STRUCTURE <fs_data> TO FIELD-SYMBOL(<fs_requestcode>).
*      ASSIGN COMPONENT 'Returnmessage' OF STRUCTURE <fs_data> TO FIELD-SYMBOL(<fs_returnmessage>).
*      IF <fs_requestcode> IS ASSIGNED.
*        <ls_bonus_key>-requestcode = <fs_requestcode>.
*      ENDIF.
*      IF <fs_returnmessage> IS ASSIGNED.
*        <fs_returnmessage> = '1111'.
*      ENDIF.
*
*    ENDLOOP.
*  ENDMETHOD.

ENDCLASS.

CLASS LHC_ZR_ZT_UI5_ODATA DEFINITION INHERITING FROM CL_ABAP_BEHAVIOR_HANDLER.
  PRIVATE SECTION.
    METHODS:
      GET_GLOBAL_AUTHORIZATIONS FOR GLOBAL AUTHORIZATION
        IMPORTING
           REQUEST requested_authorizations FOR ZrZtUi5Odata
        RESULT result,
      setReturn FOR DETERMINE ON MODIFY
            IMPORTING keys FOR ZrZtUi5Odata~setReturn.

ENDCLASS.

CLASS LHC_ZR_ZT_UI5_ODATA IMPLEMENTATION.
  METHOD GET_GLOBAL_AUTHORIZATIONS.
  ENDMETHOD.
  METHOD setreturn.
    "Read travel instances of the transferred keys
    READ ENTITIES OF zr_zt_ui5_odata IN LOCAL MODE
     ENTITY zrztui5odata
       ALL FIELDS
       WITH CORRESPONDING #( keys )
     RESULT DATA(lt_ui5odata)
     FAILED DATA(read_failed).

    "If overall travel status is already set, do nothing, i.e. remove such instances
    CHECK lt_ui5odata[] IS NOT INITIAL.

    READ TABLE lt_ui5odata INTO DATA(ls_ui5odata) INDEX 1.
    IF sy-subrc = 0.
      CALL FUNCTION 'ZZUI5_FG_ODATA_0001'
        EXPORTING
          request_code      = ls_ui5odata-requestcode
          request_parameter = ls_ui5odata-requestparameter
        IMPORTING
          return_result     = ls_ui5odata-returnresult
          return_code       = ls_ui5odata-returncode
          return_message    = ls_ui5odata-returnmessage.

      "else set overall travel status to open ('O')
      MODIFY ENTITIES OF zr_zt_ui5_odata IN LOCAL MODE
        ENTITY zrztui5odata
          UPDATE SET FIELDS
          WITH VALUE #( ( %tky    = ls_ui5odata-%tky
                          returncode = ls_ui5odata-returncode
                          returnmessage = ls_ui5odata-returnmessage
                          returnresult = ls_ui5odata-returnresult ) )
      REPORTED DATA(update_reported)
      FAILED DATA(update_failed)
      MAPPED DATA(update_mapped).

      "Set the changing parameter
      reported = CORRESPONDING #( DEEP update_reported ).

    ENDIF.

  ENDMETHOD.



ENDCLASS.
