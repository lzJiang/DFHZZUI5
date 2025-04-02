CLASS zclui5_odata_util_0005 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS save_zztmm_0003
      IMPORTING lt_input TYPE zztmm_0003_tab.
    CLASS-METHODS post
      CHANGING ls_input TYPE zztmm_0003.
    CLASS-METHODS change_manufacturedate
      CHANGING ls_input TYPE zztmm_0003.
    CLASS-METHODS change_zwmsbatch
      CHANGING ls_input TYPE zztmm_0003.
    CLASS-METHODS change_zvension
      CHANGING ls_input TYPE zztmm_0003.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCLUI5_ODATA_UTIL_0005 IMPLEMENTATION.


  METHOD change_manufacturedate.
    DATA:lv_material        TYPE zztmm_0003-material,
         lv_manufacturedate TYPE zztmm_0003-manufacturedate,
         lv_batchbysupplier TYPE i_batchtp_2-batchbysupplier.
    zcl_com_util=>matnr_zero_in( EXPORTING input = ls_input-material
                                 IMPORTING output =  lv_material ).
    SELECT SINGLE *
             FROM i_batchtp_2 WITH PRIVILEGED ACCESS
            WHERE material = @lv_material
              AND batchidentifyingplant = ''
              AND batch = @ls_input-batch
             INTO @DATA(ls_batchtp).
    IF sy-subrc NE 0.
      ls_input-flag = 'E'.
      ls_input-msg  = |物料{ ls_input-material }批次{ ls_input-batch }不存在|.
      RETURN.
    ENDIF.

    IF ls_input-manufacturedate IS INITIAL.
      lv_manufacturedate = ls_batchtp-manufacturedate.
    ENDIF.
    IF ls_input-zwmsbatch IS INITIAL.
      lv_batchbysupplier = ls_batchtp-batchbysupplier.
    ELSE.
      IF strlen( ls_input-zwmsbatch ) > 15.
        lv_batchbysupplier = ls_input-zwmsbatch+0(15).
      ELSE.
        lv_batchbysupplier = ls_input-zwmsbatch.
      ENDIF.
    ENDIF.

    MODIFY ENTITIES OF i_batchtp_2 PRIVILEGED
      ENTITY batch
        UPDATE FROM VALUE #( ( material          = lv_material
                               batch             = ls_input-batch
                               manufacturedate   = lv_manufacturedate
                               batchbysupplier   = lv_batchbysupplier
                               %control-manufacturedate = cl_abap_behv=>flag_changed
                               %control-batchbysupplier = cl_abap_behv=>flag_changed
                           ) )
      FAILED   DATA(failed)
      REPORTED DATA(reported).


    IF failed IS INITIAL.

    ELSE.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO FINAL(mtext).
      ls_input-flag = 'E'.
      zcl_com_util=>get_eml_msg( EXPORTING  ls_failed = failed
                                            ls_reported = reported
                                            lv_component = 'BATCH'
                                 IMPORTING msg = ls_input-msg     ).
      ls_input-msg = |{ ls_input-msg }'/'{ mtext }|.
      ls_input-msg = |生产日期修改失败:{ ls_input-msg }|.
    ENDIF.
  ENDMETHOD.


  METHOD change_zvension.
    DATA:lv_material TYPE zztmm_0003-material.
    zcl_com_util=>matnr_zero_in( EXPORTING input = ls_input-material
                                 IMPORTING output =  lv_material ).

    SELECT SINGLE a~*
      FROM i_clfncharacteristic WITH PRIVILEGED ACCESS AS a
     WHERE characteristic = 'Z_BANBEN'
      INTO @DATA(ls_clfncharacteristic).
    IF sy-subrc NE 0.
      ls_input-flag = 'E'.
      ls_input-msg  = |特征【Z_BANBEN】不存在|.
      RETURN.
    ENDIF.

    SELECT SINGLE b~*
      FROM i_clfncharacteristic WITH PRIVILEGED ACCESS AS a
      INNER JOIN i_batchcharacteristicvaluetp_2 WITH PRIVILEGED ACCESS AS b
        ON a~charcinternalid = b~charcinternalid
     WHERE a~characteristic = 'Z_BANBEN'
       AND b~material = @lv_material
       AND b~batch = @ls_input-batch
       AND b~batchidentifyingplant = ''
      INTO @DATA(ls_valuetp).
    IF sy-subrc = 0.
      DATA(lv_change) = 'X'.
    ENDIF.

    IF lv_change = 'X'.
      MODIFY ENTITIES OF i_batchtp_2 PRIVILEGED
      ENTITY batchcharacteristicvalue
        UPDATE FROM VALUE #( ( material                     = lv_material
                               batch                        = ls_input-batch
                               charcinternalid              = ls_valuetp-charcinternalid
                               clfncharcvaluepositionnumber = ls_valuetp-clfncharcvaluepositionnumber
                               charcvalue                   = ls_input-zvension
                               %control-charcvalue          = cl_abap_behv=>flag_changed
                           ) )
          FAILED   DATA(failed)
          REPORTED DATA(reported).
    ELSE.
      MODIFY ENTITIES OF i_batchtp_2
          ENTITY batchcharacteristic
          CREATE BY \_batchcharacteristicvaluetp
            FROM VALUE #( ( material        = lv_material
                            batch           = ls_input-batch
                            charcinternalid = ls_clfncharacteristic-charcinternalid
                            %target         = VALUE #( ( %cid                     = 'C1'
                                                         material                 = lv_material
                                                         batch                    = ls_input-batch
                                                         charcinternalid          = ls_clfncharacteristic-charcinternalid
                                                         charcvalue               = ls_input-zvension
                                                         %control-material        = cl_abap_behv=>flag_changed
                                                         %control-batch           = cl_abap_behv=>flag_changed
                                                         %control-charcinternalid = cl_abap_behv=>flag_changed
                                                         %control-charcvalue      = cl_abap_behv=>flag_changed ) )
                                                     ) )
          FAILED   failed
          REPORTED reported.
    ENDIF.

    IF failed IS INITIAL.

    ELSE.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO FINAL(mtext).
      ls_input-flag = 'E'.
      zcl_com_util=>get_eml_msg( EXPORTING  ls_failed = failed
                                            ls_reported = reported
                                            lv_component = 'BATCH'
                                 IMPORTING msg = ls_input-msg     ).
      IF ls_input-msg IS INITIAL.
        zcl_com_util=>get_eml_msg( EXPORTING  ls_failed = failed
                                              ls_reported = reported
                                              lv_component = 'BATCHCHARACTERISTIC'
                                   IMPORTING msg = ls_input-msg     ).
      ENDIF.
      IF ls_input-msg IS INITIAL.
        zcl_com_util=>get_eml_msg( EXPORTING  ls_failed = failed
                                              ls_reported = reported
                                              lv_component = 'BATCHCHARACTERISTICVALUE'
                                   IMPORTING msg = ls_input-msg     ).
      ENDIF.
      ls_input-msg = |{ ls_input-msg }'/'{ mtext }|.
      ls_input-msg = |版本修改失败:{ ls_input-msg }|.
    ENDIF.
  ENDMETHOD.


  METHOD change_zwmsbatch.
    DATA:lv_material TYPE zztmm_0003-material.
    zcl_com_util=>matnr_zero_in( EXPORTING input = ls_input-material
                                 IMPORTING output =  lv_material ).

    SELECT SINGLE a~*
      FROM i_clfncharacteristic WITH PRIVILEGED ACCESS AS a
     WHERE characteristic = 'Z_WMSBATCH'
      INTO @DATA(ls_clfncharacteristic).
    IF sy-subrc NE 0.
      ls_input-flag = 'E'.
      ls_input-msg  = |物料{ ls_input-material }批次{ ls_input-batch }特征【Z_WMSBATCH】不存在|.
      RETURN.
    ENDIF.

    SELECT SINGLE b~*
      FROM i_clfncharacteristic WITH PRIVILEGED ACCESS AS a
      INNER JOIN i_batchcharacteristicvaluetp_2 WITH PRIVILEGED ACCESS AS b
        ON a~charcinternalid = b~charcinternalid
     WHERE a~characteristic = 'Z_WMSBATCH'
       AND b~material = @lv_material
       AND b~batch = @ls_input-batch
       AND b~batchidentifyingplant = ''
      INTO @DATA(ls_valuetp).
    IF sy-subrc = 0.
      DATA(lv_change) = 'X'.
    ENDIF.
    IF lv_change = 'X'.
      MODIFY ENTITIES OF i_batchtp_2 PRIVILEGED
      ENTITY batchcharacteristicvalue
        UPDATE FROM VALUE #( ( material                     = lv_material
                               batch                        = ls_input-batch
                               charcinternalid              = ls_valuetp-charcinternalid
                               clfncharcvaluepositionnumber = ls_valuetp-clfncharcvaluepositionnumber
                               charcvalue                   = ls_input-zwmsbatch
                               %control-charcvalue          = cl_abap_behv=>flag_changed
                           ) )
          FAILED   DATA(failed)
          REPORTED DATA(reported).
    ELSE.
      MODIFY ENTITIES OF i_batchtp_2
          ENTITY batchcharacteristic
          CREATE BY \_batchcharacteristicvaluetp
            FROM VALUE #( ( material        = lv_material
                            batch           = ls_input-batch
                            charcinternalid = ls_clfncharacteristic-charcinternalid
                            %target         = VALUE #( ( %cid                     = 'C1'
                                                         material                 = lv_material
                                                         batch                    = ls_input-batch
                                                         charcinternalid          = ls_clfncharacteristic-charcinternalid
                                                         charcvalue               = ls_input-zwmsbatch
                                                         %control-material        = cl_abap_behv=>flag_changed
                                                         %control-batch           = cl_abap_behv=>flag_changed
                                                         %control-charcinternalid = cl_abap_behv=>flag_changed
                                                         %control-charcvalue      = cl_abap_behv=>flag_changed ) )
                                                     ) )
          FAILED   failed
          REPORTED reported.
    ENDIF.

    IF failed IS INITIAL.

    ELSE.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO FINAL(mtext).
      ls_input-flag = 'E'.
      zcl_com_util=>get_eml_msg( EXPORTING  ls_failed = failed
                                            ls_reported = reported
                                            lv_component = 'BATCH'
                                 IMPORTING msg = ls_input-msg     ).
      IF ls_input-msg IS INITIAL.
        zcl_com_util=>get_eml_msg( EXPORTING  ls_failed = failed
                                              ls_reported = reported
                                              lv_component = 'BATCHCHARACTERISTIC'
                                   IMPORTING msg = ls_input-msg     ).
      ENDIF.
      IF ls_input-msg IS INITIAL.
        zcl_com_util=>get_eml_msg( EXPORTING  ls_failed = failed
                                              ls_reported = reported
                                              lv_component = 'BATCHCHARACTERISTICVALUE'
                                   IMPORTING msg = ls_input-msg     ).
      ENDIF.
      ls_input-msg = |{ ls_input-msg }'/'{ mtext }|.
      ls_input-msg = |WMS批次修改失败:{ ls_input-msg }|.
    ENDIF.

  ENDMETHOD.


  METHOD post.
    DATA:lv_change     TYPE char1,
         ls_zztmm_0003 TYPE zztmm_0003.
    IF ls_input-material IS INITIAL.
      ls_input-flag = 'E'.
      ls_input-msg = '【物料号】不能为空'.
      RETURN.
    ENDIF.
    IF ls_input-batch IS INITIAL.
      ls_input-flag = 'E'.
      ls_input-msg = '【批次号】不能为空'.
      RETURN.
    ENDIF.
    IF ls_input-manufacturedate IS NOT INITIAL OR ls_input-zwmsbatch IS NOT INITIAL.
      lv_change = 'X'.
      change_manufacturedate( CHANGING ls_input = ls_input ).
    ENDIF.
    IF ls_input-zwmsbatch IS NOT INITIAL AND ls_input-flag NE 'E'.
      lv_change = 'X'.
      change_zwmsbatch( CHANGING ls_input = ls_input ).
    ENDIF.
    IF ls_input-zvension IS NOT INITIAL AND ls_input-flag NE 'E'.
      lv_change = 'X'.
      change_zvension( CHANGING ls_input = ls_input ).
    ENDIF.

    IF lv_change = 'X'.
      DATA(lv_date) = cl_abap_context_info=>get_system_date( ).
      DATA(lv_time) = cl_abap_context_info=>get_system_time( ).
      DATA(lv_user) = cl_abap_context_info=>get_user_technical_name( ).
      IF ls_input-flag NE 'E'.
        ls_input-flag = 'S'.
      ENDIF.
      ls_input-created_date = lv_date.
      ls_input-created_time = lv_time.
      ls_input-created_by   = lv_user.
      MOVE-CORRESPONDING ls_input TO ls_zztmm_0003.
      TRY.
          ls_zztmm_0003-uuid16 = cl_system_uuid=>create_uuid_x16_static( ).
        CATCH cx_uuid_error.
      ENDTRY.
      MODIFY zztmm_0003 FROM @ls_zztmm_0003.
    ELSE.
      ls_input-flag = 'E'.
      ls_input-msg = '传入所有字段值为空，未修改任何数据'.
    ENDIF.
  ENDMETHOD.


  METHOD save_zztmm_0003.


  ENDMETHOD.
ENDCLASS.
