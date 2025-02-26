CLASS zclui5_odata_util_0006 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS post
      CHANGING ls_input TYPE zzt_pp_005.
    CLASS-METHODS check
      CHANGING ls_input TYPE zzt_pp_005.
PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCLUI5_ODATA_UTIL_0006 IMPLEMENTATION.


  METHOD check.
    IF ls_input-outbillno IS INITIAL.
      ls_input-flag = 'E'.
      ls_input-msg = '【导入计划号】不能为空'.
      RETURN.
    ELSE.
      SELECT SINGLE *
               FROM zzt_pp_005 WITH PRIVILEGED ACCESS
              WHERE outbillno = @ls_input-outbillno
                AND plannedorder NE ''
               INTO @DATA(ls_zzt_pp_005).
      IF sy-subrc = 0.
        ls_input-flag = 'E'.
        ls_input-msg = |导入计划号{ ls_zzt_pp_005-outbillno }已生成对应SAP计划订单{ ls_zzt_pp_005-plannedorder },请勿重复创建！|.
        RETURN.
      ENDIF.
    ENDIF.
    IF ls_input-productionplant IS INITIAL.
      ls_input-flag = 'E'.
      ls_input-msg = '【工厂】不能为空'.
      RETURN.
    ENDIF.
    IF ls_input-product IS INITIAL.
      ls_input-flag = 'E'.
      ls_input-msg = '【产品】不能为空'.
      RETURN.
    ENDIF.
    IF ls_input-plndorderplannedstartdate IS INITIAL.
      ls_input-flag = 'E'.
      ls_input-msg = '【计划开始日期】不能为空'.
      RETURN.
    ENDIF.
    IF ls_input-plndorderplannedenddate IS INITIAL.
      ls_input-flag = 'E'.
      ls_input-msg = '【计划结束日期】不能为空'.
      RETURN.
    ENDIF.
    IF ls_input-plannedtotalqtyinbaseunit IS INITIAL.
      ls_input-flag = 'E'.
      ls_input-msg = '【数量】不能为空'.
      RETURN.
    ENDIF.
  ENDMETHOD.


  METHOD post.
    DATA:lv_material      TYPE matnr,
         lv_totalquantity TYPE i_plannedordertp-totalquantity,
         ls_zzt_pp_005    TYPE zzt_pp_005.
    zcl_com_util=>matnr_zero_in( EXPORTING input = ls_input-product
                                 IMPORTING output =  lv_material ).
    TRY.
        lv_totalquantity = ls_input-plannedtotalqtyinbaseunit.
      CATCH cx_root INTO DATA(o_err).
        ls_input-flag = 'E'.
        ls_input-msg = '数量转换异常'.
        RETURN.
    ENDTRY.
    SELECT SINGLE *
             FROM i_product WITH PRIVILEGED ACCESS
            WHERE product = @lv_material
             INTO @DATA(ls_product).
    IF sy-subrc NE 0.
      ls_input-flag = 'E'.
      ls_input-msg = |物料{ ls_input-product }不存在|.
      RETURN.
    ENDIF.
    MODIFY ENTITIES OF i_plannedordertp ENTITY plannedorder
           CREATE FIELDS ( material mrparea productionplant plndorderplannedstartdate plndorderplannedenddate materialprocurementcategory
                           plannedorderprofile totalquantity baseunit plannedorderisfirm plannedorderlongtext yy1_plannebatch_pla )
           WITH VALUE #( ( %cid = '123'
           material = lv_material
           mrparea  = ls_input-productionplant
           productionplant  = ls_input-productionplant
           plndorderplannedstartdate = ls_input-plndorderplannedstartdate
           plndorderplannedenddate = ls_input-plndorderplannedenddate
           materialprocurementcategory = 'E'
           plannedorderprofile = 'LA'
           totalquantity = lv_totalquantity
           baseunit = ls_product-baseunit
           plannedorderisfirm = 'X'
           plannedorderlongtext = ls_input-plannedorderlongtext
           yy1_plannebatch_pla = ls_input-yy1_plannebatch_pla
        ) )

      MAPPED DATA(ls_create_mapped)
      FAILED DATA(failed)
      REPORTED DATA(reported).

    IF failed IS INITIAL.
      IF ls_create_mapped IS NOT INITIAL.
        ls_input-flag = 'S'.
        READ TABLE ls_create_mapped-plannedorder INTO DATA(ls_plnum) INDEX 1.
        IF sy-subrc = 0.
          ls_input-plannedorder = ls_plnum-plannedorder.
        ENDIF.
      ELSE.
        ls_input-flag = 'E'.
        ls_input-msg = '获取SAP计划订单号失败'.
      ENDIF.
    ELSE.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO FINAL(mtext).
      ls_input-flag = 'E'.
      zcl_com_util=>get_eml_msg( EXPORTING  ls_failed = failed
                                            ls_reported = reported
                                            lv_component = 'PLANNEDORDER'
                                 IMPORTING msg = ls_input-msg     ).
      ls_input-msg = |{ ls_input-msg }'/'{ mtext }|.
      ls_input-msg = |{ ls_input-msg }|.
    ENDIF.

    DATA(lv_date) = cl_abap_context_info=>get_system_date( ).
    DATA(lv_time) = cl_abap_context_info=>get_system_time( ).
    DATA(lv_user) = cl_abap_context_info=>get_user_technical_name( ).
    ls_input-created_date = lv_date.
    ls_input-created_time = lv_time.
    ls_input-created_by   = lv_user.
    MOVE-CORRESPONDING ls_input TO ls_zzt_pp_005.
    TRY.
        ls_zzt_pp_005-uuid16 = cl_system_uuid=>create_uuid_x16_static( ).
      CATCH cx_uuid_error.
    ENDTRY.
    MODIFY zzt_pp_005 FROM @ls_zzt_pp_005.

  ENDMETHOD.
ENDCLASS.
