CLASS zclui5_odata_util_0003 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS save_lld
      IMPORTING ls_input TYPE zzt_ui5_odata.
    CLASS-METHODS check
      CHANGING ls_input TYPE zzs_ui50016_in
               flag     TYPE bapi_mtype
               msg      TYPE bapi_msg.
    CLASS-METHODS set
      CHANGING ls_input TYPE zzs_ui50016_in
               flag     TYPE bapi_mtype
               msg      TYPE bapi_msg.
    CLASS-METHODS check_del
      CHANGING lt_input TYPE zzt_ui50017_in_tab.
    CLASS-METHODS set_del
      CHANGING lt_input TYPE zzt_ui50017_in_tab.
    CLASS-METHODS send_wms
      CHANGING lt_input TYPE zzt_ui50017_in_tab.
    CLASS-METHODS send_wms_ll
      CHANGING ls_input TYPE zzs_ui50017_in.
    CLASS-METHODS send_wms_tl
      CHANGING ls_input TYPE zzs_ui50017_in.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCLUI5_ODATA_UTIL_0003 IMPLEMENTATION.


  METHOD check.
    DATA:lv_matnr              TYPE matnr,
         lv_manufacturingorder TYPE i_manufacturingorder-manufacturingorder,
         lv_storage            TYPE zzt_pp_002_item-storage.
    IF ls_input-zlllx IS INITIAL.
      flag = 'E'.
      msg = |【领料类型】不能为空|.
      RETURN.
    ENDIF.
    IF ls_input-productionplant IS INITIAL.
      flag = 'E'.
      msg = |【工厂】不能为空|.
      RETURN.
    ENDIF.
    CASE ls_input-zlllx.
      WHEN '01'.
        LOOP AT ls_input-itemd INTO DATA(ls_itemd).
          IF ls_itemd-requestedqty IS INITIAL.
            flag = 'E'.
            msg = |生产订单{ ls_itemd-manufacturingorder }预留项目{ ls_itemd-reservationitem }【申请数量】不能为空|.
            RETURN.
          ENDIF.
          IF ls_itemd-zj IS NOT INITIAL.
            zcl_com_util=>matnr_zero_in( EXPORTING input = ls_itemd-zj
                                         IMPORTING output = lv_matnr ).
            SELECT SINGLE *
                     FROM i_product WITH PRIVILEGED ACCESS
                    WHERE product = @lv_matnr
                     INTO @DATA(ls_product).
            IF sy-subrc NE 0.
              flag = 'E'.
              msg = |项目{ ls_itemd-zllitemno }组件{ ls_itemd-zj }在SAP不存在|.
              RETURN.
            ENDIF.
          ENDIF.
        ENDLOOP.
      WHEN '02'.
        LOOP AT ls_input-itemd INTO ls_itemd.
          IF ls_itemd-requestedqty IS INITIAL.
            flag = 'E'.
            msg = |采购订单项目{ ls_itemd-purchaseorder }-{ ls_itemd-purchaseorderitem }预留项目{ ls_itemd-reservationitem }【申请数量】不能为空|.
            RETURN.
          ENDIF.
          IF ls_itemd-zj IS NOT INITIAL.
            zcl_com_util=>matnr_zero_in( EXPORTING input = ls_itemd-zj
                                         IMPORTING output = lv_matnr ).
            SELECT SINGLE *
                     FROM i_product WITH PRIVILEGED ACCESS
                    WHERE product = @lv_matnr
                     INTO @ls_product.
            IF sy-subrc NE 0.
              flag = 'E'.
              msg = |项目{ ls_itemd-zllitemno }组件{ ls_itemd-zj }在SAP不存在|.
              RETURN.
            ENDIF.
          ENDIF.
        ENDLOOP.
      WHEN '03'.
        LOOP AT ls_input-itemd INTO ls_itemd.
          IF ls_itemd-zj IS INITIAL.
            flag = 'E'.
            msg = |项目{ ls_itemd-zllitemno }【组件】不能为空|.
            RETURN.
          ENDIF.
          IF ls_itemd-requestedqty IS INITIAL.
            flag = 'E'.
            msg = |项目{ ls_itemd-zllitemno }【申请数量】不能为空|.
            RETURN.
          ENDIF.
          IF ls_itemd-storagelocation IS INITIAL.
            flag = 'E'.
            msg = |项目{ ls_itemd-zllitemno }【接收地点】不能为空|.
            RETURN.
          ENDIF.
          IF ls_itemd-zj IS NOT INITIAL.
            zcl_com_util=>matnr_zero_in( EXPORTING input = ls_itemd-zj
                                         IMPORTING output = lv_matnr ).
            SELECT SINGLE *
                     FROM i_product WITH PRIVILEGED ACCESS
                    WHERE product = @lv_matnr
                     INTO @ls_product.
            IF sy-subrc NE 0.
              flag = 'E'.
              msg = |项目{ ls_itemd-zllitemno }组件{ ls_itemd-zj }在SAP不存在|.
              RETURN.
            ENDIF.
          ENDIF.
          IF ls_itemd-manufacturingorder IS INITIAL.
*            flag = 'E'.
*            msg = |项目{ ls_itemd-zllitemno }【生产任务单】不能为空|.
*            RETURN.
          ELSE.
            lv_manufacturingorder = ls_itemd-manufacturingorder.
            lv_manufacturingorder = |{ lv_manufacturingorder ALPHA = IN }|.
            SELECT SINGLE *
                       FROM zc_pp002 WITH PRIVILEGED ACCESS
                      WHERE manufacturingorder = @lv_manufacturingorder
                       INTO @DATA(ls_manufacturingorder).
            IF sy-subrc NE 0.
              flag = 'E'.
              msg = |项目{ ls_itemd-zllitemno }生产任务单{ ls_itemd-manufacturingorder }在SAP不存在|.
              RETURN.
            ENDIF.
          ENDIF.

*          SELECT SINGLE *
*                       FROM i_mfgorderoperationcomponent WITH PRIVILEGED ACCESS
*                      WHERE manufacturingorder = @lv_manufacturingorder
*                        AND material = @lv_matnr
*                       INTO @DATA(ls_mfgorderoperationcomponent).
*          IF sy-subrc NE 0.
*            flag = 'E'.
*            msg = |项目{ ls_itemd-zllitemno }生产任务单{ ls_itemd-manufacturingorder }不存在组件{ ls_itemd-zj }|.
*            RETURN.
*          ENDIF.
        ENDLOOP.
      WHEN '04'.
        LOOP AT ls_input-itemd INTO ls_itemd.
          CLEAR:lv_matnr,lv_manufacturingorder.
          IF ls_itemd-zj IS INITIAL.
            flag = 'E'.
            msg = |项目{ ls_itemd-zllitemno }【组件】不能为空|.
            RETURN.
          ENDIF.
          IF ls_itemd-requestedqty IS INITIAL.
            flag = 'E'.
            msg = |项目{ ls_itemd-zllitemno }【申请数量】不能为空|.
            RETURN.
          ENDIF.
          IF ls_itemd-storagelocation IS INITIAL.
            flag = 'E'.
            msg = |项目{ ls_itemd-zllitemno }【接收地点】不能为空|.
            RETURN.
          ENDIF.
          IF ls_itemd-storagelocationto IS INITIAL.
            flag = 'E'.
            msg = |项目{ ls_itemd-zllitemno }【发出地点】不能为空|.
            RETURN.
          ENDIF.
          IF ls_itemd-materialmfadate1 IS INITIAL.
            flag = 'E'.
            msg = |项目{ ls_itemd-zllitemno }【组件生产日期】不能为空|.
            RETURN.
          ENDIF.
          IF ls_itemd-versionnumber IS INITIAL.
            flag = 'E'.
            msg = |项目{ ls_itemd-zllitemno }【组件包材版本】不能为空|.
            RETURN.
          ENDIF.
*          IF ls_itemd-storage IS INITIAL.
*            flag = 'E'.
*            msg = |项目{ ls_itemd-zllitemno }【是否合格】不能为空|.
*            RETURN.
*          ENDIF.
*          IF lv_storage IS INITIAL.
*            lv_storage = ls_itemd-storage.
*          ELSE.
*            IF lv_storage NE ls_itemd-storage.
*              flag = 'E'.
*              msg = |同一单据只能全为【合格】或全为【不合格】|.
*              RETURN.
*            ENDIF.
*          ENDIF.
          IF ls_itemd-zj IS NOT INITIAL.
            zcl_com_util=>matnr_zero_in( EXPORTING input = ls_itemd-zj
                                         IMPORTING output = lv_matnr ).
            SELECT SINGLE *
                     FROM i_product WITH PRIVILEGED ACCESS
                    WHERE product = @lv_matnr
                     INTO @ls_product.
            IF sy-subrc NE 0.
              flag = 'E'.
              msg = |项目{ ls_itemd-zllitemno }组件{ ls_itemd-zj }在SAP不存在|.
              RETURN.
            ENDIF.
          ENDIF.
          IF ls_itemd-manufacturingorder IS INITIAL.
            flag = 'E'.
            msg = |项目{ ls_itemd-zllitemno }【生产任务单】不能为空|.
            RETURN.
          ELSE.
            lv_manufacturingorder = ls_itemd-manufacturingorder.
            lv_manufacturingorder = |{ lv_manufacturingorder ALPHA = IN }|.
            SELECT SINGLE *
                       FROM zc_pp002 WITH PRIVILEGED ACCESS
                      WHERE manufacturingorder = @lv_manufacturingorder
                       INTO @ls_manufacturingorder.
            IF sy-subrc NE 0.
              flag = 'E'.
              msg = |项目{ ls_itemd-zllitemno }生产任务单{ ls_itemd-manufacturingorder }在SAP不存在|.
              RETURN.
            ENDIF.
          ENDIF.

*          SELECT SINGLE *
*                       FROM i_mfgorderoperationcomponent WITH PRIVILEGED ACCESS
*                      WHERE manufacturingorder = @lv_manufacturingorder
*                        AND material = @lv_matnr
*                       INTO @ls_mfgorderoperationcomponent.
*          IF sy-subrc NE 0.
*            flag = 'E'.
*            msg = |项目{ ls_itemd-zllitemno }生产任务单{ ls_itemd-manufacturingorder }不存在组件{ ls_itemd-zj }|.
*            RETURN.
*          ENDIF.

          IF ls_itemd-batch IS INITIAL.
            flag = 'E'.
            msg = |项目{ ls_itemd-zllitemno }【WMS批次】不能为空|.
            RETURN.
          ELSE.
*            SELECT SINGLE a~material,
*                          a~batch AS sapbatch,
*                          a~charcvalue AS batch
*              FROM i_batchcharacteristicvaluetp_2 WITH PRIVILEGED ACCESS AS a
*              INNER JOIN i_clfncharacteristic WITH PRIVILEGED ACCESS AS b
*              ON a~charcinternalid = b~charcinternalid
*             WHERE a~material = @lv_matnr
*               AND b~characteristic = 'Z_WMSBATCH'
*               AND a~charcvalue = @ls_itemd-batch
*              INTO @DATA(ls_batch_valuetp).
*            IF sy-subrc NE 0.
*              flag = 'E'.
*              msg = |项目{ ls_itemd-zllitemno }WMS批次{ ls_itemd-batch }在SAP不存在|.
*              RETURN.
*            ENDIF.
          ENDIF.

        ENDLOOP.
    ENDCASE.

  ENDMETHOD.


  METHOD check_del.

  ENDMETHOD.


  METHOD save_lld.
    DATA:ls_result           TYPE zzs_ui50016_in,
         lt_result_0015      TYPE zzt_ui50017_in_tab,
         ls_zzt_pp_002_head  TYPE zzt_pp_002_head,
         lt_zzt_pp_002_item  TYPE TABLE OF zzt_pp_002_item,
         lt_zzt_pp_002_itemd TYPE TABLE OF zzt_pp_002_itemd.
    IF ls_input-requestcode = 'PP0014'.
      "领料单建创建保存
      CALL METHOD /ui2/cl_json=>deserialize(
        EXPORTING
          json        = ls_input-returnresult
          pretty_name = /ui2/cl_json=>pretty_mode-none "格式化参数，NONE：字段名称大写
        CHANGING
          data        = ls_result ). "数据源CREATE OBJECT json_des.
      MOVE-CORRESPONDING ls_result TO ls_zzt_pp_002_head.
      lt_zzt_pp_002_item = CORRESPONDING #( ls_result-item[] ).
      lt_zzt_pp_002_itemd = CORRESPONDING #( ls_result-itemd[] ).
      IF ls_zzt_pp_002_head IS NOT INITIAL.
        MODIFY zzt_pp_002_head FROM @ls_zzt_pp_002_head.
      ENDIF.
      IF lt_zzt_pp_002_item IS NOT INITIAL.
        MODIFY zzt_pp_002_item FROM TABLE @lt_zzt_pp_002_item.
      ENDIF.
      IF ls_zzt_pp_002_head IS NOT INITIAL.
        MODIFY zzt_pp_002_itemd FROM TABLE @lt_zzt_pp_002_itemd.
      ENDIF.
    ENDIF.
    IF ls_input-requestcode = 'PP0015'.
      "领料单建创建保存
      CALL METHOD /ui2/cl_json=>deserialize(
        EXPORTING
          json        = ls_input-returnresult
          pretty_name = /ui2/cl_json=>pretty_mode-none "格式化参数，NONE：字段名称大写
        CHANGING
          data        = lt_result_0015 ). "数据源CREATE OBJECT json_des.
      LOOP AT lt_result_0015 ASSIGNING FIELD-SYMBOL(<fs_result>).
        IF <fs_result>-yy1_flag = 'S'.
          UPDATE zzt_pp_002_head SET zllzt = @<fs_result>-zllzt WHERE zllno = @<fs_result>-zllno.
        ENDIF.
      ENDLOOP.
    ENDIF.
    IF ls_input-requestcode = 'PP0018'.
      "领料单建创建保存
      CALL METHOD /ui2/cl_json=>deserialize(
        EXPORTING
          json        = ls_input-returnresult
          pretty_name = /ui2/cl_json=>pretty_mode-none "格式化参数，NONE：字段名称大写
        CHANGING
          data        = lt_result_0015 ). "数据源CREATE OBJECT json_des.
      LOOP AT lt_result_0015 ASSIGNING <fs_result>.
        IF <fs_result>-yy1_flag = 'S'.
          UPDATE zzt_pp_002_head SET zllzt = @<fs_result>-zllzt,
                                     wmsno = @<fs_result>-wmsno WHERE zllno = @<fs_result>-zllno.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD send_wms.
    LOOP AT lt_input ASSIGNING FIELD-SYMBOL(<fs_input>).
      CASE <fs_input>-zlllx.
        WHEN '01' OR '02' OR '03'.
          send_wms_ll( CHANGING ls_input = <fs_input> ).
        WHEN '04'.
          send_wms_tl( CHANGING ls_input = <fs_input> ).
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.


  METHOD send_wms_ll.
    TYPES:BEGIN OF ty_item,
            reservationitem1          TYPE string,
            material                  TYPE string,
            productname               TYPE string,
            requestedqty              TYPE zzt_pp_002_item-requestedqty,
            entryunit                 TYPE string,
            material1                 TYPE string,
            productname1              TYPE string,
            manufacturingorder        TYPE string,
            yy1_mfgbatch_ord          TYPE string,
            bomitemdescription        TYPE string,
            materialcomponentsorttext TYPE string,
            purchaseorder             TYPE string,
            purchaseorderitem         TYPE string,
            subcontractor             TYPE string,
            unloadingpointname        TYPE string,
            reserve1                  TYPE string,
          END OF ty_item.
    TYPES BEGIN OF ty_send.
    TYPES:plant           TYPE string,
          reqworkshop     TYPE string,
          storagelocation TYPE string,
          picklist        TYPE string,
          doctype         TYPE string,
          pickliststatus  TYPE string,
          creator         TYPE string,
          creationtime    TYPE string,
          details         TYPE TABLE OF ty_item WITH DEFAULT KEY.
    TYPES END OF ty_send.
    TYPES:BEGIN OF ty_res,
            message   TYPE string,
            errorcode TYPE string,
            key       TYPE string,
            srcobject TYPE string,
          END OF ty_res.

    DATA:ls_send TYPE ty_send,
         ls_res  TYPE ty_res.
    DATA:lt_mapping TYPE /ui2/cl_json=>name_mappings.
    DATA:lv_oref TYPE zzefname,
         lt_ptab TYPE abap_parmbind_tab.
    DATA:lv_numb TYPE zzenumb VALUE 'PP005'.
    DATA:lv_data TYPE string.
    DATA:lv_msgty TYPE bapi_mtype,
         lv_msgtx TYPE bapi_msg,
         lv_resp  TYPE string.

    "获取领料单抬头和行项目信息
    SELECT SINGLE a~*,
                  b~personfullname
             FROM zzt_pp_002_head WITH PRIVILEGED ACCESS AS a
             LEFT OUTER JOIN i_businessuservh  WITH PRIVILEGED ACCESS AS b
             ON a~zcreate_user = b~userid
            WHERE zllno = @ls_input-zllno
             INTO @DATA(ls_head).
    IF sy-subrc NE 0.
      ls_input-yy1_flag = 'E'.
      ls_input-yy1_msg = |未查询到领料单{ ls_input-zllno }|.
      RETURN.
    ENDIF.

    IF ls_head-a-zllzt NE 'NEW'.
      ls_input-yy1_flag = 'E'.
      ls_input-yy1_msg = |只有【创建】状态领料单可下发WMS|.
      RETURN.
    ENDIF.

    SELECT  a~*,
            b~unitofmeasure_e
      FROM zzt_pp_002_item WITH PRIVILEGED ACCESS AS a
      LEFT OUTER JOIN i_unitofmeasure  WITH PRIVILEGED ACCESS AS b
             ON a~requestedqtyunit = b~unitofmeasure
     WHERE zllno = @ls_input-zllno
      INTO TABLE @DATA(lt_item).
    IF sy-subrc NE 0.
      ls_input-yy1_flag = 'E'.
      ls_input-yy1_msg = |未查询到领料单{ ls_input-zllno }行项目信息|.
      RETURN.
    ENDIF.
    READ TABLE lt_item INTO DATA(ls_item_1) INDEX 1.

    ls_send-plant = ls_head-a-productionplant.
    ls_send-reqworkshop = ls_item_1-a-zcj.
    ls_send-storagelocation = ls_item_1-a-storagelocation.
    ls_send-picklist = ls_head-a-zllno.
    ls_send-doctype = ls_head-a-zlllx.
    ls_send-pickliststatus = ls_head-a-zllzt.
    ls_send-creator = ls_head-personfullname.
    ls_send-creationtime = |{ ls_head-a-zcreate_date+0(4) }-{ ls_head-a-zcreate_date+4(2) }-{ ls_head-a-zcreate_date+6(2) }|
    && |T{ ls_head-a-zcreate_time+0(2) }:{ ls_head-a-zcreate_time+2(2) }:{ ls_head-a-zcreate_time+4(2) }.000Z|.

    LOOP AT lt_item ASSIGNING FIELD-SYMBOL(<fs_item>).
      APPEND INITIAL LINE TO ls_send-details ASSIGNING FIELD-SYMBOL(<fs_detail>).
      <fs_detail>-reservationitem1 = <fs_item>-a-zllitemno.
      <fs_detail>-reservationitem1 = |{ <fs_detail>-reservationitem1 ALPHA = OUT }|.
      CONDENSE <fs_detail>-reservationitem1 NO-GAPS.
      <fs_detail>-material = |{ <fs_item>-a-zj ALPHA = OUT }|.
      CONDENSE <fs_detail>-material NO-GAPS.
      <fs_detail>-productname = <fs_item>-a-zjname.
      <fs_detail>-requestedqty = <fs_item>-a-requestedqty.
      <fs_detail>-entryunit = <fs_item>-unitofmeasure_e.
      <fs_detail>-material1 = |{ <fs_item>-a-cp ALPHA = OUT }|.
      CONDENSE <fs_detail>-material1 NO-GAPS.
      <fs_detail>-productname1 = <fs_item>-a-cpname.
      <fs_detail>-manufacturingorder = zcl_com_util=>clear_tszf( <fs_item>-a-manufacturingorder ).
      <fs_detail>-yy1_mfgbatch_ord = <fs_item>-a-yy1_mfgbatch_ord.
      IF ls_head-a-zlllx = '01'.
        <fs_detail>-bomitemdescription = <fs_item>-a-zcy.
        CONDENSE <fs_detail>-bomitemdescription NO-GAPS.
        <fs_detail>-materialcomponentsorttext = <fs_item>-a-zasflag.
      ELSE.
        <fs_detail>-materialcomponentsorttext = '5'.
      ENDIF.
      <fs_detail>-subcontractor = <fs_item>-a-subcontractor.
      <fs_detail>-unloadingpointname = <fs_item>-a-zhlbz.
      <fs_detail>-reserve1 = <fs_item>-a-zbz.
    ENDLOOP.

    lt_mapping = VALUE #(
               ( abap = 'Plant'                      json = 'plant'                         )
               ( abap = 'Reqworkshop'                json = 'reqworkshop'                   )
               ( abap = 'picklist'                   json = 'picklist'                      )
               ( abap = 'Doctype'                    json = 'doctype'                       )
               ( abap = 'pickliststatus'             json = 'pickliststatus'                )
               ( abap = 'creator'                    json = 'creator'                       )
               ( abap = 'CreationTime'               json = 'creationTime'                  )
               ( abap = 'ReservationItem1'           json = 'reservationItem1'              )
               ( abap = 'Material'                   json = 'material'                      )
               ( abap = 'ProductName'                json = 'productName'                   )
               ( abap = 'RequestedQty'               json = 'requestedQty'                  )
               ( abap = 'EntryUnit'                  json = 'entryUnit'                     )
               ( abap = 'StorageLocation'            json = 'storageLocation'               )
               ( abap = 'Material1'                  json = 'material1'                     )
               ( abap = 'ProductName1'               json = 'productName1'                  )
               ( abap = 'ManufacturingOrder'         json = 'manufacturingOrder'            )
               ( abap = 'YY1_mfgbatch_ORD'           json = 'yY1_mfgbatch_ORD'              )
               ( abap = 'BOMItemDescription'         json = 'bomItemDescription'            )
               ( abap = 'MaterialComponentSortText'  json = 'materialComponentSortText'     )
               ( abap = 'PurchaseOrder'              json = 'purchaseOrder'                 )
               ( abap = 'PurchaseOrderItem'          json = 'purchaseOrderItem'             )
               ( abap = 'Subcontractor'              json = 'subcontractor'                 )
               ( abap = 'UnloadingPointName'         json = 'unloadingPointName'            )
               ( abap = 'reserve1'                   json = 'reserve1'                      )
               ).

    "获取调用类
    SELECT SINGLE zzcname
      FROM zr_vt_rest_conf
     WHERE zznumb = @lv_numb
      AND zzisst = 'X'
      INTO @lv_oref.
    IF lv_oref IS INITIAL.
      ls_input-yy1_flag = 'E'.
      ls_input-yy1_msg = |下发WMS失败，未配置激活接口PP005|.
      RETURN.
    ENDIF.

    "传入数据转JSON
    lv_data = /ui2/cl_json=>serialize(
          data          = ls_send
          compress      = abap_true
          pretty_name   = /ui2/cl_json=>pretty_mode-camel_case
          name_mappings = lt_mapping ).
*&--调用实例化接口
    DATA:lo_oref TYPE REF TO object.

    lt_ptab = VALUE #( ( name  = 'IV_NUMB' kind  = cl_abap_objectdescr=>exporting value = REF #( lv_numb ) ) ).
    TRY .
        CREATE OBJECT lo_oref TYPE (lv_oref) PARAMETER-TABLE lt_ptab.
        CALL METHOD lo_oref->('OUTBOUND')
          EXPORTING
            iv_data  = lv_data
          CHANGING
            ev_resp  = lv_resp
            ev_msgty = lv_msgty
            ev_msgtx = lv_msgtx.
      CATCH cx_root INTO DATA(lr_root).

    ENDTRY.

    IF lv_msgty = 'S'.
      ls_input-yy1_flag = 'S'.
      ls_input-yy1_msg = |下发WMS成功|.
      ls_input-zllzt = 'RELEASE'.
      /ui2/cl_json=>deserialize( EXPORTING json        = lv_resp
                                     pretty_name = /ui2/cl_json=>pretty_mode-none
                           CHANGING  data        = ls_res ).
      ls_input-wmsno = ls_res-key.
    ELSE.
      ls_input-yy1_flag = 'E'.
      ls_input-yy1_msg = |下发WMS失败:{ lv_msgtx }|.
      RETURN.
    ENDIF.
  ENDMETHOD.


  METHOD send_wms_tl.
    TYPES:BEGIN OF ty_item,
            reservationitem1   TYPE string,
            material           TYPE string,
            productname        TYPE string,
            requestedqty       TYPE zzt_pp_002_item-requestedqty,
            entryunit          TYPE string,
            material1          TYPE string,
            batch              TYPE string,
            materialmfgdate    TYPE string,
            materialmfgdate1   TYPE string,
            versionnumber      TYPE string,
            manufacturingorder TYPE string,
            storage            TYPE string,
          END OF ty_item.
    TYPES BEGIN OF ty_send.
    TYPES:plant           TYPE string,
          reqworkshop     TYPE string,
          storagelocatio2 TYPE string, "发出库位
          storagelocatio3 TYPE string, "接收库位
          picklist        TYPE string,
          doctype         TYPE string,
          pickliststatus  TYPE string,
          creator         TYPE string,
          creationtime    TYPE string,
          details         TYPE TABLE OF ty_item WITH DEFAULT KEY.
    TYPES END OF ty_send.
    TYPES:BEGIN OF ty_res,
            message   TYPE string,
            errorcode TYPE string,
            key       TYPE string,
            srcobject TYPE string,
          END OF ty_res.

    DATA:ls_send TYPE ty_send,
         ls_res  TYPE ty_res.
    DATA:lt_mapping TYPE /ui2/cl_json=>name_mappings.
    DATA:lv_oref TYPE zzefname,
         lt_ptab TYPE abap_parmbind_tab.
    DATA:lv_numb TYPE zzenumb VALUE 'PP006'.
    DATA:lv_data TYPE string.
    DATA:lv_msgty TYPE bapi_mtype,
         lv_msgtx TYPE bapi_msg,
         lv_resp  TYPE string.

    "获取领料单抬头和行项目信息
    SELECT SINGLE a~*,
                  b~personfullname
             FROM zzt_pp_002_head WITH PRIVILEGED ACCESS AS a
             LEFT OUTER JOIN i_businessuservh  WITH PRIVILEGED ACCESS AS b
             ON a~zcreate_user = b~userid
            WHERE zllno = @ls_input-zllno
             INTO @DATA(ls_head).
    IF sy-subrc NE 0.
      ls_input-yy1_flag = 'E'.
      ls_input-yy1_msg = |未查询到退料单{ ls_input-zllno }|.
      RETURN.
    ENDIF.

    IF ls_head-a-zllzt NE 'NEW'.
      ls_input-yy1_flag = 'E'.
      ls_input-yy1_msg = |只有【创建】状态退料单可下发WMS|.
      RETURN.
    ENDIF.

    SELECT  a~*,
            b~unitofmeasure_e
      FROM zzt_pp_002_item WITH PRIVILEGED ACCESS AS a
      LEFT OUTER JOIN i_unitofmeasure  WITH PRIVILEGED ACCESS AS b
             ON a~requestedqtyunit = b~unitofmeasure
     WHERE zllno = @ls_input-zllno
      INTO TABLE @DATA(lt_item).
    IF sy-subrc NE 0.
      ls_input-yy1_flag = 'E'.
      ls_input-yy1_msg = |未查询到领料单{ ls_input-zllno }行项目信息|.
      RETURN.
    ENDIF.
    READ TABLE lt_item INTO DATA(ls_item_1) INDEX 1.

    ls_send-plant = ls_head-a-productionplant.
    ls_send-reqworkshop = ls_item_1-a-zcj.
    ls_send-storagelocatio2 = ls_item_1-a-storagelocationto.
    ls_send-storagelocatio3 = ls_item_1-a-storagelocation.
    ls_send-picklist = ls_head-a-zllno.
    ls_send-doctype = ls_head-a-zlllx.
    ls_send-pickliststatus = ls_head-a-zllzt.
    ls_send-creator = ls_head-personfullname.
    ls_send-creationtime = |{ ls_head-a-zcreate_date+0(4) }-{ ls_head-a-zcreate_date+4(2) }-{ ls_head-a-zcreate_date+6(2) }|
    && |T{ ls_head-a-zcreate_time+0(2) }:{ ls_head-a-zcreate_time+2(2) }:{ ls_head-a-zcreate_time+4(2) }.000Z|.

    LOOP AT lt_item ASSIGNING FIELD-SYMBOL(<fs_item>).
      APPEND INITIAL LINE TO ls_send-details ASSIGNING FIELD-SYMBOL(<fs_detail>).
      <fs_detail>-reservationitem1 = <fs_item>-a-zllitemno.
      <fs_detail>-reservationitem1 = |{ <fs_detail>-reservationitem1 ALPHA = OUT }|.
      CONDENSE <fs_detail>-reservationitem1 NO-GAPS.
      <fs_detail>-material = |{ <fs_item>-a-zj ALPHA = OUT }|.
      CONDENSE <fs_detail>-material NO-GAPS.
      <fs_detail>-productname = <fs_item>-a-zjname.
      <fs_detail>-requestedqty = <fs_item>-a-requestedqty.
      <fs_detail>-entryunit = <fs_item>-unitofmeasure_e.
      <fs_detail>-material1 = |{ <fs_item>-a-cp ALPHA = OUT }|.
      CONDENSE <fs_detail>-material1 NO-GAPS.
      <fs_detail>-batch = <fs_item>-a-batch.
      IF <fs_item>-a-materialmfadate1 IS NOT INITIAL.
        <fs_detail>-materialmfgdate1 = |{ <fs_item>-a-materialmfadate1+0(4) }-{ <fs_item>-a-materialmfadate1+4(2) }-{ <fs_item>-a-materialmfadate1+6(2) }|
    && |T00:00:00.000Z|.
*      ELSE.
*        <fs_detail>-materialmfgdate1 = |9999-12-31T00:00:00.000Z|.
      ENDIF.
      <fs_detail>-versionnumber = <fs_item>-a-versionnumber.
*      <fs_detail>-storage = <fs_item>-a-storage.
*      SELECT SINGLE a~material,
*                    a~batch AS sapbatch,
*                    a~charcvalue AS batch,
*                    c~shelflifeexpirationdate AS materialmfgdate1,
*                    c~manufacturedate AS materialmfgdate
*        FROM i_batchcharacteristicvaluetp_2 WITH PRIVILEGED ACCESS AS a
*        INNER JOIN i_clfncharacteristic WITH PRIVILEGED ACCESS AS b
*        ON a~charcinternalid = b~charcinternalid
*        INNER JOIN i_batchdistinct WITH PRIVILEGED ACCESS AS c
*        ON a~material = c~material AND a~batch = c~batch
*       WHERE a~material = @<fs_item>-a-zj
*         AND b~characteristic = 'Z_WMSBATCH'
*         AND a~charcvalue = @<fs_item>-a-batch
*        INTO @DATA(ls_batch_valuetp).
*      IF sy-subrc = 0.
*        <fs_detail>-materialmfgdate1 = |{ ls_batch_valuetp-materialmfgdate1+0(4) }-{ ls_batch_valuetp-materialmfgdate1+4(2) }-{ ls_batch_valuetp-materialmfgdate1+6(2) }|
*    && |T00:00:00.000Z|.
*        SELECT SINGLE a~material,
*               a~batch AS sapbatch,
*               a~charcvalue AS versionnumber
*               FROM i_batchcharacteristicvaluetp_2 WITH PRIVILEGED ACCESS AS a
*               INNER JOIN i_clfncharacteristic WITH PRIVILEGED ACCESS AS b
*               ON a~charcinternalid = b~charcinternalid
*              WHERE a~material = @<fs_item>-a-zj
*                AND b~characteristic = 'Z_BANBEN'
*                AND a~batch = @ls_batch_valuetp-sapbatch
*                INTO @DATA(ls_batch_versionnumbertp).
*        IF sy-subrc = 0.
*          <fs_detail>-versionnumber = ls_batch_versionnumbertp-versionnumber.
*        ENDIF.
*      ENDIF.
*      <fs_detail>-manufacturingorder = <fs_item>-a-manufacturingorder.
      <fs_detail>-manufacturingorder = zcl_com_util=>clear_tszf( <fs_item>-a-manufacturingorder ).
      IF <fs_item>-a-yy1_mfgdate IS NOT INITIAL.
        <fs_detail>-materialmfgdate = |{ <fs_item>-a-yy1_mfgdate+0(4) }-{ <fs_item>-a-yy1_mfgdate+4(2) }-{ <fs_item>-a-yy1_mfgdate+6(2) }|
      && |T00:00:00.000Z|.
      ENDIF.

    ENDLOOP.

    lt_mapping = VALUE #(
               ( abap = 'Plant'                      json = 'plant'                         )
               ( abap = 'Reqworkshop'                json = 'reqworkshop'                   )
               ( abap = 'picklist'                   json = 'picklist'                      )
               ( abap = 'Doctype'                    json = 'doctype'                       )
               ( abap = 'pickliststatus'             json = 'pickliststatus'                )
               ( abap = 'creator'                    json = 'creator'                       )
               ( abap = 'CreationTime'               json = 'creationTime'                  )
               ( abap = 'ReservationItem1'           json = 'reservationItem1'              )
               ( abap = 'Material'                   json = 'material'                      )
               ( abap = 'ProductName'                json = 'productName'                   )
               ( abap = 'RequestedQty'               json = 'requestedQty'                  )
               ( abap = 'EntryUnit'                  json = 'entryUnit'                     )
               ( abap = 'storageLocatio2'            json = 'storageLocatio2'               )
               ( abap = 'storageLocatio3'            json = 'storageLocatio3'               )
               ( abap = 'batch'                      json = 'batch'                         )
               ( abap = 'materialmfgdate'            json = 'materialmfgdate'               )
               ( abap = 'materialmfgdate1'           json = 'materialmfgdate1'              )
               ( abap = 'versionnumber'              json = 'versionnumber'              )
               ).

    "获取调用类
    SELECT SINGLE zzcname
      FROM zr_vt_rest_conf
     WHERE zznumb = @lv_numb
      AND zzisst = 'X'
      INTO @lv_oref.
    IF lv_oref IS INITIAL.
      ls_input-yy1_flag = 'E'.
      ls_input-yy1_msg = |下发WMS失败，未配置接口PP005|.
      RETURN.
    ENDIF.

    "传入数据转JSON
    lv_data = /ui2/cl_json=>serialize(
          data          = ls_send
          compress      = abap_true
          pretty_name   = /ui2/cl_json=>pretty_mode-camel_case
          name_mappings = lt_mapping ).
*&--调用实例化接口
    DATA:lo_oref TYPE REF TO object.

    lt_ptab = VALUE #( ( name  = 'IV_NUMB' kind  = cl_abap_objectdescr=>exporting value = REF #( lv_numb ) ) ).
    TRY .
        CREATE OBJECT lo_oref TYPE (lv_oref) PARAMETER-TABLE lt_ptab.
        CALL METHOD lo_oref->('OUTBOUND')
          EXPORTING
            iv_data  = lv_data
          CHANGING
            ev_resp  = lv_resp
            ev_msgty = lv_msgty
            ev_msgtx = lv_msgtx.
      CATCH cx_root INTO DATA(lr_root).

    ENDTRY.

    IF lv_msgty = 'S'.
      ls_input-yy1_flag = 'S'.
      ls_input-yy1_msg = |下发WMS成功|.
      ls_input-zllzt = 'RELEASE'.
      /ui2/cl_json=>deserialize( EXPORTING json        = lv_resp
                                     pretty_name = /ui2/cl_json=>pretty_mode-none
                           CHANGING  data        = ls_res ).
      ls_input-wmsno = ls_res-key.
    ELSE.
      ls_input-yy1_flag = 'E'.
      ls_input-yy1_msg = |下发WMS失败:{ lv_msgtx }|.
      RETURN.
    ENDIF.
  ENDMETHOD.


  METHOD set.
    DATA:lt_item      TYPE TABLE OF zzt_pp_002_item,
         lv_qz        TYPE char10,
         lv_zllno     TYPE zezllno,
         lv_zllitemno TYPE i,
         lv_cp        TYPE string,
         lv_cpname    TYPE string.
    CASE ls_input-zlllx.
      WHEN '01'.
        "按物料+库位+AS标识汇总
        DATA(lt_collect) = ls_input-itemd.
        SORT lt_collect BY zj storagelocation zasflag.
        DELETE ADJACENT DUPLICATES FROM lt_collect COMPARING zj storagelocation zasflag.
        LOOP AT lt_collect INTO DATA(ls_collect).
          APPEND INITIAL LINE TO lt_item ASSIGNING FIELD-SYMBOL(<fs_item>).
          MOVE-CORRESPONDING ls_collect TO <fs_item>.
          CLEAR:<fs_item>-requestedqty,<fs_item>-manufacturingorder,<fs_item>-yy1_mfgbatch_ord,<fs_item>-zcy.
          CLEAR:<fs_item>-cp,<fs_item>-cpname,<fs_item>-zhlbz,<fs_item>-zbz.
          CLEAR:lv_cp,lv_cpname.
          LOOP AT ls_input-itemd INTO DATA(ls_itemd) WHERE zj = ls_collect-zj
                                                       AND storagelocation =  ls_collect-storagelocation
                                                       AND zasflag = ls_collect-zasflag.       .
            <fs_item>-requestedqty = <fs_item>-requestedqty + ls_itemd-requestedqty.
            <fs_item>-zcy = <fs_item>-zcy + ls_itemd-zcy.
            IF <fs_item>-manufacturingorder IS INITIAL.
              <fs_item>-manufacturingorder = ls_itemd-manufacturingorder.
            ELSE.
              <fs_item>-manufacturingorder = |{ <fs_item>-manufacturingorder };{ ls_itemd-manufacturingorder }|.
            ENDIF.
            IF <fs_item>-yy1_mfgbatch_ord IS INITIAL.
              <fs_item>-yy1_mfgbatch_ord = ls_itemd-yy1_mfgbatch_ord.
            ELSE.
              <fs_item>-yy1_mfgbatch_ord = |{ <fs_item>-yy1_mfgbatch_ord };{ ls_itemd-yy1_mfgbatch_ord }|.
            ENDIF.
            IF <fs_item>-cp IS INITIAL.
              <fs_item>-cp = ls_itemd-cp.
            ELSE.
              lv_cp = |*{ ls_itemd-cp }*|.
              IF <fs_item>-cp CP lv_cp.

              ELSE.
                <fs_item>-cp = |{ <fs_item>-cp };{ ls_itemd-cp }|.
              ENDIF.
            ENDIF.
            IF <fs_item>-cpname IS INITIAL.
              <fs_item>-cpname = ls_itemd-cpname.
            ELSE.
              lv_cpname = |*{ ls_itemd-cpname }*|.
              IF <fs_item>-cpname CP lv_cpname.

              ELSE.
                <fs_item>-cpname = |{ <fs_item>-cpname };{ ls_itemd-cpname }|.
              ENDIF.
            ENDIF.
            IF ls_itemd-zhlbz IS NOT INITIAL.
              IF <fs_item>-zhlbz IS INITIAL.
                <fs_item>-zhlbz = |{ ls_itemd-manufacturingorder }-{ ls_itemd-zhlbz }|.
              ELSE.
                <fs_item>-zhlbz = |{ <fs_item>-zhlbz };{ ls_itemd-manufacturingorder }-{ ls_itemd-zhlbz }|.
              ENDIF.
            ENDIF.
            IF ls_itemd-zbz IS NOT INITIAL.
              IF <fs_item>-zbz IS INITIAL.
                <fs_item>-zbz = |{ ls_itemd-manufacturingorder }-{ ls_itemd-zbz }|.
              ELSE.
                <fs_item>-zbz = |{ <fs_item>-zbz };{ ls_itemd-manufacturingorder }-{ ls_itemd-zbz }|.
              ENDIF.
            ENDIF.
          ENDLOOP.
        ENDLOOP.
      WHEN OTHERS.
        lt_item[] = CORRESPONDING #( ls_input-itemd[] ).
    ENDCASE.
    ls_input-item[] = CORRESPONDING #( lt_item[] ).
    LOOP AT ls_input-itemd ASSIGNING FIELD-SYMBOL(<fs_itemd>).
      zcl_com_util=>matnr_zero_in( EXPORTING input = <fs_itemd>-cp
                                   IMPORTING output = <fs_itemd>-cp ).
      zcl_com_util=>matnr_zero_in( EXPORTING input = <fs_itemd>-zj
                                   IMPORTING output = <fs_itemd>-zj ).
    ENDLOOP.
    LOOP AT ls_input-item ASSIGNING <fs_item>.
      zcl_com_util=>matnr_zero_in( EXPORTING input = <fs_item>-cp
                                   IMPORTING output = <fs_item>-cp ).
      zcl_com_util=>matnr_zero_in( EXPORTING input = <fs_item>-zj
                                   IMPORTING output = <fs_item>-zj ).
    ENDLOOP.
    CASE ls_input-zlllx.
      WHEN '01'.
        lv_qz = 'SCLL'.
      WHEN '02'.
        lv_qz = 'WWFL'.
      WHEN '03'.
        lv_qz = 'QTLL'.
      WHEN '04'.
        lv_qz = 'QTTL'.
    ENDCASE.
    TRY.
        CALL METHOD cl_numberrange_runtime=>number_get
          EXPORTING
            nr_range_nr = '01'
            object      = 'ZPP001'
          IMPORTING
            number      = DATA(lv_number)
            returncode  = DATA(lv_rcode).
        IF lv_number IS NOT INITIAL.
          lv_zllno = |{ lv_qz }{ lv_number ALPHA = OUT }|.
          DATA(lv_date) = cl_abap_context_info=>get_system_date( ).
          DATA(lv_time) = cl_abap_context_info=>get_system_time( ).
          DATA(lv_user) = cl_abap_context_info=>get_user_technical_name( ).
          ls_input-zllno = lv_zllno.
          ls_input-zllzt = 'NEW'.
          ls_input-zcreate_date = lv_date.
          ls_input-zcreate_time = lv_time.
          ls_input-zcreate_user = lv_user.
          LOOP AT ls_input-item ASSIGNING <fs_item>.
            <fs_item>-zllno = lv_zllno.
            IF ls_input-zlllx = '01' OR ls_input-zlllx = '02'.
              lv_zllitemno = lv_zllitemno + 1.
              <fs_item>-zllitemno = lv_zllitemno.
              CONDENSE <fs_item>-zllitemno NO-GAPS.
            ENDIF.
          ENDLOOP.
          LOOP AT ls_input-itemd ASSIGNING <fs_itemd>.
            <fs_itemd>-zllno = lv_zllno.
            IF ls_input-zlllx = '01' OR ls_input-zlllx = '02'.
              READ TABLE ls_input-item INTO DATA(ls_item) WITH KEY zj = <fs_itemd>-zj
                                                                   storagelocation =  <fs_itemd>-storagelocation.
              IF sy-subrc = 0.
                <fs_itemd>-zllitemno = ls_item-zllitemno.
              ENDIF.
            ENDIF.
          ENDLOOP.
          flag = 'S'.
        ELSE.
          flag = 'E'.
          msg = '获取领料单自动编号失败，请联系管理员'.
          RETURN.
        ENDIF.
      CATCH cx_root INTO DATA(cx_root).
        flag = 'E'.
        msg = '获取领料单自动编号异常:' && cx_root->get_longtext( ).
        RETURN.
    ENDTRY.
  ENDMETHOD.


  METHOD set_del.
    LOOP AT lt_input ASSIGNING FIELD-SYMBOL(<fs_input>).
      IF <fs_input>-zllzt NE 'NEW'.
        <fs_input>-yy1_flag = 'E'.
        <fs_input>-yy1_msg  = |非【创建】状态领料单无法删除|.
        CONTINUE.
      ENDIF.
      <fs_input>-zllzt = 'DELETE'.
      <fs_input>-yy1_flag = 'S'.
      <fs_input>-yy1_msg  = |删除成功|.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
