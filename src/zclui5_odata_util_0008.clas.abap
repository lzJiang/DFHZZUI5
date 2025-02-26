CLASS zclui5_odata_util_0008 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS post
      CHANGING lt_input TYPE zztsd_0003_tab
               flag     TYPE bapi_mtype
               msg      TYPE bapi_msg.
    CLASS-METHODS check
      CHANGING lt_input TYPE zztsd_0003_tab
               flag     TYPE bapi_mtype
               msg      TYPE bapi_msg.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCLUI5_ODATA_UTIL_0008 IMPLEMENTATION.


  METHOD check.
    DATA:lv_material TYPE matnr.
    "检查
    LOOP AT lt_input[] ASSIGNING FIELD-SYMBOL(<fs_input>).
      CLEAR:<fs_input>-flag,<fs_input>-msg.
      DATA(lv_tabix) = sy-tabix.
      IF <fs_input>-purchaseorderbycustomer IS INITIAL.
        <fs_input>-flag = 'E'.
        <fs_input>-msg  = |【外围系统订单编号】不能为空|.
        flag = 'E'.
        msg = |第{ lv_tabix }行【外围系统订单编号】不能为空|.
        CONTINUE.
      ELSE.
        SELECT SINGLE *
         FROM i_salesdocument WITH PRIVILEGED ACCESS
        WHERE purchaseorderbycustomer = @<fs_input>-purchaseorderbycustomer
          AND salesdocumenttype NE 'L2'
        INTO @DATA(ls_salesorder).
        IF sy-subrc = 0 AND <fs_input>-salesordertype = 'OR' .
          <fs_input>-flag = 'E'.
          <fs_input>-msg  = |'外围系统单号'{ <fs_input>-purchaseorderbycustomer }'已存在对应SAP销售订单'{ ls_salesorder-salesdocument }',请勿重复创建' |.
          flag = 'E'.
          CONTINUE.
        ENDIF.
      ENDIF.
      IF <fs_input>-underlyingpurchaseorderitem IS INITIAL.
        <fs_input>-flag = 'E'.
        <fs_input>-msg  = '【外围系统订单行号】不能为空'.
        flag = 'E'.
        CONTINUE.
      ENDIF.
      IF  <fs_input>-salesordertype NE 'OR'
      AND <fs_input>-salesordertype NE 'CBAR'.
        <fs_input>-flag = 'E'.
        <fs_input>-msg  = '【订单类型】传入值不支持，请检查'.
        flag = 'E'.
        CONTINUE.
      ENDIF.
      IF <fs_input>-soldtoparty IS INITIAL.
        <fs_input>-flag = 'E'.
        <fs_input>-msg  = '【客户】不能为空'.
        flag = 'E'.
        CONTINUE.
      ENDIF.
      IF <fs_input>-salesorganization IS INITIAL.
        <fs_input>-flag = 'E'.
        <fs_input>-msg  = '【销售组织】不能为空'.
        flag = 'E'.
        CONTINUE.
      ENDIF.
      IF <fs_input>-material IS INITIAL.
        <fs_input>-flag = 'E'.
        <fs_input>-msg  = '【产品编号】不能为空'.
        flag = 'E'.
        CONTINUE.
      ENDIF.
      IF <fs_input>-productionplant IS INITIAL.
        <fs_input>-flag = 'E'.
        <fs_input>-msg  = '【货主】不能为空'.
        flag = 'E'.
        CONTINUE.
      ENDIF.
      zcl_com_util=>matnr_zero_in( EXPORTING input = <fs_input>-material
                                   IMPORTING output =  lv_material ).
      SELECT SINGLE *
               FROM i_productplantbasic WITH PRIVILEGED ACCESS
              WHERE product = @lv_material
                AND plant = @<fs_input>-productionplant
              INTO @DATA(ls_productplant).
      IF ls_productplant-isbatchmanagementrequired IS NOT INITIAL.
        IF <fs_input>-batchwms IS INITIAL.
          <fs_input>-flag = 'E'.
          <fs_input>-msg  = '【WMS批次编号】不能为空'.
          flag = 'E'.
          CONTINUE.
        ELSE.
          SELECT SINGLE a~material,
                           a~batch
            FROM i_batchcharacteristicvaluetp_2 WITH PRIVILEGED ACCESS AS a
            JOIN i_clfncharacteristic WITH PRIVILEGED ACCESS AS b ON a~charcinternalid = b~charcinternalid
           WHERE material = @lv_material
             AND b~characteristic = 'Z_WMSBATCH'
             AND a~charcvalue = @<fs_input>-batchwms
            INTO @DATA(ls_valuetp).
          IF sy-subrc = 0.
            <fs_input>-batch = ls_valuetp-batch.
          ELSE.
            <fs_input>-flag = 'E'.
            <fs_input>-msg  = |物料{ <fs_input>-material }WMS批次{ <fs_input>-batchwms }未找到对应的SAP批次|.
            flag = 'E'.
            CONTINUE.
          ENDIF.
        ENDIF.
      ENDIF.
      IF <fs_input>-requestedquantity IS INITIAL.
        <fs_input>-flag = 'E'.
        <fs_input>-msg  = '【数量】不能为空'.
        flag = 'E'.
        CONTINUE.
      ELSE.
        IF cl_abap_matcher=>matches( pattern = '^(0|[1-9]\d*)(\.\d{1,3})?$'
                                        text = <fs_input>-requestedquantity ) = abap_true.

        ELSE.
          <fs_input>-flag = 'E'.
          <fs_input>-msg  = '输入的【数量】不是最多3位小数的数字，请检查'.
          flag = 'E'.
          CONTINUE.
        ENDIF.
      ENDIF.
      IF <fs_input>-requestedquantityunit IS INITIAL.
        <fs_input>-flag = 'E'.
        <fs_input>-msg  = '【销售单位】不能为空'.
        flag = 'E'.
        CONTINUE.
      ELSE.
        SELECT SINGLE *
                 FROM i_unitofmeasure WITH PRIVILEGED ACCESS
                WHERE unitofmeasure_e = @<fs_input>-requestedquantityunit
                 INTO @DATA(ls_unitofmeasure).
        IF sy-subrc NE 0.
          <fs_input>-flag = 'E'.
          <fs_input>-msg  = |【销售单位】{ <fs_input>-requestedquantityunit }在SAP不存在|.
          flag = 'E'.
          CONTINUE.
        ELSE.
          IF <fs_input>-salesorganization NE <fs_input>-productionplant.
            "跨公司销售
            SELECT SINGLE *
                     FROM zztsd_0002
                    WHERE werks = @<fs_input>-productionplant
                      AND matnr = @lv_material
                      AND meins = @ls_unitofmeasure-unitofmeasure
                      AND price > 0
                     INTO @DATA(ls_zztsd_0002).
            IF sy-subrc NE 0.
              <fs_input>-flag = 'E'.
              <fs_input>-msg  = |{ <fs_input>-salesorganization }与{ <fs_input>-productionplant }跨公司销售公司间价格未维护|.
              flag = 'E'.
              CONTINUE.
            ENDIF.
          ENDIF.
        ENDIF.

      ENDIF.
      IF <fs_input>-dj IS INITIAL.
*        <fs_input>-flag = 'E'.
*        <fs_input>-msg  = '【单价】不能为空'.
*        flag = 'E'.
*        CONTINUE.
      ELSE.
        IF cl_abap_matcher=>matches( pattern = '^(0|[1-9]\d*)(\.\d{1,2})?$'
                                                      text = <fs_input>-dj ) = abap_true.

        ELSE.
          <fs_input>-flag = 'E'.
          <fs_input>-msg  = '输入的【单价】不是最多2位小数的数字，请检查'.
          flag = 'E'.
          CONTINUE.
        ENDIF.
      ENDIF.
      IF <fs_input>-conditionratevalue IS INITIAL.
        <fs_input>-flag = 'E'.
        <fs_input>-msg  = '【含税总价】不能为空'.
        flag = 'E'.
        CONTINUE.
      ELSE.
        IF cl_abap_matcher=>matches( pattern = '^(0|[1-9]\d*)(\.\d{1,2})?$'
                                        text = <fs_input>-conditionratevalue ) = abap_true.

        ELSE.
          <fs_input>-flag = 'E'.
          <fs_input>-msg  = '输入的【含税总价】不是最多2位小数的数字，请检查'.
          flag = 'E'.
          CONTINUE.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD post.
    DATA:lv_json TYPE string,
         fsysid  TYPE zzefsysid.
    DATA:lv_salesorder TYPE vbeln.
    DATA:lt_mapping TYPE /ui2/cl_json=>name_mappings.
    TYPES:BEGIN OF ty_heads,
            salesorder        TYPE string,
            customerreturn    TYPE string,
            creditmemorequest TYPE string,
            debitmemorequest  TYPE string,
          END OF ty_heads,
          BEGIN OF ty_ress,
            d TYPE ty_heads,
          END OF  ty_ress.
    TYPES BEGIN OF ty_item_return_price.
    TYPES: condition_type       TYPE string,
           condition_rate_value TYPE i_salesdocument-totalnetamount.
    TYPES END OF ty_item_return_price.
    TYPES BEGIN OF ty_item_return.
    TYPES: underlying_purchase_order_item TYPE string,
           material                       TYPE string,
           requested_quantity             TYPE menge_d,
           requested_quantity_unit        TYPE string,
           production_plant               TYPE string,
           material_by_customer           TYPE string,
           reference_sd_document          TYPE string,
           reference_sd_document_item     TYPE string,
           sales_order_item_category      TYPE string,
           purchase_order_by_customer     TYPE string,
           storage_location               TYPE string,
           batch                          TYPE string,
           custretitmfollowupactivity     TYPE string,
           nextfllwupactivityformatlinsp  TYPE string,
           returnsrefundtype              TYPE string,
           returnsrefundprocgmode         TYPE string,
           returnsrefundcode              TYPE string,
           returnsinspectioncode          TYPE string,
           retsinspiscrtedautomly         TYPE abap_bool,
           _itempricingelement            TYPE TABLE OF ty_item_return_price WITH DEFAULT KEY.
    TYPES END OF ty_item_return.
    TYPES BEGIN OF ty_return.
    TYPES: sales_order_type           TYPE string,
           sales_organization         TYPE string,
           distribution_channel       TYPE string,
           sddocumentreason           TYPE string,
           organization_division      TYPE string,
           transaction_currency       TYPE string,
           sold_to_party              TYPE string,
           customer_payment_terms     TYPE string,
           purchase_order_by_customer TYPE string,
           reference_sd_document      TYPE string,
           sales_district             TYPE string,
           _item                      TYPE TABLE OF ty_item_return WITH DEFAULT KEY.
    TYPES END OF ty_return.


    DATA:ls_req TYPE zzs_sdi002_0001_in.
    DATA:ls_return TYPE ty_return.
    DATA:ls_ress  TYPE ty_ress,
         ls_ress4 TYPE ty_heads.
    DATA:lv_flag_0                     TYPE char1,
         lv_purchase_order_by_customer TYPE i_salesdocument-purchaseorderbycustomer.
    DATA:lt_zztsd_0003 TYPE TABLE OF zztsd_0003,
         ls_zztmm_0003 TYPE zztmm_0003,
         lv_storelocation TYPE char4.

    READ TABLE lt_input INTO DATA(ls_input_1) INDEX 1.
    ls_req-sales_order_type = ls_input_1-salesordertype.
    ls_req-sales_organization = ls_input_1-salesorganization.
    IF ls_req-sales_organization CP '3*'.
      fsysid = 'XYW'.
      lv_storelocation = '3000'.
    ELSE.
      fsysid = 'JXC'.
      lv_storelocation = '1000'.
    ENDIF.
    ls_req-purchase_order_by_customer = ls_input_1-purchaseorderbycustomer.
    ls_req-customer_payment_terms = 'N000'.
    ls_req-sales_order_type = ls_input_1-salesordertype.
    ls_req-sold_to_party = ls_input_1-soldtoparty.
    LOOP AT lt_input INTO DATA(ls_input).
      APPEND INITIAL LINE TO ls_req-to_item-results ASSIGNING FIELD-SYMBOL(<fs_result>).
      <fs_result>-material_by_customer = ls_input-underlyingpurchaseorderitem.
      <fs_result>-material = ls_input-material.
      <fs_result>-requested_quantity = ls_input-requestedquantity.
      <fs_result>-requested_quantity_unit = ls_input-requestedquantityunit.
      <fs_result>-production_plant = ls_input-productionplant.
      <fs_result>-storage_location = lv_storelocation.
      <fs_result>-batch = ls_input-batch.
      APPEND INITIAL LINE TO <fs_result>-to_pricing_element-results ASSIGNING FIELD-SYMBOL(<fs_price_result>).
      <fs_price_result>-condition_rate_value = ls_input-conditionratevalue.
    ENDLOOP.
    TRY.
        IF ls_req-sales_order_type = 'CBAR'.
          "odata4
          DATA(lo_dest) = zzcl_comm_tool=>get_dest_odata4( ).
        ELSE.
          "odata2
          lo_dest = zzcl_comm_tool=>get_dest( ).
        ENDIF.
      CATCH cx_http_dest_provider_error INTO DATA(lx_http_dest_provider_error).
        EXIT.
    ENDTRY.

    lv_purchase_order_by_customer = ls_req-purchase_order_by_customer.


    "创建销售订单
    "填充固定值
    ls_req-distribution_channel = '10'.
    ls_req-organization_division = '10'.
    ls_req-transaction_currency = 'CNY'.
    IF fsysid = 'JXC' AND ls_req-sales_order_type = 'CBAR'.
      ls_req-sddocumentreason = '001'.
    ENDIF.
    IF fsysid = 'JXC'.
      ls_req-sales_district = 'Z00001'.
    ELSEIF fsysid = 'XYW'.
      ls_req-sales_district = 'Z00002'.
    ENDIF.
    LOOP AT ls_req-to_item-results ASSIGNING FIELD-SYMBOL(<fs_item>).
      CLEAR:lv_flag_0.
      <fs_item>-material_by_customer = <fs_item>-material_by_customer.
      LOOP AT <fs_item>-to_pricing_element-results[] ASSIGNING FIELD-SYMBOL(<fs_price>).
        <fs_price>-condition_type = 'ZPR0'.
        IF <fs_price>-condition_rate_value IS INITIAL OR <fs_price>-condition_rate_value = '0' OR <fs_price>-condition_rate_value = '0.00'.
          lv_flag_0 = 'X'.
          <fs_price>-condition_rate_value = '1'.
        ENDIF.
      ENDLOOP.
      IF lv_flag_0 = 'X'.
        <fs_item>-sales_order_item_category = 'CBXN'.
      ENDIF.
      IF ls_req-sales_order_type = 'CBAR'.
        SELECT SINGLE unitofmeasureisocode
                 FROM i_unitofmeasure WITH PRIVILEGED ACCESS
                WHERE unitofmeasure_e = @<fs_item>-requested_quantity_unit
                 INTO @DATA(lv_isocode).
        <fs_item>-requested_quantity_unit = lv_isocode.
        <fs_item>-custretitmfollowupactivity = '0001'.
        <fs_item>-nextfllwupactivityformatlinsp = '0011'.
        <fs_item>-returnsrefundtype = ''.
*          <fs_item>-returnsrefundprocgmode = 'R'.
        IF <fs_item>-returnsrefundprocgmode = 'R'.
          <fs_item>-returnsrefundcode = '100'.
        ELSEIF <fs_item>-returnsrefundprocgmode = 'N'.
          <fs_item>-returnsrefundcode = ''.
        ENDIF.
        <fs_item>-returnsinspectioncode = '0001'.
        <fs_item>-retsinspiscrtedautomly = 'X'.
      ENDIF.
    ENDLOOP.

    IF ls_req-sales_order_type = 'CBAR'.
      MOVE-CORRESPONDING ls_req TO ls_return.
      LOOP AT ls_req-to_item-results ASSIGNING <fs_item>.
        APPEND INITIAL LINE TO ls_return-_item ASSIGNING FIELD-SYMBOL(<fs_item_return>).
        MOVE-CORRESPONDING <fs_item> TO <fs_item_return>.
        LOOP AT <fs_item>-to_pricing_element-results[] ASSIGNING <fs_price>.
          APPEND INITIAL LINE TO <fs_item_return>-_itempricingelement ASSIGNING FIELD-SYMBOL(<fs_item_return_price>).
          MOVE-CORRESPONDING <fs_price> TO <fs_item_return_price>.
        ENDLOOP.
      ENDLOOP.
    ENDIF.

    lt_mapping = VALUE #(
           ( abap = 'sales_order_type'                  json = 'SalesOrderType'              )
           ( abap = 'sales_organization'                json = 'SalesOrganization'           )
           ( abap = 'sales_district'                    json = 'SalesDistrict'               )
           ( abap = 'sold_to_party'                     json = 'SoldToParty'                 )
           ( abap = 'SDDocumentReason'                  json = 'SDDocumentReason'            )
           ( abap = 'distribution_channel'              json = 'DistributionChannel'         )
           ( abap = 'organization_division'             json = 'OrganizationDivision'        )
           ( abap = 'transaction_currency'              json = 'TransactionCurrency'         )
           ( abap = 'customer_payment_terms'            json = 'CustomerPaymentTerms'        )
           ( abap = 'purchase_order_by_customer'        json = 'PurchaseOrderByCustomer'     )
           ( abap = 'underlying_purchase_order_item'    json = 'UnderlyingPurchaseOrderItem' )
           ( abap = 'material'                          json = 'Material'                    )
           ( abap = 'material_by_customer'              json = 'MaterialByCustomer'          )
           ( abap = 'requested_quantity'                json = 'RequestedQuantity'           )
           ( abap = 'requested_quantity_unit'           json = 'RequestedQuantityUnit'       )
           ( abap = 'production_plant'                  json = 'ProductionPlant'             )
           ( abap = 'storage_location'                  json = 'StorageLocation'             )
           ( abap = 'batch'                             json = 'Batch'                       )
           ( abap = 'condition_rate_value'              json = 'ConditionRateValue'          )
           ( abap = 'condition_type'                    json = 'ConditionType'               )
           ( abap = 'to_item'                           json = 'to_Item'                     )
           ( abap = 'to_pricing_element'                json = 'to_PricingElement'           )
           ( abap = 'sales_order_item_category'         json = 'SalesOrderItemCategory'      )
           ( abap = 'reference_sd_document'             json = 'ReferenceSDDocument'         )
           ( abap = 'reference_sd_document_item'        json = 'ReferenceSDDocumentItem'     )

           ( abap = 'CustRetItmFollowUpActivity'        json = 'CustRetItmFollowUpActivity'     )
           ( abap = 'NextFllwUpActivityForMatlInsp'     json = 'NextFllwUpActivityForMatlInsp'  )
           ( abap = 'ReturnsRefundType'                 json = 'ReturnsRefundType'              )
           ( abap = 'ReturnsRefundProcgMode'            json = 'ReturnsRefundProcgMode'         )
           ( abap = 'ReturnsRefundCode'                 json = 'ReturnsRefundCode'              )
           ( abap = 'ReturnsInspectionCode'             json = 'ReturnsInspectionCode'          )
           ( abap = 'RetsInspIsCrtedAutomly'            json = 'RetsInspIsCrtedAutomly'         )
           ( abap = '_Item'                             json = '_Item'                          )
           ( abap = '_ItemPricingElement'               json = '_ItemPricingElement'            )
           ).
    CASE ls_req-sales_order_type.
      WHEN 'DR'.
        READ TABLE lt_mapping ASSIGNING FIELD-SYMBOL(<fs_map>) WITH KEY abap = 'production_plant'.
        IF sy-subrc = 0.
          <fs_map>-json = 'Plant'.
        ENDIF.
      WHEN 'CBAR'.
        READ TABLE lt_mapping ASSIGNING <fs_map> WITH KEY abap = 'material'.
        IF sy-subrc = 0.
          <fs_map>-json = 'Product'.
        ENDIF.
        READ TABLE lt_mapping ASSIGNING <fs_map> WITH KEY abap = 'requested_quantity_unit'.
        IF sy-subrc = 0.
          <fs_map>-json = 'RequestedQuantityISOUnit'.
        ENDIF.
        READ TABLE lt_mapping ASSIGNING <fs_map> WITH KEY abap = 'production_plant'.
        IF sy-subrc = 0.
          <fs_map>-json = 'Plant'.
        ENDIF.
        READ TABLE lt_mapping ASSIGNING <fs_map> WITH KEY abap = 'condition_rate_value'.
        IF sy-subrc = 0.
          <fs_map>-json = 'ConditionRateAmount'.
        ENDIF.
    ENDCASE.
    "传入数据转JSON
    IF ls_req-sales_order_type = 'CBAR'.
      lv_json = /ui2/cl_json=>serialize(
     data          = ls_return
     compress      = abap_true
     pretty_name   = /ui2/cl_json=>pretty_mode-camel_case
     name_mappings = lt_mapping ).
    ELSE.
      lv_json = /ui2/cl_json=>serialize(
      data          = ls_req
      compress      = abap_true
      pretty_name   = /ui2/cl_json=>pretty_mode-camel_case
      name_mappings = lt_mapping ).
    ENDIF.
*&---接口HTTP 链接调用
    TRY.
        DATA(lo_http_client) = cl_web_http_client_manager=>create_by_http_destination( lo_dest ).
        DATA(lo_request) = lo_http_client->get_http_request(   ).
        lo_http_client->enable_path_prefix( ).
        CASE ls_req-sales_order_type.
          WHEN 'OR'.
            DATA(lv_uri_path) = |/API_SALES_ORDER_SRV/A_SalesOrder?sap-language=zh|.
          WHEN 'CBAR'.
            lv_uri_path = |/api_customerreturn/srvd_a2x/sap/customerreturn/0001/CustomerReturn?sap-language=zh|.
            REPLACE ALL OCCURRENCES OF 'SalesOrderType' IN lv_json WITH 'CustomerReturnType'.
          WHEN 'DR'.
            lv_uri_path = |/API_DEBIT_MEMO_REQUEST_SRV/A_DebitMemoRequest?sap-language=zh|.
            REPLACE ALL OCCURRENCES OF 'SalesOrderType' IN lv_json WITH 'DebitMemoRequestType'.
        ENDCASE.

        lo_request->set_uri_path( EXPORTING i_uri_path = lv_uri_path ).
        lo_request->set_header_field( i_name = 'Accept' i_value = 'application/json' ).
        "lo_request->set_header_field( i_name = 'If-Match' i_value = '*' ).
        lo_http_client->set_csrf_token(  ).

        lo_request->set_content_type( 'application/json' ).

        lo_request->set_text( lv_json ).

*&---执行http post 方法
        DATA(lo_response) = lo_http_client->execute( if_web_http_client=>post ).
*&---获取http reponse 数据
        DATA(lv_res) = lo_response->get_text(  ).
*&---确定http 状态
        DATA(status) = lo_response->get_status( ).
        IF status-code = '201'.

          /ui2/cl_json=>deserialize( EXPORTING json  = lv_res
                                      CHANGING data  = ls_ress ).
          /ui2/cl_json=>deserialize( EXPORTING json  = lv_res
                          CHANGING data  = ls_ress4 ).

          flag  = 'S'.
          CASE ls_req-sales_order_type.
            WHEN 'OR'.
              msg  = ls_ress-d-salesorder.
            WHEN 'CBAR'.
              msg  = ls_ress4-customerreturn.
            WHEN 'DR'.
              msg  = ls_ress-d-debitmemorequest.
          ENDCASE.
        ELSE.
          DATA:ls_rese  TYPE zzs_odata_fail,
               ls_rese4 TYPE zzs_odata4_fail.
          IF ls_req-sales_order_type = 'CBAR'.
            /ui2/cl_json=>deserialize( EXPORTING json  = lv_res
                                       CHANGING data  = ls_rese4 ).
            flag = 'E'.
            msg = ls_rese4-error-message .
          ELSE.
            /ui2/cl_json=>deserialize( EXPORTING json  = lv_res
                                       CHANGING data  = ls_rese ).
            flag = 'E'.
            msg = ls_rese-error-message-value .
          ENDIF.


        ENDIF.
      CATCH cx_http_dest_provider_error INTO DATA(lo_error).
        flag = 'E'.
        msg = '接口调用异常1:' && lo_error->get_longtext( ) .
        RETURN.
      CATCH cx_web_http_client_error INTO DATA(lx_web_http_client_error).
        flag = 'E'.
        msg = '接口调用异常2:' && lx_web_http_client_error->get_longtext( ) .
        RETURN.
    ENDTRY.
    "关闭HTTP链接
    IF lo_http_client IS NOT INITIAL.
      TRY.
          lo_http_client->close( ).
        CATCH cx_web_http_client_error.
          "handle exception
      ENDTRY.
    ENDIF.
    IF flag = 'S'.
      lv_salesorder = msg.
      lv_salesorder = |{ lv_salesorder ALPHA = in }|.
      SELECT a~salesdocument AS sales_order,
             a~purchaseorderbycustomer AS purchase_order_by_customer,
             b~salesdocumentitem AS sales_order_item,
             b~materialbycustomer AS underlying_purchase_order_item
        FROM i_salesdocument WITH PRIVILEGED ACCESS AS a
        INNER JOIN i_salesdocumentitem  WITH PRIVILEGED ACCESS AS b
          ON a~salesdocument = b~salesdocument
       WHERE a~salesdocument = @lv_salesorder
         AND b~salesdocumentrjcnreason = ''
         INTO TABLE @DATA(lt_data_1).
    ENDIF.
    DATA(lv_date1) = cl_abap_context_info=>get_system_date( ).
    DATA(lv_time) = cl_abap_context_info=>get_system_time( ).
    DATA(lv_user) = cl_abap_context_info=>get_user_technical_name( ).
    LOOP AT lt_input ASSIGNING FIELD-SYMBOL(<fs_input>).
      <fs_input>-flag = flag.
      <fs_input>-msg = msg.
      <fs_input>-created_date = lv_date1.
      <fs_input>-created_time = lv_time.
      <fs_input>-created_by   = lv_user.
      IF lt_data_1[] IS NOT INITIAL.
        READ TABLE lt_data_1 INTO DATA(ls_data_1) WITH KEY purchase_order_by_customer = <fs_input>-purchaseorderbycustomer
                                                       underlying_purchase_order_item = <fs_input>-underlyingpurchaseorderitem.
        IF sy-subrc = 0.
          <fs_input>-salesorder = ls_data_1-sales_order.
          <fs_input>-salesorderitem = ls_data_1-sales_order_item.
        ENDIF.
      ENDIF.
    ENDLOOP.

    MOVE-CORRESPONDING lt_input TO lt_zztsd_0003.
    LOOP AT lt_zztsd_0003 ASSIGNING FIELD-SYMBOL(<fs_zztsd_0003>).
      TRY.
          <fs_zztsd_0003>-uuid16 = cl_system_uuid=>create_uuid_x16_static( ).
        CATCH cx_uuid_error.
      ENDTRY.
    ENDLOOP.
    MODIFY zztsd_0003 FROM TABLE @lt_zztsd_0003.

  ENDMETHOD.
ENDCLASS.
