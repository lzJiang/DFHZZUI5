CLASS zclui5_odata_util_0004 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS save_zztmm_0002
      IMPORTING lt_input TYPE zztmm_0002_tab.
    CLASS-METHODS post
      CHANGING lt_input TYPE zztmm_0002_tab
               flag     TYPE bapi_mtype
               msg      TYPE bapi_msg.
    CLASS-METHODS check
      CHANGING lt_input TYPE zztmm_0002_tab
               flag     TYPE bapi_mtype
               msg      TYPE bapi_msg.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCLUI5_ODATA_UTIL_0004 IMPLEMENTATION.


  METHOD check.
     READ TABLE lt_input into data(ls_input_1) INDEX 1.
     IF lt_input[] IS INITIAL.
      flag = 'E'.
      msg = |'传入项目为空'| .
      RETURN.
    ENDIF.

    "1.检查抬头
    IF ls_input_1-outbillno IS INITIAL.
      flag = 'E'.
      msg = '【外部采购订单号】不能为空'.
      RETURN.
    ELSE.
      SELECT SINGLE *
               FROM i_purchaseorderapi01 WITH PRIVILEGED ACCESS
              WHERE supplierrespsalespersonname = @ls_input_1-outbillno
              INTO @DATA(ls_purchaseorder).
      IF sy-subrc = 0.
        flag = 'E'.
        msg = |'外部系统单号'{ ls_input_1-outbillno }'已存在对应SAP采购订单'{ ls_purchaseorder-purchaseorder }',请勿重复创建' |.
        RETURN.
      ENDIF.
    ENDIF.
    IF ls_input_1-companycode IS INITIAL.
      flag = 'E'.
      msg = '【公司代码】不能为空'.
      RETURN.
    ENDIF.
    IF ls_input_1-purchaseordertype IS INITIAL.
      flag = 'E'.
      msg = '【采购订单类型】不能为空'.
      RETURN.
    ENDIF.
    IF ls_input_1-supplier IS INITIAL.
      flag = 'E'.
      msg = '【供应商编码】不能为空'.
      RETURN.
    ENDIF.
    "2.检查行项目
    LOOP AT lt_input[] INTO DATA(ls_item).
      IF ls_item-outbillitemno IS INITIAL.
        flag = 'E'.
        msg = '【外部系统订单行】不能为空'.
        RETURN.
      ENDIF.
      IF ls_item-plant IS INITIAL.
        flag = 'E'.
        msg =  |'行'{ ls_item-outbillitemno }'【收货工厂】不能为空'| .
        RETURN.
      ENDIF.
      IF ls_item-netpriceamount IS INITIAL.
        flag = 'E'.
        msg = |'行'{ ls_item-outbillitemno }'【含税单价】不能为空'| .
        RETURN.
      ENDIF.
      IF ls_item-orderquantity IS INITIAL.
        flag = 'E'.
        msg = |'行'{ ls_item-outbillitemno }'【订单数量】不能为空'| .
        RETURN.
      ENDIF.
      IF ls_item-material IS INITIAL and ls_input_1-purchaseordertype NE 'ZT4'.
        flag = 'E'.
        msg = |'行' { ls_item-outbillitemno } '【物料】不能为空'|.
        RETURN.
      ENDIF.
      IF ls_item-purchaseorderquantityunit IS INITIAL.
        flag = 'E'.
        msg = |'行'{ ls_item-outbillitemno }'【订单单位】不能为空'| .
        RETURN.
      ENDIF.
      IF ls_item-taxcode IS INITIAL.
        flag = 'E'.
        msg = |'行'{ ls_item-outbillitemno }'【税码】不能为空'| .
        RETURN.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD post.
    TYPES:BEGIN OF ty_tosubcontractingcomponent,
            material            TYPE string,
            requirementdate     TYPE string,
            quantityinentryunit TYPE string,
            plant               TYPE string,
            entryunit           TYPE string,
          END OF ty_tosubcontractingcomponent,
          BEGIN OF ty_toscheduleline,
            schedulelineorderquantity  TYPE string,
            schedulelinedeliverydate   TYPE string,
            to_subcontractingcomponent TYPE TABLE OF ty_tosubcontractingcomponent WITH DEFAULT KEY,
          END OF ty_toscheduleline,
          BEGIN OF ty_topurorderpricingelement,
            conditiontype           TYPE string,
            conditionrateamount     TYPE string,
            conditionquantity       TYPE string,
            pricingprocedurecounter TYPE string,
            pricingdocumentitem     TYPE string,
            pricingprocedurestep    TYPE string,
          END OF ty_topurorderpricingelement,
          BEGIN OF ty_toaccountassignment,
            masterfixedasset        TYPE string,
            quantity                TYPE string,
            accountassignmentnumber TYPE string,
          END OF ty_toaccountassignment,
          BEGIN OF ty_topurchaseorderitem,
            suppliermaterialnumber    TYPE string,
            purchaseorderitemtext     TYPE string,
            plant                     TYPE string,
            netpriceamount            TYPE string,
            netpricequantity          TYPE string,
            purchaseorderitemcategory TYPE string,
            accountassignmentcategory TYPE string,
            orderquantity             TYPE string,
            purchaseorderquantityunit TYPE string,
            material                  TYPE string,
            materialgroup             TYPE string,
            taxcode                   TYPE string,
            isreturnsitem             TYPE abap_bool,
            subcontractor             TYPE string,
            supplierissubcontractor   TYPE abap_bool,
            to_scheduleline           TYPE TABLE OF  ty_toscheduleline WITH DEFAULT KEY,
            to_purorderpricingelement TYPE TABLE OF  ty_topurorderpricingelement WITH DEFAULT KEY,
            to_accountassignment      TYPE TABLE OF  ty_toaccountassignment WITH DEFAULT KEY,
          END OF ty_topurchaseorderitem,
          BEGIN OF ty_topurchaseorder,
            supplierrespsalespersonname TYPE string,
            purchaseorder               TYPE string,
            purchaseordertype           TYPE string,
            purchaseorderdate           TYPE string,
            purchasingorganization      TYPE string,
            companycode                 TYPE string,
            purchasinggroup             TYPE string,
            supplier                    TYPE string,
            supplyingplant              TYPE string,
            paymentterms                TYPE string,
            to_purchaseorderitem        TYPE TABLE OF  ty_topurchaseorderitem WITH DEFAULT KEY,
          END OF ty_topurchaseorder.
    DATA:ls_send        TYPE ty_topurchaseorder,
         ls_bomlink     TYPE i_materialbomlink,
         lv_menge       TYPE menge_d,
         lv_pricequalen TYPE i,
         lv_price_6     TYPE p DECIMALS 6,
         lv_price_2     TYPE p DECIMALS 2,
         lv_bs          TYPE i.
    DATA:lv_json TYPE string.
    DATA:lt_mapping TYPE /ui2/cl_json=>name_mappings.
    TYPES:BEGIN OF ty_heads,
            purchaseorder TYPE string,
          END OF ty_heads,
          BEGIN OF ty_ress,
            d TYPE ty_heads,
          END OF  ty_ress.
    DATA:ls_ress TYPE ty_ress.
    DATA:lv_flag_0                     TYPE char1,
         lv_purchase_order_by_customer TYPE i_salesdocument-purchaseorderbycustomer.
    DATA:lv_supplier               TYPE i_supplierpurchasingorg-supplier,
         lv_purchasingorganization TYPE i_supplierpurchasingorg-purchasingorganization.
    TRY.
        DATA(lo_dest) = zzcl_comm_tool=>get_dest( ).
      CATCH cx_http_dest_provider_error INTO DATA(lx_http_dest_provider_error).
        EXIT.
    ENDTRY.

    READ TABLE lt_input INTO DATA(ls_input_1) INDEX 1.

    "转换采购订单值
    "填充抬头
    ls_send-supplierrespsalespersonname = ls_input_1-outbillno.
    ls_send-companycode = ls_input_1-companycode.
    ls_send-purchaseordertype = ls_input_1-purchaseordertype.
    ls_send-supplier = ls_input_1-supplier.
    ls_send-purchasingorganization = ls_input_1-purchasingorganization.
    ls_send-purchasinggroup = ls_input_1-purchasinggroup.
    ls_send-purchaseorderdate = |{ ls_input_1-purchaseorderdate+0(4) }-{ ls_input_1-purchaseorderdate+4(2) }-{ ls_input_1-purchaseorderdate+6(2) }T00:00:00| .
    lv_supplier = ls_input_1-supplier.
    IF strlen( ls_input_1-supplier ) = 4.
      SELECT SINGLE *
             FROM i_plant WITH PRIVILEGED ACCESS
            WHERE plant = @ls_input_1-supplier
             INTO @DATA(ls_plant).
      IF sy-subrc = 0.
        ls_send-supplyingplant = ls_input_1-supplier.
      ENDIF.
    ENDIF.
    lv_supplier = |{ lv_supplier ALPHA = IN }|.
    lv_purchasingorganization = ls_input_1-purchasingorganization.
    SELECT SINGLE *
             FROM i_supplierpurchasingorg WITH PRIVILEGED ACCESS
            WHERE supplier = @lv_supplier
              AND purchasingorganization = @lv_purchasingorganization
             INTO @DATA(ls_supplierpurchasingorg).
    LOOP AT lt_input INTO DATA(ls_req_item).
      CLEAR:ls_bomlink.
      APPEND INITIAL LINE TO ls_send-to_purchaseorderitem ASSIGNING FIELD-SYMBOL(<fs_item>).
      IF ls_send-purchaseordertype = 'ZT7'.
        <fs_item>-isreturnsitem = 'X'.
      ENDIF.
      <fs_item>-suppliermaterialnumber = ls_req_item-outbillitemno.
      <fs_item>-plant = ls_req_item-plant.
*      <fs_item>-netpriceamount = ls_req_item-netpriceamount.
*      <fs_item>-netpricequantity = '1'.
      <fs_item>-purchaseorderitemcategory = ls_req_item-purchaseorderitemcategory.
      <fs_item>-orderquantity = ls_req_item-orderquantity.
      <fs_item>-purchaseorderquantityunit = ls_req_item-purchaseorderquantityunit.
      <fs_item>-material = ls_req_item-material.
      <fs_item>-subcontractor = ls_req_item-subcontractor.
      <fs_item>-supplierissubcontractor = ls_req_item-supplierissubcontractor.
      zcl_com_util=>matnr_zero_in( EXPORTING input  = <fs_item>-material
                                   IMPORTING output = <fs_item>-material ).
      IF ls_req_item-purchaseorderitemtext IS INITIAL.
        SELECT SINGLE productname
                 FROM i_producttext WITH PRIVILEGED ACCESS
                WHERE product = @<fs_item>-material
                  AND language = '1'
                  INTO @ls_req_item-purchaseorderitemtext.
      ELSE.
        <fs_item>-purchaseorderitemtext = ls_req_item-purchaseorderitemtext.
      ENDIF.
      IF ls_input_1-purchaseordertype = 'ZT4'.
        CLEAR:<fs_item>-material.
        <fs_item>-accountassignmentcategory = 'A'.
      ENDIF.
      IF <fs_item>-accountassignmentcategory = 'A'.
        <fs_item>-materialgroup = '8001'.
        APPEND INITIAL LINE TO <fs_item>-to_accountassignment ASSIGNING FIELD-SYMBOL(<fs_accountassignment>).
        <fs_accountassignment>-masterfixedasset = ls_req_item-masterfixedasset.
        <fs_accountassignment>-quantity = '1'.
*        <fs_accountassignment>-accountassignmentnumber = '1'.
      ENDIF.
      APPEND INITIAL LINE TO <fs_item>-to_purorderpricingelement ASSIGNING FIELD-SYMBOL(<fs_price>).
      <fs_price>-conditiontype = 'PMP0'.
      CONDENSE ls_req_item-netpriceamount NO-GAPS.
      <fs_price>-conditionrateamount = ls_req_item-netpriceamount.
      SPLIT <fs_price>-conditionrateamount AT '.' INTO TABLE DATA(lt_conditionrateamount).
      lv_pricequalen = 1.
      lv_bs          = 1.
      READ TABLE lt_conditionrateamount INTO DATA(ls_conditionrateamount) INDEX 2.
      IF sy-subrc = 0.
        IF strlen( ls_conditionrateamount ) > 2.
          DATA(lv_len) = strlen( ls_conditionrateamount ).
          lv_pricequalen = ( lv_len - 2 ).
          DO lv_pricequalen TIMES.
            lv_bs = lv_bs * 10.
          ENDDO.
        ENDIF.
      ENDIF.
      lv_price_6 = <fs_price>-conditionrateamount.
      lv_price_6 = lv_price_6 * lv_bs.
      lv_price_2 = lv_price_6.
      <fs_price>-conditionrateamount = lv_price_2.
      CONDENSE <fs_price>-conditionrateamount NO-GAPS.
      <fs_price>-conditionquantity = lv_bs.
      CONDENSE <fs_price>-conditionquantity NO-GAPS.
      IF ls_supplierpurchasingorg-calculationschemagroupcode = '01'.
        <fs_price>-pricingprocedurestep = '080'.
      ELSE.
        <fs_price>-pricingprocedurestep = '060'.
      ENDIF.
      <fs_price>-pricingprocedurecounter = '001'.
      APPEND INITIAL LINE TO <fs_item>-to_purorderpricingelement ASSIGNING <fs_price>.
      <fs_price>-conditiontype = 'ZP01'.
      <fs_item>-taxcode = ls_req_item-taxcode.
      <fs_price>-conditionrateamount = zcl_com_util=>get_taxrate_by_code( <fs_item>-taxcode ).
      <fs_price>-pricingprocedurestep = '820'.
      <fs_price>-pricingprocedurecounter = '001'.

      APPEND INITIAL LINE TO <fs_item>-to_scheduleline ASSIGNING FIELD-SYMBOL(<fs_scheduleline>).
      <fs_scheduleline>-schedulelinedeliverydate = |{ ls_req_item-schedulelinedeliverydate+0(4) }-{ ls_req_item-schedulelinedeliverydate+4(2) }-{ ls_req_item-schedulelinedeliverydate+6(2) }T00:00:00| .
      IF ls_req_item-purchaseorderitemcategory = '3'.
        SELECT SINGLE   b~unitofmeasure,
                        b~unitofmeasure_e AS unittext
                 FROM i_product WITH PRIVILEGED ACCESS AS a
                 INNER JOIN i_unitofmeasure WITH PRIVILEGED ACCESS AS b
                 ON a~baseunit = b~unitofmeasure
                WHERE product = @<fs_item>-material
                 INTO @DATA(ls_baseunit).
        SELECT SINGLE   b~unitofmeasure,
                        b~unitofmeasure_e AS unittext
                 FROM i_unitofmeasure WITH PRIVILEGED ACCESS AS b
                 INNER JOIN i_productunitsofmeasure WITH PRIVILEGED ACCESS AS a
                   ON b~unitofmeasure = a~alternativeunit
                WHERE unitofmeasure_e = @<fs_item>-purchaseorderquantityunit
                  AND product = @<fs_item>-material
                 INTO @DATA(ls_requnit).
        IF sy-subrc = 0.
*          IF ls_baseunit-unitofmeasure = ls_requnit-unitofmeasure.
*
*          ELSE.
*            CLEAR:lv_menge.
*            lv_menge = ls_req_item-orderquantity.
*            DATA(lo_unit) = cl_uom_conversion=>create( ).
*            lo_unit->unit_conversion_simple( EXPORTING  input                = lv_menge
*                                                        round_sign           = 'X'
*                                                        unit_in              = ls_requnit-unitofmeasure
*                                                        unit_out             = ls_baseunit-unitofmeasure
*                                             IMPORTING  output               = lv_menge
*                                             EXCEPTIONS conversion_not_found = 01
*                                                        division_by_zero     = 02
*                                                        input_invalid        = 03
*                                                        output_invalid       = 04
*                                                        overflow             = 05
*                                                        units_missing        = 06
*                                                        unit_in_not_found    = 07
*                                                        unit_out_not_found   = 08 ).
*            <fs_item>-orderquantity = lv_menge.
*            CONDENSE <fs_item>-orderquantity NO-GAPS.
*          ENDIF.
        ELSE.
          flag = 'E'.
          msg = |物料{ <fs_item>-material }单位{ <fs_item>-purchaseorderquantityunit }不存在|.
          RETURN.
        ENDIF.
        <fs_scheduleline>-schedulelineorderquantity = <fs_item>-orderquantity.
*        CLEAR:<fs_item>-purchaseorderquantityunit.
      ENDIF.
      ls_bomlink-material = <fs_item>-material.
      ls_bomlink-plant = <fs_item>-plant.
      DATA(lv_date) = cl_abap_context_info=>get_system_date( ).
      SELECT b~*
        FROM i_materialbomlink WITH PRIVILEGED ACCESS AS a
        INNER JOIN i_bomcomponentwithkeydate WITH PRIVILEGED ACCESS AS b
          ON a~billofmaterial = b~billofmaterial AND a~billofmaterialcategory = b~billofmaterialcategory
          AND a~billofmaterialvariant = b~billofmaterialvariant
       WHERE a~billofmaterialvariantusage = '1'
         AND a~material = @ls_bomlink-material
         AND a~plant = @ls_bomlink-plant
         AND b~validityenddate >= @lv_date
         AND b~validitystartdate <= @lv_date
         INTO TABLE @DATA(lt_comp).
      SORT lt_comp BY billofmaterial DESCENDING.
      READ TABLE lt_comp INTO DATA(ls_comp_1) INDEX 1.
      IF sy-subrc = 0.
        DELETE lt_comp WHERE billofmaterial NE ls_comp_1-billofmaterial.
      ENDIF.

    ENDLOOP.

    lt_mapping = VALUE #(
           ( abap = 'SupplierRespSalesPersonName'           json = 'SupplierRespSalesPersonName'       )
           ( abap = 'CompanyCode'                           json = 'CompanyCode'                       )
           ( abap = 'PurchaseOrderType'                     json = 'PurchaseOrderType'                 )
           ( abap = 'PurchaseOrderDate'                     json = 'PurchaseOrderDate'                 )
           ( abap = 'Supplier'                              json = 'Supplier'                          )
           ( abap = 'SupplyingPlant'                        json = 'SupplyingPlant'                    )
           ( abap = 'PurchasingOrganization'                json = 'PurchasingOrganization'            )
           ( abap = 'PurchasingGroup'                       json = 'PurchasingGroup'                   )

           ( abap = 'to_PurchaseOrderItem'                  json = 'to_PurchaseOrderItem'              )
           ( abap = 'SupplierMaterialNumber'                json = 'SupplierMaterialNumber'            )
           ( abap = 'PurchaseOrderItemText'                 json = 'PurchaseOrderItemText'             )
           ( abap = 'Plant'                                 json = 'Plant'                             )
           ( abap = 'NetPriceAmount'                        json = 'NetPriceAmount'                    )
           ( abap = 'NetPriceQuantity'                      json = 'NetPriceQuantity'                  )
           ( abap = 'PurchaseOrderItemCategory'             json = 'PurchaseOrderItemCategory'         )
           ( abap = 'AccountAssignmentCategory'             json = 'AccountAssignmentCategory'         )
           ( abap = 'OrderQuantity'                         json = 'OrderQuantity'                     )
           ( abap = 'PurchaseOrderQuantityUnit'             json = 'PurchaseOrderQuantityUnit'         )
           ( abap = 'Material'                              json = 'Material'                          )
           ( abap = 'MaterialGroup'                         json = 'MaterialGroup'                     )
           ( abap = 'TaxCode'                               json = 'TaxCode'                           )
           ( abap = 'IsReturnsItem'                         json = 'IsReturnsItem'                     )
           ( abap = 'Subcontractor'                         json = 'Subcontractor'                     )
           ( abap = 'SupplierIsSubcontractor'               json = 'SupplierIsSubcontractor'           )

           ( abap = 'to_ScheduleLine'                       json = 'to_ScheduleLine'                   )
           ( abap = 'ScheduleLineOrderQuantity'             json = 'ScheduleLineOrderQuantity'         )
           ( abap = 'ScheduleLineDeliveryDate'              json = 'ScheduleLineDeliveryDate'          )

           ( abap = 'to_SubcontractingComponent'            json = 'to_SubcontractingComponent'        )
           ( abap = 'RequirementDate'                       json = 'RequirementDate'                   )
           ( abap = 'QuantityInEntryUnit'                   json = 'QuantityInEntryUnit'               )
           ( abap = 'EntryUnit'                             json = 'EntryUnit'                         )

           ( abap = 'to_purorderpricingelement'             json = 'to_PurchaseOrderPricingElement'    )
           ( abap = 'ConditionType'                         json = 'ConditionType'                     )
           ( abap = 'ConditionQuantity'                     json = 'ConditionQuantity'                 )
           ( abap = 'ConditionRateAmount'                   json = 'ConditionRateValue'                )
           ( abap = 'PricingProcedureStep'                  json = 'PricingProcedureStep'              )
           ( abap = 'PricingProcedureCounter'               json = 'PricingProcedureCounter'           )

           ( abap = 'to_AccountAssignment'                  json = 'to_AccountAssignment'              )
           ( abap = 'MasterFixedAsset'                      json = 'MasterFixedAsset'                  )
           ( abap = 'AccountAssignmentNumber'               json = 'AccountAssignmentNumber'           )
           ( abap = 'Quantity'                              json = 'Quantity'                          )
           ).

    "传入数据转JSON
    lv_json = /ui2/cl_json=>serialize(
          data          = ls_send
          compress      = abap_true
          pretty_name   = /ui2/cl_json=>pretty_mode-camel_case
          name_mappings = lt_mapping ).

*&---接口HTTP 链接调用
    TRY.
        DATA(lo_http_client) = cl_web_http_client_manager=>create_by_http_destination( lo_dest ).
        DATA(lo_request) = lo_http_client->get_http_request(   ).
        lo_http_client->enable_path_prefix( ).

        DATA(lv_uri_path) = |/API_PURCHASEORDER_PROCESS_SRV/A_PurchaseOrder?sap-language=zh|.

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
          flag  = 'S'.
          msg  = ls_ress-d-purchaseorder.
        ELSE.
          DATA:ls_rese TYPE zzs_odata_fail.
          /ui2/cl_json=>deserialize( EXPORTING json  = lv_res
                                      CHANGING data  = ls_rese ).
          flag = 'E'.
          msg = ls_rese-error-message-value .
          IF ls_rese-error-innererror-errordetails[] IS NOT INITIAL.
            LOOP AT ls_rese-error-innererror-errordetails[] ASSIGNING FIELD-SYMBOL(<fs_error_detail>) WHERE severity = 'error'.
              msg = |{ msg }/{ <fs_error_detail>-message }|.
            ENDLOOP.
          ENDIF.

        ENDIF.
      CATCH cx_http_dest_provider_error INTO DATA(lo_error).
        flag = 'E'.
        msg = '接口调用异常1:' && lo_error->get_longtext( ) .
      CATCH cx_web_http_client_error INTO DATA(lx_web_http_client_error).
        flag = 'E'.
        msg = '接口调用异常2:' && lx_web_http_client_error->get_longtext( ) .
    ENDTRY.
    "关闭HTTP链接
    IF lo_http_client IS NOT INITIAL.
      TRY.
          lo_http_client->close( ).
        CATCH cx_web_http_client_error.
          "handle exception
      ENDTRY.
    ENDIF.

    DATA:lt_zztmm_0001    TYPE TABLE OF zztmm_0001,
         lv_purchaseorder TYPE ebeln,
         lt_zztmm_0002    TYPE TABLE OF zztmm_0002.

    IF flag = 'S'.
      lv_purchaseorder = msg.
      lv_purchaseorder = |{ lv_purchaseorder ALPHA = IN }|.
      SELECT a~purchaseorder,
           b~purchaseorderitem,
           a~supplierrespsalespersonname AS outbillno,
           b~suppliermaterialnumber AS outbillitemno
      FROM i_purchaseorderapi01 WITH PRIVILEGED ACCESS AS a
      INNER JOIN i_purchaseorderitemapi01  WITH PRIVILEGED ACCESS AS b
        ON a~purchaseorder = b~purchaseorder
     WHERE a~purchaseorder = @lv_purchaseorder
       AND b~purchasingdocumentdeletioncode = ''
      INTO TABLE @DATA(lt_data).
      lt_zztmm_0001 = CORRESPONDING #( DEEP lt_data ).
      LOOP AT lt_zztmm_0001 ASSIGNING FIELD-SYMBOL(<fs_zztmm_0001>).
        <fs_zztmm_0001>-zdate = cl_abap_context_info=>get_system_date( ).
        <fs_zztmm_0001>-ztime = cl_abap_context_info=>get_system_time( ).
        <fs_zztmm_0001>-zuser = cl_abap_context_info=>get_user_technical_name( ).
      ENDLOOP.
      MODIFY zztmm_0001 FROM TABLE @lt_zztmm_0001.

      LOOP AT lt_input ASSIGNING FIELD-SYMBOL(<fs_input>).
        READ TABLE lt_zztmm_0001 INTO DATA(ls_zztmm_0001) WITH KEY outbillno = <fs_input>-outbillno
                                                                   outbillitemno = <fs_input>-outbillitemno.
        IF sy-subrc = 0.
          <fs_input>-purchaseorder = ls_zztmm_0001-purchaseorder.
          <fs_input>-purchaseorderitem = ls_zztmm_0001-purchaseorderitem.
          <fs_input>-flag = 'S'.
        ENDIF.
      ENDLOOP.

      MOVE-CORRESPONDING lt_input TO lt_zztmm_0002.
      DATA(lv_date1) = cl_abap_context_info=>get_system_date( ).
      DATA(lv_time) = cl_abap_context_info=>get_system_time( ).
      DATA(lv_user) = cl_abap_context_info=>get_user_technical_name( ).

      LOOP AT lt_zztmm_0002 ASSIGNING FIELD-SYMBOL(<fs_zztmm_0002>).
        TRY.
            <fs_zztmm_0002>-uuid16 = cl_system_uuid=>create_uuid_x16_static( ).
          CATCH cx_uuid_error.

        ENDTRY.
        <fs_zztmm_0002>-created_date = lv_date1.
        <fs_zztmm_0002>-created_time = lv_time.
        <fs_zztmm_0002>-created_by   = lv_user.
      ENDLOOP.
      MODIFY zztmm_0002 FROM TABLE @lt_zztmm_0002.
    ELSE.
      LOOP AT lt_input ASSIGNING <fs_input>.
        <fs_input>-flag = flag.
        <fs_input>-msg = msg.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD save_zztmm_0002.

    DATA:lt_zztmm_0002 TYPE TABLE OF zztmm_0002.

    MOVE-CORRESPONDING lt_input TO lt_zztmm_0002.
    DATA(lv_date) = cl_abap_context_info=>get_system_date( ).
    DATA(lv_time) = cl_abap_context_info=>get_system_time( ).
    DATA(lv_user) = cl_abap_context_info=>get_user_technical_name( ).

    LOOP AT lt_zztmm_0002 ASSIGNING FIELD-SYMBOL(<fs_zztmm_0002>).
      TRY.
          <fs_zztmm_0002>-uuid16 = cl_system_uuid=>create_uuid_x16_static( ).
        CATCH cx_uuid_error.

      ENDTRY.
      <fs_zztmm_0002>-created_date = lv_date.
      <fs_zztmm_0002>-created_time = lv_time.
      <fs_zztmm_0002>-created_by   = lv_user.
    ENDLOOP.
    MODIFY zztmm_0002 FROM TABLE @lt_zztmm_0002.

  ENDMETHOD.
ENDCLASS.
