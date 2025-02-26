CLASS zclui5_odata_util_0007 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS post
      CHANGING lt_input TYPE zztmm_0004_tab
               flag     TYPE bapi_mtype
               msg      TYPE bapi_msg.
    CLASS-METHODS check
      CHANGING lt_input TYPE zztmm_0004_tab
               flag     TYPE bapi_mtype
               msg      TYPE bapi_msg.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCLUI5_ODATA_UTIL_0007 IMPLEMENTATION.


  METHOD check.
    DATA:lv_material TYPE matnr.
    READ TABLE lt_input INTO DATA(ls_input_1) INDEX 1.
    IF lt_input[] IS INITIAL.
      flag = 'E'.
      msg = |'传入项目为空'| .
      RETURN.
    ENDIF.

    "1.检查抬头
    IF ls_input_1-outbillno IS INITIAL.
      flag = 'E'.
      msg = '【导入物料凭证号】不能为空'.
      RETURN.
    ELSE.
      SELECT SINGLE *
           FROM i_materialdocumentheader_2 WITH PRIVILEGED ACCESS
          WHERE materialdocumentheadertext = @ls_input_1-outbillno
           INTO @DATA(ls_materialdocumentheader).
      IF sy-subrc = 0.
        flag = 'E'.
        msg = |导入凭证号{ ls_input_1-outbillno }已存在物料凭证{ ls_materialdocumentheader-materialdocument },请勿重复导入|.
        RETURN.
      ENDIF.
    ENDIF.
    IF ls_input_1-documentdate IS INITIAL.
      flag = 'E'.
      msg = '【凭证日期】不能为空'.
      RETURN.
    ENDIF.
    IF ls_input_1-postingdate IS INITIAL.
      flag = 'E'.
      msg = '【过账日期】不能为空'.
      RETURN.
    ENDIF.
    "2.检查行项目
    LOOP AT lt_input[] INTO DATA(ls_item).
      DATA(lv_tabix) = sy-tabix.
      IF ls_item-outbillitemno IS INITIAL.
        flag = 'E'.
        msg = '【导入凭证行】不能为空'.
        RETURN.
      ENDIF.
      IF ls_item-goodsmovementtype IS INITIAL.
        flag = 'E'.
        msg =  |'行'{ ls_item-outbillitemno }'【移动类型】不能为空'| .
        RETURN.
      ENDIF.
      IF ls_item-batchwms IS INITIAL.
        flag = 'E'.
        msg =  |'行'{ ls_item-outbillitemno }'【WMS批次号】不能为空'| .
        RETURN.
      ENDIF.
      IF ls_item-plant IS INITIAL.
        flag = 'E'.
        msg =  |'行'{ ls_item-outbillitemno }'【工厂】不能为空'| .
        RETURN.
      ENDIF.
      IF ls_item-storagelocation IS INITIAL.
        flag = 'E'.
        msg =  |'行'{ ls_item-outbillitemno }'【库位】不能为空'| .
        RETURN.
      ENDIF.
      IF ls_item-material IS INITIAL.
        flag = 'E'.
        msg =  |'行'{ ls_item-outbillitemno }'【物料】不能为空'| .
        RETURN.
      ENDIF.
      IF ls_item-goodsmovementtype = '311' AND ls_item-entryunit IS INITIAL.
      zcl_com_util=>matnr_zero_in( EXPORTING input = ls_item-material
                                   IMPORTING output = lv_material ).
        SELECT SINGLE b~unitofmeasure_e
                 FROM i_product WITH PRIVILEGED ACCESS AS a
                 INNER JOIN i_unitofmeasure AS b
                 ON a~baseunit = b~unitofmeasure
                WHERE a~product = @lv_material
                 INTO @ls_item-entryunit.
        MODIFY lt_input FROM ls_item INDEX lv_tabix.
      ENDIF.
      IF ls_item-entryunit IS INITIAL.
        flag = 'E'.
        msg =  |'行'{ ls_item-outbillitemno }'【单位】不能为空'| .
        RETURN.
      ENDIF.
      IF ls_item-quantityinentryunit IS INITIAL.
        flag = 'E'.
        msg =  |'行'{ ls_item-outbillitemno }'【数量】不能为空'| .
        RETURN.
      ENDIF.
      IF strlen( ls_item-zvension ) > 10.
        flag = 'E'.
        msg =  |'行'{ ls_item-outbillitemno }'【版本】字段长度不能超过10位'| .
        RETURN.
      ENDIF.
      IF strlen( ls_item-batchwms ) > 30.
        flag = 'E'.
        msg =  |'行'{ ls_item-outbillitemno }'【WMS批次】字段长度不能超过30位'| .
        RETURN.
      ENDIF.
      CASE ls_item-goodsmovementtype.
        WHEN '101'.
          IF ls_item-purchaseorder IS INITIAL AND ls_item-manufacturingorder IS INITIAL.
            flag = 'E'.
            msg =  |'行'{ ls_item-outbillitemno }'移动类型101不能【采购订单】和【生产订单】均为空'| .
            RETURN.
          ENDIF.
          IF ls_item-purchaseorder IS NOT INITIAL AND ls_item-purchaseorder IS INITIAL.
            flag = 'E'.
            msg =  |'行'{ ls_item-outbillitemno }'移动类型101不能【采购订单】和【采购订单行号】必须同时填写'| .
            RETURN.
          ENDIF.
        WHEN '311'.
          IF ls_item-issuingorreceivingstorageloc IS INITIAL.
            flag = 'E'.
            msg =  |'行'{ ls_item-outbillitemno }'移动类型311【传输库位】不能为空'| .
            RETURN.
          ENDIF.
        WHEN '261'.
          IF ls_item-manufacturingorder IS INITIAL.
            flag = 'E'.
            msg =  |'行'{ ls_item-outbillitemno }'移动类型261【生产订单号】不能为空'| .
            RETURN.
          ENDIF.
        WHEN '501'.

        WHEN '541'.

        WHEN '201' OR '202' OR 'Z01' OR 'Z02' OR 'Z05' OR 'Z06'.
          IF ls_item-costcenter IS INITIAL.
            flag = 'E'.
            msg =  |'行'{ ls_item-outbillitemno }'移动类型{ ls_item-goodsmovementtype }【成本中心】不能为空'| .
            RETURN.
          ENDIF.
        WHEN '221' OR '222'.
          IF ls_item-wbselement IS INITIAL.
            flag = 'E'.
            msg =  |'行'{ ls_item-outbillitemno }'移动类型{ ls_item-goodsmovementtype }【WBS元素】不能为空'| .
            RETURN.
          ENDIF.
        WHEN OTHERS.
          flag = 'E'.
          msg =  |行{ ls_item-outbillitemno }移动类型{ ls_item-goodsmovementtype }不支持| .
          RETURN.
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.


  METHOD post.
    TYPES:BEGIN OF ty_item,
            goodsmovementtype            TYPE string,
            goodsmovementrefdoctype      TYPE string,
            delivery                     TYPE i_deliverydocumentitem-deliverydocument,
            deliveryitem                 TYPE string,
            purchaseorder                TYPE i_materialdocumentitem_2-purchaseorder,
            purchaseorderitem            TYPE string,
            manufacturingorder           TYPE aufnr,
            manufacturingorderitem       TYPE string,
            reservation                  TYPE string,
            reservationitem              TYPE string,
            costcenter                   TYPE string,
            material                     TYPE matnr,
            plant                        TYPE string,
            storagelocation              TYPE string,
            batch                        TYPE string,
            manufacturedate              TYPE string,
            quantityinbaseunit           TYPE string,
            entryunit                    TYPE string,
            quantityinentryunit          TYPE string,
            issgorrcvgmaterial           TYPE string,
            issuingorreceivingplant      TYPE string,
            issuingorreceivingstorageloc TYPE string,
            issgorrcvgbatch              TYPE string,
            shelflifeexpirationdate      TYPE string,
            reversedmaterialdocumentyear TYPE string,
            reversedmaterialdocument     TYPE string,
            reversedmaterialdocumentitem TYPE string,
            invtrymgmtreferencedocument  TYPE string,
            invtrymgmtrefdocumentitem    TYPE string,
            goodsmovementreasoncode      TYPE string,
            supplier                     TYPE string,
            customer                     TYPE string,
            materialdocumentitemtext     TYPE string,
            inventoryspecialstocktype    TYPE string,
            materialdocumentline         TYPE string,
            materialdocumentparentline   TYPE string,
            hierarchynodelevel           TYPE string,
            wbselement                   TYPE string,
            batchbysupplier(15)          TYPE c,
            inventorystocktype           TYPE string,
            inventoryusabilitycode       TYPE string,
          END OF ty_item,
          BEGIN OF tty_item,
            results TYPE TABLE OF ty_item WITH EMPTY KEY,
          END OF tty_item,
          BEGIN OF ty_data,
            documentdate               TYPE string,
            postingdate                TYPE string,
            referencedocument          TYPE string,
            goodsmovementcode          TYPE string,
            materialdocumentheadertext TYPE string,
            to_materialdocumentitem    TYPE tty_item,
          END OF ty_data.

    DATA:lv_date        TYPE string.
    DATA:lv_json TYPE string.
    DATA:lt_mapping TYPE /ui2/cl_json=>name_mappings.
    DATA:ls_data TYPE ty_data,
         ls_item TYPE ty_item,
         ls_sub  TYPE ty_item,
         lt_item TYPE TABLE OF ty_item.
    DATA:lv_mater18(18).
    DATA:lv_deliveryitem TYPE i_deliverydocumentitem-deliverydocumentitem.
*&---BAPI参数
    DATA:lv_msg  TYPE bapi_msg,
         lv_msg2 TYPE bapi_msg.
    DATA:lv_tabix TYPE i_materialdocumentitem_2-materialdocumentitem.
    DATA:lv_purchaseorderitem   TYPE i_materialdocumentitem_2-purchaseorderitem.
    DATA:lv_line_id    TYPE i_materialdocumentitem_2-materialdocumentline,
         lv_parent_id  TYPE i_materialdocumentitem_2-materialdocumentparentline,
         lv_line_depth TYPE numc2.
    DATA:lv_quantity TYPE i_posubcontractingcompapi01-requiredquantity.
    DATA:lv_remain TYPE i_posubcontractingcompapi01-requiredquantity.
    DATA:lv_materialdocument     TYPE i_materialdocumentheader_2-materialdocument,
         lv_materialdocumentyear TYPE i_materialdocumentheader_2-materialdocumentyear.
    DATA:lt_zztmm_0004 TYPE TABLE OF zztmm_0004,
         ls_zztmm_0003 TYPE zztmm_0003.

*&---=============================使用API 步骤01
    DATA(lo_dest) = zzcl_comm_tool=>get_dest( ).


*&---导入结构JSON MAPPING
    lt_mapping = VALUE #(
         ( abap = 'DocumentDate'                 json = 'DocumentDate'                )
         ( abap = 'PostingDate'                  json = 'PostingDate'                 )
         ( abap = 'ReferenceDocument'            json = 'ReferenceDocument'           )
         ( abap = 'GoodsMovementCode'            json = 'GoodsMovementCode'           )
         ( abap = 'MaterialDocumentHeaderText'   json = 'MaterialDocumentHeaderText'  )
         ( abap = 'to_MaterialDocumentItem'      json = 'to_MaterialDocumentItem'     )
         ( abap = 'results'                      json = 'results'                     )
         ( abap = 'GoodsMovementRefDocType'      json = 'GoodsMovementRefDocType'     )
         ( abap = 'GoodsMovementType'            json = 'GoodsMovementType'           )
         ( abap = 'Delivery'                     json = 'Delivery'                    )
         ( abap = 'DeliveryItem'                 json = 'DeliveryItem'                )
         ( abap = 'PurchaseOrder'                json = 'PurchaseOrder'               )
         ( abap = 'PurchaseOrderItem'            json = 'PurchaseOrderItem'           )
         ( abap = 'ManufacturingOrder'           json = 'ManufacturingOrder'          )
         ( abap = 'ManufacturingOrderItem'       json = 'ManufacturingOrderItem'      )
         ( abap = 'Reservation'                  json = 'Reservation'                 )
         ( abap = 'ReservationItem'              json = 'ReservationItem'             )
         ( abap = 'CostCenter'                   json = 'CostCenter'                  )
         ( abap = 'Material'                     json = 'Material'                    )
         ( abap = 'Plant'                        json = 'Plant'                       )
         ( abap = 'StorageLocation'              json = 'StorageLocation'             )
         ( abap = 'Batch'                        json = 'Batch'                       )
         ( abap = 'ManufactureDate'              json = 'ManufactureDate'             )
         ( abap = 'EntryUnit'                    json = 'EntryUnit'                   )
         ( abap = 'QuantityInEntryUnit'          json = 'QuantityInEntryUnit'         )
         ( abap = 'QuantityInBaseUnit'           json = 'QuantityInBaseUnit'          )
         ( abap = 'IssgOrRcvgMaterial'           json = 'IssgOrRcvgMaterial'          )
         ( abap = 'IssuingOrReceivingPlant'      json = 'IssuingOrReceivingPlant'     )
         ( abap = 'IssuingOrReceivingStorageLoc' json = 'IssuingOrReceivingStorageLoc' )
         ( abap = 'IssgOrRcvgBatch'              json = 'IssgOrRcvgBatch'             )
         ( abap = 'ShelfLifeExpirationDate'      json = 'ShelfLifeExpirationDate'     )
         ( abap = 'ReversedMaterialDocumentYear' json = 'ReversedMaterialDocumentYear' )
         ( abap = 'ReversedMaterialDocument'     json = 'ReversedMaterialDocument'    )
         ( abap = 'ReversedMaterialDocumentItem' json = 'ReversedMaterialDocumentItem' )
         ( abap = 'InvtryMgmtReferenceDocument'  json = 'InvtryMgmtReferenceDocument' )
         ( abap = 'InvtryMgmtRefDocumentItem'    json = 'InvtryMgmtRefDocumentItem'   )
         ( abap = 'GoodsMovementReasonCode'      json = 'GoodsMovementReasonCode'     )
         ( abap = 'BatchBySupplier'              json = 'BatchBySupplier'             )

         ( abap = 'Supplier'                     json = 'Supplier'                    )
         ( abap = 'Customer'                     json = 'Customer'                    )
         ( abap = 'InventorySpecialStockType'    json = 'InventorySpecialStockType'   )
         ( abap = 'InventoryStockType'           json = 'InventoryStockType'          )
         ( abap = 'InventoryUsabilityCode'       json = 'InventoryUsabilityCode'          )
         ( abap = 'MaterialDocumentItemText'     json = 'MaterialDocumentItemText'    )
         ( abap = 'WBSElement'                   json = 'WBSElement'                  )

         ( abap = 'MaterialDocumentLine'         json = 'MaterialDocumentLine'        )
         ( abap = 'MaterialDocumentParentLine'   json = 'MaterialDocumentParentLine'  )
         ( abap = 'HierarchyNodeLevel'           json = 'HierarchyNodeLevel'  )
      ).

    READ TABLE lt_input INTO DATA(ls_input_1) INDEX 1.
    IF ls_input_1-materialdocumentheadertext IS NOT INITIAL.
      SELECT SINGLE a~*
             FROM i_materialdocumentheader_2 WITH PRIVILEGED ACCESS AS a
           INNER JOIN i_materialdocumentitem_2 WITH PRIVILEGED ACCESS AS b
             ON a~materialdocument = b~materialdocument AND a~materialdocumentyear = b~materialdocumentyear
            WHERE materialdocumentheadertext = @ls_input_1-materialdocumentheadertext
              AND goodsmovementiscancelled = ''
              AND b~reversedmaterialdocument = ''
             INTO @DATA(ls_materialdocumentheader).
      IF sy-subrc = 0.
        flag = 'S'.
        msg = |导入凭证【{ ls_input_1-materialdocumentheadertext }】已生成SAP物料凭证{ ls_materialdocumentheader-materialdocument  }|
        && |-{ ls_materialdocumentheader-materialdocumentyear },请勿重复推送| .
        RETURN.
      ENDIF.
    ENDIF.

    "数据整合
    "凭证日期
    ls_data-documentdate = |{ ls_input_1-documentdate+0(4) }-{ ls_input_1-documentdate+4(2) }-{ ls_input_1-documentdate+6(2) }T00:00:00| .
    "过账日期
    ls_data-postingdate = |{ ls_input_1-postingdate+0(4) }-{ ls_input_1-postingdate+4(2) }-{ ls_input_1-postingdate+6(2) }T00:00:00| .
    "抬头文本
    ls_data-materialdocumentheadertext = ls_input_1-outbillno.
    LOOP AT lt_input ASSIGNING FIELD-SYMBOL(<fs_input>).
      CLEAR:ls_item.
      lv_tabix = lv_tabix + 1.
      lv_line_id = lv_line_id + 1.
      <fs_input>-batchwms = to_upper( <fs_input>-batchwms ).
      MOVE-CORRESPONDING <fs_input> TO ls_item.

      ls_item-manufacturingorder = |{ ls_item-manufacturingorder ALPHA = IN }|.
      ls_item-purchaseorder = |{ ls_item-purchaseorder ALPHA = IN }|.
      ls_item-delivery = |{ ls_item-delivery ALPHA = IN }|.
      lv_mater18 = <fs_input>-material.
      lv_mater18 = |{ lv_mater18 ALPHA = IN }|.
      ls_item-material = lv_mater18.
      lv_deliveryitem = ls_item-deliveryitem.
      ls_item-materialdocumentitemtext = <fs_input>-outbillitemno.
      "供应商批次
      IF <fs_input>-batchwms IS NOT INITIAL.
        ls_item-batchbysupplier = <fs_input>-batchwms.
      ENDIF.
      "匹配批次
      IF <fs_input>-batchwms IS NOT INITIAL.
        SELECT SINGLE a~material,
                  a~batch
          FROM i_batchcharacteristicvaluetp_2 WITH PRIVILEGED ACCESS AS a
          JOIN i_clfncharacteristic WITH PRIVILEGED ACCESS AS b ON a~charcinternalid = b~charcinternalid
         WHERE material = @lv_mater18
           AND b~characteristic = 'Z_WMSBATCH'
           AND a~charcvalue = @<fs_input>-batchwms
          INTO @DATA(ls_valuetp).
        IF sy-subrc = 0.
          ls_item-batch = ls_valuetp-batch.
          <fs_input>-batch = ls_item-batch.
        ELSE.
          IF ls_item-goodsmovementtype NE '101' AND ls_item-goodsmovementtype NE '501'.
            flag = 'E'.
            msg =  |行{ <fs_input>-outbillitemno }物料{ <fs_input>-material }WMS批次{ <fs_input>-batchwms }未找到对应的SAP批次| .
            EXIT.
          ENDIF.
        ENDIF.
      ENDIF.


*      IF <fs_input>-issgorrcvgbatchwms IS NOT INITIAL.
*        SELECT SINGLE a~material,
*                  a~batch
*          FROM i_batchcharacteristicvaluetp_2 WITH PRIVILEGED ACCESS AS a
*          JOIN i_clfncharacteristic WITH PRIVILEGED ACCESS AS b ON a~charcinternalid = b~charcinternalid
*         WHERE material = @lv_mater18
*           AND b~characteristic = 'Z_WMSBATCH'
*           AND a~charcvalue = @<fs_input>-issgorrcvgbatchwms
*          INTO @ls_valuetp.
*        IF sy-subrc = 0.
*          ls_item-issgorrcvgbatch = ls_valuetp-batch.
*          <fs_input>-issgorrcvgbatch = ls_item-issgorrcvgbatch.
*        ENDIF.
*      ENDIF.

      "移动类型确认 code
      CASE ls_item-goodsmovementtype.
        WHEN '101'."采购入库 生产入库

          IF ls_item-manufacturingorder IS NOT INITIAL.
            ls_data-goodsmovementcode = '02'.
            ls_item-goodsmovementrefdoctype = 'F'.
            ls_item-batch = <fs_input>-batchwms.
            <fs_input>-batch = <fs_input>-batchwms.
            SELECT SINGLE *
                     FROM i_manufacturingorderitem WITH PRIVILEGED ACCESS
                    WHERE manufacturingorder = @ls_item-manufacturingorder
                     INTO @DATA(ls_manufacturingorderitem).
            IF sy-subrc = 0.
              ls_item-inventoryusabilitycode = ls_manufacturingorderitem-inventoryusabilitycode.
            ENDIF.

          ENDIF.

          IF ls_item-purchaseorder IS NOT INITIAL.
            IF strlen( <fs_input>-batchwms ) <= 10.
              ls_item-batch = ls_valuetp-batch.
              <fs_input>-batch = ls_item-batch.
            ENDIF.
            ls_data-goodsmovementcode = '01'.
            ls_item-goodsmovementrefdoctype = 'B'.
            lv_purchaseorderitem = ls_item-purchaseorderitem.
            SELECT SINGLE *
            FROM i_purchaseorderitemapi01 WITH PRIVILEGED ACCESS
           WHERE purchaseorder = @ls_item-purchaseorder
             AND purchaseorderitem = @lv_purchaseorderitem
             AND purchaseorderitemcategory = '3'
            INTO @DATA(ls_materialdocumentitem_1).
            IF sy-subrc = 0 AND ls_item-goodsmovementtype = '101'.
              "委外订单收货，设置最后收获标识为X,将委外订单组件全部消耗
              <fs_input>-zzlast = 'X'.
            ENDIF.

          ENDIF.

          "当 是否最后一次收货 = X 时 ， 543 需要加增强计算扣减数量 = 总需要扣减数量 - 已提货数量
          "针对委外订单
          IF <fs_input>-zzlast = 'X' AND <fs_input>-purchaseorder IS NOT INITIAL.
            lv_purchaseorderitem = ls_item-purchaseorderitem.
            lv_parent_id = lv_tabix.
            DATA:lt_collect_543 TYPE TABLE OF i_posubcontractingcompapi01,
                 ls_collect_543 TYPE i_posubcontractingcompapi01.
            "委外订单组件需求数量
            SELECT a~purchaseorder,
                   a~purchaseorderitem,
                   a~material,
                   a~requiredquantity,
                   a~withdrawnquantity,
                   a~batch,
                   a~plant,
                   b~supplier
              FROM i_posubcontractingcompapi01 WITH PRIVILEGED ACCESS AS a
              LEFT JOIN i_purchaseorderapi01 WITH PRIVILEGED ACCESS AS b ON a~purchaseorder = b~purchaseorder
             WHERE a~purchaseorder = @ls_item-purchaseorder
               AND a~purchaseorderitem = @lv_purchaseorderitem
              INTO TABLE @DATA(lt_posub).

            IF lt_posub[] IS NOT INITIAL.
              "委外订单组件不为空
              ls_item-materialdocumentline = lv_parent_id.
              "委外订单组件提货数量
              SELECT *
                FROM i_materialdocumentitem_2 WITH PRIVILEGED ACCESS AS a
               WHERE a~purchaseorder = @ls_item-purchaseorder
                 AND a~purchaseorderitem = @lv_purchaseorderitem
                 AND a~goodsmovementtype IN ( '543','544' )
                INTO TABLE @DATA(lt_materialdocumentitem_543).

              LOOP AT lt_materialdocumentitem_543 INTO DATA(ls_materialdocumentitem_543).
                ls_collect_543-purchaseorder = ls_materialdocumentitem_543-purchaseorder.
                ls_collect_543-purchaseorderitem = ls_materialdocumentitem_543-purchaseorderitem.
                ls_collect_543-material = ls_materialdocumentitem_543-material.
                IF ls_materialdocumentitem_543-goodsmovementtype = '544'.
                  ls_materialdocumentitem_543-quantityinentryunit = ls_materialdocumentitem_543-quantityinentryunit * -1.
                ENDIF.
                ls_collect_543-withdrawnquantity = ls_materialdocumentitem_543-quantityinentryunit.
                COLLECT ls_collect_543 INTO lt_collect_543.
                CLEAR:ls_collect_543.
              ENDLOOP.
              SORT lt_collect_543 BY purchaseorder purchaseorderitem material.
              LOOP AT lt_posub ASSIGNING FIELD-SYMBOL(<fs_posub>).
                CLEAR:<fs_posub>-withdrawnquantity.
                READ TABLE lt_collect_543 INTO ls_collect_543 WITH KEY purchaseorder = <fs_posub>-purchaseorder
                                                                       purchaseorderitem = <fs_posub>-purchaseorderitem
                                                                       material = <fs_posub>-material BINARY SEARCH.
                IF sy-subrc = 0.
                  <fs_posub>-withdrawnquantity = ls_collect_543-withdrawnquantity.
                ENDIF.
              ENDLOOP.
              "O库存
              SELECT a~plant,
                     a~product,
                     c~lastgoodsreceiptdate,
                     a~batch,
                     a~matlwrhsstkqtyinmatlbaseunit
                FROM i_stockquantitycurrentvalue_2( p_displaycurrency = 'CNY' ) WITH PRIVILEGED ACCESS AS a
                JOIN @lt_posub AS b ON a~plant   = b~plant
                                   AND a~product = b~material
                                   AND a~supplier = b~supplier
                JOIN i_batchdistinct WITH PRIVILEGED ACCESS AS c ON a~product = c~material
                                                                AND a~batch = c~batch
               WHERE a~inventoryspecialstocktype = 'O'
                 AND a~valuationareatype = '1'
                INTO TABLE @DATA(lt_stock).

              SORT lt_stock BY plant product lastgoodsreceiptdate ASCENDING.

              LOOP AT lt_posub INTO DATA(ls_posub).
                CLEAR:ls_sub,lv_quantity.
                ls_sub-purchaseorder  =   ls_item-purchaseorder.
                ls_sub-purchaseorderitem  = ls_item-purchaseorderitem.
                ls_sub-goodsmovementtype = '543'.  " 移动类型
                ls_sub-material = ls_posub-material. " 物料号
                ls_sub-inventoryspecialstocktype = 'O'. " 特殊库存
                ls_sub-supplier = ls_posub-supplier.
                ls_sub-plant = ls_posub-plant.
                "ls_sub-batch = '20241130AA'.
                lv_quantity = ls_posub-requiredquantity - ls_posub-withdrawnquantity."剩余需求数量
*                IF lv_quantity <= 0.
                  ls_sub-quantityinentryunit = 0.
                  CONDENSE  ls_sub-quantityinentryunit NO-GAPS.
                  ls_sub-materialdocumentparentline = lv_parent_id. " 父项目编码
                  lv_line_id = lv_line_id + 1. " 子项目编号
                  ls_sub-materialdocumentline = lv_line_id.
                  APPEND ls_sub TO lt_item.
                  CONTINUE.
*                ENDIF.
*                ls_sub-quantityinentryunit = ls_posub-requiredquantity - ls_posub-withdrawnquantity.
                READ TABLE lt_stock TRANSPORTING NO FIELDS WITH KEY plant = ls_posub-plant
                                                                    product = ls_posub-material BINARY SEARCH.
                IF sy-subrc = 0.
                  LOOP AT lt_stock INTO DATA(ls_stock) FROM sy-tabix.
                    IF ls_stock-plant = ls_posub-plant AND ls_stock-product = ls_posub-material.
                      lv_remain = lv_quantity - ls_stock-matlwrhsstkqtyinmatlbaseunit.
                      IF lv_remain > 0.
                        ls_sub-batch = ls_stock-batch.
                        ls_sub-quantityinentryunit = ls_stock-matlwrhsstkqtyinmatlbaseunit.
                        CONDENSE  ls_sub-quantityinentryunit NO-GAPS.
                        ls_sub-materialdocumentparentline = lv_parent_id. " 父项目编码
                        lv_line_id = lv_line_id + 1. " 子项目编号
                        ls_sub-materialdocumentline = lv_line_id.
                        APPEND ls_sub TO lt_item.
                        lv_quantity = lv_remain .
                      ELSE.
                        ls_sub-batch = ls_stock-batch.
                        ls_sub-quantityinentryunit = lv_quantity.
                        CONDENSE  ls_sub-quantityinentryunit NO-GAPS.
                        lv_quantity = 0.
                        ls_sub-materialdocumentparentline = lv_parent_id. " 父项目编码
                        lv_line_id = lv_line_id + 1. " 子项目编号
                        ls_sub-materialdocumentline = lv_line_id.
                        APPEND ls_sub TO lt_item.
                        EXIT.
                      ENDIF.

                    ELSE.
                      EXIT.
                    ENDIF.
                  ENDLOOP.
                  IF lv_quantity > 0.
                    flag = 'E'.
                    msg = |委外订单行{ <fs_input>-purchaseorder }-{ <fs_input>-purchaseorderitem }组件{ ls_posub-material }缺少非限制O库存数量{ lv_quantity }| .
                    EXIT.
                  ENDIF.
                ENDIF.
*              ls_sub-quantityinentryunit = ls_posub-requiredquantity - ls_posub-withdrawnquantity.
*              CONDENSE ls_sub-quantityinentryunit NO-GAPS.
*
*              ls_sub-materialdocumentparentline = lv_parent_id. " 父项目编码
*              lv_line_id = lv_line_id + 1. " 子项目编号
*              ls_sub-materialdocumentline = lv_line_id.
*              APPEND ls_sub TO lt_item.

              ENDLOOP.
            ENDIF.
          ENDIF.

        WHEN '261'."生产订单投料
          ls_data-goodsmovementcode = '03'.
          "获取预留
          IF ls_item-reservation  IS INITIAL.
            SELECT SINGLE
                   b~reservation,
                   b~reservationitem
              FROM i_reservationdocumentheader WITH PRIVILEGED ACCESS AS a
              JOIN i_reservationdocumentitem WITH PRIVILEGED ACCESS AS b ON a~reservation = b~reservation
             WHERE a~orderid = @ls_item-manufacturingorder
               AND b~product = @ls_item-material
              INTO (@ls_item-reservation, @ls_item-reservationitem ).
          ELSE.
            flag = 'E'.
            msg =  |行{ <fs_input>-outbillitemno }生产订单{ <fs_input>-manufacturingorder }物料{ <fs_input>-material }未找到对应的SAP预留行| .
            EXIT.
          ENDIF.
        WHEN '311' OR '541'."库存调拨
          ls_data-goodsmovementcode = '04'.
          ls_item-issgorrcvgbatch = ls_item-batch.
        WHEN '501'."无采购订单收货
          ls_data-goodsmovementcode = '01'.
          IF strlen( <fs_input>-batchwms ) <= 10.
            ls_item-batch = ls_valuetp-batch.
            <fs_input>-batch = ls_item-batch.
          ENDIF.
        WHEN OTHERS.
          ls_data-goodsmovementcode = '06'.
      ENDCASE.
      IF <fs_input>-manufacturedate IS NOT INITIAL.
        ls_item-manufacturedate = |{ <fs_input>-manufacturedate+0(4) }-{ <fs_input>-manufacturedate+4(2) }-{ <fs_input>-manufacturedate+6(2) }T00:00:00| .
      ELSE.
        CLEAR:ls_item-manufacturedate.
      ENDIF.
      APPEND ls_item TO lt_item.

    ENDLOOP.

    IF flag = 'E'.
      LOOP AT lt_input ASSIGNING <fs_input>.
        <fs_input>-flag = flag.
        <fs_input>-msg = msg.
      ENDLOOP.
      RETURN.
    ENDIF.

    ls_data-to_materialdocumentitem-results = lt_item.

*&---接口HTTP 链接调用
    TRY.
        DATA(lo_http_client) = cl_web_http_client_manager=>create_by_http_destination( lo_dest ).
        DATA(lo_request) = lo_http_client->get_http_request(   ).
        lo_http_client->enable_path_prefix( ).

        DATA(lv_uri_path) = |/API_MATERIAL_DOCUMENT_SRV/A_MaterialDocumentHeader?sap-language=zh|.
        lo_request->set_uri_path( EXPORTING i_uri_path = lv_uri_path ).
        lo_request->set_header_field( i_name = 'Accept' i_value = 'application/json' ).
        "lo_request->set_header_field( i_name = 'If-Match' i_value = '*' ).
        lo_http_client->set_csrf_token(  ).

        lo_request->set_content_type( 'application/json' ).
        "传入数据转JSON
        lv_json = /ui2/cl_json=>serialize(
              data          = ls_data
              compress      = abap_true
              name_mappings = lt_mapping ).

        lo_request->set_text( lv_json ).

*&---执行http post 方法
        DATA(lo_response) = lo_http_client->execute( if_web_http_client=>post ).
*&---获取http reponse 数据
        DATA(lv_res) = lo_response->get_text(  ).
*&---确定http 状态
        DATA(status) = lo_response->get_status( ).
        IF status-code = '201'.
          TYPES:BEGIN OF ty_heads,
                  materialdocument     TYPE string,
                  materialdocumentyear TYPE string,
                END OF ty_heads,
                BEGIN OF ty_ress,
                  d TYPE ty_heads,
                END OF  ty_ress.
          DATA:ls_ress TYPE ty_ress.
          /ui2/cl_json=>deserialize( EXPORTING json  = lv_res
                                      CHANGING data  = ls_ress ).

          flag  = 'S'.
          msg  = 'success'.
          lv_materialdocument = ls_ress-d-materialdocument.
          lv_materialdocumentyear = ls_ress-d-materialdocumentyear.
        ELSE.
          DATA:ls_rese TYPE zzs_odata_fail.
          /ui2/cl_json=>deserialize( EXPORTING json  = lv_res
                                      CHANGING data  = ls_rese ).
          flag = 'E'.
          msg = ls_rese-error-message-value .
          IF ls_rese-error-innererror-errordetails[] IS NOT INITIAL.
            LOOP AT ls_rese-error-innererror-errordetails ASSIGNING FIELD-SYMBOL(<fs_error>).
              msg = |{ msg }/{ <fs_error>-message  }|.
            ENDLOOP.
          ENDIF.

        ENDIF.
      CATCH cx_web_http_client_error INTO DATA(lx_web_http_client_error).
        flag = 'E'.
        msg  = '接口调用异常' && lx_web_http_client_error->get_longtext( ) .
    ENDTRY.

    IF flag = 'S'.

      SELECT a~materialdocumentheadertext AS outbillno,
             b~materialdocument,
             b~materialdocumentyear,
             b~materialdocumentitem,
             b~materialdocumentitemtext AS outbillitemno,
             b~batch
        FROM i_materialdocumentheader_2 WITH PRIVILEGED ACCESS AS a
       INNER JOIN i_materialdocumentitem_2 WITH PRIVILEGED ACCESS AS b
          ON a~materialdocument = b~materialdocument AND a~materialdocumentyear = b~materialdocumentyear
       WHERE a~materialdocument = @lv_materialdocument
         AND a~materialdocumentyear = @lv_materialdocumentyear
        INTO TABLE @DATA(lt_materialdocumentitem).

      DATA(lv_date1) = cl_abap_context_info=>get_system_date( ).
      DATA(lv_time) = cl_abap_context_info=>get_system_time( ).
      DATA(lv_user) = cl_abap_context_info=>get_user_technical_name( ).
      WAIT UP TO 1 SECONDS.
      LOOP AT lt_input ASSIGNING <fs_input>.
        CLEAR:ls_zztmm_0003.
        <fs_input>-created_date = lv_date1.
        <fs_input>-created_time = lv_time.
        <fs_input>-created_by   = lv_user.
        READ TABLE lt_materialdocumentitem INTO DATA(ls_materialdocumentitem) WITH KEY outbillno = <fs_input>-outbillno
                                                                                       outbillitemno = <fs_input>-outbillitemno.
        IF sy-subrc = 0.
          <fs_input>-materialdocument = ls_materialdocumentitem-materialdocument.
          <fs_input>-materialdocumentyear = ls_materialdocumentitem-materialdocumentyear.
          <fs_input>-materialdocumentitem = ls_materialdocumentitem-materialdocumentitem.
          <fs_input>-batch = ls_materialdocumentitem-batch.
          <fs_input>-flag = 'S'.
        ENDIF.
        IF <fs_input>-goodsmovementtype = '101' OR <fs_input>-goodsmovementtype = '501'.
          ls_zztmm_0003-material = <fs_input>-material.
          ls_zztmm_0003-batch = <fs_input>-batch.
          ls_zztmm_0003-zvension = <fs_input>-zvension.
          ls_zztmm_0003-zwmsbatch = <fs_input>-batchwms.
          zclui5_odata_util_0005=>post( CHANGING ls_input = ls_zztmm_0003 ).
        ENDIF.
      ENDLOOP.


      MOVE-CORRESPONDING lt_input TO lt_zztmm_0004.
      LOOP AT lt_zztmm_0004 ASSIGNING FIELD-SYMBOL(<fs_zztmm_0004>).
        TRY.
            <fs_zztmm_0004>-uuid16 = cl_system_uuid=>create_uuid_x16_static( ).
          CATCH cx_uuid_error.
        ENDTRY.
      ENDLOOP.
      MODIFY zztmm_0004 FROM TABLE @lt_zztmm_0004.
    ELSE.
      LOOP AT lt_input ASSIGNING <fs_input>.
        <fs_input>-flag = flag.
        <fs_input>-msg = msg.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
