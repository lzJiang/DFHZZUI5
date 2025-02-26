FUNCTION zzui5_fg_odata_0021.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(REQUEST_PARAMETER) TYPE  STRING
*"  EXPORTING
*"     VALUE(RETURN_CODE) TYPE  STRING
*"     VALUE(RETURN_MESSAGE) TYPE  STRING
*"     VALUE(RETURN_RESULT) TYPE  STRING
*"----------------------------------------------------------------------
  TYPES:BEGIN OF ty_input,
          outbillno              TYPE zztmm_0002-outbillno,
          companycode            TYPE zztmm_0002-companycode,
          supplier               TYPE zztmm_0002-supplier,
          purchaseordertype      TYPE zztmm_0002-purchaseordertype,
          purchaseorderdatestart TYPE zztmm_0002-purchaseorderdate,
          purchaseorderdateend   TYPE zztmm_0002-purchaseorderdate,
        END OF ty_input.
  TYPES BEGIN OF ty_output.
  TYPES: outbillno              TYPE zztmm_0002-outbillno,
         purchaseorder          TYPE zztmm_0002-purchaseorder,
         companycode            TYPE zztmm_0002-companycode,
         purchaseordertype      TYPE zztmm_0002-purchaseordertype,
         purchaseorderdate      TYPE zztmm_0002-purchaseorderdate,
         supplier               TYPE zztmm_0002-supplier,
         purchasingorganization TYPE zztmm_0002-purchasingorganization,
         purchasinggroup        TYPE zztmm_0002-purchasinggroup,
         flag                   TYPE zztmm_0002-flag,
         msg                    TYPE zztmm_0002-msg,
         item                   TYPE zztmm_0002_tab.
  TYPES END OF ty_output.

  DATA:ls_input TYPE ty_input,
       lt_out   TYPE TABLE OF ty_output.
  DATA:lv_where TYPE string.

  lv_where = | purchaseorder ne '' and flag = 'S'|.

  "3. JSON报文转换为结构
  CALL METHOD /ui2/cl_json=>deserialize(
    EXPORTING
      json        = request_parameter
      pretty_name = /ui2/cl_json=>pretty_mode-low_case "格式化参数，NONE：字段名称大写
    CHANGING
      data        = ls_input ). "数据源CREATE OBJECT json_des.

  IF ls_input-outbillno IS NOT INITIAL.
    lv_where = | { lv_where } and outbillno like '%{ ls_input-outbillno }%'|.
  ENDIF.
  IF ls_input-companycode IS NOT INITIAL.
    lv_where = | { lv_where } and companycode like '%{ ls_input-companycode }%'|.
  ENDIF.
  IF ls_input-supplier IS NOT INITIAL.
    lv_where = | { lv_where } and supplier like '%{ ls_input-supplier }%'|.
  ENDIF.
  IF ls_input-purchaseordertype IS NOT INITIAL.
    lv_where = | { lv_where } and purchaseordertype like '%{ ls_input-purchaseordertype }%'|.
  ENDIF.
  IF ls_input-purchaseorderdatestart IS NOT INITIAL.
    lv_where = | { lv_where } and purchaseorderdate >= '{ ls_input-purchaseorderdatestart }'|.
  ENDIF.
  IF ls_input-purchaseorderdateend IS NOT INITIAL.
    lv_where = | { lv_where } and purchaseorderdate <= '{ ls_input-purchaseorderdateend }'|.
  ENDIF.

  SELECT DISTINCT outbillno
    FROM zztmm_0002
    WHERE (lv_where)
    INTO TABLE @DATA(lt_outbillno).
  IF lt_outbillno[] IS NOT INITIAL.
    SELECT *
    FROM zztmm_0002
    FOR ALL ENTRIES IN @lt_outbillno[]
    WHERE outbillno = @lt_outbillno-outbillno
      AND flag = 'S'
    INTO TABLE @DATA(lt_item).
  ENDIF.

  IF lt_item[] IS NOT INITIAL.
    DATA(lt_head) = lt_item[].
    SORT lt_head BY purchaseorder.
    DELETE ADJACENT DUPLICATES FROM lt_head COMPARING purchaseorder.

    LOOP AT lt_head INTO DATA(ls_head).
      APPEND INITIAL LINE TO lt_out ASSIGNING FIELD-SYMBOL(<fs_out>).
      MOVE-CORRESPONDING ls_head TO <fs_out>.
      LOOP AT lt_item INTO DATA(ls_item) WHERE purchaseorder = ls_head-purchaseorder.
        APPEND INITIAL LINE TO <fs_out>-item ASSIGNING FIELD-SYMBOL(<fs_item>).
        MOVE-CORRESPONDING ls_item TO <fs_item>.
      ENDLOOP.
    ENDLOOP.

    DATA(lv_lines) = lines( lt_out ).
    return_code = 'S'.
    return_message = |共查询{ lv_lines }条数据！|.
  ELSE.
    return_code = 'E'.
    return_message = '未查询到有效数据！'.
    RETURN.
  ENDIF.
*----------------------------------------------------------------------*
* 生成传输JSON
*----------------------------------------------------------------------*
*&---序列化JSON SERIALIZE JSON
  /ui2/cl_json=>serialize(
            EXPORTING
                    data          = lt_out[]
                    compress      = abap_false
                    pretty_name   = /ui2/cl_json=>pretty_mode-low_case
            RECEIVING
                    r_json        = return_result ).


ENDFUNCTION.
