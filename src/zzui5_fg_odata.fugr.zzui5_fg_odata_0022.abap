FUNCTION zzui5_fg_odata_0022.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(REQUEST_PARAMETER) TYPE  STRING
*"  EXPORTING
*"     VALUE(RETURN_CODE) TYPE  STRING
*"     VALUE(RETURN_MESSAGE) TYPE  STRING
*"     VALUE(RETURN_RESULT) TYPE  STRING
*"----------------------------------------------------------------------
  TYPES BEGIN OF ty_input.
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
  TYPES END OF ty_input.

  DATA:ls_req   TYPE ty_input,
       lt_input TYPE TABLE OF zztmm_0002.
  DATA:lv_where TYPE string,
       lv_flag  TYPE bapi_mtype,
       lv_msg   TYPE bapi_msg.

  "3. JSON报文转换为结构
  CALL METHOD /ui2/cl_json=>deserialize(
    EXPORTING
      json        = request_parameter
      pretty_name = /ui2/cl_json=>pretty_mode-none "格式化参数，NONE：字段名称大写
    CHANGING
      data        = ls_req ). "数据源CREATE OBJECT json_des.

  lt_input = ls_req-item[].
  IF lt_input[] IS NOT INITIAL.
    LOOP AT lt_input ASSIGNING FIELD-SYMBOL(<fs_input>).
      <fs_input>-outbillno   = ls_req-outbillno.
      <fs_input>-companycode = ls_req-companycode.
      <fs_input>-purchaseordertype = ls_req-purchaseordertype.
      <fs_input>-purchaseorderdate = ls_req-purchaseorderdate.
      <fs_input>-supplier = ls_req-supplier.
      <fs_input>-purchasingorganization = ls_req-purchasingorganization.
      <fs_input>-purchasinggroup = ls_req-purchasinggroup.
    ENDLOOP.
    zclui5_odata_util_0004=>check( CHANGING lt_input = lt_input
                                           flag = lv_flag
                                           msg  = lv_msg ).

    IF lv_flag NE 'E'.
      zclui5_odata_util_0004=>post( CHANGING lt_input = lt_input
                                         flag = lv_flag
                                         msg  = lv_msg ).
    ELSE.
      LOOP AT lt_input ASSIGNING <fs_input>.
        <fs_input>-flag = lv_flag.
        <fs_input>-msg  = lv_msg.
      ENDLOOP.
    ENDIF.
  ELSE.
    return_code = 'E'.
    return_message = '传入值为空'.
    RETURN.
  ENDIF.
  return_code = 'S'.
  return_message = '处理完成'.
  DATA(ls_out) = ls_req.
  ls_out-item = lt_input[].
  READ TABLE lt_input INTO DATA(ls_input_1) INDEX 1.
  IF sy-subrc = 0.
    ls_out-purchaseorder = ls_input_1-purchaseorder.
    ls_out-flag = ls_input_1-flag.
    ls_out-msg = ls_input_1-msg.
  ENDIF.
*----------------------------------------------------------------------*
* 生成传输JSON
*----------------------------------------------------------------------*
*&---序列化JSON SERIALIZE JSON
  /ui2/cl_json=>serialize(
            EXPORTING
                    data          = ls_out
                    compress      = abap_false
                    pretty_name   = /ui2/cl_json=>pretty_mode-low_case
            RECEIVING
                    r_json        = return_result ).






ENDFUNCTION.
