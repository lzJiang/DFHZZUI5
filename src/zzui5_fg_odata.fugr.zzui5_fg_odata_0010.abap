FUNCTION zzui5_fg_odata_0010.
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
  TYPES:pk_no                  TYPE zztfi_0003-pk_no,
        accountingdocument     TYPE zztfi_0003-accountingdocument,
        companycode            TYPE zztfi_0003-companycode,
        accountingdocumenttype TYPE zztfi_0003-accountingdocumenttype,
        documentdate           TYPE zztfi_0003-documentdate,
        postingdate            TYPE zztfi_0003-postingdate,
        documentheadertext     TYPE zztfi_0003-documentheadertext,
        transactioncurrency    TYPE zztfi_0003-transactioncurrency,
        jxje                   TYPE zztfi_0003-amountingroupcurrency,
        dxje                   TYPE zztfi_0003-amountingroupcurrency,
        customer               TYPE zztfi_0003-customer,
        flag                   TYPE zztfi_0003-flag,
        msg                    TYPE zztfi_0003-msg,
        item                   TYPE zzt_zztfi_0003.
  TYPES END OF ty_input.

  DATA:ls_req   TYPE ty_input,
       lt_input TYPE TABLE OF zztfi_0003.
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
*    LOOP AT lt_input ASSIGNING FIELD-SYMBOL(<fs_input>).
*      CLEAR:<fs_input>-flag,<fs_input>-msg.
    LOOP AT lt_input ASSIGNING FIELD-SYMBOL(<fs_input>).
      <fs_input>-pk_no = ls_req-pk_no.
      <fs_input>-companycode = ls_req-companycode.
      <fs_input>-accountingdocumenttype = ls_req-accountingdocumenttype.
      <fs_input>-documentdate = ls_req-documentdate.
      <fs_input>-postingdate = ls_req-postingdate.
      <fs_input>-documentheadertext = ls_req-documentheadertext.
      <fs_input>-transactioncurrency = ls_req-transactioncurrency.
    ENDLOOP.
    zclui5_odata_util_0002=>post( CHANGING lt_input = lt_input
                                           flag = lv_flag
                                           msg  = lv_msg ).
*    ENDLOOP.
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
    ls_out-accountingdocument = ls_input_1-accountingdocument.
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
