FUNCTION zzui5_fg_odata_0031.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(REQUEST_PARAMETER) TYPE  STRING
*"  EXPORTING
*"     VALUE(RETURN_CODE) TYPE  STRING
*"     VALUE(RETURN_MESSAGE) TYPE  STRING
*"     VALUE(RETURN_RESULT) TYPE  STRING
*"----------------------------------------------------------------------

  DATA:lt_input     TYPE TABLE OF zztsd_0003,
       lt_input_one TYPE TABLE OF zztsd_0003.
  DATA:lv_where TYPE string,
       lv_flag  TYPE bapi_mtype,
       lv_msg   TYPE bapi_msg.

  "3. JSON报文转换为结构
  CALL METHOD /ui2/cl_json=>deserialize(
    EXPORTING
      json        = request_parameter
      pretty_name = /ui2/cl_json=>pretty_mode-none "格式化参数，NONE：字段名称大写
    CHANGING
      data        = lt_input ). "数据源CREATE OBJECT json_des.


  IF lt_input[] IS NOT INITIAL.
    zclui5_odata_util_0008=>check( CHANGING lt_input = lt_input
                                            flag = lv_flag
                                            msg  = lv_msg ).

    IF lv_flag NE 'E'.

      DATA(lt_head) = lt_input[].
      DELETE lt_head WHERE FLAG = 'E'.
      SORT lt_head BY purchaseorderbycustomer underlyingpurchaseorderitem.
      DELETE ADJACENT DUPLICATES FROM lt_head COMPARING purchaseorderbycustomer.

      LOOP AT lt_head INTO DATA(ls_head).
        CLEAR:lt_input_one[].
        LOOP AT lt_input INTO DATA(ls_input) WHERE purchaseorderbycustomer = ls_head-purchaseorderbycustomer.
          APPEND INITIAL LINE TO lt_input_one ASSIGNING FIELD-SYMBOL(<fs_input_one>).
          MOVE-CORRESPONDING ls_input TO <fs_input_one>.
        ENDLOOP.
        zclui5_odata_util_0008=>post( CHANGING lt_input = lt_input_one
                                               flag = lv_flag
                                               msg  = lv_msg ).
        LOOP AT lt_input_one INTO DATA(ls_input_one).
          READ TABLE lt_input ASSIGNING FIELD-SYMBOL(<fs_input>) WITH KEY purchaseorderbycustomer = ls_input_one-purchaseorderbycustomer
                                                                          underlyingpurchaseorderitem = ls_input_one-underlyingpurchaseorderitem.
          IF sy-subrc = 0.
            MOVE-CORRESPONDING ls_input_one TO <fs_input>.
          ENDIF.
        ENDLOOP.
      ENDLOOP.

    ELSE.
      return_code = 'S'.
      return_message = '导入订单存在基础检查错误，请查看错误明细修复后重新导入'.
    ENDIF.
  ELSE.
    return_code = 'E'.
    return_message = '传入值为空'.
    RETURN.
  ENDIF.
  return_code = 'S'.
  return_message = '处理完成,处理明细请查看行信息'.
  DATA(lt_out) = lt_input.
*----------------------------------------------------------------------*
* 生成传输JSON
*----------------------------------------------------------------------*
*&---序列化JSON SERIALIZE JSON
  /ui2/cl_json=>serialize(
            EXPORTING
                    data          = lt_out
                    compress      = abap_false
                    pretty_name   = /ui2/cl_json=>pretty_mode-low_case
            RECEIVING
                    r_json        = return_result ).






ENDFUNCTION.
