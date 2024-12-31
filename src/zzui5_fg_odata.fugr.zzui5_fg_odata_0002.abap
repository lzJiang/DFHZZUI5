FUNCTION zzui5_fg_odata_0002.
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
          manufacturingorder TYPE i_manufacturingorder-manufacturingorder,
        END OF ty_input.

  DATA:in_input TYPE ty_input.

  "3. JSON报文转换为结构
  CALL METHOD /ui2/cl_json=>deserialize(
    EXPORTING
      json        = request_parameter
      pretty_name = /ui2/cl_json=>pretty_mode-none "格式化参数，NONE：字段名称大写
    CHANGING
      data        = in_input ). "数据源CREATE OBJECT json_des.

  IF in_input-manufacturingorder IS INITIAL.
    return_code = 'E'.
    return_message = '传入订单ID不能为空！'.
    RETURN.
  ENDIF.

  in_input-manufacturingorder = |{ in_input-manufacturingorder ALPHA = IN }|.
  SELECT SINGLE *
     FROM i_manufacturingorder WITH PRIVILEGED ACCESS
     WHERE manufacturingorder = @in_input-manufacturingorder
     INTO @DATA(ls_manufacturingorder) .


  IF ls_manufacturingorder IS NOT INITIAL.
    return_code = 'S'.
    return_message = '获取成功！'.
  ELSE.
    return_code = 'E'.
    return_message = '获取失败！'.
    RETURN.
  ENDIF.
*----------------------------------------------------------------------*
* 生成传输JSON
*----------------------------------------------------------------------*
*&---序列化JSON SERIALIZE JSON
  /ui2/cl_json=>serialize(
            EXPORTING
                    data          = ls_manufacturingorder
                    compress      = abap_false
                    pretty_name   = /ui2/cl_json=>pretty_mode-low_case
            RECEIVING
                    r_json        = return_result ).


ENDFUNCTION.
