FUNCTION zzui5_fg_odata_0018.
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
          product     TYPE i_producttext-product,
          productname TYPE i_producttext-productname,
        END OF ty_input.

  DATA:ls_input TYPE ty_input.
  DATA:lv_where TYPE string.

  lv_where = | product ne '' and Language = '1' |.

  "3. JSON报文转换为结构
  CALL METHOD /ui2/cl_json=>deserialize(
    EXPORTING
      json        = request_parameter
      pretty_name = /ui2/cl_json=>pretty_mode-none "格式化参数，NONE：字段名称大写
    CHANGING
      data        = ls_input ). "数据源CREATE OBJECT json_des.

  IF ls_input-product IS NOT INITIAL.
    lv_where = | { lv_where } and product like '%{ ls_input-product }%'|.
  ENDIF.
  IF ls_input-productname IS NOT INITIAL.
    lv_where = | { lv_where } or productname like '%{ ls_input-productname }%'|.
  ENDIF.

  SELECT product,
         productname
    FROM i_producttext WITH PRIVILEGED ACCESS
    WHERE (lv_where)
    INTO TABLE @DATA(lt_out).

  SORT lt_out BY product.
  IF lt_out[] IS NOT INITIAL.
    LOOP AT lt_out ASSIGNING FIELD-SYMBOL(<fs_out>).
      <fs_out>-product = |{ <fs_out>-product ALPHA = OUT }|.
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
