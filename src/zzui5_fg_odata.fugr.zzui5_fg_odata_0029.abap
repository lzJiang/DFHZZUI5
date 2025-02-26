FUNCTION zzui5_fg_odata_0029.
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
          manufacturingorder TYPE zc_pp002-manufacturingorder,
        END OF ty_input.
  TYPES:BEGIN OF ty_out,
          manufacturingorder TYPE zc_pp002-manufacturingorder,
          yy1_mfgbatch_ord   TYPE zc_pp002-yy1_mfgbatch_ord,
          cp                 TYPE zc_pp002-material,
          cpname             TYPE zc_pp002-productname,
          yy1_mfgdate        TYPE zc_pp002-yy1_mfgdate,
        END OF ty_out.

  DATA:ls_input TYPE ty_input,
       ls_out   TYPE ty_out.
  DATA:lv_year  TYPE string,
       lv_month TYPE string,
       lv_day   TYPE string.
  DATA:lv_where TYPE string.

  lv_where = | manufacturingorder ne ''|.


  "3. JSON报文转换为结构
  CALL METHOD /ui2/cl_json=>deserialize(
    EXPORTING
      json        = request_parameter
      pretty_name = /ui2/cl_json=>pretty_mode-none "格式化参数，NONE：字段名称大写
    CHANGING
      data        = ls_input ). "数据源CREATE OBJECT json_des.
  MOVE-CORRESPONDING ls_input TO ls_out.
  ls_input-manufacturingorder = |{ ls_input-manufacturingorder ALPHA = IN }|.
  SELECT SINGLE manufacturingorder,
                yy1_mfgbatch_ord,
                material AS cp,
                productname AS cpname,
                yy1_mfgdate
           FROM zc_pp002 WITH PRIVILEGED ACCESS
          WHERE manufacturingorder = @ls_input-manufacturingorder
           INTO CORRESPONDING FIELDS OF @ls_out.

  IF ls_out IS NOT INITIAL.
    ls_out-manufacturingorder = |{ ls_out-manufacturingorder ALPHA = OUT }|.
    ls_out-cp = |{ ls_out-cp ALPHA = OUT }|.
    return_code = 'S'.
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
                    data          = ls_out
                    compress      = abap_false
                    pretty_name   = /ui2/cl_json=>pretty_mode-low_case
            RECEIVING
                    r_json        = return_result ).






ENDFUNCTION.
