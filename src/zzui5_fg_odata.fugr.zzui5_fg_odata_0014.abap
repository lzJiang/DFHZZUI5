FUNCTION zzui5_fg_odata_0014.
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
          productionplant TYPE zc_pp006-productionplant,
          zj              TYPE i_materialstock_2-material,
          storagelocation TYPE i_materialstock_2-storagelocation,
        END OF ty_input.
  TYPES:BEGIN OF ty_out,
          productionplant  TYPE zc_pp006-productionplant,
          zj               TYPE i_materialstock_2-material,
          zjname           TYPE i_producttext-productname,
          storagelocation  TYPE i_materialstock_2-storagelocation,
          sykc             TYPE i_materialstock_2-matlwrhsstkqtyinmatlbaseunit,
          requestedqtyunit TYPE i_unitofmeasure-unitofmeasure,
          zjunit           TYPE i_unitofmeasure-unitofmeasure_e,
        END OF ty_out.

  DATA:ls_input TYPE ty_input,
       ls_out   TYPE ty_out.
  DATA:lv_year  TYPE string,
       lv_month TYPE string,
       lv_day   TYPE string.
  DATA:lv_where TYPE string.

  lv_where = | purchaseorder ne ''|.


  "3. JSON报文转换为结构
  CALL METHOD /ui2/cl_json=>deserialize(
    EXPORTING
      json        = request_parameter
      pretty_name = /ui2/cl_json=>pretty_mode-none "格式化参数，NONE：字段名称大写
    CHANGING
      data        = ls_input ). "数据源CREATE OBJECT json_des.
  MOVE-CORRESPONDING ls_input TO ls_out.
  zcl_com_util=>matnr_zero_in( EXPORTING input = ls_input-zj
                               IMPORTING output = ls_input-zj ).
  SELECT SINGLE a~productname AS zjname,
                b~baseunit AS requestedqtyunit,
                c~unitofmeasure_e AS zjunit
           FROM i_producttext WITH PRIVILEGED ACCESS AS a
           INNER JOIN i_product WITH PRIVILEGED ACCESS AS b
           ON a~product = b~product
           INNER JOIN i_unitofmeasure WITH PRIVILEGED ACCESS AS c
           ON b~baseunit = c~unitofmeasure
          WHERE a~product = @ls_input-zj
           AND  language = '1'
           INTO CORRESPONDING FIELDS OF @ls_out.

  IF ls_input-storagelocation IS NOT INITIAL.
    SELECT SUM( a~matlwrhsstkqtyinmatlbaseunit ) AS sykc
           FROM i_materialstock_2 WITH PRIVILEGED ACCESS AS a
           WHERE a~material = @ls_input-zj
             AND a~storagelocation = @ls_input-storagelocation
             AND a~plant = @ls_input-productionplant
           INTO CORRESPONDING FIELDS OF @ls_out.
  ENDIF.


  IF ls_out IS NOT INITIAL.
    ls_out-zj = |{ ls_out-zj ALPHA = OUT }|.
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
