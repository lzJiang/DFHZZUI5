FUNCTION zzui5_fg_odata_0015.
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
          productionplant   TYPE zc_pp007-productionplant,
          zlllx             TYPE zc_pp007-zlllx,
          zllzt             TYPE zc_pp007-zllzt,
          zllno             TYPE zc_pp007-zllno,
          zcreate_datestart TYPE zc_pp007-zcreate_date,
          zcreate_dateend   TYPE zc_pp007-zcreate_date,
          zjname            TYPE zc_pp007-zjname,
          zcjtext           TYPE zc_pp007-zcjtext,
        END OF ty_input.

  DATA:ls_input TYPE ty_input.
  DATA:lv_year  TYPE string,
       lv_month TYPE string,
       lv_day   TYPE string.
  DATA:lv_where TYPE string.

  DATA(lv_user) = cl_abap_context_info=>get_user_technical_name( ).
*  lv_where = | zllno ne ''|.
  IF lv_user NE 'CB9980000010'.
    lv_where = | zcreate_user = '{ lv_user }'|.
  ELSE.
    lv_where = | zllno ne ''|.
  ENDIF.

  "3. JSON报文转换为结构
  CALL METHOD /ui2/cl_json=>deserialize(
    EXPORTING
      json        = request_parameter
      pretty_name = /ui2/cl_json=>pretty_mode-none "格式化参数，NONE：字段名称大写
    CHANGING
      data        = ls_input ). "数据源CREATE OBJECT json_des.

  IF ls_input-productionplant IS NOT INITIAL.
    lv_where = | { lv_where } and productionplant = '{ ls_input-productionplant }'|.
  ENDIF.
  IF ls_input-zlllx IS NOT INITIAL.
    lv_where = | { lv_where } and zlllx = '{ ls_input-zlllx }'|.
  ENDIF.
  IF ls_input-zllzt IS NOT INITIAL.
    lv_where = | { lv_where } and zllzt = '{ ls_input-zllzt }'|.
  ENDIF.
  IF ls_input-zllno IS NOT INITIAL.
    lv_where = | { lv_where } and zllno like '%{ ls_input-zllno }%'|.
  ENDIF.
  IF ls_input-zcreate_datestart IS NOT INITIAL.
    lv_where = | { lv_where } and zcreate_date >= '{ ls_input-zcreate_datestart }'|.
  ENDIF.
  IF ls_input-zcreate_dateend IS NOT INITIAL.
    lv_where = | { lv_where } and zcreate_date <= '{ ls_input-zcreate_dateend }'|.
  ENDIF.
  IF ls_input-zjname IS NOT INITIAL.
    lv_where = | { lv_where } and zjname like '%{ ls_input-zllno }%'|.
  ENDIF.
  IF ls_input-zcjtext IS NOT INITIAL.
    lv_where = | { lv_where } and zcjtext like '%{ ls_input-zcjtext }%'|.
  ENDIF.


  SELECT DISTINCT zllno
             FROM zc_pp007
            WHERE (lv_where)
              INTO TABLE @DATA(lt_zllno).
  IF lt_zllno[] IS INITIAL.
    return_code = 'E'.
    return_message = '未查询到有效数据！'.
    RETURN.
  ENDIF.

  SELECT *
    FROM zc_pp007
    FOR ALL ENTRIES IN @lt_zllno
    WHERE zllno = @lt_zllno-zllno
    INTO TABLE @DATA(lt_out).

  SORT lt_out BY zllno zllitemno.
  IF lt_out[] IS NOT INITIAL.
    LOOP AT lt_out ASSIGNING FIELD-SYMBOL(<fs_out>).
      <fs_out>-cp = |{ <fs_out>-cp ALPHA = OUT }|.
      <fs_out>-zj = |{ <fs_out>-zj ALPHA = OUT }|.
      IF <fs_out>-storagelocationname IS INITIAL.
        "退料接收库位设置描述
        <fs_out>-storagelocationname = <fs_out>-storagelocation.
      ENDIF.
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
