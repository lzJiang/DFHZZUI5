FUNCTION zzui5_fg_odata_0012.
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
          productionplant        TYPE zc_pp005-productionplant,
          manufacturingorder     TYPE string,
          yy1_mfgbatch_ord       TYPE string,
          cpname                 TYPE string,
          manufacturingordertype TYPE string,
          zjtype                 TYPE string,
        END OF ty_input.
  TYPES:BEGIN OF ty_collect,
          reservation     TYPE zzt_pp_002_itemd-reservation,
          reservationitem TYPE zzt_pp_002_itemd-reservationitem,
          requestedqty    TYPE zzt_pp_002_itemd-requestedqty,
        END OF ty_collect.

  DATA:ls_input   TYPE ty_input,
       ls_collect TYPE ty_collect,
       lt_collect TYPE TABLE OF ty_collect.
  DATA:lv_year  TYPE string,
       lv_month TYPE string,
       lv_day   TYPE string.
  DATA:lv_where                 TYPE string,
       lv_in_manufacturingorder TYPE string,
       lv_in_yy1_mfgbatch_ord   TYPE string,
       lv_manufacturingorder    TYPE zc_pp005-manufacturingorder,
       lv_yy1_mfgbatch_ord      TYPE zc_pp005-yy1_mfgbatch_ord,
       lv_zcy                   TYPE zc_pp005-requestedqty.


  lv_where = | manufacturingorder ne ''|.


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
  IF ls_input-manufacturingorder IS NOT INITIAL.
    SPLIT ls_input-manufacturingorder AT ';' INTO TABLE DATA(lt_manufacturingorder).
    LOOP AT lt_manufacturingorder INTO DATA(manufacturingorder).
      lv_manufacturingorder = |{ manufacturingorder ALPHA = IN }|.
      IF lv_in_manufacturingorder IS INITIAL.
        lv_in_manufacturingorder = |'{ lv_manufacturingorder }'|.
      ELSE.
        lv_in_manufacturingorder = |{ lv_in_manufacturingorder },'{ lv_manufacturingorder }'|.
      ENDIF.
    ENDLOOP.
    IF lv_in_manufacturingorder IS NOT INITIAL.
      lv_where = | { lv_where } and manufacturingorder in ({ lv_in_manufacturingorder })|.
    ENDIF.
  ENDIF.
  IF ls_input-yy1_mfgbatch_ord IS NOT INITIAL.
    SPLIT ls_input-yy1_mfgbatch_ord AT ';' INTO TABLE DATA(lt_yy1_mfgbatch_ord).
    LOOP AT lt_yy1_mfgbatch_ord INTO DATA(yy1_mfgbatch_ord).
      lv_yy1_mfgbatch_ord = yy1_mfgbatch_ord.
      IF lv_in_yy1_mfgbatch_ord IS INITIAL.
        lv_in_yy1_mfgbatch_ord = |'{ lv_yy1_mfgbatch_ord }'|.
      ELSE.
        lv_in_yy1_mfgbatch_ord = |{ lv_in_yy1_mfgbatch_ord },'{ lv_yy1_mfgbatch_ord }'|.
      ENDIF.
    ENDLOOP.
    IF lv_in_yy1_mfgbatch_ord IS NOT INITIAL.
      lv_where = | { lv_where } and yy1_mfgbatch_ord in ({ lv_in_yy1_mfgbatch_ord })|.
    ENDIF.
  ENDIF.
  IF ls_input-cpname IS NOT INITIAL.
    lv_where = | { lv_where } and cpname like '%{ ls_input-cpname }%'|.
  ENDIF.
  IF ls_input-manufacturingordertype IS NOT INITIAL.
    lv_where = | { lv_where } and manufacturingordertype = '{ ls_input-manufacturingordertype }'|.
  ENDIF.
  IF ls_input-zjtype IS NOT INITIAL.
    CASE ls_input-zjtype.
      WHEN 'YFL'.
        lv_where = | { lv_where } and zjtype = 'Z003'|.
      WHEN 'QT'.
        lv_where = | { lv_where } and zjtype NE 'Z003'|.
    ENDCASE.

  ENDIF.
  SELECT *
    FROM zc_pp005
    WHERE (lv_where)
    INTO TABLE @DATA(lt_out).

  SORT lt_out BY manufacturingorder reservation reservationitem.
  IF lt_out[] IS NOT INITIAL.
    SELECT b~*
      FROM zzt_pp_002_head WITH PRIVILEGED ACCESS AS a
      INNER JOIN zzt_pp_002_itemd WITH PRIVILEGED ACCESS AS b
      ON a~zllno = b~zllno
      FOR ALL ENTRIES IN @lt_out
      WHERE b~reservation = @lt_out-reservation
        AND b~reservationitem = @lt_out-reservationitem
        AND a~zllzt NE 'DELETE'
        INTO TABLE @DATA(lt_itemd).
    LOOP AT lt_itemd INTO DATA(ls_itemd).
      ls_collect-reservation = ls_itemd-reservation.
      ls_collect-reservationitem = ls_itemd-reservationitem.
      ls_collect-requestedqty = ls_itemd-requestedqty.
      COLLECT ls_collect INTO lt_collect.
      CLEAR:ls_collect.
    ENDLOOP.
    SORT lt_collect BY reservation reservationitem.

    LOOP AT lt_out ASSIGNING FIELD-SYMBOL(<fs_out>).
      <fs_out>-manufacturingorder = |{ <fs_out>-manufacturingorder ALPHA = OUT }|.
      <fs_out>-cp = |{ <fs_out>-cp ALPHA = OUT }|.
      <fs_out>-zj = |{ <fs_out>-zj ALPHA = OUT }|.
      READ TABLE lt_collect INTO ls_collect WITH KEY reservation = <fs_out>-reservation
                                                     reservationitem = <fs_out>-reservationitem BINARY SEARCH.
      IF sy-subrc = 0.
        "已申领数量
        <fs_out>-zyslsl = ls_collect-requestedqty.
        <fs_out>-requestedqty = <fs_out>-zjqty - <fs_out>-zyslsl.
        IF <fs_out>-requestedqty < 0.
          CLEAR:<fs_out>-requestedqty.
        ENDIF.
      ENDIF.
      TRY.
          CONDENSE <fs_out>-zcy NO-GAPS.
          lv_zcy = <fs_out>-zcy.
*          DATA(lv_zcy_str) = zcl_com_util=>get_zcy( <fs_out>-zcy ).
*          lv_zcy = lv_zcy_str.
          lv_zcy = lv_zcy * ( <fs_out>-cpqty / <fs_out>-zjbsl ) * ( <fs_out>-requestedqty / <fs_out>-zjQty ).
          <fs_out>-zcy = lv_zcy.
          CONDENSE <fs_out>-zcy NO-GAPS.
        CATCH cx_root INTO DATA(lr_err).
      ENDTRY.
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
