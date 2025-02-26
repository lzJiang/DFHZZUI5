CLASS zclui5_odata_util DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS release
      CHANGING ls_input TYPE zc_pp002
               flag     TYPE bapi_mtype
               msg      TYPE bapi_msg.
    CLASS-METHODS get_msg
      IMPORTING ls_failed    TYPE any
                ls_reported  TYPE any
                lv_component TYPE string
      EXPORTING msg          TYPE bapi_msg.
    CLASS-METHODS senwms
      CHANGING ls_input TYPE zc_pp002
               flag     TYPE bapi_mtype
               msg      TYPE bapi_msg.
    CLASS-METHODS modifyorder
      CHANGING ls_input TYPE zc_pp002
               flag     TYPE bapi_mtype
               msg      TYPE bapi_msg.
    CLASS-METHODS update_zzt_pp_001
      IMPORTING ls_input TYPE zzt_ui5_odata.
    CLASS-METHODS sendbpm
      CHANGING ls_input TYPE zc_pp002
               flag     TYPE bapi_mtype
               msg      TYPE bapi_msg.
    CLASS-METHODS getbpmuserinfo
      CHANGING  ls_input           TYPE zc_pp002
                flag               TYPE bapi_mtype
                msg                TYPE bapi_msg
      RETURNING VALUE(rs_userinfo) TYPE zzs_bpm_userinfo.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCLUI5_ODATA_UTIL IMPLEMENTATION.


  METHOD getbpmuserinfo.
    TYPES:BEGIN OF ty_res,
            success  TYPE abap_bool,
            userinfo TYPE zzs_bpm_userinfo,
          END OF ty_res.

    DATA:lo_oref TYPE REF TO object.
    DATA:lv_oref TYPE zzefname,
         lt_ptab TYPE abap_parmbind_tab.
    DATA:lv_numb TYPE zzenumb VALUE 'PP004'.
    DATA:lv_data TYPE string.
    DATA:lv_msgty TYPE bapi_mtype,
         lv_msgtx TYPE bapi_msg,
         lv_resp  TYPE string,
         ls_res   TYPE ty_res.

    DATA(lv_s4_userid) = cl_abap_context_info=>get_user_technical_name( ).
    SELECT SINGLE *
             FROM i_businessuservh WITH PRIVILEGED ACCESS
            WHERE userid = @lv_s4_userid
             INTO @DATA(ls_businessuser).
    DATA(lv_email) = ls_businessuser-defaultemailaddress.
    SPLIT lv_email AT '@' INTO TABLE DATA(lt_email).
    IF sy-subrc = 0.
      READ TABLE lt_email INTO DATA(ls_email) INDEX 1.
      DATA(lv_bpmuseraccount) = ls_email.
    ENDIF.

    "test
    lv_bpmuseraccount = 'jinjinzhao'.

    "获取调用类
    SELECT SINGLE zzcname
      FROM zr_vt_rest_conf
     WHERE zznumb = @lv_numb
      INTO @lv_oref.
    IF lv_oref IS INITIAL.
      flag = 'E'.
      msg = |推送BPM失败，未配置接口PP004|.
      RETURN.
    ENDIF.
    lt_ptab = VALUE #( ( name  = 'IV_NUMB' kind  = cl_abap_objectdescr=>exporting value = REF #( lv_numb ) ) ).
    TRY .
        CREATE OBJECT lo_oref TYPE (lv_oref) PARAMETER-TABLE lt_ptab.
        CALL METHOD lo_oref->('OUTBOUND_NO_LOG_SET')
          EXPORTING
            iv_data   = lv_data
            iv_user   = lv_bpmuseraccount
            iv_method = 'GetUserInfo'
          CHANGING
            ev_resp   = lv_resp
            ev_msgty  = lv_msgty
            ev_msgtx  = lv_msgtx.
      CATCH cx_root INTO DATA(lr_root).

    ENDTRY.

    IF lv_msgty = 'S'.
      flag = 'S'.

      /ui2/cl_json=>deserialize( EXPORTING json        = lv_resp
                                           pretty_name = /ui2/cl_json=>pretty_mode-none
                            CHANGING  data        = ls_res ).
    ELSE.
      flag = 'E'.
      msg = |获取BPM用户信息失败:{ lv_msgtx }|.
      RETURN.
    ENDIF.
  ENDMETHOD.


  METHOD get_msg.
    DATA:lv_msg     TYPE bapi_msg,
         ls_t100key TYPE scx_t100key,
         lv_msgid   TYPE symsgid,
         lv_msgno   TYPE symsgno,
         lv_msgv1   LIKE sy-msgv1,
         lv_msgv2   LIKE sy-msgv2,
         lv_msgv3   LIKE sy-msgv3,
         lv_msgv4   LIKE sy-msgv4.

    FIELD-SYMBOLS:<fs_tab>     TYPE STANDARD TABLE,
                  <fs>         TYPE any,
                  <fs_msg>     TYPE REF TO if_abap_behv_message,
                  <fs_t100key> TYPE any.

    ASSIGN COMPONENT lv_component OF STRUCTURE ls_reported TO <fs_tab>.
    IF <fs_tab> IS ASSIGNED AND <fs_tab> IS NOT INITIAL.

      LOOP AT <fs_tab> ASSIGNING <fs>.
        CLEAR:lv_msgid,lv_msgno,lv_msgv1,lv_msgv2,lv_msgv3,lv_msgv3.
        ASSIGN COMPONENT '%MSG' OF STRUCTURE <fs> TO <fs_msg>.
        IF <fs_msg> IS ASSIGNED AND <fs_msg> IS NOT INITIAL.
          ASSIGN <fs_msg>->('IF_T100_MESSAGE~T100KEY') TO <fs_t100key>.
          IF <fs_t100key> IS ASSIGNED AND <fs_t100key> IS NOT INITIAL.
            ASSIGN COMPONENT 'MSGID' OF STRUCTURE <fs_t100key> TO FIELD-SYMBOL(<fs_msgid>).
            ASSIGN COMPONENT 'MSGNO' OF STRUCTURE <fs_t100key> TO FIELD-SYMBOL(<fs_msgno>).
          ENDIF.
          ASSIGN <fs_msg>->('IF_T100_DYN_MSG~MSGV1') TO FIELD-SYMBOL(<fs_msgv1>).
          ASSIGN <fs_msg>->('IF_T100_DYN_MSG~MSGV2') TO FIELD-SYMBOL(<fs_msgv2>).
          ASSIGN <fs_msg>->('IF_T100_DYN_MSG~MSGV3') TO FIELD-SYMBOL(<fs_msgv3>).
          ASSIGN <fs_msg>->('IF_T100_DYN_MSG~MSGV4') TO FIELD-SYMBOL(<fs_msgv4>).
          IF <fs_msgid> IS ASSIGNED AND <fs_msgid> IS NOT INITIAL.
            lv_msgid = <fs_msgid>.
          ENDIF.
          IF <fs_msgno> IS ASSIGNED AND <fs_msgno> IS NOT INITIAL.
            lv_msgno = <fs_msgno>.
          ENDIF.
          IF <fs_msgv1> IS ASSIGNED AND <fs_msgv1> IS NOT INITIAL.
            lv_msgv1 = <fs_msgv1>.
          ENDIF.
          IF <fs_msgv2> IS ASSIGNED AND <fs_msgv2> IS NOT INITIAL.
            lv_msgv2 = <fs_msgv2>.
          ENDIF.
          IF <fs_msgv3> IS ASSIGNED AND <fs_msgv3> IS NOT INITIAL.
            lv_msgv3 = <fs_msgv3>.
          ENDIF.
          IF <fs_msgv4> IS ASSIGNED AND <fs_msgv4> IS NOT INITIAL.
            lv_msgv4 = <fs_msgv4>.
          ENDIF.
          IF lv_msgid IS NOT INITIAL
            AND lv_msgno IS NOT INITIAL.
            MESSAGE ID lv_msgid TYPE 'S' NUMBER lv_msgno
              INTO FINAL(mtext1)
              WITH lv_msgv1 lv_msgv2 lv_msgv3 lv_msgv4.
            IF msg IS INITIAL.
              msg = mtext1.
            ELSE.
              msg = |{ msg }/{ mtext1 }|.
            ENDIF.
          ENDIF.
          UNASSIGN:<fs_msgid>,<fs_msgno>,<fs_msgv1>,<fs_msgv2>,<fs_msgv3>,<fs_msgv4>.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD modifyorder.
    "1.更新标准表字段
    DATA:lv_productionorder TYPE zc_pp002-manufacturingorder.
    lv_productionorder = ls_input-manufacturingorder.
    lv_productionorder = |{ lv_productionorder ALPHA = IN }|.
    MODIFY ENTITY i_productionordertp
     UPDATE FIELDS (
     orderplannedtotalqty
     orderplannedstartdate
     orderplannedenddate
     yy1_mfgbatch_ord
     )
     WITH VALUE #(
     (
     %key-productionorder = lv_productionorder
     %data-orderplannedtotalqty = ls_input-mfgorderplannedtotalqty
     %data-orderplannedstartdate = ls_input-mfgorderplannedstartdate
     %data-orderplannedenddate = ls_input-mfgorderplannedenddate
     %data-yy1_mfgbatch_ord = ls_input-yy1_mfgbatch_ord
     )
     )
     FAILED DATA(failed)
     REPORTED DATA(reported)
     MAPPED DATA(mapped).

    IF failed IS INITIAL.

*      COMMIT ENTITIES
*      RESPONSES
*      FAILED DATA(failed_c)
*      REPORTED DATA(reported_c).
*      IF failed_c IS INITIAL.
      "2.更新自建表数据
*      UPDATE zzt_pp_001 SET yy1_mfgbatch_ord =  @ls_input-yy1_mfgbatch_ord,
*                            yy1_mfgdate =  @ls_input-yy1_mfgdate,
*                            yy1_expdate =  @ls_input-yy1_expdate WHERE manufacturingorder = @lv_productionorder.
      "Read travel instances of the transferred keys
*      COMMIT WORK AND WAIT.
*      SELECT SINGLE *
*               FROM zc_pp002 WITH PRIVILEGED ACCESS
*              WHERE manufacturingorder = @lv_productionorder
*               INTO CORRESPONDING FIELDS OF @ls_input.
      flag = 'S'.
      msg = '修改成功'.
*      ELSE.
*        flag = 'E'.
*        msg = '标准表数据库提交失败，请联系管理员'.
*      ENDIF.
    ELSE.
*      ROLLBACK ENTITIES.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO FINAL(mtext).
      flag = 'E'.
      get_msg( EXPORTING  ls_failed = failed
                          ls_reported = reported
                          lv_component = 'PRODUCTIONORDER'
                IMPORTING msg = msg     ).
      msg = |{ msg }'/'{ mtext }|.
      msg = |修改失败:{ msg }|.
    ENDIF.
  ENDMETHOD.


  METHOD release.
    DATA:lv_productionorder TYPE zc_pp002-manufacturingorder.
    IF ls_input-orderisreleased = 'X'.
      flag = 'E'.
      msg = '订单已下达，请勿重复处理'.
      RETURN.
    ENDIF.
    IF ls_input-yy1_approvestatus NE 'Y' OR ls_input-yy1_check IS INITIAL.
      flag = 'E'.
      msg = 'BPM审批通过的生产任务单才可以下达'.
      RETURN.
    ENDIF.
    lv_productionorder = ls_input-manufacturingorder.
    lv_productionorder = |{ lv_productionorder ALPHA = IN }|.
    MODIFY ENTITY i_productionordertp
     EXECUTE release
     FROM VALUE #( ( %key-productionorder = lv_productionorder
     %param-ordrelispmtddsptmisgparts = abap_true ) )
     REQUEST VALUE #( )
     RESULT DATA(result)
     FAILED DATA(failed)
     REPORTED DATA(reported).

    IF failed IS INITIAL.
*      COMMIT ENTITIES
*      RESPONSES
*      FAILED DATA(failed_c)
*      REPORTED DATA(reported_c).
*      IF failed_c IS INITIAL.
*        SELECT SINGLE *
*           FROM zc_pp002 WITH PRIVILEGED ACCESS
*          WHERE manufacturingorder = @lv_productionorder
*           INTO CORRESPONDING FIELDS OF @ls_input.
      ls_input-orderiscreated = ''.
      ls_input-orderisreleased = 'X'.
      ls_input-mfgorderactualreleasedate = cl_abap_context_info=>get_system_date( ).
      flag = 'S'.
      msg = '下达成功'.
*      ELSE.
*        flag = 'E'.
*        msg = '下达操作数据库提交失败，请联系管理员'.
*      ENDIF.
    ELSE.
*      ROLLBACK ENTITIES.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO FINAL(mtext).
      flag = 'E'.
      get_msg( EXPORTING  ls_failed = failed
                          ls_reported = reported
                          lv_component = 'PRODUCTIONORDER'
                IMPORTING msg = msg     ).
      msg = |{ msg }'/'{ mtext }|.
      msg = |下达失败:{ msg }|.
    ENDIF.
  ENDMETHOD.


  METHOD sendbpm.
    "http://bpmtest.edongfanghong.com/YZSoft/ApiService/YZService.ashx?ts=12345678&UserAccount=A01&AppId=EAS&token=xxxxxxxx
    TYPES:BEGIN OF ty_bom,
            reservationitem           TYPE string,
            material                  TYPE string,
            productname               TYPE string,
            producttype               TYPE string,
            materialtypename          TYPE string,
            goodsmovemententryqty     TYPE i_mfgorderoperationcomponent-goodsmovemententryqty,
            entryunit                 TYPE i_mfgorderoperationcomponent-entryunit,
            bomitemdescription        TYPE string,
            materialcomponentsorttext TYPE string,
            sizeordimensiontext       TYPE string,
            unloadingpointname        TYPE string,
          END OF ty_bom.
    TYPES BEGIN OF ty_head.
    TYPES:
      applicantid                TYPE string,
      applicantname              TYPE string,
      createtime                 TYPE string,
      deptcode                   TYPE string,
      deptname                   TYPE string,
      productionplant            TYPE string,
      manufacturingorder         TYPE string,
      manufacturingordertype     TYPE string,
      manufacturingordertypename TYPE string,
      material                   TYPE string,
      productname                TYPE string,
      mfgorderplannedtotalqty    TYPE i_manufacturingorderitem-mfgorderplannedtotalqty,
      productionunit             TYPE string,
      mfgorderplannedstartdate   TYPE string,
      mfgorderplannedenddate     TYPE string,
      yy1_mfgdate                TYPE string,
      yy1_expdate                TYPE string,
      yy1_plannebatch_ord        TYPE string,
      yy1_mfgbatch_ord           TYPE string,
      sizeordimensiontext        TYPE string,
      mfgordercreationdate       TYPE string.
    TYPES END OF ty_head.
    TYPES:BEGIN OF ty_formdata,
            proc_fd_gyl_production_order   TYPE ty_head,
            proc_fd_gyl_production_order_d TYPE TABLE OF ty_bom WITH DEFAULT KEY,
          END OF ty_formdata.
    TYPES:BEGIN OF ty_send,
            processname TYPE string,
            action      TYPE string,
            comment     TYPE string,
            draft       TYPE abap_bool,
            existtaskid TYPE string,
            formdata    TYPE ty_formdata,
          END OF ty_send.
    TYPES:BEGIN OF ty_srcobject,
            taskid  TYPE string,
            sn      TYPE string,
            success TYPE abap_bool,
          END OF ty_srcobject.
    TYPES:BEGIN OF ty_res,
            errorcode TYPE string,
            message   TYPE string,
            srcobject TYPE ty_srcobject,
          END OF ty_res.

    DATA:ls_send TYPE ty_send,
         ls_res  TYPE ty_res.
    DATA:lt_mapping TYPE /ui2/cl_json=>name_mappings.
    DATA:lv_oref TYPE zzefname,
         lt_ptab TYPE abap_parmbind_tab.
    DATA:lv_numb TYPE zzenumb VALUE 'PP001'.
    DATA:lv_data TYPE string.
    DATA:lv_msgty TYPE bapi_mtype,
         lv_msgtx TYPE bapi_msg,
         lv_resp  TYPE string,
         lv_zcy   TYPE zc_pp005-requestedqty.
    "SrcObject": {
    "TaskID": "2019972",
    "AcSNtion": null,
    "OpenUrl": "http://bpmtest.edongfanghong.com//YZSoft/Forms/Read.aspx?tid=2019972",
    "success": true,
    "errorMessage": null
    "}

    DATA:lv_productionorder TYPE zc_pp002-manufacturingorder.

    DATA(lv_s4_userid) = cl_abap_context_info=>get_user_technical_name( ).
    SELECT SINGLE *
             FROM i_businessuservh WITH PRIVILEGED ACCESS
            WHERE userid = @lv_s4_userid
             INTO @DATA(ls_businessuser).
    DATA(lv_email) = ls_businessuser-defaultemailaddress.
    SPLIT lv_email AT '@' INTO TABLE DATA(lt_email).
    IF sy-subrc = 0.
      READ TABLE lt_email INTO DATA(ls_email) INDEX 1.
      DATA(lv_bpmuseraccount) = to_lower( ls_email ).
    ENDIF.
    IF sy-sysid = 'NMW' OR sy-sysid = 'NO1'.
      lv_bpmuseraccount = 'jinjinzhao'.
    ENDIF.

    lv_productionorder = ls_input-manufacturingorder.
    lv_productionorder = |{ lv_productionorder ALPHA = IN }|.
    IF ls_input-orderiscreated NE 'X'.
      flag = 'E'.
      msg = '只有【创建】状态订单才可以推送BPM'.
      RETURN.
    ENDIF.
    IF ls_input-yy1_sendbpm = 'X' AND ls_input-yy1_approvestatus NE 'N'.
      flag = 'E'.
      msg = '生产任务单已推送BPM，请勿重复推送'.
      RETURN.
    ENDIF.

    SELECT SINGLE *
             FROM zc_pp002 WITH PRIVILEGED ACCESS
            WHERE manufacturingorder = @lv_productionorder
             INTO @DATA(ls_zc_pp002).

    IF ls_zc_pp002-yy1_mfgbatch_ord IS INITIAL.
      flag = 'E'.
      msg = '【生产批次】不能为空'.
      RETURN.
    ENDIF.
*    IF ls_zc_pp002-yy1_mfgdate IS INITIAL.
*      flag = 'E'.
*      msg = '【生产日期】不能为空'.
*      RETURN.
*    ENDIF.
*    IF ls_zc_pp002-yy1_expdate IS INITIAL.
*      flag = 'E'.
*      msg = '【有效期】不能为空'.
*      RETURN.
*    ENDIF.

    SELECT SINGLE *
             FROM i_productplantbasic WITH PRIVILEGED ACCESS
            WHERE product = @ls_zc_pp002-material
              AND plant   = @ls_zc_pp002-productionplant
             INTO @DATA(ls_productplantbasic).

    SELECT a~*,
           b~producttype,
           b~sizeordimensiontext,
           d~materialtypename,
           c~productname,
           e~unitofmeasure_e AS entryunit_e,
           k~ordercomponentlongtext,
           bomhead~bomheaderquantityinbaseunit AS zjbsl
      FROM i_mfgorderoperationcomponent WITH PRIVILEGED ACCESS AS a
      INNER JOIN i_product WITH PRIVILEGED ACCESS AS b
        ON a~material = b~product
      INNER JOIN i_unitofmeasure WITH PRIVILEGED ACCESS AS e
                 ON a~entryunit = e~unitofmeasure
      LEFT OUTER JOIN i_producttext WITH PRIVILEGED ACCESS AS c
        ON a~material = c~product AND c~language = '1'
      LEFT OUTER JOIN i_producttypetext WITH PRIVILEGED ACCESS AS d
        ON b~producttype = d~producttype AND d~language = '1'
      LEFT OUTER JOIN i_mfgordercomponentlongtext  AS k
        ON  a~reservation    = k~reservation
        AND a~reservationitem = k~reservationitem
        AND a~recordtype = ''
        AND k~longtextlanguage = '1'
      INNER JOIN  i_billofmaterialheaderdex_2  AS bomhead
        ON  a~billofmaterialcategory     = bomhead~billofmaterialcategory
        AND a~billofmaterial             = bomhead~billofmaterial
        AND a~billofmaterialvariantusage = bomhead~billofmaterialvariantusage
        AND a~billofmaterialvariant      = bomhead~billofmaterialvariant
      WHERE a~manufacturingorder = @lv_productionorder
       INTO TABLE @DATA(lt_component).

*    "获取BPM用户信息
*    DATA(ls_userinfo) = getbpmuserinfo( CHANGING ls_input = ls_input
*                                                 flag = flag
*                                                 msg  = msg ).
*    IF flag = 'E'.
*      RETURN.
*    ENDIF.

    ls_send-processname = |供应链事业部_生产订单审批流程|.
    ls_send-action = |提交|.
    ls_send-comment = ||.
    ls_send-draft = ||.
    ls_send-existtaskid = ||.

    DATA(lv_date) = cl_abap_context_info=>get_system_date( ).

    MOVE-CORRESPONDING ls_zc_pp002 TO ls_send-formdata-proc_fd_gyl_production_order.
    ls_send-formdata-proc_fd_gyl_production_order-applicantid = lv_bpmuseraccount.
*    ls_send-formdata-proc_fd_gyl_production_order-applicantname = ls_userinfo-displayname.
    ls_send-formdata-proc_fd_gyl_production_order-createtime = |{ lv_date+0(4) }-{ lv_date+4(2) }-{ lv_date+6(2) }|.
*    ls_send-formdata-proc_fd_gyl_production_order-deptcode = ls_userinfo-deptcode.
*    ls_send-formdata-proc_fd_gyl_production_order-deptname = ls_userinfo-deptname.

    CLEAR:ls_send-formdata-proc_fd_gyl_production_order-yy1_mfgdate,ls_send-formdata-proc_fd_gyl_production_order-yy1_expdate.
    ls_send-formdata-proc_fd_gyl_production_order-manufacturingorder = |{ ls_send-formdata-proc_fd_gyl_production_order-manufacturingorder ALPHA = OUT }|.
    CONDENSE ls_send-formdata-proc_fd_gyl_production_order-manufacturingorder NO-GAPS.
    ls_send-formdata-proc_fd_gyl_production_order-material = |{ ls_send-formdata-proc_fd_gyl_production_order-material ALPHA = OUT }|.
    CONDENSE ls_send-formdata-proc_fd_gyl_production_order-material NO-GAPS.
    ls_send-formdata-proc_fd_gyl_production_order-mfgorderplannedstartdate = |{ ls_zc_pp002-mfgorderplannedstartdate+0(4) }-{ ls_zc_pp002-mfgorderplannedstartdate+4(2) }-{ ls_zc_pp002-mfgorderplannedstartdate+6(2) }|.
    ls_send-formdata-proc_fd_gyl_production_order-mfgorderplannedenddate = |{ ls_zc_pp002-mfgorderplannedenddate+0(4) }-{ ls_zc_pp002-mfgorderplannedenddate+4(2) }-{ ls_zc_pp002-mfgorderplannedenddate+6(2) }|.
    IF ls_zc_pp002-yy1_mfgdate IS NOT INITIAL.
      ls_send-formdata-proc_fd_gyl_production_order-yy1_mfgdate = |{ ls_zc_pp002-yy1_mfgdate+0(4) }-{ ls_zc_pp002-yy1_mfgdate+4(2) }-{ ls_zc_pp002-yy1_mfgdate+6(2) }|.
    ENDIF.
    IF ls_zc_pp002-yy1_expdate IS NOT INITIAL.
      ls_send-formdata-proc_fd_gyl_production_order-yy1_expdate = |{ ls_zc_pp002-yy1_expdate+0(4) }-{ ls_zc_pp002-yy1_expdate+4(2) }-{ ls_zc_pp002-yy1_expdate+6(2) }|.
    ENDIF.
    ls_send-formdata-proc_fd_gyl_production_order-sizeordimensiontext = ls_zc_pp002-sizeordimensiontext.
    ls_send-formdata-proc_fd_gyl_production_order-mfgordercreationdate = |{ ls_zc_pp002-mfgordercreationdate+0(4) }-{ ls_zc_pp002-mfgordercreationdate+4(2) }-{ ls_zc_pp002-mfgordercreationdate+6(2) }|.

    LOOP AT lt_component ASSIGNING FIELD-SYMBOL(<fs_component>).
      APPEND INITIAL LINE TO ls_send-formdata-proc_fd_gyl_production_order_d ASSIGNING FIELD-SYMBOL(<fs_item>).
      MOVE-CORRESPONDING <fs_component>-a TO <fs_item>.
      MOVE-CORRESPONDING <fs_component> TO <fs_item>.
      <fs_item>-entryunit = <fs_component>-entryunit_e.
      <fs_item>-reservationitem = |{ <fs_item>-reservationitem ALPHA = OUT }|.
      CONDENSE <fs_item>-reservationitem NO-GAPS.
      <fs_item>-material = |{ <fs_item>-material ALPHA = OUT }|.
      CONDENSE <fs_item>-material NO-GAPS.
      TRY.
          CLEAR:<fs_item>-bomitemdescription.
          CONDENSE <fs_component>-ordercomponentlongtext NO-GAPS.
          lv_zcy = <fs_component>-ordercomponentlongtext.
*          DATA(lv_zcy_str) = zcl_com_util=>get_zcy( <fs_component>-ordercomponentlongtext ).
*          lv_zcy = lv_zcy_str.
          lv_zcy = lv_zcy * ( ls_zc_pp002-mfgorderplannedtotalqty / <fs_component>-zjbsl ).
          <fs_item>-bomitemdescription = lv_zcy.
          CONDENSE <fs_item>-bomitemdescription NO-GAPS.
        CATCH cx_root INTO DATA(lr_err).
      ENDTRY.
    ENDLOOP.

    lt_mapping = VALUE #(
               ( abap = 'ProcessName'                                  json = 'ProcessName'            )
               ( abap = 'Action'                                       json = 'Action'                 )
               ( abap = 'Comment'                                      json = 'Comment'                )
               ( abap = 'Draft'                                        json = 'Draft'                  )
               ( abap = 'ExistTaskID'                                  json = 'ExistTaskID'            )
               ( abap = 'FormData'                                     json = 'FormData'               )
               ( abap = 'Proc_FD_GYL_Production_Order'                 json = 'Proc_FD_GYL_Production_Order'    )
               ( abap = 'Proc_FD_GYL_Production_Order_D'               json = 'Proc_FD_GYL_Production_Order_D'  )
               ( abap = 'ApplicantId'                                  json = 'ApplicantId'                  )
               ( abap = 'ApplicantName'                                json = 'ApplicantName'            )
               ( abap = 'CreateTime'                                   json = 'CreateTime'               )
               ( abap = 'DeptCode'                                     json = 'DeptCode'    )
               ( abap = 'DeptName'                                     json = 'DeptName'  )

               ( abap = 'ProductionPlant'                              json = 'ProductionPlant'            )
               ( abap = 'ManufacturingOrder'                           json = 'ManufacturingOrder'         )
               ( abap = 'ManufacturingOrderType'                       json = 'ManufacturingOrderType'     )
               ( abap = 'ManufacturingOrderTypeName'                   json = 'ManufacturingOrderTypeName' )
               ( abap = 'Material'                                     json = 'Material'                   )
               ( abap = 'ProductName'                                  json = 'ProductName'                )
               ( abap = 'MfgOrderPlannedTotalQty'                      json = 'MfgOrderPlannedTotalQty'    )
               ( abap = 'ProductionUnit'                               json = 'ProductionUnit'             )
               ( abap = 'MfgOrderPlannedStartDate'                     json = 'MfgOrderPlannedStartDate'   )
               ( abap = 'MfgOrderPlannedEndDate'                       json = 'MfgOrderPlannedEndDate'     )
               ( abap = 'YY1_mfgdate'                                  json = 'YY1_mfgdate'                )
               ( abap = 'YY1_expdate'                                  json = 'YY1_expdate'                )
               ( abap = 'YY1_plannebatch_ORD'                          json = 'YY1_plannebatch_ORD'        )
               ( abap = 'YY1_mfgbatch_ORD'                             json = 'YY1_mfgbatch_ORD'           )

               ( abap = 'sizeordimensiontext'                          json = 'SizeOrDimensionText'        )
               ( abap = 'MfgOrderCreationDate'                         json = 'MfgOrderCreationDate'       )

               ( abap = 'ReservationItem'                              json = 'ReservationItem'            )
               ( abap = 'ProductType'                                  json = 'ProductType'                )
               ( abap = 'MaterialTypeName'                             json = 'MaterialTypeName'           )
               ( abap = 'GoodsMovementEntryQty'                        json = 'GoodsMovementEntryQty'      )
               ( abap = 'EntryUnit'                                    json = 'EntryUnit'                  )
               ( abap = 'MaterialComponentSortText'                    json = 'MaterialComponentSortText'   )
               ( abap = 'BOMItemDescription'                           json = 'BOMItemDescription'   )
               ( abap = 'UnloadingPointName'                           json = 'UnloadingPointName'   )
               ).

    "获取调用类
    SELECT SINGLE zzcname
      FROM zr_vt_rest_conf
     WHERE zznumb = @lv_numb
      AND zzisst = 'X'
      INTO @lv_oref.
    IF lv_oref IS INITIAL.
      flag = 'E'.
      msg = |{ msg }/推送BPM失败，未配置激活接口PP001|.
      RETURN.
    ENDIF.

    "传入数据转JSON
    lv_data = /ui2/cl_json=>serialize(
          data          = ls_send
          pretty_name   = /ui2/cl_json=>pretty_mode-camel_case
          name_mappings = lt_mapping ).
*&--调用实例化接口
    DATA:lo_oref TYPE REF TO object.

    lt_ptab = VALUE #( ( name  = 'IV_NUMB' kind  = cl_abap_objectdescr=>exporting value = REF #( lv_numb ) ) ).
    TRY .
        CREATE OBJECT lo_oref TYPE (lv_oref) PARAMETER-TABLE lt_ptab.
        CALL METHOD lo_oref->('OUTBOUND')
          EXPORTING
            iv_data  = lv_data
          CHANGING
            ev_resp  = lv_resp
            ev_msgty = lv_msgty
            ev_msgtx = lv_msgtx.
      CATCH cx_root INTO DATA(lr_root).

    ENDTRY.

    IF lv_msgty = 'S'.
      flag = 'S'.
      msg = |推送BPM成功|.
      ls_input-yy1_sendbpm = 'X'.
      /ui2/cl_json=>deserialize( EXPORTING json        = lv_resp
                                           pretty_name = /ui2/cl_json=>pretty_mode-none
                                 CHANGING  data        = ls_res ).
      ls_input-yy1_bpm_task_id = ls_res-srcobject-taskid.
      ls_input-yy1_related_num = ls_res-srcobject-sn.
      ls_input-yy1_approvestatus = ''.
    ELSE.
      flag = 'E'.
      msg = |推送BPM失败:{ lv_msgtx }|.
      RETURN.
    ENDIF.

  ENDMETHOD.


  METHOD senwms.
    TYPES:BEGIN OF ty_bom,
            reservationitem       TYPE string,
            material              TYPE string,
            productname           TYPE string,
            producttype           TYPE string,
            producttypename       TYPE string,
            goodsmovemententryqty TYPE i_mfgorderoperationcomponent-goodsmovemententryqty,
            entryunit             TYPE i_mfgorderoperationcomponent-entryunit,
          END OF ty_bom.
    TYPES BEGIN OF ty_send.
    TYPES:productionplant            TYPE string,
          manufacturingorder         TYPE string,
          manufacturingordertype     TYPE string,
          manufacturingordertypename TYPE string,
          yy1_orderstatus            TYPE string,
          material                   TYPE string,
          productname                TYPE string,
          mfgorderplannedtotalqty    TYPE i_manufacturingorderitem-mfgorderplannedtotalqty,
          productionunit             TYPE string,
          mfgorderplannedstartdate   TYPE string,
          mfgorderplannedenddate     TYPE string,
          yy1_mfgdate                TYPE string,
          yy1_expdate                TYPE string,
          yy1_plannebatch_ord        TYPE string,
          yy1_mfgbatch_ord           TYPE string,
          yy1__approvestatus         TYPE string,
          yy1__approve               TYPE string,
          yy1__approvetime           TYPE string,
          yy1__check                 TYPE string,
          yy1__checktime             TYPE string,
          lastchangedbyuser          TYPE string,
          yy1_creationtime           TYPE string,
          lotsizequantity            TYPE i_manufacturingorderitem-mfgorderplannedtotalqty,
          item                       TYPE TABLE OF ty_bom WITH DEFAULT KEY.
    TYPES END OF ty_send.

    DATA:ls_send TYPE ty_send.
    DATA:lt_mapping TYPE /ui2/cl_json=>name_mappings.
    DATA:lv_oref TYPE zzefname,
         lt_ptab TYPE abap_parmbind_tab.
    DATA:lv_numb TYPE zzenumb VALUE 'PP002'.
    DATA:lv_data TYPE string.
    DATA:lv_msgty TYPE bapi_mtype,
         lv_msgtx TYPE bapi_msg,
         lv_resp  TYPE string.

    DATA:lv_productionorder TYPE zc_pp002-manufacturingorder.
    lv_productionorder = ls_input-manufacturingorder.
    lv_productionorder = |{ lv_productionorder ALPHA = IN }|.
    IF ls_input-orderisreleased NE 'X'.
      flag = 'E'.
      msg = '订单未下达，请下达后再推送'.
      RETURN.
    ENDIF.

    SELECT SINGLE *
             FROM zc_pp002 WITH PRIVILEGED ACCESS
            WHERE manufacturingorder = @lv_productionorder
             INTO @DATA(ls_zc_pp002).

    IF ls_zc_pp002-yy1_approvestatus = 'Y' AND ls_zc_pp002-yy1_check IS NOT INITIAL.

    ELSE.
      flag = 'E'.
      msg = 'BPM审批尚未通过，不允许推送WMS'.
      RETURN.
    ENDIF.

    IF ls_zc_pp002-yy1_sendwms = 'X'.
      flag = 'E'.
      msg = '已推送WMS，请勿重复推送'.
      RETURN.
    ENDIF.

    SELECT SINGLE *
             FROM i_productplantbasic WITH PRIVILEGED ACCESS
            WHERE product = @ls_zc_pp002-material
              AND plant   = @ls_zc_pp002-productionplant
             INTO @DATA(ls_productplantbasic).

    SELECT a~*,
           b~producttype,
           d~materialtypename AS producttypename,
           c~productname,
           e~unitofmeasure_e AS entryunit_e
      FROM i_mfgorderoperationcomponent WITH PRIVILEGED ACCESS AS a
      INNER JOIN i_product WITH PRIVILEGED ACCESS AS b
        ON a~material = b~product
      INNER JOIN i_unitofmeasure WITH PRIVILEGED ACCESS AS e
                 ON a~entryunit = e~unitofmeasure
      LEFT OUTER JOIN i_producttext WITH PRIVILEGED ACCESS AS c
        ON a~material = c~product AND c~language = '1'
      LEFT OUTER JOIN i_producttypetext WITH PRIVILEGED ACCESS AS d
        ON b~producttype = d~producttype AND d~language = '1'
     WHERE manufacturingorder = @lv_productionorder
      INTO TABLE @DATA(lt_component).

    MOVE-CORRESPONDING ls_zc_pp002 TO ls_send.
    CLEAR:ls_send-yy1_mfgdate,ls_send-yy1_expdate.
    ls_send-manufacturingorder = |{ ls_send-manufacturingorder ALPHA = OUT }|.
    CONDENSE ls_send-manufacturingorder NO-GAPS.
    ls_send-material = |{ ls_send-material ALPHA = OUT }|.
    CONDENSE ls_send-material NO-GAPS.
    ls_send-yy1_orderstatus = '下达'.
    ls_send-yy1__approvestatus = ls_zc_pp002-yy1_approvestatus.
    ls_send-yy1__approve = ls_zc_pp002-yy1_approve.
    ls_send-yy1__approvetime = ls_zc_pp002-yy1_approvetime.
    ls_send-yy1__check = ls_zc_pp002-yy1_check.
    ls_send-yy1__checktime = ls_zc_pp002-yy1_checktime.
    ls_send-mfgorderplannedstartdate = |{ ls_zc_pp002-mfgorderplannedstartdate+0(4) }-{ ls_zc_pp002-mfgorderplannedstartdate+4(2) }-{ ls_zc_pp002-mfgorderplannedstartdate+6(2) }T00:00:00.000Z|.
    ls_send-mfgorderplannedenddate = |{ ls_zc_pp002-mfgorderplannedenddate+0(4) }-{ ls_zc_pp002-mfgorderplannedenddate+4(2) }-{ ls_zc_pp002-mfgorderplannedenddate+6(2) }T00:00:00.000Z|.
    IF ls_zc_pp002-yy1_mfgdate IS NOT INITIAL.
      ls_send-yy1_mfgdate = |{ ls_zc_pp002-yy1_mfgdate+0(4) }-{ ls_zc_pp002-yy1_mfgdate+4(2) }-{ ls_zc_pp002-yy1_mfgdate+6(2) }T00:00:00.000Z|.
    ENDIF.
    IF ls_zc_pp002-yy1_expdate IS NOT INITIAL.
      ls_send-yy1_expdate = |{ ls_zc_pp002-yy1_expdate+0(4) }-{ ls_zc_pp002-yy1_expdate+4(2) }-{ ls_zc_pp002-yy1_expdate+6(2) }T00:00:00.000Z|.
    ENDIF.
    ls_send-yy1_creationtime = |{ ls_zc_pp002-mfgordercreationdate+0(4) }-{ ls_zc_pp002-mfgordercreationdate+4(2) }-{ ls_zc_pp002-mfgordercreationdate+6(2) }| &&
    |T{ ls_zc_pp002-mfgordercreationtime+0(2) }:{ ls_zc_pp002-mfgordercreationtime+2(2) }:{ ls_zc_pp002-mfgordercreationtime+4(2) }.000Z|.
    IF ls_productplantbasic-minimumlotsizequantity IS INITIAL.
      ls_send-lotsizequantity = ls_productplantbasic-fixedlotsizequantity.
    ELSE.
      ls_send-lotsizequantity = ls_productplantbasic-minimumlotsizequantity.
    ENDIF.

    LOOP AT lt_component ASSIGNING FIELD-SYMBOL(<fs_component>).
      APPEND INITIAL LINE TO ls_send-item ASSIGNING FIELD-SYMBOL(<fs_item>).
      MOVE-CORRESPONDING <fs_component>-a TO <fs_item>.
      MOVE-CORRESPONDING <fs_component> TO <fs_item>.
      <fs_item>-entryunit = <fs_component>-entryunit_e.
      <fs_item>-reservationitem = |{ <fs_item>-reservationitem ALPHA = OUT }|.
      CONDENSE <fs_item>-reservationitem NO-GAPS.
      <fs_item>-material = |{ <fs_item>-material ALPHA = OUT }|.
      CONDENSE <fs_item>-material NO-GAPS.
    ENDLOOP.

    lt_mapping = VALUE #(
               ( abap = 'ProductionPlant'                              json = 'productionPlant'          )
               ( abap = 'ManufacturingOrder'                           json = 'manufacturingOrder'       )
               ( abap = 'ManufacturingOrderType'                       json = 'manufacturingOrderType'   )
               ( abap = 'ManufacturingOrderTypeName'                   json = 'manufacturingOrderTypeName' )
               ( abap = 'YY1_orderstatus'                              json = 'yY1_orderstatus'            )
               ( abap = 'Material'                                     json = 'material'                   )
               ( abap = 'ProductName'                                  json = 'productName'                )
               ( abap = 'MfgOrderPlannedTotalQty'                      json = 'mfgOrderPlannedTotalQty'    )
               ( abap = 'ProductionUnit'                               json = 'productionUnit'             )
               ( abap = 'MfgOrderPlannedStartDate'                     json = 'mfgOrderPlannedStartDate'   )
               ( abap = 'MfgOrderPlannedEndDate'                       json = 'mfgOrderPlannedEndDate'     )
               ( abap = 'YY1_mfgdate'                                  json = 'yY1_mfgdate'                )
               ( abap = 'YY1_expdate'                                  json = 'yY1_expdate'                )
               ( abap = 'YY1_plannebatch_ORD'                          json = 'yY1_plannebatch_ORD'        )
               ( abap = 'YY1_mfgbatch_ORD'                             json = 'yY1_mfgbatch_ORD'           )
               ( abap = 'YY1__approvestatus'                           json = 'yY1__approvestatus'         )
               ( abap = 'YY1__approve'                                 json = 'yY1__approve'               )
               ( abap = 'YY1__approveTime'                             json = 'yY1__approveTime'           )
               ( abap = 'YY1__check'                                   json = 'yY1__check'                 )
               ( abap = 'YY1__checkTime'                               json = 'yY1__checkTime'             )
               ( abap = 'LastChangedByUser'                            json = 'lastChangedByUser'          )
               ( abap = 'YY1_CreationTime'                             json = 'yY1_CreationTime'           )
               ( abap = 'LotSizeQuantity'                              json = 'lotSizeQuantity'            )

               ( abap = 'ReservationItem'                              json = 'reservationItem'            )
               ( abap = 'ProductType'                                  json = 'productType'                )
               ( abap = 'MaterialTypeName'                             json = 'materialTypeName'           )
               ( abap = 'GoodsMovementEntryQty'                        json = 'goodsMovementEntryQty'      )
               ( abap = 'EntryUnit'                                    json = 'entryUnit'                  )
               ).

    "获取调用类
    SELECT SINGLE zzcname
      FROM zr_vt_rest_conf
     WHERE zznumb = @lv_numb
      AND zzisst = 'X'
      INTO @lv_oref.
    IF lv_oref IS INITIAL.
      flag = 'E'.
      msg = |{ msg }/推送WMS失败，未配置激活接口PP002|.
      RETURN.
    ENDIF.

    "传入数据转JSON
    lv_data = /ui2/cl_json=>serialize(
          data          = ls_send
          compress      = abap_true
          pretty_name   = /ui2/cl_json=>pretty_mode-camel_case
          name_mappings = lt_mapping ).
*&--调用实例化接口
    DATA:lo_oref TYPE REF TO object.

    lt_ptab = VALUE #( ( name  = 'IV_NUMB' kind  = cl_abap_objectdescr=>exporting value = REF #( lv_numb ) ) ).
    TRY .
        CREATE OBJECT lo_oref TYPE (lv_oref) PARAMETER-TABLE lt_ptab.
        CALL METHOD lo_oref->('OUTBOUND')
          EXPORTING
            iv_data  = lv_data
          CHANGING
            ev_resp  = lv_resp
            ev_msgty = lv_msgty
            ev_msgtx = lv_msgtx.
      CATCH cx_root INTO DATA(lr_root).

    ENDTRY.

    IF lv_msgty = 'S'.
      flag = 'S'.
      msg = |{ msg }/推送WMS成功|.
      ls_input-yy1_sendwms = 'X'.
    ELSE.
      flag = 'E'.
      msg = |{ msg }/推送WMS失败:{ lv_msgtx }|.
      RETURN.
    ENDIF.

  ENDMETHOD.


  METHOD update_zzt_pp_001.
    DATA:lt_returnresult TYPE TABLE OF zc_pp002,
         ls_zzt_pp_001   TYPE zzt_pp_001.
    DATA:lv_productionorder TYPE zc_pp002-manufacturingorder.

    IF ls_input-returncode = 'S'.
      IF ls_input-requestcode = 'PP0003'.
        "修改生产任务单
        CALL METHOD /ui2/cl_json=>deserialize(
          EXPORTING
            json        = ls_input-returnresult
            pretty_name = /ui2/cl_json=>pretty_mode-none "格式化参数，NONE：字段名称大写
          CHANGING
            data        = lt_returnresult ). "数据源CREATE OBJECT json_des.
        LOOP AT lt_returnresult ASSIGNING FIELD-SYMBOL(<fs_result>).
          CLEAR:lv_productionorder.
          lv_productionorder = <fs_result>-manufacturingorder.
          lv_productionorder = |{ lv_productionorder ALPHA = IN }|.
          IF <fs_result>-yy1_flag = 'S'.
            SELECT SINGLE *
                     FROM zzt_pp_001
                    WHERE manufacturingorder = @lv_productionorder
                     INTO @DATA(ls_zzt_pp_001_now).
            IF sy-subrc = 0.
              UPDATE zzt_pp_001 SET yy1_mfgbatch_ord =  @<fs_result>-yy1_mfgbatch_ord,
                                   yy1_mfgdate =  @<fs_result>-yy1_mfgdate,
                                   yy1_expdate =  @<fs_result>-yy1_expdate WHERE manufacturingorder = @lv_productionorder.
            ELSE.
              ls_zzt_pp_001 = VALUE #( manufacturingorder = lv_productionorder
                                       yy1_mfgbatch_ord   =  <fs_result>-yy1_mfgbatch_ord
                                       yy1_mfgdate        =  <fs_result>-yy1_mfgdate
                                       yy1_expdate        =  <fs_result>-yy1_expdate ).
              MODIFY zzt_pp_001 FROM @ls_zzt_pp_001.
            ENDIF.

          ENDIF.
        ENDLOOP.
      ENDIF.
      IF ls_input-requestcode = 'PP0004' OR ls_input-requestcode = 'PP0005'.
        "推送WMS
        CALL METHOD /ui2/cl_json=>deserialize(
          EXPORTING
            json        = ls_input-returnresult
            pretty_name = /ui2/cl_json=>pretty_mode-none "格式化参数，NONE：字段名称大写
          CHANGING
            data        = lt_returnresult ). "数据源CREATE OBJECT json_des.
        LOOP AT lt_returnresult ASSIGNING <fs_result>.
          CLEAR:lv_productionorder.
          lv_productionorder = <fs_result>-manufacturingorder.
          lv_productionorder = |{ lv_productionorder ALPHA = IN }|.
          IF <fs_result>-yy1_flag = 'S'.
            UPDATE zzt_pp_001 SET yy1_sendwms =  @<fs_result>-yy1_sendwms WHERE manufacturingorder = @lv_productionorder.
          ENDIF.
        ENDLOOP.
      ENDIF.
      IF ls_input-requestcode = 'PP0017'.
        "推送BPM审批
        CALL METHOD /ui2/cl_json=>deserialize(
          EXPORTING
            json        = ls_input-returnresult
            pretty_name = /ui2/cl_json=>pretty_mode-none "格式化参数，NONE：字段名称大写
          CHANGING
            data        = lt_returnresult ). "数据源CREATE OBJECT json_des.
        LOOP AT lt_returnresult ASSIGNING <fs_result>.
          CLEAR:lv_productionorder.
          lv_productionorder = <fs_result>-manufacturingorder.
          lv_productionorder = |{ lv_productionorder ALPHA = IN }|.
          IF <fs_result>-yy1_flag = 'S'.
            UPDATE zzt_pp_001 SET yy1_sendbpm =  @<fs_result>-yy1_sendbpm,
                                  yy1_bpm_task_id = @<fs_result>-yy1_bpm_task_id,
                                  yy1_related_num = @<fs_result>-yy1_related_num,
                                  yy1_approvestatus = @<fs_result>-yy1_approvestatus  WHERE manufacturingorder = @lv_productionorder.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
