*&---------------------------------------------------------------------*
*& Program Name     : ZDEV_UTIL_VERSIONCMP                                            *
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
*& Program Name     : ZDEV_UTIL_VERSIONCMP.
*& Title            : XXXX                                             *
*& Object ID        : RICEFW ID                                        *
*& Author           : XXXX XXXXX                                       *
*& Date             : MM/DD/YYYY                                       *
*& Business Contact : Functional Team                                  *
*&---------------------------------------------------------------------*
*& Program Purpose  :                                                  *
*&                                                                     *
*&                                                                     *
*&                                                                     *
*&---------------------------------------------------------------------*
*& Modification History                                                *
*&                                                                     *
*& 1) Request #     :                                                  *
*&    Developer     :                                                  *
*&    Date          :                                                  *
*&    Description   :                                                  *
*&                                                                     *
*&                                                                     *
*&---------------------------------------------------------------------*
REPORT zdev_util_version.


DATA : lv_trno  TYPE trkorr. " Request/Task

SELECT-OPTIONS so_tr FOR lv_trno NO INTERVALS. " Request/Task

TYPES : BEGIN OF ty_alv,
          actvtr    TYPE trkorr,     " Request/Task
          status    TYPE icon_int,   " Icon in text fields (substitute display, alias)
          trnum     TYPE trkorr,     " Request/Task
          tasknum   TYPE trkorr,     " Request/Task
          pgmid     TYPE pgmid,      " Program ID in Requests and Tasks
          objtype   TYPE versobjtyp,
          objtxt    TYPE ddtext,     " Explanatory Short Text
          objname   TYPE versobjnam, " Object Name in Object List
          epp       TYPE verssysnam, "
          email(35) TYPE c,          " Email(35) of type Character
        END OF ty_alv.

DATA :
  lt_object TYPE          tr_objects,
  lt_alv    TYPE TABLE OF ty_alv,
  ls_alv    TYPE          ty_alv.

DATA message TYPE REF TO cx_salv_msg. " ALV: General Error Class with Message


DATA : lt_bdcdata    TYPE TABLE OF bdcdata,    " Batch input: New table field structure
       ls_bdcdata    TYPE bdcdata,             " Batch input: New table field structure
       lt_bdcmsgcoll TYPE TABLE OF bdcmsgcoll, " Collecting messages in the SAP System
       ls_bdcmsgcoll TYPE bdcmsgcoll.          " Collecting messages in the SAP System


*----------------------------------------------------------------------*
*       CLASS lcl_alv DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_version DEFINITION. " Alv class
  PUBLIC SECTION.
    METHODS : constructor,
      fetch_data,
      display_alv,
      hotspot_click
      FOR EVENT
            link_click OF cl_salv_events_table
        IMPORTING
            row
            column.

  PROTECTED SECTION.

    DATA t_alv TYPE STANDARD TABLE OF alv_tab. " Check Table for ALV_T_T2

    DATA: lr_table   TYPE REF TO cl_salv_table,         " Basis Class for Simple Tables
          lr_columns TYPE REF TO cl_salv_columns_table, " Columns in Simple, Two-Dimensional Tables
          lr_column  TYPE REF TO cl_salv_column_table,  " Column Description of Simple, Two-Dimensional Tables
          lr_events  TYPE REF TO cl_salv_events_table.  " Events in Simple, Two-Dimensional Tables

  PRIVATE SECTION.

    METHODS :version_check
      IMPORTING
        im_objname    TYPE versobjnam  " ABAP Program Name
        im_subobjtype TYPE versobjtyp  " Target system for version comparison
        im_dest       TYPE verssysnam, " Logical destination (specified in function call)
      email_review
        IMPORTING im_data TYPE ty_alv,
      last_active.
ENDCLASS.
*----------------------------------------------------------------------*
*       CLASS lcl_alv IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_version IMPLEMENTATION. " Alv class

  METHOD constructor.
    DATA: lt_restriction TYPE sscr_restrict,
          ls_list        TYPE sscr_opt_list,
          ls_ass         TYPE sscr_ass.

    CLEAR ls_list.
    ls_list-name        = 'OBJECTKEY1'.
    ls_list-options-eq  = 'X'.
    APPEND ls_list TO lt_restriction-opt_list_tab.

    ls_ass-kind         = 'S'.
    ls_ass-name         = 'SO_TR'.
    ls_ass-sg_main      = 'I'.
    ls_ass-sg_addy      = space.
    ls_ass-op_main      = 'OBJECTKEY1'.
    APPEND ls_ass TO lt_restriction-ass_tab.

    CALL FUNCTION 'SELECT_OPTIONS_RESTRICT'
      EXPORTING
        restriction            = lt_restriction
      EXCEPTIONS
        too_late               = 1
        repeated               = 2
        selopt_without_options = 3
        selopt_without_signs   = 4
        invalid_sign           = 5
        empty_option_list      = 6
        invalid_kind           = 7
        repeated_kind_a        = 8
        OTHERS                 = 9.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF. " IF sy-subrc <> 0

    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF. " IF sy-subrc <> 0

  ENDMETHOD.

  METHOD fetch_data.

    DATA: lt_objects TYPE RANGE OF trobjtype, " Object Type
          ls_objects LIKE LINE OF  lt_objects,
          ls_request TYPE          trwbo_request_header,
          lt_objtxt  TYPE TABLE OF ko100,     " CTS: Object Types with Description
          ls_objtxt  TYPE          ko100.     " CTS: Object Types with Description

    IF  NOT so_tr IS  INITIAL.


      LOOP AT so_tr ASSIGNING FIELD-SYMBOL(<lfs_trno>).

        ls_request-trkorr = <lfs_trno>-low.


        CALL FUNCTION 'TR_GET_OBJECTS_OF_REQ_AN_TASKS'
          EXPORTING
            is_request_header      = ls_request
            iv_condense_objectlist = 'X'
          IMPORTING
            et_objects             = lt_object
          EXCEPTIONS
            invalid_input          = 1
            OTHERS                 = 2.
        IF sy-subrc <> 0.
* Implement suitable error handling here
        ELSE. " ELSE -> IF sy-subrc <> 0

          LOOP AT lt_object ASSIGNING FIELD-SYMBOL(<lfs_data>).
*                      WHERE object  IN lt_objects.

            ls_alv-trnum    = ls_request-trkorr.
            ls_alv-tasknum  = <lfs_data>-trkorr.
            ls_alv-objname  = <lfs_data>-obj_name.
            ls_alv-pgmid    = <lfs_data>-pgmid.
            ls_alv-objtype  = <lfs_data>-object.
            ls_alv-epp      =  'EPP'.
            ls_alv-email    = |E-Mail -| & |{ ls_request-trkorr }|.

            APPEND ls_alv TO lt_alv.

          ENDLOOP. " LOOP AT lt_object ASSIGNING FIELD-SYMBOL(<lfs_data>)
        ENDIF. " IF sy-subrc <> 0

      ENDLOOP. " LOOP AT so_tr ASSIGNING FIELD-SYMBOL(<lfs_trno>)


      CALL FUNCTION 'TR_OBJECT_TABLE'
        TABLES
          wt_object_text = lt_objtxt.

      LOOP AT lt_alv ASSIGNING FIELD-SYMBOL(<lfs_alv>).
        CLEAR ls_objtxt.
        READ TABLE lt_objtxt INTO ls_objtxt
                             WITH KEY
                                  pgmid   = <lfs_alv>-pgmid
                                  object  = <lfs_alv>-objtype.
        IF sy-subrc EQ 0.
          <lfs_alv>-objtxt  = ls_objtxt-text.
        ENDIF. " IF sy-subrc EQ 0
      ENDLOOP. " LOOP AT lt_alv ASSIGNING FIELD-SYMBOL(<lfs_alv>)

    ENDIF. " IF NOT so_tr IS INITIAL

    IF NOT  lt_alv IS INITIAL.
      last_active( ).
    ENDIF. " IF NOT lt_alv IS INITIAL

  ENDMETHOD. "SELECT_ALV

  METHOD display_alv.

    DATA  : lr_column TYPE REF TO cl_salv_column_list, " Individual Column Object
            ls_scolo  TYPE        lvc_s_colo.          " ALV control: Color coding
    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = lr_table
          CHANGING
            t_table      = lt_alv ).
      CATCH cx_salv_msg. "#EC Nlr_HANDLER
    ENDTRY.


** EVENTS
    lr_events = lr_table->get_event( ).
    SET HANDLER me->hotspot_click FOR lr_events.

** Column Settings
    DATA(lr_columns) = lr_table->get_columns( ).

    lr_columns = lr_table->get_columns( ).
    lr_columns->set_optimize( abap_true ).

    lr_column ?= lr_columns->get_column( 'STATUS' ).
    lr_column->set_long_text( 'Status' ).

    lr_column ?= lr_columns->get_column( 'ACTVTR' ).
    lr_column->set_long_text( 'Last Active TR' ).
    lr_column->set_short_text( ' ' ).
    lr_column->set_medium_text( ' ' ).

    lr_column ?= lr_columns->get_column( 'TRNUM' ).
    lr_column->set_short_text( ' ' ).
    lr_column->set_medium_text( ' ' ).
    lr_column->set_long_text( 'Dev-TR Number' ).

    lr_column ?= lr_columns->get_column( 'TASKNUM' ).
    lr_column->set_long_text( 'Task Number' ).

    lr_column ?= lr_columns->get_column( 'PGMID' ).
    lr_column->set_visible( if_salv_c_bool_sap=>false ).

    lr_column ?= lr_columns->get_column( 'OBJTYPE' ).
    lr_column->set_visible( if_salv_c_bool_sap=>false ).

    lr_column ?= lr_columns->get_column( 'OBJTXT' ).
    CLEAR ls_scolo.
    ls_scolo-col = 3.
    lr_column->set_color( ls_scolo ).


    lr_column ?= lr_columns->get_column( 'EPP' ).
    lr_column->set_cell_type( if_salv_c_cell_type=>hotspot ).
    lr_column->set_short_text( ' ' ).
    lr_column->set_medium_text( ' ' ).
    lr_column->set_long_text( 'Version' ).

    lr_column ?= lr_columns->get_column( 'EMAIL' ).
    lr_column->set_cell_type( if_salv_c_cell_type=>hotspot ).
    lr_column->set_long_text( 'E-Mail Developer' ).


** SORT
    DATA(lr_sort) = lr_table->get_sorts( ).
    lr_sort->add_sort( 'TRNUM' ).
    lr_sort->add_sort( 'OBJTXT' ).
    lr_sort->add_sort( 'EMAIL' ).

** Pattern
    DATA(lr_display) = lr_table->get_display_settings( ).
    lr_display->set_striped_pattern( cl_salv_display_settings=>true  ).

** Display ALV
    lr_table->display( ).

  ENDMETHOD. "display_alv

  METHOD hotspot_click.

    DATA: l_row_string TYPE string,
          l_col_string TYPE string,
          l_row        TYPE char128. " Row of type CHAR128

    READ TABLE lt_alv ASSIGNING FIELD-SYMBOL(<lfs_data>) INDEX row.
    IF <lfs_data> IS ASSIGNED.

      CASE column.

        WHEN 'EPP'.
          version_check(
            EXPORTING
              im_objname    = <lfs_data>-objname
              im_subobjtype = <lfs_data>-objtype
              im_dest       = <lfs_data>-epp         ).

        WHEN 'EMAIL'.
          email_review( <lfs_data> ).

      ENDCASE.

    ENDIF. " IF <lfs_data> IS ASSIGNED

  ENDMETHOD. " HOTSPOT_CLICK
  METHOD version_check.

    DATA      : lt_versno     TYPE TABLE OF  vrsn,       " Version number list
                lt_vrsd       TYPE TABLE OF  vrsd,       " Version management: Directory table
                ls_vrsd       TYPE           vrsd,       " Version management: Directory table
                lt_versno_rem TYPE TABLE OF  vrsn,       " Version number list
                lt_vrsd_rem   TYPE TABLE OF  vrsd,       " Version management: Directory table
                ls_vrsd_rem   TYPE           vrsd,       " Version management: Directory table
                ls_infoline1a TYPE           vrsinfolna, " Version Management: Info-Line First Part (Name)
                ls_infoline1b TYPE           vrsinfolnb, " Version Management: Info-Line Second Part (Request Info)
                ls_infoline2a TYPE           vrsinfolna, " Version Management: Info-Line First Part (Name)
                ls_infoline2b TYPE           vrsinfolnb, " Version Management: Info-Line Second Part (Request Info)
                lv_cmpdisp    TYPE           progname.   " ABAP Program Name

    CONSTANTS : lc_epp        TYPE            rfcdest           VALUE 'EPPCLNT100'. " ABAP System Field: Current Transaction Code


    CASE im_dest.
      WHEN 'EPP'.
        DATA(lv_dest) = lc_epp.
    ENDCASE.

    CALL FUNCTION 'SVRS_GET_OBJECT_REPORTS'
      EXPORTING
        objtype  = im_subobjtype
      IMPORTING
        rep_comp = lv_cmpdisp. "  dir_f5_report

    IF lv_cmpdisp IS INITIAL.
      MESSAGE s016(tsys) WITH im_subobjtype. " There is no object comparison report for object type &1
      EXIT.
    ENDIF. " IF lv_cmpdisp IS INITIAL


    CALL FUNCTION 'SVRS_GET_VERSION_DIRECTORY_46'
      EXPORTING
        objname                = im_objname
        objtype                = im_subobjtype
      TABLES
        lversno_list           = lt_versno
        version_list           = lt_vrsd
      EXCEPTIONS
        no_entry               = 1
        communication_failure_ = 2
        system_failure         = 3
        OTHERS                 = 4.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ELSE. " ELSE -> IF sy-subrc <> 0

      READ TABLE lt_vrsd INTO  ls_vrsd
                          WITH  KEY versno = '00000'.

      ls_infoline1b-korrnum  = ls_vrsd-korrnum .
      ls_infoline1b-author  = ls_vrsd-author .
      ls_infoline1b-datum =  ls_vrsd-datum.

      CALL FUNCTION 'SVRS_GET_VERSION_DIRECTORY_46'
        DESTINATION lv_dest
        EXPORTING
          objname      = im_objname
          objtype      = im_subobjtype
        TABLES
          lversno_list = lt_versno_rem
          version_list = lt_vrsd_rem
        EXCEPTIONS
          no_entry     = 1
          OTHERS       = 2.
      IF sy-subrc <> 0.

      ENDIF. " IF sy-subrc <> 0
      READ TABLE lt_vrsd_rem INTO  ls_vrsd_rem
                            WITH  KEY versno = '00000'.

      ls_infoline2b-korrnum  = ls_vrsd_rem-korrnum .
      ls_infoline2b-author  = ls_vrsd_rem-author .
      ls_infoline2b-datum =  ls_vrsd_rem-datum.

      SUBMIT (lv_cmpdisp) AND RETURN
              WITH objname  = im_objname
              WITH objnam2  = im_objname
              WITH versno1  = '00000'
              WITH versno2  = '00000'
              WITH objtyp1  = im_subobjtype
              WITH objtyp2  = im_subobjtype
              WITH infoln1a = ls_infoline1a
              WITH infoln1b = ls_infoline1b
              WITH infoln2a = ls_infoline2a
              WITH infoln2b = ls_infoline2b
              WITH log_dest = lv_dest
              WITH rem_syst = lv_dest.

    ENDIF. " IF sy-subrc <> 0

  ENDMETHOD.
  METHOD last_active.

    TYPES     : BEGIN OF ty_acttr,
                  trnum  TYPE trkorr, " Request/Task
                  versno TYPE versno, " Version Management: Version Number
                  trtype TYPE trfunction,
                END OF ty_acttr.
    DATA      : lt_acttr TYPE TABLE OF ty_acttr.

    DATA      : lt_versno     TYPE TABLE OF  vrsn,       " Version number list
                lt_vrsd       TYPE TABLE OF  vrsd,       " Version management: Directory table
                ls_vrsd       TYPE           vrsd,       " Version management: Directory table
                lt_versno_rem TYPE TABLE OF  vrsn,       " Version number list
                lt_vrsd_rem   TYPE TABLE OF  vrsd,       " Version management: Directory table
                ls_vrsd_rem   TYPE           vrsd,       " Version management: Directory table
                ls_infoline1a TYPE           vrsinfolna, " Version Management: Info-Line First Part (Name)
                ls_infoline1b TYPE           vrsinfolnb, " Version Management: Info-Line Second Part (Request Info)
                ls_infoline2a TYPE           vrsinfolna, " Version Management: Info-Line First Part (Name)
                ls_infoline2b TYPE           vrsinfolnb, " Version Management: Info-Line Second Part (Request Info)
                lv_cmpdisp    TYPE           progname.   " ABAP Program Name

    CONSTANTS : lc_epp        TYPE            rfcdest           VALUE 'EPPCLNT100'. " ABAP System Field: Current Transaction Code


    LOOP AT lt_alv ASSIGNING FIELD-SYMBOL(<lfs_alv>).

      CALL FUNCTION 'SVRS_GET_VERSION_DIRECTORY_46'
        EXPORTING
          objname                = <lfs_alv>-objname
          objtype                = <lfs_alv>-objtype
        TABLES
          lversno_list           = lt_versno
          version_list           = lt_vrsd
        EXCEPTIONS
          no_entry               = 1
          communication_failure_ = 2
          system_failure         = 3
          OTHERS                 = 4.
      IF sy-subrc <> 0.
* Implement suitable error handling here
      ELSE. " ELSE -> IF sy-subrc <> 0
        IF sy-sysid = 'E2D'.
** Find Only Active WorkBech Request
          SELECT trkorr,
                 trfunction         " Type of request/task
                 FROM e070          " Transport system: Headers of requests/tasks
                 INTO TABLE @DATA(lt_wrtr)
                 FOR ALL ENTRIES IN @lt_vrsd
                 WHERE
                 trkorr     EQ @lt_vrsd-korrnum  AND
                 trfunction EQ 'K'. " K = WorkBech TR

          CLEAR ls_vrsd.
          LOOP AT lt_vrsd INTO ls_vrsd.
** Table lt_wrtr have only Workbech TR
            READ TABLE lt_wrtr INTO DATA(ls_wrtr)
                               WITH KEY
                                    trkorr = ls_vrsd-korrnum.
            IF sy-subrc EQ 0.

              APPEND VALUE #(  trnum  = ls_vrsd-korrnum
                               versno = ls_vrsd-versno
                               trtype = ls_wrtr-trfunction )
                               TO  lt_acttr.
            ENDIF. " IF sy-subrc EQ 0
          ENDLOOP. " LOOP AT lt_vrsd INTO ls_vrsd

          SORT lt_acttr BY versno DESCENDING.

          CLEAR ls_vrsd.
          READ TABLE lt_acttr INTO DATA(ls_acttr) INDEX 1. " Read Latest Active Workbech TR Version
          IF sy-subrc EQ 0.
            <lfs_alv>-actvtr = ls_acttr-trnum. " last Active TR Number
          ENDIF. " IF sy-subrc EQ 0

          CLEAR  : lt_wrtr,
                   lt_acttr,
                   ls_acttr,
                   ls_wrtr,
                   ls_vrsd.

        ELSEIF sy-sysid = 'EPP'. " ELSE -> IF sy-sysid = 'E2D'

          READ TABLE lt_acttr INTO ls_acttr INDEX 2. " Read Latest Active Workbech TR Version
          IF sy-subrc EQ 0.
            <lfs_alv>-actvtr = ls_acttr-trnum. " last Active TR Number
            CLEAR ls_vrsd.
          ENDIF. " IF sy-subrc EQ 0

        ENDIF. " IF sy-sysid = 'E2D'
        READ TABLE lt_vrsd INTO  ls_vrsd
                            WITH  KEY versno = '00000'.

        ls_infoline1b-korrnum  = ls_vrsd-korrnum .
        ls_infoline1b-author  = ls_vrsd-author .
        ls_infoline1b-datum =  ls_vrsd-datum.

        CALL FUNCTION 'SVRS_GET_VERSION_DIRECTORY_46'
          DESTINATION lc_epp
          EXPORTING
            objname      = <lfs_alv>-objname
            objtype      = <lfs_alv>-objtype
          TABLES
            lversno_list = lt_versno_rem
            version_list = lt_vrsd_rem
          EXCEPTIONS
            no_entry     = 1
            OTHERS       = 2.
        IF sy-subrc <> 0.

        ELSE. " ELSE -> IF sy-subrc <> 0

          READ TABLE lt_vrsd_rem WITH
                                 KEY
                                 korrnum = <lfs_alv>-actvtr
                                 TRANSPORTING NO FIELDS.
          IF sy-subrc EQ 0.
            <lfs_alv>-status = '@S_OKAY@'.
          ELSE. " ELSE -> IF sy-subrc EQ 0
            <lfs_alv>-status = '@S_NONO@'.
          ENDIF. " IF sy-subrc EQ 0

        ENDIF. " IF sy-subrc <> 0
      ENDIF. " IF sy-subrc <> 0
    ENDLOOP. " LOOP AT lt_alv ASSIGNING FIELD-SYMBOL(<lfs_alv>)




  ENDMETHOD.

  METHOD email_review.

    DATA : ooutapp    TYPE ole2_object , "Outlook.Application
           omail      TYPE ole2_object , "Outlook.MailItem
           lattach    TYPE ole2_object , "Outlook.Attachment
           lattachmsg TYPE ole2_object,
           ls_email   TYPE string.

** Find User Invloved in Transport Number
    SELECT DISTINCT
           as4user   " Owner of a Request or Task
           FROM e070 " Transport system: Headers of requests/tasks
           INTO TABLE @DATA(lt_users)
           WHERE
           strkorr EQ @im_data-trnum.

** Find Email ID for Users
    SELECT a~persnumber,
    a~addrnumber,
    b~smtp_addr " E-Mail Address
    FROM usr21 AS a
    INNER JOIN adr6 AS b
    ON
    a~persnumber EQ b~persnumber AND
    a~addrnumber EQ b~addrnumber
    INTO TABLE @DATA(lt_email)
    FOR ALL ENTRIES IN @lt_users
    WHERE
    a~bname EQ @lt_users-as4user.

** Add all email in one string
    LOOP AT lt_email ASSIGNING FIELD-SYMBOL(<lfs_email>).
      IF sy-tabix EQ 1.
        ls_email = <lfs_email>-smtp_addr.
      ELSE. " ELSE -> IF sy-tabix EQ 1
        ls_email = |{ ls_email }| & |;| & |{ <lfs_email>-smtp_addr }|.
      ENDIF. " IF sy-tabix EQ 1
    ENDLOOP. " LOOP AT lt_email ASSIGNING FIELD-SYMBOL(<lfs_email>)

* Email Body, Subj, Text
    CREATE OBJECT ooutapp 'Outlook.Application'.
    IF sy-subrc NE 0.
      EXIT.
    ENDIF. " IF sy-subrc NE 0

    CALL METHOD OF ooutapp 'CreateItem' = omail
      EXPORTING
      #1 = 0.
    SET PROPERTY OF omail 'Importance' = 2 .

** Email Address
    SET PROPERTY OF omail 'To' =  ls_email.

** Subject Line
    DATA(ls_subject) = | Code Review for TR -| & |{ im_data-trnum }| .
    SET PROPERTY OF omail 'Subject' = ls_subject.

    SET PROPERTY OF omail 'Body' = 'Body text...'.

** Open Outlook in display mode
    CALL METHOD OF omail 'Display'.

    FREE OBJECT omail.

  ENDMETHOD.

ENDCLASS.


DATA lr_version TYPE REF TO lcl_version. " Version class

INITIALIZATION.
  CREATE OBJECT lr_version.

START-OF-SELECTION.

  lr_version->fetch_data( ).
  lr_version->display_alv( ).
