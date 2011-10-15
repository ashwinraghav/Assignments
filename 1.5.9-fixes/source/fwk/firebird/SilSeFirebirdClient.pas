{********************************************************************************
 *                  Standard Interface Library (SIL)                            *
 *                                                                              *
 *       General purpose library whose design is based in STRONG                *
 *   use of interfaces.                                                         *
 *                                                                              *
 *                                                                              *
 *     Copyright (C) 2000 Mariano Podestá    antiriad@gmail.com                 *
 *     Copyright (C) 2000 Leandro Conde      lconde@str.com.ar                  *
 *     Copyright (C) 2000 Lisandro Podestá   lisandrop@movi.com.ar              *
 *                                                                              *
 *     See License.txt for details.                                             *
 *                                                                              *
 *   This library is free software; you can redistribute it and/or              *
 *   modify it under the terms of the GNU Lesser General Public                 *
 *   License as published by the Free Software Foundation; either               *
 *   version 2.1 of the License, or (at your option) any later version.         *
 *                                                                              *
 *   This library is distributed in the hope that it will be useful,            *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of             *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU          *
 *   Lesser General Public License for more details.                            *
 *                                                                              *
 *   You should have received a copy of the GNU Lesser General Public           *
 *   License along with this library; if not, write to the Free Software        *
 *   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA  *
 *                                                                              *
 ********************************************************************************}

unit SilSeFirebirdClient;

{$INCLUDE Defines.inc}

interface

uses
  SilBeTypes,
  SilSeFirebirdDlls;

// Delphi consts

const //Days of week
  dSun = 1;
  dMon = 2;
  dTue = 3;
  dWed = 4;
  dThu = 5;
  dFri = 6;
  dSat = 7;

const { Months of year }
  dJan = 1;
  dFeb = 2;
  dMar = 3;
  dApr = 4;
  dMay = 5;
  dJun = 6;
  dJul = 7;
  dAug = 8;
  dSep = 9;
  dOct = 10;
  dNov = 11;
  dDec = 12;


// C Consts 
const
  cYearOffset = 1900;

const { Days of week }
  cSun = 0;
  cMon = 1;
  cTue = 2;
  cWed = 3;
  cThu = 4;
  cFri = 5;
  cSat = 6;

const { Months of year }
  cJan = 0;
  cFeb = 1;
  cMar = 2;
  cApr = 3;
  cMay = 4;
  cJun = 5;
  cJul = 6;
  cAug = 7;
  cSep = 8;
  cOct = 9;
  cNov = 10;
  cDec = 11;

const
  ISC_TRUE  = 1;
  ISC_FALSE = 0;
  

const
  DSQL_close                    = 1;
  DSQL_drop                     = 2;
  DSQL_cancel                   = 4;

const
  METADATALENGTH                = 68;

const
  SQLDA_VERSION1                = 1; (* pre V7.0 SQLDA *)
  SQLDA_VERSION2                = 2; (*     V7.0 SQLDA *)

const
  SQLDA_CURRENT_VERSION         = SQLDA_VERSION2;

const
  IBASE_DLL                     = SilSeFirebirdDlls.IBASE_DLL;

const
(* meaning is same as DIALECT_xsqlda *)
  SQL_DIALECT_V5                = 1;

const
(* flagging anything that is delimited by double quotes as an error and flagging keyword DATE as an error *)
  SQL_DIALECT_V6_TRANSITION     = 2;

const
(* supports SQL delimited identifier,
    SQLDATE/DATE, TIME, TIMESTAMP, CURRENT_DATE,
    CURRENT_TIME, CURRENT_TIMESTAMP,
    and 64-bit exact numeric type *)
  SQL_DIALECT_V6                = 3;

const
  SQL_DIALECT_CURRENT           = SQL_DIALECT_V6; (* latest DIALECT *)

type
  Int                           = SilBeTypes.Int32;               { 32 bit signed }
  UInt                          = SilBeTypes.Uint32;              { 32 bit unsigned }
  Long                          = SilBeTypes.Int32;               { 32 bit signed }
  ULong                         = SilBeTypes.Uint32;              { 32 bit unsigned }
  Short                         = SilBeTypes.Int16;               { 16 bit signed }
  UShort                        = SilBeTypes.Uint16;              { 16 bit unsigned }
  Float                         = SilBeTypes.Float32;             { 32 bit }
  UChar                         = SilBeTypes.Uint8;               { 8 bit unsigned }
  Large                         = SilBeTypes.Int64;               { 64 bit signed  }
  Void                          = System.Pointer;

type
  ISC_LONG                      = Long;
  UISC_LONG                     = ULong;
  ISC_INT64                     = Large;
  ISC_BOOLEAN                   = Short;
  ISC_STATUS                    = Long;
  UISC_STATUS                   = ULong;

type
  PStatusVector                 = ^TStatusVector;
  TStatusVector                 = array[0..19] of ISC_STATUS;

type
  PPChar                        = ^PChar;
  PSmallInt                     = ^SmallInt;
  PInt                          = ^Int;
  PInteger                      = ^Integer;
  PShort                        = ^Short;
  PUShort                       = ^UShort;
  PLong                         = ^Long;
  PULong                        = ^ULong;
  PFloat                        = ^Float;
  PUChar                        = ^UChar;
  PVoid                         = ^Pointer;
  PDouble                       = ^Double;
  PISC_LONG                     = ^ISC_LONG;
  PUISC_LONG                    = ^UISC_LONG;
  PISC_STATUS                   = ^ISC_STATUS;
  PPISC_STATUS                  = ^PISC_STATUS;
  PUISC_STATUS                  = ^UISC_STATUS;
                                
type  //  C Date/Time Structure
  PTimeType = ^TTimeType;
  TTimeType = record
    tm_sec                      : integer;    { Seconds }
    tm_min                      : integer;    { Minutes }
    tm_hour                     : integer;    { Hour (0--23) }
    tm_mday                     : integer;    { Day of month (1--31) }
    tm_mon                      : integer;    { Month (0--11) }
    tm_year                     : integer;    { Year (calendar year minus 1900) }
    tm_wday                     : integer;    { Weekday (0--6) Sunday = 0) }
    tm_yday                     : integer;    { Day of year (0--365) }
    tm_isdst                    : integer;  { 0 if daylight savings time is not in effect) }
  end;

type
  PCTimeStructure               = PTimeType;
  TCTimeStructure               = TTimeType;

type
  PTM                           = ^TM;
  TM                            = TTimeType;

type
  PISC_VARYING = ^TISC_VARYING;
  TISC_VARYING = record
    strlen: Short;
    str: array[0..0] of Char;
  end;

type
  PDataVarying = PISC_VARYING;
  
type
  TISC_BlobGetSegment = function(
          BlobHandle: PInt;
          Buffer: PChar;
          BufferSize: Long;
      var ResultLength: Long): Short; cdecl;
      
  TISC_BlobPutSegment = procedure(
          BlobHandle: PInt;
          Buffer: PChar;
          BufferLength: Short); cdecl;
          
type
  PBlob = ^TBlob;
  TBlob = record
    GetSegment         : TISC_BlobGetSegment;
    BlobHandle         : PInt;
    SegmentCount       : Long;
    MaxSegmentLength   : Long;
    TotalSize          : Long;
    PutSegment         : TISC_BlobPutSegment;
  end;
  
type
  TISC_ATT_HANDLE               = PVoid;
  PISC_ATT_HANDLE               = ^TISC_ATT_HANDLE;
  TISC_BLOB_HANDLE              = PVoid;
  PISC_BLOB_HANDLE              = ^TISC_BLOB_HANDLE;
  TISC_DB_HANDLE                = PVoid;
  PISC_DB_HANDLE                = ^TISC_DB_HANDLE;
  TISC_FORM_HANDLE              = PVoid;
  PISC_FORM_HANDLE              = ^TISC_FORM_HANDLE;
  TISC_REQ_HANDLE               = PVoid;
  PISC_REQ_HANDLE               = ^TISC_REQ_HANDLE;
  TISC_STMT_HANDLE              = PVoid;
  PISC_STMT_HANDLE              = ^TISC_STMT_HANDLE;
  TISC_SVC_HANDLE               = PVoid;
  PISC_SVC_HANDLE               = ^TISC_SVC_HANDLE;
  TISC_TR_HANDLE                = PVoid;
  PISC_TR_HANDLE                = ^TISC_TR_HANDLE;
  TISC_WIN_HANDLE               = PVoid;
  PISC_WIN_HANDLE               = ^TISC_WIN_HANDLE;
  TISC_CALLBACK                 = procedure;
  ISC_SVC_HANDLE                = ISC_LONG;

const
  TIME_SECONDS_PRECISION        = 10000;
  TIME_SECONDS_PRECISION_SCALE  = -4;

type
  PISC_DATE                     = ^ISC_DATE;
  ISC_DATE                      = Long;

type
  PISC_TIME                     = ^ISC_TIME;
  ISC_TIME                      = ULong;

type
  PISC_TIMESTAMP = ^TISC_TIMESTAMP;
  TISC_TIMESTAMP = record
    timestamp_date: ISC_DATE;
    timestamp_time: ISC_TIME;
  end;

type
  TGDS_QUAD = record
    gds_quad_high      : ISC_LONG;
    gds_quad_low       : UISC_LONG;
  end;
  TGDS__QUAD           = TGDS_QUAD;
  TISC_QUAD            = TGDS_QUAD;
  PGDS_QUAD            = ^TGDS_QUAD;
  PGDS__QUAD           = ^TGDS__QUAD;
  PISC_QUAD            = ^TISC_QUAD;

type
  PISC_ARRAY_BOUND = ^TISC_ARRAY_BOUND;
  TISC_ARRAY_BOUND = record
    array_bound_lower  : Short;
    array_bound_upper  : Short;
  end;

type
  PISC_ARRAY_DESC = ^TISC_ARRAY_DESC;
  TISC_ARRAY_DESC = record
    array_desc_dtype            : UChar;
    array_desc_scale            : Char;
    array_desc_length           : UShort;
    array_desc_field_name       : array[0..31] of Char;
    array_desc_relation_name    : array[0..31] of Char;
    array_desc_dimensions       : Short;
    array_desc_flags            : Short;
    array_desc_bounds           : array[0..15] of TISC_ARRAY_BOUND;
  end;

type
  PISC_ARRAY_DESC_V2 = ^TISC_ARRAY_DESC_V2;
  TISC_ARRAY_DESC_V2 = record
    array_desc_version          : Short;
    array_desc_dtype            : UChar;
    array_desc_subtype          : UChar;
    array_desc_scale            : Char;
    array_desc_length           : UShort;
    array_desc_field_name       : array[0..METADATALENGTH - 1] of Char;
    array_desc_relation_name    : array[0..METADATALENGTH - 1] of Char;
    array_desc_dimensions       : Short;
    array_desc_flags            : Short;
    array_desc_bounds           : array[0..15] of TISC_ARRAY_BOUND;
  end; 

const
  ARR_DESC_VERSION2             = 2;
  ARR_DESC_CURRENT_VERSION      = ARR_DESC_VERSION2;

type
  PISC_BLOB_DESC = ^TISC_BLOB_DESC;
  TISC_BLOB_DESC = record
    blob_desc_subtype           : Short;
    blob_desc_charset           : Short;
    blob_desc_segment_size      : Short;
    blob_desc_field_name        : array[0..31] of UChar;
    blob_desc_relation_name     : array[0..31] of UChar;
  end;

type
  PISC_BLOB_DESC_V2 = ^TISC_BLOB_DESC_V2;
  TISC_BLOB_DESC_V2 = record
    blob_desc_version           : Short;
    blob_desc_subtype           : Short;
    blob_desc_charset           : Short;
    blob_desc_segment_size      : Short;
    blob_desc_field_name        : array[0..METADATALENGTH - 1] of UChar;
    blob_desc_relation_name     : array[0..METADATALENGTH - 1] of UChar;
  end;

const
  BLB_DESC_VERSION2             = 2;
  BLB_DESC_CURRENT_VERSION      = BLB_DESC_VERSION2;

type
  TISC_BLOB_CTL_SOURCE_FUNCTION = function: ISC_STATUS;

type
  PISC_BLOB_CTL                 = ^TISC_BLOB_CTL;        // ISC_FAR
  TISC_BLOB_CTL = record
    (** Source filter **)
    ctl_source                  : TISC_BLOB_CTL_SOURCE_FUNCTION;
    (** Argument to pass to source filter **)
    ctl_source_handle           : PISC_BLOB_CTL;
    ctl_to_sub_type             : Short;                (** Target type **)
    ctl_from_sub_type           : Short;                (** Source type **)
    ctl_buffer_length           : UShort;               (** Length of buffer **)
    ctl_segment_length          : UShort;               (** Length of current segment **)
    ctl_bpb_length              : UShort;               (** Length of blob parameter **)
    (** block **)
    ctl_bpb                     : PChar;                (** Address of blob parameter **)
    (** block **)
    ctl_buffer                  : PUChar;               (** Address of segment buffer **)
    ctl_max_segment             : ISC_LONG;             (** Length of longest segment **)
    ctl_number_segments : ISC_LONG;                     (** Total number of segments **)
    ctl_total_length            : ISC_LONG;             (** Total length of blob **)
    ctl_status                  : PISC_STATUS;          (** Address of status vector **)
    ctl_data                    : array[0..7] of long;  (** Application specific data **)
  end;

type
  PBSTREAM = ^TBSTREAM;
  TBSTREAM = record
    bstr_blob                   : PVoid;                 (** Blob handle **)
    bstr_buffer                 : PChar;                 (** Address of buffer **)
    bstr_ptr                    : PChar;                 (** Next character **)
    bstr_length                 : Short;                 (** Length of buffer **)
    bstr_cnt                    : Short;                 (** Characters in buffer **)
    bstr_mode                   : Char;                  (** (mode) ? OUTPUT : INPUT **)
  end;

type
  PXSQLVAR_V1 = ^TXSQLVAR_V1;
  TXSQLVAR_V1 = record
    sqltype                     : Short;     (** datatype of field **)
    sqlscale                    : Short;     (** scale factor **)
    sqlsubtype                  : Short;     (** datatype subtype - BLOBs **)
     (** & text types only **)
    sqllen                      : Short;     (** length of data area **)
    sqldata                     : PChar;     (** address of data **)
    sqlind                      : PShort;    (** address of indicator variable **)
    sqlname_length              : Short;     (** length of sqlname field **)
    (** name of field, name length + space for NULL **)
    sqlname                     : array[0..31] of Char;
    relname_length              : Short;     (** length of relation name **)
    (** field's relation name + space for NULL **)
    relname                     : array[0..31] of Char;
    ownname_length              : Short;     (** length of owner name **)
    (** relation's owner name + space for NULL **)
    ownname                     : array[0..31] of Char;
    aliasname_length            : Short;     (** length of alias name **)
    (** relation's alias name + space for NULL **)
    aliasname                   : array[0..31] of Char;
  end;  // TXSQLVAR_V1

type
  PXSQLVAR_V2 = ^TXSQLVAR_V2;
  TXSQLVAR_V2 = record
    sqltype                     : Short;     (** datatype of field **)
    sqlscale                    : Short;     (** scale factor **)
    sqlprecision                : Short;     (** precision : Reserved for future **)
    sqlsubtype                  : Short;     (** datatype subtype - BLOBs **)
     (** & text types only **)
    sqllen                      : Short;     (** length of data area **)
    sqldata                     : PChar;     (** address of data **)
    sqlind                      : PShort;    (** address of indicator variable **)
    sqlname_length              : Short;     (** length of sqlname field **)
    (** name of field, name length + space for NULL **)
    sqlname                     : array[0..METADATALENGTH - 1] of Char;
    relname_length              : Short;     (** length of relation name **)
    (** field's relation name + space for NULL **)
    relname                     : array[0..METADATALENGTH - 1] of Char;
    ownname_length              : Short;     (** length of owner name **)
    (** relation's owner name + space for NULL **)
    ownname                     : array[0..METADATALENGTH - 1] of Char;
    aliasname_length            : Short;     (** length of alias name **)
    (** relation's alias name + space for NULL **)
    aliasname                   : array[0..METADATALENGTH - 1] of Char;
  end; 

type
  PXSQLVAR  = PXSQLVAR_V2;

type
  PXSQLDA = ^TXSQLDA;
  TXSQLDA = record
    version                     : Short;     (** version of this XSQLDA **)
    (** XSQLDA name field **)
    sqldaid                     : array[0..7] of Char;
    sqldabc                     : ISC_LONG;  (** length in bytes of SQLDA **)
    sqln                        : Short;     (** number of fields allocated **)
    sqld                        : Short;     (** actual number of fields **)
    (** first field address **)
    sqlvar                      : record end;
  end;

type
  PXSQLDA_V1 = ^TXSQLDA_V1;
  TXSQLDA_V1 = record
    version                     : Short;     (** version of this XSQLDA **)
    (** XSQLDA name field **)
    sqldaid                     : array[0..7] of Char;
    sqldabc                     : ISC_LONG;  (** length in bytes of SQLDA **)
    sqln                        : Short;     (** number of fields allocated **)
    sqld                        : Short;     (** actual number of fields **)
    (** first field address **)
    sqlvar                      : array[0..0] of TXSQLVAR_V1;
  end;

type
  PXSQLDA_V2 = ^TXSQLDA_V2;
  TXSQLDA_V2 = record
    version                     : Short;     (** version of this XSQLDA **)
    (** XSQLDA name field **)
    sqldaid                     : array[0..7] of Char;
    sqldabc                     : ISC_LONG;  (** length in bytes of SQLDA **)
    sqln                        : Short;     (** number of fields allocated **)
    sqld                        : Short;     (** actual number of fields **)
    (** first field address **)
    sqlvar                      : array[0..0] of TXSQLVAR_V2;
  end;

type
  TISC_START_TRANS = record
    db_handle      : PISC_DB_HANDLE;
    tpb_length     : UShort;
    tpb_address    : PChar;
  end;

type
  PISC_TEB = ^TISC_TEB;
  TISC_TEB = record
    db_handle      : PISC_DB_HANDLE;
    tpb_length     : Long;
    tpb_address    : PChar;
  end;

type
  TISC_TEB_ARRAY = array[0..0] of TISC_TEB;
  PISC_TEB_ARRAY = ^TISC_TEB_ARRAY;

type
  PUserSecData = ^TUserSecData;
  TUserSecData = record
    sec_flags: Short;           (** which fields are specified **)
    uid: Int;                   (** the user's id **)
    gid: int;                   (** the user's group id **)
    protocol: Int;              (** protocol to use for connection **)
    server: PChar;              (** server to administer **)
    user_name: PChar;           (** the user's name **)
    password: PChar;            (** the user's password **)
    group_name: PChar;          (** the group name **)
    first_name: PChar;          (** the user's first name **)
    middle_name: PChar;         (** the user's middle name **)
    last_name: PChar;           (** the user's last name **)
    dba_user_name: PChar;       (** the dba user name **)
    dba_password: PChar;        (** the dba password **)
  end;
  
type
  Tisc_attach_database = function (
      status_vector             : PISC_STATUS;
      db_name_length            : Short;
      db_name                   : PChar;
      db_handle                 : PISC_DB_HANDLE;
      parm_buffer_length        : Short;
      parm_buffer               : PChar): ISC_STATUS; stdcall;

type
  Tisc_array_gen_sdl = function   (
      status_vector             : PISC_STATUS;
      isc_array_desc            : PISC_ARRAY_DESC;
      isc_arg3                  : PShort;
      isc_arg4                  : PChar;
      isc_arg5                  : PShort): ISC_STATUS; stdcall;

type
  Tisc_array_gen_sdl2 = function  (
      status_vector             : PISC_STATUS;
      isc_array_desc            : PISC_ARRAY_DESC_V2;
      isc_arg3                  : PShort;
      isc_arg4                  : PChar;
      isc_arg5                  : PShort): ISC_STATUS; stdcall;

type
  Tisc_array_get_slice = function (
      status_vector             : PISC_STATUS;
      db_handle                 : PISC_DB_HANDLE;
      trans_handle              : PISC_TR_HANDLE;
      array_id                  : PISC_QUAD;
      descriptor                : PISC_ARRAY_DESC;
      dest_array                : PVoid;
      slice_length              : ISC_LONG): ISC_STATUS; stdcall;

type
  Tisc_array_get_slice2 = function(
      status_vector             : PISC_STATUS;
      db_handle                 : PISC_DB_HANDLE;
      trans_handle              : PISC_TR_HANDLE;
      array_id                  : PISC_QUAD;
      descriptor                : PISC_ARRAY_DESC_V2;
      dest_array                : PVoid;
      slice_length              : ISC_LONG): ISC_STATUS; stdcall;

type
  Tisc_array_lookup_bounds = function (
      status_vector             : PISC_STATUS;
      db_handle                 : PISC_DB_HANDLE;
      trans_handle              : PISC_TR_HANDLE;
      table_name                : PChar;
      column_name               : PChar;
      descriptor                : PISC_ARRAY_DESC): ISC_STATUS; stdcall;

type
  Tisc_array_lookup_bounds2 = function (
      status_vector             : PISC_STATUS;
      db_handle                 : PISC_DB_HANDLE;
      trans_handle              : PISC_TR_HANDLE;
      table_name                : PChar;
      column_name               : PChar;
      descriptor                : PISC_ARRAY_DESC_V2): ISC_STATUS; stdcall;

type
  Tisc_array_lookup_desc = function (
      status_vector             : PISC_STATUS;
      db_handle                 : PISC_DB_HANDLE;
      trans_handle              : PISC_TR_HANDLE;
      table_name                : PChar;
      column_name               : PChar;
      descriptor                : PISC_ARRAY_DESC): ISC_STATUS; stdcall;

type
  Tisc_array_lookup_desc2 = function (
      status_vector             : PISC_STATUS;
      db_handle                 : PISC_DB_HANDLE;
      trans_handle              : PISC_TR_HANDLE;
      table_name                : PChar;
      column_name               : PChar;
      descriptor                : PISC_ARRAY_DESC_V2): ISC_STATUS; stdcall;

type
  Tisc_array_set_desc = function  (
      status_vector             : PISC_STATUS;
      table_name                : PChar;
      column_name               : PChar;
      sql_dtype                 : PShort;
      sql_length                : PShort;
      sql_dimensions            : PShort;
      descriptor                : PISC_ARRAY_DESC): ISC_STATUS; stdcall;

type
  Tisc_array_set_desc2 = function  (
      status_vector             : PISC_STATUS;
      table_name                : PChar;
      column_name               : PChar;
      sql_dtype                 : PShort;
      sql_length                : PShort;
      sql_dimensions            : PShort;
      descriptor                : PISC_ARRAY_DESC_V2): ISC_STATUS; stdcall;

type
  Tisc_array_put_slice = function (
      status_vector             : PISC_STATUS;
      db_handle                 : PISC_DB_HANDLE;
      trans_handle              : PISC_TR_HANDLE;
      array_id                  : PISC_QUAD;
      descriptor                : PISC_ARRAY_DESC;
      source_array              : PVoid;
      slice_length              : PISC_LONG): ISC_STATUS; stdcall;

type
  Tisc_array_put_slice2 = function (
      status_vector             : PISC_STATUS;
      db_handle                 : PISC_DB_HANDLE;
      trans_handle              : PISC_TR_HANDLE;
      array_id                  : PISC_QUAD;
      descriptor                : PISC_ARRAY_DESC_V2;
      source_array              : PVoid;
      slice_length              : PISC_LONG): ISC_STATUS; stdcall;

type
  Tisc_blob_default_desc = procedure  (
      descriptor                : PISC_BLOB_DESC;
      table_name                : PUChar;
      column_name               : PUChar); stdcall;

type
  Tisc_blob_default_desc2 = procedure  (
      descriptor                : PISC_BLOB_DESC_V2;
      table_name                : PUChar;
      column_name               : PUChar); stdcall;

type
  Tisc_blob_gen_bpb = function    (
      status_vector             : PISC_STATUS;
      to_descriptor             : PISC_BLOB_DESC;
      from_descriptor           : PISC_BLOB_DESC;
      bpb_buffer_length         : UShort;
      bpb_buffer                : PUChar;
      bpb_length                : PUShort): ISC_STATUS; stdcall;

type
  Tisc_blob_gen_bpb2 = function    (
      status_vector             : PISC_STATUS;
      to_descriptor             : PISC_BLOB_DESC_V2;  
      from_descriptor           : PISC_BLOB_DESC_V2;
      bpb_buffer_length         : UShort;
      bpb_buffer                : PUChar;
      bpb_length                : PUShort): ISC_STATUS; stdcall;

type
  Tisc_blob_info = function       (
      status_vector             : PISC_STATUS;
      blob_handle               : PISC_BLOB_HANDLE;
      item_list_buffer_length   : Short;
      item_list_buffer          : PChar;
      result_buffer_length      : Short;
      result_buffer             : PChar): ISC_STATUS; stdcall;

type
  Tisc_blob_lookup_desc = function (
      status_vector             : PISC_STATUS;
      db_handle                 : PISC_DB_HANDLE;
      trans_handle              : PISC_TR_HANDLE;
      table_name                : PChar;
      column_name               : PChar;
      descriptor                : PISC_BLOB_DESC;
      global                    : PUChar): ISC_STATUS; stdcall;

type
  Tisc_blob_lookup_desc2 = function (
      status_vector             : PISC_STATUS;
      db_handle                 : PISC_DB_HANDLE;
      trans_handle              : PISC_TR_HANDLE;
      table_name                : PChar;
      column_name               : PChar;
      descriptor                : PISC_BLOB_DESC_v2;
      global                    : PUChar): ISC_STATUS; stdcall;

type
  Tisc_blob_set_desc = function   (
      status_vector             : PISC_STATUS;
      table_name                : PChar;
      column_name               : PChar;
      subtype                   : Short;
      charset                   : Short;
      segment_size              : Short;
      descriptor                : PISC_BLOB_DESC): ISC_STATUS; stdcall;

type
  Tisc_blob_set_desc2 = function   (
      status_vector             : PISC_STATUS;
      table_name                : PChar;
      column_name               : PChar;
      subtype                   : Short;
      charset                   : Short;
      segment_size              : Short;
      descriptor                : PISC_BLOB_DESC_V2): ISC_STATUS; stdcall;

type
  Tisc_cancel_blob = function     (
      status_vector             : PISC_STATUS;
      blob_handle               : PISC_BLOB_HANDLE): ISC_STATUS; stdcall;
                                
type                            
  Tisc_cancel_events = function   (
      status_vector             : PISC_STATUS;
      db_handle                 : PISC_DB_HANDLE;
      event_id                  : PISC_LONG): ISC_STATUS; stdcall;

type
  Tisc_close_blob = function      (
      status_vector             : PISC_STATUS;
      blob_handle               : PISC_BLOB_HANDLE): ISC_STATUS; stdcall;

type
  Tisc_commit_retaining = function (
      status_vector             : PISC_STATUS;
      tran_handle               : PISC_TR_HANDLE): ISC_STATUS; stdcall;

type
  Tisc_commit_transaction = function  (
      status_vector             : PISC_STATUS;
      tran_handle               : PISC_TR_HANDLE): ISC_STATUS; stdcall;

type
  Tisc_create_blob = function     (
      status_vector             : PISC_STATUS;
      db_handle                 : PISC_DB_HANDLE;
      tran_handle               : PISC_TR_HANDLE;
      blob_handle               : PISC_BLOB_HANDLE;
      blob_id                   : PISC_QUAD): ISC_STATUS; stdcall;

type
  Tisc_create_blob2 = function    (
      status_vector             : PISC_STATUS;
      db_handle                 : PISC_DB_HANDLE;
      tran_handle               : PISC_TR_HANDLE;
      blob_handle               : PISC_BLOB_HANDLE;
      blob_id                   : PISC_QUAD;
      bpb_length                : Short;
      bpb_address               : PChar): ISC_STATUS; stdcall;

type
  Tisc_create_database = function (
      status_vector             : PISC_STATUS;
      isc_arg2                  : Short;
      isc_arg3                  : PChar;
      db_handle                 : PISC_DB_HANDLE;
      isc_arg5                  : Short;
      isc_arg6                  : PChar;
      isc_arg7                  : Short): ISC_STATUS; stdcall;

type
  Tisc_database_info = function   (
      status_vector             : PISC_STATUS;
      db_handle                 : PISC_DB_HANDLE;
      item_list_buffer_length   : Short;
      item_list_buffer          : PChar;
      result_buffer_length      : Short;
      result_buffer             : PChar): ISC_STATUS; stdcall;

type
  Tisc_decode_date = procedure    (
      ib_date                   : PISC_QUAD;
      tm_date                   : PCTimeStructure); stdcall;

type
  Tisc_decode_sql_date = procedure (
      ib_date                   : PISC_DATE;
      tm_date                   : PCTimeStructure); stdcall;

type
  Tisc_decode_sql_time = procedure  (
      ib_time                   : PISC_TIME;
      tm_date                   : PCTimeStructure); stdcall;

type
  Tisc_decode_timestamp = procedure  (
      ib_timestamp              : PISC_TIMESTAMP;
      tm_date                   : PCTimeStructure); stdcall;

type
  Tisc_detach_database = function (
      status_vector             : PISC_STATUS;
      db_handle                 : PISC_DB_HANDLE): ISC_STATUS; stdcall;

type
  Tisc_drop_database = function   (
      status_vector             : PISC_STATUS;
      db_handle                 : PISC_DB_HANDLE): ISC_STATUS; stdcall;

type
  Tisc_dsql_allocate_statement = function (
      status_vector             : PISC_STATUS;
      db_handle                 : PISC_DB_HANDLE;
      stmt_handle               : PISC_STMT_HANDLE): ISC_STATUS; stdcall;

type
  Tisc_dsql_alloc_statement2 = function (
      status_vector             : PISC_STATUS;
      db_handle                 : PISC_DB_HANDLE;
      stmt_handle               : PISC_STMT_HANDLE): ISC_STATUS; stdcall;

type
  Tisc_dsql_describe = function   (
      status_vector             : PISC_STATUS;
      stmt_handle               : PISC_STMT_HANDLE;
      dialect                   : UShort;
      xsqlda                    : Pointer): ISC_STATUS; stdcall;

type                            
  Tisc_dsql_describe_bind = function  (
      status_vector             : PISC_STATUS;
      stmt_handle               : PISC_STMT_HANDLE;
      dialect                   : UShort;
      xsqlda                    : Pointer): ISC_STATUS; stdcall;

type
  Tisc_dsql_exec_immed2 = function (
      status_vector             : PISC_STATUS;
      db_handle                 : PISC_DB_HANDLE;
      tran_handle               : PISC_TR_HANDLE;
      length                    : UShort;
      statement                 : PChar;
      dialect                   : UShort;
      in_xsqlda,
      out_xsqlda                : Pointer): ISC_STATUS; stdcall;

type
  Tisc_dsql_execute = function    (
      status_vector             : PISC_STATUS;
      tran_handle               : PISC_TR_HANDLE;
      stmt_handle               : PISC_STMT_HANDLE;
      dialect                   : UShort;
      xsqlda                    : Pointer): ISC_STATUS; stdcall;

type
  Tisc_dsql_execute2 = function   (
      status_vector             : PISC_STATUS;
      tran_handle               : PISC_TR_HANDLE;
      stmt_handle               : PISC_STMT_HANDLE;
      dialect                   : UShort;
      in_xsqlda,
      out_xsqlda                : Pointer): ISC_STATUS; stdcall;

type
  Tisc_dsql_execute_immediate = function (status_vector     : PISC_STATUS;
      db_handle                 : PISC_DB_HANDLE;
      tran_handle               : PISC_TR_HANDLE;
      length                    : UShort;
      statement                 : PChar;
      dialect                   : UShort;
      xsqlda                    : Pointer): ISC_STATUS; stdcall;

type
  Tisc_dsql_fetch = function      (
      status_vector             : PISC_STATUS;
      stmt_handle               : PISC_STMT_HANDLE;
      dialect                   : UShort;
      xsqlda                    : Pointer): ISC_STATUS; stdcall;

type
  Tisc_dsql_finish = function    (
      db_handle                 : PISC_DB_HANDLE): ISC_STATUS; stdcall;

type
  Tisc_dsql_free_statement = function (
      status_vector             : PISC_STATUS;
      stmt_handle               : PISC_STMT_HANDLE;
      options                   : UShort): ISC_STATUS; stdcall;

type
  Tisc_dsql_insert = function     (
      status_vector             : PISC_STATUS;
      stmt_handle               : PISC_STMT_HANDLE;
      arg3                      : UShort;
      xsqlda                    : Pointer): ISC_STATUS;  stdcall;

type
  Tisc_dsql_prepare = function    (
      status_vector             : PISC_STATUS;
      tran_handle               : PISC_TR_HANDLE;
      stmt_handle               : PISC_STMT_HANDLE;
      length                    : UShort;
      statement                 : PChar;
      dialect                   : UShort;
      xsqlda                    : Pointer): ISC_STATUS; stdcall;

type
  Tisc_dsql_set_cursor_name = function (
      status_vector             : PISC_STATUS;
      stmt_handle               : PISC_STMT_HANDLE;
      cursor_name               : PChar;
      _type                     : UShort): ISC_STATUS; stdcall;

type
  Tisc_dsql_sql_info = function   (
      status_vector             : PISC_STATUS;
      stmt_handle               : PISC_STMT_HANDLE;
      item_length               : Short;
      items                     : PChar;
      buffer_length             : Short;
      buffer                    : PChar): ISC_STATUS; stdcall;

type
  Tisc_encode_date = procedure    (
      tm_date                   : PCTimeStructure;
      ib_date                   : PISC_QUAD); stdcall;

type
  Tisc_encode_sql_date = procedure (
      tm_date                   : PCTimeStructure;
      ib_date                   : PISC_DATE); stdcall;

type
  Tisc_encode_sql_time = procedure (
      tm_date                   : PCTimeStructure;
      ib_time                   : PISC_TIME); stdcall;

type
  Tisc_encode_timestamp = procedure (
      tm_date                   : PCTimeStructure;
      ib_timestamp              : PISC_TIMESTAMP); stdcall;

type
  Tisc_event_block = function     (
      event_buffer              : PPChar;
      result_buffer             : PPChar;
      id_count                  : UShort;
      event_list                : array of PChar): ISC_LONG; cdecl;

type
  Tisc_event_counts = procedure   (
      status_vector             : PISC_STATUS;
      buffer_length             : Short;
      event_buffer              : PChar;
      result_buffer             : PChar); stdcall;

type
  Tisc_expand_dpb = procedure     (
      dpb                       : PPChar;
      dpb_length                : PShort;
      item_list                 : array of Pointer); cdecl;

type
  Tisc_modify_dpb = function      (
      dpb                       : PPChar;
      isc_arg2,
      isc_arg3                  : PShort;
      isc_arg4                  : UShort;
      isc_arg5                  : PChar;
      isc_arg6                  : Short): Int; stdcall;

type
  Tisc_free = function           (
      isc_arg1                  : PChar): ISC_LONG; stdcall;

type
  Tisc_get_segment = function     (
      status_vector             : PISC_STATUS;
      blob_handle               : PISC_BLOB_HANDLE;
      actual_seg_length         : PUShort;
      seg_buffer_length         : UShort;
      seg_buffer                : PChar): ISC_STATUS; stdcall;

type
  Tisc_get_slice = function       (
      status_vector             : PISC_STATUS;
      db_handle                 : PISC_DB_HANDLE;
      tran_handle               : PISC_TR_HANDLE;
      isc_arg4                  : PISC_QUAD;
      isc_arg5                  : Short;
      isc_arg6                  : PChar;
      isc_arg7                  : Short;
      isc_arg8                  : PISC_LONG;
      isc_arg9                  : ISC_LONG;
      isc_arg10                 : PVoid;
      isc_arg11                 : PISC_LONG): ISC_STATUS; stdcall;

type
  Tisc_interprete = function      (
      buffer                    : PChar;
      status_vector             : PPISC_STATUS): ISC_STATUS; stdcall;

type
  Tisc_open_blob = function       (
      status_vector             : PISC_STATUS;
      db_handle                 : PISC_DB_HANDLE;
      tran_handle               : PISC_TR_HANDLE;
      blob_handle               : PISC_BLOB_HANDLE;
      blob_id                   : PISC_QUAD): ISC_STATUS; stdcall;

type
  Tisc_open_blob2 = function      (
      status_vector             : PISC_STATUS;
      db_handle                 : PISC_DB_HANDLE;
      tran_handle               : PISC_TR_HANDLE;
      blob_handle               : PISC_BLOB_HANDLE;
      blob_id                   : PISC_QUAD;
      bpb_length                : Short;
      bpb_buffer                : PChar): ISC_STATUS; stdcall;

type
  Tisc_prepare_transaction2 = function (
      status_vector             : PISC_STATUS;
      tran_handle               : PISC_TR_HANDLE;
      msg_length                : Short;
      msg                       : PChar): ISC_STATUS; stdcall;

type
  Tisc_print_sqlerror = procedure (
      sqlcode                   : Short;
      status_vector             : PISC_STATUS); stdcall;

type
  Tisc_print_status = function   (
      status_vector              : PISC_STATUS): ISC_STATUS; stdcall;

type
  Tisc_put_segment = function     (
      status_vector             : PISC_STATUS;
      blob_handle               : PISC_BLOB_HANDLE;
      seg_buffer_len            : UShort;
      seg_buffer                : PChar): ISC_STATUS; stdcall;

type
  Tisc_put_slice = function       (
      status_vector             : PISC_STATUS;
      db_handle                 : PISC_DB_HANDLE;
      tran_handle               : PISC_TR_HANDLE;
      isc_arg4                  : PISC_QUAD;
      isc_arg5                  : Short;
      isc_arg6                  : PChar;
      isc_arg7                  : Short;
      isc_arg8                  : PISC_LONG;
      isc_arg9                  : ISC_LONG;
      isc_arg10                 : PVoid): ISC_STATUS; stdcall;

type
  Tisc_que_events = function      (
      status_vector             : PISC_STATUS;
      db_handle                 : PISC_DB_HANDLE;
      event_id                  : PISC_LONG;
      length                    : Short;
      event_buffer              : PChar;
      event_function            : TISC_CALLBACK;
      event_function_arg        : PVoid): ISC_STATUS; stdcall;

type
  Tisc_rollback_retaining = function (
      status_vector             : PISC_STATUS;
      tran_handle               : PISC_TR_HANDLE): ISC_STATUS; stdcall;

type
  Tisc_rollback_transaction = function (
      status_vector             : PISC_STATUS;
      tran_handle               : PISC_TR_HANDLE): ISC_STATUS; stdcall;

type
  Tisc_start_multiple = function  (
      status_vector             : PISC_STATUS;
      tran_handle               : PISC_TR_HANDLE;
      db_handle_count           : Short;
      teb_vector_address        : PISC_TEB): ISC_STATUS; stdcall;

type
  Tisc_start_transaction = function (
      status_vector             : PISC_STATUS;
      tran_handle               : PISC_TR_HANDLE;
      db_handle_count           : Short;
      db_handle                 : PISC_DB_HANDLE;
      tpb_length                : UShort;
      tpb_address               : PChar): ISC_STATUS; cdecl;

type
  Tisc_sqlcode = function        (
      status_vector             : PISC_STATUS): ISC_LONG; stdcall;

type
  Tisc_sql_interprete = procedure (
      sqlcode                   : Short;
      buffer                    : PChar;
      buffer_length             : Short); stdcall;

type
  Tisc_transaction_info = function (
      status_vector             : PISC_STATUS;
      tran_handle               : PISC_TR_HANDLE;
      item_list_buffer_length   : Short;
      item_list_buffer          : PChar;
      result_buffer_length      : Short;
      result_buffer             : PChar): ISC_STATUS; stdcall;

type
  Tisc_transact_request = function (
      status_vector             : PISC_STATUS;
      db_handle                 : PISC_DB_HANDLE;
      tran_handle               : PISC_TR_HANDLE;
      isc_arg4                  : UShort;
      isc_arg5                  : PChar;
      isc_arg6                  : UShort;
      isc_arg7                  : PChar;
      isc_arg8                  : UShort;
      isc_arg9                  : PChar): ISC_STATUS; stdcall;

type
  Tisc_vax_integer = function     (
      buffer                    : PChar;
      length                    : Short): ISC_LONG; stdcall;

type
  Tisc_portable_integer = function (
      buffer                    : PChar;
      length                    : Short): ISC_INT64; stdcall;


type
  Tisc_add_user = function        (
      status_vector             : PISC_STATUS;
      user_sec_data             : PUserSecData): ISC_STATUS; stdcall;

type
  Tisc_delete_user = function     (
      status_vector             : PISC_STATUS;
      user_sec_data             : PUserSecData): ISC_STATUS; stdcall;

type
  Tisc_modify_user = function     (
      status_vector             : PISC_STATUS;
      user_sec_data             : PUserSecData): ISC_STATUS; stdcall;

(************************************)
(**  Other OSRI functions          **)
(************************************)

type
  Tisc_compile_request = function (
      status_vector             : PISC_STATUS;
      db_handle                 : PISC_DB_HANDLE;
      request_handle            : PISC_REQ_HANDLE;
      isc_arg4                  : Short;
      isc_arg5                  : PChar): ISC_STATUS; stdcall;

type
  Tisc_compile_request2 = function (
      status_vector            : PISC_STATUS;
      db_handle                 : PISC_DB_HANDLE;
      request_handle            : PISC_REQ_HANDLE;
      isc_arg4                  : Short;
      isc_arg5                  : PChar): ISC_STATUS; stdcall;

type
  Tisc_ddl = function             (
      status_vector             : PISC_STATUS;
      db_handle                 : PISC_DB_HANDLE;
      tran_handle               : PISC_TR_HANDLE;
      isc_arg4                  : Short;
      isc_arg5                  : PChar): ISC_STATUS; stdcall;

type
  Tisc_prepare_transaction = function (
      status_vector         : PISC_STATUS;
      tran_handle               : PISC_TR_HANDLE): ISC_STATUS; stdcall;


type
  Tisc_receive = function         (
      status_vector             : PISC_STATUS;
      request_handle            : PISC_REQ_HANDLE;
      isc_arg3,
      isc_arg4                  : Short;
      isc_arg5                  : PVoid;
      isc_arg6                  : Short): ISC_STATUS; stdcall;

type
  Tisc_receive2 = function        (
      status_vector             : PISC_STATUS;
      request_handle            : PISC_REQ_HANDLE;
      isc_arg3,
      isc_arg4                  : Short;
      isc_arg5                  : PVoid;
      isc_arg6,
      isc_arg7                  : Short;
      isc_arg8                  : Long): ISC_STATUS; stdcall;

type
  Tisc_reconnect_transaction = function (
      status_vector       : PISC_STATUS;
      db_handle                 : PISC_DB_HANDLE;
      tran_handle               : PISC_TR_HANDLE;
      isc_arg4                  : Short;
      isc_arg5                  : PChar): ISC_STATUS; stdcall;

type
  Tisc_release_request = function (
      status_vector             : PISC_STATUS;
      request_handle            : PISC_REQ_HANDLE): ISC_STATUS; stdcall;

type
  Tisc_request_info = function    (
      status_vector             : PISC_STATUS;
      request_handle            : PISC_REQ_HANDLE;
      isc_arg3                  : Short;
      isc_arg4                  : Short;
      isc_arg5                  : PChar;
      isc_arg6                  : Short;
      isc_arg7                  : PChar): ISC_STATUS; stdcall;

type
  Tisc_seek_blob = function       (
      status_vector             : PISC_STATUS;
      blob_handle               : PISC_BLOB_HANDLE;
      isc_arg3                  : Short;
      isc_arg4                  : ISC_LONG;
      isc_arg5                  : PISC_LONG): ISC_STATUS; stdcall;

type
  Tisc_send = function            (
      status_vector             : PISC_STATUS;
      request_handle            : PISC_REQ_HANDLE;
      isc_arg3,
      isc_arg4                  : Short;
      isc_arg5                  : PVoid;
      isc_arg6                  : Short): ISC_STATUS; stdcall;

type
  Tisc_start_and_send = function  (
      status_vector             : PISC_STATUS;
      request_handle            : PISC_REQ_HANDLE;
      tran_handle               : PISC_TR_HANDLE;
      isc_arg4,
      isc_arg5                  : Short;
      isc_arg6                  : PVoid;
      isc_arg7                  : Short): ISC_STATUS; stdcall;

type
  Tisc_start_request = function   (
      status_vector             : PISC_STATUS;
      request_handle            : PISC_REQ_HANDLE;
      tran_handle               : PISC_TR_HANDLE;
      isc_arg4                  : Short): ISC_STATUS; stdcall;

type
  Tisc_unwind_request = function  (
      status_vector             : PISC_STATUS;
      tran_handle               : PISC_TR_HANDLE;
      isc_arg3                  : Short): ISC_STATUS; stdcall;

type
  Tisc_wait_for_event = function  (
      status_vector             : PISC_STATUS;
      db_handle                 : PISC_DB_HANDLE;
      length                    : Short;
      event_buffer,
      result_buffer             : PChar): ISC_STATUS; stdcall;

  (*******************************)
  (** Other Sql functions       **)
  (*******************************)

type
  Tisc_close = function           (
      status_vector             : PISC_STATUS;
      isc_arg2                  : PChar): ISC_STATUS; stdcall;

type
  Tisc_declare = function         (
      status_vector             : PISC_STATUS;
      isc_arg2,
      isc_arg3                  : PChar): ISC_STATUS; stdcall;

type
  Tisc_describe = function        (
      status_vector             : PISC_STATUS;
      isc_arg2                  : PChar;
      isc_arg3                  : Pointer): ISC_STATUS; stdcall;

type
  Tisc_describe_bind = function   (
      status_vector             : PISC_STATUS;
      isc_arg2                  : PChar;
      isc_arg3                  : Pointer): ISC_STATUS; stdcall;

type
  Tisc_execute = function         (
      status_vector             : PISC_STATUS;
      tran_handle               : PISC_TR_HANDLE;
      isc_arg3                  : PChar;
      isc_arg4                  : Pointer): ISC_STATUS; stdcall;

type
  Tisc_execute_immediate = function (
      status_vector           : PISC_STATUS;
      db_handle                 : PISC_DB_HANDLE;
      tran_handle               : PISC_TR_HANDLE;
      isc_arg4                  : PShort;
      isc_arg5                  : PChar): ISC_STATUS; stdcall;

type
  Tisc_fetch = function           (
      status_vector             : PISC_STATUS;
      isc_arg2                  : PChar;
      isc_arg3                  : Pointer): ISC_STATUS; stdcall;

type
  Tisc_open = function            (
      status_vector             : PISC_STATUS;
      tran_handle               : PISC_TR_HANDLE;
      isc_arg3                  : PChar;
      isc_arg4                  : Pointer): ISC_STATUS; stdcall;

type
  Tisc_prepare = function         (
      status_vector             : PISC_STATUS;
      db_handle                 : PISC_DB_HANDLE;
      tran_handle               : PISC_TR_HANDLE;
      isc_arg4                  : PChar;
      isc_arg5                  : PShort;
      isc_arg6                  : PChar;
      isc_arg7                  : Pointer): ISC_STATUS; stdcall;

  (***************************************)
  (** Other Dynamic sql functions       **)
  (***************************************)

type
  Tisc_dsql_execute_m = function  (
      status_vector             : PISC_STATUS;
      tran_handle               : PISC_TR_HANDLE;
      statement_handle          : PISC_STMT_HANDLE;
      isc_arg4                  : UShort;
      isc_arg5                  : PChar;
      isc_arg6                  : UShort;
      isc_arg7                  : UShort;
      isc_arg8                  : PChar): ISC_STATUS; stdcall;

type
  Tisc_dsql_execute2_m = function (
      status_vector             : PISC_STATUS;
      tran_handle               : PISC_TR_HANDLE;
      statement_handle          : PISC_STMT_HANDLE;
      isc_arg4                  : UShort;
      isc_arg5                  : PChar;
      isc_arg6                  : UShort;
      isc_arg7                  : UShort;
      isc_arg8                  : PChar;
      isc_arg9                  : UShort;
      isc_arg10                 : PChar;
      isc_arg11                 : UShort;
      isc_arg12                 : UShort;
      isc_arg13                 : PChar): ISC_STATUS; stdcall;

type
  Tisc_dsql_execute_immediate_m = function (
      status_vector    : PISC_STATUS;
      db_handle                 : PISC_DB_HANDLE;
      tran_handle               : PISC_TR_HANDLE;
      isc_arg4                  : UShort;
      isc_arg5                  : PChar;
      isc_arg6                  : UShort;
      isc_arg7                  : UShort;
      isc_arg8                  : PChar;
      isc_arg9                  : UShort;
      isc_arg10                 : UShort;
      isc_arg11                 : PChar): ISC_STATUS; stdcall;

type
  Tisc_dsql_exec_immed3_m = function  (
      status_vector         : PISC_STATUS;
      db_handle                 : PISC_DB_HANDLE;
      tran_handle               : PISC_TR_HANDLE;
      isc_arg4                  : UShort;
      isc_arg5                  : PChar;
      isc_arg6                  : UShort;
      isc_arg7                  : UShort;
      isc_arg8                  : PChar;
      isc_arg9                  : UShort;
      isc_arg10                 : UShort;
      isc_arg11                 : PChar;
      isc_arg12                 : UShort;
      isc_arg13                 : PChar;
      isc_arg14                 : UShort;
      isc_arg15                 : UShort;
      isc_arg16                 : PChar): ISC_STATUS; stdcall;

type
  Tisc_dsql_fetch_m = function    (
      status_vector             : PISC_STATUS;
      statement_handle          : PISC_STMT_HANDLE;
      isc_arg3                  : UShort;
      isc_arg4                  : PChar;
      isc_arg5                  : UShort;
      isc_arg6                  : UShort;
      isc_arg7                  : PChar): ISC_STATUS; stdcall;

type
  Tisc_dsql_insert_m = function   (
      status_vector             : PISC_STATUS;
      statement_handle          : PISC_STMT_HANDLE;
      isc_arg3                  : UShort;
      isc_arg4                  : PChar;
      isc_arg5                  : UShort;
      isc_arg6                  : UShort;
      isc_arg7                  : PChar): ISC_STATUS; stdcall;

type
  Tisc_dsql_prepare_m = function  (
      status_vector             : PISC_STATUS;
      tran_handle               : PISC_TR_HANDLE;
      statement_handle          : PISC_STMT_HANDLE;
      isc_arg4                  : UShort;
      isc_arg5                  : PChar;
      isc_arg6                  : UShort;
      isc_arg7                  : UShort;
      isc_arg8                  : PChar;
      isc_arg9                  : UShort;
      isc_arg10                 : PChar): ISC_STATUS; stdcall;

type
  Tisc_dsql_release = function    (
      status_vector             : PISC_STATUS;
      isc_arg2                  : PChar): ISC_STATUS; stdcall;

type
  Tisc_embed_dsql_close = function(
      status_vector             : PISC_STATUS;
      isc_arg2                  : PChar): ISC_STATUS; stdcall;

type
  Tisc_embed_dsql_declare = function  (
      status_vector         : PISC_STATUS;
      isc_arg2                  : PChar;
      isc_arg3                  : PChar): ISC_STATUS; stdcall;

type
  Tisc_embed_dsql_describe = function (
      status_vector         : PISC_STATUS;
      isc_arg2                  : PChar;
      isc_arg3                  : UShort;
      isc_arg4                  : Pointer): ISC_STATUS; stdcall;

type
  Tisc_embed_dsql_describe_bind = function (
      status_vector    : PISC_STATUS;
      isc_arg2                  : PChar;
      isc_arg3                  : UShort;
      isc_arg4                  : Pointer): ISC_STATUS; stdcall;

type
  Tisc_embed_dsql_execute = function  (
      status_vector         : PISC_STATUS;
      tran_handle               : PISC_TR_HANDLE;
      isc_arg3                  : PChar;
      isc_arg4                  : UShort;
      isc_arg5                  : Pointer): ISC_STATUS; stdcall;

type
  Tisc_embed_dsql_execute2 = function (
      status_vector         : PISC_STATUS;
      tran_handle               : PISC_TR_HANDLE;
      isc_arg3                  : PChar;
      isc_arg4                  : UShort;
      isc_arg5                  : Pointer;
      isc_arg6                  : Pointer): ISC_STATUS; stdcall;

type
  Tisc_embed_dsql_execute_immed = function (
      status_vector    : PISC_STATUS;
      db_handle                 : PISC_DB_HANDLE;
      tran_handle               : PISC_TR_HANDLE;
      isc_arg4                  : UShort;
      isc_arg5                  : PChar;
      isc_arg6                  : UShort;
      isc_arg7                  : Pointer): ISC_STATUS; stdcall;
      
type
  Tisc_embed_dsql_fetch = function(
      status_vector             : PISC_STATUS;
      isc_arg2                  : PChar;
      isc_arg3                  : UShort;
      isc_arg4                  : Pointer): ISC_STATUS; stdcall;

type
  Tisc_embed_dsql_fetch2 = function  (
      status_vector         : PISC_STATUS;
      isc_arg2                  : PChar;
      isc_arg3                  : UShort;
      isc_arg4                  : Pointer;
      isc_arg5                  : UShort;
      isc_arg6                  : Long): ISC_STATUS; stdcall;

type
  Tisc_embed_dsql_open = function (
      status_vector             : PISC_STATUS;
      tran_handle               : PISC_TR_HANDLE;
      isc_arg3                  : PChar;
      isc_arg4                  : UShort;
      isc_arg5                  : Pointer): ISC_STATUS; stdcall;

type
  Tisc_embed_dsql_open2 = function (
      status_vector            : PISC_STATUS;
      tran_handle               : PISC_TR_HANDLE;
      isc_arg3                  : PChar;
      isc_arg4                  : UShort;
      isc_arg5                  : Pointer;
      isc_arg6                  : Pointer): ISC_STATUS; stdcall;

type
  Tisc_embed_dsql_insert = function (
      status_vector           : PISC_STATUS;
      isc_arg2                  : PChar;
      isc_arg3                  : UShort;
      isc_arg4                  : Pointer): ISC_STATUS; stdcall;

type
  Tisc_embed_dsql_prepare = function  (
      status_vector         : PISC_STATUS;
      db_handle                 : PISC_DB_HANDLE;
      tran_handle               : PISC_TR_HANDLE;
      isc_arg4                  : PChar;
      isc_arg5                  : UShort;
      isc_arg6                  : PChar;
      isc_arg7                  : UShort;
      isc_arg8                  : Pointer): ISC_STATUS; stdcall;

type
  Tisc_embed_dsql_release = function  (
      status_vector         : PISC_STATUS;
      isc_arg2                  : PChar): ISC_STATUS; stdcall;

  (********************************)
  (** Other Blob functions       **)
  (********************************)

type
  TBLOB_open = function           (
      blob_handle               : TISC_BLOB_HANDLE;
      isc_arg2                  : PChar;
      isc_arg3                  : int): PBSTREAM; stdcall;

type
  TBLOB_put = function            (
      isc_arg1                  : char;
      isc_arg2                  : PBSTREAM): Int; stdcall;

type
  TBLOB_close = function         (
      isc_arg1                  : PBSTREAM): Int; stdcall;

type
  TBLOB_get = function           (
      isc_arg1                  : PBSTREAM): Int; stdcall;

type
  TBLOB_display = function        (
      isc_arg1                  : PISC_QUAD;
      db_handle                 : TISC_DB_HANDLE;
      tran_handle               : TISC_TR_HANDLE;
      isc_arg4                  : PChar): Int; stdcall;

type
  TBLOB_dump = function           (
      isc_arg1                  : PISC_QUAD;
      db_handle                 : TISC_DB_HANDLE;
      tran_handle               : TISC_TR_HANDLE;
      isc_arg4                  : PChar): Int; stdcall;

type
  TBLOB_edit = function           (
      isc_arg1                  : PISC_QUAD;
      db_handle                 : TISC_DB_HANDLE;
      tran_handle               : TISC_TR_HANDLE;
      isc_arg4                  : PChar): Int; stdcall;

type
  TBLOB_load = function           (
      isc_arg1                  : PISC_QUAD;
      db_handle                 : TISC_DB_HANDLE;
      tran_handle               : TISC_TR_HANDLE;
      isc_arg4                  : PChar): Int; stdcall;

type
  TBLOB_text_dump = function      (
      isc_arg1                  : PISC_QUAD;
      db_handle                 : TISC_DB_HANDLE;
      tran_handle               : TISC_TR_HANDLE;
      isc_arg4                  : PChar): Int; stdcall;

type
  TBLOB_text_load = function      (
      isc_arg1                  : PISC_QUAD;
      db_handle                 : TISC_DB_HANDLE;
      tran_handle               : TISC_TR_HANDLE;
      isc_arg4                  : PChar): Int; stdcall;

type
  TBopen = function               (
      isc_arg1                  : PISC_QUAD;
      db_handle                 : TISC_DB_HANDLE;
      tran_handle               : TISC_TR_HANDLE;
      isc_arg4                  : PChar): Int; stdcall;

type
  TBopen2 = function              (
      isc_arg1                  : PISC_QUAD;
      db_handle                 : TISC_DB_HANDLE;
      tran_handle               : TISC_TR_HANDLE;
      isc_arg4                  : PChar;
      isc_arg5                  : UShort): PBSTREAM; stdcall;

  (********************************)
  (** Other Misc functions       **)
  (********************************)

type
  Tisc_ftof = function            (
      isc_arg1                  : PChar;
      isc_arg2                  : UShort;
      isc_arg3                  : PChar;
      isc_arg4                  : UShort): ISC_LONG; stdcall;

type
  Tisc_print_blr = function       (
      isc_arg1                  : PChar;
      isc_arg2                  : TISC_CALLBACK;
      isc_arg3                  : PVoid;
      isc_arg4                  : Short): ISC_STATUS; stdcall;

type
  Tisc_set_debug = procedure     (
      isc_arg1                  : Int); stdcall;

type
  Tisc_qtoq = procedure           (
      isc_arg1                  : PISC_QUAD;
      isc_arg2                  : PISC_QUAD); stdcall;

type
  Tisc_vtof = procedure           (
      isc_arg1                  : PChar;
      isc_arg2                  : PChar;
      isc_arg3                  : UShort); stdcall;

type
  Tisc_vtov = procedure           (
      isc_arg1                  : PChar;
      isc_arg2                  : PChar;
      isc_arg3                  : Short); stdcall;

type
  Tisc_version = function         (
      db_handle                 : PISC_DB_HANDLE;
      isc_arg2                  : TISC_CALLBACK;
      isc_arg3                  : PVoid): Int; stdcall;

type
  Tisc_reset_fpe = function      (
      isc_arg1                  : UShort): ISC_LONG; stdcall;

  (*******************************************)
  (** Service manager functions             **)
  (*******************************************)

type
  Tisc_service_attach = function  (
      status_vector             : PISC_STATUS;
      isc_arg2                  : UShort;
      isc_arg3                  : PChar;
      service_handle            : PISC_SVC_HANDLE;
      isc_arg5                  : UShort;
      isc_arg6                  : PChar): ISC_STATUS; stdcall;

type
  Tisc_service_detach = function (
      status_vector             : PISC_STATUS;
      service_handle            : PISC_SVC_HANDLE): ISC_STATUS; stdcall;

type
  Tisc_service_query = function   (
      status_vector             : PISC_STATUS;
      service_handle            : PISC_SVC_HANDLE;
      recv_handle               : PISC_SVC_HANDLE;
      isc_arg4                  : UShort;
      isc_arg5                  : PChar;
      isc_arg6                  : UShort;
      isc_arg7                  : PChar;
      isc_arg8                  : UShort;
      isc_arg9                  : PChar): ISC_STATUS; stdcall;

type
  Tisc_service_start = function  (
      status_vector             : PISC_STATUS;
      service_handle            : PISC_SVC_HANDLE;
      recv_handle               : PISC_SVC_HANDLE;
      isc_arg4                  : UShort;
      isc_arg5                  : PChar): ISC_STATUS; stdcall;

  (********************************)
  (* Client information functions *)
  (********************************)
type
  Tisc_get_client_version = procedure(
      buffer : PChar); stdcall;

type
  Tisc_get_client_major_version = function : Integer; stdcall;
  
type
  Tisc_get_client_minor_version = function : Integer; stdcall;

  (*********************************)
  (** Forms functions             **)
  (*********************************)

type
  Tisc_compile_map = function     (
      status_vector             : PISC_STATUS;
      form_handle               : PISC_FORM_HANDLE;
      request_handle            : PISC_REQ_HANDLE;
      isc_arg4                  : PShort;
      isc_arg5                  : PChar): ISC_STATUS; stdcall;

type
  Tisc_compile_menu = function    (
      status_vector             : PISC_STATUS;
      form_handle               : PISC_FORM_HANDLE;
      request_handle            : PISC_REQ_HANDLE;
      isc_arg4                  : PShort;
      isc_arg5                  : PChar): ISC_STATUS; stdcall;

type
  Tisc_compile_sub_map = function (
      status_vector             : PISC_STATUS;
      win_handle                : PISC_WIN_HANDLE;
      request_handle            : PISC_REQ_HANDLE;
      isc_arg4                  : PShort;
      isc_arg5                  : PChar): ISC_STATUS; stdcall;

type
  Tisc_create_window = function   (
      status_vector             : PISC_STATUS;
      win_handle                : PISC_WIN_HANDLE;
      isc_arg3                  : PShort;
      isc_arg4                  : PChar;
      isc_arg5                  : PShort;
      isc_arg6                  : PShort): ISC_STATUS; stdcall;

type
  Tisc_delete_window = function   (
      status_vector             : PISC_STATUS;
      win_handle                : PISC_WIN_HANDLE): ISC_STATUS; stdcall;

type
  Tisc_drive_form = function      (
      status_vector             : PISC_STATUS;
      db_handle                 : PISC_DB_HANDLE;
      tran_handle               : PISC_TR_HANDLE;
      win_handle                : PISC_WIN_HANDLE;
      request_handle            : PISC_REQ_HANDLE;
      isc_arg6                  : PUChar;
      isc_arg7                  : PUChar): ISC_STATUS; stdcall;

type
  Tisc_drive_menu = function      (
      status_vector             : PISC_STATUS;
      win_handle                : PISC_WIN_HANDLE;
      request_handle            : PISC_REQ_HANDLE;
      isc_arg4                  : PShort;
      isc_arg5                  : PChar;
      isc_arg6                  : PShort;
      isc_arg7                  : PChar;
      isc_arg8                  : PShort;
      isc_arg9                  : PShort;
      isc_arg10                 : PChar;
      isc_arg11                 : PISC_LONG): ISC_STATUS; stdcall;

type
  Tisc_form_delete = function     (
      status_vector             : PISC_STATUS;
      form_handle               : PISC_FORM_HANDLE): ISC_STATUS; stdcall;

type
  Tisc_form_fetch = function      (
      status_vector             : PISC_STATUS;
      db_handle                 : PISC_DB_HANDLE;
      tran_handle               : PISC_TR_HANDLE;
      request_handle            : PISC_REQ_HANDLE;
      isc_arg5                  : PUChar): ISC_STATUS; stdcall;

type
  Tisc_form_insert = function     (
      status_vector             : PISC_STATUS;
      db_handle                 : PISC_DB_HANDLE;
      tran_handle               : PISC_TR_HANDLE;
      request_handle            : PISC_REQ_HANDLE;
      isc_arg5                  : PUChar): ISC_STATUS; stdcall;

type
  Tisc_get_entree = function      (
      status_vector             : PISC_STATUS;
      request_handle            : PISC_REQ_HANDLE;
      isc_arg3                  : PShort;
      isc_arg4                  : PChar;
      isc_arg5                  : PISC_LONG;
      isc_arg6                  : PShort): ISC_STATUS; stdcall;

type
  Tisc_initialize_menu = function (
      status_vector             : PISC_STATUS;
      request_handle            : PISC_REQ_HANDLE): ISC_STATUS; stdcall;

type
  Tisc_menu = function            (
      status_vector             : PISC_STATUS;
      win_handle                : PISC_WIN_HANDLE;
      request_handle            : PISC_REQ_HANDLE;
      isc_arg4                  : PShort;
      isc_arg5                  : PChar): ISC_STATUS; stdcall;

type
  Tisc_load_form = function       (
      status_vector             : PISC_STATUS;
      db_handle                 : PISC_DB_HANDLE;
      tran_handle               : PISC_TR_HANDLE;
      form_handle               : PISC_FORM_HANDLE;
      isc_arg5                  : PShort;
      isc_arg6                  : PChar): ISC_STATUS; stdcall;

type
  Tisc_pop_window = function      (
      status_vector             : PISC_STATUS;
      win_handle                : PISC_WIN_HANDLE): ISC_STATUS; stdcall;

type
  Tisc_put_entree = function      (
      status_vector             : PISC_STATUS;
      request_handle            : PISC_REQ_HANDLE;
      isc_arg3                  : PShort;
      isc_arg4                  : PChar;
      isc_arg5                  : PISC_LONG): ISC_STATUS; stdcall;

type
  Tisc_reset_form = function      (
      status_vector             : PISC_STATUS;
      request_handle            : PISC_REQ_HANDLE): ISC_STATUS; stdcall;

type
  Tisc_suspend_window = function  (
      status_vector             : PISC_STATUS;
      win_handle                : PISC_WIN_HANDLE): ISC_STATUS; stdcall;

      
implementation
end.
 