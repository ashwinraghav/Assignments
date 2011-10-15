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

unit SilSiFirebirdClient;

{$INCLUDE Defines.inc}

interface

uses
  SilSeFirebirdClient;

const
  GsFirebirdClient: TGUID = '{691DB26F-C36D-419D-BB1E-B6F63103EDD5}';

type
  IFbClient = interface;
  IFbClientBlob = interface;
  IFbClientSql = interface;
  IFbClientService = interface;
  IFbClientUtils = interface;
  IFbClientSession = interface;
  IFbClientTransaction = interface;
  IFbClientSecurity = interface;
  IFbClientEvents = interface;

  IFbClient = interface
    ['{8A130170-5152-4304-91FB-CD952EBEDD3E}']
    function GetBlob: IFbClientBlob;
    function GetSql: IFbClientSql;
    function GetService: IFbClientService;
    function GetUtils: IFbClientUtils;
    function GetSession: IFbClientSession;
    function GetTransaction: IFbClientTransaction;
    function GetSecurity: IFbClientSecurity;
    function GetEvents: IFbClientEvents;
    property blob: IFbClientBlob read GetBlob;
    property sql: IFbClientSql read GetSql;
    property service: IFbClientService read GetService;
    property utils: IFbClientUtils read GetUtils;
    property session: IFbClientSession read GetSession;
    property tran: IFbClientTransaction read GetTransaction;
    property security: IFbClientSecurity read GetSecurity;
    property events: IFbClientEvents read GetEvents;
  end;

  IFbClientUtils = interface
    ['{B586E7CC-FD61-4D70-A562-4F4C21D06E5E}']
    function isc_interprete: Tisc_interprete;
    function isc_vax_integer: Tisc_vax_integer;
    function isc_decode_date: Tisc_decode_date;
    function isc_decode_sql_date: Tisc_decode_sql_date;
    function isc_decode_sql_time: Tisc_decode_sql_time;
    function isc_decode_timestamp: Tisc_decode_timestamp;
    function isc_encode_date: Tisc_encode_date;
    function isc_encode_sql_date: Tisc_encode_sql_date;
    function isc_encode_sql_time: Tisc_encode_sql_time;
    function isc_encode_timestamp: Tisc_encode_timestamp;
    function isc_free: Tisc_free;
    property interprete: Tisc_interprete read isc_interprete;
    property vax_integer: Tisc_vax_integer read isc_vax_integer;
    property decode_date: Tisc_decode_date read isc_decode_date;
    property decode_sql_date: Tisc_decode_sql_date read isc_decode_sql_date;
    property decode_sql_time: Tisc_decode_sql_time read isc_decode_sql_time;
    property decode_timestamp: Tisc_decode_timestamp read isc_decode_timestamp;
    property encode_date: Tisc_encode_date read isc_encode_date;
    property encode_sql_date: Tisc_encode_sql_date read isc_encode_sql_date;
    property encode_sql_time: Tisc_encode_sql_time read isc_encode_sql_time;
    property encode_timestamp: Tisc_encode_timestamp read isc_encode_timestamp;
    property free: Tisc_free read isc_free;
  end;

  IFbClientSession = interface
    ['{FD74FE9C-0955-48BC-BD06-68DE12895D25}']
    function isc_drop_database: Tisc_drop_database;
    function isc_detach_database: Tisc_detach_database;
    function isc_attach_database: Tisc_attach_database;
    function isc_database_info: Tisc_database_info;
    property drop: Tisc_drop_database read isc_drop_database;
    property detach: Tisc_detach_database read isc_detach_database;
    property attach: Tisc_attach_database read isc_attach_database;
    property info: Tisc_database_info read isc_database_info;
  end;

  IFbClientSql = interface
    ['{AD282B5E-45D6-4121-B3A6-215A54A155A2}']
    function isc_sqlcode: Tisc_sqlcode;
    function isc_sql_interprete: Tisc_sql_interprete;
    function isc_dsql_free_statement: Tisc_dsql_free_statement;
    function isc_dsql_execute2: Tisc_dsql_execute2;
    function isc_dsql_execute: Tisc_dsql_execute;
    function isc_dsql_set_cursor_name: Tisc_dsql_set_cursor_name;
    function isc_dsql_fetch: Tisc_dsql_fetch;
    function isc_dsql_sql_info: Tisc_dsql_sql_info;
    function isc_dsql_alloc_statement2: Tisc_dsql_alloc_statement2;
    function isc_dsql_prepare: Tisc_dsql_prepare;
    function isc_dsql_describe_bind: Tisc_dsql_describe_bind;
    function isc_dsql_describe: Tisc_dsql_describe;
    function isc_dsql_execute_immediate: Tisc_dsql_execute_immediate;
    property code: Tisc_sqlcode read isc_sqlcode;
    property interprete: Tisc_sql_interprete read isc_sql_interprete;
    property free_statement: Tisc_dsql_free_statement read isc_dsql_free_statement;
    property execute2: Tisc_dsql_execute2 read isc_dsql_execute2;
    property execute: Tisc_dsql_execute read isc_dsql_execute;
    property set_cursor_name: Tisc_dsql_set_cursor_name read isc_dsql_set_cursor_name;
    property fetch: Tisc_dsql_fetch read isc_dsql_fetch;
    property info: Tisc_dsql_sql_info read isc_dsql_sql_info;
    property alloc_statement2: Tisc_dsql_alloc_statement2 read isc_dsql_alloc_statement2;
    property prepare: Tisc_dsql_prepare read isc_dsql_prepare;
    property describe_bind: Tisc_dsql_describe_bind read isc_dsql_describe_bind;
    property describe: Tisc_dsql_describe read isc_dsql_describe;
    property execute_immediate: Tisc_dsql_execute_immediate read isc_dsql_execute_immediate;
  end;

  IFbClientTransaction = interface
    ['{9AA75315-1546-42E5-8D72-06323581076C}']
    function isc_start_multiple: Tisc_start_multiple;
    function isc_commit_transaction: Tisc_commit_transaction;
    function isc_commit_retaining: Tisc_commit_retaining;
    function isc_rollback_transaction: Tisc_rollback_transaction;
    function isc_rollback_retaining: Tisc_rollback_retaining;
    function isc_prepare_transaction : Tisc_prepare_transaction;
    function isc_prepare_transaction2 : Tisc_prepare_transaction2;
    property start_multiple: Tisc_start_multiple read isc_start_multiple;
    property commit: Tisc_commit_transaction read isc_commit_transaction;
    property commit_retaining: Tisc_commit_retaining read isc_commit_retaining;
    property rollback: Tisc_rollback_transaction read isc_rollback_transaction;
    property rollback_retaining: Tisc_rollback_retaining read isc_rollback_retaining;
    property prepare: Tisc_prepare_transaction read isc_prepare_transaction;
    property prepare2: Tisc_prepare_transaction2 read isc_prepare_transaction2;
  end;

  IFbClientService = interface
    ['{F05DE80E-1698-47E1-B375-2A947E5873F1}']
    function isc_service_attach: Tisc_service_attach;
    function isc_service_detach: Tisc_service_detach;
    function isc_service_query: Tisc_service_query;
    function isc_service_start: Tisc_service_start;
    property attach: Tisc_service_attach read isc_service_attach;
    property detach: Tisc_service_detach read isc_service_detach;
    property query: Tisc_service_query read isc_service_query;
    property start: Tisc_service_start read isc_service_start;
  end;

  IFbClientBlob = interface
    ['{A7204021-46DC-4236-97E6-C4BB3E0A42B9}']
    function BLOB_get: TBLOB_get;
    function BLOB_put: TBLOB_put;
    function isc_blob_info: Tisc_blob_info;
    function isc_open_blob2: Tisc_open_blob2;
    function isc_close_blob: Tisc_close_blob;
    function isc_get_segment: Tisc_get_segment;
    function isc_put_segment: Tisc_put_segment;
    function isc_create_blob2: Tisc_create_blob2;
    property get: TBLOB_get read BLOB_get;
    property put: TBLOB_put read BLOB_put;
    property info: Tisc_blob_info read isc_blob_info;
    property open: Tisc_open_blob2 read isc_open_blob2;
    property close: Tisc_close_blob read isc_close_blob;
    property get_segment: Tisc_get_segment read isc_get_segment;
    property put_segment: Tisc_put_segment read isc_put_segment;
    property create: Tisc_create_blob2 read isc_create_blob2;
  end;

  IFbClientSecurity = interface
    ['{D24BB05E-9DAB-43A4-9AA9-0023E6C7A2AC}']
    function isc_add_user   : Tisc_add_user;
    function isc_delete_user: Tisc_delete_user;
    function isc_modify_user: Tisc_modify_user;
    property add_user   : Tisc_add_user read isc_add_user;
    property delete_user: Tisc_delete_user read isc_delete_user;
    property modify_user: Tisc_modify_user read isc_modify_user;
  end;

  IFbClientEvents = interface
    ['{6BA70D46-8B82-4D26-BE0D-10112B1EE97F}']
    function isc_cancel_events: Tisc_cancel_events;
    function isc_que_events: Tisc_que_events;
    function isc_event_counts: Tisc_event_counts;
    function isc_event_block: Tisc_event_block;
    property cancel: Tisc_cancel_events read isc_cancel_events;
    property queue: Tisc_que_events read isc_que_events;
    property counts: Tisc_event_counts read isc_event_counts;
    property block: Tisc_event_block read isc_event_block;
  end;

implementation
end.
