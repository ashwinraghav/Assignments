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

unit SilSmFirebirdClient;

{$INCLUDE Defines.inc}

interface

uses
  Sil,
  SilScFirebirdClient,
  SilSeFirebirdClient,
  SilSiFirebirdClient;

type
  TSilFirebirdClient = class(
    TSilObject,
    IFbClient,
    IFbClientBlob,
    IFbClientSql,
    IFbClientService,
    IFbClientUtils,
    IFbClientSession,
    IFbClientTransaction,
    IFbClientSecurity,
    IFbClientEvents )
  private
    FDll: ISharedLibrary; 
  protected // IFbClient
    function GetUtils: IFbClientUtils;
    function GetSession: IFbClientSession;
    function GetSql: IFbClientSql;
    function GetTransaction: IFbClientTransaction;
    function GetBlob: IFbClientBlob;
    function GetService: IFbClientService;
    function GetSecurity: IFbClientSecurity;
    function GetEvents: IFbClientEvents;
  protected // IFbClientUtils
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
  protected // IFbClientSession
    function isc_drop_database: Tisc_drop_database;
    function isc_detach_database: Tisc_detach_database;
    function isc_attach_database: Tisc_attach_database;
    function isc_database_info: Tisc_database_info;
  protected // IFbClientSql
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
  protected // IFbClientTransaction
    function isc_start_multiple: Tisc_start_multiple;
    function isc_commit_transaction: Tisc_commit_transaction;
    function isc_commit_retaining: Tisc_commit_retaining;
    function isc_rollback_transaction: Tisc_rollback_transaction;
    function isc_rollback_retaining: Tisc_rollback_retaining;
    function isc_prepare_transaction : Tisc_prepare_transaction;
    function isc_prepare_transaction2 : Tisc_prepare_transaction2;
  protected // IFbClientService
    function isc_service_attach: Tisc_service_attach;
    function isc_service_detach: Tisc_service_detach;
    function isc_service_query: Tisc_service_query;
    function isc_service_start: Tisc_service_start;
  protected // IFbClientBlob
    function BLOB_get: TBLOB_get;
    function BLOB_put: TBLOB_put;
    function isc_blob_info: Tisc_blob_info;
    function isc_open_blob2: Tisc_open_blob2;
    function isc_close_blob: Tisc_close_blob;
    function isc_get_segment: Tisc_get_segment;
    function isc_put_segment: Tisc_put_segment;
    function isc_create_blob2: Tisc_create_blob2;
  protected // IFbClientSecurity
    function isc_add_user   : Tisc_add_user;
    function isc_delete_user: Tisc_delete_user;
    function isc_modify_user: Tisc_modify_user;
  protected // IFbClientEvents
    function isc_cancel_events: Tisc_cancel_events;
    function isc_que_events: Tisc_que_events;
    function isc_event_counts: Tisc_event_counts;
    function isc_event_block: Tisc_event_block;
  public
    constructor Create;
    destructor Destroy; override; 
  end;

implementation

{ TSilFirebirdClient }

constructor TSilFirebirdClient.Create;
begin
  inherited Create;
  FDll := Sil.OS.SharedLibrary.Load(IBASE_DLL);
end;

destructor TSilFirebirdClient.Destroy;
begin
  FDll := nil;
  inherited;
end;

function TSilFirebirdClient.GetUtils: IFbClientUtils;
begin
  Result := Self;
end;

function TSilFirebirdClient.GetSession: IFbClientSession;
begin
  Result := Self;
end;

function TSilFirebirdClient.GetSql: IFbClientSql;
begin
  Result := Self;
end;

function TSilFirebirdClient.GetTransaction: IFbClientTransaction;
begin
  Result := Self;
end;

function TSilFirebirdClient.GetBlob: IFbClientBlob;
begin
  Result := Self;
end;

function TSilFirebirdClient.GetService: IFbClientService;
begin
  Result := Self;
end;

function TSilFirebirdClient.GetSecurity: IFbClientSecurity;
begin
  Result := Self;
end;

function TSilFirebirdClient.GetEvents: IFbClientEvents;
begin
  Result := Self;
end;

function TSilFirebirdClient.isc_interprete: Tisc_interprete;
begin
  FDll.Bind('isc_interprete', 0, Result, True);
end;

function TSilFirebirdClient.isc_vax_integer: Tisc_vax_integer;
begin
  FDll.Bind('isc_vax_integer', 1, Result, True);
end;

function TSilFirebirdClient.isc_decode_date: Tisc_decode_date;
begin
  FDll.Bind('isc_decode_date', 2, Result, True);
end;

function TSilFirebirdClient.isc_decode_sql_date: Tisc_decode_sql_date;
begin
  FDll.Bind('isc_decode_sql_date', 3, Result, True);
end;

function TSilFirebirdClient.isc_decode_sql_time: Tisc_decode_sql_time;
begin
  FDll.Bind('isc_decode_sql_time', 4, Result, True);
end;

function TSilFirebirdClient.isc_decode_timestamp: Tisc_decode_timestamp;
begin
  FDll.Bind('isc_decode_timestamp', 5, Result, True);
end;

function TSilFirebirdClient.isc_encode_date: Tisc_encode_date;
begin
  FDll.Bind('isc_encode_date', 6, Result, True);
end;

function TSilFirebirdClient.isc_encode_sql_date: Tisc_encode_sql_date;
begin
  FDll.Bind('isc_encode_sql_date', 7, Result, True);
end;

function TSilFirebirdClient.isc_encode_sql_time: Tisc_encode_sql_time;
begin
  FDll.Bind('isc_encode_sql_time', 8, Result, True);
end;

function TSilFirebirdClient.isc_encode_timestamp: Tisc_encode_timestamp;
begin
  FDll.Bind('isc_encode_timestamp', 9, Result, True);
end;

function TSilFirebirdClient.isc_free: Tisc_free;
begin
  FDll.Bind('isc_free', 10, Result, True);
end;

function TSilFirebirdClient.isc_drop_database: Tisc_drop_database;
begin
  FDll.Bind('isc_drop_database', 11, Result, True);
end;

function TSilFirebirdClient.isc_detach_database: Tisc_detach_database;
begin
  FDll.Bind('isc_detach_database', 12, Result, True);
end;

function TSilFirebirdClient.isc_attach_database: Tisc_attach_database;
begin
  FDll.Bind('isc_attach_database', 13, Result, True);
end;

function TSilFirebirdClient.isc_database_info: Tisc_database_info;
begin
  FDll.Bind('isc_database_info', 14, Result, True);
end;

function TSilFirebirdClient.isc_sqlcode: Tisc_sqlcode;
begin
  FDll.Bind('isc_sqlcode', 15, Result, True);
end;

function TSilFirebirdClient.isc_sql_interprete: Tisc_sql_interprete;
begin
  FDll.Bind('isc_sql_interprete', 16, Result, True);
end;

function TSilFirebirdClient.isc_dsql_free_statement: Tisc_dsql_free_statement;
begin
  FDll.Bind('isc_dsql_free_statement', 17, Result, True);
end;

function TSilFirebirdClient.isc_dsql_execute2: Tisc_dsql_execute2;
begin
  FDll.Bind('isc_dsql_execute2', 18, Result, True);
end;

function TSilFirebirdClient.isc_dsql_execute: Tisc_dsql_execute;
begin
  FDll.Bind('isc_dsql_execute', 19, Result, True);
end;

function TSilFirebirdClient.isc_dsql_set_cursor_name: Tisc_dsql_set_cursor_name;
begin
  FDll.Bind('isc_dsql_set_cursor_name', 20, Result, True);
end;

function TSilFirebirdClient.isc_dsql_fetch: Tisc_dsql_fetch;
begin
  FDll.Bind('isc_dsql_fetch', 21, Result, True);
end;

function TSilFirebirdClient.isc_dsql_sql_info: Tisc_dsql_sql_info;
begin
  FDll.Bind('isc_dsql_sql_info', 22, Result, True);
end;

function TSilFirebirdClient.isc_dsql_alloc_statement2: Tisc_dsql_alloc_statement2;
begin
  FDll.Bind('isc_dsql_alloc_statement2', 23, Result, True);
end;

function TSilFirebirdClient.isc_dsql_prepare: Tisc_dsql_prepare;
begin
  FDll.Bind('isc_dsql_prepare', 24, Result, True);
end;

function TSilFirebirdClient.isc_dsql_describe_bind: Tisc_dsql_describe_bind;
begin
  FDll.Bind('isc_dsql_describe_bind', 25, Result, True);
end;

function TSilFirebirdClient.isc_dsql_describe: Tisc_dsql_describe;
begin
  FDll.Bind('isc_dsql_describe', 26, Result, True);
end;

function TSilFirebirdClient.isc_dsql_execute_immediate: Tisc_dsql_execute_immediate;
begin
  FDll.Bind('isc_dsql_execute_immediate', 27, Result, True);
end;

function TSilFirebirdClient.isc_start_multiple: Tisc_start_multiple;
begin
  FDll.Bind('isc_start_multiple', 28, Result, True);
end;

function TSilFirebirdClient.isc_commit_transaction: Tisc_commit_transaction;
begin
  FDll.Bind('isc_commit_transaction', 29, Result, True);
end;

function TSilFirebirdClient.isc_commit_retaining: Tisc_commit_retaining;
begin
  FDll.Bind('isc_commit_retaining', 30, Result, True);
end;

function TSilFirebirdClient.isc_rollback_transaction: Tisc_rollback_transaction;
begin
  FDll.Bind('isc_rollback_transaction', 31, Result, True);
end;

function TSilFirebirdClient.isc_rollback_retaining: Tisc_rollback_retaining;
begin
  FDll.Bind('isc_rollback_retaining', 32, Result, True);
end;

function TSilFirebirdClient.isc_prepare_transaction: Tisc_prepare_transaction;
begin
  FDll.Bind('isc_prepare_transaction', 33, Result, True);
end;

function TSilFirebirdClient.isc_prepare_transaction2: Tisc_prepare_transaction2;
begin
  FDll.Bind('isc_prepare_transaction2', 34, Result, True);
end;

function TSilFirebirdClient.isc_service_attach: Tisc_service_attach;
begin
  FDll.Bind('isc_service_attach', 35, Result, True);
end;

function TSilFirebirdClient.isc_service_detach: Tisc_service_detach;
begin
  FDll.Bind('isc_service_detach', 36, Result, True);
end;

function TSilFirebirdClient.isc_service_query: Tisc_service_query;
begin
  FDll.Bind('isc_service_query', 37, Result, True);
end;

function TSilFirebirdClient.isc_service_start: Tisc_service_start;
begin
  FDll.Bind('isc_service_start', 38, Result, True);
end;

function TSilFirebirdClient.BLOB_get: TBLOB_get;
begin
  FDll.Bind('BLOB_get', 39, Result, True);
end;

function TSilFirebirdClient.BLOB_put: TBLOB_put;
begin
  FDll.Bind('BLOB_put', 40, Result, True);
end;

function TSilFirebirdClient.isc_blob_info: Tisc_blob_info;
begin
  FDll.Bind('isc_blob_info', 41, Result, True);
end;

function TSilFirebirdClient.isc_open_blob2: Tisc_open_blob2;
begin
  FDll.Bind('isc_open_blob2', 42, Result, True);
end;

function TSilFirebirdClient.isc_close_blob: Tisc_close_blob;
begin
  FDll.Bind('isc_close_blob', 43, Result, True);
end;

function TSilFirebirdClient.isc_get_segment: Tisc_get_segment;
begin
  FDll.Bind('isc_get_segment', 44, Result, True);
end;

function TSilFirebirdClient.isc_put_segment: Tisc_put_segment;
begin
  FDll.Bind('isc_put_segment', 45, Result, True);
end;

function TSilFirebirdClient.isc_create_blob2: Tisc_create_blob2;
begin
  FDll.Bind('isc_create_blob2', 46, Result, True);
end;

function TSilFirebirdClient.isc_add_user: Tisc_add_user;
begin
  FDll.Bind('isc_add_user', 47, Result, True);
end;

function TSilFirebirdClient.isc_delete_user: Tisc_delete_user;
begin
  FDll.Bind('isc_delete_user', 48, Result, True);
end;

function TSilFirebirdClient.isc_modify_user: Tisc_modify_user;
begin
  FDll.Bind('isc_modify_user', 49, Result, True);
end;

function TSilFirebirdClient.isc_cancel_events: Tisc_cancel_events;
begin
  FDll.Bind('isc_cancel_events', 50, Result, True);
end;

function TSilFirebirdClient.isc_que_events: Tisc_que_events;
begin
  FDll.Bind('isc_que_events', 51, Result, True);
end;

function TSilFirebirdClient.isc_event_counts: Tisc_event_counts;
begin
  FDll.Bind('isc_event_counts', 52, Result, True);
end;

function TSilFirebirdClient.isc_event_block: Tisc_event_block;
begin
  FDll.Bind('isc_event_block', 53, Result, True);
end;

end.
