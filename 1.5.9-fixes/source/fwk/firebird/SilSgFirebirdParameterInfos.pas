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

unit SilSgFirebirdParameterInfos;

{$include Defines.inc}

interface

uses
  SilScFirebirdClient,
  SilSiFirebird,
  SilShFirebird,
  SilSdFirebirdParameterCaptions,
  SilSgFirebirdParameterNames,
  SilSgFirebirdParameterValues,
  SilScFirebirdParameterTypes;

const
  CFbParameterBase                    = 5500;

const
  GInfo_dpb_cdd_pathname              : RFbParameterInfo = (Code: isc_dpb_cdd_pathname              ; Name: @GName_dpb_cdd_pathname               ;  Caption: @GCaption_dpb_cdd_pathname              ; VDefault: @GValue_dpb_cdd_pathname                ;  VType: CType_dpb_cdd_pathname              ; );
  GInfo_dpb_allocation                : RFbParameterInfo = (Code: isc_dpb_allocation                ; Name: @GName_dpb_allocation                 ;  Caption: @GCaption_dpb_allocation                ; VDefault: @GValue_dpb_allocation                  ;  VType: CType_dpb_allocation                ; );
  GInfo_dpb_journal                   : RFbParameterInfo = (Code: isc_dpb_journal                   ; Name: @GName_dpb_journal                    ;  Caption: @GCaption_dpb_journal                   ; VDefault: @GValue_dpb_journal                     ;  VType: CType_dpb_journal                   ; );
  GInfo_dpb_page_size                 : RFbParameterInfo = (Code: isc_dpb_page_size                 ; Name: @GName_dpb_page_size                  ;  Caption: @GCaption_dpb_page_size                 ; VDefault: @GValue_dpb_page_size                   ;  VType: CType_dpb_page_size                 ; Flags: [fbpfCreate]  );
  GInfo_dpb_num_buffers               : RFbParameterInfo = (Code: isc_dpb_num_buffers               ; Name: @GName_dpb_num_buffers                ;  Caption: @GCaption_dpb_num_buffers               ; VDefault: @GValue_dpb_num_buffers                 ;  VType: CType_dpb_num_buffers               ; Flags: [fbpfConnect] );
  GInfo_dpb_buffer_length             : RFbParameterInfo = (Code: isc_dpb_buffer_length             ; Name: @GName_dpb_buffer_length              ;  Caption: @GCaption_dpb_buffer_length             ; VDefault: @GValue_dpb_buffer_length               ;  VType: CType_dpb_buffer_length             ; );
  GInfo_dpb_debug                     : RFbParameterInfo = (Code: isc_dpb_debug                     ; Name: @GName_dpb_debug                      ;  Caption: @GCaption_dpb_debug                     ; VDefault: @GValue_dpb_debug                       ;  VType: CType_dpb_debug                     ; );
  GInfo_dpb_garbage_collect           : RFbParameterInfo = (Code: isc_dpb_garbage_collect           ; Name: @GName_dpb_garbage_collect            ;  Caption: @GCaption_dpb_garbage_collect           ; VDefault: @GValue_dpb_garbage_collect             ;  VType: CType_dpb_garbage_collect           ; );
  GInfo_dpb_verify                    : RFbParameterInfo = (Code: isc_dpb_verify                    ; Name: @GName_dpb_verify                     ;  Caption: @GCaption_dpb_verify                    ; VDefault: @GValue_dpb_verify                      ;  VType: CType_dpb_verify                    ; );
  GInfo_dpb_sweep                     : RFbParameterInfo = (Code: isc_dpb_sweep                     ; Name: @GName_dpb_sweep                      ;  Caption: @GCaption_dpb_sweep                     ; VDefault: @GValue_dpb_sweep                       ;  VType: CType_dpb_sweep                     ; );
  GInfo_dpb_enable_journal            : RFbParameterInfo = (Code: isc_dpb_enable_journal            ; Name: @GName_dpb_enable_journal             ;  Caption: @GCaption_dpb_enable_journal            ; VDefault: @GValue_dpb_enable_journal              ;  VType: CType_dpb_enable_journal            ; );
  GInfo_dpb_disable_journal           : RFbParameterInfo = (Code: isc_dpb_disable_journal           ; Name: @GName_dpb_disable_journal            ;  Caption: @GCaption_dpb_disable_journal           ; VDefault: @GValue_dpb_disable_journal             ;  VType: CType_dpb_disable_journal           ; );
  GInfo_dpb_dbkey_scope               : RFbParameterInfo = (Code: isc_dpb_dbkey_scope               ; Name: @GName_dpb_dbkey_scope                ;  Caption: @GCaption_dpb_dbkey_scope               ; VDefault: @GValue_dpb_dbkey_scope                 ;  VType: CType_dpb_dbkey_scope               ; Flags: [fbpfConnect] );
  GInfo_dpb_number_of_users           : RFbParameterInfo = (Code: isc_dpb_number_of_users           ; Name: @GName_dpb_number_of_users            ;  Caption: @GCaption_dpb_number_of_users           ; VDefault: @GValue_dpb_number_of_users             ;  VType: CType_dpb_number_of_users           ; );
  GInfo_dpb_trace                     : RFbParameterInfo = (Code: isc_dpb_trace                     ; Name: @GName_dpb_trace                      ;  Caption: @GCaption_dpb_trace                     ; VDefault: @GValue_dpb_trace                       ;  VType: CType_dpb_trace                     ; );
  GInfo_dpb_no_garbage_collect        : RFbParameterInfo = (Code: isc_dpb_no_garbage_collect        ; Name: @GName_dpb_no_garbage_collect         ;  Caption: @GCaption_dpb_no_garbage_collect        ; VDefault: @GValue_dpb_no_garbage_collect          ;  VType: CType_dpb_no_garbage_collect        ; );
  GInfo_dpb_damaged                   : RFbParameterInfo = (Code: isc_dpb_damaged                   ; Name: @GName_dpb_damaged                    ;  Caption: @GCaption_dpb_damaged                   ; VDefault: @GValue_dpb_damaged                     ;  VType: CType_dpb_damaged                   ; );
  GInfo_dpb_license                   : RFbParameterInfo = (Code: isc_dpb_license                   ; Name: @GName_dpb_license                    ;  Caption: @GCaption_dpb_license                   ; VDefault: @GValue_dpb_license                     ;  VType: CType_dpb_license                   ; );
  GInfo_dpb_sys_user_name             : RFbParameterInfo = (Code: isc_dpb_sys_user_name             ; Name: @GName_dpb_sys_user_name              ;  Caption: @GCaption_dpb_sys_user_name             ; VDefault: @GValue_dpb_sys_user_name               ;  VType: CType_dpb_sys_user_name             ; Flags: [fbpfConnect] );
  GInfo_dpb_encrypt_key               : RFbParameterInfo = (Code: isc_dpb_encrypt_key               ; Name: @GName_dpb_encrypt_key                ;  Caption: @GCaption_dpb_encrypt_key               ; VDefault: @GValue_dpb_encrypt_key                 ;  VType: CType_dpb_encrypt_key               ; Flags: [fbpfConnect] );
  GInfo_dpb_activate_shadow           : RFbParameterInfo = (Code: isc_dpb_activate_shadow           ; Name: @GName_dpb_activate_shadow            ;  Caption: @GCaption_dpb_activate_shadow           ; VDefault: @GValue_dpb_activate_shadow             ;  VType: CType_dpb_activate_shadow           ; Flags: [fbpfConnect] );
  GInfo_dpb_sweep_interval            : RFbParameterInfo = (Code: isc_dpb_sweep_interval            ; Name: @GName_dpb_sweep_interval             ;  Caption: @GCaption_dpb_sweep_interval            ; VDefault: @GValue_dpb_sweep_interval              ;  VType: CType_dpb_sweep_interval            ; Flags: [fbpfConnect] );
  GInfo_dpb_delete_shadow             : RFbParameterInfo = (Code: isc_dpb_delete_shadow             ; Name: @GName_dpb_delete_shadow              ;  Caption: @GCaption_dpb_delete_shadow             ; VDefault: @GValue_dpb_delete_shadow               ;  VType: CType_dpb_delete_shadow             ; Flags: [fbpfConnect] );
  GInfo_dpb_force_write               : RFbParameterInfo = (Code: isc_dpb_force_write               ; Name: @GName_dpb_force_write                ;  Caption: @GCaption_dpb_force_write               ; VDefault: @GValue_dpb_force_write                 ;  VType: CType_dpb_force_write               ; Flags: [fbpfConnect] );
  GInfo_dpb_begin_log                 : RFbParameterInfo = (Code: isc_dpb_begin_log                 ; Name: @GName_dpb_begin_log                  ;  Caption: @GCaption_dpb_begin_log                 ; VDefault: @GValue_dpb_begin_log                   ;  VType: CType_dpb_begin_log                 ; Flags: [fbpfConnect] );
  GInfo_dpb_quit_log                  : RFbParameterInfo = (Code: isc_dpb_quit_log                  ; Name: @GName_dpb_quit_log                   ;  Caption: @GCaption_dpb_quit_log                  ; VDefault: @GValue_dpb_quit_log                    ;  VType: CType_dpb_quit_log                  ; Flags: [fbpfConnect] );
  GInfo_dpb_no_reserve                : RFbParameterInfo = (Code: isc_dpb_no_reserve                ; Name: @GName_dpb_no_reserve                 ;  Caption: @GCaption_dpb_no_reserve                ; VDefault: @GValue_dpb_no_reserve                  ;  VType: CType_dpb_no_reserve                ; Flags: [fbpfCreate]  );
  GInfo_dpb_user_name                 : RFbParameterInfo = (Code: isc_dpb_user_name                 ; Name: @GName_dpb_user_name                  ;  Caption: @GCaption_dpb_user_name                 ; VDefault: @GValue_dpb_user_name                   ;  VType: CType_dpb_user_name                 ; Flags: [fbpfConnect, fbpfRequired] );
  GInfo_dpb_password                  : RFbParameterInfo = (Code: isc_dpb_password                  ; Name: @GName_dpb_password                   ;  Caption: @GCaption_dpb_password                  ; VDefault: @GValue_dpb_password                    ;  VType: CType_dpb_password                  ; Flags: [fbpfConnect, fbpfRequired] );
  GInfo_dpb_password_enc              : RFbParameterInfo = (Code: isc_dpb_password_enc              ; Name: @GName_dpb_password_enc               ;  Caption: @GCaption_dpb_password_enc              ; VDefault: @GValue_dpb_password_enc                ;  VType: CType_dpb_password_enc              ; Flags: [fbpfConnect] );
  GInfo_dpb_sys_user_name_enc         : RFbParameterInfo = (Code: isc_dpb_sys_user_name_enc         ; Name: @GName_dpb_sys_user_name_enc          ;  Caption: @GCaption_dpb_sys_user_name_enc         ; VDefault: @GValue_dpb_sys_user_name_enc           ;  VType: CType_dpb_sys_user_name_enc         ; Flags: [fbpfConnect] );
  GInfo_dpb_interp                    : RFbParameterInfo = (Code: isc_dpb_interp                    ; Name: @GName_dpb_interp                     ;  Caption: @GCaption_dpb_interp                    ; VDefault: @GValue_dpb_interp                      ;  VType: CType_dpb_interp                    ; );
  GInfo_dpb_online_dump               : RFbParameterInfo = (Code: isc_dpb_online_dump               ; Name: @GName_dpb_online_dump                ;  Caption: @GCaption_dpb_online_dump               ; VDefault: @GValue_dpb_online_dump                 ;  VType: CType_dpb_online_dump               ; );
  GInfo_dpb_old_file_size             : RFbParameterInfo = (Code: isc_dpb_old_file_size             ; Name: @GName_dpb_old_file_size              ;  Caption: @GCaption_dpb_old_file_size             ; VDefault: @GValue_dpb_old_file_size               ;  VType: CType_dpb_old_file_size             ; );
  GInfo_dpb_old_num_files             : RFbParameterInfo = (Code: isc_dpb_old_num_files             ; Name: @GName_dpb_old_num_files              ;  Caption: @GCaption_dpb_old_num_files             ; VDefault: @GValue_dpb_old_num_files               ;  VType: CType_dpb_old_num_files             ; );
  GInfo_dpb_old_file                  : RFbParameterInfo = (Code: isc_dpb_old_file                  ; Name: @GName_dpb_old_file                   ;  Caption: @GCaption_dpb_old_file                  ; VDefault: @GValue_dpb_old_file                    ;  VType: CType_dpb_old_file                  ; );
  GInfo_dpb_old_start_page            : RFbParameterInfo = (Code: isc_dpb_old_start_page            ; Name: @GName_dpb_old_start_page             ;  Caption: @GCaption_dpb_old_start_page            ; VDefault: @GValue_dpb_old_start_page              ;  VType: CType_dpb_old_start_page            ; );
  GInfo_dpb_old_start_seqno           : RFbParameterInfo = (Code: isc_dpb_old_start_seqno           ; Name: @GName_dpb_old_start_seqno            ;  Caption: @GCaption_dpb_old_start_seqno           ; VDefault: @GValue_dpb_old_start_seqno             ;  VType: CType_dpb_old_start_seqno           ; );
  GInfo_dpb_old_start_file            : RFbParameterInfo = (Code: isc_dpb_old_start_file            ; Name: @GName_dpb_old_start_file             ;  Caption: @GCaption_dpb_old_start_file            ; VDefault: @GValue_dpb_old_start_file              ;  VType: CType_dpb_old_start_file            ; );
  GInfo_dpb_drop_walfile              : RFbParameterInfo = (Code: isc_dpb_drop_walfile              ; Name: @GName_dpb_drop_walfile               ;  Caption: @GCaption_dpb_drop_walfile              ; VDefault: @GValue_dpb_drop_walfile                ;  VType: CType_dpb_drop_walfile              ; );
  GInfo_dpb_old_dump_id               : RFbParameterInfo = (Code: isc_dpb_old_dump_id               ; Name: @GName_dpb_old_dump_id                ;  Caption: @GCaption_dpb_old_dump_id               ; VDefault: @GValue_dpb_old_dump_id                 ;  VType: CType_dpb_old_dump_id               ; );
  GInfo_dpb_wal_backup_dir            : RFbParameterInfo = (Code: isc_dpb_wal_backup_dir            ; Name: @GName_dpb_wal_backup_dir             ;  Caption: @GCaption_dpb_wal_backup_dir            ; VDefault: @GValue_dpb_wal_backup_dir              ;  VType: CType_dpb_wal_backup_dir            ; );
  GInfo_dpb_wal_chkptlen              : RFbParameterInfo = (Code: isc_dpb_wal_chkptlen              ; Name: @GName_dpb_wal_chkptlen               ;  Caption: @GCaption_dpb_wal_chkptlen              ; VDefault: @GValue_dpb_wal_chkptlen                ;  VType: CType_dpb_wal_chkptlen              ; );
  GInfo_dpb_wal_numbufs               : RFbParameterInfo = (Code: isc_dpb_wal_numbufs               ; Name: @GName_dpb_wal_numbufs                ;  Caption: @GCaption_dpb_wal_numbufs               ; VDefault: @GValue_dpb_wal_numbufs                 ;  VType: CType_dpb_wal_numbufs               ; );
  GInfo_dpb_wal_bufsize               : RFbParameterInfo = (Code: isc_dpb_wal_bufsize               ; Name: @GName_dpb_wal_bufsize                ;  Caption: @GCaption_dpb_wal_bufsize               ; VDefault: @GValue_dpb_wal_bufsize                 ;  VType: CType_dpb_wal_bufsize               ; );
  GInfo_dpb_wal_grp_cmt_wait          : RFbParameterInfo = (Code: isc_dpb_wal_grp_cmt_wait          ; Name: @GName_dpb_wal_grp_cmt_wait           ;  Caption: @GCaption_dpb_wal_grp_cmt_wait          ; VDefault: @GValue_dpb_wal_grp_cmt_wait            ;  VType: CType_dpb_wal_grp_cmt_wait          ; );
  GInfo_dpb_lc_messages               : RFbParameterInfo = (Code: isc_dpb_lc_messages               ; Name: @GName_dpb_lc_messages                ;  Caption: @GCaption_dpb_lc_messages               ; VDefault: @GValue_dpb_lc_messages                 ;  VType: CType_dpb_lc_messages               ; Flags: [fbpfConnect] );
  GInfo_dpb_lc_ctype                  : RFbParameterInfo = (Code: isc_dpb_lc_ctype                  ; Name: @GName_dpb_lc_ctype                   ;  Caption: @GCaption_dpb_lc_ctype                  ; VDefault: @GValue_dpb_lc_ctype                    ;  VType: CType_dpb_lc_ctype                  ; Flags: [fbpfConnect] );
  GInfo_dpb_cache_manager             : RFbParameterInfo = (Code: isc_dpb_cache_manager             ; Name: @GName_dpb_cache_manager              ;  Caption: @GCaption_dpb_cache_manager             ; VDefault: @GValue_dpb_cache_manager               ;  VType: CType_dpb_cache_manager             ; );
  GInfo_dpb_shutdown                  : RFbParameterInfo = (Code: isc_dpb_shutdown                  ; Name: @GName_dpb_shutdown                   ;  Caption: @GCaption_dpb_shutdown                  ; VDefault: @GValue_dpb_shutdown                    ;  VType: CType_dpb_shutdown                  ; );
  GInfo_dpb_online                    : RFbParameterInfo = (Code: isc_dpb_online                    ; Name: @GName_dpb_online                     ;  Caption: @GCaption_dpb_online                    ; VDefault: @GValue_dpb_online                      ;  VType: CType_dpb_online                    ; );
  GInfo_dpb_shutdown_delay            : RFbParameterInfo = (Code: isc_dpb_shutdown_delay            ; Name: @GName_dpb_shutdown_delay             ;  Caption: @GCaption_dpb_shutdown_delay            ; VDefault: @GValue_dpb_shutdown_delay              ;  VType: CType_dpb_shutdown_delay            ; );
  GInfo_dpb_reserved                  : RFbParameterInfo = (Code: isc_dpb_reserved                  ; Name: @GName_dpb_reserved                   ;  Caption: @GCaption_dpb_reserved                  ; VDefault: @GValue_dpb_reserved                    ;  VType: CType_dpb_reserved                  ; );
  GInfo_dpb_overwrite                 : RFbParameterInfo = (Code: isc_dpb_overwrite                 ; Name: @GName_dpb_overwrite                  ;  Caption: @GCaption_dpb_overwrite                 ; VDefault: @GValue_dpb_overwrite                   ;  VType: CType_dpb_overwrite                 ; );
  GInfo_dpb_sec_attach                : RFbParameterInfo = (Code: isc_dpb_sec_attach                ; Name: @GName_dpb_sec_attach                 ;  Caption: @GCaption_dpb_sec_attach                ; VDefault: @GValue_dpb_sec_attach                  ;  VType: CType_dpb_sec_attach                ; );
  GInfo_dpb_disable_wal               : RFbParameterInfo = (Code: isc_dpb_disable_wal               ; Name: @GName_dpb_disable_wal                ;  Caption: @GCaption_dpb_disable_wal               ; VDefault: @GValue_dpb_disable_wal                 ;  VType: CType_dpb_disable_wal               ; );
  GInfo_dpb_connect_timeout           : RFbParameterInfo = (Code: isc_dpb_connect_timeout           ; Name: @GName_dpb_connect_timeout            ;  Caption: @GCaption_dpb_connect_timeout           ; VDefault: @GValue_dpb_connect_timeout             ;  VType: CType_dpb_connect_timeout           ; Flags: [fbpfConnect, fbpfRequired]          );
  GInfo_dpb_dummy_packet_interval     : RFbParameterInfo = (Code: isc_dpb_dummy_packet_interval     ; Name: @GName_dpb_dummy_packet_interval      ;  Caption: @GCaption_dpb_dummy_packet_interval     ; VDefault: @GValue_dpb_dummy_packet_interval       ;  VType: CType_dpb_dummy_packet_interval     ; );
  GInfo_dpb_gbak_attach               : RFbParameterInfo = (Code: isc_dpb_gbak_attach               ; Name: @GName_dpb_gbak_attach                ;  Caption: @GCaption_dpb_gbak_attach               ; VDefault: @GValue_dpb_gbak_attach                 ;  VType: CType_dpb_gbak_attach               ; );
  GInfo_dpb_sql_role_name             : RFbParameterInfo = (Code: isc_dpb_sql_role_name             ; Name: @GName_dpb_sql_role_name              ;  Caption: @GCaption_dpb_sql_role_name             ; VDefault: @GValue_dpb_sql_role_name               ;  VType: CType_dpb_sql_role_name             ; Flags: [fbpfConnect]                         );
  GInfo_dpb_set_page_buffers          : RFbParameterInfo = (Code: isc_dpb_set_page_buffers          ; Name: @GName_dpb_set_page_buffers           ;  Caption: @GCaption_dpb_set_page_buffers          ; VDefault: @GValue_dpb_set_page_buffers            ;  VType: CType_dpb_set_page_buffers          ; );
  GInfo_dpb_working_directory         : RFbParameterInfo = (Code: isc_dpb_working_directory         ; Name: @GName_dpb_working_directory          ;  Caption: @GCaption_dpb_working_directory         ; VDefault: @GValue_dpb_working_directory           ;  VType: CType_dpb_working_directory         ; );
  GInfo_dpb_SQL_dialect               : RFbParameterInfo = (Code: isc_dpb_SQL_dialect               ; Name: @GName_dpb_SQL_dialect                ;  Caption: @GCaption_dpb_SQL_dialect               ; VDefault: @GValue_dpb_SQL_dialect                 ;  VType: CType_dpb_SQL_dialect               ; Flags: [fbpfCreate]              );
  GInfo_dpb_set_db_readonly           : RFbParameterInfo = (Code: isc_dpb_set_db_readonly           ; Name: @GName_dpb_set_db_readonly            ;  Caption: @GCaption_dpb_set_db_readonly           ; VDefault: @GValue_dpb_set_db_readonly             ;  VType: CType_dpb_set_db_readonly           ; );
  GInfo_dpb_set_db_SQL_dialect        : RFbParameterInfo = (Code: isc_dpb_set_db_SQL_dialect        ; Name: @GName_dpb_set_db_SQL_dialect         ;  Caption: @GCaption_dpb_set_db_SQL_dialect        ; VDefault: @GValue_dpb_set_db_SQL_dialect          ;  VType: CType_dpb_set_db_SQL_dialect        ; );
  GInfo_dpb_gfix_attach               : RFbParameterInfo = (Code: isc_dpb_gfix_attach               ; Name: @GName_dpb_gfix_attach                ;  Caption: @GCaption_dpb_gfix_attach               ; VDefault: @GValue_dpb_gfix_attach                 ;  VType: CType_dpb_gfix_attach               ; );
  GInfo_dpb_gstat_attach              : RFbParameterInfo = (Code: isc_dpb_gstat_attach              ; Name: @GName_dpb_gstat_attach               ;  Caption: @GCaption_dpb_gstat_attach              ; VDefault: @GValue_dpb_gstat_attach                ;  VType: CType_dpb_gstat_attach              ; );

const
  GInfo_tpb_consistency               : RFbParameterInfo = (Code: isc_tpb_consistency               ; Name: @GName_tpb_consistency                ;  Caption: @GCaption_tpb_consistency               ; VDefault: @GValue_tpb_consistency                 ;  VType: CType_tpb_consistency               ; );
  GInfo_tpb_concurrency               : RFbParameterInfo = (Code: isc_tpb_concurrency               ; Name: @GName_tpb_concurrency                ;  Caption: @GCaption_tpb_concurrency               ; VDefault: @GValue_tpb_concurrency                 ;  VType: CType_tpb_concurrency               ; );
  GInfo_tpb_shared                    : RFbParameterInfo = (Code: isc_tpb_shared                    ; Name: @GName_tpb_shared                     ;  Caption: @GCaption_tpb_shared                    ; VDefault: @GValue_tpb_shared                      ;  VType: CType_tpb_shared                    ; );
  GInfo_tpb_protected                 : RFbParameterInfo = (Code: isc_tpb_protected                 ; Name: @GName_tpb_protected                  ;  Caption: @GCaption_tpb_protected                 ; VDefault: @GValue_tpb_protected                   ;  VType: CType_tpb_protected                 ; );
  GInfo_tpb_exclusive                 : RFbParameterInfo = (Code: isc_tpb_exclusive                 ; Name: @GName_tpb_exclusive                  ;  Caption: @GCaption_tpb_exclusive                 ; VDefault: @GValue_tpb_exclusive                   ;  VType: CType_tpb_exclusive                 ; );
  GInfo_tpb_wait                      : RFbParameterInfo = (Code: isc_tpb_wait                      ; Name: @GName_tpb_wait                       ;  Caption: @GCaption_tpb_wait                      ; VDefault: @GValue_tpb_wait                        ;  VType: CType_tpb_wait                      ; );
  GInfo_tpb_nowait                    : RFbParameterInfo = (Code: isc_tpb_nowait                    ; Name: @GName_tpb_nowait                     ;  Caption: @GCaption_tpb_nowait                    ; VDefault: @GValue_tpb_nowait                      ;  VType: CType_tpb_nowait                    ; );
  GInfo_tpb_read                      : RFbParameterInfo = (Code: isc_tpb_read                      ; Name: @GName_tpb_read                       ;  Caption: @GCaption_tpb_read                      ; VDefault: @GValue_tpb_read                        ;  VType: CType_tpb_read                      ; );
  GInfo_tpb_write                     : RFbParameterInfo = (Code: isc_tpb_write                     ; Name: @GName_tpb_write                      ;  Caption: @GCaption_tpb_write                     ; VDefault: @GValue_tpb_write                       ;  VType: CType_tpb_write                     ; );
  GInfo_tpb_lock_read                 : RFbParameterInfo = (Code: isc_tpb_lock_read                 ; Name: @GName_tpb_lock_read                  ;  Caption: @GCaption_tpb_lock_read                 ; VDefault: @GValue_tpb_lock_read                   ;  VType: CType_tpb_lock_read                 ; );
  GInfo_tpb_lock_write                : RFbParameterInfo = (Code: isc_tpb_lock_write                ; Name: @GName_tpb_lock_write                 ;  Caption: @GCaption_tpb_lock_write                ; VDefault: @GValue_tpb_lock_write                  ;  VType: CType_tpb_lock_write                ; );
  GInfo_tpb_verb_time                 : RFbParameterInfo = (Code: isc_tpb_verb_time                 ; Name: @GName_tpb_verb_time                  ;  Caption: @GCaption_tpb_verb_time                 ; VDefault: @GValue_tpb_verb_time                   ;  VType: CType_tpb_verb_time                 ; );
  GInfo_tpb_commit_time               : RFbParameterInfo = (Code: isc_tpb_commit_time               ; Name: @GName_tpb_commit_time                ;  Caption: @GCaption_tpb_commit_time               ; VDefault: @GValue_tpb_commit_time                 ;  VType: CType_tpb_commit_time               ; );
  GInfo_tpb_ignore_limbo              : RFbParameterInfo = (Code: isc_tpb_ignore_limbo              ; Name: @GName_tpb_ignore_limbo               ;  Caption: @GCaption_tpb_ignore_limbo              ; VDefault: @GValue_tpb_ignore_limbo                ;  VType: CType_tpb_ignore_limbo              ; );
  GInfo_tpb_read_committed            : RFbParameterInfo = (Code: isc_tpb_read_committed            ; Name: @GName_tpb_read_committed             ;  Caption: @GCaption_tpb_read_committed            ; VDefault: @GValue_tpb_read_committed              ;  VType: CType_tpb_read_committed            ; );
  GInfo_tpb_autocommit                : RFbParameterInfo = (Code: isc_tpb_autocommit                ; Name: @GName_tpb_autocommit                 ;  Caption: @GCaption_tpb_autocommit                ; VDefault: @GValue_tpb_autocommit                  ;  VType: CType_tpb_autocommit                ; );
  GInfo_tpb_rec_version               : RFbParameterInfo = (Code: isc_tpb_rec_version               ; Name: @GName_tpb_rec_version                ;  Caption: @GCaption_tpb_rec_version               ; VDefault: @GValue_tpb_rec_version                 ;  VType: CType_tpb_rec_version               ; );
  GInfo_tpb_no_rec_version            : RFbParameterInfo = (Code: isc_tpb_no_rec_version            ; Name: @GName_tpb_no_rec_version             ;  Caption: @GCaption_tpb_no_rec_version            ; VDefault: @GValue_tpb_no_rec_version              ;  VType: CType_tpb_no_rec_version            ; );
  GInfo_tpb_restart_requests          : RFbParameterInfo = (Code: isc_tpb_restart_requests          ; Name: @GName_tpb_restart_requests           ;  Caption: @GCaption_tpb_restart_requests          ; VDefault: @GValue_tpb_restart_requests            ;  VType: CType_tpb_restart_requests          ; );
  GInfo_tpb_no_auto_undo              : RFbParameterInfo = (Code: isc_tpb_no_auto_undo              ; Name: @GName_tpb_no_auto_undo               ;  Caption: @GCaption_tpb_no_auto_undo              ; VDefault: @GValue_tpb_no_auto_undo                ;  VType: CType_tpb_no_auto_undo              ; );

const
  GInfo_bpb_source_type               : RFbParameterInfo = (Code: isc_bpb_source_type               ; Name: @GName_bpb_source_type                ;  Caption: @GCaption_bpb_source_type               ; VDefault: @GValue_bpb_source_type                 ;  VType: CType_bpb_source_type               ; );
  GInfo_bpb_target_type               : RFbParameterInfo = (Code: isc_bpb_target_type               ; Name: @GName_bpb_target_type                ;  Caption: @GCaption_bpb_target_type               ; VDefault: @GValue_bpb_target_type                 ;  VType: CType_bpb_target_type               ; );
  GInfo_bpb_type                      : RFbParameterInfo = (Code: isc_bpb_type                      ; Name: @GName_bpb_type                       ;  Caption: @GCaption_bpb_type                      ; VDefault: @GValue_bpb_type                        ;  VType: CType_bpb_type                      ; );
  GInfo_bpb_source_interp             : RFbParameterInfo = (Code: isc_bpb_source_interp             ; Name: @GName_bpb_source_interp              ;  Caption: @GCaption_bpb_source_interp             ; VDefault: @GValue_bpb_source_interp               ;  VType: CType_bpb_source_interp             ; );
  GInfo_bpb_target_interp             : RFbParameterInfo = (Code: isc_bpb_target_interp             ; Name: @GName_bpb_target_interp              ;  Caption: @GCaption_bpb_target_interp             ; VDefault: @GValue_bpb_target_interp               ;  VType: CType_bpb_target_interp             ; );
  GInfo_bpb_filter_parameter          : RFbParameterInfo = (Code: isc_bpb_filter_parameter          ; Name: @GName_bpb_filter_parameter           ;  Caption: @GCaption_bpb_filter_parameter          ; VDefault: @GValue_bpb_filter_parameter            ;  VType: CType_bpb_filter_parameter          ; );

const
  GInfo_spb_user_name                 : RFbParameterInfo = (Code: isc_spb_user_name                 ; Name: @GName_spb_user_name                  ;  Caption: @GCaption_spb_user_name                 ; VDefault: @GValue_spb_user_name                   ;  VType: CType_spb_user_name                 ; );
  GInfo_spb_sys_user_name             : RFbParameterInfo = (Code: isc_spb_sys_user_name             ; Name: @GName_spb_sys_user_name              ;  Caption: @GCaption_spb_sys_user_name             ; VDefault: @GValue_spb_sys_user_name               ;  VType: CType_spb_sys_user_name             ; );
  GInfo_spb_sys_user_name_enc         : RFbParameterInfo = (Code: isc_spb_sys_user_name_enc         ; Name: @GName_spb_sys_user_name_enc          ;  Caption: @GCaption_spb_sys_user_name_enc         ; VDefault: @GValue_spb_sys_user_name_enc           ;  VType: CType_spb_sys_user_name_enc         ; );
  GInfo_spb_password                  : RFbParameterInfo = (Code: isc_spb_password                  ; Name: @GName_spb_password                   ;  Caption: @GCaption_spb_password                  ; VDefault: @GValue_spb_password                    ;  VType: CType_spb_password                  ; );
  GInfo_spb_password_enc              : RFbParameterInfo = (Code: isc_spb_password_enc              ; Name: @GName_spb_password_enc               ;  Caption: @GCaption_spb_password_enc              ; VDefault: @GValue_spb_password_enc                ;  VType: CType_spb_password_enc              ; );
  GInfo_spb_command_line              : RFbParameterInfo = (Code: isc_spb_command_line              ; Name: @GName_spb_command_line               ;  Caption: @GCaption_spb_command_line              ; VDefault: @GValue_spb_command_line                ;  VType: CType_spb_command_line              ; );
  GInfo_spb_dbname                    : RFbParameterInfo = (Code: isc_spb_dbname                    ; Name: @GName_spb_dbname                     ;  Caption: @GCaption_spb_dbname                    ; VDefault: @GValue_spb_dbname                      ;  VType: CType_spb_dbname                    ; );
  GInfo_spb_verbose                   : RFbParameterInfo = (Code: isc_spb_verbose                   ; Name: @GName_spb_verbose                    ;  Caption: @GCaption_spb_verbose                   ; VDefault: @GValue_spb_verbose                     ;  VType: CType_spb_verbose                   ; );
  GInfo_spb_options                   : RFbParameterInfo = (Code: isc_spb_options                   ; Name: @GName_spb_options                    ;  Caption: @GCaption_spb_options                   ; VDefault: @GValue_spb_options                     ;  VType: CType_spb_options                   ; );
  GInfo_spb_connect_timeout           : RFbParameterInfo = (Code: isc_spb_connect_timeout           ; Name: @GName_spb_connect_timeout            ;  Caption: @GCaption_spb_connect_timeout           ; VDefault: @GValue_spb_connect_timeout             ;  VType: CType_spb_connect_timeout           ; );
  GInfo_spb_dummy_packet_interval     : RFbParameterInfo = (Code: isc_spb_dummy_packet_interval     ; Name: @GName_spb_dummy_packet_interval      ;  Caption: @GCaption_spb_dummy_packet_interval     ; VDefault: @GValue_spb_dummy_packet_interval       ;  VType: CType_spb_dummy_packet_interval     ; );
  GInfo_spb_sql_role_name             : RFbParameterInfo = (Code: isc_spb_sql_role_name             ; Name: @GName_spb_sql_role_name              ;  Caption: @GCaption_spb_sql_role_name             ; VDefault: @GValue_spb_sql_role_name               ;  VType: CType_spb_sql_role_name             ; );

const
  GInfo_spb_bkp_file                  : RFbParameterInfo = (Code: isc_spb_bkp_file                  ; Name: @GName_spb_bkp_file                   ;  Caption: @GCaption_spb_bkp_file                  ; VDefault: @GValue_spb_bkp_file                    ;  VType: CType_spb_bkp_file                  ; );
  GInfo_spb_bkp_factor                : RFbParameterInfo = (Code: isc_spb_bkp_factor                ; Name: @GName_spb_bkp_factor                 ;  Caption: @GCaption_spb_bkp_factor                ; VDefault: @GValue_spb_bkp_factor                  ;  VType: CType_spb_bkp_factor                ; );
  GInfo_spb_bkp_length                : RFbParameterInfo = (Code: isc_spb_bkp_length                ; Name: @GName_spb_bkp_length                 ;  Caption: @GCaption_spb_bkp_length                ; VDefault: @GValue_spb_bkp_length                  ;  VType: CType_spb_bkp_length                ; );

const
  GInfo_spb_lic_key                   : RFbParameterInfo = (Code: isc_spb_lic_key                   ; Name: @GName_spb_lic_key                    ;  Caption: @GCaption_spb_lic_key                   ; VDefault: @GValue_spb_lic_key                     ;  VType: CType_spb_lic_key                   ; );
  GInfo_spb_lic_id                    : RFbParameterInfo = (Code: isc_spb_lic_id                    ; Name: @GName_spb_lic_id                     ;  Caption: @GCaption_spb_lic_id                    ; VDefault: @GValue_spb_lic_id                      ;  VType: CType_spb_lic_id                    ; );
  GInfo_spb_lic_desc                  : RFbParameterInfo = (Code: isc_spb_lic_desc                  ; Name: @GName_spb_lic_desc                   ;  Caption: @GCaption_spb_lic_desc                  ; VDefault: @GValue_spb_lic_desc                    ;  VType: CType_spb_lic_desc                  ; );

const
  GFbLicense: array[TFbLicenseParameter] of PFbParameterInfo  = (
      @GInfo_spb_lic_key              ,
      @GInfo_spb_lic_id               ,
      @GInfo_spb_lic_desc
    );

const
  GInfo_CFG_LOCKMEM_KEY               : RFbParameterInfo = (Code: ISCCFG_LOCKMEM_KEY                ; Name: @GName_CFG_LOCKMEM_KEY                ;  Caption: @GCaption_CFG_LOCKMEM_KEY               ; VDefault: @GValue_CFG_LOCKMEM_KEY                 ;  VType: CType_CFG_LOCKMEM_KEY               ; );
  GInfo_CFG_LOCKSEM_KEY               : RFbParameterInfo = (Code: ISCCFG_LOCKSEM_KEY                ; Name: @GName_CFG_LOCKSEM_KEY                ;  Caption: @GCaption_CFG_LOCKSEM_KEY               ; VDefault: @GValue_CFG_LOCKSEM_KEY                 ;  VType: CType_CFG_LOCKSEM_KEY               ; );
  GInfo_CFG_LOCKSIG_KEY               : RFbParameterInfo = (Code: ISCCFG_LOCKSIG_KEY                ; Name: @GName_CFG_LOCKSIG_KEY                ;  Caption: @GCaption_CFG_LOCKSIG_KEY               ; VDefault: @GValue_CFG_LOCKSIG_KEY                 ;  VType: CType_CFG_LOCKSIG_KEY               ; );
  GInfo_CFG_EVNTMEM_KEY               : RFbParameterInfo = (Code: ISCCFG_EVNTMEM_KEY                ; Name: @GName_CFG_EVNTMEM_KEY                ;  Caption: @GCaption_CFG_EVNTMEM_KEY               ; VDefault: @GValue_CFG_EVNTMEM_KEY                 ;  VType: CType_CFG_EVNTMEM_KEY               ; );
  GInfo_CFG_DBCACHE_KEY               : RFbParameterInfo = (Code: ISCCFG_DBCACHE_KEY                ; Name: @GName_CFG_DBCACHE_KEY                ;  Caption: @GCaption_CFG_DBCACHE_KEY               ; VDefault: @GValue_CFG_DBCACHE_KEY                 ;  VType: CType_CFG_DBCACHE_KEY               ; );
  GInfo_CFG_PRIORITY_KEY              : RFbParameterInfo = (Code: ISCCFG_PRIORITY_KEY               ; Name: @GName_CFG_PRIORITY_KEY               ;  Caption: @GCaption_CFG_PRIORITY_KEY              ; VDefault: @GValue_CFG_PRIORITY_KEY                ;  VType: CType_CFG_PRIORITY_KEY              ; );
  GInfo_CFG_IPCMAP_KEY                : RFbParameterInfo = (Code: ISCCFG_IPCMAP_KEY                 ; Name: @GName_CFG_IPCMAP_KEY                 ;  Caption: @GCaption_CFG_IPCMAP_KEY                ; VDefault: @GValue_CFG_IPCMAP_KEY                  ;  VType: CType_CFG_IPCMAP_KEY                ; );
  GInfo_CFG_MEMMIN_KEY                : RFbParameterInfo = (Code: ISCCFG_MEMMIN_KEY                 ; Name: @GName_CFG_MEMMIN_KEY                 ;  Caption: @GCaption_CFG_MEMMIN_KEY                ; VDefault: @GValue_CFG_MEMMIN_KEY                  ;  VType: CType_CFG_MEMMIN_KEY                ; );
  GInfo_CFG_MEMMAX_KEY                : RFbParameterInfo = (Code: ISCCFG_MEMMAX_KEY                 ; Name: @GName_CFG_MEMMAX_KEY                 ;  Caption: @GCaption_CFG_MEMMAX_KEY                ; VDefault: @GValue_CFG_MEMMAX_KEY                  ;  VType: CType_CFG_MEMMAX_KEY                ; );
  GInfo_CFG_LOCKORDER_KEY             : RFbParameterInfo = (Code: ISCCFG_LOCKORDER_KEY              ; Name: @GName_CFG_LOCKORDER_KEY              ;  Caption: @GCaption_CFG_LOCKORDER_KEY             ; VDefault: @GValue_CFG_LOCKORDER_KEY               ;  VType: CType_CFG_LOCKORDER_KEY             ; );
  GInfo_CFG_ANYLOCKMEM_KEY            : RFbParameterInfo = (Code: ISCCFG_ANYLOCKMEM_KEY             ; Name: @GName_CFG_ANYLOCKMEM_KEY             ;  Caption: @GCaption_CFG_ANYLOCKMEM_KEY            ; VDefault: @GValue_CFG_ANYLOCKMEM_KEY              ;  VType: CType_CFG_ANYLOCKMEM_KEY            ; );
  GInfo_CFG_ANYLOCKSEM_KEY            : RFbParameterInfo = (Code: ISCCFG_ANYLOCKSEM_KEY             ; Name: @GName_CFG_ANYLOCKSEM_KEY             ;  Caption: @GCaption_CFG_ANYLOCKSEM_KEY            ; VDefault: @GValue_CFG_ANYLOCKSEM_KEY              ;  VType: CType_CFG_ANYLOCKSEM_KEY            ; );
  GInfo_CFG_ANYLOCKSIG_KEY            : RFbParameterInfo = (Code: ISCCFG_ANYLOCKSIG_KEY             ; Name: @GName_CFG_ANYLOCKSIG_KEY             ;  Caption: @GCaption_CFG_ANYLOCKSIG_KEY            ; VDefault: @GValue_CFG_ANYLOCKSIG_KEY              ;  VType: CType_CFG_ANYLOCKSIG_KEY            ; );
  GInfo_CFG_ANYEVNTMEM_KEY            : RFbParameterInfo = (Code: ISCCFG_ANYEVNTMEM_KEY             ; Name: @GName_CFG_ANYEVNTMEM_KEY             ;  Caption: @GCaption_CFG_ANYEVNTMEM_KEY            ; VDefault: @GValue_CFG_ANYEVNTMEM_KEY              ;  VType: CType_CFG_ANYEVNTMEM_KEY            ; );
  GInfo_CFG_LOCKHASH_KEY              : RFbParameterInfo = (Code: ISCCFG_LOCKHASH_KEY               ; Name: @GName_CFG_LOCKHASH_KEY               ;  Caption: @GCaption_CFG_LOCKHASH_KEY              ; VDefault: @GValue_CFG_LOCKHASH_KEY                ;  VType: CType_CFG_LOCKHASH_KEY              ; );
  GInfo_CFG_DEADLOCK_KEY              : RFbParameterInfo = (Code: ISCCFG_DEADLOCK_KEY               ; Name: @GName_CFG_DEADLOCK_KEY               ;  Caption: @GCaption_CFG_DEADLOCK_KEY              ; VDefault: @GValue_CFG_DEADLOCK_KEY                ;  VType: CType_CFG_DEADLOCK_KEY              ; );
  GInfo_CFG_LOCKSPIN_KEY              : RFbParameterInfo = (Code: ISCCFG_LOCKSPIN_KEY               ; Name: @GName_CFG_LOCKSPIN_KEY               ;  Caption: @GCaption_CFG_LOCKSPIN_KEY              ; VDefault: @GValue_CFG_LOCKSPIN_KEY                ;  VType: CType_CFG_LOCKSPIN_KEY              ; );
  GInfo_CFG_CONN_TIMEOUT_KEY          : RFbParameterInfo = (Code: ISCCFG_CONN_TIMEOUT_KEY           ; Name: @GName_CFG_CONN_TIMEOUT_KEY           ;  Caption: @GCaption_CFG_CONN_TIMEOUT_KEY          ; VDefault: @GValue_CFG_CONN_TIMEOUT_KEY            ;  VType: CType_CFG_CONN_TIMEOUT_KEY          ; );
  GInfo_CFG_DUMMY_INTRVL_KEY          : RFbParameterInfo = (Code: ISCCFG_DUMMY_INTRVL_KEY           ; Name: @GName_CFG_DUMMY_INTRVL_KEY           ;  Caption: @GCaption_CFG_DUMMY_INTRVL_KEY          ; VDefault: @GValue_CFG_DUMMY_INTRVL_KEY            ;  VType: CType_CFG_DUMMY_INTRVL_KEY          ; );

const
  GFbServerConfig: array[TFbServerConfigParameter] of PFbParameterInfo  = (
      @GInfo_CFG_LOCKMEM_KEY          ,
      @GInfo_CFG_LOCKSEM_KEY          ,  
      @GInfo_CFG_LOCKSIG_KEY          ,  
      @GInfo_CFG_EVNTMEM_KEY          ,  
      @GInfo_CFG_DBCACHE_KEY          ,  
      @GInfo_CFG_PRIORITY_KEY         ,  
      @GInfo_CFG_IPCMAP_KEY           ,  
      @GInfo_CFG_MEMMIN_KEY           ,  
      @GInfo_CFG_MEMMAX_KEY           ,  
      @GInfo_CFG_LOCKORDER_KEY        ,  
      @GInfo_CFG_ANYLOCKMEM_KEY       ,  
      @GInfo_CFG_ANYLOCKSEM_KEY       ,  
      @GInfo_CFG_ANYLOCKSIG_KEY       ,  
      @GInfo_CFG_ANYEVNTMEM_KEY       ,  
      @GInfo_CFG_LOCKHASH_KEY         ,  
      @GInfo_CFG_DEADLOCK_KEY         ,  
      @GInfo_CFG_LOCKSPIN_KEY         ,  
      @GInfo_CFG_CONN_TIMEOUT_KEY     ,  
      @GInfo_CFG_DUMMY_INTRVL_KEY          
    );
    
const
  GInfo_spb_sec_userid                : RFbParameterInfo = (Code: isc_spb_sec_userid                ; Name: @GName_spb_sec_userid                 ;  Caption: @GCaption_spb_sec_userid                ; VDefault: @GValue_spb_sec_userid                  ;  VType: CType_spb_sec_userid                ; );
  GInfo_spb_sec_groupid               : RFbParameterInfo = (Code: isc_spb_sec_groupid               ; Name: @GName_spb_sec_groupid                ;  Caption: @GCaption_spb_sec_groupid               ; VDefault: @GValue_spb_sec_groupid                 ;  VType: CType_spb_sec_groupid               ; );
  GInfo_spb_sec_username              : RFbParameterInfo = (Code: isc_spb_sec_username              ; Name: @GName_spb_sec_username               ;  Caption: @GCaption_spb_sec_username              ; VDefault: @GValue_spb_sec_username                ;  VType: CType_spb_sec_username              ; );
  GInfo_spb_sec_password              : RFbParameterInfo = (Code: isc_spb_sec_password              ; Name: @GName_spb_sec_password               ;  Caption: @GCaption_spb_sec_password              ; VDefault: @GValue_spb_sec_password                ;  VType: CType_spb_sec_password              ; );
  GInfo_spb_sec_groupname             : RFbParameterInfo = (Code: isc_spb_sec_groupname             ; Name: @GName_spb_sec_groupname              ;  Caption: @GCaption_spb_sec_groupname             ; VDefault: @GValue_spb_sec_groupname               ;  VType: CType_spb_sec_groupname             ; );
  GInfo_spb_sec_firstname             : RFbParameterInfo = (Code: isc_spb_sec_firstname             ; Name: @GName_spb_sec_firstname              ;  Caption: @GCaption_spb_sec_firstname             ; VDefault: @GValue_spb_sec_firstname               ;  VType: CType_spb_sec_firstname             ; );
  GInfo_spb_sec_middlename            : RFbParameterInfo = (Code: isc_spb_sec_middlename            ; Name: @GName_spb_sec_middlename             ;  Caption: @GCaption_spb_sec_middlename            ; VDefault: @GValue_spb_sec_middlename              ;  VType: CType_spb_sec_middlename            ; );
  GInfo_spb_sec_lastname              : RFbParameterInfo = (Code: isc_spb_sec_lastname              ; Name: @GName_spb_sec_lastname               ;  Caption: @GCaption_spb_sec_lastname              ; VDefault: @GValue_spb_sec_lastname                ;  VType: CType_spb_sec_lastname              ; );

const
  GFbSecurityAccountParameter: array[TFbSecurityAccountParameter] of PFbParameterInfo  = (
      @GInfo_spb_sec_userid           ,
      @GInfo_spb_sec_groupid          ,
      @GInfo_spb_sec_username         ,
      @GInfo_spb_sec_password         ,
      @GInfo_spb_sec_groupname        ,
      @GInfo_spb_sec_firstname        ,
      @GInfo_spb_sec_middlename       ,
      @GInfo_spb_sec_lastname
    );

const
  GInfo_spb_num_att                   : RFbParameterInfo = (Code: isc_spb_num_att                   ; Name: @GName_spb_num_att                    ;  Caption: @GCaption_spb_num_att                   ; VDefault: @GValue_spb_num_att                     ;  VType: CType_spb_num_att                   ; );
  GInfo_spb_num_db                    : RFbParameterInfo = (Code: isc_spb_num_db                    ; Name: @GName_spb_num_db                     ;  Caption: @GCaption_spb_num_db                    ; VDefault: @GValue_spb_num_db                      ;  VType: CType_spb_num_db                    ; );

const
  GFbDatabaseInformationParameter: array[TFbDatabaseInformationParameter] of PFbParameterInfo  = (
      @GInfo_spb_dbname               ,
      @GInfo_spb_num_att              ,
      @GInfo_spb_num_db
    );

const
  GInfo_svc_version                   : RFbParameterInfo = (Code: isc_info_svc_version              ; Name: @GName_info_svc_version               ;  Caption: @GCaption_info_svc_version              ; VDefault: @GValue_info_svc_version                ;  VType: CType_info_svc_version              ; );
  GInfo_svc_server_version            : RFbParameterInfo = (Code: isc_info_svc_server_version       ; Name: @GName_info_svc_server_version        ;  Caption: @GCaption_info_svc_server_version       ; VDefault: @GValue_info_svc_server_version         ;  VType: CType_info_svc_server_version       ; );
  GInfo_svc_implementation            : RFbParameterInfo = (Code: isc_info_svc_implementation       ; Name: @GName_info_svc_implementation        ;  Caption: @GCaption_info_svc_implementation       ; VDefault: @GValue_info_svc_implementation         ;  VType: CType_info_svc_implementation       ; );
  GInfo_svc_get_license           {*} : RFbParameterInfo = (Code: isc_info_svc_get_license          ; Name: @GName_info_svc_get_license           ;  Caption: @GCaption_info_svc_get_license          ; VDefault: @GValue_info_svc_get_license            ;  VType: CType_info_svc_get_license          ; Children: ( Count:  Length(GFbLicense)                          ; Info: @GFbLicense                           ; ); );
  GInfo_svc_get_license_mask          : RFbParameterInfo = (Code: isc_info_svc_get_license_mask     ; Name: @GName_info_svc_get_license_mask      ;  Caption: @GCaption_info_svc_get_license_mask     ; VDefault: @GValue_info_svc_get_license_mask       ;  VType: CType_info_svc_get_license_mask     ; );
  GInfo_svc_capabilities              : RFbParameterInfo = (Code: isc_info_svc_capabilities         ; Name: @GName_info_svc_capabilities          ;  Caption: @GCaption_info_svc_capabilities         ; VDefault: @GValue_info_svc_capabilities           ;  VType: CType_info_svc_capabilities         ; );
  GInfo_svc_get_config            {*} : RFbParameterInfo = (Code: isc_info_svc_get_config           ; Name: @GName_info_svc_get_config            ;  Caption: @GCaption_info_svc_get_config           ; VDefault: @GValue_info_svc_get_config             ;  VType: CType_info_svc_get_config           ; Children: ( Count:  Length(GFbServerConfig)                     ; Info: @GFbServerConfig                      ; ); );
  GInfo_svc_get_env                   : RFbParameterInfo = (Code: isc_info_svc_get_env              ; Name: @GName_info_svc_get_env               ;  Caption: @GCaption_info_svc_get_env              ; VDefault: @GValue_info_svc_get_env                ;  VType: CType_info_svc_get_env              ; );
  GInfo_svc_get_env_lock              : RFbParameterInfo = (Code: isc_info_svc_get_env_lock         ; Name: @GName_info_svc_get_env_lock          ;  Caption: @GCaption_info_svc_get_env_lock         ; VDefault: @GValue_info_svc_get_env_lock           ;  VType: CType_info_svc_get_env_lock         ; );
  GInfo_svc_get_env_msg               : RFbParameterInfo = (Code: isc_info_svc_get_env_msg          ; Name: @GName_info_svc_get_env_msg           ;  Caption: @GCaption_info_svc_get_env_msg          ; VDefault: @GValue_info_svc_get_env_msg            ;  VType: CType_info_svc_get_env_msg          ; );
  GInfo_svc_get_licensed_users        : RFbParameterInfo = (Code: isc_info_svc_get_licensed_users   ; Name: @GName_info_svc_get_licensed_users    ;  Caption: @GCaption_info_svc_get_licensed_users   ; VDefault: @GValue_info_svc_get_licensed_users     ;  VType: CType_info_svc_get_licensed_users   ; );
  GInfo_svc_user_dbpath               : RFbParameterInfo = (Code: isc_info_svc_user_dbpath          ; Name: @GName_info_svc_user_dbpath           ;  Caption: @GCaption_info_svc_user_dbpath          ; VDefault: @GValue_info_svc_user_dbpath            ;  VType: CType_info_svc_user_dbpath          ; );
  GInfo_svc_get_users             {*} : RFbParameterInfo = (Code: isc_info_svc_get_users            ; Name: @GName_info_svc_get_users             ;  Caption: @GCaption_info_svc_get_users            ; VDefault: @GValue_info_svc_get_users              ;  VType: CType_info_svc_get_users            ; Children: ( Count:  Length(GFbSecurityAccountParameter)         ; Info: @GFbSecurityAccountParameter          ; ); );
  GInfo_svc_svr_db_info           {*} : RFbParameterInfo = (Code: isc_info_svc_svr_db_info          ; Name: @GName_info_svc_svr_db_info           ;  Caption: @GCaption_info_svc_svr_db_info          ; VDefault: @GValue_info_svc_svr_db_info            ;  VType: CType_info_svc_svr_db_info          ; Children: ( Count:  Length(GFbDatabaseInformationParameter)     ; Info: @GFbDatabaseInformationParameter      ; ); );
  GInfo_svc_line                      : RFbParameterInfo = (Code: isc_info_svc_line                 ; Name: @GName_info_svc_line                  ;  Caption: @GCaption_info_svc_line                 ; VDefault: @GValue_info_svc_line                   ;  VType: CType_info_svc_line                 ; );
  GInfo_svc_to_eof                    : RFbParameterInfo = (Code: isc_info_svc_to_eof               ; Name: @GName_info_svc_to_eof                ;  Caption: @GCaption_info_svc_to_eof               ; VDefault: @GValue_info_svc_to_eof                 ;  VType: CType_info_svc_to_eof               ; );
  GInfo_svc_running                   : RFbParameterInfo = (Code: isc_info_svc_running              ; Name: @GName_info_svc_running               ;  Caption: @GCaption_info_svc_running              ; VDefault: @GValue_info_svc_running                ;  VType: CType_info_svc_running              ; );
  GInfo_svc_timeout                   : RFbParameterInfo = (Code: isc_info_svc_timeout              ; Name: @GName_info_svc_timeout               ;  Caption: @GCaption_info_svc_timeout              ; VDefault: @GValue_info_svc_timeout                ;  VType: CType_info_svc_timeout              ; );
  GInfo_svc_limbo_trans           {*} : RFbParameterInfo = (Code: isc_info_svc_limbo_trans          ; Name: @GName_info_svc_limbo_trans           ;  Caption: @GCaption_info_svc_limbo_trans          ; VDefault: @GValue_info_svc_limbo_trans            ;  VType: CType_info_svc_limbo_trans          ; );

implementation
end.
