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

unit SilSgFirebirdParameterValues;

interface

{$include Defines.inc}

uses
  SilScFirebirdParameterTypes,
  SilScFirebirdParameterValues;

const
  GValue_dpb_cdd_pathname               : TVarData  =   ( VType:  CType_dpb_cdd_pathname            ; VOleStr    :   CValue_dpb_cdd_pathname);
  GValue_dpb_allocation                 : TVarData  =   ( VType:  CType_dpb_allocation              ; );
  GValue_dpb_journal                    : TVarData  =   ( VType:  CType_dpb_journal                 ; );
  GValue_dpb_page_size                  : TVarData  =   ( VType:  CType_dpb_page_size               ; );
  GValue_dpb_num_buffers                : TVarData  =   ( VType:  CType_dpb_num_buffers             ; VByte      :   CValue_dpb_num_buffers);
  GValue_dpb_buffer_length              : TVarData  =   ( VType:  CType_dpb_buffer_length           ; );
  GValue_dpb_debug                      : TVarData  =   ( VType:  CType_dpb_debug                   ; );
  GValue_dpb_garbage_collect            : TVarData  =   ( VType:  CType_dpb_garbage_collect         ; );
  GValue_dpb_verify                     : TVarData  =   ( VType:  CType_dpb_verify                  ; VByte      :   CValue_dpb_verify);
  GValue_dpb_sweep                      : TVarData  =   ( VType:  CType_dpb_sweep                   ; );
  GValue_dpb_enable_journal             : TVarData  =   ( VType:  CType_dpb_enable_journal          ; );
  GValue_dpb_disable_journal            : TVarData  =   ( VType:  CType_dpb_disable_journal         ; );
  GValue_dpb_dbkey_scope                : TVarData  =   ( VType:  CType_dpb_dbkey_scope             ; VByte      :   CValue_dpb_dbkey_scope);
  GValue_dpb_number_of_users            : TVarData  =   ( VType:  CType_dpb_number_of_users         ; );
  GValue_dpb_trace                      : TVarData  =   ( VType:  CType_dpb_trace                   ; );
  GValue_dpb_no_garbage_collect         : TVarData  =   ( VType:  CType_dpb_no_garbage_collect      ; );
  GValue_dpb_damaged                    : TVarData  =   ( VType:  CType_dpb_damaged                 ; VByte      :   CValue_dpb_damaged);
  GValue_dpb_license                    : TVarData  =   ( VType:  CType_dpb_license                 ; VOleStr    :   CValue_dpb_license);
  GValue_dpb_sys_user_name              : TVarData  =   ( VType:  CType_dpb_sys_user_name           ; VOleStr    :   CValue_dpb_sys_user_name);
  GValue_dpb_encrypt_key                : TVarData  =   ( VType:  CType_dpb_encrypt_key             ; VOleStr    :   CValue_dpb_encrypt_key);
  GValue_dpb_activate_shadow            : TVarData  =   ( VType:  CType_dpb_activate_shadow         ; VBoolean   :   CValue_dpb_activate_shadow);
  GValue_dpb_sweep_interval             : TVarData  =   ( VType:  CType_dpb_sweep_interval          ; VInteger   :   CValue_dpb_sweep_interval);
  GValue_dpb_delete_shadow              : TVarData  =   ( VType:  CType_dpb_delete_shadow           ; VBoolean   :   CValue_dpb_delete_shadow);
  GValue_dpb_force_write                : TVarData  =   ( VType:  CType_dpb_force_write             ; VByte      :   CValue_dpb_force_write);
  GValue_dpb_begin_log                  : TVarData  =   ( VType:  CType_dpb_begin_log               ; VBoolean   :   CValue_dpb_begin_log);
  GValue_dpb_quit_log                   : TVarData  =   ( VType:  CType_dpb_quit_log                ; VBoolean   :   CValue_dpb_quit_log);
  GValue_dpb_no_reserve                 : TVarData  =   ( VType:  CType_dpb_no_reserve              ; VByte      :   CValue_dpb_no_reserve);
  GValue_dpb_user_name                  : TVarData  =   ( VType:  CType_dpb_user_name               ; VOleStr    :   CValue_dpb_user_name);
  GValue_dpb_password                   : TVarData  =   ( VType:  CType_dpb_password                ; VOleStr    :   CValue_dpb_password);
  GValue_dpb_password_enc               : TVarData  =   ( VType:  CType_dpb_password_enc            ; VOleStr    :   CValue_dpb_password_enc);
  GValue_dpb_sys_user_name_enc          : TVarData  =   ( VType:  CType_dpb_sys_user_name_enc       ; VOleStr    :   CValue_dpb_sys_user_name_enc);
  GValue_dpb_interp                     : TVarData  =   ( VType:  CType_dpb_interp                  ; );
  GValue_dpb_online_dump                : TVarData  =   ( VType:  CType_dpb_online_dump             ; );
  GValue_dpb_old_file_size              : TVarData  =   ( VType:  CType_dpb_old_file_size           ; );
  GValue_dpb_old_num_files              : TVarData  =   ( VType:  CType_dpb_old_num_files           ; );
  GValue_dpb_old_file                   : TVarData  =   ( VType:  CType_dpb_old_file                ; );
  GValue_dpb_old_start_page             : TVarData  =   ( VType:  CType_dpb_old_start_page          ; );
  GValue_dpb_old_start_seqno            : TVarData  =   ( VType:  CType_dpb_old_start_seqno         ; );
  GValue_dpb_old_start_file             : TVarData  =   ( VType:  CType_dpb_old_start_file          ; );
  GValue_dpb_drop_walfile               : TVarData  =   ( VType:  CType_dpb_drop_walfile            ; );
  GValue_dpb_old_dump_id                : TVarData  =   ( VType:  CType_dpb_old_dump_id             ; );
  GValue_dpb_wal_backup_dir             : TVarData  =   ( VType:  CType_dpb_wal_backup_dir          ; );
  GValue_dpb_wal_chkptlen               : TVarData  =   ( VType:  CType_dpb_wal_chkptlen            ; );
  GValue_dpb_wal_numbufs                : TVarData  =   ( VType:  CType_dpb_wal_numbufs             ; );
  GValue_dpb_wal_bufsize                : TVarData  =   ( VType:  CType_dpb_wal_bufsize             ; );
  GValue_dpb_wal_grp_cmt_wait           : TVarData  =   ( VType:  CType_dpb_wal_grp_cmt_wait        ; );
  GValue_dpb_lc_messages                : TVarData  =   ( VType:  CType_dpb_lc_messages             ; VOleStr    :   CValue_dpb_lc_messages);
  GValue_dpb_lc_ctype                   : TVarData  =   ( VType:  CType_dpb_lc_ctype                ; VOleStr    :   CValue_dpb_lc_ctype);
  GValue_dpb_cache_manager              : TVarData  =   ( VType:  CType_dpb_cache_manager           ; );
  GValue_dpb_shutdown                   : TVarData  =   ( VType:  CType_dpb_shutdown                ; );
  GValue_dpb_online                     : TVarData  =   ( VType:  CType_dpb_online                  ; );
  GValue_dpb_shutdown_delay             : TVarData  =   ( VType:  CType_dpb_shutdown_delay          ; );
  GValue_dpb_reserved                   : TVarData  =   ( VType:  CType_dpb_reserved                ; );
  GValue_dpb_overwrite                  : TVarData  =   ( VType:  CType_dpb_overwrite               ; );
  GValue_dpb_sec_attach                 : TVarData  =   ( VType:  CType_dpb_sec_attach              ; );
  GValue_dpb_disable_wal                : TVarData  =   ( VType:  CType_dpb_disable_wal             ; );
  GValue_dpb_connect_timeout            : TVarData  =   ( VType:  CType_dpb_connect_timeout         ; VInteger   :   CValue_dpb_connect_timeout);
  GValue_dpb_dummy_packet_interval      : TVarData  =   ( VType:  CType_dpb_dummy_packet_interval   ; );
  GValue_dpb_gbak_attach                : TVarData  =   ( VType:  CType_dpb_gbak_attach             ; );
  GValue_dpb_sql_role_name              : TVarData  =   ( VType:  CType_dpb_sql_role_name           ; VOleStr    :   CValue_dpb_sql_role_name);
  GValue_dpb_set_page_buffers           : TVarData  =   ( VType:  CType_dpb_set_page_buffers        ; );
  GValue_dpb_working_directory          : TVarData  =   ( VType:  CType_dpb_working_directory       ; );
  GValue_dpb_SQL_dialect                : TVarData  =   ( VType:  CType_dpb_SQL_dialect             ; );
  GValue_dpb_set_db_readonly            : TVarData  =   ( VType:  CType_dpb_set_db_readonly         ; );
  GValue_dpb_set_db_SQL_dialect         : TVarData  =   ( VType:  CType_dpb_set_db_SQL_dialect      ; );
  GValue_dpb_gfix_attach                : TVarData  =   ( VType:  CType_dpb_gfix_attach             ; );
  GValue_dpb_gstat_attach               : TVarData  =   ( VType:  CType_dpb_gstat_attach            ; );
                                        
const                                   
  GValue_tpb_consistency                : TVarData  =   ( VType:  CType_tpb_consistency             ; );
  GValue_tpb_concurrency                : TVarData  =   ( VType:  CType_tpb_concurrency             ; );
  GValue_tpb_shared                     : TVarData  =   ( VType:  CType_tpb_shared                  ; );
  GValue_tpb_protected                  : TVarData  =   ( VType:  CType_tpb_protected               ; );
  GValue_tpb_exclusive                  : TVarData  =   ( VType:  CType_tpb_exclusive               ; );
  GValue_tpb_wait                       : TVarData  =   ( VType:  CType_tpb_wait                    ; );
  GValue_tpb_nowait                     : TVarData  =   ( VType:  CType_tpb_nowait                  ; );
  GValue_tpb_read                       : TVarData  =   ( VType:  CType_tpb_read                    ; );
  GValue_tpb_write                      : TVarData  =   ( VType:  CType_tpb_write                   ; );
  GValue_tpb_lock_read                  : TVarData  =   ( VType:  CType_tpb_lock_read               ; );
  GValue_tpb_lock_write                 : TVarData  =   ( VType:  CType_tpb_lock_write              ; );
  GValue_tpb_verb_time                  : TVarData  =   ( VType:  CType_tpb_verb_time               ; );
  GValue_tpb_commit_time                : TVarData  =   ( VType:  CType_tpb_commit_time             ; );
  GValue_tpb_ignore_limbo               : TVarData  =   ( VType:  CType_tpb_ignore_limbo            ; );
  GValue_tpb_read_committed             : TVarData  =   ( VType:  CType_tpb_read_committed          ; );
  GValue_tpb_autocommit                 : TVarData  =   ( VType:  CType_tpb_autocommit              ; );
  GValue_tpb_rec_version                : TVarData  =   ( VType:  CType_tpb_rec_version             ; );
  GValue_tpb_no_rec_version             : TVarData  =   ( VType:  CType_tpb_no_rec_version          ; );
  GValue_tpb_restart_requests           : TVarData  =   ( VType:  CType_tpb_restart_requests        ; );
  GValue_tpb_no_auto_undo               : TVarData  =   ( VType:  CType_tpb_no_auto_undo            ; );
                                        
const                                   
  GValue_bpb_source_type                : TVarData  =   ( VType:  CType_bpb_source_type             ; );
  GValue_bpb_target_type                : TVarData  =   ( VType:  CType_bpb_target_type             ; );
  GValue_bpb_type                       : TVarData  =   ( VType:  CType_bpb_type                    ; );
  GValue_bpb_source_interp              : TVarData  =   ( VType:  CType_bpb_source_interp           ; );
  GValue_bpb_target_interp              : TVarData  =   ( VType:  CType_bpb_target_interp           ; );
  GValue_bpb_filter_parameter           : TVarData  =   ( VType:  CType_bpb_filter_parameter        ; );
                                        
const                                   
  GValue_spb_user_name                  : TVarData  =   ( VType:  CType_spb_user_name               ; );
  GValue_spb_sys_user_name              : TVarData  =   ( VType:  CType_spb_sys_user_name           ; );
  GValue_spb_sys_user_name_enc          : TVarData  =   ( VType:  CType_spb_sys_user_name_enc       ; );
  GValue_spb_password                   : TVarData  =   ( VType:  CType_spb_password                ; );
  GValue_spb_password_enc               : TVarData  =   ( VType:  CType_spb_password_enc            ; );
  GValue_spb_command_line               : TVarData  =   ( VType:  CType_spb_command_line            ; );
  GValue_spb_dbname                     : TVarData  =   ( VType:  CType_spb_dbname                  ; );
  GValue_spb_verbose                    : TVarData  =   ( VType:  CType_spb_verbose                 ; );
  GValue_spb_options                    : TVarData  =   ( VType:  CType_spb_options                 ; );
  GValue_spb_connect_timeout            : TVarData  =   ( VType:  CType_spb_connect_timeout         ; );
  GValue_spb_dummy_packet_interval      : TVarData  =   ( VType:  CType_spb_dummy_packet_interval   ; );
  GValue_spb_sql_role_name              : TVarData  =   ( VType:  CType_spb_sql_role_name           ; );
                                        
const                                   
  GValue_spb_bkp_file                   : TVarData  =   ( VType:  CType_spb_bkp_file                ; );
  GValue_spb_bkp_factor                 : TVarData  =   ( VType:  CType_spb_bkp_factor              ; );
  GValue_spb_bkp_length                 : TVarData  =   ( VType:  CType_spb_bkp_length              ; );
                                        
const                                   
  GValue_info_svc_svr_db_info           : TVarData  =   ( VType:  CType_spb_bkp_file                ; );
  GValue_info_svc_get_license           : TVarData  =   ( VType:  CType_spb_bkp_factor              ; );
  GValue_info_svc_get_license_mask      : TVarData  =   ( VType:  CType_spb_bkp_length              ; );
  GValue_info_svc_get_config            : TVarData  =   ( VType:  CType_spb_bkp_file                ; );
  GValue_info_svc_version               : TVarData  =   ( VType:  CType_spb_bkp_factor              ; );
  GValue_info_svc_server_version        : TVarData  =   ( VType:  CType_spb_bkp_length              ; );
  GValue_info_svc_implementation        : TVarData  =   ( VType:  CType_spb_bkp_file                ; );
  GValue_info_svc_capabilities          : TVarData  =   ( VType:  CType_spb_bkp_factor              ; );
  GValue_info_svc_user_dbpath           : TVarData  =   ( VType:  CType_spb_bkp_length              ; );
  GValue_info_svc_get_env               : TVarData  =   ( VType:  CType_spb_bkp_file                ; );
  GValue_info_svc_get_env_lock          : TVarData  =   ( VType:  CType_spb_bkp_factor              ; );
  GValue_info_svc_get_env_msg           : TVarData  =   ( VType:  CType_spb_bkp_length              ; );
  GValue_info_svc_line                  : TVarData  =   ( VType:  CType_spb_bkp_file                ; );
  GValue_info_svc_to_eof                : TVarData  =   ( VType:  CType_spb_bkp_factor              ; );
  GValue_info_svc_timeout               : TVarData  =   ( VType:  CType_spb_bkp_length              ; );
  GValue_info_svc_get_licensed_users    : TVarData  =   ( VType:  CType_spb_bkp_file                ; );
  GValue_info_svc_limbo_trans           : TVarData  =   ( VType:  CType_spb_bkp_factor              ; );
  GValue_info_svc_running               : TVarData  =   ( VType:  CType_spb_bkp_length              ; );
  GValue_info_svc_get_users             : TVarData  =   ( VType:  CType_spb_bkp_length              ; );
                                       
const
  GValue_spb_lic_key                    : TVarData  =   ( VType:  CType_spb_lic_key                 ; );
  GValue_spb_lic_id                     : TVarData  =   ( VType:  CType_spb_lic_id                  ; );
  GValue_spb_lic_desc                   : TVarData  =   ( VType:  CType_spb_lic_desc                ; );

const
  GValue_CFG_LOCKMEM_KEY                : TVarData  =   ( VType:  CType_CFG_LOCKMEM_KEY             ; );
  GValue_CFG_LOCKSEM_KEY                : TVarData  =   ( VType:  CType_CFG_LOCKSEM_KEY             ; );
  GValue_CFG_LOCKSIG_KEY                : TVarData  =   ( VType:  CType_CFG_LOCKSIG_KEY             ; );
  GValue_CFG_EVNTMEM_KEY                : TVarData  =   ( VType:  CType_CFG_EVNTMEM_KEY             ; );
  GValue_CFG_DBCACHE_KEY                : TVarData  =   ( VType:  CType_CFG_DBCACHE_KEY             ; );
  GValue_CFG_PRIORITY_KEY               : TVarData  =   ( VType:  CType_CFG_PRIORITY_KEY            ; );
  GValue_CFG_IPCMAP_KEY                 : TVarData  =   ( VType:  CType_CFG_IPCMAP_KEY              ; );
  GValue_CFG_MEMMIN_KEY                 : TVarData  =   ( VType:  CType_CFG_MEMMIN_KEY              ; );
  GValue_CFG_MEMMAX_KEY                 : TVarData  =   ( VType:  CType_CFG_MEMMAX_KEY              ; );
  GValue_CFG_LOCKORDER_KEY              : TVarData  =   ( VType:  CType_CFG_LOCKORDER_KEY           ; );
  GValue_CFG_ANYLOCKMEM_KEY             : TVarData  =   ( VType:  CType_CFG_ANYLOCKMEM_KEY          ; );
  GValue_CFG_ANYLOCKSEM_KEY             : TVarData  =   ( VType:  CType_CFG_ANYLOCKSEM_KEY          ; );
  GValue_CFG_ANYLOCKSIG_KEY             : TVarData  =   ( VType:  CType_CFG_ANYLOCKSIG_KEY          ; );
  GValue_CFG_ANYEVNTMEM_KEY             : TVarData  =   ( VType:  CType_CFG_ANYEVNTMEM_KEY          ; );
  GValue_CFG_LOCKHASH_KEY               : TVarData  =   ( VType:  CType_CFG_LOCKHASH_KEY            ; );
  GValue_CFG_DEADLOCK_KEY               : TVarData  =   ( VType:  CType_CFG_DEADLOCK_KEY            ; );
  GValue_CFG_LOCKSPIN_KEY               : TVarData  =   ( VType:  CType_CFG_LOCKSPIN_KEY            ; );
  GValue_CFG_CONN_TIMEOUT_KEY           : TVarData  =   ( VType:  CType_CFG_CONN_TIMEOUT_KEY        ; );
  GValue_CFG_DUMMY_INTRVL_KEY           : TVarData  =   ( VType:  CType_CFG_DUMMY_INTRVL_KEY        ; );

const
  GValue_spb_sec_userid                 : TVarData  =   ( VType:  CType_spb_sec_userid              ; );
  GValue_spb_sec_groupid                : TVarData  =   ( VType:  CType_spb_sec_groupid             ; );
  GValue_spb_sec_username               : TVarData  =   ( VType:  CType_spb_sec_username            ; );
  GValue_spb_sec_password               : TVarData  =   ( VType:  CType_spb_sec_password            ; );
  GValue_spb_sec_groupname              : TVarData  =   ( VType:  CType_spb_sec_groupname           ; );
  GValue_spb_sec_firstname              : TVarData  =   ( VType:  CType_spb_sec_firstname           ; );
  GValue_spb_sec_middlename             : TVarData  =   ( VType:  CType_spb_sec_middlename          ; );
  GValue_spb_sec_lastname               : TVarData  =   ( VType:  CType_spb_sec_lastname            ; );

const
  GValue_spb_num_att                    : TVarData  =   ( VType:  CType_spb_num_att                 ; );
  GValue_spb_num_db                     : TVarData  =   ( VType:  CType_spb_num_db                  ; );

implementation
end.
