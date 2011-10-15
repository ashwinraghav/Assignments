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

unit SilScFirebirdParameterTypes;

interface

const
  CType_dpb_cdd_pathname              =   varOleStr;
  CType_dpb_allocation                =   varEmpty; 
  CType_dpb_journal                   =   varEmpty; 
  CType_dpb_page_size                 =   varEmpty; 
  CType_dpb_num_buffers               =   varByte; 
  CType_dpb_buffer_length             =   varEmpty; 
  CType_dpb_debug                     =   varEmpty; 
  CType_dpb_garbage_collect           =   varEmpty; 
  CType_dpb_verify                    =   varByte;
  CType_dpb_sweep                     =   varEmpty; 
  CType_dpb_enable_journal            =   varEmpty; 
  CType_dpb_disable_journal           =   varEmpty; 
  CType_dpb_dbkey_scope               =   varByte; 
  CType_dpb_number_of_users           =   varEmpty; 
  CType_dpb_trace                     =   varEmpty; 
  CType_dpb_no_garbage_collect        =   varEmpty; 
  CType_dpb_damaged                   =   varByte; 
  CType_dpb_license                   =   varOleStr;
  CType_dpb_sys_user_name             =   varOleStr;
  CType_dpb_encrypt_key               =   varOleStr;
  CType_dpb_activate_shadow           =   varBoolean;
  CType_dpb_sweep_interval            =   varInteger;
  CType_dpb_delete_shadow             =   varBoolean;
  CType_dpb_force_write               =   varByte;
  CType_dpb_begin_log                 =   varBoolean;
  CType_dpb_quit_log                  =   varBoolean; 
  CType_dpb_no_reserve                =   varByte; 
  CType_dpb_user_name                 =   varOleStr; 
  CType_dpb_password                  =   varOleStr; 
  CType_dpb_password_enc              =   varOleStr;
  CType_dpb_sys_user_name_enc         =   varOleStr;
  CType_dpb_interp                    =   varEmpty; 
  CType_dpb_online_dump               =   varEmpty; 
  CType_dpb_old_file_size             =   varEmpty; 
  CType_dpb_old_num_files             =   varEmpty; 
  CType_dpb_old_file                  =   varEmpty; 
  CType_dpb_old_start_page            =   varEmpty; 
  CType_dpb_old_start_seqno           =   varEmpty; 
  CType_dpb_old_start_file            =   varEmpty; 
  CType_dpb_drop_walfile              =   varEmpty; 
  CType_dpb_old_dump_id               =   varEmpty; 
  CType_dpb_wal_backup_dir            =   varEmpty; 
  CType_dpb_wal_chkptlen              =   varEmpty; 
  CType_dpb_wal_numbufs               =   varEmpty; 
  CType_dpb_wal_bufsize               =   varEmpty; 
  CType_dpb_wal_grp_cmt_wait          =   varEmpty; 
  CType_dpb_lc_messages               =   varOleStr;
  CType_dpb_lc_ctype                  =   varOleStr;
  CType_dpb_cache_manager             =   varEmpty;
  CType_dpb_shutdown                  =   varEmpty;
  CType_dpb_online                    =   varEmpty;
  CType_dpb_shutdown_delay            =   varEmpty;
  CType_dpb_reserved                  =   varEmpty;
  CType_dpb_overwrite                 =   varEmpty; 
  CType_dpb_sec_attach                =   varEmpty; 
  CType_dpb_disable_wal               =   varEmpty; 
  CType_dpb_connect_timeout           =   varInteger; 
  CType_dpb_dummy_packet_interval     =   varEmpty; 
  CType_dpb_gbak_attach               =   varEmpty; 
  CType_dpb_sql_role_name             =   varOleStr; 
  CType_dpb_set_page_buffers          =   varEmpty; 
  CType_dpb_working_directory         =   varEmpty; 
  CType_dpb_SQL_dialect               =   varEmpty; 
  CType_dpb_set_db_readonly           =   varEmpty; 
  CType_dpb_set_db_SQL_dialect        =   varEmpty; 
  CType_dpb_gfix_attach               =   varEmpty; 
  CType_dpb_gstat_attach              =   varEmpty; 

const
  CType_tpb_consistency               =   varEmpty; 
  CType_tpb_concurrency               =   varEmpty; 
  CType_tpb_shared                    =   varEmpty; 
  CType_tpb_protected                 =   varEmpty; 
  CType_tpb_exclusive                 =   varEmpty; 
  CType_tpb_wait                      =   varEmpty; 
  CType_tpb_nowait                    =   varEmpty; 
  CType_tpb_read                      =   varEmpty; 
  CType_tpb_write                     =   varEmpty; 
  CType_tpb_lock_read                 =   varEmpty; 
  CType_tpb_lock_write                =   varEmpty; 
  CType_tpb_verb_time                 =   varEmpty; 
  CType_tpb_commit_time               =   varEmpty; 
  CType_tpb_ignore_limbo              =   varEmpty; 
  CType_tpb_read_committed            =   varEmpty; 
  CType_tpb_autocommit                =   varEmpty; 
  CType_tpb_rec_version               =   varEmpty; 
  CType_tpb_no_rec_version            =   varEmpty; 
  CType_tpb_restart_requests          =   varEmpty; 
  CType_tpb_no_auto_undo              =   varEmpty;

const
  CType_bpb_source_type               =   varEmpty;
  CType_bpb_target_type               =   varEmpty;
  CType_bpb_type                      =   varEmpty;
  CType_bpb_source_interp             =   varEmpty;
  CType_bpb_target_interp             =   varEmpty;
  CType_bpb_filter_parameter          =   varEmpty;

const
  CType_spb_user_name                 =   varOleStr;
  CType_spb_sys_user_name             =   varOleStr;
  CType_spb_sys_user_name_enc         =   varOleStr;
  CType_spb_password                  =   varOleStr;
  CType_spb_password_enc              =   varOleStr;
  CType_spb_command_line              =   varOleStr;
  CType_spb_dbname                    =   varOleStr;
  CType_spb_verbose                   =   varBoolean; 
  CType_spb_options                   =   varLongWord; 
  CType_spb_connect_timeout           =   varInteger;
  CType_spb_dummy_packet_interval     =   varEmpty; 
  CType_spb_sql_role_name             =   varOleStr; 

const
  CType_spb_bkp_file                  =   varOleStr; 
  CType_spb_bkp_factor                =   varLongWord; 
  CType_spb_bkp_length                =   varOleStr; 

const
  CType_info_svc_version              =   varLongWord;
  CType_info_svc_server_version       =   varOleStr;
  CType_info_svc_implementation       =   varOleStr;
  CType_info_svc_get_license          =   varEmpty;
  CType_info_svc_get_license_mask     =   varLongWord;
  CType_info_svc_capabilities         =   varLongWord;
  CType_info_svc_get_config           =   varEmpty;
  CType_info_svc_get_env              =   varOleStr;
  CType_info_svc_get_env_lock         =   varOleStr;
  CType_info_svc_get_env_msg          =   varOleStr;
  CType_info_svc_get_licensed_users   =   varLongWord;
  CType_info_svc_user_dbpath          =   varOleStr;
  CType_info_svc_get_users            =   varEmpty;
  CType_info_svc_svr_db_info          =   varEmpty;
  CType_info_svc_line                 =   varOleStr;
  CType_info_svc_to_eof               =   varOleStr;
  CType_info_svc_running              =   varLongWord;
  CType_info_svc_timeout              =   varEmpty;
  CType_info_svc_limbo_trans          =   varEmpty;

const
  CType_spb_lic_key                   =   varOleStr;   
  CType_spb_lic_id                    =   varOleStr;
  CType_spb_lic_desc                  =   varOleStr;

const
  CType_CFG_LOCKMEM_KEY               =   varLongWord;
  CType_CFG_LOCKSEM_KEY               =   varLongWord;
  CType_CFG_LOCKSIG_KEY               =   varLongWord;
  CType_CFG_EVNTMEM_KEY               =   varLongWord;
  CType_CFG_DBCACHE_KEY               =   varLongWord;
  CType_CFG_PRIORITY_KEY              =   varLongWord;
  CType_CFG_IPCMAP_KEY                =   varLongWord;
  CType_CFG_MEMMIN_KEY                =   varLongWord;
  CType_CFG_MEMMAX_KEY                =   varLongWord;
  CType_CFG_LOCKORDER_KEY             =   varLongWord;
  CType_CFG_ANYLOCKMEM_KEY            =   varLongWord;
  CType_CFG_ANYLOCKSEM_KEY            =   varLongWord;
  CType_CFG_ANYLOCKSIG_KEY            =   varLongWord;
  CType_CFG_ANYEVNTMEM_KEY            =   varLongWord;
  CType_CFG_LOCKHASH_KEY              =   varLongWord;
  CType_CFG_DEADLOCK_KEY              =   varLongWord;
  CType_CFG_LOCKSPIN_KEY              =   varLongWord;
  CType_CFG_CONN_TIMEOUT_KEY          =   varLongWord;
  CType_CFG_DUMMY_INTRVL_KEY          =   varLongWord;

const
  CType_spb_sec_userid                =   varLongWord;
  CType_spb_sec_groupid               =   varLongWord;
  CType_spb_sec_username              =   varOleStr;
  CType_spb_sec_password              =   varOleStr;
  CType_spb_sec_groupname             =   varOleStr;
  CType_spb_sec_firstname             =   varOleStr;
  CType_spb_sec_middlename            =   varOleStr;
  CType_spb_sec_lastname              =   varOleStr;

const
  CType_spb_num_att                   =   varLongWord;
  CType_spb_num_db                    =   varLongWord;

implementation
end.
