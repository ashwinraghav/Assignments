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

unit SilSdFirebirdParameterCaptions;

interface

resourcestring
  GCaption_dpb_cdd_pathname             = 'cdd pathname';
  GCaption_dpb_allocation               = 'allocation';
  GCaption_dpb_journal                  = 'journal';
  GCaption_dpb_page_size                = 'Page Size';
  GCaption_dpb_num_buffers              = 'num buffers';
  GCaption_dpb_buffer_length            = 'Buffer Length';
  GCaption_dpb_debug                    = 'Debug';
  GCaption_dpb_garbage_collect          = 'Garbage Collect';
  GCaption_dpb_verify                   = 'Verify';
  GCaption_dpb_sweep                    = 'Sweep';
  GCaption_dpb_enable_journal           = 'Enable Journal';
  GCaption_dpb_disable_journal          = 'Disable Journal';
  GCaption_dpb_dbkey_scope              = 'DBKEY Scope';
  GCaption_dpb_number_of_users          = 'Number Of Users';
  GCaption_dpb_trace                    = 'Trace';
  GCaption_dpb_no_garbage_collect       = 'No Garbage Collect';
  GCaption_dpb_damaged                  = 'Mark Damaged';
  GCaption_dpb_license                  = 'License';
  GCaption_dpb_sys_user_name            = 'System User Name';
  GCaption_dpb_encrypt_key              = 'Encryption Key';
  GCaption_dpb_activate_shadow          = 'Activate Shadow';
  GCaption_dpb_sweep_interval           = 'Sweep Interval';
  GCaption_dpb_delete_shadow            = 'Delete Shadow';
  GCaption_dpb_force_write              = 'Force Write';
  GCaption_dpb_begin_log                = 'Begin Log';
  GCaption_dpb_quit_log                 = 'Quit Log';
  GCaption_dpb_no_reserve               = 'No Reserve';
  GCaption_dpb_user_name                = 'User ID';
  GCaption_dpb_password                 = 'Password';
  GCaption_dpb_password_enc             = 'Encrypted Password';
  GCaption_dpb_sys_user_name_enc        = 'Encrypted System User';
  GCaption_dpb_interp                   = 'Interp';
  GCaption_dpb_online_dump              = 'Online Dump';
  GCaption_dpb_old_file_size            = 'old file size';
  GCaption_dpb_old_num_files            = 'old num files';
  GCaption_dpb_old_file                 = 'old file';
  GCaption_dpb_old_start_page           = 'old start page';
  GCaption_dpb_old_start_seqno          = 'old start seqno';
  GCaption_dpb_old_start_file           = 'old start file';
  GCaption_dpb_drop_walfile             = 'drop walfile';
  GCaption_dpb_old_dump_id              = 'old dump id';
  GCaption_dpb_wal_backup_dir           = 'wal backup dir';
  GCaption_dpb_wal_chkptlen             = 'wal chkptlen';
  GCaption_dpb_wal_numbufs              = 'wal numbufs';
  GCaption_dpb_wal_bufsize              = 'wal bufsize';
  GCaption_dpb_wal_grp_cmt_wait         = 'wal grp cmt wait';
  GCaption_dpb_lc_messages              = 'LC Messages File';
  GCaption_dpb_lc_ctype                 = 'Character Set';
  GCaption_dpb_cache_manager            = 'Cache Manager';
  GCaption_dpb_shutdown                 = 'Shutdown';
  GCaption_dpb_online                   = 'Online';
  GCaption_dpb_shutdown_delay           = 'Shutdown Delay';
  GCaption_dpb_reserved                 = 'Reserved';
  GCaption_dpb_overwrite                = 'Overwrite';
  GCaption_dpb_sec_attach               = 'sec attach';
  GCaption_dpb_disable_wal              = 'disable wal';
  GCaption_dpb_connect_timeout          = 'Connect Timeout';
  GCaption_dpb_dummy_packet_interval    = 'dummy packet interval';
  GCaption_dpb_gbak_attach              = 'gbak attach';
  GCaption_dpb_sql_role_name            = 'SQL Role Name';
  GCaption_dpb_set_page_buffers         = 'Set Page Buffers';
  GCaption_dpb_working_directory        = 'Working Directory';
  GCaption_dpb_SQL_dialect              = 'SQL Dialect';
  GCaption_dpb_set_db_readonly          = 'Set DB Readonly';
  GCaption_dpb_set_db_SQL_dialect       = 'Set DB SQL Dialect';
  GCaption_dpb_gfix_attach              = 'gfix attach';
  GCaption_dpb_gstat_attach             = 'gstat attach';

resourcestring
  GCaption_tpb_consistency              = 'Consistency';
  GCaption_tpb_concurrency              = 'Concurrency';
  GCaption_tpb_shared                   = 'Shared';
  GCaption_tpb_protected                = 'Protected';
  GCaption_tpb_exclusive                = 'Exclusive';
  GCaption_tpb_wait                     = 'Wait';
  GCaption_tpb_nowait                   = 'NoWait';
  GCaption_tpb_read                     = 'Read';
  GCaption_tpb_write                    = 'Write';
  GCaption_tpb_lock_read                = 'Lock Read';
  GCaption_tpb_lock_write               = 'Lock Write';
  GCaption_tpb_verb_time                = 'Verb Time';
  GCaption_tpb_commit_time              = 'Commit Time';
  GCaption_tpb_ignore_limbo             = 'Ignore Limbo';
  GCaption_tpb_read_committed           = 'Read Committed';
  GCaption_tpb_autocommit               = 'Autocommit';
  GCaption_tpb_rec_version              = 'Record Version';
  GCaption_tpb_no_rec_version           = 'No Record Version';
  GCaption_tpb_restart_requests         = 'Restart Requests';
  GCaption_tpb_no_auto_undo             = 'No Auto Undo';

resourcestring
  GCaption_bpb_source_type              = 'source type';
  GCaption_bpb_target_type              = 'target type';
  GCaption_bpb_type                     = 'type';
  GCaption_bpb_source_interp            = 'source interp';
  GCaption_bpb_target_interp            = 'target interp';
  GCaption_bpb_filter_parameter         = 'filter parameter';
                                        
resourcestring                          
  GCaption_spb_user_name                = 'user name';
  GCaption_spb_sys_user_name            = 'sys user name';
  GCaption_spb_sys_user_name_enc        = 'sys user name enc';
  GCaption_spb_password                 = 'password';
  GCaption_spb_password_enc             = 'password enc';
  GCaption_spb_command_line             = 'command line';
  GCaption_spb_dbname                   = 'dbname';
  GCaption_spb_verbose                  = 'verbose';
  GCaption_spb_options                  = 'options';
  GCaption_spb_connect_timeout          = 'connect timeout';
  GCaption_spb_dummy_packet_interval    = 'dummy packet interval';
  GCaption_spb_sql_role_name            = 'sql role name';
                                        
resourcestring                          
  GCaption_spb_bkp_file                 = 'backup file name';
  GCaption_spb_bkp_factor               = 'backup factor';
  GCaption_spb_bkp_length               = 'backup length';
                                        
resourcestring                          
  GCaption_info_svc_svr_db_info         = 'server database info';
  GCaption_info_svc_get_license         = 'get license';
  GCaption_info_svc_get_license_mask    = 'get license mask';
  GCaption_info_svc_get_config          = 'get config';
  GCaption_info_svc_version             = 'version';
  GCaption_info_svc_server_version      = 'server version';
  GCaption_info_svc_implementation      = 'implementation';
  GCaption_info_svc_capabilities        = 'capabilities';
  GCaption_info_svc_user_dbpath         = 'user database path';
  GCaption_info_svc_get_env             = 'get environment';
  GCaption_info_svc_get_env_lock        = 'get environment lock';
  GCaption_info_svc_get_env_msg         = 'get environment msg';
  GCaption_info_svc_line                = 'get line';
  GCaption_info_svc_to_eof              = 'get to eof';
  GCaption_info_svc_timeout             = 'timeout';
  GCaption_info_svc_get_licensed_users  = 'get licensed users';      
  GCaption_info_svc_limbo_trans         = 'limbo transactions';
  GCaption_info_svc_running             = 'running';
  GCaption_info_svc_get_users           = 'get users';

resourcestring
  GCaption_spb_lic_key                  = 'License Key';
  GCaption_spb_lic_id                   = 'License Id';
  GCaption_spb_lic_desc                 = 'License Description';

resourcestring
  GCaption_CFG_LOCKMEM_KEY              = 'Lock Memory';
  GCaption_CFG_LOCKSEM_KEY              = 'Lock Semaphores';
  GCaption_CFG_LOCKSIG_KEY              = 'Lock Sig';
  GCaption_CFG_EVNTMEM_KEY              = 'Event Memory';
  GCaption_CFG_DBCACHE_KEY              = 'Database Cache';
  GCaption_CFG_PRIORITY_KEY             = 'Priority';
  GCaption_CFG_IPCMAP_KEY               = 'IPC Map';
  GCaption_CFG_MEMMIN_KEY               = 'Memory Min';
  GCaption_CFG_MEMMAX_KEY               = 'Memory Max';
  GCaption_CFG_LOCKORDER_KEY            = 'Lock Order';
  GCaption_CFG_ANYLOCKMEM_KEY           = 'Any Lock Memory';
  GCaption_CFG_ANYLOCKSEM_KEY           = 'Any Lock Semaphores';
  GCaption_CFG_ANYLOCKSIG_KEY           = 'Any Lock Sig';
  GCaption_CFG_ANYEVNTMEM_KEY           = 'Any Event Memory';
  GCaption_CFG_LOCKHASH_KEY             = 'Lock Hash';
  GCaption_CFG_DEADLOCK_KEY             = 'Deadlock';
  GCaption_CFG_LOCKSPIN_KEY             = 'Lock Spin';
  GCaption_CFG_CONN_TIMEOUT_KEY         = 'Connect Timeout';
  GCaption_CFG_DUMMY_INTRVL_KEY         = 'Dummy Interval';

resourcestring
  GCaption_spb_sec_userid               = 'User Id';
  GCaption_spb_sec_groupid              = 'Group Id';
  GCaption_spb_sec_username             = 'User Name';
  GCaption_spb_sec_password             = 'Password';
  GCaption_spb_sec_groupname            = 'Group Name';
  GCaption_spb_sec_firstname            = 'First Name';
  GCaption_spb_sec_middlename           = 'Middle Name';
  GCaption_spb_sec_lastname             = 'Last Name';

resourcestring
  GCaption_spb_num_att                  = 'Attatch Count';
  GCaption_spb_num_db                   = 'Database Count';

implementation
end.
