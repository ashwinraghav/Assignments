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

unit SilSgFirebirdParameterNames;

interface

{$include Defines.inc}

uses
  SilSiFirebird;

const
  GName_dpb_cdd_pathname            : PChar   =   CDpbCddPathName         ;
  GName_dpb_allocation              : PChar   =   CDpbAllocation          ;
  GName_dpb_journal                 : PChar   =   CDpbJournal             ;
  GName_dpb_page_size               : PChar   =   CDpbPageSize            ;
  GName_dpb_num_buffers             : PChar   =   CDpbNumBuffers          ;
  GName_dpb_buffer_length           : PChar   =   CDpbBufferLength        ;
  GName_dpb_debug                   : PChar   =   CDpbDebug               ;
  GName_dpb_garbage_collect         : PChar   =   CDpbGarbageCollect      ;
  GName_dpb_verify                  : PChar   =   CDpbVerify              ;
  GName_dpb_sweep                   : PChar   =   CDpbSweep               ;
  GName_dpb_enable_journal          : PChar   =   CDpbEnableJournal       ;
  GName_dpb_disable_journal         : PChar   =   CDpbDisableJournal      ;
  GName_dpb_dbkey_scope             : PChar   =   CDpbDbkeyScope          ;
  GName_dpb_number_of_users         : PChar   =   CDpbNumberOfUsers       ;
  GName_dpb_trace                   : PChar   =   CDpbTrace               ;
  GName_dpb_no_garbage_collect      : PChar   =   CDpbNoGarbageCollect    ;
  GName_dpb_damaged                 : PChar   =   CDpbDamaged             ;
  GName_dpb_license                 : PChar   =   CDpbLicense             ;
  GName_dpb_sys_user_name           : PChar   =   CDpbSystemUser          ;
  GName_dpb_encrypt_key             : PChar   =   CDpbEncryptKey          ;
  GName_dpb_activate_shadow         : PChar   =   CDpbActivateShadow      ;
  GName_dpb_sweep_interval          : PChar   =   CDpbSweepInterval       ;
  GName_dpb_delete_shadow           : PChar   =   CDpbDeleteShadow        ;
  GName_dpb_force_write             : PChar   =   CDpbForceWrite          ;
  GName_dpb_begin_log               : PChar   =   CDpbBeginLog            ;
  GName_dpb_quit_log                : PChar   =   CDpbQuitLog             ;
  GName_dpb_no_reserve              : PChar   =   CDpbNoReserve           ;
  GName_dpb_user_name               : PChar   =   CDpbUserName            ;
  GName_dpb_password                : PChar   =   CDpbPassword            ;
  GName_dpb_password_enc            : PChar   =   CDpbPasswordEncrypted   ;
  GName_dpb_sys_user_name_enc       : PChar   =   CDpbSystemUserEncrypted ;
  GName_dpb_interp                  : PChar   =   CDpbInterp              ;
  GName_dpb_online_dump             : PChar   =   CDpbOnlineDump          ;
  GName_dpb_old_file_size           : PChar   =   CDpbOldFileSize         ;
  GName_dpb_old_num_files           : PChar   =   CDpbOldNumFiles         ;
  GName_dpb_old_file                : PChar   =   CDpbOldFile             ;
  GName_dpb_old_start_page          : PChar   =   CDpbOldStartPage        ;
  GName_dpb_old_start_seqno         : PChar   =   CDpbOldStartSeqno       ;
  GName_dpb_old_start_file          : PChar   =   CDpbOldStartFile        ;
  GName_dpb_drop_walfile            : PChar   =   CDpbDropWalFile         ;
  GName_dpb_old_dump_id             : PChar   =   CDpbOldDumpId           ;
  GName_dpb_wal_backup_dir          : PChar   =   CDpbWalBackupDir        ;
  GName_dpb_wal_chkptlen            : PChar   =   CDpbWalChkptLen         ;
  GName_dpb_wal_numbufs             : PChar   =   CDpbWalNumBufs          ;
  GName_dpb_wal_bufsize             : PChar   =   CDpbWalBufSize          ;
  GName_dpb_wal_grp_cmt_wait        : PChar   =   CDpbWalGrpCmtWait       ;
  GName_dpb_lc_messages             : PChar   =   CDpbLcMessages          ;
  GName_dpb_lc_ctype                : PChar   =   CDpbLcCtype             ;
  GName_dpb_cache_manager           : PChar   =   CDpbCacheManager        ;
  GName_dpb_shutdown                : PChar   =   CDpbShutdown            ;
  GName_dpb_online                  : PChar   =   CDpbOnline              ;
  GName_dpb_shutdown_delay          : PChar   =   CDpbShutdownDelay       ;
  GName_dpb_reserved                : PChar   =   CDpbReserved            ;
  GName_dpb_overwrite               : PChar   =   CDpbOverwrite           ;
  GName_dpb_sec_attach              : PChar   =   CDpbSecAttach           ;
  GName_dpb_disable_wal             : PChar   =   CDpbDisableWal          ;
  GName_dpb_connect_timeout         : PChar   =   CDpbConnectTimeout      ;
  GName_dpb_dummy_packet_interval   : PChar   =   CDpbDummyPacketInterval ;
  GName_dpb_gbak_attach             : PChar   =   CDpbGbakAttach          ;
  GName_dpb_sql_role_name           : PChar   =   CDpbSqlRole             ;
  GName_dpb_set_page_buffers        : PChar   =   CDpbSetPageBuffers      ;
  GName_dpb_working_directory       : PChar   =   CDpbWorkingDirectory    ;
  GName_dpb_SQL_dialect             : PChar   =   CDpbSqlDialect          ;
  GName_dpb_set_db_readonly         : PChar   =   CDpbSetDbReadonly       ;
  GName_dpb_set_db_SQL_dialect      : PChar   =   CDpbSetDbSqlDialect     ;
  GName_dpb_gfix_attach             : PChar   =   CDpbGfixAttach          ;
  GName_dpb_gstat_attach            : PChar   =   CDpbGstatAttach         ;

const
  GName_tpb_consistency             : PChar   =   CTpbConsistency         ;               
  GName_tpb_concurrency             : PChar   =   CTpbConcurrency         ;      
  GName_tpb_shared                  : PChar   =   CTpbShared              ;      
  GName_tpb_protected               : PChar   =   CTpbProtected           ;      
  GName_tpb_exclusive               : PChar   =   CTpbExclusive           ;      
  GName_tpb_wait                    : PChar   =   CTpbWait                ;      
  GName_tpb_nowait                  : PChar   =   CTpbNoWait              ;      
  GName_tpb_read                    : PChar   =   CTpbRead                ;      
  GName_tpb_write                   : PChar   =   CTpbWrite               ;      
  GName_tpb_lock_read               : PChar   =   CTpbLockRead            ;      
  GName_tpb_lock_write              : PChar   =   CTpbLockWrite           ;      
  GName_tpb_verb_time               : PChar   =   CTpbVerbTime            ;      
  GName_tpb_commit_time             : PChar   =   CTpbCommitTime          ;      
  GName_tpb_ignore_limbo            : PChar   =   CTpbIgnoreLimbo         ;      
  GName_tpb_read_committed          : PChar   =   CTpbReadCommitted       ;      
  GName_tpb_autocommit              : PChar   =   CTpbAutocommit          ;      
  GName_tpb_rec_version             : PChar   =   CTpbRecVersion          ;      
  GName_tpb_no_rec_version          : PChar   =   CTpbNoRecVersion        ;      
  GName_tpb_restart_requests        : PChar   =   CTpbRestartRequests     ;      
  GName_tpb_no_auto_undo            : PChar   =   CTpbNoAutoUndo          ;      

const
  GName_bpb_source_type             : PChar   =   CBpbSourceType          ;
  GName_bpb_target_type             : PChar   =   CBpbTargetType          ;      
  GName_bpb_type                    : PChar   =   CBpbType                ;      
  GName_bpb_source_interp           : PChar   =   CBpbSourceInterp        ;      
  GName_bpb_target_interp           : PChar   =   CBpbTargetInterp        ;      
  GName_bpb_filter_parameter        : PChar   =   CBpbFilterParameter     ;      

const
  GName_spb_user_name               : PChar   =   CSpbUserName            ;                  
  GName_spb_sys_user_name           : PChar   =   CSpbSysUserName         ;      
  GName_spb_sys_user_name_enc       : PChar   =   CSpbSysUserNameEncrypted;      
  GName_spb_password                : PChar   =   CSpbPassword            ;      
  GName_spb_password_enc            : PChar   =   CSpbPasswordEncrypted   ;      
  GName_spb_command_line            : PChar   =   CSpbCommandLine         ;      
  GName_spb_dbname                  : PChar   =   CSpbDatabaseName        ;      
  GName_spb_verbose                 : PChar   =   CSpbVerbose             ;      
  GName_spb_options                 : PChar   =   CSpbOptions             ;      
  GName_spb_connect_timeout         : PChar   =   CSpbConnectTimeout      ;      
  GName_spb_dummy_packet_interval   : PChar   =   CSpbDummyPacketInterval ;      
  GName_spb_sql_role_name           : PChar   =   CSpbSqlRoleName         ;

const
  GName_spb_bkp_file                : PChar   =   CBkpFileName            ;
  GName_spb_bkp_factor              : PChar   =   CBkpFactor              ;
  GName_spb_bkp_length              : PChar   =   CBkpLength              ;

const
  GName_info_svc_svr_db_info        : PChar   =   CInfoServerDbInfo       ;       
  GName_info_svc_get_license        : PChar   =   CInfoGetLicense         ;      
  GName_info_svc_get_license_mask   : PChar   =   CInfoGetLicenseMask     ;      
  GName_info_svc_get_config         : PChar   =   CInfoGetConfig          ;      
  GName_info_svc_version            : PChar   =   CInfoVersion            ;      
  GName_info_svc_server_version     : PChar   =   CInfoServerVersion      ;      
  GName_info_svc_implementation     : PChar   =   CInfoImplementation     ;      
  GName_info_svc_capabilities       : PChar   =   CInfoCapabilities       ;      
  GName_info_svc_user_dbpath        : PChar   =   CInfoUserDBPath         ;      
  GName_info_svc_get_env            : PChar   =   CInfoGetEnv             ;      
  GName_info_svc_get_env_lock       : PChar   =   CInfoGetEnvLock         ;      
  GName_info_svc_get_env_msg        : PChar   =   CInfoGetEnvMsg          ;      
  GName_info_svc_line               : PChar   =   CInfoLine               ;      
  GName_info_svc_to_eof             : PChar   =   CInfoToEOF              ;      
  GName_info_svc_timeout            : PChar   =   CInfoTimeout            ;
  GName_info_svc_get_licensed_users : PChar   =   CInfoGetLicensedUsers   ;
  GName_info_svc_limbo_trans        : PChar   =   CInfoLimboTrans         ;
  GName_info_svc_running            : PChar   =   CInfoRunning            ;
  GName_info_svc_get_users          : PChar   =   CInfoGetUsers           ;

const
  GName_spb_lic_key                 : PChar   =   CLicKey                 ;      
  GName_spb_lic_id                  : PChar   =   CLicId                  ;      
  GName_spb_lic_desc                : PChar   =   CLicDescription         ;      

const
  GName_CFG_LOCKMEM_KEY             : PChar   =   CCfgLockMemKey          ;      
  GName_CFG_LOCKSEM_KEY             : PChar   =   CCfgLockSemKey          ;      
  GName_CFG_LOCKSIG_KEY             : PChar   =   CCfgLockSigKey          ;      
  GName_CFG_EVNTMEM_KEY             : PChar   =   CCfgEvntMemKey          ;      
  GName_CFG_DBCACHE_KEY             : PChar   =   CCfgDbCacheKey          ;      
  GName_CFG_PRIORITY_KEY            : PChar   =   CCfgPriorityKey         ;      
  GName_CFG_IPCMAP_KEY              : PChar   =   CCfgIpcMapKey           ;      
  GName_CFG_MEMMIN_KEY              : PChar   =   CCfgMemMinKey           ;      
  GName_CFG_MEMMAX_KEY              : PChar   =   CCfgMemMaxKey           ;      
  GName_CFG_LOCKORDER_KEY           : PChar   =   CCfgLockOrderKey        ;      
  GName_CFG_ANYLOCKMEM_KEY          : PChar   =   CCfgAnyLockMemKey       ;      
  GName_CFG_ANYLOCKSEM_KEY          : PChar   =   CCfgAnyLockSemKey       ;      
  GName_CFG_ANYLOCKSIG_KEY          : PChar   =   CCfgAnyLockSigKey       ;      
  GName_CFG_ANYEVNTMEM_KEY          : PChar   =   CCfgAnyEvntMemKey       ;      
  GName_CFG_LOCKHASH_KEY            : PChar   =   CCfgLockHashKey         ;      
  GName_CFG_DEADLOCK_KEY            : PChar   =   CCfgDeadlockKey         ;      
  GName_CFG_LOCKSPIN_KEY            : PChar   =   CCfgLockSpinKey         ;      
  GName_CFG_CONN_TIMEOUT_KEY        : PChar   =   CCfgConnTimeoutKey      ;      
  GName_CFG_DUMMY_INTRVL_KEY        : PChar   =   CCfgDummyIntrvlKey      ;      

const
  GName_spb_sec_userid              : PChar   =   CSecUserId              ;      
  GName_spb_sec_groupid             : PChar   =   CSecGroupId             ;      
  GName_spb_sec_username            : PChar   =   CSecUserName            ;      
  GName_spb_sec_password            : PChar   =   CSecPassword            ;      
  GName_spb_sec_groupname           : PChar   =   CSecGroupName           ;      
  GName_spb_sec_firstname           : PChar   =   CSecFirstName           ;      
  GName_spb_sec_middlename          : PChar   =   CSecMiddleName          ;      
  GName_spb_sec_lastname            : PChar   =   CSecLastName            ;      

const
  GName_spb_num_att                 : PChar   =   CDbiNumAttachments      ;      
  GName_spb_num_db                  : PChar   =   CDbiNumDatabases        ;      

implementation
end.
