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

unit SilScFirebirdParameterValues;

interface

const
  CNullStr  = '';

const
  CValue_dpb_cdd_pathname              =   CNullStr;
//CValue_dpb_allocation                =   varEmpty;
//CValue_dpb_journal                   =   varEmpty;
//CValue_dpb_page_size                 =   varEmpty;
  CValue_dpb_num_buffers               =   75;
//CValue_dpb_buffer_length             =   varEmpty;
//CValue_dpb_debug                     =   varEmpty;
//CValue_dpb_garbage_collect           =   varEmpty;
  CValue_dpb_verify                    =   0;
//CValue_dpb_sweep                     =   varEmpty;
//CValue_dpb_enable_journal            =   varEmpty;
//CValue_dpb_disable_journal           =   varEmpty;
  CValue_dpb_dbkey_scope               =   1;
//CValue_dpb_number_of_users           =   varEmpty;
//CValue_dpb_trace                     =   varEmpty;
//CValue_dpb_no_garbage_collect        =   varEmpty;
  CValue_dpb_damaged                   =   0;
  CValue_dpb_license                   =   CNullStr;
  CValue_dpb_sys_user_name             =   'SYSDBA';
  CValue_dpb_encrypt_key               =   CNullStr;
  CValue_dpb_activate_shadow           =   False;
  CValue_dpb_sweep_interval            =   20000;
  CValue_dpb_delete_shadow             =   False;
  CValue_dpb_force_write               =   0;
  CValue_dpb_begin_log                 =   False;
  CValue_dpb_quit_log                  =   False;
  CValue_dpb_no_reserve                =   0;
  CValue_dpb_user_name                 =   CNullStr;
  CValue_dpb_password                  =   CNullStr;
  CValue_dpb_password_enc              =   CNullStr;
  CValue_dpb_sys_user_name_enc         =   CNullStr;
//CValue_dpb_interp                    =   varEmpty;
//CValue_dpb_online_dump               =   varEmpty;
//CValue_dpb_old_file_size             =   varEmpty;
//CValue_dpb_old_num_files             =   varEmpty;
//CValue_dpb_old_file                  =   varEmpty;
//CValue_dpb_old_start_page            =   varEmpty;
//CValue_dpb_old_start_seqno           =   varEmpty;
//CValue_dpb_old_start_file            =   varEmpty;
//CValue_dpb_drop_walfile              =   varEmpty;
//CValue_dpb_old_dump_id               =   varEmpty;
//CValue_dpb_wal_backup_dir            =   varEmpty;
//CValue_dpb_wal_chkptlen              =   varEmpty;
//CValue_dpb_wal_numbufs               =   varEmpty;
//CValue_dpb_wal_bufsize               =   varEmpty;
//CValue_dpb_wal_grp_cmt_wait          =   varEmpty;
  CValue_dpb_lc_messages               =   CNullStr;
  CValue_dpb_lc_ctype                  =   CNullStr;
//CValue_dpb_cache_manager             =   varEmpty;
//CValue_dpb_shutdown                  =   varEmpty;
//CValue_dpb_online                    =   varEmpty;
//CValue_dpb_shutdown_delay            =   varEmpty;
//CValue_dpb_reserved                  =   varEmpty;
//CValue_dpb_overwrite                 =   varEmpty;
//CValue_dpb_sec_attach                =   varEmpty;
//CValue_dpb_disable_wal               =   varEmpty;
  CValue_dpb_connect_timeout           =   30;
//CValue_dpb_dummy_packet_interval     =   varEmpty;
//CValue_dpb_gbak_attach               =   varEmpty;
  CValue_dpb_sql_role_name             =   CNullStr;
//CValue_dpb_set_page_buffers          =   varEmpty;
//CValue_dpb_working_directory         =   varEmpty;
//CValue_dpb_SQL_dialect               =   3;
//CValue_dpb_set_db_readonly           =   varEmpty;
//CValue_dpb_set_db_SQL_dialect        =   varEmpty;
//CValue_dpb_gfix_attach               =   varEmpty;
//CValue_dpb_gstat_attach              =   varEmpty;

//const
//CValue_tpb_consistency               =   varEmpty;
//CValue_tpb_concurrency               =   varEmpty;
//CValue_tpb_shared                    =   varEmpty;
//CValue_tpb_protected                 =   varEmpty;
//CValue_tpb_exclusive                 =   varEmpty;
//CValue_tpb_wait                      =   varEmpty;
//CValue_tpb_nowait                    =   varEmpty;
//CValue_tpb_read                      =   varEmpty;
//CValue_tpb_write                     =   varEmpty;
//CValue_tpb_lock_read                 =   varEmpty;
//CValue_tpb_lock_write                =   varEmpty;
//CValue_tpb_verb_time                 =   varEmpty;
//CValue_tpb_commit_time               =   varEmpty;
//CValue_tpb_ignore_limbo              =   varEmpty;
//CValue_tpb_read_committed            =   varEmpty;
//CValue_tpb_autocommit                =   varEmpty;
//CValue_tpb_rec_version               =   varEmpty;
//CValue_tpb_no_rec_version            =   varEmpty;
//CValue_tpb_restart_requests          =   varEmpty;
//CValue_tpb_no_auto_undo              =   varEmpty;

//const
//CValue_bpb_source_type               =   varEmpty;
//CValue_bpb_target_type               =   varEmpty;
//CValue_bpb_type                      =   varEmpty;
//CValue_bpb_source_interp             =   varEmpty;
//CValue_bpb_target_interp             =   varEmpty;
//CValue_bpb_filter_parameter          =   varEmpty;

//const
//CValue_spb_user_name                 =   varEmpty;
//CValue_spb_sys_user_name             =   varEmpty;
//CValue_spb_sys_user_name_enc         =   varEmpty;
//CValue_spb_password                  =   varEmpty;
//CValue_spb_password_enc              =   varEmpty;
//CValue_spb_command_line              =   varEmpty;
//CValue_spb_dbname                    =   varEmpty;
//CValue_spb_verbose                   =   varEmpty;
//CValue_spb_options                   =   varEmpty;
//CValue_spb_connect_timeout           =   varEmpty;
//CValue_spb_dummy_packet_interval     =   varEmpty;
//CValue_spb_sql_role_name             =   varEmpty;

//const
//CValue_spb_bkp_file                  =   varEmpty;
//CValue_spb_bkp_factor                =   varEmpty;
//CValue_spb_bkp_length                =   varEmpty;

//const
//  isc_spb_lic_key                 =          5;
//  isc_spb_lic_id                  =          6;
//  isc_spb_lic_desc                =          7;

//const
//  ISCCFG_LOCKMEM_KEY             =          0;
//  ISCCFG_LOCKSEM_KEY             =          1;
//  ISCCFG_LOCKSIG_KEY             =          2;
//  ISCCFG_EVNTMEM_KEY             =          3;
//  ISCCFG_DBCACHE_KEY             =          4;
//  ISCCFG_PRIORITY_KEY            =          5;
//  ISCCFG_IPCMAP_KEY              =          6;
//  ISCCFG_MEMMIN_KEY              =          7;
//  ISCCFG_MEMMAX_KEY              =          8;
//  ISCCFG_LOCKORDER_KEY           =          9;
//  ISCCFG_ANYLOCKMEM_KEY          =         10;
//  ISCCFG_ANYLOCKSEM_KEY          =         11;
//  ISCCFG_ANYLOCKSIG_KEY          =         12;
//  ISCCFG_ANYEVNTMEM_KEY          =         13;
//  ISCCFG_LOCKHASH_KEY            =         14;
//  ISCCFG_DEADLOCK_KEY            =         15;
//  ISCCFG_LOCKSPIN_KEY            =         16;
//  ISCCFG_CONN_TIMEOUT_KEY        =         17;
//  ISCCFG_DUMMY_INTRVL_KEY        =         18;

//const
//  isc_spb_sec_userid              =          5;
//  isc_spb_sec_groupid             =          6;
//  isc_spb_sec_username            =          7;
//  isc_spb_sec_password            =          8;
//  isc_spb_sec_groupname           =          9;
//  isc_spb_sec_firstname           =         10;
//  isc_spb_sec_middlename          =         11;
//  isc_spb_sec_lastname            =         12;

//const
//  isc_spb_num_att                 =          5;
//  isc_spb_num_db                  =          6;

implementation
end.
