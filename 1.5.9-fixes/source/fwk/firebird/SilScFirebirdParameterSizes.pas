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

unit SilScFirebirdParameterSizes;

interface

const
  CSize_dpb_cdd_pathname              =   0;
  CSize_dpb_allocation                =   0; 
  CSize_dpb_journal                   =   0; 
  CSize_dpb_page_size                 =   0; 
  CSize_dpb_num_buffers               =   SizeOf(Byte); 
  CSize_dpb_buffer_length             =   0; 
  CSize_dpb_debug                     =   0; 
  CSize_dpb_garbage_collect           =   0; 
  CSize_dpb_verify                    =   SizeOf(Byte);
  CSize_dpb_sweep                     =   0; 
  CSize_dpb_enable_journal            =   0; 
  CSize_dpb_disable_journal           =   0; 
  CSize_dpb_dbkey_scope               =   SizeOf(Byte); 
  CSize_dpb_number_of_users           =   0; 
  CSize_dpb_trace                     =   0; 
  CSize_dpb_no_garbage_collect        =   0; 
  CSize_dpb_damaged                   =   SizeOf(Byte); 
  CSize_dpb_license                   =   0;
  CSize_dpb_sys_user_name             =   0;
  CSize_dpb_encrypt_key               =   0;
  CSize_dpb_activate_shadow           =   0;
  CSize_dpb_sweep_interval            =   SizeOf(Integer);
  CSize_dpb_delete_shadow             =   0;
  CSize_dpb_force_write               =   SizeOf(Byte);
  CSize_dpb_begin_log                 =   0;
  CSize_dpb_quit_log                  =   0; 
  CSize_dpb_no_reserve                =   SizeOf(Byte); 
  CSize_dpb_user_name                 =   0; 
  CSize_dpb_password                  =   0; 
  CSize_dpb_password_enc              =   0;
  CSize_dpb_sys_user_name_enc         =   0;
  CSize_dpb_interp                    =   0; 
  CSize_dpb_online_dump               =   0; 
  CSize_dpb_old_file_size             =   0; 
  CSize_dpb_old_num_files             =   0; 
  CSize_dpb_old_file                  =   0; 
  CSize_dpb_old_start_page            =   0; 
  CSize_dpb_old_start_seqno           =   0; 
  CSize_dpb_old_start_file            =   0; 
  CSize_dpb_drop_walfile              =   0; 
  CSize_dpb_old_dump_id               =   0; 
  CSize_dpb_wal_backup_dir            =   0; 
  CSize_dpb_wal_chkptlen              =   0; 
  CSize_dpb_wal_numbufs               =   0; 
  CSize_dpb_wal_bufsize               =   0; 
  CSize_dpb_wal_grp_cmt_wait          =   0; 
  CSize_dpb_lc_messages               =   0;
  CSize_dpb_lc_ctype                  =   0;
  CSize_dpb_cache_manager             =   0; 
  CSize_dpb_shutdown                  =   0; 
  CSize_dpb_online                    =   0; 
  CSize_dpb_shutdown_delay            =   0; 
  CSize_dpb_reserved                  =   0; 
  CSize_dpb_overwrite                 =   0; 
  CSize_dpb_sec_attach                =   0; 
  CSize_dpb_disable_wal               =   0; 
  CSize_dpb_connect_timeout           =   SizeOf(Integer); 
  CSize_dpb_dummy_packet_interval     =   0; 
  CSize_dpb_gbak_attach               =   0; 
  CSize_dpb_sql_role_name             =   0; 
  CSize_dpb_set_page_buffers          =   0; 
  CSize_dpb_working_directory         =   0; 
  CSize_dpb_SQL_dialect               =   0; 
  CSize_dpb_set_db_readonly           =   0; 
  CSize_dpb_set_db_SQL_dialect        =   0; 
  CSize_dpb_gfix_attach               =   0; 
  CSize_dpb_gstat_attach              =   0; 

const
  CSize_tpb_consistency               =   0; 
  CSize_tpb_concurrency               =   0; 
  CSize_tpb_shared                    =   0; 
  CSize_tpb_protected                 =   0; 
  CSize_tpb_exclusive                 =   0; 
  CSize_tpb_wait                      =   0; 
  CSize_tpb_nowait                    =   0; 
  CSize_tpb_read                      =   0; 
  CSize_tpb_write                     =   0; 
  CSize_tpb_lock_read                 =   0; 
  CSize_tpb_lock_write                =   0; 
  CSize_tpb_verb_time                 =   0; 
  CSize_tpb_commit_time               =   0; 
  CSize_tpb_ignore_limbo              =   0; 
  CSize_tpb_read_committed            =   0; 
  CSize_tpb_autocommit                =   0; 
  CSize_tpb_rec_version               =   0; 
  CSize_tpb_no_rec_version            =   0; 
  CSize_tpb_restart_requests          =   0; 
  CSize_tpb_no_auto_undo              =   0; 

const
  CSize_bpb_source_type               =   0; 
  CSize_bpb_target_type               =   0; 
  CSize_bpb_type                      =   0; 
  CSize_bpb_source_interp             =   0; 
  CSize_bpb_target_interp             =   0; 
  CSize_bpb_filter_parameter          =   0; 

const
  CSize_spb_user_name                 =   0; 
  CSize_spb_sys_user_name             =   0; 
  CSize_spb_sys_user_name_enc         =   0; 
  CSize_spb_password                  =   0; 
  CSize_spb_password_enc              =   0; 
  CSize_spb_command_line              =   0; 
  CSize_spb_dbname                    =   0; 
  CSize_spb_verbose                   =   0; 
  CSize_spb_options                   =   0; 
  CSize_spb_connect_timeout           =   0; 
  CSize_spb_dummy_packet_interval     =   0; 
  CSize_spb_sql_role_name             =   0;

const
  CSize_spb_bkp_file                  =   0; 
  CSize_spb_bkp_factor                =   0; 
  CSize_spb_bkp_length                =   0; 

implementation
end.
 