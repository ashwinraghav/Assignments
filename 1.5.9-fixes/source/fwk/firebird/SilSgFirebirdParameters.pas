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

unit SilSgFirebirdParameters;

{$INCLUDE Defines.inc}

interface

uses
  SilScFirebirdClient,
  SilSiFirebird,
  SilShFirebird,
  SilSgFirebirdParameterInfos,
  SilSmFirebirdParameterBlock;

type
  TSilFirebirdParameterBlock  = SilSmFirebirdParameterBlock.TSilFirebirdParameterBlock;
  TSilFirebirdServiceRequestBuffer  = SilSmFirebirdParameterBlock.TSilFirebirdServiceRequestBuffer;

const
  GFbDatabaseParameterBlock: array[TFbDatabaseParameter] of PFbParameterInfo  = (
      @GInfo_dpb_cdd_pathname           ,
      @GInfo_dpb_allocation             ,
      @GInfo_dpb_journal                ,
      @GInfo_dpb_page_size              ,
      @GInfo_dpb_num_buffers            ,
      @GInfo_dpb_buffer_length          ,
      @GInfo_dpb_debug                  ,
      @GInfo_dpb_garbage_collect        ,
      @GInfo_dpb_verify                 ,
      @GInfo_dpb_sweep                  ,
      @GInfo_dpb_enable_journal         ,
      @GInfo_dpb_disable_journal        ,
      @GInfo_dpb_dbkey_scope            ,
      @GInfo_dpb_number_of_users        ,
      @GInfo_dpb_trace                  ,
      @GInfo_dpb_no_garbage_collect     ,
      @GInfo_dpb_damaged                ,
      @GInfo_dpb_license                ,
      @GInfo_dpb_sys_user_name          ,
      @GInfo_dpb_encrypt_key            ,
      @GInfo_dpb_activate_shadow        ,
      @GInfo_dpb_sweep_interval         ,
      @GInfo_dpb_delete_shadow          ,
      @GInfo_dpb_force_write            ,
      @GInfo_dpb_begin_log              ,
      @GInfo_dpb_quit_log               ,
      @GInfo_dpb_no_reserve             ,
      @GInfo_dpb_user_name              ,
      @GInfo_dpb_password               ,
      @GInfo_dpb_password_enc           ,
      @GInfo_dpb_sys_user_name_enc      ,
      @GInfo_dpb_interp                 ,
      @GInfo_dpb_online_dump            ,
      @GInfo_dpb_old_file_size          ,
      @GInfo_dpb_old_num_files          ,
      @GInfo_dpb_old_file               ,
      @GInfo_dpb_old_start_page         ,
      @GInfo_dpb_old_start_seqno        ,
      @GInfo_dpb_old_start_file         ,
      @GInfo_dpb_drop_walfile           ,
      @GInfo_dpb_old_dump_id            ,
      @GInfo_dpb_wal_backup_dir         ,
      @GInfo_dpb_wal_chkptlen           ,
      @GInfo_dpb_wal_numbufs            ,
      @GInfo_dpb_wal_bufsize            ,
      @GInfo_dpb_wal_grp_cmt_wait       ,
      @GInfo_dpb_lc_messages            ,
      @GInfo_dpb_lc_ctype               ,
      @GInfo_dpb_cache_manager          ,
      @GInfo_dpb_shutdown               ,
      @GInfo_dpb_online                 ,
      @GInfo_dpb_shutdown_delay         ,
      @GInfo_dpb_reserved               ,
      @GInfo_dpb_overwrite              ,
      @GInfo_dpb_sec_attach             ,
      @GInfo_dpb_disable_wal            ,
      @GInfo_dpb_connect_timeout        ,
      @GInfo_dpb_dummy_packet_interval  ,
      @GInfo_dpb_gbak_attach            ,
      @GInfo_dpb_sql_role_name          ,
      @GInfo_dpb_set_page_buffers       ,
      @GInfo_dpb_working_directory      ,
      @GInfo_dpb_SQL_dialect            ,
      @GInfo_dpb_set_db_readonly        ,
      @GInfo_dpb_set_db_SQL_dialect     ,
      @GInfo_dpb_gfix_attach            ,
      @GInfo_dpb_gstat_attach
    );
                                                        
const
  GFbTransactionParameterBlock: array[TFbTransactionParameter] of PFbParameterInfo  = (
      @GInfo_tpb_consistency            ,
      @GInfo_tpb_concurrency            ,
      @GInfo_tpb_shared                 ,
      @GInfo_tpb_protected              ,
      @GInfo_tpb_exclusive              ,
      @GInfo_tpb_wait                   ,
      @GInfo_tpb_nowait                 ,
      @GInfo_tpb_read                   ,
      @GInfo_tpb_write                  ,
      @GInfo_tpb_lock_read              ,
      @GInfo_tpb_lock_write             ,
      @GInfo_tpb_verb_time              ,
      @GInfo_tpb_commit_time            ,
      @GInfo_tpb_ignore_limbo           ,
      @GInfo_tpb_read_committed         ,
      @GInfo_tpb_autocommit             ,
      @GInfo_tpb_rec_version            ,
      @GInfo_tpb_no_rec_version         ,
      @GInfo_tpb_restart_requests       ,
      @GInfo_tpb_no_auto_undo
    );

const
  GFbServiceParameterBlock: array[TFbServiceParameter] of PFbParameterInfo  = (
      @GInfo_spb_user_name              ,   
      @GInfo_spb_sys_user_name          ,   
      @GInfo_spb_sys_user_name_enc      ,   
      @GInfo_spb_password               ,   
      @GInfo_spb_password_enc           ,   
      @GInfo_spb_connect_timeout        ,   
      @GInfo_spb_dummy_packet_interval  ,   
      @GInfo_spb_sql_role_name          
    );

const
  GFbRequestBufferBlock: array[TFbRequestParameter] of PFbParameterInfo  = (
      @GInfo_spb_command_line           ,
      @GInfo_spb_dbname                 ,
      @GInfo_spb_verbose                ,
      @GInfo_spb_options                ,
      @GInfo_spb_bkp_file               ,
      @GInfo_spb_bkp_factor             ,
      @GInfo_spb_bkp_length
    );

const
  GFbInfoBlock: array[TFbInfoParameter] of PFbParameterInfo  = (
      @GInfo_svc_svr_db_info            ,
      @GInfo_svc_get_license            ,
      @GInfo_svc_get_license_mask       ,
      @GInfo_svc_get_config             ,
      @GInfo_svc_version                ,
      @GInfo_svc_server_version         ,
      @GInfo_svc_implementation         ,
      @GInfo_svc_capabilities           ,
      @GInfo_svc_user_dbpath            ,
      @GInfo_svc_get_env                ,
      @GInfo_svc_get_env_lock           ,
      @GInfo_svc_get_env_msg            ,
      @GInfo_svc_line                   ,
      @GInfo_svc_to_eof                 ,
      @GInfo_svc_timeout                ,
      @GInfo_svc_get_licensed_users     ,
      @GInfo_svc_limbo_trans            ,
      @GInfo_svc_running                ,
      @GInfo_svc_get_users
    );

implementation
end.
