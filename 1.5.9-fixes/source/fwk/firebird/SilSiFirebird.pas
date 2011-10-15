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

unit SilSiFirebird;

{$INCLUDE Defines.inc}

interface

uses
  Sil, SilContainer, SilLhDataType, 
  SilScFirebirdClient,
  SilSeFirebirdClient,
  SilSiFirebirdClient;

type
  ESilFirebird = class(Exception);

const
  CFbApplication                = '{D874939A-0C10-4C32-B209-7D43B5883FFD}';
  CFbServiceManager             = '{1E3B4E1D-8EB9-4D7E-8573-05BE5627C95C}';
  CFbServiceBackupParameters    = '{2B32BEF5-3E75-49C2-962F-06EDC548E793}';
  CFbHandle                     = '{83D4F970-E290-49B2-BD13-9AACF8EA8396}';
  CFbHandled                    = '{1EB57522-CB96-4ABB-9000-57AA75211E14}';
  CFbParameters                 = '{CFE5BC1A-D119-4450-988F-5C3AB2BE7D70}';
  CFbParameterFields            = '{BCF1CE4F-BCA1-45A0-A6AE-C8E13C57B960}';
  CFbSession                    = '{76E2379A-EFFC-472D-9E70-3DC85F2A34C2}';
  CFbTransactions               = '{386CDD6B-CB85-49FC-98AE-97BF573B76F1}';
  CFbSessionOptions             = '{82A2598D-E306-454F-AD68-3DF468AC840D}';
  CFbTransactionOptions         = '{94F66D12-5AEB-461D-AE99-3C7EF5D77EC7}';
  CFbTransactionOptionsDef      = '{05995683-A915-4967-B48E-3EE6A36D522C}';
  CFbDatabase                   = '{AA9BFF4C-C57D-4C99-B643-1F433E8A3D3E}';
  CFbSchema                     = '{A7461939-A08C-41DC-A4D5-9B73BD9839C3}';
  CFbTransaction                = '{95DA6D6E-754C-451D-B1C7-ED66D84B9910}';
  CFbStatement                  = '{83BA7EF3-E1CD-4918-91CA-9E62EED8EBCB}';
  CFbCommand                    = '{9F9632E8-927E-4668-BBE3-1B4560D88ECD}';
  CFbDomain                     = '{4A477661-E88A-4BD0-9919-0F9520ACF2CF}';
  CFbVariables                  = '{7C5681FE-F419-445A-976A-3E5517DA79BE}';
  CFbVariable                   = '{7D9AD10B-86AE-4B7E-8416-7526910434BF}';
  CFbField                      = '{B1A1874E-F3C9-4872-B124-970F2D9EE1F5}';
  CFbBindings                   = '{A5789F07-EF00-4E12-935B-FBB79559E119}';
  CFbBinding                    = '{0C722CC2-18F2-48F2-A5E4-C031ED1A9A19}';
  CFbBuffer                     = '{F75FC510-910F-4A43-AAFF-4706B4864AFB}';
  CFbData                       = '{D975EA71-053F-415B-931F-52F81384A755}';
  CFbCursor                     = '{ED350F27-B223-453C-827C-53F52D458E1B}';
  CFbSchemaObject               = '{BB28459C-E7D1-4421-B057-8D68383AAA18}';
  CFbSchemaDomains              = '{0B875ABD-EC89-4C37-98FA-2EE127C74E23}';
  CFbSchemaDomain               = '{8476A6C5-6D65-4574-AAB9-ABE6EF4C0BAE}';
  CFbSchemaRelations            = '{3CE17B9F-1F11-4CFF-9D04-952F5C1CF6E0}';
  CFbSchemaRelation             = '{01C5E88B-15F7-4E34-9F27-23A70DF86253}';
  CFbSchemaTables               = '{ED099295-104A-4B6A-974C-FD7A82DC59BF}';
  CFbSchemaTable                = '{4376EFAC-B238-4015-882A-5861017A2FAA}';
  CFbSchemaViews                = '{E6BCED1D-FA05-423A-9E2C-615EB9C000A2}';
  CFbSchemaView                 = '{43E62300-0542-4B3A-A097-937B2A73472F}';
  CFbSchemaProcedures           = '{E4E17542-5F16-4EC9-895B-19512E77F965}';
  CFbSchemaProcedure            = '{36174DEC-4A7C-4CA3-87EC-F7F4CEE50717}';
  CFbSchemaProcedureParameters  = '{40363A0D-6DEE-4105-90DA-F3FD0489AD80}';
  CFbSchemaProcedureParameter   = '{05CF2BEF-A196-4E56-BD03-5BD372B0FEAF}';
  CFbSchemaTriggers             = '{7A07E6C8-BCF2-48FA-B5DC-281A64649E27}';
  CFbSchemaFunctions            = '{77B62A29-005D-4FC2-AA41-C798705928AB}';
  CFbSchemaFields               = '{2B14F14D-063A-4102-900E-2E701F2C3E87}';
  CFbSchemaGenerators           = '{9324B075-DBBB-48D4-8A5A-BC4312E8CBD3}';
  CFbSchemaIndexes              = '{3106EECD-9605-48AB-B310-32385738C47E}';
  CFbSchemaRoles                = '{C6292BE5-3BBB-462E-8C0D-40B40B15EBB4}';
  CFbSchemaDatabase             = '{D07C6F1E-FB3B-4ADA-AA6D-3C42A3BCCEEC}';
  CFbSchemaCollations           = '{98130199-AD26-41A6-92C3-F0C8AFFF3CFF}';
  CFbSchemaCharsets             = '{2FD09D21-B418-4006-971E-91DB0EF28C48}';

const
  GFbBackupService: TGUID = '{791B3A2F-E647-4EDE-B9FA-B6A25B3E11EA}';
  GFbRestoreService: TGUID = '{C8C4C493-4FD1-4FB8-8EFE-A5B9EA4D9521}';

const
  CDpbCddPathName               = 'cdd_pathname'              ;
  CDpbAllocation                = 'allocation'                ;
  CDpbJournal                   = 'journal'                   ;
  CDpbPageSize                  = 'page_size'                 ;
  CDpbNumBuffers                = 'num_buffers'               ;
  CDpbBufferLength              = 'buffer_length'             ;
  CDpbDebug                     = 'debug'                     ;
  CDpbGarbageCollect            = 'garbage_collect'           ;
  CDpbVerify                    = 'verify'                    ;
  CDpbSweep                     = 'sweep'                     ;
  CDpbEnableJournal             = 'enable_journal'            ;
  CDpbDisableJournal            = 'disable_journal'           ;
  CDpbDbkeyScope                = 'dbkey_scope'               ;
  CDpbNumberOfUsers             = 'number_of_users'           ;
  CDpbTrace                     = 'trace'                     ;
  CDpbNoGarbageCollect          = 'no_garbage_collect'        ;
  CDpbDamaged                   = 'damaged'                   ;
  CDpbLicense                   = 'license'                   ;
  CDpbSystemUser                = 'sys_user_name'             ;
  CDpbEncryptKey                = 'encrypt_key'               ;
  CDpbActivateShadow            = 'activate_shadow'           ;
  CDpbSweepInterval             = 'sweep_interval'            ;
  CDpbDeleteShadow              = 'delete_shadow'             ;
  CDpbForceWrite                = 'force_write'               ;
  CDpbBeginLog                  = 'begin_log'                 ;
  CDpbQuitLog                   = 'quit_log'                  ;
  CDpbNoReserve                 = 'no_reserve'                ;
  CDpbUserName                  = 'user_name'                 ;
  CDpbPassword                  = 'password'                  ;
  CDpbPasswordEncrypted         = 'password_enc'              ;
  CDpbSystemUserEncrypted       = 'sys_user_name_enc'         ;
  CDpbInterp                    = 'interp'                    ;
  CDpbOnlineDump                = 'online_dump'               ;
  CDpbOldFileSize               = 'old_file_size'             ;
  CDpbOldNumFiles               = 'old_num_files'             ;
  CDpbOldFile                   = 'old_file'                  ;
  CDpbOldStartPage              = 'old_start_page'            ;
  CDpbOldStartSeqno             = 'old_start_seqno'           ;
  CDpbOldStartFile              = 'old_start_file'            ;
  CDpbDropWalFile               = 'drop_walfile'              ;
  CDpbOldDumpId                 = 'old_dump_id'               ;
  CDpbWalBackupDir              = 'wal_backup_dir'            ;
  CDpbWalChkptLen               = 'wal_chkptlen'              ;
  CDpbWalNumBufs                = 'wal_numbufs'               ;
  CDpbWalBufSize                = 'wal_bufsize'               ;
  CDpbWalGrpCmtWait             = 'wal_grp_cmt_wait'          ;
  CDpbLcMessages                = 'lc_messages'               ;
  CDpbLcCtype                   = 'lc_ctype'                  ;
  CDpbCacheManager              = 'cache_manager'             ;
  CDpbShutdown                  = 'shutdown'                  ;
  CDpbOnline                    = 'online'                    ;
  CDpbShutdownDelay             = 'shutdown_delay'            ;
  CDpbReserved                  = 'reserved'                  ;
  CDpbOverwrite                 = 'overwrite'                 ;
  CDpbSecAttach                 = 'sec_attach'                ;
  CDpbDisableWal                = 'disable_wal'               ;
  CDpbConnectTimeout            = 'connect_timeout'           ;
  CDpbDummyPacketInterval       = 'dummy_packet_interval'     ;
  CDpbGbakAttach                = 'gbak_attach'               ;
  CDpbSqlRole                   = 'sql_role_name'             ;
  CDpbSetPageBuffers            = 'set_page_buffers'          ;
  CDpbWorkingDirectory          = 'working_directory'         ;
  CDpbSqlDialect                = 'sql_dialect'               ;
  CDpbSetDbReadonly             = 'set_db_readonly'           ;
  CDpbSetDbSqlDialect           = 'set_db_sql_dialect'        ;
  CDpbGfixAttach                = 'gfix_attach'               ;
  CDpbGstatAttach               = 'gstat_attach'              ;

const
  CTpbConsistency               = 'consistency'               ;
  CTpbConcurrency               = 'concurrency'               ;
  CTpbShared                    = 'shared'                    ;
  CTpbProtected                 = 'protected'                 ;
  CTpbExclusive                 = 'exclusive'                 ;
  CTpbWait                      = 'wait'                      ;
  CTpbNoWait                    = 'nowait'                    ;
  CTpbRead                      = 'read'                      ;
  CTpbWrite                     = 'write'                     ;
  CTpbLockRead                  = 'lock_read'                 ;
  CTpbLockWrite                 = 'lock_write'                ;
  CTpbVerbTime                  = 'verb_time'                 ;
  CTpbCommitTime                = 'commit_time'               ;
  CTpbIgnoreLimbo               = 'ignore_limbo'              ;
  CTpbReadCommitted             = 'read_committed'            ;
  CTpbAutocommit                = 'autocommit'                ;
  CTpbRecVersion                = 'rec_version'               ;
  CTpbNoRecVersion              = 'no_rec_version'            ;
  CTpbRestartRequests           = 'restart_requests'          ;
  CTpbNoAutoUndo                = 'no_auto_undo'              ;

const
  CBpbSourceType                = 'source_type'               ;
  CBpbTargetType                = 'target_type'               ;
  CBpbType                      = 'type'                      ;
  CBpbSourceInterp              = 'source_interp'             ;
  CBpbTargetInterp              = 'target_interp'             ;
  CBpbFilterParameter           = 'filter_parameter'          ;

const
  CSpbUserName                  = 'user_name'                 ;
  CSpbSysUserName               = 'sys_user_name'             ;
  CSpbSysUserNameEncrypted      = 'sys_user_name_enc'         ;
  CSpbPassword                  = 'password'                  ;
  CSpbPasswordEncrypted         = 'password_enc'              ;
  CSpbCommandLine               = 'command_line'              ;
  CSpbDatabaseName              = 'dbname'                    ;
  CSpbVerbose                   = 'verbose'                   ;
  CSpbOptions                   = 'options'                   ;
  CSpbConnectTimeout            = 'connect_timeout'           ;
  CSpbDummyPacketInterval       = 'dummy_packet_interval'     ;
  CSpbSqlRoleName               = 'sql_role_name'             ;

const
  CBkpFileName                  = 'file'                      ;
  CBkpLength                    = 'length'                    ;
  CBkpFactor                    = 'factor'                    ;

const
  CBkpIgnoreChecksums           = 'ignore_checksums'          ;
  CBkpIgnoreLimbo               = 'ignore_limbo'              ;
  CBkpMetadataOnly              = 'metadata_only'             ;
  CBkpNoGarbageCollect          = 'no_garbage_collect'        ;
  CBkpOldDescriptions           = 'old_descriptions'          ;
  CBkpNonTransportable          = 'non_transportable'         ;
  CBkpConvert                   = 'convert'                   ;
  CBkpExpand                    = 'expand'                    ;

const
  CRprCommitTrans               = 'commit_trans'              ;
  CRprRollbackTrans             = 'rollback_trans'            ;
  CRprRecoverTwoPhase           = 'recover_two_phase'         ;

const
  CTrnId                        = 'tra_id'                    ;
  CTrnSingleTransId             = 'single_tra_id'             ;
  CTrnMultiTransId              = 'multi_tra_id'              ;
  CTrnState                     = 'tra_state'                 ;
  CTrnStateLimbo                = 'tra_state_limbo'           ;
  CTrnStateCommit               = 'tra_state_commit'          ;
  CTrnStateRollback             = 'tra_state_rollback'        ;
  CTrnStateUnknown              = 'tra_state_unknown'         ;
  CTrnHostSite                  = 'tra_host_site'             ;
  CTrnRemoteSite                = 'tra_remote_site'           ;
  CTrnDbPath                    = 'tra_db_path'               ;
  CTrnAdvise                    = 'tra_advise'                ;
  CTrnAdviseCommit              = 'tra_advise_commit'         ;
  CTrnAdviseRollback            = 'tra_advise_rollback'       ;
  CTrnAdviseUnknown             = 'tra_advise_unknown'        ;

const
  CInfoServerDbInfo             = 'svr_db_info'               ;
  CInfoGetLicense               = 'get_license'               ;
  CInfoGetLicenseMask           = 'get_license_mask'          ;
  CInfoGetConfig                = 'get_config'                ;
  CInfoVersion                  = 'version'                   ;
  CInfoServerVersion            = 'server_version'            ;
  CInfoImplementation           = 'implementation'            ;
  CInfoCapabilities             = 'capabilities'              ;
  CInfoUserDBPath               = 'user_dbpath'               ;
  CInfoGetEnv                   = 'get_env'                   ;
  CInfoGetEnvLock               = 'get_env_lock'              ;
  CInfoGetEnvMsg                = 'get_env_msg'               ;
  CInfoLine                     = 'get_line'                  ;
  CInfoToEOF                    = 'get_to_eof'                ;
  CInfoTimeout                  = 'timeout'                   ;
  CInfoGetLicensedUsers         = 'get_licensed_users'        ;
  CInfoLimboTrans               = 'limbo_trans'               ;
  CInfoRunning                  = 'running'                   ;
  CInfoGetUsers                 = 'get_users'                 ;

const
  CLicKey                       = 'lic_key'                   ;
  CLicId                        = 'lic_id'                    ;
  CLicDescription               = 'lic_desc'                  ;

const
  CCfgLockMemKey                = 'LOCKMEM_KEY'               ;
  CCfgLockSemKey                = 'LOCKSEM_KEY'               ;
  CCfgLockSigKey                = 'LOCKSIG_KEY'               ;
  CCfgEvntMemKey                = 'EVNTMEM_KEY'               ;
  CCfgDbCacheKey                = 'DBCACHE_KEY'               ;
  CCfgPriorityKey               = 'PRIORITY_KEY'              ;
  CCfgIpcMapKey                 = 'IPCMAP_KEY'                ;
  CCfgMemMinKey                 = 'MEMMIN_KEY'                ;
  CCfgMemMaxKey                 = 'MEMMAX_KEY'                ;
  CCfgLockOrderKey              = 'LOCKORDER_KEY'             ;
  CCfgAnyLockMemKey             = 'ANYLOCKMEM_KEY'            ;
  CCfgAnyLockSemKey             = 'ANYLOCKSEM_KEY'            ;
  CCfgAnyLockSigKey             = 'ANYLOCKSIG_KEY'            ;
  CCfgAnyEvntMemKey             = 'ANYEVNTMEM_KEY'            ;
  CCfgLockHashKey               = 'LOCKHASH_KEY'              ;
  CCfgDeadlockKey               = 'DEADLOCK_KEY'              ;
  CCfgLockSpinKey               = 'LOCKSPIN_KEY'              ;
  CCfgConnTimeoutKey            = 'CONN_TIMEOUT_KEY'          ;
  CCfgDummyIntrvlKey            = 'DUMMY_INTRVL_KEY'          ;

const
  CSecUserId                    = 'userid'                    ;    
  CSecGroupId                   = 'groupid'                   ;
  CSecUserName                  = 'username'                  ;
  CSecPassword                  = 'password'                  ;
  CSecGroupName                 = 'groupname'                 ;
  CSecFirstName                 = 'firstname'                 ;
  CSecMiddleName                = 'middlename'                ;
  CSecLastName                  = 'lastname'                  ;

const
  CDbiNumAttachments            = 'num_att'                   ;
  CDbiNumDatabases              = 'num_db'                    ;

{$IFDEF USE_FORCEDENUM}
type
  {$MINENUMSIZE 2}
  TFbType = (
      fbbtUndefined  = 0, 
      fbbtVarying    = SQL_VARYING    div 2,
      fbbtText       = SQL_TEXT       div 2,
      fbbtDouble     = SQL_DOUBLE     div 2,
      fbbtFloat      = SQL_FLOAT      div 2,
      fbbtLong       = SQL_LONG       div 2,
      fbbtShort      = SQL_SHORT      div 2,
      fbbtTimestamp  = SQL_TIMESTAMP  div 2,
      fbbtBlob       = SQL_BLOB       div 2,
      fbbtDFloat     = SQL_D_FLOAT    div 2,
      fbbtArray      = SQL_ARRAY      div 2,
      fbbtQuad       = SQL_QUAD       div 2,
      fbbtTypeTime   = SQL_TYPE_TIME  div 2,
      fbbtTypeDate   = SQL_TYPE_DATE  div 2,
      fbbtLarge      = SQL_INT64      div 2,
      fbbtDate       = SQL_DATE       div 2
    );
{$ELSE}
{$MESSAGE Fatal 'Lo siento, esta unit solo compila en Delphi 7.0 o posterior'}
{$ENDIF}

type
  {$MINENUMSIZE 2}
  TFbBlobType = (
      fbbtBlobUntyped       =   isc_blob_untyped       ,
      fbbtBlobText          =   isc_blob_text          ,
      fbbtBlobBLR           =   isc_blob_blr           ,
      fbbtBlobACL           =   isc_blob_acl           ,
      fbbtBlobRanges        =   isc_blob_ranges        ,
      fbbtBlobSummary       =   isc_blob_summary       ,
      fbbtBlobFormat        =   isc_blob_format        ,
      fbbtBlobTRA           =   isc_blob_tra           ,
      fbbtBlobExtfile       =   isc_blob_extfile       ,
      fbbtBlobFormattedMemo =   isc_blob_formatted_memo,
      fbbtBlobParadoxOle    =   isc_blob_paradox_ole   ,
      fbbtBlobGraphic       =   isc_blob_graphic       ,
      fbbtBlobDbaseOle      =   isc_blob_dbase_ole     ,
      fbbtBlobTypedBinary   =   isc_blob_typed_binary
    );

type
  {$MINENUMSIZE 1}
  TFbDataType = (
      fbdtUndefined  , 
      fbdtVarying    ,
      fbdtText       ,
      fbdtDouble     ,
      fbdtFloat      ,
      fbdtLong       ,
      fbdtShort      ,
      fbdtTimestamp  ,
      fbdtBlob       ,
      fbdtDFloat     ,
      fbdtArray      ,
      fbdtQuad       ,
      fbdtTypeTime   ,
      fbdtTypeDate   ,
      fbdtLarge
    );

type
  PFbBuffer = ^IFbBuffer;
  
  IFbApplication = interface;
  IFbServiceManager = interface;
  IFbParameters = interface;
  IFbSession = interface;
  IFbTransactions = interface;
  IFbSessionOptions = interface;
  IFbTransactionOptions = interface;
  IFbTransactionOptionsDef = interface;
  IFbDatabase = interface;
  IFbSchema = interface;
  IFbSchemaDomains = interface;
  IFbSchemaDomain = interface;
  IFbSchemaRelations = interface;
  IFbSchemaRelation = interface;
  IFbSchemaTables = interface;
  IFbSchemaTable = interface;
  IFbSchemaViews = interface;
  IFbSchemaView = interface;
  IFbSchemaProcedures = interface;
  IFbSchemaProcedure = interface;
  IFbSchemaProcedureParameters = interface;
  IFbSchemaProcedureParameter = interface;
  IFbSchemaTriggers = interface;
  IFbSchemaFunctions = interface;
  IFbSchemaFields = interface;
  IFbSchemaGenerators = interface;
  IFbSchemaIndexes = interface;
  IFbSchemaRoles = interface;
  IFbSchemaDatabase = interface;
  IFbSchemaCollations = interface;
  IFbSchemaCharsets = interface;
  IFbTransaction = interface;
  IFbStatement = interface;
  IFbCommand = interface;
  IFbDomain = interface;
  IFbVariables = interface;
  IFbVariable = interface;
  IFbBuffer = interface;
  IFbBindings = interface;
  IFbBinding = interface;
  IFbCursor = interface;
  IFbData = interface;

  IFbApplication = interface
    [CFbApplication]
    function GetProperties: IFbParameters;
    function Connect(const Database: string; const Parameters: IParameterList = nil): IFbSession; overload;
    function Connect(const HostName, Database: string; const Parameters: IParameterList = nil): IFbSession; overload; overload;
    function ServiceManager(const HostName: string; const Parameters: IParameterList = nil): IFbServiceManager;
    property Properties: IFbParameters read GetProperties;
  end;


  IFbServiceBackupParameters = interface (IDispatch)
    [CFbServiceBackupParameters]
  end;

  IFbServiceManager = interface
    [CFbServiceManager]
    procedure Query(const Parameters: IParameterList; Timeout: LongWord = INFINITE); overload; 
    function Query(const Items: array of string; const Parameters: IParameterList; Timeout: LongWord = INFINITE): IParameterList; overload; 
    function Query(const Items: array of string; Timeout: LongWord = INFINITE): IParameterList; overload; 
    procedure Backup(const DatabaseName: string; const Parameters: IParameterList = nil);
    procedure Restore(const DatabaseName: string; const Parameters: IParameterList = nil);
  end;

  TFbTransactionModel = (
      fbtmUnespecified,
      fbtmConcurrency,
      fbtmCommittedNoVersion,
      fbtmCommittedVersion,
      fbtmConsistency
    );
    
  TFbTransactionAccess = (
      fbtaUnespecified,
      fbtaReadOnly,
      fbtaReadWrite
    );

  TFbTransactionResolution = (
      fbtrUnespecified,
      fbtrWait,
      fbtrNoWait
    );

  TFbLockDirective = (
      fbldUnespecified,
      fbldShared,
      fbldProtected
    );

  TFbLockOption = (
      fbloUnespecified,
      fbloRead,
      fbloWrite
    );

  IFbHandle = interface
    [CFbHandle]
    function GetValue: Pointer;
    function GetIsAssigned: Boolean;
    procedure Close;
    property Value: Pointer read GetValue;
    property IsAssigned: Boolean read GetIsAssigned;
  end;

  IFbHandled = interface
    [CFbHandled]
    function GetHandle: IFbHandle;
    property Handle: IFbHandle read GetHandle;
  end;

  TFbParameterField = (
      fbfdCode,
      fbfdName
    );

  TFbParameterKind = (
      fbpkDatabaseParameters,
      fbpkTransactionParameters,    
      fbpkServiceParameters,
      fbpkRequestBuffer,
      fbpkReturnBuffer
    );

  TFbParameterFlag = (
      fbpfConnect,
      fbpfCreate,
      fbpfDefault,
      fbpfRequired
    );

  TFbParameterFlags = set of TFbParameterFlag;

  PFbParameterInfo = ^RFbParameterInfo;
  PFbParameterArray = ^TFbParameterArray;
  PFbParameterList = ^TFbParameterList;

  TFbParameterList = record
    Count: Integer;
    Info: PFbParameterArray;
  end;

  RFbParameterInfo = record
    Code: Integer;
    Name: PPChar;
    Caption: PResStringRec;
    VDefault: PVariant;
    VType: Word;
    Children: TFbParameterList;
    //Size: LongWord;
    //Kind: TFbParameterKind;
    Flags: TFbParameterFlags;
  end;

  TFbParameterArray = array[0 .. High(Integer) div SizeOf(PFbParameterInfo) - 1] of PFbParameterInfo;

  IFbParameters = interface
    [CFbParameters]
    function GetCount: Integer;
    function GetItem(Index: Integer): PFbParameterInfo;
    function Find(const Name: string; out Item: PFbParameterInfo): Boolean; overload; 
    function Find(Code: Integer; out Item: PFbParameterInfo): Boolean; overload; 
    function Enumerate(var Enum: IEnumerator; out Item: PFbParameterInfo): Boolean;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: PFbParameterInfo read GetItem;
  end;

  IFbSession = interface
    [CFbSession]
    function GetApplication: IFbApplication;
    function GetDatabase: IFbDatabase;
    function GetTransactions: IFbTransactions;
    function StartTransaction(const Name: string = ''; AutoCommit: Boolean = True): IFbTransaction;
    function Statement(const Text: string): IFbStatement;
    property Application: IFbApplication read GetApplication;
    property Database: IFbDatabase read GetDatabase;
    property Transactions: IFbTransactions read GetTransactions;
  end;

  TFbTransactionField = (
      fbtfID,
      fbtfInstance,
      fbtfName
    );

  IFbTransactions = interface
    [CFbTransactions]
    function GetCount: Integer;
    function GetOptions: IFbTransactionOptionsDef;
    function GetItems(Index: Integer): IFbTransaction;
    function GetByName(const Name: string): IFbTransaction;
    function Find(Field: TFbTransactionField; Data: HData; Item: PInteger = nil): Boolean; overload;
    function Find(Field: TFbTransactionField; Data: HData; out Item: IFbTransaction): Boolean; overload;
    function Find(Field: TFbTransactionField; Data: HData; const IID: TGUID; out Item): Boolean; overload;
    function Start(const Name: string = ''; AutoCommit: Boolean = True): IFbTransaction;
    property Count: Integer read GetCount;
    property Defaults: IFbTransactionOptionsDef read GetOptions;
    property Items[Index: Integer]: IFbTransaction read GetItems;
    property ItemByName[const Name: string]: IFbTransaction read GetByName; default;
  end;

  IFbSessionOptions = interface
    [CFbSessionOptions]
  end;

  IFbTransactionOptions = interface
    [CFbTransactionOptions]
    function GetModel: TFbTransactionModel;
    function GetAccess: TFbTransactionAccess;
    function GetResolution: TFbTransactionResolution;
    function GetAutoCommit: Boolean;
    function GetRetaining: Boolean;
    property Model: TFbTransactionModel read GetModel;
    property Access: TFbTransactionAccess read GetAccess;
    property Resolution: TFbTransactionResolution read GetResolution;
    property AutoCommit: Boolean read GetAutoCommit;
    property Retaining: Boolean read GetRetaining;
  end;

  IFbTransactionOptionsDef = interface (IFbTransactionOptions)
    [CFbTransactionOptionsDef]
    procedure SetAccess(const Value: TFbTransactionAccess);
    procedure SetModel(const Value: TFbTransactionModel);
    procedure SetResolution(const Value: TFbTransactionResolution);
    procedure SetAutoCommit(const Value: Boolean);
    procedure SetRetaining(const Value: Boolean);
    property Model: TFbTransactionModel read GetModel write SetModel;
    property Access: TFbTransactionAccess read GetAccess write SetAccess;
    property Resolution: TFbTransactionResolution read GetResolution write SetResolution;
    property AutoCommit: Boolean read GetAutoCommit write SetAutoCommit;
    property Retaining: Boolean read GetRetaining write SetRetaining;
  end;

  IFbTransaction = interface (IFbHandled)
    [CFbTransaction]
    function GetSession: IFbSession;
    function GetID: Integer;
    function GetName: String;
    function GetIsPending: Boolean;
    function GetIsTerminated: Boolean;
    function GetFailed: Boolean;
    procedure SetFailed(Value: Boolean);
    function GetRetaining: Boolean;
    procedure SetRetaining(Value: Boolean);
    function GetAutoCommit: Boolean;
    procedure SetAutoCommit(Value: Boolean);
    procedure Commit; overload;
    procedure Rollback; overload; 
    procedure Commit(Retaining: Boolean); overload; 
    procedure Rollback(Retaining: Boolean); overload; 
    property Session: IFbSession read GetSession;
    property ID: Integer read GetID;
    property Name: String read GetName;
    property IsPending: Boolean read GetIsPending;
    property IsTerminated: Boolean read GetIsTerminated;
    property Failed: Boolean read GetFailed write SetFailed;
    property AutoCommit: Boolean read GetAutoCommit write SetAutoCommit;
  end;

  IFbDatabase = interface (IFbHandled)
    [CFbDatabase]
    function GetSchema: IFbSchema;
    property Schema: IFbSchema read GetSchema;
  end;

  IFbSchema = interface
    [CFbSchema]
    function Domains(
                const Transaction: IFbTransaction = nil;
                const Filter: String = '';
                const Args: IParameterList = nil;
                const Order: string = ''): IFbSchemaDomains;
    function Tables(
                const Transaction: IFbTransaction = nil;
                const Filter: String = '';
                const Args: IParameterList = nil;
                const Order: string = ''): IFbSchemaTables;
    function Views(
                const Transaction: IFbTransaction = nil;
                const Filter: String = '';
                const Args: IParameterList = nil;
                const Order: string = ''): IFbSchemaViews;
    function Procedures(
                const Transaction: IFbTransaction = nil;
                const Filter: String = '';
                const Args: IParameterList = nil;
                const Order: string = ''): IFbSchemaProcedures;
    function ProcedureByName(
                const Name: String;
                const Transaction: IFbTransaction = nil): IFbSchemaProcedure;
    function Parameters(
                const ProcedureName: string;
                const Transaction: IFbTransaction = nil;
                const Filter: String = '';
                const Args: IParameterList = nil;
                const Order: string = ''): IFbSchemaProcedureParameters;
    function ParameterByName(
                const ProcedureName, ParameterName: string;
                const Transaction: IFbTransaction = nil): IFbSchemaProcedureParameter;
    function Generators(
                const Transaction: IFbTransaction = nil;
                const Filter: String = '';
                const Args: IParameterList = nil;
                const Order: string = ''): IFbSchemaGenerators;
    function Collations(
                const Transaction: IFbTransaction = nil;
                const Filter: String = '';
                const Args: IParameterList = nil;
                const Order: string = ''): IFbSchemaCollations;
    function Charsets(
                const Transaction: IFbTransaction = nil;
                const Filter: String = '';
                const Args: IParameterList = nil;
                const Order: string = ''): IFbSchemaCharsets;
  end;

  RFbDomainData = record
    BaseType: TFbType;
    BlobType: TFbBlobType;
    Length: LongWord;
    Scale: Integer;    
    AllowNulls: Boolean;
  end;

  IFbDomain = interface
    [CFbDomain]
    function GetName: string;
    function GetBaseType: TFbType;
    function GetDataType: TFbDataType;
    function GetBlobType: TFbBlobType;
    function GetLength: LongWord;
    function GetSize: LongWord;
    function GetScale: Integer;
    function GetAllowNulls: Boolean;
    property Name: string read GetName;
    property BaseType: TFbType read GetBaseType;
    property DataType: TFbDataType read GetDataType;
    property BlobType: TFbBlobType read GetBlobType;
    property Length: LongWord read GetLength;
    property Size: LongWord read GetSize;
    property Scale: Integer read GetScale;
    property AllowNulls: Boolean read GetAllowNulls;
  end;

  TFbVariableKind = (
      fbvkParameters,
      fbvkFields
    );

  TFbVariableKindSet = set of TFbVariableKind;

  TFbStatementKind = (
      fbskUndefined,
      fbskSelect,
      fbskInsert,
      fbskUpdate,
      fbskDelete,
      fbskDDL,
      fbskGetSegment,
      fbskPutSegment,
      fbskExecProcedure,
      fbskStartTrans,
      fbskCommit,
      fbskRollback,
      fbskSelectForUpdate,
      fbskSetGenerator
    );

  IFbStatement = interface (IFbHandled)
    [CFbStatement]
    function GetSession: IFbSession;
    function GetKind: TFbStatementKind;
    function GetParameters: IFbVariables;
    function GetFields: IFbVariables;
    function Prepare(const Transaction: IFbTransaction = nil; const CursorName: string = ''): IFbCommand;
    property Session: IFbSession read GetSession;
    property Kind: TFbStatementKind read GetKind;
    property Parameters: IFbVariables read GetParameters;
    property Fields: IFbVariables read GetFields;
  end;

  IFbCommand = interface  (IFbHandled)
    [CFbCommand]
    function GetSession: IFbSession;
    function GetStatement: IFbStatement;
    function GetTransaction: IFbTransaction;
    function GetParameters: IFbBuffer;
    function Open(const Parameters: IFbBuffer = nil; const Fields: IFbBindings = nil): IFbCursor; overload;
    function Open(const Parameters: IParameterList; const Fields: IFbBindings = nil): IFbCursor; overload;
    function Open(const Parameters: array of RParameter; const Fields: IFbBindings = nil): IFbCursor; overload;
    function Execute(const Parameters: IFbBuffer = nil; Results: PFbBuffer = nil): Boolean; overload;
    function Execute(const Parameters: IParameterList; Results: PFbBuffer = nil): Boolean; overload;
    function Execute(const Parameters: array of RParameter; Results: PFbBuffer = nil): Boolean; overload;
    property Session: IFbSession read GetSession;
    property Transaction: IFbTransaction read GetTransaction;
    property Statement: IFbStatement read GetStatement;
    property Parameters: IFbBuffer read GetParameters;
  end;

  IFbVariables = interface
    [CFbVariables]
    function GetSession: IFbSession;
    function GetCount: Integer;
    function GetItem(Index: Integer): IFbVariable;
    function Get(const Name: string): IFbVariable; 
    function Find(const Name: string; out Item: IFbVariable): Boolean;
    function Enumerate(var Enum: IEnumerator; out Item: IFbVariable): Boolean; overload;
    function Enumerate(var Enum: IEnumerator; const IID: TGUID; out Item): Boolean; overload;
    property Session: IFbSession read GetSession;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: IFbVariable read GetItem;
    property Variable[const Name: string]: IFbVariable read Get; default;
  end;

  IFbVariable = interface
    [CFbVariable]
    function GetSession: IFbSession;
    function GetList: IFbVariables;
    function GetIndex: Integer;
    function GetPosition: Integer;
    function GetName: string;
    function GetKind: TFbVariableKind;
    function GetDomain: IFbDomain;
    property Session: IFbSession read GetSession;
    property List: IFbVariables read GetList;
    property Index: Integer read GetIndex;
    property Position: Integer read GetPosition;
    property Name: string read GetName;
    property Kind: TFbVariableKind read GetKind;
    property Domain: IFbDomain read GetDomain;
  end;

  IFbField = interface (IFbVariable)
    [CFbField]
    function GetFieldName: string;
    function GetOwnerName: string;
    function GetTableName: string;
    function GetAliasName: string;
    property FieldName: string read GetFieldName;
    property OwnerName: string read GetOwnerName;
    property TableName: string read GetTableName;
    property AliasName: string read GetAliasName;
  end;

  IFbBindings = interface
    [CFbBindings]
    function GetSession: IFbSession;
    function GetCount: Integer;
    function GetValues(Index: Integer): IFbBinding;
    function GetBindingByName(const Name: string): IFbBinding;
    function Add(const Binding: IFbBinding): Integer; overload;
    function Add(const Source: IFbVariable; const Domain: RFbDomainData): IFbBinding; overload;
    function Add(const Source: IFbVariable): IFbBinding; overload;
    function Add(const Name: string; const Domain: RFbDomainData): IFbBinding; overload;
    function Find(const Binding: IFbVariable; out Index: Integer): Boolean; overload;
    function Find(const Source: IFbVariable; out Item: IFbBinding): Boolean; overload;
    function Find(const Name: string; out Item: IFbBinding): Boolean; overload;
    function Find(Position: Integer; out Item: IFbBinding): Boolean; overload;
    function Enumerate(var Enum: IEnumerator; out Item: IFbBinding): Boolean; overload;
    function Remove(const Item: IFbBinding): Integer;
    procedure Clear;
    property Session: IFbSession read GetSession;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: IFbBinding read GetValues;
    property Bindings[const Name: string]: IFbBinding read GetBindingByName; default;
  end;

  IFbBinding = interface
    [CFbBinding]
    function GetList: IFbBindings;
    function GetIndex: Integer;
    function GetPosition: Integer;
    function GetName: string;
    function GetDomain: IFbDomain;
    property List: IFbBindings read GetList;
    property Index: Integer read GetIndex;
    property Position: Integer read GetPosition;
    property Name: string read GetName;
    property Domain: IFbDomain read GetDomain;
  end;

  IFbBuffer = interface
    [CFbBuffer]
    function GetCommand: IFbCommand;
    function GetBindings: IFbBindings;
    function GetCount: Integer;
    function GetItems(Index: Integer): IFbData;
    function GetByBinding(const Binding: IFbBinding): IFbData;
    function GetByOrdinal(Ordinal: LongWord): IFbData;
    function GetByName(const Name: string): IFbData;
    function Enumerate(var Enum: IEnumerator; out Item: IFbData): Boolean; overload;
    property Command: IFbCommand read GetCommand;
    property Bindings: IFbBindings read GetBindings;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: IFbData read GetItems; default;
    property ItemByOrdinal[Ordinal: LongWord]: IFbData read GetByOrdinal;
    property ItemByBinding[const Binding: IFbBinding]: IFbData read GetByBinding;
    property ItemByName[const Name: string]: IFbData read GetByName;
  end;

  IFbData = interface (IVariable)
    [CFbData]
    function GetBuffer: IFbBuffer;                                           
    function GetBinding: IFbBinding;
    function GetIsAssigned: Boolean;
    function GetValue: Variant;
    procedure SetValue(const Value: Variant);
    function GetIsNull: Boolean;
    procedure SetIsNull(Value: Boolean);
    procedure Clear;
    property Buffer: IFbBuffer read GetBuffer;
    property Binding: IFbBinding read GetBinding;
    property Value: Variant read GetValue write SetValue;
    property IsAssigned: Boolean read GetIsAssigned;
    property IsNull: Boolean read GetIsNull write SetIsNull;
  end;

  TSilFirebirdCursorState = (
      fbcsUndefined,
      fbcsClosed,
      fbcsOpened,
      fbcsFetching
    );

  IFbCursor = interface
    [CFbCursor]
    function GetCommand: IFbCommand;
    function GetBindings: IFbBindings;
    function GetFields: IFbBuffer;
    function GetHasMore: Boolean;
    function GetPosition: Integer;
    function GetState: TSilFirebirdCursorState;
    function Fetch: Boolean;
    function Enumerate(var Enum: IEnumerator; out Fields: IFbBuffer): Boolean; overload;
    procedure Close;
    property Command: IFbCommand read GetCommand;
    property Bindings: IFbBindings read GetBindings;
    property Fields: IFbBuffer read GetFields;
    property HasMore: Boolean read GetHasMore;
    property Position: Integer read GetPosition;
    property State: TSilFirebirdCursorState read GetState;
  end;

  IFbSchemaObject = interface
    [CFbSchemaObject]
    function GetCursor: IFbCursor;
    function Enumerate(var Enum: IEnumerator; out Fields): Boolean; overload;
    property Cursor: IFbCursor read GetCursor;
  end;

  IFbSchemaDomains = interface (IFbSchemaObject)
    [CFbSchemaDomains]
    function GetDomainFields: IFbSchemaDomain;
    function Enumerate(var Enum: IEnumerator; out Fields: IFbSchemaDomain): Boolean; overload;
    property Fields: IFbSchemaDomain read GetDomainFields;
  end;

  IFbSchemaDomain = interface
    [CFbSchemaDomain]
    function GetName: IFbData;                   
    function GetQueryName: IFbData;
    function GetValidationBlr: IFbData;
    function GetValidationSource: IFbData;
    function GetComputedBlr: IFbData;
    function GetComputedSource: IFbData;
    function GetDefaultValue: IFbData;
    function GetDefaultSource: IFbData;
    function GetLength: IFbData;
    function GetScale: IFbData;
    function GetDataType: IFbData;
    function GetDataSubtype: IFbData;
    function GetMissingValue: IFbData;
    function GetMissingSource: IFbData;
    function GetDescription: IFbData;
    function GetSystemFlag: IFbData;
    function GetQueryHeader: IFbData;
    function GetSegmentLength: IFbData;
    function GetEditString: IFbData;              
    function GetExternalLength: IFbData;          
    function GetExternalScale: IFbData;           
    function GetExternalType: IFbData;            
    function GetDimensions: IFbData;              
    function GetNullFlag: IFbData;                
    function GetCharacterLength: IFbData;         
    function GetCollationId: IFbData;        
    function GetCharsetId: IFbData;               
    function GetPrecision: IFbData;
    property Name: IFbData read GetName;
    property QueryName: IFbData read GetQueryName;
    property ValidationBlr: IFbData read GetValidationBlr;
    property ValidationSource: IFbData read GetValidationSource;
    property ComputedBlr: IFbData read GetComputedBlr;
    property ComputedSource: IFbData read GetComputedSource;
    property DefaultValue: IFbData read GetDefaultValue;
    property DefaultSource: IFbData read GetDefaultSource;
    property Length: IFbData read GetLength;
    property Scale: IFbData read GetScale;
    property DataType: IFbData read GetDataType;
    property DataSubtype: IFbData read GetDataSubtype;
    property MissingValue: IFbData read GetMissingValue;
    property MissingSource: IFbData read GetMissingSource;
    property Description: IFbData read GetDescription;
    property SystemFlag: IFbData read GetSystemFlag;
    property QueryHeader: IFbData read GetQueryHeader;
    property SegmentLength: IFbData read GetSegmentLength;
    property EditString: IFbData read GetEditString;
    property ExternalLength: IFbData read GetExternalLength;
    property ExternalScale: IFbData read GetExternalScale;
    property ExternalType: IFbData read GetExternalType;
    property Dimensions: IFbData read GetDimensions;
    property NullFlag: IFbData read GetNullFlag;
    property CharacterLength: IFbData read GetCharacterLength;
    property CollationId: IFbData read GetCollationId;
    property CharsetId: IFbData read GetCharsetId;
    property Precision: IFbData read GetPrecision;               
  end;
  
  IFbSchemaRelations = interface (IFbSchemaObject)
    [CFbSchemaRelations]
    function GetRelationFields: IFbSchemaRelation;
    function Enumerate(var Enum: IEnumerator; out Fields: IFbSchemaRelation): Boolean; overload;
    property Fields: IFbSchemaRelation read GetRelationFields;
  end;

  IFbSchemaRelation = interface
    [CFbSchemaRelation]
    function GetDescription: IFbData;          // DESCRIPTION
    function GetId: IFbData;                   // RELATION_ID
    function GetSystemFlag: IFbData;           // SYSTEM_FLAG
    function GetDbkeyLength: IFbData;          // DBKEY_LENGTH
    function GetFormat: IFbData;               // FORMAT
    function GetFieldCount: IFbData;           // FIELD_ID
    function GetName: IFbData;                 // RELATION_NAME
    function GetSecurityClass: IFbData;        // SECURITY_CLASS
    function GetRuntime: IFbData;              // RUNTIME
    function GetExternalDescription: IFbData;  // EXTERNAL_DESCRIPTION
    function GetOwnerName: IFbData;            // OWNER_NAME
    function GetDefaultClass: IFbData;         // DEFAULT_CLASS
    function GetFlags: IFbData;                // FLAGS
    property Description: IFbData read GetDescription;
    property Id: IFbData read GetId;
    property SystemFlag: IFbData read GetSystemFlag;
    property DbkeyLength: IFbData read GetDbkeyLength;
    property Format: IFbData read GetFormat;
    property FieldCount: IFbData read GetFieldCount;
    property Name: IFbData read GetName;
    property SecurityClass: IFbData read GetSecurityClass;
    property Runtime: IFbData read GetRuntime;
    property ExternalDescription: IFbData read GetExternalDescription;
    property OwnerName: IFbData read GetOwnerName;
    property DefaultClass: IFbData read GetDefaultClass;
    property Flags: IFbData read GetFlags;
  end;

  IFbSchemaTables = interface (IFbSchemaRelations)
    [CFbSchemaTables]
    function GetTableFields: IFbSchemaTable;
    function Enumerate(var Enum: IEnumerator; out Fields: IFbSchemaTable): Boolean; overload;
    property Fields: IFbSchemaTable read GetTableFields;
  end;

  IFbSchemaTable = interface (IFbSchemaRelation)
    [CFbSchemaTable]
    function GetExternalFile: IFbData;         // EXTERNAL_FILE
    property ExternalFile: IFbData read GetExternalFile;
  end;
  
  IFbSchemaViews = interface (IFbSchemaRelations)
    [CFbSchemaViews]
    function GetViewFields: IFbSchemaView;
    function Enumerate(var Enum: IEnumerator; out Fields: IFbSchemaView): Boolean; overload;
    property Fields: IFbSchemaView read GetViewFields;
  end;

  IFbSchemaView = interface (IFbSchemaRelation)
    [CFbSchemaView]
    function GetViewBlr: IFbData;              // VIEW_BLR
    function GetViewSource: IFbData;           // VIEW_SOURCE
    property ViewBlr: IFbData read GetViewBlr;
    property ViewSource: IFbData read GetViewSource;
  end;

  IFbSchemaProcedures = interface (IFbSchemaObject)
    [CFbSchemaProcedures]
    function GetProcFields: IFbSchemaProcedure;
    function Enumerate(var Enum: IEnumerator; out Fields: IFbSchemaProcedure): Boolean; overload;
    property Fields: IFbSchemaProcedure read GetProcFields;
  end;

  IFbSchemaProcedure = interface
    [CFbSchemaProcedure]
    function GetName: IFbData;         // RDB$PROCEDURE_NAME
    function GetId: IFbData;           // RDB$PROCEDURE_ID
    function GetInputs: IFbData;       // RDB$PROCEDURE_INPUTS
    function GetOutputs: IFbData;      // RDB$PROCEDURE_OUTPUTS
    function GetDescription: IFbData;  // RDB$DESCRIPTION
    function GetSource: IFbData;       // RDB$PROCEDURE_SOURCE
    function GetBlr: IFbData;          // RDB$PROCEDURE_BLR
    function GetSecurity: IFbData;     // RDB$SECURITY_CLASS
    function GetOwner: IFbData;        // RDB$OWNER_NAME
    function GetRuntime: IFbData;      // RDB$RUNTIME
    function GetSystem: IFbData;       // RDB$SYSTEM_FLAG
    function GetParameters: IFbSchemaProcedureParameters;
    property Name: IFbData read GetName;
    property Id: IFbData read GetId;
    property Inputs: IFbData read GetInputs;
    property Outputs: IFbData read GetOutputs;
    property Description: IFbData read GetDescription;
    property Source: IFbData read GetSource;
    property Blr: IFbData read GetBlr;
    property Security: IFbData read GetSecurity;
    property Owner: IFbData read GetOwner;
    property Runtime: IFbData read GetRuntime;
    property SystemFlag: IFbData read GetSystem;
    property Parameters: IFbSchemaProcedureParameters read GetParameters;
  end;

  IFbSchemaProcedureParameters = interface (IFbSchemaObject)
    [CFbSchemaProcedureParameters]
    function GetProcParamFields: IFbSchemaProcedureParameter;
    function Enumerate(var Enum: IEnumerator; out Fields: IFbSchemaProcedureParameter): Boolean; overload;
    property Fields: IFbSchemaProcedureParameter read GetProcParamFields;
  end;

  IFbSchemaProcedureParameter = interface
    [CFbSchemaProcedureParameter]
    function GetName: IFbData;             // RDB$PARAMETER_NAME
    function GetOwner: IFbData;            // RDB$PROCEDURE_NAME
    function GetNumber: IFbData;           // RDB$PARAMETER_NUMBER
    function GetKind: IFbData;             // RDB$PARAMETER_TYPE
    function GetField: IFbData;            // RDB$FIELD_SOURCE
    function GetDescription: IFbData;      // RDB$DESCRIPTION
    function GetSystemFlag: IFbData;       // RDB$SYSTEM_FLAG
    property Name: IFbData read GetName;
    property Owner: IFbData read GetOwner;
    property Number: IFbData read GetNumber;
    property Kind: IFbData read GetKind;
    property Field: IFbData read GetField;
    property Description: IFbData read GetDescription;
    property SystemFlag: IFbData read GetSystemFlag;
  end;
  
  IFbSchemaTriggers = interface (IFbSchemaObject)
    [CFbSchemaTriggers]
  end;

  IFbSchemaFunctions = interface (IFbSchemaObject)
    [CFbSchemaFunctions]
  end;

  IFbSchemaFields = interface (IFbSchemaObject)
    [CFbSchemaFields]
  end;

  IFbSchemaGenerators = interface (IFbSchemaObject)
    [CFbSchemaGenerators]
  end;

  IFbSchemaIndexes = interface (IFbSchemaObject)
    [CFbSchemaIndexes]
  end;

  IFbSchemaRoles = interface (IFbSchemaObject)
    [CFbSchemaRoles]
  end;

  IFbSchemaDatabase = interface (IFbSchemaObject)
    [CFbSchemaDatabase]
  end;

  IFbSchemaCollations = interface (IFbSchemaObject)
    [CFbSchemaCollations]
  end;

  IFbSchemaCharsets = interface (IFbSchemaObject)
    [CFbSchemaCharsets]
  end;

implementation
end.
