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

unit SilShFirebird;

{$INCLUDE Defines.inc}

interface

uses
  Sil,
  SilSeFirebirdClient,
  SilSiFirebird,
  SilLiDataType,
  SilLkValue;

type
  PFbBufferInternal = ^IFbBufferInternal;
  
  IFbApplicationInternal = interface;
  IFbServicesInternal = interface;
  IFbParametersInternal = interface;
  IFbParamBlock = interface;
  IFbDatabaseInternal = interface;
  IFbTransactionInternal = interface;
  IFbTransactionsInternal = interface;
  IFbSchemaInternal = interface;
  IFbSchemaQueries = interface;
  IFbCommandInternal = interface;
  IFbVariablesInternal = interface;
  IFbVariableInternal = interface;
  IFbBufferInternal = interface;
  IFbDataInternal = interface;
  IFbBindingsInternal = interface;
  IFbBindingInternal = interface;
  IFbValuesInternal = interface;
  IFbValueInternal = interface;

  PPXSQLDA_V1 = ^PXSQLDA_V1;

  RFbTransactionDefaults = record
    Model: TFbTransactionModel;
    Access: TFbTransactionAccess;
    Resolution: TFbTransactionResolution;
    AutoCommit: Boolean;
    Retaining: Boolean;
  end;

  RFbFieldData = record
    FieldName: string;
    OwnerName: string;
    TableName: string;
    AliasName: string;
  end;

  RFbVariableData = record
    Field: RFbFieldData;
    Domain: RFbDomainData;
  end;

  TSilFirebirdDomainClass = class of TSilFirebirdDomainType;

  TSilFirebirdDomainType = class(TSilObject)
    constructor Create(const Data: RFbDomainData); overload; virtual; abstract;
  end;

  IFbApplicationInternal = interface (IFbApplication)
    [CFbApplication]
    function DoGetProperties: IFbParametersInternal;
    function GetParameters(const List: array of PFbParameterInfo; const Block: IFbParamBlock): IFbParametersInternal;
    property Properties: IFbParametersInternal read DoGetProperties;
  end;

  IFbServicesInternal = interface (IFbServiceManager)
    [CFbServiceManager]
    function DoGetApplication: IFbApplicationInternal;
    function DoGetHandle: PISC_SVC_HANDLE;
    property Application: IFbApplicationInternal read DoGetApplication;
    property Handle: PISC_SVC_HANDLE read DoGetHandle;
  end;

  IFbSessionInternal = interface (IFbSession)
    [CFbSession]
    function DoGetApplication: IFbApplicationInternal;
    function DoGetDatabase: IFbDatabaseInternal;
    function DoGetTransactions: IFbTransactionsInternal;
    property Application: IFbApplicationInternal read DoGetApplication;
    property Database: IFbDatabaseInternal read DoGetDatabase;
    property Transactions: IFbTransactionsInternal read DoGetTransactions;
  end;

  IFbDatabaseInternal = interface (IFbDatabase)
    [CFbDatabase]
    function DoGetHandle: PISC_DB_HANDLE;
    function DoGetSchema: IFbSchemaInternal;
    property Handle: PISC_DB_HANDLE read DoGetHandle;
    property Schema: IFbSchemaInternal read DoGetSchema;
  end;

  IFbSchemaInternal = interface (IFbSchema)
    [CFbSchema]
    function GetQueries: IFbSchemaQueries;
    property Queries: IFbSchemaQueries read GetQueries;
  end;
  
  IFbParamBlock = interface
    ['{081F229C-81D5-4DC3-9417-B712936E2C13}']
    procedure AddBoolean(Item: PFbParameterInfo; var Buffer: string; const Value: Boolean);
    function GetBoolean(Item: PFbParameterInfo; var Buffer: PChar): Boolean;
    procedure AddByte(Item: PFbParameterInfo; var Buffer: string; const Value: Byte);
    function GetByte(Item: PFbParameterInfo; var Buffer: PChar): Byte;
    procedure AddWord(Item: PFbParameterInfo; var Buffer: string; const Value: Word);
    function GetWord(Item: PFbParameterInfo; var Buffer: PChar): Word;
    procedure AddInteger(Item: PFbParameterInfo; var Buffer: string; const Value: Integer);
    function GetInteger(Item: PFbParameterInfo; var Buffer: PChar): Integer;
    procedure AddString(Item: PFbParameterInfo; var Buffer: string; const Value: String);
    function GetString(Item: PFbParameterInfo; var Buffer: PChar): String;
  end;

  IFbParametersInternal = interface (IFbParameters)
    [CFbParameters]
    function Add(var Buffer: string; const Param: string; const Value: Variant): Boolean; overload; 
    procedure Add(var Buffer: string; const Parameters: IParameters); overload;
    function Parse(Buffer: PChar; const Parameters: IParameterList): PChar;
  end;

  IFbTransactionInternal = interface (IFbTransaction)
    [CFbTransaction]
    function DoGetList: IFbTransactionsInternal;
    function DoGetSession: IFbSessionInternal;
    function DoGetHandle: PISC_TR_HANDLE;
    procedure Detach;
    property List: IFbTransactionsInternal read DoGetList;
    property Session: IFbSessionInternal read DoGetSession;
    property Handle: PISC_TR_HANDLE read DoGetHandle;
  end;

  IFbTransactionsInternal = interface (IFbTransactions)
    [CFbTransactions]
    function DoGetSession: IFbSessionInternal;
    function Add(const Transaction: IFbTransactionInternal): Integer;
    function Remove(const Transaction: IFbTransactionInternal): Integer;
    property Session: IFbSessionInternal read DoGetSession;
  end;

  IFbDomainInternal = interface (IFbDomain)
    [CFbDomain]
    function GetHandler(const Command: IFbCommandInternal): IDataHandler;
  end;

  IFbStatementInternal = interface (IFbStatement)
    [CFbStatement]
    function DoGetSession: IFbSessionInternal;
    function DoGetHandle: PISC_STMT_HANDLE;
    function DoGetNames: IStringList;
    property Session: IFbSessionInternal read DoGetSession;
    property Handle: PISC_STMT_HANDLE read DoGetHandle;
    property Names: IStringList read DoGetNames;
  end;

  IFbCommandInternal = interface (IFbCommand)
    [CFbCommand]
    function DoGetSession: IFbSessionInternal;
    function DoGetTransaction: IFbTransactionInternal;
    function DoGetStatement: IFbStatementInternal;
    function DoGetHandle: PISC_STMT_HANDLE;
    procedure Execute(const Parameters: IFbValuesInternal; const Results: IFbBufferInternal = nil); overload;
    function Execute(const Parameters: IFbValuesInternal; Results: PFbBufferInternal): Boolean; overload;
    property Session: IFbSessionInternal read DoGetSession;
    property Transaction: IFbTransactionInternal read DoGetTransaction;
    property Statement: IFbStatementInternal read DoGetStatement;
    property Handle: PISC_STMT_HANDLE read DoGetHandle;
  end;

  IFbVariablesInternal = interface (IFbVariables)
    [CFbVariables]
    function DoGetSession: IFbSessionInternal;
    function Add(const Item: IFbVariableInternal): Integer;
    function Remove(const Item: IFbVariableInternal): Integer;
    property Session: IFbSessionInternal read DoGetSession;
  end;

  IFbVariableInternal = interface (IFbVariable)
    [CFbVariable]
    function DoGetSession: IFbSessionInternal;
    procedure Detach;
    property Session: IFbSessionInternal read DoGetSession;
  end;

  IFbBindingsInternal = interface (IFbBindings)
    [CFbBindings]
    function DoGetSession: IFbSessionInternal;
    function GetSize: LongWord;
    function Enumerate(var Enum: IEnumerator; out Item: IFbBindingInternal): Boolean; overload;
    property Session: IFbSessionInternal read DoGetSession;
    property Size: LongWord read GetSize;
  end;

  IFbBindingInternal = interface (IFbBinding)
    [CFbBinding]
    function DoGetSession: IFbSessionInternal;
    function DoGetList: IFbBindingsInternal;
    function DoGetDomain: IFbDomainInternal;
    function DoGetSize: LongWord;
    function DoGetOffset: LongWord;
    property Session: IFbSessionInternal read DoGetSession;
    property List: IFbBindingsInternal read DoGetList;
    property Domain: IFbDomainInternal read DoGetDomain;
    property Size: LongWord read DoGetSize;
    property Offset: LongWord read DoGetOffset;
  end;

  PFbBufferEntry = ^RFbBufferEntry;
  RFbBufferEntry = record
    Status: Short;
    Reserved: Short;
    Data: record end;
  end;

  IFbBufferInternal = interface (IFbBuffer)
    [CFbBuffer]
    function DoGetCommand: IFbCommandInternal;
    function DoGetBindings: IFbBindingsInternal;
    function DoGetValues: IFbValuesInternal;
    function DoGetMemory: PChar;
    function DoGetSize: LongWord;
    property Command: IFbCommandInternal read DoGetCommand;
    property Bindings: IFbBindingsInternal read DoGetBindings;
    property Values: IFbValuesInternal read DoGetValues;
    property Memory: PChar read DoGetMemory;
    property Size: LongWord read DoGetSize;
  end;

  IFbDataInternal = interface (IFbData)
    [CFbData]
    function DoGetBuffer: IFbBufferInternal;
    function DoGetBinding: IFbBindingInternal;
    function DoGetTotalSize: LongWord;
    function DoGetMemory: PChar;
    function DoGetStatus: PShort;
    property Buffer: IFbBufferInternal read DoGetBuffer;
    property Binding: IFbBindingInternal read DoGetBinding;
    property Status: PShort read DoGetStatus;
    property Memory: PChar read DoGetMemory;
    property TotalSize: LongWord read DoGetTotalSize;
  end;

  IFbValuesInternal = interface
    ['{8FB56175-B331-4E08-A7FD-19B944CF6B64}']
    function DoGetCommand: IFbCommandInternal;
    function DoGetBindings: IFbBindingsInternal;
    function DoGetBuffer: IFbBufferInternal;
    function DoGetHandle: PXSQLDA_V1;
    procedure DoSetHandle(Value: PXSQLDA_V1);
    function Enumerate(var Enum: IEnumerator; out Item: IFbValueInternal): Boolean; overload;
    function Add(const Item: IFbValueInternal): Integer;
    function Remove(const Item: IFbValueInternal): Integer;
    property Command: IFbCommandInternal read DoGetCommand;
    property Bindings: IFbBindingsInternal read DoGetBindings;
    property Buffer: IFbBufferInternal read DoGetBuffer;
    property Handle: PXSQLDA_V1 read DoGetHandle write DoSetHandle;
  end;

  IFbValueInternal = interface 
    ['{2A9A5ABB-3F1F-4398-A09A-2E31F88A5284}']
    function DoGetCommand: IFbCommandInternal;
    function DoGetBinding: IFbBindingInternal;
    function DoGetHandle: PXSQLVAR_V1;
    procedure DoSetHandle(Value: PXSQLVAR_V1);
    procedure Detach;
    property Command: IFbCommandInternal read DoGetCommand;
    property Binding: IFbBindingInternal read DoGetBinding;
    property Handle: PXSQLVAR_V1 read DoGetHandle write DoSetHandle;
  end;

  IFbCursorInternal = interface (IFbCursor)
    [CFbCursor]
    function DoGetCommand: IFbCommandInternal;
    function DoGetBindings: IFbBindingsInternal;
    function DoGetBuffer: IFbBufferInternal;
    property Command: IFbCommandInternal read DoGetCommand;
    property Bindings: IFbBindingsInternal read DoGetBindings;
    property Buffer: IFbBufferInternal read DoGetBuffer;
  end;

  IFbSchemaQueries = interface
    ['{D6693B0F-E90B-49A6-A3AB-E06AB6B503E5}']
    function QueryDomains(const Filter, Order: string; const Transaction: IFbTransaction = nil): IFbCommandInternal;
    function QueryRelations(const Filter, Order: string; const Transaction: IFbTransaction = nil): IFbCommandInternal;
    function QueryProcedures(const Filter, Order: string; const Transaction: IFbTransaction = nil): IFbCommandInternal;
    function QueryProcedureParameters(const ProcedureName: string; const Filter, Order: string; const Transaction: IFbTransaction = nil): IFbCommandInternal;
  end;

  IFbSchemaObjectInternal = interface (IFbSchemaObject)
    [CFbSchemaObject]
    function DoGetCursor: IFbCursorInternal;
    function DoGetFields: IUnknown;
    property Cursor: IFbCursorInternal read DoGetCursor;
    property Fields: IUnknown read DoGetFields;
  end;

  TSilFirebirdVariableClass = class of TSilFirebirdVariableType;

  TSilFirebirdVariableType = class(TSilObject)
    constructor Create(const List: IFbVariablesInternal; const Statement: IFbStatementInternal; const Source: PXSQLVAR_V1); overload; virtual; abstract;
  end;

  TSilFirebirdDataClass = class of TSilFirebirdDataType;

  TSilFirebirdDataType = class(TSilVariable)
    constructor Create(const List: IFbBufferInternal; const Binding: IFbBindingInternal; const Buffer: PFbBufferEntry); reintroduce; overload; virtual; abstract;
  end;

  TSilFirebirdBlockClass = class of TSilFirebirdBlockType; 
  TSilFirebirdBlockType = class(TSilObject)
    constructor Create(Dummy: Integer); overload; virtual; abstract;     
  end;

implementation
end.
