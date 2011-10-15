unit UmOuchCommon;

interface

uses
	Classes,
	Sil, SilData,
	UiOuchClient;

type
	//
	// INTERFACE PARA PROPIEDADES TIPO GUID  
	//
	IOuchGuid = interface
		['{380A7B84-2B9A-4A39-99EA-BC4D427B7211}']
		function GetGuid: TGUID;
		procedure SetGUID(const Value: TGUID);
		// propiedades
		property GUID: TGUID read GetGuid write SetGUID;
	end;

	//
	// OBJETO PERSISTENTE
	//
{$M+}
	TOuchPersistentObject = class ( TInterfacedObject, IOuchPersistentObject )
	private
		fModified: boolean;
	protected // IOuchPersistentObject
		function GetModified: boolean;
		procedure SetModified(const Value: boolean);
		procedure Changed; virtual;
		function LoadFrom( const ATable: IDataRowset ): boolean; virtual;
		function SaveTo( const ATable: IDataRowset ): boolean; virtual;
		procedure LoadFields( const ATable: IDataRowset ); virtual;
		procedure SaveFields( const ATable: IDataRowset ); virtual;
		procedure DeleteFrom( const ATable: IDataRowset ); virtual;
		procedure CreateStructure( const ATable: IDataRowsetDef ); virtual; 
		// propiedades
		property Modified: boolean read GetModified write SetModified;
	end;
{$M-}

	//
	// OBJETO IDENTIFICADO
	//
	TOuchIdentifiedObject = class ( TOuchPersistentObject, IOuchIdentifiedObject )
	private
		fId: TGUID;
    function GetIdent: Variant;
    procedure SetIdent(const Value: Variant);
	protected // IOuchsIdentifiedObject
		function GetId: TGUID;
		procedure SetId(const Value: TGUID ); virtual;
		function LoadFrom( const ATable: IDataRowset ): boolean; override;
		function SaveTo( const ATable: IDataRowset ): boolean; override;
		procedure DeleteFrom( const ATable: IDataRowset ); override;
		procedure CreateStructure( const ATable: IDataRowsetDef ); override;
		// propiedades
		property Id: TGUID read GetId write SetId;
	public
		constructor Create; reintroduce;
	published
		property IDENTITY: Variant read GetIdent write SetIdent;
	end;

	//
	// LISTA DE OBJETOS IDENTIFICADOS
	//
	TOuchIdentifiedObjectList = class ( TOuchPersistentObject, IOuchIdentifiedObjectList )
	private
		fList: IInterfaceList;
	protected // IOuchsIdentifiedObject
		function LoadFrom( const ATable: IDataRowset ): boolean; override;
		function SaveTo( const ATable: IDataRowset ): boolean; override;
		procedure DeleteFrom( const ATable: IDataRowset ); override;
		procedure CreateStructure( const ATable: IDataRowsetDef ); override;
	protected // IOuchIdentifiedObjectList
		procedure Clear;
		procedure Add( const AItem: IOuchIdentifiedObject );
		procedure Delete( AIndex: integer );
		procedure DeleteById( const AID: TGUID );
		function IndexByID(const AID: TGUID): integer;
		function GetCount: integer;
		function GetItem(AIndex: integer): IOuchIdentifiedObject;
		function GetItemByID(const AID: TGUID): IOuchIdentifiedObject;
		function NewItem: IOuchIdentifiedObject; virtual; abstract;
		// propiedades
		property Count: integer read GetCount;
		property Item[ AIndex: integer ]: IOuchIdentifiedObject read GetItem;
		property ItemByID[ const AID: TGUID ]: IOuchIdentifiedObject read GetItemByID;
	end;

	//
	// TABLA GENERICA
	//
	TOuchTable = class ( TOuchIdentifiedObjectList, IOuchTable )
	private
		fFileName: string;
		fDef: IDataRowset;
	private
		function Open(const AFileName: string; out ADef: IDataRowset ): boolean;
	protected // IOuchTable
		function Save: boolean;
		function Load: boolean;
		function GetFileName: string;
		procedure SetFileName(const Value: string);
		// propiedades
		property FileName: string read GetFileName write SetFileName;
	end;

implementation

uses
	UmOuchTools;

type
	TOuchGuid = class ( TInterfacedObject, IOuchGuid )
	private
		fGUID: TGUID;
	protected // IOuchGuid
		function GetGuid: TGUID;
		procedure SetGUID(const Value: TGUID);
		// propiedades
		property GUID: TGUID read GetGuid write SetGUID;
	public
		constructor Create( const AGUID: TGUID ); reintroduce;
	end;

{ TOuchPersistentObject }

procedure TOuchPersistentObject.Changed;
begin
	fModified := true;
end;

function TOuchPersistentObject.GetModified: boolean;
begin
	result := fModified;
end;

function TOuchPersistentObject.LoadFrom(const ATable: IDataRowset): boolean;
begin
	fModified := false;
	LoadFields( ATable );
	result := true;
end;

function TOuchPersistentObject.SaveTo(const ATable: IDataRowset): boolean;
begin
	SaveFields( ATable );
	fModified := false;
	result := true;
end;

procedure TOuchPersistentObject.DeleteFrom(const ATable: IDataRowset);
begin
	fModified := true;
end;

procedure TOuchPersistentObject.SetModified(const Value: boolean);
begin
	fModified := false;
end;

procedure TOuchPersistentObject.LoadFields(const ATable: IDataRowset);
begin
	OuchTool.ReadFields( self, ATable );
end;

procedure TOuchPersistentObject.SaveFields(const ATable: IDataRowset);
begin
	OuchTool.WriteFields( self, ATable );
end;

procedure TOuchPersistentObject.CreateStructure(const ATable: IDataRowsetDef);
begin
	OuchTool.CreateFields( self, ATable );
end;

{ TOuchIdentifiedObject }

constructor TOuchIdentifiedObject.Create;
begin
	inherited Create();
	fId := Sil.Guid.Null;
end;

function TOuchIdentifiedObject.LoadFrom(const ATable: IDataRowset): boolean;
begin
	result := Sil.Guid.IsEmpty( fId ) or
		ATable.Find( [ Sil.Guid.ToStr( Id ) ], false );
	if not result or ATable.IsEof then exit;

	try
		LoadFields( ATable );
	except
		result := false;
	end;
	if result then
		fModified := false;
end;

function TOuchIdentifiedObject.SaveTo(const ATable: IDataRowset): boolean;
var
	isnew: boolean;
begin
	result := fModified;
	if not result then exit;

	ATable.ActiveIndexName := kIndexByIdName;

	isnew := Sil.Guid.IsEmpty( fId );
	if isnew then
		fId := Sil.Guid.Create;

	if isnew or not ATable.Find( [ Sil.Guid.ToStr( fId ) ], false ) then
		ATable.Append else
		ATable.Edit;

	try
		SaveFields( ATable );
	except
		result := false;
	end;

	if result then
	begin
		ATable.Post;
		fModified := false;
	end;
end;

procedure TOuchIdentifiedObject.DeleteFrom(const ATable: IDataRowset);
begin
	ATable.ActiveIndexName := kIndexByIdName;

	if ATable.Find( [ Sil.Guid.ToStr( fId ) ], false ) then
		ATable.Delete;

	fModified := true;
end;

procedure TOuchIdentifiedObject.CreateStructure(const ATable: IDataRowsetDef);
begin
	ATable.Indexes.CreateItem( kIndexByIdName, 'IDENTITY' );
end;

function TOuchIdentifiedObject.GetId: TGUID;
begin
	result := fId;
end;

procedure TOuchIdentifiedObject.SetId(const Value: TGUID);
begin
	if Sil.Guid.IsEqual( fId, Value ) then exit;
	fId := Value;
	Changed();
end;

function TOuchIdentifiedObject.GetIdent: Variant;
begin
	result := Sil.Guid.ToStr( fId );
end;

procedure TOuchIdentifiedObject.SetIdent(const Value: Variant);
begin
	ID := Sil.Guid.FromStr( Value );
end;

{ TOuchIdentifiedObjectList }

procedure TOuchIdentifiedObjectList.Add(const AItem: IOuchIdentifiedObject);
begin
	if ( fList = nil ) then
		fList := Sil.Tk.InterfaceList;

	fList.Add( AItem );
end;

procedure TOuchIdentifiedObjectList.Clear;
begin
	if ( fList <> nil ) then
		fList.Clear;
end;

procedure TOuchIdentifiedObjectList.Delete(AIndex: integer);
begin
	if ( fList <> nil ) then
		fList.Delete( AIndex );
end;

procedure TOuchIdentifiedObjectList.DeleteById(const AID: TGUID);
var
	i1: integer;
begin
	i1 := IndexById( AID );
	if ( i1 >= 0 ) then
		fList.Delete( i1 );
end;

function TOuchIdentifiedObjectList.GetItem(AIndex: integer): IOuchIdentifiedObject;
begin
	if ( fList <> nil ) and ( 0 <= AIndex ) and ( AIndex < fList.Count ) then
		result := IOuchIdentifiedObject( fList[ AIndex ] ) else
		result := nil;
end;

function TOuchIdentifiedObjectList.GetItemByID( const AID: TGUID): IOuchIdentifiedObject;
var
	i1: integer;
begin
	i1 := IndexById( AID );
	result := GetItem( i1 );
end;

function TOuchIdentifiedObjectList.IndexByID(const AID: TGUID): integer;
begin
	if ( fList <> nil ) then
		for result := 0 to fList.Count - 1 do
			if Sil.Guid.IsEqual( Item[ result ].Id, AID ) then
				exit;

	result := -1;
end;

function TOuchIdentifiedObjectList.LoadFrom(const ATable: IDataRowset): boolean;
var
	itm: IOuchIdentifiedObject;
begin
	result := inherited LoadFrom( ATable );

	ATable.First;

	if ( fList <> nil ) then
	begin
		fList.Clear;

		while not ATable.IsEof do
		begin
			itm := NewItem;
			itm.LoadFrom( ATable );
			fList.Add( itm );
			ATable.Next;
		end;
	end;
end;

function TOuchIdentifiedObjectList.SaveTo(const ATable: IDataRowset): boolean;
var
	i1: integer;
begin
	if ( fList <> nil ) then
	begin
		for i1 := 0 to fList.Count - 1 do
			Item[ i1 ].SaveTo( ATable );
	end;

	result := inherited SaveTo( ATable );
end;

procedure TOuchIdentifiedObjectList.DeleteFrom(const ATable: IDataRowset);
var
	i1: integer;
begin
	inherited;

	if ( fList <> nil ) then
		for i1 := fList.Count - 1 downto 0 do
			Item[ i1 ].DeleteFrom( ATable );
end;

procedure TOuchIdentifiedObjectList.CreateStructure(const ATable: IDataRowsetDef);
begin
	inherited;

	NewItem.CreateStructure( ATable );
end;

function TOuchIdentifiedObjectList.GetCount: integer;
begin
	if ( fList <> nil ) then
		result := fList.Count else
		result := 0;
end;

{ TOuchTable }

function TOuchTable.GetFileName: string;
begin
	result := fFileName;
end;

function TOuchTable.Load: boolean;
begin
	result := ( fDef <> nil ) or Open( fFileName, fDef );
	if not result then exit;

	result := LoadFrom( fDef );
end;

function TOuchTable.Save: boolean;
begin
	result := ( fDef <> nil ) or Open( fFileName, fDef );
	if not result then exit;

	result := SaveTo( fDef );
end;

function TOuchTable.Open(const AFileName: string; out ADef: IDataRowset ): boolean;
var
	tdef: IDataRowsetDef;
begin
	result := true;
	try
		ADef := SilData.Tk.OpenFile( AFileName );
		exit;
	except
	end;

	tdef := SilData.Tk.CreateFile( AFileName );
	try
		CreateStructure( tdef );
		tdef.Build;
	except
		result := false;
	end;
	ADef := tdef.Rowset;
end;

procedure TOuchTable.SetFileName(const Value: string);
begin
	if ( fFileName = Value ) then exit;
	fFileName := Value;
	Changed();
end;

{ TOuchGuid }

constructor TOuchGuid.Create(const AGUID: TGUID);
begin
	inherited Create;
	fGUID := AGUID;
end;

function TOuchGuid.GetGuid: TGUID;
begin
	result := fGUID;
end;

procedure TOuchGuid.SetGUID(const Value: TGUID);
begin
	fGUID := Value;
end;

end.

