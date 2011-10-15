unit UiOuchClient;

interface

uses
	Sil, SilData;

type
	IOuchPersistentObject = interface;
	IOuchIdentifiedObject = interface;
	IOuchIdentifiedObjectList = interface;
	IOuchTable = interface;
	IOuchUserAppearance = interface;
	IOuchUsersAppearanceList = interface;

	//
	// Objeto persistente
	//
	IOuchPersistentObject = interface
		['{438EE228-BAA6-433F-B769-9DACF023736C}']
		function GetModified: boolean;
		procedure SetModified(const Value: boolean);
		procedure Changed;
		function LoadFrom( const ATable: IDataRowset ): boolean;
		function SaveTo( const ATable: IDataRowset ): boolean;
		procedure LoadFields( const ATable: IDataRowset );
		procedure SaveFields( const ATable: IDataRowset );
		procedure DeleteFrom( const ATable: IDataRowset );
		procedure CreateStructure( const ATable: IDataRowsetDef ); 
		// propiedades
		property Modified: boolean read GetModified write SetModified;
	end;

	//
	// Objeto identificado con un GUID
	//
	IOuchIdentifiedObject = interface ( IOuchPersistentObject )
		['{5667C2E5-07ED-46D9-B759-7597CE695BDE}']
		function GetId: TGUID;
		procedure SetId(const Value: TGUID );
		// propiedades
		property Id: TGUID read GetId write SetId;
	end;

	//
	// Lista de elementos persistentes
	//
	IOuchIdentifiedObjectList = interface ( IOuchPersistentObject )
		['{14C71DEE-BAE0-41A8-B22E-4E56DFF466B7}']
		procedure Clear;
		procedure Add( const AItem: IOuchIdentifiedObject );
		procedure Delete( AIndex: integer );
		procedure DeleteById( const AID: TGUID );
		function IndexByID(const AID: TGUID): integer;
    function GetCount: integer;
		function GetItem(AIndex: integer): IOuchIdentifiedObject;
		function GetItemByID(const AID: TGUID): IOuchIdentifiedObject;
		function NewItem: IOuchIdentifiedObject;
		// propiedades
		property Count: integer read GetCount;
		property Item[ AIndex: integer ]: IOuchIdentifiedObject read GetItem;
		property ItemByID[ const AID: TGUID ]: IOuchIdentifiedObject read GetItemByID;
	end;

	//
	// Tabla genérica
	//
	IOuchTable = interface ( IOuchIdentifiedObjectList )
		['{276999C1-C8B0-4C6E-81E6-749244FBB549}']
		function Save: boolean;
		function Load: boolean;
		function GetFileName: string;
		procedure SetFileName(const Value: string);
		// propiedades
		property FileName: string read GetFileName write SetFileName;
	end;

	//
	// Configuración de la apariencia de un usuario
	//
	IOuchUserAppearance = interface ( IOuchIdentifiedObject )
		['{3000BE66-BE9F-4CC3-8E92-7F6B44C337B7}']
		function GetUserGuid: TGUID;
		function GetEnabled: boolean;
    function GetIsDefault: boolean;
		function GetGroup: string;
		function GetShowConnecting: boolean;
		function GetShowMessages: boolean;
		procedure SetEnabled(const Value: boolean);
		procedure SetGroup(const Value: string);
		procedure SetShowConnecting(const Value: boolean);
		procedure SetShowMessages(const Value: boolean);
		// propiedades
		property IsDefault: boolean read GetIsDefault;
		property UserGuid: TGUID read GetUserGuid;
		property Enabled: boolean read GetEnabled write SetEnabled;
		property Group: string read GetGroup write SetGroup;
		property ShowConnecting: boolean read GetShowConnecting write SetShowConnecting;
		property ShowMessages: boolean read GetShowMessages write SetShowMessages;
	end;

	//
	// Lista de apariencias de usuarios
	//
	IOuchUsersAppearanceList = interface
		['{AF625793-7EC4-46DA-95F2-EAE9C8E1D74D}']
		function GetAppearance(AIndex: integer): IOuchUserAppearance;
		function GetAppearanceById(const AID: TGUID): IOuchUserAppearance;
		function GetDefaultAppearance: IOuchUserAppearance;
		// propiedades
		property Appearance[ AIndex: integer ]: IOuchUserAppearance read GetAppearance;
		property AppearanceById[ const AID: TGUID ]: IOuchUserAppearance read GetAppearanceById;
		property DefaultAppearance: IOuchUserAppearance read GetDefaultAppearance;
	end;


const
	// Indices
	kIndexByIdName = 'Id';

	// Campos de tablas
	{kfID = 'Id';
	kfISDEFAULT = 'IsDefault';
	kfUSERGUID = 'UserGuid';
	kfENABLED = 'Enabled';
	kfGROUP = 'Group';
	kfSHOWCONNECTING = 'ShowConnecting';
	kfSHOWMESSAGES = 'ShowMessages';}

implementation

end.

