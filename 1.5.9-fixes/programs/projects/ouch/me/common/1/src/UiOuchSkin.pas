unit UiOuchSkin;

interface

uses
	Classes,
	Sil;
	
const
	// Nombre de la dll del skin
	kOuchSkinLibraryName = 'OuchSkin.dll';

const
	// Nombre de la función principal de la dll
	kOuchSkinFunctionName = 'OuchSkinDllFn';

type
	TTagStates = ( kosStVisible, kosStEnabled );
	TTagState = set of TTagStates;

	// Interface implementada por el skin
	//
	IOuchSkin = interface ( IConnectable )
		['{043B5C32-6C14-11D5-BA25-006008AE4EDF}']
		procedure Initialize( const AClientVer: IVersionNumber );
		procedure Show;
		procedure Hide;
		procedure SetTagValue( const ATag: string; const AValue: Variant );
		procedure SetTagState( const ATag: string; const AState: TTagState );
		function GetTagValue( const ATag: string ): Variant;
		function GetTagState( const ATag: string ): TTagState;
		// propiedades
		property TagValue[ const ATag: string ]: Variant read GetTagValue write SetTagValue;
		property TagState[ const ATag: string ]: TTagState read GetTagState write SetTagState;
	end;

	// Interface de eventos del skin
	//
	IOuchSkinEvents = interface
		['{043B5C33-6C14-11D5-BA25-006008AE4EDF}']
		procedure OnShutDown( const ASkin: IOuchSkin );
	end;

type
	// Class tool retornada por la función principal de la dll
	OuchSkinTk = class
		class function NewSkin( Owner: TComponent ): IOuchSkin; virtual; abstract;
	end;
	OuchSkinTkType = class of OuchSkinTk;

	// Prototipo de la función principal de la dll
	TOuchSkinFunction = function: OuchSkinTkType;

const
	// Tags del skin
	kosTagCurrentUserName = 'CurrentUserName';

implementation

end.

