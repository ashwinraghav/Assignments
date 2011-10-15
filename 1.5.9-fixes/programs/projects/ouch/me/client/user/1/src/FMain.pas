unit FMain;

interface

uses
	Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
	Sil,
	// App
	UiOuchSkin, UiOuchEngine,
	UEngine;

type
	TOuchApp = class( TForm, IUnknown, IOuchClientEngineEvents )
		procedure FormCreate(Sender: TObject);
		procedure FormClose(Sender: TObject; var Action: TCloseAction);
	private
		fSkinLib: ISharedLibrary;
		fSkinFn: TOuchSkinFunction;
		fSkinTk: OuchSkinTkType;
		fEngineLib: ISharedLibrary;
		fEngineFn: TOuchEngineFunction;
		fClientEngine: IOuchClientEngine;
	protected // IOuchClientEngineEvents
		procedure OnShutDown( const AEngine: IOuchClientEngine );
	protected
		function LoadLib(const AName: string; out ALib: ISharedLibrary ): boolean;
	end;

var
	OuchApp: TOuchApp;

implementation

{$R *.DFM}

procedure TOuchApp.FormCreate(Sender: TObject);
var
	skin: IOuchSkin;
	engine: IOuchEngine;
begin
	// Mapea las librerías
	if not LoadLib( kOuchSkinLibraryName, fSkinLib ) then exit;
	//if not LoadLib( KOuchEngineLibraryName, fEngineLib ) then {exit};

	// Obtiene las funciones principales de las dlls
	if ( fEngineLib <> nil ) then
		fEngineLib.Bind( KOuchEngineFunctionName, fEngineFn );
	fSkinLib.Bind( 'OuchSkinDllFn'{kOuchSkinFunctionName}, fSkinFn );

	// Obtiene la class tool
	if Assigned( fSkinFn ) then
		fSkinTk := fSkinFn();

	// Instancia un skin
	if Assigned( fSkinTk ) then
		skin := fSkinTk.NewSkin( nil );

	// Instancia un engine
	if Assigned( fEngineFn ) then
		engine := fEngineFn();

	// Crea el engine cliente
	fClientEngine := OuchClientTk.NewEngine( skin, engine, self );

	// Inicializa la aplicacion
	if ( fClientEngine <> nil ) then
		fClientEngine.Initialize;
end;

procedure TOuchApp.OnShutDown(const AEngine: IOuchClientEngine);
begin
	fClientEngine := nil;
	Close;
end;

procedure TOuchApp.FormClose(Sender: TObject; var Action: TCloseAction);
begin
	Action := caFree;
end;

function TOuchApp.LoadLib(const AName: string; out ALib: ISharedLibrary ): boolean;
begin
	ALib := Sil.Os.SharedLibrary.Load( AName, false );
	result := ( ALib <> nil );
	if not result then
		ShowMessage( Format( 'No se encuentra la librería %s.', [ AName ] ) );
end;

end.

