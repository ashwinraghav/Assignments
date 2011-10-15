unit OskUMain;

interface

uses
	Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
	WTaskBar, StdCtrls, TreeNT, ExtCtrls,
	Sil,
	// App
	UiOuchSkin, Menus;

type
	TOuchSkinBase = class(TForm, IUnknown, IOuchSkin)
		Taskbar: TWinTaskbar;
    pnHeader: TPanel;
    pnUsers: TPanel;
    treeUsers: TTreeNT;
    pnFooter: TPanel;
    PopupMenu1: TPopupMenu;
    Cerrar1: TMenuItem;
		procedure FormCreate(Sender: TObject);
		procedure FormDestroy(Sender: TObject);
		procedure FormKeyDown(Sender: TObject; var Key: Word;
			Shift: TShiftState);
		procedure TaskbarClick(Sender: TObject);
		procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Cerrar1Click(Sender: TObject);
	private
		fClntList: IEventList;
	protected
		procedure NotifyShutdown;
		function FindTag( const ATag: string ): TComponent;
	protected // IOuchSkin
		procedure AddListener( const AClient: IUnknown );
		procedure RemoveListener( const AClient: IUnknown );
		procedure Initialize( const AClientVer: IVersionNumber );
		procedure SkinShow;
		procedure SkinHide;
		procedure IOuchSkin.Show = SkinShow;
		procedure IOuchSkin.Hide = SkinHide;
		procedure SetTagValue( const ATag: string; const AValue: Variant );
		procedure SetTagState( const ATag: string; const AState: TTagState );
		function GetTagValue( const ATag: string ): Variant;
		function GetTagState( const ATag: string ): TTagState;
		// propiedades
		property TagValue[ const ATag: string ]: Variant read GetTagValue write SetTagValue;
		property TagState[ const ATag: string ]: TTagState read GetTagState write SetTagState;
	end;

	LocalOuchSkinTk = class( OuchSkinTk )
		class function NewSkin( Owner: TComponent ): IOuchSkin; override;
	end;

function OuchSkinDllFn: OuchSkinTkType;

implementation

{$R *.DFM}

function OuchSkinDllFn: OuchSkinTkType;
begin
	result := LocalOuchSkinTk;
end;

{ LocalOuchSkinTk }

class function LocalOuchSkinTk.NewSkin( Owner: TComponent ): IOuchSkin;
begin
	result := TOuchSkinBase.Create( Owner );
end;

procedure TOuchSkinBase.FormCreate(Sender: TObject);
begin
	TaskBar.HideApp := true;
	Taskbar.ShowIcon := true;
end;

procedure TOuchSkinBase.FormDestroy(Sender: TObject);
begin
	Taskbar.ShowIcon := false;
end;

procedure TOuchSkinBase.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
	if Key = VK_ESCAPE then
		SkinHide;
end;

procedure TOuchSkinBase.SkinShow;
begin
	Show();
end;

procedure TOuchSkinBase.SkinHide;
begin
	Hide;
end;

procedure TOuchSkinBase.TaskbarClick(Sender: TObject);
begin
	Show();
end;

procedure TOuchSkinBase.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
	NotifyShutdown;
	fClntList := nil;
	Action := caFree;
end;

procedure TOuchSkinBase.AddListener(const AClient: IUnknown);
begin
	Sv.EventCaster.Add( fClntList, AClient );
end;

procedure TOuchSkinBase.RemoveListener(const AClient: IUnknown);
begin
	Sv.EventCaster.Remove( fClntList, AClient );
end;

procedure TOuchSkinBase.NotifyShutdown;
var
	n: IEnumerator;
	l: IOuchSkinEvents;
begin
	if ( fClntList <> nil ) then
		while fClntList.Enumerate( n, l, IOuchSkinEvents ) do
			l.OnShutDown( self );
end;

procedure TOuchSkinBase.Initialize(const AClientVer: IVersionNumber);
begin
	Caption := Caption + Sil.Os.Version.ToStr( AClientVer, ' (build: %b)' ); 
end;

procedure TOuchSkinBase.SetTagValue(const ATag: string; const AValue: Variant);
var
	comp: TComponent;
begin
	try
		comp := FindTag( ATag );
		if ( comp = nil ) then exit;
		if ( comp is TLabel ) then
			TLabel( comp ).Caption := AValue
		else if ( comp is TButton ) then
			TButton( comp ).Caption := AValue;
	except
	end;
end;

procedure TOuchSkinBase.SetTagState(const ATag: string; const AState: TTagState);
var
	comp: TComponent;
begin
	try
		comp := FindTag( ATag );
		if ( comp = nil ) then exit;
		if ( comp is TLabel ) then
		begin
			TLabel( comp ).Visible := kosStVisible in AState;
			TLabel( comp ).Enabled := kosStEnabled in AState;
		end
		else if ( comp is TButton ) then
		begin
			TButton( comp ).Visible := kosStVisible in AState;
			TButton( comp ).Enabled := kosStEnabled in AState;
		end;
	except
	end;
end;

function TOuchSkinBase.FindTag(const ATag: string): TComponent;
begin
	result := FindComponent( 'lb' + ATag );
	if ( result <> nil ) then exit;
	result := FindComponent( 'bt' + ATag );
	if ( result <> nil ) then exit;
	result := FindComponent( ATag );
	if ( result <> nil ) then exit;
end;

function TOuchSkinBase.GetTagState(const ATag: string): TTagState;
begin
	result := [];
end;

function TOuchSkinBase.GetTagValue(const ATag: string): Variant;
begin
	result := 0;
end;

procedure TOuchSkinBase.Cerrar1Click(Sender: TObject);
begin
	Close;
end;

end.
