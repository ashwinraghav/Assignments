unit DmTrayBar;

interface

uses
  Classes, CoolTrayIcon, Menus, ImgList, Controls, 

  Sil, SilVCL, UiOuch;

type
  TDataTrayBar = class(
    TDataModule,
    IOuchTraybar )
    tiMain: TCoolTrayIcon;
    pmTray: TPopupMenu;
    imTray: TImageList;
    miClose: TMenuItem;
    miActivar: TMenuItem;
    tiMsg: TCoolTrayIcon;
    imMsg: TImageList;
    procedure TraybarCreate(Sender: TObject);
    procedure TraybarDestroy(Sender: TObject);
  private
    FApplication: IOuchApplication;
  protected // IOuchTraybar
    procedure OnReceived(const Msg: IOuchMessage); 
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
  public 
    property Application: IOuchApplication read FApplication;
  end;

implementation

{$R *.dfm}

{ TDataTrayBar }

constructor TDataTrayBar.Create(Owner: TComponent);
begin
  inherited Create(nil);
  VCL.ComObj.Make(Self);
  FApplication := Owner as IOuchApplication;
end;

destructor TDataTrayBar.Destroy;
begin
  FApplication := nil;
  VCL.ComObj.Free(Self);
  inherited;
end;

procedure TDataTrayBar.TraybarCreate(Sender: TObject);
begin
  with Sil.Os.Module.Current.Info.Version do
    tiMain.Hint := Tags.Std.ProductName + ' (' + Number.ToStr(CLongVersion) + ')';
    
  tiMain.ShowHint := True;
  tiMain.IconIndex := 20;
  tiMain.IconVisible := True;
end;

procedure TDataTrayBar.TraybarDestroy(Sender: TObject);
begin
  tiMain.IconVisible := False;
end;

procedure TDataTrayBar.OnReceived(const Msg: IOuchMessage);
begin

end;

end.
