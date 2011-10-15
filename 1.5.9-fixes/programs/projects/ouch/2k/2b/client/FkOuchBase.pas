unit FkOuchBase;

interface

uses
  Windows,

  Sil, SilVCL, Classes, Forms, Controls,

  UiOuch;

type
  TFormOuchBaseClass = class of TFormOuchBase;
  TFormOuchBase = class(
    TForm,
    IDispatchable,
    IOuchWindow,
    IOuchStreamable )
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    FApplication: IOuchApplication;
    FList: IOuchWindowList;
  private
    function DoCheckProfile: Boolean;
  protected // IOuchWindow
    function GetInstance: TComponent;
  protected // IOuchStreamable
    procedure Load(const Reader: IOuchProfileReader); virtual;
    procedure Store(const Writer: IOuchProfileWriter); virtual;
  public
    constructor Create(AOwner: TComponent); overload; override;
    destructor Destroy; override;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    procedure FreeInstance; override;
    class function Create(AOwner: TComponent; const List: IOuchWindowList): TFormOuchBase; reintroduce; overload;
  public
    property Application: IOuchApplication read FApplication;
  end;

implementation

uses
  UtOuch;

{$R *.dfm}

{ TFormOuchBase }

class function TFormOuchBase.Create(AOwner: TComponent; const List: IOuchWindowList): TFormOuchBase;
begin
  Result := TFormOuchBase(Self.NewInstance());
  Result.FApplication := AOwner as IOuchApplication;
  Result.FList := List;
  Result.Create(AOwner);
end;

procedure TFormOuchBase.FreeInstance;
begin
  FApplication := nil;
  if Assigned(FList) then
    FList.Remove(Self); // no la destruye porque los forms no cuentan refs
  FList := nil;
  inherited;
end;

constructor TFormOuchBase.Create(AOwner: TComponent);
begin
  inherited;
  VCL.ComObj.Make(Self, False);
  if Assigned(FList) then FList.Add(Self);
end;

destructor TFormOuchBase.Destroy;
begin
  VCL.ComObj.Free(Self);
  inherited;
end;

procedure TFormOuchBase.AfterConstruction;
begin
  inherited;
  if DoCheckProfile() then FApplication.Engine.Profile.Load(Self);
end;

procedure TFormOuchBase.BeforeDestruction;
begin
  if DoCheckProfile() then FApplication.Engine.Profile.Store(Self);
  inherited;
end;

procedure TFormOuchBase.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

function TFormOuchBase.GetInstance: TComponent;
begin
  Result := Self;
end;

procedure TFormOuchBase.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
  begin
    ModalResult := mrCancel;
    Close;
  end;
end;

procedure TFormOuchBase.Load(const Reader: IOuchProfileReader);
begin

  if not Reader.ReadProperty(Self, 'Left') or not Reader.ReadProperty(Self, 'Top') then
    Position := poScreenCenter;

  Reader.ReadProperty(Self, 'Height');
  Reader.ReadProperty(Self, 'Width');
end;

procedure TFormOuchBase.Store(const Writer: IOuchProfileWriter);
begin
  Writer.WriteProperty(Self, 'Left');
  Writer.WriteProperty(Self, 'Top');
  Writer.WriteProperty(Self, 'Height');
  Writer.WriteProperty(Self, 'Width');
end;

function TFormOuchBase.DoCheckProfile: Boolean;
begin
  Result := Assigned(FApplication)
        and Assigned(FApplication.Engine)
        and Assigned(FApplication.Engine.Profile);
end;

end.
