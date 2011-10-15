unit DmDataHistory;

interface

uses
  Classes, DB,
  Sil, SilVcl, SilData, SilVmDataRowset, UiOuch;

type
  TOuchDbHistory = class(
    TDataModule,
    IOuchDataset,
    IOuchDbHistory )
    sdData: TSilDataset;
    fdId: TIntegerField;
    fdSndTime: TDateTimeField;
    fdRcvTime: TDateTimeField;
    fdTo: TGuidField;
    fdKind: TStringField;
    fdStatus: TStringField;
    fdFrom: TGuidField;
    fdText: TStringField;
  private
    function DoGetCursor: IDataRowset;
    procedure DoSetCursor(const Value: IDataRowset);
  protected // IOuchDbRowset
    function GetDataSet: TDataSet;
  protected // IOuchDbHistory
  public
    class function Create(Owner: TComponent; const Cursor: IDataRowset): IOuchDbHistory; reintroduce; overload;
    constructor Create(Owner: TComponent); overload; override;
    destructor Destroy; override;
  public 
    property Cursor: IDataRowset read DoGetCursor write DoSetCursor;
  end;

implementation

{$R *.dfm}

(*)
    CreateItem('id', ftLongWord);
    CreateItem('sndtime', ftDateTime);
    CreateItem('rcvtime', ftDateTime);
    CreateItem('from', ftGuid);
    CreateItem('to', ftGuid);
    CreateItem('kind', ftString, 2);
    CreateItem('status', ftString, 2);
    CreateItem('text', ftMemo);
(*)

{ TDataHistory }

class function TOuchDbHistory.Create(Owner: TComponent; const Cursor: IDataRowset): IOuchDbHistory;
var
  Instance: TOuchDbHistory;
begin
  Instance := Create(Owner);
  Instance.Cursor := Cursor;
  Result := Instance;
end;

constructor TOuchDbHistory.Create(Owner: TComponent);
begin
  inherited Create(nil);
  VCL.ComObj.Make(Self);
end;

destructor TOuchDbHistory.Destroy;
begin
  VCL.ComObj.Free(Self);
  inherited;
end;

function TOuchDbHistory.GetDataSet: TDataSet;
begin
  Result := sdData;
end;

function TOuchDbHistory.DoGetCursor: IDataRowset;
begin
  Result := sdData.Cursor;
end;

procedure TOuchDbHistory.DoSetCursor(const Value: IDataRowset);
begin
  if Assigned(Value) then
  begin
    sdData.DisableControls;
    try
      sdData.Cursor := Value;
    finally
      sdData.EnableControls;
    end;
  end;
end;

end.
