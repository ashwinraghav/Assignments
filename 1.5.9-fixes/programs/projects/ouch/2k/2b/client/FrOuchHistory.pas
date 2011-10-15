unit FrOuchHistory;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, VirtualTrees, ExtCtrls,

  Sil, SilVcl, SilData, UiOuch, DB;

type
  TFrameOuchHistory = class(
    TFrame,
    IOuchView )
    vtList: TVirtualStringTree;
    procedure vtListGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
    procedure vtListInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure vtListFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
  private
    FApplication: IOuchApplication;
    FAccount: IOuchAccount;
    FHistory: IOuchDbHistory;
  private
    function DoGetMessages(const Account: IOuchAccount): IDataRowset;
    procedure DoClearMessages;
    procedure DoRefreshMessages;
    procedure DoOpenHistory(const Account: IOuchAccount);
    procedure DoCloseHistory;
    procedure DoSetupColumns;
    procedure DoSetupData;
  protected // IOuchView
    procedure BeginUpdate;
    procedure EndUpdate;
  protected
    procedure SetAccount(const Value: IOuchAccount);
  public
    class function Create(Owner: TComponent; Parent: TWinControl; const Account: IOuchAccount): TFrameOuchHistory; reintroduce; overload;
    constructor Create(Owner: TComponent); overload; override;
    destructor Destroy; override;
    procedure Loaded; override;
    property Account: IOuchAccount read FAccount write SetAccount;
  end;

implementation

uses DmDataHistory;

{$R *.dfm}

type
  PDataItem = ^TDataItem;
  TDataItem = record
    RecNo: LongWord;
    dummy: string;
  end;

{ TFrameOuchHistory }

class function TFrameOuchHistory.Create(Owner: TComponent; Parent: TWinControl; const Account: IOuchAccount): TFrameOuchHistory;
var
  Instance: TFrameOuchHistory;
begin
  Instance := Create(Owner);
  Instance.Parent := Parent;
  Instance.Visible := True;
  Instance.Account := Account;
  Result := Instance;
end;

constructor TFrameOuchHistory.Create(Owner: TComponent);
begin
  inherited;
  FApplication := Owner as IOuchApplication;
end;

destructor TFrameOuchHistory.Destroy;
begin
  FApplication := nil;
  inherited;
end;

procedure TFrameOuchHistory.vtListInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var
  Data: PDataItem;
begin
  Data := Sender.GetNodeData(Node);
  System.Initialize(Data^);
end;

procedure TFrameOuchHistory.vtListFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  Data: PDataItem;
begin
  Data := Sender.GetNodeData(Node);
  System.Finalize(Data^);
end;

procedure TFrameOuchHistory.vtListGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
var
  ColumnItem: TVirtualTreeColumn;
begin
  if Assigned(FHistory) then
  begin
    FHistory.DataSet.RecNo := Node.Index + 1;
    with vtList.Header.Columns do
    begin
      if IsValidColumn(Column) then
      begin
        ColumnItem := Items[Column];
        CellText := FHistory.DataSet.Fields[ColumnItem.Tag].DisplayText
      end else
        CellText := '';
    end;
  end else
    CellText := '';
end;

procedure TFrameOuchHistory.BeginUpdate;
begin
  vtList.BeginUpdate;
end;

procedure TFrameOuchHistory.EndUpdate;
begin
  vtList.EndUpdate;
  vtList.Invalidate;
end;

procedure TFrameOuchHistory.SetAccount(const Value: IOuchAccount);
begin
  BeginUpdate;
  try
    if Assigned(FHistory) then DoCloseHistory;
    FAccount := Value;
    if Assigned(FAccount)    then DoOpenHistory(FAccount);
  finally
    EndUpdate;
  end;
end;

procedure TFrameOuchHistory.Loaded;
begin
  inherited;
  vtList.NodeDataSize := SizeOf(TDataItem);
end;

procedure TFrameOuchHistory.DoOpenHistory(const Account: IOuchAccount);
var
  Rowset: IDataRowset;
begin
  Rowset := DoGetMessages(Account);
  if Assigned(Rowset) then
  begin
    FHistory := TOuchDbHistory.Create(nil, Rowset);
    DoRefreshMessages;
  end;
end;

procedure TFrameOuchHistory.DoCloseHistory;
begin
  DoClearMessages;
  FHistory := nil;
end;

function TFrameOuchHistory.DoGetMessages(const Account: IOuchAccount): IDataRowset;
begin
  if not Assigned(Account) or not FApplication.Engine.Client.OpenHistory(Account.ID, Result) then
    Result := nil;
end;

procedure TFrameOuchHistory.DoClearMessages;
begin
  vtList.Header.Columns.Clear;
  vtList.Clear;
end;

procedure TFrameOuchHistory.DoRefreshMessages;
begin
  DoSetupColumns;
  DoSetupData;
end;

procedure TFrameOuchHistory.DoSetupColumns;
var
  Item: TField;
  Index: Integer;
begin
  with FHistory.Dataset.Fields do
    for Index := 0 to Count - 1 do
    begin
      Item := Fields[Index];
      if Item.Visible then
        with vtList.Header do
          with Columns.Add do
          begin
            Text := Item.DisplayName;
            Width := Item.DisplayWidth * 8;
            Tag := Item.Index;
          end;
    end;
end;

procedure TFrameOuchHistory.DoSetupData;
begin
  vtList.RootNodeCount := FHistory.DataSet.RecordCount;
  vtList.ReinitNode(vtList.RootNode, True);
end;

end.
