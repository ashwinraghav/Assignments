unit FrOuchEvents;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, VirtualTrees,

  Sil, SilVCL, UiOuch;

type
  TFrameOuchEvents = class(
    TFrame,
    IOuchView,
    IOuchEventView )
    vtList: TVirtualStringTree;
    procedure vtListInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure vtListFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vtListGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: WideString);
    procedure vtListCompareNodes(Sender: TBaseVirtualTree; Node1,
      Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure vtListHeaderClick(Sender: TVTHeader; Column: TColumnIndex;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  private
    FApplication: IOuchApplication;
  private
    procedure DoSort;
  protected // IOuchView
    procedure BeginUpdate;
    procedure EndUpdate;
  protected // IOuchEventView
    procedure Add(const Message: string; const Operation: string = ''); overload;
    procedure Add(const Message: string; const Args: array of const; const Operation: string = ''); overload;
  public
    constructor Create(Owner: TComponent); overload; override;
    class function Create(Owner: TComponent; Parent: TWinControl): IOuchEventView; reintroduce; overload; 
    destructor Destroy; override;
    procedure Loaded; override;
  end;

implementation

uses SilBtDateTime;

{$R *.dfm}


const
  cxTimeStamp = 0;
  cxOperation = 1;
  cxMessage   = 2;

type
  PEvent = ^REvent;
  REvent = record
    TimeStamp: TDateTime;
    Message: string;
    Operation: string;
  end;

{ TFrameOuchEvents }

class function TFrameOuchEvents.Create(Owner: TComponent; Parent: TWinControl): IOuchEventView;
var
  Instance: TFrameOuchEvents;
begin
  Instance := Create(Owner);
  Instance.Parent := Parent;
  Instance.Visible := True;
  Result := Instance;
end;

constructor TFrameOuchEvents.Create(Owner: TComponent);
begin
  inherited;
  Owner.GetInterface(IOuchApplication, FApplication);
  VCL.ComObj.Make(Self);
end;

destructor TFrameOuchEvents.Destroy;
begin
  VCL.ComObj.Free(Self);
  FApplication := nil;
  inherited;
end;

procedure TFrameOuchEvents.Loaded;
begin
  inherited;
  vtList.NodeDataSize := SizeOf(REvent);
end;

procedure TFrameOuchEvents.Add(const Message, Operation: string);
var
  Node: PVirtualNode;
  Data: PEvent;
begin
  Node := vtList.AddChild(nil);
  vtList.ReinitNode(Node, False);
  Data := vtList.GetNodeData(Node);
  Data.TimeStamp := Sil.DateTime.Now;
  Data.Message := Message;
  Data.Operation := Operation;
  vtList.InvalidateNode(Node);
  DoSort;
end;

procedure TFrameOuchEvents.Add(const Message: string; const Args: array of const; const Operation: string);
begin
  Add(Sil.Str.Format(Message, Args), Operation);
end;

procedure TFrameOuchEvents.BeginUpdate;
begin
  vtList.BeginUpdate;
end;

procedure TFrameOuchEvents.EndUpdate;
begin
  vtList.EndUpdate;
  vtList.Invalidate;
end;

procedure TFrameOuchEvents.vtListInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var
  Data: PEvent;
begin
  Data := Sender.GetNodeData(Node);
  if Assigned(Data) then System.Initialize(Data^);
end;

procedure TFrameOuchEvents.vtListFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  Data: PEvent;
begin
  Data := Sender.GetNodeData(Node);
  if Assigned(Data) then System.Finalize(Data^);
end;

procedure TFrameOuchEvents.vtListGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
var
  Data: PEvent;
begin
  Data := Sender.GetNodeData(Node);
  case Column of
    cxTimeStamp:  CellText := Sil.DateTime.ToStr(Data.TimeStamp, 'yyyy-mm-dd hh:nn');
    cxOperation:  CellText := Data.Operation;
    cxMessage:    CellText := Data.Message;
  end;
end;

procedure TFrameOuchEvents.vtListCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var
  Data1, Data2: PEvent;
begin
  Data1 := Sender.GetNodeData(Node1);
  Data2 := Sender.GetNodeData(Node2);
  if Assigned(Data1) and Assigned(Data2) then
    case Column of
      cxTimeStamp:  Result := Sil.DateTime.ToMilisecs(Data1.TimeStamp) - Sil.DateTime.ToMilisecs(Data2.TimeStamp);
      cxOperation:  Result := Sil.Str.CompareText(Data1.Operation, Data2.Operation, True);
      cxMessage:    Result := Sil.Str.CompareText(Data1.Message, Data2.Message, True);
    end;
end;

procedure TFrameOuchEvents.vtListHeaderClick(Sender: TVTHeader; Column: TColumnIndex; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
const
  Direction: array[Boolean] of TSortDirection = (sdAscending, sdDescending);
begin
  Sender.SortColumn := Column;
  Sender.SortDirection := Direction[Sender.SortDirection = sdAscending];
  DoSort;
end;

procedure TFrameOuchEvents.DoSort;
begin
  vtList.SortTree(vtList.Header.SortColumn, vtList.Header.SortDirection);
end;

end.
