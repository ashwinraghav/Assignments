unit FmRowset;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, Sil, SilSiFirebird;

type
  TFormRowset = class(TForm)
    lvList: TListView;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    FCursor: IFbCursor;
    FCounter: IPerformanceCounter;
  protected
    procedure DoFill(const Cursor: IFbCursor);
    procedure DoAddColumns(const Cursor: IFbCursor);
    procedure DoAddFields(Index: Integer; const Cursor: IFbCursor);
  public
    class procedure Show(const Cursor: IFbCursor; Owner: TComponent = nil; OnClose: TNotifyEvent = nil); overload;
    constructor Create(Owner: TComponent); override;  
    destructor Destroy; override; 
  end;

implementation

{$R *.dfm}

{ TForm2 }

constructor TFormRowset.Create(Owner: TComponent);
begin
  inherited;
  FCounter := Sil.Os.Performance.Create;
end;

destructor TFormRowset.Destroy;
begin
  FCursor := nil;
  inherited;
end;

procedure TFormRowset.DoFill(const Cursor: IFbCursor);
var
  Index: Integer;
begin
  Index := 0;
  FCursor := Cursor;
  DoAddColumns(Cursor);
  FCounter.Reset;
  while Cursor.Fetch do
  begin
    DoAddFields(Index, Cursor);
    Inc(Index);
  end;
  Caption := 'Total: ' + Sil.Int.ToStr(Index) + ' Time: ' + Sil.Float.ToStr(FCounter.ToSeconds()) + ' sec';
end;

class procedure TFormRowset.Show(const Cursor: IFbCursor; Owner: TComponent; OnClose: TNotifyEvent);
var
  Instance: TFormRowset;
begin
  Instance := Create(Owner); 
  Instance.DoFill(Cursor);
  Instance.OnDestroy := OnClose;
  Instance.Show;
end;

procedure TFormRowset.DoAddFields(Index: Integer; const Cursor: IFbCursor);
var
  Item: TListItem;
  Enum: IEnumerator;
  Data: IFbData;
begin
  Item := lvList.Items.Add;
  Item.Caption := Sil.Int.ToStr(Index);
  with Cursor.Fields do
    while Enumerate(Enum, Data) do
      with Data do
        if not IsNull then
          Item.SubItems.Add(AnsiString.Value) else
          Item.SubItems.Add('(NULL)');
end;

procedure TFormRowset.DoAddColumns(const Cursor: IFbCursor);
var
  Enum: IEnumerator;
  Item: IFbBinding;
begin
  lvList.Columns.Add.Caption := '#';
  with Cursor.Bindings do
    while Enumerate(Enum, Item) do
      with lvList.Columns.Add do
        Caption := Item.Name;
end;

procedure TFormRowset.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TFormRowset.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_ESCAPE: Close;
    else Exit;
  end;
  Key := 0;
end;

end.
