unit FmTestBaseListPerformance;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,

  Sil, SilContainer;

type
  TFormTestPerformance = class(TForm)
    btDelphiListAdd: TButton;
    btSilListAdd: TButton;
    btSilListEnum: TButton;
    edLapse1: TEdit;
    edLapse2: TEdit;
    edLapse4: TEdit;
    btDelphiListEnum: TButton;
    edLapse3: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    btSilContainerAdd: TButton;
    btSilContainerEnum: TButton;
    edLapse5: TEdit;
    edLapse6: TEdit;
    procedure btDelphiListAddClick(Sender: TObject);
    procedure btSilListAddClick(Sender: TObject);
    procedure btDelphiListEnumClick(Sender: TObject);
    procedure btSilListEnumClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btSilContainerAddClick(Sender: TObject);
    procedure btSilContainerEnumClick(Sender: TObject);
  private
    FCounter: IPerformanceCounter;
  public
    { Public declarations }
  end;

var
  FormTestPerformance: TFormTestPerformance;

implementation

uses SilLtContainer;

{$R *.dfm}

procedure TFormTestPerformance.FormCreate(Sender: TObject);
begin
  FCounter := Sil.Os.Performance.Create();
end;

procedure TFormTestPerformance.btDelphiListAddClick(Sender: TObject);
var
  I: Integer;
  List: TList;
begin
  List := TList.Create;
  try
    FCounter.Reset;
    for I := 0 to 1000000 do
      List.Add(Pointer(I));

    edLapse1.Text := Sil.Float.ToStr(FCounter.ToMilliseconds());

  finally
    List.Free;
  end;
end;

procedure TFormTestPerformance.btSilListAddClick(Sender: TObject);
var
  I: Integer;
  List: IPointerList;
begin
  List := Sil.List.PointerList();
  try
    FCounter.Reset;
    for I := 0 to 1000000 do
      List.Add(Pointer(I));

    edLapse2.Text := Sil.Float.ToStr(FCounter.ToMilliseconds());

  finally
    List := nil;
  end;
end;

procedure TFormTestPerformance.btDelphiListEnumClick(Sender: TObject);
var
  I: Integer;
  List: TList;
  Item: Pointer;
begin
  List := TList.Create;
  try
    for I := 0 to 1000000 do
      List.Add(Pointer(I));

    FCounter.Reset;
    for I := 0 to List.Count - 1 do
    begin
      Item := List[I];
      if Item = List.Last then
        Beep;
    end;

    edLapse3.Text := Sil.Float.ToStr(FCounter.ToMilliseconds());

  finally
    List.Free;
  end;
end;

procedure TFormTestPerformance.btSilListEnumClick(Sender: TObject);
var
  I: Integer;
  List: IPointerList;
  Enum: IEnumerator;
  Item: Pointer;
begin
  List := Sil.List.PointerList();
  try
    for I := 0 to 1000000 do
      List.Add(Pointer(I));

    FCounter.Reset;
    with List do
      while Enumerate(Enum, Item) do
      begin
        if Item = List.Last then
          Beep;
      end;

    edLapse4.Text := Sil.Float.ToStr(FCounter.ToMilliseconds());

  finally
    List := nil;
  end;
end;

procedure TFormTestPerformance.btSilContainerAddClick(Sender: TObject);
var
  I: Integer;
  List: IContainerDynamic;
begin
  List := SilContainer.Vector.Create(SizeOf(Pointer));
  try
    FCounter.Reset;

    for I := 0 to 1000000 do
      List.Add(@I);

    edLapse5.Text := Sil.Float.ToStr(FCounter.ToMilliseconds());

  finally
    List := nil;
  end;
end;

procedure TFormTestPerformance.btSilContainerEnumClick(Sender: TObject);
var
  I: Integer;
  List: IContainerDynamic;
begin
  List := SilContainer.Vector.Create(SizeOf(Pointer));
  try
    for I := 0 to 1000000 do
      List.Add(@I);

    FCounter.Reset;

    with List.Cursors.First do
      while IsValid do
      begin
        if Item = List.Items.Last  then
          Beep;
        Next;
      end;

    edLapse6.Text := Sil.Float.ToStr(FCounter.ToMilliseconds());

  finally
    List := nil;
  end;
end;

end.
