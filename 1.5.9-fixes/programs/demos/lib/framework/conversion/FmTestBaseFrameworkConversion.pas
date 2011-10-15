unit FmTestBaseFrameworkConversion;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Sil, SilTool, SilBeDataType, SilLiDataType, Grids;

type
  TFormTestConvert = class(TForm)
    cbTarget: TComboBox;
    edValue: TEdit;
    btCanConvert: TButton;
    btConvertNow: TButton;
    cbOrigin: TComboBox;
    edResult: TMemo;
    btBuiidMap: TButton;
    sgMap: TStringGrid;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btCanConvertClick(Sender: TObject);
    procedure btConvertNowClick(Sender: TObject);
    procedure edValueExit(Sender: TObject);
    procedure btBuiidMapClick(Sender: TObject);
    procedure sgMapMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
  private
    FCntr: IPerformanceCounter;
    FTypes: IDataTypeList;
    FMouseCell: TPoint;
  private
    procedure DoSave;
  public
    { Public declarations }
  end;

var
  FormTestConvert: TFormTestConvert;

implementation

uses SilOiPerformance;

{$R *.dfm}

procedure TFormTestConvert.FormCreate(Sender: TObject);
begin
  FCntr := Sil.Os.Performance.Create();
  FTypes := Sil.Typ.List;

  with SilTool.Sv.Configuration.Open('TestConvert.ini', True).Get('Config', True) do
  begin
    edValue.Text := ReadString('Value', '', True);
    cbOrigin.ItemIndex := ReadInteger('Origin', 8, True);
    cbTarget.ItemIndex := ReadInteger('Target', 12, True);
  end;
end;

procedure TFormTestConvert.FormDestroy(Sender: TObject);
begin
  DoSave;
  FTypes := nil;
  FCntr := nil;
end;

procedure TFormTestConvert.btCanConvertClick(Sender: TObject);
var
  Source, Origin, Target, Result: IDataHandler;
  Text: string;
begin
  ASSERT((cbOrigin.ItemIndex >= Ord(Low(TDataType))) and (cbOrigin.ItemIndex <= Ord(High(TDataType))));
  ASSERT((cbTarget.ItemIndex >= Ord(Low(TDataType))) and (cbTarget.ItemIndex <= Ord(High(TDataType))));

  Source := FTypes.Get(dtWideString);
  Origin := FTypes.Get(TDataType(cbOrigin.ItemIndex));
  Target := FTypes.Get(TDataType(cbTarget.ItemIndex));
  Result := FTypes.Get(dtWideString);

  Sil.Str.Add(Text, 'conversión: %-12s -> %-12s = %s', [Source.DataType.Name, Origin.DataType.Name, Sil.Str.IIf(Source.CanConvert(Origin), 'soportada', 'no soportada')], sLineBreak);
  Sil.Str.Add(Text, 'conversión: %-12s -> %-12s = %s', [Origin.DataType.Name, Target.DataType.Name, Sil.Str.IIf(Origin.CanConvert(Target), 'soportada', 'no soportada')], sLineBreak);
  Sil.Str.Add(Text, 'conversión: %-12s -> %-12s = %s', [Target.DataType.Name, Result.DataType.Name, Sil.Str.IIf(Target.CanConvert(Result), 'soportada', 'no soportada')], sLineBreak);

  edResult.Visible := True;
  sgMap.Visible := False;
  edResult.Text := Text;
  DoSave;
end;

procedure TFormTestConvert.btConvertNowClick(Sender: TObject);
var
  Value: WideString;
  Size: LongWord;
  Time: Double;
  Source, Origin, Target, Result: IDataHandler;
  Status: TDataTypecastStatus;
  OriginBuffer: array[0 .. 1024] of Byte;
  TargetBuffer: array[0 .. 1024] of Byte;
  Text: string;
begin
  ASSERT((cbOrigin.ItemIndex >= Ord(Low(TDataType))) and (cbOrigin.ItemIndex <= Ord(High(TDataType))));
  ASSERT((cbTarget.ItemIndex >= Ord(Low(TDataType))) and (cbTarget.ItemIndex <= Ord(High(TDataType))));

  Source := FTypes.Get(dtWideString);
  Origin := FTypes.Get(TDataType(cbOrigin.ItemIndex));
  Target := FTypes.Get(TDataType(cbTarget.ItemIndex));
  Result := FTypes.Get(dtWideString);

  Origin.Handler.Initialize(@OriginBuffer);
  try
    Value := edValue.Text;

    FCntr.Reset;
    Status := Origin.Assign(Source, @Value, @OriginBuffer, SizeOf(Value), SizeOf(OriginBuffer), nil, @Size);
    Time := FCntr.ToMilliseconds();
    
    Sil.Str.Add(Text, 'conversión: %-12s -> %-12s = %s [%8.4f mS]', [Source.DataType.Name, Origin.DataType.Name, Sil.Enum.Name(TypeInfo(TDataTypecastStatus), Ord(Status), 'tc'), Time], sLineBreak);
    Status := tcSOk;
    Target.Handler.Initialize(@TargetBuffer);
    try
      FCntr.Reset;
      Status := Origin.Convert(Target, @OriginBuffer, @TargetBuffer, SizeOf(OriginBuffer), SizeOf(TargetBuffer), nil, @Size, Status);
      Time := FCntr.ToMilliseconds();
      Sil.Str.Add(Text, 'conversión: %-12s -> %-12s = %s [%8.4f mS]', [Origin.DataType.Name, Target.DataType.Name, Sil.Enum.Name(TypeInfo(TDataTypecastStatus), Ord(Status), 'tc'), Time], sLineBreak);
      Status := tcSOk;
      Value := '';
      FCntr.Reset;
      Status := Target.Cast.WideString(@TargetBuffer, @Value, SizeOf(TargetBuffer), SizeOf(Value), nil, @Size, Status);
      Time := FCntr.ToMilliseconds();
      Sil.Str.Add(Text, 'conversión: %-12s -> %-12s = %s [%8.4f mS]', [Target.DataType.Name, Result.DataType.Name, Sil.Enum.Name(TypeInfo(TDataTypecastStatus), Ord(Status), 'tc'), Time], sLineBreak);
      Caption := Sil.Enum.Name(TypeInfo(TDataTypecastStatus), Ord(Status), 'tc');

      Sil.Str.Add(Text, 'Value = %s', [Value], sLineBreak);
      edResult.Text := Text;

    finally
      Target.Handler.Finalize(@TargetBuffer);
    end;

  finally
    Origin.Handler.Finalize(@OriginBuffer);
  end;

  edResult.Visible := True;
  sgMap.Visible := False;

  DoSave;
end;

procedure TFormTestConvert.DoSave;
begin
  with SilTool.Sv.Configuration.Open('TestConvert.ini', True).Get('Config', True) do
  begin
    WriteString('Value', edValue.Text);
    WriteInteger('Origin', cbOrigin.ItemIndex);
    WriteInteger('Target', cbTarget.ItemIndex);
  end;
end;

procedure TFormTestConvert.edValueExit(Sender: TObject);
begin
  DoSave;
end;

procedure TFormTestConvert.btBuiidMapClick(Sender: TObject);
var
  Origin, Destination: TDataType;
  Supported: Boolean;
begin
  sgMap.ColCount := 1 + Ord(High(Destination)) - Ord(Low(Destination)) + 1;
  sgMap.RowCount := 1 + Ord(High(Origin)) - Ord(Low(Origin)) + 1;
  
  for Destination := Low(Destination) to High(Destination) do
    sgMap.Cells[1 + Ord(Destination), 0] := Sil.Enum.Name(TypeInfo(TDataType), Ord(Destination), 'dt');
    
  for Origin := Low(Origin) to High(Origin) do
  begin
    sgMap.Cells[0, 1 + Ord(Origin)] := Sil.Enum.Name(TypeInfo(TDataType), Ord(Origin), 'dt');    
    for Destination := Low(Destination) to High(Destination) do
    begin
      Supported := FTypes.Get(Origin).CanConvert(Destination);
      sgMap.Cells[1 + Ord(Destination), 1 + Ord(Origin)] := Sil.Str.Iif(Supported, 'S', 'N');
    end;
  end;

  edResult.Visible := False;
  sgMap.Visible := True;
end;

procedure TFormTestConvert.sgMapMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  sgMap.MouseToCell(X, Y, FMouseCell.X, FMouseCell.Y);
end;

end.
