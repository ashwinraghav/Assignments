unit FmTestBaseListParameters;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls,

  Sil;

type
  TFormTestParameters = class(TForm)
    ParamAdd: TButton;
    Enumerate: TButton;
    Server: TButton;
    Local: TButton;
    Display: TMemo;
    ParamMerge: TButton;
    ParamLookup: TButton;
    ParamClear: TButton;
    ParamCreate: TButton;
    procedure ParamDefineClick(Sender: TObject);
    procedure EnumerateClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SliceClick(Sender: TObject);
    procedure ParamLookupClick(Sender: TObject);
    procedure ParamMergeClick(Sender: TObject);
    procedure ParamClearClick(Sender: TObject);
    procedure ParamCreateClick(Sender: TObject);
  private
    FParams: IParameterList;
  private
    procedure DoEnumerate(const List: IParameters);
  end;

var
  FormTestParameters: TFormTestParameters;

implementation

uses
  SilLfParameters;

{$R *.DFM}

procedure TFormTestParameters.FormCreate(Sender: TObject);
begin
  FParams := Sil.List.Parameters();
end;

procedure TFormTestParameters.ParamDefineClick(Sender: TObject);
begin
  FParams['Server.Port'] := 1234;
  FParams['Server.Address'] := '127.0.0.1';
  FParams['Server.ReadTimeout'] := 1000;
  FParams['Local.Port'] := '1243';
  FParams['Local.Name'] := 'prueba';

  DoEnumerate(FParams);
end;

procedure TFormTestParameters.ParamCreateClick(Sender: TObject);
begin
  with Sil.List.Params do
    FParams := Create([
        Void('Params.Void'),
        Float('Params.Float', 1.4555),
        AnsiString('Params.String', 'this is a string'),
        LongWord('Params.Cardinal', $86776555),
        Integer('Params.Integer', -2),
        WideString('Params.WideString', 'this is a wide string'),
        Boolean('Params.Boolean', True),
        LargeInt('Params.Largeint', $1234567812345678),
        Datetime('Params.Datetime', Sil.DateTime.Now),
        Variant('Params.Variant', Sil.Vart.FromDate(Sil.DateTime.Now))
      ]);

  DoEnumerate(FParams);
end;

procedure TFormTestParameters.EnumerateClick(Sender: TObject);
begin
  DoEnumerate(FParams);
end;

procedure TFormTestParameters.SliceClick(Sender: TObject);
var
  Subparams: IParameters;
begin
  if (Sender is TButton) then
  begin
    Subparams := FParams.Slice(TButton(Sender).Name);
    DoEnumerate(Subparams);
  end;
end;

procedure TFormTestParameters.ParamLookupClick(Sender: TObject);
var
  Server: IParameters;
begin
  Display.Lines.Clear;

  Display.Lines.Add('specifying full parameter name: ');
  Display.Lines.Add(Sil.Str.Format('  Server.Address = %s', [Sil.Vart.ToStr(FParams['Server.Address'])]));

  Display.Lines.Add('slicing Server first: ');
  Server := FParams.Slice('Server');
  Display.Lines.Add(Sil.Str.Format('  Port = %s', [Sil.Vart.ToStr(Server['Port'])]));
end;

procedure TFormTestParameters.ParamMergeClick(Sender: TObject);
var
  Params: IParameterList;
begin
  Params := Sil.List.Parameters();

  Params['Server.WriteTimeout'] := 3000;
  Params['Server.Priority'] := 'High';
  Params['Local.SessionPool.Limit'] := 10;

  FParams.Merge(Params, mkSource);

  DoEnumerate(FParams);
end;

procedure TFormTestParameters.ParamClearClick(Sender: TObject);
begin
  FParams.Clear;
  DoEnumerate(FParams);
end;

procedure TFormTestParameters.DoEnumerate(const List: IParameters);
var
  Enum: IEnumerator;
  Item: RParameter;
begin
  Display.Lines.Clear;
  with List do
    while Enumerate(Enum, Item) do
      Display.Lines.Add(Item.Name + '=' + Sil.Vart.ToStr(Item.Value));
end;

end.
