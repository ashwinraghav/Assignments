unit FmTestInvoke;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Sil, UiTest, StdCtrls, SilBeTypeInfo, SilLiTypeInfo;

type
  IPrueba = interface
    ['{13AF0470-F339-4E65-8528-6E1698361B08}']
    function GetName: string; stdcall;
    function GetValue: Double; stdcall;
  end;


  TFormTestInvoke = class(
    TForm,
    IPrueba )
    Test: TButton;
    cbProperties: TComboBox;
    edValue: TEdit;
    edResult: TEdit;
    Invoke: TButton;
    cbMethods: TComboBox;
    stParams: TLabel;
    edArguments: TEdit;
    Button1: TButton;
    Button2: TButton;
    procedure TestClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure cbMethodsSelect(Sender: TObject);
    procedure InvokeClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  protected
    function GetName: string; stdcall;
    function GetValue: Double; stdcall;
  end;

var
  FormTestInvoke: TFormTestInvoke;

implementation

uses
  SilLfDelphiInvoke,
  SilLeDelphiTypeInfo,
  SilLmDelphiTypeInfo,
  UmTest;

{$R *.dfm}

{ TFormTestInvoke }

procedure TFormTestInvoke.FormCreate(Sender: TObject);
var
  Enum: IEnumerator;
  Method: ITypeMethod;
  Def: ITypeInfo;
  sName: string;
begin
  cbProperties.Clear;
  cbMethods.Clear;

  Def := TSilDelphiTypeDef.Create(TypeInfo(ITest));

  with Def.Data.AsInterface do
    with Methods do
      while Enumerate(Enum, Method) do
      begin
        sName := Method.Name;
        cbMethods.Items.Add(Method.Name);
        if (Sil.Text.Compare(sName, 'get', 3) = 0) or (Sil.Text.Compare(sName, 'set', 3) = 0) then
        begin
          sName := Str.Extract(sName, 4);        
          if cbProperties.Items.IndexOf(sName) = -1 then
            cbProperties.Items.Add(sName);
        end;
      end;

end;

procedure TFormTestInvoke.TestClick(Sender: TObject);
var
  Def: ITypeInfo;
  Test: ITest;
  Value: Extended;
begin
  Def := TSilDelphiTypeDef.Create(TypeInfo(ITest));

  Test := TTest.Create(Str.ToFloat(edValue.Text)) ;
  Value := GetProperty(Test, Def.Data.AsInterface, cbProperties.Text);

  edResult.Text := Float.ToStr(Value);

end;

procedure TFormTestInvoke.cbMethodsSelect(Sender: TObject);
var
  Def: ITypeInfo;
  Methods: ITypeMethods;
  Method: ITypeMethod;
  Text: string;
  Enum: IEnumerator;
  Param: ITypeParam;
begin
  Def := TSilDelphiTypeDef.Create(TypeInfo(ITest));

  Methods := Def.Data.AsInterface.Methods;
  Text := '';
  if Methods.Find(cbMethods.Text, Method) then
  begin
    with Method.Params do
      while Enumerate(Enum, Param) do
      begin
        Str.Add(Text, '', '; ');
        if paOut in Param.Flags then
          Str.Add(Text, 'out ')
        else if paConst in Param.Flags then
          Str.Add(Text, 'const ')
        else if paVar in Param.Flags then
          Str.Add(Text, 'var ');

        Str.Add(Text, Param.ParamName);
        Str.Add(Text, ': ');
        if paArray in Param.Flags then
          Str.Add(Text, 'array of ');
        Str.Add(Text, Param.TypeName);
      end;
    if Str.NotEmpty(Text) then Text := '(' + Text + ')';
    Text := Method.Name + Text;
    if Method.MethodKind = mkFunction then
    begin
      Param := Method.Result;
      Str.Add(Text, Param.TypeName, ': ');
    end;
  end;
  stParams.Caption := Text;
end;

procedure TFormTestInvoke.InvokeClick(Sender: TObject);
var
  Def: ITypeInfo;
  Methods: ITypeMethods;
  Method: ITypeMethod;
  Params: ITypeParams;
  Param: ITypeParam;
  Text, Arg: string;
  Args: array of TVarRec;
  Index, Count, Pos: Integer;
  Data: array[0 .. 255] of Pointer;
  Test: ITest;
  DataPtr: PPointer;
  Value: Variant;
begin
  Def := TSilDelphiTypeDef.Create(TypeInfo(ITest));

  Test := TTest.Create(Str.ToFloat(edValue.Text)) ;
  Methods := Def.Data.AsInterface.Methods;

  if Methods.Find(cbMethods.Text, Method) then
  begin
    Params := Method.Params;
    Count := Params.Count - 1;

    if Method.MethodKind = mkFunction then
      Inc(Count);

    SetLength(Args, Count);
    Text := edArguments.Text;
    Pos := 0;
    DataPtr := @Data;
    
    for Index := Low(Args) to High(Args) do
    begin
      if Index < Params.Count - 1 then
        Param := Params[Index + 1] else
        Param := Method.Result;
        
      if (Method.MethodKind <> mkFunction) or (Index <> High(Args)) or (Param.Flags * [paOut, paVar] = []) then
      begin
        if Str.Enumerate(Text, ';', Arg, Pos) then
          with Args[Index] do
          begin
            VType := vtAnsiString;
            string(VString) := Arg;
          end;
      end else
      begin
        with Args[Index] do
        begin
          VType := vtPointer;
          VPointer := DataPtr;
          Inc(DataPtr);
        end;
      end;
    end;        

    InvokeMethod(Test, Def.Data.AsInterface, Method.Name, Args, @Value);
  end;
end;

procedure TFormTestInvoke.Button1Click(Sender: TObject);
var
  Prueba: IPrueba;
  Name: string;
  Value: Double;
begin
  Prueba := Self;
  Name := Prueba.GetName;
  Value := Prueba.GetValue;
end;

function TFormTestInvoke.GetName: string;
begin
  Result := Caption;
end;

function TFormTestInvoke.GetValue: Double;
begin
  Result := Left * 100.6;
end;

end.
