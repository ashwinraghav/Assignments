unit FmTestMain;

interface

{$INCLUDE Defines.inc}

uses
  Classes, Controls, Forms, StdCtrls, Sil,
  
  SilBeTypeInfo, SilLiTypeInfo;

type
  PChunk = ^TChunk;
  PData = ^IInvokable;

  {$M+}
  TChunk = class
  private
    FName: string;
    FValue: Variant;
    procedure SetName(const Value: string);
    procedure SetValue(const Value: Variant);
  published
    property Name: string read FName write SetName;
    property Value: Variant read FValue write SetValue;
  end;
  {$M-}

  IObject = interface(IInvokable)
    ['{66FC8CE4-2FDE-41D5-8CF8-4D18FB6ADCCF}']
    function GetX: Integer; stdcall;
    function GetY: Integer; stdcall;
    procedure SetY(Value: Integer); stdcall;
    function Enabled(const Dato: String): Boolean; stdcall;
    procedure Reset; overload; stdcall;
    procedure Assign(const Values: array of string); stdcall;
    property X: Integer read GetX;
    property Y: Integer read GetY write SetY;
  end;

  TForm1 = class(TForm, IObject)
    Memo: TMemo;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  protected
    function GetX: Integer; stdcall;
    function GetY: Integer; stdcall;
    procedure SetY(Value: Integer); stdcall;
    function Enabled(const Dato: String): Boolean; stdcall;
    procedure Reset; overload; stdcall;
    procedure Assign(const Values: array of string); reintroduce; stdcall;
  end;

var
  Form1: TForm1;

implementation

uses
  SilLfDelphiInvoke,
//  SilLfDynarray,
  SilLeDelphiTypeInfo,
  SilLmDelphiTypeInfo;

{$R *.dfm}

const
  MethodStr : array[TMethodKind] of string =
    ( 'procedure',
      'function',
      'constructor',
      'destructor',
      'class procedure',
      'class function',
      'safe procedure',
      'safe function'
    );

  CallStr: array[TCallingKind] of string =
    (
      'register',
      'cdecl',
      'pascal',
      'stdcall',
      'safecall'
    );

procedure TForm1.Button2Click(Sender: TObject);
var
  Def: ITypeInfo;
  Obj: IObject;
  Value: array of string;
begin
  Def := TSilDelphiTypeDef.Create(TypeInfo(IObject));

  Obj := Self;
  
  SetLength(Value, 3);
  Value[0] := 'arg 0';
  Value[1] := 'arg 1';
  Value[2] := 'arg 2';

  InvokeMethod(Self, Def.Data.AsInterface, 'Assign', [Obj, Value]);



end;

procedure TForm1.Button3Click(Sender: TObject);
var
  Obj: IObject;
  Value: array of string;
begin
  Obj := Self;


  Obj.Enabled('pirulo');

  SetLength(Value, 3);
  Value[0] := 'arg 0';
  Value[1] := 'arg 1';
  Value[2] := 'arg 2';

  Obj.Assign(Value);

//
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  Def: ITypeInfo;
  nMethods: IEnumerator;
  nParams: IEnumerator;
  Mthd: ITypeMethod;
  Parm: ITypeParam;
  S, T: string;
begin
  Def := TSilDelphiTypeDef.Create(TypeInfo(IObject));

  with Def.Data.AsInterface do
  begin
    Memo.Lines.Add(         'type' );
    Memo.Lines.Add(         '  ' + Name + ' = ' + 'interface (' + ParentType.Name + ')');
    Memo.Lines.Add(         '    [' + Sil.GUID.ToStr(GUID) + ']');
    while Methods.Enumerate(nMethods, Mthd) do
    begin
      Memo.Lines.Add(       '    ' + MethodStr[Mthd.MethodKind] + ' ' + Mthd.Name + '(');

      while Mthd.Params.Enumerate(nParams, Parm) do
      begin
        T := '';
        if pfVar in Parm.Flags then
          S := '  var '
        else if pfConst in Parm.Flags then
          S := 'const '
        else if pfOut in Parm.Flags then
          S := '  out '
        else
          S := '      ';

        S := S + Parm.ParamName;

        if Parm.TypeName <> '' then
          S := S + ': ';

        if pfArray in Parm.Flags then
          S := S + ' array of '
        else
          S := S + ' ';

        if Parm.TypeName <> '' then
           S := S + Parm.TypeName;

        if nParams.HasMore then
           S := S + ';' + ' //' + Sil.Int.ToStr((Mthd as ITypeInterfaceMethod).Index) + ' ' + Sil.Int.ToStr((Mthd as ITypeInterfaceMethod).Position);

        Memo.Lines.Add(     '        ' + S );

      end;

      if Mthd.MethodKind in [mkFunction, mkClassFunction] then
        S := ': ' + Mthd.Result.TypeName else
        S := '';

      S := S + '; ' + CallStr[Mthd.CallingKind] + ';';

      Memo.Lines.Add(       '      )' + S );
    end;
    Memo.Lines.Add(         '  end;');
  end;

end;

function TForm1.GetX: Integer;
begin
  Result := 12345678;
end;

function TForm1.GetY: Integer;
begin
  Result := 1;
end;

procedure TForm1.Reset;
begin

end;

procedure TForm1.SetY(Value: Integer);
begin
  Caption := Int.ToHex(Value, 8);
end;

function TForm1.Enabled(const Dato: String): Boolean;
begin
  Result := Str.CompareText(Dato, 'pirulo', True) = 0;
  Caption := Dato;
end;

procedure TForm1.Assign(const Values: array of string);
var
  I: Integer;
begin
  for I := Low(Values) to High(Values) do
    Memo.Lines.Add(Int.ToStr(I) + ': ' + Values[I])
end;

{ TChunk }

procedure TChunk.SetName(const Value: string);
begin
  FName := Value;
end;

procedure TChunk.SetValue(const Value: Variant);
begin
  FValue := Value;
end;

end.
