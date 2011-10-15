(***************************************************************************)
(*                                                                         *)
(*     Standard Interface Library  - Base Tool Services                    *)
(*                                                                         *)
(* IMPORTANT!!                                                             *)
(*                                                                         *)
(* This demo only works in Delphi 6 or later                               *)
(* This is alpha code many features still doesn't work                     *)
(* Please, use only as an inspiration or to see how things work            *)
(*                                                                         *)
(* We'll keep working to make this a standard library feature.             *)
(*                                                                         *)
(***************************************************************************)

unit FmTestBaseToolsTypeinfo;

interface

{$INCLUDE Defines.inc}

uses
  Classes, Controls, Forms, StdCtrls, Sil;

type
  // Sample interface: we use it to show how we can read its type info
  ISampleObject = interface(IInvokable)
    ['{66FC8CE4-2FDE-41D5-8CF8-4D18FB6ADCCF}']
    function GetX: Integer; stdcall;
    function GetY: Integer; stdcall;
    procedure SetY(Value: Integer); stdcall;
    function Enabled(const Dato: String): Boolean; stdcall;
    procedure Reset; overload; stdcall;
    procedure Assign(const Value: string); stdcall;
    property X: Integer read GetX;
    property Y: Integer read GetY write SetY;
  end;

  TFormTestTypeinfo = class(
    TForm,
    ISampleObject // We implement it as a target for the Invoke demo
    )
    Memo: TMemo;
    btDumpInterface: TButton;
    Button2: TButton;
    procedure btDumpInterfaceClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  protected
    function GetX: Integer; stdcall;
    function GetY: Integer; stdcall;
    procedure SetY(Value: Integer); stdcall;
    function Enabled(const Dato: String): Boolean; stdcall;
    procedure Reset; overload; stdcall;
    procedure Assign(const Value: string); reintroduce; stdcall;
  end;

var
  FormTestTypeinfo: TFormTestTypeinfo;

implementation

uses           
  SilLfDelphiInvoke;

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

procedure TFormTestTypeinfo.Button2Click(Sender: TObject);
var
  Def: ITypeInfo;
  Y: Integer;
  Value: string;
begin
  Def := Sil.Typ.Info(System.TypeInfo(ISampleObject));

  Y := 123456789;
  Value := 'This is the argument value';

  InvokeMethod(Self, Def.Data.AsInterface, 'Assign', [Value]);
  InvokeMethod(Self, Def.Data.AsInterface, 'SetY', [Y]);
end;

procedure TFormTestTypeinfo.btDumpInterfaceClick(Sender: TObject);
var
  Def: ITypeInfo;
  nMethods: IEnumerator;
  nParams: IEnumerator;
  Mthd: ITypeMethod;
  Parm: ITypeParam;
  S, T: string;
begin
  Def := Sil.Typ.Info(System.TypeInfo(ISampleObject));

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
        if paVar in Parm.Flags then
          S := '  var '
        else if paConst in Parm.Flags then
          S := 'const '
        else if paOut in Parm.Flags then
          S := '  out '
        else
          S := '      ';

        S := S + Parm.ParamName;

        if Parm.TypeName <> '' then
          S := S + ': ';

        if paArray in Parm.Flags then
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

function TFormTestTypeinfo.GetX: Integer;
begin
  Result := 12345678;
end;

function TFormTestTypeinfo.GetY: Integer;
begin
  Result := 1;
end;

procedure TFormTestTypeinfo.Reset;
begin

end;

procedure TFormTestTypeinfo.SetY(Value: Integer);
begin
  Memo.Lines.Add(Sil.Int.ToStr(Value));
end;

function TFormTestTypeinfo.Enabled(const Dato: String): Boolean;
begin
  Result := True;
end;

procedure TFormTestTypeinfo.Assign(const Value: string); 
begin
  Memo.Lines.Add(Value)
end;

end.
