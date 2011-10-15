unit SilLfInvokeUtils;

{$I Defines.inc}

interface

uses
  SilBeTypeInfo,
  SilLiTypeInfo;
                   
type
  PParam = ^TParam;
  TParam = record
    Kind: TTypeKind;
    Size: 0 .. 14;
    case Integer of
      0: (Data: array[0..14] of Pointer);
      1: (VInt64: Int64);
      2: (VInteger: Integer);
      3: (VLongWord: LongWord);
      4: (VPointer: Pointer);
      5: (VWord: Word);
      6: (VByte: Byte);
      7: (VShort: Shortint);
      8: (VSmall: Smallint);
      9: (VChar: Char);
     10: (VWideChar: WideChar);
     11: (VBoolean: Boolean);
     12: (VWordBool: WordBool);
     13: (VLongBool: LongBool);
     14: (VSingle: Single);
     15: (VDouble: Double);
     16: (VExtended: Extended);
     17: (VReal: Real);
     18: (VComp: Comp);
     19: (VCurrency: Currency);
  end;

type
  TParamList = array of TParam;


function DynArrayLength(V: Pointer): Integer;
function DynArrayHigh(V: Pointer): Integer;
procedure DoIntfFromObject(
  const Item: ITypeParam;
    var Param: TParam;
  const Value: TVarRec);

procedure DoIntfFromUnknown(
  const Item: ITypeParam;
    var Param: TParam;
  const Value: TVarRec);

procedure DoIntfFromValue(
  const Item: ITypeParam;
    var Param: TParam;
  const Value: TVarRec);

procedure DoItemFromValue(
  const Item: ITypeParam;
    var Param: TParam;
  const Value: TVarRec);

procedure DoInt64FromValue(
  const Item: ITypeParam;
    var Param: TParam;
  const Value: TVarRec);

procedure DoSingleFromValue(
  const Item: ITypeParam;
    var Param: TParam;
  const Value: TVarRec);

procedure DoDoubleFromValue(
  const Item: ITypeParam;
    var Param: TParam;
  const Value: TVarRec);

procedure DoExtendedFromValue(
  const Item: ITypeParam;
    var Param: TParam;
  const Value: TVarRec);

procedure DoCompFromValue(
  const Item: ITypeParam;
    var Param: TParam;
  const Value: TVarRec);

procedure DoCurrencyFromValue(
  const Item: ITypeParam;
    var Param: TParam;
  const Value: TVarRec);

procedure DoFloatFromValue(
  const Item: ITypeParam;
    var Param: TParam;
  const Value: TVarRec);

implementation

uses
  Sil;

procedure DoAssignResult(
        Value: Pointer;
  const Param: ITypeParam;
  const Arg: TVarRec);
begin
  if Arg.VType = vtPointer then
    case Param.BaseType.TypeKind of
      tkInteger:      Integer(Arg.VPointer^) := Integer(Value);
      tkChar:         Char(Arg.VPointer^) := Char(Value);
      tkEnumeration:  ;
      //tkSet: ;
      tkFloat: ;
      tkString:       ;
      tkClass:        Pointer(Arg.VPointer^) := Value;
      tkInterface:    IUnknown(Arg.VPointer^) := IUnknown(Value);
      tkMethod: ;
      tkWChar: ;
      tkLString: ;
      tkWString: ;
      tkVariant: ;
      tkArray: ;
      tkRecord: ;
      tkInt64: ;
      tkDynArray: ;
    end;
end;

procedure DoSetupParam(
  const Item: ITypeParam;
    var Param: TParam;
  const Value: TVarRec);
begin
  Param.Size := 1;
  Param.Kind := Item.BaseType.TypeKind; // para el finalize

  if (Item.Flags * [paVar, paOut]) <> []  then
    begin
      if Value.VType <> vtPointer then raise Sil.Error.Create('Al llamar a %s en el parametro %s se esperaba por referencia', [Item.Method.Name, Item.ParamName]); //!TODO!
      Param.Data[0] := Value.VPointer;
      if paOut in Item.Flags then PPointer(Param.Data[0])^ := nil;
    end
  else
    case Param.Kind of
      tkFloat:     DoFloatFromValue(Item, Param, Value);
      tkInt64:     DoInt64FromValue(Item, Param, Value);
      tkInterface: DoIntfFromValue(Item, Param, Value);
      else         DoItemFromValue(Item, Param, Value);
    end;
end;

function DynArrayLength(V: Pointer): Integer;
asm
    CALL      System.@DynArrayLength
end;

function DynArrayHigh(V: Pointer): Integer;
asm
    CALL      System.@DynArrayHigh
end;

procedure DoIntfFromObject(
  const Item: ITypeParam;
    var Param: TParam;
  const Value: TVarRec);
var
  Info: ITypeInterface;
  Method: ITypeInterfaceMethod;
begin
  Info := Item.BaseType.Data.AsInterface;
  if not Value.VObject.GetInterface(Info.GUID, Param.Data[0]) then
  begin
    Method := Item.Method as ITypeInterfaceMethod;
    raise Sil.Error.Create('%s no soporta %s que %s.%s espera como %s', [Value.VObject.ClassName, Info.Name, Method.Owner.Name, Method.Name, Item.ParamName]); //!TODO!
  end;    
end;

procedure DoIntfFromUnknown(
  const Item: ITypeParam;
    var Param: TParam;
  const Value: TVarRec);
var
  Info: ITypeInterface;
  Method: ITypeInterfaceMethod;
begin
  Info := Item.BaseType.Data.AsInterface;
  if IUnknown(Value.VInterface).QueryInterface(Info.GUID, Param.Data[0]) <> 0 then
  begin
    Method := Item.Method as ITypeInterfaceMethod;
    raise Sil.Error.Create('El valor recibido no soporta %s que %s.%s espera como %s', [Info.Name, Method.Owner.Name, Method.Name, Item.ParamName]); //!TODO!
  end;
end;

procedure DoIntfFromValue(
  const Item: ITypeParam;
    var Param: TParam;
  const Value: TVarRec);
var
  Info: ITypeInterface;
begin
  case Value.VType of
    vtInterface:  DoIntfFromUnknown(Item, Param, Value);
    vtObject:     DoIntfFromObject(Item, Param, Value);
    else
      begin
        Info := Item.BaseType.Data.AsInterface;
        raise Sil.Error.Create('El valor recibido para %s no puede convertirse a %s', [Item.ParamName, Info.Name]);
      end;
  end;
end;

procedure DoItemFromValue(
  const Item: ITypeParam;
    var Param: TParam;
  const Value: TVarRec);
begin
  Param.Data[0] := Value.VPointer;
  if (paArray in Item.Flags) then
  begin
    if Value.VType <> vtPointer then raise Sil.Error.Create('El valor recibido para %s no es un array dinamico', [Item.ParamName]);
    Integer(Param.Data[1]) := DynArrayHigh(Param.Data[0]);
    Param.Size := 2;
  end;
end;

procedure DoInt64FromValue(
  const Item: ITypeParam;
    var Param: TParam;
  const Value: TVarRec);
begin
  Param.Size := (SizeOf(Int64) + SizeOf(Param.Data[0]) - 1) div SizeOf(Param.Data[0]);
  case Value.VType of
    vtInteger:    Param.VInt64 := Value.VInteger;
    vtString:     Param.VInt64 := Sil.Str.ToLarge(Value.VString^);
    vtAnsiString: Param.VInt64 := Sil.Str.ToLarge(string(Value.VAnsiString));
    vtWideChar:   Param.VInt64 := Ord(Value.VWideChar);
    vtChar:       Param.VInt64 := Ord(Value.VChar);
    vtVariant:    Param.VInt64 := Value.VVariant^;
    vtInt64:      Param.VInt64 := Value.VInt64^;
    else          raise Sil.Error.Create('El valor recibido para %s no es compatible con un Int64', [Item.ParamName]);
  end;
end;

procedure DoSingleFromValue(
  const Item: ITypeParam;
    var Param: TParam;
  const Value: TVarRec);
begin
  Param.Size := (SizeOf(Single) + SizeOf(Param.Data[0]) - 1) div SizeOf(Param.Data[0]);
  case Value.VType of
    vtExtended:   Param.VSingle := Value.VExtended^;
    vtInteger:    Param.VSingle := Value.VInteger;
    vtString:     Param.VSingle := Sil.Str.ToFloat(Value.VString^);
    vtAnsiString: Param.VSingle := Sil.Str.ToFloat(string(Value.VAnsiString));
    vtVariant:    Param.VSingle := Value.VVariant^;
    vtInt64:      Param.VSingle := Value.VInt64^;
    vtCurrency:   Param.VSingle := Value.VCurrency^;
    else          raise Sil.Error.Create('El valor recibido para %s no es compatible con un Single', [Item.ParamName]);
  end;
end;

procedure DoDoubleFromValue(
  const Item: ITypeParam;
    var Param: TParam;
  const Value: TVarRec);
begin
  Param.Size := (SizeOf(Double) + SizeOf(Param.Data[0]) - 1) div SizeOf(Param.Data[0]);
  case Value.VType of
    vtExtended:   Param.VDouble := Value.VExtended^;
    vtInteger:    Param.VDouble := Value.VInteger;
    vtString:     Param.VDouble := Sil.Str.ToFloat(Value.VString^);
    vtAnsiString: Param.VDouble := Sil.Str.ToFloat(string(Value.VAnsiString));
    vtVariant:    Param.VDouble := Value.VVariant^;
    vtInt64:      Param.VDouble := Value.VInt64^;
    vtCurrency:   Param.VDouble := Value.VCurrency^;
    else          raise Sil.Error.Create('El valor recibido para %s no es compatible con un Double', [Item.ParamName]);
  end;
end;

procedure DoExtendedFromValue(
  const Item: ITypeParam;
    var Param: TParam;
  const Value: TVarRec);
begin
  Param.Size := (SizeOf(Extended) + SizeOf(Param.Data[0]) - 1) div SizeOf(Param.Data[0]);
  case Value.VType of
    vtExtended:   Param.VExtended := Value.VExtended^;
    vtCurrency:   Param.VExtended := Value.VCurrency^;
    vtInteger:    Param.VExtended := Value.VInteger;
    vtString:     Param.VExtended := Sil.Str.ToFloat(Value.VString^);
    vtAnsiString: Param.VExtended := Sil.Str.ToFloat(string(Value.VAnsiString));
    vtVariant:    Param.VExtended := Value.VVariant^;
    vtInt64:      Param.VExtended := Value.VInt64^;
    else          raise Sil.Error.Create('El valor recibido para %s no es compatible con un Extended', [Item.ParamName]);
  end;
end;

procedure DoCompFromValue(
  const Item: ITypeParam;
    var Param: TParam;
  const Value: TVarRec);
begin
  Param.Size := (SizeOf(Comp) + SizeOf(Param.Data[0]) - 1) div SizeOf(Param.Data[0]);
  case Value.VType of
    vtExtended:   Param.VComp := Value.VExtended^;
    vtInteger:    Param.VComp := Value.VInteger;
    vtString:     Param.VComp := Sil.Str.ToInt(Value.VString^);
    vtAnsiString: Param.VComp := Sil.Str.ToInt(string(Value.VAnsiString));
    vtVariant:    Param.VComp := Value.VVariant^;
    vtInt64:      Param.VComp := Value.VInt64^;
    vtCurrency:   Param.VComp := Value.VCurrency^;
    else          raise Sil.Error.Create('El valor recibido para %s no es compatible con un Comp', [Item.ParamName]);
  end;
end;

procedure DoCurrencyFromValue(
  const Item: ITypeParam;
    var Param: TParam;
  const Value: TVarRec);
begin
  Param.Size := (SizeOf(Currency) + SizeOf(Param.Data[0]) - 1) div SizeOf(Param.Data[0]);
  case Value.VType of
    vtExtended:   Param.VCurrency := Value.VExtended^;
    vtInteger:    Param.VCurrency := Value.VInteger;
    vtString:     Param.VCurrency := Sil.Str.ToCurrency(Value.VString^);
    vtAnsiString: Param.VCurrency := Sil.Str.ToCurrency(string(Value.VAnsiString));
    vtVariant:    Param.VCurrency := Value.VVariant^;
    vtInt64:      Param.VCurrency := Value.VInt64^;
    vtCurrency:   Param.VCurrency := Value.VCurrency^;
    else          raise Sil.Error.Create('El valor recibido para %s no es compatible con un Currency', [Item.ParamName]);
  end;
end;

procedure DoFloatFromValue(
  const Item: ITypeParam;
    var Param: TParam;
  const Value: TVarRec);
begin
  case Item.BaseType.Data.AsFloat.FloatType of
    ftSingle:     DoSingleFromValue(Item, Param, Value);
    ftDouble:     DoDoubleFromValue(Item, Param, Value);
    ftExtended:   DoExtendedFromValue(Item, Param, Value);
    ftComp:       DoCompFromValue(Item, Param, Value);
    ftCurr:       DoCurrencyFromValue(Item, Param, Value);
  end;
end;

end.
 