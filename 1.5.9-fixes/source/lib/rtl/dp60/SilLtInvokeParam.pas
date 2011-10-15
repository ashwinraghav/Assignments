unit SilLtInvokeParam;

interface

uses
  SilBkTool,
  SilBeTypeInfo,
  SilLiTypeInfo,
  SilLkInvokeParam;

type
  Params = class(Tool)
    class function GetSize(
        const Info: ITypeMethod): Integer;
    
    class function Setup(
        const Buffer: Pointer;
        const Instance: IUnknown;
        const Method: ITypeMethod;
        const Return: PVariant;
        const Arguments: array of const): TSilResultObject;
        
    class procedure Cleanup(
        const Buffer: Pointer);
  end;

type
  PBufferHeader = ^RBufferHeader;
  RBufferHeader = record
    Result: TSilResultObject;
    First: TSilParamObject;
    Last: TSilParamObject;
    Data: record end;
  end;

implementation

uses
  SilLiEnumerator,
  SilLmInvokeParam;

{ Params }

class function Params.GetSize(const Info: ITypeMethod): Integer;
var
  Enum: IEnumerator;
  Param: ITypeParam;
begin
  Result := SizeOf(RBufferHeader);
  with Info.Params do
    while Enumerate(Enum, Param) do
      Inc(Result, TSilParamObject.GetSize(Param));
  if Info.MethodKind = mkFunction then
    Inc(Result, TSilResultObject.GetSize(Info.Result)); 
end;

class function Params.Setup(
  const Buffer: Pointer;
  const Instance: IInterface;
  const Method: ITypeMethod;
  const Return: PVariant;
  const Arguments: array of const): TSilResultObject;
var
  Header: PBufferHeader;
  Enum: IEnumerator;
  Info: ITypeParam;
  VarRec: TVarRec;
  Ptr: PVarRec;
  Param: TSilParamObject;
  BufPtr: PByte;
begin
  Header := Buffer;
  Header.Result := nil;
  Header.Last := nil;
  Header.First := nil;
  Param := nil;

  VarRec.VType := vtInterface;
  VarRec.VInterface := Pointer(Instance);

  with Method.Params do
  begin
    BufPtr := @Header.Data;
    while Enumerate(Enum, Info) do
    try
      if Enum.Iteration > 0 then
        Ptr := @Arguments[Enum.Iteration - 1] else
        Ptr := @VarRec;
      Param := TSilParamObject.Create(BufPtr, Info, Ptr, Param);
      if not Assigned(Header.First) then Header.First := Param;
      Header.Last := Param;
      Inc(BufPtr, Param.Size);
    except
      Cleanup(Buffer);
      raise;
    end;
  end;

  if Method.MethodKind = mkFunction then
  try
    ASSERT(Assigned(Return));
    
    VarRec.VType := vtVariant;
    VarRec.VVariant := Return;

    Result := TSilResultObject.Create(BufPtr, Method.Result, @VarRec);
    Header.Result := Result;
  except
    Cleanup(Buffer);
    raise;
  end else
    Result := nil;
end;

type
  TSilParamObjectAccess = class(TSilParamObject);
  
class procedure Params.Cleanup(const Buffer: Pointer);
var
  Header: PBufferHeader;
  Item: ^TSilParamObject;
  Obj: TSilParamObject;
begin
  Header := Buffer;
  Item := @Header.Last;
  try
    while Assigned(Item^) do
    begin
      Obj := Item^;
      Item^ := nil;
      Item := @TSilParamObjectAccess(Obj).FPrev;
      Obj.Free;
    end;
  finally
    Header.Result.Free;
    Header.Result := nil;
  end;
end;

end.
