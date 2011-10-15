unit UiStub;

interface

uses
  Sil,
  SilBeTypeInfo,
  SilLiTypeInfo;

type
  TQueryInterface = function(const IID: TGUID; out Obj): HResult of object; stdcall;
  TAddRef = function: Integer of object; stdcall;
  TRelease = function: Integer of object; stdcall;

type
  PInterfaceStub = ^RInterfaceStub;
  RInterfaceStub = packed record
    QueryInterface: TQueryInterface;
    AddRef: TAddRef;
    Release: TRelease;
    Invoke: TMethod;
  end;

type
  PMethod = ^RMethod;
  RMethod = packed record
    Ordinal: Word;
    Offset: Word;
    Instance: PInterfaceStub;
  end;

implementation
end.
