unit UmTestStringsStub;

interface

uses
  Sil, UiTestStringsStub;

type
  TTestStringsDll = class(
    TSilObject,
    ITestStringsDll )
  private
    FDll: ISharedLibrary;
  protected // ITestStringsDll
    function DoGetCreateStringList: TTestStringsCreateStringList;
  public
    constructor Create(const FileName: string);
    destructor Destroy; override;
  end;

implementation

uses SilOtSharedLibrary;

{ TTestStringsDll }

constructor TTestStringsDll.Create(const FileName: string);
begin
  inherited Create;
  FDll := Sil.Os.SharedLibrary.Load(FileName);
end;

destructor TTestStringsDll.Destroy;
begin
  FDll := nil;
  inherited;
end;

function TTestStringsDll.DoGetCreateStringList: TTestStringsCreateStringList;
begin
  FDll.Bind('CreateStringList', 0, Result, True);
end;

end.
