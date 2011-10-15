unit UiTestStringsStub;

interface

uses
  Sil;

type
  TTestStringsCreateStringList = function: IStringList; stdcall;

type
  ITestStringsDll = interface
    ['{FAD509C8-88B2-4786-8F5E-DE1498FDB3A2}']
    function DoGetCreateStringList: TTestStringsCreateStringList;
    property CreateStringList: TTestStringsCreateStringList read DoGetCreateStringList;
  end;

implementation
end.
 