unit FmTestStub;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Sil, SilLiTypeInfo, UiStub, UmStub, StdCtrls;

type
  ITest = interface (IInvokable)
    ['{0D4AA9A2-F937-498F-B4E8-243A8EC361E0}']
    procedure Foo(const Number: Integer; const Text: string); 
    procedure Goo(const Text: string; out Data: Integer); 
    function Zoo(const Data: ITest): string;    
  end;

  TForm1 = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    function DoQueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function DoAddRef: Integer; stdcall;
    function DoRelease: Integer; stdcall;
    procedure DoInvoke; 
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses UfStub, SilLmDelphiTypeInfo;

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  Info: ITypeInterface;
  Rec: RInterfaceStub;
  Instance: Pointer;
begin
  Info := TSilDelphiTypeInterface.CreateNew(TypeInfo(ITest));

  
  Rec.QueryInterface := DoQueryInterface;
  Rec.AddRef := DoAddRef;
  Rec.Release := DoRelease;
  Rec.Invoke.Code := @TForm1.DoInvoke;
  Rec.Invoke.Data := Self;

  Instance := NewInterface(Info, Rec);

  IUnknown(Instance)._AddRef;
  
  IUnknown(Instance)._Release;
end;

function TForm1.DoAddRef: Integer;
begin

end;

function TForm1.DoQueryInterface(const IID: TGUID; out Obj): HResult;
begin

end;

function TForm1.DoRelease: Integer;
begin

end;

procedure TForm1.DoInvoke;
begin

end;

end.
