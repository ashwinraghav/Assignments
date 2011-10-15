unit SilSmDC;

interface

uses
  Windows,
  SilOmHandle;

type
  TDisplayContext = class(TSilHandle)
  private
    FWindow: THandle;
  protected
    procedure HandleIsValid(var Result: Boolean); override;
    procedure HandleClose; override;
  public
    constructor Create(WindowHandle: THandle);
  end;

implementation

{ TDisplayContext }

constructor TDisplayContext.Create(WindowHandle: THandle);
begin
  inherited CreateNew(Windows.GetDC(WindowHandle), True);
  FWindow := WindowHandle;
end;

procedure TDisplayContext.HandleClose;
begin
  Windows.ReleaseDC(FWindow, Self.Handle);
end;

procedure TDisplayContext.HandleIsValid(var Result: Boolean);
begin
  Result := (Self.Handle > 0);
end;

end.
