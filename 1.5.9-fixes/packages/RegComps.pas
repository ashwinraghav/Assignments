unit RegComps;

interface


procedure Register;

implementation

uses
  Classes,
  button,
  Digital,
  ScrollBox,
  sClock,
  InterfaceProperty,

  SilVcl,
  SilVmAdoServer,
  SilVmSocketProt,
  SilVmDataRowset,
  SilVmDataConnection;

procedure Register;
begin
  RegisterComponents('SIL',
    [ TButtonEx,
      TScroller,
      TSGaugeScale,
      TSClock,
      TDigital,
      TSilSocketProtocol,
      TSilDataset,
      TSilSqlConnection,
      TSilAdoSocketServer,
      TSilQuery,
      TSilStoredProc,
      TSilButton,
      TSil3dButton,
      TSilFlowNode,
      TSilGradientPanel
    ]);
end;

end.
