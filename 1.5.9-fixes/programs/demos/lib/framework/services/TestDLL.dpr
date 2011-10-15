(****************************************************************************)
(*                                                                          *)
(*     Standard Interface Library  - Base Library Services                  *)
(*                                                                          *)
(* This demo shows the GlobalServiceList in action.                         *)
(*                                                                          *)
(* This is a demo DLL which exports the entry point required by the demo    *)
(*  application.                                                            *)
(*                                                                          *)
(****************************************************************************)

library TestDLL;

uses
  SilDll,
  SilModule,
  SilLibrary,
  Sil;

{$R *.res}

function Spawn(const Runnable: IRunnable): IThread; stdcall;
begin
  Result := Sil.Os.Thread.Spawn('DLL', Runnable);
end;

exports
  Spawn;

begin
end.
