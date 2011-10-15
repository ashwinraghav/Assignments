(****************************************************************************)
(*                                                                          *)
(*     Standard Interface Library  - Application Framework Services         *)
(*                                                                          *)
(* Demonstration of the Sil.Clone tool                                      *)
(*                                                                          *)
(* This program shows how is used the clone mechanism for arbitrary objects *)
(*  The only requisite is that the target object implements the interface   *)
(*  ICloneable.                                                             *)
(*                                                                          *)
(* There are many kinds of objects which already implements this interface, *)
(* for example: MemoryStream, DataRowsets, etc.                             *)
(*                                                                          *)
(****************************************************************************)

unit FmTestBaseFrameworkCloneMemoryStream;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls,

  Sil;

type
  TFormTestCloneMemory = class(TForm)
    btClone: TButton;
    edWrite: TEdit;
    edRead: TEdit;
    procedure btCloneClick(Sender: TObject);
  private
    FOriginal,
    FClone: IMemoryStream;
  public
  end;

var
  FormTestCloneMemory: TFormTestCloneMemory;

implementation

{$R *.DFM}

procedure TFormTestCloneMemory.btCloneClick(Sender: TObject);
var
  s1, s2: String;
begin
  // create a memory stream to hold our data
  FOriginal := Sil.Tk.MemoryStream;

  // read the value to write to it
  s1 := edWrite.Text;

  // do so
  FOriginal.Write(s1[1], Length(s1));

  // create a clone of the first stream: this only creates a new stream object, it does not
  //  copies the data from one to another. Rather, the new stream shares the data with the old
  //  one mantaining a different set of access state like the current position.
  // And, generally speaking, this will be the mechanism used for underlying implementation
  //  of the clone semantics.
  FClone := Sil.Clone.Make(FOriginal) as IMemoryStream;

  // try to read the data from the cloned object
  SetLength(s2, FClone.Size);
  FClone.Read(s2[1], Length(s2));
  edRead.Text := s2;
end;

end.
