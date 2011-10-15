(***************************************************************************)
(*                                                                         *)
(*     Standard Interface Library  - Application Framework Services        *)
(*                                                                         *)
(*   This demonstrates a mechanism by means of which any objects that have *)
(* Lock/Unlock semantics (like BeginUpdate/EndUpdate) can be wrapped       *)
(* around an ILockable implementation an the used by a ILocker which       *)
(* keeps it locked as long as there has any ILocks created on it.          *)
(*                                                                         *)
(*   Also, demonstrate how can a object be checked for locking state       *)
(* Without needing an extra interface to provide us that information.      *)
(*                                                                         *)
(***************************************************************************)

unit FmTestBaseFrameworkLockers;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls,

  Sil;

type
  TFormTestLockers = class(
    TForm,
    ILockable
    )
    btAddLock: TButton;
    btRemoveLocks: TButton;
    lbCount: TLabel;
    btExclusiveLock: TButton;
    Label1: TLabel;
    Memo: TMemo;
    procedure btAddLockClick(Sender: TObject);
    procedure btRemoveLocksClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btExclusiveLockClick(Sender: TObject);
  private
    // we're going to use an interface list to hold the ILocks taken from the locker
    FLocks: IInterfaceList;
    // this is a counted locked over ourselves
    FCounter: ILocker;
  protected // simple ILockable implementation which only writes out a string to the memo.
		procedure Lock;   
		procedure Unlock;
  end;

var
  FormTestLockers: TFormTestLockers;

implementation

{$R *.DFM}

{ TForm1 }

procedure TFormTestLockers.FormCreate(Sender: TObject);
begin
  // creates the InterfaceList to holds tha ILocks
  FLocks := Sil.List.InterfaceList();
  // creates the counted ILocker: note the Boolean parameter
  //  which we pass in True. This is the one that specifies the
  //  counted semantics. Without it (pass False) the ILocker
  //  only keeps one ILock over an object at any given time.
  // The first parameter is the ILockable over which to act upon.
  //  this can be any implementation with the required semantics.
  FCounter := Sil.Lock.AsLocker(ILockable(Self), True);
end;

procedure TFormTestLockers.FormDestroy(Sender: TObject);
begin
  // release the ILocker
  FCounter := nil;
  // release the list
  FLocks := nil;
end;

procedure TFormTestLockers.Lock;
begin
  // log out the Lock operation
  Memo.Lines.Add('Locked');
end;

procedure TFormTestLockers.Unlock;
begin
  // log out the UnLock operation
  Memo.Lines.Add('Unlocked');
end;

procedure TFormTestLockers.btAddLockClick(Sender: TObject);
// Takes a lock from the ILocker and puts it in the list 
begin
  // Sil.Lock.Take: receives an object (in this case an ILocker)
  //  and obtains an ILock from it. Depending on the type of object
  //  different action is taken.
  //  If an anonymous IUnknown is passed then different interfaces
  //  are asked for in turn and depending of which of them is
  //  implemented the corresponding acction is taken.
  // Note that the result of this operation is an ILock and that it
  //  is inserted into the interface list.
  FLocks.Add(Sil.Lock.Take(FCounter));
  // Refresh the count of locks taken
  lbCount.Caption := Int.ToStr(FLocks.Count);
end;

procedure TFormTestLockers.btRemoveLocksClick(Sender: TObject);
// Deletes and frees up tha last lock taken
begin
  // Deleting the last element from the list: this destroys the ILock which in turn decrements
  //  the lock count of the ILocker
  with FLocks do
    if Count > 0 then
      Delete(Count - 1);
  lbCount.Caption := Int.ToStr(FLocks.Count);
end;

procedure TFormTestLockers.btExclusiveLockClick(Sender: TObject);
// Try to take an exclusive lock
begin
  Memo.Lines.Add('Trying to take an exclusive lock ...');
  // This only can succeed if no one is holding a lock over this object.
  //  This function signals failure by returning a nil ILock
  if Sil.Lock.Take(FCounter, True) <> nil then
  begin
    Memo.Lines.Add('Succeeded! We have a exclusive lock so we can operate safely');
    Sleep(1000);
    Memo.Lines.Add('Goodbye! We''re done here.');
  end else
    Memo.Lines.Add('Failed! Maybe someone has a lock on it ...');
end;

end.
