{********************************************************************************
 *                  Standard Interface Library (SIL)                            *
 *                                                                              *
 *       General purpose library whose design is based in STRONG                *
 *   use of interfaces.                                                         *
 *                                                                              *
 *                                                                              *
 *     Copyright (C) 2000 Mariano Podestá    antiriad@gmail.com                 *
 *     Copyright (C) 2000 Leandro Conde      lconde@str.com.ar                  *
 *     Copyright (C) 2000 Lisandro Podestá   lisandrop@movi.com.ar              *
 *                                                                              *
 *     See License.txt for details.                                             *
 *                                                                              *
 *   This library is free software; you can redistribute it and/or              *
 *   modify it under the terms of the GNU Lesser General Public                 *
 *   License as published by the Free Software Foundation; either               *
 *   version 2.1 of the License, or (at your option) any later version.         *
 *                                                                              *
 *   This library is distributed in the hope that it will be useful,            *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of             *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU          *
 *   Lesser General Public License for more details.                            *
 *                                                                              *
 *   You should have received a copy of the GNU Lesser General Public           *
 *   License along with this library; if not, write to the Free Software        *
 *   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA  *
 *                                                                              *
 ********************************************************************************}

unit SilSmLeakChecker;

{$I Defines.inc}

interface

uses
  SilBeError,
  SilLkObject;

type
  ESilMemoryLeak = class(ESilException)
  private 
    FMemory: LongWord;
    FFinalMemory: LongWord;
    function GetLeak: Integer;
  public
    constructor Create(InitialMemory, FinalMemory: LongWord);
    property InitialMemory: LongWord read FMemory;
    property FinalMemory: LongWord read FFinalMemory;
    property Leak: Integer read GetLeak;
  end;

  TSilMemoryLeakChecker = class(TSilObject)
  private
    FMemory: LongWord;
  public
    class function Create: IUnknown; reintroduce; 
    class function NewInstance: TObject; override;
    procedure FreeInstance; override;
  end;

implementation

uses
  SilBtMem,
  SilSdLeakChecker;

{ ESilMemoryLeak }

constructor ESilMemoryLeak.Create(InitialMemory, FinalMemory: LongWord);
begin
  FMemory := InitialMemory;
  FFinalMemory := FinalMemory;
  inherited CreateResFmt(@SMemoryLeakError, [GetLeak(), InitialMemory, FinalMemory]);
end;

function ESilMemoryLeak.GetLeak: Integer;
begin
  Result := Integer(FFinalMemory) - Integer(FMemory);
end;

{ TSilMemoryLeakChecker }

class function TSilMemoryLeakChecker.Create: IUnknown;
begin
  Result := inherited Create();
end;

class function TSilMemoryLeakChecker.NewInstance: TObject;
var
  InitialMemory: LongWord;
begin
  InitialMemory := Mem.Info.Used;
  Result := inherited NewInstance;
  TSilMemoryLeakChecker(Result).FMemory := InitialMemory;
end;

procedure TSilMemoryLeakChecker.FreeInstance;
var
  InitialMemory: LongWord;
  FinalMemory: LongWord;
begin
  InitialMemory := FMemory;
  inherited; // destruye la instancia
  FinalMemory := Mem.Info.Used;
  if InitialMemory <> FinalMemory then raise ESilMemoryLeak.Create(InitialMemory, FinalMemory);
end;

end.
