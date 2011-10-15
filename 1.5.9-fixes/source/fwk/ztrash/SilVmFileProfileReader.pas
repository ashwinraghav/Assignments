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

unit SilVmFileProfileReader;

interface

uses
  Sil,
  SilViNewProfiler;

type 
  TFileProfileReader = class(
  //- extends
    TInterfacedObject,
  //- implements
    IProfileReader )
  private
    FFileName: string;
  protected //- IProfileReader
    function GetStream: IStream;
  public
    constructor Create(const FileName: string);
  end;


  TStreamProfileReader = class(
  //- extends
    TInterfacedObject,
  //- implements
    IProfileReader )
  private
    FStream: IStream;
  protected //- IProfileReader
    function GetStream: IStream;
  public
    constructor Create(const AStream: IStream);
    destructor Destroy; override;
  end;


implementation

{ TFileProfileReader }

constructor TFileProfileReader.Create(const FileName: string);
begin
  inherited Create;
  FFileName := FileName;
end;

function TFileProfileReader.GetStream: IStream;
begin
  Result := OS.FileSystem.OpenFile(FFileName, fmAccessRead, fmShareReadWrite, True).Stream;
end;

{ TStreamProfileReader }

constructor TStreamProfileReader.Create(const AStream: IStream);
begin
  inherited Create;
  FStream := AStream;
end;

destructor TStreamProfileReader.Destroy;
begin
  FStream := nil;
  inherited;
end;

function TStreamProfileReader.GetStream: IStream;
begin
  Result := FStream;
end;

end.
