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

unit SilStDataRowset;

{$i Defines.inc}

interface

uses
  Sil,
  SilSiDataAccess;

type
  Tk = class (Tool)
    class function OpenFile(const FileName: String; Access: TFileAccessMode = fmAccessReadWrite; Share: TFileShareMode = fmShareNone): IDataRowset;
    class function CreateFile(const FileName: String; Access: TFileAccessMode = fmAccessReadWrite; Share: TFileShareMode = fmShareNone): IDataRowsetDef;
    class function Stream(const Stream: IRandomStream): IDataRowset;
    class function Memory: IDataRowsetDef;
    {$IFDEF D60}
    class function Filter(const Source: IDataRowset; const FilterCond: String; const IndexName: String = ''): IDataRowset;
    class function Select(const Source: IDataRowset; const FilterCond: String; const IndexName: String = ''): IDataRowset;
    class function Copy(const Source, Dest: IDataRowset; const FilterCond: String; const IndexName: String = ''): IDataRowset;
    {$ENDIF}
  end;

implementation

uses
  SilEval,
  SilSmDataRowset {$IFDEF D60},
  SilSmDataRowsetFilter {$ENDIF};

{ Tk }

class function Tk.CreateFile(const FileName: String; Access: TFileAccessMode; Share: TFileShareMode): IDataRowsetDef;
begin
  Result := TDataRowset.CreateFile(FileName, Access, Share);
end;

{$IFDEF D60}
class function Tk.Filter(const Source: IDataRowset; const FilterCond: String; const IndexName: String): IDataRowset;
begin
  Result := TSilDataRowsetFilter.Create(Source, FilterCond, IndexName);
end;

class function Tk.Select(const Source: IDataRowset; const FilterCond: String; const IndexName: String = ''): IDataRowset;
var
  Def: IDataRowsetDef;
begin
  Def := Memory();
  Def.CopyFrom(Source.RowsetDef);
  Def.Build();
  Result := Def.Rowset;



end;

class function Tk.Copy(const Source, Dest: IDataRowset; const FilterCond, IndexName: String): IDataRowset;
(*)var
  HasIndex: Boolean;
  Evaluator: IEvaluator;(*)
begin
//  Evaluator := SilEval.Tool.Create(Self, FilterCond);
(*  try
    HasIndex := Assigned(Source.ActiveIndex);
    if not HasIndex and Str.IsAssigned(IndexName) then Source.ActiveIndexName := IndexName; 
    if not HasIndex then Source.First;

    while not Source.IsEof do
    begin
      if Evaluator.Execute then
      begin
        Dest.Append;
        Dest.AppendRecordFrom(Source);
        Dest.Post;
      end else
      if HasIndex then
        Break;

      Source.Next;
    end;

    Dest.First;
  finally
    Evaluator := nil;
  end; *)
end;

{$ENDIF}

class function Tk.Memory: IDataRowsetDef;
var
  Stream: IRandomStream;
begin
  Stream := Sil.Tk.MemoryStream;
  Result := TDataRowset.Create(Stream);
end;

class function Tk.OpenFile(const FileName: String; Access: TFileAccessMode; Share: TFileShareMode): IDataRowset;
begin
  Result := TDataRowset.OpenFile(FileName, Access, Share);
end;

class function Tk.Stream(const Stream: IRandomStream): IDataRowset;
begin
  Result := TDataRowset.Create(Stream);
end;

end.
