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

unit SilSmHTTP;

interface

uses
  Sil;

type
  TSilHttpClient = class(
    TSilObject )
  protected // IHttpClient
    (*)function Options(const Uri: string; const Tags: IHttpTagList = nil): IHttpMessage;
    function Get(const Uri: string; const Tags: IHttpTagList = nil): IHttpMessage;
    function Head(const Uri: string; const Tags: IHttpTagList = nil): IHttpMessage;
    function Put(const Uri: string; const Entity: IHttpEntity; const Tags: IHttpTagList = nil): IHttpMessage;
    function Delete(const Uri: string; const Tags: IHttpTagList = nil): IHttpMessage;
    function Trace(const Uri: string; const Tags: IHttpTagList = nil): IHttpMessage;(*)
  public
    constructor Create;
    destructor Destroy; override; 
  end;

implementation

uses
  SilSfHttpTokens;

{ TSilHttpClient }

constructor TSilHttpClient.Create;
begin

end;

destructor TSilHttpClient.Destroy;
begin

  inherited;
end;

(*)
function TSilHttpClient.Delete(const Uri: string;
  const Tags: IHttpTagList): IHttpMessage;
begin

end;

function TSilHttpClient.Get(const Uri: string;
  const Tags: IHttpTagList): IHttpMessage;
begin

end;

function TSilHttpClient.Head(const Uri: string;
  const Tags: IHttpTagList): IHttpMessage;
begin

end;

function TSilHttpClient.Options(const Uri: string;
  const Tags: IHttpTagList): IHttpMessage;
begin

end;

function TSilHttpClient.Put(const Uri: string; const Entity: IHttpEntity;
  const Tags: IHttpTagList): IHttpMessage;
begin

end;

function TSilHttpClient.Trace(const Uri: string;
  const Tags: IHttpTagList): IHttpMessage;
begin

end;
(*)

end.
