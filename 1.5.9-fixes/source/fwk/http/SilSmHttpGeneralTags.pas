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

unit SilSmHttpGeneralTags;

{$I Defines.inc}

interface

uses
  Sil,
  SilLexer,
  SilSiHttpTypes,
  SilSiHttpTags,
  SilSkHttpObjectTags;

type
  TSilHttpGeneralTags = class(
    TSilHttpObjectTags,
    IHttpGeneralTags )
  private
    FCacheControl: IHttpTagCacheControl;
    FConnection: IHttpTagConnection;
    FDate: IHttpTagDate;
    FPragma: IHttpTagPragma;
    FTrailer: IHttpTagTrailer;
    FTransferEncoding: IHttpTagTransferEncoding;
    FUpgrade: IHttpTagUpgrade;
    FVia: IHttpTagVia;
    FWarning: IHttpTagWarning;
  protected // IHttpGeneralTags
    function GetCacheControl: IHttpTagCacheControl;
    function GetConnection: IHttpTagConnection;
    function GetDate: IHttpTagDate;
    function GetPragma: IHttpTagPragma;
    function GetTrailer: IHttpTagTrailer;
    function GetTransferEncoding: IHttpTagTransferEncoding;
    function GetUpgrade: IHttpTagUpgrade;
    function GetVia: IHttpTagVia;
    function GetWarning: IHttpTagWarning;
  end;
  
implementation

{ TSilHttpGeneralTags }

function TSilHttpGeneralTags.GetCacheControl: IHttpTagCacheControl;
begin
  DoCheck(@FCacheControl, @Result, httCacheControl, IHttpTagCacheControl);
end;

function TSilHttpGeneralTags.GetConnection: IHttpTagConnection;
begin
  DoCheck(@FConnection, @Result, httConnection, IHttpTagConnection);
end;

function TSilHttpGeneralTags.GetDate: IHttpTagDate;
begin
  DoCheck(@FDate, @Result, httDate, IHttpTagDate);
end;

function TSilHttpGeneralTags.GetPragma: IHttpTagPragma;
begin
  DoCheck(@FPragma, @Result, httPragma, IHttpTagPragma);
end;

function TSilHttpGeneralTags.GetTrailer: IHttpTagTrailer;
begin
  DoCheck(@FTrailer, @Result, httTrailer, IHttpTagTrailer);
end;

function TSilHttpGeneralTags.GetTransferEncoding: IHttpTagTransferEncoding;
begin
  DoCheck(@FTransferEncoding, @Result, httTransferEncoding, IHttpTagTransferEncoding);
end;

function TSilHttpGeneralTags.GetUpgrade: IHttpTagUpgrade;
begin
  DoCheck(@FUpgrade, @Result, httUpgrade, IHttpTagUpgrade);
end;

function TSilHttpGeneralTags.GetVia: IHttpTagVia;
begin
  DoCheck(@FVia, @Result, httVia, IHttpTagVia);
end;

function TSilHttpGeneralTags.GetWarning: IHttpTagWarning;
begin
  DoCheck(@FWarning, @Result, httWarning, IHttpTagWarning);
end;

end.
 