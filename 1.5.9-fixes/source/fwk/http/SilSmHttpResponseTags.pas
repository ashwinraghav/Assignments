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

unit SilSmHttpResponseTags;

{$I Defines.inc}

interface

uses
  Sil,
  SilLexer,
  SilSiHttpTypes,
  SilSiHttpTags,
  SilSkHttpObjectTags;

type
  TSilHttpResponseTags = class(
    TSilHttpObjectTags,
    IHttpResponseTags )
  private
    FAcceptRanges: IHttpTagAcceptRanges;
    FAge: IHttpTagAge;
    FETag: IHttpTagETag;
    FLocation: IHttpTagLocation;
    FProxyAuthenticate: IHttpTagProxyAuthenticate;
    FRetryAfter: IHttpTagRetryAfter;
    FServer: IHttpTagServer;
    FVary: IHttpTagVary;
    FWWWAuthenticate: IHttpTagWWWAuthenticate;
  protected // IHttpResponseTags
    function GetAcceptRanges: IHttpTagAcceptRanges;
    function GetAge: IHttpTagAge;
    function GetETag: IHttpTagETag;
    function GetLocation: IHttpTagLocation;
    function GetProxyAuthenticate: IHttpTagProxyAuthenticate;
    function GetRetryAfter: IHttpTagRetryAfter;
    function GetServer: IHttpTagServer;
    function GetVary: IHttpTagVary;
    function GetWWWAuthenticate: IHttpTagWWWAuthenticate;
  end;

implementation

{ TSilHttpResponseTags }

function TSilHttpResponseTags.GetAcceptRanges: IHttpTagAcceptRanges;
begin
  DoCheck(@FAcceptRanges, @Result, httAcceptRanges, IHttpTagAcceptRanges);
end;

function TSilHttpResponseTags.GetAge: IHttpTagAge;
begin
  DoCheck(@FAge, @Result, httAge, IHttpTagAge);
end;

function TSilHttpResponseTags.GetETag: IHttpTagETag;
begin
  DoCheck(@FETag, @Result, httETag, IHttpTagETag);
end;

function TSilHttpResponseTags.GetLocation: IHttpTagLocation;
begin
  DoCheck(@FLocation, @Result, httLocation, IHttpTagLocation);
end;

function TSilHttpResponseTags.GetProxyAuthenticate: IHttpTagProxyAuthenticate;
begin
  DoCheck(@FProxyAuthenticate, @Result, httProxyAuthenticate, IHttpTagProxyAuthenticate);
end;

function TSilHttpResponseTags.GetRetryAfter: IHttpTagRetryAfter;
begin
  DoCheck(@FRetryAfter, @Result, httRetryAfter, IHttpTagRetryAfter);
end;

function TSilHttpResponseTags.GetServer: IHttpTagServer;
begin
  DoCheck(@FServer, @Result, httServer, IHttpTagServer);
end;

function TSilHttpResponseTags.GetVary: IHttpTagVary;
begin
  DoCheck(@FVary, @Result, httVary, IHttpTagVary);
end;

function TSilHttpResponseTags.GetWWWAuthenticate: IHttpTagWWWAuthenticate;
begin
  DoCheck(@FWWWAuthenticate, @Result, httWWWAuthenticate, IHttpTagWWWAuthenticate);
end;

end.
 