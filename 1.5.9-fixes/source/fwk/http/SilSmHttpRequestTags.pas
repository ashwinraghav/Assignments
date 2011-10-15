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

unit SilSmHttpRequestTags;

{$I Defines.inc}

interface

uses
  Sil,
  SilLexer,
  SilSiHttpTypes,
  SilSiHttpTags,
  SilSkHttpObjectTags;

type
  TSilHttpRequestTags = class(
    TSilHttpObjectTags,
    IHttpRequestTags )
  private
    FAccept: IHttpTagAccept;
    FAcceptCharset: IHttpTagAcceptCharset;
    FAcceptEncoding: IHttpTagAcceptEncoding;
    FAcceptLanguage: IHttpTagAcceptLanguage;
    FAuthorization: IHttpTagAuthorization;
    FExpect: IHttpTagExpect;
    FFrom: IHttpTagFrom;
    FHost: IHttpTagHost;
    FIfMatch: IHttpTagIfMatch;
    FIfModifiedSince: IHttpTagIfModifiedSince;
    FIfNoneMatch: IHttpTagIfNoneMatch;
    FIfRange: IHttpTagIfRange;
    FIfUnmodifiedSince: IHttpTagIfUnmodifiedSince;
    FMaxForwards: IHttpTagMaxForwards;
    FProxyAuthorization: IHttpTagProxyAuthorization;
    FRange: IHttpTagRange;
    FReferer: IHttpTagReferer;
    FTE: IHttpTagTE;
    FUserAgent: IHttpTagUserAgent;
  protected // IHttpRequestTags
    function GetAccept: IHttpTagAccept;
    function GetAcceptCharset: IHttpTagAcceptCharset;
    function GetAcceptEncoding: IHttpTagAcceptEncoding;
    function GetAcceptLanguage: IHttpTagAcceptLanguage;
    function GetAuthorization: IHttpTagAuthorization;
    function GetExpect: IHttpTagExpect;
    function GetFrom: IHttpTagFrom;
    function GetHost: IHttpTagHost;
    function GetIfMatch: IHttpTagIfMatch;
    function GetIfModifiedSince: IHttpTagIfModifiedSince;
    function GetIfNoneMatch: IHttpTagIfNoneMatch;
    function GetIfRange: IHttpTagIfRange;
    function GetIfUnmodifiedSince: IHttpTagIfUnmodifiedSince;
    function GetMaxForwards: IHttpTagMaxForwards;
    function GetProxyAuthorization: IHttpTagProxyAuthorization;
    function GetRange: IHttpTagRange;
    function GetReferer: IHttpTagReferer;
    function GetTE: IHttpTagTE;
    function GetUserAgent: IHttpTagUserAgent;
  end;

implementation

{ TSilHttpRequestTags }

function TSilHttpRequestTags.GetAccept: IHttpTagAccept;
begin
  DoCheck(@FAccept, @Result, httAccept, IHttpTagAccept);
end;

function TSilHttpRequestTags.GetAcceptCharset: IHttpTagAcceptCharset;
begin
  DoCheck(@FAcceptCharset, @Result, httAcceptCharset, IHttpTagAcceptCharset);
end;

function TSilHttpRequestTags.GetAcceptEncoding: IHttpTagAcceptEncoding;
begin
  DoCheck(@FAcceptEncoding, @Result, httAcceptEncoding, IHttpTagAcceptEncoding);
end;

function TSilHttpRequestTags.GetAcceptLanguage: IHttpTagAcceptLanguage;
begin
  DoCheck(@FAcceptLanguage, @Result, httAcceptLanguage, IHttpTagAcceptLanguage);
end;

function TSilHttpRequestTags.GetAuthorization: IHttpTagAuthorization;
begin
  DoCheck(@FAuthorization, @Result, httAuthorization, IHttpTagAuthorization);
end;

function TSilHttpRequestTags.GetExpect: IHttpTagExpect;
begin
  DoCheck(@FExpect, @Result, httExpect, IHttpTagExpect);
end;

function TSilHttpRequestTags.GetFrom: IHttpTagFrom;
begin
  DoCheck(@FFrom, @Result, httFrom, IHttpTagFrom);
end;

function TSilHttpRequestTags.GetHost: IHttpTagHost;
begin
  DoCheck(@FHost, @Result, httHost, IHttpTagHost);
end;

function TSilHttpRequestTags.GetIfMatch: IHttpTagIfMatch;
begin
  DoCheck(@FIfMatch, @Result, httIfMatch, IHttpTagIfMatch);
end;

function TSilHttpRequestTags.GetIfModifiedSince: IHttpTagIfModifiedSince;
begin
  DoCheck(@FIfModifiedSince, @Result, httIfModifiedSince, IHttpTagIfModifiedSince);
end;

function TSilHttpRequestTags.GetIfNoneMatch: IHttpTagIfNoneMatch;
begin
  DoCheck(@FIfNoneMatch, @Result, httIfNoneMatch, IHttpTagIfNoneMatch);
end;

function TSilHttpRequestTags.GetIfRange: IHttpTagIfRange;
begin
  DoCheck(@FIfRange, @Result, httIfRange, IHttpTagIfRange);
end;

function TSilHttpRequestTags.GetIfUnmodifiedSince: IHttpTagIfUnmodifiedSince;
begin
  DoCheck(@FIfUnmodifiedSince, @Result, httIfUnmodifiedSince, IHttpTagIfUnmodifiedSince);
end;

function TSilHttpRequestTags.GetMaxForwards: IHttpTagMaxForwards;
begin
  DoCheck(@FMaxForwards, @Result, httMaxForwards, IHttpTagMaxForwards);
end;

function TSilHttpRequestTags.GetProxyAuthorization: IHttpTagProxyAuthorization;
begin
  DoCheck(@FProxyAuthorization, @Result, httProxyAuthorization, IHttpTagProxyAuthorization);
end;

function TSilHttpRequestTags.GetRange: IHttpTagRange;
begin
  DoCheck(@FRange, @Result, httRange, IHttpTagRange);
end;

function TSilHttpRequestTags.GetReferer: IHttpTagReferer;
begin
  DoCheck(@FReferer, @Result, httReferer, IHttpTagReferer);
end;

function TSilHttpRequestTags.GetTE: IHttpTagTE;
begin
  DoCheck(@FTE, @Result, httTE, IHttpTagTE);
end;

function TSilHttpRequestTags.GetUserAgent: IHttpTagUserAgent;
begin
  DoCheck(@FUserAgent, @Result, httUserAgent, IHttpTagUserAgent);
end;

end.
