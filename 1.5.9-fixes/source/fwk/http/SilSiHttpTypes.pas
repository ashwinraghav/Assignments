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

unit SilSiHttpTypes;

interface

uses
  Sil;

type
  THttpMethod = (
      httpUnknown,
      httpOptions,
      httpGet,
      httpHead,
      httpPost,
      httpPut,
      httpDelete,
      httpTrace,
      httpConnect,
      httpExtension
    );

type
  THttpTagKind = (
      htkUnknown,
      htkGeneral,
      htkRequest,
      htkResponse,
      htkEntity,
      htkExtension
    );

type
  THttpTag = (
      httUnknown,
      //General
      httCacheControl, httConnection, httDate, httPragma, httTrailer,
      httTransferEncoding, httUpgrade, httVia, httWarning,
      //Request
      httAccept, httAcceptCharset, httAcceptEncoding, httAcceptLanguage,
      httAuthorization, httExpect, httFrom, httHost,
      httIfMatch, httIfModifiedSince, httIfNoneMatch, httIfRange, httIfUnmodifiedSince,
      httMaxForwards,
      httProxyConnection, httProxyAuthorization,
      httRange, httReferer, httTE, httUserAgent,
      // Response
      httAcceptRanges, httAge, httETag, httLocation, httProxyAuthenticate,
      httRetryAfter, httServer, httVary, httWWWAuthenticate,
      //Entity
      httAllow,
      httContentEncoding, httContentLanguage, httContentLength, httContentLocation,
      httContentMD5, httContentRange, httContentType,
      httExpires, httLastModified,
      //extension-header
      httExtension
    );

type
  THttpStatus =
    (
      hstContinue,                               //  "100"
      hstSwitchingProtocols,                     //  "101"
      hstOK,                                     //  "200"
      hstCreated,                                //  "201"
      hstAccepted,                               //  "202"
      hstNonAuthoritativeInformation,            //  "203"
      hstNoContent,                              //  "204"
      hstResetContent,                           //  "205"
      hstPartialContent,                         //  "206"
      hstMultipleChoices,                        //  "300"
      hstMovedPermanently,                       //  "301"
      hstFound,                                  //  "302"
      hstSeeOther,                               //  "303"
      hstNotModified,                            //  "304"
      hstUseProxy,                               //  "305"
      hstTemporaryRedirect,                      //  "307"
      hseBadRequest,                             //  "400"
      hseUnauthorized,                           //  "401"
      hsePaymentRequired,                        //  "402"
      hseForbidden,                              //  "403"
      hseNotFound,                               //  "404"
      hseMethodNotAllowed,                       //  "405"
      hseNotAcceptable,                          //  "406"
      hseProxyAuthenticationRequired,            //  "407"
      hseRequestTimeout,                         //  "408"
      hseConflict,                               //  "409"
      hseGone,                                   //  "410"
      hseLengthRequired,                         //  "411"
      hsePreconditionFailed,                     //  "412"
      hseRequestEntityTooLarge,                  //  "413"
      hseRequestURITooLarge,                     //  "414"
      hseUnsupportedMediaType,                   //  "415"
      hseRequestedRangeNotSatisfiable,           //  "416"
      hseExpectationFailed,                      //  "417"
      hseInternalServerError,                    //  "500"
      hseNotImplemented,                         //  "501"
      hseBadGateway,                             //  "502"
      hseServiceUnavailable,                     //  "503"
      hseGatewayTimeOut,                         //  "504"
      hseHTTPVersionNotSupported,                //  "505"
      hsxExtension                               //  extension-code
    );

type
  IHttpObject = interface
    ['{5D113284-9060-46FB-8AFA-2D0CA7E70A43}']
    procedure WriteTo(const Writer: IWriter);
  end;

  IHttpVersion = interface
    ['{830E89D9-011A-45C2-B5B6-8F0C64D5EBE3}']
    function GetMajor: Integer;
    procedure SetMajor(Value: Integer);
    function GetMinor: Integer;
    procedure SetMinor(Value: Integer);
    property Major: Integer read GetMajor write SetMajor;
    property Minor: Integer read GetMinor write SetMinor;
  end;

  THttpTagDateFormat = (hdfRfc1123, hdfRfc850, hdfAsctime);

  IHttpDate = interface
    ['{2604E0E5-C3AB-488A-A77C-33C959075271}']
    function GetValue: TDateTime;
    function GetFormat: THttpTagDateFormat;
    property Value: TDateTime read GetValue;
    property Format: THttpTagDateFormat read GetFormat;
  end;

  IHttpHost = interface
    ['{C4875352-3934-4CE8-94D8-3402DD1FC9EE}']
    function GetHost: String;
    function GetPort: Word;
    property Host: String read GetHost;
    property Port: Word read GetPort;
  end;

  IHttpUri = interface
    ['{A1570DF8-588C-450C-90D7-33BD74121CF8}']
  end;

implementation
end.
