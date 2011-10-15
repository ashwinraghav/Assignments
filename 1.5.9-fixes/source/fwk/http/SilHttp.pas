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

unit SilHttp;

interface

{$include Defines.inc}

{$IFDEF D60}

uses
  SilSiHttpTypes,
  SilSiHttpData,
  SilSiHttpTags,
  SilSiHttpClient,
  SilStHttp;

type
  THttpStatus                                 = SilSiHttpTypes.THttpStatus;

const
  hstContinue                                 = THttpStatus(SilSiHttpTypes.hstContinue                                 );
  hstSwitchingProtocols                       = THttpStatus(SilSiHttpTypes.hstSwitchingProtocols                       );
  hstOK                                       = THttpStatus(SilSiHttpTypes.hstOK                                       );
  hstCreated                                  = THttpStatus(SilSiHttpTypes.hstCreated                                  );
  hstAccepted                                 = THttpStatus(SilSiHttpTypes.hstAccepted                                 );
  hstNonAuthoritativeInformation              = THttpStatus(SilSiHttpTypes.hstNonAuthoritativeInformation              );
  hstNoContent                                = THttpStatus(SilSiHttpTypes.hstNoContent                                );
  hstResetContent                             = THttpStatus(SilSiHttpTypes.hstResetContent                             );
  hstPartialContent                           = THttpStatus(SilSiHttpTypes.hstPartialContent                           );
  hstMultipleChoices                          = THttpStatus(SilSiHttpTypes.hstMultipleChoices                          );
  hstMovedPermanently                         = THttpStatus(SilSiHttpTypes.hstMovedPermanently                         );
  hstFound                                    = THttpStatus(SilSiHttpTypes.hstFound                                    );
  hstSeeOther                                 = THttpStatus(SilSiHttpTypes.hstSeeOther                                 );
  hstNotModified                              = THttpStatus(SilSiHttpTypes.hstNotModified                              );
  hstUseProxy                                 = THttpStatus(SilSiHttpTypes.hstUseProxy                                 );
  hstTemporaryRedirect                        = THttpStatus(SilSiHttpTypes.hstTemporaryRedirect                        );
  hseBadRequest                               = THttpStatus(SilSiHttpTypes.hseBadRequest                               );
  hseUnauthorized                             = THttpStatus(SilSiHttpTypes.hseUnauthorized                             );
  hsePaymentRequired                          = THttpStatus(SilSiHttpTypes.hsePaymentRequired                          );
  hseForbidden                                = THttpStatus(SilSiHttpTypes.hseForbidden                                );
  hseNotFound                                 = THttpStatus(SilSiHttpTypes.hseNotFound                                 );
  hseMethodNotAllowed                         = THttpStatus(SilSiHttpTypes.hseMethodNotAllowed                         );
  hseNotAcceptable                            = THttpStatus(SilSiHttpTypes.hseNotAcceptable                            );
  hseProxyAuthenticationRequired              = THttpStatus(SilSiHttpTypes.hseProxyAuthenticationRequired              );
  hseRequestTimeout                           = THttpStatus(SilSiHttpTypes.hseRequestTimeout                           );
  hseConflict                                 = THttpStatus(SilSiHttpTypes.hseConflict                                 );
  hseGone                                     = THttpStatus(SilSiHttpTypes.hseGone                                     );
  hseLengthRequired                           = THttpStatus(SilSiHttpTypes.hseLengthRequired                           );
  hsePreconditionFailed                       = THttpStatus(SilSiHttpTypes.hsePreconditionFailed                       );
  hseRequestEntityTooLarge                    = THttpStatus(SilSiHttpTypes.hseRequestEntityTooLarge                    );
  hseRequestURITooLarge                       = THttpStatus(SilSiHttpTypes.hseRequestURITooLarge                       );
  hseUnsupportedMediaType                     = THttpStatus(SilSiHttpTypes.hseUnsupportedMediaType                     );
  hseRequestedRangeNotSatisfiable             = THttpStatus(SilSiHttpTypes.hseRequestedRangeNotSatisfiable             );
  hseExpectationFailed                        = THttpStatus(SilSiHttpTypes.hseExpectationFailed                        );
  hseInternalServerError                      = THttpStatus(SilSiHttpTypes.hseInternalServerError                      );
  hseNotImplemented                           = THttpStatus(SilSiHttpTypes.hseNotImplemented                           );
  hseBadGateway                               = THttpStatus(SilSiHttpTypes.hseBadGateway                               );
  hseServiceUnavailable                       = THttpStatus(SilSiHttpTypes.hseServiceUnavailable                       );
  hseGatewayTimeOut                           = THttpStatus(SilSiHttpTypes.hseGatewayTimeOut                           );
  hseHTTPVersionNotSupported                  = THttpStatus(SilSiHttpTypes.hseHTTPVersionNotSupported                  );
  hsxExtension                                = THttpStatus(SilSiHttpTypes.hsxExtension                                );

type
  THttpMethod                                 = SilSiHttpTypes.THttpMethod;
                                              
const                                         
  httpUnknown                                 = THttpMethod(SilSiHttpTypes.httpUnknown  );
  httpOptions                                 = THttpMethod(SilSiHttpTypes.httpOptions  );
  httpGet                                     = THttpMethod(SilSiHttpTypes.httpGet      );
  httpHead                                    = THttpMethod(SilSiHttpTypes.httpHead     );
  httpPost                                    = THttpMethod(SilSiHttpTypes.httpPost     );
  httpPut                                     = THttpMethod(SilSiHttpTypes.httpPut      );
  httpDelete                                  = THttpMethod(SilSiHttpTypes.httpDelete   );
  httpTrace                                   = THttpMethod(SilSiHttpTypes.httpTrace    );
  httpConnect                                 = THttpMethod(SilSiHttpTypes.httpConnect  );
  httpExtension                               = THttpMethod(SilSiHttpTypes.httpExtension);
                                              
type                                          
  THttpTagKind                                = SilSiHttpTypes.THttpTagKind;
                                              
const                                         
  htkUnknown                                  = THttpTagKind(SilSiHttpTypes.htkUnknown  );
  htkGeneral                                  = THttpTagKind(SilSiHttpTypes.htkGeneral  );
  htkRequest                                  = THttpTagKind(SilSiHttpTypes.htkRequest  );
  htkResponse                                 = THttpTagKind(SilSiHttpTypes.htkResponse );
  htkEntity                                   = THttpTagKind(SilSiHttpTypes.htkEntity   );
  htkExtension                                = THttpTagKind(SilSiHttpTypes.htkExtension);
                                              
type                                          
  THttpTag                                    = SilSiHttpTypes.THttpTag;
                                              
const                                         
  httUnknown                                  = THttpTag(SilSiHttpTypes.httUnknown                  );
  httCacheControl                             = THttpTag(SilSiHttpTypes.httCacheControl             );
  httConnection                               = THttpTag(SilSiHttpTypes.httConnection               );
  httDate                                     = THttpTag(SilSiHttpTypes.httDate                     );
  httPragma                                   = THttpTag(SilSiHttpTypes.httPragma                   );
  httTrailer                                  = THttpTag(SilSiHttpTypes.httTrailer                  );
  httTransferEncoding                         = THttpTag(SilSiHttpTypes.httTransferEncoding         );
  httUpgrade                                  = THttpTag(SilSiHttpTypes.httUpgrade                  );
  httVia                                      = THttpTag(SilSiHttpTypes.httVia                      );
  httWarning                                  = THttpTag(SilSiHttpTypes.httWarning                  );
  httAccept                                   = THttpTag(SilSiHttpTypes.httAccept                   );
  httAcceptCharset                            = THttpTag(SilSiHttpTypes.httAcceptCharset            );
  httAcceptEncoding                           = THttpTag(SilSiHttpTypes.httAcceptEncoding           );
  httAcceptLanguage                           = THttpTag(SilSiHttpTypes.httAcceptLanguage           );
  httAuthorization                            = THttpTag(SilSiHttpTypes.httAuthorization            );
  httExpect                                   = THttpTag(SilSiHttpTypes.httExpect                   );
  httFrom                                     = THttpTag(SilSiHttpTypes.httFrom                     );
  httHost                                     = THttpTag(SilSiHttpTypes.httHost                     );
  httIfMatch                                  = THttpTag(SilSiHttpTypes.httIfMatch                  );
  httIfModifiedSince                          = THttpTag(SilSiHttpTypes.httIfModifiedSince          );
  httIfNoneMatch                              = THttpTag(SilSiHttpTypes.httIfNoneMatch              );
  httIfRange                                  = THttpTag(SilSiHttpTypes.httIfRange                  );
  httIfUnmodifiedSince                        = THttpTag(SilSiHttpTypes.httIfUnmodifiedSince        );
  httMaxForwards                              = THttpTag(SilSiHttpTypes.httMaxForwards              );
  httProxyAuthorization                       = THttpTag(SilSiHttpTypes.httProxyAuthorization       );
  httRange                                    = THttpTag(SilSiHttpTypes.httRange                    );
  httReferer                                  = THttpTag(SilSiHttpTypes.httReferer                  );
  httTE                                       = THttpTag(SilSiHttpTypes.httTE                       );
  httUserAgent                                = THttpTag(SilSiHttpTypes.httUserAgent                );
  httAcceptRanges                             = THttpTag(SilSiHttpTypes.httAcceptRanges             );
  httAge                                      = THttpTag(SilSiHttpTypes.httAge                      );
  httETag                                     = THttpTag(SilSiHttpTypes.httETag                     );
  httLocation                                 = THttpTag(SilSiHttpTypes.httLocation                 );
  httProxyAuthenticate                        = THttpTag(SilSiHttpTypes.httProxyAuthenticate        );
  httRetryAfter                               = THttpTag(SilSiHttpTypes.httRetryAfter               );
  httServer                                   = THttpTag(SilSiHttpTypes.httServer                   );
  httVary                                     = THttpTag(SilSiHttpTypes.httVary                     );
  httWWWAuthenticate                          = THttpTag(SilSiHttpTypes.httWWWAuthenticate          );
  httAllow                                    = THttpTag(SilSiHttpTypes.httAllow                    );
  httContentEncoding                          = THttpTag(SilSiHttpTypes.httContentEncoding          );
  httContentLanguage                          = THttpTag(SilSiHttpTypes.httContentLanguage          );
  httContentLength                            = THttpTag(SilSiHttpTypes.httContentLength            );
  httContentLocation                          = THttpTag(SilSiHttpTypes.httContentLocation          );
  httContentMD5                               = THttpTag(SilSiHttpTypes.httContentMD5               );
  httContentRange                             = THttpTag(SilSiHttpTypes.httContentRange             );
  httContentType                              = THttpTag(SilSiHttpTypes.httContentType              );
  httExpires                                  = THttpTag(SilSiHttpTypes.httExpires                  );
  httLastModified                             = THttpTag(SilSiHttpTypes.httLastModified             );
  httExtension                                = THttpTag(SilSiHttpTypes.httExtension                );
                                              
type                                          
  THttpTagDateFormat                          = SilSiHttpTypes.THttpTagDateFormat;
                                              
const                                         
  hdfRfc1123                                  = THttpTagDateFormat(SilSiHttpTypes.hdfRfc1123);
  hdfRfc850                                   = THttpTagDateFormat(SilSiHttpTypes.hdfRfc850 );
  hdfAsctime                                  = THttpTagDateFormat(SilSiHttpTypes.hdfAsctime);
                                              
type                                          
  IHttpObject                                 = SilSiHttpTypes.IHttpObject ;
  IHttpVersion                                = SilSiHttpTypes.IHttpVersion;
  IHttpDate                                   = SilSiHttpTypes.IHttpDate   ;
  IHttpHost                                   = SilSiHttpTypes.IHttpHost   ;
  IHttpUri                                    = SilSiHttpTypes.IHttpUri    ;
                                              
type                                          
  IHttpMessage                                = SilSiHttpData.IHttpMessage       ;
  IHttpRequest                                = SilSiHttpData.IHttpRequest       ;
  IHttpFiler                                  = SilSiHttpData.IHttpFiler         ;
  IHttpRequestParams                          = SilSiHttpData.IHttpRequestParams ;
  IHttpRequestUri                             = SilSiHttpData.IHttpRequestUri    ;
  IHttpResponse                               = SilSiHttpData.IHttpResponse      ;
  IHttpResponseStatus                         = SilSiHttpData.IHttpResponseStatus;
  IHttpEntity                                 = SilSiHttpData.IHttpEntity        ;
                                              
type                                          
  IHttpTag                                    = SilSiHttpTags.IHttpTag                    ; 
  IHttpTags                                   = SilSiHttpTags.IHttpTags                   ; 
  IHttpTagList                                = SilSiHttpTags.IHttpTagList                ;
                                              
type                                          
  THttpAcceptRanges                           = SilSiHttpTags.THttpAcceptRanges;
                                              
const                                         
  harBytes                                    = THttpAcceptRanges(SilSiHttpTags.harBytes);
  harNone                                     = THttpAcceptRanges(SilSiHttpTags.harNone );
  harOther                                    = THttpAcceptRanges(SilSiHttpTags.harOther);
                                              
type                                          
  IHttpHostInfo                               = SilSiHttpTags.IHttpHostInfo               ; 
  IHttpProtocolInfo                           = SilSiHttpTags.IHttpProtocolInfo           ;
  IHttpWarning                                = SilSiHttpTags.IHttpWarning                ;
  IHttpRange                                  = SilSiHttpTags.IHttpRange                  ; 
  IHttpEntityTag                              = SilSiHttpTags.IHttpEntityTag              ;
                                              
type                                          
  IHttpTagCacheControl                        = SilSiHttpTags.IHttpTagCacheControl        ; 
  IHttpTagConnection                          = SilSiHttpTags.IHttpTagConnection          ;
  IHttpTagDate                                = SilSiHttpTags.IHttpTagDate                ;
  IHttpTagPragma                              = SilSiHttpTags.IHttpTagPragma              ;
  IHttpTagTrailer                             = SilSiHttpTags.IHttpTagTrailer             ;
  IHttpTagTransferEncoding                    = SilSiHttpTags.IHttpTagTransferEncoding    ;
  IHttpTagUpgrade                             = SilSiHttpTags.IHttpTagUpgrade             ;
  IHttpTagVia                                 = SilSiHttpTags.IHttpTagVia                 ;
  IHttpTagWarning                             = SilSiHttpTags.IHttpTagWarning             ;
  IHttpTagAcceptRanges                        = SilSiHttpTags.IHttpTagAcceptRanges        ; 
  IHttpTagAge                                 = SilSiHttpTags.IHttpTagAge                 ; 
  IHttpTagETag                                = SilSiHttpTags.IHttpTagETag                ; 
  IHttpTagLocation                            = SilSiHttpTags.IHttpTagLocation            ; 
  IHttpTagProxyAuthenticate                   = SilSiHttpTags.IHttpTagProxyAuthenticate   ; 
  IHttpTagRetryAfter                          = SilSiHttpTags.IHttpTagRetryAfter          ; 
  IHttpTagServer                              = SilSiHttpTags.IHttpTagServer              ; 
  IHttpTagVary                                = SilSiHttpTags.IHttpTagVary                ; 
  IHttpTagWWWAuthenticate                     = SilSiHttpTags.IHttpTagWWWAuthenticate     ; 
  IHttpTagAllow                               = SilSiHttpTags.IHttpTagAllow               ; 
  IHttpTagAccept                              = SilSiHttpTags.IHttpTagAccept              ; 
  IHttpTagAcceptCharset                       = SilSiHttpTags.IHttpTagAcceptCharset       ; 
  IHttpTagAcceptEncoding                      = SilSiHttpTags.IHttpTagAcceptEncoding      ; 
  IHttpTagAcceptLanguage                      = SilSiHttpTags.IHttpTagAcceptLanguage      ; 
  IHttpTagAuthorization                       = SilSiHttpTags.IHttpTagAuthorization       ; 
  IHttpTagExpect                              = SilSiHttpTags.IHttpTagExpect              ; 
  IHttpTagFrom                                = SilSiHttpTags.IHttpTagFrom                ; 
  IHttpTagHost                                = SilSiHttpTags.IHttpTagHost                ; 
  IHttpTagIfMatch                             = SilSiHttpTags.IHttpTagIfMatch             ; 
  IHttpTagIfModifiedSince                     = SilSiHttpTags.IHttpTagIfModifiedSince     ; 
  IHttpTagIfNoneMatch                         = SilSiHttpTags.IHttpTagIfNoneMatch         ; 
  IHttpTagIfRange                             = SilSiHttpTags.IHttpTagIfRange             ; 
  IHttpTagIfUnmodifiedSince                   = SilSiHttpTags.IHttpTagIfUnmodifiedSince   ; 
  IHttpTagMaxForwards                         = SilSiHttpTags.IHttpTagMaxForwards         ; 
  IHttpTagProxyAuthorization                  = SilSiHttpTags.IHttpTagProxyAuthorization  ; 
  IHttpTagRange                               = SilSiHttpTags.IHttpTagRange               ; 
  IHttpTagReferer                             = SilSiHttpTags.IHttpTagReferer             ; 
  IHttpTagTE                                  = SilSiHttpTags.IHttpTagTE                  ; 
  IHttpTagUserAgent                           = SilSiHttpTags.IHttpTagUserAgent           ; 
  IHttpTagContentEncoding                     = SilSiHttpTags.IHttpTagContentEncoding     ; 
  IHttpTagContentLanguage                     = SilSiHttpTags.IHttpTagContentLanguage     ; 
  IHttpTagContentLength                       = SilSiHttpTags.IHttpTagContentLength       ; 
  IHttpTagContentLocation                     = SilSiHttpTags.IHttpTagContentLocation     ; 
  IHttpTagContentMD5                          = SilSiHttpTags.IHttpTagContentMD5          ; 
  IHttpTagContentRange                        = SilSiHttpTags.IHttpTagContentRange        ; 
  IHttpTagContentType                         = SilSiHttpTags.IHttpTagContentType         ; 
  IHttpTagExpires                             = SilSiHttpTags.IHttpTagExpires             ; 
  IHttpTagLastModified                        = SilSiHttpTags.IHttpTagLastModified        ;
                                              
type                                          
  IHttpGeneralTags                            = SilSiHttpTags.IHttpGeneralTags            ;
  IHttpRequestTags                            = SilSiHttpTags.IHttpRequestTags            ;
  IHttpResponseTags                           = SilSiHttpTags.IHttpResponseTags           ;
  IHttpEntityTags                             = SilSiHttpTags.IHttpEntityTags             ;

type
  IHttpClient                                 = SilSiHttpClient.IHttpClient   ;
  IHttpProcessor                              = SilSiHttpClient.IHttpProcessor;

type                                          
  Tk                                          = SilStHttp.Http;
                                              
{$ENDIF}

implementation
end.
