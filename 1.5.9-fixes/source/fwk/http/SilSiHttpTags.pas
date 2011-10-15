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

unit SilSiHttpTags;

interface

uses
  Sil,
  SilSiHttpTypes;

type
  IHttpTag = interface  (IHttpObject)
    ['{B91C925E-6B7E-4D28-9CC0-E6390A305138}']
    function GetID: THttpTag;
    function GetKind: THttpTagKind;
    function GetName: string;
    function GetValue: Variant;
    procedure SetValue(const Value: Variant);
    property Id: THttpTag read GetID;
    property Kind: THttpTagKind read GetKind;
    property Name: string read GetName;
    property Value: Variant read GetValue write SetValue;
  end;

  IHttpTags = interface  (IHttpObject)
    ['{BD8E090D-851D-4F61-9577-383551F24E83}']
    function GetCount: Integer;
    function Enumerate(var Enum: IEnumerator; out Tag: IHttpTag): Boolean; overload;
    function Enumerate(var Enum: IEnumerator; const IID: TGUID; out Tag): Boolean; overload;
    function Get(const Name: string): IHttpTag;
    function Find(const IID: TGUID; out Tag): Boolean; overload;
    function Find(const Name: string; out Tag: IHttpTag): Boolean; overload;
    function Find(const ID: THttpTag; out Tag: IHttpTag): Boolean; overload;
    function IsPresent(const IID: TGUID): Boolean; overload;
    function IsPresent(const Name: string): Boolean; overload;
    function IsPresent(const ID: THttpTag): Boolean; overload;
    property Count: Integer read GetCount;
    property Tag[const Name: string]: IHttpTag read Get; default;
  end;

  IHttpTagList = interface (IHttpTags)
    ['{EE5511B8-29BB-4635-8D9F-653E7CA1919F}']
    function Add(const Tag: IHttpTag): IHttpTag; overload;
    function Add(const Name: string; const Value: string = ''): IHttpTag; overload;
    function Add(const ID: THttpTag; const Value: string = ''): IHttpTag; overload;
  end;

  IHttpHostInfo = interface (IHttpHost)
    ['{C4875352-3934-4CE8-94D8-3402DD1FC9EE}']
    function GetPseudonym: String;
    property Pseudonym: String read GetPseudonym;
  end;

  IHttpProtocolInfo = interface
    ['{5E0ED1A6-BEE3-4C01-8599-A1A14802C927}']
    function GetName: String;
    function GetVersion: Double;
    property Name: String read GetName;
    property Version: Double read GetVersion;
  end;
  
  IHttpWarning = interface
    ['{90D4328D-A2D4-49CB-97E9-0C5B134ED7F1}']
    function GetCode: Word;
    function GetAgent: IHttpHostInfo;
    function GetText: String;
    function GetDate: IHttpDate;
    property Code: Word read GetCode;
    property Agent: IHttpHostInfo read GetAgent;
    property Text: String read GetText;
    property Date: IHttpDate read GetDate;
  end;

  THttpAcceptRanges = (
      harBytes,
      harNone,
      harOther
    );

  IHttpRange = interface
    ['{AF208C60-444E-4F6B-ADEC-C24966414D78}']
    function GetRange: THttpAcceptRanges;
    function GetOther: String;
    property Range: THttpAcceptRanges read GetRange;
    property Other: String read GetOther;
  end;

  IHttpEntityTag = interface
    ['{9B2D6623-C6E6-4FBF-96BF-F47147A0E93B}']
    function GetIsAll: Boolean; // *
    function GetIsWeak: Boolean;
    function GetOpaqueTag: String;
    property IsAll: Boolean read GetIsAll;
    property IsWeak: Boolean read GetIsWeak;
    property OpaqueTag: String read GetOpaqueTag;
  end;
 
  IHttpTagCacheControl = interface (IHttpTag)
    ['{D2539223-1B98-499D-81DA-A3F227C3C89A}']
    (*)
    Cache-Control   = "Cache-Control" ":" 1#cache-directive
    cache-directive = cache-request-directive
         | cache-response-directive
    cache-request-directive =
           "no-cache"                          ; Section 14.9.1
         | "no-store"                          ; Section 14.9.2
         | "max-age" "=" delta-seconds         ; Section 14.9.3, 14.9.4
         | "max-stale" [ "=" delta-seconds ]   ; Section 14.9.3
         | "min-fresh" "=" delta-seconds       ; Section 14.9.3
         | "no-transform"                      ; Section 14.9.5
         | "only-if-cached"                    ; Section 14.9.4
         | cache-extension                     ; Section 14.9.6
     cache-response-directive =
           "public"                               ; Section 14.9.1
         | "private" [ "=" <"> 1#field-name <"> ] ; Section 14.9.1
         | "no-cache" [ "=" <"> 1#field-name <"> ]; Section 14.9.1
         | "no-store"                             ; Section 14.9.2
         | "no-transform"                         ; Section 14.9.5
         | "must-revalidate"                      ; Section 14.9.4
         | "proxy-revalidate"                     ; Section 14.9.4
         | "max-age" "=" delta-seconds            ; Section 14.9.3
         | "s-maxage" "=" delta-seconds           ; Section 14.9.3
         | cache-extension                        ; Section 14.9.6
    cache-extension = token [ "=" ( token | quoted-string ) ]
    (*)
  end;

  IHttpTagConnection = interface (IHttpTag)
    ['{1FC74BCB-D493-4CB0-875A-7941556EFB1D}']
    function GetToken: String;
    property Token: String read GetToken;
    (*)
       Connection = "Connection" ":" 1#(connection-token)
       connection-token  = token
       Connection: close
    (*)
  end;

  IHttpTagDate = interface (IHttpTag)
    ['{C0AFD28C-83BB-470D-8A8B-C3F3776C8AB3}']
    function GetDate: IHttpDate;
    property Date: IHttpDate read GetDate;
    (*)
       Date  = "Date" ":" HTTP-date
       HTTP-date    = rfc1123-date | rfc850-date | asctime-date
       rfc1123-date = wkday "," SP date1 SP time SP "GMT"
       rfc850-date  = weekday "," SP date2 SP time SP "GMT"
       asctime-date = wkday SP date3 SP time SP 4DIGIT
       date1        = 2DIGIT SP month SP 4DIGIT
                      ; day month year (e.g., 02 Jun 1982)
       date2        = 2DIGIT "-" month "-" 2DIGIT
                      ; day-month-year (e.g., 02-Jun-82)
       date3        = month SP ( 2DIGIT | ( SP 1DIGIT ))
                      ; month day (e.g., Jun  2)
       time         = 2DIGIT ":" 2DIGIT ":" 2DIGIT
                      ; 00:00:00 - 23:59:59
       wkday        = "Mon" | "Tue" | "Wed"
                    | "Thu" | "Fri" | "Sat" | "Sun"
       weekday      = "Monday" | "Tuesday" | "Wednesday"
                    | "Thursday" | "Friday" | "Saturday" | "Sunday"
       month        = "Jan" | "Feb" | "Mar" | "Apr"
                    | "May" | "Jun" | "Jul" | "Aug"
                    | "Sep" | "Oct" | "Nov" | "Dec"
    (*)
  end;

  IHttpTagPragma = interface (IHttpTag)
    ['{69653B70-F33E-4B7A-8457-EAFD38E4A0CC}']
    (*)
       Pragma            = "Pragma" ":" 1#pragma-directive
       pragma-directive  = "no-cache" | extension-pragma
       extension-pragma  = token [ "=" ( token | quoted-string ) ]
    (*)
  end;

  IHttpTagTrailer = interface (IHttpTag)
    ['{F31CDF9F-A46C-4D22-BAF7-28B09C939C30}']
      //function GetFieldName: String;
      //property FieldName: String read GetFieldName;
    (*)
       Trailer  = "Trailer" ":" 1#field-name
    (*)
  end;

  IHttpTagTransferEncoding = interface (IHttpTag)
    ['{3531839A-8C72-48C9-8D34-741C50D635D7}']
    (*)
     Transfer-Encoding       = "Transfer-Encoding" ":" 1#transfer-coding
    (*)
  end;

  IHttpTagUpgrade = interface (IHttpTag)
    ['{C577E112-76AF-4267-A45D-83B31642C6E2}']
    (*)
       Upgrade        = "Upgrade" ":" 1#product
    (*)
  end;

  IHttpTagVia = interface (IHttpTag)
    ['{22588D0A-C643-4A08-B2D6-E252C6364242}']
    //property ReceivedProtocol: list of IHttpProtocolInfo;
    //property ReceivedBy: list of IHttpHostInfo;
    (*)
      Via =  "Via" ":" 1#( received-protocol received-by [ comment ] )
      received-protocol = [ protocol-name "/" ] protocol-version
      protocol-name     = token
      protocol-version  = token
      received-by       = ( host [ ":" port ] ) | pseudonym
      pseudonym         = token
    (*)
  end;

  IHttpTagWarning = interface (IHttpTag)
    ['{4AAE234F-4892-46E3-A6B4-62E21A20AD27}']
    //property Warnings: list of IHttpWarning; 
    (*)
       Warning    = "Warning" ":" 1#warning-value
       warning-value = warn-code SP warn-agent SP warn-text [SP warn-date]
       warn-code  = 3DIGIT
       warn-agent = ( host [ ":" port ] ) | pseudonym ; the name or pseudonym of the server adding the Warning header, for use in debugging
       warn-text  = quoted-string
       warn-date  = <"> HTTP-date <">
    (*)
  end;

  IHttpTagAcceptRanges = interface (IHttpTag)
    ['{48AA6B6D-6917-4464-A25B-A0355ED94F43}']
    // property Value: list of IHttpRange;
    (*)
          Accept-Ranges     = "Accept-Ranges" ":" acceptable-ranges
          acceptable-ranges = 1#range-unit | "none"
    (*)
  end;

  IHttpTagAge = interface (IHttpTag)
    ['{922A84C7-1205-4AC4-839F-B0E0E66E2B0C}']
    function GetValue: LongWord;
    property Value: LongWord read GetValue;
    (*)
           Age = "Age" ":" age-value
           age-value = delta-seconds
    (*)
  end;

  IHttpTagETag = interface (IHttpTag)
    ['{4D24749B-913C-4AF4-912E-054748ABFA3D}']
    function GetEntity: IHttpEntityTag;
    property Entity: IHttpEntityTag read GetEntity;
    (*)
      ETag = "ETag" ":" entity-tag
      entity-tag = [ weak ] opaque-tag
      weak       = "W/"
      opaque-tag = quoted-string
    (*)
  end;

  IHttpTagLocation = interface (IHttpTag)
    ['{CE67F054-3937-4692-8C8D-C9DAD998B02B}']
    function GetValue: IHttpUri;
    property Value: IHttpUri read GetValue;
    (*)
       Location       = "Location" ":" absoluteURI
    (*)
  end;

  IHttpTagProxyAuthenticate = interface (IHttpTag)
    ['{CB4BAFF3-DDE0-4FA1-90B2-252FC2F479B6}']
    // property Challenge: list of String;
    (*)
       Proxy-Authenticate  = "Proxy-Authenticate" ":" 1#challenge
    (*)
  end;



  IHttpTagRetryAfter = interface (IHttpTag)
    ['{89347CB1-5B87-4C05-BAF9-ED3C7366C5AE}']
    function GetIsAbsolute: Boolean;    
    function GetDate: IHttpDate;
    function GetSeconds: LongWord;
    property IsAbsolute: Boolean read GetIsAbsolute;    
    property Date: IHttpDate read GetDate;
    property Seconds: LongWord read GetSeconds;
    (*)
       Retry-After  = "Retry-After" ":" ( HTTP-date | delta-seconds )
    (*)
  end;

  IHttpTagServer = interface (IHttpTag)
    ['{3407F603-1E34-4FEB-9A0F-E8B3BFE8E434}']
    //property Product: list of String;
    (*)
       Server         = "Server" ":" 1*( product | comment )
    (*)
  end;

  IHttpTagVary = interface (IHttpTag)
    ['{67CC5D35-5082-42E8-B230-483F2238B637}']
    // property FieldName: list of String;
    (*)
       Vary  = "Vary" ":" ( "*" | 1#field-name )
    (*)
  end;

  IHttpTagWWWAuthenticate = interface (IHttpTag)
    ['{13D4205A-15B3-4CA1-92DB-9A82F3608E92}']
    //property Challenge: list of String;
    (*)

       WWW-Authenticate  = "WWW-Authenticate" ":" 1#challenge
    (*)
  end;

  IHttpTagAllow = interface (IHttpTag)

    ['{2FB22F36-7D08-4AA1-887E-A64740FA25E9}']
    // property Method: list of String;
    (*)
          Allow   = "Allow" ":" #Method
    (*)
  end;

  IHttpTagAccept = interface (IHttpTag)
    ['{DCF22A66-A539-4E5E-BF8E-B533EDA79442}']
    (*)property MediaRanges: IHttpMediaRangeList;
      IHttpMediaRange = interface
        ['{BC15F354-DDF3-43AE-BFCB-8388CA26F7A0}']
        property MediaType: String;
        property MediaSubtype: String;
        property Parameters: IParameterList;
      end;

      property QValue: Word;
      property Parameters: IParameterList;(*)

    (*)
       Accept         = "Accept" ":"
                        #( media-range [ accept-params ] )
       media-range    = ( "*/*"
                        | ( type "/" "*" )
                        | ( type "/" subtype )
                        ) *( ";" parameter )
       accept-params  = ";" "q" "=" qvalue *( accept-extension )
       accept-extension = ";" token [ "=" ( token | quoted-string ) ]
    (*)
  end;

  IHttpTagAcceptCharset = interface (IHttpTag)
    ['{AFADFE29-07DD-4D88-B7E3-CC47DF02EEEA}']
      (*)property Charset: String;
      property QValue: Word;(*)
      (*)
      Accept-Charset = "Accept-Charset" ":"
              1#( ( charset | "*" )[ ";" "q" "=" qvalue ] )
      (*)
  end;

  IHttpTagAcceptEncoding = interface (IHttpTag)
    ['{CF90AEE4-C55C-41C3-B0A8-B7530980BE76}']
      (*)property Coding: String;
      property QValue: Word;(*)
    (*)
       Accept-Encoding  = "Accept-Encoding" ":"
                          1#( codings [ ";" "q" "=" qvalue ] )
       codings          = ( content-coding | "*" )
    (*)
  end;

  IHttpTagAcceptLanguage = interface (IHttpTag)
    ['{5FB9F13D-7134-412D-A550-C35FE4E09E36}']
      (*)property Range: IHttpLaguageRangeList;

      IHttpLaguageRange = interface
        ['{0727C85B-5A7D-483B-BF44-B6BD67E9B860}']
        property Name: String;
        property QValue: Word;
      end;(*)

    (*)
       Accept-Language = "Accept-Language" ":"
                         1#( language-range [ ";" "q" "=" qvalue ] )
       language-range  = ( ( 1*8ALPHA *( "-" 1*8ALPHA ) ) | "*" )
    (*)
  end;

  IHttpTagAuthorization = interface (IHttpTag)
    ['{757A8795-0C7B-494B-8205-09B9DE1475DA}']
    function GetCredentials: String;
    property Credentials: String read GetCredentials;
    (*)
          Authorization  = "Authorization" ":" credentials
    (*)
  end;

  IHttpTagExpect = interface (IHttpTag)
    ['{92DB7515-D647-4D63-8C4D-B6244E468E05}']
    (*)property IsContinue: Boolean;
    property Extension: IParameterList;(*)
    (*)

      Expect       =  "Expect" ":" 1#expectation

      expectation  =  "100-continue" | expectation-extension
      expectation-extension =  token [ "=" ( token | quoted-string )
                               *expect-params ]
      expect-params =  ";" token [ "=" ( token | quoted-string ) ]

    (*)
  end;

  IHttpTagFrom = interface (IHttpTag)
    ['{60152DCA-8460-4DB7-93AE-7415DDA9866A}']
    function GetMailbox: String;
    property Mailbox: String read GetMailbox;
    (*)
       From   = "From" ":" mailbox
    (*)
  end;

  IHttpTagHost = interface (IHttpTag)
    ['{21C825F5-10DB-4B37-94A2-8E8939721440}']
    function GetValue: IHttpHost;
    property Value: IHttpHost read GetValue;
    (*)
       Host = "Host" ":" host [ ":" port ] ; Section 3.2.2
    (*)
  end;

  IHttpTagIfMatch = interface (IHttpTag)
    ['{E9891C32-3C54-4CAE-A1E2-A3F78E99E500}']
    (*)property Entity: IHttpEntityTag;(*)
    (*)
       If-Match = "If-Match" ":" ( "*" | 1#entity-tag )
    (*)
  end;

  IHttpTagIfModifiedSince = interface (IHttpTag)
    ['{B4DD05CC-614B-4A6E-A9B0-33D1E3CE5EE5}']
    function GetDate: IHttpDate;
    property Date: IHttpDate read GetDate;
    (*)
       If-Modified-Since = "If-Modified-Since" ":" HTTP-date
    (*)
  end;

  IHttpTagIfNoneMatch = interface (IHttpTag)
    ['{E9E863FD-8005-4DC8-BBF7-6E4BEE001E10}']
    //property Entity: list of IHttpEntityTag;
    (*)
       If-None-Match = "If-None-Match" ":" ( "*" | 1#entity-tag )
    (*)
  end;

  IHttpTagIfRange = interface (IHttpTag)
    ['{87813E7A-D8B7-482D-A1AB-FCAA756B802C}']
    function GetEntity: IHttpEntityTag;
    function GetDate: IHttpDate;
    property Entity: IHttpEntityTag read GetEntity;
    property Date: IHttpDate read GetDate;
    (*)
        If-Range = "If-Range" ":" ( entity-tag | HTTP-date )
    (*)
  end;

  IHttpTagIfUnmodifiedSince = interface (IHttpTag)
    ['{CC5DDDB2-9E1F-482C-9684-801BBB632638}']
    function GetDate: IHttpDate;
    property Date: IHttpDate read GetDate;
    (*)
      If-Unmodified-Since = "If-Unmodified-Since" ":" HTTP-date
    (*)
  end;

  IHttpTagMaxForwards = interface (IHttpTag)
    ['{39C69136-F8B9-4169-A1AE-0B9D7348672D}']
    function GetValue: Integer;
    property Value: Integer read GetValue;
    (*)
       Max-Forwards   = "Max-Forwards" ":" 1*DIGIT
    (*)
  end;

  IHttpTagProxyAuthorization = interface (IHttpTag)
    ['{67C147B1-E5CE-4A9A-9050-E0C640385657}']
    function GetCredentials: String;
    property Credentials: String read GetCredentials;
    (*)
       Proxy-Authorization     = "Proxy-Authorization" ":" credentials
    (*)
  end;

  IHttpTagRange = interface (IHttpTag)
    ['{EB9A1820-5457-411B-AD83-6CAE823C8600}']
      //property FirstBytePos: LongWord;
      //property LastBytePos: LongWord;
    (*)
       Range = "Range" ":" ranges-specifier
       ranges-specifier = byte-ranges-specifier
       byte-ranges-specifier = bytes-unit "=" byte-range-set
       byte-range-set  = 1#( byte-range-spec | suffix-byte-range-spec )
       byte-range-spec = first-byte-pos "-" [last-byte-pos]
       first-byte-pos  = 1*DIGIT
       last-byte-pos   = 1*DIGIT
       suffix-byte-range-spec = "-" suffix-length
       suffix-length = 1*DIGIT
    (*)
  end;

  IHttpTagReferer = interface (IHttpTag)
    ['{5B14F1FB-063D-4C85-9755-454C96F3E1A1}']
    (*)
       Referer        = "Referer" ":" ( absoluteURI | relativeURI )
    (*)
  end;

  IHttpTagTE = interface (IHttpTag)
    ['{3F37743A-211F-421A-BFAC-9CC7DBA13173}']                
    (*)
       TE        = "TE" ":" #( t-codings )
       t-codings = "trailers" | ( transfer-extension [ accept-params ] )
    (*)
  end;

  IHttpTagUserAgent = interface (IHttpTag)
    ['{3423B426-A44B-4438-94DB-D694AD91B7F1}']

    (*)
       User-Agent     = "User-Agent" ":" 1*( product | comment )
    (*)
  end;

  IHttpTagContentEncoding = interface (IHttpTag)
    ['{B8014AB8-AED6-4D7C-A6AD-1DACD22A6492}']
    (*)
       Content-Encoding  = "Content-Encoding" ":" 1#content-coding
    (*)
  end;

  
  
  IHttpTagContentLanguage = interface (IHttpTag)
    ['{608FA4C7-98FF-497B-9002-54984D6B3A74}']
    (*)
       Content-Language  = "Content-Language" ":" 1#language-tag
    (*)
  end;

  IHttpTagContentLength = interface (IHttpTag)
    ['{F4312C53-4014-46D6-BB61-0B1F197AE50B}']
    (*)
       Content-Length    = "Content-Length" ":" 1*DIGIT
    (*)
  end;
  
  IHttpTagContentLocation = interface (IHttpTag)
    ['{E93C5960-8D55-48A3-B320-B405B0E68F43}']
    (*)
       Content-Location = "Content-Location" ":" ( absoluteURI | relativeURI )
    (*)
  end;
  
  IHttpTagContentMD5 = interface (IHttpTag)
    ['{33D1966E-7DD8-4E6C-A893-99263CDCB3E4}']
    (*)
        Content-MD5   = "Content-MD5" ":" md5-digest
        md5-digest   = <base64 of 128 bit MD5 digest as per RFC 1864>
    (*)
  end;

  IHttpTagContentRange = interface (IHttpTag)
    ['{73164A75-6EC7-473F-A126-5F7252C9D109}']
    (*)
       Content-Range = "Content-Range" ":" content-range-spec
       content-range-spec      = byte-content-range-spec
       byte-content-range-spec = bytes-unit SP
                                 byte-range-resp-spec "/"
                                 ( instance-length | "*" )
       byte-range-resp-spec = (first-byte-pos "-" last-byte-pos)
                                      | "*"
       instance-length           = 1*DIGIT
    (*)
  end;

  IHttpTagContentType = interface (IHttpTag)
    ['{EB0C6873-98C1-4998-9767-B3B1FB530981}']
    (*)
       Content-Type   = "Content-Type" ":" media-type
       media-type     = type "/" subtype *( ";" parameter )
       type           = token
       subtype        = token
    (*)
  end;
  
  IHttpTagExpires = interface (IHttpTag)
    ['{23250142-9814-434A-9A8C-573BDFE89B78}']
    (*)
      Expires = "Expires" ":" HTTP-date
    (*)
  end;
  
  IHttpTagLastModified = interface (IHttpTag)
    ['{36361905-132A-4668-BCF8-045BFF2A0655}']
    (*)
       Last-Modified  = "Last-Modified" ":" HTTP-date
    (*)
  end;

  IHttpObjectTags = interface
    ['{85D41109-2A13-4E3A-B87B-477E3F603573}']
    function GetList: IHttpTagList;
    property List: IHttpTagList read GetList;
  end;

  IHttpGeneralTags = interface (IHttpObjectTags) 
    ['{5ECD925B-173F-4085-8BE9-935E4E2A44B8}']
    function GetCacheControl: IHttpTagCacheControl;
    function GetConnection: IHttpTagConnection;
    function GetDate: IHttpTagDate;
    function GetPragma: IHttpTagPragma;
    function GetTrailer: IHttpTagTrailer;
    function GetTransferEncoding: IHttpTagTransferEncoding;
    function GetUpgrade: IHttpTagUpgrade;
    function GetVia: IHttpTagVia;
    function GetWarning: IHttpTagWarning;
    property CacheControl: IHttpTagCacheControl read GetCacheControl;
    property Connection: IHttpTagConnection read GetConnection;
    property Date: IHttpTagDate read GetDate;
    property Pragma: IHttpTagPragma read GetPragma;
    property Trailer: IHttpTagTrailer read GetTrailer;
    property TransferEncoding: IHttpTagTransferEncoding read GetTransferEncoding;
    property Upgrade: IHttpTagUpgrade read GetUpgrade;
    property Via: IHttpTagVia read GetVia;
    property Warning: IHttpTagWarning read GetWarning;
  end;

  IHttpRequestTags = interface (IHttpObjectTags) 
    ['{FA185E78-CF85-4CF0-84B1-8E6CA2703AAA}']
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
    property Accept: IHttpTagAccept read GetAccept;
    property AcceptCharset: IHttpTagAcceptCharset read GetAcceptCharset;
    property AcceptEncoding: IHttpTagAcceptEncoding read GetAcceptEncoding;
    property AcceptLanguage: IHttpTagAcceptLanguage read GetAcceptLanguage;
    property Authorization: IHttpTagAuthorization read GetAuthorization;
    property Expect: IHttpTagExpect read GetExpect;
    property From: IHttpTagFrom read GetFrom;
    property Host: IHttpTagHost read GetHost;
    property IfMatch: IHttpTagIfMatch read GetIfMatch;
    property IfModifiedSince: IHttpTagIfModifiedSince read GetIfModifiedSince;
    property IfNoneMatch: IHttpTagIfNoneMatch read GetIfNoneMatch;
    property IfRange: IHttpTagIfRange read GetIfRange;
    property IfUnmodifiedSince: IHttpTagIfUnmodifiedSince read GetIfUnmodifiedSince;
    property MaxForwards: IHttpTagMaxForwards read GetMaxForwards;
    property ProxyAuthorization: IHttpTagProxyAuthorization read GetProxyAuthorization;
    property Range: IHttpTagRange read GetRange;
    property Referer: IHttpTagReferer read GetReferer;
    property TE: IHttpTagTE read GetTE;
    property UserAgent: IHttpTagUserAgent read GetUserAgent;
  end;

  IHttpResponseTags = interface (IHttpObjectTags) 
    ['{7670C8E8-3BF9-41BA-891E-9BA1376D567F}']
    function GetAcceptRanges: IHttpTagAcceptRanges;
    function GetAge: IHttpTagAge;
    function GetETag: IHttpTagETag;
    function GetLocation: IHttpTagLocation;
    function GetProxyAuthenticate: IHttpTagProxyAuthenticate;
    function GetRetryAfter: IHttpTagRetryAfter;
    function GetServer: IHttpTagServer;
    function GetVary: IHttpTagVary;
    function GetWWWAuthenticate: IHttpTagWWWAuthenticate;  
    property AcceptRanges: IHttpTagAcceptRanges read GetAcceptRanges;
    property Age: IHttpTagAge read GetAge;
    property ETag: IHttpTagETag read GetETag;
    property Location: IHttpTagLocation read GetLocation;
    property ProxyAuthenticate: IHttpTagProxyAuthenticate read GetProxyAuthenticate;
    property RetryAfter: IHttpTagRetryAfter read GetRetryAfter;
    property Server: IHttpTagServer read GetServer;
    property Vary: IHttpTagVary read GetVary;
    property WWWAuthenticate: IHttpTagWWWAuthenticate read GetWWWAuthenticate;
  end;

  IHttpEntityTags = interface (IHttpObjectTags) 
    ['{96E17AE6-DAD3-4506-8B92-046A323236FC}']
    function GetAllow: IHttpTagAllow;
    function GetContentEncoding: IHttpTagContentEncoding;
    function GetContentLanguage: IHttpTagContentLanguage;
    function GetContentLength: IHttpTagContentLength;
    function GetContentLocation: IHttpTagContentLocation;
    function GetContentMD5: IHttpTagContentMD5;
    function GetContentRange: IHttpTagContentRange;
    function GetContentType: IHttpTagContentType;
    function GetExpires: IHttpTagExpires;
    function GetLastModified: IHttpTagLastModified;
    property Allow: IHttpTagAllow read GetAllow;
    property ContentEncoding: IHttpTagContentEncoding read GetContentEncoding;
    property ContentLanguage: IHttpTagContentLanguage read GetContentLanguage;
    property ContentLength: IHttpTagContentLength read GetContentLength;
    property ContentLocation: IHttpTagContentLocation read GetContentLocation;
    property ContentMD5: IHttpTagContentMD5 read GetContentMD5;
    property ContentRange: IHttpTagContentRange read GetContentRange;
    property ContentType: IHttpTagContentType read GetContentType;
    property Expires: IHttpTagExpires read GetExpires;
    property LastModified: IHttpTagLastModified read GetLastModified;
  end;

implementation
end.
