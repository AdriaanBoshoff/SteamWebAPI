///////////////////////////////////////////////////////////////////////////////
//    MIT License
//
//    Copyright (c) 2018 Adriaan Boshoff
//
//    Permission is hereby granted, free of charge, to any person obtaining a copy
//    of this software and associated documentation files (the "Software"), to deal
//    in the Software without restriction, including without limitation the rights
//    to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
//    copies of the Software, and to permit persons to whom the Software is
//    furnished to do so, subject to the following conditions:
//
//    The above copyright notice and this permission notice shall be included in all
//    copies or substantial portions of the Software.
//
//    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
//    IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
//    FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
//    AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
//    LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
//    OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
//    SOFTWARE.
///////////////////////////////////////////////////////////////////////////////

unit uSteamWebAPI;

interface

uses
  System.SysUtils, IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient,
  IdHTTP;

type
  TPlayerService = class(TObject)
  private
    fUserKey: string;
  public
    constructor Create(WebAPIKey: string);
    function GetRecentlyPlayedGames(SteamID: string; Count: Integer): string;
    function GetOwnedGames(SteamID: string; Include_Appinfo, Include_Played_Free_Games: Boolean; AppIDs_Filter: string): string;
    function GetSteamLevel(SteamID: string): string;
    function GetBadges(SteamID: string): string;
    function GetCommunityBadgeProgress(SteamID, BadgeID: string): string;
    function IsPlayingSharedGame(SteamID, AppID_Playing: string): string;
  end;

implementation

{ TPlayerService }

constructor TPlayerService.Create(WebAPIKey: string);
begin
  fUserKey := WebAPIKey;
end;

function TPlayerService.GetBadges(SteamID: string): string;
var
  sdata: string;
  httpclient: TIdHTTP;
begin
  httpclient := TIdHTTP.Create(nil);
  try
    sdata := httpclient.Get
    ('http://api.steampowered.com/IPlayerService/GetBadges/v1' +
    '?key=' + fUserKey
    + '&steamid=' + SteamID
    );
  finally
    Result := sdata;
    httpclient.Free;
  end;
end;

function TPlayerService.GetCommunityBadgeProgress(SteamID,
  BadgeID: string): string;
var
  sdata: string;
  httpclient: TIdHTTP;
begin
  httpclient := TIdHTTP.Create(nil);
  try
    sdata := httpclient.Get
    ('http://api.steampowered.com/IPlayerService/GetCommunityBadgeProgress/v1' +
    '?key=' + fUserKey
    + '&steamid=' + SteamID
    + '&badgeid=' + BadgeID
    );
  finally
    Result := sdata;
    httpclient.Free;
  end;
end;

function TPlayerService.GetOwnedGames(SteamID: string; Include_Appinfo,
  Include_Played_Free_Games: Boolean; AppIDs_Filter: string): string;
var
  sdata: string;
  httpclient: TIdHTTP;
begin
  httpclient := TIdHTTP.Create(nil);
  try
    sdata := httpclient.Get
    ('http://api.steampowered.com/IPlayerService/GetOwnedGames/v1' +
    '?key=' + fUserKey
    + '&steamid=' + SteamID
    + '&include_appinfo=' + BoolToStr(Include_Appinfo)
    + '&include_played_free_games=' + BoolToStr(Include_Played_Free_Games)
    + '&appids_filter=' + AppIDs_Filter
    );
  finally
    Result := sdata;
    httpclient.Free;
  end;
end;

function TPlayerService.GetRecentlyPlayedGames(SteamID: string;
  Count: Integer): string;
var
  sdata: string;
  httpclient: TIdHTTP;
begin
  httpclient := TIdHTTP.Create(nil);
  try
    sdata := httpclient.Get
    ('http://api.steampowered.com/IPlayerService/GetRecentlyPlayedGames/v1' +
    '?key=' + fUserKey
    + '&steamid=' + SteamID
    + '&count=' + IntToStr(Count)
    );
  finally
    Result := sdata;
    httpclient.Free;
  end;
end;

function TPlayerService.GetSteamLevel(SteamID: string): string;
var
  sdata: string;
  httpclient: TIdHTTP;
begin
  httpclient := TIdHTTP.Create(nil);
  try
    sdata := httpclient.Get
    ('http://api.steampowered.com/IPlayerService/GetSteamLevel/v1' +
    '?key=' + fUserKey
    + '&steamid=' + SteamID
    );
  finally
    Result := sdata;
    httpclient.Free;
  end;
end;

function TPlayerService.IsPlayingSharedGame(SteamID,
  AppID_Playing: string): string;
var
  sdata: string;
  httpclient: TIdHTTP;
begin
  httpclient := TIdHTTP.Create(nil);
  try
    sdata := httpclient.Get
    ('http://api.steampowered.com/IPlayerService/IsPlayingSharedGame/v1' +
    '?key=' + fUserKey
    + '&steamid=' + SteamID
    + '&appid_playing=' + AppID_Playing
    );
  finally
    Result := sdata;
    httpclient.Free;
  end;
end;

end.
