# SteamWebAPI
Add the uSteamWebAPI.pas to your project and uses then enjoy. I would reccomend this as a parser https://github.com/thomaserlang/delphi-json

# Supported:

## IPlayerService Interface
Refer: https://partner.steamgames.com/doc/webapi/IPlayerService

### Example:
<b>Delphi Code</b>
```pascal
procedure TForm1.btn1Click(Sender: TObject);
var
  steamapi: TPlayerService;
begin
  mmo1.WordWrap := True;
  steamapi := TPlayerService.Create('F03F1797000F71B5391DA0EF04795C1A'); // SteamWEBAPI Key
  mmo1.Text := steamapi.GetSteamLevel('76561198113034550'); 
  steamapi.Free;
end;
```

<b>Json Response</b>
```json
{  
   "response":{  
      "player_level":34
   }
}
```

## IGameServersService Interface
Refer: https://partner.steamgames.com/doc/webapi/IGameServersService

### Example:
<b>Delphi Code</b>
```pascal
procedure TForm1.btn1Click(Sender: TObject);
var
  steamapi: TGameServersService;
begin
  mmo1.WordWrap := True;
  steamapi := TGameServersService.Create('F03F1797000F71B5391DA0EF04795C1A');
  mmo1.Text := steamapi.CreateAccount('252490','This is the memo text');
  steamapi.Free;
end;
```

## ISteamApps Interface
Refer: https://partner.steamgames.com/doc/webapi/ISteamApps

# Types:
```pascal
type
  TPlayerService = class(TObject)
  private
    fUserKey: string;
  public
    constructor Create(WebAPIKey: string);
    function GetRecentlyPlayedGames(SteamID: string; Count: Integer): string;
    function GetOwnedGames(SteamID: string;
      Include_Appinfo, Include_Played_Free_Games: Boolean;
      AppIDs_Filter: string): string;
    function GetSteamLevel(SteamID: string): string;
    function GetBadges(SteamID: string): string;
    function GetCommunityBadgeProgress(SteamID, BadgeID: string): string;
    function IsPlayingSharedGame(SteamID, AppID_Playing: string): string;
    function RecordOfflinePlaytime(SteamID, Ticket, Play_Sessions
      : string): string;
    procedure ChangeAPIKey(WebAPIKey: string);
  end;

type
  TGameServersService = class(TObject)
  private
    fUserKey: string;
  public
    constructor Create(WebAPIKey: string);
    function GetAccountList: string;
    function CreateAccount(AppID, Memo: string): string;
    function DeleteAccount(SteamID: string): string;
    function SetMemo(SteamID, Memo: string): string;
    function ResetLoginToken(SteamID: string): string;
    function GetAccountPublicInfo(SteamID: string): string;
    function QueryLoginToken(Login_Token: string): string;
    function GetServerSteamIDsByIP(Server_IPs: string): string;
    function GetServerIPsBySteamID(Server_SteamIDs: string): string;
    function SetBanStatus(SteamID, Ban_Seconds: string;
      Banned: Boolean): string;
    procedure ChangeAPIKey(WebAPIKey: string);
  end;

type
  TSteamApps = class(TObject)
  private
    fUserKey: string;
  public
    constructor Create(WebAPIKey: string);
    function GetAppBetas(AppID: string): string;
    function GetAppBuilds(AppID: string; Count: Integer): string;
    function GetAppDepotVersions(AppID: string): string;
    function GetAppList: string;
    function GetCheatingReports(AppID, Time_Begin, Time_End: string; Include_Reports, Include_Bans: Boolean; Report_ID_Min: string): string;
    function GetPlayersBanned(AppID: string): string;
    function GetServerList(Filter, Limit: string): string;
    function GetServersAtAddress(IPAddress: string): string;
    function SetAppBuildLive(AppID, BuildID, BetaKey, Description: string): string;
    function UpToDateCheck(AppID, Version: string): string;
  end;
  ```
