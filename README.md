# SteamWebAPI
Steam Web API for Delphi

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
