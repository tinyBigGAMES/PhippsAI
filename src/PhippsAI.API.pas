{===============================================================================
  ___  _     _                    _    ___ ™
 | _ \| |_  (_) _ __  _ __  ___  /_\  |_ _|
 |  _/| ' \ | || '_ \| '_ \(_-< / _ \  | |
 |_|  |_||_||_|| .__/| .__//__//_/ \_\|___|
               |_|   |_|
         Your Personal AI Butler

 Copyright © 2025-present tinyBigGAMES™ LLC
 All Rights Reserved.

 https://github.com/tinyBigGAMES/PhippsAI

 See LICENSE file for license information
===============================================================================}

unit PhippsAI.API;

{$I PhippsAI.Defines.inc}

interface

uses
  WinApi.Windows,
  System.SysUtils,
  PhippsAI.CLibs,
  PhippsAI.Core,
  PhippsAI.Utils;

function  phInit(): Boolean; cdecl; exports phInit;
procedure phQuit(); cdecl; exports phQuit;
function  phVersion(): PWideChar; cdecl; exports phVersion;

function  phGetError(): PWideChar; cdecl; exports phGetError;

function  phGetConfigFilename(): PWideChar; cdecl; exports phGetConfigFilename;
procedure phSetConfigFilename(const AFilename: PWideChar=nil); cdecl; exports phSetConfigFilename;

function  phGetModelPath(): PWideChar; cdecl; exports phGetModelPath;
procedure phSetModelPath(const APath: PWideChar); cdecl; exports phSetModelPath;

function  phGetModelFilename(): PWideChar; cdecl; exports phGetModelFilename;
procedure phSetModelFilename(const AFilename: PWideChar=nil); cdecl; exports phSetModelFilename;

function  phGetMaxContext(): UInt32; cdecl; exports phGetMaxContext;
procedure phSetMaxContext(const AValue: UInt32=1024); cdecl; exports phSetMaxContext;

function  phGetMainGPU(): Int32; cdecl; exports phGetMainGPU;
procedure phSetMainGPU(const AValue: Int32=-1); cdecl; exports phSetMainGPU;

function  phGetGPULayers(): Int32; cdecl; exports phGetGPULayers;
procedure phSetGPULayers(const AValue: Int32=-1); cdecl; exports phSetGPULayers;

function  phGetMaxThreads(): Int32; cdecl; exports phGetMaxThreads;
procedure phSetMaxThreads(const AValue: Int32=4); cdecl; exports phSetMaxThreads;

procedure phSetSearchAPIKey(const AValue: PWideChar=nil); cdecl; exports phSetSearchAPIKey;
function  phGetSearchAPIKey(): PWideChar; cdecl; exports phGetSearchAPIKey;

function  phGetShowThinking(): Boolean; cdecl; exports phGetShowThinking;
procedure phSetShowThinking(const AValue: Boolean=True); cdecl; exports phSetShowThinking;

function  phSaveConfig(): Boolean; cdecl; exports phSaveConfig;
function  phLoadConfig(): Boolean; cdecl; exports phLoadConfig;

function  phLoadModel(): Boolean; cdecl; exports phLoadModel;
procedure phUnloadModel(); cdecl; exports phUnloadModel;
function  phModelLoaded(): Boolean; cdecl; exports phModelLoaded;

procedure phClearMessages(); cdecl; exports phClearMessages;
procedure phAddUserMessage(const AContent: PWideChar); cdecl; exports phAddUserMessage;
procedure phAddAssistantMessage(const AContent: PWideChar); cdecl; exports phAddAssistantMessage;
function  phGetLastUserMessage(): PWideChar; cdecl; exports phGetLastUserMessage;

function  phRunInference(): Boolean; cdecl; exports phRunInference;
function  phGetInferenceResponse(): PWideChar; cdecl; exports phGetInferenceResponse;
procedure phGetPerformance(const ATotalInputTokens: PInt32; ATotalOutputTokens: PInt32; ATokensPerSecond: PDouble); cdecl; exports phGetPerformance;

function  phGetInferenceCancelCallback(): phInferenceCancelCallback; cdecl; exports phGetInferenceCancelCallback;
procedure phSetInferenceCancelCallback(const AHandler: phInferenceCancelCallback; const AUserData: Pointer); cdecl; exports phSetInferenceCancelCallback;

function  phGetInferenceTokenCallback(): phInferenceTokenCallback; cdecl; exports phGetInferenceTokenCallback;
procedure phSetInferenceTokenlCallback(const AHandler: phInferenceTokenCallback; const AUserData: Pointer); cdecl; exports phSetInferenceTokenlCallback;

function  phGetInfoCallback(): phInfoCallback; cdecl; exports phGetInfoCallback;
procedure phSetInfoCallback(const AHandler: phInfoCallback; const AUserData: Pointer); cdecl; exports phSetInfoCallback;

function  phGetLoadModelProgressCallback(): phLoadModelProgressCallback; cdecl; exports phGetLoadModelProgressCallback;
procedure phSetLoadModelProgressCallback(const AHandler: phLoadModelProgressCallback; const AUserData: Pointer); cdecl; exports phSetLoadModelProgressCallback;

function  phGetLoadModelCallback(): phLoadModelCallback; cdecl; exports phGetLoadModelCallback;
procedure phSetLoadModelCallback(const AHandler: phLoadModelCallback; const AUserData: Pointer); cdecl; exports phSetLoadModelCallback;

function  phGetInferenceStartCallback(): phInferenceStartCallback; cdecl; exports phGetInferenceStartCallback;
procedure phSetInferenceStartCallback(const AHandler: phInferenceStartCallback; const AUserData: Pointer); cdecl; exports phSetInferenceStartCallback;

function  phGetInferenceEndCallback(): phInferenceEndCallback; cdecl; exports phGetInferenceEndCallback;
procedure phSetInferenceEndCallback(const AHandler: phInferenceEndCallback; const AUserData: Pointer); cdecl; exports phSetInferenceEndCallback;

function  phGetThinkStartCallback(): phThinkStartCallback; cdecl; exports phGetThinkStartCallback;
procedure phSetThinkStartCallback(const AHandler: phThinkStartCallback; const AUserData: Pointer); cdecl; exports phSetThinkStartCallback;

function  phGetThinkEndCallback(): phThinkEndCallback; cdecl; exports phGetThinkEndCallback;
procedure phSetThinkEndCallback(const AHandler: phThinkEndCallback; const AUserData: Pointer); cdecl; exports phSetThinkEndCallback;

procedure phSetTokenRightMargin(const AMargin: Int32=10); cdecl; exports phSetTokenRightMargin;
procedure phSetTokenMaxLineLength(const ALength: Int32=120); cdecl; exports phSetTokenMaxLineLength;

procedure phAddRawMessage(const AContent: PWideChar; const AAddEndMessage: Boolean); cdecl; exports phAddRawMessage;


implementation

type
  { TAI }
  TAI = class(TPhippsAI)
  private
    FVersion: string;
    function GetDLLVersion(): string;
  public
    constructor Create(); override;
    destructor Destroy(); override;
    function GetVersion(): string; override;
  end;

{ TAI }
function TAI.GetDLLVersion(): string;
var
  Size, Handle: DWORD;
  Buffer: Pointer;
  FileInfo: Pointer;
  VerValue: PVSFixedFileInfo;
  VerSize: UINT;
  LFilename: string;

begin
  Result := '0.0.0'; // Default version if info not found

  LFilename := GetCurrentDLLFilename();

  // Get the size of the version info block
  Size := GetFileVersionInfoSize(PChar(LFilename), Handle);
  if Size = 0 then
    Exit;

  // Allocate buffer for version info
  GetMem(Buffer, Size);
  try
    // Retrieve version info
    if GetFileVersionInfo(PChar(LFilename), Handle, Size, Buffer) then
    begin
      // Query version info
      if VerQueryValue(Buffer, '\', FileInfo, VerSize) and (VerSize >= SizeOf(TVSFixedFileInfo)) then
      begin
        VerValue := PVSFixedFileInfo(FileInfo);
        Result := Format('%d.%d.%d',
          [HiWord(VerValue.dwFileVersionMS), LoWord(VerValue.dwFileVersionMS),
           HiWord(VerValue.dwFileVersionLS)]);
      end;
    end;
  finally
    FreeMem(Buffer);
  end;
end;

constructor TAI.Create();
begin
  inherited;
  FVersion := GetDLLVersion();
end;

destructor TAI.Destroy();
begin
  inherited;
end;

function TAI.GetVersion(): string;
begin
  Result := FVersion;
end;

var
  LAI: TAI = nil;

function  phInit(): Boolean;
begin
  Result := False;
  if Assigned(LAI) then Exit;

  LAI := TAI.Create();

  Result := True;
end;

procedure phQuit();
begin
  if Assigned(LAI) then
  begin
    LAI.Free();
    LAI := nil;
  end;
end;

function  phVersion(): PWideChar; cdecl;
begin
  Result := nil;

  if not Assigned(LAI) then Exit;

  Result := PWideChar(LAI.GetVersion());
end;

function  phGetError(): PWideChar;
begin
  Result := nil;
  if not Assigned(LAI) then Exit;

  Result := PWideChar(LAI.GetError());
end;

function  phGetConfigFilename(): PWideChar;
begin
  Result := nil;
  if not Assigned(LAI) then Exit;

  Result := PWideChar(LAI.GetConfigFilename());

end;

procedure phSetConfigFilename(const AFilename: PWideChar=nil);
begin
  if not Assigned(LAI) then Exit;

  LAI.SetConfigFilename(string(AFilename));
end;

function  phGetModelPath(): PWideChar;
begin
  Result := nil;
  if not Assigned(LAI) then Exit;

  Result := PWideChar(LAI.GetModelPath());
end;

procedure phSetModelPath(const APath: PWideChar);
begin
  if not Assigned(LAI) then Exit;

  LAI.SetModelPath(string(APath));
end;

function  phGetModelFilename(): PWideChar;
begin
  Result := nil;
  if not Assigned(LAI) then Exit;

  Result := PWideChar(LAI.GetModelFilename());
end;

procedure phSetModelFilename(const AFilename: PWideChar=nil);
begin
  if not Assigned(LAI) then Exit;

  LAI.SetModelFilename(string(AFilename));
end;

function  phGetMaxContext(): UInt32;
begin
  Result := 0;
  if not Assigned(LAI) then Exit;

  Result := LAI.GetMaxContext();
end;

procedure phSetMaxContext(const AValue: UInt32);
begin
  if not Assigned(LAI) then Exit;

  LAI.SetMaxContext(AValue);
end;

function  phGetMainGPU(): Int32;
begin
  Result := -1;
  if not Assigned(LAI) then Exit;

  Result := LAI.GetMainGPU();
end;

procedure phSetMainGPU(const AValue: Int32);
begin
  if not Assigned(LAI) then Exit;

  LAI.SetMainGPU(AValue);
end;

function  phGetGPULayers(): Int32;
begin
  Result := -1;
  if not Assigned(LAI) then Exit;

  Result := LAI.GetGPULayers();
end;

procedure phSetGPULayers(const AValue: Int32);
begin
  if not Assigned(LAI) then Exit;

  LAI.SetGPULayers(AValue);
end;

function  phGetMaxThreads(): Int32;
begin
  Result := 0;
  if not Assigned(LAI) then Exit;

  Result := LAI.GetMaxThreads();
end;

procedure phSetMaxThreads(const AValue: Int32);
begin
  if not Assigned(LAI) then Exit;

  LAI.SetMaxThreads(AValue);
end;

procedure phSetSearchAPIKey(const AValue: PWideChar=nil);
begin
  if not Assigned(LAI) then Exit;

  LAI.SetSearchAPIKey(string(AValue));
end;

function  phGetSearchAPIKey(): PWideChar;
begin
  Result := nil;
  if not Assigned(LAI) then Exit;

  Result := PWideChar(LAI.GetSearchAPIKey());
end;

function  phGetShowThinking(): Boolean;
begin
  Result := False;
  if not Assigned(LAI) then Exit;

  Result := LAI.GetShowThinking();
end;

procedure phSetShowThinking(const AValue: Boolean=True);
begin
  if not Assigned(LAI) then Exit;

  LAI.SetShowThinking(AValue);
end;

function  phSaveConfig(): Boolean;
begin
  Result := False;
  if not Assigned(LAI) then Exit;

  Result := LAI.SaveConfig();
end;

function  phLoadConfig(): Boolean;
begin
  Result := False;
  if not Assigned(LAI) then Exit;

  Result := LAI.LoadConfig();
end;

function  phLoadModel(): Boolean;
begin
  Result := False;
  if not Assigned(LAI) then Exit;

  Result := LAI.LoadModel();
end;

procedure phUnloadModel();
begin
  if not Assigned(LAI) then Exit;

  LAI.UnloadModel();
end;

function  phModelLoaded(): Boolean;
begin
  Result := False;
  if not Assigned(LAI) then Exit;

  Result := LAI.ModelLoaded();
end;

procedure phClearMessages();
begin
  if not Assigned(LAI) then Exit;

  LAI.ClearMessages();
end;

procedure phAddUserMessage(const AContent: PWideChar);
begin
  if not Assigned(LAI) then Exit;

  LAI.AddUserMessage(string(AContent));
end;

procedure phAddAssistantMessage(const AContent: PWideChar);
begin
  if not Assigned(LAI) then Exit;

  LAI.AddAssistantMessage(string(AContent));
end;

function  phGetLastUserMessage(): PWideChar;
begin
  Result := nil;
  if not Assigned(LAI) then Exit;

  Result := PWideChar(LAI.GetLastUserMessage());
end;

function  phRunInference(): Boolean;
begin
  Result := False;
  if not Assigned(LAI) then Exit;

  Result := LAI.RunInference();
end;

function  phGetInferenceResponse(): PWideChar;
begin
  Result := nil;
  if not Assigned(LAI) then Exit;

  Result := PWideChar(LAI.GetInferenceResponse());
end;

procedure phGetPerformance(const ATotalInputTokens: PInt32; ATotalOutputTokens: PInt32; ATokensPerSecond: PDouble);
begin
  if not Assigned(LAI) then Exit;

  LAI.GetPerformance(ATotalInputTokens, ATotalOutputTokens, ATokensPerSecond);
end;

function  phGetInferenceCancelCallback(): phInferenceCancelCallback;
begin
  Result := nil;
  if not Assigned(LAI) then Exit;

  Result := LAI.GetInferenceCancelCallback();
end;

procedure phSetInferenceCancelCallback(const AHandler: phInferenceCancelCallback; const AUserData: Pointer);
begin
  if not Assigned(LAI) then Exit;

  LAI.SetInferenceCancelCallback(Ahandler, AUserData);
end;

function  phGetInferenceTokenCallback(): phInferenceTokenCallback;
begin
  Result := nil;
  if not Assigned(LAI) then Exit;

  Result := LAI.GetInferenceTokenCallback();
end;

procedure phSetInferenceTokenlCallback(const AHandler: phInferenceTokenCallback; const AUserData: Pointer);
begin
  if not Assigned(LAI) then Exit;

  LAI.SetInferenceTokenlCallback(AHandler, AUserData);
end;

function  phGetInfoCallback(): phInfoCallback;
begin
  Result := nil;
  if not Assigned(LAI) then Exit;

  Result := LAI.GetInfoCallback();
end;

procedure phSetInfoCallback(const AHandler: phInfoCallback; const AUserData: Pointer);
begin
  if not Assigned(LAI) then Exit;

  LAI.SetInfoCallback(AHandler, AUserData);
end;

function  phGetLoadModelProgressCallback(): phLoadModelProgressCallback;
begin
  Result := nil;
  if not Assigned(LAI) then Exit;

  Result := LAI.GetLoadModelProgressCallback();
end;

procedure phSetLoadModelProgressCallback(const AHandler: phLoadModelProgressCallback; const AUserData: Pointer);
begin
  if not Assigned(LAI) then Exit;

  LAI.SetLoadModelProgressCallback(AHandler, AUserData);
end;

function  phGetLoadModelCallback(): phLoadModelCallback;
begin
  Result := nil;
  if not Assigned(LAI) then Exit;

  Result := LAI.GetLoadModelCallback();
end;

procedure phSetLoadModelCallback(const AHandler: phLoadModelCallback; const AUserData: Pointer);
begin
  if not Assigned(LAI) then Exit;

  LAI.SetLoadModelCallback(AHandler, AUserData);
end;

function  phGetInferenceStartCallback(): phInferenceStartCallback;
begin
  Result := nil;
  if not Assigned(LAI) then Exit;

  Result := LAI.GetInferenceStartCallback();
end;

procedure phSetInferenceStartCallback(const AHandler: phInferenceStartCallback; const AUserData: Pointer);
begin
  if not Assigned(LAI) then Exit;

  LAI.SetInferenceStartCallback(AHandler, AUserData);
end;

function  phGetInferenceEndCallback(): phInferenceEndCallback;
begin
  Result := nil;
  if not Assigned(LAI) then Exit;

  Result := LAI.GetInferenceEndCallback();
end;

procedure phSetInferenceEndCallback(const AHandler: phInferenceEndCallback; const AUserData: Pointer);
begin
  if not Assigned(LAI) then Exit;

  LAI.SetInferenceEndCallback(AHandler, AUserData);
end;

function  phGetThinkStartCallback(): phThinkStartCallback;
begin
  Result := nil;
  if not Assigned(LAI) then Exit;

  Result := LAI.GetThinkStartCallback();
end;

procedure phSetThinkStartCallback(const AHandler: phThinkStartCallback; const AUserData: Pointer);
begin
  if not Assigned(LAI) then Exit;

  LAI.SetThinkStartCallback(AHandler, AUserData);
end;

function  phGetThinkEndCallback(): phThinkEndCallback;
begin
  Result := nil;
  if not Assigned(LAI) then Exit;

  Result := LAI.GetThinkEndCallback();
end;

procedure phSetThinkEndCallback(const AHandler: phThinkEndCallback; const AUserData: Pointer);
begin
  if not Assigned(LAI) then Exit;

  LAI.SetThinkEndCallback(AHandler, AUserData);
end;

procedure phSetTokenRightMargin(const AMargin: Int32);
begin
  if not Assigned(LAI) then Exit;

  LAI.SetTokenRightMargin(AMargin);
end;

procedure phSetTokenMaxLineLength(const ALength: Int32);
begin
  if not Assigned(LAI) then Exit;

  LAI.SetTokenMaxLineLength(ALength)
end;

procedure phAddRawMessage(const AContent: PWideChar; const AAddEndMessage: Boolean);
begin
  if not Assigned(LAI) then Exit;

  LAI.AddRawMessage(string(AContent), AAddEndMessage);
end;


end.
