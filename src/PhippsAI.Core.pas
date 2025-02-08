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

unit PhippsAI.Core;

{$I PhippsAI.Defines.inc}

interface

uses
  WinApi.Windows,
  System.Generics.Collections,
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.Math,
  System.JSON,
  System.IniFiles,
  System.RegularExpressions,
  MemoryDLL,
  PhippsAI.CLibs,
  PhippsAI.Utils;

type

  phInferenceCancelCallback = function(const AUserData: Pointer): Boolean; cdecl;
  phInferenceTokenCallback = procedure(const AToken: PWideChar; const AUserData: Pointer); cdecl;
  phInfoCallback = procedure(const ALevel: Integer; const AText: PWideChar; const AUserData: Pointer); cdecl;
  phLoadModelProgressCallback = function(const AModelName: PWideChar; const AProgress: Single; const AUserData: Pointer): Boolean; cdecl;
  phLoadModelCallback = procedure(const AModelName: PWideChar; const ASuccess: Boolean; const AUserData: Pointer); cdecl;
  phInferenceStartCallback = procedure(const AUserData: Pointer); cdecl;
  phInferenceEndCallback = procedure(const AUserData: Pointer); cdecl;
  phThinkStartCallback = procedure(const AUserData: Pointer); cdecl;
  phThinkEndCallback = procedure(const AUserData: Pointer); cdecl;

  { TPhippsAI }
  TPhippsAI = class(TBaseObject)
  protected const
    ENVVAR_APIKEY = 'TAVILY_API_KEY';
  protected type
    PConfig = ^TConfig;
    TConfig = record
      ModelPath: string;
      ModelFilename: string;
      ConfigFilename: string;
      MaxContext: UInt32;
      MainGPU: Int32;
      GPULayers: Int32;
      MaxThreads: Int32;
      TokenRightMargin: Int32;
      TokenMaxLineLength: Int32;
      SearchAPIKey: string;
      ShowThinking: Boolean;
    end;
    PPerformanceResult = ^TPerformanceResult;
    TPerformanceResult = record
      TokensPerSecond: Double;
      TotalInputTokens: Int32;
      TotalOutputTokens: Int32;
    end;
    TInference = record
      Active: Boolean;
      Prompt: string;
      Response: string;
      Perf: TPerformanceResult;
      ModelFilename: string;
      Model: Pllama_model;
      Ctx: Pllama_context;
      Thinking: Boolean;
    end;
    TCallbacks = record
      InferenceCancel: TCallback<phInferenceCancelCallback>;
      InferenceToken: TCallback<phInferenceTokenCallback>;
      Info: TCallback<phInfoCallback>;
      LoadModelProgress: TCallback<phLoadModelProgressCallback>;
      LoadModel: TCallback<phLoadModelCallback>;
      InferenceStart: TCallback<phInferenceStartCallback>;
      InferenceEnd: TCallback<phInferenceEndCallback>;
      ThinkStart: TCallback<phThinkStartCallback>;
      ThinkEnd: TCallback<phThinkEndCallback>;
    end;

  private
    FError: string;
    FConfig: TConfig;
    FMessages: TStringList;
    FAddEndMessage: Boolean;
    FLastUserMessage: string;
    FInference: TInference;
    FTokenResponse: TTokenResponse;
    FCallbacks: TCallbacks;
    FTokenRightMargin: Integer;
    FTokenMaxLineLength: Integer;
    function  TokenToPiece(const AVocab: Pllama_vocab; const AContext: Pllama_context; const AToken: llama_token; const ASpecial: Boolean): string;
    function  CalcPerformance(const AContext: Pllama_context): TPerformanceResult;
    procedure AddEndMessage();
    procedure AddMessage(const ARole, AContent: string);
    function  DoRunInference(const ARawPrompt: string=''): Boolean;
    procedure DoOnNextToken(const AToken: string);
    function  LookupSearchApiKey(): string;
  public
    constructor Create(); override;
    destructor Destroy(); override;

    function GetVersion(): string; virtual;

    procedure SetError(const AText: string; const AArgs: array of const);
    function  GetError(): string;

    function  GetConfigFilename(): string;
    procedure SetConfigFilename(const AFilename: string='config');

    function  GetModelPath(): string;
    procedure SetModelPath(const APath: string='C:/LLM/GGUF');

    function  GetModelFilename(): string;
    procedure SetModelFilename(const AFilename: string='PhippsAI.gguf');

    function  GetMaxContext(): UInt32;
    procedure SetMaxContext(const AValue: UInt32=1024);

    function  GetMainGPU(): Int32;
    procedure SetMainGPU(const AValue: Int32=-1);

    function  GetGPULayers(): Int32;
    procedure SetGPULayers(const AValue: Int32=-1);

    function  GetMaxThreads(): Int32;
    procedure SetMaxThreads(const AValue: Int32=4);

    procedure SetSearchAPIKey(const AValue: string='');
    function  GetSearchAPIKey(): string;

    function  GetInferenceCancelCallback(): phInferenceCancelCallback;
    procedure SetInferenceCancelCallback(const AHandler: phInferenceCancelCallback; const AUserData: Pointer);

    function  GetInferenceTokenCallback(): phInferenceTokenCallback;
    procedure SetInferenceTokenlCallback(const AHandler: phInferenceTokenCallback; const AUserData: Pointer);

    function  GetInfoCallback(): phInfoCallback;
    procedure SetInfoCallback(const AHandler: phInfoCallback; const AUserData: Pointer);

    function  GetLoadModelProgressCallback(): phLoadModelProgressCallback;
    procedure SetLoadModelProgressCallback(const AHandler: phLoadModelProgressCallback; const AUserData: Pointer);

    function  GetLoadModelCallback(): phLoadModelCallback;
    procedure SetLoadModelCallback(const AHandler: phLoadModelCallback; const AUserData: Pointer);

    function  GetInferenceStartCallback(): phInferenceStartCallback;
    procedure SetInferenceStartCallback(const AHandler: phInferenceStartCallback; const AUserData: Pointer);

    function  GetInferenceEndCallback(): phInferenceEndCallback;
    procedure SetInferenceEndCallback(const AHandler: phInferenceEndCallback; const AUserData: Pointer);

    function  GetThinkStartCallback(): phThinkStartCallback;
    procedure SetThinkStartCallback(const AHandler: phThinkStartCallback; const AUserData: Pointer);

    function  GetThinkEndCallback(): phThinkEndCallback;
    procedure SetThinkEndCallback(const AHandler: phThinkEndCallback; const AUserData: Pointer);

    procedure SetTokenRightMargin(const AMargin: Int32=10);
    procedure SetTokenMaxLineLength(const ALength: Int32=120);

    function  GetShowThinking(): Boolean;
    procedure SetShowThinking(const AValue: Boolean=True);

    function  SaveConfig(): Boolean;
    function  LoadConfig(): Boolean;

    function  LoadModel(): Boolean;
    procedure UnloadModel();
    function  ModelLoaded(): Boolean;

    procedure ClearMessages();
    procedure AddUserMessage(const AContent: string);
    procedure AddAssistantMessage(const AContent: string);
    procedure AddRawMessage(const AContent: string; const AAddEndMessage: Boolean);
    function  GetLastUserMessage(): string;

    function  RunInference(): Boolean;
    function  GetInferenceResponse(): string;
    procedure GetPerformance(const ATotalInputTokens: PInt32; ATotalOutputTokens: PInt32; ATokensPerSecond: PDouble);

    function  OnCancelInference(): Boolean; virtual;
    procedure OnNextToken(const AToken: string); virtual;
    procedure OnInfo(const ALevel: Integer; const AText: string); virtual;
    function  OnLoadModelProgress(const AModelFilename: string; const AProgress: Single): Boolean; virtual;
    procedure OnLoadModel(const AModelFilename: string; const ASuccess: Boolean); virtual;
    procedure OnInferenceStart(); virtual;
    procedure OnInferenceEnd(); virtual;
    procedure OnThinkStart(); virtual;
    procedure OnThinkEnd(); virtual;
  end;

implementation

const
CTools =
'''
You are an advanced reasoning assistant tasked with solving complex dilemmas in a manner that mirrors human thought processes, including introspection, chain-of-thought reasoning, and moral deliberation. To approach this task effectively:
You are an expert in answering questions and invoking functions only when necessary.

Decision Rules:
1. If the question is about historical facts, general knowledge, or well-established scientific principles, you MUST answer directly without using any functions. DO NOT call `tool_websearch` for these types of questions.
2. If the question requires real-time, updated, or external information (e.g., "current net worth," "latest news," "today’s weather"), you MUST invoke `tool_websearch`. DO NOT attempt to answer with outdated information.
3. If a function cannot be used to answer the question, explicitly state that you cannot answer rather than using a tool unnecessarily.
4. If the question is missing parameters required by the function, point it out.

Function Invocation Rules:
- When you call a function, you MUST return it in JSON format using `"function_call"`.
- DO NOT include any additional text, disclaimers, or explanations.
- The correct format for a function call is:
{
  "role": "assistant",
  "content": null,
  "function_call": {
    "name": "function_name",
    "arguments": "{\"param1\": \"value1\", \"param2\": \"value2\"}"
  }
}

Function Response Handling:
- When a function response is received, integrate the information naturally into the answer.
- DO NOT return the function response as-is; rephrase it conversationally.
- DO NOT append or repeat text from the function call in the final response.
- If the function response contains numbers or structured data, format it cleanly without redundant tokens.
- Ensure the final response is human-readable and free of artifacts like stray brackets, repeated characters, or function call remnants.

Special Rules for Historical and Scientific Knowledge:
- For historical topics (e.g., "How did the ancient Chinese make saltpeter?"), answer directly.
- For general scientific principles (e.g., "How does gravity work?"), answer directly.
- For modern, evolving topics (e.g., "What are the latest AI advancements?"), invoke `tool_websearch`.

Here is a list of functions in JSON format that you can invoke:
[
  {
      "name": "tool_websearch",
      "description": "Search the web for information based on the given query. Use ONLY when: 1) The information needed is likely to be current/recent, 2) The topic is beyond the assistant's knowledge base, 3) Real-time or changing data is needed, 4) Verification of uncertain information is required. DO NOT USE for: basic facts, historical information, or general knowledge the assistant already has.",
      "parameters": {
          "type": "object",
          "required": [
              "query"
          ],
          "properties": {
              "query": {
                  "type": "string",
                  "description": "The search query used to retrieve relevant information from the web."
              }
          }
      }
  }
]

''';

function TPhippsAI.LookupSearchApiKey(): string;
begin
  Result := GetEnvVarValue(ENVVAR_APIKEY);
end;

constructor TPhippsAI.Create();
begin
  inherited;
  FMessages := TStringList.Create();
  SetError('', []);
  SetConfigFilename();
  SetModelPath();
  SetModelFilename();
  SetMaxContext();
  SetMainGPU();
  SetGPULayers();
  SetMaxThreads();
  SetSearchAPIKey();
  SetShowThinking();
  SetTokenRightMargin();
  SetTokenMaxLineLength();
  LoadConfig();
  ClearMessages();
end;

destructor TPhippsAI.Destroy();
begin
  SaveConfig();
  UnloadModel();
  SaveConfig();
  FMessages.Free();
  inherited;
end;

function TPhippsAI.GetVersion(): string;
begin
  Result := '0.1.0';
end;

procedure TPhippsAI.SetError(const AText: string; const AArgs: array of const);
begin
  FError := Format(AText, AArgs);
end;

function  TPhippsAI.GetError(): string;
begin
  Result := FError;
end;

function  TPhippsAI.GetConfigFilename(): string;
begin
  Result := FConfig.ConfigFilename;
end;

procedure TPhippsAI.SetConfigFilename(const AFilename: string);
begin
  FConfig.ConfigFilename := TPath.ChangeExtension(AFilename, 'ini');
end;

function  TPhippsAI.GetModelPath(): string;
begin
  Result := FConfig.ModelPath;
end;

procedure TPhippsAI.SetModelPath(const APath: string);
begin
  FConfig.ModelPath := APath;
end;

function  TPhippsAI.GetModelFilename(): string;
begin
  Result := FConfig.ModelFilename;
end;

procedure TPhippsAI.SetModelFilename(const AFilename: string);
begin
  FConfig.ModelFilename := AFilename;
end;

function  TPhippsAI.GetMaxContext(): UInt32;
begin
  Result := FConfig.MaxContext;
end;

procedure TPhippsAI.SetMaxContext(const AValue: UInt32);
begin
  FConfig.MaxContext := AValue;
end;

function  TPhippsAI.GetMainGPU(): Int32;
begin
  Result := FConfig.MainGPU;
end;

procedure TPhippsAI.SetMainGPU(const AValue: Int32);
begin
  FConfig.MainGPU := AValue;
end;

function  TPhippsAI.GetGPULayers(): Int32;
begin
  Result := FConfig.GPULayers;
end;

procedure TPhippsAI.SetGPULayers(const AValue: Int32);
begin
  FConfig.GPULayers := AValue;
end;

function  TPhippsAI.GetMaxThreads(): Int32;
begin
  Result := FConfig.MaxThreads;
end;

procedure TPhippsAI.SetMaxThreads(const AValue: Int32);
begin
  FConfig.MaxThreads := AValue;
end;

procedure TPhippsAI.SetSearchAPIKey(const AValue: string);
begin
  if AValue.IsEmpty then
    FConfig.SearchAPIKey := LookupSearchApiKey()
  else
    FConfig.SearchAPIKey := AValue;
end;

function  TPhippsAI.GetSearchAPIKey(): string;
begin
  Result := FConfig.SearchAPIKey;
end;

function  TPhippsAI.GetInferenceCancelCallback(): phInferenceCancelCallback;
begin
  Result := FCallbacks.InferenceCancel.Handler;
end;

procedure TPhippsAI.SetInferenceCancelCallback(const AHandler: phInferenceCancelCallback; const AUserData: Pointer);
begin
  FCallbacks.InferenceCancel.Handler := AHandler;
  FCallbacks.InferenceCancel.UserData := AUserData;
end;

function  TPhippsAI.GetInferenceTokenCallback(): phInferenceTokenCallback;
begin
  Result := FCallbacks.InferenceToken.Handler;
end;

procedure TPhippsAI.SetInferenceTokenlCallback(const AHandler: phInferenceTokenCallback; const AUserData: Pointer);
begin
  FCallbacks.InferenceToken.Handler := AHandler;
  FCallbacks.InferenceToken.UserData := AUserData;
end;

function  TPhippsAI.GetInfoCallback(): phInfoCallback;
begin
  Result := FCallbacks.Info.Handler;
end;

procedure TPhippsAI.SetInfoCallback(const AHandler: phInfoCallback; const AUserData: Pointer);
begin
  FCallbacks.Info.Handler := AHandler;
  FCallbacks.Info.UserData := AUserData;
end;

function  TPhippsAI.GetLoadModelProgressCallback(): phLoadModelProgressCallback;
begin
  Result := FCallbacks.LoadModelProgress.Handler;
end;

procedure TPhippsAI.SetLoadModelProgressCallback(const AHandler: phLoadModelProgressCallback; const AUserData: Pointer);
begin
  FCallbacks.LoadModelProgress.Handler := AHandler;
  FCallbacks.LoadModelProgress.UserData := AUserData;
end;

function  TPhippsAI.GetLoadModelCallback(): phLoadModelCallback;
begin
  Result := FCallbacks.LoadModel.Handler;
end;

procedure TPhippsAI.SetLoadModelCallback(const AHandler: phLoadModelCallback; const AUserData: Pointer);
begin
  FCallbacks.LoadModel.Handler := AHandler;
  FCallbacks.LoadModel.UserData := AUserData;
end;

function  TPhippsAI.GetInferenceStartCallback(): phInferenceStartCallback;
begin
  Result := FCallbacks.InferenceStart.Handler;
end;

procedure TPhippsAI.SetInferenceStartCallback(const AHandler: phInferenceStartCallback; const AUserData: Pointer);
begin
  FCallbacks.InferenceStart.Handler := AHandler;
  FCallbacks.InferenceStart.UserData := AUserData;
end;

function  TPhippsAI.GetInferenceEndCallback(): phInferenceEndCallback;
begin
  Result := FCallbacks.InferenceEnd.Handler;
end;

procedure TPhippsAI.SetInferenceEndCallback(const AHandler: phInferenceEndCallback; const AUserData: Pointer);
begin
  FCallbacks.InferenceEnd.Handler := AHandler;
  FCallbacks.InferenceEnd.UserData := AUserData;
end;

function  TPhippsAI.GetThinkStartCallback(): phThinkStartCallback;
begin
  Result := FCallbacks.ThinkStart.Handler;
end;

procedure TPhippsAI.SetThinkStartCallback(const AHandler: phThinkStartCallback; const AUserData: Pointer);
begin
  FCallbacks.ThinkStart.Handler := AHandler;
  FCallbacks.ThinkStart.UserData := AUserData;
end;

function  TPhippsAI.GetThinkEndCallback(): phThinkEndCallback;
begin
  Result := FCallbacks.ThinkEnd.Handler;
end;

procedure TPhippsAI.SetThinkEndCallback(const AHandler: phThinkEndCallback; const AUserData: Pointer);
begin
  FCallbacks.ThinkEnd.Handler := AHandler;
  FCallbacks.ThinkEnd.UserData := AUserData;
end;

procedure TPhippsAI.SetTokenRightMargin(const AMargin: Int32);
begin
  FTokenRightMargin := EnsureRange(AMargin, 0, MaxInt);
  FTokenResponse.SetRightMargin(AMargin);
end;

procedure TPhippsAI.SetTokenMaxLineLength(const ALength: Int32);
begin
  FTokenMaxLineLength := EnsureRange(ALength, 0, MaxInt);
  FTokenResponse.SetMaxLineLength(ALength);
end;

function  TPhippsAI.GetShowThinking(): Boolean;
begin
  Result := FConfig.ShowThinking;
end;

procedure TPhippsAI.SetShowThinking(const AValue: Boolean);
begin
  FConfig.ShowThinking := AValue;
end;

function  TPhippsAI.SaveConfig(): Boolean;
var
  LIniFile: TIniFile;
  LFilename: string;
begin
  Result := False;
  try
    LFilename := TPath.GetFullPath(FConfig.ConfigFilename);
    LIniFile := TIniFile.Create(LFilename);
    try
      LIniFile.WriteString('General', 'ModelPath', FConfig.ModelPath);
      LIniFile.WriteString('General', 'ModelFilename', FConfig.ModelFilename);
      LIniFile.WriteString('General', 'ConfigFilename', FConfig.ConfigFilename);
      LIniFile.WriteInteger('General', 'MaxContext', FConfig.MaxContext);
      LIniFile.WriteInteger('General', 'MainGPU', FConfig.MainGPU);
      LIniFile.WriteInteger('General', 'GPULayers', FConfig.GPULayers);
      LIniFile.WriteInteger('General', 'MaxThreads', FConfig.MaxThreads);
      LIniFile.WriteInteger('General', 'TokenRightMargin', FConfig.TokenRightMargin);
      LIniFile.WriteInteger('General', 'TokenMaxLineLength', FConfig.TokenMaxLineLength);
      LIniFile.WriteString('General', 'SearchAPIKey', FConfig.SearchAPIKey);
      LIniFile.WriteBool('General', 'ShowThinking', FConfig.ShowThinking);
      LIniFile.WriteInteger('Config', 'FTokenRightMargin', FTokenRightMargin);
      LIniFile.WriteInteger('Config', 'FTokenMaxLineLength', FTokenMaxLineLength);

      Result := True;
    finally
      LIniFile.Free;
    end;
  except
    on E: Exception do
    begin
      SetError('Error saving configuration: %s', [E.Message]);
    end;
  end;
end;

function  TPhippsAI.LoadConfig(): Boolean;
var
  LIniFile: TIniFile;
  LFilename: string;
begin
  Result := False;
  try
    LFilename := TPath.GetFullPath(FConfig.ConfigFilename);
    if not FileExists(LFilename) then Exit;

    LIniFile := TIniFile.Create(LFilename);
    try
      FConfig.ModelPath := LIniFile.ReadString('General', 'ModelPath', '');
      FConfig.ModelFilename := LIniFile.ReadString('General', 'ModelFilename', '');
      FConfig.ConfigFilename := LIniFile.ReadString('General', 'ConfigFilename', '');
      FConfig.MaxContext := LIniFile.ReadInteger('General', 'MaxContext', 0);
      FConfig.MainGPU := LIniFile.ReadInteger('General', 'MainGPU', -1);
      FConfig.GPULayers := LIniFile.ReadInteger('General', 'GPULayers', -1);
      FConfig.MaxThreads := LIniFile.ReadInteger('General', 'MaxThreads', 4);
      FConfig.TokenRightMargin := LIniFile.ReadInteger('General', 'TokenRightMargin', 10);
      FConfig.TokenMaxLineLength := LIniFile.ReadInteger('General', 'TokenMaxLineLength', 120);
      FConfig.SearchAPIKey := LIniFile.ReadString('General', 'SearchAPIKey', '');
      FConfig.ShowThinking := LIniFile.ReadBool('General', 'ShowThinking', True);
      FTokenRightMargin := LIniFile.ReadInteger('Config', 'FTokenRightMargin', 10);
      FTokenMaxLineLength := LIniFile.ReadInteger('Config', 'FTokenMaxLineLength', 120);
      Result := True;
    finally
      LIniFile.Free;
    end;
  except
    on E: Exception do
    begin
      SetError('Error loading configuration: %s', [E.Message]);
    end;
  end;
end;

procedure TPhippsAI_CErrCallback(const AText: PUTF8Char; AUserData: Pointer); cdecl;
begin
  if Assigned(AUserData) then
    TPhippsAI(AUserData).OnInfo(GGML_LOG_LEVEL_ERROR, Utf8ToString(AText));
end;

procedure TPhippsAI_LogCallback(ALevel: ggml_log_level; const AText: PUTF8Char; AUserData: Pointer); cdecl;
begin
  if Assigned(AUserData) then
    TPhippsAI(AUserData).OnInfo(ALevel, Utf8ToString(AText));
end;

function TPhippsAI_ProgressCallback(AProgress: single; AUserData: pointer): Boolean; cdecl;
var
  LPhippsAI: TPhippsAI;
begin
  LPhippsAI := AUserData;
  if Assigned(LPhippsAI) then
    Result := LPhippsAI.OnLoadModelProgress(LPhippsAI.FInference.ModelFilename, AProgress)
  else
    Result := True;
end;

function  TPhippsAI.LoadModel(): Boolean;
var
  LModelParams: llama_model_params;
begin
  Result := False;

  if ModelLoaded() then Exit(True);

  FInference := Default(TInference);

  redirect_cerr_to_callback(TPhippsAI_CErrCallback, nil);
  llama_log_set(TPhippsAI_LogCallback, Self);

  LModelParams := llama_model_default_params();

  LModelParams.progress_callback := TPhippsAI_ProgressCallback;
  LModelParams.progress_callback_user_data := Self;
  LModelParams.main_gpu := FConfig.MainGPU;

  if FConfig.GPULayers < 0 then
    LModelParams.n_gpu_layers := MaxInt
  else
    LModelParams.n_gpu_layers := FConfig.GPULayers;

  FInference.ModelFilename := TPath.Combine(FConfig.ModelPath, FConfig.ModelFilename);
  FInference.ModelFilename := FInference.ModelFilename.Replace('\', '/');

  FInference.Model :=  llama_load_model_from_file(AsUtf8(FInference.ModelFilename), LModelParams);
  if not Assigned(FInference.Model) then
  begin
    OnLoadModel(FInference.ModelFilename, False);
    SetError('Failed to load model: "%s"', [FInference.ModelFilename]);
    Exit;
  end;
  OnLoadModel(FInference.ModelFilename, True);

  Result := True;
end;

function  TPhippsAI.ModelLoaded(): Boolean;
begin
  Result := False;
  if not Assigned(FInference.Model) then Exit;

  Result := True;
end;

procedure TPhippsAI.ClearMessages();
begin
  FMessages.Clear();
  FAddEndMessage := False;
  FLastUserMessage := '';
  AddUserMessage(Format('The Current Data: %s', [FormatDateTime('mmmm, dd, yyyy', Now)]));
  AddUserMessage(Format('The Current Time: %s', [FormatDateTime('mmmm, dd, yyyy - hh:nn:ss AM/PM', Now)]));
  AddUserMessage(CTools);
end;

procedure TPhippsAI.AddUserMessage(const AContent: string);
begin
  if AContent.IsEmpty then Exit;

  AddMessage('User', AContent);
  FAddEndMessage := True;
  FLastUserMessage := AContent;
end;

procedure TPhippsAI.AddAssistantMessage(const AContent: string);
begin
  if AContent.IsEmpty then Exit;

  AddMessage('Assistant', AContent);
  FAddEndMessage := False;
end;

procedure TPhippsAI.AddRawMessage(const AContent: string; const AAddEndMessage: Boolean);
begin
  FMessages.Add(AContent);
  FAddEndMessage := AAddEndMessage;
end;

function  TPhippsAI.GetLastUserMessage(): string;
begin
  Result := FLastUserMessage;
end;

procedure TPhippsAI.UnloadModel();
begin
  if not ModelLoaded() then Exit;
  llama_free_model(FInference.Model);
  restore_cerr();
  FInference := Default(TInference);
end;

function TPhippsAI.TokenToPiece(const AVocab: Pllama_vocab; const AContext: Pllama_context; const AToken: llama_token; const ASpecial: Boolean): string;
var
  LTokens: Int32;
  LCheck: Int32;
  LBuffer: TArray<UTF8Char>;
begin
  try
    SetLength(LBuffer, 9);
    LTokens := llama_token_to_piece(AVocab, AToken, @LBuffer[0], 8, 0, ASpecial);
    if LTokens < 0 then
      begin
        SetLength(LBuffer, (-LTokens)+1);
        LCheck := llama_token_to_piece(AVocab, AToken, @LBuffer[0], -LTokens, 0, ASpecial);
        Assert(LCheck = -LTokens);
        LBuffer[-LTokens] := #0;
      end
    else
      begin
        LBuffer[LTokens] := #0;
      end;
    Result := UTF8ToString(@LBuffer[0]);
  except
    on E: Exception do
    begin
      SetError(E.Message, []);
      Exit;
    end;
  end;
end;

function TPhippsAI.CalcPerformance(const AContext: Pllama_context): TPerformanceResult;
var
  LTotalTimeSec: Double;
  APerfData: llama_perf_context_data;
begin
  APerfData := llama_perf_context(AContext);

  // Convert milliseconds to seconds
  LTotalTimeSec := APerfData.t_eval_ms / 1000;

  // Total input tokens (n_p_eval assumed to be input tokens)
  Result.TotalInputTokens := APerfData.n_p_eval;

  // Total output tokens (n_eval assumed to be output tokens)
  Result.TotalOutputTokens := APerfData.n_eval;

  // Calculate tokens per second (total tokens / time in seconds)
  if LTotalTimeSec > 0 then
    Result.TokensPerSecond := (Result.TotalInputTokens + Result.TotalOutputTokens) / LTotalTimeSec
  else
    Result.TokensPerSecond := 0;
end;

function  TPhippsAI.DoRunInference(const ARawPrompt: string): Boolean;
var
  LNumPrompt: Integer;
  LPromptTokens: TArray<llama_token>;
  LCtxParams: llama_context_params;
  LNumPredict: integer;
  LCtx: Pllama_context;
  LSmplrParams: llama_sampler_chain_params;
  LSmplr: Pllama_sampler;
  N: Integer;
  LTokenStr: string;
  LBatch: llama_batch;
  LNewTokenId: llama_token;
  LNumPos: Integer;
  LPrompt: UTF8String;
  LFirstToken: Boolean;
  V: Int32;
  LBuf: array[0..255] of UTF8Char;
  LKey: string;
  LMaxContext: integer;
  LVocab: Pllama_vocab;

  function BuildPrompt(): string;
  var
    LItem: string;
  begin
    for LItem in FMessages do
    begin
      Result := Result + LItem + ' ';
    end;

  end;

begin
  Result := False;

  // check if inference is already runnig
  if FInference.Active then
  begin
    SetError('[%s] Inference already active', ['RunInference']);
    Exit;
  end;

  // check if model not loaded
  if not ModelLoaded() then
  begin
    SetError('[%s] Model not loaded', ['RunInference']);
    Exit;
  end;

  if FMessages.Count <= 0 then
  begin
    SetError('Not messages was found', []);
    Exit(False);
  end;

  AddEndMessage();

  if ARawPrompt.IsEmpty then
    FInference.Prompt := BuildPrompt()
  else
    FInference.Prompt := ARawPrompt;

  if FInference.Prompt.IsEmpty then
  begin
    SetError('[%s] Inference prompt was empty', ['RunInference']);
    Exit;
  end;

  FInference.Active := True;
  FInference.Response := '';

  FError := '';
  LFirstToken := True;
  LMaxContext := 0;

  for V := 0 to llama_model_meta_count(FInference.Model)-1 do
  begin
    llama_model_meta_key_by_index(FInference.Model, V, @LBuf[0], length(LBuf));
    LKey := string(LBuf);
    if LKey.Contains('context_length') then
    begin
      llama_model_meta_val_str_by_index(FInference.Model, V, @LBuf[0], length(LBuf));
      LKey := string(LBuf);
      LMaxContext :=  LKey.ToInteger;
      break;
    end;
  end;

  if LMaxContext > 0 then
    LNumPredict := EnsureRange(FConfig.MaxContext, 512, LMaxContext)
  else
    LNumPredict := 512;

  LVocab := llama_model_get_vocab(FInference.Model);

  LPrompt := UTF8String(FInference.Prompt);

  LNumPrompt := -llama_tokenize(LVocab, PUTF8Char(LPrompt), Length(LPrompt), nil, 0, true, true);

  SetLength(LPromptTokens, LNumPrompt);

  if llama_tokenize(LVocab, PUTF8Char(LPrompt), Length(LPrompt), @LPromptTokens[0], Length(LPromptTokens), true, true) < 0 then
  begin
    SetError('Failed to tokenize prompt', []);
  end;

  LCtxParams := llama_context_default_params();
  LCtxParams.n_ctx := LNumPrompt + LNumPredict - 1;
  LCtxParams.n_batch := LNumPrompt;
  LCtxParams.no_perf := false;
  LCtxParams.n_threads := EnsureRange(FConfig.MaxThreads, 1, GetPhysicalProcessorCount());
  LCtxParams.n_threads_batch := LCtxParams.n_threads;
  LCtxParams.flash_attn := False;

  LCtx := llama_new_context_with_model(FInference.Model, LCtxParams);
  if LCtx = nil then
  begin
    SetError('Failed to create inference context', []);
    llama_free_model(FInference.Model);
    exit;
  end;

  LSmplrParams := llama_sampler_chain_default_params();
  LSmplr := llama_sampler_chain_init(LSmplrParams);
  llama_sampler_chain_add(LSmplr, llama_sampler_init_greedy());

  LBatch := llama_batch_get_one(@LPromptTokens[0], Length(LPromptTokens));

  LNumPos := 0;

  FInference.Perf := Default(TPerformanceResult);

  OnInferenceStart();

  llama_perf_context_reset(LCtx);

  while LNumPos + LBatch.n_tokens < LNumPrompt + LNumPredict do
  begin
    if OnCancelInference() then Break;

    N := llama_decode(LCtx, LBatch);
    if N <> 0 then
    begin
      SetError('Failed to decode context', []);
      llama_sampler_free(LSmplr);
      llama_free(LCtx);
      llama_free_model(FInference.Model);
      Exit;
    end;

    LNumPos := LNumPos + LBatch.n_tokens;

    LNewTokenId := llama_sampler_sample(LSmplr, LCtx, -1);

    if llama_token_is_eog(LVocab, LNewTokenId) then
    begin
      break;
    end;

    if llama_vocab_is_eog(LVocab, LNewTokenId) then
    begin
      break;
    end;

    LTokenStr := TokenToPiece(LVocab, LCtx, LNewTokenId, false);

    if LFirstToken then
    begin
      LTokenStr := LTokenStr.Trim();
      LFirstToken := False;
    end;

    case FTokenResponse.AddToken(LTokenStr) of
      tpaWait:
      begin
      end;

      tpaAppend:
      begin
        DoOnNextToken(FTokenResponse.LastWord(False));
      end;

      tpaNewline:
      begin
        DoOnNextToken(#10);
        DoOnNextToken(FTokenResponse.LastWord(True));
      end;
    end;

    LBatch := llama_batch_get_one(@LNewTokenId, 1);
  end;

  if FTokenResponse.Finalize then
  begin
    case FTokenResponse.AddToken('') of
      tpaWait:
      begin
      end;

      tpaAppend:
      begin
        DoOnNextToken(FTokenResponse.LastWord(False));
      end;

      tpaNewline:
      begin
        DoOnNextToken(#10);
        DoOnNextToken(FTokenResponse.LastWord(True));
      end;
    end;
  end;

  OnInferenceEnd();

  FInference.Perf := CalcPerformance(LCtx);

  llama_sampler_free(LSmplr);
  llama_free(LCtx);

  FInference.Active := False;
  FAddEndMessage := False;

  Result := True;
end;

procedure TPhippsAI.AddMessage(const ARole, AContent: string);
var
  LMsg: string;
  LRole: string;
  LContent: string;
begin
  if ARole.IsEmpty then Exit;
  if AContent.IsEmpty then Exit;

  LRole := ARole;
  LContent := AContent.Trim();

  if LRole = 'User' then
    LMsg := '<｜User｜>' + LContent
  else
  if LRole = 'Assistant' then
    LMsg := '<｜Assistant｜>' + LContent + '<｜end▁of▁sentence｜>';

  FMessages.Add(LMsg);
end;

procedure TPhippsAI.AddEndMessage();
begin
  if FAddEndMessage then
  begin
    FMessages.Add('<｜Assistant｜>');
  end;
end;

function  TPhippsAI.RunInference(): Boolean;
var
  LResponse: string;
  LFuncName: string;
  LFuncArgs: TFuncArgs;
  LFuncResponse: string;
  LKey, LValue: string;
  LSearch: string;
  LJsonObj: string;
begin
  Result := DoRunInference();

  LResponse := GetInferenceResponse();

  LJsonObj := ExtractFunctionCallJSON(LResponse);

  LResponse := CleanAndConvertJSON(LJsonObj);

  LJsonObj := NormalizeJSONArguments(LResponse);

  try
    LFuncArgs := nil;
    if IsFunctionCall(LJsonObj, LFuncName, LFuncArgs) then
    begin
      //AddRawMessage(LJsonObj, False);
      for LKey in LFuncArgs.Keys do
      begin
        LValue := LFuncArgs[LKey];
        if LFuncName = 'tool_websearch' then
          begin
            OnNextToken(CRLF);
            LSearch := TavilyWebSearch(FConfig.SearchAPIKey, LValue)
          end
        else
          LSearch := LValue;
        LFuncResponse := CreateFunctionResponseJSON(LFuncName, LSearch);
        //AddRawMessage(LFuncResponse, True);
        AddAssistantMessage(LFuncResponse);
      end;
      Result := RunInference();
    end;
  finally
    if Assigned(LFuncArgs) then
    begin
      LFuncArgs.Free();
      LFuncArgs := nil;
    end;
  end;
end;

function  TPhippsAI.GetInferenceResponse(): string;
begin
  Result := FInference.Response;
end;

procedure TPhippsAI.GetPerformance(const ATotalInputTokens: PInt32; ATotalOutputTokens: PInt32; ATokensPerSecond: PDouble);
begin
  if Assigned(ATotalInputTokens) then
    ATotalInputTokens^ := FInference.Perf.TotalInputTokens;

  if Assigned(ATotalOutputTokens) then
    ATotalOutputTokens^ := FInference.Perf.TotalOutputTokens;

  if Assigned(ATokensPerSecond) then
    ATokensPerSecond^ := FInference.Perf.TokensPerSecond;
end;

function  TPhippsAI.OnCancelInference(): Boolean;
begin
  Result := Boolean(GetAsyncKeyState(VK_ESCAPE) <> 0);
end;

procedure TPhippsAI.DoOnNextToken(const AToken: string);
var
  LToken: string;
begin
  LToken := AToken;
  FInference.Response := FInference.Response + LToken;

  if LToken.StartsWith('<think>') then
    begin
      LToken := '';
      FInference.Thinking := True;
      OnThinkStart();
    end
  else
  if LToken.StartsWith('</think>') then
    begin
      LToken := '';
      FInference.Thinking := False;
      OnThinkEnd();
    end;

  if FInference.Thinking then
  begin
    if not FConfig.ShowThinking then Exit;
  end;

  OnNextToken(LToken);
end;

procedure TPhippsAI.OnNextToken(const AToken: string);
begin
  Print(AToken, []);
end;

procedure TPhippsAI.OnInfo(const ALevel: Integer; const AText: string);
begin
  if Assigned(FCallbacks.Info.Handler) then
    begin
      FCallbacks.Info.Handler(ALevel, PWideChar(AText), FCallbacks.Info.UserData);
    end
  else
    begin
      //Print(AText, []);
    end;
end;

function  TPhippsAI.OnLoadModelProgress(const AModelFilename: string; const AProgress: Single): Boolean;
begin
  Result := True;

  if Assigned(FCallbacks.LoadModelProgress.Handler) then
    begin
      Result := FCallbacks.LoadModelProgress.Handler(PWideChar(AModelFilename), AProgress, FCallbacks.LoadModelProgress.UserData);
    end
  else
    begin
      Print(#13+'Loading model "%s" (%3.2f%s)...', [AModelFilename, AProgress*100, '%']);
      if AProgress >= 1 then
      begin
        Print(#27'[2K', []); // Clear the current line
        Print(#27'[G', []);  // Move cursor to the beginning of the line
      end;
    end;
end;

procedure TPhippsAI.OnLoadModel(const AModelFilename: string; const ASuccess: Boolean);
begin
  if Assigned(FCallbacks.LoadModel.Handler) then
    begin
      FCallbacks.LoadModel.Handler(PWideChar(AModelFilename), ASuccess, FCallbacks.LoadModel.UserData);
    end
  else
    begin
      if ASuccess then
        PrintLn('Sucessfully loaded model "%s"', [AModelFilename])
      else
        PrintLn('Failed to loaded model "%s"', [AModelFilename]);
    end;
end;

procedure TPhippsAI.OnInferenceStart();
begin
  if Assigned(FCallbacks.InferenceStart.Handler) then
    begin
      FCallbacks.InferenceStart.Handler(FCallbacks.InferenceStart.UserData);
    end
  else
    begin
      //PrintLn('', []);
      //PrintLn('[Inference Start]', []);
    end;
end;

procedure TPhippsAI.OnInferenceEnd();
begin
  if Assigned(FCallbacks.InferenceEnd.Handler) then
    begin
      FCallbacks.InferenceEnd.Handler(FCallbacks.InferenceEnd.UserData);
    end
  else
  begin
    //PrintLn('', []);
    //PrintLn('[Inference End]', []);
  end;
end;

procedure TPhippsAI.OnThinkStart();
begin
  if Assigned(FCallbacks.ThinkStart.Handler) then
    begin
      FCallbacks.ThinkStart.Handler(FCallbacks.ThinkStart.UserData);
    end
  else
    begin
      OnNextToken('<think>'+CRLF);
    end;
end;

procedure TPhippsAI.OnThinkEnd();
begin
  if Assigned(FCallbacks.ThinkEnd.Handler) then
    begin
      FCallbacks.ThinkEnd.Handler(FCallbacks.ThinkEnd.UserData);
    end
  else
    begin
      OnNextToken('</think>'+CRLF);
    end;
end;

{============================================================================}


var
  DepsDLLHandle: THandle = 0;
  DepsDLLFilename: string = '';

function LoadDLL1(var AError: string): Boolean;
var
  LResStream: TResourceStream;

  function f4d2d860c7f549a2ae1608b54be72701(): string;
  const
    CValue = '6e2e4fc31c3249b2ab65ba856dc86a61';
  begin
    Result := CValue;
  end;

  procedure SetError(const AText: string);
  begin
    AError := AText;
  end;

begin
  Result := False;
  AError := 'Failed to load LuaJIT DLL';

  // load deps DLL
  if DepsDLLHandle <> 0 then Exit(True);
  try
    if not Boolean((FindResource(HInstance, PChar(f4d2d860c7f549a2ae1608b54be72701()), RT_RCDATA) <> 0)) then
    begin
      SetError('Failed to find Deps DLL resource');
      Exit;
    end;
    LResStream := TResourceStream.Create(HInstance, f4d2d860c7f549a2ae1608b54be72701(), RT_RCDATA);
    try
      LResStream.Position := 0;
      DepsDLLFilename := TPath.Combine(TPath.GetTempPath,
        TPath.ChangeExtension(TPath.GetGUIDFileName.ToLower, '.'));
      if not HasEnoughDiskSpace(TPath.GetDirectoryName(DepsDLLFilename), LResStream.Size) then
      begin
        AError := 'Not enough disk space to extract the Deps DLL';
        Exit;
      end;

      LResStream.SaveToFile(DepsDLLFilename);
      if not TFile.Exists(DepsDLLFilename) then
      begin
        SetError('Failed to find extracted Deps DLL');
        Exit;
      end;
      DepsDLLHandle := LoadLibrary(PChar(DepsDLLFilename));
      if DepsDLLHandle = 0 then
      begin
        SetError('Failed to load extracted Deps DLL: ' + SysErrorMessage(GetLastError));
        Exit;
      end;

      GetExports(DepsDLLHandle);

      Result := True;
    finally
      LResStream.Free();
    end;
  except
    on E: Exception do
      SetError('Unexpected error: ' + E.Message);
  end;
end;

procedure UnloadDLL1();
begin
  // unload deps DLL
  if DepsDLLHandle <> 0 then
  begin
    FreeLibrary(DepsDLLHandle);
    TFile.Delete(DepsDLLFilename);
    DepsDLLHandle := 0;
    DepsDLLFilename := '';
  end;
end;

function LoadDLL2(var AError: string): Boolean;
var
  LResStream: TResourceStream;

  function f4d2d860c7f549a2ae1608b54be72701(): string;
  const
    CValue = '6e2e4fc31c3249b2ab65ba856dc86a61';
  begin
    Result := CValue;
  end;

  procedure SetError(const AText: string);
  begin
    AError := AText;
  end;

begin
  Result := False;
  AError := 'Failed to load LuaJIT DLL';

  // load deps DLL
  if DepsDLLHandle <> 0 then Exit(True);
  try
    if not Boolean((FindResource(HInstance, PChar(f4d2d860c7f549a2ae1608b54be72701()), RT_RCDATA) <> 0)) then
    begin
      SetError('Failed to find Deps DLL resource');
      Exit;
    end;
    LResStream := TResourceStream.Create(HInstance, f4d2d860c7f549a2ae1608b54be72701(), RT_RCDATA);
    try
      DepsDLLHandle := LoadMemoryDLL(LResStream.Memory, LResStream.Size);
      if DepsDLLHandle = 0 then
      begin
        SetError('Failed to load extracted Deps DLL: ' + SysErrorMessage(GetLastError));
        Exit;
      end;

      GetExports(DepsDLLHandle);

      Result := True;
    finally
      LResStream.Free();
    end;
  except
    on E: Exception do
      SetError('Unexpected error: ' + E.Message);
  end;
end;

procedure UnloadDLL2();
begin
  if DepsDLLHandle <> 0 then
  begin
    FreeLibrary(DepsDLLHandle);
    DepsDLLHandle := 0;
  end;
end;

initialization
var
  LError: string;
begin
  ReportMemoryLeaksOnShutdown := True;
  SetConsoleCP(CP_UTF8);
  SetConsoleOutputCP(CP_UTF8);
  EnableVirtualTerminalProcessing();
  if not LoadDLL2(LError) then
  begin
    MessageBox(0, PChar(LError), 'Critical Initialization Error', MB_ICONERROR);
    Halt(1); // Exit the application with a non-zero exit code to indicate failure
  end;
end;

finalization
begin
  try
    UnloadDLL2();
  except
    on E: Exception do
    begin
      MessageBox(0, PChar(E.Message), 'Critical Shutdown Error', MB_ICONERROR);
    end;
  end;
end;

end.

