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

 BSD 3-Clause License

 Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions are met:

 1. Redistributions of source code must retain the above copyright notice,
    this list of conditions and the following disclaimer.

 2. Redistributions in binary form must reproduce the above copyright notice,
    this list of conditions and the following disclaimer in the documentation
    and/or other materials provided with the distribution.

 3. Neither the name of the copyright holder nor the names of its
    contributors may be used to endorse or promote products derived from
    this software without specific prior written permission.

 THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
 LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 POSSIBILITY OF SUCH DAMAGE.

-------------------------------------------------------------------------------

 This project uses the following open-source libraries:
 * llama.cpp - https://github.com/ggerganov/llama.cpp
 * MemoryDLL - https://github.com/tinyBigGAMES/MemoryDLL

-------------------------------------------------------------------------------

 >>> USAGE NOTES <<<
 ===================

 Model Requirement:
 - You must use the DeepSeek R1 model, specifically:
   deepseek-r1-distill-llama-8b-abliterated-q4_k_m.gguf
 - Download it from our Hugging Face repository:
   https://huggingface.co/tinybiggames/DeepSeek-R1-Distill-Llama-8B-abliterated-Q4_K_M-GGUF/resolve/main/deepseek-r1-distill-llama-8b-abliterated-q4_k_m.gguf?download=true

 Model Overview:
 - This model is distilled from Llama 3.1 8B.
 - It supports function calling, a required feature for this project.

 Implemented Tool: Web Search
 - The model is configured to execute function calls to perform real-time web
   searches when needed.
 - The web search tool utilizes Tavily for retrieving real-time information.

 Setting Up Web Search:
 - To enable web search functionality, you need a free Tavily account:
   https://tavily.com/

 Setting the API Key:
  Option 1: Call the following function in your code:
            phSetSearchAPIKey('YOUR_TAVILY_APIKEY')

  Option 2: Set an environment variable:
  - Create an environment variable named TAVILY_API_KEY.
  - Assign it your Tavily API key.

  If the API key is not provided, the library will attempt to retrieve it from
  the environment variable automatically.

-------------------------------------------------------------------------------

 >>> CHANGELOG <<<
 =================

 Version 0.1.0:
  - Initial release
===============================================================================}

/// <summary>
///   The <c>PhippsAI</c> unit provides an interface for performing local AI inference
///   using the DeepSeek R1 reasoning model with optional web search capabilities.
/// </summary>
/// <remarks>
///   This library is designed to run AI inference locally, leveraging the DeepSeek R1 model
///   for advanced reasoning tasks. It also supports web search integration, allowing the model
///   to retrieve external information when necessary.
///
///   <para><b>Key Features:</b></para>
///   - Load and manage the DeepSeek R1 model for inference.
///   - Perform reasoning-based AI tasks locally without requiring an internet connection.
///   - Optionally integrate web search (powered by Tavily) for enhanced responses.
///   - Configure model settings such as GPU acceleration, context size, and threading.
///   - Manage conversation history for contextual AI responses.
///   - Monitor performance metrics such as token throughput and processing speed.
///
///   <para><b>Usage Notes:</b></para>
///   - Ensure the DeepSeek R1 model files are available locally before initializing the library.
///   - Call <c>phInit</c> before using any other functions and <c>phQuit</c> when done.
///   - To use web search functionality, set an API key using <c>phSetSearchAPIKey</c>.
/// </remarks>
/// <example>
///   Below is an example of initializing and running an inference task with PhippsAI:
///   <code>
///   if phInit() then
///   begin
///     phSetModelPath('C:\AI\Models\');
///     phSetModelFilename('deepseek-r1.gguf');
///
///     if phLoadModel() then
///     begin
///       phAddUserMessage('What is quantum mechanics?');
///       if phRunInference() then
///         WriteLn('AI Response: ', phGetInferenceResponse());
///       phUnloadModel();
///     end;
///
///     phQuit();
///   end
///   else
///     WriteLn('Failed to initialize PhippsAI: ', phGetError());
///   </code>
/// </example>
unit PhippsAI;

{$IFDEF FPC}
{$MODE DELPHIUNICODE}
{$ENDIF}

{$IFNDEF WIN64}
  // Generates a compile-time error if the target platform is not Win64
  {$MESSAGE Error 'Unsupported platform'}
{$ENDIF}

{$Z4}  // Sets the enumeration size to 4 bytes
{$A8}  // Sets the alignment for record fields to 8 bytes

interface

/// <summary>
///   The name of the PhippsAI dynamic-link library (DLL).
/// </summary>
/// <remarks>
///   This constant defines the filename of the external PhippsAI library.
///   The library must be present in the application’s directory or accessible in the system’s path
///   for the functions in this unit to work correctly.
/// </remarks>
/// <example>
///   <code>
///   WriteLn('Using library: ', PHIPPSAI_DLL);
///   </code>
/// </example>
const
  PHIPPSAI_DLL = 'PhippsAI.dll';

type
  /// <summary>
  ///   Pointer to a 32-bit integer.
  /// </summary>
  /// <remarks>
  ///   This type alias defines a pointer to a signed 32-bit integer (<c>Int32</c>).
  ///   It is commonly used for passing integer values by reference in the PhippsAI library.
  /// </remarks>
  /// <example>
  ///   <code>
  ///   var
  ///     Value: Int32;
  ///     PValue: PInt32;
  ///   begin
  ///     Value := 100;
  ///     PValue := @Value;
  ///     WriteLn('Pointer Value: ', PValue^);
  ///   end;
  ///   </code>
  /// </example>
  PInt32 = ^Int32;

  /// <summary>
  ///   Callback function type for handling inference cancellation requests.
  /// </summary>
  /// <remarks>
  ///   This callback allows the user to provide a mechanism for canceling an ongoing inference task.
  ///   When set, the inference process periodically calls this function, allowing external logic
  ///   to determine whether the inference should be stopped.
  /// </remarks>
  /// <param name="AUserData">
  ///   A pointer to user-defined data passed to the callback. This can be used to store
  ///   state information relevant to the cancellation logic.
  /// </param>
  /// <returns>
  ///   A <c>Boolean</c> value indicating whether the inference should be canceled:
  ///   <list type="bullet">
  ///     <item><c>True</c> → Cancel the inference process.</item>
  ///     <item><c>False</c> → Continue running the inference.</item>
  ///   </list>
  /// </returns>
  /// <example>
  ///   <code>
  ///   function MyCancelCallback(const AUserData: Pointer): Boolean; cdecl;
  ///   begin
  ///     Result := SomeConditionToCancel; // Set to True to cancel inference
  ///   end;
  ///   </code>
  /// </example>
  phInferenceCancelCallback = function(const AUserData: Pointer): Boolean; cdecl;

  /// <summary>
  ///   Callback procedure type for receiving generated tokens during inference.
  /// </summary>
  /// <remarks>
  ///   This callback is triggered whenever the AI model generates a new token (word or subword)
  ///   during inference. It enables real-time processing of model output, such as displaying partial
  ///   results, logging, or modifying behavior based on generated text.
  /// </remarks>
  /// <param name="AToken">
  ///   The generated token as a <c>PWideChar</c> (UTF-16 encoded string).
  /// </param>
  /// <param name="AUserData">
  ///   A pointer to user-defined data passed to the callback, allowing for custom processing.
  /// </param>
  /// <example>
  ///   <code>
  ///   procedure MyTokenCallback(const AToken: PWideChar; const AUserData: Pointer); cdecl;
  ///   begin
  ///     Write(AToken); // Print tokens as they are generated
  ///   end;
  ///   </code>
  /// </example>
  phInferenceTokenCallback = procedure(const AToken: PWideChar; const AUserData: Pointer); cdecl;

  /// <summary>
  ///   Callback procedure type for receiving informational messages from the PhippsAI library.
  /// </summary>
  /// <remarks>
  ///   This callback is triggered when the library generates informational messages, such as
  ///   warnings, logs, or status updates. The level of the message can be used to differentiate
  ///   between general information, warnings, and critical errors.
  /// </remarks>
  /// <param name="ALevel">
  ///   An integer representing the log level or severity of the message:
  ///   <list type="bullet">
  ///     <item><c>0</c> → General information.</item>
  ///     <item><c>1</c> → Warning message.</item>
  ///     <item><c>2</c> → Error message.</item>
  ///   </list>
  /// </param>
  /// <param name="AText">
  ///   A <c>PWideChar</c> containing the message text.
  /// </param>
  /// <param name="AUserData">
  ///   A pointer to user-defined data passed to the callback.
  /// </param>
  /// <example>
  ///   <code>
  ///   procedure MyInfoCallback(const ALevel: Integer; const AText: PWideChar; const AUserData: Pointer); cdecl;
  ///   begin
  ///     case ALevel of
  ///       0: WriteLn('[INFO] ', AText);
  ///       1: WriteLn('[WARNING] ', AText);
  ///       2: WriteLn('[ERROR] ', AText);
  ///     end;
  ///   end;
  ///   </code>
  /// </example>
  phInfoCallback = procedure(const ALevel: Integer; const AText: PWideChar; const AUserData: Pointer); cdecl;

  /// <summary>
  ///   Callback function type for monitoring model loading progress.
  /// </summary>
  /// <remarks>
  ///   This callback is called periodically while a model is being loaded, providing real-time progress updates.
  ///   It allows the user to track the loading status and potentially cancel the loading process if needed.
  /// </remarks>
  /// <param name="AModelName">
  ///   A <c>PWideChar</c> containing the name of the model being loaded.
  /// </param>
  /// <param name="AProgress">
  ///   A <c>Single</c> value representing the percentage of completion (0.0 to 100.0).
  /// </param>
  /// <param name="AUserData">
  ///   A pointer to user-defined data passed to the callback.
  /// </param>
  /// <returns>
  ///   A <c>Boolean</c> value:
  ///   <list type="bullet">
  ///     <item><c>True</c> → Continue loading the model.</item>
  ///     <item><c>False</c> → Cancel the model loading process.</item>
  ///   </list>
  /// </returns>
  /// <example>
  ///   <code>
  ///   function MyLoadModelProgressCallback(const AModelName: PWideChar; const AProgress: Single; const AUserData: Pointer): Boolean; cdecl;
  ///   begin
  ///     WriteLn(Format('Loading %s: %.2f%% complete', [AModelName, AProgress]));
  ///     Result := True; // Return False to cancel loading
  ///   end;
  ///   </code>
  /// </example>
  phLoadModelProgressCallback = function(const AModelName: PWideChar; const AProgress: Single; const AUserData: Pointer): Boolean; cdecl;

  /// <summary>
  ///   Callback procedure type for handling the completion status of a model load operation.
  /// </summary>
  /// <remarks>
  ///   This callback is triggered when the PhippsAI library finishes loading a model.
  ///   It informs the user whether the model was successfully loaded or if the operation failed.
  ///   This is useful for logging, UI updates, or triggering subsequent actions based on model availability.
  /// </remarks>
  /// <param name="AModelName">
  ///   A <c>PWideChar</c> containing the name of the model that was attempted to be loaded.
  /// </param>
  /// <param name="ASuccess">
  ///   A <c>Boolean</c> value indicating the outcome of the model loading operation:
  ///   <list type="bullet">
  ///     <item><c>True</c> → The model was successfully loaded and is ready for inference.</item>
  ///     <item><c>False</c> → Model loading failed (check <c>phGetError</c> for details).</item>
  ///   </list>
  /// </param>
  /// <param name="AUserData">
  ///   A pointer to user-defined data passed to the callback, allowing for custom handling of the model load event.
  /// </param>
  /// <example>
  ///   <code>
  ///   procedure MyLoadModelCallback(const AModelName: PWideChar; const ASuccess: Boolean; const AUserData: Pointer); cdecl;
  ///   begin
  ///     if ASuccess then
  ///       WriteLn('Model "', AModelName, '" loaded successfully.')
  ///     else
  ///       WriteLn('Failed to load model: ', AModelName);
  ///   end;
  ///   </code>
  /// </example>
  phLoadModelCallback = procedure(const AModelName: PWideChar; const ASuccess: Boolean; const AUserData: Pointer); cdecl;

  /// <summary>
  ///   Callback procedure type triggered when an inference process starts.
  /// </summary>
  /// <remarks>
  ///   This callback is called when the PhippsAI library begins running an inference task.
  ///   It allows the user to track inference start events, update UI elements, or log the beginning of processing.
  ///   This can be useful for displaying "AI is thinking..." messages or setting up resources before inference.
  /// </remarks>
  /// <param name="AUserData">
  ///   A pointer to user-defined data passed to the callback, allowing for custom behavior based on application context.
  /// </param>
  /// <example>
  ///   <code>
  ///   procedure MyInferenceStartCallback(const AUserData: Pointer); cdecl;
  ///   begin
  ///     WriteLn('Inference started.');
  ///   end;
  ///   </code>
  /// </example>
  phInferenceStartCallback = procedure(const AUserData: Pointer); cdecl;

  /// <summary>
  ///   Callback procedure type triggered when an inference process completes.
  /// </summary>
  /// <remarks>
  ///   This callback is called when the PhippsAI library finishes running an inference task.
  ///   It allows the user to track the completion of inference, update UI elements, or process the final output.
  ///   This can be useful for hiding loading indicators or triggering actions after inference is done.
  /// </remarks>
  /// <param name="AUserData">
  ///   A pointer to user-defined data passed to the callback, allowing for custom behavior based on application context.
  /// </param>
  /// <example>
  ///   <code>
  ///   procedure MyInferenceEndCallback(const AUserData: Pointer); cdecl;
  ///   begin
  ///     WriteLn('Inference completed.');
  ///   end;
  ///   </code>
  /// </example>
  phInferenceEndCallback = procedure(const AUserData: Pointer); cdecl;

  /// <summary>
  ///   Callback procedure type triggered when the AI model enters the "thinking" phase.
  /// </summary>
  /// <remarks>
  ///   This callback is called when the PhippsAI library starts its internal reasoning process before generating tokens.
  ///   The "thinking" phase involves analyzing the context, formulating a response, and preparing token output.
  ///   This callback can be useful for UI updates (e.g., showing a "Thinking..." indicator) or logging purposes.
  /// </remarks>
  /// <param name="AUserData">
  ///   A pointer to user-defined data passed to the callback, allowing for custom processing.
  /// </param>
  /// <example>
  ///   <code>
  ///   procedure MyThinkStartCallback(const AUserData: Pointer); cdecl;
  ///   begin
  ///     WriteLn('AI is thinking...');
  ///   end;
  ///   </code>
  /// </example>
  phThinkStartCallback = procedure(const AUserData: Pointer); cdecl;

  /// <summary>
  ///   Callback procedure type triggered when the AI model exits the "thinking" phase.
  /// </summary>
  /// <remarks>
  ///   This callback is called when the PhippsAI library completes its internal reasoning process
  ///   and is ready to begin generating tokens.
  ///   It can be used to update UI elements (e.g., hiding a "Thinking..." indicator) or log inference progress.
  /// </remarks>
  /// <param name="AUserData">
  ///   A pointer to user-defined data passed to the callback, allowing for custom processing.
  /// </param>
  /// <example>
  ///   <code>
  ///   procedure MyThinkEndCallback(const AUserData: Pointer); cdecl;
  ///   begin
  ///     WriteLn('AI has finished thinking.');
  ///   end;
  ///   </code>
  /// </example>
  phThinkEndCallback = procedure(const AUserData: Pointer); cdecl;

/// <summary>
///   Initializes the PhippsAI library, preparing it for use.
/// </summary>
/// <remarks>
///   This function must be called before using any other functions in the PhippsAI library.
///   If initialization fails, use <c>phGetError</c> to retrieve error details.
/// </remarks>
/// <returns>
///   Returns <c>True</c> if the library is successfully initialized; otherwise, returns <c>False</c>.
/// </returns>
/// <example>
///   <code>
///   if phInit() then
///     WriteLn('PhippsAI initialized successfully.')
///   else
///     WriteLn('Initialization failed: ', phGetError());
///   </code>
/// </example>
function phInit(): Boolean; cdecl; external PHIPPSAI_DLL;

/// <summary>
///   Shuts down the PhippsAI library and releases all allocated resources.
/// </summary>
/// <remarks>
///   Call this function when the library is no longer needed to free memory and close any active processes.
///   Failure to call <c>phQuit</c> may result in memory leaks.
/// </remarks>
/// <example>
///   <code>
///   phQuit();
///   </code>
/// </example>
procedure phQuit(); cdecl; external PHIPPSAI_DLL;

/// <summary>
///   Retrieves the current version of the PhippsAI library.
/// </summary>
/// <remarks>
///   This function returns a pointer to a wide string containing the version information.
///   The format of the returned string may vary but typically follows semantic versioning (e.g., "1.0.3").
/// </remarks>
/// <returns>
///   A <c>PWideChar</c> pointing to the version string of the library.
/// </returns>
/// <example>
///   <code>
///   WriteLn('PhippsAI Version: ', phVersion());
///   </code>
/// </example>
function phVersion(): PWideChar; cdecl; external PHIPPSAI_DLL;

/// <summary>
///   Retrieves the last error message from the PhippsAI library.
/// </summary>
/// <remarks>
///   If any function fails, this function provides details about the error encountered.
///   The returned string is managed by the library and should not be freed manually.
/// </remarks>
/// <returns>
///   A <c>PWideChar</c> containing a human-readable error message.
///   If no error has occurred, an empty string may be returned.
/// </returns>
/// <example>
///   <code>
///   if not phInit() then
///     WriteLn('Error: ', phGetError());
///   </code>
/// </example>
function phGetError(): PWideChar; cdecl; external PHIPPSAI_DLL;

/// <summary>
///   Retrieves the current configuration filename used by the PhippsAI library.
/// </summary>
/// <remarks>
///   This function returns a pointer to a wide string containing the filename of the current
///   configuration file. The configuration file typically stores various settings for
///   model management, inference parameters, and hardware configurations.
/// </remarks>
/// <returns>
///   A <c>PWideChar</c> representing the path to the current configuration file.
///   If no configuration file is set, this function may return an empty string or <c>nil</c>.
/// </returns>
/// <example>
///   <code>
///   WriteLn('Current Config File: ', phGetConfigFilename());
///   </code>
/// </example>
function phGetConfigFilename(): PWideChar; cdecl; external PHIPPSAI_DLL;

/// <summary>
///   Sets the configuration filename for the PhippsAI library.
/// </summary>
/// <remarks>
///   This function allows the user to specify a custom configuration file for PhippsAI.
///   If the parameter <c>AFilename</c> is <c>nil</c>, the library may revert to its default configuration file.
///   It is recommended to set the configuration file before calling <c>phLoadConfig</c> to ensure proper settings are loaded.
/// </remarks>
/// <param name="AFilename">
///   The full path to the configuration file as a <c>PWideChar</c>.
///   If <c>nil</c>, the library may use the default configuration file.
/// </param>
/// <example>
///   <code>
///   phSetConfigFilename('config.ini');
///   WriteLn('New Config File: ', phGetConfigFilename());
///   </code>
/// </example>
procedure phSetConfigFilename(const AFilename: PWideChar=nil); cdecl; external PHIPPSAI_DLL;

/// <summary>
///   Retrieves the current directory path where AI models are stored.
/// </summary>
/// <remarks>
///   This function returns a pointer to a wide string containing the directory path
///   where PhippsAI searches for model files. The model path is used to locate and load
///   AI models required for inference tasks.
/// </remarks>
/// <returns>
///   A <c>PWideChar</c> representing the current model directory path.
///   If no model path is set, this function may return an empty string or <c>nil</c>.
/// </returns>
/// <example>
///   <code>
///   WriteLn('Current Model Path: ', phGetModelPath());
///   </code>
/// </example>
function phGetModelPath(): PWideChar; cdecl; external PHIPPSAI_DLL;

/// <summary>
///   Sets the directory path where AI models are stored.
/// </summary>
/// <remarks>
///   This function allows the user to specify a custom directory where AI models are located.
///   The path must be set before calling <c>phLoadModel</c> to ensure the library can locate the models.
/// </remarks>
/// <param name="APath">
///   The full directory path as a <c>PWideChar</c>.
/// </param>
/// <example>
///   <code>
///   phSetModelPath('C:\AI\Models\');
///   WriteLn('Updated Model Path: ', phGetModelPath());
///   </code>
/// </example>
procedure phSetModelPath(const APath: PWideChar); cdecl; external PHIPPSAI_DLL;

/// <summary>
///   Retrieves the filename of the currently selected AI model.
/// </summary>
/// <remarks>
///   This function returns a pointer to a wide string containing the filename of the AI model
///   that is set to be used for inference. The filename should correspond to a model file
///   stored in the directory specified by <c>phGetModelPath</c>.
/// </remarks>
/// <returns>
///   A <c>PWideChar</c> representing the filename of the current AI model.
///   If no model filename is set, this function may return an empty string or <c>nil</c>.
/// </returns>
/// <example>
///   <code>
///   WriteLn('Current Model Filename: ', phGetModelFilename());
///   </code>
/// </example>
function phGetModelFilename(): PWideChar; cdecl; external PHIPPSAI_DLL;

/// <summary>
///   Sets the filename of the AI model to be used for inference.
/// </summary>
/// <remarks>
///   This function allows the user to specify a model file within the directory set by
///   <c>phSetModelPath</c>. If <c>AFilename</c> is <c>nil</c>, the library may revert to
///   a default model filename or require explicit selection.
///   It is recommended to set the model filename before calling <c>phLoadModel</c>.
/// </remarks>
/// <param name="AFilename">
///   The filename of the AI model, including extension, as a <c>PWideChar</c>.
///   If <c>nil</c>, the library may use a default model filename.
/// </param>
/// <example>
///   <code>
///   phSetModeFilename('mymodel.gguf');
///   WriteLn('Updated Model Filename: ', phGetModelFilename());
///   </code>
/// </example>
procedure phSetModelFilename(const AFilename: PWideChar=nil); cdecl; external PHIPPSAI_DLL;

/// <summary>
///   Retrieves the maximum context size used for AI model inference.
/// </summary>
/// <remarks>
///   The context size determines how many tokens (words or subwords) the model can process
///   at once. A larger context allows the model to consider more prior conversation history
///   but may increase memory usage and computation time.
/// </remarks>
/// <returns>
///   A <c>UInt32</c> representing the maximum number of tokens that can be considered in the context.
/// </returns>
/// <example>
///   <code>
///   WriteLn('Current Max Context Size: ', phGetMaxContext());
///   </code>
/// </example>
function phGetMaxContext(): UInt32; cdecl; external PHIPPSAI_DLL;

/// <summary>
///   Sets the maximum context size for AI model inference.
/// </summary>
/// <remarks>
///   This function adjusts how many tokens the AI model retains during inference.
///   A larger context may improve response coherence but requires more memory and processing power.
///   The default value is <c>1024</c>, but this can be adjusted based on model capabilities and system resources.
/// </remarks>
/// <param name="AValue">
///   The maximum number of tokens (context size) as a <c>UInt32</c>. A typical range is 512 to 4096 tokens,
///   depending on the model.
/// </param>
/// <example>
///   <code>
///   phSetMaxContext(2048);
///   WriteLn('Updated Max Context Size: ', phGetMaxContext());
///   </code>
/// </example>
procedure phSetMaxContext(const AValue: UInt32=1024); cdecl; external PHIPPSAI_DLL;

/// <summary>
///   Retrieves the index of the main GPU used for AI model inference.
/// </summary>
/// <remarks>
///   This function returns the GPU index currently assigned for running inference.
///   If the value is <c>-1</c>, the library will automatically select the best available GPU.
///   If the value is <c>0</c> or higher, it corresponds to a specific GPU device index.
/// </remarks>
/// <returns>
///   An <c>Int32</c> representing the GPU index used for inference:
///   <list type="bullet">
///     <item>-1: Automatically select the best available GPU.</item>
///     <item>0 or higher: Use the specified GPU device index.</item>
///   </list>
/// </returns>
/// <example>
///   <code>
///   WriteLn('Current Main GPU Index: ', phGetMainGPU());
///   </code>
/// </example>
function phGetMainGPU(): Int32; cdecl; external PHIPPSAI_DLL;

/// <summary>
///   Sets the main GPU to be used for AI model inference.
/// </summary>
/// <remarks>
///   This function allows users to manually specify which GPU should be used for inference.
///   If <c>-1</c> is passed, the library will automatically select the most suitable GPU.
///   If <c>0</c> or higher is provided, the specified GPU index will be used.
/// </remarks>
/// <param name="AValue">
///   The GPU index as an <c>Int32</c>:
///   <list type="bullet">
///     <item>-1: Automatically select the best available GPU.</item>
///     <item>0 or higher: Manually specify a GPU index to use.</item>
///   </list>
/// </param>
/// <example>
///   <code>
///   phSetMainGPU(-1); // Auto-select best GPU
///   phSetMainGPU(0);  // Force GPU 0 for inference
///   WriteLn('Updated Main GPU Index: ', phGetMainGPU());
///   </code>
/// </example>
procedure phSetMainGPU(const AValue: Int32=-1); cdecl; external PHIPPSAI_DLL;

/// <summary>
///   Retrieves the number of GPU layers used during inference.
/// </summary>
/// <remarks>
///   This function returns the current number of GPU layers being utilized by the AI model during inference.
///   The number of GPU layers affects performance, as more layers offloaded to the GPU can improve speed,
///   but excessive usage may exceed the GPU’s memory capacity.
/// </remarks>
/// <returns>
///   An <c>Int32</c> representing the number of GPU layers used:
///   <list type="bullet">
///     <item><c>-1</c> → Use the maximum number of layers supported by the selected GPU.</item>
///     <item><c>0</c> → Use only the CPU for inference (no GPU acceleration).</item>
///     <item><c>1-N</c> → Specify the exact number of layers to be processed on the GPU.</item>
///   </list>
/// </returns>
/// <example>
///   <code>
///   WriteLn('Current GPU Layers: ', phGetGPULayers());
///   </code>
/// </example>
function phGetGPULayers(): Int32; cdecl; external PHIPPSAI_DLL;

/// <summary>
///   Sets the number of GPU layers to be used during inference.
/// </summary>
/// <remarks>
///   Adjusts the number of model layers offloaded to the GPU for processing.
///   Using more GPU layers can improve performance but requires sufficient VRAM.
///   The best setting depends on the specific hardware capabilities.
/// </remarks>
/// <param name="AValue">
///   The number of layers to be processed on the GPU:
///   <list type="bullet">
///     <item><c>-1</c> → Use the maximum number of layers the selected GPU can handle.</item>
///     <item><c>0</c> → Use CPU-only mode (disable GPU acceleration).</item>
///     <item><c>1-N</c> → Specify an exact number of layers to offload to the GPU.</item>
///   </list>
/// </param>
/// <example>
///   <code>
///   phSetGPULayers(10); // Use 10 layers on the GPU
///   WriteLn('Updated GPU Layers: ', phGetGPULayers());
///   </code>
/// </example>
procedure phSetGPULayers(const AValue: Int32=-1); cdecl; external PHIPPSAI_DLL;

/// <summary>
///   Retrieves the maximum number of threads used for inference.
/// </summary>
/// <remarks>
///   This function returns the current number of threads allocated for AI model inference.
///   Using multiple threads can enhance performance by parallelizing computations,
///   but excessive threading may lead to resource contention and reduced efficiency.
///   The value is automatically clipped between <c>1</c> and the total number of physical CPU cores available.
/// </remarks>
/// <returns>
///   An <c>Int32</c> representing the number of threads used during inference.
/// </returns>
/// <example>
///   <code>
///   WriteLn('Current Max Threads: ', phGetMaxThreads());
///   </code>
/// </example>
function phGetMaxThreads(): Int32; cdecl; external PHIPPSAI_DLL;

/// <summary>
///   Sets the maximum number of threads to be used for inference.
/// </summary>
/// <remarks>
///   This function allows adjusting the number of CPU threads allocated for inference.
///   The optimal thread count depends on the system's CPU capabilities and workload.
///   The specified value will be automatically constrained between 1 and the total number of
///   physical CPU cores available on the device.
/// </remarks>
/// <param name="AValue">
///   The number of CPU threads to use for inference.
///   The library will ensure this value stays within the valid range:
///   <list type="bullet">
///     <item><c>1</c> → Minimum threading (single-threaded execution).</item>
///     <item><c>N</c> → A user-defined number of threads, up to the maximum CPU core count.</item>
///   </list>
/// </param>
/// <example>
///   <code>
///   phSetMaxThreads(8); // Set the AI model to use 8 CPU threads
///   WriteLn('Updated Max Threads: ', phGetMaxThreads());
///   </code>
/// </example>
procedure phSetMaxThreads(const AValue: Int32=4); cdecl; external PHIPPSAI_DLL;

/// <summary>
///   Retrieves the currently set API key for the web search tool.
/// </summary>
/// <remarks>
///   This function returns the API key used to authenticate web search requests.
///   The search functionality is powered by <c>tavily.com</c>, and users need to sign up for an API key.
///   If no API key is set, this function may return an empty string or <c>nil</c>.
/// </remarks>
/// <returns>
///   A <c>PWideChar</c> containing the current search API key.
/// </returns>
/// <example>
///   <code>
///   WriteLn('Current Search API Key: ', phGetSearchAPIKey());
///   </code>
/// </example>
function phGetSearchAPIKey(): PWideChar; cdecl; external PHIPPSAI_DLL;

/// <summary>
///   Sets the API key for the web search tool.
/// </summary>
/// <remarks>
///   This function allows the user to configure an API key for accessing the search service
///   provided by <c>tavily.com</c>. An API key is required to perform web searches.
///   If <c>AValue</c> is <c>nil</c>, the existing API key may be cleared.
/// </remarks>
/// <param name="AValue">
///   The API key as a <c>PWideChar</c>. If <c>nil</c>, the search functionality may be disabled.
/// </param>
/// <example>
///   <code>
///   phSetSearchAPIKey('your-api-key-here');
///   WriteLn('Updated Search API Key: ', phGetSearchAPIKey());
///   </code>
/// </example>
procedure phSetSearchAPIKey(const AValue: PWideChar=nil); cdecl; external PHIPPSAI_DLL;

/// <summary>
///   Retrieves the currently set inference cancellation callback function.
/// </summary>
/// <remarks>
///   This function returns the callback function used to determine whether an inference process should be canceled.
///   If no callback has been set, this function may return <c>nil</c>.
///   The cancellation callback is periodically checked during inference execution to allow external control over stopping the process.
/// </remarks>
/// <returns>
///   A <c>phInferenceCancelCallback</c> function pointer, or <c>nil</c> if no callback is set.
/// </returns>
/// <example>
///   <code>
///   if Assigned(phGetInferenceCancelCallback()) then
///     WriteLn('A cancel callback is set.')
///   else
///     WriteLn('No cancel callback is set.');
///   </code>
/// </example>
function phGetInferenceCancelCallback(): phInferenceCancelCallback; cdecl; external PHIPPSAI_DLL;

/// <summary>
///   Sets the inference cancellation callback function.
/// </summary>
/// <remarks>
///   This function assigns a callback that is periodically checked during inference execution.
///   The callback function should return <c>True</c> to cancel the inference or <c>False</c> to allow it to continue.
///   A <c>AUserData</c> pointer can be provided to pass custom context data to the callback function.
///   Passing <c>nil</c> as <c>AHandler</c> disables the cancellation callback.
/// </remarks>
/// <param name="AHandler">
///   A function pointer to a <c>phInferenceCancelCallback</c> function.
///   If <c>nil</c>, the cancellation mechanism will be disabled.
/// </param>
/// <param name="AUserData">
///   A pointer to user-defined data that will be passed to the callback function during inference.
/// </param>
/// <example>
///   <code>
///   function MyCancelCallback(const AUserData: Pointer): Boolean; cdecl;
///   begin
///     Result := SomeConditionToCancel; // Return True to stop inference
///   end;
///
///   begin
///     phSetInferenceCancelCallback(@MyCancelCallback, nil);
///   end;
///   </code>
/// </example>
procedure phSetInferenceCancelCallback(const AHandler: phInferenceCancelCallback; const AUserData: Pointer); cdecl; external PHIPPSAI_DLL;

/// <summary>
///   Retrieves the currently set inference token callback function.
/// </summary>
/// <remarks>
///   This function returns the callback function that is triggered whenever a new token (word or subword)
///   is generated by the AI model during inference.
///   If no callback is set, this function may return <c>nil</c>.
///   The token callback is useful for real-time streaming of AI-generated text.
/// </remarks>
/// <returns>
///   A <c>phInferenceTokenCallback</c> function pointer, or <c>nil</c> if no callback is set.
/// </returns>
/// <example>
///   <code>
///   if Assigned(phGetInferenceTokenCallback()) then
///     WriteLn('A token callback is set.')
///   else
///     WriteLn('No token callback is set.');
///   </code>
/// </example>
function phGetInferenceTokenCallback(): phInferenceTokenCallback; cdecl; external PHIPPSAI_DLL;

/// <summary>
///   Sets the inference token callback function.
/// </summary>
/// <remarks>
///   This function assigns a callback that is triggered each time the AI model generates a new token.
///   The callback function receives the generated token and can be used for real-time streaming of AI responses.
///   A <c>AUserData</c> pointer can be provided to pass custom context data to the callback function.
///   Passing <c>nil</c> as <c>AHandler</c> disables the token callback.
/// </remarks>
/// <param name="AHandler">
///   A function pointer to a <c>phInferenceTokenCallback</c> function.
///   If <c>nil</c>, the token streaming mechanism will be disabled.
/// </param>
/// <param name="AUserData">
///   A pointer to user-defined data that will be passed to the callback function during inference.
/// </param>
/// <example>
///   <code>
///   procedure MyTokenCallback(const AToken: PWideChar; const AUserData: Pointer); cdecl;
///   begin
///     Write(AToken); // Print tokens as they are generated
///   end;
///
///   begin
///     phSetInferenceTokenCallback(@MyTokenCallback, nil);
///   end;
///   </code>
/// </example>
procedure phSetInferenceTokenlCallback(const AHandler: phInferenceTokenCallback; const AUserData: Pointer); cdecl; external PHIPPSAI_DLL;

/// <summary>
///   Retrieves the currently set information callback function.
/// </summary>
/// <remarks>
///   This function returns the callback function that handles informational messages generated
///   by the PhippsAI library. These messages can include logs, warnings, and errors.
///   If no callback is set, this function may return <c>nil</c>.
/// </remarks>
/// <returns>
///   A <c>phInfoCallback</c> function pointer, or <c>nil</c> if no callback is set.
/// </returns>
/// <example>
///   <code>
///   if Assigned(phGetInfoCallback()) then
///     WriteLn('An info callback is set.')
///   else
///     WriteLn('No info callback is set.');
///   </code>
/// </example>
function phGetInfoCallback(): phInfoCallback; cdecl; external PHIPPSAI_DLL;

/// <summary>
///   Sets the information callback function.
/// </summary>
/// <remarks>
///   This function assigns a callback that receives logs, warnings, and error messages
///   from the PhippsAI library. The callback allows real-time handling of messages for
///   debugging, UI updates, or logging.
///   A <c>AUserData</c> pointer can be provided to pass custom context data to the callback function.
///   Passing <c>nil</c> as <c>AHandler</c> disables the info callback.
/// </remarks>
/// <param name="AHandler">
///   A function pointer to a <c>phInfoCallback</c> function.
///   If <c>nil</c>, the info callback will be disabled.
/// </param>
/// <param name="AUserData">
///   A pointer to user-defined data that will be passed to the callback function.
/// </param>
/// <example>
///   <code>
///   procedure MyInfoCallback(const ALevel: Integer; const AText: PWideChar; const AUserData: Pointer); cdecl;
///   begin
///     case ALevel of
///       0: WriteLn('[INFO] ', AText);
///       1: WriteLn('[WARNING] ', AText);
///       2: WriteLn('[ERROR] ', AText);
///     end;
///   end;
///
///   begin
///     phSetInfoCallback(@MyInfoCallback, nil);
///   end;
///   </code>
/// </example>
procedure phSetInfoCallback(const AHandler: phInfoCallback; const AUserData: Pointer); cdecl; external PHIPPSAI_DLL;

/// <summary>
///   Retrieves the currently set model loading progress callback function.
/// </summary>
/// <remarks>
///   This function returns the callback function that is triggered during the model loading process.
///   The callback receives progress updates as the model loads, allowing for real-time monitoring
///   and the option to cancel loading if necessary.
///   If no callback is set, this function may return <c>nil</c>.
/// </remarks>
/// <returns>
///   A <c>phLoadModelProgressCallback</c> function pointer, or <c>nil</c> if no callback is set.
/// </returns>
/// <example>
///   <code>
///   if Assigned(phGetLoadModelProgressCallback()) then
///     WriteLn('A model load progress callback is set.')
///   else
///     WriteLn('No model load progress callback is set.');
///   </code>
/// </example>
function phGetLoadModelProgressCallback(): phLoadModelProgressCallback; cdecl; external PHIPPSAI_DLL;

/// <summary>
///   Sets the model loading progress callback function.
/// </summary>
/// <remarks>
///   This function assigns a callback that is triggered periodically during the model loading process.
///   The callback provides a progress percentage (0.0 to 100.0) and allows tracking of the model load status.
///   The callback also supports the option to cancel model loading by returning <c>False</c>.
///   A <c>AUserData</c> pointer can be provided to pass custom context data to the callback function.
///   Passing <c>nil</c> as <c>AHandler</c> disables the model load progress callback.
/// </remarks>
/// <param name="AHandler">
///   A function pointer to a <c>phLoadModelProgressCallback</c> function.
///   If <c>nil</c>, the model loading progress callback will be disabled.
/// </param>
/// <param name="AUserData">
///   A pointer to user-defined data that will be passed to the callback function.
/// </param>
/// <example>
///   <code>
///   function MyLoadModelProgressCallback(const AModelName: PWideChar; const AProgress: Single; const AUserData: Pointer): Boolean; cdecl;
///   begin
///     WriteLn(Format('Loading %s: %.2f%% complete', [AModelName, AProgress]));
///     Result := True; // Return False to cancel loading
///   end;
///
///   begin
///     phSetLoadModelProgressCallback(@MyLoadModelProgressCallback, nil);
///   end;
///   </code>
/// </example>
procedure phSetLoadModelProgressCallback(const AHandler: phLoadModelProgressCallback; const AUserData: Pointer); cdecl; external PHIPPSAI_DLL;

/// <summary>
///   Retrieves the currently set model load completion callback function.
/// </summary>
/// <remarks>
///   This function returns the callback function that is triggered when the PhippsAI library
///   finishes loading a model. The callback informs the user whether the model was successfully
///   loaded or if the operation failed.
///   If no callback is set, this function may return <c>nil</c>.
/// </remarks>
/// <returns>
///   A <c>phLoadModelCallback</c> function pointer, or <c>nil</c> if no callback is set.
/// </returns>
/// <example>
///   <code>
///   if Assigned(phGetLoadModelCallback()) then
///     WriteLn('A model load callback is set.')
///   else
///     WriteLn('No model load callback is set.');
///   </code>
/// </example>
function phGetLoadModelCallback(): phLoadModelCallback; cdecl; external PHIPPSAI_DLL;

/// <summary>
///   Sets the model load completion callback function.
/// </summary>
/// <remarks>
///   This function assigns a callback that is triggered when the model loading process finishes.
///   The callback provides the model name and whether the loading was successful.
///   A <c>AUserData</c> pointer can be provided to pass custom context data to the callback function.
///   Passing <c>nil</c> as <c>AHandler</c> disables the model load callback.
/// </remarks>
/// <param name="AHandler">
///   A function pointer to a <c>phLoadModelCallback</c> function.
///   If <c>nil</c>, the model load callback will be disabled.
/// </param>
/// <param name="AUserData">
///   A pointer to user-defined data that will be passed to the callback function.
/// </param>
/// <example>
///   <code>
///   procedure MyLoadModelCallback(const AModelName: PWideChar; const ASuccess: Boolean; const AUserData: Pointer); cdecl;
///   begin
///     if ASuccess then
///       WriteLn('Model "', AModelName, '" loaded successfully.')
///     else
///       WriteLn('Failed to load model: ', AModelName);
///   end;
///
///   begin
///     phSetLoadModelCallback(@MyLoadModelCallback, nil);
///   end;
///   </code>
/// </example>
procedure phSetLoadModelCallback(const AHandler: phLoadModelCallback; const AUserData: Pointer); cdecl; external PHIPPSAI_DLL;

/// <summary>
///   Retrieves the currently set inference start callback function.
/// </summary>
/// <remarks>
///   This function returns the callback function that is triggered when the AI model begins
///   running an inference task. The callback allows external logic to respond to inference
///   initiation, such as updating UI elements or logging events.
///   If no callback is set, this function may return <c>nil</c>.
/// </remarks>
/// <returns>
///   A <c>phInferenceStartCallback</c> function pointer, or <c>nil</c> if no callback is set.
/// </returns>
/// <example>
///   <code>
///   if Assigned(phGetInferenceStartCallback()) then
///     WriteLn('An inference start callback is set.')
///   else
///     WriteLn('No inference start callback is set.');
///   </code>
/// </example>
function phGetInferenceStartCallback(): phInferenceStartCallback; cdecl; external PHIPPSAI_DLL;

/// <summary>
///   Sets the inference start callback function.
/// </summary>
/// <remarks>
///   This function assigns a callback that is triggered when the AI model begins processing an
///   inference task. The callback can be used to display loading indicators, log inference start
///   times, or trigger other pre-processing tasks.
///   A <c>AUserData</c> pointer can be provided to pass custom context data to the callback function.
///   Passing <c>nil</c> as <c>AHandler</c> disables the inference start callback.
/// </remarks>
/// <param name="AHandler">
///   A function pointer to a <c>phInferenceStartCallback</c> function.
///   If <c>nil</c>, the inference start callback will be disabled.
/// </param>
/// <param name="AUserData">
///   A pointer to user-defined data that will be passed to the callback function.
/// </param>
/// <example>
///   <code>
///   procedure MyInferenceStartCallback(const AUserData: Pointer); cdecl;
///   begin
///     WriteLn('Inference process has started.');
///   end;
///
///   begin
///     phSetInferenceStartCallback(@MyInferenceStartCallback, nil);
///   end;
///   </code>
/// </example>
procedure phSetInferenceStartCallback(const AHandler: phInferenceStartCallback; const AUserData: Pointer); cdecl; external PHIPPSAI_DLL;

/// <summary>
///   Retrieves the currently set inference end callback function.
/// </summary>
/// <remarks>
///   This function returns the callback function that is triggered when the AI model completes
///   an inference task. The callback allows external logic to respond to inference completion,
///   such as updating UI elements, processing output, or logging events.
///   If no callback is set, this function may return <c>nil</c>.
/// </remarks>
/// <returns>
///   A <c>phInferenceEndCallback</c> function pointer, or <c>nil</c> if no callback is set.
/// </returns>
/// <example>
///   <code>
///   if Assigned(phGetInferenceEndCallback()) then
///     WriteLn('An inference end callback is set.')
///   else
///     WriteLn('No inference end callback is set.');
///   </code>
/// </example>
function phGetInferenceEndCallback(): phInferenceEndCallback; cdecl; external PHIPPSAI_DLL;

/// <summary>
///   Sets the inference end callback function.
/// </summary>
/// <remarks>
///   This function assigns a callback that is triggered when the AI model completes an
///   inference task. The callback can be used to hide loading indicators, process the AI-generated
///   response, log inference times, or trigger other post-processing tasks.
///   A <c>AUserData</c> pointer can be provided to pass custom context data to the callback function.
///   Passing <c>nil</c> as <c>AHandler</c> disables the inference end callback.
/// </remarks>
/// <param name="AHandler">
///   A function pointer to a <c>phInferenceEndCallback</c> function.
///   If <c>nil</c>, the inference end callback will be disabled.
/// </param>
/// <param name="AUserData">
///   A pointer to user-defined data that will be passed to the callback function.
/// </param>
/// <example>
///   <code>
///   procedure MyInferenceEndCallback(const AUserData: Pointer); cdecl;
///   begin
///     WriteLn('Inference process has completed.');
///   end;
///
///   begin
///     phSetInferenceEndCallback(@MyInferenceEndCallback, nil);
///   end;
///   </code>
/// </example>
procedure phSetInferenceEndCallback(const AHandler: phInferenceEndCallback; const AUserData: Pointer); cdecl; external PHIPPSAI_DLL;

/// <summary>
///   Retrieves the currently set "thinking start" callback function.
/// </summary>
/// <remarks>
///   This function returns the callback that is triggered when the AI model begins its
///   internal reasoning process before generating a response.
///   The "thinking" phase involves analyzing context, formulating a response, and preparing
///   token output. This callback can be used to display UI elements (e.g., a "Thinking..." indicator),
///   log events, or perform preparatory tasks.
///   If no callback is set, this function may return <c>nil</c>.
/// </remarks>
/// <returns>
///   A <c>phThinkStartCallback</c> function pointer, or <c>nil</c> if no callback is set.
/// </returns>
/// <example>
///   <code>
///   if Assigned(phGetThinkStartCallback()) then
///     WriteLn('A think start callback is set.')
///   else
///     WriteLn('No think start callback is set.');
///   </code>
/// </example>
function phGetThinkStartCallback(): phThinkStartCallback; cdecl; external PHIPPSAI_DLL;

/// <summary>
///   Sets the "thinking start" callback function.
/// </summary>
/// <remarks>
///   This function assigns a callback that is triggered when the AI model starts its internal
///   reasoning process before generating a response. The callback can be used to inform the
///   user that the model is processing input.
///   A <c>AUserData</c> pointer can be provided to pass custom context data to the callback function.
///   Passing <c>nil</c> as <c>AHandler</c> disables the thinking start callback.
/// </remarks>
/// <param name="AHandler">
///   A function pointer to a <c>phThinkStartCallback</c> function.
///   If <c>nil</c>, the thinking start callback will be disabled.
/// </param>
/// <param name="AUserData">
///   A pointer to user-defined data that will be passed to the callback function.
/// </param>
/// <example>
///   <code>
///   procedure MyThinkStartCallback(const AUserData: Pointer); cdecl;
///   begin
///     WriteLn('AI is now thinking...');
///   end;
///
///   begin
///     phSetThinkStartCallback(@MyThinkStartCallback, nil);
///   end;
///   </code>
/// </example>
procedure phSetThinkStartCallback(const AHandler: phThinkStartCallback; const AUserData: Pointer); cdecl; external PHIPPSAI_DLL;

/// <summary>
///   Retrieves the currently set "thinking end" callback function.
/// </summary>
/// <remarks>
///   This function returns the callback that is triggered when the AI model completes its
///   internal reasoning phase and is ready to generate a response.
///   The "thinking" phase involves analyzing context and formulating a response.
///   This callback is useful for updating UI elements (e.g., hiding a "Thinking..." indicator)
///   or logging inference progress.
///   If no callback is set, this function may return <c>nil</c>.
/// </remarks>
/// <returns>
///   A <c>phThinkEndCallback</c> function pointer, or <c>nil</c> if no callback is set.
/// </returns>
/// <example>
///   <code>
///   if Assigned(phGetThinkEndCallback()) then
///     WriteLn('A think end callback is set.')
///   else
///     WriteLn('No think end callback is set.');
///   </code>
/// </example>
function phGetThinkEndCallback(): phThinkEndCallback; cdecl; external PHIPPSAI_DLL;

/// <summary>
///   Sets the "thinking end" callback function.
/// </summary>
/// <remarks>
///   This function assigns a callback that is triggered when the AI model completes its
///   reasoning phase before generating a response. The callback can be used to indicate
///   that the AI is ready to provide output.
///   A <c>AUserData</c> pointer can be provided to pass custom context data to the callback function.
///   Passing <c>nil</c> as <c>AHandler</c> disables the thinking end callback.
/// </remarks>
/// <param name="AHandler">
///   A function pointer to a <c>phThinkEndCallback</c> function.
///   If <c>nil</c>, the thinking end callback will be disabled.
/// </param>
/// <param name="AUserData">
///   A pointer to user-defined data that will be passed to the callback function.
/// </param>
/// <example>
///   <code>
///   procedure MyThinkEndCallback(const AUserData: Pointer); cdecl;
///   begin
///     WriteLn('AI has finished thinking.');
///   end;
///
///   begin
///     phSetThinkEndCallback(@MyThinkEndCallback, nil);
///   end;
///   </code>
/// </example>
procedure phSetThinkEndCallback(const AHandler: phThinkEndCallback; const AUserData: Pointer); cdecl; external PHIPPSAI_DLL;

/// <summary>
///   Sets the right margin for token display formatting.
/// </summary>
/// <remarks>
///   This function defines the right margin for text formatting when displaying AI-generated tokens.
///   It helps control how text is wrapped or aligned during inference output.
///   By default, the margin is set to <c>10</c>.
/// </remarks>
/// <param name="AMargin">
///   An <c>Int32</c> value specifying the right margin for text output.
/// </param>
/// <example>
///   <code>
///   phSetTokenRightMargin(15); // Set right margin to 15
///   </code>
/// </example>
procedure phSetTokenRightMargin(const AMargin: Int32=10); cdecl; external PHIPPSAI_DLL;

/// <summary>
///   Sets the maximum line length for AI-generated token output.
/// </summary>
/// <remarks>
///   This function specifies the maximum number of characters allowed in a single line
///   when formatting AI-generated text output. It helps maintain readability by preventing
///   excessively long lines of text. By default, the maximum line length is <c>120</c>.
/// </remarks>
/// <param name="ALength">
///   An <c>Int32</c> value specifying the maximum line length.
/// </param>
/// <example>
///   <code>
///   phSetTokenMaxLineLength(100); // Set max line length to 100 characters
///   </code>
/// </example>
procedure phSetTokenMaxLineLength(const ALength: Int32=120); cdecl; external PHIPPSAI_DLL;

/// <summary>
///   Retrieves the current setting for displaying "thinking" tokens during inference.
/// </summary>
/// <remarks>
///   The PhippsAI library, powered by the DeepSeek R1 reasoning LLM, generates "thinking" tokens
///   that represent intermediate reasoning steps before producing a final response.
///   By default, these tokens are displayed, providing transparency into the model’s reasoning process.
///   This function checks whether the display of these tokens is currently enabled or disabled.
/// </remarks>
/// <returns>
///   A <c>Boolean</c> value:
///   <list type="bullet">
///     <item><c>True</c> → "Thinking" tokens are displayed during inference.</item>
///     <item><c>False</c> → "Thinking" tokens are hidden from the output.</item>
///   </list>
/// </returns>
/// <example>
///   <code>
///   if phGetShowThinking() then
///     WriteLn('Thinking tokens are currently displayed.')
///   else
///     WriteLn('Thinking tokens are hidden.');
///   </code>
/// </example>
function phGetShowThinking(): Boolean; cdecl; external PHIPPSAI_DLL;

/// <summary>
///   Enables or disables the display of "thinking" tokens during inference.
/// </summary>
/// <remarks>
///   This function allows the user to control whether intermediate "thinking" tokens
///   generated by the DeepSeek R1 reasoning LLM should be displayed.
///   These tokens can provide insight into the model’s thought process,
///   but they can also be hidden for a cleaner output.
///   By default, this feature is enabled.
/// </remarks>
/// <param name="AValue">
///   A <c>Boolean</c> value:
///   <list type="bullet">
///     <item><c>True</c> → Enable the display of "thinking" tokens (default).</item>
///     <item><c>False</c> → Hide "thinking" tokens from the output.</item>
///   </list>
/// </param>
/// <example>
///   <code>
///   phSetShowThinking(False); // Disable the display of thinking tokens
///   WriteLn('Thinking tokens visibility updated: ', phGetShowThinking());
///   </code>
/// </example>
procedure phSetShowThinking(const AValue: Boolean=True); cdecl; external PHIPPSAI_DLL;

/// <summary>
///   Saves the current configuration settings to the INI file.
/// </summary>
/// <remarks>
///   This function writes all current configuration parameters to the INI file specified
///   by <c>phSetConfigFilename</c>. If no filename has been set, the library may use a default path.
///   Saving the configuration ensures that settings persist between application runs.
/// </remarks>
/// <returns>
///   A <c>Boolean</c> value:
///   <list type="bullet">
///     <item><c>True</c> → Configuration successfully saved.</item>
///     <item><c>False</c> → Failed to save configuration (check for file permissions or invalid path).</item>
///   </list>
/// </returns>
/// <example>
///   <code>
///   if phSaveConfig() then
///     WriteLn('Configuration saved successfully.')
///   else
///     WriteLn('Failed to save configuration.');
///   </code>
/// </example>
function phSaveConfig(): Boolean; cdecl; external PHIPPSAI_DLL;

/// <summary>
///   Loads configuration settings from the INI file.
/// </summary>
/// <remarks>
///   This function reads configuration parameters from the INI file specified
///   by <c>phSetConfigFilename</c> and applies them to the library.
///   If no filename is set, the library may attempt to load a default configuration file.
/// </remarks>
/// <returns>
///   A <c>Boolean</c> value:
///   <list type="bullet">
///     <item><c>True</c> → Configuration loaded successfully.</item>
///     <item><c>False</c> → Failed to load configuration (file may not exist or contain errors).</item>
///   </list>
/// </returns>
/// <example>
///   <code>
///   if phLoadConfig() then
///     WriteLn('Configuration loaded successfully.')
///   else
///     WriteLn('Failed to load configuration.');
///   </code>
/// </example>
function phLoadConfig(): Boolean; cdecl; external PHIPPSAI_DLL;

/// <summary>
///   Loads the DeepSeek R1 reasoning model for local inference.
/// </summary>
/// <remarks>
///   This function loads the AI model defined by <c>phSetModelFilename</c> and <c>phSetModelPath</c>.
///   The model must be compatible with DeepSeek R1, as this library is specifically designed for it.
///   If the model is successfully loaded, it can be used for inference.
/// </remarks>
/// <returns>
///   A <c>Boolean</c> value:
///   <list type="bullet">
///     <item><c>True</c> → Model loaded successfully and ready for inference.</item>
///     <item><c>False</c> → Model failed to load (check model file path, compatibility, or system resources).</item>
///   </list>
/// </returns>
/// <example>
///   <code>
///   if phLoadModel() then
///     WriteLn('Model loaded successfully.')
///   else
///     WriteLn('Failed to load model: ', phGetError());
///   </code>
/// </example>
function phLoadModel(): Boolean; cdecl; external PHIPPSAI_DLL;

/// <summary>
///   Unloads the currently loaded DeepSeek R1 model and frees associated resources.
/// </summary>
/// <remarks>
///   This function ensures that the AI model is properly unloaded, freeing memory and computational resources.
///   After calling this function, the model will no longer be available for inference until it is reloaded with <c>phLoadModel</c>.
///   This is particularly useful when switching between models or cleaning up resources before exiting the application.
/// </remarks>
/// <example>
///   <code>
///   phUnloadModel();
///   WriteLn('Model has been unloaded.');
///   </code>
/// </example>
procedure phUnloadModel(); cdecl; external PHIPPSAI_DLL;

/// <summary>
///   Checks whether a DeepSeek R1 model is currently loaded.
/// </summary>
/// <remarks>
///   This function verifies if an AI model is currently loaded and ready for inference.
///   It is useful for ensuring that <c>phLoadModel</c> was successful before attempting inference.
/// </remarks>
/// <returns>
///   A <c>Boolean</c> value:
///   <list type="bullet">
///     <item><c>True</c> → A model is currently loaded and available for inference.</item>
///     <item><c>False</c> → No model is loaded.</item>
///   </list>
/// </returns>
/// <example>
///   <code>
///   if phModelLoaded() then
///     WriteLn('A model is currently loaded.')
///   else
///     WriteLn('No model loaded.');
///   </code>
/// </example>
function phModelLoaded(): Boolean; cdecl; external PHIPPSAI_DLL;

/// <summary>
///   Clears all stored messages from the conversation history.
/// </summary>
/// <remarks>
///   This function removes all messages stored in the AI's memory, including both user and assistant messages.
///   It is useful for starting a new conversation without previous context.
/// </remarks>
/// <example>
///   <code>
///   phClearMessages();
///   WriteLn('All messages cleared.');
///   </code>
/// </example>
procedure phClearMessages(); cdecl; external PHIPPSAI_DLL;

/// <summary>
///   Adds a new user message to the conversation history.
/// </summary>
/// <remarks>
///   This function appends a user message to the stored conversation history.
///   The message will be used as part of the context for generating responses in subsequent inference tasks.
/// </remarks>
/// <param name="AContent">
///   The message content as a <c>PWideChar</c>. It should be a valid UTF-16 encoded string.
/// </param>
/// <example>
///   <code>
///   phAddUserMessage('Hello, how are you?');
///   WriteLn('User message added.');
///   </code>
/// </example>
procedure phAddUserMessage(const AContent: PWideChar); cdecl; external PHIPPSAI_DLL;

/// <summary>
///   Adds a new assistant message to the conversation history.
/// </summary>
/// <remarks>
///   This function appends an assistant-generated response to the stored conversation history.
///   It is useful for tracking responses within multi-turn interactions.
/// </remarks>
/// <param name="AContent">
///   The assistant's message as a <c>PWideChar</c>. It should be a valid UTF-16 encoded string.
/// </param>
/// <example>
///   <code>
///   phAddAssistantMessage('I am doing great, how can I help you?');
///   WriteLn('Assistant message added.');
///   </code>
/// </example>
procedure phAddAssistantMessage(const AContent: PWideChar); cdecl; external PHIPPSAI_DLL;

/// <summary>
///   Retrieves the last message sent by the user.
/// </summary>
/// <remarks>
///   This function returns the most recent message added by the user.
///   It is useful for analyzing the latest user input before generating a response.
/// </remarks>
/// <returns>
///   A <c>PWideChar</c> containing the last user message.
///   If there are no messages in history, this function may return <c>nil</c> or an empty string.
/// </returns>
/// <example>
///   <code>
///   WriteLn('Last User Message: ', phGetLastUserMessage());
///   </code>
/// </example>
function phGetLastUserMessage(): PWideChar; cdecl; external PHIPPSAI_DLL;

/// <summary>
///   Executes an inference task using the currently loaded DeepSeek R1 model.
/// </summary>
/// <remarks>
///   This function processes the stored conversation history and generates a response using the AI model.
///   The inference process considers previous user and assistant messages to produce a coherent response.
///   Before calling this function, ensure that a model is loaded using <c>phLoadModel</c>.
/// </remarks>
/// <returns>
///   A <c>Boolean</c> value:
///   <list type="bullet">
///     <item><c>True</c> → Inference started successfully.</item>
///     <item><c>False</c> → Inference failed (check if a model is loaded or verify system resources).</item>
///   </list>
/// </returns>
/// <example>
///   <code>
///   if phRunInference() then
///     WriteLn('Inference completed successfully.')
///   else
///     WriteLn('Inference failed: ', phGetError());
///   </code>
/// </example>
function phRunInference(): Boolean; cdecl; external PHIPPSAI_DLL;

/// <summary>
///   Retrieves the AI-generated response from the last inference run.
/// </summary>
/// <remarks>
///   This function returns the text generated by the AI model as a response to the user input.
///   It should be called after <c>phRunInference</c> to retrieve the output.
/// </remarks>
/// <returns>
///   A <c>PWideChar</c> containing the model-generated response.
///   If no response is available, this function may return <c>nil</c> or an empty string.
/// </returns>
/// <example>
///   <code>
///   if phRunInference() then
///     WriteLn('AI Response: ', phGetInferenceResponse());
///   </code>
/// </example>
function phGetInferenceResponse(): PWideChar; cdecl; external PHIPPSAI_DLL;

/// <summary>
///   Retrieves inference performance metrics, including token processing speed.
/// </summary>
/// <remarks>
///   This function provides key performance data related to the last inference operation:
///   <list type="bullet">
///     <item><c>ATotalInputTokens</c> → The number of input tokens processed.</item>
///     <item><c>ATotalOutputTokens</c> → The number of tokens generated by the model.</item>
///     <item><c>ATokensPerSecond</c> → The generation speed in tokens per second.</item>
///   </list>
///   These metrics help analyze the efficiency of the inference process.
/// </remarks>
/// <param name="ATotalInputTokens">
///   A pointer to an <c>Int32</c> variable that will receive the total number of input tokens.
/// </param>
/// <param name="ATotalOutputTokens">
///   A pointer to an <c>Int32</c> variable that will receive the total number of output tokens.
/// </param>
/// <param name="ATokensPerSecond">
///   A pointer to a <c>Double</c> variable that will receive the inference speed in tokens per second.
/// </param>
/// <example>
///   <code>
///   var
///     InputTokens, OutputTokens: Int32;
///     Speed: Double;
///   begin
///     phGetPerformance(@InputTokens, @OutputTokens, @Speed);
///     WriteLn('Performance Metrics:');
///     WriteLn('  Input Tokens: ', InputTokens);
///     WriteLn('  Output Tokens: ', OutputTokens);
///     WriteLn('  Tokens per Second: ', Speed:0:2);
///   end;
///   </code>
/// </example>
procedure phGetPerformance(const ATotalInputTokens: PInt32; ATotalOutputTokens: PInt32; ATokensPerSecond: PDouble); cdecl; external PHIPPSAI_DLL;

procedure phAddRawMessage(const AContent: PWideChar; const AAddEndMessage: Boolean); cdecl; external PHIPPSAI_DLL;


implementation

uses
  Math;

initialization

{$IFNDEF FPC}
  ReportMemoryLeaksOnShutdown := True;
{$ENDIF}

  SetExceptionMask(GetExceptionMask + [exOverflow, exInvalidOp]);

finalization

  phQuit();


end.
