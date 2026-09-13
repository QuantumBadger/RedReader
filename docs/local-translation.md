# Local post and comment translation

The Android app runs inference in-process with llama.cpp. It does not send post or
comment text to a translation server. The original Reddit data remains unchanged.

## Using the feature

1. Download the official [HY-MT2-1.8B Q4_K_M GGUF](https://huggingface.co/tencent/Hy-MT2-1.8B-GGUF/tree/main)
   to the device (approximately 1.13 GB). Use standard Q4_K_M, Q6_K or Q8_0 files;
   the specialized STQ 1.25-bit/2-bit variants are not supported by this integration.
2. Open **Settings → Local translation → Import model** and choose the file.
   The app copies it into private, non-backed-up storage. Keep enough free space
   for the downloaded file and its imported copy. Replacing a model temporarily
   requires room for both imported versions.
3. Choose a target language. The initial default is Simplified Chinese.
4. Open a post's action menu and select **Translate** to translate its title and
   available self-text. Open a comment's action menu to translate that comment.
   If you previously customized action menus, enable Translate under Settings → Menus.
5. The dialog shows selectable original text and translated text separately. Close
   it to cancel. Rotation cancels unfinished inference; tap Translate to restart.

This version translates one post or comment at a time, not an entire thread in one
action. Linked article bodies, image OCR and video transcription are not included.
Results are displayed as text; the model is instructed to preserve Markdown, code
and URLs, but formatting fidelity is not guaranteed.

Local inference requires **64-bit Android 6.0+** (arm64-v8a or x86_64). The rest of
RedReader keeps its original minimum Android version and 32-bit support. Actual RAM
requirements exceed the model file size and depend on the model and device.

## Architecture and model changes

```
Post/comment action → TranslationDialog → TranslationService
                                         → TranslationProvider
                                           → GgufTranslationProvider
                                             → LlamaNative (JNI) → llama.cpp
```

- `TranslationRequest` carries original text and a BCP 47 target language tag.
- `TranslationProvider` owns model-specific prompting, supported languages and
  inference. Its blocking methods run on the service's background worker. It must
  honor cancellation and report failures rather than fabricate a translation.
- `TranslationService` serializes requests and delivers results on the supplied
  callback executor. Closing a dialog cancels its future and suppresses late UI updates.
- `GgufTranslationProvider` supplies the HY-MT2 translation prompt. The runtime uses
  the GGUF chat template and the model's tokenizer. A different model may need a
  different provider/prompt; importing an arbitrary GGUF does not guarantee suitability.
- `LocalTranslation` is the single composition point for choosing a provider. A
  future engine can implement the same interface without changing Reddit or UI code.
- `TranslationModelStore` imports files through Android's document picker. Import
  checks GGUF magic/version; full model compatibility is checked during inference.
  Copy failure preserves the previous model. Replacement/removal waits for active
  inference to finish. **Remove model** deletes the private copy, not the source file.

The initial backend uses CPU inference, an 8,192-token context, at most 4,096 output
tokens, and up to four CPU threads. It reserves room for the output and rejects an
oversized input explicitly. It does not silently truncate or switch to a cloud model.
Native model, context and sampler memory are released after each request, including
failure and cancellation. This favors bounded memory use over repeated-load latency.
No translation-result cache is persisted.

## Building and validation

`src/main/cpp/CMakeLists.txt` downloads llama.cpp at commit
`3057bb66c86c46d5781e50e85462a760ba7d1feb`, with a pinned SHA-256 archive checksum.
The first native build requires network access. Gradle uses the project's configured
NDK and CMake 3.22.1. The model weights are not bundled in the APK or committed to Git.
The llama.cpp MIT license is included in `assets/licenses/llama.cpp.txt`.

Build with `./gradlew assembleDebug`. No model is required to compile the app.
For a provider/runtime upgrade, validate actual translations and cancellation with
the intended model, and verify native libraries for each supported ABI.

Device acceptance scenarios:

- With no model, Translate explains how to import one; browsing remains usable.
- Import a valid model, translate a post with self-text and a nested comment,
  and verify original text, links and Reddit actions remain available.
- Translate emoji/non-ASCII content and switch the target language.
- Cancel while loading and while generating; dismiss/reopen and rotate the screen.
- Import a non-GGUF file, cancel the document picker, and interrupt an import;
  the previous complete model should remain usable.
- Replace/remove the model and translate again. Try an oversized input and verify
  an explicit failure instead of a truncated translation.

Reference: [Tencent model instructions](https://github.com/Tencent-Hunyuan/Hy-MT2),
[llama.cpp Android documentation](https://github.com/ggml-org/llama.cpp/blob/master/docs/android.md).

### Validation performed

- `./gradlew :pmd :Checkstyle :lintDebug :assembleDebug --console=plain` passed;
  Android Lint reported no errors or warnings. No unit tests were run.
- `zipalign -c -P 16 4 build/outputs/apk/debug/RedReader-debug.apk` passed.
- The packaged ARM64 JNI library ran the official Q4_K_M model on an Android
  emulator through a temporary `app_process` integration harness. English text
  translated into Chinese with its emoji preserved; cancellation during model
  loading raised `InterruptedIOException`. The example took approximately 53
  seconds on that emulator; this is not a physical-device performance estimate.
- Full settings/import/menu interaction and physical-device memory/performance
  still require device acceptance checks. The native scenario does not validate
  those UI flows.
