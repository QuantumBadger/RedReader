/*
 * This file is part of RedReader, licensed under the GNU General Public License,
 * version 3 or later. See LICENSE.txt for details.
 */
#include <jni.h>
#include <stdexcept>
#include <string>

#ifdef RR_TRANSLATION_SUPPORTED
#include "llama.h"
#include <algorithm>
#include <memory>
#include <mutex>
#include <thread>
#include <vector>

namespace {
class Cancelled : public std::runtime_error {
public:
    Cancelled() : std::runtime_error("Translation cancelled") {}
};

// llama.cpp may check abort from a native worker; obtain that thread's JNIEnv.
class Cancellation {
    JavaVM *vm;
    jobject signal;
    jmethodID method;
    JNIEnv *owner;
public:
    Cancellation(JNIEnv *env, jobject input) : owner(env) {
        env->GetJavaVM(&vm);
        signal = env->NewGlobalRef(input);
        jclass type = env->GetObjectClass(input);
        method = env->GetMethodID(type, "isCancelled", "()Z");
        env->DeleteLocalRef(type);
        if (!signal || !method) {
            if (signal) env->DeleteGlobalRef(signal);
            throw std::runtime_error("Could not initialize cancellation");
        }
    }
    ~Cancellation() { owner->DeleteGlobalRef(signal); }
    bool cancelled() const {
        JNIEnv *env = nullptr;
        const bool attach = vm->GetEnv(reinterpret_cast<void **>(&env), JNI_VERSION_1_6)
                == JNI_EDETACHED;
#ifdef __ANDROID__
        if (attach && vm->AttachCurrentThread(&env, nullptr) != JNI_OK) return true;
#else
        if (attach && vm->AttachCurrentThread(reinterpret_cast<void **>(&env), nullptr)
                != JNI_OK) return true;
#endif
        if (!env) return true;
        const bool result = env->CallBooleanMethod(signal, method) == JNI_TRUE;
        if (attach) vm->DetachCurrentThread();
        return result;
    }
    void check() const { if (cancelled()) throw Cancelled(); }
    static bool abort(void *self) { return static_cast<Cancellation *>(self)->cancelled(); }
    static bool progress(float, void *self) { return !abort(self); }
};

std::string bytes(JNIEnv *env, jbyteArray input) {
    const jsize size = env->GetArrayLength(input);
    std::string result(size, '\0');
    env->GetByteArrayRegion(input, 0, size, reinterpret_cast<jbyte *>(result.data()));
    return result;
}

std::string generate(const std::string &path, const std::string &prompt, Cancellation &cancel) {
    static std::once_flag initialized;
    std::call_once(initialized, [] { llama_backend_init(); });
    cancel.check();

    auto modelParams = llama_model_default_params();
    modelParams.n_gpu_layers = 0;
    modelParams.progress_callback = Cancellation::progress;
    modelParams.progress_callback_user_data = &cancel;
    using Model = std::unique_ptr<llama_model, decltype(&llama_model_free)>;
    Model model(llama_model_load_from_file(path.c_str(), modelParams), llama_model_free);
    cancel.check();
    if (!model) throw std::runtime_error("Could not load this GGUF model");

    const auto *vocab = llama_model_get_vocab(model.get());
    const char *tmpl = llama_model_chat_template(model.get(), nullptr);
    if (!tmpl) throw std::runtime_error("The model has no supported chat template");
    const llama_chat_message message = {"user", prompt.c_str()};
    int size = llama_chat_apply_template(tmpl, &message, 1, true, nullptr, 0);
    if (size <= 0) throw std::runtime_error("Unsupported model chat template");
    std::vector<char> formatted(size + 1);
    size = llama_chat_apply_template(tmpl, &message, 1, true, formatted.data(), formatted.size());
    if (size <= 0 || size >= static_cast<int>(formatted.size())) {
        throw std::runtime_error("Could not format the translation prompt");
    }

    // The chat template includes its BOS token. Do not insert it a second time.
    int count = llama_tokenize(vocab, formatted.data(), size, nullptr, 0, false, true);
    if (count >= 0) throw std::runtime_error("Could not tokenize the translation prompt");
    std::vector<llama_token> tokens(-count);
    count = llama_tokenize(vocab, formatted.data(), size, tokens.data(), tokens.size(), false, true);
    if (count <= 0) throw std::runtime_error("Could not tokenize the translation prompt");
    tokens.resize(count);

    constexpr int contextSize = 8192;
    constexpr int maxOutput = 4096;
    if (count + maxOutput > contextSize) {
        throw std::runtime_error("Text is too long; translate a shorter post or comment");
    }
    if (llama_model_n_ctx_train(model.get()) < contextSize) {
        throw std::runtime_error("This model does not support the required context size");
    }
    auto contextParams = llama_context_default_params();
    contextParams.n_ctx = contextSize;
    contextParams.n_batch = 256;
    contextParams.n_ubatch = 128;
    const int threads = std::max(1u, std::min(4u, std::thread::hardware_concurrency()));
    contextParams.n_threads = threads;
    contextParams.n_threads_batch = threads;
    contextParams.abort_callback = Cancellation::abort;
    contextParams.abort_callback_data = &cancel;
    using Context = std::unique_ptr<llama_context, decltype(&llama_free)>;
    Context context(llama_init_from_model(model.get(), contextParams), llama_free);
    cancel.check();
    if (!context) throw std::runtime_error("Not enough memory to initialize the model");

    for (int offset = 0; offset < count; offset += contextParams.n_batch) {
        cancel.check();
        const int batchSize = std::min(static_cast<int>(contextParams.n_batch), count - offset);
        const int status = llama_decode(context.get(), llama_batch_get_one(
                tokens.data() + offset, batchSize));
        cancel.check();
        if (status != 0) throw std::runtime_error("Could not process the translation input");
    }

    using Sampler = std::unique_ptr<llama_sampler, decltype(&llama_sampler_free)>;
    Sampler sampler(llama_sampler_chain_init(llama_sampler_chain_default_params()),
            llama_sampler_free);
    llama_sampler_chain_add(sampler.get(), llama_sampler_init_penalties(
            llama_vocab_n_tokens(vocab), 64, 1.05f, 0.0f, 0.0f));
    llama_sampler_chain_add(sampler.get(), llama_sampler_init_top_k(20));
    llama_sampler_chain_add(sampler.get(), llama_sampler_init_top_p(0.6f, 1));
    llama_sampler_chain_add(sampler.get(), llama_sampler_init_temp(0.7f));
    llama_sampler_chain_add(sampler.get(), llama_sampler_init_dist(LLAMA_DEFAULT_SEED));
    std::string result;
    for (int generated = 0; generated < maxOutput; ++generated) {
        cancel.check();
        llama_token token = llama_sampler_sample(sampler.get(), context.get(), -1);
        if (llama_vocab_is_eog(vocab, token)) return result;
        char piece[256];
        int length = llama_token_to_piece(vocab, token, piece, sizeof(piece), 0, false);
        if (length < 0) {
            std::vector<char> large(-length);
            length = llama_token_to_piece(vocab, token, large.data(), large.size(), 0, false);
            if (length < 0) throw std::runtime_error("Could not decode a generated token");
            result.append(large.data(), length);
        } else {
            result.append(piece, length);
        }
        const int status = llama_decode(context.get(), llama_batch_get_one(&token, 1));
        cancel.check();
        if (status != 0) throw std::runtime_error("Translation inference failed");
    }
    throw std::runtime_error("Translation exceeded the output limit; use a shorter text");
}
} // namespace
#endif

extern "C" JNIEXPORT jbyteArray JNICALL
Java_org_quantumbadger_redreader_translation_LlamaNative_generate(
        JNIEnv *env, jclass, jbyteArray modelPath, jbyteArray prompt, jobject signal) {
    const char *errorClass = "java/io/IOException";
    std::string error;
#ifdef RR_TRANSLATION_SUPPORTED
    try {
        Cancellation cancellation(env, signal);
        const std::string result = generate(bytes(env, modelPath), bytes(env, prompt), cancellation);
        auto output = env->NewByteArray(result.size());
        if (output) env->SetByteArrayRegion(output, 0, result.size(),
                reinterpret_cast<const jbyte *>(result.data()));
        return output;
    } catch (const Cancelled &e) {
        errorClass = "java/io/InterruptedIOException";
        error = e.what();
    } catch (const std::exception &e) {
        error = e.what();
    }
#else
    error = "Local translation requires a 64-bit Android device";
#endif
    if (!env->ExceptionCheck()) env->ThrowNew(env->FindClass(errorClass), error.c_str());
    return nullptr;
}
