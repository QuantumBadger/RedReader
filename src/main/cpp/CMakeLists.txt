cmake_minimum_required(VERSION 3.22)
project(redreader_translation LANGUAGES C CXX)

add_library(redreader_translation SHARED translation-jni.cpp)
target_compile_features(redreader_translation PRIVATE cxx_std_17)

# Keep the rest of RedReader available on 32-bit devices.
if(ANDROID AND NOT ANDROID_ABI MATCHES "^(arm64-v8a|x86_64)$")
    return()
endif()

include(FetchContent)
set(BUILD_SHARED_LIBS OFF CACHE BOOL "" FORCE)
set(LLAMA_BUILD_COMMON OFF CACHE BOOL "" FORCE)
set(LLAMA_BUILD_TESTS OFF CACHE BOOL "" FORCE)
set(LLAMA_BUILD_EXAMPLES OFF CACHE BOOL "" FORCE)
set(LLAMA_BUILD_TOOLS OFF CACHE BOOL "" FORCE)
set(LLAMA_BUILD_SERVER OFF CACHE BOOL "" FORCE)
set(LLAMA_OPENSSL OFF CACHE BOOL "" FORCE)
set(GGML_NATIVE OFF CACHE BOOL "" FORCE)
set(GGML_OPENMP OFF CACHE BOOL "" FORCE)
set(GGML_LLAMAFILE OFF CACHE BOOL "" FORCE)
set(GGML_METAL OFF CACHE BOOL "" FORCE)
set(GGML_BLAS OFF CACHE BOOL "" FORCE)
set(GGML_CPU_KLEIDIAI OFF CACHE BOOL "" FORCE)
set(CMAKE_POSITION_INDEPENDENT_CODE ON)

FetchContent_Declare(llama
    URL https://codeload.github.com/ggml-org/llama.cpp/tar.gz/3057bb66c86c46d5781e50e85462a760ba7d1feb
    URL_HASH SHA256=624ecdc13c247c87a2bd315d365542a24639510b4e40d1b51deae9b55634e5cc
)
FetchContent_MakeAvailable(llama)
target_compile_definitions(redreader_translation PRIVATE RR_TRANSLATION_SUPPORTED)
target_link_libraries(redreader_translation PRIVATE llama)

if(NOT ANDROID)
    find_package(JNI REQUIRED)
    target_include_directories(redreader_translation PRIVATE ${JNI_INCLUDE_DIRS})
endif()
