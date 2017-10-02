#pragma once
#include <algorithm>
#include <memory>
#include <string>
#include <vector>
#include "llvm/ADT/STLExtras.h"
#include "llvm/ExecutionEngine/ExecutionEngine.h"
#include "llvm/ExecutionEngine/JITSymbol.h"
#include "llvm/ExecutionEngine/Orc/CompileUtils.h"
#include "llvm/ExecutionEngine/Orc/IRCompileLayer.h"
#include "llvm/ExecutionEngine/Orc/LambdaResolver.h"
#include "llvm/ExecutionEngine/Orc/RTDyldObjectLinkingLayer.h"
#include "llvm/ExecutionEngine/RTDyldMemoryManager.h"
#include "llvm/ExecutionEngine/SectionMemoryManager.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/Mangler.h"
#include "llvm/Support/DynamicLibrary.h"
#include "llvm/Support/Error.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Target/TargetMachine.h"

using namespace llvm;
using namespace orc;
using namespace llvm::orc;
class SimpleJIT {
    TargetMachine *tm;
    const DataLayout dl;
    llvm::orc::RTDyldObjectLinkingLayerBase objectLayer;
    llvm::orc::IRCompileLayer<decltype(objectLayer), llvm::orc::SimpleCompiler> compileLayer;

   public:
    using ModuleHandle = decltype(compileLayer)::ModuleHandleT;
    SimpleJIT()
        : tm(EngineBuilder().selectTarget()),
          dl(tm->createDataLayout()),
          compileLayer(objectLayer, SimpleCompiler(*tm)) {
        llvm::sys::DynamicLibrary::LoadLibraryPermanently(nullptr);
    }

    TargetMachine &getTargetMachine() { return *tm; }

    SimpleJIT::ModuleHandle addModule(Module *M) {
        // Build our symbol resolver:
        // Lambda 1: Look back into the JIT itself to find symbols that are part
        // of
        //           the same "logical dylib".
        // Lambda 2: Search for external symbols in the host process.
        auto Resolver = createLambdaResolver(
            [&](const std::string &Name) {
                if (auto Sym = compileLayer.findSymbol(Name, false)) return Sym;
                return JITSymbol(nullptr);
            },
            [](const std::string &Name) {
                if (auto SymAddr =
                        RTDyldMemoryManager::getSymbolAddressInProcess(Name))
                    return JITSymbol(SymAddr, JITSymbolFlags::Exported);
                return JITSymbol(nullptr);
            });

        // Add the set to the JIT with the resolver we created above and a newly
        // created SectionMemoryManager.
        return cantFail(
            compileLayer.addModule(std::move(M), std::move(Resolver)));
    }

    JITSymbol findSymbol(const std::string Name) {
        std::string MangledName;
        raw_string_ostream MangledNameStream(MangledName);
        Mangler::getNameWithPrefix(MangledNameStream, Name, dl);
        return compileLayer.findSymbol(MangledNameStream.str(), true);
    }

    JITTargetAddress getSymbolAddress(const std::string Name) {
        return findSymbol(Name).getAddress();
    }

    void removeModule(SimpleJIT::ModuleHandle H) {
        cantFail(compileLayer.removeModule(H));
    }
};
