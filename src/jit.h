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
#include "sxhc/libstg.h"

using namespace llvm;
using namespace orc;
using namespace llvm::orc;
class SimpleJIT {
    std::vector<VModuleKey> ModuleKeys;
    // SymbolStringPool SSP;
    ExecutionSession ES;

    std::shared_ptr<SymbolResolver> Resolver;
    // std::shared_ptr<llvm::orc::LegacyLookupFnResolver<llvm::LegacyLookupFn>> Resolver;

    using ObjLayerT = llvm::orc::RTDyldObjectLinkingLayer;
    using CompileLayerT = llvm::orc::IRCompileLayer<ObjLayerT, llvm::orc::SimpleCompiler>;


    std::unique_ptr<TargetMachine> TM;
    const DataLayout DL;


    ObjLayerT ObjectLayer;
    CompileLayerT CompileLayer;

    // using OptimizeFunction =
    //     std::function<std::unique_ptr<Module>(std::unique_ptr<Module>)>;

    // IRTransformLayer<decltype(CompileLayer), OptimizeFunction> OptimizeLayer;

    public:
    SimpleJIT() :
        Resolver(createLegacyLookupResolver(ES,
                    [this](const std::string &Name) -> JITSymbol {


                    if (Name == "printOnlyInt")
                    return JITSymbol((JITTargetAddress)&printOnlyInt, JITSymbolFlags::Exported);

                    if (auto Sym = CompileLayer.findSymbol(Name, false)) {
                    return Sym;
                    } else if (auto Err = Sym.takeError()) {
                    errs() << "LOOKING FOR: " << Name << ":" << __LINE__ << "\n";
                    assert(false);
                    return std::move(Err);
                    }
                    if (auto SymAddr =
                            RTDyldMemoryManager::getSymbolAddressInProcess(Name)) {
                    return JITSymbol(SymAddr, JITSymbolFlags::Exported);
                    }


                    errs() << "LOOKING FOR: " << Name << ":" << __LINE__ << "\n";
                    assert(false);
                    return nullptr;
                    },
                    [](Error Err) { cantFail(std::move(Err), "lookupFlags failed"); })),
        TM(EngineBuilder().selectTarget()), DL(TM->createDataLayout()),
        ObjectLayer(ES,
                [this](VModuleKey) {
                return RTDyldObjectLinkingLayer::Resources{
                std::make_shared<SectionMemoryManager>(), Resolver};
                }),
        CompileLayer(ObjectLayer, SimpleCompiler(*TM)) {
            llvm::sys::DynamicLibrary::LoadLibraryPermanently(nullptr);
        }

    TargetMachine &getTargetMachine() { return *TM; }

    VModuleKey addModule(std::unique_ptr<Module> M) {


        auto K = ES.allocateVModule();
        cantFail(CompileLayer.addModule(K, std::move(M)));
        ModuleKeys.push_back(K);
        return K;

        /*
        // Build our symbol resolver:
        // Lambda 1: Look back into the JIT itself to find symbols that are part
        // of
        //           the same "logical dylib".
        // Lambda 2: Search for external symbols in the host process.
        auto Resolver = createLambdaResolver(
        [&](const std::string &Name) {
        if (auto Sym = CompileLayer.findSymbol(Name, false)) return Sym;
        return JITSymbol(nullptr);
        },
        [](const std::string &Name) {
        errs() << "** looking for symbol: " << Name << " in current process.\n";
        if (Name == "printOnlyInt")
        return JITSymbol((JITTargetAddress)&printOnlyInt, JITSymbolFlags::Exported);
        if (auto SymAddr =
        RTDyldMemoryManager::getSymbolAddressInProcess(Name)) {
        return JITSymbol(SymAddr, JITSymbolFlags::Exported);
        }
        return JITSymbol(nullptr);
        });
        M->setDataLayout(DL);
        // Add the set to the JIT with the resolver we created above and a newly
        // created SectionMemoryManager.
        return cantFail(
        CompileLayer.addModule(std::move(M), std::move(Resolver)));
        */
    }


    JITSymbol findSymbol (const std::string Name) {
        return findMangledSymbol(mangle(Name));
    }


    void removeModule(VModuleKey H) {
        cantFail(CompileLayer.removeModule(H));
    }

    private:
    std::string mangle(const std::string &Name) const {
        std::string MangledName;
        {
            raw_string_ostream MangledNameStream(MangledName);
            Mangler::getNameWithPrefix(MangledNameStream, Name, DL);
        }
        return MangledName;
    }

    JITSymbol findMangledSymbol(const std::string &Name) {
#ifdef LLVM_ON_WIN32
        // The symbol lookup of ObjectLinkingLayer uses the SymbolRef::SF_Exported
        // flag to decide whether a symbol will be visible or not, when we call
        // IRCompileLayer::findSymbolIn with ExportedSymbolsOnly set to true.
        //
        // But for Windows COFF objects, this flag is currently never set.
        // For a potential solution see: https://reviews.llvm.org/rL258665
        // For now, we allow non-exported symbols on Windows as a workaround.
        const bool ExportedSymbolsOnly = false;
#else
        const bool ExportedSymbolsOnly = true;
#endif

        // Search modules in reverse order: from last added to first added.
        // This is the opposite of the usual search order for dlsym, but makes more
        // sense in a REPL where we want to bind to the newest available definition.
        for (auto H : make_range(ModuleKeys.rbegin(), ModuleKeys.rend()))
            if (auto Sym = CompileLayer.findSymbolIn(H, Name, ExportedSymbolsOnly))
                return Sym;

        // If we can't find the symbol in the JIT, try looking in the host process.
        if (auto SymAddr = RTDyldMemoryManager::getSymbolAddressInProcess(Name))
            return JITSymbol(SymAddr, JITSymbolFlags::Exported);

#ifdef LLVM_ON_WIN32
        // For Windows retry without "_" at beginning, as RTDyldMemoryManager uses
        // GetProcAddress and standard libraries like msvcrt.dll use names
        // with and without "_" (for example "_itoa" but "sin").
        if (Name.length() > 2 && Name[0] == '_')
            if (auto SymAddr =
                    RTDyldMemoryManager::getSymbolAddressInProcess(Name.substr(1)))
                return JITSymbol(SymAddr, JITSymbolFlags::Exported);
#endif

        return nullptr;
    }

};
