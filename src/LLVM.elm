module LLVM exposing (..)

import Maybe.Extra
import Html exposing (address)

type alias Program =
    List Module

type alias Module =
    List ModuleComponent

type ModuleComponent =
    Function
    | GlobalVariable GlobalVariableModel
    | SymbolTableEntry

type Identifier =
    Named String
    | Unnamed Int
    | Constant

type alias Linker =
    {}

type LinkageType =
    Private
    | Internal 
    | AvailableExternally
    | LinkOnce 
    | LinkOnceOdr
    | Weak
    | WeakOdr
    | Appending
    | Common
    | ExternWeak
    | External

type CallingConvention a =
    CCC
    | FastCC
    | ColdCC
    | CC10 
    | CC11
    | WebKitJSCC
    | AnyRegCC
    | PreserveMostCC
    | PreserveAllCC
    | CXXFastTLSCC
    | SwiftCC
    | CC Int
    | Custom a

type VisibilityStyle =
    Default
    | Hidden
    | Protected

type DLLStorageClass =
    DLLImport
    | DLLExport 

type ThreadLocalStorageModel =
    GeneralDynamic
    | LocalDynamic
    | InitialExec
    | LocalExec

type RuntimePreemtionSpecifier =
    DSOPreemptable
    | DSOLocal

{-type alias StructureType = 
    Debug.todo "WTH are StructureTypes"-}

{-type alias NonIntegalPointerType = 
    Debug.todo "WTH are NonIntegralPointerTypes"-}

type UnnamedAddressType =
    UnnamedAddress
    | LocalUnnamedAddress

type AddressSpace = 
    AddressSpace Int

type alias ExternallyInitialized =
    {}

type GlobalOrConstant =
    GlobalType
    | ConstantType

type Type =
    I32

type alias ConstantModel =
    {
        type_ : Type,
        string : String
    }

type alias Section =
    String

type alias Alignment = 
    Int

type GlobalVariableModel =
    GlobalVariableModel
    {
        globalVarName : String
        , linkage : Maybe LinkageType
        , preemptionSpecifier : Maybe RuntimePreemtionSpecifier
        , visibility : Maybe VisibilityStyle
        , dllStorageClass : Maybe DLLStorageClass
        , threadLocal : Maybe ThreadLocalStorageModel
        , address : Maybe UnnamedAddressType
        , addrSpace : Maybe AddressSpace
        , externallyInitialized : Maybe ExternallyInitialized
        , globalOrConstant : GlobalOrConstant
        , type_ : Type
        , initializerConstant : Maybe ConstantModel
        , section : Maybe Section
        , comdat : Maybe String -- WTH is comdat?
        , align : Maybe Alignment 
    }

combine : Linker -> List Module -> Module
combine =
    Debug.todo "Implement Combine"

viewLinkageType : LinkageType -> String
viewLinkageType l =
    case l of
        Private -> "private"
        Internal -> "internal"
        AvailableExternally -> "available_externally"
        LinkOnce -> "linkonce"
        Weak -> "weak"
        Common -> "common"
        Appending -> "appending"
        ExternWeak -> "extern_weak"
        LinkOnceOdr -> "linkonce_odr"
        WeakOdr -> "weak_odr"
        External -> "external"

viewRuntimePreemptionSpecifier : RuntimePreemtionSpecifier -> String
viewRuntimePreemptionSpecifier r = 
    case r of
        DSOPreemptable -> "dso_preemptable"
        DSOLocal -> "dso_local"

viewVisibilityStyle : VisibilityStyle -> String
viewVisibilityStyle v =
    case v of
        Default -> "default"
        Hidden -> "hidden"
        Protected -> "protected"

viewGlobalVariableModel : GlobalVariableModel -> String
viewGlobalVariableModel (GlobalVariableModel g) =
    String.join " " <| 
    Maybe.Extra.values
        [
            Just <| "@" ++ g.globalVarName
            , Just "="
            , Maybe.map viewLinkageType g.linkage
            , Maybe.map viewRuntimePreemptionSpecifier g.preemptionSpecifier
            , Maybe.map viewVisibilityStyle g.visibility
        ]

globalVariableModel : Type -> String -> GlobalOrConstant -> GlobalVariableModel 
globalVariableModel t name goc =
    GlobalVariableModel 
        {
            addrSpace = Nothing
            , address = Nothing
            , align = Nothing
            , comdat = Nothing
            , dllStorageClass = Nothing
            , externallyInitialized = Nothing
            , globalOrConstant = goc
            , globalVarName = name
            , initializerConstant = Nothing
            , linkage = Nothing
            , preemptionSpecifier = Nothing
            , section = Nothing
            , threadLocal = Nothing
            , type_ = t
            , visibility = Nothing
        }



restrictVisibilityStyle : LinkageType -> VisibilityStyle -> VisibilityStyle
restrictVisibilityStyle l v =
    case l of
        Internal -> Default
        Private -> Default
        _ -> v