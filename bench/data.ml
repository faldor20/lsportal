let absurdly_long_with_chunks={test|
<!DOCTYPE html>
<html lang="en">

<head>
    <meta charset="utf-8">
    <meta http-equiv="X-UA-Compatible" content="IE=edge">
    <meta name="viewport" content="width=device-width, initial-scale=1">

    <title>Specification</title>
    <meta name="description" content="This document describes the 3.17.x version of the language server protocol. An implementation for node of the 3.17.x version of the protocol can be found here.">


    <!-- link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0-beta.2/css/bootstrap.min.css" integrity="sha384-PsH8R72JQ3SOdhVi3uxftmaW6Vc51MKb0q5P2rRUpPvrszuE4W1povHYgTpBfshb" crossorigin="anonymous" -->
    <link rel="stylesheet" href="/language-server-protocol/css/bootswatch/cosmo/bootstrap.min.css">
    <link rel="stylesheet" href="/language-server-protocol/css/fontawesome-all.min.css">
    <link rel="stylesheet" href="/language-server-protocol/css/main.css">
    <link rel="shortcut icon" href="/language-server-protocol/img/favicon.svg">

    <link rel="canonical" href="https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/">
    <link rel="alternate" type="application/rss+xml" title="Official page for Language Server Protocol" href="https://microsoft.github.io/language-server-protocol/feed.xml" />
</head>


<body>
    <div id="cookie-banner"></div>
    <header class="navbar navbar-expand-lg navbar-dark bg-primary lsp-navbar">
    <div class="container navbar-container">
        <a class="navbar-brand" href="/language-server-protocol/">
            <h1>LSP / LSIF</h1>
        </a>

        <button class="navbar-toggler" type="button" data-toggle="collapse" data-target="#navbarSupportedContent" aria-controls="navbarSupportedContent" aria-expanded="false" aria-label="Toggle navigation">
                <span class="navbar-toggler-icon"></span>
        </button>

        <div class="navbar-collapse collapse" id="navbarSupportedContent" role="navigation" aria-label="Top Level">
            
            

            <ul class="navbar-nav ml-auto">
                <li class="nav-item">
                    <a class="nav-link" href="/language-server-protocol/overviews/lsp/overview">Overview</a>
                </li>
                <li class="nav-item">
                    <a class="nav-link" href="/language-server-protocol/implementors/servers/">Implementations</a>
                </li>
                <li class="nav-item">
                    <a class="nav-link" href="/language-server-protocol/specifications/specification-current">Specification</a>
                </li>
            </ul>
        </div>

    </div>
</header>

    <div class="page-content">
        <div class="wrapper">
            <div class="container">
    <div class="row single-page">
        <div class="d-none col-lg-2 d-lg-block lsp-sidebar">
            <div class="card">
                <div id="spec-group-accordion" data-children=".item">
                    
                    <div class="item">
                        <a class="nav-link" data-toggle="collapse" data-parent="#spec-group-accordion" href="#lsp" aria-expanded="true">
                            LSP
                        </a>
                        <div id="lsp" class="show" role="tabpanel">
                            <ul class="nav toc flex-column" style="padding-left: 0.5rem">
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="/language-server-protocol/specifications/lsp/3.17/specification">
                                        3.17 (Current)
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="/language-server-protocol/specifications/lsp/3.18/specification">
                                        3.18 (Upcoming)
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="/language-server-protocol/specifications/specification-3-16">
                                        3.16 (Previous)
                                    </a>
                                </li>
                                
                            </ul>
                        </div>
                    </div>
                    
                    <div class="item">
                        <a class="nav-link" data-toggle="collapse" data-parent="#spec-group-accordion" href="#lsif" aria-expanded="true">
                            LSIF
                        </a>
                        <div id="lsif" class="show" role="tabpanel">
                            <ul class="nav toc flex-column" style="padding-left: 0.5rem">
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="/language-server-protocol/specifications/lsif/0.6.0/specification">
                                        0.6.0 (Current)
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="/language-server-protocol/specifications/lsif/0.5.0/specification">
                                        0.5.0 (Previous)
                                    </a>
                                </li>
                                
                            </ul>
                        </div>
                    </div>
                    
                    <div class="item">
                        <a class="nav-link" data-toggle="collapse" data-parent="#spec-group-accordion" href="#base" aria-expanded="true">
                            Base Protocol
                        </a>
                        <div id="base" class="show" role="tabpanel">
                            <ul class="nav toc flex-column" style="padding-left: 0.5rem">
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="/language-server-protocol/specifications/base/0.9/specification">
                                        0.9 (Upcoming)
                                    </a>
                                </li>
                                
                            </ul>
                        </div>
                    </div>
                    
                </div>
            </div>
        </div>

        <div class="col-12 col-lg-7" role="navigation" aria-label="Main">
            <nav id="small-nav" class="docs-nav d-lg-none">
                <label for="small-nav-dropdown"><h4>Topics</h4></label>
                <select name="specifications" id="small-nav-dropdown">
                    
                    <optgroup class="item" label="LSP">
                        
                        <option value="/language-server-protocol/specifications/lsp/3.17/specification">3.17 (Current)</option>
                        
                        <option value="/language-server-protocol/specifications/lsp/3.18/specification">3.18 (Upcoming)</option>
                        
                        <option value="/language-server-protocol/specifications/specification-3-16">3.16 (Previous)</option>
                        
                    </optgroup>
                    
                    <optgroup class="item" label="LSIF">
                        
                        <option value="/language-server-protocol/specifications/lsif/0.6.0/specification">0.6.0 (Current)</option>
                        
                        <option value="/language-server-protocol/specifications/lsif/0.5.0/specification">0.5.0 (Previous)</option>
                        
                    </optgroup>
                    
                    <optgroup class="item" label="Base Protocol">
                        
                        <option value="/language-server-protocol/specifications/base/0.9/specification">0.9 (Upcoming)</option>
                        
                    </optgroup>
                    
                </select>
            </nav>

            <h1>Language Server Protocol Specification - 3.17</h1>

            <div class="d-block d-lg-none lsp-sidebar">
                
                <div class="card">
                    <div id="inline-sections-accordion" data-children=".item">
                        
                        <div class="item">
                            <a class="nav-link" data-toggle="collapse" data-parent="#inline-sections-accordion" href="#baseProtocol-inline" aria-expanded="true">
                                Base Protocol
                            </a>
                            <div id="baseProtocol-inline" class="collapse" role="tabpanel">
                                <ul class="nav toc flex-column" style="padding-left: 0.5rem">
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#headerPart">
                                            Header Part
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#contentPart">
                                            Content Part
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#baseTypes">
                                            Base Types
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#requestMessage">
                                            Request Message
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#responseMessage">
                                            Response Message
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#notificationMessage">
                                            Notification Message
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#cancelRequest">
                                            Cancellation Support
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#progress">
                                            Progress Support
                                        </a>
                                    </li>
                                    
                                </ul>
                            </div>
                        </div>
                        
                        <div class="item">
                            <a class="nav-link" data-toggle="collapse" data-parent="#inline-sections-accordion" href="#languageServerProtocol-inline" aria-expanded="true">
                                Language Server Protocol
                            </a>
                            <div id="languageServerProtocol-inline" class="collapse" role="tabpanel">
                                <ul class="nav toc flex-column" style="padding-left: 0.5rem">
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#languageServerProtocol">
                                            Overview
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#capabilities">
                                            Capabilities
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#messageOrdering">
                                            Message Ordering
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#messageDocumentation">
                                            Message Documentation
                                        </a>
                                    </li>
                                    
                                </ul>
                            </div>
                        </div>
                        
                        <div class="item">
                            <a class="nav-link" data-toggle="collapse" data-parent="#inline-sections-accordion" href="#basicJsonStructures-inline" aria-expanded="true">
                                Basic JSON Structures
                            </a>
                            <div id="basicJsonStructures-inline" class="collapse" role="tabpanel">
                                <ul class="nav toc flex-column" style="padding-left: 0.5rem">
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#uri">
                                            URI
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#regExp">
                                            Regular Expression
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#enumerations">
                                            Enumerations
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#textDocuments">
                                            Text Documents
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#position">
                                            Position
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#range">
                                            Range
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#textDocumentItem">
                                            Text Document Item
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#textDocumentIdentifier">
                                            Text Document Identifier
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#versionedTextDocumentIdentifier">
                                            Versioned Text Document Identifier
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#textDocumentPositionParams">
                                            Text Document Position Params
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#documentFilter">
                                            Document Filter
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#textEdit">
                                            Text Edit
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#textEditArray">
                                            Text Edit Array
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#textDocumentEdit">
                                            Text Document Edit
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#location">
                                            Location
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#locationLink">
                                            Location Link
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#diagnostic">
                                            Diagnostic
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#command">
                                            Command
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#markupContent">
                                            Markup Content
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#resourceChanges">
                                            File Resource Changes
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#workspaceEdit">
                                            Workspace Edit
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#workDoneProgress">
                                            Work Done Progress
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#clientInitiatedProgress">
                                            Client Initiated Progress
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#serverInitiatedProgress">
                                            Server Initiated Progress
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#partialResults">
                                            Partial Results
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#partialResultParams">
                                            Partial Result Params
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#traceValue">
                                            Trace Value
                                        </a>
                                    </li>
                                    
                                </ul>
                            </div>
                        </div>
                        
                        <div class="item">
                            <a class="nav-link" data-toggle="collapse" data-parent="#inline-sections-accordion" href="#lifeCycleMessages-inline" aria-expanded="true">
                                Lifecycle Messages
                            </a>
                            <div id="lifeCycleMessages-inline" class="collapse" role="tabpanel">
                                <ul class="nav toc flex-column" style="padding-left: 0.5rem">
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#lifeCycleMessages">
                                            Overview
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#initialize">
                                            Initialize
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#initialized">
                                            Initialized
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#client_registerCapability">
                                            Register Capability
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#client_unregisterCapability">
                                            Unregister Capability
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#setTrace">
                                            Set Trace
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#logTrace">
                                            Log Trace
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#shutdown">
                                            Shutdown
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#exit">
                                            Exit
                                        </a>
                                    </li>
                                    
                                </ul>
                            </div>
                        </div>
                        
                        <div class="item">
                            <a class="nav-link" data-toggle="collapse" data-parent="#inline-sections-accordion" href="#textSynchronization-inline" aria-expanded="true">
                                Document Synchronization
                            </a>
                            <div id="textSynchronization-inline" class="collapse" role="tabpanel">
                                <ul class="nav toc flex-column" style="padding-left: 0.5rem">
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#textDocument_synchronization">
                                            Overview - Text Document
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#textDocument_didOpen">
                                            Did Open Text Document
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#textDocument_didChange">
                                            Did Change Text Document
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#textDocument_willSave">
                                            Will Save Text Document
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#textDocument_willSaveWaitUntil">
                                            Will Save Document Wait Until
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#textDocument_didSave">
                                            Did Save Text Document
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#textDocument_didClose">
                                            Did Close Text Document
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#textDocument_didRename">
                                            Rename a Text Document
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#notebookDocument_synchronization">
                                            Overview - Notebook Document
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#notebookDocument_didOpen">
                                            Did Open Notebook Document
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#notebookDocument_didChange">
                                            Did Change Notebook Document
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#notebookDocument_didSave">
                                            Did Save Notebook Document
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#notebookDocument_didClose">
                                            Did Close Notebook Document
                                        </a>
                                    </li>
                                    
                                </ul>
                            </div>
                        </div>
                        
                        <div class="item">
                            <a class="nav-link" data-toggle="collapse" data-parent="#inline-sections-accordion" href="#languageFeatures-inline" aria-expanded="true">
                                Language Features
                            </a>
                            <div id="languageFeatures-inline" class="collapse" role="tabpanel">
                                <ul class="nav toc flex-column" style="padding-left: 0.5rem">
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#languageFeatures">
                                            Overview
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#textDocument_declaration">
                                            Go to Declaration
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#textDocument_definition">
                                            Go to Definition
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#textDocument_typeDefinition">
                                            Go to Type Definition
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#textDocument_implementation">
                                            Go to Implementation
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#textDocument_references">
                                            Find References
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#textDocument_prepareCallHierarchy">
                                            Prepare Call Hierarchy
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#callHierarchy_incomingCalls">
                                            Call Hierarchy Incoming Calls
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#callHierarchy_outgoingCalls">
                                            Call Hierarchy Outgoing Calls
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#textDocument_prepareTypeHierarchy">
                                            Prepare Type Hierarchy
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#typeHierarchy_supertypes">
                                            Type Hierarchy Super Types
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#typeHierarchy_subtypes">
                                            Type Hierarchy Sub Types
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#textDocument_documentHighlight">
                                            Document Highlight
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#textDocument_documentLink">
                                            Document Link
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#documentLink_resolve">
                                            Document Link Resolve
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#textDocument_hover">
                                            Hover
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#textDocument_codeLens">
                                            Code Lens
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#codeLens_refresh">
                                            Code Lens Refresh
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#textDocument_foldingRange">
                                            Folding Range
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#textDocument_selectionRange">
                                            Selection Range
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#textDocument_documentSymbol">
                                            Document Symbols
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#textDocument_semanticTokens">
                                            Semantic Tokens
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#textDocument_inlineValue">
                                            Inline Value
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#workspace_inlineValue_refresh">
                                            Inline Value Refresh
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#textDocument_inlayHint">
                                            Inlay Hint
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#inlayHint_resolve">
                                            Inlay Hint Resolve
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#workspace_inlayHint_refresh">
                                            Inlay Hint Refresh
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#textDocument_moniker">
                                            Moniker
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#textDocument_completion">
                                            Completion Proposals
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#completionItem_resolve">
                                            Completion Item Resolve
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#textDocument_publishDiagnostics">
                                            Publish Diagnostics
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#textDocument_pullDiagnostics">
                                            Pull Diagnostics
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#textDocument_signatureHelp">
                                            Signature Help
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#textDocument_codeAction">
                                            Code Action
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#codeAction_resolve">
                                            Code Action Resolve
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#textDocument_documentColor">
                                            Document Color
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#textDocument_colorPresentation">
                                            Color Presentation
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#textDocument_formatting">
                                            Formatting
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#textDocument_rangeFormatting">
                                            Range Formatting
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#textDocument_onTypeFormatting">
                                            On type Formatting
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#textDocument_rename">
                                            Rename
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#textDocument_prepareRename">
                                            Prepare Rename
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#textDocument_linkedEditingRange">
                                            Linked Editing Range
                                        </a>
                                    </li>
                                    
                                </ul>
                            </div>
                        </div>
                        
                        <div class="item">
                            <a class="nav-link" data-toggle="collapse" data-parent="#inline-sections-accordion" href="#workspaceFeatures-inline" aria-expanded="true">
                                Workspace Features
                            </a>
                            <div id="workspaceFeatures-inline" class="collapse" role="tabpanel">
                                <ul class="nav toc flex-column" style="padding-left: 0.5rem">
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#workspace_symbol">
                                            Workspace Symbols
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#workspace_symbolResolve">
                                            Workspace Symbol Resolve
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#workspace_configuration">
                                            Get Configuration
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#workspace_didChangeConfiguration">
                                            Did Change Configuration
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#workspace_workspaceFolders">
                                            Workspace Folders
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#workspace_didChangeWorkspaceFolders">
                                            Did Change Workspace Folders
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#workspace_willCreateFiles">
                                            Will Create Files
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#workspace_didCreateFiles">
                                            Did Create Files
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#workspace_willRenameFiles">
                                            Will Rename Files
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#workspace_didRenameFiles">
                                            Did Rename Files
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#workspace_willDeleteFiles">
                                            Will Delete Files
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#workspace_didDeleteFiles">
                                            Did Delete Files
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#workspace_didChangeWatchedFiles">
                                            Did Change Watched Files
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#workspace_executeCommand">
                                            Execute Command
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#workspace_applyEdit">
                                            Apply Edit
                                        </a>
                                    </li>
                                    
                                </ul>
                            </div>
                        </div>
                        
                        <div class="item">
                            <a class="nav-link" data-toggle="collapse" data-parent="#inline-sections-accordion" href="#windowFeatures-inline" aria-expanded="true">
                                Window Features
                            </a>
                            <div id="windowFeatures-inline" class="collapse" role="tabpanel">
                                <ul class="nav toc flex-column" style="padding-left: 0.5rem">
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#window_showMessage">
                                            Show Message Notification
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#window_showMessageRequest">
                                            Show Message Request
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#window_logMessage">
                                            Log Message
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#window_showDocument">
                                            Show Document
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#window_workDoneProgress_create">
                                            Create Work Done Progress
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#window_workDoneProgress_cancel">
                                            Cancel a Work Done Progress
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#telemetry_event">
                                            Sent Telemetry
                                        </a>
                                    </li>
                                    
                                </ul>
                            </div>
                        </div>
                        
                        <div class="item">
                            <a class="nav-link" data-toggle="collapse" data-parent="#inline-sections-accordion" href="#miscellaneous-inline" aria-expanded="true">
                                Miscellaneous
                            </a>
                            <div id="miscellaneous-inline" class="collapse" role="tabpanel">
                                <ul class="nav toc flex-column" style="padding-left: 0.5rem">
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#implementationConsiderations">
                                            Implementation Considerations
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#metaModel">
                                            Meta Model
                                        </a>
                                    </li>
                                    
                                </ul>
                            </div>
                        </div>
                        
                        <div class="item">
                            <a class="nav-link" data-toggle="collapse" data-parent="#inline-sections-accordion" href="#changeLog-inline" aria-expanded="true">
                                Change Log
                            </a>
                            <div id="changeLog-inline" class="collapse" role="tabpanel">
                                <ul class="nav toc flex-column" style="padding-left: 0.5rem">
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#version_3_17_0">
                                            3.17.0
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#version_3_16_0">
                                            3.16.0
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#version_3_15_0">
                                            3.15.0
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#version_3_14_0">
                                            3.14.0
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#version_3_13_0">
                                            3.13.0
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#version_3_12_0">
                                            3.12.0
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#version_3_11_0">
                                            3.11.0
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#version_3_10_0">
                                            3.10.0
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#version_3_9_0">
                                            3.9.0
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#version_3_8_0">
                                            3.8.0
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#version_3_7_0">
                                            3.7.0
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#version_3_6_0">
                                            3.6.0
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#version_3_5_0">
                                            3.5.0
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#version_3_4_0">
                                            3.4.0
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#version_3_3_0">
                                            3.3.0
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#version_3_2_0">
                                            3.2.0
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#version_3_1_0">
                                            3.1.0
                                        </a>
                                    </li>
                                    
                                    <li class="nav-item">
                                        <a class="nav-link toc-link" href="#version_3_0_0">
                                            3.0
                                        </a>
                                    </li>
                                    
                                </ul>
                            </div>
                        </div>
                        
                    </div>
                </div>
                
            </div>

            <div id="markdown-content-container">
<p>This document describes the 3.17.x version of the language server protocol. An implementation for node of the 3.17.x version of the protocol can be found <a href="https://github.com/Microsoft/vscode-languageserver-node">here</a>.</p>

<p><strong>Note:</strong> edits to this specification can be made via a pull request against this markdown <a href="https://github.com/Microsoft/language-server-protocol/blob/gh-pages/_specifications/lsp/3.17/specification.md">document</a>.</p>

<h2 id="-whats-new-in-317-"><a href="#whatIsNew" name="whatIsNew" class="anchor"> What’s new in 3.17 </a></h2>

<p>All new 3.17 features are tagged with a corresponding since version 3.17 text or in JSDoc using <code class="language-plaintext highlighter-rouge">@since 3.17.0</code> annotation. Major new feature are: type hierarchy, inline values, inlay hints, notebook document support and a meta model that describes the 3.17 LSP version.</p>

<p>A detailed list of the changes can be found in the <a href="#version_3_17_0">change log</a></p>

<p>The version of the specification is used to group features into a new specification release and to refer to their first appearance. Features in the spec are kept compatible using so called capability flags which are exchanged between the client and the server during initialization.</p>

<h2 id="-base-protocol-"><a href="#baseProtocol" name="baseProtocol" class="anchor"> Base Protocol </a></h2>

<p>The base protocol consists of a header and a content part (comparable to HTTP). The header and content part are
separated by a ‘\r\n’.</p>

<h3 id="-header-part-"><a href="#headerPart" name="headerPart" class="anchor"> Header Part </a></h3>

<p>The header part consists of header fields. Each header field is comprised of a name and a value, separated by ‘: ‘ (a colon and a space). The structure of header fields conform to the <a href="https://tools.ietf.org/html/rfc7230#section-3.2">HTTP semantic</a>. Each header field is terminated by ‘\r\n’. Considering the last header field and the overall header itself are each terminated with ‘\r\n’, and that at least one header is mandatory, this means that two ‘\r\n’ sequences always immediately precede the content part of a message.</p>

<p>Currently the following header fields are supported:</p>

<table class="table table-bordered table-responsive">
  <thead>
    <tr>
      <th style="text-align: left">Header Field Name</th>
      <th style="text-align: left">Value Type</th>
      <th style="text-align: left">Description</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td style="text-align: left">Content-Length</td>
      <td style="text-align: left">number</td>
      <td style="text-align: left">The length of the content part in bytes. This header is required.</td>
    </tr>
    <tr>
      <td style="text-align: left">Content-Type</td>
      <td style="text-align: left">string</td>
      <td style="text-align: left">The mime type of the content part. Defaults to application/vscode-jsonrpc; charset=utf-8</td>
    </tr>
  </tbody>
</table>

<p>The header part is encoded using the ‘ascii’ encoding. This includes the ‘\r\n’ separating the header and content part.</p>

<h3 id="-content-part-"><a href="#contentPart" name="contentPart" class="anchor"> Content Part </a></h3>

<p>Contains the actual content of the message. The content part of a message uses <a href="http://www.jsonrpc.org/">JSON-RPC</a> to describe requests, responses and notifications. The content part is encoded using the charset provided in the Content-Type field. It defaults to <code class="language-plaintext highlighter-rouge">utf-8</code>, which is the only encoding supported right now. If a server or client receives a header with a different encoding than <code class="language-plaintext highlighter-rouge">utf-8</code> it should respond with an error.</p>

<p>(Prior versions of the protocol used the string constant <code class="language-plaintext highlighter-rouge">utf8</code> which is not a correct encoding constant according to <a href="http://www.iana.org/assignments/character-sets/character-sets.xhtml">specification</a>.) For backwards compatibility it is highly recommended that a client and a server treats the string <code class="language-plaintext highlighter-rouge">utf8</code> as <code class="language-plaintext highlighter-rouge">utf-8</code>.</p>

<h3 id="example">Example:</h3>

<div class="language-plaintext highlighter-rouge"><div class="highlight"><pre class="highlight"><code>Content-Length: ...\r\n
\r\n
{
	"jsonrpc": "2.0",
	"id": 1,
	"method": "textDocument/completion",
	"params": {
		...
	}
}
</code></pre></div></div>
<h3 id="base-protocol-json-structures">Base Protocol JSON structures</h3>

<p>The following TypeScript definitions describe the base <a href="http://www.jsonrpc.org/specification">JSON-RPC protocol</a>:</p>

<h4 id="-base-types-"><a href="#baseTypes" name="baseTypes" class="anchor"> Base Types </a></h4>

<p>The protocol use the following definitions for integers, unsigned integers, decimal numbers, objects and arrays:</p>

<div class="anchorHolder"><a href="#integer" name="integer" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * Defines an integer number in the range of -2^31 to 2^31 - 1.
 */</span>
<span class="k">export</span> <span class="kd">type</span> <span class="nx">integer</span> <span class="o">=</span> <span class="kr">number</span><span class="p">;</span>
</code></pre></div></div>

<div class="anchorHolder"><a href="#uinteger" name="uinteger" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * Defines an unsigned integer number in the range of 0 to 2^31 - 1.
 */</span>
<span class="k">export</span> <span class="kd">type</span> <span class="nx">uinteger</span> <span class="o">=</span> <span class="kr">number</span><span class="p">;</span>
</code></pre></div></div>

<div class="anchorHolder"><a href="#decimal" name="decimal" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * Defines a decimal number. Since decimal numbers are very
 * rare in the language server specification we denote the
 * exact range with every decimal using the mathematics
 * interval notation (e.g. [0, 1] denotes all decimals d with
 * 0 &lt;= d &lt;= 1.
 */</span>
<span class="k">export</span> <span class="kd">type</span> <span class="nx">decimal</span> <span class="o">=</span> <span class="kr">number</span><span class="p">;</span>
</code></pre></div></div>

<div class="anchorHolder"><a href="#lspAny" name="lspAny" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * The LSP any type
 *
 * @since 3.17.0
 */</span>
<span class="k">export</span> <span class="kd">type</span> <span class="nx">LSPAny</span> <span class="o">=</span> <span class="nx">LSPObject</span> <span class="o">|</span> <span class="nx">LSPArray</span> <span class="o">|</span> <span class="kr">string</span> <span class="o">|</span> <span class="nx">integer</span> <span class="o">|</span> <span class="nx">uinteger</span> <span class="o">|</span>
	<span class="nx">decimal</span> <span class="o">|</span> <span class="nx">boolean</span> <span class="o">|</span> <span class="kc">null</span><span class="p">;</span>
</code></pre></div></div>

<div class="anchorHolder"><a href="#lspObject" name="lspObject" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * LSP object definition.
 *
 * @since 3.17.0
 */</span>
<span class="k">export</span> <span class="kd">type</span> <span class="nx">LSPObject</span> <span class="o">=</span> <span class="p">{</span> <span class="p">[</span><span class="na">key</span><span class="p">:</span> <span class="kr">string</span><span class="p">]:</span> <span class="nx">LSPAny</span> <span class="p">};</span>
</code></pre></div></div>

<div class="anchorHolder"><a href="#lspArray" name="lspArray" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * LSP arrays.
 *
 * @since 3.17.0
 */</span>
<span class="k">export</span> <span class="kd">type</span> <span class="nx">LSPArray</span> <span class="o">=</span> <span class="nx">LSPAny</span><span class="p">[];</span>
</code></pre></div></div>

<h4 id="-abstract-message-"><a href="#abstractMessage" name="abstractMessage" class="anchor"> Abstract Message </a></h4>

<p>A general message as defined by JSON-RPC. The language server protocol always uses “2.0” as the <code class="language-plaintext highlighter-rouge">jsonrpc</code> version.</p>

<div class="anchorHolder"><a href="#message" name="message" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="kr">interface</span> <span class="nx">Message</span> <span class="p">{</span>
	<span class="nl">jsonrpc</span><span class="p">:</span> <span class="kr">string</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>
<h4 id="-request-message-"><a href="#requestMessage" name="requestMessage" class="anchor"> Request Message </a></h4>

<p>A request message to describe a request between the client and the server. Every processed request must send a response back to the sender of the request.</p>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="kr">interface</span> <span class="nx">RequestMessage</span> <span class="kd">extends</span> <span class="nx">Message</span> <span class="p">{</span>

	<span class="cm">/**
	 * The request id.
	 */</span>
	<span class="nl">id</span><span class="p">:</span> <span class="nx">integer</span> <span class="o">|</span> <span class="kr">string</span><span class="p">;</span>

	<span class="cm">/**
	 * The method to be invoked.
	 */</span>
	<span class="nl">method</span><span class="p">:</span> <span class="kr">string</span><span class="p">;</span>

	<span class="cm">/**
	 * The method's params.
	 */</span>
	<span class="nl">params</span><span class="p">?:</span> <span class="nx">array</span> <span class="o">|</span> <span class="nx">object</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<h4 id="-response-message-"><a href="#responseMessage" name="responseMessage" class="anchor"> Response Message </a></h4>

<p>A Response Message sent as a result of a request. If a request doesn’t provide a result value the receiver of a request still needs to return a response message to conform to the JSON-RPC specification. The result property of the ResponseMessage should be set to <code class="language-plaintext highlighter-rouge">null</code> in this case to signal a successful request.</p>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="kr">interface</span> <span class="nx">ResponseMessage</span> <span class="kd">extends</span> <span class="nx">Message</span> <span class="p">{</span>
	<span class="cm">/**
	 * The request id.
	 */</span>
	<span class="nl">id</span><span class="p">:</span> <span class="nx">integer</span> <span class="o">|</span> <span class="kr">string</span> <span class="o">|</span> <span class="kc">null</span><span class="p">;</span>

	<span class="cm">/**
	 * The result of a request. This member is REQUIRED on success.
	 * This member MUST NOT exist if there was an error invoking the method.
	 */</span>
	<span class="nl">result</span><span class="p">?:</span> <span class="kr">string</span> <span class="o">|</span> <span class="kr">number</span> <span class="o">|</span> <span class="nx">boolean</span> <span class="o">|</span> <span class="nx">array</span> <span class="o">|</span> <span class="nx">object</span> <span class="o">|</span> <span class="kc">null</span><span class="p">;</span>

	<span class="cm">/**
	 * The error object in case a request fails.
	 */</span>
	<span class="nl">error</span><span class="p">?:</span> <span class="nx">ResponseError</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<div class="anchorHolder"><a href="#responseError" name="responseError" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="kr">interface</span> <span class="nx">ResponseError</span> <span class="p">{</span>
	<span class="cm">/**
	 * A number indicating the error type that occurred.
	 */</span>
	<span class="nl">code</span><span class="p">:</span> <span class="nx">integer</span><span class="p">;</span>

	<span class="cm">/**
	 * A string providing a short description of the error.
	 */</span>
	<span class="nl">message</span><span class="p">:</span> <span class="kr">string</span><span class="p">;</span>

	<span class="cm">/**
	 * A primitive or structured value that contains additional
	 * information about the error. Can be omitted.
	 */</span>
	<span class="nl">data</span><span class="p">?:</span> <span class="kr">string</span> <span class="o">|</span> <span class="kr">number</span> <span class="o">|</span> <span class="nx">boolean</span> <span class="o">|</span> <span class="nx">array</span> <span class="o">|</span> <span class="nx">object</span> <span class="o">|</span> <span class="kc">null</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<div class="anchorHolder"><a href="#errorCodes" name="errorCodes" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="k">namespace</span> <span class="nx">ErrorCodes</span> <span class="p">{</span>
	<span class="c1">// Defined by JSON-RPC</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">ParseError</span><span class="p">:</span> <span class="nx">integer</span> <span class="o">=</span> <span class="o">-</span><span class="mi">32700</span><span class="p">;</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">InvalidRequest</span><span class="p">:</span> <span class="nx">integer</span> <span class="o">=</span> <span class="o">-</span><span class="mi">32600</span><span class="p">;</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">MethodNotFound</span><span class="p">:</span> <span class="nx">integer</span> <span class="o">=</span> <span class="o">-</span><span class="mi">32601</span><span class="p">;</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">InvalidParams</span><span class="p">:</span> <span class="nx">integer</span> <span class="o">=</span> <span class="o">-</span><span class="mi">32602</span><span class="p">;</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">InternalError</span><span class="p">:</span> <span class="nx">integer</span> <span class="o">=</span> <span class="o">-</span><span class="mi">32603</span><span class="p">;</span>

	<span class="cm">/**
	 * This is the start range of JSON-RPC reserved error codes.
	 * It doesn't denote a real error code. No LSP error codes should
	 * be defined between the start and end range. For backwards
	 * compatibility the `ServerNotInitialized` and the `UnknownErrorCode`
	 * are left in the range.
	 *
	 * @since 3.16.0
	 */</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">jsonrpcReservedErrorRangeStart</span><span class="p">:</span> <span class="nx">integer</span> <span class="o">=</span> <span class="o">-</span><span class="mi">32099</span><span class="p">;</span>
	<span class="cm">/** @deprecated use jsonrpcReservedErrorRangeStart */</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">serverErrorStart</span><span class="p">:</span> <span class="nx">integer</span> <span class="o">=</span> <span class="nx">jsonrpcReservedErrorRangeStart</span><span class="p">;</span>

	<span class="cm">/**
	 * Error code indicating that a server received a notification or
	 * request before the server has received the `initialize` request.
	 */</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">ServerNotInitialized</span><span class="p">:</span> <span class="nx">integer</span> <span class="o">=</span> <span class="o">-</span><span class="mi">32002</span><span class="p">;</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">UnknownErrorCode</span><span class="p">:</span> <span class="nx">integer</span> <span class="o">=</span> <span class="o">-</span><span class="mi">32001</span><span class="p">;</span>

	<span class="cm">/**
	 * This is the end range of JSON-RPC reserved error codes.
	 * It doesn't denote a real error code.
	 *
	 * @since 3.16.0
	 */</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">jsonrpcReservedErrorRangeEnd</span> <span class="o">=</span> <span class="o">-</span><span class="mi">32000</span><span class="p">;</span>
	<span class="cm">/** @deprecated use jsonrpcReservedErrorRangeEnd */</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">serverErrorEnd</span><span class="p">:</span> <span class="nx">integer</span> <span class="o">=</span> <span class="nx">jsonrpcReservedErrorRangeEnd</span><span class="p">;</span>

	<span class="cm">/**
	 * This is the start range of LSP reserved error codes.
	 * It doesn't denote a real error code.
	 *
	 * @since 3.16.0
	 */</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">lspReservedErrorRangeStart</span><span class="p">:</span> <span class="nx">integer</span> <span class="o">=</span> <span class="o">-</span><span class="mi">32899</span><span class="p">;</span>

	<span class="cm">/**
	 * A request failed but it was syntactically correct, e.g the
	 * method name was known and the parameters were valid. The error
	 * message should contain human readable information about why
	 * the request failed.
	 *
	 * @since 3.17.0
	 */</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">RequestFailed</span><span class="p">:</span> <span class="nx">integer</span> <span class="o">=</span> <span class="o">-</span><span class="mi">32803</span><span class="p">;</span>

	<span class="cm">/**
	 * The server cancelled the request. This error code should
	 * only be used for requests that explicitly support being
	 * server cancellable.
	 *
	 * @since 3.17.0
	 */</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">ServerCancelled</span><span class="p">:</span> <span class="nx">integer</span> <span class="o">=</span> <span class="o">-</span><span class="mi">32802</span><span class="p">;</span>

	<span class="cm">/**
	 * The server detected that the content of a document got
	 * modified outside normal conditions. A server should
	 * NOT send this error code if it detects a content change
	 * in it unprocessed messages. The result even computed
	 * on an older state might still be useful for the client.
	 *
	 * If a client decides that a result is not of any use anymore
	 * the client should cancel the request.
	 */</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">ContentModified</span><span class="p">:</span> <span class="nx">integer</span> <span class="o">=</span> <span class="o">-</span><span class="mi">32801</span><span class="p">;</span>

	<span class="cm">/**
	 * The client has canceled a request and a server as detected
	 * the cancel.
	 */</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">RequestCancelled</span><span class="p">:</span> <span class="nx">integer</span> <span class="o">=</span> <span class="o">-</span><span class="mi">32800</span><span class="p">;</span>

	<span class="cm">/**
	 * This is the end range of LSP reserved error codes.
	 * It doesn't denote a real error code.
	 *
	 * @since 3.16.0
	 */</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">lspReservedErrorRangeEnd</span><span class="p">:</span> <span class="nx">integer</span> <span class="o">=</span> <span class="o">-</span><span class="mi">32800</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>
<h4 id="-notification-message-"><a href="#notificationMessage" name="notificationMessage" class="anchor"> Notification Message </a></h4>

<p>A notification message. A processed notification message must not send a response back. They work like events.</p>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="kr">interface</span> <span class="nx">NotificationMessage</span> <span class="kd">extends</span> <span class="nx">Message</span> <span class="p">{</span>
	<span class="cm">/**
	 * The method to be invoked.
	 */</span>
	<span class="nl">method</span><span class="p">:</span> <span class="kr">string</span><span class="p">;</span>

	<span class="cm">/**
	 * The notification's params.
	 */</span>
	<span class="nl">params</span><span class="p">?:</span> <span class="nx">array</span> <span class="o">|</span> <span class="nx">object</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<h4 id="--notifications-and-requests-"><a href="#dollarRequests" name="dollarRequests" class="anchor"> $ Notifications and Requests </a></h4>

<p>Notification and requests whose methods start with ‘$/’ are messages which are protocol implementation dependent and might not be implementable in all clients or servers. For example if the server implementation uses a single threaded synchronous programming language then there is little a server can do to react to a <code class="language-plaintext highlighter-rouge">$/cancelRequest</code> notification. If a server or client receives notifications starting with ‘$/’ it is free to ignore the notification. If a server or client receives a request starting with ‘$/’ it must error the request with error code <code class="language-plaintext highlighter-rouge">MethodNotFound</code> (e.g. <code class="language-plaintext highlighter-rouge">-32601</code>).</p>

<h4 id="-cancellation-support-arrow_right-arrow_left"><a href="#cancelRequest" name="cancelRequest" class="anchor"> Cancellation Support (<img class="emoji" title=":arrow_right:" alt=":arrow_right:" src="https://github.githubassets.com/images/icons/emoji/unicode/27a1.png" height="20" width="20"> <img class="emoji" title=":arrow_left:" alt=":arrow_left:" src="https://github.githubassets.com/images/icons/emoji/unicode/2b05.png" height="20" width="20">)</a></h4>

<p>The base protocol offers support for request cancellation. To cancel a request, a notification message with the following properties is sent:</p>

<p><em>Notification</em>:</p>
<ul>
  <li>method: ‘$/cancelRequest’</li>
  <li>params: <code class="language-plaintext highlighter-rouge">CancelParams</code> defined as follows:</li>
</ul>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="kr">interface</span> <span class="nx">CancelParams</span> <span class="p">{</span>
	<span class="cm">/**
	 * The request id to cancel.
	 */</span>
	<span class="nl">id</span><span class="p">:</span> <span class="nx">integer</span> <span class="o">|</span> <span class="kr">string</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<p>A request that got canceled still needs to return from the server and send a response back. It can not be left open / hanging. This is in line with the JSON-RPC protocol that requires that every request sends a response back. In addition it allows for returning partial results on cancel. If the request returns an error response on cancellation it is advised to set the error code to <code class="language-plaintext highlighter-rouge">ErrorCodes.RequestCancelled</code>.</p>

<h4 id="-progress-support-arrow_right-arrow_left"><a href="#progress" name="progress" class="anchor"> Progress Support (<img class="emoji" title=":arrow_right:" alt=":arrow_right:" src="https://github.githubassets.com/images/icons/emoji/unicode/27a1.png" height="20" width="20"> <img class="emoji" title=":arrow_left:" alt=":arrow_left:" src="https://github.githubassets.com/images/icons/emoji/unicode/2b05.png" height="20" width="20">)</a></h4>

<blockquote>
  <p><em>Since version 3.15.0</em></p>
</blockquote>

<p>The base protocol offers also support to report progress in a generic fashion. This mechanism can be used to report any kind of progress including <a href="#workDoneProgress">work done progress</a> (usually used to report progress in the user interface using a progress bar) and partial result progress to support streaming of results.</p>

<p>A progress notification has the following properties:</p>

<p><em>Notification</em>:</p>
<ul>
  <li>method: ‘$/progress’</li>
  <li>params: <code class="language-plaintext highlighter-rouge">ProgressParams</code> defined as follows:</li>
</ul>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="kd">type</span> <span class="nx">ProgressToken</span> <span class="o">=</span> <span class="nx">integer</span> <span class="o">|</span> <span class="kr">string</span><span class="p">;</span>
</code></pre></div></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="kr">interface</span> <span class="nx">ProgressParams</span><span class="o">&lt;</span><span class="nx">T</span><span class="o">&gt;</span> <span class="p">{</span>
	<span class="cm">/**
	 * The progress token provided by the client or server.
	 */</span>
	<span class="na">token</span><span class="p">:</span> <span class="nx">ProgressToken</span><span class="p">;</span>

	<span class="cm">/**
	 * The progress data.
	 */</span>
	<span class="nl">value</span><span class="p">:</span> <span class="nx">T</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<p>Progress is reported against a token. The token is different than the request ID which allows to report progress out of band and also for notification.</p>

<h2 id="-language-server-protocol-"><a href="#languageServerProtocol" name="languageServerProtocol" class="anchor"> Language Server Protocol </a></h2>

<p>The language server protocol defines a set of JSON-RPC request, response and notification messages which are exchanged using the above base protocol. This section starts describing the basic JSON structures used in the protocol. The document uses TypeScript interfaces in strict mode to describe these. This means for example that a <code class="language-plaintext highlighter-rouge">null</code> value has to be explicitly listed and that a mandatory property must be listed even if a falsify value might exist. Based on the basic JSON structures, the actual requests with their responses and the notifications are described.</p>

<p>An example would be a request send from the client to the server to request a hover value for a symbol at a certain position in a text document. The request’s method would be <code class="language-plaintext highlighter-rouge">textDocument/hover</code> with a parameter like this:</p>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="kr">interface</span> <span class="nx">HoverParams</span> <span class="p">{</span>
	<span class="nl">textDocument</span><span class="p">:</span> <span class="kr">string</span><span class="p">;</span> <span class="cm">/** The text document's URI in string form */</span>
	<span class="nl">position</span><span class="p">:</span> <span class="p">{</span> <span class="na">line</span><span class="p">:</span> <span class="nx">uinteger</span><span class="p">;</span> <span class="nl">character</span><span class="p">:</span> <span class="nx">uinteger</span><span class="p">;</span> <span class="p">};</span>
<span class="p">}</span>
</code></pre></div></div>

<p>The result of the request would be the hover to be presented. In its simple form it can be a string. So the result looks like this:</p>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="kr">interface</span> <span class="nx">HoverResult</span> <span class="p">{</span>
	<span class="nl">value</span><span class="p">:</span> <span class="kr">string</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<p>Please also note that a response return value of <code class="language-plaintext highlighter-rouge">null</code> indicates no result. It doesn’t tell the client to resend the request.</p>

<p>In general, the language server protocol supports JSON-RPC messages, however the base protocol defined here uses a convention such that the parameters passed to request/notification messages should be of <code class="language-plaintext highlighter-rouge">object</code> type (if passed at all). However, this does not disallow using <code class="language-plaintext highlighter-rouge">Array</code> parameter types in custom messages.</p>

<p>The protocol currently assumes that one server serves one tool. There is currently no support in the protocol to share one server between different tools. Such a sharing would require additional protocol e.g. to lock a document to support concurrent editing.</p>

<h3 id="-capabilities-"><a href="#capabilities" name="capabilities" class="anchor"> Capabilities </a></h3>

<p>Not every language server can support all features defined by the protocol. LSP therefore provides ‘capabilities’. A capability groups a set of language features. A development tool and the language server announce their supported features using capabilities. As an example, a server announces that it can handle the <code class="language-plaintext highlighter-rouge">textDocument/hover</code> request, but it might not handle the <code class="language-plaintext highlighter-rouge">workspace/symbol</code> request. Similarly, a development tool announces its ability to provide <code class="language-plaintext highlighter-rouge">about to save</code> notifications before a document is saved, so that a server can compute textual edits to format the edited document before it is saved.</p>

<p>The set of capabilities is exchanged between the client and server during the <a href="#initialize">initialize</a> request.</p>

<h3 id="-request-notification-and-response-ordering-"><a href="#messageOrdering" name="messageOrdering" class="anchor"> Request, Notification and Response Ordering </a></h3>

<p>Responses to requests should be sent in roughly the same order as the requests appear on the server or client side. So for example if a server receives a <code class="language-plaintext highlighter-rouge">textDocument/completion</code> request and then a <code class="language-plaintext highlighter-rouge">textDocument/signatureHelp</code> request it will usually first return the response for the <code class="language-plaintext highlighter-rouge">textDocument/completion</code> and then the response for <code class="language-plaintext highlighter-rouge">textDocument/signatureHelp</code>.</p>

<p>However, the server may decide to use a parallel execution strategy and may wish to return responses in a different order than the requests were received. The server may do so as long as this reordering doesn’t affect the correctness of the responses. For example, reordering the result of <code class="language-plaintext highlighter-rouge">textDocument/completion</code> and <code class="language-plaintext highlighter-rouge">textDocument/signatureHelp</code> is allowed, as each of these requests usually won’t affect the output of the other. On the other hand, the server most likely should not reorder <code class="language-plaintext highlighter-rouge">textDocument/definition</code> and <code class="language-plaintext highlighter-rouge">textDocument/rename</code> requests, since executing the latter may affect the result of the former.</p>

<h3 id="-message-documentation-"><a href="#messageDocumentation" name="messageDocumentation" class="anchor"> Message Documentation </a></h3>

<p>As said LSP defines a set of requests, responses and notifications. Each of those are documented using the following format:</p>

<ul>
  <li>a header describing the request</li>
  <li>an optional <em>Client capability</em> section describing the client capability of the request. This includes the client capabilities property path and JSON structure.</li>
  <li>an optional <em>Server Capability</em> section describing the server capability of the request. This includes the server capabilities property path and JSON structure. Clients should ignore server capabilities they don’t understand (e.g. the initialize request shouldn’t fail in this case).</li>
  <li>an optional <em>Registration Options</em> section describing the registration option if the request or notification supports dynamic capability registration. See the <a href="#client_registerCapability">register</a> and <a href="#client_unregisterCapability">unregister</a> request for how this works in detail.</li>
  <li>a <em>Request</em> section describing the format of the request sent. The method is a string identifying the request, the params are documented using a TypeScript interface. It is also documented whether the request supports work done progress and partial result progress.</li>
  <li>a <em>Response</em> section describing the format of the response. The result item describes the returned data in case of a success. The optional partial result item describes the returned data of a partial result notification. The error.data describes the returned data in case of an error. Please remember that in case of a failure the response already contains an error.code and an error.message field. These fields are only specified if the protocol forces the use of certain error codes or messages. In cases where the server can decide on these values freely they aren’t listed here.</li>
</ul>

<h3 id="-basic-json-structures-"><a href="#basicJsonStructures" name="basicJsonStructures" class="anchor"> Basic JSON Structures </a></h3>

<p>There are quite some JSON structures that are shared between different requests and notifications. Their structure and capabilities are documented in this section.</p>

<h4 id="-uri-"><a href="#uri" name="uri" class="anchor"> URI </a></h4>

<p>URI’s are transferred as strings. The URI’s format is defined in <a href="https://tools.ietf.org/html/rfc3986">https://tools.ietf.org/html/rfc3986</a></p>

<div class="language-plaintext highlighter-rouge"><div class="highlight"><pre class="highlight"><code>  foo://example.com:8042/over/there?name=ferret#nose
  \_/   \______________/\_________/ \_________/ \__/
   |           |            |            |        |
scheme     authority       path        query   fragment
   |   _____________________|__
  / \ /                        \
  urn:example:animal:ferret:nose
</code></pre></div></div>

<p>We also maintain a node module to parse a string into <code class="language-plaintext highlighter-rouge">scheme</code>, <code class="language-plaintext highlighter-rouge">authority</code>, <code class="language-plaintext highlighter-rouge">path</code>, <code class="language-plaintext highlighter-rouge">query</code>, and <code class="language-plaintext highlighter-rouge">fragment</code> URI components. The GitHub repository is <a href="https://github.com/Microsoft/vscode-uri">https://github.com/Microsoft/vscode-uri</a> the npm module is <a href="https://www.npmjs.com/package/vscode-uri">https://www.npmjs.com/package/vscode-uri</a>.</p>

<p>Many of the interfaces contain fields that correspond to the URI of a document. For clarity, the type of such a field is declared as a <code class="language-plaintext highlighter-rouge">DocumentUri</code>. Over the wire, it will still be transferred as a string, but this guarantees that the contents of that string can be parsed as a valid URI.</p>

<p>Care should be taken to handle encoding in URIs. For example, some clients (such as VS Code) may encode colons in drive letters while others do not. The URIs below are both valid, but clients and servers should be consistent with the form they use themselves to ensure the other party doesn’t interpret them as distinct URIs. Clients and servers should not assume that each other are encoding the same way (for example a client encoding colons in drive letters cannot assume server responses will have encoded colons). The same applies to casing of drive letters - one party should not assume the other party will return paths with drive letters cased the same as it.</p>

<div class="language-plaintext highlighter-rouge"><div class="highlight"><pre class="highlight"><code>file:///c:/project/readme.md
file:///C%3A/project/readme.md
</code></pre></div></div>

<div class="anchorHolder"><a href="#documentUri" name="documentUri" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="kd">type</span> <span class="nx">DocumentUri</span> <span class="o">=</span> <span class="kr">string</span><span class="p">;</span>
</code></pre></div></div>

<p>There is also a tagging interface for normal non document URIs. It maps to a <code class="language-plaintext highlighter-rouge">string</code> as well.</p>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="kd">type</span> <span class="nx">URI</span> <span class="o">=</span> <span class="kr">string</span><span class="p">;</span>
</code></pre></div></div>
<h4 id="-regular-expressions-"><a href="#regExp" name="regExp" class="anchor"> Regular Expressions </a></h4>

<p>Regular expression are a powerful tool and there are actual use cases for them in the language server protocol. However the downside with them is that almost every programming language has its own set of regular expression features so the specification can not simply refer to them as a regular expression. So the LSP uses a two step approach to support regular expressions:</p>

<ul>
  <li>the client will announce which regular expression engine it will use. This will allow server that are written for a very specific client make full use of the regular expression capabilities of the client</li>
  <li>the specification will define a set of regular expression features that should be supported by a client. Instead of writing a new specification LSP will refer to the <a href="https://tc39.es/ecma262/#sec-regexp-regular-expression-objects">ECMAScript Regular Expression specification</a> and remove features from it that are not necessary in the context of LSP or hard to implement for other clients.</li>
</ul>

<p><em>Client Capability</em>:</p>

<p>The following client capability is used to announce a client’s regular expression engine</p>

<ul>
  <li>property path (optional): <code class="language-plaintext highlighter-rouge">general.regularExpressions</code>
</li>
  <li>property type: <code class="language-plaintext highlighter-rouge">RegularExpressionsClientCapabilities</code> defined as follows:</li>
</ul>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * Client capabilities specific to regular expressions.
 */</span>
<span class="k">export</span> <span class="kr">interface</span> <span class="nx">RegularExpressionsClientCapabilities</span> <span class="p">{</span>
	<span class="cm">/**
	 * The engine's name.
	 */</span>
	<span class="nl">engine</span><span class="p">:</span> <span class="kr">string</span><span class="p">;</span>

	<span class="cm">/**
	 * The engine's version.
	 */</span>
	<span class="nl">version</span><span class="p">?:</span> <span class="kr">string</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<p>The following table lists the well known engine values. Please note that the table should be driven by the community which integrates LSP into existing clients. It is not the goal of the spec to list all available regular expression engines.</p>

<table>
  <thead>
    <tr>
      <th>Engine</th>
      <th>Version</th>
      <th>Documentation</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td>ECMAScript</td>
      <td><code class="language-plaintext highlighter-rouge">ES2020</code></td>
      <td>
<a href="https://tc39.es/ecma262/#sec-regexp-regular-expression-objects">ECMAScript 2020</a> &amp; <a href="https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions">MDN</a>
</td>
    </tr>
  </tbody>
</table>

<p><em>Regular Expression Subset</em>:</p>

<p>The following features from the <a href="https://tc39.es/ecma262/#sec-regexp-regular-expression-objects">ECMAScript 2020</a> regular expression specification are NOT mandatory for a client:</p>

<ul>
  <li>
<em>Assertions</em>: Lookahead assertion, Negative lookahead assertion, lookbehind assertion, negative lookbehind assertion.</li>
  <li>
<em>Character classes</em>: matching control characters using caret notation (e.g. <code class="language-plaintext highlighter-rouge">\cX</code>) and matching UTF-16 code units (e.g. <code class="language-plaintext highlighter-rouge">\uhhhh</code>).</li>
  <li>
<em>Group and ranges</em>: named capturing groups.</li>
  <li>
<em>Unicode property escapes</em>: none of the features needs to be supported.</li>
</ul>

<p>The only regular expression flag that a client needs to support is ‘i’ to specify a case insensitive search.</p>

<h4 id="-enumerations-"><a href="#enumerations" name="enumerations" class="anchor"> Enumerations </a></h4>

<p>The protocol supports two kind of enumerations: (a) integer based enumerations and (b) strings based enumerations. Integer based enumerations usually start with <code class="language-plaintext highlighter-rouge">1</code>. The ones that don’t are historical and they were kept to stay backwards compatible. If appropriate the value set of an enumeration is announced by the defining side (e.g. client or server) and transmitted to the other side during the initialize handshake. An example is the <code class="language-plaintext highlighter-rouge">CompletionItemKind</code> enumeration. It is announced by the client using the <code class="language-plaintext highlighter-rouge">textDocument.completion.completionItemKind</code> client property.</p>

<p>To support the evolution of enumerations the using side of an enumeration shouldn’t fail on an enumeration value it doesn’t know. It should simply ignore it as a value it can use and try to do its best to preserve the value on round trips. Lets look at the <code class="language-plaintext highlighter-rouge">CompletionItemKind</code> enumeration as an example again: if in a future version of the specification an additional completion item kind with the value <code class="language-plaintext highlighter-rouge">n</code> gets added and announced by a client a (older) server not knowing about the value should not fail but simply ignore the value as a usable item kind.</p>

<h4 id="-text-documents-"><a href="#textDocuments" name="textDocuments" class="anchor"> Text Documents </a></h4>

<p>The current protocol is tailored for textual documents whose content can be represented as a string. There is currently no support for binary documents. A position inside a document (see Position definition below) is expressed as a zero-based line and character offset.</p>

<blockquote>
  <p>New in 3.17</p>
</blockquote>

<p>Prior to 3.17 the offsets were always based on a UTF-16 string representation. So in a string of the form <code class="language-plaintext highlighter-rouge">a𐐀b</code> the character offset of the character <code class="language-plaintext highlighter-rouge">a</code> is 0, the character offset of <code class="language-plaintext highlighter-rouge">𐐀</code> is 1 and the character offset of b is 3 since <code class="language-plaintext highlighter-rouge">𐐀</code> is represented using two code units in UTF-16. Since 3.17 clients and servers can agree on a different string encoding representation (e.g. UTF-8). The client announces it’s supported encoding via the client capability <a href="#clientCapabilities"><code class="language-plaintext highlighter-rouge">general.positionEncodings</code></a>. The value is an array of position encodings the client supports, with decreasing preference (e.g. the encoding at index <code class="language-plaintext highlighter-rouge">0</code> is the most preferred one). To stay backwards compatible the only mandatory encoding is UTF-16 represented via the string <code class="language-plaintext highlighter-rouge">utf-16</code>. The server can pick one of the encodings offered by the client and signals that encoding back to the client via the initialize result’s property <a href="#serverCapabilities"><code class="language-plaintext highlighter-rouge">capabilities.positionEncoding</code></a>. If the string value <code class="language-plaintext highlighter-rouge">utf-16</code> is missing from the client’s capability <code class="language-plaintext highlighter-rouge">general.positionEncodings</code> servers can safely assume that the client supports UTF-16. If the server omits the position encoding in its initialize result the encoding defaults to the string value <code class="language-plaintext highlighter-rouge">utf-16</code>. Implementation considerations: since the conversion from one encoding into another requires the content of the file / line the conversion is best done where the file is read which is usually on the server side.</p>

<p>To ensure that both client and server split the string into the same line representation the protocol specifies the following end-of-line sequences: ‘\n’, ‘\r\n’ and ‘\r’. Positions are line end character agnostic. So you can not specify a position that denotes <code class="language-plaintext highlighter-rouge">\r|\n</code> or <code class="language-plaintext highlighter-rouge">\n|</code> where <code class="language-plaintext highlighter-rouge">|</code> represents the character offset.</p>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kd">const</span> <span class="nx">EOL</span><span class="p">:</span> <span class="kr">string</span><span class="p">[]</span> <span class="o">=</span> <span class="p">[</span><span class="dl">'</span><span class="se">\n</span><span class="dl">'</span><span class="p">,</span> <span class="dl">'</span><span class="se">\r\n</span><span class="dl">'</span><span class="p">,</span> <span class="dl">'</span><span class="se">\r</span><span class="dl">'</span><span class="p">];</span>
</code></pre></div></div>

<h4 id="-position-"><a href="#position" name="position" class="anchor"> Position </a></h4>

<p>Position in a text document expressed as zero-based line and zero-based character offset. A position is between two characters like an ‘insert’ cursor in an editor. Special values like for example <code class="language-plaintext highlighter-rouge">-1</code> to denote the end of a line are not supported.</p>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="kr">interface</span> <span class="nx">Position</span> <span class="p">{</span>
	<span class="cm">/**
	 * Line position in a document (zero-based).
	 */</span>
	<span class="nl">line</span><span class="p">:</span> <span class="nx">uinteger</span><span class="p">;</span>

	<span class="cm">/**
	 * Character offset on a line in a document (zero-based). The meaning of this
	 * offset is determined by the negotiated `PositionEncodingKind`.
	 *
	 * If the character value is greater than the line length it defaults back
	 * to the line length.
	 */</span>
	<span class="nl">character</span><span class="p">:</span> <span class="nx">uinteger</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<p>When describing positions the protocol needs to specify how offsets (specifically character offsets) should be interpreted.
The corresponding <code class="language-plaintext highlighter-rouge">PositionEncodingKind</code> is negotiated between the client and the server during initialization.</p>

<div class="anchorHolder"><a href="#positionEncodingKind" name="positionEncodingKind" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * A type indicating how positions are encoded,
 * specifically what column offsets mean.
 *
 * @since 3.17.0
 */</span>
<span class="k">export</span> <span class="kd">type</span> <span class="nx">PositionEncodingKind</span> <span class="o">=</span> <span class="kr">string</span><span class="p">;</span>

<span class="cm">/**
 * A set of predefined position encoding kinds.
 *
 * @since 3.17.0
 */</span>
<span class="k">export</span> <span class="k">namespace</span> <span class="nx">PositionEncodingKind</span> <span class="p">{</span>

	<span class="cm">/**
	 * Character offsets count UTF-8 code units (e.g bytes).
	 */</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">UTF8</span><span class="p">:</span> <span class="nx">PositionEncodingKind</span> <span class="o">=</span> <span class="dl">'</span><span class="s1">utf-8</span><span class="dl">'</span><span class="p">;</span>

	<span class="cm">/**
	 * Character offsets count UTF-16 code units.
	 *
	 * This is the default and must always be supported
	 * by servers
	 */</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">UTF16</span><span class="p">:</span> <span class="nx">PositionEncodingKind</span> <span class="o">=</span> <span class="dl">'</span><span class="s1">utf-16</span><span class="dl">'</span><span class="p">;</span>

	<span class="cm">/**
	 * Character offsets count UTF-32 code units.
	 *
	 * Implementation note: these are the same as Unicode code points,
	 * so this `PositionEncodingKind` may also be used for an
	 * encoding-agnostic representation of character offsets.
	 */</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">UTF32</span><span class="p">:</span> <span class="nx">PositionEncodingKind</span> <span class="o">=</span> <span class="dl">'</span><span class="s1">utf-32</span><span class="dl">'</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<h4 id="-range-"><a href="#range" name="range" class="anchor"> Range </a></h4>

<p>A range in a text document expressed as (zero-based) start and end positions. A range is comparable to a selection in an editor. Therefore, the end position is exclusive. If you want to specify a range that contains a line including the line ending character(s) then use an end position denoting the start of the next line. For example:</p>
<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="p">{</span>
    <span class="nl">start</span><span class="p">:</span> <span class="p">{</span> <span class="na">line</span><span class="p">:</span> <span class="mi">5</span><span class="p">,</span> <span class="na">character</span><span class="p">:</span> <span class="mi">23</span> <span class="p">},</span>
    <span class="nx">end</span> <span class="p">:</span> <span class="p">{</span> <span class="nl">line</span><span class="p">:</span> <span class="mi">6</span><span class="p">,</span> <span class="nx">character</span><span class="p">:</span> <span class="mi">0</span> <span class="p">}</span>
<span class="p">}</span>
</code></pre></div></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="kr">interface</span> <span class="nx">Range</span> <span class="p">{</span>
	<span class="cm">/**
	 * The range's start position.
	 */</span>
	<span class="nl">start</span><span class="p">:</span> <span class="nx">Position</span><span class="p">;</span>

	<span class="cm">/**
	 * The range's end position.
	 */</span>
	<span class="nl">end</span><span class="p">:</span> <span class="nx">Position</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<h4 id="-textdocumentitem-"><a href="#textDocumentItem" name="textDocumentItem" class="anchor"> TextDocumentItem </a></h4>

<p>An item to transfer a text document from the client to the server.</p>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="kr">interface</span> <span class="nx">TextDocumentItem</span> <span class="p">{</span>
	<span class="cm">/**
	 * The text document's URI.
	 */</span>
	<span class="nl">uri</span><span class="p">:</span> <span class="nx">DocumentUri</span><span class="p">;</span>

	<span class="cm">/**
	 * The text document's language identifier.
	 */</span>
	<span class="nl">languageId</span><span class="p">:</span> <span class="kr">string</span><span class="p">;</span>

	<span class="cm">/**
	 * The version number of this document (it will increase after each
	 * change, including undo/redo).
	 */</span>
	<span class="nl">version</span><span class="p">:</span> <span class="nx">integer</span><span class="p">;</span>

	<span class="cm">/**
	 * The content of the opened text document.
	 */</span>
	<span class="nl">text</span><span class="p">:</span> <span class="kr">string</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<p>Text documents have a language identifier to identify a document on the server side when it handles more than one language to avoid re-interpreting the file extension. If a document refers to one of the programming languages listed below it is recommended that clients use those ids.</p>

<table class="table table-bordered table-responsive">
  <thead>
    <tr>
      <th>Language</th>
      <th>Identifier</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td>ABAP</td>
      <td><code class="language-plaintext highlighter-rouge">abap</code></td>
    </tr>
    <tr>
      <td>Windows Bat</td>
      <td><code class="language-plaintext highlighter-rouge">bat</code></td>
    </tr>
    <tr>
      <td>BibTeX</td>
      <td><code class="language-plaintext highlighter-rouge">bibtex</code></td>
    </tr>
    <tr>
      <td>Clojure</td>
      <td><code class="language-plaintext highlighter-rouge">clojure</code></td>
    </tr>
    <tr>
      <td>Coffeescript</td>
      <td><code class="language-plaintext highlighter-rouge">coffeescript</code></td>
    </tr>
    <tr>
      <td>C</td>
      <td><code class="language-plaintext highlighter-rouge">c</code></td>
    </tr>
    <tr>
      <td>C++</td>
      <td><code class="language-plaintext highlighter-rouge">cpp</code></td>
    </tr>
    <tr>
      <td>C#</td>
      <td><code class="language-plaintext highlighter-rouge">csharp</code></td>
    </tr>
    <tr>
      <td>CSS</td>
      <td><code class="language-plaintext highlighter-rouge">css</code></td>
    </tr>
    <tr>
      <td>Diff</td>
      <td><code class="language-plaintext highlighter-rouge">diff</code></td>
    </tr>
    <tr>
      <td>Dart</td>
      <td><code class="language-plaintext highlighter-rouge">dart</code></td>
    </tr>
    <tr>
      <td>Dockerfile</td>
      <td><code class="language-plaintext highlighter-rouge">dockerfile</code></td>
    </tr>
    <tr>
      <td>Elixir</td>
      <td><code class="language-plaintext highlighter-rouge">elixir</code></td>
    </tr>
    <tr>
      <td>Erlang</td>
      <td><code class="language-plaintext highlighter-rouge">erlang</code></td>
    </tr>
    <tr>
      <td>F#</td>
      <td><code class="language-plaintext highlighter-rouge">fsharp</code></td>
    </tr>
    <tr>
      <td>Git</td>
      <td>
<code class="language-plaintext highlighter-rouge">git-commit</code> and <code class="language-plaintext highlighter-rouge">git-rebase</code>
</td>
    </tr>
    <tr>
      <td>Go</td>
      <td><code class="language-plaintext highlighter-rouge">go</code></td>
    </tr>
    <tr>
      <td>Groovy</td>
      <td><code class="language-plaintext highlighter-rouge">groovy</code></td>
    </tr>
    <tr>
      <td>Handlebars</td>
      <td><code class="language-plaintext highlighter-rouge">handlebars</code></td>
    </tr>
    <tr>
      <td>HTML</td>
      <td><code class="language-plaintext highlighter-rouge">html</code></td>
    </tr>
    <tr>
      <td>Ini</td>
      <td><code class="language-plaintext highlighter-rouge">ini</code></td>
    </tr>
    <tr>
      <td>Java</td>
      <td><code class="language-plaintext highlighter-rouge">java</code></td>
    </tr>
    <tr>
      <td>JavaScript</td>
      <td><code class="language-plaintext highlighter-rouge">javascript</code></td>
    </tr>
    <tr>
      <td>JavaScript React</td>
      <td><code class="language-plaintext highlighter-rouge">javascriptreact</code></td>
    </tr>
    <tr>
      <td>JSON</td>
      <td><code class="language-plaintext highlighter-rouge">json</code></td>
    </tr>
    <tr>
      <td>LaTeX</td>
      <td><code class="language-plaintext highlighter-rouge">latex</code></td>
    </tr>
    <tr>
      <td>Less</td>
      <td><code class="language-plaintext highlighter-rouge">less</code></td>
    </tr>
    <tr>
      <td>Lua</td>
      <td><code class="language-plaintext highlighter-rouge">lua</code></td>
    </tr>
    <tr>
      <td>Makefile</td>
      <td><code class="language-plaintext highlighter-rouge">makefile</code></td>
    </tr>
    <tr>
      <td>Markdown</td>
      <td><code class="language-plaintext highlighter-rouge">markdown</code></td>
    </tr>
    <tr>
      <td>Objective-C</td>
      <td><code class="language-plaintext highlighter-rouge">objective-c</code></td>
    </tr>
    <tr>
      <td>Objective-C++</td>
      <td><code class="language-plaintext highlighter-rouge">objective-cpp</code></td>
    </tr>
    <tr>
      <td>Perl</td>
      <td><code class="language-plaintext highlighter-rouge">perl</code></td>
    </tr>
    <tr>
      <td>Perl 6</td>
      <td><code class="language-plaintext highlighter-rouge">perl6</code></td>
    </tr>
    <tr>
      <td>PHP</td>
      <td><code class="language-plaintext highlighter-rouge">php</code></td>
    </tr>
    <tr>
      <td>Powershell</td>
      <td><code class="language-plaintext highlighter-rouge">powershell</code></td>
    </tr>
    <tr>
      <td>Pug</td>
      <td><code class="language-plaintext highlighter-rouge">jade</code></td>
    </tr>
    <tr>
      <td>Python</td>
      <td><code class="language-plaintext highlighter-rouge">python</code></td>
    </tr>
    <tr>
      <td>R</td>
      <td><code class="language-plaintext highlighter-rouge">r</code></td>
    </tr>
    <tr>
      <td>Razor (cshtml)</td>
      <td><code class="language-plaintext highlighter-rouge">razor</code></td>
    </tr>
    <tr>
      <td>Ruby</td>
      <td><code class="language-plaintext highlighter-rouge">ruby</code></td>
    </tr>
    <tr>
      <td>Rust</td>
      <td><code class="language-plaintext highlighter-rouge">rust</code></td>
    </tr>
    <tr>
      <td>SCSS</td>
      <td>
<code class="language-plaintext highlighter-rouge">scss</code> (syntax using curly brackets), <code class="language-plaintext highlighter-rouge">sass</code> (indented syntax)</td>
    </tr>
    <tr>
      <td>Scala</td>
      <td><code class="language-plaintext highlighter-rouge">scala</code></td>
    </tr>
    <tr>
      <td>ShaderLab</td>
      <td><code class="language-plaintext highlighter-rouge">shaderlab</code></td>
    </tr>
    <tr>
      <td>Shell Script (Bash)</td>
      <td><code class="language-plaintext highlighter-rouge">shellscript</code></td>
    </tr>
    <tr>
      <td>SQL</td>
      <td><code class="language-plaintext highlighter-rouge">sql</code></td>
    </tr>
    <tr>
      <td>Swift</td>
      <td><code class="language-plaintext highlighter-rouge">swift</code></td>
    </tr>
    <tr>
      <td>TypeScript</td>
      <td><code class="language-plaintext highlighter-rouge">typescript</code></td>
    </tr>
    <tr>
      <td>TypeScript React</td>
      <td><code class="language-plaintext highlighter-rouge">typescriptreact</code></td>
    </tr>
    <tr>
      <td>TeX</td>
      <td><code class="language-plaintext highlighter-rouge">tex</code></td>
    </tr>
    <tr>
      <td>Visual Basic</td>
      <td><code class="language-plaintext highlighter-rouge">vb</code></td>
    </tr>
    <tr>
      <td>XML</td>
      <td><code class="language-plaintext highlighter-rouge">xml</code></td>
    </tr>
    <tr>
      <td>XSL</td>
      <td><code class="language-plaintext highlighter-rouge">xsl</code></td>
    </tr>
    <tr>
      <td>YAML</td>
      <td><code class="language-plaintext highlighter-rouge">yaml</code></td>
    </tr>
  </tbody>
</table>

<h4 id="-textdocumentidentifier-"><a href="#textDocumentIdentifier" name="textDocumentIdentifier" class="anchor"> TextDocumentIdentifier </a></h4>

<p>Text documents are identified using a URI. On the protocol level, URIs are passed as strings. The corresponding JSON structure looks like this:</p>
<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="kr">interface</span> <span class="nx">TextDocumentIdentifier</span> <span class="p">{</span>
	<span class="cm">/**
	 * The text document's URI.
	 */</span>
	<span class="nl">uri</span><span class="p">:</span> <span class="nx">DocumentUri</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>
<h4 id="-versionedtextdocumentidentifier-"><a href="#versionedTextDocumentIdentifier" name="versionedTextDocumentIdentifier" class="anchor"> VersionedTextDocumentIdentifier </a></h4>

<p>An identifier to denote a specific version of a text document. This information usually flows from the client to the server.</p>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="kr">interface</span> <span class="nx">VersionedTextDocumentIdentifier</span> <span class="kd">extends</span> <span class="nx">TextDocumentIdentifier</span> <span class="p">{</span>
	<span class="cm">/**
	 * The version number of this document.
	 *
	 * The version number of a document will increase after each change,
	 * including undo/redo. The number doesn't need to be consecutive.
	 */</span>
	<span class="nl">version</span><span class="p">:</span> <span class="nx">integer</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<p>An identifier which optionally denotes a specific version of a text document. This information usually flows from the server to the client.</p>

<div class="anchorHolder"><a href="#optionalVersionedTextDocumentIdentifier" name="optionalVersionedTextDocumentIdentifier" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="kr">interface</span> <span class="nx">OptionalVersionedTextDocumentIdentifier</span> <span class="kd">extends</span> <span class="nx">TextDocumentIdentifier</span> <span class="p">{</span>
	<span class="cm">/**
	 * The version number of this document. If an optional versioned text document
	 * identifier is sent from the server to the client and the file is not
	 * open in the editor (the server has not received an open notification
	 * before) the server can send `null` to indicate that the version is
	 * known and the content on disk is the master (as specified with document
	 * content ownership).
	 *
	 * The version number of a document will increase after each change,
	 * including undo/redo. The number doesn't need to be consecutive.
	 */</span>
	<span class="nl">version</span><span class="p">:</span> <span class="nx">integer</span> <span class="o">|</span> <span class="kc">null</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>
<h4 id="-textdocumentpositionparams-"><a href="#textDocumentPositionParams" name="textDocumentPositionParams" class="anchor"> TextDocumentPositionParams </a></h4>

<p>Was <code class="language-plaintext highlighter-rouge">TextDocumentPosition</code> in 1.0 with inlined parameters.</p>

<p>A parameter literal used in requests to pass a text document and a position inside that document. It is up to the client to decide how a selection is converted into a position when issuing a request for a text document. The client can for example honor or ignore the selection direction to make LSP request consistent with features implemented internally.</p>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="kr">interface</span> <span class="nx">TextDocumentPositionParams</span> <span class="p">{</span>
	<span class="cm">/**
	 * The text document.
	 */</span>
	<span class="nl">textDocument</span><span class="p">:</span> <span class="nx">TextDocumentIdentifier</span><span class="p">;</span>

	<span class="cm">/**
	 * The position inside the text document.
	 */</span>
	<span class="nl">position</span><span class="p">:</span> <span class="nx">Position</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>
<h4 id="-documentfilter-"><a href="#documentFilter" name="documentFilter" class="anchor"> DocumentFilter </a></h4>

<p>A document filter denotes a document through properties like <code class="language-plaintext highlighter-rouge">language</code>, <code class="language-plaintext highlighter-rouge">scheme</code> or <code class="language-plaintext highlighter-rouge">pattern</code>. An example is a filter that applies to TypeScript files on disk. Another example is a filter that applies to JSON files with name <code class="language-plaintext highlighter-rouge">package.json</code>:</p>
<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="p">{</span> <span class="nl">language</span><span class="p">:</span> <span class="dl">'</span><span class="s1">typescript</span><span class="dl">'</span><span class="p">,</span> <span class="nx">scheme</span><span class="p">:</span> <span class="dl">'</span><span class="s1">file</span><span class="dl">'</span> <span class="p">}</span>
<span class="p">{</span> <span class="na">language</span><span class="p">:</span> <span class="dl">'</span><span class="s1">json</span><span class="dl">'</span><span class="p">,</span> <span class="na">pattern</span><span class="p">:</span> <span class="dl">'</span><span class="s1">**/package.json</span><span class="dl">'</span> <span class="p">}</span>
</code></pre></div></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">DocumentFilter</span> <span class="p">{</span>
	<span class="cm">/**
	 * A language id, like `typescript`.
	 */</span>
	<span class="nl">language</span><span class="p">?:</span> <span class="kr">string</span><span class="p">;</span>

	<span class="cm">/**
	 * A Uri [scheme](#Uri.scheme), like `file` or `untitled`.
	 */</span>
	<span class="nl">scheme</span><span class="p">?:</span> <span class="kr">string</span><span class="p">;</span>

	<span class="cm">/**
	 * A glob pattern, like `*.{ts,js}`.
	 *
	 * Glob patterns can have the following syntax:
	 * - `*` to match one or more characters in a path segment
	 * - `?` to match on one character in a path segment
	 * - `**` to match any number of path segments, including none
	 * - `{}` to group sub patterns into an OR expression. (e.g. `**​/*.{ts,js}`
	 *   matches all TypeScript and JavaScript files)
	 * - `[]` to declare a range of characters to match in a path segment
	 *   (e.g., `example.[0-9]` to match on `example.0`, `example.1`, …)
	 * - `[!...]` to negate a range of characters to match in a path segment
	 *   (e.g., `example.[!0-9]` to match on `example.a`, `example.b`, but
	 *   not `example.0`)
	 */</span>
	<span class="nl">pattern</span><span class="p">?:</span> <span class="kr">string</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<p>Please note that for a document filter to be valid at least one of the properties for <code class="language-plaintext highlighter-rouge">language</code>, <code class="language-plaintext highlighter-rouge">scheme</code>, or <code class="language-plaintext highlighter-rouge">pattern</code> must be set. To keep the type definition simple all properties are marked as optional.</p>

<p>A document selector is the combination of one or more document filters.</p>

<div class="anchorHolder"><a href="#documentSelector" name="documentSelector" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kd">type</span> <span class="nx">DocumentSelector</span> <span class="o">=</span> <span class="nx">DocumentFilter</span><span class="p">[];</span>
</code></pre></div></div>

<h4 id="-textedit--annotatedtextedit-"><a href="#textEdit" name="textEdit" class="anchor"> TextEdit &amp; AnnotatedTextEdit </a></h4>

<blockquote>
  <p>New in version 3.16: Support for <code class="language-plaintext highlighter-rouge">AnnotatedTextEdit</code>.</p>
</blockquote>

<p>A textual edit applicable to a text document.</p>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="kr">interface</span> <span class="nx">TextEdit</span> <span class="p">{</span>
	<span class="cm">/**
	 * The range of the text document to be manipulated. To insert
	 * text into a document create a range where start === end.
	 */</span>
	<span class="nl">range</span><span class="p">:</span> <span class="nx">Range</span><span class="p">;</span>

	<span class="cm">/**
	 * The string to be inserted. For delete operations use an
	 * empty string.
	 */</span>
	<span class="nl">newText</span><span class="p">:</span> <span class="kr">string</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>
<p>Since 3.16.0 there is also the concept of an annotated text edit which supports to add an annotation to a text edit. The annotation can add information describing the change to the text edit.</p>

<div class="anchorHolder"><a href="#changeAnnotation" name="changeAnnotation" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * Additional information that describes document changes.
 *
 * @since 3.16.0
 */</span>
<span class="k">export</span> <span class="kr">interface</span> <span class="nx">ChangeAnnotation</span> <span class="p">{</span>
	<span class="cm">/**
	 * A human-readable string describing the actual change. The string
	 * is rendered prominent in the user interface.
	 */</span>
	<span class="nl">label</span><span class="p">:</span> <span class="kr">string</span><span class="p">;</span>

	<span class="cm">/**
	 * A flag which indicates that user confirmation is needed
	 * before applying the change.
	 */</span>
	<span class="nl">needsConfirmation</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>

	<span class="cm">/**
	 * A human-readable string which is rendered less prominent in
	 * the user interface.
	 */</span>
	<span class="nl">description</span><span class="p">?:</span> <span class="kr">string</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<p>Usually clients provide options to group the changes along the annotations they are associated with. To support this in the protocol an edit or resource operation refers to a change annotation using an identifier and not the change annotation literal directly. This allows servers to use the identical annotation across multiple edits or resource operations which then allows clients to group the operations under that change annotation. The actual change annotations together with their identifiers are managed by the workspace edit via the new property <code class="language-plaintext highlighter-rouge">changeAnnotations</code>.</p>

<div class="anchorHolder"><a href="#changeAnnotationIdentifier" name="changeAnnotationIdentifier" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * An identifier referring to a change annotation managed by a workspace
 * edit.
 *
 * @since 3.16.0.
 */</span>
<span class="k">export</span> <span class="kd">type</span> <span class="nx">ChangeAnnotationIdentifier</span> <span class="o">=</span> <span class="kr">string</span><span class="p">;</span>
</code></pre></div></div>

<div class="anchorHolder"><a href="#annotatedTextEdit" name="annotatedTextEdit" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * A special text edit with an additional change annotation.
 *
 * @since 3.16.0.
 */</span>
<span class="k">export</span> <span class="kr">interface</span> <span class="nx">AnnotatedTextEdit</span> <span class="kd">extends</span> <span class="nx">TextEdit</span> <span class="p">{</span>
	<span class="cm">/**
	 * The actual annotation identifier.
	 */</span>
	<span class="nl">annotationId</span><span class="p">:</span> <span class="nx">ChangeAnnotationIdentifier</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>
<h4 id="-textedit-"><a href="#textEditArray" name="textEditArray" class="anchor"> TextEdit[] </a></h4>

<p>Complex text manipulations are described with an array of <code class="language-plaintext highlighter-rouge">TextEdit</code>’s or <code class="language-plaintext highlighter-rouge">AnnotatedTextEdit</code>’s, representing a single change to the document.</p>

<p>All text edits ranges refer to positions in the document they are computed on. They therefore move a document from state S1 to S2 without describing any intermediate state. Text edits ranges must never overlap, that means no part of the original document must be manipulated by more than one edit. However, it is possible that multiple edits have the same start position: multiple inserts, or any number of inserts followed by a single remove or replace edit. If multiple inserts have the same position, the order in the array defines the order in which the inserted strings appear in the resulting text.</p>

<h4 id="-textdocumentedit-"><a href="#textDocumentEdit" name="textDocumentEdit" class="anchor"> TextDocumentEdit </a></h4>

<blockquote>
  <p>New in version 3.16: support for <code class="language-plaintext highlighter-rouge">AnnotatedTextEdit</code>. The support is guarded by the client capability <code class="language-plaintext highlighter-rouge">workspace.workspaceEdit.changeAnnotationSupport</code>. If a client doesn’t signal the capability, servers shouldn’t send <code class="language-plaintext highlighter-rouge">AnnotatedTextEdit</code> literals back to the client.</p>
</blockquote>

<p>Describes textual changes on a single text document. The text document is referred to as a <code class="language-plaintext highlighter-rouge">OptionalVersionedTextDocumentIdentifier</code> to allow clients to check the text document version before an edit is applied. A <code class="language-plaintext highlighter-rouge">TextDocumentEdit</code> describes all changes on a version Si and after they are applied move the document to version Si+1. So the creator of a <code class="language-plaintext highlighter-rouge">TextDocumentEdit</code> doesn’t need to sort the array of edits or do any kind of ordering. However the edits must be non overlapping.</p>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">TextDocumentEdit</span> <span class="p">{</span>
	<span class="cm">/**
	 * The text document to change.
	 */</span>
	<span class="nl">textDocument</span><span class="p">:</span> <span class="nx">OptionalVersionedTextDocumentIdentifier</span><span class="p">;</span>

	<span class="cm">/**
	 * The edits to be applied.
	 *
	 * @since 3.16.0 - support for AnnotatedTextEdit. This is guarded by the
	 * client capability `workspace.workspaceEdit.changeAnnotationSupport`
	 */</span>
	<span class="nl">edits</span><span class="p">:</span> <span class="p">(</span><span class="nx">TextEdit</span> <span class="o">|</span> <span class="nx">AnnotatedTextEdit</span><span class="p">)[];</span>
<span class="p">}</span>
</code></pre></div></div>
<h4 id="-location-"><a href="#location" name="location" class="anchor"> Location </a></h4>

<p>Represents a location inside a resource, such as a line inside a text file.</p>
<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="kr">interface</span> <span class="nx">Location</span> <span class="p">{</span>
	<span class="nl">uri</span><span class="p">:</span> <span class="nx">DocumentUri</span><span class="p">;</span>
	<span class="nl">range</span><span class="p">:</span> <span class="nx">Range</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>
<h4 id="-locationlink-"><a href="#locationLink" name="locationLink" class="anchor"> LocationLink </a></h4>

<p>Represents a link between a source and a target location.</p>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="kr">interface</span> <span class="nx">LocationLink</span> <span class="p">{</span>

	<span class="cm">/**
	 * Span of the origin of this link.
	 *
	 * Used as the underlined span for mouse interaction. Defaults to the word
	 * range at the mouse position.
	 */</span>
	<span class="nl">originSelectionRange</span><span class="p">?:</span> <span class="nx">Range</span><span class="p">;</span>

	<span class="cm">/**
	 * The target resource identifier of this link.
	 */</span>
	<span class="nl">targetUri</span><span class="p">:</span> <span class="nx">DocumentUri</span><span class="p">;</span>

	<span class="cm">/**
	 * The full target range of this link. If the target for example is a symbol
	 * then target range is the range enclosing this symbol not including
	 * leading/trailing whitespace but everything else like comments. This
	 * information is typically used to highlight the range in the editor.
	 */</span>
	<span class="nl">targetRange</span><span class="p">:</span> <span class="nx">Range</span><span class="p">;</span>

	<span class="cm">/**
	 * The range that should be selected and revealed when this link is being
	 * followed, e.g the name of a function. Must be contained by the
	 * `targetRange`. See also `DocumentSymbol#range`
	 */</span>
	<span class="nl">targetSelectionRange</span><span class="p">:</span> <span class="nx">Range</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>
<h4 id="-diagnostic-"><a href="#diagnostic" name="diagnostic" class="anchor"> Diagnostic </a></h4>

<p>Represents a diagnostic, such as a compiler error or warning. Diagnostic objects are only valid in the scope of a resource.</p>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">Diagnostic</span> <span class="p">{</span>
	<span class="cm">/**
	 * The range at which the message applies.
	 */</span>
	<span class="nl">range</span><span class="p">:</span> <span class="nx">Range</span><span class="p">;</span>

	<span class="cm">/**
	 * The diagnostic's severity. Can be omitted. If omitted it is up to the
	 * client to interpret diagnostics as error, warning, info or hint.
	 */</span>
	<span class="nl">severity</span><span class="p">?:</span> <span class="nx">DiagnosticSeverity</span><span class="p">;</span>

	<span class="cm">/**
	 * The diagnostic's code, which might appear in the user interface.
	 */</span>
	<span class="nl">code</span><span class="p">?:</span> <span class="nx">integer</span> <span class="o">|</span> <span class="kr">string</span><span class="p">;</span>

	<span class="cm">/**
	 * An optional property to describe the error code.
	 *
	 * @since 3.16.0
	 */</span>
	<span class="nl">codeDescription</span><span class="p">?:</span> <span class="nx">CodeDescription</span><span class="p">;</span>

	<span class="cm">/**
	 * A human-readable string describing the source of this
	 * diagnostic, e.g. 'typescript' or 'super lint'.
	 */</span>
	<span class="nl">source</span><span class="p">?:</span> <span class="kr">string</span><span class="p">;</span>

	<span class="cm">/**
	 * The diagnostic's message.
	 */</span>
	<span class="nl">message</span><span class="p">:</span> <span class="kr">string</span><span class="p">;</span>

	<span class="cm">/**
	 * Additional metadata about the diagnostic.
	 *
	 * @since 3.15.0
	 */</span>
	<span class="nl">tags</span><span class="p">?:</span> <span class="nx">DiagnosticTag</span><span class="p">[];</span>

	<span class="cm">/**
	 * An array of related diagnostic information, e.g. when symbol-names within
	 * a scope collide all definitions can be marked via this property.
	 */</span>
	<span class="nl">relatedInformation</span><span class="p">?:</span> <span class="nx">DiagnosticRelatedInformation</span><span class="p">[];</span>

	<span class="cm">/**
	 * A data entry field that is preserved between a
	 * `textDocument/publishDiagnostics` notification and
	 * `textDocument/codeAction` request.
	 *
	 * @since 3.16.0
	 */</span>
	<span class="nl">data</span><span class="p">?:</span> <span class="nx">unknown</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<p>The protocol currently supports the following diagnostic severities and tags:</p>

<div class="anchorHolder"><a href="#diagnosticSeverity" name="diagnosticSeverity" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="k">namespace</span> <span class="nx">DiagnosticSeverity</span> <span class="p">{</span>
	<span class="cm">/**
	 * Reports an error.
	 */</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nb">Error</span><span class="p">:</span> <span class="mi">1</span> <span class="o">=</span> <span class="mi">1</span><span class="p">;</span>
	<span class="cm">/**
	 * Reports a warning.
	 */</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">Warning</span><span class="p">:</span> <span class="mi">2</span> <span class="o">=</span> <span class="mi">2</span><span class="p">;</span>
	<span class="cm">/**
	 * Reports an information.
	 */</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">Information</span><span class="p">:</span> <span class="mi">3</span> <span class="o">=</span> <span class="mi">3</span><span class="p">;</span>
	<span class="cm">/**
	 * Reports a hint.
	 */</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">Hint</span><span class="p">:</span> <span class="mi">4</span> <span class="o">=</span> <span class="mi">4</span><span class="p">;</span>
<span class="p">}</span>

<span class="k">export</span> <span class="kd">type</span> <span class="nx">DiagnosticSeverity</span> <span class="o">=</span> <span class="mi">1</span> <span class="o">|</span> <span class="mi">2</span> <span class="o">|</span> <span class="mi">3</span> <span class="o">|</span> <span class="mi">4</span><span class="p">;</span>
</code></pre></div></div>

<div class="anchorHolder"><a href="#diagnosticTag" name="diagnosticTag" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * The diagnostic tags.
 *
 * @since 3.15.0
 */</span>
<span class="k">export</span> <span class="k">namespace</span> <span class="nx">DiagnosticTag</span> <span class="p">{</span>
	<span class="cm">/**
	 * Unused or unnecessary code.
	 *
	 * Clients are allowed to render diagnostics with this tag faded out
	 * instead of having an error squiggle.
	 */</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">Unnecessary</span><span class="p">:</span> <span class="mi">1</span> <span class="o">=</span> <span class="mi">1</span><span class="p">;</span>
	<span class="cm">/**
	 * Deprecated or obsolete code.
	 *
	 * Clients are allowed to rendered diagnostics with this tag strike through.
	 */</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">Deprecated</span><span class="p">:</span> <span class="mi">2</span> <span class="o">=</span> <span class="mi">2</span><span class="p">;</span>
<span class="p">}</span>

<span class="k">export</span> <span class="kd">type</span> <span class="nx">DiagnosticTag</span> <span class="o">=</span> <span class="mi">1</span> <span class="o">|</span> <span class="mi">2</span><span class="p">;</span>
</code></pre></div></div>

<p><code class="language-plaintext highlighter-rouge">DiagnosticRelatedInformation</code> is defined as follows:</p>

<div class="anchorHolder"><a href="#diagnosticRelatedInformation" name="diagnosticRelatedInformation" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * Represents a related message and source code location for a diagnostic.
 * This should be used to point to code locations that cause or are related to
 * a diagnostics, e.g when duplicating a symbol in a scope.
 */</span>
<span class="k">export</span> <span class="kr">interface</span> <span class="nx">DiagnosticRelatedInformation</span> <span class="p">{</span>
	<span class="cm">/**
	 * The location of this related diagnostic information.
	 */</span>
	<span class="nl">location</span><span class="p">:</span> <span class="nx">Location</span><span class="p">;</span>

	<span class="cm">/**
	 * The message of this related diagnostic information.
	 */</span>
	<span class="nl">message</span><span class="p">:</span> <span class="kr">string</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<p><code class="language-plaintext highlighter-rouge">CodeDescription</code> is defined as follows:</p>

<div class="anchorHolder"><a href="#codeDescription" name="codeDescription" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * Structure to capture a description for an error code.
 *
 * @since 3.16.0
 */</span>
<span class="k">export</span> <span class="kr">interface</span> <span class="nx">CodeDescription</span> <span class="p">{</span>
	<span class="cm">/**
	 * An URI to open with more information about the diagnostic error.
	 */</span>
	<span class="nl">href</span><span class="p">:</span> <span class="nx">URI</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>
<h4 id="-command-"><a href="#command" name="command" class="anchor"> Command </a></h4>

<p>Represents a reference to a command. Provides a title which will be used to represent a command in the UI. Commands are identified by a string identifier. The recommended way to handle commands is to implement their execution on the server side if the client and server provides the corresponding capabilities. Alternatively the tool extension code could handle the command. The protocol currently doesn’t specify a set of well-known commands.</p>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="kr">interface</span> <span class="nx">Command</span> <span class="p">{</span>
	<span class="cm">/**
	 * Title of the command, like `save`.
	 */</span>
	<span class="nl">title</span><span class="p">:</span> <span class="kr">string</span><span class="p">;</span>
	<span class="cm">/**
	 * The identifier of the actual command handler.
	 */</span>
	<span class="nl">command</span><span class="p">:</span> <span class="kr">string</span><span class="p">;</span>
	<span class="cm">/**
	 * Arguments that the command handler should be
	 * invoked with.
	 */</span>
	<span class="nl">arguments</span><span class="p">?:</span> <span class="nx">LSPAny</span><span class="p">[];</span>
<span class="p">}</span>
</code></pre></div></div>
<h4 id="-markupcontent-"><a href="#markupContent" name="markupContent" class="anchor"> MarkupContent </a></h4>

<p>A <code class="language-plaintext highlighter-rouge">MarkupContent</code> literal represents a string value which content can be represented in different formats. Currently <code class="language-plaintext highlighter-rouge">plaintext</code> and <code class="language-plaintext highlighter-rouge">markdown</code> are supported formats. A <code class="language-plaintext highlighter-rouge">MarkupContent</code> is usually used in documentation properties of result literals like <code class="language-plaintext highlighter-rouge">CompletionItem</code> or <code class="language-plaintext highlighter-rouge">SignatureInformation</code>. If the format is <code class="language-plaintext highlighter-rouge">markdown</code> the content should follow the <a href="https://github.github.com/gfm/">GitHub Flavored Markdown Specification</a>.</p>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * Describes the content type that a client supports in various
 * result literals like `Hover`, `ParameterInfo` or `CompletionItem`.
 *
 * Please note that `MarkupKinds` must not start with a `$`. This kinds
 * are reserved for internal usage.
 */</span>
<span class="k">export</span> <span class="k">namespace</span> <span class="nx">MarkupKind</span> <span class="p">{</span>
	<span class="cm">/**
	 * Plain text is supported as a content format
	 */</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">PlainText</span><span class="p">:</span> <span class="dl">'</span><span class="s1">plaintext</span><span class="dl">'</span> <span class="o">=</span> <span class="dl">'</span><span class="s1">plaintext</span><span class="dl">'</span><span class="p">;</span>

	<span class="cm">/**
	 * Markdown is supported as a content format
	 */</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">Markdown</span><span class="p">:</span> <span class="dl">'</span><span class="s1">markdown</span><span class="dl">'</span> <span class="o">=</span> <span class="dl">'</span><span class="s1">markdown</span><span class="dl">'</span><span class="p">;</span>
<span class="p">}</span>
<span class="k">export</span> <span class="kd">type</span> <span class="nx">MarkupKind</span> <span class="o">=</span> <span class="dl">'</span><span class="s1">plaintext</span><span class="dl">'</span> <span class="o">|</span> <span class="dl">'</span><span class="s1">markdown</span><span class="dl">'</span><span class="p">;</span>
</code></pre></div></div>

<div class="anchorHolder"><a href="#markupContentDefinition" name="markupContentInnerDefinition" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * A `MarkupContent` literal represents a string value which content is
 * interpreted base on its kind flag. Currently the protocol supports
 * `plaintext` and `markdown` as markup kinds.
 *
 * If the kind is `markdown` then the value can contain fenced code blocks like
 * in GitHub issues.
 *
 * Here is an example how such a string can be constructed using
 * JavaScript / TypeScript:
 * ```typescript
 * let markdown: MarkdownContent = {
 * 	kind: MarkupKind.Markdown,
 * 	value: [
 * 		'# Header',
 * 		'Some text',
 * 		'```typescript',
 * 		'someCode();',
 * 		'```'
 * 	].join('\n')
 * };
 * ```
 *
 * *Please Note* that clients might sanitize the return markdown. A client could
 * decide to remove HTML from the markdown to avoid script execution.
 */</span>
<span class="k">export</span> <span class="kr">interface</span> <span class="nx">MarkupContent</span> <span class="p">{</span>
	<span class="cm">/**
	 * The type of the Markup
	 */</span>
	<span class="nl">kind</span><span class="p">:</span> <span class="nx">MarkupKind</span><span class="p">;</span>

	<span class="cm">/**
	 * The content itself
	 */</span>
	<span class="nl">value</span><span class="p">:</span> <span class="kr">string</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<p>In addition clients should signal the markdown parser they are using via the client capability <code class="language-plaintext highlighter-rouge">general.markdown</code> introduced in version 3.16.0 defined as follows:</p>

<div class="anchorHolder"><a href="#markdownClientCapabilities" name="markdownClientCapabilities" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * Client capabilities specific to the used markdown parser.
 *
 * @since 3.16.0
 */</span>
<span class="k">export</span> <span class="kr">interface</span> <span class="nx">MarkdownClientCapabilities</span> <span class="p">{</span>
	<span class="cm">/**
	 * The name of the parser.
	 */</span>
	<span class="nl">parser</span><span class="p">:</span> <span class="kr">string</span><span class="p">;</span>

	<span class="cm">/**
	 * The version of the parser.
	 */</span>
	<span class="nl">version</span><span class="p">?:</span> <span class="kr">string</span><span class="p">;</span>

	<span class="cm">/**
	 * A list of HTML tags that the client allows / supports in
	 * Markdown.
	 *
	 * @since 3.17.0
	 */</span>
	<span class="nl">allowedTags</span><span class="p">?:</span> <span class="kr">string</span><span class="p">[];</span>
<span class="p">}</span>
</code></pre></div></div>

<p>Known markdown parsers used by clients right now are:</p>

<table>
  <thead>
    <tr>
      <th>Parser</th>
      <th>Version</th>
      <th>Documentation</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td>marked</td>
      <td>1.1.0</td>
      <td><a href="https://marked.js.org/">Marked Documentation</a></td>
    </tr>
    <tr>
      <td>Python-Markdown</td>
      <td>3.2.2</td>
      <td><a href="https://python-markdown.github.io">Python-Markdown Documentation</a></td>
    </tr>
  </tbody>
</table>

<h3 id="-file-resource-changes-"><a href="#resourceChanges" name="resourceChanges" class="anchor"> File Resource changes </a></h3>

<blockquote>
  <p>New in version 3.13. Since version 3.16 file resource changes can carry an additional property <code class="language-plaintext highlighter-rouge">changeAnnotation</code> to describe the actual change in more detail. Whether a client has support for change annotations is guarded by the client capability <code class="language-plaintext highlighter-rouge">workspace.workspaceEdit.changeAnnotationSupport</code>.</p>
</blockquote>

<p>File resource changes allow servers to create, rename and delete files and folders via the client. Note that the names talk about files but the operations are supposed to work on files and folders. This is in line with other naming in the Language Server Protocol (see file watchers which can watch files and folders). The corresponding change literals look as follows:</p>

<div class="anchorHolder"><a href="#createFileOptions" name="createFileOptions" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * Options to create a file.
 */</span>
<span class="k">export</span> <span class="kr">interface</span> <span class="nx">CreateFileOptions</span> <span class="p">{</span>
	<span class="cm">/**
	 * Overwrite existing file. Overwrite wins over `ignoreIfExists`
	 */</span>
	<span class="nl">overwrite</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>

	<span class="cm">/**
	 * Ignore if exists.
	 */</span>
	<span class="nl">ignoreIfExists</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<div class="anchorHolder"><a href="#createFile" name="createFile" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * Create file operation
 */</span>
<span class="k">export</span> <span class="kr">interface</span> <span class="nx">CreateFile</span> <span class="p">{</span>
	<span class="cm">/**
	 * A create
	 */</span>
	<span class="nl">kind</span><span class="p">:</span> <span class="dl">'</span><span class="s1">create</span><span class="dl">'</span><span class="p">;</span>

	<span class="cm">/**
	 * The resource to create.
	 */</span>
	<span class="nl">uri</span><span class="p">:</span> <span class="nx">DocumentUri</span><span class="p">;</span>

	<span class="cm">/**
	 * Additional options
	 */</span>
	<span class="nl">options</span><span class="p">?:</span> <span class="nx">CreateFileOptions</span><span class="p">;</span>

	<span class="cm">/**
	 * An optional annotation identifier describing the operation.
	 *
	 * @since 3.16.0
	 */</span>
	<span class="nl">annotationId</span><span class="p">?:</span> <span class="nx">ChangeAnnotationIdentifier</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<div class="anchorHolder"><a href="#renameFileOptions" name="renameFileOptions" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * Rename file options
 */</span>
<span class="k">export</span> <span class="kr">interface</span> <span class="nx">RenameFileOptions</span> <span class="p">{</span>
	<span class="cm">/**
	 * Overwrite target if existing. Overwrite wins over `ignoreIfExists`
	 */</span>
	<span class="nl">overwrite</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>

	<span class="cm">/**
	 * Ignores if target exists.
	 */</span>
	<span class="nl">ignoreIfExists</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<div class="anchorHolder"><a href="#renameFile" name="renameFile" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * Rename file operation
 */</span>
<span class="k">export</span> <span class="kr">interface</span> <span class="nx">RenameFile</span> <span class="p">{</span>
	<span class="cm">/**
	 * A rename
	 */</span>
	<span class="nl">kind</span><span class="p">:</span> <span class="dl">'</span><span class="s1">rename</span><span class="dl">'</span><span class="p">;</span>

	<span class="cm">/**
	 * The old (existing) location.
	 */</span>
	<span class="nl">oldUri</span><span class="p">:</span> <span class="nx">DocumentUri</span><span class="p">;</span>

	<span class="cm">/**
	 * The new location.
	 */</span>
	<span class="nl">newUri</span><span class="p">:</span> <span class="nx">DocumentUri</span><span class="p">;</span>

	<span class="cm">/**
	 * Rename options.
	 */</span>
	<span class="nl">options</span><span class="p">?:</span> <span class="nx">RenameFileOptions</span><span class="p">;</span>

	<span class="cm">/**
	 * An optional annotation identifier describing the operation.
	 *
	 * @since 3.16.0
	 */</span>
	<span class="nl">annotationId</span><span class="p">?:</span> <span class="nx">ChangeAnnotationIdentifier</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<div class="anchorHolder"><a href="#deleteFileOptions" name="deleteFileOptions" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * Delete file options
 */</span>
<span class="k">export</span> <span class="kr">interface</span> <span class="nx">DeleteFileOptions</span> <span class="p">{</span>
	<span class="cm">/**
	 * Delete the content recursively if a folder is denoted.
	 */</span>
	<span class="nl">recursive</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>

	<span class="cm">/**
	 * Ignore the operation if the file doesn't exist.
	 */</span>
	<span class="nl">ignoreIfNotExists</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<div class="anchorHolder"><a href="#deleteFile" name="deleteFile" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * Delete file operation
 */</span>
<span class="k">export</span> <span class="kr">interface</span> <span class="nx">DeleteFile</span> <span class="p">{</span>
	<span class="cm">/**
	 * A delete
	 */</span>
	<span class="nl">kind</span><span class="p">:</span> <span class="dl">'</span><span class="s1">delete</span><span class="dl">'</span><span class="p">;</span>

	<span class="cm">/**
	 * The file to delete.
	 */</span>
	<span class="nl">uri</span><span class="p">:</span> <span class="nx">DocumentUri</span><span class="p">;</span>

	<span class="cm">/**
	 * Delete options.
	 */</span>
	<span class="nl">options</span><span class="p">?:</span> <span class="nx">DeleteFileOptions</span><span class="p">;</span>

	<span class="cm">/**
	 * An optional annotation identifier describing the operation.
	 *
	 * @since 3.16.0
	 */</span>
	<span class="nl">annotationId</span><span class="p">?:</span> <span class="nx">ChangeAnnotationIdentifier</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>
<h4 id="-workspaceedit-"><a href="#workspaceEdit" name="workspaceEdit" class="anchor"> WorkspaceEdit </a></h4>

<p>A workspace edit represents changes to many resources managed in the workspace. The edit should either provide <code class="language-plaintext highlighter-rouge">changes</code> or <code class="language-plaintext highlighter-rouge">documentChanges</code>. If the client can handle versioned document edits and if <code class="language-plaintext highlighter-rouge">documentChanges</code> are present, the latter are preferred over <code class="language-plaintext highlighter-rouge">changes</code>.</p>

<p>Since version 3.13.0 a workspace edit can contain resource operations (create, delete or rename files and folders) as well. If resource operations are present clients need to execute the operations in the order in which they are provided. So a workspace edit for example can consist of the following two changes: (1) create file a.txt and (2) a text document edit which insert text into file a.txt. An invalid sequence (e.g. (1) delete file a.txt and (2) insert text into file a.txt) will cause failure of the operation. How the client recovers from the failure is described by the client capability: <code class="language-plaintext highlighter-rouge">workspace.workspaceEdit.failureHandling</code></p>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">WorkspaceEdit</span> <span class="p">{</span>
	<span class="cm">/**
	 * Holds changes to existing resources.
	 */</span>
	<span class="nl">changes</span><span class="p">?:</span> <span class="p">{</span> <span class="p">[</span><span class="na">uri</span><span class="p">:</span> <span class="nx">DocumentUri</span><span class="p">]:</span> <span class="nx">TextEdit</span><span class="p">[];</span> <span class="p">};</span>

	<span class="cm">/**
	 * Depending on the client capability
	 * `workspace.workspaceEdit.resourceOperations` document changes are either
	 * an array of `TextDocumentEdit`s to express changes to n different text
	 * documents where each text document edit addresses a specific version of
	 * a text document. Or it can contain above `TextDocumentEdit`s mixed with
	 * create, rename and delete file / folder operations.
	 *
	 * Whether a client supports versioned document edits is expressed via
	 * `workspace.workspaceEdit.documentChanges` client capability.
	 *
	 * If a client neither supports `documentChanges` nor
	 * `workspace.workspaceEdit.resourceOperations` then only plain `TextEdit`s
	 * using the `changes` property are supported.
	 */</span>
	<span class="nl">documentChanges</span><span class="p">?:</span> <span class="p">(</span>
		<span class="nx">TextDocumentEdit</span><span class="p">[]</span> <span class="o">|</span>
		<span class="p">(</span><span class="nx">TextDocumentEdit</span> <span class="o">|</span> <span class="nx">CreateFile</span> <span class="o">|</span> <span class="nx">RenameFile</span> <span class="o">|</span> <span class="nx">DeleteFile</span><span class="p">)[]</span>
	<span class="p">);</span>

	<span class="cm">/**
	 * A map of change annotations that can be referenced in
	 * `AnnotatedTextEdit`s or create, rename and delete file / folder
	 * operations.
	 *
	 * Whether clients honor this property depends on the client capability
	 * `workspace.changeAnnotationSupport`.
	 *
	 * @since 3.16.0
	 */</span>
	<span class="nl">changeAnnotations</span><span class="p">?:</span> <span class="p">{</span>
		<span class="p">[</span><span class="na">id</span><span class="p">:</span> <span class="kr">string</span> <span class="cm">/* ChangeAnnotationIdentifier */</span><span class="p">]:</span> <span class="nx">ChangeAnnotation</span><span class="p">;</span>
	<span class="p">};</span>
<span class="p">}</span>
</code></pre></div></div>

<h5 id="-workspaceeditclientcapabilities-"><a href="#workspaceEditClientCapabilities" name="workspaceEditClientCapabilities" class="anchor"> WorkspaceEditClientCapabilities </a></h5>

<blockquote>
  <p>New in version 3.13: <code class="language-plaintext highlighter-rouge">ResourceOperationKind</code> and <code class="language-plaintext highlighter-rouge">FailureHandlingKind</code> and the client capability <code class="language-plaintext highlighter-rouge">workspace.workspaceEdit.resourceOperations</code> as well as <code class="language-plaintext highlighter-rouge">workspace.workspaceEdit.failureHandling</code>.</p>
</blockquote>

<p>The capabilities of a workspace edit has evolved over the time. Clients can describe their support using the following client capability:</p>

<p><em>Client Capability</em>:</p>
<ul>
  <li>property path (optional): <code class="language-plaintext highlighter-rouge">workspace.workspaceEdit</code>
</li>
  <li>property type: <code class="language-plaintext highlighter-rouge">WorkspaceEditClientCapabilities</code> defined as follows:</li>
</ul>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">WorkspaceEditClientCapabilities</span> <span class="p">{</span>
	<span class="cm">/**
	 * The client supports versioned document changes in `WorkspaceEdit`s
	 */</span>
	<span class="nl">documentChanges</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>

	<span class="cm">/**
	 * The resource operations the client supports. Clients should at least
	 * support 'create', 'rename' and 'delete' files and folders.
	 *
	 * @since 3.13.0
	 */</span>
	<span class="nl">resourceOperations</span><span class="p">?:</span> <span class="nx">ResourceOperationKind</span><span class="p">[];</span>

	<span class="cm">/**
	 * The failure handling strategy of a client if applying the workspace edit
	 * fails.
	 *
	 * @since 3.13.0
	 */</span>
	<span class="nl">failureHandling</span><span class="p">?:</span> <span class="nx">FailureHandlingKind</span><span class="p">;</span>

	<span class="cm">/**
	 * Whether the client normalizes line endings to the client specific
	 * setting.
	 * If set to `true` the client will normalize line ending characters
	 * in a workspace edit to the client specific new line character(s).
	 *
	 * @since 3.16.0
	 */</span>
	<span class="nl">normalizesLineEndings</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>

	<span class="cm">/**
	 * Whether the client in general supports change annotations on text edits,
	 * create file, rename file and delete file changes.
	 *
	 * @since 3.16.0
	 */</span>
	<span class="nl">changeAnnotationSupport</span><span class="p">?:</span> <span class="p">{</span>
		<span class="cm">/**
		 * Whether the client groups edits with equal labels into tree nodes,
		 * for instance all edits labelled with "Changes in Strings" would
		 * be a tree node.
		 */</span>
		<span class="nx">groupsOnLabel</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>
	<span class="p">};</span>
<span class="p">}</span>
</code></pre></div></div>

<div class="anchorHolder"><a href="#resourceOperationKind" name="resourceOperationKind" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * The kind of resource operations supported by the client.
 */</span>
<span class="k">export</span> <span class="kd">type</span> <span class="nx">ResourceOperationKind</span> <span class="o">=</span> <span class="dl">'</span><span class="s1">create</span><span class="dl">'</span> <span class="o">|</span> <span class="dl">'</span><span class="s1">rename</span><span class="dl">'</span> <span class="o">|</span> <span class="dl">'</span><span class="s1">delete</span><span class="dl">'</span><span class="p">;</span>

<span class="k">export</span> <span class="k">namespace</span> <span class="nx">ResourceOperationKind</span> <span class="p">{</span>

	<span class="cm">/**
	 * Supports creating new files and folders.
	 */</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">Create</span><span class="p">:</span> <span class="nx">ResourceOperationKind</span> <span class="o">=</span> <span class="dl">'</span><span class="s1">create</span><span class="dl">'</span><span class="p">;</span>

	<span class="cm">/**
	 * Supports renaming existing files and folders.
	 */</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">Rename</span><span class="p">:</span> <span class="nx">ResourceOperationKind</span> <span class="o">=</span> <span class="dl">'</span><span class="s1">rename</span><span class="dl">'</span><span class="p">;</span>

	<span class="cm">/**
	 * Supports deleting existing files and folders.
	 */</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">Delete</span><span class="p">:</span> <span class="nx">ResourceOperationKind</span> <span class="o">=</span> <span class="dl">'</span><span class="s1">delete</span><span class="dl">'</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<div class="anchorHolder"><a href="#failureHandlingKind" name="failureHandlingKind" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kd">type</span> <span class="nx">FailureHandlingKind</span> <span class="o">=</span> <span class="dl">'</span><span class="s1">abort</span><span class="dl">'</span> <span class="o">|</span> <span class="dl">'</span><span class="s1">transactional</span><span class="dl">'</span> <span class="o">|</span> <span class="dl">'</span><span class="s1">undo</span><span class="dl">'</span>
	<span class="o">|</span> <span class="dl">'</span><span class="s1">textOnlyTransactional</span><span class="dl">'</span><span class="p">;</span>

<span class="k">export</span> <span class="k">namespace</span> <span class="nx">FailureHandlingKind</span> <span class="p">{</span>

	<span class="cm">/**
	 * Applying the workspace change is simply aborted if one of the changes
	 * provided fails. All operations executed before the failing operation
	 * stay executed.
	 */</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">Abort</span><span class="p">:</span> <span class="nx">FailureHandlingKind</span> <span class="o">=</span> <span class="dl">'</span><span class="s1">abort</span><span class="dl">'</span><span class="p">;</span>

	<span class="cm">/**
	 * All operations are executed transactional. That means they either all
	 * succeed or no changes at all are applied to the workspace.
	 */</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">Transactional</span><span class="p">:</span> <span class="nx">FailureHandlingKind</span> <span class="o">=</span> <span class="dl">'</span><span class="s1">transactional</span><span class="dl">'</span><span class="p">;</span>


	<span class="cm">/**
	 * If the workspace edit contains only textual file changes they are
	 * executed transactional. If resource changes (create, rename or delete
	 * file) are part of the change the failure handling strategy is abort.
	 */</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">TextOnlyTransactional</span><span class="p">:</span> <span class="nx">FailureHandlingKind</span>
		<span class="o">=</span> <span class="dl">'</span><span class="s1">textOnlyTransactional</span><span class="dl">'</span><span class="p">;</span>

	<span class="cm">/**
	 * The client tries to undo the operations already executed. But there is no
	 * guarantee that this is succeeding.
	 */</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">Undo</span><span class="p">:</span> <span class="nx">FailureHandlingKind</span> <span class="o">=</span> <span class="dl">'</span><span class="s1">undo</span><span class="dl">'</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<h4 id="-work-done-progress-"><a href="#workDoneProgress" name="workDoneProgress" class="anchor"> Work Done Progress </a></h4>

<blockquote>
  <p><em>Since version 3.15.0</em></p>
</blockquote>

<p>Work done progress is reported using the generic <a href="#progress"><code class="language-plaintext highlighter-rouge">$/progress</code></a> notification. The value payload of a work done progress notification can be of three different forms.</p>

<h5 id="-work-done-progress-begin-"><a href="#workDoneProgressBegin" name="workDoneProgressBegin" class="anchor"> Work Done Progress Begin </a></h5>

<p>To start progress reporting a <code class="language-plaintext highlighter-rouge">$/progress</code> notification with the following payload must be sent:</p>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">WorkDoneProgressBegin</span> <span class="p">{</span>

	<span class="nl">kind</span><span class="p">:</span> <span class="dl">'</span><span class="s1">begin</span><span class="dl">'</span><span class="p">;</span>

	<span class="cm">/**
	 * Mandatory title of the progress operation. Used to briefly inform about
	 * the kind of operation being performed.
	 *
	 * Examples: "Indexing" or "Linking dependencies".
	 */</span>
	<span class="nl">title</span><span class="p">:</span> <span class="kr">string</span><span class="p">;</span>

	<span class="cm">/**
	 * Controls if a cancel button should show to allow the user to cancel the
	 * long running operation. Clients that don't support cancellation are
	 * allowed to ignore the setting.
	 */</span>
	<span class="nl">cancellable</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>

	<span class="cm">/**
	 * Optional, more detailed associated progress message. Contains
	 * complementary information to the `title`.
	 *
	 * Examples: "3/25 files", "project/src/module2", "node_modules/some_dep".
	 * If unset, the previous progress message (if any) is still valid.
	 */</span>
	<span class="nl">message</span><span class="p">?:</span> <span class="kr">string</span><span class="p">;</span>

	<span class="cm">/**
	 * Optional progress percentage to display (value 100 is considered 100%).
	 * If not provided infinite progress is assumed and clients are allowed
	 * to ignore the `percentage` value in subsequent in report notifications.
	 *
	 * The value should be steadily rising. Clients are free to ignore values
	 * that are not following this rule. The value range is [0, 100]
	 */</span>
	<span class="nl">percentage</span><span class="p">?:</span> <span class="nx">uinteger</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<h5 id="-work-done-progress-report-"><a href="#workDoneProgressReport" name="workDoneProgressReport" class="anchor"> Work Done Progress Report </a></h5>

<p>Reporting progress is done using the following payload:</p>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">WorkDoneProgressReport</span> <span class="p">{</span>

	<span class="nl">kind</span><span class="p">:</span> <span class="dl">'</span><span class="s1">report</span><span class="dl">'</span><span class="p">;</span>

	<span class="cm">/**
	 * Controls enablement state of a cancel button. This property is only valid
	 * if a cancel button got requested in the `WorkDoneProgressBegin` payload.
	 *
	 * Clients that don't support cancellation or don't support control the
	 * button's enablement state are allowed to ignore the setting.
	 */</span>
	<span class="nl">cancellable</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>

	<span class="cm">/**
	 * Optional, more detailed associated progress message. Contains
	 * complementary information to the `title`.
	 *
	 * Examples: "3/25 files", "project/src/module2", "node_modules/some_dep".
	 * If unset, the previous progress message (if any) is still valid.
	 */</span>
	<span class="nl">message</span><span class="p">?:</span> <span class="kr">string</span><span class="p">;</span>

	<span class="cm">/**
	 * Optional progress percentage to display (value 100 is considered 100%).
	 * If not provided infinite progress is assumed and clients are allowed
	 * to ignore the `percentage` value in subsequent in report notifications.
	 *
	 * The value should be steadily rising. Clients are free to ignore values
	 * that are not following this rule. The value range is [0, 100]
	 */</span>
	<span class="nl">percentage</span><span class="p">?:</span> <span class="nx">uinteger</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<h5 id="-work-done-progress-end-"><a href="#workDoneProgressEnd" name="workDoneProgressEnd" class="anchor"> Work Done Progress End </a></h5>

<p>Signaling the end of a progress reporting is done using the following payload:</p>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">WorkDoneProgressEnd</span> <span class="p">{</span>

	<span class="nl">kind</span><span class="p">:</span> <span class="dl">'</span><span class="s1">end</span><span class="dl">'</span><span class="p">;</span>

	<span class="cm">/**
	 * Optional, a final message indicating to for example indicate the outcome
	 * of the operation.
	 */</span>
	<span class="nl">message</span><span class="p">?:</span> <span class="kr">string</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<h5 id="-initiating-work-done-progress-"><a href="#initiatingWorkDoneProgress" name="initiatingWorkDoneProgress" class="anchor"> Initiating Work Done Progress </a></h5>

<p>Work Done progress can be initiated in two different ways:</p>

<ol>
  <li>by the sender of a request (mostly clients) using the predefined <code class="language-plaintext highlighter-rouge">workDoneToken</code> property in the requests parameter literal. The document will refer to this kind of progress as client initiated progress.</li>
  <li>by a server using the request <code class="language-plaintext highlighter-rouge">window/workDoneProgress/create</code>. The document will refer to this kind of progress as server initiated progress.</li>
</ol>

<h6 id="client-initiated-progress-"><a href="#clientInitiatedProgress" name="clientInitiatedProgress" class="anchor">Client Initiated Progress </a></h6>

<p>Consider a client sending a <code class="language-plaintext highlighter-rouge">textDocument/reference</code> request to a server and the client accepts work done progress reporting on that request. To signal this to the server the client would add a <code class="language-plaintext highlighter-rouge">workDoneToken</code> property to the reference request parameters. Something like this:</p>

<div class="language-json highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="p">{</span><span class="w">
	</span><span class="nl">"textDocument"</span><span class="p">:</span><span class="w"> </span><span class="p">{</span><span class="w">
		</span><span class="nl">"uri"</span><span class="p">:</span><span class="w"> </span><span class="s2">"file:///folder/file.ts"</span><span class="w">
	</span><span class="p">},</span><span class="w">
	</span><span class="nl">"position"</span><span class="p">:</span><span class="w"> </span><span class="p">{</span><span class="w">
		</span><span class="nl">"line"</span><span class="p">:</span><span class="w"> </span><span class="mi">9</span><span class="p">,</span><span class="w">
		</span><span class="nl">"character"</span><span class="p">:</span><span class="w"> </span><span class="mi">5</span><span class="w">
	</span><span class="p">},</span><span class="w">
	</span><span class="nl">"context"</span><span class="p">:</span><span class="w"> </span><span class="p">{</span><span class="w">
		</span><span class="nl">"includeDeclaration"</span><span class="p">:</span><span class="w"> </span><span class="kc">true</span><span class="w">
	</span><span class="p">},</span><span class="w">
	</span><span class="err">//</span><span class="w"> </span><span class="err">The</span><span class="w"> </span><span class="err">token</span><span class="w"> </span><span class="err">used</span><span class="w"> </span><span class="err">to</span><span class="w"> </span><span class="err">report</span><span class="w"> </span><span class="err">work</span><span class="w"> </span><span class="err">done</span><span class="w"> </span><span class="err">progress.</span><span class="w">
	</span><span class="nl">"workDoneToken"</span><span class="p">:</span><span class="w"> </span><span class="s2">"1d546990-40a3-4b77-b134-46622995f6ae"</span><span class="w">
</span><span class="p">}</span><span class="w">
</span></code></pre></div></div>

<p>The corresponding type definition for the parameter property looks like this:</p>

<div class="anchorHolder"><a href="#workDoneProgressParams" name="workDoneProgressParams" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">WorkDoneProgressParams</span> <span class="p">{</span>
	<span class="cm">/**
	 * An optional token that a server can use to report work done progress.
	 */</span>
	<span class="nl">workDoneToken</span><span class="p">?:</span> <span class="nx">ProgressToken</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<p>A server uses the <code class="language-plaintext highlighter-rouge">workDoneToken</code> to report progress for the specific <code class="language-plaintext highlighter-rouge">textDocument/reference</code>. For the above request the <code class="language-plaintext highlighter-rouge">$/progress</code> notification params look like this:</p>

<div class="language-json highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="p">{</span><span class="w">
	</span><span class="nl">"token"</span><span class="p">:</span><span class="w"> </span><span class="s2">"1d546990-40a3-4b77-b134-46622995f6ae"</span><span class="p">,</span><span class="w">
	</span><span class="nl">"value"</span><span class="p">:</span><span class="w"> </span><span class="p">{</span><span class="w">
		</span><span class="nl">"kind"</span><span class="p">:</span><span class="w"> </span><span class="s2">"begin"</span><span class="p">,</span><span class="w">
		</span><span class="nl">"title"</span><span class="p">:</span><span class="w"> </span><span class="s2">"Finding references for A#foo"</span><span class="p">,</span><span class="w">
		</span><span class="nl">"cancellable"</span><span class="p">:</span><span class="w"> </span><span class="kc">false</span><span class="p">,</span><span class="w">
		</span><span class="nl">"message"</span><span class="p">:</span><span class="w"> </span><span class="s2">"Processing file X.ts"</span><span class="p">,</span><span class="w">
		</span><span class="nl">"percentage"</span><span class="p">:</span><span class="w"> </span><span class="mi">0</span><span class="w">
	</span><span class="p">}</span><span class="w">
</span><span class="p">}</span><span class="w">
</span></code></pre></div></div>

<p>The token received via the <code class="language-plaintext highlighter-rouge">workDoneToken</code> property in a request’s param literal is only valid as long as the request has not send a response back.</p>

<p>There is no specific client capability signaling whether a client will send a progress token per request. The reason for this is that this is in many clients not a static aspect and might even change for every request instance for the same request type. So the capability is signal on every request instance by the presence of a <code class="language-plaintext highlighter-rouge">workDoneToken</code> property.</p>

<p>To avoid that clients set up a progress monitor user interface before sending a request but the server doesn’t actually report any progress a server needs to signal general work done progress reporting support in the corresponding server capability. For the above find references example a server would signal such a support by setting the <code class="language-plaintext highlighter-rouge">referencesProvider</code> property in the server capabilities as follows:</p>

<div class="language-json highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="p">{</span><span class="w">
	</span><span class="nl">"referencesProvider"</span><span class="p">:</span><span class="w"> </span><span class="p">{</span><span class="w">
		</span><span class="nl">"workDoneProgress"</span><span class="p">:</span><span class="w"> </span><span class="kc">true</span><span class="w">
	</span><span class="p">}</span><span class="w">
</span><span class="p">}</span><span class="w">
</span></code></pre></div></div>

<p>The corresponding type definition for the server capability looks like this:</p>

<div class="anchorHolder"><a href="#workDoneProgressOptions" name="workDoneProgressOptions" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">WorkDoneProgressOptions</span> <span class="p">{</span>
	<span class="nl">workDoneProgress</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>
<h6 id="server-initiated-progress-"><a href="#serverInitiatedProgress" name="serverInitiatedProgress" class="anchor">Server Initiated Progress </a></h6>

<p>Servers can also initiate progress reporting using the <code class="language-plaintext highlighter-rouge">window/workDoneProgress/create</code> request. This is useful if the server needs to report progress outside of a request (for example the server needs to re-index a database). The token can then be used to report progress using the same notifications used as for client initiated progress. The token provided in the create request should only be used once (e.g. only one begin, many report and one end notification should be sent to it).</p>

<p>To keep the protocol backwards compatible servers are only allowed to use <code class="language-plaintext highlighter-rouge">window/workDoneProgress/create</code> request if the client signals corresponding support using the client capability <code class="language-plaintext highlighter-rouge">window.workDoneProgress</code> which is defined as follows:</p>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code>	<span class="cm">/**
	 * Window specific client capabilities.
	 */</span>
	<span class="nb">window</span><span class="p">?:</span> <span class="p">{</span>
		<span class="cm">/**
		 * Whether client supports server initiated progress using the
		 * `window/workDoneProgress/create` request.
		 */</span>
		<span class="nx">workDoneProgress</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>
	<span class="p">};</span>
</code></pre></div></div>
<h4 id="-partial-result-progress-"><a href="#partialResults" name="partialResults" class="anchor"> Partial Result Progress </a></h4>

<blockquote>
  <p><em>Since version 3.15.0</em></p>
</blockquote>

<p>Partial results are also reported using the generic <a href="#progress"><code class="language-plaintext highlighter-rouge">$/progress</code></a> notification. The value payload of a partial result progress notification is in most cases the same as the final result. For example the <code class="language-plaintext highlighter-rouge">workspace/symbol</code> request has <code class="language-plaintext highlighter-rouge">SymbolInformation[]</code> | <code class="language-plaintext highlighter-rouge">WorkspaceSymbol[]</code> as the result type. Partial result is therefore also of type <code class="language-plaintext highlighter-rouge">SymbolInformation[]</code> | <code class="language-plaintext highlighter-rouge">WorkspaceSymbol[]</code>. Whether a client accepts partial result notifications for a request is signaled by adding a <code class="language-plaintext highlighter-rouge">partialResultToken</code> to the request parameter. For example, a <code class="language-plaintext highlighter-rouge">textDocument/reference</code> request that supports both work done and partial result progress might look like this:</p>

<div class="language-json highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="p">{</span><span class="w">
	</span><span class="nl">"textDocument"</span><span class="p">:</span><span class="w"> </span><span class="p">{</span><span class="w">
		</span><span class="nl">"uri"</span><span class="p">:</span><span class="w"> </span><span class="s2">"file:///folder/file.ts"</span><span class="w">
	</span><span class="p">},</span><span class="w">
	</span><span class="nl">"position"</span><span class="p">:</span><span class="w"> </span><span class="p">{</span><span class="w">
		</span><span class="nl">"line"</span><span class="p">:</span><span class="w"> </span><span class="mi">9</span><span class="p">,</span><span class="w">
		</span><span class="nl">"character"</span><span class="p">:</span><span class="w"> </span><span class="mi">5</span><span class="w">
	</span><span class="p">},</span><span class="w">
	</span><span class="nl">"context"</span><span class="p">:</span><span class="w"> </span><span class="p">{</span><span class="w">
		</span><span class="nl">"includeDeclaration"</span><span class="p">:</span><span class="w"> </span><span class="kc">true</span><span class="w">
	</span><span class="p">},</span><span class="w">
	</span><span class="err">//</span><span class="w"> </span><span class="err">The</span><span class="w"> </span><span class="err">token</span><span class="w"> </span><span class="err">used</span><span class="w"> </span><span class="err">to</span><span class="w"> </span><span class="err">report</span><span class="w"> </span><span class="err">work</span><span class="w"> </span><span class="err">done</span><span class="w"> </span><span class="err">progress.</span><span class="w">
	</span><span class="nl">"workDoneToken"</span><span class="p">:</span><span class="w"> </span><span class="s2">"1d546990-40a3-4b77-b134-46622995f6ae"</span><span class="p">,</span><span class="w">
	</span><span class="err">//</span><span class="w"> </span><span class="err">The</span><span class="w"> </span><span class="err">token</span><span class="w"> </span><span class="err">used</span><span class="w"> </span><span class="err">to</span><span class="w"> </span><span class="err">report</span><span class="w"> </span><span class="err">partial</span><span class="w"> </span><span class="err">result</span><span class="w"> </span><span class="err">progress.</span><span class="w">
	</span><span class="nl">"partialResultToken"</span><span class="p">:</span><span class="w"> </span><span class="s2">"5f6f349e-4f81-4a3b-afff-ee04bff96804"</span><span class="w">
</span><span class="p">}</span><span class="w">
</span></code></pre></div></div>

<p>The <code class="language-plaintext highlighter-rouge">partialResultToken</code> is then used to report partial results for the find references request.</p>

<p>If a server reports partial result via a corresponding <code class="language-plaintext highlighter-rouge">$/progress</code>, the whole result must be reported using n <code class="language-plaintext highlighter-rouge">$/progress</code> notifications. Each of the n <code class="language-plaintext highlighter-rouge">$/progress</code> notification appends items to the result. The final response has to be empty in terms of result values. This avoids confusion about how the final result should be interpreted, e.g. as another partial result or as a replacing result.</p>

<p>If the response errors the provided partial results should be treated as follows:</p>

<ul>
  <li>the <code class="language-plaintext highlighter-rouge">code</code> equals to <code class="language-plaintext highlighter-rouge">RequestCancelled</code>: the client is free to use the provided results but should make clear that the request got canceled and may be incomplete.</li>
  <li>in all other cases the provided partial results shouldn’t be used.</li>
</ul>

<h4 id="-partialresultparams-"><a href="#partialResultParams" name="partialResultParams" class="anchor"> PartialResultParams </a></h4>

<p>A parameter literal used to pass a partial result token.</p>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">PartialResultParams</span> <span class="p">{</span>
	<span class="cm">/**
	 * An optional token that a server can use to report partial results (e.g.
	 * streaming) to the client.
	 */</span>
	<span class="nl">partialResultToken</span><span class="p">?:</span> <span class="nx">ProgressToken</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>
<h4 id="-tracevalue-"><a href="#traceValue" name="traceValue" class="anchor"> TraceValue </a></h4>

<p>A <code class="language-plaintext highlighter-rouge">TraceValue</code> represents the level of verbosity with which the server systematically reports its execution trace using <a href="#logTrace">$/logTrace</a> notifications.
The initial trace value is set by the client at initialization and can be modified later using the <a href="#setTrace">$/setTrace</a> notification.</p>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kd">type</span> <span class="nx">TraceValue</span> <span class="o">=</span> <span class="dl">'</span><span class="s1">off</span><span class="dl">'</span> <span class="o">|</span> <span class="dl">'</span><span class="s1">messages</span><span class="dl">'</span> <span class="o">|</span> <span class="dl">'</span><span class="s1">verbose</span><span class="dl">'</span><span class="p">;</span>
</code></pre></div></div>

<h3 id="-server-lifecycle-"><a href="#lifeCycleMessages" name="lifeCycleMessages" class="anchor"> Server lifecycle </a></h3>

<p>The current protocol specification defines that the lifecycle of a server is managed by the client (e.g. a tool like VS Code or Emacs). It is up to the client to decide when to start (process-wise) and when to shutdown a server.</p>

<h4 id="initialize-request-leftwards_arrow_with_hook"><a href="#initialize" name="initialize" class="anchor">Initialize Request (<img class="emoji" title=":leftwards_arrow_with_hook:" alt=":leftwards_arrow_with_hook:" src="https://github.githubassets.com/images/icons/emoji/unicode/21a9.png" height="20" width="20">)</a></h4>

<p>The initialize request is sent as the first request from the client to the server. If the server receives a request or notification before the <code class="language-plaintext highlighter-rouge">initialize</code> request it should act as follows:</p>

<ul>
  <li>For a request the response should be an error with <code class="language-plaintext highlighter-rouge">code: -32002</code>. The message can be picked by the server.</li>
  <li>Notifications should be dropped, except for the exit notification. This will allow the exit of a server without an initialize request.</li>
</ul>

<p>Until the server has responded to the <code class="language-plaintext highlighter-rouge">initialize</code> request with an <code class="language-plaintext highlighter-rouge">InitializeResult</code>, the client must not send any additional requests or notifications to the server. In addition the server is not allowed to send any requests or notifications to the client until it has responded with an <code class="language-plaintext highlighter-rouge">InitializeResult</code>, with the exception that during the <code class="language-plaintext highlighter-rouge">initialize</code> request the server is allowed to send the notifications <code class="language-plaintext highlighter-rouge">window/showMessage</code>, <code class="language-plaintext highlighter-rouge">window/logMessage</code> and <code class="language-plaintext highlighter-rouge">telemetry/event</code> as well as the <code class="language-plaintext highlighter-rouge">window/showMessageRequest</code> request to the client. In case the client sets up a progress token in the initialize params (e.g. property <code class="language-plaintext highlighter-rouge">workDoneToken</code>) the server is also allowed to use that token (and only that token) using the <code class="language-plaintext highlighter-rouge">$/progress</code> notification sent from the server to the client.</p>

<p>The <code class="language-plaintext highlighter-rouge">initialize</code> request may only be sent once.</p>

<p><em>Request</em>:</p>
<ul>
  <li>method: ‘initialize’</li>
  <li>params: <code class="language-plaintext highlighter-rouge">InitializeParams</code> defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#initializeParams" name="initializeParams" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="kr">interface</span> <span class="nx">InitializeParams</span> <span class="kd">extends</span> <span class="nx">WorkDoneProgressParams</span> <span class="p">{</span>
	<span class="cm">/**
	 * The process Id of the parent process that started the server. Is null if
	 * the process has not been started by another process. If the parent
	 * process is not alive then the server should exit (see exit notification)
	 * its process.
	 */</span>
	<span class="nl">processId</span><span class="p">:</span> <span class="nx">integer</span> <span class="o">|</span> <span class="kc">null</span><span class="p">;</span>

	<span class="cm">/**
	 * Information about the client
	 *
	 * @since 3.15.0
	 */</span>
	<span class="nl">clientInfo</span><span class="p">?:</span> <span class="p">{</span>
		<span class="cm">/**
		 * The name of the client as defined by the client.
		 */</span>
		<span class="na">name</span><span class="p">:</span> <span class="kr">string</span><span class="p">;</span>

		<span class="cm">/**
		 * The client's version as defined by the client.
		 */</span>
		<span class="nl">version</span><span class="p">?:</span> <span class="kr">string</span><span class="p">;</span>
	<span class="p">};</span>

	<span class="cm">/**
	 * The locale the client is currently showing the user interface
	 * in. This must not necessarily be the locale of the operating
	 * system.
	 *
	 * Uses IETF language tags as the value's syntax
	 * (See https://en.wikipedia.org/wiki/IETF_language_tag)
	 *
	 * @since 3.16.0
	 */</span>
	<span class="nl">locale</span><span class="p">?:</span> <span class="kr">string</span><span class="p">;</span>

	<span class="cm">/**
	 * The rootPath of the workspace. Is null
	 * if no folder is open.
	 *
	 * @deprecated in favour of `rootUri`.
	 */</span>
	<span class="nl">rootPath</span><span class="p">?:</span> <span class="kr">string</span> <span class="o">|</span> <span class="kc">null</span><span class="p">;</span>

	<span class="cm">/**
	 * The rootUri of the workspace. Is null if no
	 * folder is open. If both `rootPath` and `rootUri` are set
	 * `rootUri` wins.
	 *
	 * @deprecated in favour of `workspaceFolders`
	 */</span>
	<span class="nl">rootUri</span><span class="p">:</span> <span class="nx">DocumentUri</span> <span class="o">|</span> <span class="kc">null</span><span class="p">;</span>

	<span class="cm">/**
	 * User provided initialization options.
	 */</span>
	<span class="nl">initializationOptions</span><span class="p">?:</span> <span class="nx">LSPAny</span><span class="p">;</span>

	<span class="cm">/**
	 * The capabilities provided by the client (editor or tool)
	 */</span>
	<span class="nl">capabilities</span><span class="p">:</span> <span class="nx">ClientCapabilities</span><span class="p">;</span>

	<span class="cm">/**
	 * The initial trace setting. If omitted trace is disabled ('off').
	 */</span>
	<span class="nl">trace</span><span class="p">?:</span> <span class="nx">TraceValue</span><span class="p">;</span>

	<span class="cm">/**
	 * The workspace folders configured in the client when the server starts.
	 * This property is only available if the client supports workspace folders.
	 * It can be `null` if the client supports workspace folders but none are
	 * configured.
	 *
	 * @since 3.6.0
	 */</span>
	<span class="nl">workspaceFolders</span><span class="p">?:</span> <span class="nx">WorkspaceFolder</span><span class="p">[]</span> <span class="o">|</span> <span class="kc">null</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>
<p>Where <code class="language-plaintext highlighter-rouge">ClientCapabilities</code> and <code class="language-plaintext highlighter-rouge">TextDocumentClientCapabilities</code> are defined as follows:</p>

<h5 id="textdocumentclientcapabilities">TextDocumentClientCapabilities</h5>

<p><code class="language-plaintext highlighter-rouge">TextDocumentClientCapabilities</code> define capabilities the editor / tool provides on text documents.</p>

<div class="anchorHolder"><a href="#textDocumentClientCapabilities" name="textDocumentClientCapabilities" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * Text document specific client capabilities.
 */</span>
<span class="k">export</span> <span class="kr">interface</span> <span class="nx">TextDocumentClientCapabilities</span> <span class="p">{</span>

	<span class="nl">synchronization</span><span class="p">?:</span> <span class="nx">TextDocumentSyncClientCapabilities</span><span class="p">;</span>

	<span class="cm">/**
	 * Capabilities specific to the `textDocument/completion` request.
	 */</span>
	<span class="nl">completion</span><span class="p">?:</span> <span class="nx">CompletionClientCapabilities</span><span class="p">;</span>

	<span class="cm">/**
	 * Capabilities specific to the `textDocument/hover` request.
	 */</span>
	<span class="nl">hover</span><span class="p">?:</span> <span class="nx">HoverClientCapabilities</span><span class="p">;</span>

	<span class="cm">/**
	 * Capabilities specific to the `textDocument/signatureHelp` request.
	 */</span>
	<span class="nl">signatureHelp</span><span class="p">?:</span> <span class="nx">SignatureHelpClientCapabilities</span><span class="p">;</span>

	<span class="cm">/**
	 * Capabilities specific to the `textDocument/declaration` request.
	 *
	 * @since 3.14.0
	 */</span>
	<span class="nl">declaration</span><span class="p">?:</span> <span class="nx">DeclarationClientCapabilities</span><span class="p">;</span>

	<span class="cm">/**
	 * Capabilities specific to the `textDocument/definition` request.
	 */</span>
	<span class="nl">definition</span><span class="p">?:</span> <span class="nx">DefinitionClientCapabilities</span><span class="p">;</span>

	<span class="cm">/**
	 * Capabilities specific to the `textDocument/typeDefinition` request.
	 *
	 * @since 3.6.0
	 */</span>
	<span class="nl">typeDefinition</span><span class="p">?:</span> <span class="nx">TypeDefinitionClientCapabilities</span><span class="p">;</span>

	<span class="cm">/**
	 * Capabilities specific to the `textDocument/implementation` request.
	 *
	 * @since 3.6.0
	 */</span>
	<span class="nl">implementation</span><span class="p">?:</span> <span class="nx">ImplementationClientCapabilities</span><span class="p">;</span>

	<span class="cm">/**
	 * Capabilities specific to the `textDocument/references` request.
	 */</span>
	<span class="nl">references</span><span class="p">?:</span> <span class="nx">ReferenceClientCapabilities</span><span class="p">;</span>

	<span class="cm">/**
	 * Capabilities specific to the `textDocument/documentHighlight` request.
	 */</span>
	<span class="nl">documentHighlight</span><span class="p">?:</span> <span class="nx">DocumentHighlightClientCapabilities</span><span class="p">;</span>

	<span class="cm">/**
	 * Capabilities specific to the `textDocument/documentSymbol` request.
	 */</span>
	<span class="nl">documentSymbol</span><span class="p">?:</span> <span class="nx">DocumentSymbolClientCapabilities</span><span class="p">;</span>

	<span class="cm">/**
	 * Capabilities specific to the `textDocument/codeAction` request.
	 */</span>
	<span class="nl">codeAction</span><span class="p">?:</span> <span class="nx">CodeActionClientCapabilities</span><span class="p">;</span>

	<span class="cm">/**
	 * Capabilities specific to the `textDocument/codeLens` request.
	 */</span>
	<span class="nl">codeLens</span><span class="p">?:</span> <span class="nx">CodeLensClientCapabilities</span><span class="p">;</span>

	<span class="cm">/**
	 * Capabilities specific to the `textDocument/documentLink` request.
	 */</span>
	<span class="nl">documentLink</span><span class="p">?:</span> <span class="nx">DocumentLinkClientCapabilities</span><span class="p">;</span>

	<span class="cm">/**
	 * Capabilities specific to the `textDocument/documentColor` and the
	 * `textDocument/colorPresentation` request.
	 *
	 * @since 3.6.0
	 */</span>
	<span class="nl">colorProvider</span><span class="p">?:</span> <span class="nx">DocumentColorClientCapabilities</span><span class="p">;</span>

	<span class="cm">/**
	 * Capabilities specific to the `textDocument/formatting` request.
	 */</span>
	<span class="nl">formatting</span><span class="p">?:</span> <span class="nx">DocumentFormattingClientCapabilities</span><span class="p">;</span>

	<span class="cm">/**
	 * Capabilities specific to the `textDocument/rangeFormatting` request.
	 */</span>
	<span class="nl">rangeFormatting</span><span class="p">?:</span> <span class="nx">DocumentRangeFormattingClientCapabilities</span><span class="p">;</span>

	<span class="cm">/** request.
	 * Capabilities specific to the `textDocument/onTypeFormatting` request.
	 */</span>
	<span class="nl">onTypeFormatting</span><span class="p">?:</span> <span class="nx">DocumentOnTypeFormattingClientCapabilities</span><span class="p">;</span>

	<span class="cm">/**
	 * Capabilities specific to the `textDocument/rename` request.
	 */</span>
	<span class="nl">rename</span><span class="p">?:</span> <span class="nx">RenameClientCapabilities</span><span class="p">;</span>

	<span class="cm">/**
	 * Capabilities specific to the `textDocument/publishDiagnostics`
	 * notification.
	 */</span>
	<span class="nl">publishDiagnostics</span><span class="p">?:</span> <span class="nx">PublishDiagnosticsClientCapabilities</span><span class="p">;</span>

	<span class="cm">/**
	 * Capabilities specific to the `textDocument/foldingRange` request.
	 *
	 * @since 3.10.0
	 */</span>
	<span class="nl">foldingRange</span><span class="p">?:</span> <span class="nx">FoldingRangeClientCapabilities</span><span class="p">;</span>

	<span class="cm">/**
	 * Capabilities specific to the `textDocument/selectionRange` request.
	 *
	 * @since 3.15.0
	 */</span>
	<span class="nl">selectionRange</span><span class="p">?:</span> <span class="nx">SelectionRangeClientCapabilities</span><span class="p">;</span>

	<span class="cm">/**
	 * Capabilities specific to the `textDocument/linkedEditingRange` request.
	 *
	 * @since 3.16.0
	 */</span>
	<span class="nl">linkedEditingRange</span><span class="p">?:</span> <span class="nx">LinkedEditingRangeClientCapabilities</span><span class="p">;</span>

	<span class="cm">/**
	 * Capabilities specific to the various call hierarchy requests.
	 *
	 * @since 3.16.0
	 */</span>
	<span class="nl">callHierarchy</span><span class="p">?:</span> <span class="nx">CallHierarchyClientCapabilities</span><span class="p">;</span>

	<span class="cm">/**
	 * Capabilities specific to the various semantic token requests.
	 *
	 * @since 3.16.0
	 */</span>
	<span class="nl">semanticTokens</span><span class="p">?:</span> <span class="nx">SemanticTokensClientCapabilities</span><span class="p">;</span>

	<span class="cm">/**
	 * Capabilities specific to the `textDocument/moniker` request.
	 *
	 * @since 3.16.0
	 */</span>
	<span class="nl">moniker</span><span class="p">?:</span> <span class="nx">MonikerClientCapabilities</span><span class="p">;</span>

	<span class="cm">/**
	 * Capabilities specific to the various type hierarchy requests.
	 *
	 * @since 3.17.0
	 */</span>
	<span class="nl">typeHierarchy</span><span class="p">?:</span> <span class="nx">TypeHierarchyClientCapabilities</span><span class="p">;</span>

	<span class="cm">/**
	 * Capabilities specific to the `textDocument/inlineValue` request.
	 *
	 * @since 3.17.0
	 */</span>
	<span class="nl">inlineValue</span><span class="p">?:</span> <span class="nx">InlineValueClientCapabilities</span><span class="p">;</span>

	<span class="cm">/**
	 * Capabilities specific to the `textDocument/inlayHint` request.
	 *
	 * @since 3.17.0
	 */</span>
	<span class="nl">inlayHint</span><span class="p">?:</span> <span class="nx">InlayHintClientCapabilities</span><span class="p">;</span>

	<span class="cm">/**
	 * Capabilities specific to the diagnostic pull model.
	 *
	 * @since 3.17.0
	 */</span>
	<span class="nl">diagnostic</span><span class="p">?:</span> <span class="nx">DiagnosticClientCapabilities</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<h5 id="notebookdocumentclientcapabilities">NotebookDocumentClientCapabilities</h5>

<p><code class="language-plaintext highlighter-rouge">NotebookDocumentClientCapabilities</code> define capabilities the editor / tool provides on notebook documents.</p>

<div class="anchorHolder"><a href="#notebookDocumentClientCapabilities" name="notebookDocumentClientCapabilities" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * Capabilities specific to the notebook document support.
 *
 * @since 3.17.0
 */</span>
<span class="k">export</span> <span class="kr">interface</span> <span class="nx">NotebookDocumentClientCapabilities</span> <span class="p">{</span>
	<span class="cm">/**
	 * Capabilities specific to notebook document synchronization
	 *
	 * @since 3.17.0
	 */</span>
	<span class="nl">synchronization</span><span class="p">:</span> <span class="nx">NotebookDocumentSyncClientCapabilities</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<p><code class="language-plaintext highlighter-rouge">ClientCapabilities</code> define capabilities for dynamic registration, workspace and text document features the client supports. The <code class="language-plaintext highlighter-rouge">experimental</code> can be used to pass experimental capabilities under development. For future compatibility a <code class="language-plaintext highlighter-rouge">ClientCapabilities</code> object literal can have more properties set than currently defined. Servers receiving a <code class="language-plaintext highlighter-rouge">ClientCapabilities</code> object literal with unknown properties should ignore these properties. A missing property should be interpreted as an absence of the capability. If a missing property normally defines sub properties, all missing sub properties should be interpreted as an absence of the corresponding capability.</p>

<p>Client capabilities got introduced with version 3.0 of the protocol. They therefore only describe capabilities that got introduced in 3.x or later. Capabilities that existed in the 2.x version of the protocol are still mandatory for clients. Clients cannot opt out of providing them. So even if a client omits the <code class="language-plaintext highlighter-rouge">ClientCapabilities.textDocument.synchronization</code> it is still required that the client provides text document synchronization (e.g. open, changed and close notifications).</p>

<div class="anchorHolder"><a href="#clientCapabilities" name="clientCapabilities" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="kr">interface</span> <span class="nx">ClientCapabilities</span> <span class="p">{</span>
	<span class="cm">/**
	 * Workspace specific client capabilities.
	 */</span>
	<span class="nl">workspace</span><span class="p">?:</span> <span class="p">{</span>
		<span class="cm">/**
		 * The client supports applying batch edits
		 * to the workspace by supporting the request
		 * 'workspace/applyEdit'
		 */</span>
		<span class="nx">applyEdit</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>

		<span class="cm">/**
		 * Capabilities specific to `WorkspaceEdit`s
		 */</span>
		<span class="nl">workspaceEdit</span><span class="p">?:</span> <span class="nx">WorkspaceEditClientCapabilities</span><span class="p">;</span>

		<span class="cm">/**
		 * Capabilities specific to the `workspace/didChangeConfiguration`
		 * notification.
		 */</span>
		<span class="nl">didChangeConfiguration</span><span class="p">?:</span> <span class="nx">DidChangeConfigurationClientCapabilities</span><span class="p">;</span>

		<span class="cm">/**
		 * Capabilities specific to the `workspace/didChangeWatchedFiles`
		 * notification.
		 */</span>
		<span class="nl">didChangeWatchedFiles</span><span class="p">?:</span> <span class="nx">DidChangeWatchedFilesClientCapabilities</span><span class="p">;</span>

		<span class="cm">/**
		 * Capabilities specific to the `workspace/symbol` request.
		 */</span>
		<span class="nl">symbol</span><span class="p">?:</span> <span class="nx">WorkspaceSymbolClientCapabilities</span><span class="p">;</span>

		<span class="cm">/**
		 * Capabilities specific to the `workspace/executeCommand` request.
		 */</span>
		<span class="nl">executeCommand</span><span class="p">?:</span> <span class="nx">ExecuteCommandClientCapabilities</span><span class="p">;</span>

		<span class="cm">/**
		 * The client has support for workspace folders.
		 *
		 * @since 3.6.0
		 */</span>
		<span class="nl">workspaceFolders</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>

		<span class="cm">/**
		 * The client supports `workspace/configuration` requests.
		 *
		 * @since 3.6.0
		 */</span>
		<span class="nl">configuration</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>

		<span class="cm">/**
		 * Capabilities specific to the semantic token requests scoped to the
		 * workspace.
		 *
		 * @since 3.16.0
		 */</span>
		 <span class="nl">semanticTokens</span><span class="p">?:</span> <span class="nx">SemanticTokensWorkspaceClientCapabilities</span><span class="p">;</span>

		<span class="cm">/**
		 * Capabilities specific to the code lens requests scoped to the
		 * workspace.
		 *
		 * @since 3.16.0
		 */</span>
		<span class="nl">codeLens</span><span class="p">?:</span> <span class="nx">CodeLensWorkspaceClientCapabilities</span><span class="p">;</span>

		<span class="cm">/**
		 * The client has support for file requests/notifications.
		 *
		 * @since 3.16.0
		 */</span>
		<span class="nl">fileOperations</span><span class="p">?:</span> <span class="p">{</span>
			<span class="cm">/**
			 * Whether the client supports dynamic registration for file
			 * requests/notifications.
			 */</span>
			<span class="nx">dynamicRegistration</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>

			<span class="cm">/**
			 * The client has support for sending didCreateFiles notifications.
			 */</span>
			<span class="nl">didCreate</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>

			<span class="cm">/**
			 * The client has support for sending willCreateFiles requests.
			 */</span>
			<span class="nl">willCreate</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>

			<span class="cm">/**
			 * The client has support for sending didRenameFiles notifications.
			 */</span>
			<span class="nl">didRename</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>

			<span class="cm">/**
			 * The client has support for sending willRenameFiles requests.
			 */</span>
			<span class="nl">willRename</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>

			<span class="cm">/**
			 * The client has support for sending didDeleteFiles notifications.
			 */</span>
			<span class="nl">didDelete</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>

			<span class="cm">/**
			 * The client has support for sending willDeleteFiles requests.
			 */</span>
			<span class="nl">willDelete</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>
		<span class="p">};</span>

		<span class="cm">/**
		 * Client workspace capabilities specific to inline values.
		 *
		 * @since 3.17.0
		 */</span>
		<span class="nl">inlineValue</span><span class="p">?:</span> <span class="nx">InlineValueWorkspaceClientCapabilities</span><span class="p">;</span>

		<span class="cm">/**
		 * Client workspace capabilities specific to inlay hints.
		 *
		 * @since 3.17.0
		 */</span>
		<span class="nl">inlayHint</span><span class="p">?:</span> <span class="nx">InlayHintWorkspaceClientCapabilities</span><span class="p">;</span>

		<span class="cm">/**
		 * Client workspace capabilities specific to diagnostics.
		 *
		 * @since 3.17.0.
		 */</span>
		<span class="nl">diagnostics</span><span class="p">?:</span> <span class="nx">DiagnosticWorkspaceClientCapabilities</span><span class="p">;</span>
	<span class="p">};</span>

	<span class="cm">/**
	 * Text document specific client capabilities.
	 */</span>
	<span class="nl">textDocument</span><span class="p">?:</span> <span class="nx">TextDocumentClientCapabilities</span><span class="p">;</span>

	<span class="cm">/**
	 * Capabilities specific to the notebook document support.
	 *
	 * @since 3.17.0
	 */</span>
	<span class="nl">notebookDocument</span><span class="p">?:</span> <span class="nx">NotebookDocumentClientCapabilities</span><span class="p">;</span>

	<span class="cm">/**
	 * Window specific client capabilities.
	 */</span>
	<span class="nl">window</span><span class="p">?:</span> <span class="p">{</span>
		<span class="cm">/**
		 * It indicates whether the client supports server initiated
		 * progress using the `window/workDoneProgress/create` request.
		 *
		 * The capability also controls Whether client supports handling
		 * of progress notifications. If set servers are allowed to report a
		 * `workDoneProgress` property in the request specific server
		 * capabilities.
		 *
		 * @since 3.15.0
		 */</span>
		<span class="nx">workDoneProgress</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>

		<span class="cm">/**
		 * Capabilities specific to the showMessage request
		 *
		 * @since 3.16.0
		 */</span>
		<span class="nl">showMessage</span><span class="p">?:</span> <span class="nx">ShowMessageRequestClientCapabilities</span><span class="p">;</span>

		<span class="cm">/**
		 * Client capabilities for the show document request.
		 *
		 * @since 3.16.0
		 */</span>
		<span class="nl">showDocument</span><span class="p">?:</span> <span class="nx">ShowDocumentClientCapabilities</span><span class="p">;</span>
	<span class="p">};</span>

	<span class="cm">/**
	 * General client capabilities.
	 *
	 * @since 3.16.0
	 */</span>
	<span class="nl">general</span><span class="p">?:</span> <span class="p">{</span>
		<span class="cm">/**
		 * Client capability that signals how the client
		 * handles stale requests (e.g. a request
		 * for which the client will not process the response
		 * anymore since the information is outdated).
		 *
		 * @since 3.17.0
		 */</span>
		<span class="nx">staleRequestSupport</span><span class="p">?:</span> <span class="p">{</span>
			<span class="cm">/**
			 * The client will actively cancel the request.
			 */</span>
			<span class="na">cancel</span><span class="p">:</span> <span class="nx">boolean</span><span class="p">;</span>

			<span class="cm">/**
			 * The list of requests for which the client
			 * will retry the request if it receives a
			 * response with error code `ContentModified``
			 */</span>
			 <span class="nl">retryOnContentModified</span><span class="p">:</span> <span class="kr">string</span><span class="p">[];</span>
		<span class="p">}</span>

		<span class="cm">/**
		 * Client capabilities specific to regular expressions.
		 *
		 * @since 3.16.0
		 */</span>
		<span class="nl">regularExpressions</span><span class="p">?:</span> <span class="nx">RegularExpressionsClientCapabilities</span><span class="p">;</span>

		<span class="cm">/**
		 * Client capabilities specific to the client's markdown parser.
		 *
		 * @since 3.16.0
		 */</span>
		<span class="nl">markdown</span><span class="p">?:</span> <span class="nx">MarkdownClientCapabilities</span><span class="p">;</span>

		<span class="cm">/**
		 * The position encodings supported by the client. Client and server
		 * have to agree on the same position encoding to ensure that offsets
		 * (e.g. character position in a line) are interpreted the same on both
		 * side.
		 *
		 * To keep the protocol backwards compatible the following applies: if
		 * the value 'utf-16' is missing from the array of position encodings
		 * servers can assume that the client supports UTF-16. UTF-16 is
		 * therefore a mandatory encoding.
		 *
		 * If omitted it defaults to ['utf-16'].
		 *
		 * Implementation considerations: since the conversion from one encoding
		 * into another requires the content of the file / line the conversion
		 * is best done where the file is read which is usually on the server
		 * side.
		 *
		 * @since 3.17.0
		 */</span>
		<span class="nl">positionEncodings</span><span class="p">?:</span> <span class="nx">PositionEncodingKind</span><span class="p">[];</span>
	<span class="p">};</span>

	<span class="cm">/**
	 * Experimental client capabilities.
	 */</span>
	<span class="nl">experimental</span><span class="p">?:</span> <span class="nx">LSPAny</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Response</em>:</p>
<ul>
  <li>result: <code class="language-plaintext highlighter-rouge">InitializeResult</code> defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#initializeResult" name="initializeResult" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="kr">interface</span> <span class="nx">InitializeResult</span> <span class="p">{</span>
	<span class="cm">/**
	 * The capabilities the language server provides.
	 */</span>
	<span class="nl">capabilities</span><span class="p">:</span> <span class="nx">ServerCapabilities</span><span class="p">;</span>

	<span class="cm">/**
	 * Information about the server.
	 *
	 * @since 3.15.0
	 */</span>
	<span class="nl">serverInfo</span><span class="p">?:</span> <span class="p">{</span>
		<span class="cm">/**
		 * The name of the server as defined by the server.
		 */</span>
		<span class="na">name</span><span class="p">:</span> <span class="kr">string</span><span class="p">;</span>

		<span class="cm">/**
		 * The server's version as defined by the server.
		 */</span>
		<span class="nl">version</span><span class="p">?:</span> <span class="kr">string</span><span class="p">;</span>
	<span class="p">};</span>
<span class="p">}</span>
</code></pre></div></div>
<ul>
  <li>error.code:</li>
</ul>

<div class="anchorHolder"><a href="#initializeErrorCodes" name="initializeErrorCodes" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * Known error codes for an `InitializeErrorCodes`;
 */</span>
<span class="k">export</span> <span class="k">namespace</span> <span class="nx">InitializeErrorCodes</span> <span class="p">{</span>

	<span class="cm">/**
	 * If the protocol version provided by the client can't be handled by
	 * the server.
	 *
	 * @deprecated This initialize error got replaced by client capabilities.
	 * There is no version handshake in version 3.0x
	 */</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">unknownProtocolVersion</span><span class="p">:</span> <span class="mi">1</span> <span class="o">=</span> <span class="mi">1</span><span class="p">;</span>
<span class="p">}</span>

<span class="k">export</span> <span class="kd">type</span> <span class="nx">InitializeErrorCodes</span> <span class="o">=</span> <span class="mi">1</span><span class="p">;</span>
</code></pre></div></div>

<ul>
  <li>error.data:</li>
</ul>

<div class="anchorHolder"><a href="#initializeError" name="initializeError" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="kr">interface</span> <span class="nx">InitializeError</span> <span class="p">{</span>
	<span class="cm">/**
	 * Indicates whether the client execute the following retry logic:
	 * (1) show the message provided by the ResponseError to the user
	 * (2) user selects retry or cancel
	 * (3) if user selected retry the initialize method is sent again.
	 */</span>
	<span class="nl">retry</span><span class="p">:</span> <span class="nx">boolean</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<p>The server can signal the following capabilities:</p>

<div class="anchorHolder"><a href="#serverCapabilities" name="serverCapabilities" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="kr">interface</span> <span class="nx">ServerCapabilities</span> <span class="p">{</span>

	<span class="cm">/**
	 * The position encoding the server picked from the encodings offered
	 * by the client via the client capability `general.positionEncodings`.
	 *
	 * If the client didn't provide any position encodings the only valid
	 * value that a server can return is 'utf-16'.
	 *
	 * If omitted it defaults to 'utf-16'.
	 *
	 * @since 3.17.0
	 */</span>
	<span class="nl">positionEncoding</span><span class="p">?:</span> <span class="nx">PositionEncodingKind</span><span class="p">;</span>

	<span class="cm">/**
	 * Defines how text documents are synced. Is either a detailed structure
	 * defining each notification or for backwards compatibility the
	 * TextDocumentSyncKind number. If omitted it defaults to
	 * `TextDocumentSyncKind.None`.
	 */</span>
	<span class="nl">textDocumentSync</span><span class="p">?:</span> <span class="nx">TextDocumentSyncOptions</span> <span class="o">|</span> <span class="nx">TextDocumentSyncKind</span><span class="p">;</span>

	<span class="cm">/**
	 * Defines how notebook documents are synced.
	 *
	 * @since 3.17.0
	 */</span>
	<span class="nl">notebookDocumentSync</span><span class="p">?:</span> <span class="nx">NotebookDocumentSyncOptions</span>
		<span class="o">|</span> <span class="nx">NotebookDocumentSyncRegistrationOptions</span><span class="p">;</span>

	<span class="cm">/**
	 * The server provides completion support.
	 */</span>
	<span class="nl">completionProvider</span><span class="p">?:</span> <span class="nx">CompletionOptions</span><span class="p">;</span>

	<span class="cm">/**
	 * The server provides hover support.
	 */</span>
	<span class="nl">hoverProvider</span><span class="p">?:</span> <span class="nx">boolean</span> <span class="o">|</span> <span class="nx">HoverOptions</span><span class="p">;</span>

	<span class="cm">/**
	 * The server provides signature help support.
	 */</span>
	<span class="nl">signatureHelpProvider</span><span class="p">?:</span> <span class="nx">SignatureHelpOptions</span><span class="p">;</span>

	<span class="cm">/**
	 * The server provides go to declaration support.
	 *
	 * @since 3.14.0
	 */</span>
	<span class="nl">declarationProvider</span><span class="p">?:</span> <span class="nx">boolean</span> <span class="o">|</span> <span class="nx">DeclarationOptions</span>
		<span class="o">|</span> <span class="nx">DeclarationRegistrationOptions</span><span class="p">;</span>

	<span class="cm">/**
	 * The server provides goto definition support.
	 */</span>
	<span class="nl">definitionProvider</span><span class="p">?:</span> <span class="nx">boolean</span> <span class="o">|</span> <span class="nx">DefinitionOptions</span><span class="p">;</span>

	<span class="cm">/**
	 * The server provides goto type definition support.
	 *
	 * @since 3.6.0
	 */</span>
	<span class="nl">typeDefinitionProvider</span><span class="p">?:</span> <span class="nx">boolean</span> <span class="o">|</span> <span class="nx">TypeDefinitionOptions</span>
		<span class="o">|</span> <span class="nx">TypeDefinitionRegistrationOptions</span><span class="p">;</span>

	<span class="cm">/**
	 * The server provides goto implementation support.
	 *
	 * @since 3.6.0
	 */</span>
	<span class="nl">implementationProvider</span><span class="p">?:</span> <span class="nx">boolean</span> <span class="o">|</span> <span class="nx">ImplementationOptions</span>
		<span class="o">|</span> <span class="nx">ImplementationRegistrationOptions</span><span class="p">;</span>

	<span class="cm">/**
	 * The server provides find references support.
	 */</span>
	<span class="nl">referencesProvider</span><span class="p">?:</span> <span class="nx">boolean</span> <span class="o">|</span> <span class="nx">ReferenceOptions</span><span class="p">;</span>

	<span class="cm">/**
	 * The server provides document highlight support.
	 */</span>
	<span class="nl">documentHighlightProvider</span><span class="p">?:</span> <span class="nx">boolean</span> <span class="o">|</span> <span class="nx">DocumentHighlightOptions</span><span class="p">;</span>

	<span class="cm">/**
	 * The server provides document symbol support.
	 */</span>
	<span class="nl">documentSymbolProvider</span><span class="p">?:</span> <span class="nx">boolean</span> <span class="o">|</span> <span class="nx">DocumentSymbolOptions</span><span class="p">;</span>

	<span class="cm">/**
	 * The server provides code actions. The `CodeActionOptions` return type is
	 * only valid if the client signals code action literal support via the
	 * property `textDocument.codeAction.codeActionLiteralSupport`.
	 */</span>
	<span class="nl">codeActionProvider</span><span class="p">?:</span> <span class="nx">boolean</span> <span class="o">|</span> <span class="nx">CodeActionOptions</span><span class="p">;</span>

	<span class="cm">/**
	 * The server provides code lens.
	 */</span>
	<span class="nl">codeLensProvider</span><span class="p">?:</span> <span class="nx">CodeLensOptions</span><span class="p">;</span>

	<span class="cm">/**
	 * The server provides document link support.
	 */</span>
	<span class="nl">documentLinkProvider</span><span class="p">?:</span> <span class="nx">DocumentLinkOptions</span><span class="p">;</span>

	<span class="cm">/**
	 * The server provides color provider support.
	 *
	 * @since 3.6.0
	 */</span>
	<span class="nl">colorProvider</span><span class="p">?:</span> <span class="nx">boolean</span> <span class="o">|</span> <span class="nx">DocumentColorOptions</span>
		<span class="o">|</span> <span class="nx">DocumentColorRegistrationOptions</span><span class="p">;</span>

	<span class="cm">/**
	 * The server provides document formatting.
	 */</span>
	<span class="nl">documentFormattingProvider</span><span class="p">?:</span> <span class="nx">boolean</span> <span class="o">|</span> <span class="nx">DocumentFormattingOptions</span><span class="p">;</span>

	<span class="cm">/**
	 * The server provides document range formatting.
	 */</span>
	<span class="nl">documentRangeFormattingProvider</span><span class="p">?:</span> <span class="nx">boolean</span> <span class="o">|</span> <span class="nx">DocumentRangeFormattingOptions</span><span class="p">;</span>

	<span class="cm">/**
	 * The server provides document formatting on typing.
	 */</span>
	<span class="nl">documentOnTypeFormattingProvider</span><span class="p">?:</span> <span class="nx">DocumentOnTypeFormattingOptions</span><span class="p">;</span>

	<span class="cm">/**
	 * The server provides rename support. RenameOptions may only be
	 * specified if the client states that it supports
	 * `prepareSupport` in its initial `initialize` request.
	 */</span>
	<span class="nl">renameProvider</span><span class="p">?:</span> <span class="nx">boolean</span> <span class="o">|</span> <span class="nx">RenameOptions</span><span class="p">;</span>

	<span class="cm">/**
	 * The server provides folding provider support.
	 *
	 * @since 3.10.0
	 */</span>
	<span class="nl">foldingRangeProvider</span><span class="p">?:</span> <span class="nx">boolean</span> <span class="o">|</span> <span class="nx">FoldingRangeOptions</span>
		<span class="o">|</span> <span class="nx">FoldingRangeRegistrationOptions</span><span class="p">;</span>

	<span class="cm">/**
	 * The server provides execute command support.
	 */</span>
	<span class="nl">executeCommandProvider</span><span class="p">?:</span> <span class="nx">ExecuteCommandOptions</span><span class="p">;</span>

	<span class="cm">/**
	 * The server provides selection range support.
	 *
	 * @since 3.15.0
	 */</span>
	<span class="nl">selectionRangeProvider</span><span class="p">?:</span> <span class="nx">boolean</span> <span class="o">|</span> <span class="nx">SelectionRangeOptions</span>
		<span class="o">|</span> <span class="nx">SelectionRangeRegistrationOptions</span><span class="p">;</span>

	<span class="cm">/**
	 * The server provides linked editing range support.
	 *
	 * @since 3.16.0
	 */</span>
	<span class="nl">linkedEditingRangeProvider</span><span class="p">?:</span> <span class="nx">boolean</span> <span class="o">|</span> <span class="nx">LinkedEditingRangeOptions</span>
		<span class="o">|</span> <span class="nx">LinkedEditingRangeRegistrationOptions</span><span class="p">;</span>

	<span class="cm">/**
	 * The server provides call hierarchy support.
	 *
	 * @since 3.16.0
	 */</span>
	<span class="nl">callHierarchyProvider</span><span class="p">?:</span> <span class="nx">boolean</span> <span class="o">|</span> <span class="nx">CallHierarchyOptions</span>
		<span class="o">|</span> <span class="nx">CallHierarchyRegistrationOptions</span><span class="p">;</span>

	<span class="cm">/**
	 * The server provides semantic tokens support.
	 *
	 * @since 3.16.0
	 */</span>
	<span class="nl">semanticTokensProvider</span><span class="p">?:</span> <span class="nx">SemanticTokensOptions</span>
		<span class="o">|</span> <span class="nx">SemanticTokensRegistrationOptions</span><span class="p">;</span>

	<span class="cm">/**
	 * Whether server provides moniker support.
	 *
	 * @since 3.16.0
	 */</span>
	<span class="nl">monikerProvider</span><span class="p">?:</span> <span class="nx">boolean</span> <span class="o">|</span> <span class="nx">MonikerOptions</span> <span class="o">|</span> <span class="nx">MonikerRegistrationOptions</span><span class="p">;</span>

	<span class="cm">/**
	 * The server provides type hierarchy support.
	 *
	 * @since 3.17.0
	 */</span>
	<span class="nl">typeHierarchyProvider</span><span class="p">?:</span> <span class="nx">boolean</span> <span class="o">|</span> <span class="nx">TypeHierarchyOptions</span>
		 <span class="o">|</span> <span class="nx">TypeHierarchyRegistrationOptions</span><span class="p">;</span>

	<span class="cm">/**
	 * The server provides inline values.
	 *
	 * @since 3.17.0
	 */</span>
	<span class="nl">inlineValueProvider</span><span class="p">?:</span> <span class="nx">boolean</span> <span class="o">|</span> <span class="nx">InlineValueOptions</span>
		 <span class="o">|</span> <span class="nx">InlineValueRegistrationOptions</span><span class="p">;</span>

	<span class="cm">/**
	 * The server provides inlay hints.
	 *
	 * @since 3.17.0
	 */</span>
	<span class="nl">inlayHintProvider</span><span class="p">?:</span> <span class="nx">boolean</span> <span class="o">|</span> <span class="nx">InlayHintOptions</span>
		 <span class="o">|</span> <span class="nx">InlayHintRegistrationOptions</span><span class="p">;</span>

	<span class="cm">/**
	 * The server has support for pull model diagnostics.
	 *
	 * @since 3.17.0
	 */</span>
	<span class="nl">diagnosticProvider</span><span class="p">?:</span> <span class="nx">DiagnosticOptions</span> <span class="o">|</span> <span class="nx">DiagnosticRegistrationOptions</span><span class="p">;</span>

	<span class="cm">/**
	 * The server provides workspace symbol support.
	 */</span>
	<span class="nl">workspaceSymbolProvider</span><span class="p">?:</span> <span class="nx">boolean</span> <span class="o">|</span> <span class="nx">WorkspaceSymbolOptions</span><span class="p">;</span>

	<span class="cm">/**
	 * Workspace specific server capabilities
	 */</span>
	<span class="nl">workspace</span><span class="p">?:</span> <span class="p">{</span>
		<span class="cm">/**
		 * The server supports workspace folder.
		 *
		 * @since 3.6.0
		 */</span>
		<span class="nx">workspaceFolders</span><span class="p">?:</span> <span class="nx">WorkspaceFoldersServerCapabilities</span><span class="p">;</span>

		<span class="cm">/**
		 * The server is interested in file notifications/requests.
		 *
		 * @since 3.16.0
		 */</span>
		<span class="nl">fileOperations</span><span class="p">?:</span> <span class="p">{</span>
			<span class="cm">/**
			 * The server is interested in receiving didCreateFiles
			 * notifications.
			 */</span>
			<span class="nx">didCreate</span><span class="p">?:</span> <span class="nx">FileOperationRegistrationOptions</span><span class="p">;</span>

			<span class="cm">/**
			 * The server is interested in receiving willCreateFiles requests.
			 */</span>
			<span class="nl">willCreate</span><span class="p">?:</span> <span class="nx">FileOperationRegistrationOptions</span><span class="p">;</span>

			<span class="cm">/**
			 * The server is interested in receiving didRenameFiles
			 * notifications.
			 */</span>
			<span class="nl">didRename</span><span class="p">?:</span> <span class="nx">FileOperationRegistrationOptions</span><span class="p">;</span>

			<span class="cm">/**
			 * The server is interested in receiving willRenameFiles requests.
			 */</span>
			<span class="nl">willRename</span><span class="p">?:</span> <span class="nx">FileOperationRegistrationOptions</span><span class="p">;</span>

			<span class="cm">/**
			 * The server is interested in receiving didDeleteFiles file
			 * notifications.
			 */</span>
			<span class="nl">didDelete</span><span class="p">?:</span> <span class="nx">FileOperationRegistrationOptions</span><span class="p">;</span>

			<span class="cm">/**
			 * The server is interested in receiving willDeleteFiles file
			 * requests.
			 */</span>
			<span class="nl">willDelete</span><span class="p">?:</span> <span class="nx">FileOperationRegistrationOptions</span><span class="p">;</span>
		<span class="p">};</span>
	<span class="p">};</span>

	<span class="cm">/**
	 * Experimental server capabilities.
	 */</span>
	<span class="nl">experimental</span><span class="p">?:</span> <span class="nx">LSPAny</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<h4 id="initialized-notification-arrow_right"><a href="#initialized" name="initialized" class="anchor">Initialized Notification (<img class="emoji" title=":arrow_right:" alt=":arrow_right:" src="https://github.githubassets.com/images/icons/emoji/unicode/27a1.png" height="20" width="20">)</a></h4>

<p>The initialized notification is sent from the client to the server after the client received the result of the <code class="language-plaintext highlighter-rouge">initialize</code> request but before the client is sending any other request or notification to the server. The server can use the <code class="language-plaintext highlighter-rouge">initialized</code> notification, for example, to dynamically register capabilities. The <code class="language-plaintext highlighter-rouge">initialized</code> notification may only be sent once.</p>

<p><em>Notification</em>:</p>
<ul>
  <li>method: ‘initialized’</li>
  <li>params: <code class="language-plaintext highlighter-rouge">InitializedParams</code> defined as follows:</li>
</ul>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="kr">interface</span> <span class="nx">InitializedParams</span> <span class="p">{</span>
<span class="p">}</span>
</code></pre></div></div>

<h4 id="register-capability-arrow_right_hook"><a href="#client_registerCapability" name="client_registerCapability" class="anchor">Register Capability (<img class="emoji" title=":arrow_right_hook:" alt=":arrow_right_hook:" src="https://github.githubassets.com/images/icons/emoji/unicode/21aa.png" height="20" width="20">)</a></h4>

<p>The <code class="language-plaintext highlighter-rouge">client/registerCapability</code> request is sent from the server to the client to register for a new capability on the client side. Not all clients need to support dynamic capability registration. A client opts in via the <code class="language-plaintext highlighter-rouge">dynamicRegistration</code> property on the specific client capabilities. A client can even provide dynamic registration for capability A but not for capability B (see <code class="language-plaintext highlighter-rouge">TextDocumentClientCapabilities</code> as an example).</p>

<p>Server must not register the same capability both statically through the initialize result and dynamically for the same document selector. If a server wants to support both static and dynamic registration it needs to check the client capability in the initialize request and only register the capability statically if the client doesn’t support dynamic registration for that capability.</p>

<p><em>Request</em>:</p>
<ul>
  <li>method: ‘client/registerCapability’</li>
  <li>params: <code class="language-plaintext highlighter-rouge">RegistrationParams</code>
</li>
</ul>

<p>Where <code class="language-plaintext highlighter-rouge">RegistrationParams</code> are defined as follows:</p>

<div class="anchorHolder"><a href="#registration" name="registration" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * General parameters to register for a capability.
 */</span>
<span class="k">export</span> <span class="kr">interface</span> <span class="nx">Registration</span> <span class="p">{</span>
	<span class="cm">/**
	 * The id used to register the request. The id can be used to deregister
	 * the request again.
	 */</span>
	<span class="nl">id</span><span class="p">:</span> <span class="kr">string</span><span class="p">;</span>

	<span class="cm">/**
	 * The method / capability to register for.
	 */</span>
	<span class="nl">method</span><span class="p">:</span> <span class="kr">string</span><span class="p">;</span>

	<span class="cm">/**
	 * Options necessary for the registration.
	 */</span>
	<span class="nl">registerOptions</span><span class="p">?:</span> <span class="nx">LSPAny</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<div class="anchorHolder"><a href="#registrationParams" name="registrationParams" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">RegistrationParams</span> <span class="p">{</span>
	<span class="nl">registrations</span><span class="p">:</span> <span class="nx">Registration</span><span class="p">[];</span>
<span class="p">}</span>
</code></pre></div></div>

<p>Since most of the registration options require to specify a document selector there is a base interface that can be used. See <code class="language-plaintext highlighter-rouge">TextDocumentRegistrationOptions</code>.</p>

<p>An example JSON-RPC message to register dynamically for the <code class="language-plaintext highlighter-rouge">textDocument/willSaveWaitUntil</code> feature on the client side is as follows (only details shown):</p>

<div class="language-json highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="p">{</span><span class="w">
	</span><span class="nl">"method"</span><span class="p">:</span><span class="w"> </span><span class="s2">"client/registerCapability"</span><span class="p">,</span><span class="w">
	</span><span class="nl">"params"</span><span class="p">:</span><span class="w"> </span><span class="p">{</span><span class="w">
		</span><span class="nl">"registrations"</span><span class="p">:</span><span class="w"> </span><span class="p">[</span><span class="w">
			</span><span class="p">{</span><span class="w">
				</span><span class="nl">"id"</span><span class="p">:</span><span class="w"> </span><span class="s2">"79eee87c-c409-4664-8102-e03263673f6f"</span><span class="p">,</span><span class="w">
				</span><span class="nl">"method"</span><span class="p">:</span><span class="w"> </span><span class="s2">"textDocument/willSaveWaitUntil"</span><span class="p">,</span><span class="w">
				</span><span class="nl">"registerOptions"</span><span class="p">:</span><span class="w"> </span><span class="p">{</span><span class="w">
					</span><span class="nl">"documentSelector"</span><span class="p">:</span><span class="w"> </span><span class="p">[</span><span class="w">
						</span><span class="p">{</span><span class="w"> </span><span class="nl">"language"</span><span class="p">:</span><span class="w"> </span><span class="s2">"javascript"</span><span class="w"> </span><span class="p">}</span><span class="w">
					</span><span class="p">]</span><span class="w">
				</span><span class="p">}</span><span class="w">
			</span><span class="p">}</span><span class="w">
		</span><span class="p">]</span><span class="w">
	</span><span class="p">}</span><span class="w">
</span><span class="p">}</span><span class="w">
</span></code></pre></div></div>

<p>This message is sent from the server to the client and after the client has successfully executed the request further <code class="language-plaintext highlighter-rouge">textDocument/willSaveWaitUntil</code> requests for JavaScript text documents are sent from the client to the server.</p>

<p><em>Response</em>:</p>
<ul>
  <li>result: void.</li>
  <li>error: code and message set in case an exception happens during the request.</li>
</ul>

<p><code class="language-plaintext highlighter-rouge">StaticRegistrationOptions</code> can be used to register a feature in the initialize result with a given server control ID to be able to un-register the feature later on.</p>

<div class="anchorHolder"><a href="#staticRegistrationOptions" name="staticRegistrationOptions" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * Static registration options to be returned in the initialize request.
 */</span>
<span class="k">export</span> <span class="kr">interface</span> <span class="nx">StaticRegistrationOptions</span> <span class="p">{</span>
	<span class="cm">/**
	 * The id used to register the request. The id can be used to deregister
	 * the request again. See also Registration#id.
	 */</span>
	<span class="nl">id</span><span class="p">?:</span> <span class="kr">string</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<p><code class="language-plaintext highlighter-rouge">TextDocumentRegistrationOptions</code> can be used to dynamically register for requests for a set of text documents.</p>

<div class="anchorHolder"><a href="#textDocumentRegistrationOptions" name="textDocumentRegistrationOptions" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * General text document registration options.
 */</span>
<span class="k">export</span> <span class="kr">interface</span> <span class="nx">TextDocumentRegistrationOptions</span> <span class="p">{</span>
	<span class="cm">/**
	 * A document selector to identify the scope of the registration. If set to
	 * null the document selector provided on the client side will be used.
	 */</span>
	<span class="nl">documentSelector</span><span class="p">:</span> <span class="nx">DocumentSelector</span> <span class="o">|</span> <span class="kc">null</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>
<h4 id="unregister-capability-arrow_right_hook"><a href="#client_unregisterCapability" name="client_unregisterCapability" class="anchor">Unregister Capability (<img class="emoji" title=":arrow_right_hook:" alt=":arrow_right_hook:" src="https://github.githubassets.com/images/icons/emoji/unicode/21aa.png" height="20" width="20">)</a></h4>

<p>The <code class="language-plaintext highlighter-rouge">client/unregisterCapability</code> request is sent from the server to the client to unregister a previously registered capability.</p>

<p><em>Request</em>:</p>
<ul>
  <li>method: ‘client/unregisterCapability’</li>
  <li>params: <code class="language-plaintext highlighter-rouge">UnregistrationParams</code>
</li>
</ul>

<p>Where <code class="language-plaintext highlighter-rouge">UnregistrationParams</code> are defined as follows:</p>

<div class="anchorHolder"><a href="#unregistration" name="unregistration" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * General parameters to unregister a capability.
 */</span>
<span class="k">export</span> <span class="kr">interface</span> <span class="nx">Unregistration</span> <span class="p">{</span>
	<span class="cm">/**
	 * The id used to unregister the request or notification. Usually an id
	 * provided during the register request.
	 */</span>
	<span class="nl">id</span><span class="p">:</span> <span class="kr">string</span><span class="p">;</span>

	<span class="cm">/**
	 * The method / capability to unregister for.
	 */</span>
	<span class="nl">method</span><span class="p">:</span> <span class="kr">string</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<div class="anchorHolder"><a href="#unregistrationParams" name="unregistrationParams" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">UnregistrationParams</span> <span class="p">{</span>
	<span class="c1">// This should correctly be named `unregistrations`. However changing this</span>
	<span class="c1">// is a breaking change and needs to wait until we deliver a 4.x version</span>
	<span class="c1">// of the specification.</span>
	<span class="nl">unregisterations</span><span class="p">:</span> <span class="nx">Unregistration</span><span class="p">[];</span>
<span class="p">}</span>
</code></pre></div></div>

<p>An example JSON-RPC message to unregister the above registered <code class="language-plaintext highlighter-rouge">textDocument/willSaveWaitUntil</code> feature looks like this:</p>

<div class="language-json highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="p">{</span><span class="w">
	</span><span class="nl">"method"</span><span class="p">:</span><span class="w"> </span><span class="s2">"client/unregisterCapability"</span><span class="p">,</span><span class="w">
	</span><span class="nl">"params"</span><span class="p">:</span><span class="w"> </span><span class="p">{</span><span class="w">
		</span><span class="nl">"unregisterations"</span><span class="p">:</span><span class="w"> </span><span class="p">[</span><span class="w">
			</span><span class="p">{</span><span class="w">
				</span><span class="nl">"id"</span><span class="p">:</span><span class="w"> </span><span class="s2">"79eee87c-c409-4664-8102-e03263673f6f"</span><span class="p">,</span><span class="w">
				</span><span class="nl">"method"</span><span class="p">:</span><span class="w"> </span><span class="s2">"textDocument/willSaveWaitUntil"</span><span class="w">
			</span><span class="p">}</span><span class="w">
		</span><span class="p">]</span><span class="w">
	</span><span class="p">}</span><span class="w">
</span><span class="p">}</span><span class="w">
</span></code></pre></div></div>
<p><em>Response</em>:</p>
<ul>
  <li>result: void.</li>
  <li>error: code and message set in case an exception happens during the request.</li>
</ul>

<h4 id="settrace-notification-arrow_right"><a href="#setTrace" name="setTrace" class="anchor">SetTrace Notification (<img class="emoji" title=":arrow_right:" alt=":arrow_right:" src="https://github.githubassets.com/images/icons/emoji/unicode/27a1.png" height="20" width="20">)</a></h4>

<p>A notification that should be used by the client to modify the trace setting of the server.</p>

<p><em>Notification</em>:</p>
<ul>
  <li>method: ‘$/setTrace’</li>
  <li>params: <code class="language-plaintext highlighter-rouge">SetTraceParams</code> defined as follows:</li>
</ul>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="kr">interface</span> <span class="nx">SetTraceParams</span> <span class="p">{</span>
	<span class="cm">/**
	 * The new value that should be assigned to the trace setting.
	 */</span>
	<span class="nl">value</span><span class="p">:</span> <span class="nx">TraceValue</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<h4 id="logtrace-notification-arrow_left"><a href="#logTrace" name="logTrace" class="anchor">LogTrace Notification (<img class="emoji" title=":arrow_left:" alt=":arrow_left:" src="https://github.githubassets.com/images/icons/emoji/unicode/2b05.png" height="20" width="20">)</a></h4>

<p>A notification to log the trace of the server’s execution.
The amount and content of these notifications depends on the current <code class="language-plaintext highlighter-rouge">trace</code> configuration.
If <code class="language-plaintext highlighter-rouge">trace</code> is <code class="language-plaintext highlighter-rouge">'off'</code>, the server should not send any <code class="language-plaintext highlighter-rouge">logTrace</code> notification.
If <code class="language-plaintext highlighter-rouge">trace</code> is <code class="language-plaintext highlighter-rouge">'messages'</code>, the server should not add the <code class="language-plaintext highlighter-rouge">'verbose'</code> field in the <code class="language-plaintext highlighter-rouge">LogTraceParams</code>.</p>

<p><code class="language-plaintext highlighter-rouge">$/logTrace</code> should be used for systematic trace reporting. For single debugging messages, the server should send <a href="#window_logMessage"><code class="language-plaintext highlighter-rouge">window/logMessage</code></a> notifications.</p>

<p><em>Notification</em>:</p>
<ul>
  <li>method: ‘$/logTrace’</li>
  <li>params: <code class="language-plaintext highlighter-rouge">LogTraceParams</code> defined as follows:</li>
</ul>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="kr">interface</span> <span class="nx">LogTraceParams</span> <span class="p">{</span>
	<span class="cm">/**
	 * The message to be logged.
	 */</span>
	<span class="nl">message</span><span class="p">:</span> <span class="kr">string</span><span class="p">;</span>
	<span class="cm">/**
	 * Additional information that can be computed if the `trace` configuration
	 * is set to `'verbose'`
	 */</span>
	<span class="nl">verbose</span><span class="p">?:</span> <span class="kr">string</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<h4 id="shutdown-request-leftwards_arrow_with_hook"><a href="#shutdown" name="shutdown" class="anchor">Shutdown Request (<img class="emoji" title=":leftwards_arrow_with_hook:" alt=":leftwards_arrow_with_hook:" src="https://github.githubassets.com/images/icons/emoji/unicode/21a9.png" height="20" width="20">)</a></h4>

<p>The shutdown request is sent from the client to the server. It asks the server to shut down, but to not exit (otherwise the response might not be delivered correctly to the client). There is a separate exit notification that asks the server to exit. Clients must not send any notifications other than <code class="language-plaintext highlighter-rouge">exit</code> or requests to a server to which they have sent a shutdown request. Clients should also wait with sending the <code class="language-plaintext highlighter-rouge">exit</code> notification until they have received a response from the <code class="language-plaintext highlighter-rouge">shutdown</code> request.</p>

<p>If a server receives requests after a shutdown request those requests should error with <code class="language-plaintext highlighter-rouge">InvalidRequest</code>.</p>

<p><em>Request</em>:</p>
<ul>
  <li>method: ‘shutdown’</li>
  <li>params: none</li>
</ul>

<p><em>Response</em>:</p>
<ul>
  <li>result: null</li>
  <li>error: code and message set in case an exception happens during shutdown request.</li>
</ul>

<h4 id="exit-notification-arrow_right"><a href="#exit" name="exit" class="anchor">Exit Notification (<img class="emoji" title=":arrow_right:" alt=":arrow_right:" src="https://github.githubassets.com/images/icons/emoji/unicode/27a1.png" height="20" width="20">)</a></h4>

<p>A notification to ask the server to exit its process.
The server should exit with <code class="language-plaintext highlighter-rouge">success</code> code 0 if the shutdown request has been received before; otherwise with <code class="language-plaintext highlighter-rouge">error</code> code 1.</p>

<p><em>Notification</em>:</p>
<ul>
  <li>method: ‘exit’</li>
  <li>params: none</li>
</ul>

<h3 id="text-document-synchronization"><a href="#textDocument_synchronization" name="textDocument_synchronization" class="anchor">Text Document Synchronization</a></h3>

<p>Client support for <code class="language-plaintext highlighter-rouge">textDocument/didOpen</code>, <code class="language-plaintext highlighter-rouge">textDocument/didChange</code> and <code class="language-plaintext highlighter-rouge">textDocument/didClose</code> notifications is mandatory in the protocol and clients can not opt out supporting them. This includes both full and incremental synchronization in the <code class="language-plaintext highlighter-rouge">textDocument/didChange</code> notification. In addition a server must either implement all three of them or none. Their capabilities are therefore controlled via a combined client and server capability. Opting out of text document synchronization makes only sense if the documents shown by the client are read only. Otherwise the server might receive request for documents, for which the content is managed in the client (e.g. they might have changed).</p>

<p><a href="#textDocument_synchronization_cc" name="textDocument_synchronization_cc" class="anchor"><em>Client Capability</em>:</a></p>
<ul>
  <li>property path (optional): <code class="language-plaintext highlighter-rouge">textDocument.synchronization.dynamicRegistration</code>
</li>
  <li>property type: <code class="language-plaintext highlighter-rouge">boolean</code>
</li>
</ul>

<p>Controls whether text document synchronization supports dynamic registration.</p>

<p><a href="#textDocument_synchronization_sc" name="textDocument_synchronization_sc" class="anchor"><em>Server Capability</em>:</a></p>
<ul>
  <li>property path (optional): <code class="language-plaintext highlighter-rouge">textDocumentSync</code>
</li>
  <li>property type: <code class="language-plaintext highlighter-rouge">TextDocumentSyncKind | TextDocumentSyncOptions</code>. The below definition of the <code class="language-plaintext highlighter-rouge">TextDocumentSyncOptions</code> only covers the properties specific to the open, change and close notifications. A complete definition covering all properties can be found <a href="#textDocument_didClose">here</a>:</li>
</ul>

<div class="anchorHolder"><a href="#textDocumentSyncKind" name="textDocumentSyncKind" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * Defines how the host (editor) should sync document changes to the language
 * server.
 */</span>
<span class="k">export</span> <span class="k">namespace</span> <span class="nx">TextDocumentSyncKind</span> <span class="p">{</span>
	<span class="cm">/**
	 * Documents should not be synced at all.
	 */</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">None</span> <span class="o">=</span> <span class="mi">0</span><span class="p">;</span>

	<span class="cm">/**
	 * Documents are synced by always sending the full content
	 * of the document.
	 */</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">Full</span> <span class="o">=</span> <span class="mi">1</span><span class="p">;</span>

	<span class="cm">/**
	 * Documents are synced by sending the full content on open.
	 * After that only incremental updates to the document are
	 * sent.
	 */</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">Incremental</span> <span class="o">=</span> <span class="mi">2</span><span class="p">;</span>
<span class="p">}</span>

<span class="k">export</span> <span class="kd">type</span> <span class="nx">TextDocumentSyncKind</span> <span class="o">=</span> <span class="mi">0</span> <span class="o">|</span> <span class="mi">1</span> <span class="o">|</span> <span class="mi">2</span><span class="p">;</span>
</code></pre></div></div>

<div class="anchorHolder"><a href="#textDocumentSyncOptions" name="textDocumentSyncOptions" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">TextDocumentSyncOptions</span> <span class="p">{</span>
	<span class="cm">/**
	 * Open and close notifications are sent to the server. If omitted open
	 * close notifications should not be sent.
	 */</span>
	<span class="nl">openClose</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>

	<span class="cm">/**
	 * Change notifications are sent to the server. See
	 * TextDocumentSyncKind.None, TextDocumentSyncKind.Full and
	 * TextDocumentSyncKind.Incremental. If omitted it defaults to
	 * TextDocumentSyncKind.None.
	 */</span>
	<span class="nl">change</span><span class="p">?:</span> <span class="nx">TextDocumentSyncKind</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<h4 id="didopentextdocument-notification-arrow_right"><a href="#textDocument_didOpen" name="textDocument_didOpen" class="anchor">DidOpenTextDocument Notification (<img class="emoji" title=":arrow_right:" alt=":arrow_right:" src="https://github.githubassets.com/images/icons/emoji/unicode/27a1.png" height="20" width="20">)</a></h4>

<p>The document open notification is sent from the client to the server to signal newly opened text documents. The document’s content is now managed by the client and the server must not try to read the document’s content using the document’s Uri. Open in this sense means it is managed by the client. It doesn’t necessarily mean that its content is presented in an editor. An open notification must not be sent more than once without a corresponding close notification send before. This means open and close notification must be balanced and the max open count for a particular textDocument is one. Note that a server’s ability to fulfill requests is independent of whether a text document is open or closed.</p>

<p>The <code class="language-plaintext highlighter-rouge">DidOpenTextDocumentParams</code> contain the language id the document is associated with. If the language id of a document changes, the client needs to send a <code class="language-plaintext highlighter-rouge">textDocument/didClose</code> to the server followed by a <code class="language-plaintext highlighter-rouge">textDocument/didOpen</code> with the new language id if the server handles the new language id as well.</p>

<p><em>Client Capability</em>:
See general synchronization <a href="#textDocument_synchronization_cc">client capabilities</a>.</p>

<p><em>Server Capability</em>:
See general synchronization <a href="#textDocument_synchronization_sc">server capabilities</a>.</p>

<p><em>Registration Options</em>: <a href="#textDocumentRegistrationOptions"><code class="language-plaintext highlighter-rouge">TextDocumentRegistrationOptions</code></a></p>

<p><em>Notification</em>:</p>
<ul>
  <li>method: ‘textDocument/didOpen’</li>
  <li>params: <code class="language-plaintext highlighter-rouge">DidOpenTextDocumentParams</code> defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#didOpenTextDocumentParams" name="didOpenTextDocumentParams" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="kr">interface</span> <span class="nx">DidOpenTextDocumentParams</span> <span class="p">{</span>
	<span class="cm">/**
	 * The document that was opened.
	 */</span>
	<span class="nl">textDocument</span><span class="p">:</span> <span class="nx">TextDocumentItem</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>
<h4 id="didchangetextdocument-notification-arrow_right"><a href="#textDocument_didChange" name="textDocument_didChange" class="anchor">DidChangeTextDocument Notification (<img class="emoji" title=":arrow_right:" alt=":arrow_right:" src="https://github.githubassets.com/images/icons/emoji/unicode/27a1.png" height="20" width="20">)</a></h4>

<p>The document change notification is sent from the client to the server to signal changes to a text document. Before a client can change a text document it must claim ownership of its content using the <code class="language-plaintext highlighter-rouge">textDocument/didOpen</code> notification. In 2.0 the shape of the params has changed to include proper version numbers.</p>

<p><em>Client Capability</em>:
See general synchronization <a href="#textDocument_synchronization_cc">client capabilities</a>.</p>

<p><em>Server Capability</em>:
See general synchronization <a href="#textDocument_synchronization_sc">server capabilities</a>.</p>

<p><em>Registration Options</em>: <code class="language-plaintext highlighter-rouge">TextDocumentChangeRegistrationOptions</code> defined as follows:</p>

<div class="anchorHolder"><a href="#textDocumentChangeRegistrationOptions" name="textDocumentChangeRegistrationOptions" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * Describe options to be used when registering for text document change events.
 */</span>
<span class="k">export</span> <span class="kr">interface</span> <span class="nx">TextDocumentChangeRegistrationOptions</span>
	<span class="kd">extends</span> <span class="nx">TextDocumentRegistrationOptions</span> <span class="p">{</span>
	<span class="cm">/**
	 * How documents are synced to the server. See TextDocumentSyncKind.Full
	 * and TextDocumentSyncKind.Incremental.
	 */</span>
	<span class="nl">syncKind</span><span class="p">:</span> <span class="nx">TextDocumentSyncKind</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Notification</em>:</p>
<ul>
  <li>method: <code class="language-plaintext highlighter-rouge">textDocument/didChange</code>
</li>
  <li>params: <code class="language-plaintext highlighter-rouge">DidChangeTextDocumentParams</code> defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#didChangeTextDocumentParams" name="didChangeTextDocumentParams" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="kr">interface</span> <span class="nx">DidChangeTextDocumentParams</span> <span class="p">{</span>
	<span class="cm">/**
	 * The document that did change. The version number points
	 * to the version after all provided content changes have
	 * been applied.
	 */</span>
	<span class="nl">textDocument</span><span class="p">:</span> <span class="nx">VersionedTextDocumentIdentifier</span><span class="p">;</span>

	<span class="cm">/**
	 * The actual content changes. The content changes describe single state
	 * changes to the document. So if there are two content changes c1 (at
	 * array index 0) and c2 (at array index 1) for a document in state S then
	 * c1 moves the document from S to S' and c2 from S' to S''. So c1 is
	 * computed on the state S and c2 is computed on the state S'.
	 *
	 * To mirror the content of a document using change events use the following
	 * approach:
	 * - start with the same initial content
	 * - apply the 'textDocument/didChange' notifications in the order you
	 *   receive them.
	 * - apply the `TextDocumentContentChangeEvent`s in a single notification
	 *   in the order you receive them.
	 */</span>
	<span class="nl">contentChanges</span><span class="p">:</span> <span class="nx">TextDocumentContentChangeEvent</span><span class="p">[];</span>
<span class="p">}</span>
</code></pre></div></div>

<div class="anchorHolder"><a href="#textDocumentContentChangeEvent" name="textDocumentContentChangeEvent" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * An event describing a change to a text document. If only a text is provided
 * it is considered to be the full content of the document.
 */</span>
<span class="k">export</span> <span class="kd">type</span> <span class="nx">TextDocumentContentChangeEvent</span> <span class="o">=</span> <span class="p">{</span>
	<span class="cm">/**
	 * The range of the document that changed.
	 */</span>
	<span class="na">range</span><span class="p">:</span> <span class="nx">Range</span><span class="p">;</span>

	<span class="cm">/**
	 * The optional length of the range that got replaced.
	 *
	 * @deprecated use range instead.
	 */</span>
	<span class="nl">rangeLength</span><span class="p">?:</span> <span class="nx">uinteger</span><span class="p">;</span>

	<span class="cm">/**
	 * The new text for the provided range.
	 */</span>
	<span class="nl">text</span><span class="p">:</span> <span class="kr">string</span><span class="p">;</span>
<span class="p">}</span> <span class="o">|</span> <span class="p">{</span>
	<span class="cm">/**
	 * The new text of the whole document.
	 */</span>
	<span class="na">text</span><span class="p">:</span> <span class="kr">string</span><span class="p">;</span>
<span class="p">};</span>
</code></pre></div></div>

<h4 id="willsavetextdocument-notification-arrow_right"><a href="#textDocument_willSave" name="textDocument_willSave" class="anchor">WillSaveTextDocument Notification (<img class="emoji" title=":arrow_right:" alt=":arrow_right:" src="https://github.githubassets.com/images/icons/emoji/unicode/27a1.png" height="20" width="20">)</a></h4>

<p>The document will save notification is sent from the client to the server before the document is actually saved. If a server has registered for open / close events clients should ensure that the document is open before a <code class="language-plaintext highlighter-rouge">willSave</code> notification is sent since clients can’t change the content of a file without ownership transferal.</p>

<p><em>Client Capability</em>:</p>
<ul>
  <li>property name (optional): <code class="language-plaintext highlighter-rouge">textDocument.synchronization.willSave</code>
</li>
  <li>property type: <code class="language-plaintext highlighter-rouge">boolean</code>
</li>
</ul>

<p>The capability indicates that the client supports <code class="language-plaintext highlighter-rouge">textDocument/willSave</code> notifications.</p>

<p><em>Server Capability</em>:</p>
<ul>
  <li>property name (optional): <code class="language-plaintext highlighter-rouge">textDocumentSync.willSave</code>
</li>
  <li>property type: <code class="language-plaintext highlighter-rouge">boolean</code>
</li>
</ul>

<p>The capability indicates that the server is interested in <code class="language-plaintext highlighter-rouge">textDocument/willSave</code> notifications.</p>

<p><em>Registration Options</em>: <code class="language-plaintext highlighter-rouge">TextDocumentRegistrationOptions</code></p>

<p><em>Notification</em>:</p>
<ul>
  <li>method: ‘textDocument/willSave’</li>
  <li>params: <code class="language-plaintext highlighter-rouge">WillSaveTextDocumentParams</code> defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#willSaveTextDocumentParams" name="willSaveTextDocumentParams" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * The parameters send in a will save text document notification.
 */</span>
<span class="k">export</span> <span class="kr">interface</span> <span class="nx">WillSaveTextDocumentParams</span> <span class="p">{</span>
	<span class="cm">/**
	 * The document that will be saved.
	 */</span>
	<span class="nl">textDocument</span><span class="p">:</span> <span class="nx">TextDocumentIdentifier</span><span class="p">;</span>

	<span class="cm">/**
	 * The 'TextDocumentSaveReason'.
	 */</span>
	<span class="nl">reason</span><span class="p">:</span> <span class="nx">TextDocumentSaveReason</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<div class="anchorHolder"><a href="#textDocumentSaveReason" name="textDocumentSaveReason" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * Represents reasons why a text document is saved.
 */</span>
<span class="k">export</span> <span class="k">namespace</span> <span class="nx">TextDocumentSaveReason</span> <span class="p">{</span>

	<span class="cm">/**
	 * Manually triggered, e.g. by the user pressing save, by starting
	 * debugging, or by an API call.
	 */</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">Manual</span> <span class="o">=</span> <span class="mi">1</span><span class="p">;</span>

	<span class="cm">/**
	 * Automatic after a delay.
	 */</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">AfterDelay</span> <span class="o">=</span> <span class="mi">2</span><span class="p">;</span>

	<span class="cm">/**
	 * When the editor lost focus.
	 */</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">FocusOut</span> <span class="o">=</span> <span class="mi">3</span><span class="p">;</span>
<span class="p">}</span>

<span class="k">export</span> <span class="kd">type</span> <span class="nx">TextDocumentSaveReason</span> <span class="o">=</span> <span class="mi">1</span> <span class="o">|</span> <span class="mi">2</span> <span class="o">|</span> <span class="mi">3</span><span class="p">;</span>
</code></pre></div></div>

<h4 id="willsavewaituntiltextdocument-request-leftwards_arrow_with_hook"><a href="#textDocument_willSaveWaitUntil" name="textDocument_willSaveWaitUntil" class="anchor">WillSaveWaitUntilTextDocument Request (<img class="emoji" title=":leftwards_arrow_with_hook:" alt=":leftwards_arrow_with_hook:" src="https://github.githubassets.com/images/icons/emoji/unicode/21a9.png" height="20" width="20">)</a></h4>

<p>The document will save request is sent from the client to the server before the document is actually saved. The request can return an array of TextEdits which will be applied to the text document before it is saved. Please note that clients might drop results if computing the text edits took too long or if a server constantly fails on this request. This is done to keep the save fast and reliable.  If a server has registered for open / close events clients should ensure that the document is open before a <code class="language-plaintext highlighter-rouge">willSaveWaitUntil</code> notification is sent since clients can’t change the content of a file without ownership transferal.</p>

<p><em>Client Capability</em>:</p>
<ul>
  <li>property name (optional): <code class="language-plaintext highlighter-rouge">textDocument.synchronization.willSaveWaitUntil</code>
</li>
  <li>property type: <code class="language-plaintext highlighter-rouge">boolean</code>
</li>
</ul>

<p>The capability indicates that the client supports <code class="language-plaintext highlighter-rouge">textDocument/willSaveWaitUntil</code> requests.</p>

<p><em>Server Capability</em>:</p>
<ul>
  <li>property name (optional): <code class="language-plaintext highlighter-rouge">textDocumentSync.willSaveWaitUntil</code>
</li>
  <li>property type: <code class="language-plaintext highlighter-rouge">boolean</code>
</li>
</ul>

<p>The capability indicates that the server is interested in <code class="language-plaintext highlighter-rouge">textDocument/willSaveWaitUntil</code> requests.</p>

<p><em>Registration Options</em>: <code class="language-plaintext highlighter-rouge">TextDocumentRegistrationOptions</code></p>

<p><em>Request</em>:</p>
<ul>
  <li>method: <code class="language-plaintext highlighter-rouge">textDocument/willSaveWaitUntil</code>
</li>
  <li>params: <code class="language-plaintext highlighter-rouge">WillSaveTextDocumentParams</code>
</li>
</ul>

<p><em>Response</em>:</p>
<ul>
  <li>result: <a href="#textEdit"><code class="language-plaintext highlighter-rouge">TextEdit[]</code></a> | <code class="language-plaintext highlighter-rouge">null</code>
</li>
  <li>error: code and message set in case an exception happens during the <code class="language-plaintext highlighter-rouge">textDocument/willSaveWaitUntil</code> request.</li>
</ul>

<h4 id="didsavetextdocument-notification-arrow_right"><a href="#textDocument_didSave" name="textDocument_didSave" class="anchor">DidSaveTextDocument Notification (<img class="emoji" title=":arrow_right:" alt=":arrow_right:" src="https://github.githubassets.com/images/icons/emoji/unicode/27a1.png" height="20" width="20">)</a></h4>

<p>The document save notification is sent from the client to the server when the document was saved in the client.</p>

<p><em>Client Capability</em>:</p>
<ul>
  <li>property name (optional): <code class="language-plaintext highlighter-rouge">textDocument.synchronization.didSave</code>
</li>
  <li>property type: <code class="language-plaintext highlighter-rouge">boolean</code>
</li>
</ul>

<p>The capability indicates that the client supports <code class="language-plaintext highlighter-rouge">textDocument/didSave</code> notifications.</p>

<p><em>Server Capability</em>:</p>
<ul>
  <li>property name (optional): <code class="language-plaintext highlighter-rouge">textDocumentSync.save</code>
</li>
  <li>property type: <code class="language-plaintext highlighter-rouge">boolean | SaveOptions</code> where <code class="language-plaintext highlighter-rouge">SaveOptions</code> is defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#saveOptions" name="saveOptions" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">SaveOptions</span> <span class="p">{</span>
	<span class="cm">/**
	 * The client is supposed to include the content on save.
	 */</span>
	<span class="nl">includeText</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<p>The capability indicates that the server is interested in <code class="language-plaintext highlighter-rouge">textDocument/didSave</code> notifications.</p>

<p><em>Registration Options</em>: <code class="language-plaintext highlighter-rouge">TextDocumentSaveRegistrationOptions</code> defined as follows:</p>

<div class="anchorHolder"><a href="#textDocumentSaveRegistrationOptions" name="textDocumentSaveRegistrationOptions" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">TextDocumentSaveRegistrationOptions</span>
	<span class="kd">extends</span> <span class="nx">TextDocumentRegistrationOptions</span> <span class="p">{</span>
	<span class="cm">/**
	 * The client is supposed to include the content on save.
	 */</span>
	<span class="nl">includeText</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Notification</em>:</p>
<ul>
  <li>method: <code class="language-plaintext highlighter-rouge">textDocument/didSave</code>
</li>
  <li>params: <code class="language-plaintext highlighter-rouge">DidSaveTextDocumentParams</code> defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#didSaveTextDocumentParams" name="didSaveTextDocumentParams" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="kr">interface</span> <span class="nx">DidSaveTextDocumentParams</span> <span class="p">{</span>
	<span class="cm">/**
	 * The document that was saved.
	 */</span>
	<span class="nl">textDocument</span><span class="p">:</span> <span class="nx">TextDocumentIdentifier</span><span class="p">;</span>

	<span class="cm">/**
	 * Optional the content when saved. Depends on the includeText value
	 * when the save notification was requested.
	 */</span>
	<span class="nl">text</span><span class="p">?:</span> <span class="kr">string</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<h4 id="didclosetextdocument-notification-arrow_right"><a href="#textDocument_didClose" name="textDocument_didClose" class="anchor">DidCloseTextDocument Notification (<img class="emoji" title=":arrow_right:" alt=":arrow_right:" src="https://github.githubassets.com/images/icons/emoji/unicode/27a1.png" height="20" width="20">)</a></h4>

<p>The document close notification is sent from the client to the server when the document got closed in the client. The document’s master now exists where the document’s Uri points to (e.g. if the document’s Uri is a file Uri the master now exists on disk). As with the open notification the close notification is about managing the document’s content. Receiving a close notification doesn’t mean that the document was open in an editor before. A close notification requires a previous open notification to be sent. Note that a server’s ability to fulfill requests is independent of whether a text document is open or closed.</p>

<p><em>Client Capability</em>:
See general synchronization <a href="#textDocument_synchronization_cc">client capabilities</a>.</p>

<p><em>Server Capability</em>:
See general synchronization <a href="#textDocument_synchronization_sc">server capabilities</a>.</p>

<p><em>Registration Options</em>: <code class="language-plaintext highlighter-rouge">TextDocumentRegistrationOptions</code></p>

<p><em>Notification</em>:</p>
<ul>
  <li>method: <code class="language-plaintext highlighter-rouge">textDocument/didClose</code>
</li>
  <li>params: <code class="language-plaintext highlighter-rouge">DidCloseTextDocumentParams</code> defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#didCloseTextDocumentParams" name="didCloseTextDocumentParams" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="kr">interface</span> <span class="nx">DidCloseTextDocumentParams</span> <span class="p">{</span>
	<span class="cm">/**
	 * The document that was closed.
	 */</span>
	<span class="nl">textDocument</span><span class="p">:</span> <span class="nx">TextDocumentIdentifier</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>
<h4 id="renaming-a-document"><a href="#textDocument_didRename" name="textDocument_didRename" class="anchor">Renaming a document</a></h4>

<p>Document renames should be signaled to a server sending a document close notification with the document’s old name followed by an open notification using the document’s new name. Major reason is that besides the name other attributes can change as well like the language that is associated with the document. In addition the new document could not be of interest for the server anymore.</p>

<p>Servers can participate in a document rename by subscribing for the <a href="#workspace_didRenameFiles"><code class="language-plaintext highlighter-rouge">workspace/didRenameFiles</code></a> notification or the <a href="#workspace_willRenameFiles"><code class="language-plaintext highlighter-rouge">workspace/willRenameFiles</code></a> request.</p>

<p>The final structure of the <code class="language-plaintext highlighter-rouge">TextDocumentSyncClientCapabilities</code> and the <code class="language-plaintext highlighter-rouge">TextDocumentSyncOptions</code> server options look like this</p>

<div class="anchorHolder"><a href="#textDocumentSyncClientCapabilities" name="textDocumentSyncClientCapabilities" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">TextDocumentSyncClientCapabilities</span> <span class="p">{</span>
	<span class="cm">/**
	 * Whether text document synchronization supports dynamic registration.
	 */</span>
	<span class="nl">dynamicRegistration</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>

	<span class="cm">/**
	 * The client supports sending will save notifications.
	 */</span>
	<span class="nl">willSave</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>

	<span class="cm">/**
	 * The client supports sending a will save request and
	 * waits for a response providing text edits which will
	 * be applied to the document before it is saved.
	 */</span>
	<span class="nl">willSaveWaitUntil</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>

	<span class="cm">/**
	 * The client supports did save notifications.
	 */</span>
	<span class="nl">didSave</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<div class="anchorHolder"><a href="#textDocumentSyncOptions" name="textDocumentSyncOptions" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">TextDocumentSyncOptions</span> <span class="p">{</span>
	<span class="cm">/**
	 * Open and close notifications are sent to the server. If omitted open
	 * close notification should not be sent.
	 */</span>
	<span class="nl">openClose</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>
	<span class="cm">/**
	 * Change notifications are sent to the server. See
	 * TextDocumentSyncKind.None, TextDocumentSyncKind.Full and
	 * TextDocumentSyncKind.Incremental. If omitted it defaults to
	 * TextDocumentSyncKind.None.
	 */</span>
	<span class="nl">change</span><span class="p">?:</span> <span class="nx">TextDocumentSyncKind</span><span class="p">;</span>
	<span class="cm">/**
	 * If present will save notifications are sent to the server. If omitted
	 * the notification should not be sent.
	 */</span>
	<span class="nl">willSave</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>
	<span class="cm">/**
	 * If present will save wait until requests are sent to the server. If
	 * omitted the request should not be sent.
	 */</span>
	<span class="nl">willSaveWaitUntil</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>
	<span class="cm">/**
	 * If present save notifications are sent to the server. If omitted the
	 * notification should not be sent.
	 */</span>
	<span class="nl">save</span><span class="p">?:</span> <span class="nx">boolean</span> <span class="o">|</span> <span class="nx">SaveOptions</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<h3 id="notebook-document-synchronization"><a href="#notebookDocument_synchronization" name="notebookDocument_synchronization" class="anchor">Notebook Document Synchronization</a></h3>

<p>Notebooks are becoming more and more popular. Adding support for them to the language server protocol allows notebook editors to reuse language smarts provided by the server inside a notebook or a notebook cell, respectively. To reuse protocol parts and therefore server implementations notebooks are modeled in the following way in LSP:</p>

<ul>
  <li>
<em>notebook document</em>: a collection of notebook cells typically stored in a file on disk. A notebook document has a type and can be uniquely identified using a resource URI.</li>
  <li>
<em>notebook cell</em>: holds the actual text content. Cells have a kind (either code or markdown). The actual text content of the cell is stored in a text document which can be synced to the server like all other text documents. Cell text documents have an URI however servers should not rely on any format for this URI since it is up to the client on how it will create these URIs. The URIs must be unique across ALL notebook cells and can therefore be used to uniquely identify a notebook cell or the cell’s text document.</li>
</ul>

<p>The two concepts are defined as follows:</p>

<div class="anchorHolder"><a href="#notebookDocument" name="notebookDocument" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * A notebook document.
 *
 * @since 3.17.0
 */</span>
<span class="k">export</span> <span class="kr">interface</span> <span class="nx">NotebookDocument</span> <span class="p">{</span>

	<span class="cm">/**
	 * The notebook document's URI.
	 */</span>
	<span class="nl">uri</span><span class="p">:</span> <span class="nx">URI</span><span class="p">;</span>

	<span class="cm">/**
	 * The type of the notebook.
	 */</span>
	<span class="nl">notebookType</span><span class="p">:</span> <span class="kr">string</span><span class="p">;</span>

	<span class="cm">/**
	 * The version number of this document (it will increase after each
	 * change, including undo/redo).
	 */</span>
	<span class="nl">version</span><span class="p">:</span> <span class="nx">integer</span><span class="p">;</span>

	<span class="cm">/**
	 * Additional metadata stored with the notebook
	 * document.
	 */</span>
	<span class="nl">metadata</span><span class="p">?:</span> <span class="nx">LSPObject</span><span class="p">;</span>

	<span class="cm">/**
	 * The cells of a notebook.
	 */</span>
	<span class="nl">cells</span><span class="p">:</span> <span class="nx">NotebookCell</span><span class="p">[];</span>
<span class="p">}</span>
</code></pre></div></div>

<div class="anchorHolder"><a href="#notebookCell" name="notebookCell" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * A notebook cell.
 *
 * A cell's document URI must be unique across ALL notebook
 * cells and can therefore be used to uniquely identify a
 * notebook cell or the cell's text document.
 *
 * @since 3.17.0
 */</span>
<span class="k">export</span> <span class="kr">interface</span> <span class="nx">NotebookCell</span> <span class="p">{</span>

	<span class="cm">/**
	 * The cell's kind
	 */</span>
	<span class="nl">kind</span><span class="p">:</span> <span class="nx">NotebookCellKind</span><span class="p">;</span>

	<span class="cm">/**
	 * The URI of the cell's text document
	 * content.
	 */</span>
	<span class="nl">document</span><span class="p">:</span> <span class="nx">DocumentUri</span><span class="p">;</span>

	<span class="cm">/**
	 * Additional metadata stored with the cell.
	 */</span>
	<span class="nl">metadata</span><span class="p">?:</span> <span class="nx">LSPObject</span><span class="p">;</span>

	<span class="cm">/**
	 * Additional execution summary information
	 * if supported by the client.
	 */</span>
	<span class="nl">executionSummary</span><span class="p">?:</span> <span class="nx">ExecutionSummary</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<div class="anchorHolder"><a href="#notebookCellKind" name="notebookCellKind" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * A notebook cell kind.
 *
 * @since 3.17.0
 */</span>
<span class="k">export</span> <span class="k">namespace</span> <span class="nx">NotebookCellKind</span> <span class="p">{</span>

	<span class="cm">/**
	 * A markup-cell is formatted source that is used for display.
	 */</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">Markup</span><span class="p">:</span> <span class="mi">1</span> <span class="o">=</span> <span class="mi">1</span><span class="p">;</span>

	<span class="cm">/**
	 * A code-cell is source code.
	 */</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">Code</span><span class="p">:</span> <span class="mi">2</span> <span class="o">=</span> <span class="mi">2</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<div class="anchorHolder"><a href="#executionSummary" name="executionSummary" class="linkableAnchor"></a></div>

let testString=
  `
<!--html-->
let a=10
  
<!--html-->  
`
<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">ExecutionSummary</span> <span class="p">{</span>
	<span class="cm">/**
	 * A strict monotonically increasing value
	 * indicating the execution order of a cell
	 * inside a notebook.
	 */</span>
	<span class="nl">executionOrder</span><span class="p">:</span> <span class="nx">uinteger</span><span class="p">;</span>

	<span class="cm">/**
	 * Whether the execution was successful or
	 * not if known by the client.
	 */</span>
	<span class="nl">success</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<p>Next we describe how notebooks, notebook cells and the content of a notebook cell should be synchronized to a language server.</p>

<p>Syncing the text content of a cell is relatively easy since clients should model them as text documents. However since the URI of a notebook cell’s text document should be opaque, servers can not know its scheme nor its path. However what is know is the notebook document itself. We therefore introduce a special filter for notebook cell documents:</p>

<div class="anchorHolder"><a href="#notebookCellTextDocumentFilter" name="notebookCellTextDocumentFilter" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * A notebook cell text document filter denotes a cell text
 * document by different properties.
 *
 * @since 3.17.0
 */</span>
<span class="k">export</span> <span class="kr">interface</span> <span class="nx">NotebookCellTextDocumentFilter</span> <span class="p">{</span>
	<span class="cm">/**
	 * A filter that matches against the notebook
	 * containing the notebook cell. If a string
	 * value is provided it matches against the
	 * notebook type. '*' matches every notebook.
	 */</span>
	<span class="nl">notebook</span><span class="p">:</span> <span class="kr">string</span> <span class="o">|</span> <span class="nx">NotebookDocumentFilter</span><span class="p">;</span>

	<span class="cm">/**
	 * A language id like `python`.
	 *
	 * Will be matched against the language id of the
	 * notebook cell document. '*' matches every language.
	 */</span>
	<span class="nl">language</span><span class="p">?:</span> <span class="kr">string</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<div class="anchorHolder"><a href="#notebookDocumentFilter" name="notebookDocumentFilter" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * A notebook document filter denotes a notebook document by
 * different properties.
 *
 * @since 3.17.0
 */</span>
<span class="k">export</span> <span class="kd">type</span> <span class="nx">NotebookDocumentFilter</span> <span class="o">=</span> <span class="p">{</span>
	<span class="cm">/** The type of the enclosing notebook. */</span>
	<span class="na">notebookType</span><span class="p">:</span> <span class="kr">string</span><span class="p">;</span>

	<span class="cm">/** A Uri [scheme](#Uri.scheme), like `file` or `untitled`. */</span>
	<span class="nl">scheme</span><span class="p">?:</span> <span class="kr">string</span><span class="p">;</span>

	<span class="cm">/** A glob pattern. */</span>
	<span class="nl">pattern</span><span class="p">?:</span> <span class="kr">string</span><span class="p">;</span>
<span class="p">}</span> <span class="o">|</span> <span class="p">{</span>
	<span class="cm">/** The type of the enclosing notebook. */</span>
	<span class="nx">notebookType</span><span class="p">?:</span> <span class="kr">string</span><span class="p">;</span>

	<span class="cm">/** A Uri [scheme](#Uri.scheme), like `file` or `untitled`.*/</span>
	<span class="nl">scheme</span><span class="p">:</span> <span class="kr">string</span><span class="p">;</span>

	<span class="cm">/** A glob pattern. */</span>
	<span class="nl">pattern</span><span class="p">?:</span> <span class="kr">string</span><span class="p">;</span>
<span class="p">}</span> <span class="o">|</span> <span class="p">{</span>
	<span class="cm">/** The type of the enclosing notebook. */</span>
	<span class="nx">notebookType</span><span class="p">?:</span> <span class="kr">string</span><span class="p">;</span>

	<span class="cm">/** A Uri [scheme](#Uri.scheme), like `file` or `untitled`. */</span>
	<span class="nl">scheme</span><span class="p">?:</span> <span class="kr">string</span><span class="p">;</span>

	<span class="cm">/** A glob pattern. */</span>
	<span class="nl">pattern</span><span class="p">:</span> <span class="kr">string</span><span class="p">;</span>
<span class="p">};</span>
</code></pre></div></div>

<p>Given these structures a Python cell document in a Jupyter notebook stored on disk in a folder having <code class="language-plaintext highlighter-rouge">books1</code> in its path can be identified as follows;</p>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="p">{</span>
	<span class="nl">notebook</span><span class="p">:</span> <span class="p">{</span>
		<span class="na">scheme</span><span class="p">:</span> <span class="dl">'</span><span class="s1">file</span><span class="dl">'</span><span class="p">,</span>
		<span class="nx">pattern</span> <span class="dl">'</span><span class="s1">**/books1/**</span><span class="dl">'</span><span class="p">,</span>
		<span class="na">notebookType</span><span class="p">:</span> <span class="dl">'</span><span class="s1">jupyter-notebook</span><span class="dl">'</span>
	<span class="p">},</span>
	<span class="nx">language</span><span class="p">:</span> <span class="dl">'</span><span class="s1">python</span><span class="dl">'</span>
<span class="p">}</span>
</code></pre></div></div>

<p>A <code class="language-plaintext highlighter-rouge">NotebookCellTextDocumentFilter</code> can be used to register providers for certain requests like code complete or hover. If such a provider is registered the client will send the corresponding <code class="language-plaintext highlighter-rouge">textDocument/*</code> requests to the server using the cell text document’s URI as the document URI.</p>

<p>There are cases where simply only knowing about a cell’s text content is not enough for a server to reason about the cells content and to provide good language smarts. Sometimes it is necessary to know all cells of a notebook document including the notebook document itself. Consider a notebook that has two JavaScript cells with the following content</p>

<p>Cell one:</p>

<div class="language-javascript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="kd">function</span> <span class="nx">add</span><span class="p">(</span><span class="nx">a</span><span class="p">,</span> <span class="nx">b</span><span class="p">)</span> <span class="p">{</span>
	<span class="k">return</span> <span class="nx">a</span> <span class="o">+</span> <span class="nx">b</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<p>Cell two:</p>

<div class="language-javascript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="nx">add</span><span class="cm">/*&lt;cursor&gt;*/</span><span class="p">;</span>
</code></pre></div></div>
<p>Requesting code assist in cell two at the marked cursor position should propose the function <code class="language-plaintext highlighter-rouge">add</code> which is only possible if the server knows about cell one and cell two and knows that they belong to the same notebook document.</p>

<p>The protocol will therefore support two modes when it comes to synchronizing cell text content:</p>

<ul>
  <li>
<em>cellContent</em>: in this mode only the cell text content is synchronized to the server using the standard <code class="language-plaintext highlighter-rouge">textDocument/did*</code> notification. No notebook document and no cell structure is synchronized. This mode allows for easy adoption of notebooks since servers can reuse most of it implementation logic.</li>
  <li>
<em>notebook</em>: in this mode the notebook document, the notebook cells and the notebook cell text content is synchronized to the server. To allow servers to create a consistent picture of a notebook document the cell text content is NOT synchronized using the standard <code class="language-plaintext highlighter-rouge">textDocument/did*</code> notifications. It is instead synchronized using special <code class="language-plaintext highlighter-rouge">notebookDocument/did*</code> notifications. This ensures that the cell and its text content arrives on the server using one open, change or close event.</li>
</ul>

<p>To request the cell content only a normal document selector can be used. For example the selector <code class="language-plaintext highlighter-rouge">[{ language: 'python' }]</code> will synchronize Python notebook document cells to the server. However since this might synchronize unwanted documents as well a document filter can also be a <code class="language-plaintext highlighter-rouge">NotebookCellTextDocumentFilter</code>. So <code class="language-plaintext highlighter-rouge">{ notebook: { scheme: 'file', notebookType: 'jupyter-notebook' }, language: 'python' }</code> synchronizes all Python cells in a Jupyter notebook stored on disk.</p>

<p>To synchronize the whole notebook document a server provides a <code class="language-plaintext highlighter-rouge">notebookDocumentSync</code> in its server capabilities. For example:</p>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="p">{</span>
	<span class="nl">notebookDocumentSync</span><span class="p">:</span> <span class="p">{</span>
		<span class="na">notebookSelector</span><span class="p">:</span> <span class="p">{</span>
			<span class="na">notebook</span><span class="p">:</span> <span class="p">{</span> <span class="na">scheme</span><span class="p">:</span> <span class="dl">'</span><span class="s1">file</span><span class="dl">'</span><span class="p">,</span> <span class="na">notebookType</span><span class="p">:</span> <span class="dl">'</span><span class="s1">jupyter-notebook</span><span class="dl">'</span> <span class="p">},</span>
			<span class="na">cells</span><span class="p">:</span> <span class="p">[{</span> <span class="na">language</span><span class="p">:</span> <span class="dl">'</span><span class="s1">python</span><span class="dl">'</span> <span class="p">}]</span>
		<span class="p">}</span>
	<span class="p">}</span>
<span class="p">}</span>
</code></pre></div></div>
<p>Synchronizes the notebook including all Python cells to the server if the notebook is stored on disk.</p>

<p><em>Client Capability</em>:</p>

<p>The following client capabilities are defined for notebook documents:</p>

<ul>
  <li>property name (optional): <code class="language-plaintext highlighter-rouge">notebookDocument.synchronization</code>
</li>
  <li>property type: <code class="language-plaintext highlighter-rouge">NotebookDocumentSyncClientCapabilities</code> defined as follows</li>
</ul>

<div class="anchorHolder"><a href="#notebookDocumentSyncClientCapabilities" name="notebookDocumentSyncClientCapabilities" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * Notebook specific client capabilities.
 *
 * @since 3.17.0
 */</span>
<span class="k">export</span> <span class="kr">interface</span> <span class="nx">NotebookDocumentSyncClientCapabilities</span> <span class="p">{</span>

	<span class="cm">/**
	 * Whether implementation supports dynamic registration. If this is
	 * set to `true` the client supports the new
	 * `(TextDocumentRegistrationOptions &amp; StaticRegistrationOptions)`
	 * return value for the corresponding server capability as well.
	 */</span>
	<span class="nl">dynamicRegistration</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>

	<span class="cm">/**
	 * The client supports sending execution summary data per cell.
	 */</span>
	<span class="nl">executionSummarySupport</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Server Capability</em>:</p>

<p>The following server capabilities are defined for notebook documents:</p>

<ul>
  <li>property name (optional): <code class="language-plaintext highlighter-rouge">notebookDocumentSync</code>
</li>
  <li>property type: <code class="language-plaintext highlighter-rouge">NotebookDocumentOptions | NotebookDocumentRegistrationOptions</code> where <code class="language-plaintext highlighter-rouge">NotebookDocumentOptions</code> is defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#notebookDocumentSyncOptions" name="notebookDocumentSyncOptions" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * Options specific to a notebook plus its cells
 * to be synced to the server.
 *
 * If a selector provides a notebook document
 * filter but no cell selector all cells of a
 * matching notebook document will be synced.
 *
 * If a selector provides no notebook document
 * filter but only a cell selector all notebook
 * documents that contain at least one matching
 * cell will be synced.
 *
 * @since 3.17.0
 */</span>
<span class="k">export</span> <span class="kr">interface</span> <span class="nx">NotebookDocumentSyncOptions</span> <span class="p">{</span>
	<span class="cm">/**
	 * The notebooks to be synced
	 */</span>
	<span class="nl">notebookSelector</span><span class="p">:</span> <span class="p">({</span>
		<span class="cm">/**
		 * The notebook to be synced. If a string
		 * value is provided it matches against the
		 * notebook type. '*' matches every notebook.
		 */</span>
		<span class="na">notebook</span><span class="p">:</span> <span class="kr">string</span> <span class="o">|</span> <span class="nx">NotebookDocumentFilter</span><span class="p">;</span>

		<span class="cm">/**
		 * The cells of the matching notebook to be synced.
		 */</span>
		<span class="nl">cells</span><span class="p">?:</span> <span class="p">{</span> <span class="na">language</span><span class="p">:</span> <span class="kr">string</span> <span class="p">}[];</span>
	<span class="p">}</span> <span class="o">|</span> <span class="p">{</span>
		<span class="cm">/**
		 * The notebook to be synced. If a string
		 * value is provided it matches against the
		 * notebook type. '*' matches every notebook.
		 */</span>
		<span class="nx">notebook</span><span class="p">?:</span> <span class="kr">string</span> <span class="o">|</span> <span class="nx">NotebookDocumentFilter</span><span class="p">;</span>

		<span class="cm">/**
		 * The cells of the matching notebook to be synced.
		 */</span>
		<span class="nl">cells</span><span class="p">:</span> <span class="p">{</span> <span class="na">language</span><span class="p">:</span> <span class="kr">string</span> <span class="p">}[];</span>
	<span class="p">})[];</span>

	<span class="cm">/**
	 * Whether save notification should be forwarded to
	 * the server. Will only be honored if mode === `notebook`.
	 */</span>
	<span class="nl">save</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Registration Options</em>: <code class="language-plaintext highlighter-rouge">NotebookDocumentRegistrationOptions</code> defined as follows:</p>

<div class="anchorHolder"><a href="#notebookDocumentSyncRegistrationOptions" name="notebookDocumentSyncRegistrationOptions" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * Registration options specific to a notebook.
 *
 * @since 3.17.0
 */</span>
<span class="k">export</span> <span class="kr">interface</span> <span class="nx">NotebookDocumentSyncRegistrationOptions</span> <span class="kd">extends</span>
	<span class="nx">NotebookDocumentSyncOptions</span><span class="p">,</span> <span class="nx">StaticRegistrationOptions</span> <span class="p">{</span>
<span class="p">}</span>
</code></pre></div></div>

<h4 id="didopennotebookdocument-notification-arrow_right"><a href="#notebookDocument_didOpen" name="notebookDocument_didOpen" class="anchor">DidOpenNotebookDocument Notification (<img class="emoji" title=":arrow_right:" alt=":arrow_right:" src="https://github.githubassets.com/images/icons/emoji/unicode/27a1.png" height="20" width="20">)</a></h4>

<p>The open notification is sent from the client to the server when a notebook document is opened. It is only sent by a client if the server requested the synchronization mode <code class="language-plaintext highlighter-rouge">notebook</code> in its <code class="language-plaintext highlighter-rouge">notebookDocumentSync</code> capability.</p>

<p><em>Notification</em>:</p>

<ul>
  <li>method: <code class="language-plaintext highlighter-rouge">notebookDocument/didOpen</code>
</li>
  <li>params: <code class="language-plaintext highlighter-rouge">DidOpenNotebookDocumentParams</code> defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#didOpenNotebookDocumentParams" name="didOpenNotebookDocumentParams" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * The params sent in an open notebook document notification.
 *
 * @since 3.17.0
 */</span>
<span class="k">export</span> <span class="kr">interface</span> <span class="nx">DidOpenNotebookDocumentParams</span> <span class="p">{</span>

	<span class="cm">/**
	 * The notebook document that got opened.
	 */</span>
	<span class="nl">notebookDocument</span><span class="p">:</span> <span class="nx">NotebookDocument</span><span class="p">;</span>

	<span class="cm">/**
	 * The text documents that represent the content
	 * of a notebook cell.
	 */</span>
	<span class="nl">cellTextDocuments</span><span class="p">:</span> <span class="nx">TextDocumentItem</span><span class="p">[];</span>
<span class="p">}</span>
</code></pre></div></div>

<h4 id="didchangenotebookdocument-notification-arrow_right"><a href="#notebookDocument_didChange" name="notebookDocument_didChange" class="anchor">DidChangeNotebookDocument Notification (<img class="emoji" title=":arrow_right:" alt=":arrow_right:" src="https://github.githubassets.com/images/icons/emoji/unicode/27a1.png" height="20" width="20">)</a></h4>

<p>The change notification is sent from the client to the server when a notebook document changes. It is only sent by a client if the server requested the synchronization mode <code class="language-plaintext highlighter-rouge">notebook</code> in its <code class="language-plaintext highlighter-rouge">notebookDocumentSync</code> capability.</p>

<p><em>Notification</em>:</p>

<ul>
  <li>method: <code class="language-plaintext highlighter-rouge">notebookDocument/didChange</code>
</li>
  <li>params: <code class="language-plaintext highlighter-rouge">DidChangeNotebookDocumentParams</code> defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#didChangeNotebookDocumentParams" name="didChangeNotebookDocumentParams" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * The params sent in a change notebook document notification.
 *
 * @since 3.17.0
 */</span>
<span class="k">export</span> <span class="kr">interface</span> <span class="nx">DidChangeNotebookDocumentParams</span> <span class="p">{</span>

	<span class="cm">/**
	 * The notebook document that did change. The version number points
	 * to the version after all provided changes have been applied.
	 */</span>
	<span class="nl">notebookDocument</span><span class="p">:</span> <span class="nx">VersionedNotebookDocumentIdentifier</span><span class="p">;</span>

	<span class="cm">/**
	 * The actual changes to the notebook document.
	 *
	 * The change describes single state change to the notebook document.
	 * So it moves a notebook document, its cells and its cell text document
	 * contents from state S to S'.
	 *
	 * To mirror the content of a notebook using change events use the
	 * following approach:
	 * - start with the same initial content
	 * - apply the 'notebookDocument/didChange' notifications in the order
	 *   you receive them.
	 */</span>
	<span class="nl">change</span><span class="p">:</span> <span class="nx">NotebookDocumentChangeEvent</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<div class="anchorHolder"><a href="#versionedNotebookDocumentIdentifier" name="versionedNotebookDocumentIdentifier" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * A versioned notebook document identifier.
 *
 * @since 3.17.0
 */</span>
<span class="k">export</span> <span class="kr">interface</span> <span class="nx">VersionedNotebookDocumentIdentifier</span> <span class="p">{</span>

	<span class="cm">/**
	 * The version number of this notebook document.
	 */</span>
	<span class="nl">version</span><span class="p">:</span> <span class="nx">integer</span><span class="p">;</span>

	<span class="cm">/**
	 * The notebook document's URI.
	 */</span>
	<span class="nl">uri</span><span class="p">:</span> <span class="nx">URI</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<div class="anchorHolder"><a href="#notebookDocumentChangeEvent" name="notebookDocumentChangeEvent" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * A change event for a notebook document.
 *
 * @since 3.17.0
 */</span>
<span class="k">export</span> <span class="kr">interface</span> <span class="nx">NotebookDocumentChangeEvent</span> <span class="p">{</span>
	<span class="cm">/**
	 * The changed meta data if any.
	 */</span>
	<span class="nl">metadata</span><span class="p">?:</span> <span class="nx">LSPObject</span><span class="p">;</span>

	<span class="cm">/**
	 * Changes to cells
	 */</span>
	<span class="nl">cells</span><span class="p">?:</span> <span class="p">{</span>
		<span class="cm">/**
		 * Changes to the cell structure to add or
		 * remove cells.
		 */</span>
		<span class="nx">structure</span><span class="p">?:</span> <span class="p">{</span>
			<span class="cm">/**
			 * The change to the cell array.
			 */</span>
			<span class="na">array</span><span class="p">:</span> <span class="nx">NotebookCellArrayChange</span><span class="p">;</span>

			<span class="cm">/**
			 * Additional opened cell text documents.
			 */</span>
			<span class="nl">didOpen</span><span class="p">?:</span> <span class="nx">TextDocumentItem</span><span class="p">[];</span>

			<span class="cm">/**
			 * Additional closed cell text documents.
			 */</span>
			<span class="nl">didClose</span><span class="p">?:</span> <span class="nx">TextDocumentIdentifier</span><span class="p">[];</span>
		<span class="p">};</span>

		<span class="cm">/**
		 * Changes to notebook cells properties like its
		 * kind, execution summary or metadata.
		 */</span>
		<span class="nl">data</span><span class="p">?:</span> <span class="nx">NotebookCell</span><span class="p">[];</span>

		<span class="cm">/**
		 * Changes to the text content of notebook cells.
		 */</span>
		<span class="nl">textContent</span><span class="p">?:</span> <span class="p">{</span>
			<span class="na">document</span><span class="p">:</span> <span class="nx">VersionedTextDocumentIdentifier</span><span class="p">;</span>
			<span class="nl">changes</span><span class="p">:</span> <span class="nx">TextDocumentContentChangeEvent</span><span class="p">[];</span>
		<span class="p">}[];</span>
	<span class="p">};</span>
<span class="p">}</span>
</code></pre></div></div>

<div class="anchorHolder"><a href="#notebookCellArrayChange" name="notebookCellArrayChange" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * A change describing how to move a `NotebookCell`
 * array from state S to S'.
 *
 * @since 3.17.0
 */</span>
<span class="k">export</span> <span class="kr">interface</span> <span class="nx">NotebookCellArrayChange</span> <span class="p">{</span>
	<span class="cm">/**
	 * The start offset of the cell that changed.
	 */</span>
	<span class="nl">start</span><span class="p">:</span> <span class="nx">uinteger</span><span class="p">;</span>

	<span class="cm">/**
	 * The deleted cells
	 */</span>
	<span class="nl">deleteCount</span><span class="p">:</span> <span class="nx">uinteger</span><span class="p">;</span>

	<span class="cm">/**
	 * The new cells, if any
	 */</span>
	<span class="nl">cells</span><span class="p">?:</span> <span class="nx">NotebookCell</span><span class="p">[];</span>
<span class="p">}</span>
</code></pre></div></div>

<h4 id="didsavenotebookdocument-notification-arrow_right"><a href="#notebookDocument_didSave" name="notebookDocument_didSave" class="anchor">DidSaveNotebookDocument Notification (<img class="emoji" title=":arrow_right:" alt=":arrow_right:" src="https://github.githubassets.com/images/icons/emoji/unicode/27a1.png" height="20" width="20">)</a></h4>

<p>The save notification is sent from the client to the server when a notebook document is saved. It is only sent by a client if the server requested the synchronization mode <code class="language-plaintext highlighter-rouge">notebook</code> in its <code class="language-plaintext highlighter-rouge">notebookDocumentSync</code> capability.</p>

<p><em>Notification</em>:</p>

<div class="anchorHolder"><a href="#notebookDocument_didSave" name="notebookDocument_didSave" class="linkableAnchor"></a></div>

<ul>
  <li>method: <code class="language-plaintext highlighter-rouge">notebookDocument/didSave</code>
</li>
  <li>params: <code class="language-plaintext highlighter-rouge">DidSaveNotebookDocumentParams</code> defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#didSaveNotebookDocumentParams" name="didSaveNotebookDocumentParams" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * The params sent in a save notebook document notification.
 *
 * @since 3.17.0
 */</span>
<span class="k">export</span> <span class="kr">interface</span> <span class="nx">DidSaveNotebookDocumentParams</span> <span class="p">{</span>
	<span class="cm">/**
	 * The notebook document that got saved.
	 */</span>
	<span class="nl">notebookDocument</span><span class="p">:</span> <span class="nx">NotebookDocumentIdentifier</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<h4 id="didclosenotebookdocument-notification-arrow_right"><a href="#notebookDocument_didClose" name="notebookDocument_didClose" class="anchor">DidCloseNotebookDocument Notification (<img class="emoji" title=":arrow_right:" alt=":arrow_right:" src="https://github.githubassets.com/images/icons/emoji/unicode/27a1.png" height="20" width="20">)</a></h4>

<p>The close notification is sent from the client to the server when a notebook document is closed. It is only sent by a client if the server requested the synchronization mode <code class="language-plaintext highlighter-rouge">notebook</code> in its <code class="language-plaintext highlighter-rouge">notebookDocumentSync</code> capability.</p>

<p><em>Notification</em>:</p>

<div class="anchorHolder"><a href="#notebookDocument_didClose" name="notebookDocument_didClose" class="linkableAnchor"></a></div>

<ul>
  <li>method: <code class="language-plaintext highlighter-rouge">notebookDocument/didClose</code>
</li>
  <li>params: <code class="language-plaintext highlighter-rouge">DidCloseNotebookDocumentParams</code> defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#didCloseNotebookDocumentParams" name="didCloseNotebookDocumentParams" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * The params sent in a close notebook document notification.
 *
 * @since 3.17.0
 */</span>
<span class="k">export</span> <span class="kr">interface</span> <span class="nx">DidCloseNotebookDocumentParams</span> <span class="p">{</span>

	<span class="cm">/**
	 * The notebook document that got closed.
	 */</span>
	<span class="nl">notebookDocument</span><span class="p">:</span> <span class="nx">NotebookDocumentIdentifier</span><span class="p">;</span>

	<span class="cm">/**
	 * The text documents that represent the content
	 * of a notebook cell that got closed.
	 */</span>
	<span class="nl">cellTextDocuments</span><span class="p">:</span> <span class="nx">TextDocumentIdentifier</span><span class="p">[];</span>
<span class="p">}</span>
</code></pre></div></div>

<div class="anchorHolder"><a href="#notebookDocumentIdentifier" name="notebookDocumentIdentifier" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * A literal to identify a notebook document in the client.
 *
 * @since 3.17.0
 */</span>
<span class="k">export</span> <span class="kr">interface</span> <span class="nx">NotebookDocumentIdentifier</span> <span class="p">{</span>
	<span class="cm">/**
	 * The notebook document's URI.
	 */</span>
	<span class="nl">uri</span><span class="p">:</span> <span class="nx">URI</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<h3 id="language-features"><a href="#languageFeatures" name="languageFeatures" class="anchor">Language Features</a></h3>

<p>Language Features provide the actual smarts in the language server protocol. They are usually executed on a [text document, position] tuple. The main language feature categories are:</p>

<ul>
  <li>code comprehension features like Hover or Goto Definition.</li>
  <li>coding features like diagnostics, code complete or code actions.</li>
</ul>

<p>The language features should be computed on the <a href="#textDocument_synchronization">synchronized state</a> of the document.</p>

<h4 id="goto-declaration-request-leftwards_arrow_with_hook"><a href="#textDocument_declaration" name="textDocument_declaration" class="anchor">Goto Declaration Request (<img class="emoji" title=":leftwards_arrow_with_hook:" alt=":leftwards_arrow_with_hook:" src="https://github.githubassets.com/images/icons/emoji/unicode/21a9.png" height="20" width="20">)</a></h4>

<blockquote>
  <p><em>Since version 3.14.0</em></p>
</blockquote>

<p>The go to declaration request is sent from the client to the server to resolve the declaration location of a symbol at a given text document position.</p>

<p>The result type <a href="#locationLink"><code class="language-plaintext highlighter-rouge">LocationLink</code></a>[] got introduced with version 3.14.0 and depends on the corresponding client capability <code class="language-plaintext highlighter-rouge">textDocument.declaration.linkSupport</code>.</p>

<p><em>Client Capability</em>:</p>
<ul>
  <li>property name (optional): <code class="language-plaintext highlighter-rouge">textDocument.declaration</code>
</li>
  <li>property type: <code class="language-plaintext highlighter-rouge">DeclarationClientCapabilities</code> defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#declarationClientCapabilities" name="declarationClientCapabilities" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">DeclarationClientCapabilities</span> <span class="p">{</span>
	<span class="cm">/**
	 * Whether declaration supports dynamic registration. If this is set to
	 * `true` the client supports the new `DeclarationRegistrationOptions`
	 * return value for the corresponding server capability as well.
	 */</span>
	<span class="nl">dynamicRegistration</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>

	<span class="cm">/**
	 * The client supports additional metadata in the form of declaration links.
	 */</span>
	<span class="nl">linkSupport</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Server Capability</em>:</p>
<ul>
  <li>property name (optional): <code class="language-plaintext highlighter-rouge">declarationProvider</code>
</li>
  <li>property type: <code class="language-plaintext highlighter-rouge">boolean | DeclarationOptions | DeclarationRegistrationOptions</code> where <code class="language-plaintext highlighter-rouge">DeclarationOptions</code> is defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#declarationOptions" name="declarationOptions" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">DeclarationOptions</span> <span class="kd">extends</span> <span class="nx">WorkDoneProgressOptions</span> <span class="p">{</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Registration Options</em>: <code class="language-plaintext highlighter-rouge">DeclarationRegistrationOptions</code> defined as follows:</p>

<div class="anchorHolder"><a href="#declarationRegistrationOptions" name="declarationRegistrationOptions" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">DeclarationRegistrationOptions</span> <span class="kd">extends</span> <span class="nx">DeclarationOptions</span><span class="p">,</span>
	<span class="nx">TextDocumentRegistrationOptions</span><span class="p">,</span> <span class="nx">StaticRegistrationOptions</span> <span class="p">{</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Request</em>:</p>
<ul>
  <li>method: <code class="language-plaintext highlighter-rouge">textDocument/declaration</code>
</li>
  <li>params: <code class="language-plaintext highlighter-rouge">DeclarationParams</code> defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#declarationParams" name="declarationParams" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">DeclarationParams</span> <span class="kd">extends</span> <span class="nx">TextDocumentPositionParams</span><span class="p">,</span>
	<span class="nx">WorkDoneProgressParams</span><span class="p">,</span> <span class="nx">PartialResultParams</span> <span class="p">{</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Response</em>:</p>
<ul>
  <li>result: <a href="#location"><code class="language-plaintext highlighter-rouge">Location</code></a> | <a href="#location"><code class="language-plaintext highlighter-rouge">Location</code></a>[] | <a href="#locationLink"><code class="language-plaintext highlighter-rouge">LocationLink</code></a>[] |<code class="language-plaintext highlighter-rouge">null</code>
</li>
  <li>partial result: <a href="#location"><code class="language-plaintext highlighter-rouge">Location</code></a>[] | <a href="#locationLink"><code class="language-plaintext highlighter-rouge">LocationLink</code></a>[]</li>
  <li>error: code and message set in case an exception happens during the declaration request.</li>
</ul>

<h4 id="goto-definition-request-leftwards_arrow_with_hook"><a href="#textDocument_definition" name="textDocument_definition" class="anchor">Goto Definition Request (<img class="emoji" title=":leftwards_arrow_with_hook:" alt=":leftwards_arrow_with_hook:" src="https://github.githubassets.com/images/icons/emoji/unicode/21a9.png" height="20" width="20">)</a></h4>

<p>The go to definition request is sent from the client to the server to resolve the definition location of a symbol at a given text document position.</p>

<p>The result type <a href="#locationLink"><code class="language-plaintext highlighter-rouge">LocationLink</code></a>[] got introduced with version 3.14.0 and depends on the corresponding client capability <code class="language-plaintext highlighter-rouge">textDocument.definition.linkSupport</code>.</p>

<p><em>Client Capability</em>:</p>
<ul>
  <li>property name (optional): <code class="language-plaintext highlighter-rouge">textDocument.definition</code>
</li>
  <li>property type: <code class="language-plaintext highlighter-rouge">DefinitionClientCapabilities</code> defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#definitionClientCapabilities" name="definitionClientCapabilities" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">DefinitionClientCapabilities</span> <span class="p">{</span>
	<span class="cm">/**
	 * Whether definition supports dynamic registration.
	 */</span>
	<span class="nl">dynamicRegistration</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>

	<span class="cm">/**
	 * The client supports additional metadata in the form of definition links.
	 *
	 * @since 3.14.0
	 */</span>
	<span class="nl">linkSupport</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Server Capability</em>:</p>
<ul>
  <li>property name (optional): <code class="language-plaintext highlighter-rouge">definitionProvider</code>
</li>
  <li>property type: <code class="language-plaintext highlighter-rouge">boolean | DefinitionOptions</code> where <code class="language-plaintext highlighter-rouge">DefinitionOptions</code> is defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#definitionOptions" name="definitionOptions" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">DefinitionOptions</span> <span class="kd">extends</span> <span class="nx">WorkDoneProgressOptions</span> <span class="p">{</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Registration Options</em>: <code class="language-plaintext highlighter-rouge">DefinitionRegistrationOptions</code> defined as follows:</p>

<div class="anchorHolder"><a href="#definitionRegistrationOptions" name="definitionRegistrationOptions" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">DefinitionRegistrationOptions</span> <span class="kd">extends</span>
	<span class="nx">TextDocumentRegistrationOptions</span><span class="p">,</span> <span class="nx">DefinitionOptions</span> <span class="p">{</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Request</em>:</p>
<ul>
  <li>method: <code class="language-plaintext highlighter-rouge">textDocument/definition</code>
</li>
  <li>params: <code class="language-plaintext highlighter-rouge">DefinitionParams</code> defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#definitionParams" name="definitionParams" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">DefinitionParams</span> <span class="kd">extends</span> <span class="nx">TextDocumentPositionParams</span><span class="p">,</span>
	<span class="nx">WorkDoneProgressParams</span><span class="p">,</span> <span class="nx">PartialResultParams</span> <span class="p">{</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Response</em>:</p>
<ul>
  <li>result: <a href="#location"><code class="language-plaintext highlighter-rouge">Location</code></a> | <a href="#location"><code class="language-plaintext highlighter-rouge">Location</code></a>[] | <a href="#locationLink"><code class="language-plaintext highlighter-rouge">LocationLink</code></a>[] | <code class="language-plaintext highlighter-rouge">null</code>
</li>
  <li>partial result: <a href="#location"><code class="language-plaintext highlighter-rouge">Location</code></a>[] | <a href="#locationLink"><code class="language-plaintext highlighter-rouge">LocationLink</code></a>[]</li>
  <li>error: code and message set in case an exception happens during the definition request.</li>
</ul>

<h4 id="goto-type-definition-request-leftwards_arrow_with_hook"><a href="#textDocument_typeDefinition" name="textDocument_typeDefinition" class="anchor">Goto Type Definition Request (<img class="emoji" title=":leftwards_arrow_with_hook:" alt=":leftwards_arrow_with_hook:" src="https://github.githubassets.com/images/icons/emoji/unicode/21a9.png" height="20" width="20">)</a></h4>

<blockquote>
  <p><em>Since version 3.6.0</em></p>
</blockquote>

<p>The go to type definition request is sent from the client to the server to resolve the type definition location of a symbol at a given text document position.</p>

<p>The result type <a href="#locationLink"><code class="language-plaintext highlighter-rouge">LocationLink</code></a>[] got introduced with version 3.14.0 and depends on the corresponding client capability <code class="language-plaintext highlighter-rouge">textDocument.typeDefinition.linkSupport</code>.</p>

<p><em>Client Capability</em>:</p>
<ul>
  <li>property name (optional): <code class="language-plaintext highlighter-rouge">textDocument.typeDefinition</code>
</li>
  <li>property type: <code class="language-plaintext highlighter-rouge">TypeDefinitionClientCapabilities</code> defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#typeDefinitionClientCapabilities" name="typeDefinitionClientCapabilities" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">TypeDefinitionClientCapabilities</span> <span class="p">{</span>
	<span class="cm">/**
	 * Whether implementation supports dynamic registration. If this is set to
	 * `true` the client supports the new `TypeDefinitionRegistrationOptions`
	 * return value for the corresponding server capability as well.
	 */</span>
	<span class="nl">dynamicRegistration</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>

	<span class="cm">/**
	 * The client supports additional metadata in the form of definition links.
	 *
	 * @since 3.14.0
	 */</span>
	<span class="nl">linkSupport</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Server Capability</em>:</p>
<ul>
  <li>property name (optional): <code class="language-plaintext highlighter-rouge">typeDefinitionProvider</code>
</li>
  <li>property type: <code class="language-plaintext highlighter-rouge">boolean | TypeDefinitionOptions | TypeDefinitionRegistrationOptions</code> where <code class="language-plaintext highlighter-rouge">TypeDefinitionOptions</code> is defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#typeDefinitionOptions" name="typeDefinitionOptions" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">TypeDefinitionOptions</span> <span class="kd">extends</span> <span class="nx">WorkDoneProgressOptions</span> <span class="p">{</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Registration Options</em>: <code class="language-plaintext highlighter-rouge">TypeDefinitionRegistrationOptions</code> defined as follows:</p>

<div class="anchorHolder"><a href="#typeDefinitionRegistrationOptions" name="typeDefinitionRegistrationOptions" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">TypeDefinitionRegistrationOptions</span> <span class="kd">extends</span>
	<span class="nx">TextDocumentRegistrationOptions</span><span class="p">,</span> <span class="nx">TypeDefinitionOptions</span><span class="p">,</span>
	<span class="nx">StaticRegistrationOptions</span> <span class="p">{</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Request</em>:</p>
<ul>
  <li>method: <code class="language-plaintext highlighter-rouge">textDocument/typeDefinition</code>
</li>
  <li>params: <code class="language-plaintext highlighter-rouge">TypeDefinitionParams</code> defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#typeDefinitionParams" name="typeDefinitionParams" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">TypeDefinitionParams</span> <span class="kd">extends</span> <span class="nx">TextDocumentPositionParams</span><span class="p">,</span>
	<span class="nx">WorkDoneProgressParams</span><span class="p">,</span> <span class="nx">PartialResultParams</span> <span class="p">{</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Response</em>:</p>
<ul>
  <li>result: <a href="#location"><code class="language-plaintext highlighter-rouge">Location</code></a> | <a href="#location"><code class="language-plaintext highlighter-rouge">Location</code></a>[] | <a href="#locationLink"><code class="language-plaintext highlighter-rouge">LocationLink</code></a>[] | <code class="language-plaintext highlighter-rouge">null</code>
</li>
  <li>partial result: <a href="#location"><code class="language-plaintext highlighter-rouge">Location</code></a>[] | <a href="#locationLink"><code class="language-plaintext highlighter-rouge">LocationLink</code></a>[]</li>
  <li>error: code and message set in case an exception happens during the definition request.</li>
</ul>

<h4 id="goto-implementation-request-leftwards_arrow_with_hook"><a href="#textDocument_implementation" name="textDocument_implementation" class="anchor">Goto Implementation Request (<img class="emoji" title=":leftwards_arrow_with_hook:" alt=":leftwards_arrow_with_hook:" src="https://github.githubassets.com/images/icons/emoji/unicode/21a9.png" height="20" width="20">)</a></h4>

<blockquote>
  <p><em>Since version 3.6.0</em></p>
</blockquote>

<p>The go to implementation request is sent from the client to the server to resolve the implementation location of a symbol at a given text document position.</p>

<p>The result type <a href="#locationLink"><code class="language-plaintext highlighter-rouge">LocationLink</code></a>[] got introduced with version 3.14.0 and depends on the corresponding client capability <code class="language-plaintext highlighter-rouge">textDocument.implementation.linkSupport</code>.</p>

<p><em>Client Capability</em>:</p>
<ul>
  <li>property name (optional): <code class="language-plaintext highlighter-rouge">textDocument.implementation</code>
</li>
  <li>property type: <code class="language-plaintext highlighter-rouge">ImplementationClientCapabilities</code> defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#implementationClientCapabilities" name="implementationClientCapabilities" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">ImplementationClientCapabilities</span> <span class="p">{</span>
	<span class="cm">/**
	 * Whether implementation supports dynamic registration. If this is set to
	 * `true` the client supports the new `ImplementationRegistrationOptions`
	 * return value for the corresponding server capability as well.
	 */</span>
	<span class="nl">dynamicRegistration</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>

	<span class="cm">/**
	 * The client supports additional metadata in the form of definition links.
	 *
	 * @since 3.14.0
	 */</span>
	<span class="nl">linkSupport</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Server Capability</em>:</p>
<ul>
  <li>property name (optional): <code class="language-plaintext highlighter-rouge">implementationProvider</code>
</li>
  <li>property type: <code class="language-plaintext highlighter-rouge">boolean | ImplementationOptions | ImplementationRegistrationOptions</code> where <code class="language-plaintext highlighter-rouge">ImplementationOptions</code> is defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#implementationOptions" name="implementationOptions" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">ImplementationOptions</span> <span class="kd">extends</span> <span class="nx">WorkDoneProgressOptions</span> <span class="p">{</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Registration Options</em>: <code class="language-plaintext highlighter-rouge">ImplementationRegistrationOptions</code> defined as follows:</p>

<div class="anchorHolder"><a href="#implementationRegistrationOptions" name="implementationRegistrationOptions" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">ImplementationRegistrationOptions</span> <span class="kd">extends</span>
	<span class="nx">TextDocumentRegistrationOptions</span><span class="p">,</span> <span class="nx">ImplementationOptions</span><span class="p">,</span>
	<span class="nx">StaticRegistrationOptions</span> <span class="p">{</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Request</em>:</p>
<ul>
  <li>method: <code class="language-plaintext highlighter-rouge">textDocument/implementation</code>
</li>
  <li>params: <code class="language-plaintext highlighter-rouge">ImplementationParams</code> defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#implementationParams" name="implementationParams" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">ImplementationParams</span> <span class="kd">extends</span> <span class="nx">TextDocumentPositionParams</span><span class="p">,</span>
	<span class="nx">WorkDoneProgressParams</span><span class="p">,</span> <span class="nx">PartialResultParams</span> <span class="p">{</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Response</em>:</p>
<ul>
  <li>result: <a href="#location"><code class="language-plaintext highlighter-rouge">Location</code></a> | <a href="#location"><code class="language-plaintext highlighter-rouge">Location</code></a>[] | <a href="#locationLink"><code class="language-plaintext highlighter-rouge">LocationLink</code></a>[] | <code class="language-plaintext highlighter-rouge">null</code>
</li>
  <li>partial result: <a href="#location"><code class="language-plaintext highlighter-rouge">Location</code></a>[] | <a href="#locationLink"><code class="language-plaintext highlighter-rouge">LocationLink</code></a>[]</li>
  <li>error: code and message set in case an exception happens during the definition request.</li>
</ul>

<h4 id="find-references-request-leftwards_arrow_with_hook"><a href="#textDocument_references" name="textDocument_references" class="anchor">Find References Request (<img class="emoji" title=":leftwards_arrow_with_hook:" alt=":leftwards_arrow_with_hook:" src="https://github.githubassets.com/images/icons/emoji/unicode/21a9.png" height="20" width="20">)</a></h4>

<p>The references request is sent from the client to the server to resolve project-wide references for the symbol denoted by the given text document position.</p>

<p><em>Client Capability</em>:</p>
<ul>
  <li>property name (optional): <code class="language-plaintext highlighter-rouge">textDocument.references</code>
</li>
  <li>property type: <code class="language-plaintext highlighter-rouge">ReferenceClientCapabilities</code> defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#referenceClientCapabilities" name="referenceClientCapabilities" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">ReferenceClientCapabilities</span> <span class="p">{</span>
	<span class="cm">/**
	 * Whether references supports dynamic registration.
	 */</span>
	<span class="nl">dynamicRegistration</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Server Capability</em>:</p>
<ul>
  <li>property name (optional): <code class="language-plaintext highlighter-rouge">referencesProvider</code>
</li>
  <li>property type: <code class="language-plaintext highlighter-rouge">boolean | ReferenceOptions</code> where <code class="language-plaintext highlighter-rouge">ReferenceOptions</code> is defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#referenceOptions" name="referenceOptions" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">ReferenceOptions</span> <span class="kd">extends</span> <span class="nx">WorkDoneProgressOptions</span> <span class="p">{</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Registration Options</em>: <code class="language-plaintext highlighter-rouge">ReferenceRegistrationOptions</code> defined as follows:</p>

<div class="anchorHolder"><a href="#referenceRegistrationOptions" name="referenceRegistrationOptions" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">ReferenceRegistrationOptions</span> <span class="kd">extends</span>
	<span class="nx">TextDocumentRegistrationOptions</span><span class="p">,</span> <span class="nx">ReferenceOptions</span> <span class="p">{</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Request</em>:</p>
<ul>
  <li>method: <code class="language-plaintext highlighter-rouge">textDocument/references</code>
</li>
  <li>params: <code class="language-plaintext highlighter-rouge">ReferenceParams</code> defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#referenceParams" name="referenceParams" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">ReferenceParams</span> <span class="kd">extends</span> <span class="nx">TextDocumentPositionParams</span><span class="p">,</span>
	<span class="nx">WorkDoneProgressParams</span><span class="p">,</span> <span class="nx">PartialResultParams</span> <span class="p">{</span>
	<span class="nl">context</span><span class="p">:</span> <span class="nx">ReferenceContext</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<div class="anchorHolder"><a href="#referenceContext" name="referenceContext" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">ReferenceContext</span> <span class="p">{</span>
	<span class="cm">/**
	 * Include the declaration of the current symbol.
	 */</span>
	<span class="nl">includeDeclaration</span><span class="p">:</span> <span class="nx">boolean</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>
<p><em>Response</em>:</p>
<ul>
  <li>result: <a href="#location"><code class="language-plaintext highlighter-rouge">Location</code></a>[] | <code class="language-plaintext highlighter-rouge">null</code>
</li>
  <li>partial result: <a href="#location"><code class="language-plaintext highlighter-rouge">Location</code></a>[]</li>
  <li>error: code and message set in case an exception happens during the reference request.</li>
</ul>

<h4 id="prepare-call-hierarchy-request-leftwards_arrow_with_hook"><a href="#textDocument_prepareCallHierarchy" name="textDocument_prepareCallHierarchy" class="anchor">Prepare Call Hierarchy Request (<img class="emoji" title=":leftwards_arrow_with_hook:" alt=":leftwards_arrow_with_hook:" src="https://github.githubassets.com/images/icons/emoji/unicode/21a9.png" height="20" width="20">)</a></h4>

<blockquote>
  <p><em>Since version 3.16.0</em></p>
</blockquote>

<p>The call hierarchy request is sent from the client to the server to return a call hierarchy for the language element of given text document positions. The call hierarchy requests are executed in two steps:</p>

<ol>
  <li>first a call hierarchy item is resolved for the given text document position</li>
  <li>for a call hierarchy item the incoming or outgoing call hierarchy items are resolved.</li>
</ol>

<p><em>Client Capability</em>:</p>

<ul>
  <li>property name (optional): <code class="language-plaintext highlighter-rouge">textDocument.callHierarchy</code>
</li>
  <li>property type: <code class="language-plaintext highlighter-rouge">CallHierarchyClientCapabilities</code> defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#callHierarchyClientCapabilities" name="callHierarchyClientCapabilities" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="kr">interface</span> <span class="nx">CallHierarchyClientCapabilities</span> <span class="p">{</span>
	<span class="cm">/**
	 * Whether implementation supports dynamic registration. If this is set to
	 * `true` the client supports the new `(TextDocumentRegistrationOptions &amp;
	 * StaticRegistrationOptions)` return value for the corresponding server
	 * capability as well.
	 */</span>
	<span class="nl">dynamicRegistration</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Server Capability</em>:</p>

<ul>
  <li>property name (optional): <code class="language-plaintext highlighter-rouge">callHierarchyProvider</code>
</li>
  <li>property type: <code class="language-plaintext highlighter-rouge">boolean | CallHierarchyOptions | CallHierarchyRegistrationOptions</code> where <code class="language-plaintext highlighter-rouge">CallHierarchyOptions</code> is defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#callHierarchyOptions" name="callHierarchyOptions" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">CallHierarchyOptions</span> <span class="kd">extends</span> <span class="nx">WorkDoneProgressOptions</span> <span class="p">{</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Registration Options</em>: <code class="language-plaintext highlighter-rouge">CallHierarchyRegistrationOptions</code> defined as follows:</p>

<div class="anchorHolder"><a href="#callHierarchyRegistrationOptions" name="callHierarchyRegistrationOptions" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">CallHierarchyRegistrationOptions</span> <span class="kd">extends</span>
	<span class="nx">TextDocumentRegistrationOptions</span><span class="p">,</span> <span class="nx">CallHierarchyOptions</span><span class="p">,</span>
	<span class="nx">StaticRegistrationOptions</span> <span class="p">{</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Request</em>:</p>

<ul>
  <li>method: <code class="language-plaintext highlighter-rouge">textDocument/prepareCallHierarchy</code>
</li>
  <li>params: <code class="language-plaintext highlighter-rouge">CallHierarchyPrepareParams</code> defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#callHierarchyPrepareParams" name="callHierarchyPrepareParams" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">CallHierarchyPrepareParams</span> <span class="kd">extends</span> <span class="nx">TextDocumentPositionParams</span><span class="p">,</span>
	<span class="nx">WorkDoneProgressParams</span> <span class="p">{</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Response</em>:</p>

<ul>
  <li>result: <code class="language-plaintext highlighter-rouge">CallHierarchyItem[] | null</code> defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#callHierarchyItem" name="callHierarchyItem" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">CallHierarchyItem</span> <span class="p">{</span>
	<span class="cm">/**
	 * The name of this item.
	 */</span>
	<span class="nl">name</span><span class="p">:</span> <span class="kr">string</span><span class="p">;</span>

	<span class="cm">/**
	 * The kind of this item.
	 */</span>
	<span class="nl">kind</span><span class="p">:</span> <span class="nx">SymbolKind</span><span class="p">;</span>

	<span class="cm">/**
	 * Tags for this item.
	 */</span>
	<span class="nl">tags</span><span class="p">?:</span> <span class="nx">SymbolTag</span><span class="p">[];</span>

	<span class="cm">/**
	 * More detail for this item, e.g. the signature of a function.
	 */</span>
	<span class="nl">detail</span><span class="p">?:</span> <span class="kr">string</span><span class="p">;</span>

	<span class="cm">/**
	 * The resource identifier of this item.
	 */</span>
	<span class="nl">uri</span><span class="p">:</span> <span class="nx">DocumentUri</span><span class="p">;</span>

	<span class="cm">/**
	 * The range enclosing this symbol not including leading/trailing whitespace
	 * but everything else, e.g. comments and code.
	 */</span>
	<span class="nl">range</span><span class="p">:</span> <span class="nx">Range</span><span class="p">;</span>

	<span class="cm">/**
	 * The range that should be selected and revealed when this symbol is being
	 * picked, e.g. the name of a function. Must be contained by the
	 * [`range`](#CallHierarchyItem.range).
	 */</span>
	<span class="nl">selectionRange</span><span class="p">:</span> <span class="nx">Range</span><span class="p">;</span>

	<span class="cm">/**
	 * A data entry field that is preserved between a call hierarchy prepare and
	 * incoming calls or outgoing calls requests.
	 */</span>
	<span class="nl">data</span><span class="p">?:</span> <span class="nx">unknown</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<ul>
  <li>error: code and message set in case an exception happens during the ‘textDocument/prepareCallHierarchy’ request</li>
</ul>

<h4 id="call-hierarchy-incoming-calls-leftwards_arrow_with_hook"><a href="#callHierarchy_incomingCalls" name="callHierarchy_incomingCalls" class="anchor">Call Hierarchy Incoming Calls (<img class="emoji" title=":leftwards_arrow_with_hook:" alt=":leftwards_arrow_with_hook:" src="https://github.githubassets.com/images/icons/emoji/unicode/21a9.png" height="20" width="20">)</a></h4>

<blockquote>
  <p><em>Since version 3.16.0</em></p>
</blockquote>

<p>The request is sent from the client to the server to resolve incoming calls for a given call hierarchy item. The request doesn’t define its own client and server capabilities. It is only issued if a server registers for the <a href="#textDocument_prepareCallHierarchy"><code class="language-plaintext highlighter-rouge">textDocument/prepareCallHierarchy</code> request</a>.</p>

<p><em>Request</em>:</p>

<ul>
  <li>method: <code class="language-plaintext highlighter-rouge">callHierarchy/incomingCalls</code>
</li>
  <li>params: <code class="language-plaintext highlighter-rouge">CallHierarchyIncomingCallsParams</code> defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#callHierarchyIncomingCallsParams" name="callHierarchyIncomingCallsParams" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">CallHierarchyIncomingCallsParams</span> <span class="kd">extends</span>
	<span class="nx">WorkDoneProgressParams</span><span class="p">,</span> <span class="nx">PartialResultParams</span> <span class="p">{</span>
	<span class="nl">item</span><span class="p">:</span> <span class="nx">CallHierarchyItem</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Response</em>:</p>

<ul>
  <li>result: <code class="language-plaintext highlighter-rouge">CallHierarchyIncomingCall[] | null</code> defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#callHierarchyIncomingCall" name="callHierarchyIncomingCall" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">CallHierarchyIncomingCall</span> <span class="p">{</span>

	<span class="cm">/**
	 * The item that makes the call.
	 */</span>
	<span class="nl">from</span><span class="p">:</span> <span class="nx">CallHierarchyItem</span><span class="p">;</span>

	<span class="cm">/**
	 * The ranges at which the calls appear. This is relative to the caller
	 * denoted by [`this.from`](#CallHierarchyIncomingCall.from).
	 */</span>
	<span class="nl">fromRanges</span><span class="p">:</span> <span class="nx">Range</span><span class="p">[];</span>
<span class="p">}</span>
</code></pre></div></div>

<ul>
  <li>partial result: <code class="language-plaintext highlighter-rouge">CallHierarchyIncomingCall[]</code>
</li>
  <li>error: code and message set in case an exception happens during the ‘callHierarchy/incomingCalls’ request</li>
</ul>

<h4 id="call-hierarchy-outgoing-calls-leftwards_arrow_with_hook"><a href="#callHierarchy_outgoingCalls" name="callHierarchy_outgoingCalls" class="anchor">Call Hierarchy Outgoing Calls (<img class="emoji" title=":leftwards_arrow_with_hook:" alt=":leftwards_arrow_with_hook:" src="https://github.githubassets.com/images/icons/emoji/unicode/21a9.png" height="20" width="20">)</a></h4>

<blockquote>
  <p><em>Since version 3.16.0</em></p>
</blockquote>

<p>The request is sent from the client to the server to resolve outgoing calls for a given call hierarchy item. The request doesn’t define its own client and server capabilities. It is only issued if a server registers for the <a href="#textDocument_prepareCallHierarchy"><code class="language-plaintext highlighter-rouge">textDocument/prepareCallHierarchy</code> request</a>.</p>

<p><em>Request</em>:</p>

<ul>
  <li>method: <code class="language-plaintext highlighter-rouge">callHierarchy/outgoingCalls</code>
</li>
  <li>params: <code class="language-plaintext highlighter-rouge">CallHierarchyOutgoingCallsParams</code> defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#callHierarchyOutgoingCallsParams" name="callHierarchyOutgoingCallsParams" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">CallHierarchyOutgoingCallsParams</span> <span class="kd">extends</span>
	<span class="nx">WorkDoneProgressParams</span><span class="p">,</span> <span class="nx">PartialResultParams</span> <span class="p">{</span>
	<span class="nl">item</span><span class="p">:</span> <span class="nx">CallHierarchyItem</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Response</em>:</p>

<ul>
  <li>result: <code class="language-plaintext highlighter-rouge">CallHierarchyOutgoingCall[] | null</code> defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#callHierarchyOutgoingCall" name="callHierarchyOutgoingCall" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">CallHierarchyOutgoingCall</span> <span class="p">{</span>

	<span class="cm">/**
	 * The item that is called.
	 */</span>
	<span class="nl">to</span><span class="p">:</span> <span class="nx">CallHierarchyItem</span><span class="p">;</span>

	<span class="cm">/**
	 * The range at which this item is called. This is the range relative to
	 * the caller, e.g the item passed to `callHierarchy/outgoingCalls` request.
	 */</span>
	<span class="nl">fromRanges</span><span class="p">:</span> <span class="nx">Range</span><span class="p">[];</span>
<span class="p">}</span>
</code></pre></div></div>

<ul>
  <li>partial result: <code class="language-plaintext highlighter-rouge">CallHierarchyOutgoingCall[]</code>
</li>
  <li>error: code and message set in case an exception happens during the ‘callHierarchy/outgoingCalls’ request</li>
</ul>

<h4 id="prepare-type-hierarchy-request-leftwards_arrow_with_hook"><a href="#textDocument_prepareTypeHierarchy" name="textDocument_prepareTypeHierarchy" class="anchor">Prepare Type Hierarchy Request (<img class="emoji" title=":leftwards_arrow_with_hook:" alt=":leftwards_arrow_with_hook:" src="https://github.githubassets.com/images/icons/emoji/unicode/21a9.png" height="20" width="20">)</a></h4>

<blockquote>
  <p><em>Since version 3.17.0</em></p>
</blockquote>

<p>The type hierarchy request is sent from the client to the server to return a type hierarchy for the language element of given text document positions. Will return <code class="language-plaintext highlighter-rouge">null</code> if the server couldn’t infer a valid type from the position. The type hierarchy requests are executed in two steps:</p>

<ol>
  <li>first a type hierarchy item is prepared for the given text document position.</li>
  <li>for a type hierarchy item the supertype or subtype type hierarchy items are resolved.</li>
</ol>

<p><em>Client Capability</em>:</p>

<ul>
  <li>property name (optional): <code class="language-plaintext highlighter-rouge">textDocument.typeHierarchy</code>
</li>
  <li>property type: <code class="language-plaintext highlighter-rouge">TypeHierarchyClientCapabilities</code> defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#typeHierarchyClientCapabilities" name="typeHierarchyClientCapabilities" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="kd">type</span> <span class="nx">TypeHierarchyClientCapabilities</span> <span class="o">=</span> <span class="p">{</span>
	<span class="cm">/**
	 * Whether implementation supports dynamic registration. If this is set to
	 * `true` the client supports the new `(TextDocumentRegistrationOptions &amp;
	 * StaticRegistrationOptions)` return value for the corresponding server
	 * capability as well.
	 */</span>
	<span class="nx">dynamicRegistration</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>
<span class="p">};</span>
</code></pre></div></div>

<p><em>Server Capability</em>:</p>

<ul>
  <li>property name (optional): <code class="language-plaintext highlighter-rouge">typeHierarchyProvider</code>
</li>
  <li>property type: <code class="language-plaintext highlighter-rouge">boolean | TypeHierarchyOptions | TypeHierarchyRegistrationOptions</code> where <code class="language-plaintext highlighter-rouge">TypeHierarchyOptions</code> is defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#typeHierarchyOptions" name="typeHierarchyOptions" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">TypeHierarchyOptions</span> <span class="kd">extends</span> <span class="nx">WorkDoneProgressOptions</span> <span class="p">{</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Registration Options</em>: <code class="language-plaintext highlighter-rouge">TypeHierarchyRegistrationOptions</code> defined as follows:</p>

<div class="anchorHolder"><a href="#typeHierarchyRegistrationOptions" name="typeHierarchyRegistrationOptions" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">TypeHierarchyRegistrationOptions</span> <span class="kd">extends</span>
	<span class="nx">TextDocumentRegistrationOptions</span><span class="p">,</span> <span class="nx">TypeHierarchyOptions</span><span class="p">,</span>
	<span class="nx">StaticRegistrationOptions</span> <span class="p">{</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Request</em>:</p>

<ul>
  <li>method: ‘textDocument/prepareTypeHierarchy’</li>
  <li>params: <code class="language-plaintext highlighter-rouge">TypeHierarchyPrepareParams</code> defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#typeHierarchyPrepareParams" name="typeHierarchyPrepareParams" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">TypeHierarchyPrepareParams</span> <span class="kd">extends</span> <span class="nx">TextDocumentPositionParams</span><span class="p">,</span>
	<span class="nx">WorkDoneProgressParams</span> <span class="p">{</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Response</em>:</p>

<ul>
  <li>result: <code class="language-plaintext highlighter-rouge">TypeHierarchyItem[] | null</code> defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#typeHierarchyItem" name="typeHierarchyItem" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">TypeHierarchyItem</span> <span class="p">{</span>
	<span class="cm">/**
	 * The name of this item.
	 */</span>
	<span class="nl">name</span><span class="p">:</span> <span class="kr">string</span><span class="p">;</span>

	<span class="cm">/**
	 * The kind of this item.
	 */</span>
	<span class="nl">kind</span><span class="p">:</span> <span class="nx">SymbolKind</span><span class="p">;</span>

	<span class="cm">/**
	 * Tags for this item.
	 */</span>
	<span class="nl">tags</span><span class="p">?:</span> <span class="nx">SymbolTag</span><span class="p">[];</span>

	<span class="cm">/**
	 * More detail for this item, e.g. the signature of a function.
	 */</span>
	<span class="nl">detail</span><span class="p">?:</span> <span class="kr">string</span><span class="p">;</span>

	<span class="cm">/**
	 * The resource identifier of this item.
	 */</span>
	<span class="nl">uri</span><span class="p">:</span> <span class="nx">DocumentUri</span><span class="p">;</span>

	<span class="cm">/**
	 * The range enclosing this symbol not including leading/trailing whitespace
	 * but everything else, e.g. comments and code.
	 */</span>
	<span class="nl">range</span><span class="p">:</span> <span class="nx">Range</span><span class="p">;</span>

	<span class="cm">/**
	 * The range that should be selected and revealed when this symbol is being
	 * picked, e.g. the name of a function. Must be contained by the
	 * [`range`](#TypeHierarchyItem.range).
	 */</span>
	<span class="nl">selectionRange</span><span class="p">:</span> <span class="nx">Range</span><span class="p">;</span>

	<span class="cm">/**
	 * A data entry field that is preserved between a type hierarchy prepare and
	 * supertypes or subtypes requests. It could also be used to identify the
	 * type hierarchy in the server, helping improve the performance on
	 * resolving supertypes and subtypes.
	 */</span>
	<span class="nl">data</span><span class="p">?:</span> <span class="nx">LSPAny</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<ul>
  <li>error: code and message set in case an exception happens during the ‘textDocument/prepareTypeHierarchy’ request</li>
</ul>

<h4 id="type-hierarchy-supertypesleftwards_arrow_with_hook"><a href="#typeHierarchy_supertypes" name="typeHierarchy_supertypes" class="anchor">Type Hierarchy Supertypes(<img class="emoji" title=":leftwards_arrow_with_hook:" alt=":leftwards_arrow_with_hook:" src="https://github.githubassets.com/images/icons/emoji/unicode/21a9.png" height="20" width="20">)</a></h4>

<blockquote>
  <p><em>Since version 3.17.0</em></p>
</blockquote>

<p>The request is sent from the client to the server to resolve the supertypes for a given type hierarchy item. Will return <code class="language-plaintext highlighter-rouge">null</code> if the server couldn’t infer a valid type from <code class="language-plaintext highlighter-rouge">item</code> in the params. The request doesn’t define its own client and server capabilities. It is only issued if a server registers for the <a href="#textDocument_prepareTypeHierarchy"><code class="language-plaintext highlighter-rouge">textDocument/prepareTypeHierarchy</code> request</a>.</p>

<p><em>Request</em>:</p>

<ul>
  <li>method: ‘typeHierarchy/supertypes’</li>
  <li>params: <code class="language-plaintext highlighter-rouge">TypeHierarchySupertypesParams</code> defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#typeHierarchySupertypesParams" name="typeHierarchySupertypesParams" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">TypeHierarchySupertypesParams</span> <span class="kd">extends</span>
	<span class="nx">WorkDoneProgressParams</span><span class="p">,</span> <span class="nx">PartialResultParams</span> <span class="p">{</span>
	<span class="nl">item</span><span class="p">:</span> <span class="nx">TypeHierarchyItem</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>
<p><em>Response</em>:</p>

<ul>
  <li>result: <code class="language-plaintext highlighter-rouge">TypeHierarchyItem[] | null</code>
</li>
  <li>partial result: <code class="language-plaintext highlighter-rouge">TypeHierarchyItem[]</code>
</li>
  <li>error: code and message set in case an exception happens during the ‘typeHierarchy/supertypes’ request</li>
</ul>

<h4 id="type-hierarchy-subtypesleftwards_arrow_with_hook"><a href="#typeHierarchy_subtypes" name="typeHierarchy_subtypes" class="anchor">Type Hierarchy Subtypes(<img class="emoji" title=":leftwards_arrow_with_hook:" alt=":leftwards_arrow_with_hook:" src="https://github.githubassets.com/images/icons/emoji/unicode/21a9.png" height="20" width="20">)</a></h4>

<blockquote>
  <p><em>Since version 3.17.0</em></p>
</blockquote>

<p>The request is sent from the client to the server to resolve the subtypes for a given type hierarchy item. Will return <code class="language-plaintext highlighter-rouge">null</code> if the server couldn’t infer a valid type from <code class="language-plaintext highlighter-rouge">item</code> in the params. The request doesn’t define its own client and server capabilities. It is only issued if a server registers for the <a href="#textDocument_prepareTypeHierarchy"><code class="language-plaintext highlighter-rouge">textDocument/prepareTypeHierarchy</code> request</a>.</p>

<p><em>Request</em>:</p>

<ul>
  <li>method: ‘typeHierarchy/subtypes’</li>
  <li>params: <code class="language-plaintext highlighter-rouge">TypeHierarchySubtypesParams</code> defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#typeHierarchySubtypesParams" name="typeHierarchySubtypesParams" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">TypeHierarchySubtypesParams</span> <span class="kd">extends</span>
	<span class="nx">WorkDoneProgressParams</span><span class="p">,</span> <span class="nx">PartialResultParams</span> <span class="p">{</span>
	<span class="nl">item</span><span class="p">:</span> <span class="nx">TypeHierarchyItem</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>
<p><em>Response</em>:</p>

<ul>
  <li>result: <code class="language-plaintext highlighter-rouge">TypeHierarchyItem[] | null</code>
</li>
  <li>partial result: <code class="language-plaintext highlighter-rouge">TypeHierarchyItem[]</code>
</li>
  <li>error: code and message set in case an exception happens during the ‘typeHierarchy/subtypes’ request</li>
</ul>

<h4 id="document-highlights-request-leftwards_arrow_with_hook"><a href="#textDocument_documentHighlight" name="textDocument_documentHighlight" class="anchor">Document Highlights Request (<img class="emoji" title=":leftwards_arrow_with_hook:" alt=":leftwards_arrow_with_hook:" src="https://github.githubassets.com/images/icons/emoji/unicode/21a9.png" height="20" width="20">)</a></h4>

<p>The document highlight request is sent from the client to the server to resolve document highlights for a given text document position.
For programming languages this usually highlights all references to the symbol scoped to this file. However, we kept ‘textDocument/documentHighlight’
and ‘textDocument/references’ separate requests since the first one is allowed to be more fuzzy. Symbol matches usually have a <code class="language-plaintext highlighter-rouge">DocumentHighlightKind</code>
of <code class="language-plaintext highlighter-rouge">Read</code> or <code class="language-plaintext highlighter-rouge">Write</code> whereas fuzzy or textual matches use <code class="language-plaintext highlighter-rouge">Text</code> as the kind.</p>

<p><em>Client Capability</em>:</p>
<ul>
  <li>property name (optional): <code class="language-plaintext highlighter-rouge">textDocument.documentHighlight</code>
</li>
  <li>property type: <code class="language-plaintext highlighter-rouge">DocumentHighlightClientCapabilities</code> defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#documentHighlightClientCapabilities" name="documentHighlightClientCapabilities" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">DocumentHighlightClientCapabilities</span> <span class="p">{</span>
	<span class="cm">/**
	 * Whether document highlight supports dynamic registration.
	 */</span>
	<span class="nl">dynamicRegistration</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Server Capability</em>:</p>
<ul>
  <li>property name (optional): <code class="language-plaintext highlighter-rouge">documentHighlightProvider</code>
</li>
  <li>property type: <code class="language-plaintext highlighter-rouge">boolean | DocumentHighlightOptions</code> where <code class="language-plaintext highlighter-rouge">DocumentHighlightOptions</code> is defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#documentHighlightOptions" name="documentHighlightOptions" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">DocumentHighlightOptions</span> <span class="kd">extends</span> <span class="nx">WorkDoneProgressOptions</span> <span class="p">{</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Registration Options</em>: <code class="language-plaintext highlighter-rouge">DocumentHighlightRegistrationOptions</code> defined as follows:</p>

<div class="anchorHolder"><a href="#documentHighlightRegistrationOptions" name="documentHighlightRegistrationOptions" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">DocumentHighlightRegistrationOptions</span> <span class="kd">extends</span>
	<span class="nx">TextDocumentRegistrationOptions</span><span class="p">,</span> <span class="nx">DocumentHighlightOptions</span> <span class="p">{</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Request</em>:</p>
<ul>
  <li>method: <code class="language-plaintext highlighter-rouge">textDocument/documentHighlight</code>
</li>
  <li>params: <code class="language-plaintext highlighter-rouge">DocumentHighlightParams</code> defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#documentHighlightParams" name="documentHighlightParams" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">DocumentHighlightParams</span> <span class="kd">extends</span> <span class="nx">TextDocumentPositionParams</span><span class="p">,</span>
	<span class="nx">WorkDoneProgressParams</span><span class="p">,</span> <span class="nx">PartialResultParams</span> <span class="p">{</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Response</em>:</p>
<ul>
  <li>result: <code class="language-plaintext highlighter-rouge">DocumentHighlight[]</code> | <code class="language-plaintext highlighter-rouge">null</code> defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#documentHighlight" name="documentHighlight" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * A document highlight is a range inside a text document which deserves
 * special attention. Usually a document highlight is visualized by changing
 * the background color of its range.
 *
 */</span>
<span class="k">export</span> <span class="kr">interface</span> <span class="nx">DocumentHighlight</span> <span class="p">{</span>
	<span class="cm">/**
	 * The range this highlight applies to.
	 */</span>
	<span class="nl">range</span><span class="p">:</span> <span class="nx">Range</span><span class="p">;</span>

	<span class="cm">/**
	 * The highlight kind, default is DocumentHighlightKind.Text.
	 */</span>
	<span class="nl">kind</span><span class="p">?:</span> <span class="nx">DocumentHighlightKind</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<div class="anchorHolder"><a href="#documentHighlightKind" name="documentHighlightKind" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * A document highlight kind.
 */</span>
<span class="k">export</span> <span class="k">namespace</span> <span class="nx">DocumentHighlightKind</span> <span class="p">{</span>
	<span class="cm">/**
	 * A textual occurrence.
	 */</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">Text</span> <span class="o">=</span> <span class="mi">1</span><span class="p">;</span>

	<span class="cm">/**
	 * Read-access of a symbol, like reading a variable.
	 */</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">Read</span> <span class="o">=</span> <span class="mi">2</span><span class="p">;</span>

	<span class="cm">/**
	 * Write-access of a symbol, like writing to a variable.
	 */</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">Write</span> <span class="o">=</span> <span class="mi">3</span><span class="p">;</span>
<span class="p">}</span>

<span class="k">export</span> <span class="kd">type</span> <span class="nx">DocumentHighlightKind</span> <span class="o">=</span> <span class="mi">1</span> <span class="o">|</span> <span class="mi">2</span> <span class="o">|</span> <span class="mi">3</span><span class="p">;</span>
</code></pre></div></div>

<ul>
  <li>partial result: <code class="language-plaintext highlighter-rouge">DocumentHighlight[]</code>
</li>
  <li>error: code and message set in case an exception happens during the document highlight request.</li>
</ul>

<h4 id="document-link-request-leftwards_arrow_with_hook"><a href="#textDocument_documentLink" name="textDocument_documentLink" class="anchor">Document Link Request (<img class="emoji" title=":leftwards_arrow_with_hook:" alt=":leftwards_arrow_with_hook:" src="https://github.githubassets.com/images/icons/emoji/unicode/21a9.png" height="20" width="20">)</a></h4>

<p>The document links request is sent from the client to the server to request the location of links in a document.</p>

<p><em>Client Capability</em>:</p>
<ul>
  <li>property name (optional): <code class="language-plaintext highlighter-rouge">textDocument.documentLink</code>
</li>
  <li>property type: <code class="language-plaintext highlighter-rouge">DocumentLinkClientCapabilities</code> defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#documentLinkClientCapabilities" name="documentLinkClientCapabilities" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">DocumentLinkClientCapabilities</span> <span class="p">{</span>
	<span class="cm">/**
	 * Whether document link supports dynamic registration.
	 */</span>
	<span class="nl">dynamicRegistration</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>

	<span class="cm">/**
	 * Whether the client supports the `tooltip` property on `DocumentLink`.
	 *
	 * @since 3.15.0
	 */</span>
	<span class="nl">tooltipSupport</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Server Capability</em>:</p>
<ul>
  <li>property name (optional): <code class="language-plaintext highlighter-rouge">documentLinkProvider</code>
</li>
  <li>property type: <code class="language-plaintext highlighter-rouge">DocumentLinkOptions</code> defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#documentLinkOptions" name="documentLinkOptions" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">DocumentLinkOptions</span> <span class="kd">extends</span> <span class="nx">WorkDoneProgressOptions</span> <span class="p">{</span>
	<span class="cm">/**
	 * Document links have a resolve provider as well.
	 */</span>
	<span class="nl">resolveProvider</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Registration Options</em>: <code class="language-plaintext highlighter-rouge">DocumentLinkRegistrationOptions</code> defined as follows:</p>

<div class="anchorHolder"><a href="#documentLinkRegistrationOptions" name="documentLinkRegistrationOptions" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">DocumentLinkRegistrationOptions</span> <span class="kd">extends</span>
	<span class="nx">TextDocumentRegistrationOptions</span><span class="p">,</span> <span class="nx">DocumentLinkOptions</span> <span class="p">{</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Request</em>:</p>
<ul>
  <li>method: <code class="language-plaintext highlighter-rouge">textDocument/documentLink</code>
</li>
  <li>params: <code class="language-plaintext highlighter-rouge">DocumentLinkParams</code> defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#documentLinkParams" name="documentLinkParams" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="kr">interface</span> <span class="nx">DocumentLinkParams</span> <span class="kd">extends</span> <span class="nx">WorkDoneProgressParams</span><span class="p">,</span>
	<span class="nx">PartialResultParams</span> <span class="p">{</span>
	<span class="cm">/**
	 * The document to provide document links for.
	 */</span>
	<span class="nl">textDocument</span><span class="p">:</span> <span class="nx">TextDocumentIdentifier</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Response</em>:</p>
<ul>
  <li>result: <code class="language-plaintext highlighter-rouge">DocumentLink[]</code> | <code class="language-plaintext highlighter-rouge">null</code>.</li>
</ul>

<div class="anchorHolder"><a href="#documentLink" name="documentLink" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * A document link is a range in a text document that links to an internal or
 * external resource, like another text document or a web site.
 */</span>
<span class="kr">interface</span> <span class="nx">DocumentLink</span> <span class="p">{</span>
	<span class="cm">/**
	 * The range this link applies to.
	 */</span>
	<span class="nl">range</span><span class="p">:</span> <span class="nx">Range</span><span class="p">;</span>

	<span class="cm">/**
	 * The uri this link points to. If missing a resolve request is sent later.
	 */</span>
	<span class="nl">target</span><span class="p">?:</span> <span class="nx">URI</span><span class="p">;</span>

	<span class="cm">/**
	 * The tooltip text when you hover over this link.
	 *
	 * If a tooltip is provided, is will be displayed in a string that includes
	 * instructions on how to trigger the link, such as `{0} (ctrl + click)`.
	 * The specific instructions vary depending on OS, user settings, and
	 * localization.
	 *
	 * @since 3.15.0
	 */</span>
	<span class="nl">tooltip</span><span class="p">?:</span> <span class="kr">string</span><span class="p">;</span>

	<span class="cm">/**
	 * A data entry field that is preserved on a document link between a
	 * DocumentLinkRequest and a DocumentLinkResolveRequest.
	 */</span>
	<span class="nl">data</span><span class="p">?:</span> <span class="nx">LSPAny</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>
<ul>
  <li>partial result: <code class="language-plaintext highlighter-rouge">DocumentLink[]</code>
</li>
  <li>error: code and message set in case an exception happens during the document link request.</li>
</ul>

<h4 id="document-link-resolve-request-leftwards_arrow_with_hook"><a href="#documentLink_resolve" name="documentLink_resolve" class="anchor">Document Link Resolve Request (<img class="emoji" title=":leftwards_arrow_with_hook:" alt=":leftwards_arrow_with_hook:" src="https://github.githubassets.com/images/icons/emoji/unicode/21a9.png" height="20" width="20">)</a></h4>

<p>The document link resolve request is sent from the client to the server to resolve the target of a given document link.</p>

<p><em>Request</em>:</p>
<ul>
  <li>method: <code class="language-plaintext highlighter-rouge">documentLink/resolve</code>
</li>
  <li>params: <code class="language-plaintext highlighter-rouge">DocumentLink</code>
</li>
</ul>

<p><em>Response</em>:</p>
<ul>
  <li>result: <code class="language-plaintext highlighter-rouge">DocumentLink</code>
</li>
  <li>error: code and message set in case an exception happens during the document link resolve request.</li>
</ul>

<h4 id="hover-request-leftwards_arrow_with_hook"><a href="#textDocument_hover" name="textDocument_hover" class="anchor">Hover Request (<img class="emoji" title=":leftwards_arrow_with_hook:" alt=":leftwards_arrow_with_hook:" src="https://github.githubassets.com/images/icons/emoji/unicode/21a9.png" height="20" width="20">)</a></h4>

<p>The hover request is sent from the client to the server to request hover information at a given text document position.</p>

<p><em>Client Capability</em>:</p>
<ul>
  <li>property name (optional): <code class="language-plaintext highlighter-rouge">textDocument.hover</code>
</li>
  <li>property type: <code class="language-plaintext highlighter-rouge">HoverClientCapabilities</code> defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#hoverClientCapabilities" name="hoverClientCapabilities" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">HoverClientCapabilities</span> <span class="p">{</span>
	<span class="cm">/**
	 * Whether hover supports dynamic registration.
	 */</span>
	<span class="nl">dynamicRegistration</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>

	<span class="cm">/**
	 * Client supports the follow content formats if the content
	 * property refers to a `literal of type MarkupContent`.
	 * The order describes the preferred format of the client.
	 */</span>
	<span class="nl">contentFormat</span><span class="p">?:</span> <span class="nx">MarkupKind</span><span class="p">[];</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Server Capability</em>:</p>
<ul>
  <li>property name (optional): <code class="language-plaintext highlighter-rouge">hoverProvider</code>
</li>
  <li>property type: <code class="language-plaintext highlighter-rouge">boolean | HoverOptions</code> where <code class="language-plaintext highlighter-rouge">HoverOptions</code> is defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#hoverOptions" name="hoverOptions" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">HoverOptions</span> <span class="kd">extends</span> <span class="nx">WorkDoneProgressOptions</span> <span class="p">{</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Registration Options</em>: <code class="language-plaintext highlighter-rouge">HoverRegistrationOptions</code> defined as follows:</p>

<div class="anchorHolder"><a href="#hoverRegistrationOptions" name="hoverRegistrationOptions" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">HoverRegistrationOptions</span>
	<span class="kd">extends</span> <span class="nx">TextDocumentRegistrationOptions</span><span class="p">,</span> <span class="nx">HoverOptions</span> <span class="p">{</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Request</em>:</p>
<ul>
  <li>method: <code class="language-plaintext highlighter-rouge">textDocument/hover</code>
</li>
  <li>params: <code class="language-plaintext highlighter-rouge">HoverParams</code> defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#hoverParams" name="hoverParams" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">HoverParams</span> <span class="kd">extends</span> <span class="nx">TextDocumentPositionParams</span><span class="p">,</span>
	<span class="nx">WorkDoneProgressParams</span> <span class="p">{</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Response</em>:</p>
<ul>
  <li>result: <code class="language-plaintext highlighter-rouge">Hover</code> | <code class="language-plaintext highlighter-rouge">null</code> defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#hover" name="hover" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * The result of a hover request.
 */</span>
<span class="k">export</span> <span class="kr">interface</span> <span class="nx">Hover</span> <span class="p">{</span>
	<span class="cm">/**
	 * The hover's content
	 */</span>
	<span class="nl">contents</span><span class="p">:</span> <span class="nx">MarkedString</span> <span class="o">|</span> <span class="nx">MarkedString</span><span class="p">[]</span> <span class="o">|</span> <span class="nx">MarkupContent</span><span class="p">;</span>

	<span class="cm">/**
	 * An optional range is a range inside a text document
	 * that is used to visualize a hover, e.g. by changing the background color.
	 */</span>
	<span class="nl">range</span><span class="p">?:</span> <span class="nx">Range</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<p>Where <code class="language-plaintext highlighter-rouge">MarkedString</code> is defined as follows:</p>

<div class="anchorHolder"><a href="#markedString" name="markedString" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * MarkedString can be used to render human readable text. It is either a
 * markdown string or a code-block that provides a language and a code snippet.
 * The language identifier is semantically equal to the optional language
 * identifier in fenced code blocks in GitHub issues.
 *
 * The pair of a language and a value is an equivalent to markdown:
 * ```${language}
 * ${value}
 * ```
 *
 * Note that markdown strings will be sanitized - that means html will be
 * escaped.
 *
 * @deprecated use MarkupContent instead.
 */</span>
<span class="kd">type</span> <span class="nx">MarkedString</span> <span class="o">=</span> <span class="kr">string</span> <span class="o">|</span> <span class="p">{</span> <span class="na">language</span><span class="p">:</span> <span class="kr">string</span><span class="p">;</span> <span class="nl">value</span><span class="p">:</span> <span class="kr">string</span> <span class="p">};</span>
</code></pre></div></div>

<ul>
  <li>error: code and message set in case an exception happens during the hover request.</li>
</ul>

<h4 id="code-lens-request-leftwards_arrow_with_hook"><a href="#textDocument_codeLens" name="textDocument_codeLens" class="anchor">Code Lens Request (<img class="emoji" title=":leftwards_arrow_with_hook:" alt=":leftwards_arrow_with_hook:" src="https://github.githubassets.com/images/icons/emoji/unicode/21a9.png" height="20" width="20">)</a></h4>

<p>The code lens request is sent from the client to the server to compute code lenses for a given text document.</p>

<p><em>Client Capability</em>:</p>
<ul>
  <li>property name (optional): <code class="language-plaintext highlighter-rouge">textDocument.codeLens</code>
</li>
  <li>property type: <code class="language-plaintext highlighter-rouge">CodeLensClientCapabilities</code> defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#codeLensClientCapabilities" name="codeLensClientCapabilities" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">CodeLensClientCapabilities</span> <span class="p">{</span>
	<span class="cm">/**
	 * Whether code lens supports dynamic registration.
	 */</span>
	<span class="nl">dynamicRegistration</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Server Capability</em>:</p>
<ul>
  <li>property name (optional): <code class="language-plaintext highlighter-rouge">codeLensProvider</code>
</li>
  <li>property type: <code class="language-plaintext highlighter-rouge">CodeLensOptions</code> defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#codeLensOptions" name="codeLensOptions" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">CodeLensOptions</span> <span class="kd">extends</span> <span class="nx">WorkDoneProgressOptions</span> <span class="p">{</span>
	<span class="cm">/**
	 * Code lens has a resolve provider as well.
	 */</span>
	<span class="nl">resolveProvider</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Registration Options</em>: <code class="language-plaintext highlighter-rouge">CodeLensRegistrationOptions</code> defined as follows:</p>

<div class="anchorHolder"><a href="#codeLensRegistrationOptions" name="codeLensRegistrationOptions" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">CodeLensRegistrationOptions</span> <span class="kd">extends</span>
	<span class="nx">TextDocumentRegistrationOptions</span><span class="p">,</span> <span class="nx">CodeLensOptions</span> <span class="p">{</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Request</em>:</p>
<ul>
  <li>method: <code class="language-plaintext highlighter-rouge">textDocument/codeLens</code>
</li>
  <li>params: <code class="language-plaintext highlighter-rouge">CodeLensParams</code> defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#codeLensParams" name="codeLensParams" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="kr">interface</span> <span class="nx">CodeLensParams</span> <span class="kd">extends</span> <span class="nx">WorkDoneProgressParams</span><span class="p">,</span> <span class="nx">PartialResultParams</span> <span class="p">{</span>
	<span class="cm">/**
	 * The document to request code lens for.
	 */</span>
	<span class="nl">textDocument</span><span class="p">:</span> <span class="nx">TextDocumentIdentifier</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Response</em>:</p>
<ul>
  <li>result: <code class="language-plaintext highlighter-rouge">CodeLens[]</code> | <code class="language-plaintext highlighter-rouge">null</code> defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#codeLens" name="codeLens" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * A code lens represents a command that should be shown along with
 * source text, like the number of references, a way to run tests, etc.
 *
 * A code lens is _unresolved_ when no command is associated to it. For
 * performance reasons the creation of a code lens and resolving should be done
 * in two stages.
 */</span>
<span class="kr">interface</span> <span class="nx">CodeLens</span> <span class="p">{</span>
	<span class="cm">/**
	 * The range in which this code lens is valid. Should only span a single
	 * line.
	 */</span>
	<span class="nl">range</span><span class="p">:</span> <span class="nx">Range</span><span class="p">;</span>

	<span class="cm">/**
	 * The command this code lens represents.
	 */</span>
	<span class="nl">command</span><span class="p">?:</span> <span class="nx">Command</span><span class="p">;</span>

	<span class="cm">/**
	 * A data entry field that is preserved on a code lens item between
	 * a code lens and a code lens resolve request.
	 */</span>
	<span class="nl">data</span><span class="p">?:</span> <span class="nx">LSPAny</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>
<ul>
  <li>partial result: <code class="language-plaintext highlighter-rouge">CodeLens[]</code>
</li>
  <li>error: code and message set in case an exception happens during the code lens request.</li>
</ul>

<h4 id="code-lens-resolve-request-leftwards_arrow_with_hook"><a href="#codeLens_resolve" name="codeLens_resolve" class="anchor">Code Lens Resolve Request (<img class="emoji" title=":leftwards_arrow_with_hook:" alt=":leftwards_arrow_with_hook:" src="https://github.githubassets.com/images/icons/emoji/unicode/21a9.png" height="20" width="20">)</a></h4>

<p>The code lens resolve request is sent from the client to the server to resolve the command for a given code lens item.</p>

<p><em>Request</em>:</p>
<ul>
  <li>method: <code class="language-plaintext highlighter-rouge">codeLens/resolve</code>
</li>
  <li>params: <code class="language-plaintext highlighter-rouge">CodeLens</code>
</li>
</ul>

<p><em>Response</em>:</p>
<ul>
  <li>result: <code class="language-plaintext highlighter-rouge">CodeLens</code>
</li>
  <li>error: code and message set in case an exception happens during the code lens resolve request.</li>
</ul>

<h4 id="code-lens-refresh-request-arrow_right_hook"><a href="#codeLens_refresh" name="codeLens_refresh" class="anchor">Code Lens Refresh Request (<img class="emoji" title=":arrow_right_hook:" alt=":arrow_right_hook:" src="https://github.githubassets.com/images/icons/emoji/unicode/21aa.png" height="20" width="20">)</a></h4>

<blockquote>
  <p><em>Since version 3.16.0</em></p>
</blockquote>

<p>The <code class="language-plaintext highlighter-rouge">workspace/codeLens/refresh</code> request is sent from the server to the client. Servers can use it to ask clients to refresh the code lenses currently shown in editors. As a result the client should ask the server to recompute the code lenses for these editors. This is useful if a server detects a configuration change which requires a re-calculation of all code lenses. Note that the client still has the freedom to delay the re-calculation of the code lenses if for example an editor is currently not visible.</p>

<p><em>Client Capability</em>:</p>

<ul>
  <li>property name (optional): <code class="language-plaintext highlighter-rouge">workspace.codeLens</code>
</li>
  <li>property type: <code class="language-plaintext highlighter-rouge">CodeLensWorkspaceClientCapabilities</code> defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#codeLensWorkspaceClientCapabilities" name="codeLensWorkspaceClientCapabilities" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">CodeLensWorkspaceClientCapabilities</span> <span class="p">{</span>
	<span class="cm">/**
	 * Whether the client implementation supports a refresh request sent from the
	 * server to the client.
	 *
	 * Note that this event is global and will force the client to refresh all
	 * code lenses currently shown. It should be used with absolute care and is
	 * useful for situation where a server for example detect a project wide
	 * change that requires such a calculation.
	 */</span>
	<span class="nl">refreshSupport</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Request</em>:</p>

<ul>
  <li>method: <code class="language-plaintext highlighter-rouge">workspace/codeLens/refresh</code>
</li>
  <li>params: none</li>
</ul>

<p><em>Response</em>:</p>

<ul>
  <li>result: void</li>
  <li>error: code and message set in case an exception happens during the ‘workspace/codeLens/refresh’ request</li>
</ul>

<h4 id="folding-range-request-leftwards_arrow_with_hook"><a href="#textDocument_foldingRange" name="textDocument_foldingRange" class="anchor">Folding Range Request (<img class="emoji" title=":leftwards_arrow_with_hook:" alt=":leftwards_arrow_with_hook:" src="https://github.githubassets.com/images/icons/emoji/unicode/21a9.png" height="20" width="20">)</a></h4>

<blockquote>
  <p><em>Since version 3.10.0</em></p>
</blockquote>

<p>The folding range request is sent from the client to the server to return all folding ranges found in a given text document.</p>

<p><em>Client Capability</em>:</p>
<ul>
  <li>property name (optional): <code class="language-plaintext highlighter-rouge">textDocument.foldingRange</code>
</li>
  <li>property type: <code class="language-plaintext highlighter-rouge">FoldingRangeClientCapabilities</code> defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#foldingRangeClientCapabilities" name="foldingRangeClientCapabilities" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">FoldingRangeClientCapabilities</span> <span class="p">{</span>
	<span class="cm">/**
	 * Whether implementation supports dynamic registration for folding range
	 * providers. If this is set to `true` the client supports the new
	 * `FoldingRangeRegistrationOptions` return value for the corresponding
	 * server capability as well.
	 */</span>
	<span class="nl">dynamicRegistration</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>

	<span class="cm">/**
	 * The maximum number of folding ranges that the client prefers to receive
	 * per document. The value serves as a hint, servers are free to follow the
	 * limit.
	 */</span>
	<span class="nl">rangeLimit</span><span class="p">?:</span> <span class="nx">uinteger</span><span class="p">;</span>

	<span class="cm">/**
	 * If set, the client signals that it only supports folding complete lines.
	 * If set, client will ignore specified `startCharacter` and `endCharacter`
	 * properties in a FoldingRange.
	 */</span>
	<span class="nl">lineFoldingOnly</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>

	<span class="cm">/**
	 * Specific options for the folding range kind.
	 *
	 * @since 3.17.0
	 */</span>
	<span class="nl">foldingRangeKind</span><span class="p">?</span> <span class="p">:</span> <span class="p">{</span>
		<span class="cm">/**
		 * The folding range kind values the client supports. When this
		 * property exists the client also guarantees that it will
		 * handle values outside its set gracefully and falls back
		 * to a default value when unknown.
		 */</span>
		<span class="nx">valueSet</span><span class="p">?:</span> <span class="nx">FoldingRangeKind</span><span class="p">[];</span>
	<span class="p">};</span>

	<span class="cm">/**
	 * Specific options for the folding range.
	 * @since 3.17.0
	 */</span>
	<span class="nl">foldingRange</span><span class="p">?:</span> <span class="p">{</span>
		<span class="cm">/**
		* If set, the client signals that it supports setting collapsedText on
		* folding ranges to display custom labels instead of the default text.
		*
		* @since 3.17.0
		*/</span>
		<span class="nx">collapsedText</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>
	<span class="p">};</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Server Capability</em>:</p>
<ul>
  <li>property name (optional): <code class="language-plaintext highlighter-rouge">foldingRangeProvider</code>
</li>
  <li>property type: <code class="language-plaintext highlighter-rouge">boolean | FoldingRangeOptions | FoldingRangeRegistrationOptions</code> where <code class="language-plaintext highlighter-rouge">FoldingRangeOptions</code> is defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#foldingRangeOptions" name="foldingRangeOptions" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">FoldingRangeOptions</span> <span class="kd">extends</span> <span class="nx">WorkDoneProgressOptions</span> <span class="p">{</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Registration Options</em>: <code class="language-plaintext highlighter-rouge">FoldingRangeRegistrationOptions</code> defined as follows:</p>

<div class="anchorHolder"><a href="#foldingRangeRegistrationOptions" name="foldingRangeRegistrationOptions" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">FoldingRangeRegistrationOptions</span> <span class="kd">extends</span>
	<span class="nx">TextDocumentRegistrationOptions</span><span class="p">,</span> <span class="nx">FoldingRangeOptions</span><span class="p">,</span>
	<span class="nx">StaticRegistrationOptions</span> <span class="p">{</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Request</em>:</p>

<ul>
  <li>method: <code class="language-plaintext highlighter-rouge">textDocument/foldingRange</code>
</li>
  <li>params: <code class="language-plaintext highlighter-rouge">FoldingRangeParams</code> defined as follows</li>
</ul>

<div class="anchorHolder"><a href="#foldingRangeParams" name="foldingRangeParams" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">FoldingRangeParams</span> <span class="kd">extends</span> <span class="nx">WorkDoneProgressParams</span><span class="p">,</span>
	<span class="nx">PartialResultParams</span> <span class="p">{</span>
	<span class="cm">/**
	 * The text document.
	 */</span>
	<span class="nl">textDocument</span><span class="p">:</span> <span class="nx">TextDocumentIdentifier</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Response</em>:</p>
<ul>
  <li>result: <code class="language-plaintext highlighter-rouge">FoldingRange[] | null</code> defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#foldingRangeKind" name="foldingRangeKind" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * A set of predefined range kinds.
 */</span>
<span class="k">export</span> <span class="k">namespace</span> <span class="nx">FoldingRangeKind</span> <span class="p">{</span>
	<span class="cm">/**
	 * Folding range for a comment
	 */</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">Comment</span> <span class="o">=</span> <span class="dl">'</span><span class="s1">comment</span><span class="dl">'</span><span class="p">;</span>

	<span class="cm">/**
	 * Folding range for imports or includes
	 */</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">Imports</span> <span class="o">=</span> <span class="dl">'</span><span class="s1">imports</span><span class="dl">'</span><span class="p">;</span>

	<span class="cm">/**
	 * Folding range for a region (e.g. `#region`)
	 */</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">Region</span> <span class="o">=</span> <span class="dl">'</span><span class="s1">region</span><span class="dl">'</span><span class="p">;</span>
<span class="p">}</span>

<span class="cm">/**
 * The type is a string since the value set is extensible
 */</span>
<span class="k">export</span> <span class="kd">type</span> <span class="nx">FoldingRangeKind</span> <span class="o">=</span> <span class="kr">string</span><span class="p">;</span>
</code></pre></div></div>

<div class="anchorHolder"><a href="#foldingRange" name="foldingRange" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * Represents a folding range. To be valid, start and end line must be bigger
 * than zero and smaller than the number of lines in the document. Clients
 * are free to ignore invalid ranges.
 */</span>
<span class="k">export</span> <span class="kr">interface</span> <span class="nx">FoldingRange</span> <span class="p">{</span>

	<span class="cm">/**
	 * The zero-based start line of the range to fold. The folded area starts
	 * after the line's last character. To be valid, the end must be zero or
	 * larger and smaller than the number of lines in the document.
	 */</span>
	<span class="nl">startLine</span><span class="p">:</span> <span class="nx">uinteger</span><span class="p">;</span>

	<span class="cm">/**
	 * The zero-based character offset from where the folded range starts. If
	 * not defined, defaults to the length of the start line.
	 */</span>
	<span class="nl">startCharacter</span><span class="p">?:</span> <span class="nx">uinteger</span><span class="p">;</span>

	<span class="cm">/**
	 * The zero-based end line of the range to fold. The folded area ends with
	 * the line's last character. To be valid, the end must be zero or larger
	 * and smaller than the number of lines in the document.
	 */</span>
	<span class="nl">endLine</span><span class="p">:</span> <span class="nx">uinteger</span><span class="p">;</span>

	<span class="cm">/**
	 * The zero-based character offset before the folded range ends. If not
	 * defined, defaults to the length of the end line.
	 */</span>
	<span class="nl">endCharacter</span><span class="p">?:</span> <span class="nx">uinteger</span><span class="p">;</span>

	<span class="cm">/**
	 * Describes the kind of the folding range such as `comment` or `region`.
	 * The kind is used to categorize folding ranges and used by commands like
	 * 'Fold all comments'. See [FoldingRangeKind](#FoldingRangeKind) for an
	 * enumeration of standardized kinds.
	 */</span>
	<span class="nl">kind</span><span class="p">?:</span> <span class="nx">FoldingRangeKind</span><span class="p">;</span>

	<span class="cm">/**
	 * The text that the client should show when the specified range is
	 * collapsed. If not defined or not supported by the client, a default
	 * will be chosen by the client.
	 *
	 * @since 3.17.0 - proposed
	 */</span>
	<span class="nl">collapsedText</span><span class="p">?:</span> <span class="kr">string</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<ul>
  <li>partial result: <code class="language-plaintext highlighter-rouge">FoldingRange[]</code>
</li>
  <li>error: code and message set in case an exception happens during the ‘textDocument/foldingRange’ request</li>
</ul>

<h4 id="selection-range-request-leftwards_arrow_with_hook"><a href="#textDocument_selectionRange" name="textDocument_selectionRange" class="anchor">Selection Range Request (<img class="emoji" title=":leftwards_arrow_with_hook:" alt=":leftwards_arrow_with_hook:" src="https://github.githubassets.com/images/icons/emoji/unicode/21a9.png" height="20" width="20">)</a></h4>

<blockquote>
  <p><em>Since version 3.15.0</em></p>
</blockquote>

<p>The selection range request is sent from the client to the server to return suggested selection ranges at an array of given positions. A selection range is a range around the cursor position which the user might be interested in selecting.</p>

<p>A selection range in the return array is for the position in the provided parameters at the same index. Therefore positions[i] must be contained in result[i].range. To allow for results where some positions have selection ranges and others do not, result[i].range is allowed to be the empty range at positions[i].</p>

<p>Typically, but not necessary, selection ranges correspond to the nodes of the syntax tree.</p>

<p><em>Client Capability</em>:</p>
<ul>
  <li>property name (optional): <code class="language-plaintext highlighter-rouge">textDocument.selectionRange</code>
</li>
  <li>property type: <code class="language-plaintext highlighter-rouge">SelectionRangeClientCapabilities</code> defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#selectionRangeClientCapabilities" name="selectionRangeClientCapabilities" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">SelectionRangeClientCapabilities</span> <span class="p">{</span>
	<span class="cm">/**
	 * Whether implementation supports dynamic registration for selection range
	 * providers. If this is set to `true` the client supports the new
	 * `SelectionRangeRegistrationOptions` return value for the corresponding
	 * server capability as well.
	 */</span>
	<span class="nl">dynamicRegistration</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Server Capability</em>:</p>
<ul>
  <li>property name (optional): <code class="language-plaintext highlighter-rouge">selectionRangeProvider</code>
</li>
  <li>property type: <code class="language-plaintext highlighter-rouge">boolean | SelectionRangeOptions | SelectionRangeRegistrationOptions</code> where <code class="language-plaintext highlighter-rouge">SelectionRangeOptions</code> is defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#selectionRangeOptions" name="selectionRangeOptions" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">SelectionRangeOptions</span> <span class="kd">extends</span> <span class="nx">WorkDoneProgressOptions</span> <span class="p">{</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Registration Options</em>: <code class="language-plaintext highlighter-rouge">SelectionRangeRegistrationOptions</code> defined as follows:</p>

<div class="anchorHolder"><a href="#selectionRangeRegistrationOptions" name="selectionRangeRegistrationOptions" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">SelectionRangeRegistrationOptions</span> <span class="kd">extends</span>
	<span class="nx">SelectionRangeOptions</span><span class="p">,</span> <span class="nx">TextDocumentRegistrationOptions</span><span class="p">,</span>
	<span class="nx">StaticRegistrationOptions</span> <span class="p">{</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Request</em>:</p>

<ul>
  <li>method: <code class="language-plaintext highlighter-rouge">textDocument/selectionRange</code>
</li>
  <li>params: <code class="language-plaintext highlighter-rouge">SelectionRangeParams</code> defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#selectionRangeParams" name="selectionRangeParams" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">SelectionRangeParams</span> <span class="kd">extends</span> <span class="nx">WorkDoneProgressParams</span><span class="p">,</span>
	<span class="nx">PartialResultParams</span> <span class="p">{</span>
	<span class="cm">/**
	 * The text document.
	 */</span>
	<span class="nl">textDocument</span><span class="p">:</span> <span class="nx">TextDocumentIdentifier</span><span class="p">;</span>

	<span class="cm">/**
	 * The positions inside the text document.
	 */</span>
	<span class="nl">positions</span><span class="p">:</span> <span class="nx">Position</span><span class="p">[];</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Response</em>:</p>

<ul>
  <li>result: <code class="language-plaintext highlighter-rouge">SelectionRange[] | null</code> defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#selectionRange" name="selectionRange" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">SelectionRange</span> <span class="p">{</span>
	<span class="cm">/**
	 * The [range](#Range) of this selection range.
	 */</span>
	<span class="nl">range</span><span class="p">:</span> <span class="nx">Range</span><span class="p">;</span>
	<span class="cm">/**
	 * The parent selection range containing this range. Therefore
	 * `parent.range` must contain `this.range`.
	 */</span>
	<span class="nl">parent</span><span class="p">?:</span> <span class="nx">SelectionRange</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<ul>
  <li>partial result: <code class="language-plaintext highlighter-rouge">SelectionRange[]</code>
</li>
  <li>error: code and message set in case an exception happens during the ‘textDocument/selectionRange’ request</li>
</ul>

<h4 id="document-symbols-request-leftwards_arrow_with_hook"><a href="#textDocument_documentSymbol" name="textDocument_documentSymbol" class="anchor">Document Symbols Request (<img class="emoji" title=":leftwards_arrow_with_hook:" alt=":leftwards_arrow_with_hook:" src="https://github.githubassets.com/images/icons/emoji/unicode/21a9.png" height="20" width="20">)</a></h4>

<p>The document symbol request is sent from the client to the server. The returned result is either</p>

<ul>
  <li>
<code class="language-plaintext highlighter-rouge">SymbolInformation[]</code> which is a flat list of all symbols found in a given text document. Then neither the symbol’s location range nor the symbol’s container name should be used to infer a hierarchy.</li>
  <li>
<code class="language-plaintext highlighter-rouge">DocumentSymbol[]</code> which is a hierarchy of symbols found in a given text document.</li>
</ul>

<p>Servers should whenever possible return <code class="language-plaintext highlighter-rouge">DocumentSymbol</code> since it is the richer data structure.</p>

<p><em>Client Capability</em>:</p>
<ul>
  <li>property name (optional): <code class="language-plaintext highlighter-rouge">textDocument.documentSymbol</code>
</li>
  <li>property type: <code class="language-plaintext highlighter-rouge">DocumentSymbolClientCapabilities</code> defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#documentSymbolClientCapabilities" name="documentSymbolClientCapabilities" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">DocumentSymbolClientCapabilities</span> <span class="p">{</span>
	<span class="cm">/**
	 * Whether document symbol supports dynamic registration.
	 */</span>
	<span class="nl">dynamicRegistration</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>

	<span class="cm">/**
	 * Specific capabilities for the `SymbolKind` in the
	 * `textDocument/documentSymbol` request.
	 */</span>
	<span class="nl">symbolKind</span><span class="p">?:</span> <span class="p">{</span>
		<span class="cm">/**
		 * The symbol kind values the client supports. When this
		 * property exists the client also guarantees that it will
		 * handle values outside its set gracefully and falls back
		 * to a default value when unknown.
		 *
		 * If this property is not present the client only supports
		 * the symbol kinds from `File` to `Array` as defined in
		 * the initial version of the protocol.
		 */</span>
		<span class="nx">valueSet</span><span class="p">?:</span> <span class="nx">SymbolKind</span><span class="p">[];</span>
	<span class="p">};</span>

	<span class="cm">/**
	 * The client supports hierarchical document symbols.
	 */</span>
	<span class="nl">hierarchicalDocumentSymbolSupport</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>

	<span class="cm">/**
	 * The client supports tags on `SymbolInformation`. Tags are supported on
	 * `DocumentSymbol` if `hierarchicalDocumentSymbolSupport` is set to true.
	 * Clients supporting tags have to handle unknown tags gracefully.
	 *
	 * @since 3.16.0
	 */</span>
	<span class="nl">tagSupport</span><span class="p">?:</span> <span class="p">{</span>
		<span class="cm">/**
		 * The tags supported by the client.
		 */</span>
		<span class="na">valueSet</span><span class="p">:</span> <span class="nx">SymbolTag</span><span class="p">[];</span>
	<span class="p">};</span>

	<span class="cm">/**
	 * The client supports an additional label presented in the UI when
	 * registering a document symbol provider.
	 *
	 * @since 3.16.0
	 */</span>
	<span class="nl">labelSupport</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Server Capability</em>:</p>
<ul>
  <li>property name (optional): <code class="language-plaintext highlighter-rouge">documentSymbolProvider</code>
</li>
  <li>property type: <code class="language-plaintext highlighter-rouge">boolean | DocumentSymbolOptions</code> where <code class="language-plaintext highlighter-rouge">DocumentSymbolOptions</code> is defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#documentSymbolOptions" name="documentSymbolOptions" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">DocumentSymbolOptions</span> <span class="kd">extends</span> <span class="nx">WorkDoneProgressOptions</span> <span class="p">{</span>
	<span class="cm">/**
	 * A human-readable string that is shown when multiple outlines trees
	 * are shown for the same document.
	 *
	 * @since 3.16.0
	 */</span>
	<span class="nl">label</span><span class="p">?:</span> <span class="kr">string</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Registration Options</em>: <code class="language-plaintext highlighter-rouge">DocumentSymbolRegistrationOptions</code> defined as follows:</p>

<div class="anchorHolder"><a href="#documentSymbolRegistrationOptions" name="documentSymbolRegistrationOptions" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">DocumentSymbolRegistrationOptions</span> <span class="kd">extends</span>
	<span class="nx">TextDocumentRegistrationOptions</span><span class="p">,</span> <span class="nx">DocumentSymbolOptions</span> <span class="p">{</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Request</em>:</p>
<ul>
  <li>method: <code class="language-plaintext highlighter-rouge">textDocument/documentSymbol</code>
</li>
  <li>params: <code class="language-plaintext highlighter-rouge">DocumentSymbolParams</code> defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#documentSymbolParams" name="documentSymbolParams" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">DocumentSymbolParams</span> <span class="kd">extends</span> <span class="nx">WorkDoneProgressParams</span><span class="p">,</span>
	<span class="nx">PartialResultParams</span> <span class="p">{</span>
	<span class="cm">/**
	 * The text document.
	 */</span>
	<span class="nl">textDocument</span><span class="p">:</span> <span class="nx">TextDocumentIdentifier</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Response</em>:</p>
<ul>
  <li>result: <code class="language-plaintext highlighter-rouge">DocumentSymbol[]</code> | <code class="language-plaintext highlighter-rouge">SymbolInformation[]</code> | <code class="language-plaintext highlighter-rouge">null</code> defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#symbolKind" name="symbolKind" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * A symbol kind.
 */</span>
<span class="k">export</span> <span class="k">namespace</span> <span class="nx">SymbolKind</span> <span class="p">{</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">File</span> <span class="o">=</span> <span class="mi">1</span><span class="p">;</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">Module</span> <span class="o">=</span> <span class="mi">2</span><span class="p">;</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">Namespace</span> <span class="o">=</span> <span class="mi">3</span><span class="p">;</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">Package</span> <span class="o">=</span> <span class="mi">4</span><span class="p">;</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">Class</span> <span class="o">=</span> <span class="mi">5</span><span class="p">;</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">Method</span> <span class="o">=</span> <span class="mi">6</span><span class="p">;</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">Property</span> <span class="o">=</span> <span class="mi">7</span><span class="p">;</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">Field</span> <span class="o">=</span> <span class="mi">8</span><span class="p">;</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">Constructor</span> <span class="o">=</span> <span class="mi">9</span><span class="p">;</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">Enum</span> <span class="o">=</span> <span class="mi">10</span><span class="p">;</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">Interface</span> <span class="o">=</span> <span class="mi">11</span><span class="p">;</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nb">Function</span> <span class="o">=</span> <span class="mi">12</span><span class="p">;</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">Variable</span> <span class="o">=</span> <span class="mi">13</span><span class="p">;</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">Constant</span> <span class="o">=</span> <span class="mi">14</span><span class="p">;</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nb">String</span> <span class="o">=</span> <span class="mi">15</span><span class="p">;</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nb">Number</span> <span class="o">=</span> <span class="mi">16</span><span class="p">;</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nb">Boolean</span> <span class="o">=</span> <span class="mi">17</span><span class="p">;</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nb">Array</span> <span class="o">=</span> <span class="mi">18</span><span class="p">;</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nb">Object</span> <span class="o">=</span> <span class="mi">19</span><span class="p">;</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">Key</span> <span class="o">=</span> <span class="mi">20</span><span class="p">;</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">Null</span> <span class="o">=</span> <span class="mi">21</span><span class="p">;</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">EnumMember</span> <span class="o">=</span> <span class="mi">22</span><span class="p">;</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">Struct</span> <span class="o">=</span> <span class="mi">23</span><span class="p">;</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">Event</span> <span class="o">=</span> <span class="mi">24</span><span class="p">;</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">Operator</span> <span class="o">=</span> <span class="mi">25</span><span class="p">;</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">TypeParameter</span> <span class="o">=</span> <span class="mi">26</span><span class="p">;</span>
<span class="p">}</span>

<span class="k">export</span> <span class="kd">type</span> <span class="nx">SymbolKind</span> <span class="o">=</span> <span class="mi">1</span> <span class="o">|</span> <span class="mi">2</span> <span class="o">|</span> <span class="mi">3</span> <span class="o">|</span> <span class="mi">4</span> <span class="o">|</span> <span class="mi">5</span> <span class="o">|</span> <span class="mi">6</span> <span class="o">|</span> <span class="mi">7</span> <span class="o">|</span> <span class="mi">8</span> <span class="o">|</span> <span class="mi">9</span> <span class="o">|</span> <span class="mi">10</span> <span class="o">|</span> <span class="mi">11</span> <span class="o">|</span> <span class="mi">12</span> <span class="o">|</span> <span class="mi">13</span> <span class="o">|</span> <span class="mi">14</span> <span class="o">|</span> <span class="mi">15</span> <span class="o">|</span> <span class="mi">16</span> <span class="o">|</span> <span class="mi">17</span> <span class="o">|</span> <span class="mi">18</span> <span class="o">|</span> <span class="mi">19</span> <span class="o">|</span> <span class="mi">20</span> <span class="o">|</span> <span class="mi">21</span> <span class="o">|</span> <span class="mi">22</span> <span class="o">|</span> <span class="mi">23</span> <span class="o">|</span> <span class="mi">24</span> <span class="o">|</span> <span class="mi">25</span> <span class="o">|</span> <span class="mi">26</span><span class="p">;</span>
</code></pre></div></div>

<div class="anchorHolder"><a href="#symbolTag" name="symbolTag" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * Symbol tags are extra annotations that tweak the rendering of a symbol.
 *
 * @since 3.16
 */</span>
<span class="k">export</span> <span class="k">namespace</span> <span class="nx">SymbolTag</span> <span class="p">{</span>

	<span class="cm">/**
	 * Render a symbol as obsolete, usually using a strike-out.
	 */</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">Deprecated</span><span class="p">:</span> <span class="mi">1</span> <span class="o">=</span> <span class="mi">1</span><span class="p">;</span>
<span class="p">}</span>

<span class="k">export</span> <span class="kd">type</span> <span class="nx">SymbolTag</span> <span class="o">=</span> <span class="mi">1</span><span class="p">;</span>
</code></pre></div></div>

<div class="anchorHolder"><a href="#documentSymbol" name="documentSymbol" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * Represents programming constructs like variables, classes, interfaces etc.
 * that appear in a document. Document symbols can be hierarchical and they
 * have two ranges: one that encloses its definition and one that points to its
 * most interesting range, e.g. the range of an identifier.
 */</span>
<span class="k">export</span> <span class="kr">interface</span> <span class="nx">DocumentSymbol</span> <span class="p">{</span>

	<span class="cm">/**
	 * The name of this symbol. Will be displayed in the user interface and
	 * therefore must not be an empty string or a string only consisting of
	 * white spaces.
	 */</span>
	<span class="nl">name</span><span class="p">:</span> <span class="kr">string</span><span class="p">;</span>

	<span class="cm">/**
	 * More detail for this symbol, e.g the signature of a function.
	 */</span>
	<span class="nl">detail</span><span class="p">?:</span> <span class="kr">string</span><span class="p">;</span>

	<span class="cm">/**
	 * The kind of this symbol.
	 */</span>
	<span class="nl">kind</span><span class="p">:</span> <span class="nx">SymbolKind</span><span class="p">;</span>

	<span class="cm">/**
	 * Tags for this document symbol.
	 *
	 * @since 3.16.0
	 */</span>
	<span class="nl">tags</span><span class="p">?:</span> <span class="nx">SymbolTag</span><span class="p">[];</span>

	<span class="cm">/**
	 * Indicates if this symbol is deprecated.
	 *
	 * @deprecated Use tags instead
	 */</span>
	<span class="nl">deprecated</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>

	<span class="cm">/**
	 * The range enclosing this symbol not including leading/trailing whitespace
	 * but everything else like comments. This information is typically used to
	 * determine if the clients cursor is inside the symbol to reveal in the
	 * symbol in the UI.
	 */</span>
	<span class="nl">range</span><span class="p">:</span> <span class="nx">Range</span><span class="p">;</span>

	<span class="cm">/**
	 * The range that should be selected and revealed when this symbol is being
	 * picked, e.g. the name of a function. Must be contained by the `range`.
	 */</span>
	<span class="nl">selectionRange</span><span class="p">:</span> <span class="nx">Range</span><span class="p">;</span>

	<span class="cm">/**
	 * Children of this symbol, e.g. properties of a class.
	 */</span>
	<span class="nl">children</span><span class="p">?:</span> <span class="nx">DocumentSymbol</span><span class="p">[];</span>
<span class="p">}</span>
</code></pre></div></div>

<div class="anchorHolder"><a href="#symbolInformation" name="symbolInformation" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * Represents information about programming constructs like variables, classes,
 * interfaces etc.
 *
 * @deprecated use DocumentSymbol or WorkspaceSymbol instead.
 */</span>
<span class="k">export</span> <span class="kr">interface</span> <span class="nx">SymbolInformation</span> <span class="p">{</span>
	<span class="cm">/**
	 * The name of this symbol.
	 */</span>
	<span class="nl">name</span><span class="p">:</span> <span class="kr">string</span><span class="p">;</span>

	<span class="cm">/**
	 * The kind of this symbol.
	 */</span>
	<span class="nl">kind</span><span class="p">:</span> <span class="nx">SymbolKind</span><span class="p">;</span>

	<span class="cm">/**
	 * Tags for this symbol.
	 *
	 * @since 3.16.0
	 */</span>
	<span class="nl">tags</span><span class="p">?:</span> <span class="nx">SymbolTag</span><span class="p">[];</span>

	<span class="cm">/**
	 * Indicates if this symbol is deprecated.
	 *
	 * @deprecated Use tags instead
	 */</span>
	<span class="nl">deprecated</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>

	<span class="cm">/**
	 * The location of this symbol. The location's range is used by a tool
	 * to reveal the location in the editor. If the symbol is selected in the
	 * tool the range's start information is used to position the cursor. So
	 * the range usually spans more then the actual symbol's name and does
	 * normally include things like visibility modifiers.
	 *
	 * The range doesn't have to denote a node range in the sense of an abstract
	 * syntax tree. It can therefore not be used to re-construct a hierarchy of
	 * the symbols.
	 */</span>
	<span class="nl">location</span><span class="p">:</span> <span class="nx">Location</span><span class="p">;</span>

	<span class="cm">/**
	 * The name of the symbol containing this symbol. This information is for
	 * user interface purposes (e.g. to render a qualifier in the user interface
	 * if necessary). It can't be used to re-infer a hierarchy for the document
	 * symbols.
	 */</span>
	<span class="nl">containerName</span><span class="p">?:</span> <span class="kr">string</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<ul>
  <li>partial result: <code class="language-plaintext highlighter-rouge">DocumentSymbol[]</code> | <code class="language-plaintext highlighter-rouge">SymbolInformation[]</code>. <code class="language-plaintext highlighter-rouge">DocumentSymbol[]</code> and <code class="language-plaintext highlighter-rouge">SymbolInformation[]</code> can not be mixed. That means the first chunk defines the type of all the other chunks.</li>
  <li>error: code and message set in case an exception happens during the document symbol request.</li>
</ul>

<h4 id="semantic-tokens-leftwards_arrow_with_hook"><a href="#textDocument_semanticTokens" name="textDocument_semanticTokens" class="anchor">Semantic Tokens (<img class="emoji" title=":leftwards_arrow_with_hook:" alt=":leftwards_arrow_with_hook:" src="https://github.githubassets.com/images/icons/emoji/unicode/21a9.png" height="20" width="20">)</a></h4>

<blockquote>
  <p><em>Since version 3.16.0</em></p>
</blockquote>

<p>The request is sent from the client to the server to resolve semantic tokens for a given file. Semantic tokens are used to add additional color information to a file that depends on language specific symbol information. A semantic token request usually produces a large result. The protocol therefore supports encoding tokens with numbers. In addition optional support for deltas is available.</p>

<p><em>General Concepts</em></p>

<p>Tokens are represented using one token type combined with n token modifiers. A token type is something like <code class="language-plaintext highlighter-rouge">class</code> or <code class="language-plaintext highlighter-rouge">function</code> and token modifiers are like <code class="language-plaintext highlighter-rouge">static</code> or <code class="language-plaintext highlighter-rouge">async</code>. The protocol defines a set of token types and modifiers but clients are allowed to extend these and announce the values they support in the corresponding client capability. The predefined values are:</p>

<div class="anchorHolder"><a href="#semanticTokenTypes" name="semanticTokenTypes" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">enum</span> <span class="nx">SemanticTokenTypes</span> <span class="p">{</span>
	<span class="k">namespace</span> <span class="o">=</span> <span class="dl">'</span><span class="s1">namespace</span><span class="dl">'</span><span class="p">,</span>
	<span class="cm">/**
	 * Represents a generic type. Acts as a fallback for types which
	 * can't be mapped to a specific type like class or enum.
	 */</span>
	<span class="kd">type</span> <span class="o">=</span> <span class="dl">'</span><span class="s1">type</span><span class="dl">'</span><span class="p">,</span>
	<span class="kd">class</span> <span class="o">=</span> <span class="dl">'</span><span class="s1">class</span><span class="dl">'</span><span class="p">,</span>
	<span class="kr">enum</span> <span class="o">=</span> <span class="dl">'</span><span class="s1">enum</span><span class="dl">'</span><span class="p">,</span>
	<span class="kr">interface</span> <span class="o">=</span> <span class="dl">'</span><span class="s1">interface</span><span class="dl">'</span><span class="p">,</span>
	<span class="nx">struct</span> <span class="o">=</span> <span class="dl">'</span><span class="s1">struct</span><span class="dl">'</span><span class="p">,</span>
	<span class="nx">typeParameter</span> <span class="o">=</span> <span class="dl">'</span><span class="s1">typeParameter</span><span class="dl">'</span><span class="p">,</span>
	<span class="nx">parameter</span> <span class="o">=</span> <span class="dl">'</span><span class="s1">parameter</span><span class="dl">'</span><span class="p">,</span>
	<span class="nx">variable</span> <span class="o">=</span> <span class="dl">'</span><span class="s1">variable</span><span class="dl">'</span><span class="p">,</span>
	<span class="nx">property</span> <span class="o">=</span> <span class="dl">'</span><span class="s1">property</span><span class="dl">'</span><span class="p">,</span>
	<span class="nx">enumMember</span> <span class="o">=</span> <span class="dl">'</span><span class="s1">enumMember</span><span class="dl">'</span><span class="p">,</span>
	<span class="nx">event</span> <span class="o">=</span> <span class="dl">'</span><span class="s1">event</span><span class="dl">'</span><span class="p">,</span>
	<span class="kd">function</span> <span class="o">=</span> <span class="dl">'</span><span class="s1">function</span><span class="dl">'</span><span class="p">,</span>
	<span class="nx">method</span> <span class="o">=</span> <span class="dl">'</span><span class="s1">method</span><span class="dl">'</span><span class="p">,</span>
	<span class="nx">macro</span> <span class="o">=</span> <span class="dl">'</span><span class="s1">macro</span><span class="dl">'</span><span class="p">,</span>
	<span class="nx">keyword</span> <span class="o">=</span> <span class="dl">'</span><span class="s1">keyword</span><span class="dl">'</span><span class="p">,</span>
	<span class="nx">modifier</span> <span class="o">=</span> <span class="dl">'</span><span class="s1">modifier</span><span class="dl">'</span><span class="p">,</span>
	<span class="nx">comment</span> <span class="o">=</span> <span class="dl">'</span><span class="s1">comment</span><span class="dl">'</span><span class="p">,</span>
	<span class="kr">string</span> <span class="o">=</span> <span class="dl">'</span><span class="s1">string</span><span class="dl">'</span><span class="p">,</span>
	<span class="kr">number</span> <span class="o">=</span> <span class="dl">'</span><span class="s1">number</span><span class="dl">'</span><span class="p">,</span>
	<span class="nx">regexp</span> <span class="o">=</span> <span class="dl">'</span><span class="s1">regexp</span><span class="dl">'</span><span class="p">,</span>
	<span class="nx">operator</span> <span class="o">=</span> <span class="dl">'</span><span class="s1">operator</span><span class="dl">'</span><span class="p">,</span>
	<span class="cm">/**
	 * @since 3.17.0
	 */</span>
	<span class="nx">decorator</span> <span class="o">=</span> <span class="dl">'</span><span class="s1">decorator</span><span class="dl">'</span>
<span class="p">}</span>
</code></pre></div></div>

<div class="anchorHolder"><a href="#semanticTokenModifiers" name="semanticTokenModifiers" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">enum</span> <span class="nx">SemanticTokenModifiers</span> <span class="p">{</span>
	<span class="nx">declaration</span> <span class="o">=</span> <span class="dl">'</span><span class="s1">declaration</span><span class="dl">'</span><span class="p">,</span>
	<span class="nx">definition</span> <span class="o">=</span> <span class="dl">'</span><span class="s1">definition</span><span class="dl">'</span><span class="p">,</span>
	<span class="k">readonly</span> <span class="o">=</span> <span class="dl">'</span><span class="s1">readonly</span><span class="dl">'</span><span class="p">,</span>
	<span class="k">static</span> <span class="o">=</span> <span class="dl">'</span><span class="s1">static</span><span class="dl">'</span><span class="p">,</span>
	<span class="nx">deprecated</span> <span class="o">=</span> <span class="dl">'</span><span class="s1">deprecated</span><span class="dl">'</span><span class="p">,</span>
	<span class="kd">abstract</span> <span class="o">=</span> <span class="dl">'</span><span class="s1">abstract</span><span class="dl">'</span><span class="p">,</span>
	<span class="k">async</span> <span class="o">=</span> <span class="dl">'</span><span class="s1">async</span><span class="dl">'</span><span class="p">,</span>
	<span class="nx">modification</span> <span class="o">=</span> <span class="dl">'</span><span class="s1">modification</span><span class="dl">'</span><span class="p">,</span>
	<span class="nx">documentation</span> <span class="o">=</span> <span class="dl">'</span><span class="s1">documentation</span><span class="dl">'</span><span class="p">,</span>
	<span class="nx">defaultLibrary</span> <span class="o">=</span> <span class="dl">'</span><span class="s1">defaultLibrary</span><span class="dl">'</span>
<span class="p">}</span>
</code></pre></div></div>

<p>The protocol defines an additional token format capability to allow future extensions of the format. The only format that is currently specified is <code class="language-plaintext highlighter-rouge">relative</code> expressing that the tokens are described using relative positions (see Integer Encoding for Tokens below).</p>

<div class="anchorHolder"><a href="#tokenFormat" name="tokenFormat" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="k">namespace</span> <span class="nx">TokenFormat</span> <span class="p">{</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">Relative</span><span class="p">:</span> <span class="dl">'</span><span class="s1">relative</span><span class="dl">'</span> <span class="o">=</span> <span class="dl">'</span><span class="s1">relative</span><span class="dl">'</span><span class="p">;</span>
<span class="p">}</span>

<span class="k">export</span> <span class="kd">type</span> <span class="nx">TokenFormat</span> <span class="o">=</span> <span class="dl">'</span><span class="s1">relative</span><span class="dl">'</span><span class="p">;</span>
</code></pre></div></div>

<p><em>Integer Encoding for Tokens</em></p>

<p>On the capability level types and modifiers are defined using strings. However the real encoding happens using numbers. The server therefore needs to let the client know which numbers it is using for which types and modifiers. They do so using a legend, which is defined as follows:</p>

<div class="anchorHolder"><a href="#semanticTokensLegend" name="semanticTokensLegend" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">SemanticTokensLegend</span> <span class="p">{</span>
	<span class="cm">/**
	 * The token types a server uses.
	 */</span>
	<span class="nl">tokenTypes</span><span class="p">:</span> <span class="kr">string</span><span class="p">[];</span>

	<span class="cm">/**
	 * The token modifiers a server uses.
	 */</span>
	<span class="nl">tokenModifiers</span><span class="p">:</span> <span class="kr">string</span><span class="p">[];</span>
<span class="p">}</span>
</code></pre></div></div>

<p>Token types are looked up by index, so a <code class="language-plaintext highlighter-rouge">tokenType</code> value of <code class="language-plaintext highlighter-rouge">1</code> means <code class="language-plaintext highlighter-rouge">tokenTypes[1]</code>. Since a token type can have n modifiers, multiple token modifiers can be set by using bit flags,
so a <code class="language-plaintext highlighter-rouge">tokenModifier</code> value of <code class="language-plaintext highlighter-rouge">3</code> is first viewed as binary <code class="language-plaintext highlighter-rouge">0b00000011</code>, which means <code class="language-plaintext highlighter-rouge">[tokenModifiers[0], tokenModifiers[1]]</code> because bits 0 and 1 are set.</p>

<p>There are different ways how the position of a token can be expressed in a file. Absolute positions or relative positions. The protocol for the token format <code class="language-plaintext highlighter-rouge">relative</code> uses relative positions, because most tokens remain stable relative to each other when edits are made in a file. This simplifies the computation of a delta if a server supports it. So each token is represented using 5 integers. A specific token <code class="language-plaintext highlighter-rouge">i</code> in the file consists of the following array indices:</p>

<ul>
  <li>at index <code class="language-plaintext highlighter-rouge">5*i</code>   - <code class="language-plaintext highlighter-rouge">deltaLine</code>: token line number, relative to the previous token</li>
  <li>at index <code class="language-plaintext highlighter-rouge">5*i+1</code> - <code class="language-plaintext highlighter-rouge">deltaStart</code>: token start character, relative to the previous token (relative to 0 or the previous token’s start if they are on the same line)</li>
  <li>at index <code class="language-plaintext highlighter-rouge">5*i+2</code> - <code class="language-plaintext highlighter-rouge">length</code>: the length of the token.</li>
  <li>at index <code class="language-plaintext highlighter-rouge">5*i+3</code> - <code class="language-plaintext highlighter-rouge">tokenType</code>: will be looked up in <code class="language-plaintext highlighter-rouge">SemanticTokensLegend.tokenTypes</code>. We currently ask that <code class="language-plaintext highlighter-rouge">tokenType</code> &lt; 65536.</li>
  <li>at index <code class="language-plaintext highlighter-rouge">5*i+4</code> - <code class="language-plaintext highlighter-rouge">tokenModifiers</code>: each set bit will be looked up in <code class="language-plaintext highlighter-rouge">SemanticTokensLegend.tokenModifiers</code>
</li>
</ul>

<p>The <code class="language-plaintext highlighter-rouge">deltaStart</code> and the <code class="language-plaintext highlighter-rouge">length</code> values must be encoded using the encoding the client and server agrees on during the <code class="language-plaintext highlighter-rouge">initialize</code> request (see also <a href="#textDocuments">TextDocuments</a>).
Whether a token can span multiple lines is defined by the client capability <code class="language-plaintext highlighter-rouge">multilineTokenSupport</code>. If multiline tokens are not supported and a tokens length takes it past the end of the line, it should be treated as if the token ends at the end of the line and will not wrap onto the next line.</p>

<p>The client capability <code class="language-plaintext highlighter-rouge">overlappingTokenSupport</code> defines whether tokens can overlap each other.</p>

<p>Lets look at a concrete example which uses single line tokens without overlaps for encoding a file with 3 tokens in a number array. We start with absolute positions to demonstrate how they can easily be transformed into relative positions:</p>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="p">{</span> <span class="nl">line</span><span class="p">:</span> <span class="mi">2</span><span class="p">,</span> <span class="nx">startChar</span><span class="p">:</span>  <span class="mi">5</span><span class="p">,</span> <span class="nx">length</span><span class="p">:</span> <span class="mi">3</span><span class="p">,</span> <span class="nx">tokenType</span><span class="p">:</span> <span class="dl">"</span><span class="s2">property</span><span class="dl">"</span><span class="p">,</span>
	<span class="nx">tokenModifiers</span><span class="p">:</span> <span class="p">[</span><span class="dl">"</span><span class="s2">private</span><span class="dl">"</span><span class="p">,</span> <span class="dl">"</span><span class="s2">static</span><span class="dl">"</span><span class="p">]</span>
<span class="p">},</span>
<span class="p">{</span> <span class="na">line</span><span class="p">:</span> <span class="mi">2</span><span class="p">,</span> <span class="na">startChar</span><span class="p">:</span> <span class="mi">10</span><span class="p">,</span> <span class="na">length</span><span class="p">:</span> <span class="mi">4</span><span class="p">,</span> <span class="na">tokenType</span><span class="p">:</span> <span class="dl">"</span><span class="s2">type</span><span class="dl">"</span><span class="p">,</span> <span class="na">tokenModifiers</span><span class="p">:</span> <span class="p">[]</span> <span class="p">},</span>
<span class="p">{</span> <span class="na">line</span><span class="p">:</span> <span class="mi">5</span><span class="p">,</span> <span class="na">startChar</span><span class="p">:</span>  <span class="mi">2</span><span class="p">,</span> <span class="na">length</span><span class="p">:</span> <span class="mi">7</span><span class="p">,</span> <span class="na">tokenType</span><span class="p">:</span> <span class="dl">"</span><span class="s2">class</span><span class="dl">"</span><span class="p">,</span> <span class="na">tokenModifiers</span><span class="p">:</span> <span class="p">[]</span> <span class="p">}</span>
</code></pre></div></div>

<p>First of all, a legend must be devised. This legend must be provided up-front on registration and capture all possible token types and modifiers. For the example we use this legend:</p>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="p">{</span>
   <span class="nl">tokenTypes</span><span class="p">:</span> <span class="p">[</span><span class="dl">'</span><span class="s1">property</span><span class="dl">'</span><span class="p">,</span> <span class="dl">'</span><span class="s1">type</span><span class="dl">'</span><span class="p">,</span> <span class="dl">'</span><span class="s1">class</span><span class="dl">'</span><span class="p">],</span>
   <span class="nx">tokenModifiers</span><span class="p">:</span> <span class="p">[</span><span class="dl">'</span><span class="s1">private</span><span class="dl">'</span><span class="p">,</span> <span class="dl">'</span><span class="s1">static</span><span class="dl">'</span><span class="p">]</span>
<span class="p">}</span>
</code></pre></div></div>

<p>The first transformation step is to encode <code class="language-plaintext highlighter-rouge">tokenType</code> and <code class="language-plaintext highlighter-rouge">tokenModifiers</code> as integers using the legend. As said, token types are looked up by index, so a <code class="language-plaintext highlighter-rouge">tokenType</code> value of <code class="language-plaintext highlighter-rouge">1</code> means <code class="language-plaintext highlighter-rouge">tokenTypes[1]</code>. Multiple token modifiers can be set by using bit flags, so a <code class="language-plaintext highlighter-rouge">tokenModifier</code> value of <code class="language-plaintext highlighter-rouge">3</code> is first viewed as binary <code class="language-plaintext highlighter-rouge">0b00000011</code>, which means <code class="language-plaintext highlighter-rouge">[tokenModifiers[0], tokenModifiers[1]]</code> because bits 0 and 1 are set. Using this legend, the tokens now are:</p>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="p">{</span> <span class="nl">line</span><span class="p">:</span> <span class="mi">2</span><span class="p">,</span> <span class="nx">startChar</span><span class="p">:</span>  <span class="mi">5</span><span class="p">,</span> <span class="nx">length</span><span class="p">:</span> <span class="mi">3</span><span class="p">,</span> <span class="nx">tokenType</span><span class="p">:</span> <span class="mi">0</span><span class="p">,</span> <span class="nx">tokenModifiers</span><span class="p">:</span> <span class="mi">3</span> <span class="p">},</span>
<span class="p">{</span> <span class="na">line</span><span class="p">:</span> <span class="mi">2</span><span class="p">,</span> <span class="na">startChar</span><span class="p">:</span> <span class="mi">10</span><span class="p">,</span> <span class="na">length</span><span class="p">:</span> <span class="mi">4</span><span class="p">,</span> <span class="na">tokenType</span><span class="p">:</span> <span class="mi">1</span><span class="p">,</span> <span class="na">tokenModifiers</span><span class="p">:</span> <span class="mi">0</span> <span class="p">},</span>
<span class="p">{</span> <span class="na">line</span><span class="p">:</span> <span class="mi">5</span><span class="p">,</span> <span class="na">startChar</span><span class="p">:</span>  <span class="mi">2</span><span class="p">,</span> <span class="na">length</span><span class="p">:</span> <span class="mi">7</span><span class="p">,</span> <span class="na">tokenType</span><span class="p">:</span> <span class="mi">2</span><span class="p">,</span> <span class="na">tokenModifiers</span><span class="p">:</span> <span class="mi">0</span> <span class="p">}</span>
</code></pre></div></div>

<p>The next step is to represent each token relative to the previous token in the file. In this case, the second token is on the same line as the first token, so the <code class="language-plaintext highlighter-rouge">startChar</code> of the second token is made relative to the <code class="language-plaintext highlighter-rouge">startChar</code> of the first token, so it will be <code class="language-plaintext highlighter-rouge">10 - 5</code>. The third token is on a different line than the second token, so the <code class="language-plaintext highlighter-rouge">startChar</code> of the third token will not be altered:</p>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="p">{</span> <span class="nl">deltaLine</span><span class="p">:</span> <span class="mi">2</span><span class="p">,</span> <span class="nx">deltaStartChar</span><span class="p">:</span> <span class="mi">5</span><span class="p">,</span> <span class="nx">length</span><span class="p">:</span> <span class="mi">3</span><span class="p">,</span> <span class="nx">tokenType</span><span class="p">:</span> <span class="mi">0</span><span class="p">,</span> <span class="nx">tokenModifiers</span><span class="p">:</span> <span class="mi">3</span> <span class="p">},</span>
<span class="p">{</span> <span class="na">deltaLine</span><span class="p">:</span> <span class="mi">0</span><span class="p">,</span> <span class="na">deltaStartChar</span><span class="p">:</span> <span class="mi">5</span><span class="p">,</span> <span class="na">length</span><span class="p">:</span> <span class="mi">4</span><span class="p">,</span> <span class="na">tokenType</span><span class="p">:</span> <span class="mi">1</span><span class="p">,</span> <span class="na">tokenModifiers</span><span class="p">:</span> <span class="mi">0</span> <span class="p">},</span>
<span class="p">{</span> <span class="na">deltaLine</span><span class="p">:</span> <span class="mi">3</span><span class="p">,</span> <span class="na">deltaStartChar</span><span class="p">:</span> <span class="mi">2</span><span class="p">,</span> <span class="na">length</span><span class="p">:</span> <span class="mi">7</span><span class="p">,</span> <span class="na">tokenType</span><span class="p">:</span> <span class="mi">2</span><span class="p">,</span> <span class="na">tokenModifiers</span><span class="p">:</span> <span class="mi">0</span> <span class="p">}</span>
</code></pre></div></div>

<p>Finally, the last step is to inline each of the 5 fields for a token in a single array, which is a memory friendly representation:</p>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="c1">// 1st token,  2nd token,  3rd token</span>
<span class="p">[</span>  <span class="mi">2</span><span class="p">,</span><span class="mi">5</span><span class="p">,</span><span class="mi">3</span><span class="p">,</span><span class="mi">0</span><span class="p">,</span><span class="mi">3</span><span class="p">,</span>  <span class="mi">0</span><span class="p">,</span><span class="mi">5</span><span class="p">,</span><span class="mi">4</span><span class="p">,</span><span class="mi">1</span><span class="p">,</span><span class="mi">0</span><span class="p">,</span>  <span class="mi">3</span><span class="p">,</span><span class="mi">2</span><span class="p">,</span><span class="mi">7</span><span class="p">,</span><span class="mi">2</span><span class="p">,</span><span class="mi">0</span> <span class="p">]</span>
</code></pre></div></div>

<p>Now assume that the user types a new empty line at the beginning of the file which results in the following tokens in the file:</p>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="p">{</span> <span class="nl">line</span><span class="p">:</span> <span class="mi">3</span><span class="p">,</span> <span class="nx">startChar</span><span class="p">:</span>  <span class="mi">5</span><span class="p">,</span> <span class="nx">length</span><span class="p">:</span> <span class="mi">3</span><span class="p">,</span> <span class="nx">tokenType</span><span class="p">:</span> <span class="dl">"</span><span class="s2">property</span><span class="dl">"</span><span class="p">,</span>
	<span class="nx">tokenModifiers</span><span class="p">:</span> <span class="p">[</span><span class="dl">"</span><span class="s2">private</span><span class="dl">"</span><span class="p">,</span> <span class="dl">"</span><span class="s2">static</span><span class="dl">"</span><span class="p">]</span>
<span class="p">},</span>
<span class="p">{</span> <span class="na">line</span><span class="p">:</span> <span class="mi">3</span><span class="p">,</span> <span class="na">startChar</span><span class="p">:</span> <span class="mi">10</span><span class="p">,</span> <span class="na">length</span><span class="p">:</span> <span class="mi">4</span><span class="p">,</span> <span class="na">tokenType</span><span class="p">:</span> <span class="dl">"</span><span class="s2">type</span><span class="dl">"</span><span class="p">,</span> <span class="na">tokenModifiers</span><span class="p">:</span> <span class="p">[]</span> <span class="p">},</span>
<span class="p">{</span> <span class="na">line</span><span class="p">:</span> <span class="mi">6</span><span class="p">,</span> <span class="na">startChar</span><span class="p">:</span>  <span class="mi">2</span><span class="p">,</span> <span class="na">length</span><span class="p">:</span> <span class="mi">7</span><span class="p">,</span> <span class="na">tokenType</span><span class="p">:</span> <span class="dl">"</span><span class="s2">class</span><span class="dl">"</span><span class="p">,</span> <span class="na">tokenModifiers</span><span class="p">:</span> <span class="p">[]</span> <span class="p">}</span>
</code></pre></div></div>

<p>Running the same transformations as above will result in the following number array:</p>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="c1">// 1st token,  2nd token,  3rd token</span>
<span class="p">[</span>  <span class="mi">3</span><span class="p">,</span><span class="mi">5</span><span class="p">,</span><span class="mi">3</span><span class="p">,</span><span class="mi">0</span><span class="p">,</span><span class="mi">3</span><span class="p">,</span>  <span class="mi">0</span><span class="p">,</span><span class="mi">5</span><span class="p">,</span><span class="mi">4</span><span class="p">,</span><span class="mi">1</span><span class="p">,</span><span class="mi">0</span><span class="p">,</span>  <span class="mi">3</span><span class="p">,</span><span class="mi">2</span><span class="p">,</span><span class="mi">7</span><span class="p">,</span><span class="mi">2</span><span class="p">,</span><span class="mi">0</span><span class="p">]</span>
</code></pre></div></div>

<p>The delta is now expressed on these number arrays without any form of interpretation what these numbers mean. This is comparable to the text document edits send from the server to the client to modify the content of a file. Those are character based and don’t make any assumption about the meaning of the characters. So <code class="language-plaintext highlighter-rouge">[  2,5,3,0,3,  0,5,4,1,0,  3,2,7,2,0 ]</code> can be transformed into <code class="language-plaintext highlighter-rouge">[  3,5,3,0,3,  0,5,4,1,0,  3,2,7,2,0]</code> using the following edit description: <code class="language-plaintext highlighter-rouge">{ start:  0, deleteCount: 1, data: [3] }</code> which tells the client to simply replace the first number (e.g. <code class="language-plaintext highlighter-rouge">2</code>) in the array with <code class="language-plaintext highlighter-rouge">3</code>.</p>

<p>Semantic token edits behave conceptually like <a href="#textEditArray">text edits</a> on documents: if an edit description consists of n edits all n edits are based on the same state Sm of the number array. They will move the number array from state Sm to Sm+1. A client applying the edits must not assume that they are sorted. An easy algorithm to apply them to the number array is to sort the edits and apply them from the back to the front of the number array.</p>

<p><em>Client Capability</em>:</p>

<p>The following client capabilities are defined for semantic token requests sent from the client to the server:</p>

<ul>
  <li>property name (optional): <code class="language-plaintext highlighter-rouge">textDocument.semanticTokens</code>
</li>
  <li>property type: <code class="language-plaintext highlighter-rouge">SemanticTokensClientCapabilities</code> defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#semanticTokensClientCapabilities" name="semanticTokensClientCapabilities" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="kr">interface</span> <span class="nx">SemanticTokensClientCapabilities</span> <span class="p">{</span>
	<span class="cm">/**
	 * Whether implementation supports dynamic registration. If this is set to
	 * `true` the client supports the new `(TextDocumentRegistrationOptions &amp;
	 * StaticRegistrationOptions)` return value for the corresponding server
	 * capability as well.
	 */</span>
	<span class="nl">dynamicRegistration</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>

	<span class="cm">/**
	 * Which requests the client supports and might send to the server
	 * depending on the server's capability. Please note that clients might not
	 * show semantic tokens or degrade some of the user experience if a range
	 * or full request is advertised by the client but not provided by the
	 * server. If for example the client capability `requests.full` and
	 * `request.range` are both set to true but the server only provides a
	 * range provider the client might not render a minimap correctly or might
	 * even decide to not show any semantic tokens at all.
	 */</span>
	<span class="nl">requests</span><span class="p">:</span> <span class="p">{</span>
		<span class="cm">/**
		 * The client will send the `textDocument/semanticTokens/range` request
		 * if the server provides a corresponding handler.
		 */</span>
		<span class="nx">range</span><span class="p">?:</span> <span class="nx">boolean</span> <span class="o">|</span> <span class="p">{</span>
		<span class="p">};</span>

		<span class="cm">/**
		 * The client will send the `textDocument/semanticTokens/full` request
		 * if the server provides a corresponding handler.
		 */</span>
		<span class="nl">full</span><span class="p">?:</span> <span class="nx">boolean</span> <span class="o">|</span> <span class="p">{</span>
			<span class="cm">/**
			 * The client will send the `textDocument/semanticTokens/full/delta`
			 * request if the server provides a corresponding handler.
			 */</span>
			<span class="nx">delta</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>
		<span class="p">};</span>
	<span class="p">};</span>

	<span class="cm">/**
	 * The token types that the client supports.
	 */</span>
	<span class="nl">tokenTypes</span><span class="p">:</span> <span class="kr">string</span><span class="p">[];</span>

	<span class="cm">/**
	 * The token modifiers that the client supports.
	 */</span>
	<span class="nl">tokenModifiers</span><span class="p">:</span> <span class="kr">string</span><span class="p">[];</span>

	<span class="cm">/**
	 * The formats the clients supports.
	 */</span>
	<span class="nl">formats</span><span class="p">:</span> <span class="nx">TokenFormat</span><span class="p">[];</span>

	<span class="cm">/**
	 * Whether the client supports tokens that can overlap each other.
	 */</span>
	<span class="nl">overlappingTokenSupport</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>

	<span class="cm">/**
	 * Whether the client supports tokens that can span multiple lines.
	 */</span>
	<span class="nl">multilineTokenSupport</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>

	<span class="cm">/**
	 * Whether the client allows the server to actively cancel a
	 * semantic token request, e.g. supports returning
	 * ErrorCodes.ServerCancelled. If a server does the client
	 * needs to retrigger the request.
	 *
	 * @since 3.17.0
	 */</span>
	<span class="nl">serverCancelSupport</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>

	<span class="cm">/**
	 * Whether the client uses semantic tokens to augment existing
	 * syntax tokens. If set to `true` client side created syntax
	 * tokens and semantic tokens are both used for colorization. If
	 * set to `false` the client only uses the returned semantic tokens
	 * for colorization.
	 *
	 * If the value is `undefined` then the client behavior is not
	 * specified.
	 *
	 * @since 3.17.0
	 */</span>
	<span class="nl">augmentsSyntaxTokens</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Server Capability</em>:</p>

<p>The following server capabilities are defined for semantic tokens:</p>

<ul>
  <li>property name (optional): <code class="language-plaintext highlighter-rouge">semanticTokensProvider</code>
</li>
  <li>property type: <code class="language-plaintext highlighter-rouge">SemanticTokensOptions | SemanticTokensRegistrationOptions</code> where <code class="language-plaintext highlighter-rouge">SemanticTokensOptions</code> is defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#semanticTokensOptions" name="semanticTokensOptions" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">SemanticTokensOptions</span> <span class="kd">extends</span> <span class="nx">WorkDoneProgressOptions</span> <span class="p">{</span>
	<span class="cm">/**
	 * The legend used by the server
	 */</span>
	<span class="nl">legend</span><span class="p">:</span> <span class="nx">SemanticTokensLegend</span><span class="p">;</span>

	<span class="cm">/**
	 * Server supports providing semantic tokens for a specific range
	 * of a document.
	 */</span>
	<span class="nl">range</span><span class="p">?:</span> <span class="nx">boolean</span> <span class="o">|</span> <span class="p">{</span>
	<span class="p">};</span>

	<span class="cm">/**
	 * Server supports providing semantic tokens for a full document.
	 */</span>
	<span class="nl">full</span><span class="p">?:</span> <span class="nx">boolean</span> <span class="o">|</span> <span class="p">{</span>
		<span class="cm">/**
		 * The server supports deltas for full documents.
		 */</span>
		<span class="nx">delta</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>
	<span class="p">};</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Registration Options</em>: <code class="language-plaintext highlighter-rouge">SemanticTokensRegistrationOptions</code> defined as follows:</p>

<div class="anchorHolder"><a href="#semanticTokensRegistrationOptions" name="semanticTokensRegistrationOptions" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">SemanticTokensRegistrationOptions</span> <span class="kd">extends</span>
	<span class="nx">TextDocumentRegistrationOptions</span><span class="p">,</span> <span class="nx">SemanticTokensOptions</span><span class="p">,</span>
	<span class="nx">StaticRegistrationOptions</span> <span class="p">{</span>
<span class="p">}</span>
</code></pre></div></div>

<p>Since the registration option handles range, full and delta requests the method used to register for semantic tokens requests is <code class="language-plaintext highlighter-rouge">textDocument/semanticTokens</code> and not one of the specific methods described below.</p>

<p><strong>Requesting semantic tokens for a whole file</strong></p>

<p><em>Request</em>:</p>

<div class="anchorHolder"><a href="#semanticTokens_fullRequest" name="semanticTokens_fullRequest" class="linkableAnchor"></a></div>

<ul>
  <li>method: <code class="language-plaintext highlighter-rouge">textDocument/semanticTokens/full</code>
</li>
  <li>params: <code class="language-plaintext highlighter-rouge">SemanticTokensParams</code> defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#semanticTokensParams" name="semanticTokensParams" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">SemanticTokensParams</span> <span class="kd">extends</span> <span class="nx">WorkDoneProgressParams</span><span class="p">,</span>
	<span class="nx">PartialResultParams</span> <span class="p">{</span>
	<span class="cm">/**
	 * The text document.
	 */</span>
	<span class="nl">textDocument</span><span class="p">:</span> <span class="nx">TextDocumentIdentifier</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Response</em>:</p>

<ul>
  <li>result: <code class="language-plaintext highlighter-rouge">SemanticTokens | null</code> where <code class="language-plaintext highlighter-rouge">SemanticTokens</code> is defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#semanticTokens" name="semanticTokens" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">SemanticTokens</span> <span class="p">{</span>
	<span class="cm">/**
	 * An optional result id. If provided and clients support delta updating
	 * the client will include the result id in the next semantic token request.
	 * A server can then instead of computing all semantic tokens again simply
	 * send a delta.
	 */</span>
	<span class="nl">resultId</span><span class="p">?:</span> <span class="kr">string</span><span class="p">;</span>

	<span class="cm">/**
	 * The actual tokens.
	 */</span>
	<span class="nl">data</span><span class="p">:</span> <span class="nx">uinteger</span><span class="p">[];</span>
<span class="p">}</span>
</code></pre></div></div>

<ul>
  <li>partial result: <code class="language-plaintext highlighter-rouge">SemanticTokensPartialResult</code> defines as follows:</li>
</ul>

<div class="anchorHolder"><a href="#semanticTokensPartialResult" name="semanticTokensPartialResult" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">SemanticTokensPartialResult</span> <span class="p">{</span>
	<span class="nl">data</span><span class="p">:</span> <span class="nx">uinteger</span><span class="p">[];</span>
<span class="p">}</span>
</code></pre></div></div>

<ul>
  <li>error: code and message set in case an exception happens during the ‘textDocument/semanticTokens/full’ request</li>
</ul>

<p><strong>Requesting semantic token delta for a whole file</strong></p>

<p><em>Request</em>:</p>

<div class="anchorHolder"><a href="#semanticTokens_deltaRequest" name="semanticTokens_deltaRequest" class="linkableAnchor"></a></div>

<ul>
  <li>method: <code class="language-plaintext highlighter-rouge">textDocument/semanticTokens/full/delta</code>
</li>
  <li>params: <code class="language-plaintext highlighter-rouge">SemanticTokensDeltaParams</code> defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#semanticTokensDeltaParams" name="semanticTokensDeltaParams" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">SemanticTokensDeltaParams</span> <span class="kd">extends</span> <span class="nx">WorkDoneProgressParams</span><span class="p">,</span>
	<span class="nx">PartialResultParams</span> <span class="p">{</span>
	<span class="cm">/**
	 * The text document.
	 */</span>
	<span class="nl">textDocument</span><span class="p">:</span> <span class="nx">TextDocumentIdentifier</span><span class="p">;</span>

	<span class="cm">/**
	 * The result id of a previous response. The result Id can either point to
	 * a full response or a delta response depending on what was received last.
	 */</span>
	<span class="nl">previousResultId</span><span class="p">:</span> <span class="kr">string</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Response</em>:</p>

<ul>
  <li>result: <code class="language-plaintext highlighter-rouge">SemanticTokens | SemanticTokensDelta | null</code> where <code class="language-plaintext highlighter-rouge">SemanticTokensDelta</code> is defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#semanticTokensDelta" name="semanticTokensDelta" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">SemanticTokensDelta</span> <span class="p">{</span>
	<span class="k">readonly</span> <span class="nx">resultId</span><span class="p">?:</span> <span class="kr">string</span><span class="p">;</span>
	<span class="cm">/**
	 * The semantic token edits to transform a previous result into a new
	 * result.
	 */</span>
	<span class="nl">edits</span><span class="p">:</span> <span class="nx">SemanticTokensEdit</span><span class="p">[];</span>
<span class="p">}</span>
</code></pre></div></div>

<div class="anchorHolder"><a href="#semanticTokensEdit" name="semanticTokensEdit" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">SemanticTokensEdit</span> <span class="p">{</span>
	<span class="cm">/**
	 * The start offset of the edit.
	 */</span>
	<span class="nl">start</span><span class="p">:</span> <span class="nx">uinteger</span><span class="p">;</span>

	<span class="cm">/**
	 * The count of elements to remove.
	 */</span>
	<span class="nl">deleteCount</span><span class="p">:</span> <span class="nx">uinteger</span><span class="p">;</span>

	<span class="cm">/**
	 * The elements to insert.
	 */</span>
	<span class="nl">data</span><span class="p">?:</span> <span class="nx">uinteger</span><span class="p">[];</span>
<span class="p">}</span>
</code></pre></div></div>

<ul>
  <li>partial result: <code class="language-plaintext highlighter-rouge">SemanticTokensDeltaPartialResult</code> defines as follows:</li>
</ul>

<div class="anchorHolder"><a href="#semanticTokensDeltaPartialResult" name="semanticTokensDeltaPartialResult" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">SemanticTokensDeltaPartialResult</span> <span class="p">{</span>
	<span class="nl">edits</span><span class="p">:</span> <span class="nx">SemanticTokensEdit</span><span class="p">[];</span>
<span class="p">}</span>
</code></pre></div></div>

<ul>
  <li>error: code and message set in case an exception happens during the ‘textDocument/semanticTokens/full/delta’ request</li>
</ul>

<p><strong>Requesting semantic tokens for a range</strong></p>

<p>There are two uses cases where it can be beneficial to only compute semantic tokens for a visible range:</p>

<ul>
  <li>for faster rendering of the tokens in the user interface when a user opens a file. In this use cases servers should also implement the <code class="language-plaintext highlighter-rouge">textDocument/semanticTokens/full</code> request as well to allow for flicker free scrolling and semantic coloring of a minimap.</li>
  <li>if computing semantic tokens for a full document is too expensive servers can only provide a range call. In this case the client might not render a minimap correctly or might even decide to not show any semantic tokens at all.</li>
</ul>

<p>A server is allowed to compute the semantic tokens for a broader range than requested by the client. However if the server does the semantic tokens for the broader range must be complete and correct.</p>

<p><em>Request</em>:</p>

<div class="anchorHolder"><a href="#semanticTokens_rangeRequest" name="semanticTokens_rangeRequest" class="linkableAnchor"></a></div>

<ul>
  <li>method: <code class="language-plaintext highlighter-rouge">textDocument/semanticTokens/range</code>
</li>
  <li>params: <code class="language-plaintext highlighter-rouge">SemanticTokensRangeParams</code> defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#semanticTokensRangeParams" name="semanticTokensRangeParams" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">SemanticTokensRangeParams</span> <span class="kd">extends</span> <span class="nx">WorkDoneProgressParams</span><span class="p">,</span>
	<span class="nx">PartialResultParams</span> <span class="p">{</span>
	<span class="cm">/**
	 * The text document.
	 */</span>
	<span class="nl">textDocument</span><span class="p">:</span> <span class="nx">TextDocumentIdentifier</span><span class="p">;</span>

	<span class="cm">/**
	 * The range the semantic tokens are requested for.
	 */</span>
	<span class="nl">range</span><span class="p">:</span> <span class="nx">Range</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Response</em>:</p>

<ul>
  <li>result: <code class="language-plaintext highlighter-rouge">SemanticTokens | null</code>
</li>
  <li>partial result: <code class="language-plaintext highlighter-rouge">SemanticTokensPartialResult</code>
</li>
  <li>error: code and message set in case an exception happens during the ‘textDocument/semanticTokens/range’ request</li>
</ul>

<p><strong>Requesting a refresh of all semantic tokens</strong></p>

<p>The <code class="language-plaintext highlighter-rouge">workspace/semanticTokens/refresh</code> request is sent from the server to the client. Servers can use it to ask clients to refresh the editors for which this server provides semantic tokens. As a result the client should ask the server to recompute the semantic tokens for these editors. This is useful if a server detects a project wide configuration change which requires a re-calculation of all semantic tokens. Note that the client still has the freedom to delay the re-calculation of the semantic tokens if for example an editor is currently not visible.</p>

<p><em>Client Capability</em>:</p>

<ul>
  <li>property name (optional): <code class="language-plaintext highlighter-rouge">workspace.semanticTokens</code>
</li>
  <li>property type: <code class="language-plaintext highlighter-rouge">SemanticTokensWorkspaceClientCapabilities</code> defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#semanticTokensWorkspaceClientCapabilities" name="semanticTokensWorkspaceClientCapabilities" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">SemanticTokensWorkspaceClientCapabilities</span> <span class="p">{</span>
	<span class="cm">/**
	 * Whether the client implementation supports a refresh request sent from
	 * the server to the client.
	 *
	 * Note that this event is global and will force the client to refresh all
	 * semantic tokens currently shown. It should be used with absolute care
	 * and is useful for situation where a server for example detect a project
	 * wide change that requires such a calculation.
	 */</span>
	<span class="nl">refreshSupport</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Request</em>:</p>

<div class="anchorHolder"><a href="#semanticTokens_refreshRequest" name="semanticTokens_refreshRequest" class="linkableAnchor"></a></div>

<ul>
  <li>method: <code class="language-plaintext highlighter-rouge">workspace/semanticTokens/refresh</code>
</li>
  <li>params: none</li>
</ul>

<p><em>Response</em>:</p>

<ul>
  <li>result: void</li>
  <li>error: code and message set in case an exception happens during the ‘workspace/semanticTokens/refresh’ request</li>
</ul>

<h4 id="inlay-hint-request-leftwards_arrow_with_hook"><a href="#textDocument_inlayHint" name="textDocument_inlayHint" class="anchor">Inlay Hint Request (<img class="emoji" title=":leftwards_arrow_with_hook:" alt=":leftwards_arrow_with_hook:" src="https://github.githubassets.com/images/icons/emoji/unicode/21a9.png" height="20" width="20">)</a></h4>

<blockquote>
  <p><em>Since version 3.17.0</em></p>
</blockquote>

<p>The inlay hints request is sent from the client to the server to compute inlay hints for a given [text document, range] tuple that may be rendered in the editor in place with other text.</p>

<p><em>Client Capability</em>:</p>
<ul>
  <li>property name (optional): <code class="language-plaintext highlighter-rouge">textDocument.inlayHint</code>
</li>
  <li>property type: <code class="language-plaintext highlighter-rouge">InlayHintClientCapabilities</code> defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#inlayHintClientCapabilities" name="inlayHintClientCapabilities" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * Inlay hint client capabilities.
 *
 * @since 3.17.0
 */</span>
<span class="k">export</span> <span class="kr">interface</span> <span class="nx">InlayHintClientCapabilities</span> <span class="p">{</span>

	<span class="cm">/**
	 * Whether inlay hints support dynamic registration.
	 */</span>
	<span class="nl">dynamicRegistration</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>

	<span class="cm">/**
	 * Indicates which properties a client can resolve lazily on an inlay
	 * hint.
	 */</span>
	<span class="nl">resolveSupport</span><span class="p">?:</span> <span class="p">{</span>

		<span class="cm">/**
		 * The properties that a client can resolve lazily.
		 */</span>
		<span class="na">properties</span><span class="p">:</span> <span class="kr">string</span><span class="p">[];</span>
	<span class="p">};</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Server Capability</em>:</p>
<ul>
  <li>property name (optional): <code class="language-plaintext highlighter-rouge">inlayHintProvider</code>
</li>
  <li>property type: <code class="language-plaintext highlighter-rouge">InlayHintOptions</code> defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#inlayHintOptions" name="inlayHintOptions" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * Inlay hint options used during static registration.
 *
 * @since 3.17.0
 */</span>
<span class="k">export</span> <span class="kr">interface</span> <span class="nx">InlayHintOptions</span> <span class="kd">extends</span> <span class="nx">WorkDoneProgressOptions</span> <span class="p">{</span>
	<span class="cm">/**
	 * The server provides support to resolve additional
	 * information for an inlay hint item.
	 */</span>
	<span class="nl">resolveProvider</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Registration Options</em>: <code class="language-plaintext highlighter-rouge">InlayHintRegistrationOptions</code> defined as follows:</p>

<div class="anchorHolder"><a href="#inlayHintRegistrationOptions" name="inlayHintRegistrationOptions" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * Inlay hint options used during static or dynamic registration.
 *
 * @since 3.17.0
 */</span>
<span class="k">export</span> <span class="kr">interface</span> <span class="nx">InlayHintRegistrationOptions</span> <span class="kd">extends</span> <span class="nx">InlayHintOptions</span><span class="p">,</span>
	<span class="nx">TextDocumentRegistrationOptions</span><span class="p">,</span> <span class="nx">StaticRegistrationOptions</span> <span class="p">{</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Request</em>:</p>
<ul>
  <li>method: <code class="language-plaintext highlighter-rouge">textDocument/inlayHint</code>
</li>
  <li>params: <code class="language-plaintext highlighter-rouge">InlayHintParams</code> defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#inlayHintParams" name="inlayHintParams" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * A parameter literal used in inlay hint requests.
 *
 * @since 3.17.0
 */</span>
<span class="k">export</span> <span class="kr">interface</span> <span class="nx">InlayHintParams</span> <span class="kd">extends</span> <span class="nx">WorkDoneProgressParams</span> <span class="p">{</span>
	<span class="cm">/**
	 * The text document.
	 */</span>
	<span class="nl">textDocument</span><span class="p">:</span> <span class="nx">TextDocumentIdentifier</span><span class="p">;</span>

	<span class="cm">/**
	 * The visible document range for which inlay hints should be computed.
	 */</span>
	<span class="nl">range</span><span class="p">:</span> <span class="nx">Range</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Response</em>:</p>
<ul>
  <li>result: <code class="language-plaintext highlighter-rouge">InlayHint[]</code> | <code class="language-plaintext highlighter-rouge">null</code> defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#inlayHint" name="inlayHint" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * Inlay hint information.
 *
 * @since 3.17.0
 */</span>
<span class="k">export</span> <span class="kr">interface</span> <span class="nx">InlayHint</span> <span class="p">{</span>

	<span class="cm">/**
	 * The position of this hint.
	 */</span>
	<span class="nl">position</span><span class="p">:</span> <span class="nx">Position</span><span class="p">;</span>

	<span class="cm">/**
	 * The label of this hint. A human readable string or an array of
	 * InlayHintLabelPart label parts.
	 *
	 * *Note* that neither the string nor the label part can be empty.
	 */</span>
	<span class="nl">label</span><span class="p">:</span> <span class="kr">string</span> <span class="o">|</span> <span class="nx">InlayHintLabelPart</span><span class="p">[];</span>

	<span class="cm">/**
	 * The kind of this hint. Can be omitted in which case the client
	 * should fall back to a reasonable default.
	 */</span>
	<span class="nl">kind</span><span class="p">?:</span> <span class="nx">InlayHintKind</span><span class="p">;</span>

	<span class="cm">/**
	 * Optional text edits that are performed when accepting this inlay hint.
	 *
	 * *Note* that edits are expected to change the document so that the inlay
	 * hint (or its nearest variant) is now part of the document and the inlay
	 * hint itself is now obsolete.
	 *
	 * Depending on the client capability `inlayHint.resolveSupport` clients
	 * might resolve this property late using the resolve request.
	 */</span>
	<span class="nl">textEdits</span><span class="p">?:</span> <span class="nx">TextEdit</span><span class="p">[];</span>

	<span class="cm">/**
	 * The tooltip text when you hover over this item.
	 *
	 * Depending on the client capability `inlayHint.resolveSupport` clients
	 * might resolve this property late using the resolve request.
	 */</span>
	<span class="nl">tooltip</span><span class="p">?:</span> <span class="kr">string</span> <span class="o">|</span> <span class="nx">MarkupContent</span><span class="p">;</span>

	<span class="cm">/**
	 * Render padding before the hint.
	 *
	 * Note: Padding should use the editor's background color, not the
	 * background color of the hint itself. That means padding can be used
	 * to visually align/separate an inlay hint.
	 */</span>
	<span class="nl">paddingLeft</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>

	<span class="cm">/**
	 * Render padding after the hint.
	 *
	 * Note: Padding should use the editor's background color, not the
	 * background color of the hint itself. That means padding can be used
	 * to visually align/separate an inlay hint.
	 */</span>
	<span class="nl">paddingRight</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>


	<span class="cm">/**
	 * A data entry field that is preserved on an inlay hint between
	 * a `textDocument/inlayHint` and a `inlayHint/resolve` request.
	 */</span>
	<span class="nl">data</span><span class="p">?:</span> <span class="nx">LSPAny</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<div class="anchorHolder"><a href="#inlayHintLabelPart" name="inlayHintLabelPart" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * An inlay hint label part allows for interactive and composite labels
 * of inlay hints.
 *
 * @since 3.17.0
 */</span>
<span class="k">export</span> <span class="kr">interface</span> <span class="nx">InlayHintLabelPart</span> <span class="p">{</span>

	<span class="cm">/**
	 * The value of this label part.
	 */</span>
	<span class="nl">value</span><span class="p">:</span> <span class="kr">string</span><span class="p">;</span>

	<span class="cm">/**
	 * The tooltip text when you hover over this label part. Depending on
	 * the client capability `inlayHint.resolveSupport` clients might resolve
	 * this property late using the resolve request.
	 */</span>
	<span class="nl">tooltip</span><span class="p">?:</span> <span class="kr">string</span> <span class="o">|</span> <span class="nx">MarkupContent</span><span class="p">;</span>

	<span class="cm">/**
	 * An optional source code location that represents this
	 * label part.
	 *
	 * The editor will use this location for the hover and for code navigation
	 * features: This part will become a clickable link that resolves to the
	 * definition of the symbol at the given location (not necessarily the
	 * location itself), it shows the hover that shows at the given location,
	 * and it shows a context menu with further code navigation commands.
	 *
	 * Depending on the client capability `inlayHint.resolveSupport` clients
	 * might resolve this property late using the resolve request.
	 */</span>
	<span class="nl">location</span><span class="p">?:</span> <span class="nx">Location</span><span class="p">;</span>

	<span class="cm">/**
	 * An optional command for this label part.
	 *
	 * Depending on the client capability `inlayHint.resolveSupport` clients
	 * might resolve this property late using the resolve request.
	 */</span>
	<span class="nl">command</span><span class="p">?:</span> <span class="nx">Command</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<div class="anchorHolder"><a href="#inlayHintKind" name="inlayHintKind" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * Inlay hint kinds.
 *
 * @since 3.17.0
 */</span>
<span class="k">export</span> <span class="k">namespace</span> <span class="nx">InlayHintKind</span> <span class="p">{</span>

	<span class="cm">/**
	 * An inlay hint that for a type annotation.
	 */</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">Type</span> <span class="o">=</span> <span class="mi">1</span><span class="p">;</span>

	<span class="cm">/**
	 * An inlay hint that is for a parameter.
	 */</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">Parameter</span> <span class="o">=</span> <span class="mi">2</span><span class="p">;</span>
<span class="p">}</span>

<span class="k">export</span> <span class="kd">type</span> <span class="nx">InlayHintKind</span> <span class="o">=</span> <span class="mi">1</span> <span class="o">|</span> <span class="mi">2</span><span class="p">;</span>
</code></pre></div></div>

<ul>
  <li>error: code and message set in case an exception happens during the inlay hint request.</li>
</ul>

<h4 id="inlay-hint-resolve-request-leftwards_arrow_with_hook"><a href="#inlayHint_resolve" name="inlayHint_resolve" class="anchor">Inlay Hint Resolve Request (<img class="emoji" title=":leftwards_arrow_with_hook:" alt=":leftwards_arrow_with_hook:" src="https://github.githubassets.com/images/icons/emoji/unicode/21a9.png" height="20" width="20">)</a></h4>

<blockquote>
  <p><em>Since version 3.17.0</em></p>
</blockquote>

<p>The request is sent from the client to the server to resolve additional information for a given inlay hint. This is usually used to compute
the <code class="language-plaintext highlighter-rouge">tooltip</code>, <code class="language-plaintext highlighter-rouge">location</code> or <code class="language-plaintext highlighter-rouge">command</code> properties of an inlay hint’s label part to avoid its unnecessary computation during the <code class="language-plaintext highlighter-rouge">textDocument/inlayHint</code> request.</p>

<p>Consider the clients announces the <code class="language-plaintext highlighter-rouge">label.location</code> property as a property that can be resolved lazy using the client capability</p>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="nx">textDocument</span><span class="p">.</span><span class="nx">inlayHint</span><span class="p">.</span><span class="nx">resolveSupport</span> <span class="o">=</span> <span class="p">{</span> <span class="na">properties</span><span class="p">:</span> <span class="p">[</span><span class="dl">'</span><span class="s1">label.location</span><span class="dl">'</span><span class="p">]</span> <span class="p">};</span>
</code></pre></div></div>

<p>then an inlay hint with a label part without a location needs to be resolved using the <code class="language-plaintext highlighter-rouge">inlayHint/resolve</code> request before it can be used.</p>

<p><em>Client Capability</em>:</p>
<ul>
  <li>property name (optional): <code class="language-plaintext highlighter-rouge">textDocument.inlayHint.resolveSupport</code>
</li>
  <li>property type: <code class="language-plaintext highlighter-rouge">{ properties: string[]; }</code>
</li>
</ul>

<p><em>Request</em>:</p>
<ul>
  <li>method: <code class="language-plaintext highlighter-rouge">inlayHint/resolve</code>
</li>
  <li>params: <code class="language-plaintext highlighter-rouge">InlayHint</code>
</li>
</ul>

<p><em>Response</em>:</p>
<ul>
  <li>result: <code class="language-plaintext highlighter-rouge">InlayHint</code>
</li>
  <li>error: code and message set in case an exception happens during the completion resolve request.</li>
</ul>

<h4 id="inlay-hint-refresh-request--arrow_right_hook"><a href="#workspace_inlayHint_refresh" name="workspace_inlayHint_refresh" class="anchor">Inlay Hint Refresh Request  (<img class="emoji" title=":arrow_right_hook:" alt=":arrow_right_hook:" src="https://github.githubassets.com/images/icons/emoji/unicode/21aa.png" height="20" width="20">)</a></h4>

<blockquote>
  <p><em>Since version 3.17.0</em></p>
</blockquote>

<p>The <code class="language-plaintext highlighter-rouge">workspace/inlayHint/refresh</code> request is sent from the server to the client. Servers can use it to ask clients to refresh the inlay hints currently shown in editors. As a result the client should ask the server to recompute the inlay hints for these editors. This is useful if a server detects a configuration change which requires a re-calculation of all inlay hints. Note that the client still has the freedom to delay the re-calculation of the inlay hints if for example an editor is currently not visible.</p>

<p><em>Client Capability</em>:</p>

<ul>
  <li>property name (optional): <code class="language-plaintext highlighter-rouge">workspace.inlayHint</code>
</li>
  <li>property type: <code class="language-plaintext highlighter-rouge">InlayHintWorkspaceClientCapabilities</code> defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#inlayHintWorkspaceClientCapabilities" name="inlayHintWorkspaceClientCapabilities" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * Client workspace capabilities specific to inlay hints.
 *
 * @since 3.17.0
 */</span>
<span class="k">export</span> <span class="kr">interface</span> <span class="nx">InlayHintWorkspaceClientCapabilities</span> <span class="p">{</span>
	<span class="cm">/**
	 * Whether the client implementation supports a refresh request sent from
	 * the server to the client.
	 *
	 * Note that this event is global and will force the client to refresh all
	 * inlay hints currently shown. It should be used with absolute care and
	 * is useful for situation where a server for example detects a project wide
	 * change that requires such a calculation.
	 */</span>
	<span class="nl">refreshSupport</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Request</em>:</p>
<ul>
  <li>method: <code class="language-plaintext highlighter-rouge">workspace/inlayHint/refresh</code>
</li>
  <li>params: none</li>
</ul>

<p><em>Response</em>:</p>

<ul>
  <li>result: void</li>
  <li>error: code and message set in case an exception happens during the ‘workspace/inlayHint/refresh’ request</li>
</ul>

<h4 id="inline-value-request-leftwards_arrow_with_hook"><a href="#textDocument_inlineValue" name="textDocument_inlineValue" class="anchor">Inline Value Request (<img class="emoji" title=":leftwards_arrow_with_hook:" alt=":leftwards_arrow_with_hook:" src="https://github.githubassets.com/images/icons/emoji/unicode/21a9.png" height="20" width="20">)</a></h4>

<blockquote>
  <p><em>Since version 3.17.0</em></p>
</blockquote>

<p>The inline value request is sent from the client to the server to compute inline values for a given text document that may be rendered in the editor at the end of lines.</p>

<p><em>Client Capability</em>:</p>
<ul>
  <li>property name (optional): <code class="language-plaintext highlighter-rouge">textDocument.inlineValue</code>
</li>
  <li>property type: <code class="language-plaintext highlighter-rouge">InlineValueClientCapabilities</code> defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#inlineValueClientCapabilities" name="inlineValueClientCapabilities" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * Client capabilities specific to inline values.
 *
 * @since 3.17.0
 */</span>
<span class="k">export</span> <span class="kr">interface</span> <span class="nx">InlineValueClientCapabilities</span> <span class="p">{</span>
	<span class="cm">/**
	 * Whether implementation supports dynamic registration for inline
	 * value providers.
	 */</span>
	<span class="nl">dynamicRegistration</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Server Capability</em>:</p>
<ul>
  <li>property name (optional): <code class="language-plaintext highlighter-rouge">inlineValueProvider</code>
</li>
  <li>property type: <code class="language-plaintext highlighter-rouge">InlineValueOptions</code> defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#inlineValueOptions" name="inlineValueOptions" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * Inline value options used during static registration.
 *
 * @since 3.17.0
 */</span>
<span class="k">export</span> <span class="kr">interface</span> <span class="nx">InlineValueOptions</span> <span class="kd">extends</span> <span class="nx">WorkDoneProgressOptions</span> <span class="p">{</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Registration Options</em>: <code class="language-plaintext highlighter-rouge">InlineValueRegistrationOptions</code> defined as follows:</p>

<div class="anchorHolder"><a href="#inlineValueRegistrationOptions" name="inlineValueRegistrationOptions" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * Inline value options used during static or dynamic registration.
 *
 * @since 3.17.0
 */</span>
<span class="k">export</span> <span class="kr">interface</span> <span class="nx">InlineValueRegistrationOptions</span> <span class="kd">extends</span> <span class="nx">InlineValueOptions</span><span class="p">,</span>
	<span class="nx">TextDocumentRegistrationOptions</span><span class="p">,</span> <span class="nx">StaticRegistrationOptions</span> <span class="p">{</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Request</em>:</p>
<ul>
  <li>method: <code class="language-plaintext highlighter-rouge">textDocument/inlineValue</code>
</li>
  <li>params: <code class="language-plaintext highlighter-rouge">InlineValueParams</code> defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#inlineValueParams" name="inlineValueParams" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * A parameter literal used in inline value requests.
 *
 * @since 3.17.0
 */</span>
<span class="k">export</span> <span class="kr">interface</span> <span class="nx">InlineValueParams</span> <span class="kd">extends</span> <span class="nx">WorkDoneProgressParams</span> <span class="p">{</span>
	<span class="cm">/**
	 * The text document.
	 */</span>
	<span class="nl">textDocument</span><span class="p">:</span> <span class="nx">TextDocumentIdentifier</span><span class="p">;</span>

	<span class="cm">/**
	 * The document range for which inline values should be computed.
	 */</span>
	<span class="nl">range</span><span class="p">:</span> <span class="nx">Range</span><span class="p">;</span>

	<span class="cm">/**
	 * Additional information about the context in which inline values were
	 * requested.
	 */</span>
	<span class="nl">context</span><span class="p">:</span> <span class="nx">InlineValueContext</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<div class="anchorHolder"><a href="#inlineValueContext" name="inlineValueContext" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * @since 3.17.0
 */</span>
<span class="k">export</span> <span class="kr">interface</span> <span class="nx">InlineValueContext</span> <span class="p">{</span>
	<span class="cm">/**
	 * The stack frame (as a DAP Id) where the execution has stopped.
	 */</span>
	<span class="nl">frameId</span><span class="p">:</span> <span class="nx">integer</span><span class="p">;</span>

	<span class="cm">/**
	 * The document range where execution has stopped.
	 * Typically the end position of the range denotes the line where the
	 * inline values are shown.
	 */</span>
	<span class="nl">stoppedLocation</span><span class="p">:</span> <span class="nx">Range</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Response</em>:</p>
<ul>
  <li>result: <code class="language-plaintext highlighter-rouge">InlineValue[]</code> | <code class="language-plaintext highlighter-rouge">null</code> defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#inlineValueText" name="inlineValueText" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * Provide inline value as text.
 *
 * @since 3.17.0
 */</span>
<span class="k">export</span> <span class="kr">interface</span> <span class="nx">InlineValueText</span> <span class="p">{</span>
	<span class="cm">/**
	 * The document range for which the inline value applies.
	 */</span>
	<span class="nl">range</span><span class="p">:</span> <span class="nx">Range</span><span class="p">;</span>

	<span class="cm">/**
	 * The text of the inline value.
	 */</span>
	<span class="nl">text</span><span class="p">:</span> <span class="kr">string</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<div class="anchorHolder"><a href="#inlineValueVariableLookup" name="inlineValueVariableLookup" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * Provide inline value through a variable lookup.
 *
 * If only a range is specified, the variable name will be extracted from
 * the underlying document.
 *
 * An optional variable name can be used to override the extracted name.
 *
 * @since 3.17.0
 */</span>
<span class="k">export</span> <span class="kr">interface</span> <span class="nx">InlineValueVariableLookup</span> <span class="p">{</span>
	<span class="cm">/**
	 * The document range for which the inline value applies.
	 * The range is used to extract the variable name from the underlying
	 * document.
	 */</span>
	<span class="nl">range</span><span class="p">:</span> <span class="nx">Range</span><span class="p">;</span>

	<span class="cm">/**
	 * If specified the name of the variable to look up.
	 */</span>
	<span class="nl">variableName</span><span class="p">?:</span> <span class="kr">string</span><span class="p">;</span>

	<span class="cm">/**
	 * How to perform the lookup.
	 */</span>
	<span class="nl">caseSensitiveLookup</span><span class="p">:</span> <span class="nx">boolean</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<div class="anchorHolder"><a href="#inlineValueEvaluatableExpression" name="inlineValueEvaluatableExpression" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * Provide an inline value through an expression evaluation.
 *
 * If only a range is specified, the expression will be extracted from the
 * underlying document.
 *
 * An optional expression can be used to override the extracted expression.
 *
 * @since 3.17.0
 */</span>
<span class="k">export</span> <span class="kr">interface</span> <span class="nx">InlineValueEvaluatableExpression</span> <span class="p">{</span>
	<span class="cm">/**
	 * The document range for which the inline value applies.
	 * The range is used to extract the evaluatable expression from the
	 * underlying document.
	 */</span>
	<span class="nl">range</span><span class="p">:</span> <span class="nx">Range</span><span class="p">;</span>

	<span class="cm">/**
	 * If specified the expression overrides the extracted expression.
	 */</span>
	<span class="nl">expression</span><span class="p">?:</span> <span class="kr">string</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<div class="anchorHolder"><a href="#inlineValue" name="inlineValue" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * Inline value information can be provided by different means:
 * - directly as a text value (class InlineValueText).
 * - as a name to use for a variable lookup (class InlineValueVariableLookup)
 * - as an evaluatable expression (class InlineValueEvaluatableExpression)
 * The InlineValue types combines all inline value types into one type.
 *
 * @since 3.17.0
 */</span>
<span class="k">export</span> <span class="kd">type</span> <span class="nx">InlineValue</span> <span class="o">=</span> <span class="nx">InlineValueText</span> <span class="o">|</span> <span class="nx">InlineValueVariableLookup</span>
	<span class="o">|</span> <span class="nx">InlineValueEvaluatableExpression</span><span class="p">;</span>
</code></pre></div></div>
<ul>
  <li>error: code and message set in case an exception happens during the inline values request.</li>
</ul>

<h4 id="inline-value-refresh-request--arrow_right_hook"><a href="#workspace_inlineValue_refresh" name="workspace_inlineValue_refresh" class="anchor">Inline Value Refresh Request  (<img class="emoji" title=":arrow_right_hook:" alt=":arrow_right_hook:" src="https://github.githubassets.com/images/icons/emoji/unicode/21aa.png" height="20" width="20">)</a></h4>

<blockquote>
  <p><em>Since version 3.17.0</em></p>
</blockquote>

<p>The <code class="language-plaintext highlighter-rouge">workspace/inlineValue/refresh</code> request is sent from the server to the client. Servers can use it to ask clients to refresh the inline values currently shown in editors. As a result the client should ask the server to recompute the inline values for these editors. This is useful if a server detects a configuration change which requires a re-calculation of all inline values. Note that the client still has the freedom to delay the re-calculation of the inline values if for example an editor is currently not visible.</p>

<p><em>Client Capability</em>:</p>

<ul>
  <li>property name (optional): <code class="language-plaintext highlighter-rouge">workspace.inlineValue</code>
</li>
  <li>property type: <code class="language-plaintext highlighter-rouge">InlineValueWorkspaceClientCapabilities</code> defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#inlineValueWorkspaceClientCapabilities" name="inlineValueWorkspaceClientCapabilities" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * Client workspace capabilities specific to inline values.
 *
 * @since 3.17.0
 */</span>
<span class="k">export</span> <span class="kr">interface</span> <span class="nx">InlineValueWorkspaceClientCapabilities</span> <span class="p">{</span>
	<span class="cm">/**
	 * Whether the client implementation supports a refresh request sent from
	 * the server to the client.
	 *
	 * Note that this event is global and will force the client to refresh all
	 * inline values currently shown. It should be used with absolute care and
	 * is useful for situation where a server for example detect a project wide
	 * change that requires such a calculation.
	 */</span>
	<span class="nl">refreshSupport</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>
<p><em>Request</em>:</p>
<ul>
  <li>method: <code class="language-plaintext highlighter-rouge">workspace/inlineValue/refresh</code>
</li>
  <li>params: none</li>
</ul>

<p><em>Response</em>:</p>

<ul>
  <li>result: void</li>
  <li>error: code and message set in case an exception happens during the ‘workspace/inlineValue/refresh’ request</li>
</ul>

<h4 id="monikers-leftwards_arrow_with_hook"><a href="#textDocument_moniker" name="textDocument_moniker" class="anchor">Monikers (<img class="emoji" title=":leftwards_arrow_with_hook:" alt=":leftwards_arrow_with_hook:" src="https://github.githubassets.com/images/icons/emoji/unicode/21a9.png" height="20" width="20">)</a></h4>

<blockquote>
  <p><em>Since version 3.16.0</em></p>
</blockquote>

<p>Language Server Index Format (LSIF) introduced the concept of symbol monikers to help associate symbols across different indexes. This request adds capability for LSP server implementations to provide the same symbol moniker information given a text document position. Clients can utilize this method to get the moniker at the current location in a file user is editing and do further code navigation queries in other services that rely on LSIF indexes and link symbols together.</p>

<p>The <code class="language-plaintext highlighter-rouge">textDocument/moniker</code> request is sent from the client to the server to get the symbol monikers for a given text document position. An array of Moniker types is returned as response to indicate possible monikers at the given location. If no monikers can be calculated, an empty array or <code class="language-plaintext highlighter-rouge">null</code> should be returned.</p>

<p><em>Client Capabilities</em>:</p>

<ul>
  <li>property name (optional): <code class="language-plaintext highlighter-rouge">textDocument.moniker</code>
</li>
  <li>property type: <code class="language-plaintext highlighter-rouge">MonikerClientCapabilities</code> defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#monikerClientCapabilities" name="monikerClientCapabilities" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="kr">interface</span> <span class="nx">MonikerClientCapabilities</span> <span class="p">{</span>
	<span class="cm">/**
	 * Whether implementation supports dynamic registration. If this is set to
	 * `true` the client supports the new `(TextDocumentRegistrationOptions &amp;
	 * StaticRegistrationOptions)` return value for the corresponding server
	 * capability as well.
	 */</span>
	<span class="nl">dynamicRegistration</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Server Capability</em>:</p>

<ul>
  <li>property name (optional): <code class="language-plaintext highlighter-rouge">monikerProvider</code>
</li>
  <li>property type: <code class="language-plaintext highlighter-rouge">boolean | MonikerOptions | MonikerRegistrationOptions</code> is defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#monikerOptions" name="monikerOptions" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">MonikerOptions</span> <span class="kd">extends</span> <span class="nx">WorkDoneProgressOptions</span> <span class="p">{</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Registration Options</em>: <code class="language-plaintext highlighter-rouge">MonikerRegistrationOptions</code> defined as follows:</p>

<div class="anchorHolder"><a href="#monikerRegistrationOptions" name="monikerRegistrationOptions" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">MonikerRegistrationOptions</span> <span class="kd">extends</span>
	<span class="nx">TextDocumentRegistrationOptions</span><span class="p">,</span> <span class="nx">MonikerOptions</span> <span class="p">{</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Request</em>:</p>

<ul>
  <li>method: <code class="language-plaintext highlighter-rouge">textDocument/moniker</code>
</li>
  <li>params: <code class="language-plaintext highlighter-rouge">MonikerParams</code> defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#monikerParams" name="monikerParams" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">MonikerParams</span> <span class="kd">extends</span> <span class="nx">TextDocumentPositionParams</span><span class="p">,</span>
	<span class="nx">WorkDoneProgressParams</span><span class="p">,</span> <span class="nx">PartialResultParams</span> <span class="p">{</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Response</em>:</p>

<ul>
  <li>result: <code class="language-plaintext highlighter-rouge">Moniker[] | null</code>
</li>
  <li>partial result: <code class="language-plaintext highlighter-rouge">Moniker[]</code>
</li>
  <li>error: code and message set in case an exception happens during the ‘textDocument/moniker’ request</li>
</ul>

<p><code class="language-plaintext highlighter-rouge">Moniker</code> is defined as follows:</p>

<div class="anchorHolder"><a href="#uniquenessLevel" name="uniquenessLevel" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * Moniker uniqueness level to define scope of the moniker.
 */</span>
<span class="k">export</span> <span class="kr">enum</span> <span class="nx">UniquenessLevel</span> <span class="p">{</span>
	<span class="cm">/**
	 * The moniker is only unique inside a document
	 */</span>
	<span class="nb">document</span> <span class="o">=</span> <span class="dl">'</span><span class="s1">document</span><span class="dl">'</span><span class="p">,</span>

	<span class="cm">/**
	 * The moniker is unique inside a project for which a dump got created
	 */</span>
	<span class="nx">project</span> <span class="o">=</span> <span class="dl">'</span><span class="s1">project</span><span class="dl">'</span><span class="p">,</span>

	<span class="cm">/**
	 * The moniker is unique inside the group to which a project belongs
	 */</span>
	<span class="nx">group</span> <span class="o">=</span> <span class="dl">'</span><span class="s1">group</span><span class="dl">'</span><span class="p">,</span>

	<span class="cm">/**
	 * The moniker is unique inside the moniker scheme.
	 */</span>
	<span class="nx">scheme</span> <span class="o">=</span> <span class="dl">'</span><span class="s1">scheme</span><span class="dl">'</span><span class="p">,</span>

	<span class="cm">/**
	 * The moniker is globally unique
	 */</span>
	<span class="nb">global</span> <span class="o">=</span> <span class="dl">'</span><span class="s1">global</span><span class="dl">'</span>
<span class="p">}</span>
</code></pre></div></div>

<div class="anchorHolder"><a href="#monikerKind" name="monikerKind" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * The moniker kind.
 */</span>
<span class="k">export</span> <span class="kr">enum</span> <span class="nx">MonikerKind</span> <span class="p">{</span>
	<span class="cm">/**
	 * The moniker represent a symbol that is imported into a project
	 */</span>
	<span class="k">import</span> <span class="o">=</span> <span class="dl">'</span><span class="s1">import</span><span class="dl">'</span><span class="p">,</span>

	<span class="cm">/**
	 * The moniker represents a symbol that is exported from a project
	 */</span>
	<span class="k">export</span> <span class="o">=</span> <span class="dl">'</span><span class="s1">export</span><span class="dl">'</span><span class="p">,</span>

	<span class="cm">/**
	 * The moniker represents a symbol that is local to a project (e.g. a local
	 * variable of a function, a class not visible outside the project, ...)
	 */</span>
	<span class="nx">local</span> <span class="o">=</span> <span class="dl">'</span><span class="s1">local</span><span class="dl">'</span>
<span class="p">}</span>
</code></pre></div></div>

<div class="anchorHolder"><a href="#moniker" name="moniker" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * Moniker definition to match LSIF 0.5 moniker definition.
 */</span>
<span class="k">export</span> <span class="kr">interface</span> <span class="nx">Moniker</span> <span class="p">{</span>
	<span class="cm">/**
	 * The scheme of the moniker. For example tsc or .Net
	 */</span>
	<span class="nl">scheme</span><span class="p">:</span> <span class="kr">string</span><span class="p">;</span>

	<span class="cm">/**
	 * The identifier of the moniker. The value is opaque in LSIF however
	 * schema owners are allowed to define the structure if they want.
	 */</span>
	<span class="nl">identifier</span><span class="p">:</span> <span class="kr">string</span><span class="p">;</span>

	<span class="cm">/**
	 * The scope in which the moniker is unique
	 */</span>
	<span class="nl">unique</span><span class="p">:</span> <span class="nx">UniquenessLevel</span><span class="p">;</span>

	<span class="cm">/**
	 * The moniker kind if known.
	 */</span>
	<span class="nl">kind</span><span class="p">?:</span> <span class="nx">MonikerKind</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<h5 id="notes">Notes</h5>

<p>Server implementations of this method should ensure that the moniker calculation matches to those used in the corresponding LSIF implementation to ensure symbols can be associated correctly across IDE sessions and LSIF indexes.</p>
<h4 id="completion-request-leftwards_arrow_with_hook"><a href="#textDocument_completion" name="textDocument_completion" class="anchor">Completion Request (<img class="emoji" title=":leftwards_arrow_with_hook:" alt=":leftwards_arrow_with_hook:" src="https://github.githubassets.com/images/icons/emoji/unicode/21a9.png" height="20" width="20">)</a></h4>

<p>The Completion request is sent from the client to the server to compute completion items at a given cursor position. Completion items are presented in the <a href="https://code.visualstudio.com/docs/editor/intellisense">IntelliSense</a> user interface. If computing full completion items is expensive, servers can additionally provide a handler for the completion item resolve request (‘completionItem/resolve’). This request is sent when a completion item is selected in the user interface. A typical use case is for example: the <code class="language-plaintext highlighter-rouge">textDocument/completion</code> request doesn’t fill in the <code class="language-plaintext highlighter-rouge">documentation</code> property for returned completion items since it is expensive to compute. When the item is selected in the user interface then a ‘completionItem/resolve’ request is sent with the selected completion item as a parameter. The returned completion item should have the documentation property filled in. By default the request can only delay the computation of the <code class="language-plaintext highlighter-rouge">detail</code> and <code class="language-plaintext highlighter-rouge">documentation</code> properties. Since 3.16.0 the client
can signal that it can resolve more properties lazily. This is done using the <code class="language-plaintext highlighter-rouge">completionItem#resolveSupport</code> client capability which lists all properties that can be filled in during a ‘completionItem/resolve’ request. All other properties (usually <code class="language-plaintext highlighter-rouge">sortText</code>, <code class="language-plaintext highlighter-rouge">filterText</code>, <code class="language-plaintext highlighter-rouge">insertText</code> and <code class="language-plaintext highlighter-rouge">textEdit</code>) must be provided in the <code class="language-plaintext highlighter-rouge">textDocument/completion</code> response and must not be changed during resolve.</p>

<p>The language server protocol uses the following model around completions:</p>

<ul>
  <li>to achieve consistency across languages and to honor different clients usually the client is responsible for filtering and sorting. This has also the advantage that client can experiment with different filter and sorting models. However servers can enforce different behavior by setting a <code class="language-plaintext highlighter-rouge">filterText</code> / <code class="language-plaintext highlighter-rouge">sortText</code>
</li>
  <li>for speed clients should be able to filter an already received completion list if the user continues typing. Servers can opt out of this using a <code class="language-plaintext highlighter-rouge">CompletionList</code> and mark it as <code class="language-plaintext highlighter-rouge">isIncomplete</code>.</li>
</ul>

<p>A completion item provides additional means to influence filtering and sorting. They are expressed by either creating a <code class="language-plaintext highlighter-rouge">CompletionItem</code> with a <code class="language-plaintext highlighter-rouge">insertText</code> or with a <code class="language-plaintext highlighter-rouge">textEdit</code>. The two modes differ as follows:</p>

<ul>
  <li>
    <p><strong>Completion item provides an insertText / label without a text edit</strong>: in the model the client should filter against what the user has already typed using the word boundary rules of the language (e.g. resolving the word under the cursor position). The reason for this mode is that it makes it extremely easy for a server to implement a basic completion list and get it filtered on the client.</p>
  </li>
  <li>
    <p><strong>Completion Item with text edits</strong>: in this mode the server tells the client that it actually knows what it is doing. If you create a completion item with a text edit at the current cursor position no word guessing takes place and no filtering should happen. This mode can be combined with a sort text and filter text to customize two things. If the text edit is a replace edit then the range denotes the word used for filtering. If the replace changes the text it most likely makes sense to specify a filter text to be used.</p>
  </li>
</ul>

<p><em>Client Capability</em>:</p>
<ul>
  <li>property name (optional): <code class="language-plaintext highlighter-rouge">textDocument.completion</code>
</li>
  <li>property type: <code class="language-plaintext highlighter-rouge">CompletionClientCapabilities</code> defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#completionClientCapabilities" name="completionClientCapabilities" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">CompletionClientCapabilities</span> <span class="p">{</span>
	<span class="cm">/**
	 * Whether completion supports dynamic registration.
	 */</span>
	<span class="nl">dynamicRegistration</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>

	<span class="cm">/**
	 * The client supports the following `CompletionItem` specific
	 * capabilities.
	 */</span>
	<span class="nl">completionItem</span><span class="p">?:</span> <span class="p">{</span>
		<span class="cm">/**
		 * Client supports snippets as insert text.
		 *
		 * A snippet can define tab stops and placeholders with `$1`, `$2`
		 * and `${3:foo}`. `$0` defines the final tab stop, it defaults to
		 * the end of the snippet. Placeholders with equal identifiers are
		 * linked, that is typing in one will update others too.
		 */</span>
		<span class="nx">snippetSupport</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>

		<span class="cm">/**
		 * Client supports commit characters on a completion item.
		 */</span>
		<span class="nl">commitCharactersSupport</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>

		<span class="cm">/**
		 * Client supports the follow content formats for the documentation
		 * property. The order describes the preferred format of the client.
		 */</span>
		<span class="nl">documentationFormat</span><span class="p">?:</span> <span class="nx">MarkupKind</span><span class="p">[];</span>

		<span class="cm">/**
		 * Client supports the deprecated property on a completion item.
		 */</span>
		<span class="nl">deprecatedSupport</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>

		<span class="cm">/**
		 * Client supports the preselect property on a completion item.
		 */</span>
		<span class="nl">preselectSupport</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>

		<span class="cm">/**
		 * Client supports the tag property on a completion item. Clients
		 * supporting tags have to handle unknown tags gracefully. Clients
		 * especially need to preserve unknown tags when sending a completion
		 * item back to the server in a resolve call.
		 *
		 * @since 3.15.0
		 */</span>
		<span class="nl">tagSupport</span><span class="p">?:</span> <span class="p">{</span>
			<span class="cm">/**
			 * The tags supported by the client.
			 */</span>
			<span class="na">valueSet</span><span class="p">:</span> <span class="nx">CompletionItemTag</span><span class="p">[];</span>
		<span class="p">};</span>

		<span class="cm">/**
		 * Client supports insert replace edit to control different behavior if
		 * a completion item is inserted in the text or should replace text.
		 *
		 * @since 3.16.0
		 */</span>
		<span class="nl">insertReplaceSupport</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>

		<span class="cm">/**
		 * Indicates which properties a client can resolve lazily on a
		 * completion item. Before version 3.16.0 only the predefined properties
		 * `documentation` and `detail` could be resolved lazily.
		 *
		 * @since 3.16.0
		 */</span>
		<span class="nl">resolveSupport</span><span class="p">?:</span> <span class="p">{</span>
			<span class="cm">/**
			 * The properties that a client can resolve lazily.
			 */</span>
			<span class="na">properties</span><span class="p">:</span> <span class="kr">string</span><span class="p">[];</span>
		<span class="p">};</span>

		<span class="cm">/**
		 * The client supports the `insertTextMode` property on
		 * a completion item to override the whitespace handling mode
		 * as defined by the client (see `insertTextMode`).
		 *
		 * @since 3.16.0
		 */</span>
		<span class="nl">insertTextModeSupport</span><span class="p">?:</span> <span class="p">{</span>
			<span class="na">valueSet</span><span class="p">:</span> <span class="nx">InsertTextMode</span><span class="p">[];</span>
		<span class="p">};</span>

		<span class="cm">/**
		 * The client has support for completion item label
		 * details (see also `CompletionItemLabelDetails`).
		 *
		 * @since 3.17.0
		 */</span>
		<span class="nl">labelDetailsSupport</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>
	<span class="p">};</span>

	<span class="nl">completionItemKind</span><span class="p">?:</span> <span class="p">{</span>
		<span class="cm">/**
		 * The completion item kind values the client supports. When this
		 * property exists the client also guarantees that it will
		 * handle values outside its set gracefully and falls back
		 * to a default value when unknown.
		 *
		 * If this property is not present the client only supports
		 * the completion items kinds from `Text` to `Reference` as defined in
		 * the initial version of the protocol.
		 */</span>
		<span class="nx">valueSet</span><span class="p">?:</span> <span class="nx">CompletionItemKind</span><span class="p">[];</span>
	<span class="p">};</span>

	<span class="cm">/**
	 * The client supports to send additional context information for a
	 * `textDocument/completion` request.
	 */</span>
	<span class="nl">contextSupport</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>

	<span class="cm">/**
	 * The client's default when the completion item doesn't provide a
	 * `insertTextMode` property.
	 *
	 * @since 3.17.0
	 */</span>
	<span class="nl">insertTextMode</span><span class="p">?:</span> <span class="nx">InsertTextMode</span><span class="p">;</span>

	<span class="cm">/**
	 * The client supports the following `CompletionList` specific
	 * capabilities.
	 *
	 * @since 3.17.0
	 */</span>
	<span class="nl">completionList</span><span class="p">?:</span> <span class="p">{</span>
		<span class="cm">/**
		 * The client supports the following itemDefaults on
		 * a completion list.
		 *
		 * The value lists the supported property names of the
		 * `CompletionList.itemDefaults` object. If omitted
		 * no properties are supported.
		 *
		 * @since 3.17.0
		 */</span>
		<span class="nx">itemDefaults</span><span class="p">?:</span> <span class="kr">string</span><span class="p">[];</span>
	<span class="p">}</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Server Capability</em>:</p>
<ul>
  <li>property name (optional): <code class="language-plaintext highlighter-rouge">completionProvider</code>
</li>
  <li>property type: <code class="language-plaintext highlighter-rouge">CompletionOptions</code> defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#completionOptions" name="completionOptions" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * Completion options.
 */</span>
<span class="k">export</span> <span class="kr">interface</span> <span class="nx">CompletionOptions</span> <span class="kd">extends</span> <span class="nx">WorkDoneProgressOptions</span> <span class="p">{</span>
	<span class="cm">/**
	 * The additional characters, beyond the defaults provided by the client (typically
	 * [a-zA-Z]), that should automatically trigger a completion request. For example
	 * `.` in JavaScript represents the beginning of an object property or method and is
	 * thus a good candidate for triggering a completion request.
	 *
	 * Most tools trigger a completion request automatically without explicitly
	 * requesting it using a keyboard shortcut (e.g. Ctrl+Space). Typically they
	 * do so when the user starts to type an identifier. For example if the user
	 * types `c` in a JavaScript file code complete will automatically pop up
	 * present `console` besides others as a completion item. Characters that
	 * make up identifiers don't need to be listed here.
	 */</span>
	<span class="nl">triggerCharacters</span><span class="p">?:</span> <span class="kr">string</span><span class="p">[];</span>

	<span class="cm">/**
	 * The list of all possible characters that commit a completion. This field
	 * can be used if clients don't support individual commit characters per
	 * completion item. See client capability
	 * `completion.completionItem.commitCharactersSupport`.
	 *
	 * If a server provides both `allCommitCharacters` and commit characters on
	 * an individual completion item the ones on the completion item win.
	 *
	 * @since 3.2.0
	 */</span>
	<span class="nl">allCommitCharacters</span><span class="p">?:</span> <span class="kr">string</span><span class="p">[];</span>

	<span class="cm">/**
	 * The server provides support to resolve additional
	 * information for a completion item.
	 */</span>
	<span class="nl">resolveProvider</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>

	<span class="cm">/**
	 * The server supports the following `CompletionItem` specific
	 * capabilities.
	 *
	 * @since 3.17.0
	 */</span>
	<span class="nl">completionItem</span><span class="p">?:</span> <span class="p">{</span>
		<span class="cm">/**
		 * The server has support for completion item label
		 * details (see also `CompletionItemLabelDetails`) when receiving
		 * a completion item in a resolve call.
		 *
		 * @since 3.17.0
		 */</span>
		<span class="nx">labelDetailsSupport</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>
	<span class="p">}</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Registration Options</em>: <code class="language-plaintext highlighter-rouge">CompletionRegistrationOptions</code> options defined as follows:</p>

<div class="anchorHolder"><a href="#completionRegistrationOptions" name="completionRegistrationOptions" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">CompletionRegistrationOptions</span>
	<span class="kd">extends</span> <span class="nx">TextDocumentRegistrationOptions</span><span class="p">,</span> <span class="nx">CompletionOptions</span> <span class="p">{</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Request</em>:</p>
<ul>
  <li>method: <code class="language-plaintext highlighter-rouge">textDocument/completion</code>
</li>
  <li>params: <code class="language-plaintext highlighter-rouge">CompletionParams</code> defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#completionParams" name="completionParams" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">CompletionParams</span> <span class="kd">extends</span> <span class="nx">TextDocumentPositionParams</span><span class="p">,</span>
	<span class="nx">WorkDoneProgressParams</span><span class="p">,</span> <span class="nx">PartialResultParams</span> <span class="p">{</span>
	<span class="cm">/**
	 * The completion context. This is only available if the client specifies
	 * to send this using the client capability
	 * `completion.contextSupport === true`
	 */</span>
	<span class="nl">context</span><span class="p">?:</span> <span class="nx">CompletionContext</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<div class="anchorHolder"><a href="#completionTriggerKind" name="completionTriggerKind" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * How a completion was triggered
 */</span>
<span class="k">export</span> <span class="k">namespace</span> <span class="nx">CompletionTriggerKind</span> <span class="p">{</span>
	<span class="cm">/**
	 * Completion was triggered by typing an identifier (24x7 code
	 * complete), manual invocation (e.g Ctrl+Space) or via API.
	 */</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">Invoked</span><span class="p">:</span> <span class="mi">1</span> <span class="o">=</span> <span class="mi">1</span><span class="p">;</span>

	<span class="cm">/**
	 * Completion was triggered by a trigger character specified by
	 * the `triggerCharacters` properties of the
	 * `CompletionRegistrationOptions`.
	 */</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">TriggerCharacter</span><span class="p">:</span> <span class="mi">2</span> <span class="o">=</span> <span class="mi">2</span><span class="p">;</span>

	<span class="cm">/**
	 * Completion was re-triggered as the current completion list is incomplete.
	 */</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">TriggerForIncompleteCompletions</span><span class="p">:</span> <span class="mi">3</span> <span class="o">=</span> <span class="mi">3</span><span class="p">;</span>
<span class="p">}</span>
<span class="k">export</span> <span class="kd">type</span> <span class="nx">CompletionTriggerKind</span> <span class="o">=</span> <span class="mi">1</span> <span class="o">|</span> <span class="mi">2</span> <span class="o">|</span> <span class="mi">3</span><span class="p">;</span>
</code></pre></div></div>

<div class="anchorHolder"><a href="#completionContext" name="completionContext" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * Contains additional information about the context in which a completion
 * request is triggered.
 */</span>
<span class="k">export</span> <span class="kr">interface</span> <span class="nx">CompletionContext</span> <span class="p">{</span>
	<span class="cm">/**
	 * How the completion was triggered.
	 */</span>
	<span class="nl">triggerKind</span><span class="p">:</span> <span class="nx">CompletionTriggerKind</span><span class="p">;</span>

	<span class="cm">/**
	 * The trigger character (a single character) that has trigger code
	 * complete. Is undefined if
	 * `triggerKind !== CompletionTriggerKind.TriggerCharacter`
	 */</span>
	<span class="nl">triggerCharacter</span><span class="p">?:</span> <span class="kr">string</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Response</em>:</p>
<ul>
  <li>result: <code class="language-plaintext highlighter-rouge">CompletionItem[]</code> | <code class="language-plaintext highlighter-rouge">CompletionList</code> | <code class="language-plaintext highlighter-rouge">null</code>. If a <code class="language-plaintext highlighter-rouge">CompletionItem[]</code> is provided it is interpreted to be complete. So it is the same as <code class="language-plaintext highlighter-rouge">{ isIncomplete: false, items }</code>
</li>
</ul>

<div class="anchorHolder"><a href="#completionList" name="completionList" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * Represents a collection of [completion items](#CompletionItem) to be
 * presented in the editor.
 */</span>
<span class="k">export</span> <span class="kr">interface</span> <span class="nx">CompletionList</span> <span class="p">{</span>
	<span class="cm">/**
	 * This list is not complete. Further typing should result in recomputing
	 * this list.
	 *
	 * Recomputed lists have all their items replaced (not appended) in the
	 * incomplete completion sessions.
	 */</span>
	<span class="nl">isIncomplete</span><span class="p">:</span> <span class="nx">boolean</span><span class="p">;</span>

	<span class="cm">/**
	 * In many cases the items of an actual completion result share the same
	 * value for properties like `commitCharacters` or the range of a text
	 * edit. A completion list can therefore define item defaults which will
	 * be used if a completion item itself doesn't specify the value.
	 *
	 * If a completion list specifies a default value and a completion item
	 * also specifies a corresponding value the one from the item is used.
	 *
	 * Servers are only allowed to return default values if the client
	 * signals support for this via the `completionList.itemDefaults`
	 * capability.
	 *
	 * @since 3.17.0
	 */</span>
	<span class="nl">itemDefaults</span><span class="p">?:</span> <span class="p">{</span>
		<span class="cm">/**
		 * A default commit character set.
		 *
		 * @since 3.17.0
		 */</span>
		<span class="nx">commitCharacters</span><span class="p">?:</span> <span class="kr">string</span><span class="p">[];</span>

		<span class="cm">/**
		 * A default edit range
		 *
		 * @since 3.17.0
		 */</span>
		<span class="nl">editRange</span><span class="p">?:</span> <span class="nx">Range</span> <span class="o">|</span> <span class="p">{</span>
			<span class="na">insert</span><span class="p">:</span> <span class="nx">Range</span><span class="p">;</span>
			<span class="nl">replace</span><span class="p">:</span> <span class="nx">Range</span><span class="p">;</span>
		<span class="p">};</span>

		<span class="cm">/**
		 * A default insert text format
		 *
		 * @since 3.17.0
		 */</span>
		<span class="nl">insertTextFormat</span><span class="p">?:</span> <span class="nx">InsertTextFormat</span><span class="p">;</span>

		<span class="cm">/**
		 * A default insert text mode
		 *
		 * @since 3.17.0
		 */</span>
		<span class="nl">insertTextMode</span><span class="p">?:</span> <span class="nx">InsertTextMode</span><span class="p">;</span>

		<span class="cm">/**
		 * A default data value.
		 *
		 * @since 3.17.0
		 */</span>
		<span class="nl">data</span><span class="p">?:</span> <span class="nx">LSPAny</span><span class="p">;</span>
	<span class="p">}</span>

	<span class="cm">/**
	 * The completion items.
	 */</span>
	<span class="nl">items</span><span class="p">:</span> <span class="nx">CompletionItem</span><span class="p">[];</span>
<span class="p">}</span>
</code></pre></div></div>

<div class="anchorHolder"><a href="#insertTextFormat" name="insertTextFormat" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * Defines whether the insert text in a completion item should be interpreted as
 * plain text or a snippet.
 */</span>
<span class="k">export</span> <span class="k">namespace</span> <span class="nx">InsertTextFormat</span> <span class="p">{</span>
	<span class="cm">/**
	 * The primary text to be inserted is treated as a plain string.
	 */</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">PlainText</span> <span class="o">=</span> <span class="mi">1</span><span class="p">;</span>

	<span class="cm">/**
	 * The primary text to be inserted is treated as a snippet.
	 *
	 * A snippet can define tab stops and placeholders with `$1`, `$2`
	 * and `${3:foo}`. `$0` defines the final tab stop, it defaults to
	 * the end of the snippet. Placeholders with equal identifiers are linked,
	 * that is typing in one will update others too.
	 */</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">Snippet</span> <span class="o">=</span> <span class="mi">2</span><span class="p">;</span>
<span class="p">}</span>

<span class="k">export</span> <span class="kd">type</span> <span class="nx">InsertTextFormat</span> <span class="o">=</span> <span class="mi">1</span> <span class="o">|</span> <span class="mi">2</span><span class="p">;</span>
</code></pre></div></div>

<div class="anchorHolder"><a href="#completionItemTag" name="completionItemTag" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * Completion item tags are extra annotations that tweak the rendering of a
 * completion item.
 *
 * @since 3.15.0
 */</span>
<span class="k">export</span> <span class="k">namespace</span> <span class="nx">CompletionItemTag</span> <span class="p">{</span>
	<span class="cm">/**
	 * Render a completion as obsolete, usually using a strike-out.
	 */</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">Deprecated</span> <span class="o">=</span> <span class="mi">1</span><span class="p">;</span>
<span class="p">}</span>

<span class="k">export</span> <span class="kd">type</span> <span class="nx">CompletionItemTag</span> <span class="o">=</span> <span class="mi">1</span><span class="p">;</span>
</code></pre></div></div>

<div class="anchorHolder"><a href="#insertReplaceEdit" name="insertReplaceEdit" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * A special text edit to provide an insert and a replace operation.
 *
 * @since 3.16.0
 */</span>
<span class="k">export</span> <span class="kr">interface</span> <span class="nx">InsertReplaceEdit</span> <span class="p">{</span>
	<span class="cm">/**
	 * The string to be inserted.
	 */</span>
	<span class="nl">newText</span><span class="p">:</span> <span class="kr">string</span><span class="p">;</span>

	<span class="cm">/**
	 * The range if the insert is requested
	 */</span>
	<span class="nl">insert</span><span class="p">:</span> <span class="nx">Range</span><span class="p">;</span>

	<span class="cm">/**
	 * The range if the replace is requested.
	 */</span>
	<span class="nl">replace</span><span class="p">:</span> <span class="nx">Range</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<div class="anchorHolder"><a href="#insertTextMode" name="insertTextMode" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * How whitespace and indentation is handled during completion
 * item insertion.
 *
 * @since 3.16.0
 */</span>
<span class="k">export</span> <span class="k">namespace</span> <span class="nx">InsertTextMode</span> <span class="p">{</span>
	<span class="cm">/**
	 * The insertion or replace strings is taken as it is. If the
	 * value is multi line the lines below the cursor will be
	 * inserted using the indentation defined in the string value.
	 * The client will not apply any kind of adjustments to the
	 * string.
	 */</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">asIs</span><span class="p">:</span> <span class="mi">1</span> <span class="o">=</span> <span class="mi">1</span><span class="p">;</span>

	<span class="cm">/**
	 * The editor adjusts leading whitespace of new lines so that
	 * they match the indentation up to the cursor of the line for
	 * which the item is accepted.
	 *
	 * Consider a line like this: &lt;2tabs&gt;&lt;cursor&gt;&lt;3tabs&gt;foo. Accepting a
	 * multi line completion item is indented using 2 tabs and all
	 * following lines inserted will be indented using 2 tabs as well.
	 */</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">adjustIndentation</span><span class="p">:</span> <span class="mi">2</span> <span class="o">=</span> <span class="mi">2</span><span class="p">;</span>
<span class="p">}</span>

<span class="k">export</span> <span class="kd">type</span> <span class="nx">InsertTextMode</span> <span class="o">=</span> <span class="mi">1</span> <span class="o">|</span> <span class="mi">2</span><span class="p">;</span>
</code></pre></div></div>

<div class="anchorHolder"><a href="#completionItemLabelDetails" name="completionItemLabelDetails" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * Additional details for a completion item label.
 *
 * @since 3.17.0
 */</span>
<span class="k">export</span> <span class="kr">interface</span> <span class="nx">CompletionItemLabelDetails</span> <span class="p">{</span>

	<span class="cm">/**
	 * An optional string which is rendered less prominently directly after
	 * {@link CompletionItem.label label}, without any spacing. Should be
	 * used for function signatures or type annotations.
	 */</span>
	<span class="nl">detail</span><span class="p">?:</span> <span class="kr">string</span><span class="p">;</span>

	<span class="cm">/**
	 * An optional string which is rendered less prominently after
	 * {@link CompletionItemLabelDetails.detail}. Should be used for fully qualified
	 * names or file path.
	 */</span>
	<span class="nl">description</span><span class="p">?:</span> <span class="kr">string</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<div class="anchorHolder"><a href="#completionItem" name="completionItem" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">CompletionItem</span> <span class="p">{</span>

	<span class="cm">/**
	 * The label of this completion item.
	 *
	 * The label property is also by default the text that
	 * is inserted when selecting this completion.
	 *
	 * If label details are provided the label itself should
	 * be an unqualified name of the completion item.
	 */</span>
	<span class="nl">label</span><span class="p">:</span> <span class="kr">string</span><span class="p">;</span>

	<span class="cm">/**
	 * Additional details for the label
	 *
	 * @since 3.17.0
	 */</span>
	<span class="nl">labelDetails</span><span class="p">?:</span> <span class="nx">CompletionItemLabelDetails</span><span class="p">;</span>


	<span class="cm">/**
	 * The kind of this completion item. Based of the kind
	 * an icon is chosen by the editor. The standardized set
	 * of available values is defined in `CompletionItemKind`.
	 */</span>
	<span class="nl">kind</span><span class="p">?:</span> <span class="nx">CompletionItemKind</span><span class="p">;</span>

	<span class="cm">/**
	 * Tags for this completion item.
	 *
	 * @since 3.15.0
	 */</span>
	<span class="nl">tags</span><span class="p">?:</span> <span class="nx">CompletionItemTag</span><span class="p">[];</span>

	<span class="cm">/**
	 * A human-readable string with additional information
	 * about this item, like type or symbol information.
	 */</span>
	<span class="nl">detail</span><span class="p">?:</span> <span class="kr">string</span><span class="p">;</span>

	<span class="cm">/**
	 * A human-readable string that represents a doc-comment.
	 */</span>
	<span class="nl">documentation</span><span class="p">?:</span> <span class="kr">string</span> <span class="o">|</span> <span class="nx">MarkupContent</span><span class="p">;</span>

	<span class="cm">/**
	 * Indicates if this item is deprecated.
	 *
	 * @deprecated Use `tags` instead if supported.
	 */</span>
	<span class="nl">deprecated</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>

	<span class="cm">/**
	 * Select this item when showing.
	 *
	 * *Note* that only one completion item can be selected and that the
	 * tool / client decides which item that is. The rule is that the *first*
	 * item of those that match best is selected.
	 */</span>
	<span class="nl">preselect</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>

	<span class="cm">/**
	 * A string that should be used when comparing this item
	 * with other items. When omitted the label is used
	 * as the sort text for this item.
	 */</span>
	<span class="nl">sortText</span><span class="p">?:</span> <span class="kr">string</span><span class="p">;</span>

	<span class="cm">/**
	 * A string that should be used when filtering a set of
	 * completion items. When omitted the label is used as the
	 * filter text for this item.
	 */</span>
	<span class="nl">filterText</span><span class="p">?:</span> <span class="kr">string</span><span class="p">;</span>

	<span class="cm">/**
	 * A string that should be inserted into a document when selecting
	 * this completion. When omitted the label is used as the insert text
	 * for this item.
	 *
	 * The `insertText` is subject to interpretation by the client side.
	 * Some tools might not take the string literally. For example
	 * VS Code when code complete is requested in this example
	 * `con&lt;cursor position&gt;` and a completion item with an `insertText` of
	 * `console` is provided it will only insert `sole`. Therefore it is
	 * recommended to use `textEdit` instead since it avoids additional client
	 * side interpretation.
	 */</span>
	<span class="nl">insertText</span><span class="p">?:</span> <span class="kr">string</span><span class="p">;</span>

	<span class="cm">/**
	 * The format of the insert text. The format applies to both the
	 * `insertText` property and the `newText` property of a provided
	 * `textEdit`. If omitted defaults to `InsertTextFormat.PlainText`.
	 *
	 * Please note that the insertTextFormat doesn't apply to
	 * `additionalTextEdits`.
	 */</span>
	<span class="nl">insertTextFormat</span><span class="p">?:</span> <span class="nx">InsertTextFormat</span><span class="p">;</span>

	<span class="cm">/**
	 * How whitespace and indentation is handled during completion
	 * item insertion. If not provided the client's default value depends on
	 * the `textDocument.completion.insertTextMode` client capability.
	 *
	 * @since 3.16.0
	 * @since 3.17.0 - support for `textDocument.completion.insertTextMode`
	 */</span>
	<span class="nl">insertTextMode</span><span class="p">?:</span> <span class="nx">InsertTextMode</span><span class="p">;</span>

	<span class="cm">/**
	 * An edit which is applied to a document when selecting this completion.
	 * When an edit is provided the value of `insertText` is ignored.
	 *
	 * *Note:* The range of the edit must be a single line range and it must
	 * contain the position at which completion has been requested.
	 *
	 * Most editors support two different operations when accepting a completion
	 * item. One is to insert a completion text and the other is to replace an
	 * existing text with a completion text. Since this can usually not be
	 * predetermined by a server it can report both ranges. Clients need to
	 * signal support for `InsertReplaceEdit`s via the
	 * `textDocument.completion.completionItem.insertReplaceSupport` client
	 * capability property.
	 *
	 * *Note 1:* The text edit's range as well as both ranges from an insert
	 * replace edit must be a [single line] and they must contain the position
	 * at which completion has been requested.
	 * *Note 2:* If an `InsertReplaceEdit` is returned the edit's insert range
	 * must be a prefix of the edit's replace range, that means it must be
	 * contained and starting at the same position.
	 *
	 * @since 3.16.0 additional type `InsertReplaceEdit`
	 */</span>
	<span class="nl">textEdit</span><span class="p">?:</span> <span class="nx">TextEdit</span> <span class="o">|</span> <span class="nx">InsertReplaceEdit</span><span class="p">;</span>

	<span class="cm">/**
	 * The edit text used if the completion item is part of a CompletionList and
	 * CompletionList defines an item default for the text edit range.
	 *
	 * Clients will only honor this property if they opt into completion list
	 * item defaults using the capability `completionList.itemDefaults`.
	 *
	 * If not provided and a list's default range is provided the label
	 * property is used as a text.
	 *
	 * @since 3.17.0
	 */</span>
	<span class="nl">textEditText</span><span class="p">?:</span> <span class="kr">string</span><span class="p">;</span>

	<span class="cm">/**
	 * An optional array of additional text edits that are applied when
	 * selecting this completion. Edits must not overlap (including the same
	 * insert position) with the main edit nor with themselves.
	 *
	 * Additional text edits should be used to change text unrelated to the
	 * current cursor position (for example adding an import statement at the
	 * top of the file if the completion item will insert an unqualified type).
	 */</span>
	<span class="nl">additionalTextEdits</span><span class="p">?:</span> <span class="nx">TextEdit</span><span class="p">[];</span>

	<span class="cm">/**
	 * An optional set of characters that when pressed while this completion is
	 * active will accept it first and then type that character. *Note* that all
	 * commit characters should have `length=1` and that superfluous characters
	 * will be ignored.
	 */</span>
	<span class="nl">commitCharacters</span><span class="p">?:</span> <span class="kr">string</span><span class="p">[];</span>

	<span class="cm">/**
	 * An optional command that is executed *after* inserting this completion.
	 * *Note* that additional modifications to the current document should be
	 * described with the additionalTextEdits-property.
	 */</span>
	<span class="nl">command</span><span class="p">?:</span> <span class="nx">Command</span><span class="p">;</span>

	<span class="cm">/**
	 * A data entry field that is preserved on a completion item between
	 * a completion and a completion resolve request.
	 */</span>
	<span class="nl">data</span><span class="p">?:</span> <span class="nx">LSPAny</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<div class="anchorHolder"><a href="#completionItemKind" name="completionItemKind" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * The kind of a completion entry.
 */</span>
<span class="k">export</span> <span class="k">namespace</span> <span class="nx">CompletionItemKind</span> <span class="p">{</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">Text</span> <span class="o">=</span> <span class="mi">1</span><span class="p">;</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">Method</span> <span class="o">=</span> <span class="mi">2</span><span class="p">;</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nb">Function</span> <span class="o">=</span> <span class="mi">3</span><span class="p">;</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">Constructor</span> <span class="o">=</span> <span class="mi">4</span><span class="p">;</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">Field</span> <span class="o">=</span> <span class="mi">5</span><span class="p">;</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">Variable</span> <span class="o">=</span> <span class="mi">6</span><span class="p">;</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">Class</span> <span class="o">=</span> <span class="mi">7</span><span class="p">;</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">Interface</span> <span class="o">=</span> <span class="mi">8</span><span class="p">;</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">Module</span> <span class="o">=</span> <span class="mi">9</span><span class="p">;</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">Property</span> <span class="o">=</span> <span class="mi">10</span><span class="p">;</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">Unit</span> <span class="o">=</span> <span class="mi">11</span><span class="p">;</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">Value</span> <span class="o">=</span> <span class="mi">12</span><span class="p">;</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">Enum</span> <span class="o">=</span> <span class="mi">13</span><span class="p">;</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">Keyword</span> <span class="o">=</span> <span class="mi">14</span><span class="p">;</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">Snippet</span> <span class="o">=</span> <span class="mi">15</span><span class="p">;</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">Color</span> <span class="o">=</span> <span class="mi">16</span><span class="p">;</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">File</span> <span class="o">=</span> <span class="mi">17</span><span class="p">;</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">Reference</span> <span class="o">=</span> <span class="mi">18</span><span class="p">;</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">Folder</span> <span class="o">=</span> <span class="mi">19</span><span class="p">;</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">EnumMember</span> <span class="o">=</span> <span class="mi">20</span><span class="p">;</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">Constant</span> <span class="o">=</span> <span class="mi">21</span><span class="p">;</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">Struct</span> <span class="o">=</span> <span class="mi">22</span><span class="p">;</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">Event</span> <span class="o">=</span> <span class="mi">23</span><span class="p">;</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">Operator</span> <span class="o">=</span> <span class="mi">24</span><span class="p">;</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">TypeParameter</span> <span class="o">=</span> <span class="mi">25</span><span class="p">;</span>
<span class="p">}</span>

<span class="k">export</span> <span class="kd">type</span> <span class="nx">CompletionItemKind</span> <span class="o">=</span> <span class="mi">1</span> <span class="o">|</span> <span class="mi">2</span> <span class="o">|</span> <span class="mi">3</span> <span class="o">|</span> <span class="mi">4</span> <span class="o">|</span> <span class="mi">5</span> <span class="o">|</span> <span class="mi">6</span> <span class="o">|</span> <span class="mi">7</span> <span class="o">|</span> <span class="mi">8</span> <span class="o">|</span> <span class="mi">9</span> <span class="o">|</span> <span class="mi">10</span> <span class="o">|</span> <span class="mi">11</span> <span class="o">|</span> <span class="mi">12</span> <span class="o">|</span> <span class="mi">13</span> <span class="o">|</span> <span class="mi">14</span> <span class="o">|</span> <span class="mi">15</span> <span class="o">|</span> <span class="mi">16</span> <span class="o">|</span> <span class="mi">17</span> <span class="o">|</span> <span class="mi">18</span> <span class="o">|</span> <span class="mi">19</span> <span class="o">|</span> <span class="mi">20</span> <span class="o">|</span> <span class="mi">21</span> <span class="o">|</span> <span class="mi">22</span> <span class="o">|</span> <span class="mi">23</span> <span class="o">|</span> <span class="mi">24</span> <span class="o">|</span> <span class="mi">25</span><span class="p">;</span>
</code></pre></div></div>
<ul>
  <li>partial result: <code class="language-plaintext highlighter-rouge">CompletionItem[]</code> or <code class="language-plaintext highlighter-rouge">CompletionList</code> followed by <code class="language-plaintext highlighter-rouge">CompletionItem[]</code>. If the first provided result item is of type <code class="language-plaintext highlighter-rouge">CompletionList</code> subsequent partial results of <code class="language-plaintext highlighter-rouge">CompletionItem[]</code> add to the <code class="language-plaintext highlighter-rouge">items</code> property of the <code class="language-plaintext highlighter-rouge">CompletionList</code>.</li>
  <li>error: code and message set in case an exception happens during the completion request.</li>
</ul>

<p>Completion items support snippets (see <code class="language-plaintext highlighter-rouge">InsertTextFormat.Snippet</code>). The snippet format is as follows:</p>

<h5 id="snippet-syntax"><a href="#snippet_syntax" name="snippet_syntax" class="anchor">Snippet Syntax</a></h5>

<p>The <code class="language-plaintext highlighter-rouge">body</code> of a snippet can use special constructs to control cursors and the text being inserted. The following are supported features and their syntaxes:</p>

<h5 id="tab-stops">Tab stops</h5>

<p>With tab stops, you can make the editor cursor move inside a snippet. Use <code class="language-plaintext highlighter-rouge">$1</code>, <code class="language-plaintext highlighter-rouge">$2</code> to specify cursor locations. The number is the order in which tab stops will be visited, whereas <code class="language-plaintext highlighter-rouge">$0</code> denotes the final cursor position. Multiple tab stops are linked and updated in sync.</p>

<h5 id="placeholders">Placeholders</h5>

<p>Placeholders are tab stops with values, like <code class="language-plaintext highlighter-rouge">${1:foo}</code>. The placeholder text will be inserted and selected such that it can be easily changed. Placeholders can be nested, like <code class="language-plaintext highlighter-rouge">${1:another ${2:placeholder}}</code>.</p>

<h5 id="choice">Choice</h5>

<p>Placeholders can have choices as values. The syntax is a comma separated enumeration of values, enclosed with the pipe-character, for example <code class="language-plaintext highlighter-rouge">${1|one,two,three|}</code>. When the snippet is inserted and the placeholder selected, choices will prompt the user to pick one of the values.</p>

<h5 id="variables">Variables</h5>

<p>With <code class="language-plaintext highlighter-rouge">$name</code> or <code class="language-plaintext highlighter-rouge">${name:default}</code> you can insert the value of a variable. When a variable isn’t set, its <em>default</em> or the empty string is inserted. When a variable is unknown (that is, its name isn’t defined) the name of the variable is inserted and it is transformed into a placeholder.</p>

<p>The following variables can be used:</p>

<ul>
  <li>
<code class="language-plaintext highlighter-rouge">TM_SELECTED_TEXT</code> The currently selected text or the empty string</li>
  <li>
<code class="language-plaintext highlighter-rouge">TM_CURRENT_LINE</code> The contents of the current line</li>
  <li>
<code class="language-plaintext highlighter-rouge">TM_CURRENT_WORD</code> The contents of the word under cursor or the empty string</li>
  <li>
<code class="language-plaintext highlighter-rouge">TM_LINE_INDEX</code> The zero-index based line number</li>
  <li>
<code class="language-plaintext highlighter-rouge">TM_LINE_NUMBER</code> The one-index based line number</li>
  <li>
<code class="language-plaintext highlighter-rouge">TM_FILENAME</code> The filename of the current document</li>
  <li>
<code class="language-plaintext highlighter-rouge">TM_FILENAME_BASE</code> The filename of the current document without its extensions</li>
  <li>
<code class="language-plaintext highlighter-rouge">TM_DIRECTORY</code> The directory of the current document</li>
  <li>
<code class="language-plaintext highlighter-rouge">TM_FILEPATH</code> The full file path of the current document</li>
</ul>

<h5 id="variable-transforms">Variable Transforms</h5>

<p>Transformations allow you to modify the value of a variable before it is inserted. The definition of a transformation consists of three parts:</p>

<ol>
  <li>A <a href="#regExp">regular expression</a> that is matched against the value of a variable, or the empty string when the variable cannot be resolved.</li>
  <li>A “format string” that allows to reference matching groups from the regular expression. The format string allows for conditional inserts and simple modifications.</li>
  <li>Options that are passed to the regular expression.</li>
</ol>

<p>The following example inserts the name of the current file without its ending, so from <code class="language-plaintext highlighter-rouge">foo.txt</code> it makes <code class="language-plaintext highlighter-rouge">foo</code>.</p>

<div class="language-plaintext highlighter-rouge"><div class="highlight"><pre class="highlight"><code>${TM_FILENAME/(.*)\..+$/$1/}
  |           |         | |
  |           |         | |-&gt; no options
  |           |         |
  |           |         |-&gt; references the contents of the first
  |           |             capture group
  |           |
  |           |-&gt; regex to capture everything before
  |               the final `.suffix`
  |
  |-&gt; resolves to the filename
</code></pre></div></div>

<h5 id="grammar">Grammar</h5>

<p>Below is the EBNF (<a href="https://en.wikipedia.org/wiki/Extended_Backus-Naur_form">extended Backus-Naur form</a>) for snippets. With <code class="language-plaintext highlighter-rouge">\</code> (backslash), you can escape <code class="language-plaintext highlighter-rouge">$</code>, <code class="language-plaintext highlighter-rouge">}</code> and <code class="language-plaintext highlighter-rouge">\</code>. Within choice elements, the backslash also escapes comma and pipe characters.</p>

<div class="language-plaintext highlighter-rouge"><div class="highlight"><pre class="highlight"><code>any         ::= tabstop | placeholder | choice | variable | text
tabstop     ::= '$' int | '${' int '}'
placeholder ::= '${' int ':' any '}'
choice      ::= '${' int '|' text (',' text)* '|}'
variable    ::= '$' var | '${' var }'
                | '${' var ':' any '}'
                | '${' var '/' regex '/' (format | text)+ '/' options '}'
format      ::= '$' int | '${' int '}'
                | '${' int ':' '/upcase' | '/downcase' | '/capitalize' '}'
                | '${' int ':+' if '}'
                | '${' int ':?' if ':' else '}'
                | '${' int ':-' else '}' | '${' int ':' else '}'
regex       ::= Regular Expression value (ctor-string)
options     ::= Regular Expression option (ctor-options)
var         ::= [_a-zA-Z] [_a-zA-Z0-9]*
int         ::= [0-9]+
text        ::= .*
if			::= text
else		::= text
</code></pre></div></div>

<h4 id="completion-item-resolve-request-leftwards_arrow_with_hook"><a href="#completionItem_resolve" name="completionItem_resolve" class="anchor">Completion Item Resolve Request (<img class="emoji" title=":leftwards_arrow_with_hook:" alt=":leftwards_arrow_with_hook:" src="https://github.githubassets.com/images/icons/emoji/unicode/21a9.png" height="20" width="20">)</a></h4>

<p>The request is sent from the client to the server to resolve additional information for a given completion item.</p>

<p><em>Request</em>:</p>
<ul>
  <li>method: <code class="language-plaintext highlighter-rouge">completionItem/resolve</code>
</li>
  <li>params: <code class="language-plaintext highlighter-rouge">CompletionItem</code>
</li>
</ul>

<p><em>Response</em>:</p>
<ul>
  <li>result: <code class="language-plaintext highlighter-rouge">CompletionItem</code>
</li>
  <li>error: code and message set in case an exception happens during the completion resolve request.</li>
</ul>

<h4 id="publishdiagnostics-notification-arrow_left"><a href="#textDocument_publishDiagnostics" name="textDocument_publishDiagnostics" class="anchor">PublishDiagnostics Notification (<img class="emoji" title=":arrow_left:" alt=":arrow_left:" src="https://github.githubassets.com/images/icons/emoji/unicode/2b05.png" height="20" width="20">)</a></h4>

<p>Diagnostics notifications are sent from the server to the client to signal results of validation runs.</p>

<p>Diagnostics are “owned” by the server so it is the server’s responsibility to clear them if necessary. The following rule is used for VS Code servers that generate diagnostics:</p>

<ul>
  <li>if a language is single file only (for example HTML) then diagnostics are cleared by the server when the file is closed. Please note that open / close events don’t necessarily reflect what the user sees in the user interface. These events are ownership events. So with the current version of the specification it is possible that problems are not cleared although the file is not visible in the user interface since the client has not closed the file yet.</li>
  <li>if a language has a project system (for example C#) diagnostics are not cleared when a file closes. When a project is opened all diagnostics for all files are recomputed (or read from a cache).</li>
</ul>

<p>When a file changes it is the server’s responsibility to re-compute diagnostics and push them to the client. If the computed set is empty it has to push the empty array to clear former diagnostics. Newly pushed diagnostics always replace previously pushed diagnostics. There is no merging that happens on the client side.</p>

<p>See also the <a href="#diagnostic">Diagnostic</a> section.</p>

<p><em>Client Capability</em>:</p>
<ul>
  <li>property name (optional): <code class="language-plaintext highlighter-rouge">textDocument.publishDiagnostics</code>
</li>
  <li>property type: <code class="language-plaintext highlighter-rouge">PublishDiagnosticsClientCapabilities</code> defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#publishDiagnosticsClientCapabilities" name="publishDiagnosticsClientCapabilities" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">PublishDiagnosticsClientCapabilities</span> <span class="p">{</span>
	<span class="cm">/**
	 * Whether the clients accepts diagnostics with related information.
	 */</span>
	<span class="nl">relatedInformation</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>

	<span class="cm">/**
	 * Client supports the tag property to provide meta data about a diagnostic.
	 * Clients supporting tags have to handle unknown tags gracefully.
	 *
	 * @since 3.15.0
	 */</span>
	<span class="nl">tagSupport</span><span class="p">?:</span> <span class="p">{</span>
		<span class="cm">/**
		 * The tags supported by the client.
		 */</span>
		<span class="na">valueSet</span><span class="p">:</span> <span class="nx">DiagnosticTag</span><span class="p">[];</span>
	<span class="p">};</span>

	<span class="cm">/**
	 * Whether the client interprets the version property of the
	 * `textDocument/publishDiagnostics` notification's parameter.
	 *
	 * @since 3.15.0
	 */</span>
	<span class="nl">versionSupport</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>

	<span class="cm">/**
	 * Client supports a codeDescription property
	 *
	 * @since 3.16.0
	 */</span>
	<span class="nl">codeDescriptionSupport</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>

	<span class="cm">/**
	 * Whether code action supports the `data` property which is
	 * preserved between a `textDocument/publishDiagnostics` and
	 * `textDocument/codeAction` request.
	 *
	 * @since 3.16.0
	 */</span>
	<span class="nl">dataSupport</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Notification</em>:</p>
<ul>
  <li>method: <code class="language-plaintext highlighter-rouge">textDocument/publishDiagnostics</code>
</li>
  <li>params: <code class="language-plaintext highlighter-rouge">PublishDiagnosticsParams</code> defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#publishDiagnosticsParams" name="publishDiagnosticsParams" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="kr">interface</span> <span class="nx">PublishDiagnosticsParams</span> <span class="p">{</span>
	<span class="cm">/**
	 * The URI for which diagnostic information is reported.
	 */</span>
	<span class="nl">uri</span><span class="p">:</span> <span class="nx">DocumentUri</span><span class="p">;</span>

	<span class="cm">/**
	 * Optional the version number of the document the diagnostics are published
	 * for.
	 *
	 * @since 3.15.0
	 */</span>
	<span class="nl">version</span><span class="p">?:</span> <span class="nx">integer</span><span class="p">;</span>

	<span class="cm">/**
	 * An array of diagnostic information items.
	 */</span>
	<span class="nl">diagnostics</span><span class="p">:</span> <span class="nx">Diagnostic</span><span class="p">[];</span>
<span class="p">}</span>
</code></pre></div></div>

<h4 id="pull-diagnostics"><a href="#textDocument_pullDiagnostics" name="textDocument_pullDiagnostics" class="anchor">Pull Diagnostics</a></h4>

<p>Diagnostics are currently published by the server to the client using a notification. This model has the advantage that for workspace wide diagnostics the server has the freedom to compute them at a server preferred point in time. On the other hand the approach has the disadvantage that the server can’t prioritize the computation for the file in which the user types or which are visible in the editor. Inferring the client’s UI state from the <code class="language-plaintext highlighter-rouge">textDocument/didOpen</code> and <code class="language-plaintext highlighter-rouge">textDocument/didChange</code> notifications might lead to false positives since these notifications are ownership transfer notifications.</p>

<p>The specification therefore introduces the concept of diagnostic pull requests to give a client more control over the documents for which diagnostics should be computed and at which point in time.</p>

<p><em>Client Capability</em>:</p>
<ul>
  <li>property name (optional): <code class="language-plaintext highlighter-rouge">textDocument.diagnostic</code>
</li>
  <li>property type: <code class="language-plaintext highlighter-rouge">DiagnosticClientCapabilities</code> defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#diagnosticClientCapabilities" name="diagnosticClientCapabilities" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * Client capabilities specific to diagnostic pull requests.
 *
 * @since 3.17.0
 */</span>
<span class="k">export</span> <span class="kr">interface</span> <span class="nx">DiagnosticClientCapabilities</span> <span class="p">{</span>
	<span class="cm">/**
	 * Whether implementation supports dynamic registration. If this is set to
	 * `true` the client supports the new
	 * `(TextDocumentRegistrationOptions &amp; StaticRegistrationOptions)`
	 * return value for the corresponding server capability as well.
	 */</span>
	<span class="nl">dynamicRegistration</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>

	<span class="cm">/**
	 * Whether the clients supports related documents for document diagnostic
	 * pulls.
	 */</span>
	<span class="nl">relatedDocumentSupport</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Server Capability</em>:</p>
<ul>
  <li>property name (optional): <code class="language-plaintext highlighter-rouge">diagnosticProvider</code>
</li>
  <li>property type: <code class="language-plaintext highlighter-rouge">DiagnosticOptions</code> defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#diagnosticOptions" name="diagnosticOptions" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * Diagnostic options.
 *
 * @since 3.17.0
 */</span>
<span class="k">export</span> <span class="kr">interface</span> <span class="nx">DiagnosticOptions</span> <span class="kd">extends</span> <span class="nx">WorkDoneProgressOptions</span> <span class="p">{</span>
	<span class="cm">/**
	 * An optional identifier under which the diagnostics are
	 * managed by the client.
	 */</span>
	<span class="nl">identifier</span><span class="p">?:</span> <span class="kr">string</span><span class="p">;</span>

	<span class="cm">/**
	 * Whether the language has inter file dependencies meaning that
	 * editing code in one file can result in a different diagnostic
	 * set in another file. Inter file dependencies are common for
	 * most programming languages and typically uncommon for linters.
	 */</span>
	<span class="nl">interFileDependencies</span><span class="p">:</span> <span class="nx">boolean</span><span class="p">;</span>

	<span class="cm">/**
	 * The server provides support for workspace diagnostics as well.
	 */</span>
	<span class="nl">workspaceDiagnostics</span><span class="p">:</span> <span class="nx">boolean</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Registration Options</em>: <code class="language-plaintext highlighter-rouge">DiagnosticRegistrationOptions</code> options defined as follows:</p>

<div class="anchorHolder"><a href="#diagnosticRegistrationOptions" name="diagnosticRegistrationOptions" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * Diagnostic registration options.
 *
 * @since 3.17.0
 */</span>
<span class="k">export</span> <span class="kr">interface</span> <span class="nx">DiagnosticRegistrationOptions</span> <span class="kd">extends</span>
	<span class="nx">TextDocumentRegistrationOptions</span><span class="p">,</span> <span class="nx">DiagnosticOptions</span><span class="p">,</span>
	<span class="nx">StaticRegistrationOptions</span> <span class="p">{</span>
<span class="p">}</span>
</code></pre></div></div>

<h5 id="document-diagnosticsleftwards_arrow_with_hook"><a href="#textDocument_diagnostic" name="textDocument_diagnostic" class="anchor">Document Diagnostics(<img class="emoji" title=":leftwards_arrow_with_hook:" alt=":leftwards_arrow_with_hook:" src="https://github.githubassets.com/images/icons/emoji/unicode/21a9.png" height="20" width="20">)</a></h5>

<p>The text document diagnostic request is sent from the client to the server to ask the server to compute the diagnostics for a given document. As with other pull requests the server is asked to compute the diagnostics for the currently synced version of the document.</p>

<p><em>Request</em>:</p>
<ul>
  <li>method: ‘textDocument/diagnostic’.</li>
  <li>params: <code class="language-plaintext highlighter-rouge">DocumentDiagnosticParams</code> defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#documentDiagnosticParams" name="documentDiagnosticParams" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * Parameters of the document diagnostic request.
 *
 * @since 3.17.0
 */</span>
<span class="k">export</span> <span class="kr">interface</span> <span class="nx">DocumentDiagnosticParams</span> <span class="kd">extends</span> <span class="nx">WorkDoneProgressParams</span><span class="p">,</span>
	<span class="nx">PartialResultParams</span> <span class="p">{</span>
	<span class="cm">/**
	 * The text document.
	 */</span>
	<span class="nl">textDocument</span><span class="p">:</span> <span class="nx">TextDocumentIdentifier</span><span class="p">;</span>

	<span class="cm">/**
	 * The additional identifier  provided during registration.
	 */</span>
	<span class="nl">identifier</span><span class="p">?:</span> <span class="kr">string</span><span class="p">;</span>

	<span class="cm">/**
	 * The result id of a previous response if provided.
	 */</span>
	<span class="nl">previousResultId</span><span class="p">?:</span> <span class="kr">string</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Response</em>:</p>
<ul>
  <li>result: <code class="language-plaintext highlighter-rouge">DocumentDiagnosticReport</code> defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#documentDiagnosticReport" name="documentDiagnosticReport" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * The result of a document diagnostic pull request. A report can
 * either be a full report containing all diagnostics for the
 * requested document or a unchanged report indicating that nothing
 * has changed in terms of diagnostics in comparison to the last
 * pull request.
 *
 * @since 3.17.0
 */</span>
<span class="k">export</span> <span class="kd">type</span> <span class="nx">DocumentDiagnosticReport</span> <span class="o">=</span> <span class="nx">RelatedFullDocumentDiagnosticReport</span>
	<span class="o">|</span> <span class="nx">RelatedUnchangedDocumentDiagnosticReport</span><span class="p">;</span>
</code></pre></div></div>

<div class="anchorHolder"><a href="#documentDiagnosticReportKind" name="documentDiagnosticReportKind" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * The document diagnostic report kinds.
 *
 * @since 3.17.0
 */</span>
<span class="k">export</span> <span class="k">namespace</span> <span class="nx">DocumentDiagnosticReportKind</span> <span class="p">{</span>
	<span class="cm">/**
	 * A diagnostic report with a full
	 * set of problems.
	 */</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">Full</span> <span class="o">=</span> <span class="dl">'</span><span class="s1">full</span><span class="dl">'</span><span class="p">;</span>

	<span class="cm">/**
	 * A report indicating that the last
	 * returned report is still accurate.
	 */</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">Unchanged</span> <span class="o">=</span> <span class="dl">'</span><span class="s1">unchanged</span><span class="dl">'</span><span class="p">;</span>
<span class="p">}</span>

<span class="k">export</span> <span class="kd">type</span> <span class="nx">DocumentDiagnosticReportKind</span> <span class="o">=</span> <span class="dl">'</span><span class="s1">full</span><span class="dl">'</span> <span class="o">|</span> <span class="dl">'</span><span class="s1">unchanged</span><span class="dl">'</span><span class="p">;</span>
</code></pre></div></div>

<div class="anchorHolder"><a href="#fullDocumentDiagnosticReport" name="fullDocumentDiagnosticReport" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * A diagnostic report with a full set of problems.
 *
 * @since 3.17.0
 */</span>
<span class="k">export</span> <span class="kr">interface</span> <span class="nx">FullDocumentDiagnosticReport</span> <span class="p">{</span>
	<span class="cm">/**
	 * A full document diagnostic report.
	 */</span>
	<span class="nl">kind</span><span class="p">:</span> <span class="nx">DocumentDiagnosticReportKind</span><span class="p">.</span><span class="nx">Full</span><span class="p">;</span>

	<span class="cm">/**
	 * An optional result id. If provided it will
	 * be sent on the next diagnostic request for the
	 * same document.
	 */</span>
	<span class="nl">resultId</span><span class="p">?:</span> <span class="kr">string</span><span class="p">;</span>

	<span class="cm">/**
	 * The actual items.
	 */</span>
	<span class="nl">items</span><span class="p">:</span> <span class="nx">Diagnostic</span><span class="p">[];</span>
<span class="p">}</span>
</code></pre></div></div>

<div class="anchorHolder"><a href="#unchangedDocumentDiagnosticReport" name="unchangedDocumentDiagnosticReport" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * A diagnostic report indicating that the last returned
 * report is still accurate.
 *
 * @since 3.17.0
 */</span>
<span class="k">export</span> <span class="kr">interface</span> <span class="nx">UnchangedDocumentDiagnosticReport</span> <span class="p">{</span>
	<span class="cm">/**
	 * A document diagnostic report indicating
	 * no changes to the last result. A server can
	 * only return `unchanged` if result ids are
	 * provided.
	 */</span>
	<span class="nl">kind</span><span class="p">:</span> <span class="nx">DocumentDiagnosticReportKind</span><span class="p">.</span><span class="nx">Unchanged</span><span class="p">;</span>

	<span class="cm">/**
	 * A result id which will be sent on the next
	 * diagnostic request for the same document.
	 */</span>
	<span class="nl">resultId</span><span class="p">:</span> <span class="kr">string</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<div class="anchorHolder"><a href="#relatedFullDocumentDiagnosticReport" name="relatedFullDocumentDiagnosticReport" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * A full diagnostic report with a set of related documents.
 *
 * @since 3.17.0
 */</span>
<span class="k">export</span> <span class="kr">interface</span> <span class="nx">RelatedFullDocumentDiagnosticReport</span> <span class="kd">extends</span>
	<span class="nx">FullDocumentDiagnosticReport</span> <span class="p">{</span>
	<span class="cm">/**
	 * Diagnostics of related documents. This information is useful
	 * in programming languages where code in a file A can generate
	 * diagnostics in a file B which A depends on. An example of
	 * such a language is C/C++ where marco definitions in a file
	 * a.cpp and result in errors in a header file b.hpp.
	 *
	 * @since 3.17.0
	 */</span>
	<span class="nl">relatedDocuments</span><span class="p">?:</span> <span class="p">{</span>
		<span class="p">[</span><span class="na">uri</span><span class="p">:</span> <span class="kr">string</span> <span class="cm">/** DocumentUri */</span><span class="p">]:</span>
			<span class="nx">FullDocumentDiagnosticReport</span> <span class="o">|</span> <span class="nx">UnchangedDocumentDiagnosticReport</span><span class="p">;</span>
	<span class="p">};</span>
<span class="p">}</span>
</code></pre></div></div>

<div class="anchorHolder"><a href="#relatedUnchangedDocumentDiagnosticReport" name="relatedUnchangedDocumentDiagnosticReport" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * An unchanged diagnostic report with a set of related documents.
 *
 * @since 3.17.0
 */</span>
<span class="k">export</span> <span class="kr">interface</span> <span class="nx">RelatedUnchangedDocumentDiagnosticReport</span> <span class="kd">extends</span>
	<span class="nx">UnchangedDocumentDiagnosticReport</span> <span class="p">{</span>
	<span class="cm">/**
	 * Diagnostics of related documents. This information is useful
	 * in programming languages where code in a file A can generate
	 * diagnostics in a file B which A depends on. An example of
	 * such a language is C/C++ where marco definitions in a file
	 * a.cpp and result in errors in a header file b.hpp.
	 *
	 * @since 3.17.0
	 */</span>
	<span class="nl">relatedDocuments</span><span class="p">?:</span> <span class="p">{</span>
		<span class="p">[</span><span class="na">uri</span><span class="p">:</span> <span class="kr">string</span> <span class="cm">/** DocumentUri */</span><span class="p">]:</span>
			<span class="nx">FullDocumentDiagnosticReport</span> <span class="o">|</span> <span class="nx">UnchangedDocumentDiagnosticReport</span><span class="p">;</span>
	<span class="p">};</span>
<span class="p">}</span>
</code></pre></div></div>
<ul>
  <li>partial result: The first literal send need to be a <code class="language-plaintext highlighter-rouge">DocumentDiagnosticReport</code> followed by n <code class="language-plaintext highlighter-rouge">DocumentDiagnosticReportPartialResult</code> literals defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#documentDiagnosticReportPartialResult" name="documentDiagnosticReportPartialResult" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * A partial result for a document diagnostic report.
 *
 * @since 3.17.0
 */</span>
<span class="k">export</span> <span class="kr">interface</span> <span class="nx">DocumentDiagnosticReportPartialResult</span> <span class="p">{</span>
	<span class="nl">relatedDocuments</span><span class="p">:</span> <span class="p">{</span>
		<span class="p">[</span><span class="na">uri</span><span class="p">:</span> <span class="kr">string</span> <span class="cm">/** DocumentUri */</span><span class="p">]:</span>
			<span class="nx">FullDocumentDiagnosticReport</span> <span class="o">|</span> <span class="nx">UnchangedDocumentDiagnosticReport</span><span class="p">;</span>
	<span class="p">};</span>
<span class="p">}</span>
</code></pre></div></div>
<ul>
  <li>error: code and message set in case an exception happens during the diagnostic request. A server is also allowed to return an error with code <code class="language-plaintext highlighter-rouge">ServerCancelled</code> indicating that the server can’t compute the result right now. A server can return a <code class="language-plaintext highlighter-rouge">DiagnosticServerCancellationData</code> data to indicate whether the client should re-trigger the request. If no data is provided it defaults to <code class="language-plaintext highlighter-rouge">{ retriggerRequest: true }</code>:</li>
</ul>

<div class="anchorHolder"><a href="#diagnosticServerCancellationData" name="diagnosticServerCancellationData" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * Cancellation data returned from a diagnostic request.
 *
 * @since 3.17.0
 */</span>
<span class="k">export</span> <span class="kr">interface</span> <span class="nx">DiagnosticServerCancellationData</span> <span class="p">{</span>
	<span class="nl">retriggerRequest</span><span class="p">:</span> <span class="nx">boolean</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<h5 id="workspace-diagnosticsleftwards_arrow_with_hook"><a href="#workspace_diagnostic" name="workspace_diagnostic" class="anchor">Workspace Diagnostics(<img class="emoji" title=":leftwards_arrow_with_hook:" alt=":leftwards_arrow_with_hook:" src="https://github.githubassets.com/images/icons/emoji/unicode/21a9.png" height="20" width="20">)</a></h5>

<p>The workspace diagnostic request is sent from the client to the server to ask the server to compute workspace wide diagnostics which previously where pushed from the server to the client. In contrast to the document diagnostic request the workspace request can be long running and is not bound to a specific workspace or document state. If the client supports streaming for the workspace diagnostic pull it is legal to provide a document diagnostic report multiple times for the same document URI. The last one reported will win over previous reports.</p>

<p>If a client receives a diagnostic report for a document in a workspace diagnostic request for which the client also issues individual document diagnostic pull requests the client needs to decide which diagnostics win and should be presented. In general:</p>

<ul>
  <li>diagnostics for a higher document version should win over those from a lower document version (e.g. note that document versions are steadily increasing)</li>
  <li>diagnostics from a document pull should win over diagnostics from a workspace pull.</li>
</ul>

<p><em>Request</em>:</p>
<ul>
  <li>method: ‘workspace/diagnostic’.</li>
  <li>params: <code class="language-plaintext highlighter-rouge">WorkspaceDiagnosticParams</code> defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#workspaceDiagnosticParams" name="workspaceDiagnosticParams" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * Parameters of the workspace diagnostic request.
 *
 * @since 3.17.0
 */</span>
<span class="k">export</span> <span class="kr">interface</span> <span class="nx">WorkspaceDiagnosticParams</span> <span class="kd">extends</span> <span class="nx">WorkDoneProgressParams</span><span class="p">,</span>
	<span class="nx">PartialResultParams</span> <span class="p">{</span>
	<span class="cm">/**
	 * The additional identifier provided during registration.
	 */</span>
	<span class="nl">identifier</span><span class="p">?:</span> <span class="kr">string</span><span class="p">;</span>

	<span class="cm">/**
	 * The currently known diagnostic reports with their
	 * previous result ids.
	 */</span>
	<span class="nl">previousResultIds</span><span class="p">:</span> <span class="nx">PreviousResultId</span><span class="p">[];</span>
<span class="p">}</span>
</code></pre></div></div>

<div class="anchorHolder"><a href="#previousResultId" name="previousResultId" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * A previous result id in a workspace pull request.
 *
 * @since 3.17.0
 */</span>
<span class="k">export</span> <span class="kr">interface</span> <span class="nx">PreviousResultId</span> <span class="p">{</span>
	<span class="cm">/**
	 * The URI for which the client knows a
	 * result id.
	 */</span>
	<span class="nl">uri</span><span class="p">:</span> <span class="nx">DocumentUri</span><span class="p">;</span>

	<span class="cm">/**
	 * The value of the previous result id.
	 */</span>
	<span class="nl">value</span><span class="p">:</span> <span class="kr">string</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Response</em>:</p>
<ul>
  <li>result: <code class="language-plaintext highlighter-rouge">WorkspaceDiagnosticReport</code> defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#workspaceDiagnosticReport" name="workspaceDiagnosticReport" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * A workspace diagnostic report.
 *
 * @since 3.17.0
 */</span>
<span class="k">export</span> <span class="kr">interface</span> <span class="nx">WorkspaceDiagnosticReport</span> <span class="p">{</span>
	<span class="nl">items</span><span class="p">:</span> <span class="nx">WorkspaceDocumentDiagnosticReport</span><span class="p">[];</span>
<span class="p">}</span>
</code></pre></div></div>

<div class="anchorHolder"><a href="#workspaceFullDocumentDiagnosticReport" name="workspaceFullDocumentDiagnosticReport" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * A full document diagnostic report for a workspace diagnostic result.
 *
 * @since 3.17.0
 */</span>
<span class="k">export</span> <span class="kr">interface</span> <span class="nx">WorkspaceFullDocumentDiagnosticReport</span> <span class="kd">extends</span>
	<span class="nx">FullDocumentDiagnosticReport</span> <span class="p">{</span>

	<span class="cm">/**
	 * The URI for which diagnostic information is reported.
	 */</span>
	<span class="nl">uri</span><span class="p">:</span> <span class="nx">DocumentUri</span><span class="p">;</span>

	<span class="cm">/**
	 * The version number for which the diagnostics are reported.
	 * If the document is not marked as open `null` can be provided.
	 */</span>
	<span class="nl">version</span><span class="p">:</span> <span class="nx">integer</span> <span class="o">|</span> <span class="kc">null</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<div class="anchorHolder"><a href="#workspaceUnchangedDocumentDiagnosticReport" name="workspaceUnchangedDocumentDiagnosticReport" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * An unchanged document diagnostic report for a workspace diagnostic result.
 *
 * @since 3.17.0
 */</span>
<span class="k">export</span> <span class="kr">interface</span> <span class="nx">WorkspaceUnchangedDocumentDiagnosticReport</span> <span class="kd">extends</span>
	<span class="nx">UnchangedDocumentDiagnosticReport</span> <span class="p">{</span>

	<span class="cm">/**
	 * The URI for which diagnostic information is reported.
	 */</span>
	<span class="nl">uri</span><span class="p">:</span> <span class="nx">DocumentUri</span><span class="p">;</span>

	<span class="cm">/**
	 * The version number for which the diagnostics are reported.
	 * If the document is not marked as open `null` can be provided.
	 */</span>
	<span class="nl">version</span><span class="p">:</span> <span class="nx">integer</span> <span class="o">|</span> <span class="kc">null</span><span class="p">;</span>
<span class="p">};</span>
</code></pre></div></div>

<div class="anchorHolder"><a href="#workspaceDocumentDiagnosticReport" name="workspaceDocumentDiagnosticReport" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * A workspace diagnostic document report.
 *
 * @since 3.17.0
 */</span>
<span class="k">export</span> <span class="kd">type</span> <span class="nx">WorkspaceDocumentDiagnosticReport</span> <span class="o">=</span>
	<span class="nx">WorkspaceFullDocumentDiagnosticReport</span>
	<span class="o">|</span> <span class="nx">WorkspaceUnchangedDocumentDiagnosticReport</span><span class="p">;</span>
</code></pre></div></div>

<ul>
  <li>partial result: The first literal send need to be a <code class="language-plaintext highlighter-rouge">WorkspaceDiagnosticReport</code> followed by n <code class="language-plaintext highlighter-rouge">WorkspaceDiagnosticReportPartialResult</code> literals defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#workspaceDiagnosticReportPartialResult" name="workspaceDiagnosticReportPartialResult" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * A partial result for a workspace diagnostic report.
 *
 * @since 3.17.0
 */</span>
<span class="k">export</span> <span class="kr">interface</span> <span class="nx">WorkspaceDiagnosticReportPartialResult</span> <span class="p">{</span>
	<span class="nl">items</span><span class="p">:</span> <span class="nx">WorkspaceDocumentDiagnosticReport</span><span class="p">[];</span>
<span class="p">}</span>
</code></pre></div></div>

<ul>
  <li>error: code and message set in case an exception happens during the diagnostic request. A server is also allowed to return and error with code <code class="language-plaintext highlighter-rouge">ServerCancelled</code> indicating that the server can’t compute the result right now. A server can return a <code class="language-plaintext highlighter-rouge">DiagnosticServerCancellationData</code> data to indicate whether the client should re-trigger the request. If no data is provided it defaults to <code class="language-plaintext highlighter-rouge">{ retriggerRequest: true }</code>:</li>
</ul>

<h5 id="diagnostics-refresharrow_right_hook"><a href="#diagnostic_refresh" name="diagnostic_refresh" class="anchor">Diagnostics Refresh(<img class="emoji" title=":arrow_right_hook:" alt=":arrow_right_hook:" src="https://github.githubassets.com/images/icons/emoji/unicode/21aa.png" height="20" width="20">)</a></h5>

<p>The <code class="language-plaintext highlighter-rouge">workspace/diagnostic/refresh</code> request is sent from the server to the client. Servers can use it to ask clients to refresh all needed document and workspace diagnostics. This is useful if a server detects a project wide configuration change which requires a re-calculation of all diagnostics.</p>

<p><em>Client Capability</em>:</p>

<ul>
  <li>property name (optional): <code class="language-plaintext highlighter-rouge">workspace.diagnostics</code>
</li>
  <li>property type: <code class="language-plaintext highlighter-rouge">DiagnosticWorkspaceClientCapabilities</code> defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#diagnosticWorkspaceClientCapabilities" name="diagnosticWorkspaceClientCapabilities" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * Workspace client capabilities specific to diagnostic pull requests.
 *
 * @since 3.17.0
 */</span>
<span class="k">export</span> <span class="kr">interface</span> <span class="nx">DiagnosticWorkspaceClientCapabilities</span> <span class="p">{</span>
	<span class="cm">/**
	 * Whether the client implementation supports a refresh request sent from
	 * the server to the client.
	 *
	 * Note that this event is global and will force the client to refresh all
	 * pulled diagnostics currently shown. It should be used with absolute care
	 * and is useful for situation where a server for example detects a project
	 * wide change that requires such a calculation.
	 */</span>
	<span class="nl">refreshSupport</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Request</em>:</p>
<ul>
  <li>method: <code class="language-plaintext highlighter-rouge">workspace/diagnostic/refresh</code>
</li>
  <li>params: none</li>
</ul>

<p><em>Response</em>:</p>

<ul>
  <li>result: void</li>
  <li>error: code and message set in case an exception happens during the ‘workspace/diagnostic/refresh’ request</li>
</ul>

<h5 id="implementation-considerations">Implementation Considerations</h5>

<p>Generally the language server specification doesn’t enforce any specific client implementation since those usually depend on how the client UI behaves. However since diagnostics can be provided on a document and workspace level here are some tips:</p>

<ul>
  <li>a client should pull actively for the document the users types in.</li>
  <li>if the server signals inter file dependencies a client should also pull for visible documents to ensure accurate diagnostics. However the pull should happen less frequently.</li>
  <li>if the server signals workspace pull support a client should also pull for workspace diagnostics. It is recommended for clients to implement partial result progress for the workspace pull to allow servers to keep the request open for a long time. If a server closes a workspace diagnostic pull request the client should re-trigger the request.</li>
</ul>

<h4 id="signature-help-request-leftwards_arrow_with_hook"><a href="#textDocument_signatureHelp" name="textDocument_signatureHelp" class="anchor">Signature Help Request (<img class="emoji" title=":leftwards_arrow_with_hook:" alt=":leftwards_arrow_with_hook:" src="https://github.githubassets.com/images/icons/emoji/unicode/21a9.png" height="20" width="20">)</a></h4>

<p>The signature help request is sent from the client to the server to request signature information at a given cursor position.</p>

<p><em>Client Capability</em>:</p>
<ul>
  <li>property name (optional): <code class="language-plaintext highlighter-rouge">textDocument.signatureHelp</code>
</li>
  <li>property type: <code class="language-plaintext highlighter-rouge">SignatureHelpClientCapabilities</code> defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#signatureHelpClientCapabilities" name="signatureHelpClientCapabilities" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">SignatureHelpClientCapabilities</span> <span class="p">{</span>
	<span class="cm">/**
	 * Whether signature help supports dynamic registration.
	 */</span>
	<span class="nl">dynamicRegistration</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>

	<span class="cm">/**
	 * The client supports the following `SignatureInformation`
	 * specific properties.
	 */</span>
	<span class="nl">signatureInformation</span><span class="p">?:</span> <span class="p">{</span>
		<span class="cm">/**
		 * Client supports the follow content formats for the documentation
		 * property. The order describes the preferred format of the client.
		 */</span>
		<span class="nx">documentationFormat</span><span class="p">?:</span> <span class="nx">MarkupKind</span><span class="p">[];</span>

		<span class="cm">/**
		 * Client capabilities specific to parameter information.
		 */</span>
		<span class="nl">parameterInformation</span><span class="p">?:</span> <span class="p">{</span>
			<span class="cm">/**
			 * The client supports processing label offsets instead of a
			 * simple label string.
			 *
			 * @since 3.14.0
			 */</span>
			<span class="nx">labelOffsetSupport</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>
		<span class="p">};</span>

		<span class="cm">/**
		 * The client supports the `activeParameter` property on
		 * `SignatureInformation` literal.
		 *
		 * @since 3.16.0
		 */</span>
		<span class="nl">activeParameterSupport</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>
	<span class="p">};</span>

	<span class="cm">/**
	 * The client supports to send additional context information for a
	 * `textDocument/signatureHelp` request. A client that opts into
	 * contextSupport will also support the `retriggerCharacters` on
	 * `SignatureHelpOptions`.
	 *
	 * @since 3.15.0
	 */</span>
	<span class="nl">contextSupport</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Server Capability</em>:</p>
<ul>
  <li>property name (optional): <code class="language-plaintext highlighter-rouge">signatureHelpProvider</code>
</li>
  <li>property type: <code class="language-plaintext highlighter-rouge">SignatureHelpOptions</code> defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#signatureHelpOptions" name="signatureHelpOptions" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">SignatureHelpOptions</span> <span class="kd">extends</span> <span class="nx">WorkDoneProgressOptions</span> <span class="p">{</span>
	<span class="cm">/**
	 * The characters that trigger signature help
	 * automatically.
	 */</span>
	<span class="nl">triggerCharacters</span><span class="p">?:</span> <span class="kr">string</span><span class="p">[];</span>

	<span class="cm">/**
	 * List of characters that re-trigger signature help.
	 *
	 * These trigger characters are only active when signature help is already
	 * showing. All trigger characters are also counted as re-trigger
	 * characters.
	 *
	 * @since 3.15.0
	 */</span>
	<span class="nl">retriggerCharacters</span><span class="p">?:</span> <span class="kr">string</span><span class="p">[];</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Registration Options</em>: <code class="language-plaintext highlighter-rouge">SignatureHelpRegistrationOptions</code> defined as follows:</p>

<div class="anchorHolder"><a href="#signatureHelpRegistrationOptions" name="signatureHelpRegistrationOptions" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">SignatureHelpRegistrationOptions</span>
	<span class="kd">extends</span> <span class="nx">TextDocumentRegistrationOptions</span><span class="p">,</span> <span class="nx">SignatureHelpOptions</span> <span class="p">{</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Request</em>:</p>
<ul>
  <li>method: <code class="language-plaintext highlighter-rouge">textDocument/signatureHelp</code>
</li>
  <li>params: <code class="language-plaintext highlighter-rouge">SignatureHelpParams</code> defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#signatureHelpParams" name="signatureHelpParams" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">SignatureHelpParams</span> <span class="kd">extends</span> <span class="nx">TextDocumentPositionParams</span><span class="p">,</span>
	<span class="nx">WorkDoneProgressParams</span> <span class="p">{</span>
	<span class="cm">/**
	 * The signature help context. This is only available if the client
	 * specifies to send this using the client capability
	 * `textDocument.signatureHelp.contextSupport === true`
	 *
	 * @since 3.15.0
	 */</span>
	<span class="nl">context</span><span class="p">?:</span> <span class="nx">SignatureHelpContext</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<div class="anchorHolder"><a href="#signatureHelpTriggerKind" name="signatureHelpTriggerKind" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * How a signature help was triggered.
 *
 * @since 3.15.0
 */</span>
<span class="k">export</span> <span class="k">namespace</span> <span class="nx">SignatureHelpTriggerKind</span> <span class="p">{</span>
	<span class="cm">/**
	 * Signature help was invoked manually by the user or by a command.
	 */</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">Invoked</span><span class="p">:</span> <span class="mi">1</span> <span class="o">=</span> <span class="mi">1</span><span class="p">;</span>
	<span class="cm">/**
	 * Signature help was triggered by a trigger character.
	 */</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">TriggerCharacter</span><span class="p">:</span> <span class="mi">2</span> <span class="o">=</span> <span class="mi">2</span><span class="p">;</span>
	<span class="cm">/**
	 * Signature help was triggered by the cursor moving or by the document
	 * content changing.
	 */</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">ContentChange</span><span class="p">:</span> <span class="mi">3</span> <span class="o">=</span> <span class="mi">3</span><span class="p">;</span>
<span class="p">}</span>
<span class="k">export</span> <span class="kd">type</span> <span class="nx">SignatureHelpTriggerKind</span> <span class="o">=</span> <span class="mi">1</span> <span class="o">|</span> <span class="mi">2</span> <span class="o">|</span> <span class="mi">3</span><span class="p">;</span>
</code></pre></div></div>

<div class="anchorHolder"><a href="#signatureHelpContext" name="signatureHelpContext" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * Additional information about the context in which a signature help request
 * was triggered.
 *
 * @since 3.15.0
 */</span>
<span class="k">export</span> <span class="kr">interface</span> <span class="nx">SignatureHelpContext</span> <span class="p">{</span>
	<span class="cm">/**
	 * Action that caused signature help to be triggered.
	 */</span>
	<span class="nl">triggerKind</span><span class="p">:</span> <span class="nx">SignatureHelpTriggerKind</span><span class="p">;</span>

	<span class="cm">/**
	 * Character that caused signature help to be triggered.
	 *
	 * This is undefined when triggerKind !==
	 * SignatureHelpTriggerKind.TriggerCharacter
	 */</span>
	<span class="nl">triggerCharacter</span><span class="p">?:</span> <span class="kr">string</span><span class="p">;</span>

	<span class="cm">/**
	 * `true` if signature help was already showing when it was triggered.
	 *
	 * Retriggers occur when the signature help is already active and can be
	 * caused by actions such as typing a trigger character, a cursor move, or
	 * document content changes.
	 */</span>
	<span class="nl">isRetrigger</span><span class="p">:</span> <span class="nx">boolean</span><span class="p">;</span>

	<span class="cm">/**
	 * The currently active `SignatureHelp`.
	 *
	 * The `activeSignatureHelp` has its `SignatureHelp.activeSignature` field
	 * updated based on the user navigating through available signatures.
	 */</span>
	<span class="nl">activeSignatureHelp</span><span class="p">?:</span> <span class="nx">SignatureHelp</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Response</em>:</p>
<ul>
  <li>result: <code class="language-plaintext highlighter-rouge">SignatureHelp</code> | <code class="language-plaintext highlighter-rouge">null</code> defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#signatureHelp" name="signatureHelp" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * Signature help represents the signature of something
 * callable. There can be multiple signature but only one
 * active and only one active parameter.
 */</span>
<span class="k">export</span> <span class="kr">interface</span> <span class="nx">SignatureHelp</span> <span class="p">{</span>
	<span class="cm">/**
	 * One or more signatures. If no signatures are available the signature help
	 * request should return `null`.
	 */</span>
	<span class="nl">signatures</span><span class="p">:</span> <span class="nx">SignatureInformation</span><span class="p">[];</span>

	<span class="cm">/**
	 * The active signature. If omitted or the value lies outside the
	 * range of `signatures` the value defaults to zero or is ignore if
	 * the `SignatureHelp` as no signatures.
	 *
	 * Whenever possible implementors should make an active decision about
	 * the active signature and shouldn't rely on a default value.
	 *
	 * In future version of the protocol this property might become
	 * mandatory to better express this.
	 */</span>
	<span class="nl">activeSignature</span><span class="p">?:</span> <span class="nx">uinteger</span><span class="p">;</span>

	<span class="cm">/**
	 * The active parameter of the active signature. If omitted or the value
	 * lies outside the range of `signatures[activeSignature].parameters`
	 * defaults to 0 if the active signature has parameters. If
	 * the active signature has no parameters it is ignored.
	 * In future version of the protocol this property might become
	 * mandatory to better express the active parameter if the
	 * active signature does have any.
	 */</span>
	<span class="nl">activeParameter</span><span class="p">?:</span> <span class="nx">uinteger</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<div class="anchorHolder"><a href="#signatureInformation" name="signatureInformation" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * Represents the signature of something callable. A signature
 * can have a label, like a function-name, a doc-comment, and
 * a set of parameters.
 */</span>
<span class="k">export</span> <span class="kr">interface</span> <span class="nx">SignatureInformation</span> <span class="p">{</span>
	<span class="cm">/**
	 * The label of this signature. Will be shown in
	 * the UI.
	 */</span>
	<span class="nl">label</span><span class="p">:</span> <span class="kr">string</span><span class="p">;</span>

	<span class="cm">/**
	 * The human-readable doc-comment of this signature. Will be shown
	 * in the UI but can be omitted.
	 */</span>
	<span class="nl">documentation</span><span class="p">?:</span> <span class="kr">string</span> <span class="o">|</span> <span class="nx">MarkupContent</span><span class="p">;</span>

	<span class="cm">/**
	 * The parameters of this signature.
	 */</span>
	<span class="nl">parameters</span><span class="p">?:</span> <span class="nx">ParameterInformation</span><span class="p">[];</span>

	<span class="cm">/**
	 * The index of the active parameter.
	 *
	 * If provided, this is used in place of `SignatureHelp.activeParameter`.
	 *
	 * @since 3.16.0
	 */</span>
	<span class="nl">activeParameter</span><span class="p">?:</span> <span class="nx">uinteger</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<div class="anchorHolder"><a href="#parameterInformation" name="parameterInformation" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * Represents a parameter of a callable-signature. A parameter can
 * have a label and a doc-comment.
 */</span>
<span class="k">export</span> <span class="kr">interface</span> <span class="nx">ParameterInformation</span> <span class="p">{</span>

	<span class="cm">/**
	 * The label of this parameter information.
	 *
	 * Either a string or an inclusive start and exclusive end offsets within
	 * its containing signature label. (see SignatureInformation.label). The
	 * offsets are based on a UTF-16 string representation as `Position` and
	 * `Range` does.
	 *
	 * *Note*: a label of type string should be a substring of its containing
	 * signature label. Its intended use case is to highlight the parameter
	 * label part in the `SignatureInformation.label`.
	 */</span>
	<span class="nl">label</span><span class="p">:</span> <span class="kr">string</span> <span class="o">|</span> <span class="p">[</span><span class="nx">uinteger</span><span class="p">,</span> <span class="nx">uinteger</span><span class="p">];</span>

	<span class="cm">/**
	 * The human-readable doc-comment of this parameter. Will be shown
	 * in the UI but can be omitted.
	 */</span>
	<span class="nl">documentation</span><span class="p">?:</span> <span class="kr">string</span> <span class="o">|</span> <span class="nx">MarkupContent</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<ul>
  <li>error: code and message set in case an exception happens during the signature help request.</li>
</ul>

<h4 id="code-action-request-leftwards_arrow_with_hook"><a href="#textDocument_codeAction" name="textDocument_codeAction" class="anchor">Code Action Request (<img class="emoji" title=":leftwards_arrow_with_hook:" alt=":leftwards_arrow_with_hook:" src="https://github.githubassets.com/images/icons/emoji/unicode/21a9.png" height="20" width="20">)</a></h4>

<p>The code action request is sent from the client to the server to compute commands for a given text document and range. These commands are typically code fixes to either fix problems or to beautify/refactor code. The result of a <code class="language-plaintext highlighter-rouge">textDocument/codeAction</code> request is an array of <code class="language-plaintext highlighter-rouge">Command</code> literals which are typically presented in the user interface. To ensure that a server is useful in many clients the commands specified in a code actions should be handled by the server and not by the client (see <code class="language-plaintext highlighter-rouge">workspace/executeCommand</code> and <code class="language-plaintext highlighter-rouge">ServerCapabilities.executeCommandProvider</code>). If the client supports providing edits with a code action then that mode should be used.</p>

<p><em>Since version 3.16.0:</em> a client can offer a server to delay the computation of code action properties during a ‘textDocument/codeAction’ request:</p>

<p>This is useful for cases where it is expensive to compute the value of a property (for example the <code class="language-plaintext highlighter-rouge">edit</code> property). Clients signal this through the <code class="language-plaintext highlighter-rouge">codeAction.resolveSupport</code> capability which lists all properties a client can resolve lazily. The server capability <code class="language-plaintext highlighter-rouge">codeActionProvider.resolveProvider</code> signals that a server will offer a <code class="language-plaintext highlighter-rouge">codeAction/resolve</code> route. To help servers to uniquely identify a code action in the resolve request, a code action literal can optional carry a data property. This is also guarded by an additional client capability <code class="language-plaintext highlighter-rouge">codeAction.dataSupport</code>. In general, a client should offer data support if it offers resolve support. It should also be noted that servers shouldn’t alter existing attributes of a code action in a codeAction/resolve request.</p>

<blockquote>
  <p><em>Since version 3.8.0:</em> support for CodeAction literals to enable the following scenarios:</p>
</blockquote>

<ul>
  <li>the ability to directly return a workspace edit from the code action request. This avoids having another server roundtrip to execute an actual code action. However server providers should be aware that if the code action is expensive to compute or the edits are huge it might still be beneficial if the result is simply a command and the actual edit is only computed when needed.</li>
  <li>the ability to group code actions using a kind. Clients are allowed to ignore that information. However it allows them to better group code action for example into corresponding menus (e.g. all refactor code actions into a refactor menu).</li>
</ul>

<p>Clients need to announce their support for code action literals (e.g. literals of type <code class="language-plaintext highlighter-rouge">CodeAction</code>) and code action kinds via the corresponding client capability <code class="language-plaintext highlighter-rouge">codeAction.codeActionLiteralSupport</code>.</p>

<p><em>Client Capability</em>:</p>
<ul>
  <li>property name (optional): <code class="language-plaintext highlighter-rouge">textDocument.codeAction</code>
</li>
  <li>property type: <code class="language-plaintext highlighter-rouge">CodeActionClientCapabilities</code> defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#codeActionClientCapabilities" name="codeActionClientCapabilities" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">CodeActionClientCapabilities</span> <span class="p">{</span>
	<span class="cm">/**
	 * Whether code action supports dynamic registration.
	 */</span>
	<span class="nl">dynamicRegistration</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>

	<span class="cm">/**
	 * The client supports code action literals as a valid
	 * response of the `textDocument/codeAction` request.
	 *
	 * @since 3.8.0
	 */</span>
	<span class="nl">codeActionLiteralSupport</span><span class="p">?:</span> <span class="p">{</span>
		<span class="cm">/**
		 * The code action kind is supported with the following value
		 * set.
		 */</span>
		<span class="na">codeActionKind</span><span class="p">:</span> <span class="p">{</span>

			<span class="cm">/**
			 * The code action kind values the client supports. When this
			 * property exists the client also guarantees that it will
			 * handle values outside its set gracefully and falls back
			 * to a default value when unknown.
			 */</span>
			<span class="na">valueSet</span><span class="p">:</span> <span class="nx">CodeActionKind</span><span class="p">[];</span>
		<span class="p">};</span>
	<span class="p">};</span>

	<span class="cm">/**
	 * Whether code action supports the `isPreferred` property.
	 *
	 * @since 3.15.0
	 */</span>
	<span class="nl">isPreferredSupport</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>

	<span class="cm">/**
	 * Whether code action supports the `disabled` property.
	 *
	 * @since 3.16.0
	 */</span>
	<span class="nl">disabledSupport</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>

	<span class="cm">/**
	 * Whether code action supports the `data` property which is
	 * preserved between a `textDocument/codeAction` and a
	 * `codeAction/resolve` request.
	 *
	 * @since 3.16.0
	 */</span>
	<span class="nl">dataSupport</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>


	<span class="cm">/**
	 * Whether the client supports resolving additional code action
	 * properties via a separate `codeAction/resolve` request.
	 *
	 * @since 3.16.0
	 */</span>
	<span class="nl">resolveSupport</span><span class="p">?:</span> <span class="p">{</span>
		<span class="cm">/**
		 * The properties that a client can resolve lazily.
		 */</span>
		<span class="na">properties</span><span class="p">:</span> <span class="kr">string</span><span class="p">[];</span>
	<span class="p">};</span>

	<span class="cm">/**
	 * Whether the client honors the change annotations in
	 * text edits and resource operations returned via the
	 * `CodeAction#edit` property by for example presenting
	 * the workspace edit in the user interface and asking
	 * for confirmation.
	 *
	 * @since 3.16.0
	 */</span>
	<span class="nl">honorsChangeAnnotations</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Server Capability</em>:</p>
<ul>
  <li>property name (optional): <code class="language-plaintext highlighter-rouge">codeActionProvider</code>
</li>
  <li>property type: <code class="language-plaintext highlighter-rouge">boolean | CodeActionOptions</code> where <code class="language-plaintext highlighter-rouge">CodeActionOptions</code> is defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#codeActionOptions" name="codeActionOptions" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">CodeActionOptions</span> <span class="kd">extends</span> <span class="nx">WorkDoneProgressOptions</span> <span class="p">{</span>
	<span class="cm">/**
	 * CodeActionKinds that this server may return.
	 *
	 * The list of kinds may be generic, such as `CodeActionKind.Refactor`,
	 * or the server may list out every specific kind they provide.
	 */</span>
	<span class="nl">codeActionKinds</span><span class="p">?:</span> <span class="nx">CodeActionKind</span><span class="p">[];</span>

	<span class="cm">/**
	 * The server provides support to resolve additional
	 * information for a code action.
	 *
	 * @since 3.16.0
	 */</span>
	<span class="nl">resolveProvider</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Registration Options</em>: <code class="language-plaintext highlighter-rouge">CodeActionRegistrationOptions</code> defined as follows:</p>

<div class="anchorHolder"><a href="#codeActionRegistrationOptions" name="codeActionRegistrationOptions" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">CodeActionRegistrationOptions</span> <span class="kd">extends</span>
	<span class="nx">TextDocumentRegistrationOptions</span><span class="p">,</span> <span class="nx">CodeActionOptions</span> <span class="p">{</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Request</em>:</p>
<ul>
  <li>method: <code class="language-plaintext highlighter-rouge">textDocument/codeAction</code>
</li>
  <li>params: <code class="language-plaintext highlighter-rouge">CodeActionParams</code> defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#codeActionParams" name="codeActionParams" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * Params for the CodeActionRequest
 */</span>
<span class="k">export</span> <span class="kr">interface</span> <span class="nx">CodeActionParams</span> <span class="kd">extends</span> <span class="nx">WorkDoneProgressParams</span><span class="p">,</span>
	<span class="nx">PartialResultParams</span> <span class="p">{</span>
	<span class="cm">/**
	 * The document in which the command was invoked.
	 */</span>
	<span class="nl">textDocument</span><span class="p">:</span> <span class="nx">TextDocumentIdentifier</span><span class="p">;</span>

	<span class="cm">/**
	 * The range for which the command was invoked.
	 */</span>
	<span class="nl">range</span><span class="p">:</span> <span class="nx">Range</span><span class="p">;</span>

	<span class="cm">/**
	 * Context carrying additional information.
	 */</span>
	<span class="nl">context</span><span class="p">:</span> <span class="nx">CodeActionContext</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<div class="anchorHolder"><a href="#codeActionKind" name="codeActionKind" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * The kind of a code action.
 *
 * Kinds are a hierarchical list of identifiers separated by `.`,
 * e.g. `"refactor.extract.function"`.
 *
 * The set of kinds is open and client needs to announce the kinds it supports
 * to the server during initialization.
 */</span>
<span class="k">export</span> <span class="kd">type</span> <span class="nx">CodeActionKind</span> <span class="o">=</span> <span class="kr">string</span><span class="p">;</span>

<span class="cm">/**
 * A set of predefined code action kinds.
 */</span>
<span class="k">export</span> <span class="k">namespace</span> <span class="nx">CodeActionKind</span> <span class="p">{</span>

	<span class="cm">/**
	 * Empty kind.
	 */</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">Empty</span><span class="p">:</span> <span class="nx">CodeActionKind</span> <span class="o">=</span> <span class="dl">''</span><span class="p">;</span>

	<span class="cm">/**
	 * Base kind for quickfix actions: 'quickfix'.
	 */</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">QuickFix</span><span class="p">:</span> <span class="nx">CodeActionKind</span> <span class="o">=</span> <span class="dl">'</span><span class="s1">quickfix</span><span class="dl">'</span><span class="p">;</span>

	<span class="cm">/**
	 * Base kind for refactoring actions: 'refactor'.
	 */</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">Refactor</span><span class="p">:</span> <span class="nx">CodeActionKind</span> <span class="o">=</span> <span class="dl">'</span><span class="s1">refactor</span><span class="dl">'</span><span class="p">;</span>

	<span class="cm">/**
	 * Base kind for refactoring extraction actions: 'refactor.extract'.
	 *
	 * Example extract actions:
	 *
	 * - Extract method
	 * - Extract function
	 * - Extract variable
	 * - Extract interface from class
	 * - ...
	 */</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">RefactorExtract</span><span class="p">:</span> <span class="nx">CodeActionKind</span> <span class="o">=</span> <span class="dl">'</span><span class="s1">refactor.extract</span><span class="dl">'</span><span class="p">;</span>

	<span class="cm">/**
	 * Base kind for refactoring inline actions: 'refactor.inline'.
	 *
	 * Example inline actions:
	 *
	 * - Inline function
	 * - Inline variable
	 * - Inline constant
	 * - ...
	 */</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">RefactorInline</span><span class="p">:</span> <span class="nx">CodeActionKind</span> <span class="o">=</span> <span class="dl">'</span><span class="s1">refactor.inline</span><span class="dl">'</span><span class="p">;</span>

	<span class="cm">/**
	 * Base kind for refactoring rewrite actions: 'refactor.rewrite'.
	 *
	 * Example rewrite actions:
	 *
	 * - Convert JavaScript function to class
	 * - Add or remove parameter
	 * - Encapsulate field
	 * - Make method static
	 * - Move method to base class
	 * - ...
	 */</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">RefactorRewrite</span><span class="p">:</span> <span class="nx">CodeActionKind</span> <span class="o">=</span> <span class="dl">'</span><span class="s1">refactor.rewrite</span><span class="dl">'</span><span class="p">;</span>

	<span class="cm">/**
	 * Base kind for source actions: `source`.
	 *
	 * Source code actions apply to the entire file.
	 */</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">Source</span><span class="p">:</span> <span class="nx">CodeActionKind</span> <span class="o">=</span> <span class="dl">'</span><span class="s1">source</span><span class="dl">'</span><span class="p">;</span>

	<span class="cm">/**
	 * Base kind for an organize imports source action:
	 * `source.organizeImports`.
	 */</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">SourceOrganizeImports</span><span class="p">:</span> <span class="nx">CodeActionKind</span> <span class="o">=</span>
		<span class="dl">'</span><span class="s1">source.organizeImports</span><span class="dl">'</span><span class="p">;</span>

	<span class="cm">/**
	 * Base kind for a 'fix all' source action: `source.fixAll`.
	 *
	 * 'Fix all' actions automatically fix errors that have a clear fix that
	 * do not require user input. They should not suppress errors or perform
	 * unsafe fixes such as generating new types or classes.
	 *
	 * @since 3.17.0
	 */</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">SourceFixAll</span><span class="p">:</span> <span class="nx">CodeActionKind</span> <span class="o">=</span> <span class="dl">'</span><span class="s1">source.fixAll</span><span class="dl">'</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<div class="anchorHolder"><a href="#codeActionContext" name="codeActionContext" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * Contains additional diagnostic information about the context in which
 * a code action is run.
 */</span>
<span class="k">export</span> <span class="kr">interface</span> <span class="nx">CodeActionContext</span> <span class="p">{</span>
	<span class="cm">/**
	 * An array of diagnostics known on the client side overlapping the range
	 * provided to the `textDocument/codeAction` request. They are provided so
	 * that the server knows which errors are currently presented to the user
	 * for the given range. There is no guarantee that these accurately reflect
	 * the error state of the resource. The primary parameter
	 * to compute code actions is the provided range.
	 */</span>
	<span class="nl">diagnostics</span><span class="p">:</span> <span class="nx">Diagnostic</span><span class="p">[];</span>

	<span class="cm">/**
	 * Requested kind of actions to return.
	 *
	 * Actions not of this kind are filtered out by the client before being
	 * shown. So servers can omit computing them.
	 */</span>
	<span class="nl">only</span><span class="p">?:</span> <span class="nx">CodeActionKind</span><span class="p">[];</span>

	<span class="cm">/**
	 * The reason why code actions were requested.
	 *
	 * @since 3.17.0
	 */</span>
	<span class="nl">triggerKind</span><span class="p">?:</span> <span class="nx">CodeActionTriggerKind</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<div class="anchorHolder"><a href="#codeActionTriggerKind" name="codeActionTriggerKind" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * The reason why code actions were requested.
 *
 * @since 3.17.0
 */</span>
<span class="k">export</span> <span class="k">namespace</span> <span class="nx">CodeActionTriggerKind</span> <span class="p">{</span>
	<span class="cm">/**
	 * Code actions were explicitly requested by the user or by an extension.
	 */</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">Invoked</span><span class="p">:</span> <span class="mi">1</span> <span class="o">=</span> <span class="mi">1</span><span class="p">;</span>

	<span class="cm">/**
	 * Code actions were requested automatically.
	 *
	 * This typically happens when current selection in a file changes, but can
	 * also be triggered when file content changes.
	 */</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">Automatic</span><span class="p">:</span> <span class="mi">2</span> <span class="o">=</span> <span class="mi">2</span><span class="p">;</span>
<span class="p">}</span>

<span class="k">export</span> <span class="kd">type</span> <span class="nx">CodeActionTriggerKind</span> <span class="o">=</span> <span class="mi">1</span> <span class="o">|</span> <span class="mi">2</span><span class="p">;</span>
</code></pre></div></div>

<p><em>Response</em>:</p>
<ul>
  <li>result: <code class="language-plaintext highlighter-rouge">(Command | CodeAction)[]</code> | <code class="language-plaintext highlighter-rouge">null</code> where <code class="language-plaintext highlighter-rouge">CodeAction</code> is defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#codeAction" name="codeAction" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * A code action represents a change that can be performed in code, e.g. to fix
 * a problem or to refactor code.
 *
 * A CodeAction must set either `edit` and/or a `command`. If both are supplied,
 * the `edit` is applied first, then the `command` is executed.
 */</span>
<span class="k">export</span> <span class="kr">interface</span> <span class="nx">CodeAction</span> <span class="p">{</span>

	<span class="cm">/**
	 * A short, human-readable, title for this code action.
	 */</span>
	<span class="nl">title</span><span class="p">:</span> <span class="kr">string</span><span class="p">;</span>

	<span class="cm">/**
	 * The kind of the code action.
	 *
	 * Used to filter code actions.
	 */</span>
	<span class="nl">kind</span><span class="p">?:</span> <span class="nx">CodeActionKind</span><span class="p">;</span>

	<span class="cm">/**
	 * The diagnostics that this code action resolves.
	 */</span>
	<span class="nl">diagnostics</span><span class="p">?:</span> <span class="nx">Diagnostic</span><span class="p">[];</span>

	<span class="cm">/**
	 * Marks this as a preferred action. Preferred actions are used by the
	 * `auto fix` command and can be targeted by keybindings.
	 *
	 * A quick fix should be marked preferred if it properly addresses the
	 * underlying error. A refactoring should be marked preferred if it is the
	 * most reasonable choice of actions to take.
	 *
	 * @since 3.15.0
	 */</span>
	<span class="nl">isPreferred</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>

	<span class="cm">/**
	 * Marks that the code action cannot currently be applied.
	 *
	 * Clients should follow the following guidelines regarding disabled code
	 * actions:
	 *
	 * - Disabled code actions are not shown in automatic lightbulbs code
	 *   action menus.
	 *
	 * - Disabled actions are shown as faded out in the code action menu when
	 *   the user request a more specific type of code action, such as
	 *   refactorings.
	 *
	 * - If the user has a keybinding that auto applies a code action and only
	 *   a disabled code actions are returned, the client should show the user
	 *   an error message with `reason` in the editor.
	 *
	 * @since 3.16.0
	 */</span>
	<span class="nl">disabled</span><span class="p">?:</span> <span class="p">{</span>

		<span class="cm">/**
		 * Human readable description of why the code action is currently
		 * disabled.
		 *
		 * This is displayed in the code actions UI.
		 */</span>
		<span class="na">reason</span><span class="p">:</span> <span class="kr">string</span><span class="p">;</span>
	<span class="p">};</span>

	<span class="cm">/**
	 * The workspace edit this code action performs.
	 */</span>
	<span class="nl">edit</span><span class="p">?:</span> <span class="nx">WorkspaceEdit</span><span class="p">;</span>

	<span class="cm">/**
	 * A command this code action executes. If a code action
	 * provides an edit and a command, first the edit is
	 * executed and then the command.
	 */</span>
	<span class="nl">command</span><span class="p">?:</span> <span class="nx">Command</span><span class="p">;</span>

	<span class="cm">/**
	 * A data entry field that is preserved on a code action between
	 * a `textDocument/codeAction` and a `codeAction/resolve` request.
	 *
	 * @since 3.16.0
	 */</span>
	<span class="nl">data</span><span class="p">?:</span> <span class="nx">LSPAny</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>
<ul>
  <li>partial result: <code class="language-plaintext highlighter-rouge">(Command | CodeAction)[]</code>
</li>
  <li>error: code and message set in case an exception happens during the code action request.</li>
</ul>

<h4 id="code-action-resolve-request-leftwards_arrow_with_hook"><a href="#codeAction_resolve" name="codeAction_resolve" class="anchor">Code Action Resolve Request (<img class="emoji" title=":leftwards_arrow_with_hook:" alt=":leftwards_arrow_with_hook:" src="https://github.githubassets.com/images/icons/emoji/unicode/21a9.png" height="20" width="20">)</a></h4>

<blockquote>
  <p><em>Since version 3.16.0</em></p>
</blockquote>

<p>The request is sent from the client to the server to resolve additional information for a given code action. This is usually used to compute
the <code class="language-plaintext highlighter-rouge">edit</code> property of a code action to avoid its unnecessary computation during the <code class="language-plaintext highlighter-rouge">textDocument/codeAction</code> request.</p>

<p>Consider the clients announces the <code class="language-plaintext highlighter-rouge">edit</code> property as a property that can be resolved lazy using the client capability</p>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="nx">textDocument</span><span class="p">.</span><span class="nx">codeAction</span><span class="p">.</span><span class="nx">resolveSupport</span> <span class="o">=</span> <span class="p">{</span> <span class="na">properties</span><span class="p">:</span> <span class="p">[</span><span class="dl">'</span><span class="s1">edit</span><span class="dl">'</span><span class="p">]</span> <span class="p">};</span>
</code></pre></div></div>

<p>then a code action</p>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="p">{</span>
    <span class="dl">"</span><span class="s2">title</span><span class="dl">"</span><span class="p">:</span> <span class="dl">"</span><span class="s2">Do Foo</span><span class="dl">"</span>
<span class="p">}</span>
</code></pre></div></div>

<p>needs to be resolved using the <code class="language-plaintext highlighter-rouge">codeAction/resolve</code> request before it can be applied.</p>

<p><em>Client Capability</em>:</p>
<ul>
  <li>property name (optional): <code class="language-plaintext highlighter-rouge">textDocument.codeAction.resolveSupport</code>
</li>
  <li>property type: <code class="language-plaintext highlighter-rouge">{ properties: string[]; }</code>
</li>
</ul>

<p><em>Request</em>:</p>
<ul>
  <li>method: <code class="language-plaintext highlighter-rouge">codeAction/resolve</code>
</li>
  <li>params: <code class="language-plaintext highlighter-rouge">CodeAction</code>
</li>
</ul>

<p><em>Response</em>:</p>
<ul>
  <li>result: <code class="language-plaintext highlighter-rouge">CodeAction</code>
</li>
  <li>error: code and message set in case an exception happens during the code action resolve request.</li>
</ul>

<h4 id="document-color-request-leftwards_arrow_with_hook"><a href="#textDocument_documentColor" name="textDocument_documentColor" class="anchor">Document Color Request (<img class="emoji" title=":leftwards_arrow_with_hook:" alt=":leftwards_arrow_with_hook:" src="https://github.githubassets.com/images/icons/emoji/unicode/21a9.png" height="20" width="20">)</a></h4>

<blockquote>
  <p><em>Since version 3.6.0</em></p>
</blockquote>

<p>The document color request is sent from the client to the server to list all color references found in a given text document. Along with the range, a color value in RGB is returned.</p>

<p>Clients can use the result to decorate color references in an editor. For example:</p>
<ul>
  <li>Color boxes showing the actual color next to the reference</li>
  <li>Show a color picker when a color reference is edited</li>
</ul>

<p><em>Client Capability</em>:</p>
<ul>
  <li>property name (optional): <code class="language-plaintext highlighter-rouge">textDocument.colorProvider</code>
</li>
  <li>property type: <code class="language-plaintext highlighter-rouge">DocumentColorClientCapabilities</code> defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#documentColorClientCapabilities" name="documentColorClientCapabilities" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">DocumentColorClientCapabilities</span> <span class="p">{</span>
	<span class="cm">/**
	 * Whether document color supports dynamic registration.
	 */</span>
	<span class="nl">dynamicRegistration</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Server Capability</em>:</p>
<ul>
  <li>property name (optional): <code class="language-plaintext highlighter-rouge">colorProvider</code>
</li>
  <li>property type: <code class="language-plaintext highlighter-rouge">boolean | DocumentColorOptions | DocumentColorRegistrationOptions</code> where <code class="language-plaintext highlighter-rouge">DocumentColorOptions</code> is defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#documentColorOptions" name="documentColorOptions" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">DocumentColorOptions</span> <span class="kd">extends</span> <span class="nx">WorkDoneProgressOptions</span> <span class="p">{</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Registration Options</em>: <code class="language-plaintext highlighter-rouge">DocumentColorRegistrationOptions</code> defined as follows:</p>

<div class="anchorHolder"><a href="#documentColorRegistrationOptions" name="documentColorRegistrationOptions" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">DocumentColorRegistrationOptions</span> <span class="kd">extends</span>
	<span class="nx">TextDocumentRegistrationOptions</span><span class="p">,</span> <span class="nx">StaticRegistrationOptions</span><span class="p">,</span>
	<span class="nx">DocumentColorOptions</span> <span class="p">{</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Request</em>:</p>

<ul>
  <li>method: <code class="language-plaintext highlighter-rouge">textDocument/documentColor</code>
</li>
  <li>params: <code class="language-plaintext highlighter-rouge">DocumentColorParams</code> defined as follows</li>
</ul>

<div class="anchorHolder"><a href="#documentColorParams" name="documentColorParams" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="kr">interface</span> <span class="nx">DocumentColorParams</span> <span class="kd">extends</span> <span class="nx">WorkDoneProgressParams</span><span class="p">,</span>
	<span class="nx">PartialResultParams</span> <span class="p">{</span>
	<span class="cm">/**
	 * The text document.
	 */</span>
	<span class="nl">textDocument</span><span class="p">:</span> <span class="nx">TextDocumentIdentifier</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Response</em>:</p>
<ul>
  <li>result: <code class="language-plaintext highlighter-rouge">ColorInformation[]</code> defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#colorInformation" name="colorInformation" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="kr">interface</span> <span class="nx">ColorInformation</span> <span class="p">{</span>
	<span class="cm">/**
	 * The range in the document where this color appears.
	 */</span>
	<span class="nl">range</span><span class="p">:</span> <span class="nx">Range</span><span class="p">;</span>

	<span class="cm">/**
	 * The actual color value for this color range.
	 */</span>
	<span class="nl">color</span><span class="p">:</span> <span class="nx">Color</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<div class="anchorHolder"><a href="#color" name="color" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * Represents a color in RGBA space.
 */</span>
<span class="kr">interface</span> <span class="nx">Color</span> <span class="p">{</span>

	<span class="cm">/**
	 * The red component of this color in the range [0-1].
	 */</span>
	<span class="k">readonly</span> <span class="nx">red</span><span class="p">:</span> <span class="nx">decimal</span><span class="p">;</span>

	<span class="cm">/**
	 * The green component of this color in the range [0-1].
	 */</span>
	<span class="k">readonly</span> <span class="nx">green</span><span class="p">:</span> <span class="nx">decimal</span><span class="p">;</span>

	<span class="cm">/**
	 * The blue component of this color in the range [0-1].
	 */</span>
	<span class="k">readonly</span> <span class="nx">blue</span><span class="p">:</span> <span class="nx">decimal</span><span class="p">;</span>

	<span class="cm">/**
	 * The alpha component of this color in the range [0-1].
	 */</span>
	<span class="k">readonly</span> <span class="nx">alpha</span><span class="p">:</span> <span class="nx">decimal</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>
<ul>
  <li>partial result: <code class="language-plaintext highlighter-rouge">ColorInformation[]</code>
</li>
  <li>error: code and message set in case an exception happens during the ‘textDocument/documentColor’ request</li>
</ul>

<h4 id="color-presentation-request-leftwards_arrow_with_hook"><a href="#textDocument_colorPresentation" name="textDocument_colorPresentation" class="anchor">Color Presentation Request (<img class="emoji" title=":leftwards_arrow_with_hook:" alt=":leftwards_arrow_with_hook:" src="https://github.githubassets.com/images/icons/emoji/unicode/21a9.png" height="20" width="20">)</a></h4>

<blockquote>
  <p><em>Since version 3.6.0</em></p>
</blockquote>

<p>The color presentation request is sent from the client to the server to obtain a list of presentations for a color value at a given location. Clients can use the result to</p>
<ul>
  <li>modify a color reference.</li>
  <li>show in a color picker and let users pick one of the presentations</li>
</ul>

<p>This request has no special capabilities and registration options since it is send as a resolve request for the <code class="language-plaintext highlighter-rouge">textDocument/documentColor</code> request.</p>

<p><em>Request</em>:</p>

<ul>
  <li>method: <code class="language-plaintext highlighter-rouge">textDocument/colorPresentation</code>
</li>
  <li>params: <code class="language-plaintext highlighter-rouge">ColorPresentationParams</code> defined as follows</li>
</ul>

<div class="anchorHolder"><a href="#colorPresentationParams" name="colorPresentationParams" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="kr">interface</span> <span class="nx">ColorPresentationParams</span> <span class="kd">extends</span> <span class="nx">WorkDoneProgressParams</span><span class="p">,</span>
	<span class="nx">PartialResultParams</span> <span class="p">{</span>
	<span class="cm">/**
	 * The text document.
	 */</span>
	<span class="nl">textDocument</span><span class="p">:</span> <span class="nx">TextDocumentIdentifier</span><span class="p">;</span>

	<span class="cm">/**
	 * The color information to request presentations for.
	 */</span>
	<span class="nl">color</span><span class="p">:</span> <span class="nx">Color</span><span class="p">;</span>

	<span class="cm">/**
	 * The range where the color would be inserted. Serves as a context.
	 */</span>
	<span class="nl">range</span><span class="p">:</span> <span class="nx">Range</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Response</em>:</p>
<ul>
  <li>result: <code class="language-plaintext highlighter-rouge">ColorPresentation[]</code> defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#colorPresentation" name="colorPresentation" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="kr">interface</span> <span class="nx">ColorPresentation</span> <span class="p">{</span>
	<span class="cm">/**
	 * The label of this color presentation. It will be shown on the color
	 * picker header. By default this is also the text that is inserted when
	 * selecting this color presentation.
	 */</span>
	<span class="nl">label</span><span class="p">:</span> <span class="kr">string</span><span class="p">;</span>
	<span class="cm">/**
	 * An [edit](#TextEdit) which is applied to a document when selecting
	 * this presentation for the color. When omitted the
	 * [label](#ColorPresentation.label) is used.
	 */</span>
	<span class="nl">textEdit</span><span class="p">?:</span> <span class="nx">TextEdit</span><span class="p">;</span>
	<span class="cm">/**
	 * An optional array of additional [text edits](#TextEdit) that are applied
	 * when selecting this color presentation. Edits must not overlap with the
	 * main [edit](#ColorPresentation.textEdit) nor with themselves.
	 */</span>
	<span class="nl">additionalTextEdits</span><span class="p">?:</span> <span class="nx">TextEdit</span><span class="p">[];</span>
<span class="p">}</span>
</code></pre></div></div>

<ul>
  <li>partial result: <code class="language-plaintext highlighter-rouge">ColorPresentation[]</code>
</li>
  <li>error: code and message set in case an exception happens during the ‘textDocument/colorPresentation’ request</li>
</ul>

<h4 id="document-formatting-request--leftwards_arrow_with_hook"><a href="#textDocument_formatting" name="textDocument_formatting" class="anchor">Document Formatting Request  (<img class="emoji" title=":leftwards_arrow_with_hook:" alt=":leftwards_arrow_with_hook:" src="https://github.githubassets.com/images/icons/emoji/unicode/21a9.png" height="20" width="20">)</a></h4>

<p>The document formatting request is sent from the client to the server to format a whole document.</p>

<p><em>Client Capability</em>:</p>
<ul>
  <li>property name (optional): <code class="language-plaintext highlighter-rouge">textDocument.formatting</code>
</li>
  <li>property type: <code class="language-plaintext highlighter-rouge">DocumentFormattingClientCapabilities</code> defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#documentFormattingClientCapabilities" name="documentFormattingClientCapabilities" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">DocumentFormattingClientCapabilities</span> <span class="p">{</span>
	<span class="cm">/**
	 * Whether formatting supports dynamic registration.
	 */</span>
	<span class="nl">dynamicRegistration</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Server Capability</em>:</p>
<ul>
  <li>property name (optional): <code class="language-plaintext highlighter-rouge">documentFormattingProvider</code>
</li>
  <li>property type: <code class="language-plaintext highlighter-rouge">boolean | DocumentFormattingOptions</code> where <code class="language-plaintext highlighter-rouge">DocumentFormattingOptions</code> is defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#documentFormattingOptions" name="documentFormattingOptions" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">DocumentFormattingOptions</span> <span class="kd">extends</span> <span class="nx">WorkDoneProgressOptions</span> <span class="p">{</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Registration Options</em>: <code class="language-plaintext highlighter-rouge">DocumentFormattingRegistrationOptions</code> defined as follows:</p>

<div class="anchorHolder"><a href="#documentFormattingRegistrationOptions" name="documentFormattingRegistrationOptions" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">DocumentFormattingRegistrationOptions</span> <span class="kd">extends</span>
	<span class="nx">TextDocumentRegistrationOptions</span><span class="p">,</span> <span class="nx">DocumentFormattingOptions</span> <span class="p">{</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Request</em>:</p>
<ul>
  <li>method: <code class="language-plaintext highlighter-rouge">textDocument/formatting</code>
</li>
  <li>params: <code class="language-plaintext highlighter-rouge">DocumentFormattingParams</code> defined as follows</li>
</ul>

<div class="anchorHolder"><a href="#documentFormattingParams" name="documentFormattingParams" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="kr">interface</span> <span class="nx">DocumentFormattingParams</span> <span class="kd">extends</span> <span class="nx">WorkDoneProgressParams</span> <span class="p">{</span>
	<span class="cm">/**
	 * The document to format.
	 */</span>
	<span class="nl">textDocument</span><span class="p">:</span> <span class="nx">TextDocumentIdentifier</span><span class="p">;</span>

	<span class="cm">/**
	 * The format options.
	 */</span>
	<span class="nl">options</span><span class="p">:</span> <span class="nx">FormattingOptions</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<div class="anchorHolder"><a href="#formattingOptions" name="formattingOptions" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * Value-object describing what options formatting should use.
 */</span>
<span class="kr">interface</span> <span class="nx">FormattingOptions</span> <span class="p">{</span>
	<span class="cm">/**
	 * Size of a tab in spaces.
	 */</span>
	<span class="nl">tabSize</span><span class="p">:</span> <span class="nx">uinteger</span><span class="p">;</span>

	<span class="cm">/**
	 * Prefer spaces over tabs.
	 */</span>
	<span class="nl">insertSpaces</span><span class="p">:</span> <span class="nx">boolean</span><span class="p">;</span>

	<span class="cm">/**
	 * Trim trailing whitespace on a line.
	 *
	 * @since 3.15.0
	 */</span>
	<span class="nl">trimTrailingWhitespace</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>

	<span class="cm">/**
	 * Insert a newline character at the end of the file if one does not exist.
	 *
	 * @since 3.15.0
	 */</span>
	<span class="nl">insertFinalNewline</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>

	<span class="cm">/**
	 * Trim all newlines after the final newline at the end of the file.
	 *
	 * @since 3.15.0
	 */</span>
	<span class="nl">trimFinalNewlines</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>

	<span class="cm">/**
	 * Signature for further properties.
	 */</span>
	<span class="p">[</span><span class="nx">key</span><span class="p">:</span> <span class="kr">string</span><span class="p">]:</span> <span class="nx">boolean</span> <span class="o">|</span> <span class="nx">integer</span> <span class="o">|</span> <span class="kr">string</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Response</em>:</p>
<ul>
  <li>result: <a href="#textEdit"><code class="language-plaintext highlighter-rouge">TextEdit[]</code></a> | <code class="language-plaintext highlighter-rouge">null</code> describing the modification to the document to be formatted.</li>
  <li>error: code and message set in case an exception happens during the formatting request.</li>
</ul>

<h4 id="document-range-formatting-request-leftwards_arrow_with_hook"><a href="#textDocument_rangeFormatting" name="textDocument_rangeFormatting" class="anchor">Document Range Formatting Request (<img class="emoji" title=":leftwards_arrow_with_hook:" alt=":leftwards_arrow_with_hook:" src="https://github.githubassets.com/images/icons/emoji/unicode/21a9.png" height="20" width="20">)</a></h4>

<p>The document range formatting request is sent from the client to the server to format a given range in a document.</p>

<p><em>Client Capability</em>:</p>
<ul>
  <li>property name (optional): <code class="language-plaintext highlighter-rouge">textDocument.rangeFormatting</code>
</li>
  <li>property type: <code class="language-plaintext highlighter-rouge">DocumentRangeFormattingClientCapabilities</code> defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#documentRangeFormattingClientCapabilities" name="documentRangeFormattingClientCapabilities" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">DocumentRangeFormattingClientCapabilities</span> <span class="p">{</span>
	<span class="cm">/**
	 * Whether formatting supports dynamic registration.
	 */</span>
	<span class="nl">dynamicRegistration</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Server Capability</em>:</p>
<ul>
  <li>property name (optional): <code class="language-plaintext highlighter-rouge">documentRangeFormattingProvider</code>
</li>
  <li>property type: <code class="language-plaintext highlighter-rouge">boolean | DocumentRangeFormattingOptions</code> where <code class="language-plaintext highlighter-rouge">DocumentRangeFormattingOptions</code> is defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#documentRangeFormattingOptions" name="documentRangeFormattingOptions" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">DocumentRangeFormattingOptions</span> <span class="kd">extends</span>
	<span class="nx">WorkDoneProgressOptions</span> <span class="p">{</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Registration Options</em>: <code class="language-plaintext highlighter-rouge">DocumentFormattingRegistrationOptions</code> defined as follows:</p>

<div class="anchorHolder"><a href="#documentRangeFormattingRegistrationOptions" name="documentRangeFormattingRegistrationOptions" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">DocumentRangeFormattingRegistrationOptions</span> <span class="kd">extends</span>
	<span class="nx">TextDocumentRegistrationOptions</span><span class="p">,</span> <span class="nx">DocumentRangeFormattingOptions</span> <span class="p">{</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Request</em>:</p>
<ul>
  <li>method: <code class="language-plaintext highlighter-rouge">textDocument/rangeFormatting</code>,</li>
  <li>params: <code class="language-plaintext highlighter-rouge">DocumentRangeFormattingParams</code> defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#documentRangeFormattingParams" name="documentRangeFormattingParams" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="kr">interface</span> <span class="nx">DocumentRangeFormattingParams</span> <span class="kd">extends</span> <span class="nx">WorkDoneProgressParams</span> <span class="p">{</span>
	<span class="cm">/**
	 * The document to format.
	 */</span>
	<span class="nl">textDocument</span><span class="p">:</span> <span class="nx">TextDocumentIdentifier</span><span class="p">;</span>

	<span class="cm">/**
	 * The range to format
	 */</span>
	<span class="nl">range</span><span class="p">:</span> <span class="nx">Range</span><span class="p">;</span>

	<span class="cm">/**
	 * The format options
	 */</span>
	<span class="nl">options</span><span class="p">:</span> <span class="nx">FormattingOptions</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Response</em>:</p>
<ul>
  <li>result: <a href="#textEdit"><code class="language-plaintext highlighter-rouge">TextEdit[]</code></a> | <code class="language-plaintext highlighter-rouge">null</code> describing the modification to the document to be formatted.</li>
  <li>error: code and message set in case an exception happens during the range formatting request.</li>
</ul>

<h4 id="document-on-type-formatting-request-leftwards_arrow_with_hook"><a href="#textDocument_onTypeFormatting" name="textDocument_onTypeFormatting" class="anchor">Document on Type Formatting Request (<img class="emoji" title=":leftwards_arrow_with_hook:" alt=":leftwards_arrow_with_hook:" src="https://github.githubassets.com/images/icons/emoji/unicode/21a9.png" height="20" width="20">)</a></h4>

<p>The document on type formatting request is sent from the client to the server to format parts of the document during typing.</p>

<p><em>Client Capability</em>:</p>
<ul>
  <li>property name (optional): <code class="language-plaintext highlighter-rouge">textDocument.onTypeFormatting</code>
</li>
  <li>property type: <code class="language-plaintext highlighter-rouge">DocumentOnTypeFormattingClientCapabilities</code> defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#documentOnTypeFormattingClientCapabilities" name="documentOnTypeFormattingClientCapabilities" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">DocumentOnTypeFormattingClientCapabilities</span> <span class="p">{</span>
	<span class="cm">/**
	 * Whether on type formatting supports dynamic registration.
	 */</span>
	<span class="nl">dynamicRegistration</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Server Capability</em>:</p>
<ul>
  <li>property name (optional): <code class="language-plaintext highlighter-rouge">documentOnTypeFormattingProvider</code>
</li>
  <li>property type: <code class="language-plaintext highlighter-rouge">DocumentOnTypeFormattingOptions</code> defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#documentOnTypeFormattingOptions" name="documentOnTypeFormattingOptions" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">DocumentOnTypeFormattingOptions</span> <span class="p">{</span>
	<span class="cm">/**
	 * A character on which formatting should be triggered, like `{`.
	 */</span>
	<span class="nl">firstTriggerCharacter</span><span class="p">:</span> <span class="kr">string</span><span class="p">;</span>

	<span class="cm">/**
	 * More trigger characters.
	 */</span>
	<span class="nl">moreTriggerCharacter</span><span class="p">?:</span> <span class="kr">string</span><span class="p">[];</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Registration Options</em>: <code class="language-plaintext highlighter-rouge">DocumentOnTypeFormattingRegistrationOptions</code> defined as follows:</p>

<div class="anchorHolder"><a href="#documentOnTypeFormattingRegistrationOptions" name="documentOnTypeFormattingRegistrationOptions" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">DocumentOnTypeFormattingRegistrationOptions</span> <span class="kd">extends</span>
	<span class="nx">TextDocumentRegistrationOptions</span><span class="p">,</span> <span class="nx">DocumentOnTypeFormattingOptions</span> <span class="p">{</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Request</em>:</p>
<ul>
  <li>method: <code class="language-plaintext highlighter-rouge">textDocument/onTypeFormatting</code>
</li>
  <li>params: <code class="language-plaintext highlighter-rouge">DocumentOnTypeFormattingParams</code> defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#documentOnTypeFormattingParams" name="documentOnTypeFormattingParams" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="kr">interface</span> <span class="nx">DocumentOnTypeFormattingParams</span> <span class="p">{</span>

	<span class="cm">/**
	 * The document to format.
	 */</span>
	<span class="nl">textDocument</span><span class="p">:</span> <span class="nx">TextDocumentIdentifier</span><span class="p">;</span>

	<span class="cm">/**
	 * The position around which the on type formatting should happen.
	 * This is not necessarily the exact position where the character denoted
	 * by the property `ch` got typed.
	 */</span>
	<span class="nl">position</span><span class="p">:</span> <span class="nx">Position</span><span class="p">;</span>

	<span class="cm">/**
	 * The character that has been typed that triggered the formatting
	 * on type request. That is not necessarily the last character that
	 * got inserted into the document since the client could auto insert
	 * characters as well (e.g. like automatic brace completion).
	 */</span>
	<span class="nl">ch</span><span class="p">:</span> <span class="kr">string</span><span class="p">;</span>

	<span class="cm">/**
	 * The formatting options.
	 */</span>
	<span class="nl">options</span><span class="p">:</span> <span class="nx">FormattingOptions</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Response</em>:</p>
<ul>
  <li>result: <a href="#textEdit"><code class="language-plaintext highlighter-rouge">TextEdit[]</code></a> | <code class="language-plaintext highlighter-rouge">null</code> describing the modification to the document.</li>
  <li>error: code and message set in case an exception happens during the range formatting request.</li>
</ul>

<h4 id="rename-request-leftwards_arrow_with_hook"><a href="#textDocument_rename" name="textDocument_rename" class="anchor">Rename Request (<img class="emoji" title=":leftwards_arrow_with_hook:" alt=":leftwards_arrow_with_hook:" src="https://github.githubassets.com/images/icons/emoji/unicode/21a9.png" height="20" width="20">)</a></h4>

<p>The rename request is sent from the client to the server to ask the server to compute a workspace change so that the client can perform a workspace-wide rename of a symbol.</p>

<p><em>Client Capability</em>:</p>
<ul>
  <li>property name (optional): <code class="language-plaintext highlighter-rouge">textDocument.rename</code>
</li>
  <li>property type: <code class="language-plaintext highlighter-rouge">RenameClientCapabilities</code> defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#prepareSupportDefaultBehavior" name="prepareSupportDefaultBehavior" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="k">namespace</span> <span class="nx">PrepareSupportDefaultBehavior</span> <span class="p">{</span>
	<span class="cm">/**
	 * The client's default behavior is to select the identifier
	 * according to the language's syntax rule.
	 */</span>
	 <span class="k">export</span> <span class="kd">const</span> <span class="nx">Identifier</span><span class="p">:</span> <span class="mi">1</span> <span class="o">=</span> <span class="mi">1</span><span class="p">;</span>
<span class="p">}</span>

<span class="k">export</span> <span class="kd">type</span> <span class="nx">PrepareSupportDefaultBehavior</span> <span class="o">=</span> <span class="mi">1</span><span class="p">;</span>
</code></pre></div></div>

<div class="anchorHolder"><a href="#renameClientCapabilities" name="renameClientCapabilities" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">RenameClientCapabilities</span> <span class="p">{</span>
	<span class="cm">/**
	 * Whether rename supports dynamic registration.
	 */</span>
	<span class="nl">dynamicRegistration</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>

	<span class="cm">/**
	 * Client supports testing for validity of rename operations
	 * before execution.
	 *
	 * @since version 3.12.0
	 */</span>
	<span class="nl">prepareSupport</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>

	<span class="cm">/**
	 * Client supports the default behavior result
	 * (`{ defaultBehavior: boolean }`).
	 *
	 * The value indicates the default behavior used by the
	 * client.
	 *
	 * @since version 3.16.0
	 */</span>
	<span class="nl">prepareSupportDefaultBehavior</span><span class="p">?:</span> <span class="nx">PrepareSupportDefaultBehavior</span><span class="p">;</span>

	<span class="cm">/**
	 * Whether the client honors the change annotations in
	 * text edits and resource operations returned via the
	 * rename request's workspace edit by for example presenting
	 * the workspace edit in the user interface and asking
	 * for confirmation.
	 *
	 * @since 3.16.0
	 */</span>
	<span class="nl">honorsChangeAnnotations</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Server Capability</em>:</p>
<ul>
  <li>property name (optional): <code class="language-plaintext highlighter-rouge">renameProvider</code>
</li>
  <li>property type: <code class="language-plaintext highlighter-rouge">boolean | RenameOptions</code> where <code class="language-plaintext highlighter-rouge">RenameOptions</code> is defined as follows:</li>
</ul>

<p><code class="language-plaintext highlighter-rouge">RenameOptions</code> may only be specified if the client states that it supports <code class="language-plaintext highlighter-rouge">prepareSupport</code> in its initial <code class="language-plaintext highlighter-rouge">initialize</code> request.</p>

<div class="anchorHolder"><a href="#renameOptions" name="renameOptions" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">RenameOptions</span> <span class="kd">extends</span> <span class="nx">WorkDoneProgressOptions</span> <span class="p">{</span>
	<span class="cm">/**
	 * Renames should be checked and tested before being executed.
	 */</span>
	<span class="nl">prepareProvider</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Registration Options</em>: <code class="language-plaintext highlighter-rouge">RenameRegistrationOptions</code> defined as follows:</p>

<div class="anchorHolder"><a href="#renameRegistrationOptions" name="renameRegistrationOptions" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">RenameRegistrationOptions</span> <span class="kd">extends</span>
	<span class="nx">TextDocumentRegistrationOptions</span><span class="p">,</span> <span class="nx">RenameOptions</span> <span class="p">{</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Request</em>:</p>
<ul>
  <li>method: <code class="language-plaintext highlighter-rouge">textDocument/rename</code>
</li>
  <li>params: <code class="language-plaintext highlighter-rouge">RenameParams</code> defined as follows</li>
</ul>

<div class="anchorHolder"><a href="#renameParams" name="renameParams" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="kr">interface</span> <span class="nx">RenameParams</span> <span class="kd">extends</span> <span class="nx">TextDocumentPositionParams</span><span class="p">,</span>
	<span class="nx">WorkDoneProgressParams</span> <span class="p">{</span>
	<span class="cm">/**
	 * The new name of the symbol. If the given name is not valid the
	 * request must return a [ResponseError](#ResponseError) with an
	 * appropriate message set.
	 */</span>
	<span class="nl">newName</span><span class="p">:</span> <span class="kr">string</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Response</em>:</p>
<ul>
  <li>result: <a href="#workspaceedit"><code class="language-plaintext highlighter-rouge">WorkspaceEdit</code></a> | <code class="language-plaintext highlighter-rouge">null</code> describing the modification to the workspace. <code class="language-plaintext highlighter-rouge">null</code> should be treated the same was as <a href="#workspaceedit"><code class="language-plaintext highlighter-rouge">WorkspaceEdit</code></a> with no changes (no change was required).</li>
  <li>error: code and message set in case when rename could not be performed for any reason. Examples include: there is nothing at given <code class="language-plaintext highlighter-rouge">position</code> to rename (like a space), given symbol does not support renaming by the server or the code is invalid (e.g. does not compile).</li>
</ul>

<h4 id="prepare-rename-request-leftwards_arrow_with_hook"><a href="#textDocument_prepareRename" name="textDocument_prepareRename" class="anchor">Prepare Rename Request (<img class="emoji" title=":leftwards_arrow_with_hook:" alt=":leftwards_arrow_with_hook:" src="https://github.githubassets.com/images/icons/emoji/unicode/21a9.png" height="20" width="20">)</a></h4>

<blockquote>
  <p><em>Since version 3.12.0</em></p>
</blockquote>

<p>The prepare rename request is sent from the client to the server to setup and test the validity of a rename operation at a given location.</p>

<p><em>Request</em>:</p>
<ul>
  <li>method: <code class="language-plaintext highlighter-rouge">textDocument/prepareRename</code>
</li>
  <li>params: <code class="language-plaintext highlighter-rouge">PrepareRenameParams</code> defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#prepareRenameParams" name="prepareRenameParams" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">PrepareRenameParams</span> <span class="kd">extends</span> <span class="nx">TextDocumentPositionParams</span><span class="p">,</span> <span class="nx">WorkDoneProgressParams</span> <span class="p">{</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Response</em>:</p>
<ul>
  <li>result: <code class="language-plaintext highlighter-rouge">Range | { range: Range, placeholder: string } | { defaultBehavior: boolean } | null</code> describing a <a href="#range"><code class="language-plaintext highlighter-rouge">Range</code></a> of the string to rename and optionally a placeholder text of the string content to be renamed. If <code class="language-plaintext highlighter-rouge">{ defaultBehavior: boolean }</code> is returned (since 3.16) the rename position is valid and the client should use its default behavior to compute the rename range. If <code class="language-plaintext highlighter-rouge">null</code> is returned then it is deemed that a ‘textDocument/rename’ request is not valid at the given position.</li>
  <li>error: code and message set in case the element can’t be renamed. Clients should show the information in their user interface.</li>
</ul>

<h4 id="linked-editing-rangeleftwards_arrow_with_hook"><a href="#textDocument_linkedEditingRange" name="textDocument_linkedEditingRange" class="anchor">Linked Editing Range(<img class="emoji" title=":leftwards_arrow_with_hook:" alt=":leftwards_arrow_with_hook:" src="https://github.githubassets.com/images/icons/emoji/unicode/21a9.png" height="20" width="20">)</a></h4>

<blockquote>
  <p><em>Since version 3.16.0</em></p>
</blockquote>

<p>The linked editing request is sent from the client to the server to return for a given position in a document the range of the symbol at the position and all ranges that have the same content. Optionally a word pattern can be returned to describe valid contents. A rename to one of the ranges can be applied to all other ranges if the new content is valid. If no result-specific word pattern is provided, the word pattern from the client’s language configuration is used.</p>

<p><em>Client Capabilities</em>:</p>

<ul>
  <li>property name (optional): <code class="language-plaintext highlighter-rouge">textDocument.linkedEditingRange</code>
</li>
  <li>property type: <code class="language-plaintext highlighter-rouge">LinkedEditingRangeClientCapabilities</code> defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#linkedEditingRangeClientCapabilities" name="linkedEditingRangeClientCapabilities" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">LinkedEditingRangeClientCapabilities</span> <span class="p">{</span>
	<span class="cm">/**
	 * Whether the implementation supports dynamic registration.
	 * If this is set to `true` the client supports the new
	 * `(TextDocumentRegistrationOptions &amp; StaticRegistrationOptions)`
	 * return value for the corresponding server capability as well.
	 */</span>
	<span class="nl">dynamicRegistration</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Server Capability</em>:</p>

<ul>
  <li>property name (optional): <code class="language-plaintext highlighter-rouge">linkedEditingRangeProvider</code>
</li>
  <li>property type: <code class="language-plaintext highlighter-rouge">boolean</code> | <code class="language-plaintext highlighter-rouge">LinkedEditingRangeOptions</code> | <code class="language-plaintext highlighter-rouge">LinkedEditingRangeRegistrationOptions</code> defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#linkedEditingRangeOptions" name="linkedEditingRangeOptions" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">LinkedEditingRangeOptions</span> <span class="kd">extends</span> <span class="nx">WorkDoneProgressOptions</span> <span class="p">{</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Registration Options</em>: <code class="language-plaintext highlighter-rouge">LinkedEditingRangeRegistrationOptions</code> defined as follows:</p>

<div class="anchorHolder"><a href="#linkedEditingRangeRegistrationOptions" name="linkedEditingRangeRegistrationOptions" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">LinkedEditingRangeRegistrationOptions</span> <span class="kd">extends</span>
	<span class="nx">TextDocumentRegistrationOptions</span><span class="p">,</span> <span class="nx">LinkedEditingRangeOptions</span><span class="p">,</span>
	<span class="nx">StaticRegistrationOptions</span> <span class="p">{</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Request</em>:</p>

<ul>
  <li>method: <code class="language-plaintext highlighter-rouge">textDocument/linkedEditingRange</code>
</li>
  <li>params: <code class="language-plaintext highlighter-rouge">LinkedEditingRangeParams</code> defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#linkedEditingRangeParams" name="linkedEditingRangeParams" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">LinkedEditingRangeParams</span> <span class="kd">extends</span> <span class="nx">TextDocumentPositionParams</span><span class="p">,</span>
	<span class="nx">WorkDoneProgressParams</span> <span class="p">{</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Response</em>:</p>

<ul>
  <li>result: <code class="language-plaintext highlighter-rouge">LinkedEditingRanges</code> | <code class="language-plaintext highlighter-rouge">null</code> defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#linkedEditingRanges" name="linkedEditingRanges" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">LinkedEditingRanges</span> <span class="p">{</span>
	<span class="cm">/**
	 * A list of ranges that can be renamed together. The ranges must have
	 * identical length and contain identical text content. The ranges cannot
	 * overlap.
	 */</span>
	<span class="nl">ranges</span><span class="p">:</span> <span class="nx">Range</span><span class="p">[];</span>

	<span class="cm">/**
	 * An optional word pattern (regular expression) that describes valid
	 * contents for the given ranges. If no pattern is provided, the client
	 * configuration's word pattern will be used.
	 */</span>
	<span class="nl">wordPattern</span><span class="p">?:</span> <span class="kr">string</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>
<ul>
  <li>error: code and message set in case an exception happens during the ‘textDocument/linkedEditingRange’ request</li>
</ul>

<h3 id="workspace-features"><a href="#workspaceFeatures" name="workspaceFeatures" class="anchor">Workspace Features</a></h3>

<h4 id="workspace-symbols-request-leftwards_arrow_with_hook"><a href="#workspace_symbol" name="workspace_symbol" class="anchor">Workspace Symbols Request (<img class="emoji" title=":leftwards_arrow_with_hook:" alt=":leftwards_arrow_with_hook:" src="https://github.githubassets.com/images/icons/emoji/unicode/21a9.png" height="20" width="20">)</a></h4>

<p>The workspace symbol request is sent from the client to the server to list project-wide symbols matching the query string. Since 3.17.0 servers can also provider a handler for <code class="language-plaintext highlighter-rouge">workspaceSymbol/resolve</code> requests. This allows servers to return workspace symbols without a range for a <code class="language-plaintext highlighter-rouge">workspace/symbol</code> request. Clients then need to resolve the range when necessary using the <code class="language-plaintext highlighter-rouge">workspaceSymbol/resolve</code> request. Servers can only use this new model if clients advertise support for it via the <code class="language-plaintext highlighter-rouge">workspace.symbol.resolveSupport</code> capability.</p>

<p><em>Client Capability</em>:</p>
<ul>
  <li>property path (optional): <code class="language-plaintext highlighter-rouge">workspace.symbol</code>
</li>
  <li>property type: <code class="language-plaintext highlighter-rouge">WorkspaceSymbolClientCapabilities</code> defined as follows:</li>
</ul>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="kr">interface</span> <span class="nx">WorkspaceSymbolClientCapabilities</span> <span class="p">{</span>
	<span class="cm">/**
	 * Symbol request supports dynamic registration.
	 */</span>
	<span class="nl">dynamicRegistration</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>

	<span class="cm">/**
	 * Specific capabilities for the `SymbolKind` in the `workspace/symbol`
	 * request.
	 */</span>
	<span class="nl">symbolKind</span><span class="p">?:</span> <span class="p">{</span>
		<span class="cm">/**
		 * The symbol kind values the client supports. When this
		 * property exists the client also guarantees that it will
		 * handle values outside its set gracefully and falls back
		 * to a default value when unknown.
		 *
		 * If this property is not present the client only supports
		 * the symbol kinds from `File` to `Array` as defined in
		 * the initial version of the protocol.
		 */</span>
		<span class="nx">valueSet</span><span class="p">?:</span> <span class="nx">SymbolKind</span><span class="p">[];</span>
	<span class="p">};</span>

	<span class="cm">/**
	 * The client supports tags on `SymbolInformation` and `WorkspaceSymbol`.
	 * Clients supporting tags have to handle unknown tags gracefully.
	 *
	 * @since 3.16.0
	 */</span>
	<span class="nl">tagSupport</span><span class="p">?:</span> <span class="p">{</span>
		<span class="cm">/**
		 * The tags supported by the client.
		 */</span>
		<span class="na">valueSet</span><span class="p">:</span> <span class="nx">SymbolTag</span><span class="p">[];</span>
	<span class="p">};</span>

	<span class="cm">/**
	 * The client support partial workspace symbols. The client will send the
	 * request `workspaceSymbol/resolve` to the server to resolve additional
	 * properties.
	 *
	 * @since 3.17.0 - proposedState
	 */</span>
	<span class="nl">resolveSupport</span><span class="p">?:</span> <span class="p">{</span>
		<span class="cm">/**
		 * The properties that a client can resolve lazily. Usually
		 * `location.range`
		 */</span>
		<span class="na">properties</span><span class="p">:</span> <span class="kr">string</span><span class="p">[];</span>
	<span class="p">};</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Server Capability</em>:</p>
<ul>
  <li>property path (optional): <code class="language-plaintext highlighter-rouge">workspaceSymbolProvider</code>
</li>
  <li>property type: <code class="language-plaintext highlighter-rouge">boolean | WorkspaceSymbolOptions</code> where <code class="language-plaintext highlighter-rouge">WorkspaceSymbolOptions</code> is defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#workspaceSymbolOptions" name="workspaceSymbolOptions" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">WorkspaceSymbolOptions</span> <span class="kd">extends</span> <span class="nx">WorkDoneProgressOptions</span> <span class="p">{</span>
	<span class="cm">/**
	 * The server provides support to resolve additional
	 * information for a workspace symbol.
	 *
	 * @since 3.17.0
	 */</span>
	<span class="nl">resolveProvider</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Registration Options</em>: <code class="language-plaintext highlighter-rouge">WorkspaceSymbolRegistrationOptions</code> defined as follows:</p>

<div class="anchorHolder"><a href="#workspaceSymbolRegistrationOptions" name="workspaceSymbolRegistrationOptions" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">WorkspaceSymbolRegistrationOptions</span>
	<span class="kd">extends</span> <span class="nx">WorkspaceSymbolOptions</span> <span class="p">{</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Request</em>:</p>
<ul>
  <li>method: ‘workspace/symbol’</li>
  <li>params: <code class="language-plaintext highlighter-rouge">WorkspaceSymbolParams</code> defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#workspaceSymbolParams" name="workspaceSymbolParams" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * The parameters of a Workspace Symbol Request.
 */</span>
<span class="kr">interface</span> <span class="nx">WorkspaceSymbolParams</span> <span class="kd">extends</span> <span class="nx">WorkDoneProgressParams</span><span class="p">,</span>
	<span class="nx">PartialResultParams</span> <span class="p">{</span>
	<span class="cm">/**
	 * A query string to filter symbols by. Clients may send an empty
	 * string here to request all symbols.
	 */</span>
	<span class="nl">query</span><span class="p">:</span> <span class="kr">string</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Response</em>:</p>
<ul>
  <li>result: <code class="language-plaintext highlighter-rouge">SymbolInformation[]</code> | <code class="language-plaintext highlighter-rouge">WorkspaceSymbol[]</code> | <code class="language-plaintext highlighter-rouge">null</code>. See above for the definition of <code class="language-plaintext highlighter-rouge">SymbolInformation</code>. It is recommended that you use the new <code class="language-plaintext highlighter-rouge">WorkspaceSymbol</code>. However whether the workspace symbol can return a location without a range depends on the client capability <code class="language-plaintext highlighter-rouge">workspace.symbol.resolveSupport</code>. <code class="language-plaintext highlighter-rouge">WorkspaceSymbol</code>which is defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#workspaceSymbol" name="workspaceSymbol" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * A special workspace symbol that supports locations without a range
 *
 * @since 3.17.0
 */</span>
<span class="k">export</span> <span class="kr">interface</span> <span class="nx">WorkspaceSymbol</span> <span class="p">{</span>
	<span class="cm">/**
	 * The name of this symbol.
	 */</span>
	<span class="nl">name</span><span class="p">:</span> <span class="kr">string</span><span class="p">;</span>

	<span class="cm">/**
	 * The kind of this symbol.
	 */</span>
	<span class="nl">kind</span><span class="p">:</span> <span class="nx">SymbolKind</span><span class="p">;</span>

	<span class="cm">/**
	 * Tags for this completion item.
	 */</span>
	<span class="nl">tags</span><span class="p">?:</span> <span class="nx">SymbolTag</span><span class="p">[];</span>

	<span class="cm">/**
	 * The name of the symbol containing this symbol. This information is for
	 * user interface purposes (e.g. to render a qualifier in the user interface
	 * if necessary). It can't be used to re-infer a hierarchy for the document
	 * symbols.
	 */</span>
	<span class="nl">containerName</span><span class="p">?:</span> <span class="kr">string</span><span class="p">;</span>

	<span class="cm">/**
	 * The location of this symbol. Whether a server is allowed to
	 * return a location without a range depends on the client
	 * capability `workspace.symbol.resolveSupport`.
	 *
	 * See also `SymbolInformation.location`.
	 */</span>
	<span class="nl">location</span><span class="p">:</span> <span class="nx">Location</span> <span class="o">|</span> <span class="p">{</span> <span class="na">uri</span><span class="p">:</span> <span class="nx">DocumentUri</span> <span class="p">};</span>

	<span class="cm">/**
	 * A data entry field that is preserved on a workspace symbol between a
	 * workspace symbol request and a workspace symbol resolve request.
	 */</span>
	<span class="nl">data</span><span class="p">?:</span> <span class="nx">LSPAny</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>
<ul>
  <li>partial result: <code class="language-plaintext highlighter-rouge">SymbolInformation[]</code> | <code class="language-plaintext highlighter-rouge">WorkspaceSymbol[]</code> as defined above.</li>
  <li>error: code and message set in case an exception happens during the workspace symbol request.</li>
</ul>

<h4 id="workspace-symbol-resolve-request-leftwards_arrow_with_hook"><a href="#workspace_symbolResolve" name="workspace_symbolResolve" class="anchor">Workspace Symbol Resolve Request (<img class="emoji" title=":leftwards_arrow_with_hook:" alt=":leftwards_arrow_with_hook:" src="https://github.githubassets.com/images/icons/emoji/unicode/21a9.png" height="20" width="20">)</a></h4>

<p>The request is sent from the client to the server to resolve additional information for a given workspace symbol.</p>

<p><em>Request</em>:</p>
<ul>
  <li>method: ‘workspaceSymbol/resolve’</li>
  <li>params: <code class="language-plaintext highlighter-rouge">WorkspaceSymbol</code>
</li>
</ul>

<p><em>Response</em>:</p>
<ul>
  <li>result: <code class="language-plaintext highlighter-rouge">WorkspaceSymbol</code>
</li>
  <li>error: code and message set in case an exception happens during the workspace symbol resolve request.</li>
</ul>

<h4 id="configuration-request-arrow_right_hook"><a href="#workspace_configuration" name="workspace_configuration" class="anchor">Configuration Request (<img class="emoji" title=":arrow_right_hook:" alt=":arrow_right_hook:" src="https://github.githubassets.com/images/icons/emoji/unicode/21aa.png" height="20" width="20">)</a></h4>

<blockquote>
  <p><em>Since version 3.6.0</em></p>
</blockquote>

<p>The <code class="language-plaintext highlighter-rouge">workspace/configuration</code> request is sent from the server to the client to fetch configuration settings from the client. The request can fetch several configuration settings in one roundtrip. The order of the returned configuration settings correspond to the order of the passed <code class="language-plaintext highlighter-rouge">ConfigurationItems</code> (e.g. the first item in the response is the result for the first configuration item in the params).</p>

<p>A <code class="language-plaintext highlighter-rouge">ConfigurationItem</code> consists of the configuration section to ask for and an additional scope URI. The configuration section asked for is defined by the server and doesn’t necessarily need to correspond to the configuration store used by the client. So a server might ask for a configuration <code class="language-plaintext highlighter-rouge">cpp.formatterOptions</code> but the client stores the configuration in an XML store layout differently. It is up to the client to do the necessary conversion. If a scope URI is provided the client should return the setting scoped to the provided resource. If the client for example uses <a href="http://editorconfig.org/">EditorConfig</a> to manage its settings the configuration should be returned for the passed resource URI. If the client can’t provide a configuration setting for a given scope then <code class="language-plaintext highlighter-rouge">null</code> needs to be present in the returned array.</p>

<p>This pull model replaces the old push model were the client signaled configuration change via an event. If the server still needs to react to configuration changes (since the server caches the result of <code class="language-plaintext highlighter-rouge">workspace/configuration</code> requests) the server should register for an empty configuration change using the following registration pattern:</p>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="nx">connection</span><span class="p">.</span><span class="nx">client</span><span class="p">.</span><span class="nx">register</span><span class="p">(</span><span class="nx">DidChangeConfigurationNotification</span><span class="p">.</span><span class="kd">type</span><span class="p">,</span> <span class="kc">undefined</span><span class="p">);</span>
</code></pre></div></div>

<p><em>Client Capability</em>:</p>
<ul>
  <li>property path (optional): <code class="language-plaintext highlighter-rouge">workspace.configuration</code>
</li>
  <li>property type: <code class="language-plaintext highlighter-rouge">boolean</code>
</li>
</ul>

<p><em>Request</em>:</p>
<ul>
  <li>method: ‘workspace/configuration’</li>
  <li>params: <code class="language-plaintext highlighter-rouge">ConfigurationParams</code> defined as follows</li>
</ul>

<div class="anchorHolder"><a href="#configurationParams" name="configurationParams" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">ConfigurationParams</span> <span class="p">{</span>
	<span class="nl">items</span><span class="p">:</span> <span class="nx">ConfigurationItem</span><span class="p">[];</span>
<span class="p">}</span>
</code></pre></div></div>

<div class="anchorHolder"><a href="#configurationItem" name="configurationItem" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">ConfigurationItem</span> <span class="p">{</span>
	<span class="cm">/**
	 * The scope to get the configuration section for.
	 */</span>
	<span class="nl">scopeUri</span><span class="p">?:</span> <span class="nx">URI</span><span class="p">;</span>

	<span class="cm">/**
	 * The configuration section asked for.
	 */</span>
	<span class="nl">section</span><span class="p">?:</span> <span class="kr">string</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Response</em>:</p>
<ul>
  <li>result: LSPAny[]</li>
  <li>error: code and message set in case an exception happens during the ‘workspace/configuration’ request</li>
</ul>

<h4 id="didchangeconfiguration-notification-arrow_right"><a href="#workspace_didChangeConfiguration" name="workspace_didChangeConfiguration" class="anchor">DidChangeConfiguration Notification (<img class="emoji" title=":arrow_right:" alt=":arrow_right:" src="https://github.githubassets.com/images/icons/emoji/unicode/27a1.png" height="20" width="20">)</a></h4>

<p>A notification sent from the client to the server to signal the change of configuration settings.</p>

<p><em>Client Capability</em>:</p>
<ul>
  <li>property path (optional): <code class="language-plaintext highlighter-rouge">workspace.didChangeConfiguration</code>
</li>
  <li>property type: <code class="language-plaintext highlighter-rouge">DidChangeConfigurationClientCapabilities</code> defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#didChangeConfigurationClientCapabilities" name="didChangeConfigurationClientCapabilities" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">DidChangeConfigurationClientCapabilities</span> <span class="p">{</span>
	<span class="cm">/**
	 * Did change configuration notification supports dynamic registration.
	 */</span>
	<span class="nl">dynamicRegistration</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Notification</em>:</p>
<ul>
  <li>method: ‘workspace/didChangeConfiguration’,</li>
  <li>params: <code class="language-plaintext highlighter-rouge">DidChangeConfigurationParams</code> defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#didChangeConfigurationParams" name="didChangeConfigurationParams" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="kr">interface</span> <span class="nx">DidChangeConfigurationParams</span> <span class="p">{</span>
	<span class="cm">/**
	 * The actual changed settings
	 */</span>
	<span class="nl">settings</span><span class="p">:</span> <span class="nx">LSPAny</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<h4 id="workspace-folders-request-arrow_right_hook"><a href="#workspace_workspaceFolders" name="workspace_workspaceFolders" class="anchor">Workspace folders request (<img class="emoji" title=":arrow_right_hook:" alt=":arrow_right_hook:" src="https://github.githubassets.com/images/icons/emoji/unicode/21aa.png" height="20" width="20">)</a></h4>

<blockquote>
  <p><em>Since version 3.6.0</em></p>
</blockquote>

<p>Many tools support more than one root folder per workspace. Examples for this are VS Code’s multi-root support, Atom’s project folder support or Sublime’s project support. If a client workspace consists of multiple roots then a server typically needs to know about this. The protocol up to now assumes one root folder which is announced to the server by the <code class="language-plaintext highlighter-rouge">rootUri</code> property of the <code class="language-plaintext highlighter-rouge">InitializeParams</code>. If the client supports workspace folders and announces them via the corresponding <code class="language-plaintext highlighter-rouge">workspaceFolders</code> client capability, the <code class="language-plaintext highlighter-rouge">InitializeParams</code> contain an additional property <code class="language-plaintext highlighter-rouge">workspaceFolders</code> with the configured workspace folders when the server starts.</p>

<p>The <code class="language-plaintext highlighter-rouge">workspace/workspaceFolders</code> request is sent from the server to the client to fetch the current open list of workspace folders. Returns <code class="language-plaintext highlighter-rouge">null</code> in the response if only a single file is open in the tool. Returns an empty array if a workspace is open but no folders are configured.</p>

<p><em>Client Capability</em>:</p>
<ul>
  <li>property path (optional): <code class="language-plaintext highlighter-rouge">workspace.workspaceFolders</code>
</li>
  <li>property type: <code class="language-plaintext highlighter-rouge">boolean</code>
</li>
</ul>

<p><em>Server Capability</em>:</p>
<ul>
  <li>property path (optional): <code class="language-plaintext highlighter-rouge">workspace.workspaceFolders</code>
</li>
  <li>property type: <code class="language-plaintext highlighter-rouge">WorkspaceFoldersServerCapabilities</code> defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#workspaceFoldersServerCapabilities" name="workspaceFoldersServerCapabilities" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">WorkspaceFoldersServerCapabilities</span> <span class="p">{</span>
	<span class="cm">/**
	 * The server has support for workspace folders
	 */</span>
	<span class="nl">supported</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>

	<span class="cm">/**
	 * Whether the server wants to receive workspace folder
	 * change notifications.
	 *
	 * If a string is provided, the string is treated as an ID
	 * under which the notification is registered on the client
	 * side. The ID can be used to unregister for these events
	 * using the `client/unregisterCapability` request.
	 */</span>
	<span class="nl">changeNotifications</span><span class="p">?:</span> <span class="kr">string</span> <span class="o">|</span> <span class="nx">boolean</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Request</em>:</p>
<ul>
  <li>method: <code class="language-plaintext highlighter-rouge">workspace/workspaceFolders</code>
</li>
  <li>params: none</li>
</ul>

<p><em>Response</em>:</p>
<ul>
  <li>result: <code class="language-plaintext highlighter-rouge">WorkspaceFolder[] | null</code> defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#workspaceFolder" name="workspaceFolder" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">WorkspaceFolder</span> <span class="p">{</span>
	<span class="cm">/**
	 * The associated URI for this workspace folder.
	 */</span>
	<span class="nl">uri</span><span class="p">:</span> <span class="nx">URI</span><span class="p">;</span>

	<span class="cm">/**
	 * The name of the workspace folder. Used to refer to this
	 * workspace folder in the user interface.
	 */</span>
	<span class="nl">name</span><span class="p">:</span> <span class="kr">string</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>
<ul>
  <li>error: code and message set in case an exception happens during the ‘workspace/workspaceFolders’ request</li>
</ul>

<h4 id="didchangeworkspacefolders-notification-arrow_right"><a href="#workspace_didChangeWorkspaceFolders" name="workspace_didChangeWorkspaceFolders" class="anchor">DidChangeWorkspaceFolders Notification (<img class="emoji" title=":arrow_right:" alt=":arrow_right:" src="https://github.githubassets.com/images/icons/emoji/unicode/27a1.png" height="20" width="20">)</a></h4>

<blockquote>
  <p><em>Since version 3.6.0</em></p>
</blockquote>

<p>The <code class="language-plaintext highlighter-rouge">workspace/didChangeWorkspaceFolders</code> notification is sent from the client to the server to inform the server about workspace folder configuration changes. A server can register for this notification by using either the <em>server capability</em> <code class="language-plaintext highlighter-rouge">workspace.workspaceFolders.changeNotifications</code> or by using the dynamic capability registration mechanism. To dynamically register for the <code class="language-plaintext highlighter-rouge">workspace/didChangeWorkspaceFolders</code> send a <code class="language-plaintext highlighter-rouge">client/registerCapability</code> request from the server to the client. The registration parameter must have a <code class="language-plaintext highlighter-rouge">registrations</code> item of the following form, where <code class="language-plaintext highlighter-rouge">id</code> is a unique id used to unregister the capability (the example uses a UUID):</p>
<div class="language-ts highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="p">{</span>
	<span class="nl">id</span><span class="p">:</span> <span class="dl">"</span><span class="s2">28c6150c-bd7b-11e7-abc4-cec278b6b50a</span><span class="dl">"</span><span class="p">,</span>
	<span class="nx">method</span><span class="p">:</span> <span class="dl">"</span><span class="s2">workspace/didChangeWorkspaceFolders</span><span class="dl">"</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Notification</em>:</p>
<ul>
  <li>method: ‘workspace/didChangeWorkspaceFolders’</li>
  <li>params: <code class="language-plaintext highlighter-rouge">DidChangeWorkspaceFoldersParams</code> defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#didChangeWorkspaceFoldersParams" name="didChangeWorkspaceFoldersParams" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">DidChangeWorkspaceFoldersParams</span> <span class="p">{</span>
	<span class="cm">/**
	 * The actual workspace folder change event.
	 */</span>
	<span class="nl">event</span><span class="p">:</span> <span class="nx">WorkspaceFoldersChangeEvent</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<div class="anchorHolder"><a href="#workspaceFoldersChangeEvent" name="workspaceFoldersChangeEvent" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * The workspace folder change event.
 */</span>
<span class="k">export</span> <span class="kr">interface</span> <span class="nx">WorkspaceFoldersChangeEvent</span> <span class="p">{</span>
	<span class="cm">/**
	 * The array of added workspace folders
	 */</span>
	<span class="nl">added</span><span class="p">:</span> <span class="nx">WorkspaceFolder</span><span class="p">[];</span>

	<span class="cm">/**
	 * The array of the removed workspace folders
	 */</span>
	<span class="nl">removed</span><span class="p">:</span> <span class="nx">WorkspaceFolder</span><span class="p">[];</span>
<span class="p">}</span>
</code></pre></div></div>

<h4 id="willcreatefiles-request-leftwards_arrow_with_hook"><a href="#workspace_willCreateFiles" name="workspace_willCreateFiles" class="anchor">WillCreateFiles Request (<img class="emoji" title=":leftwards_arrow_with_hook:" alt=":leftwards_arrow_with_hook:" src="https://github.githubassets.com/images/icons/emoji/unicode/21a9.png" height="20" width="20">)</a></h4>

<p>The will create files request is sent from the client to the server before files are actually created as long as the creation is triggered from within the client either by a user action or by applying a workspace edit. The request can return a <code class="language-plaintext highlighter-rouge">WorkspaceEdit</code> which will be applied to workspace before the files are created. Hence the <code class="language-plaintext highlighter-rouge">WorkspaceEdit</code> can not manipulate the content of the files to be created. Please note that clients might drop results if computing the edit took too long or if a server constantly fails on this request. This is done to keep creates fast and reliable.</p>

<p><em>Client Capability</em>:</p>
<ul>
  <li>property name (optional): <code class="language-plaintext highlighter-rouge">workspace.fileOperations.willCreate</code>
</li>
  <li>property type: <code class="language-plaintext highlighter-rouge">boolean</code>
</li>
</ul>

<p>The capability indicates that the client supports sending <code class="language-plaintext highlighter-rouge">workspace/willCreateFiles</code> requests.</p>

<p><em>Server Capability</em>:</p>
<ul>
  <li>property name (optional): <code class="language-plaintext highlighter-rouge">workspace.fileOperations.willCreate</code>
</li>
  <li>property type: <code class="language-plaintext highlighter-rouge">FileOperationRegistrationOptions</code> where <code class="language-plaintext highlighter-rouge">FileOperationRegistrationOptions</code> is defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#fileOperationRegistrationOptions" name="fileOperationRegistrationOptions" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * The options to register for file operations.
 *
 * @since 3.16.0
 */</span>
<span class="kr">interface</span> <span class="nx">FileOperationRegistrationOptions</span> <span class="p">{</span>
	<span class="cm">/**
	 * The actual filters.
	 */</span>
	<span class="nl">filters</span><span class="p">:</span> <span class="nx">FileOperationFilter</span><span class="p">[];</span>
<span class="p">}</span>
</code></pre></div></div>

<div class="anchorHolder"><a href="#fileOperationPatternKind" name="fileOperationPatternKind" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * A pattern kind describing if a glob pattern matches a file a folder or
 * both.
 *
 * @since 3.16.0
 */</span>
<span class="k">export</span> <span class="k">namespace</span> <span class="nx">FileOperationPatternKind</span> <span class="p">{</span>
	<span class="cm">/**
	 * The pattern matches a file only.
	 */</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">file</span><span class="p">:</span> <span class="dl">'</span><span class="s1">file</span><span class="dl">'</span> <span class="o">=</span> <span class="dl">'</span><span class="s1">file</span><span class="dl">'</span><span class="p">;</span>

	<span class="cm">/**
	 * The pattern matches a folder only.
	 */</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">folder</span><span class="p">:</span> <span class="dl">'</span><span class="s1">folder</span><span class="dl">'</span> <span class="o">=</span> <span class="dl">'</span><span class="s1">folder</span><span class="dl">'</span><span class="p">;</span>
<span class="p">}</span>

<span class="k">export</span> <span class="kd">type</span> <span class="nx">FileOperationPatternKind</span> <span class="o">=</span> <span class="dl">'</span><span class="s1">file</span><span class="dl">'</span> <span class="o">|</span> <span class="dl">'</span><span class="s1">folder</span><span class="dl">'</span><span class="p">;</span>
</code></pre></div></div>

<div class="anchorHolder"><a href="#fileOperationPatternOptions" name="fileOperationPatternOptions" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * Matching options for the file operation pattern.
 *
 * @since 3.16.0
 */</span>
<span class="k">export</span> <span class="kr">interface</span> <span class="nx">FileOperationPatternOptions</span> <span class="p">{</span>

	<span class="cm">/**
	 * The pattern should be matched ignoring casing.
	 */</span>
	<span class="nl">ignoreCase</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<div class="anchorHolder"><a href="#fileOperationPattern" name="fileOperationPattern" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * A pattern to describe in which file operation requests or notifications
 * the server is interested in.
 *
 * @since 3.16.0
 */</span>
<span class="kr">interface</span> <span class="nx">FileOperationPattern</span> <span class="p">{</span>
	<span class="cm">/**
	 * The glob pattern to match. Glob patterns can have the following syntax:
	 * - `*` to match one or more characters in a path segment
	 * - `?` to match on one character in a path segment
	 * - `**` to match any number of path segments, including none
	 * - `{}` to group sub patterns into an OR expression. (e.g. `**​/*.{ts,js}`
	 *   matches all TypeScript and JavaScript files)
	 * - `[]` to declare a range of characters to match in a path segment
	 *   (e.g., `example.[0-9]` to match on `example.0`, `example.1`, …)
	 * - `[!...]` to negate a range of characters to match in a path segment
	 *   (e.g., `example.[!0-9]` to match on `example.a`, `example.b`, but
	 *   not `example.0`)
	 */</span>
	<span class="nl">glob</span><span class="p">:</span> <span class="kr">string</span><span class="p">;</span>

	<span class="cm">/**
	 * Whether to match files or folders with this pattern.
	 *
	 * Matches both if undefined.
	 */</span>
	<span class="nl">matches</span><span class="p">?:</span> <span class="nx">FileOperationPatternKind</span><span class="p">;</span>

	<span class="cm">/**
	 * Additional options used during matching.
	 */</span>
	<span class="nl">options</span><span class="p">?:</span> <span class="nx">FileOperationPatternOptions</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<div class="anchorHolder"><a href="#fileOperationFilter" name="fileOperationFilter" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * A filter to describe in which file operation requests or notifications
 * the server is interested in.
 *
 * @since 3.16.0
 */</span>
<span class="k">export</span> <span class="kr">interface</span> <span class="nx">FileOperationFilter</span> <span class="p">{</span>

	<span class="cm">/**
	 * A Uri like `file` or `untitled`.
	 */</span>
	<span class="nl">scheme</span><span class="p">?:</span> <span class="kr">string</span><span class="p">;</span>

	<span class="cm">/**
	 * The actual file operation pattern.
	 */</span>
	<span class="nl">pattern</span><span class="p">:</span> <span class="nx">FileOperationPattern</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<p>The capability indicates that the server is interested in receiving <code class="language-plaintext highlighter-rouge">workspace/willCreateFiles</code> requests.</p>

<p><em>Registration Options</em>: none</p>

<p><em>Request</em>:</p>
<ul>
  <li>method: ‘workspace/willCreateFiles’</li>
  <li>params: <code class="language-plaintext highlighter-rouge">CreateFilesParams</code> defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#createFilesParams" name="createFilesParams" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * The parameters sent in notifications/requests for user-initiated creation
 * of files.
 *
 * @since 3.16.0
 */</span>
<span class="k">export</span> <span class="kr">interface</span> <span class="nx">CreateFilesParams</span> <span class="p">{</span>

	<span class="cm">/**
	 * An array of all files/folders created in this operation.
	 */</span>
	<span class="nl">files</span><span class="p">:</span> <span class="nx">FileCreate</span><span class="p">[];</span>
<span class="p">}</span>
</code></pre></div></div>

<div class="anchorHolder"><a href="#fileCreate" name="fileCreate" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * Represents information on a file/folder create.
 *
 * @since 3.16.0
 */</span>
<span class="k">export</span> <span class="kr">interface</span> <span class="nx">FileCreate</span> <span class="p">{</span>

	<span class="cm">/**
	 * A file:// URI for the location of the file/folder being created.
	 */</span>
	<span class="nl">uri</span><span class="p">:</span> <span class="kr">string</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Response</em>:</p>
<ul>
  <li>result:<code class="language-plaintext highlighter-rouge">WorkspaceEdit</code> | <code class="language-plaintext highlighter-rouge">null</code>
</li>
  <li>error: code and message set in case an exception happens during the <code class="language-plaintext highlighter-rouge">willCreateFiles</code> request.</li>
</ul>

<h4 id="didcreatefiles-notification-arrow_right"><a href="#workspace_didCreateFiles" name="workspace_didCreateFiles" class="anchor">DidCreateFiles Notification (<img class="emoji" title=":arrow_right:" alt=":arrow_right:" src="https://github.githubassets.com/images/icons/emoji/unicode/27a1.png" height="20" width="20">)</a></h4>

<p>The did create files notification is sent from the client to the server when files were created from within the client.</p>

<p><em>Client Capability</em>:</p>
<ul>
  <li>property name (optional): <code class="language-plaintext highlighter-rouge">workspace.fileOperations.didCreate</code>
</li>
  <li>property type: <code class="language-plaintext highlighter-rouge">boolean</code>
</li>
</ul>

<p>The capability indicates that the client supports sending <code class="language-plaintext highlighter-rouge">workspace/didCreateFiles</code> notifications.</p>

<p><em>Server Capability</em>:</p>
<ul>
  <li>property name (optional): <code class="language-plaintext highlighter-rouge">workspace.fileOperations.didCreate</code>
</li>
  <li>property type: <code class="language-plaintext highlighter-rouge">FileOperationRegistrationOptions</code>
</li>
</ul>

<p>The capability indicates that the server is interested in receiving <code class="language-plaintext highlighter-rouge">workspace/didCreateFiles</code> notifications.</p>

<p><em>Notification</em>:</p>
<ul>
  <li>method: ‘workspace/didCreateFiles’</li>
  <li>params: <code class="language-plaintext highlighter-rouge">CreateFilesParams</code>
</li>
</ul>

<h4 id="willrenamefiles-request-leftwards_arrow_with_hook"><a href="#workspace_willRenameFiles" name="workspace_willRenameFiles" class="anchor">WillRenameFiles Request (<img class="emoji" title=":leftwards_arrow_with_hook:" alt=":leftwards_arrow_with_hook:" src="https://github.githubassets.com/images/icons/emoji/unicode/21a9.png" height="20" width="20">)</a></h4>

<p>The will rename files request is sent from the client to the server before files are actually renamed as long as the rename is triggered from within the client either by a user action or by applying a workspace edit. The request can return a WorkspaceEdit which will be applied to workspace before the files are renamed. Please note that clients might drop results if computing the edit took too long or if a server constantly fails on this request. This is done to keep renames fast and reliable.</p>

<p><em>Client Capability</em>:</p>
<ul>
  <li>property name (optional): <code class="language-plaintext highlighter-rouge">workspace.fileOperations.willRename</code>
</li>
  <li>property type: <code class="language-plaintext highlighter-rouge">boolean</code>
</li>
</ul>

<p>The capability indicates that the client supports sending <code class="language-plaintext highlighter-rouge">workspace/willRenameFiles</code> requests.</p>

<p><em>Server Capability</em>:</p>
<ul>
  <li>property name (optional): <code class="language-plaintext highlighter-rouge">workspace.fileOperations.willRename</code>
</li>
  <li>property type: <code class="language-plaintext highlighter-rouge">FileOperationRegistrationOptions</code>
</li>
</ul>

<p>The capability indicates that the server is interested in receiving <code class="language-plaintext highlighter-rouge">workspace/willRenameFiles</code> requests.</p>

<p><em>Registration Options</em>: none</p>

<p><em>Request</em>:</p>
<ul>
  <li>method: ‘workspace/willRenameFiles’</li>
  <li>params: <code class="language-plaintext highlighter-rouge">RenameFilesParams</code> defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#renameFilesParams" name="renameFilesParams" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * The parameters sent in notifications/requests for user-initiated renames
 * of files.
 *
 * @since 3.16.0
 */</span>
<span class="k">export</span> <span class="kr">interface</span> <span class="nx">RenameFilesParams</span> <span class="p">{</span>

	<span class="cm">/**
	 * An array of all files/folders renamed in this operation. When a folder
	 * is renamed, only the folder will be included, and not its children.
	 */</span>
	<span class="nl">files</span><span class="p">:</span> <span class="nx">FileRename</span><span class="p">[];</span>
<span class="p">}</span>
</code></pre></div></div>

<div class="anchorHolder"><a href="#fileRename" name="fileRename" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * Represents information on a file/folder rename.
 *
 * @since 3.16.0
 */</span>
<span class="k">export</span> <span class="kr">interface</span> <span class="nx">FileRename</span> <span class="p">{</span>

	<span class="cm">/**
	 * A file:// URI for the original location of the file/folder being renamed.
	 */</span>
	<span class="nl">oldUri</span><span class="p">:</span> <span class="kr">string</span><span class="p">;</span>

	<span class="cm">/**
	 * A file:// URI for the new location of the file/folder being renamed.
	 */</span>
	<span class="nl">newUri</span><span class="p">:</span> <span class="kr">string</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Response</em>:</p>
<ul>
  <li>result:<code class="language-plaintext highlighter-rouge">WorkspaceEdit</code> | <code class="language-plaintext highlighter-rouge">null</code>
</li>
  <li>error: code and message set in case an exception happens during the <code class="language-plaintext highlighter-rouge">workspace/willRenameFiles</code> request.</li>
</ul>

<h4 id="didrenamefiles-notification-arrow_right"><a href="#workspace_didRenameFiles" name="workspace_didRenameFiles" class="anchor">DidRenameFiles Notification (<img class="emoji" title=":arrow_right:" alt=":arrow_right:" src="https://github.githubassets.com/images/icons/emoji/unicode/27a1.png" height="20" width="20">)</a></h4>

<p>The did rename files notification is sent from the client to the server when files were renamed from within the client.</p>

<p><em>Client Capability</em>:</p>
<ul>
  <li>property name (optional): <code class="language-plaintext highlighter-rouge">workspace.fileOperations.didRename</code>
</li>
  <li>property type: <code class="language-plaintext highlighter-rouge">boolean</code>
</li>
</ul>

<p>The capability indicates that the client supports sending <code class="language-plaintext highlighter-rouge">workspace/didRenameFiles</code> notifications.</p>

<p><em>Server Capability</em>:</p>
<ul>
  <li>property name (optional): <code class="language-plaintext highlighter-rouge">workspace.fileOperations.didRename</code>
</li>
  <li>property type: <code class="language-plaintext highlighter-rouge">FileOperationRegistrationOptions</code>
</li>
</ul>

<p>The capability indicates that the server is interested in receiving <code class="language-plaintext highlighter-rouge">workspace/didRenameFiles</code> notifications.</p>

<p><em>Notification</em>:</p>
<ul>
  <li>method: ‘workspace/didRenameFiles’</li>
  <li>params: <code class="language-plaintext highlighter-rouge">RenameFilesParams</code>
</li>
</ul>

<h4 id="willdeletefiles-request-leftwards_arrow_with_hook"><a href="#workspace_willDeleteFiles" name="workspace_willDeleteFiles" class="anchor">WillDeleteFiles Request (<img class="emoji" title=":leftwards_arrow_with_hook:" alt=":leftwards_arrow_with_hook:" src="https://github.githubassets.com/images/icons/emoji/unicode/21a9.png" height="20" width="20">)</a></h4>

<p>The will delete files request is sent from the client to the server before files are actually deleted as long as the deletion is triggered from within the client either by a user action or by applying a workspace edit. The request can return a WorkspaceEdit which will be applied to workspace before the files are deleted. Please note that clients might drop results if computing the edit took too long or if a server constantly fails on this request. This is done to keep deletes fast and reliable.</p>

<p><em>Client Capability</em>:</p>
<ul>
  <li>property name (optional): <code class="language-plaintext highlighter-rouge">workspace.fileOperations.willDelete</code>
</li>
  <li>property type: <code class="language-plaintext highlighter-rouge">boolean</code>
</li>
</ul>

<p>The capability indicates that the client supports sending <code class="language-plaintext highlighter-rouge">workspace/willDeleteFiles</code> requests.</p>

<p><em>Server Capability</em>:</p>
<ul>
  <li>property name (optional): <code class="language-plaintext highlighter-rouge">workspace.fileOperations.willDelete</code>
</li>
  <li>property type: <code class="language-plaintext highlighter-rouge">FileOperationRegistrationOptions</code>
</li>
</ul>

<p>The capability indicates that the server is interested in receiving <code class="language-plaintext highlighter-rouge">workspace/willDeleteFiles</code> requests.</p>

<p><em>Registration Options</em>: none</p>

<p><em>Request</em>:</p>
<ul>
  <li>method: <code class="language-plaintext highlighter-rouge">workspace/willDeleteFiles</code>
</li>
  <li>params: <code class="language-plaintext highlighter-rouge">DeleteFilesParams</code> defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#deleteFilesParams" name="deleteFilesParams" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * The parameters sent in notifications/requests for user-initiated deletes
 * of files.
 *
 * @since 3.16.0
 */</span>
<span class="k">export</span> <span class="kr">interface</span> <span class="nx">DeleteFilesParams</span> <span class="p">{</span>

	<span class="cm">/**
	 * An array of all files/folders deleted in this operation.
	 */</span>
	<span class="nl">files</span><span class="p">:</span> <span class="nx">FileDelete</span><span class="p">[];</span>
<span class="p">}</span>
</code></pre></div></div>

<div class="anchorHolder"><a href="#fileDelete" name="fileDelete" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * Represents information on a file/folder delete.
 *
 * @since 3.16.0
 */</span>
<span class="k">export</span> <span class="kr">interface</span> <span class="nx">FileDelete</span> <span class="p">{</span>

	<span class="cm">/**
	 * A file:// URI for the location of the file/folder being deleted.
	 */</span>
	<span class="nl">uri</span><span class="p">:</span> <span class="kr">string</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Response</em>:</p>
<ul>
  <li>result:<code class="language-plaintext highlighter-rouge">WorkspaceEdit</code> | <code class="language-plaintext highlighter-rouge">null</code>
</li>
  <li>error: code and message set in case an exception happens during the <code class="language-plaintext highlighter-rouge">workspace/willDeleteFiles</code> request.</li>
</ul>

<h4 id="diddeletefiles-notification-arrow_right"><a href="#workspace_didDeleteFiles" name="workspace_didDeleteFiles" class="anchor">DidDeleteFiles Notification (<img class="emoji" title=":arrow_right:" alt=":arrow_right:" src="https://github.githubassets.com/images/icons/emoji/unicode/27a1.png" height="20" width="20">)</a></h4>

<p>The did delete files notification is sent from the client to the server when files were deleted from within the client.</p>

<p><em>Client Capability</em>:</p>
<ul>
  <li>property name (optional): <code class="language-plaintext highlighter-rouge">workspace.fileOperations.didDelete</code>
</li>
  <li>property type: <code class="language-plaintext highlighter-rouge">boolean</code>
</li>
</ul>

<p>The capability indicates that the client supports sending <code class="language-plaintext highlighter-rouge">workspace/didDeleteFiles</code> notifications.</p>

<p><em>Server Capability</em>:</p>
<ul>
  <li>property name (optional): <code class="language-plaintext highlighter-rouge">workspace.fileOperations.didDelete</code>
</li>
  <li>property type: <code class="language-plaintext highlighter-rouge">FileOperationRegistrationOptions</code>
</li>
</ul>

<p>The capability indicates that the server is interested in receiving <code class="language-plaintext highlighter-rouge">workspace/didDeleteFiles</code> notifications.</p>

<p><em>Notification</em>:</p>
<ul>
  <li>method: ‘workspace/didDeleteFiles’</li>
  <li>params: <code class="language-plaintext highlighter-rouge">DeleteFilesParams</code>
</li>
</ul>

<h4 id="didchangewatchedfiles-notification-arrow_right"><a href="#workspace_didChangeWatchedFiles" name="workspace_didChangeWatchedFiles" class="anchor">DidChangeWatchedFiles Notification (<img class="emoji" title=":arrow_right:" alt=":arrow_right:" src="https://github.githubassets.com/images/icons/emoji/unicode/27a1.png" height="20" width="20">)</a></h4>

<p>The watched files notification is sent from the client to the server when the client detects changes to files and folders watched by the language client (note although the name suggest that only file events are sent it is about file system events which include folders as well). It is recommended that servers register for these file system events using the registration mechanism. In former implementations clients pushed file events without the server actively asking for it.</p>

<p>Servers are allowed to run their own file system watching mechanism and not rely on clients to provide file system events. However this is not recommended due to the following reasons:</p>

<ul>
  <li>to our experience getting file system watching on disk right is challenging, especially if it needs to be supported across multiple OSes.</li>
  <li>file system watching is not for free especially if the implementation uses some sort of polling and keeps a file system tree in memory to compare time stamps (as for example some node modules do)</li>
  <li>a client usually starts more than one server. If every server runs its own file system watching it can become a CPU or memory problem.</li>
  <li>in general there are more server than client implementations. So this problem is better solved on the client side.</li>
</ul>

<p><em>Client Capability</em>:</p>
<ul>
  <li>property path (optional): <code class="language-plaintext highlighter-rouge">workspace.didChangeWatchedFiles</code>
</li>
  <li>property type: <code class="language-plaintext highlighter-rouge">DidChangeWatchedFilesClientCapabilities</code> defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#didChangeWatchedFilesClientCapabilities" name="didChangeWatchedFilesClientCapabilities" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">DidChangeWatchedFilesClientCapabilities</span> <span class="p">{</span>
	<span class="cm">/**
	 * Did change watched files notification supports dynamic registration.
	 * Please note that the current protocol doesn't support static
	 * configuration for file changes from the server side.
	 */</span>
	<span class="nl">dynamicRegistration</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>

	<span class="cm">/**
	 * Whether the client has support for relative patterns
	 * or not.
	 *
	 * @since 3.17.0
	 */</span>
	<span class="nl">relativePatternSupport</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Registration Options</em>: <code class="language-plaintext highlighter-rouge">DidChangeWatchedFilesRegistrationOptions</code> defined as follows:</p>

<div class="anchorHolder"><a href="#didChangeWatchedFilesRegistrationOptions" name="didChangeWatchedFilesRegistrationOptions" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * Describe options to be used when registering for file system change events.
 */</span>
<span class="k">export</span> <span class="kr">interface</span> <span class="nx">DidChangeWatchedFilesRegistrationOptions</span> <span class="p">{</span>
	<span class="cm">/**
	 * The watchers to register.
	 */</span>
	<span class="nl">watchers</span><span class="p">:</span> <span class="nx">FileSystemWatcher</span><span class="p">[];</span>
<span class="p">}</span>
</code></pre></div></div>

<div class="anchorHolder"><a href="#pattern" name="pattern" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * The glob pattern to watch relative to the base path. Glob patterns can have
 * the following syntax:
 * - `*` to match one or more characters in a path segment
 * - `?` to match on one character in a path segment
 * - `**` to match any number of path segments, including none
 * - `{}` to group conditions (e.g. `**​/*.{ts,js}` matches all TypeScript
 *   and JavaScript files)
 * - `[]` to declare a range of characters to match in a path segment
 *   (e.g., `example.[0-9]` to match on `example.0`, `example.1`, …)
 * - `[!...]` to negate a range of characters to match in a path segment
 *   (e.g., `example.[!0-9]` to match on `example.a`, `example.b`,
 *   but not `example.0`)
 *
 * @since 3.17.0
 */</span>
<span class="k">export</span> <span class="kd">type</span> <span class="nx">Pattern</span> <span class="o">=</span> <span class="kr">string</span><span class="p">;</span>
</code></pre></div></div>

<div class="anchorHolder"><a href="#relativePattern" name="relativePattern" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * A relative pattern is a helper to construct glob patterns that are matched
 * relatively to a base URI. The common value for a `baseUri` is a workspace
 * folder root, but it can be another absolute URI as well.
 *
 * @since 3.17.0
 */</span>
<span class="k">export</span> <span class="kr">interface</span> <span class="nx">RelativePattern</span> <span class="p">{</span>
	<span class="cm">/**
	 * A workspace folder or a base URI to which this pattern will be matched
	 * against relatively.
	 */</span>
	<span class="nl">baseUri</span><span class="p">:</span> <span class="nx">WorkspaceFolder</span> <span class="o">|</span> <span class="nx">URI</span><span class="p">;</span>

	<span class="cm">/**
	 * The actual glob pattern;
	 */</span>
	<span class="nl">pattern</span><span class="p">:</span> <span class="nx">Pattern</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<div class="anchorHolder"><a href="#globPattern" name="globPattern" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * The glob pattern. Either a string pattern or a relative pattern.
 *
 * @since 3.17.0
 */</span>
<span class="k">export</span> <span class="kd">type</span> <span class="nx">GlobPattern</span> <span class="o">=</span> <span class="nx">Pattern</span> <span class="o">|</span> <span class="nx">RelativePattern</span><span class="p">;</span>
</code></pre></div></div>

<div class="anchorHolder"><a href="#fileSystemWatcher" name="fileSystemWatcher" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">FileSystemWatcher</span> <span class="p">{</span>
	<span class="cm">/**
	 * The glob pattern to watch. See {@link GlobPattern glob pattern}
	 * for more detail.
	 *
 	 * @since 3.17.0 support for relative patterns.
	 */</span>
	<span class="nl">globPattern</span><span class="p">:</span> <span class="nx">GlobPattern</span><span class="p">;</span>

	<span class="cm">/**
	 * The kind of events of interest. If omitted it defaults
	 * to WatchKind.Create | WatchKind.Change | WatchKind.Delete
	 * which is 7.
	 */</span>
	<span class="nl">kind</span><span class="p">?:</span> <span class="nx">WatchKind</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<div class="anchorHolder"><a href="#watchKind" name="watchKind" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="k">namespace</span> <span class="nx">WatchKind</span> <span class="p">{</span>
	<span class="cm">/**
	 * Interested in create events.
	 */</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">Create</span> <span class="o">=</span> <span class="mi">1</span><span class="p">;</span>

	<span class="cm">/**
	 * Interested in change events
	 */</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">Change</span> <span class="o">=</span> <span class="mi">2</span><span class="p">;</span>

	<span class="cm">/**
	 * Interested in delete events
	 */</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">Delete</span> <span class="o">=</span> <span class="mi">4</span><span class="p">;</span>
<span class="p">}</span>
<span class="k">export</span> <span class="kd">type</span> <span class="nx">WatchKind</span> <span class="o">=</span> <span class="nx">uinteger</span><span class="p">;</span>
</code></pre></div></div>

<p><em>Notification</em>:</p>
<ul>
  <li>method: ‘workspace/didChangeWatchedFiles’</li>
  <li>params: <code class="language-plaintext highlighter-rouge">DidChangeWatchedFilesParams</code> defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#didChangeWatchedFilesParams" name="didChangeWatchedFilesParams" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="kr">interface</span> <span class="nx">DidChangeWatchedFilesParams</span> <span class="p">{</span>
	<span class="cm">/**
	 * The actual file events.
	 */</span>
	<span class="nl">changes</span><span class="p">:</span> <span class="nx">FileEvent</span><span class="p">[];</span>
<span class="p">}</span>
</code></pre></div></div>

<p>Where FileEvents are described as follows:</p>

<div class="anchorHolder"><a href="#fileEvent" name="fileEvent" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * An event describing a file change.
 */</span>
<span class="kr">interface</span> <span class="nx">FileEvent</span> <span class="p">{</span>
	<span class="cm">/**
	 * The file's URI.
	 */</span>
	<span class="nl">uri</span><span class="p">:</span> <span class="nx">DocumentUri</span><span class="p">;</span>
	<span class="cm">/**
	 * The change type.
	 */</span>
	<span class="nl">type</span><span class="p">:</span> <span class="nx">uinteger</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<div class="anchorHolder"><a href="#fileChangeType" name="fileChangeType" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * The file event type.
 */</span>
<span class="k">export</span> <span class="k">namespace</span> <span class="nx">FileChangeType</span> <span class="p">{</span>
	<span class="cm">/**
	 * The file got created.
	 */</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">Created</span> <span class="o">=</span> <span class="mi">1</span><span class="p">;</span>
	<span class="cm">/**
	 * The file got changed.
	 */</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">Changed</span> <span class="o">=</span> <span class="mi">2</span><span class="p">;</span>
	<span class="cm">/**
	 * The file got deleted.
	 */</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">Deleted</span> <span class="o">=</span> <span class="mi">3</span><span class="p">;</span>
<span class="p">}</span>

<span class="k">export</span> <span class="kd">type</span> <span class="nx">FileChangeType</span> <span class="o">=</span> <span class="mi">1</span> <span class="o">|</span> <span class="mi">2</span> <span class="o">|</span> <span class="mi">3</span><span class="p">;</span>
</code></pre></div></div>

<h4 id="execute-a-command-leftwards_arrow_with_hook"><a href="#workspace_executeCommand" name="workspace_executeCommand" class="anchor">Execute a command (<img class="emoji" title=":leftwards_arrow_with_hook:" alt=":leftwards_arrow_with_hook:" src="https://github.githubassets.com/images/icons/emoji/unicode/21a9.png" height="20" width="20">)</a></h4>

<p>The <code class="language-plaintext highlighter-rouge">workspace/executeCommand</code> request is sent from the client to the server to trigger command execution on the server. In most cases the server creates a <code class="language-plaintext highlighter-rouge">WorkspaceEdit</code> structure and applies the changes to the workspace using the request <code class="language-plaintext highlighter-rouge">workspace/applyEdit</code> which is sent from the server to the client.</p>

<p><em>Client Capability</em>:</p>
<ul>
  <li>property path (optional): <code class="language-plaintext highlighter-rouge">workspace.executeCommand</code>
</li>
  <li>property type: <code class="language-plaintext highlighter-rouge">ExecuteCommandClientCapabilities</code> defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#executeCommandClientCapabilities" name="executeCommandClientCapabilities" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">ExecuteCommandClientCapabilities</span> <span class="p">{</span>
	<span class="cm">/**
	 * Execute command supports dynamic registration.
	 */</span>
	<span class="nl">dynamicRegistration</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Server Capability</em>:</p>
<ul>
  <li>property path (optional): <code class="language-plaintext highlighter-rouge">executeCommandProvider</code>
</li>
  <li>property type: <code class="language-plaintext highlighter-rouge">ExecuteCommandOptions</code> defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#executeCommandOptions" name="executeCommandOptions" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">ExecuteCommandOptions</span> <span class="kd">extends</span> <span class="nx">WorkDoneProgressOptions</span> <span class="p">{</span>
	<span class="cm">/**
	 * The commands to be executed on the server
	 */</span>
	<span class="nl">commands</span><span class="p">:</span> <span class="kr">string</span><span class="p">[];</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Registration Options</em>: <code class="language-plaintext highlighter-rouge">ExecuteCommandRegistrationOptions</code> defined as follows:</p>

<div class="anchorHolder"><a href="#executeCommandRegistrationOptions" name="executeCommandRegistrationOptions" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * Execute command registration options.
 */</span>
<span class="k">export</span> <span class="kr">interface</span> <span class="nx">ExecuteCommandRegistrationOptions</span>
	<span class="kd">extends</span> <span class="nx">ExecuteCommandOptions</span> <span class="p">{</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Request</em>:</p>
<ul>
  <li>method: ‘workspace/executeCommand’</li>
  <li>params: <code class="language-plaintext highlighter-rouge">ExecuteCommandParams</code> defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#executeCommandParams" name="executeCommandParams" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">ExecuteCommandParams</span> <span class="kd">extends</span> <span class="nx">WorkDoneProgressParams</span> <span class="p">{</span>

	<span class="cm">/**
	 * The identifier of the actual command handler.
	 */</span>
	<span class="nl">command</span><span class="p">:</span> <span class="kr">string</span><span class="p">;</span>
	<span class="cm">/**
	 * Arguments that the command should be invoked with.
	 */</span>
	<span class="nl">arguments</span><span class="p">?:</span> <span class="nx">LSPAny</span><span class="p">[];</span>
<span class="p">}</span>
</code></pre></div></div>

<p>The arguments are typically specified when a command is returned from the server to the client. Example requests that return a command are <code class="language-plaintext highlighter-rouge">textDocument/codeAction</code> or <code class="language-plaintext highlighter-rouge">textDocument/codeLens</code>.</p>

<p><em>Response</em>:</p>
<ul>
  <li>result: <code class="language-plaintext highlighter-rouge">LSPAny</code>
</li>
  <li>error: code and message set in case an exception happens during the request.</li>
</ul>

<h4 id="applies-a-workspaceedit-arrow_right_hook"><a href="#workspace_applyEdit" name="workspace_applyEdit" class="anchor">Applies a WorkspaceEdit (<img class="emoji" title=":arrow_right_hook:" alt=":arrow_right_hook:" src="https://github.githubassets.com/images/icons/emoji/unicode/21aa.png" height="20" width="20">)</a></h4>

<p>The <code class="language-plaintext highlighter-rouge">workspace/applyEdit</code> request is sent from the server to the client to modify resource on the client side.</p>

<p><em>Client Capability</em>:</p>
<ul>
  <li>property path (optional): <code class="language-plaintext highlighter-rouge">workspace.applyEdit</code>
</li>
  <li>property type: <code class="language-plaintext highlighter-rouge">boolean</code>
</li>
</ul>

<p>See also the <a href="#workspaceEditClientCapabilities">WorkspaceEditClientCapabilities</a> for the supported capabilities of a workspace edit.</p>

<p><em>Request</em>:</p>
<ul>
  <li>method: ‘workspace/applyEdit’</li>
  <li>params: <code class="language-plaintext highlighter-rouge">ApplyWorkspaceEditParams</code> defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#applyWorkspaceEditParams" name="applyWorkspaceEditParams" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">ApplyWorkspaceEditParams</span> <span class="p">{</span>
	<span class="cm">/**
	 * An optional label of the workspace edit. This label is
	 * presented in the user interface for example on an undo
	 * stack to undo the workspace edit.
	 */</span>
	<span class="nl">label</span><span class="p">?:</span> <span class="kr">string</span><span class="p">;</span>

	<span class="cm">/**
	 * The edits to apply.
	 */</span>
	<span class="nl">edit</span><span class="p">:</span> <span class="nx">WorkspaceEdit</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Response</em>:</p>
<ul>
  <li>result: <code class="language-plaintext highlighter-rouge">ApplyWorkspaceEditResult</code> defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#applyWorkspaceEditResult" name="applyWorkspaceEditResult" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">ApplyWorkspaceEditResult</span> <span class="p">{</span>
	<span class="cm">/**
	 * Indicates whether the edit was applied or not.
	 */</span>
	<span class="nl">applied</span><span class="p">:</span> <span class="nx">boolean</span><span class="p">;</span>

	<span class="cm">/**
	 * An optional textual description for why the edit was not applied.
	 * This may be used by the server for diagnostic logging or to provide
	 * a suitable error for a request that triggered the edit.
	 */</span>
	<span class="nl">failureReason</span><span class="p">?:</span> <span class="kr">string</span><span class="p">;</span>

	<span class="cm">/**
	 * Depending on the client's failure handling strategy `failedChange`
	 * might contain the index of the change that failed. This property is
	 * only available if the client signals a `failureHandling` strategy
	 * in its client capabilities.
	 */</span>
	<span class="nl">failedChange</span><span class="p">?:</span> <span class="nx">uinteger</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>
<ul>
  <li>error: code and message set in case an exception happens during the request.</li>
</ul>

<h3 id="window-features"><a href="#windowFeatures" name="windowFeatures" class="anchor">Window Features</a></h3>

<h4 id="showmessage-notification-arrow_left"><a href="#window_showMessage" name="window_showMessage" class="anchor">ShowMessage Notification (<img class="emoji" title=":arrow_left:" alt=":arrow_left:" src="https://github.githubassets.com/images/icons/emoji/unicode/2b05.png" height="20" width="20">)</a></h4>

<p>The show message notification is sent from a server to a client to ask the client to display a particular message in the user interface.</p>

<p><em>Notification</em>:</p>
<ul>
  <li>method: ‘window/showMessage’</li>
  <li>params: <code class="language-plaintext highlighter-rouge">ShowMessageParams</code> defined as follows:</li>
</ul>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="kr">interface</span> <span class="nx">ShowMessageParams</span> <span class="p">{</span>
	<span class="cm">/**
	 * The message type. See {@link MessageType}.
	 */</span>
	<span class="nl">type</span><span class="p">:</span> <span class="nx">MessageType</span><span class="p">;</span>

	<span class="cm">/**
	 * The actual message.
	 */</span>
	<span class="nl">message</span><span class="p">:</span> <span class="kr">string</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<p>Where the type is defined as follows:</p>

<div class="anchorHolder"><a href="#messageType" name="messageType" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="k">namespace</span> <span class="nx">MessageType</span> <span class="p">{</span>
	<span class="cm">/**
	 * An error message.
	 */</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nb">Error</span> <span class="o">=</span> <span class="mi">1</span><span class="p">;</span>
	<span class="cm">/**
	 * A warning message.
	 */</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">Warning</span> <span class="o">=</span> <span class="mi">2</span><span class="p">;</span>
	<span class="cm">/**
	 * An information message.
	 */</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">Info</span> <span class="o">=</span> <span class="mi">3</span><span class="p">;</span>
	<span class="cm">/**
	 * A log message.
	 */</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">Log</span> <span class="o">=</span> <span class="mi">4</span><span class="p">;</span>
	<span class="cm">/**
	 * A debug message.
	 *
	 * @since 3.18.0
	 * @proposed
	 */</span>
	<span class="k">export</span> <span class="kd">const</span> <span class="nx">Debug</span> <span class="o">=</span> <span class="mi">5</span><span class="p">;</span>
<span class="p">}</span>

<span class="k">export</span> <span class="kd">type</span> <span class="nx">MessageType</span> <span class="o">=</span> <span class="mi">1</span> <span class="o">|</span> <span class="mi">2</span> <span class="o">|</span> <span class="mi">3</span> <span class="o">|</span> <span class="mi">4</span> <span class="o">|</span> <span class="mi">5</span><span class="p">;</span>
</code></pre></div></div>

<h4 id="showmessage-request-arrow_right_hook"><a href="#window_showMessageRequest" name="window_showMessageRequest" class="anchor">ShowMessage Request (<img class="emoji" title=":arrow_right_hook:" alt=":arrow_right_hook:" src="https://github.githubassets.com/images/icons/emoji/unicode/21aa.png" height="20" width="20">)</a></h4>

<p>The show message request is sent from a server to a client to ask the client to display a particular message in the user interface. In addition to the show message notification the request allows to pass actions and to wait for an answer from the client.</p>

<p><em>Client Capability</em>:</p>
<ul>
  <li>property path (optional): <code class="language-plaintext highlighter-rouge">window.showMessage</code>
</li>
  <li>property type: <code class="language-plaintext highlighter-rouge">ShowMessageRequestClientCapabilities</code> defined as follows:</li>
</ul>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * Show message request client capabilities
 */</span>
<span class="k">export</span> <span class="kr">interface</span> <span class="nx">ShowMessageRequestClientCapabilities</span> <span class="p">{</span>
	<span class="cm">/**
	 * Capabilities specific to the `MessageActionItem` type.
	 */</span>
	<span class="nl">messageActionItem</span><span class="p">?:</span> <span class="p">{</span>
		<span class="cm">/**
		 * Whether the client supports additional attributes which
		 * are preserved and sent back to the server in the
		 * request's response.
		 */</span>
		<span class="nx">additionalPropertiesSupport</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>
	<span class="p">};</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Request</em>:</p>
<ul>
  <li>method: ‘window/showMessageRequest’</li>
  <li>params: <code class="language-plaintext highlighter-rouge">ShowMessageRequestParams</code> defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#showMessageRequestParams" name="showMessageRequestParams" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="kr">interface</span> <span class="nx">ShowMessageRequestParams</span> <span class="p">{</span>
	<span class="cm">/**
	 * The message type. See {@link MessageType}
	 */</span>
	<span class="nl">type</span><span class="p">:</span> <span class="nx">MessageType</span><span class="p">;</span>

	<span class="cm">/**
	 * The actual message
	 */</span>
	<span class="nl">message</span><span class="p">:</span> <span class="kr">string</span><span class="p">;</span>

	<span class="cm">/**
	 * The message action items to present.
	 */</span>
	<span class="nl">actions</span><span class="p">?:</span> <span class="nx">MessageActionItem</span><span class="p">[];</span>
<span class="p">}</span>
</code></pre></div></div>

<p>Where the <code class="language-plaintext highlighter-rouge">MessageActionItem</code> is defined as follows:</p>

<div class="anchorHolder"><a href="#messageActionItem" name="messageActionItem" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="kr">interface</span> <span class="nx">MessageActionItem</span> <span class="p">{</span>
	<span class="cm">/**
	 * A short title like 'Retry', 'Open Log' etc.
	 */</span>
	<span class="nl">title</span><span class="p">:</span> <span class="kr">string</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Response</em>:</p>
<ul>
  <li>result: the selected <code class="language-plaintext highlighter-rouge">MessageActionItem</code> | <code class="language-plaintext highlighter-rouge">null</code> if none got selected.</li>
  <li>error: code and message set in case an exception happens during showing a message.</li>
</ul>

<h4 id="show-document-request-arrow_right_hook"><a href="#window_showDocument" name="window_showDocument" class="anchor">Show Document Request (<img class="emoji" title=":arrow_right_hook:" alt=":arrow_right_hook:" src="https://github.githubassets.com/images/icons/emoji/unicode/21aa.png" height="20" width="20">)</a></h4>

<blockquote>
  <p>New in version 3.16.0</p>
</blockquote>

<p>The show document request is sent from a server to a client to ask the client to display a particular resource referenced by a URI in the user interface.</p>

<p><em>Client Capability</em>:</p>
<ul>
  <li>property path (optional): <code class="language-plaintext highlighter-rouge">window.showDocument</code>
</li>
  <li>property type: <code class="language-plaintext highlighter-rouge">ShowDocumentClientCapabilities</code> defined as follows:</li>
</ul>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * Client capabilities for the show document request.
 *
 * @since 3.16.0
 */</span>
<span class="k">export</span> <span class="kr">interface</span> <span class="nx">ShowDocumentClientCapabilities</span> <span class="p">{</span>
	<span class="cm">/**
	 * The client has support for the show document
	 * request.
	 */</span>
	<span class="nl">support</span><span class="p">:</span> <span class="nx">boolean</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Request</em>:</p>
<ul>
  <li>method: ‘window/showDocument’</li>
  <li>params: <code class="language-plaintext highlighter-rouge">ShowDocumentParams</code> defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#showDocumentParams" name="showDocumentParams" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * Params to show a resource.
 *
 * @since 3.16.0
 */</span>
<span class="k">export</span> <span class="kr">interface</span> <span class="nx">ShowDocumentParams</span> <span class="p">{</span>
	<span class="cm">/**
	 * The uri to show.
	 */</span>
	<span class="nl">uri</span><span class="p">:</span> <span class="nx">URI</span><span class="p">;</span>

	<span class="cm">/**
	 * Indicates to show the resource in an external program.
	 * To show, for example, `https://code.visualstudio.com/`
	 * in the default WEB browser set `external` to `true`.
	 */</span>
	<span class="nl">external</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>

	<span class="cm">/**
	 * An optional property to indicate whether the editor
	 * showing the document should take focus or not.
	 * Clients might ignore this property if an external
	 * program is started.
	 */</span>
	<span class="nl">takeFocus</span><span class="p">?:</span> <span class="nx">boolean</span><span class="p">;</span>

	<span class="cm">/**
	 * An optional selection range if the document is a text
	 * document. Clients might ignore the property if an
	 * external program is started or the file is not a text
	 * file.
	 */</span>
	<span class="nl">selection</span><span class="p">?:</span> <span class="nx">Range</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Response</em>:</p>

<ul>
  <li>result: <code class="language-plaintext highlighter-rouge">ShowDocumentResult</code> defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#showDocumentResult" name="showDocumentResult" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="cm">/**
 * The result of an show document request.
 *
 * @since 3.16.0
 */</span>
<span class="k">export</span> <span class="kr">interface</span> <span class="nx">ShowDocumentResult</span> <span class="p">{</span>
	<span class="cm">/**
	 * A boolean indicating if the show was successful.
	 */</span>
	<span class="nl">success</span><span class="p">:</span> <span class="nx">boolean</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>
<ul>
  <li>error: code and message set in case an exception happens during showing a document.</li>
</ul>

<h4 id="logmessage-notification-arrow_left"><a href="#window_logMessage" name="window_logMessage" class="anchor">LogMessage Notification (<img class="emoji" title=":arrow_left:" alt=":arrow_left:" src="https://github.githubassets.com/images/icons/emoji/unicode/2b05.png" height="20" width="20">)</a></h4>

<p>The log message notification is sent from the server to the client to ask the client to log a particular message.</p>

<p><em>Notification</em>:</p>
<ul>
  <li>method: ‘window/logMessage’</li>
  <li>params: <code class="language-plaintext highlighter-rouge">LogMessageParams</code> defined as follows:</li>
</ul>

<div class="anchorHolder"><a href="#logMessageParams" name="logMessageParams" class="linkableAnchor"></a></div>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="kr">interface</span> <span class="nx">LogMessageParams</span> <span class="p">{</span>
	<span class="cm">/**
	 * The message type. See {@link MessageType}
	 */</span>
	<span class="nl">type</span><span class="p">:</span> <span class="nx">MessageType</span><span class="p">;</span>

	<span class="cm">/**
	 * The actual message
	 */</span>
	<span class="nl">message</span><span class="p">:</span> <span class="kr">string</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<h4 id="-create-work-done-progress-arrow_right_hook"><a href="#window_workDoneProgress_create" name="window_workDoneProgress_create" class="anchor"> Create Work Done Progress (<img class="emoji" title=":arrow_right_hook:" alt=":arrow_right_hook:" src="https://github.githubassets.com/images/icons/emoji/unicode/21aa.png" height="20" width="20">)</a></h4>

<p>The <code class="language-plaintext highlighter-rouge">window/workDoneProgress/create</code> request is sent from the server to the client to ask the client to create a work done progress.</p>

<p><em>Client Capability</em>:</p>
<ul>
  <li>property name (optional): <code class="language-plaintext highlighter-rouge">window.workDoneProgress</code>
</li>
  <li>property type: <code class="language-plaintext highlighter-rouge">boolean</code>
</li>
</ul>

<p><em>Request</em>:</p>

<ul>
  <li>method: ‘window/workDoneProgress/create’</li>
  <li>params: <code class="language-plaintext highlighter-rouge">WorkDoneProgressCreateParams</code> defined as follows:</li>
</ul>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">WorkDoneProgressCreateParams</span> <span class="p">{</span>
	<span class="cm">/**
	 * The token to be used to report progress.
	 */</span>
	<span class="nl">token</span><span class="p">:</span> <span class="nx">ProgressToken</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>

<p><em>Response</em>:</p>

<ul>
  <li>result: void</li>
  <li>error: code and message set in case an exception happens during the ‘window/workDoneProgress/create’ request. In case an error occurs a server must not send any progress notification using the token provided in the <code class="language-plaintext highlighter-rouge">WorkDoneProgressCreateParams</code>.</li>
</ul>

<h4 id="-cancel-a-work-done-progress-arrow_right"><a href="#window_workDoneProgress_cancel" name="window_workDoneProgress_cancel" class="anchor"> Cancel a Work Done Progress (<img class="emoji" title=":arrow_right:" alt=":arrow_right:" src="https://github.githubassets.com/images/icons/emoji/unicode/27a1.png" height="20" width="20">)</a></h4>

<p>The <code class="language-plaintext highlighter-rouge">window/workDoneProgress/cancel</code> notification is sent from the client to the server to cancel a progress initiated on the server side using the <code class="language-plaintext highlighter-rouge">window/workDoneProgress/create</code>. The progress need not be marked as <code class="language-plaintext highlighter-rouge">cancellable</code> to be cancelled and a client may cancel a progress for any number of reasons: in case of error, reloading a workspace etc.</p>

<p><em>Notification</em>:</p>

<ul>
  <li>method: ‘window/workDoneProgress/cancel’</li>
  <li>params: <code class="language-plaintext highlighter-rouge">WorkDoneProgressCancelParams</code> defined as follows:</li>
</ul>

<div class="language-typescript highlighter-rouge"><div class="highlight"><pre class="highlight"><code><span class="k">export</span> <span class="kr">interface</span> <span class="nx">WorkDoneProgressCancelParams</span> <span class="p">{</span>
	<span class="cm">/**
	 * The token to be used to report progress.
	 */</span>
	<span class="nl">token</span><span class="p">:</span> <span class="nx">ProgressToken</span><span class="p">;</span>
<span class="p">}</span>
</code></pre></div></div>
<h4 id="telemetry-notification-arrow_left"><a href="#telemetry_event" name="telemetry_event" class="anchor">Telemetry Notification (<img class="emoji" title=":arrow_left:" alt=":arrow_left:" src="https://github.githubassets.com/images/icons/emoji/unicode/2b05.png" height="20" width="20">)</a></h4>

<p>The telemetry notification is sent from the server to the client to ask the client to log a telemetry event. The protocol doesn’t specify the payload since no interpretation of the data happens in the protocol. Most clients even don’t handle the event directly but forward them to the extensions owing the corresponding server issuing the event.</p>

<p><em>Notification</em>:</p>
<ul>
  <li>method: ‘telemetry/event’</li>
  <li>params: ‘object’ | ‘array’;</li>
</ul>

<h4 id="miscellaneous"><a href="#miscellaneous" name="miscellaneous" class="anchor">Miscellaneous</a></h4>

<h4 id="implementation-considerations-1"><a href="#implementationConsiderations" name="implementationConsiderations" class="anchor">Implementation Considerations</a></h4>

<p>Language servers usually run in a separate process and clients communicate with them in an asynchronous fashion. Additionally clients usually allow users to interact with the source code even if request results are pending. We recommend the following implementation pattern to avoid that clients apply outdated response results:</p>

<ul>
  <li>if a client sends a request to the server and the client state changes in a way that it invalidates the response it should do the following:
    <ul>
      <li>cancel the server request and ignore the result if the result is not useful for the client anymore. If necessary the client should resend the request.</li>
      <li>keep the request running if the client can still make use of the result by, for example, transforming it to a new result by applying the state change to the result.</li>
    </ul>
  </li>
  <li>servers should therefore not decide by themselves to cancel requests simply due to that fact that a state change notification is detected in the queue. As said the result could still be useful for the client.</li>
  <li>if a server detects an internal state change (for example, a project context changed) that invalidates the result of a request in execution the server can error these requests with <code class="language-plaintext highlighter-rouge">ContentModified</code>. If clients receive a <code class="language-plaintext highlighter-rouge">ContentModified</code> error, it generally should not show it in the UI for the end-user. Clients can resend the request if they know how to do so. It should be noted that for all position based requests it might be especially hard for clients to re-craft a request.</li>
  <li>a client should not send resolve requests for out of date objects (for example, code lenses, …). If a server receives a resolve request for an out of date object the server can error these requests with <code class="language-plaintext highlighter-rouge">ContentModified</code>.</li>
  <li>if a client notices that a server exits unexpectedly, it should try to restart the server. However clients should be careful not to restart a crashing server endlessly. VS Code, for example, doesn’t restart a server which has crashed 5 times in the last 180 seconds.</li>
</ul>

<p>Servers usually support different communication channels (e.g. stdio, pipes, …). To ease the usage of servers in different clients it is highly recommended that a server implementation supports the following command line arguments to pick the communication channel:</p>

<ul>
  <li>
<strong>stdio</strong>: uses stdio as the communication channel.</li>
  <li>
<strong>pipe</strong>: use pipes (Windows) or socket files (Linux, Mac) as the communication channel. The pipe / socket file name is passed as the next arg or with <code class="language-plaintext highlighter-rouge">--pipe=</code>.</li>
  <li>
<strong>socket</strong>: uses a socket as the communication channel. The port is passed as next arg or with <code class="language-plaintext highlighter-rouge">--port=</code>.</li>
  <li>
<strong>node-ipc</strong>: use node IPC communication between the client and the server. This is only supported if both client and server run under node.</li>
</ul>

<p>To support the case that the editor starting a server crashes an editor should also pass its process id to the server. This allows the server to monitor the editor process and to shutdown itself if the editor process dies. The process id passed on the command line should be the same as the one passed in the initialize parameters. The command line argument to use is <code class="language-plaintext highlighter-rouge">--clientProcessId</code>.</p>

<h4 id="meta-model"><a href="#metaModel" name="metaModel" class="anchor">Meta Model</a></h4>

<p>Since 3.17 there is a meta model describing the LSP protocol:</p>

<ul>
  <li>
<a href="../metaModel/metaModel.json">metaModel.json</a>: The actual meta model for the LSP 3.17 specification</li>
  <li>
<a href="../metaModel/metaModel.ts">metaModel.ts</a>: A TypeScript file defining the data types that make up the meta model.</li>
  <li>
<a href="../metaModel/metaModel.schema.json">metaModel.schema.json</a>: A JSON schema file defining the data types that make up the meta model. Can be used to generate code to read the meta model JSON file.</li>
</ul>

<h3 id="change-log"><a href="#changeLog" name="changeLog" class="anchor">Change Log</a></h3>

<h4 id="3170-05102022"><a href="#version_3_17_0" name="version_3_17_0" class="anchor">3.17.0 (05/10/2022)</a></h4>

<ul>
  <li>Specify how clients will handle stale requests.</li>
  <li>Add support for a completion item label details.</li>
  <li>Add support for workspace symbol resolve request.</li>
  <li>Add support for label details and insert text mode on completion items.</li>
  <li>Add support for shared values on CompletionItemList.</li>
  <li>Add support for HTML tags in Markdown.</li>
  <li>Add support for collapsed text in folding.</li>
  <li>Add support for trigger kinds on code action requests.</li>
  <li>Add the following support to semantic tokens:
    <ul>
      <li>server cancelable</li>
      <li>augmentation of syntax tokens</li>
    </ul>
  </li>
  <li>Add support to negotiate the position encoding.</li>
  <li>Add support for relative patterns in file watchers.</li>
  <li>Add support for type hierarchies</li>
  <li>Add support for inline values.</li>
  <li>Add support for inlay hints.</li>
  <li>Add support for notebook documents.</li>
  <li>Add support for diagnostic pull model.</li>
</ul>

<h4 id="3160-12142020"><a href="#version_3_16_0" name="version_3_16_0" class="anchor">3.16.0 (12/14/2020)</a></h4>

<ul>
  <li>Add support for tracing.</li>
  <li>Add semantic token support.</li>
  <li>Add call hierarchy support.</li>
  <li>Add client capability for resolving text edits on completion items.</li>
  <li>Add support for client default behavior on renames.</li>
  <li>Add support for insert and replace ranges on <code class="language-plaintext highlighter-rouge">CompletionItem</code>.</li>
  <li>Add support for diagnostic code descriptions.</li>
  <li>Add support for document symbol provider label.</li>
  <li>Add support for tags on <code class="language-plaintext highlighter-rouge">SymbolInformation</code> and <code class="language-plaintext highlighter-rouge">DocumentSymbol</code>.</li>
  <li>Add support for moniker request method.</li>
  <li>Add support for code action <code class="language-plaintext highlighter-rouge">data</code> property.</li>
  <li>Add support for code action <code class="language-plaintext highlighter-rouge">disabled</code> property.</li>
  <li>Add support for code action resolve request.</li>
  <li>Add support for diagnostic <code class="language-plaintext highlighter-rouge">data</code> property.</li>
  <li>Add support for signature information <code class="language-plaintext highlighter-rouge">activeParameter</code> property.</li>
  <li>Add support for <code class="language-plaintext highlighter-rouge">workspace/didCreateFiles</code> notifications and <code class="language-plaintext highlighter-rouge">workspace/willCreateFiles</code> requests.</li>
  <li>Add support for <code class="language-plaintext highlighter-rouge">workspace/didRenameFiles</code> notifications and <code class="language-plaintext highlighter-rouge">workspace/willRenameFiles</code> requests.</li>
  <li>Add support for <code class="language-plaintext highlighter-rouge">workspace/didDeleteFiles</code> notifications and <code class="language-plaintext highlighter-rouge">workspace/willDeleteFiles</code> requests.</li>
  <li>Add client capability to signal whether the client normalizes line endings.</li>
  <li>Add support to preserve additional attributes on <code class="language-plaintext highlighter-rouge">MessageActionItem</code>.</li>
  <li>Add support to provide the clients locale in the initialize call.</li>
  <li>Add support for opening and showing a document in the client user interface.</li>
  <li>Add support for linked editing.</li>
  <li>Add support for change annotations in text edits as well as in create file, rename file and delete file operations.</li>
</ul>

<h4 id="3150-01142020"><a href="#version_3_15_0" name="version_3_15_0" class="anchor">3.15.0 (01/14/2020)</a></h4>

<ul>
  <li>Add generic progress reporting support.</li>
  <li>Add specific work done progress reporting support to requests where applicable.</li>
  <li>Add specific partial result progress support to requests where applicable.</li>
  <li>Add support for <code class="language-plaintext highlighter-rouge">textDocument/selectionRange</code>.</li>
  <li>Add support for server and client information.</li>
  <li>Add signature help context.</li>
  <li>Add Erlang and Elixir to the list of supported programming languages</li>
  <li>Add <code class="language-plaintext highlighter-rouge">version</code> on <code class="language-plaintext highlighter-rouge">PublishDiagnosticsParams</code>
</li>
  <li>Add <code class="language-plaintext highlighter-rouge">CodeAction#isPreferred</code> support.</li>
  <li>Add <code class="language-plaintext highlighter-rouge">CompletionItem#tag</code> support.</li>
  <li>Add <code class="language-plaintext highlighter-rouge">Diagnostic#tag</code> support.</li>
  <li>Add <code class="language-plaintext highlighter-rouge">DocumentLink#tooltip</code> support.</li>
  <li>Add <code class="language-plaintext highlighter-rouge">trimTrailingWhitespace</code>, <code class="language-plaintext highlighter-rouge">insertFinalNewline</code> and <code class="language-plaintext highlighter-rouge">trimFinalNewlines</code> to <code class="language-plaintext highlighter-rouge">FormattingOptions</code>.</li>
  <li>Clarified <code class="language-plaintext highlighter-rouge">WorkspaceSymbolParams#query</code> parameter.</li>
</ul>

<h4 id="3140-12132018"><a href="#version_3_14_0" name="version_3_14_0" class="anchor">3.14.0 (12/13/2018)</a></h4>

<ul>
  <li>Add support for signature label offsets.</li>
  <li>Add support for location links.</li>
  <li>Add support for <code class="language-plaintext highlighter-rouge">textDocument/declaration</code> request.</li>
</ul>

<h4 id="3130-9112018"><a href="#version_3_13_0" name="version_3_13_0" class="anchor">3.13.0 (9/11/2018)</a></h4>

<ul>
  <li>Add support for file and folder operations (create, rename, move) to workspace edits.</li>
</ul>

<h4 id="3120-8232018"><a href="#version_3_12_0" name="version_3_12_0" class="anchor">3.12.0 (8/23/2018)</a></h4>

<ul>
  <li>Add support for <code class="language-plaintext highlighter-rouge">textDocument/prepareRename</code> request.</li>
</ul>

<h4 id="3110-8212018"><a href="#version_3_11_0" name="version_3_11_0" class="anchor">3.11.0 (8/21/2018)</a></h4>

<ul>
  <li>Add support for CodeActionOptions to allow a server to provide a list of code action it supports.</li>
</ul>

<h4 id="3100-7232018"><a href="#version_3_10_0" name="version_3_10_0" class="anchor">3.10.0 (7/23/2018)</a></h4>

<ul>
  <li>Add support for hierarchical document symbols as a valid response to a <code class="language-plaintext highlighter-rouge">textDocument/documentSymbol</code> request.</li>
  <li>Add support for folding ranges as a valid response to a <code class="language-plaintext highlighter-rouge">textDocument/foldingRange</code> request.</li>
</ul>

<h4 id="390-7102018"><a href="#version_3_9_0" name="version_3_9_0" class="anchor">3.9.0 (7/10/2018)</a></h4>

<ul>
  <li>Add support for <code class="language-plaintext highlighter-rouge">preselect</code> property in <code class="language-plaintext highlighter-rouge">CompletionItem</code>
</li>
</ul>

<h4 id="380-6112018"><a href="#version_3_8_0" name="version_3_8_0" class="anchor">3.8.0 (6/11/2018)</a></h4>

<ul>
  <li>Added support for CodeAction literals to the <code class="language-plaintext highlighter-rouge">textDocument/codeAction</code> request.</li>
  <li>ColorServerCapabilities.colorProvider can also be a boolean</li>
  <li>Corrected ColorPresentationParams.colorInfo to color (as in the <code class="language-plaintext highlighter-rouge">d.ts</code> and in implementations)</li>
</ul>

<h4 id="370-452018"><a href="#version_3_7_0" name="version_3_7_0" class="anchor">3.7.0 (4/5/2018)</a></h4>

<ul>
  <li>Added support for related information to Diagnostics.</li>
</ul>

<h4 id="360-2222018"><a href="#version_3_6_0" name="version_3_6_0" class="anchor">3.6.0 (2/22/2018)</a></h4>

<p>Merge the proposed protocol for workspace folders, configuration, go to type definition, go to implementation and document color provider into the main branch of the specification. For details see:</p>

<ul>
  <li><a href="https://microsoft.github.io/language-server-protocol/specification#workspace_workspaceFolders">Get Workspace Folders</a></li>
  <li><a href="https://microsoft.github.io/language-server-protocol/specification#workspace_didChangeWorkspaceFolders">DidChangeWorkspaceFolders Notification</a></li>
  <li><a href="https://microsoft.github.io/language-server-protocol/specification#workspace_configuration">Get Configuration</a></li>
  <li><a href="https://microsoft.github.io/language-server-protocol/specification#textDocument_typeDefinition">Go to Type Definition</a></li>
  <li><a href="https://microsoft.github.io/language-server-protocol/specification#textDocument_implementation">Go to Implementation</a></li>
  <li><a href="https://microsoft.github.io/language-server-protocol/specification#textDocument_documentColor">Document Color</a></li>
  <li><a href="https://microsoft.github.io/language-server-protocol/specification#textDocument_colorPresentation">Color Presentation</a></li>
</ul>

<p>In addition we enhanced the <code class="language-plaintext highlighter-rouge">CompletionTriggerKind</code> with a new value <code class="language-plaintext highlighter-rouge">TriggerForIncompleteCompletions: 3 = 3</code> to signal the a completion request got trigger since the last result was incomplete.</p>

<h4 id="350"><a href="#version_3_5_0" name="version_3_5_0" class="anchor">3.5.0</a></h4>

<p>Decided to skip this version to bring the protocol version number in sync the with npm module vscode-languageserver-protocol.</p>

<h4 id="340-11272017"><a href="#version_3_4_0" name="version_3_4_0" class="anchor">3.4.0 (11/27/2017)</a></h4>

<ul>
  <li><a href="https://github.com/Microsoft/language-server-protocol/issues/129">extensible completion item and symbol kinds</a></li>
</ul>

<h4 id="330-11242017"><a href="version_3_3_0" name="version_3_3_0" class="anchor">3.3.0 (11/24/2017)</a></h4>

<ul>
  <li>Added support for <code class="language-plaintext highlighter-rouge">CompletionContext</code>
</li>
  <li>Added support for <code class="language-plaintext highlighter-rouge">MarkupContent</code>
</li>
  <li>Removed old New and Updated markers.</li>
</ul>

<h4 id="320-09262017"><a href="version_3_2_0" name="version_3_2_0" class="anchor">3.2.0 (09/26/2017)</a></h4>

<ul>
  <li>Added optional <code class="language-plaintext highlighter-rouge">commitCharacters</code> property to the <code class="language-plaintext highlighter-rouge">CompletionItem</code>
</li>
</ul>

<h4 id="310-02282017"><a href="version_3_1_0" name="version_3_1_0" class="anchor">3.1.0 (02/28/2017)</a></h4>

<ul>
  <li>Make the <code class="language-plaintext highlighter-rouge">WorkspaceEdit</code> changes backwards compatible.</li>
  <li>Updated the specification to correctly describe the breaking changes from 2.x to 3.x around <code class="language-plaintext highlighter-rouge">WorkspaceEdit</code>and <code class="language-plaintext highlighter-rouge">TextDocumentEdit</code>.</li>
</ul>

<h4 id="30-version"><a href="#version_3_0_0" name="version_3_0_0" class="anchor">3.0 Version</a></h4>

<ul>
  <li>add support for client feature flags to support that servers can adapt to different client capabilities. An example is the new <code class="language-plaintext highlighter-rouge">textDocument/willSaveWaitUntil</code> request which not all clients might be able to support. If the feature is disabled in the client capabilities sent on the initialize request, the server can’t rely on receiving the request.</li>
  <li>add support to experiment with new features. The new <code class="language-plaintext highlighter-rouge">ClientCapabilities.experimental</code> section together with feature flags allow servers to provide experimental feature without the need of ALL clients to adopt them immediately.</li>
  <li>servers can more dynamically react to client features. Capabilities can now be registered and unregistered after the initialize request using the new <code class="language-plaintext highlighter-rouge">client/registerCapability</code> and <code class="language-plaintext highlighter-rouge">client/unregisterCapability</code>. This for example allows servers to react to settings or configuration changes without a restart.</li>
  <li>add support for <code class="language-plaintext highlighter-rouge">textDocument/willSave</code> notification and <code class="language-plaintext highlighter-rouge">textDocument/willSaveWaitUntil</code> request.</li>
  <li>add support for <code class="language-plaintext highlighter-rouge">textDocument/documentLink</code> request.</li>
  <li>add a <code class="language-plaintext highlighter-rouge">rootUri</code> property to the initializeParams in favor of the <code class="language-plaintext highlighter-rouge">rootPath</code> property.</li>
</ul>
</div>
        </div>

        <div class="d-none col-lg-3 d-lg-block lsp-sidebar">
            
            <div class="card">
                <div id="side-sections-accordion" data-children=".item">
                    
                    <div class="item">
                        <a class="nav-link" data-toggle="collapse" data-parent="#side-sections-accordion" href="#baseProtocol-side" aria-expanded="true">
                            Base Protocol
                        </a>
                        <div id="baseProtocol-side" class="collapse" role="tabpanel">
                            <ul class="nav toc flex-column" style="padding-left: 0.5rem">
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#headerPart">
                                        Header Part
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#contentPart">
                                        Content Part
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#baseTypes">
                                        Base Types
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#requestMessage">
                                        Request Message
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#responseMessage">
                                        Response Message
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#notificationMessage">
                                        Notification Message
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#cancelRequest">
                                        Cancellation Support
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#progress">
                                        Progress Support
                                    </a>
                                </li>
                                
                            </ul>
                        </div>
                    </div>
                    
                    <div class="item">
                        <a class="nav-link" data-toggle="collapse" data-parent="#side-sections-accordion" href="#languageServerProtocol-side" aria-expanded="true">
                            Language Server Protocol
                        </a>
                        <div id="languageServerProtocol-side" class="collapse" role="tabpanel">
                            <ul class="nav toc flex-column" style="padding-left: 0.5rem">
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#languageServerProtocol">
                                        Overview
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#capabilities">
                                        Capabilities
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#messageOrdering">
                                        Message Ordering
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#messageDocumentation">
                                        Message Documentation
                                    </a>
                                </li>
                                
                            </ul>
                        </div>
                    </div>
                    
                    <div class="item">
                        <a class="nav-link" data-toggle="collapse" data-parent="#side-sections-accordion" href="#basicJsonStructures-side" aria-expanded="true">
                            Basic JSON Structures
                        </a>
                        <div id="basicJsonStructures-side" class="collapse" role="tabpanel">
                            <ul class="nav toc flex-column" style="padding-left: 0.5rem">
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#uri">
                                        URI
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#regExp">
                                        Regular Expression
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#enumerations">
                                        Enumerations
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#textDocuments">
                                        Text Documents
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#position">
                                        Position
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#range">
                                        Range
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#textDocumentItem">
                                        Text Document Item
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#textDocumentIdentifier">
                                        Text Document Identifier
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#versionedTextDocumentIdentifier">
                                        Versioned Text Document Identifier
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#textDocumentPositionParams">
                                        Text Document Position Params
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#documentFilter">
                                        Document Filter
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#textEdit">
                                        Text Edit
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#textEditArray">
                                        Text Edit Array
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#textDocumentEdit">
                                        Text Document Edit
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#location">
                                        Location
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#locationLink">
                                        Location Link
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#diagnostic">
                                        Diagnostic
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#command">
                                        Command
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#markupContent">
                                        Markup Content
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#resourceChanges">
                                        File Resource Changes
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#workspaceEdit">
                                        Workspace Edit
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#workDoneProgress">
                                        Work Done Progress
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#clientInitiatedProgress">
                                        Client Initiated Progress
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#serverInitiatedProgress">
                                        Server Initiated Progress
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#partialResults">
                                        Partial Results
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#partialResultParams">
                                        Partial Result Params
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#traceValue">
                                        Trace Value
                                    </a>
                                </li>
                                
                            </ul>
                        </div>
                    </div>
                    
                    <div class="item">
                        <a class="nav-link" data-toggle="collapse" data-parent="#side-sections-accordion" href="#lifeCycleMessages-side" aria-expanded="true">
                            Lifecycle Messages
                        </a>
                        <div id="lifeCycleMessages-side" class="collapse" role="tabpanel">
                            <ul class="nav toc flex-column" style="padding-left: 0.5rem">
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#lifeCycleMessages">
                                        Overview
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#initialize">
                                        Initialize
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#initialized">
                                        Initialized
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#client_registerCapability">
                                        Register Capability
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#client_unregisterCapability">
                                        Unregister Capability
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#setTrace">
                                        Set Trace
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#logTrace">
                                        Log Trace
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#shutdown">
                                        Shutdown
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#exit">
                                        Exit
                                    </a>
                                </li>
                                
                            </ul>
                        </div>
                    </div>
                    
                    <div class="item">
                        <a class="nav-link" data-toggle="collapse" data-parent="#side-sections-accordion" href="#textSynchronization-side" aria-expanded="true">
                            Document Synchronization
                        </a>
                        <div id="textSynchronization-side" class="collapse" role="tabpanel">
                            <ul class="nav toc flex-column" style="padding-left: 0.5rem">
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#textDocument_synchronization">
                                        Overview - Text Document
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#textDocument_didOpen">
                                        Did Open Text Document
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#textDocument_didChange">
                                        Did Change Text Document
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#textDocument_willSave">
                                        Will Save Text Document
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#textDocument_willSaveWaitUntil">
                                        Will Save Document Wait Until
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#textDocument_didSave">
                                        Did Save Text Document
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#textDocument_didClose">
                                        Did Close Text Document
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#textDocument_didRename">
                                        Rename a Text Document
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#notebookDocument_synchronization">
                                        Overview - Notebook Document
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#notebookDocument_didOpen">
                                        Did Open Notebook Document
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#notebookDocument_didChange">
                                        Did Change Notebook Document
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#notebookDocument_didSave">
                                        Did Save Notebook Document
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#notebookDocument_didClose">
                                        Did Close Notebook Document
                                    </a>
                                </li>
                                
                            </ul>
                        </div>
                    </div>
                    
                    <div class="item">
                        <a class="nav-link" data-toggle="collapse" data-parent="#side-sections-accordion" href="#languageFeatures-side" aria-expanded="true">
                            Language Features
                        </a>
                        <div id="languageFeatures-side" class="collapse" role="tabpanel">
                            <ul class="nav toc flex-column" style="padding-left: 0.5rem">
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#languageFeatures">
                                        Overview
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#textDocument_declaration">
                                        Go to Declaration
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#textDocument_definition">
                                        Go to Definition
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#textDocument_typeDefinition">
                                        Go to Type Definition
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#textDocument_implementation">
                                        Go to Implementation
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#textDocument_references">
                                        Find References
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#textDocument_prepareCallHierarchy">
                                        Prepare Call Hierarchy
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#callHierarchy_incomingCalls">
                                        Call Hierarchy Incoming Calls
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#callHierarchy_outgoingCalls">
                                        Call Hierarchy Outgoing Calls
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#textDocument_prepareTypeHierarchy">
                                        Prepare Type Hierarchy
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#typeHierarchy_supertypes">
                                        Type Hierarchy Super Types
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#typeHierarchy_subtypes">
                                        Type Hierarchy Sub Types
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#textDocument_documentHighlight">
                                        Document Highlight
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#textDocument_documentLink">
                                        Document Link
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#documentLink_resolve">
                                        Document Link Resolve
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#textDocument_hover">
                                        Hover
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#textDocument_codeLens">
                                        Code Lens
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#codeLens_refresh">
                                        Code Lens Refresh
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#textDocument_foldingRange">
                                        Folding Range
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#textDocument_selectionRange">
                                        Selection Range
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#textDocument_documentSymbol">
                                        Document Symbols
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#textDocument_semanticTokens">
                                        Semantic Tokens
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#textDocument_inlineValue">
                                        Inline Value
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#workspace_inlineValue_refresh">
                                        Inline Value Refresh
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#textDocument_inlayHint">
                                        Inlay Hint
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#inlayHint_resolve">
                                        Inlay Hint Resolve
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#workspace_inlayHint_refresh">
                                        Inlay Hint Refresh
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#textDocument_moniker">
                                        Moniker
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#textDocument_completion">
                                        Completion Proposals
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#completionItem_resolve">
                                        Completion Item Resolve
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#textDocument_publishDiagnostics">
                                        Publish Diagnostics
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#textDocument_pullDiagnostics">
                                        Pull Diagnostics
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#textDocument_signatureHelp">
                                        Signature Help
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#textDocument_codeAction">
                                        Code Action
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#codeAction_resolve">
                                        Code Action Resolve
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#textDocument_documentColor">
                                        Document Color
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#textDocument_colorPresentation">
                                        Color Presentation
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#textDocument_formatting">
                                        Formatting
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#textDocument_rangeFormatting">
                                        Range Formatting
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#textDocument_onTypeFormatting">
                                        On type Formatting
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#textDocument_rename">
                                        Rename
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#textDocument_prepareRename">
                                        Prepare Rename
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#textDocument_linkedEditingRange">
                                        Linked Editing Range
                                    </a>
                                </li>
                                
                            </ul>
                        </div>
                    </div>
                    
                    <div class="item">
                        <a class="nav-link" data-toggle="collapse" data-parent="#side-sections-accordion" href="#workspaceFeatures-side" aria-expanded="true">
                            Workspace Features
                        </a>
                        <div id="workspaceFeatures-side" class="collapse" role="tabpanel">
                            <ul class="nav toc flex-column" style="padding-left: 0.5rem">
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#workspace_symbol">
                                        Workspace Symbols
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#workspace_symbolResolve">
                                        Workspace Symbol Resolve
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#workspace_configuration">
                                        Get Configuration
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#workspace_didChangeConfiguration">
                                        Did Change Configuration
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#workspace_workspaceFolders">
                                        Workspace Folders
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#workspace_didChangeWorkspaceFolders">
                                        Did Change Workspace Folders
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#workspace_willCreateFiles">
                                        Will Create Files
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#workspace_didCreateFiles">
                                        Did Create Files
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#workspace_willRenameFiles">
                                        Will Rename Files
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#workspace_didRenameFiles">
                                        Did Rename Files
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#workspace_willDeleteFiles">
                                        Will Delete Files
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#workspace_didDeleteFiles">
                                        Did Delete Files
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#workspace_didChangeWatchedFiles">
                                        Did Change Watched Files
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#workspace_executeCommand">
                                        Execute Command
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#workspace_applyEdit">
                                        Apply Edit
                                    </a>
                                </li>
                                
                            </ul>
                        </div>
                    </div>
                    
                    <div class="item">
                        <a class="nav-link" data-toggle="collapse" data-parent="#side-sections-accordion" href="#windowFeatures-side" aria-expanded="true">
                            Window Features
                        </a>
                        <div id="windowFeatures-side" class="collapse" role="tabpanel">
                            <ul class="nav toc flex-column" style="padding-left: 0.5rem">
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#window_showMessage">
                                        Show Message Notification
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#window_showMessageRequest">
                                        Show Message Request
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#window_logMessage">
                                        Log Message
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#window_showDocument">
                                        Show Document
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#window_workDoneProgress_create">
                                        Create Work Done Progress
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#window_workDoneProgress_cancel">
                                        Cancel a Work Done Progress
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#telemetry_event">
                                        Sent Telemetry
                                    </a>
                                </li>
                                
                            </ul>
                        </div>
                    </div>
                    
                    <div class="item">
                        <a class="nav-link" data-toggle="collapse" data-parent="#side-sections-accordion" href="#miscellaneous-side" aria-expanded="true">
                            Miscellaneous
                        </a>
                        <div id="miscellaneous-side" class="collapse" role="tabpanel">
                            <ul class="nav toc flex-column" style="padding-left: 0.5rem">
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#implementationConsiderations">
                                        Implementation Considerations
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#metaModel">
                                        Meta Model
                                    </a>
                                </li>
                                
                            </ul>
                        </div>
                    </div>
                    
                    <div class="item">
                        <a class="nav-link" data-toggle="collapse" data-parent="#side-sections-accordion" href="#changeLog-side" aria-expanded="true">
                            Change Log
                        </a>
                        <div id="changeLog-side" class="collapse" role="tabpanel">
                            <ul class="nav toc flex-column" style="padding-left: 0.5rem">
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#version_3_17_0">
                                        3.17.0
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#version_3_16_0">
                                        3.16.0
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#version_3_15_0">
                                        3.15.0
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#version_3_14_0">
                                        3.14.0
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#version_3_13_0">
                                        3.13.0
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#version_3_12_0">
                                        3.12.0
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#version_3_11_0">
                                        3.11.0
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#version_3_10_0">
                                        3.10.0
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#version_3_9_0">
                                        3.9.0
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#version_3_8_0">
                                        3.8.0
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#version_3_7_0">
                                        3.7.0
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#version_3_6_0">
                                        3.6.0
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#version_3_5_0">
                                        3.5.0
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#version_3_4_0">
                                        3.4.0
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#version_3_3_0">
                                        3.3.0
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#version_3_2_0">
                                        3.2.0
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#version_3_1_0">
                                        3.1.0
                                    </a>
                                </li>
                                
                                <li class="nav-item">
                                    <a class="nav-link toc-link" href="#version_3_0_0">
                                        3.0
                                    </a>
                                </li>
                                
                            </ul>
                        </div>
                    </div>
                    
                </div>
            </div>
            
        </div>
    </div>
</div>

        </div>
    </div>

    <footer class="footer">
    <script>
        function manageConsent() {
            if (WcpConsent.siteConsent.isConsentRequired) {
                WcpConsent.siteConsent.manageConsent();
            }
        }
    </script>

    <div class="container">
        <div class="row">
            <div class="col-sm-10">
                <ul class="links">
                    <li>
                        <span class="message">Hello from Seattle and Zürich.</span>
                    </li>
                    <li>
                        <a class="github-button" href="https://github.com/Microsoft/language-server-protocol" data-icon="octicon-star" data-show-count="true" aria-label="Star Microsoft/language-server-protocol on GitHub">Star</a>
                    </li>
                    <li>
                        <a class="github-button" href="https://github.com/Microsoft/language-server-protocol/subscription" aria-label="Watch Microsoft/language-server-protocol on GitHub">Watch</a>
                    </li>
                </ul>
            </div>
            <div class="col-sm-2">
                <ul class="links">
                    <li style="display: none;">
                        <a id="footer-cookie-link" style="cursor: pointer; padding-right:20px" onclick="manageConsent()" aria-label="Manage cookies">Manage cookies</a>
                    </li>
                    <li>
                        <div class="copyright">
                            <a id="footer-microsoft-link" class="logo" href="https://www.microsoft.com">
                                <picture>
                                    <source srcset="/language-server-protocol/img/microsoft-logo.png" media="(prefers-color-scheme: dark)"></source>
                                    <img src="/language-server-protocol/img/microsoft-logo-inverted.png" height="20" alt="Microsoft">
                                </picture>
                            </a>
                            <span>© 2023 Microsoft</span>
                        </div>
                    </li>
                </ul>
            </div>
        </div>
    </div>
</footer>

    <script>
  var baseurl = '/language-server-protocol';
</script>

<script src="https://code.jquery.com/jquery-3.2.1.slim.min.js" integrity="sha384-KJ3o2DKtIkvYIK3UENzmM7KCkRr/rE9/Qpg6aAZGJwFDMVNA/GpGFF93hXpG5KkN" crossorigin="anonymous"></script>

<!-- Microsoft EU cookie compliance library -->
<script src="https://wcpstatic.microsoft.com/mscc/lib/v2/wcp-consent.js"></script>
<!-- Global site tag (gtag.js) - Google Analytics -->
<script async src="https://www.googletagmanager.com/gtag/js?id=UA-62780441-30"></script>

<script src="https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.12.3/umd/popper.min.js" integrity="sha384-vFJXuSJphROIrBnz7yo7oB41mKfc8JzQZiCq4NCceLEaO4IHwicKwpJf9c9IpFgh" crossorigin="anonymous"></script>
<script src="https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0-beta.2/js/bootstrap.min.js" integrity="sha384-alpBpkh1PFOepccYVYDB4do5UnbKysX5WZXm3XxPqe5iKTfUKjNkCk9SaVuEZflJ" crossorigin="anonymous"></script>
<script async defer src="https://buttons.github.io/buttons.js"></script>
<script src="/language-server-protocol/js/page.js"></script>


<script type="text/javascript">
  var linkableTypes = new Map([
  ["integer", "#integer"],
    ["uinteger", "#uinteger"],
    ["decimal", "#decimal"],
    ["BaseAny", "#baseAny"],
    ["LSPAny", "#lspAny"],
    ["BaseObject", "#baseObject"],
    ["LSPObject", "#lspObject"],
    ["BaseArray", "#baseArray"],
    ["LSPArray", "#lspArray"],
    ["Message", "#message"],
    ["RequestMessage", "#requestMessage"],
    ["ResponseMessage", "#responseMessage"],
    ["ResponseError", "#responseError"],
    ["ErrorCodes", "#errorCodes"],
    ["NotificationMessage", "#notificationMessage"],
    ["CancelParams", "#cancelRequest"],
    ["ProgressParams", "#progress"],
    ["ProgressToken", "#progress"],
    ["Uri", "#uri"],
    ["URI", "#uri"],
    ["DocumentUri", "#documentUri"],
    ["RegularExpressionsClientCapabilities", "#regExp"],
    ["TextDocument", "#textDocuments"],
    ["EOL", "#textDocuments"],
    ["Position", "#position"],
    ["PositionEncodingKind", "#positionEncodingKind"],
    ["Range", "#range"],
    ["Location", "#location"],
    ["LocationLink", "#locationLink"],
    ["Diagnostic", "#diagnostic"],
    ["DiagnosticSeverity", "#diagnosticSeverity"],
    ["DiagnosticTag", "#diagnosticTag"],
    ["DiagnosticRelatedInformation", "#diagnosticRelatedInformation"],
    ["CodeDescription", "#codeDescription"],
    ["Command", "#command"],
    ["TextEdit", "#textEdit"],
    ["ChangeAnnotation", "#changeAnnotation"],
    ["ChangeAnnotationIdentifier", "#changeAnnotationIdentifier"],
    ["AnnotatedTextEdit", "#annotatedTextEdit"],
    ["TextDocumentEdit", "#textDocumentEdit"],
    ["CreateFileOptions", "#createFileOptions"],
    ["CreateFile", "#createFile"],
    ["RenameFileOptions", "#renameFileOptions"],
    ["RenameFile", "#renameFile"],
    ["DeleteFileOptions", "#deleteFileOptions"],
    ["DeleteFile", "#deleteFile"],
    ["WorkspaceEdit", "#workspaceEdit"],
    ["WorkspaceEditClientCapabilities", "#workspaceEditClientCapabilities"],
    ["ResourceOperationKind", "#resourceOperationKind"],
    ["FailureHandlingKind", "#failureHandlingKind"],
    ["TextDocumentIdentifier", "#textDocumentIdentifier"],
    ["TextDocumentItem", "#textDocumentItem"],
    ["VersionedTextDocumentIdentifier", "#versionedTextDocumentIdentifier"],
    ["OptionalVersionedTextDocumentIdentifier", "#optionalVersionedTextDocumentIdentifier"],
    ["TextDocumentPositionParams", "#textDocumentPositionParams"],
    ["DocumentFilter", "#documentFilter"],
    ["DocumentSelector", "#documentSelector"],
    ["StaticRegistrationOptions", "#staticRegistrationOptions"],
    ["TextDocumentRegistrationOptions", "#textDocumentRegistrationOptions"],
    ["MarkupKind", "#markupContent"],
    ["MarkupContent", "#markupContentInnerDefinition"],
    ["MarkdownClientCapabilities", "#markdownClientCapabilities"],
    ["WorkDoneProgressBegin", "#workDoneProgressBegin"],
    ["$/progress", "#workDoneProgressBegin"],
    ["WorkDoneProgressReport", "#workDoneProgressReport"],
    ["WorkDoneProgressEnd", "#workDoneProgressEnd"],
    ["WorkDoneProgressParams", "#workDoneProgressParams"],
    ["WorkDoneProgressOptions", "#workDoneProgressOptions"],
    ["PartialResultParams", "#partialResultParams"],
    ["TraceValue", "#traceValue"],
    ["InitializeParams", "#initializeParams"],
    ["InitializeResult", "#initializeResult"],
    ["InitializedParams", "#initialized"],
    ["InitializeErrorCodes", "#initializeErrorCodes"],
    ["InitializeError", "#initializeError"],
    ["TextDocumentClientCapabilities", "#textDocumentClientCapabilities"],
    ["NotebookDocumentClientCapabilities", "#notebookDocumentClientCapabilities"],
    ["ClientCapabilities", "#clientCapabilities"],
    ["ServerCapabilities", "#serverCapabilities"],
    ["LogTraceParams", "#logTrace"],
    ["SetTraceParams", "#setTrace"],
    ["ShowMessageParams", "#window_showMessage"],
    ["MessageType", "#messageType"],
    ["ShowMessageRequestClientCapabilities", "#window_showMessageRequest"],
    ["ShowMessageRequestParams", "#showMessageRequestParams"],
    ["MessageActionItem", "#messageActionItem"],
    ["ShowDocumentClientCapabilities", "#window_showDocument"],
    ["ShowDocumentParams", "#showDocumentParams"],
    ["ShowDocumentResult", "#showDocumentResult"],
    ["LogMessageParams", "#logMessageParams"],
    ["WorkDoneProgressCreateParams", "#window_workDoneProgress_create"],
    ["WorkDoneProgressCancelParams", "#window_workDoneProgress_cancel"],
    ["telemetry/event", "#telemetry_event"],
    ["Registration", "#registration"],
    ["RegistrationParams", "#registrationParams"],
    ["Unregistration", "#unregistration"],
    ["UnregistrationParams", "#unregistrationParams"],
    ["WorkspaceFoldersServerCapabilities", "#workspaceFoldersServerCapabilities"],
    ["WorkspaceFolder", "#workspaceFolder"],
    ["workspace/workspaceFolders", "#workspace_workspaceFolders"],
    ["DidChangeWorkspaceFoldersParams", "#didChangeWorkspaceFoldersParams"],
    ["WorkspaceFoldersChangeEvent", "#workspaceFoldersChangeEvent"],
    ["DidChangeConfigurationClientCapabilities", "#didChangeConfigurationClientCapabilities"],
    ["DidChangeConfigurationParams", "#didChangeConfigurationParams"],
    ["ConfigurationParams", "#configurationParams"],
    ["ConfigurationItem", "#configurationItem"],
    ["DidChangeWatchedFilesClientCapabilities", "#didChangeWatchedFilesClientCapabilities"],
    ["DidChangeWatchedFilesRegistrationOptions", "#didChangeWatchedFilesRegistrationOptions"],
    ["Pattern", "#pattern"],
    ["RelativePattern", "#relativePattern"],
    ["GlobPattern", "#globPattern"],
    ["FileSystemWatcher", "#fileSystemWatcher"],
    ["WatchKind", "#watchKind"],
    ["DidChangeWatchedFilesParams", "#didChangeWatchedFilesParams"],
    ["FileEvent", "#fileEvent"],
    ["FileChangeType", "#fileChangeType"],
    ["WorkspaceSymbolClientCapabilities", "#workspace_symbol"],
    ["WorkspaceSymbolOptions", "#workspaceSymbolOptions"],
    ["WorkspaceSymbolRegistrationOptions", "#workspaceSymbolRegistrationOptions"],
    ["WorkspaceSymbolParams", "#workspaceSymbolParams"],
    ["WorkspaceSymbol", "#workspaceSymbol"],
    ["workspace/executeCommand", "#workspace_executeCommand"],
    ["ExecuteCommandClientCapabilities", "#executeCommandClientCapabilities"],
    ["ExecuteCommandOptions", "#executeCommandOptions"],
    ["ExecuteCommandRegistrationOptions", "#executeCommandRegistrationOptions"],
    ["ExecuteCommandParams", "#executeCommandParams"],
    ["workspace/applyEdit", "#workspace_applyEdit"],
    ["ApplyWorkspaceEditParams", "#applyWorkspaceEditParams"],
    ["ApplyWorkspaceEditResult", "#applyWorkspaceEditResult"],
    ["ApplyWorkspaceEditResponse", "#applyWorkspaceEditResponse"],
    ["workspace/willCreateFiles", "#workspace_willCreateFiles"],
    ["FileOperationRegistrationOptions", "#fileOperationRegistrationOptions"],
    ["FileOperationPatternKind", "#fileOperationPatternKind"],
    ["FileOperationPatternOptions", "#fileOperationPatternOptions"],
    ["FileOperationPattern", "#fileOperationPattern"],
    ["FileOperationFilter", "#fileOperationFilter"],
    ["CreateFilesParams", "#createFilesParams"],
    ["FileCreate", "#fileCreate"],
    ["workspace/didCreateFiles", "#workspace_didCreateFiles"],
    ["workspace/willRenameFiles", "#workspace_willRenameFiles"],
    ["RenameFilesParams", "#renameFilesParams"],
    ["FileRename", "#fileRename"],
    ["workspace/didRenameFiles", "#workspace_didRenameFiles"],
    ["workspace/willDeleteFiles", "#workspace_willDeleteFiles"],
    ["DeleteFilesParams", "#deleteFilesParams"],
    ["FileDelete", "#fileDelete"],
    ["workspace/didDeleteFiles", "#workspace_didDeleteFiles"],
    ["TextDocumentSyncKind", "#textDocumentSyncKind"],
    ["TextDocumentSyncOptions", "#textDocumentSyncOptions"],
    ["textDocument/didOpen", "#textDocument_didOpen"],
    ["DidOpenTextDocumentParams", "#didOpenTextDocumentParams"],
    ["textDocument/didChange", "#textDocument_didChange"],
    ["TextDocumentChangeRegistrationOptions", "#textDocumentChangeRegistrationOptions"],
    ["DidChangeTextDocumentParams", "#didChangeTextDocumentParams"],
    ["TextDocumentContentChangeEvent", "#textDocumentContentChangeEvent"],
    ["textDocument/willSave", "#textDocument_willSave"],
    ["WillSaveTextDocumentParams", "#willSaveTextDocumentParams"],
    ["TextDocumentSaveReason", "#textDocumentSaveReason"],
    ["textDocument/willSaveWaitUntil", "#textDocument_willSaveWaitUntil"],
    ["textDocument/didSave", "#textDocument_didSave"],
    ["SaveOptions", "#saveOptions"],
    ["TextDocumentSaveRegistrationOptions", "#textDocumentSaveRegistrationOptions"],
    ["DidSaveTextDocumentParams", "#didSaveTextDocumentParams"],
    ["textDocument/didClose", "#textDocument_didClose"],
    ["DidCloseTextDocumentParams", "#didCloseTextDocumentParams"],
    ["TextDocumentSyncClientCapabilities", "#textDocumentSyncClientCapabilities"],
    ["TextDocumentSyncKind", "#textDocumentSyncKind"],
    ["TextDocumentSyncOptions", "#textDocumentSyncOptions"],
    ["textDocument/publishDiagnostics", "#textDocument_publishDiagnostics"],
    ["PublishDiagnosticsClientCapabilities", "#publishDiagnosticsClientCapabilities"],
    ["PublishDiagnosticsParams", "#publishDiagnosticsParams"],
    ["textDocument/completion", "#textDocument_completion"],
    ["CompletionClientCapabilities", "#completionClientCapabilities"],
    ["CompletionOptions", "#completionOptions"],
    ["CompletionRegistrationOptions", "#completionRegistrationOptions"],
    ["CompletionParams", "#completionParams"],
    ["CompletionTriggerKind", "#completionTriggerKind"],
    ["CompletionContext", "#completionContext"],
    ["CompletionList", "#completionList"],
    ["InsertTextFormat", "#insertTextFormat"],
    ["CompletionItemTag", "#completionItemTag"],
    ["InsertReplaceEdit", "#insertReplaceEdit"],
    ["InsertTextMode", "#insertTextMode"],
    ["CompletionItemLabelDetails", "#completionItemLabelDetails"],
    ["CompletionItem", "#completionItem"],
    ["CompletionItemKind", "#completionItemKind"],
    ["completionItem/resolve", "#completionItem_resolve"],
    ["textDocument/hover", "#textDocument_hover"],
    ["HoverClientCapabilities", "#hoverClientCapabilities"],
    ["HoverOptions", "#hoverOptions"],
    ["HoverRegistrationOptions", "#hoverRegistrationOptions"],
    ["HoverParams", "#hoverParams"],
    ["Hover", "#hover"],
    ["MarkedString", "#markedString"],
    ["textDocument/signatureHelp", "#textDocument_signatureHelp"],
    ["SignatureHelpClientCapabilities", "#signatureHelpClientCapabilities"],
    ["SignatureHelpOptions", "#signatureHelpOptions"],
    ["SignatureHelpRegistrationOptions", "#signatureHelpRegistrationOptions"],
    ["SignatureHelpParams", "#signatureHelpParams"],
    ["SignatureHelpTriggerKind", "#signatureHelpTriggerKind"],
    ["SignatureHelpContext", "#signatureHelpContext"],
    ["SignatureHelp", "#signatureHelp"],
    ["SignatureInformation", "#signatureInformation"],
    ["ParameterInformation", "#parameterInformation"],
    ["textDocument/declaration", "#textDocument_declaration"],
    ["DeclarationClientCapabilities", "#declarationClientCapabilities"],
    ["DeclarationOptions", "#declarationOptions"],
    ["DeclarationRegistrationOptions", "#declarationRegistrationOptions"],
    ["DeclarationParams", "#declarationParams"],
    ["textDocument/definition", "#textDocument_definition"],
    ["DefinitionClientCapabilities", "#definitionClientCapabilities"],
    ["DefinitionOptions", "#definitionOptions"],
    ["DefinitionRegistrationOptions", "#definitionRegistrationOptions"],
    ["DefinitionParams", "#definitionParams"],
    ["textDocument/typeDefinition", "#textDocument_typeDefinition"],
    ["TypeDefinitionClientCapabilities", "#typeDefinitionClientCapabilities"],
    ["TypeDefinitionOptions", "#typeDefinitionOptions"],
    ["TypeDefinitionRegistrationOptions", "#typeDefinitionRegistrationOptions"],
    ["TypeDefinitionParams", "#typeDefinitionParams"],
    ["textDocument/implementation", "#textDocument_implementation"],
    ["ImplementationClientCapabilities", "#implementationClientCapabilities"],
    ["ImplementationOptions", "#implementationOptions"],
    ["ImplementationRegistrationOptions", "#implementationRegistrationOptions"],
    ["ImplementationParams", "#implementationParams"],
    ["textDocument/references", "#textDocument_references"],
    ["ReferenceClientCapabilities", "#referenceClientCapabilities"],
    ["ReferenceOptions", "#referenceOptions"],
    ["ReferenceRegistrationOptions", "#referenceRegistrationOptions"],
    ["ReferenceParams", "#referenceParams"],
    ["ReferenceContext", "#referenceContext"],
    ["textDocument/documentHighlight", "#textDocument_documentHighlight"],
    ["DocumentHighlightClientCapabilities", "#documentHighlightClientCapabilities"],
    ["DocumentHighlightOptions", "#documentHighlightOptions"],
    ["DocumentHighlightRegistrationOptions", "#documentHighlightRegistrationOptions"],
    ["DocumentHighlightParams", "#documentHighlightParams"],
    ["DocumentHighlight", "#documentHighlight"],
    ["DocumentHighlightKind", "#documentHighlightKind"],
    ["textDocument/documentSymbol", "#textDocument_documentSymbol"],
    ["DocumentSymbolClientCapabilities", "#documentSymbolClientCapabilities"],
    ["DocumentSymbolOptions", "#documentSymbolOptions"],
    ["DocumentSymbolRegistrationOptions", "#documentSymbolRegistrationOptions"],
    ["DocumentSymbolParams", "#documentSymbolParams"],
    ["SymbolKind", "#symbolKind"],
    ["SymbolTag", "#symbolTag"],
    ["DocumentSymbol", "#documentSymbol"],
    ["SymbolInformation", "#symbolInformation"],
    ["textDocument/codeAction", "#textDocument_codeAction"],
    ["CodeActionClientCapabilities", "#codeActionClientCapabilities"],
    ["CodeActionOptions", "#codeActionOptions"],
    ["CodeActionRegistrationOptions", "#codeActionRegistrationOptions"],
    ["CodeActionParams", "#codeActionParams"],
    ["CodeActionKind", "#codeActionKind"],
    ["CodeActionContext", "#codeActionContext"],
    ["CodeActionTriggerKind", "#codeActionTriggerKind"],
    ["CodeAction", "#codeAction"],
    ["codeAction/resolve", "#codeAction_resolve"],
    ["textDocument/codeLens", "#textDocument_codeLens"],
    ["CodeLensClientCapabilities", "#codeLensClientCapabilities"],
    ["CodeLensOptions", "#codeLensOptions"],
    ["CodeLensRegistrationOptions", "#codeLensRegistrationOptions"],
    ["CodeLensParams", "#codeLensParams"],
    ["CodeLens", "#codeLens"],
    ["codeLens/resolve", "#codeLens_resolve"],
    ["CodeLensWorkspaceClientCapabilities", "#codeLensWorkspaceClientCapabilities"],
    ["workspace/codeLens/refresh", "#codeLens_refresh"],
    ["DocumentLinkClientCapabilities", "#documentLinkClientCapabilities"],
    ["textDocument/documentLink", "#textDocument_documentLink"],
    ["DocumentLinkOptions", "#documentLinkOptions"],
    ["DocumentLinkRegistrationOptions", "#documentLinkRegistrationOptions"],
    ["DocumentLinkParams", "#documentLinkParams"],
    ["DocumentLink", "#documentLink"],
    ["documentLink/resolve", "#documentLink_resolve"],
    ["textDocument/documentColor", "#textDocument_documentColor"],
    ["DocumentColorClientCapabilities", "#documentColorClientCapabilities"],
    ["DocumentColorOptions", "#documentColorOptions"],
    ["DocumentColorRegistrationOptions", "#documentColorRegistrationOptions"],
    ["DocumentColorParams", "#documentColorParams"],
    ["ColorInformation", "#colorInformation"],
    ["Color", "#color"],
    ["textDocument/colorPresentation", "#textDocument_colorPresentation"],
    ["ColorPresentationParams", "#colorPresentationParams"],
    ["ColorPresentation", "#colorPresentation"],
    ["DocumentFormattingClientCapabilities", "#documentFormattingClientCapabilities"],
    ["textDocument/formatting", "#textDocument_formatting"],
    ["DocumentFormattingClientCapabilities", "#documentFormattingClientCapabilities"],
    ["DocumentFormattingOptions", "#documentFormattingOptions"],
    ["DocumentFormattingRegistrationOptions", "#documentFormattingRegistrationOptions"],
    ["DocumentFormattingParams", "#documentFormattingParams"],
    ["FormattingOptions", "#formattingOptions"],
    ["textDocument/rangeFormatting", "#textDocument_rangeFormatting"],
    ["DocumentRangeFormattingClientCapabilities", "#documentRangeFormattingClientCapabilities"],
    ["DocumentRangeFormattingOptions", "#documentRangeFormattingOptions"],
    ["DocumentRangeFormattingRegistrationOptions", "#documentRangeFormattingRegistrationOptions"],
    ["DocumentRangeFormattingParams", "#documentRangeFormattingParams"],
    ["DocumentRangesFormattingParams", "#documentRangesFormattingParams"],
    ["DocumentOnTypeFormattingClientCapabilities", "#documentOnTypeFormattingClientCapabilities"],
    ["textDocument/onTypeFormatting", "#textDocument_onTypeFormatting"],
    ["DocumentOnTypeFormattingOptions", "#documentOnTypeFormattingOptions"],
    ["DocumentOnTypeFormattingRegistrationOptions", "#documentOnTypeFormattingRegistrationOptions"],
    ["DocumentOnTypeFormattingParams", "#documentOnTypeFormattingParams"],
    ["PrepareSupportDefaultBehavior", "#prepareSupportDefaultBehavior"],
    ["RenameClientCapabilities", "#renameClientCapabilities"],
    ["RenameOptions", "#renameOptions"],
    ["RenameRegistrationOptions", "#renameRegistrationOptions"],
    ["textDocument/rename", "#textDocument_rename"],
    ["RenameParams", "#renameParams"],
    ["textDocument/prepareRename", "#textDocument_prepareRename"],
    ["PrepareRenameParams", "#prepareRenameParams"],
    ["textDocument/foldingRange", "#textDocument_foldingRange"],
    ["FoldingRangeClientCapabilities", "#foldingRangeClientCapabilities"],
    ["FoldingRangeOptions", "#foldingRangeOptions"],
    ["FoldingRangeRegistrationOptions", "#foldingRangeRegistrationOptions"],
    ["FoldingRangeParams", "#foldingRangeParams"],
    ["FoldingRangeKind", "#foldingRangeKind"],
    ["FoldingRange", "#foldingRange"],
    ["workspace/foldingRange/refresh", "#workspace_foldingRange_refresh"],
    ["FoldingRangeWorkspaceClientCapabilities", "#foldingRangeWorkspaceClientCapabilities"],
    ["textDocument/selectionRange", "#textDocument_selectionRange"],
    ["SelectionRangeClientCapabilities", "#selectionRangeClientCapabilities"],
    ["SelectionRangeOptions", "#selectionRangeOptions"],
    ["SelectionRangeRegistrationOptions", "#selectionRangeRegistrationOptions"],
    ["SelectionRangeParams", "#selectionRangeParams"],
    ["SelectionRange", "#selectionRange"],
    ["textDocument/prepareCallHierarchy", "#textDocument_prepareCallHierarchy"],
    ["CallHierarchyClientCapabilities", "#callHierarchyClientCapabilities"],
    ["CallHierarchyOptions", "#callHierarchyOptions"],
    ["CallHierarchyRegistrationOptions", "#callHierarchyRegistrationOptions"],
    ["CallHierarchyPrepareParams", "#callHierarchyPrepareParams"],
    ["CallHierarchyItem", "#callHierarchyItem"],
    ["callHierarchy/incomingCalls", "#callHierarchy_incomingCalls"],
    ["CallHierarchyIncomingCallsParams", "#callHierarchyIncomingCallsParams"],
    ["CallHierarchyIncomingCall", "#callHierarchyIncomingCall"],
    ["callHierarchy/outgoingCalls", "#callHierarchy_outgoingCalls"],
    ["CallHierarchyOutgoingCallsParams", "#callHierarchyOutgoingCallsParams"],
    ["CallHierarchyOutgoingCall", "#callHierarchyOutgoingCall"],
    ["SemanticTokenTypes", "#semanticTokenTypes"],
    ["SemanticTokenModifiers", "#semanticTokenModifiers"],
    ["TokenFormat", "#tokenFormat"],
    ["SemanticTokensLegend", "#semanticTokensLegend"],
    ["SemanticTokensClientCapabilities", "#semanticTokensClientCapabilities"],
    ["SemanticTokensOptions", "#semanticTokensOptions"],
    ["SemanticTokensRegistrationOptions", "#semanticTokensRegistrationOptions"],
    ["textDocument/semanticTokens/full", "#semanticTokens_fullRequest"],
    ["SemanticTokensParams", "#semanticTokensParams"],
    ["SemanticTokens", "#semanticTokens"],
    ["SemanticTokensPartialResult", "#semanticTokensPartialResult"],
    ["textDocument/semanticTokens/full/delta", "#semanticTokens_deltaRequest"],
    ["SemanticTokensDeltaParams", "#semanticTokensDeltaParams"],
    ["SemanticTokensDelta", "#semanticTokensDelta"],
    ["SemanticTokensEdit", "#semanticTokensEdit"],
    ["SemanticTokensDeltaPartialResult", "#semanticTokensDeltaPartialResult"],
    ["textDocument/semanticTokens/range", "#semanticTokens_rangeRequest"],
    ["SemanticTokensRangeParams", "#semanticTokensRangeParams"],
    ["SemanticTokensWorkspaceClientCapabilities", "#semanticTokensWorkspaceClientCapabilities"],
    ["workspace/semanticTokens/refresh", "#semanticTokens_refreshRequest"],
    ["textDocument/linkedEditingRange", "#textDocument_linkedEditingRange"],
    ["LinkedEditingRangeClientCapabilities", "#linkedEditingRangeClientCapabilities"],
    ["LinkedEditingRangeOptions", "#linkedEditingRangeOptions"],
    ["LinkedEditingRangeRegistrationOptions", "#linkedEditingRangeRegistrationOptions"],
    ["LinkedEditingRangeParams", "#linkedEditingRangeParams"],
    ["LinkedEditingRanges", "#linkedEditingRanges"],
    ["textDocument/moniker", "#textDocument_moniker"],
    ["MonikerClientCapabilities", "#monikerClientCapabilities"],
    ["MonikerOptions", "#monikerOptions"],
    ["MonikerRegistrationOptions", "#monikerRegistrationOptions"],
    ["MonikerParams", "#monikerParams"],
    ["UniquenessLevel", "#uniquenessLevel"],
    ["MonikerKind", "#monikerKind"],
    ["Moniker", "#moniker"],
    ["TypeHierarchyClientCapabilities", "#typeHierarchyClientCapabilities"],
    ["TypeHierarchyOptions", "#typeHierarchyOptions"],
    ["TypeHierarchyRegistrationOptions", "#typeHierarchyRegistrationOptions"],
    ["TypeHierarchyPrepareParams", "#typeHierarchyPrepareParams"],
    ["TypeHierarchyItem", "#typeHierarchyItem"],
    ["TypeHierarchySupertypesParams", "#typeHierarchySupertypesParams"],
    ["TypeHierarchySubtypesParams", "#typeHierarchySubtypesParams"],
    ["InlayHintClientCapabilities", "#inlayHintClientCapabilities"],
    ["InlayHintOptions", "#inlayHintOptions"],
    ["InlayHintRegistrationOptions", "#inlayHintRegistrationOptions"],
    ["InlayHintParams", "#inlayHintParams"],
    ["InlayHint", "#inlayHint"],
    ["InlayHintLabelPart", "#inlayHintLabelPart"],
    ["InlayHintKind", "#inlayHintKind"],
    ["InlayHintWorkspaceClientCapabilities", "#inlayHintWorkspaceClientCapabilities"],
    ["workspace/inlayHint/refresh", "#workspace_inlayHint_refresh"],
    ["InlineValueClientCapabilities", "#inlineValueClientCapabilities"],
    ["InlineValueOptions", "#inlineValueOptions"],
    ["InlineValueRegistrationOptions", "#inlineValueRegistrationOptions"],
    ["InlineValueParams", "#inlineValueParams"],
    ["InlineValueContext", "#inlineValueContext"],
    ["InlineValueText", "#inlineValueText"],
    ["InlineValueVariableLookup", "#inlineValueVariableLookup"],
    ["InlineValueEvaluatableExpression", "#inlineValueEvaluatableExpression"],
    ["InlineValue", "#inlineValue"],
    ["InlineValueWorkspaceClientCapabilities", "#inlineValueWorkspaceClientCapabilities"],
    ["workspace/inlineValue/refresh", "#workspace_inlineValue_refresh"],
    ["NotebookDocument", "#notebookDocument"],
    ["NotebookCell", "#notebookCell"],
    ["NotebookCellKind", "#notebookCellKind"],
    ["ExecutionSummary", "#executionSummary"],
    ["NotebookCellTextDocumentFilter", "#notebookCellTextDocumentFilter"],
    ["NotebookDocumentFilter", "#notebookDocumentFilter"],
    ["NotebookDocumentSyncClientCapabilities", "#notebookDocumentSyncClientCapabilities"],
    ["NotebookDocumentSyncOptions", "#notebookDocumentSyncOptions"],
    ["NotebookDocumentSyncRegistrationOptions", "#notebookDocumentSyncRegistrationOptions"],
    ["notebookDocument/didOpen", "#notebookDocument_didOpen"],
    ["DidOpenNotebookDocumentParams", "#didOpenNotebookDocumentParams"],
    ["notebookDocument/didChange", "#notebookDocument_didChange"],
    ["DidChangeNotebookDocumentParams", "#didChangeNotebookDocumentParams"],
    ["NotebookDocumentChangeEvent", "#notebookDocumentChangeEvent"],
    ["NotebookCellArrayChange", "#notebookCellArrayChange"],
    ["notebookDocument/didSave", "#notebookDocument_didSave"],
    ["DidSaveNotebookDocumentParams", "#didSaveNotebookDocumentParams"],
    ["notebookDocument/didClose", "#notebookDocument_didClose"],
    ["DidCloseNotebookDocumentParams", "#didCloseNotebookDocumentParams"],
    ["NotebookDocumentIdentifier", "#notebookDocumentIdentifier"],
    ["VersionedNotebookDocumentIdentifier", "#versionedNotebookDocumentIdentifier"],
    ["DiagnosticClientCapabilities", "#diagnosticClientCapabilities"],
    ["DiagnosticWorkspaceClientCapabilities", "#diagnosticWorkspaceClientCapabilities"],
    ["DiagnosticOptions", "#diagnosticOptions"],
    ["DiagnosticRegistrationOptions", "#diagnosticRegistrationOptions"],
    ["DocumentDiagnosticParams", "#documentDiagnosticParams"],
    ["DocumentDiagnosticReport", "#documentDiagnosticReport"],
    ["DocumentDiagnosticReportKind", "#documentDiagnosticReportKind"],
    ["FullDocumentDiagnosticReport", "#fullDocumentDiagnosticReport"],
    ["UnchangedDocumentDiagnosticReport", "#unchangedDocumentDiagnosticReport"],
    ["RelatedFullDocumentDiagnosticReport", "#relatedFullDocumentDiagnosticReport"],
    ["RelatedUnchangedDocumentDiagnosticReport", "#relatedUnchangedDocumentDiagnosticReport"],
    ["DocumentDiagnosticReportPartialResult", "#documentDiagnosticReportPartialResult"],
    ["DiagnosticServerCancellationData", "#diagnosticServerCancellationData"],
    ["WorkspaceDiagnosticParams", "#workspaceDiagnosticParams"],
    ["PreviousResultId", "#previousResultId"],
    ["WorkspaceDiagnosticReport", "#workspaceDiagnosticReport"],
    ["WorkspaceFullDocumentDiagnosticReport", "#workspaceFullDocumentDiagnosticReport"],
    ["WorkspaceUnchangedDocumentDiagnosticReport", "#workspaceUnchangedDocumentDiagnosticReport"],
    ["WorkspaceDocumentDiagnosticReport", "#workspaceDocumentDiagnosticReport"],
    ["WorkspaceDiagnosticReportPartialResult", "#workspaceDiagnosticReportPartialResult"],
    ["workspace/diagnostic/refresh", "#diagnostic_refresh"],
    ["textDocument/inlineCompletion", "#textDocument_inlineCompletion"],
    ["InlineCompletionClientCapabilities", "#inlineCompletionClientCapabilities"],
    ["InlineCompletionOptions", "#inlineCompletionOptions"],
    ["InlineCompletionRegistrationOptions", "#inlineCompletionRegistrationOptions"],
    ["InlineCompletionParams", "#inlineCompletionParams"],
    ["InlineCompletionContext", "#inlineCompletionContext"],
    ["InlineCompletionTriggerKind", "#inlineCompletionTriggerKind"],
    ["SelectedCompletionInfo", "#selectedCompletionInfo"],
    ["InlineCompletionList", "#inlineCompletionList"],
    ["InlineCompletionItem", "#inlineCompletionItem"],
    ]);
</script>




<script type="text/javascript">
  function tryGetAssociatedLink(name) {
    var link = linkableTypes.get(name);

    if (!link) {
      return;
    }

    var anchor = document.createElement("a");
    var hrefUrl = link[0] === '/' ?
      `${baseurl}/specifications${link}` :

      link;

    anchor.setAttribute("href", hrefUrl);
    anchor.textContent = name;
    return anchor;
  }

  var elements = document.querySelectorAll("code .nx, code.highlighter-rouge");
  for (var element of elements) {
    var typeName = element.textContent;
    var linkNode = tryGetAssociatedLink(typeName);

    if (linkNode) {
      element.replaceChildren(linkNode);
    }
  }
</script>

</body>

</html>
let testString=
  `
<!--html-->
let a=10
  
<!--html-->  
`
	|test}

