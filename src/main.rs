use std::any::Any;
use std::collections::HashMap;
use std::fmt::format;
use std::hash::Hash;
use std::iter;

use chiquito::compiler::semantic::analyser::analyse;
use chiquito::compiler::semantic::{AnalysisResult, SymTable};
use chiquito::compiler::Message::{ParseErr, RuntimeErr, SemErr};
use chiquito::parser::ast::debug_sym_factory::DebugSymRefFactory;
use chiquito::parser::ast::tl::TLDecl;
use chiquito::parser::ast::{DebugSymRef, Identifier};
use chiquito::parser::lang::TLDeclsParser;
use chiquito_language_server::chumsky::{Func, ImCompleteSemanticToken};
use chiquito_language_server::completion::completion;
use chiquito_language_server::jump_definition::get_definition;
use chiquito_language_server::range_factory::RangeFactory;
use chiquito_language_server::semantic_token::{semantic_token_from_ast, LEGEND_TYPE};
use chumsky::primitive::Container;
use dashmap::DashMap;
use im_rc::HashSet;
use lalrpop_util::ParseError::{
    ExtraToken, InvalidToken, UnrecognizedEof, UnrecognizedToken, User,
};
use num_bigint::BigInt;
use ropey::Rope;
use serde::{Deserialize, Serialize};
use serde_json::Value;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::notification::Notification;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};
#[derive(Debug)]
struct Backend {
    client: Client,
    sym_table_map: DashMap<String, SymTable>,
    document_map: DashMap<String, Rope>,
    semantic_token_map: DashMap<String, Vec<ImCompleteSemanticToken>>,
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            server_info: None,
            offset_encoding: None,
            capabilities: ServerCapabilities {
                inlay_hint_provider: Some(OneOf::Left(true)),
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                completion_provider: Some(CompletionOptions {
                    resolve_provider: Some(false),
                    trigger_characters: Some(vec![".".to_string()]),
                    work_done_progress_options: Default::default(),
                    all_commit_characters: None,
                    completion_item: None,
                }),
                execute_command_provider: Some(ExecuteCommandOptions {
                    commands: vec!["dummy.do_something".to_string()],
                    work_done_progress_options: Default::default(),
                }),

                workspace: Some(WorkspaceServerCapabilities {
                    workspace_folders: Some(WorkspaceFoldersServerCapabilities {
                        supported: Some(true),
                        change_notifications: Some(OneOf::Left(true)),
                    }),
                    file_operations: None,
                }),
                semantic_tokens_provider: Some(
                    SemanticTokensServerCapabilities::SemanticTokensRegistrationOptions(
                        SemanticTokensRegistrationOptions {
                            text_document_registration_options: {
                                TextDocumentRegistrationOptions {
                                    document_selector: Some(vec![DocumentFilter {
                                        language: Some("chiquito".to_string()),
                                        scheme: Some("file".to_string()),
                                        pattern: None,
                                    }]),
                                }
                            },
                            semantic_tokens_options: SemanticTokensOptions {
                                work_done_progress_options: WorkDoneProgressOptions::default(),
                                legend: SemanticTokensLegend {
                                    token_types: LEGEND_TYPE.into(),
                                    token_modifiers: vec![],
                                },
                                range: Some(true),
                                full: Some(SemanticTokensFullOptions::Bool(true)),
                            },
                            static_registration_options: StaticRegistrationOptions::default(),
                        },
                    ),
                ),
                // definition: Some(GotoCapability::default()),
                definition_provider: Some(OneOf::Left(true)),
                references_provider: Some(OneOf::Left(true)),
                rename_provider: Some(OneOf::Left(true)),
                ..ServerCapabilities::default()
            },
        })
    }
    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "initialized!")
            .await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        self.client
            .log_message(MessageType::INFO, "file opened!")
            .await;
        self.on_change(TextDocumentItem {
            uri: params.text_document.uri,
            text: params.text_document.text,
            version: params.text_document.version,
        })
        .await
    }

    async fn did_change(&self, mut params: DidChangeTextDocumentParams) {
        self.on_change(TextDocumentItem {
            uri: params.text_document.uri,
            text: std::mem::take(&mut params.content_changes[0].text),
            version: params.text_document.version,
        })
        .await
    }

    async fn did_save(&self, _: DidSaveTextDocumentParams) {
        self.client
            .log_message(MessageType::INFO, "file saved!")
            .await;
    }
    async fn did_close(&self, _: DidCloseTextDocumentParams) {
        self.client
            .log_message(MessageType::INFO, "file closed!")
            .await;
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let definition = async {
            let uri = params.text_document_position_params.text_document.uri;
            let sym_table = self.sym_table_map.get(uri.as_str())?;
            let rope = self.document_map.get(uri.as_str())?;

            let position = params.text_document_position_params.position;
            let char = rope.try_line_to_char(position.line as usize).ok()?;
            let offset = char + position.character as usize;
            // self.client
            //     .log_message(MessageType::INFO, &format!("{:#?}, {}", sym_table, offset))
            //     .await;
            let span = get_definition(&sym_table, uri.path(), offset);
            self.client
                .log_message(MessageType::INFO, &format!("{:?}, ", span))
                .await;
            span.and_then(|(_, range)| {
                let start_position = offset_to_position(range.start, &rope)?;
                let end_position = offset_to_position(range.end, &rope)?;

                let range = Range::new(start_position, end_position);

                Some(GotoDefinitionResponse::Scalar(Location::new(uri, range)))
            })
        }
        .await;
        Ok(definition)
    }

    async fn references(&self, params: ReferenceParams) -> Result<Option<Vec<Location>>> {
        let reference_list = || -> Option<Vec<Location>> {
            let uri = params.text_document_position.text_document.uri;
            let symbols = self.sym_table_map.get(&uri.to_string())?;
            let rope = self.document_map.get(&uri.to_string())?;

            let position = params.text_document_position.position;
            let char = rope.try_line_to_char(position.line as usize).ok()?;
            let offset: usize = char + position.character as usize;
            let found_symbol = symbols.find_symbol_by_offset(uri.path().to_string(), offset);
            let mut reference_list = vec![];
            if let Some(found_symbol) = found_symbol {
                found_symbol.usages.iter().for_each(|usage| {
                    reference_list.push(usage.clone());
                });
            }
            let ret = reference_list
                .into_iter()
                .filter_map(|dsym| {
                    let start_position = offset_to_position(dsym.start, &rope)?;
                    let end_position = offset_to_position(dsym.end, &rope)?;

                    let range = Range::new(start_position, end_position);

                    Some(Location::new(uri.clone(), range))
                })
                .collect::<Vec<_>>();
            Some(ret)
        }();
        Ok(reference_list)
    }

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        let uri = params.text_document.uri.to_string();
        self.client
            .log_message(MessageType::LOG, "semantic_token_full")
            .await;
        // let semantic_tokens = || -> Option<Vec<SemanticToken>> {
        //     let mut im_complete_tokens = self.semantic_token_map.get_mut(&uri)?;
        //     let rope = self.document_map.get(&uri)?;
        //     let ast = self.sym_table_map.get(&uri)?;
        //     let extends_tokens = semantic_token_from_ast(&ast);
        //     im_complete_tokens.extend(extends_tokens);
        //     im_complete_tokens.sort_by(|a, b| a.start.cmp(&b.start));
        //     let mut pre_line = 0;
        //     let mut pre_start = 0;
        //     let semantic_tokens = im_complete_tokens
        //         .iter()
        //         .filter_map(|token| {
        //             let line = rope.try_byte_to_line(token.start).ok()? as u32;
        //             let first = rope.try_line_to_char(line as usize).ok()? as u32;
        //             let start = rope.try_byte_to_char(token.start).ok()? as u32 - first;
        //             let delta_line = line - pre_line;
        //             let delta_start = if delta_line == 0 {
        //                 start - pre_start
        //             } else {
        //                 start
        //             };
        //             let ret = Some(SemanticToken {
        //                 delta_line,
        //                 delta_start,
        //                 length: token.length as u32,
        //                 token_type: token.token_type as u32,
        //                 token_modifiers_bitset: 0,
        //             });
        //             pre_line = line;
        //             pre_start = start;
        //             ret
        //         })
        //         .collect::<Vec<_>>();
        //     Some(semantic_tokens)
        // }();
        // if let Some(semantic_token) = semantic_tokens {
        //     return Ok(Some(SemanticTokensResult::Tokens(SemanticTokens {
        //         result_id: None,
        //         data: semantic_token,
        //     })));
        // }
        Ok(None)
    }

    async fn semantic_tokens_range(
        &self,
        params: SemanticTokensRangeParams,
    ) -> Result<Option<SemanticTokensRangeResult>> {
        let uri = params.text_document.uri.to_string();
        let semantic_tokens = || -> Option<Vec<SemanticToken>> {
            let im_complete_tokens = self.semantic_token_map.get(&uri)?;
            let rope = self.document_map.get(&uri)?;
            let mut pre_line = 0;
            let mut pre_start = 0;
            let semantic_tokens = im_complete_tokens
                .iter()
                .filter_map(|token| {
                    let line = rope.try_byte_to_line(token.start).ok()? as u32;
                    let first = rope.try_line_to_char(line as usize).ok()? as u32;
                    let start = rope.try_byte_to_char(token.start).ok()? as u32 - first;
                    let ret = Some(SemanticToken {
                        delta_line: line - pre_line,
                        delta_start: if start >= pre_start {
                            start - pre_start
                        } else {
                            start
                        },
                        length: token.length as u32,
                        token_type: token.token_type as u32,
                        token_modifiers_bitset: 0,
                    });
                    pre_line = line;
                    pre_start = start;
                    ret
                })
                .collect::<Vec<_>>();
            Some(semantic_tokens)
        }();
        if let Some(semantic_token) = semantic_tokens {
            return Ok(Some(SemanticTokensRangeResult::Tokens(SemanticTokens {
                result_id: None,
                data: semantic_token,
            })));
        }
        Ok(None)
    }

    async fn inlay_hint(
        &self,
        params: tower_lsp::lsp_types::InlayHintParams,
    ) -> Result<Option<Vec<InlayHint>>> {
        self.client
            .log_message(
                MessageType::INFO,
                format!("inlay hint @ {:?}", params.range),
            )
            .await;
        let uri = &params.text_document.uri;
        let mut inlay_hint_list = vec![];
        if let Some(ast) = self.sym_table_map.get(uri.as_str()) {
            match self.document_map.get(uri.as_str()) {
                Some(rope) => {
                    let starting_char = rope
                        .try_line_to_char(params.range.start.line as usize)
                        .ok()
                        .unwrap();
                    let starting_offset = starting_char + params.range.start.character as usize;
                    let ending_char = rope
                        .try_line_to_char(params.range.end.line as usize)
                        .ok()
                        .unwrap();
                    let ending_offset = ending_char + params.range.end.character as usize;
                    let mut found_symbols = Vec::new();
                    for offset in starting_offset..ending_offset {
                        match ast.find_symbol_by_offset(uri.path().to_string(), offset) {
                            Some(symbol) => {
                                if found_symbols.contains(&symbol.definition_ref) {
                                    continue;
                                }
                                found_symbols.push(symbol.definition_ref);
                                let label = match symbol.category {
                                    chiquito::compiler::semantic::SymbolCategory::InputSignal
                                    | chiquito::compiler::semantic::SymbolCategory::InputWGVar => {
                                        "input".to_string()
                                    }
                                    chiquito::compiler::semantic::SymbolCategory::OutputSignal
                                    | chiquito::compiler::semantic::SymbolCategory::OutputWGVar => {
                                        "output".to_string()
                                    }
                                    chiquito::compiler::semantic::SymbolCategory::InoutSignal
                                    | chiquito::compiler::semantic::SymbolCategory::InoutWGVar => {
                                        "inout".to_string()
                                    }
                                    _ => "".to_string(),
                                };
                                for usage in symbol.usages {
                                    if symbol.category
                                        == chiquito::compiler::semantic::SymbolCategory::State
                                    {
                                        add_inlay_hint(
                                            &mut inlay_hint_list,
                                            "state".to_string(),
                                            usage.end,
                                            &rope,
                                            InlayHintKind::TYPE,
                                        );
                                    } else if symbol.ty.is_some() {
                                        add_inlay_hint(
                                            &mut inlay_hint_list,
                                            symbol.ty.clone().unwrap(),
                                            usage.end,
                                            &rope,
                                            InlayHintKind::TYPE,
                                        );
                                    }
                                }
                                if label.is_empty() {
                                    continue;
                                }
                                add_inlay_hint(
                                    &mut inlay_hint_list,
                                    label,
                                    offset,
                                    &rope,
                                    InlayHintKind::PARAMETER,
                                );
                            }
                            None => (),
                        }
                    }
                }
                None => (),
            };
        }

        Ok(Some(inlay_hint_list))
    }

    // async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
    //     let uri = params.text_document_position.text_document.uri;
    //     let position = params.text_document_position.position;
    //     let completions = || -> Option<Vec<CompletionItem>> {
    //         let rope = self.document_map.get(&uri.to_string())?;
    //         let ast = self.sym_table_map.get(&uri.to_string())?;
    //         let char = rope.try_line_to_char(position.line as usize).ok()?;
    //         let offset = char + position.character as usize;
    //         let completions = completion(&ast, offset);
    //         let mut ret = Vec::with_capacity(completions.len());
    //         for (_, item) in completions {
    //             match item {
    //                 chiquito_language_server::completion::ImCompleteCompletionItem::Variable(
    //                     var,
    //                 ) => {
    //                     ret.push(CompletionItem {
    //                         label: var.clone(),
    //                         insert_text: Some(var.clone()),
    //                         kind: Some(CompletionItemKind::VARIABLE),
    //                         detail: Some(var),
    //                         ..Default::default()
    //                     });
    //                 }
    //                 chiquito_language_server::completion::ImCompleteCompletionItem::Function(
    //                     name,
    //                     args,
    //                 ) => {
    //                     ret.push(CompletionItem {
    //                         label: name.clone(),
    //                         kind: Some(CompletionItemKind::FUNCTION),
    //                         detail: Some(name.clone()),
    //                         insert_text: Some(format!(
    //                             "{}({})",
    //                             name,
    //                             args.iter()
    //                                 .enumerate()
    //                                 .map(|(index, item)| { format!("${{{}:{}}}", index + 1, item) })
    //                                 .collect::<Vec<_>>()
    //                                 .join(",")
    //                         )),
    //                         insert_text_format: Some(InsertTextFormat::SNIPPET),
    //                         ..Default::default()
    //                     });
    //                 }
    //             }
    //         }
    //         Some(ret)
    //     }();
    //     Ok(completions.map(CompletionResponse::Array))
    // }

    async fn rename(&self, params: RenameParams) -> Result<Option<WorkspaceEdit>> {
        let workspace_edit = || -> Option<WorkspaceEdit> {
            let uri = params.text_document_position.text_document.uri;
            let ast = self.sym_table_map.get(&uri.to_string())?;
            let rope = self.document_map.get(&uri.to_string())?;

            let position = params.text_document_position.position;
            let char = rope.try_line_to_char(position.line as usize).ok()?;
            let offset = char + position.character as usize;
            let symbol = ast.find_symbol_by_offset(uri.path().to_string(), offset);
            let reference_list = match symbol {
                Some(symbol) => {
                    let mut reference_list = symbol.usages.clone();
                    reference_list.push(symbol.definition_ref.clone());
                    reference_list
                }
                None => vec![],
            };
            let new_name = params.new_name;
            if !reference_list.is_empty() {
                let edit_list = reference_list
                    .into_iter()
                    .filter_map(|dsym| {
                        let start_position = offset_to_position(dsym.start, &rope)?;
                        let end_position = offset_to_position(dsym.end, &rope)?;
                        Some(TextEdit::new(
                            Range::new(start_position, end_position),
                            new_name.clone(),
                        ))
                    })
                    .collect::<Vec<_>>();
                let mut map = HashMap::new();
                map.insert(uri, edit_list);
                let workspace_edit = WorkspaceEdit::new(map);
                Some(workspace_edit)
            } else {
                None
            }
        }();
        Ok(workspace_edit)
    }

    async fn did_change_configuration(&self, _: DidChangeConfigurationParams) {
        self.client
            .log_message(MessageType::INFO, "configuration changed!")
            .await;
    }

    async fn did_change_workspace_folders(&self, _: DidChangeWorkspaceFoldersParams) {
        self.client
            .log_message(MessageType::INFO, "workspace folders changed!")
            .await;
    }

    async fn did_change_watched_files(&self, _: DidChangeWatchedFilesParams) {
        self.client
            .log_message(MessageType::INFO, "watched files have changed!")
            .await;
    }

    async fn execute_command(&self, _: ExecuteCommandParams) -> Result<Option<Value>> {
        self.client
            .log_message(MessageType::INFO, "command executed!")
            .await;

        match self.client.apply_edit(WorkspaceEdit::default()).await {
            Ok(res) if res.applied => self.client.log_message(MessageType::INFO, "applied").await,
            Ok(_) => self.client.log_message(MessageType::INFO, "rejected").await,
            Err(err) => self.client.log_message(MessageType::ERROR, err).await,
        }

        Ok(None)
    }
}

fn add_inlay_hint(
    inlay_hint_list: &mut Vec<InlayHint>,
    label: String,
    offset: usize,
    rope: &Rope,
    kind: InlayHintKind,
) {
    inlay_hint_list.push(InlayHint {
        text_edits: None,
        tooltip: None,
        kind: Some(kind),
        padding_left: None,
        padding_right: Some(true),
        data: None,
        position: offset_to_position(offset, &rope).unwrap(),
        label: InlayHintLabel::LabelParts(vec![InlayHintLabelPart {
            value: if kind == InlayHintKind::TYPE {
                format!(": {}", label)
            } else {
                label
            },
            tooltip: None,
            location: None,
            command: None,
        }]),
    });
}
#[derive(Debug, Deserialize, Serialize)]
struct InlayHintParams {
    path: String,
}

enum CustomNotification {}
impl Notification for CustomNotification {
    type Params = InlayHintParams;
    const METHOD: &'static str = "custom/notification";
}
struct TextDocumentItem {
    uri: Url,
    text: String,
    version: i32,
}

impl Backend {
    async fn on_change(&self, params: TextDocumentItem) {
        let rope = ropey::Rope::from_str(&params.text);
        self.document_map
            .insert(params.uri.to_string(), rope.clone());
        let debug_sym_factory = DebugSymRefFactory::new(params.uri.path(), &params.text);
        let parser = TLDeclsParser::new();
        let declarations = parser.parse(&debug_sym_factory, &params.text);
        let range_factory = RangeFactory::new(params.uri.path(), &params.text);
        match declarations {
            Ok(declarations) => {
                let AnalysisResult { symbols, messages } = analyse(&declarations);

                let diagnostics = messages
                    .into_iter()
                    .filter_map(|message| {
                        let (message, dsym) = match message {
                            ParseErr { msg } => (msg, None),
                            SemErr { msg, dsym } => (msg, Some(dsym)),
                            RuntimeErr { msg, dsym } => (msg, Some(dsym)),
                        };

                        || -> Option<Diagnostic> {
                            let range = match dsym {
                                Some(ref dsym) => range_factory.create(dsym.start, dsym.end),
                                None => range_factory.create(0, 0),
                            };

                            Some(Diagnostic::new(
                                range,
                                Some(DiagnosticSeverity::ERROR),
                                Some(NumberOrString::String("E01".to_string())),
                                Some("Chiquito analyzer".to_string()),
                                message,
                                None,
                                None,
                            ))
                        }()
                    })
                    .collect::<Vec<_>>();

                self.client
                    .publish_diagnostics(
                        params.uri.clone(),
                        diagnostics.clone(),
                        Some(params.version),
                    )
                    .await;

                self.client
                    .log_message(MessageType::INFO, &format!("SymTable: {:?}", symbols))
                    .await;

                self.sym_table_map.insert(params.uri.to_string(), symbols);

                // self.client
                //     .log_message(MessageType::INFO, &format!("{:?}", semantic_tokens))
                //     .await;
                // self.semantic_token_map
                //     .insert(params.uri.to_string(), semantic_tokens);
            }
            Err(err) => {
                let mut diagnostics = vec![];
                match err {
                    InvalidToken { location } => {
                        diagnostics.push(Diagnostic::new_simple(
                            range_factory.create(location, location),
                            "Invalid token".to_string(),
                        ));
                    }
                    // TODO make use of the "expected" field
                    UnrecognizedEof { location, .. } => {
                        diagnostics.push(Diagnostic::new_simple(
                            range_factory.create(location, location),
                            "Unrecognized EoF".to_string(),
                        ));
                    }
                    // TODO make use of the "expected" field
                    UnrecognizedToken { token, .. } => {
                        diagnostics.push(Diagnostic::new_simple(
                            range_factory.create(token.0, token.2),
                            format!("Unrecognized token: {:?}", token),
                        ));
                    }
                    ExtraToken { token } => {
                        diagnostics.push(Diagnostic::new_simple(
                            range_factory.create(token.0, token.2),
                            format!("Extra token: {:?}", token),
                        ));
                    }
                    User { error } => {
                        diagnostics.push(Diagnostic::new_simple(
                            range_factory.create(0, 0),
                            error.to_string(),
                        ));
                    }
                };

                self.client
                    .log_message(MessageType::INFO, &format!("{:?}", diagnostics.clone()))
                    .await;

                self.client
                    .publish_diagnostics(params.uri.clone(), diagnostics, Some(params.version))
                    .await;
            }
        }
    }
}

#[tokio::main]
async fn main() {
    env_logger::init();

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::build(|client| Backend {
        client,
        sym_table_map: DashMap::new(),
        document_map: DashMap::new(),
        semantic_token_map: DashMap::new(),
    })
    .finish();

    serde_json::json!({"test": 20});
    Server::new(stdin, stdout, socket).serve(service).await;
}

fn offset_to_position(offset: usize, rope: &Rope) -> Option<Position> {
    let line = rope.try_char_to_line(offset).ok()?;
    let first_char_of_line = rope.try_line_to_char(line).ok()?;
    let column = offset - first_char_of_line;
    Some(Position::new(line as u32, column as u32))
}
