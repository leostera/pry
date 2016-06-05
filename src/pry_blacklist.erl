-module(pry_blacklist).

-export([
          blacklist/0,
          is_blacklisted/1
        ]).

-spec is_blacklisted(atom()) -> true | false.
is_blacklisted(Module) -> lists:member(Module, blacklist()).

-spec blacklist() -> [ atom() ].
blacklist() ->
  % io:format("~p", [[list_to_atom(filename:basename(F, ".beam")) || F <- filelib:wildcard(filename:join([code:lib_dir(kernel), "..", "*", "ebin", "*.beam"]))]]).
  [asn1_db,asn1ct,asn1ct_check,asn1ct_constructed_ber_bin_v2,
   asn1ct_constructed_per,asn1ct_eval_ext,asn1ct_func,asn1ct_gen,
   asn1ct_gen_ber_bin_v2,asn1ct_gen_check,asn1ct_gen_per,asn1ct_imm,asn1ct_name,
   asn1ct_parser2,asn1ct_pretty_format,asn1ct_rtt,asn1ct_table,asn1ct_tok,
   asn1ct_value,asn1rt,asn1rt_nif,ct,ct_config,ct_config_plain,ct_config_xml,
   ct_conn_log_h,ct_cover,ct_event,ct_framework,ct_ftp,ct_gen_conn,ct_groups,
   ct_hooks,ct_hooks_lock,ct_logs,ct_make,ct_master,ct_master_event,
   ct_master_logs,ct_master_status,ct_netconfc,ct_property_test,ct_release_test,
   ct_repeat,ct_rpc,ct_run,ct_slave,ct_snmp,ct_ssh,ct_telnet,ct_telnet_client,
   ct_testspec,ct_util,ct_webtool,ct_webtool_sup,cth_conn_log,cth_log_redirect,
   cth_surefire,unix_telnet,vts,beam_a,beam_asm,beam_block,beam_bool,beam_bsm,
   beam_clean,beam_dead,beam_dict,beam_disasm,beam_except,beam_flatten,
   beam_jump,beam_listing,beam_opcodes,beam_peep,beam_receive,beam_split,
   beam_trim,beam_type,beam_utils,beam_validator,beam_z,cerl,cerl_clauses,
   cerl_inline,cerl_sets,cerl_trees,compile,core_lib,core_lint,core_parse,
   core_pp,core_scan,erl_bifs,rec_env,sys_core_dsetel,sys_core_fold,
   sys_core_fold_lists,sys_core_inline,sys_pre_attributes,sys_pre_expand,
   v3_codegen,v3_core,v3_kernel,v3_kernel_pp,v3_life,
   'CosEventChannelAdmin_AlreadyConnected','CosEventChannelAdmin_ConsumerAdmin',
   'CosEventChannelAdmin_EventChannel','CosEventChannelAdmin_ProxyPullConsumer',
   'CosEventChannelAdmin_ProxyPullConsumer_impl',
   'CosEventChannelAdmin_ProxyPullSupplier',
   'CosEventChannelAdmin_ProxyPushConsumer',
   'CosEventChannelAdmin_ProxyPushConsumer_impl',
   'CosEventChannelAdmin_ProxyPushSupplier',
   'CosEventChannelAdmin_SupplierAdmin',
   'CosEventChannelAdmin_SupplierAdmin_impl','CosEventChannelAdmin_TypeError',
   'CosEventComm_Disconnected','CosEventComm_PullConsumer',
   'CosEventComm_PullSupplier','CosEventComm_PushConsumer',
   'CosEventComm_PushSupplier',cosEventApp,oe_CosEventChannelAdmin,
   oe_CosEventComm,oe_CosEventComm_CAdmin,oe_CosEventComm_CAdmin_impl,
   oe_CosEventComm_Channel,oe_CosEventComm_Channel_impl,oe_CosEventComm_Event,
   oe_CosEventComm_PullerS,oe_CosEventComm_PullerS_impl,oe_CosEventComm_PusherS,
   oe_CosEventComm_PusherS_impl,oe_cosEventApp,'CosEventDomainAdmin',
   'CosEventDomainAdmin_AlreadyExists','CosEventDomainAdmin_Connection',
   'CosEventDomainAdmin_ConnectionIDSeq',
   'CosEventDomainAdmin_ConnectionNotFound',
   'CosEventDomainAdmin_CycleCreationForbidden','CosEventDomainAdmin_CycleSeq',
   'CosEventDomainAdmin_DiamondCreationForbidden',
   'CosEventDomainAdmin_DiamondSeq','CosEventDomainAdmin_DomainIDSeq',
   'CosEventDomainAdmin_DomainNotFound','CosEventDomainAdmin_EventDomain',
   'CosEventDomainAdmin_EventDomainFactory',
   'CosEventDomainAdmin_EventDomainFactory_impl',
   'CosEventDomainAdmin_EventDomain_impl','CosEventDomainAdmin_MemberIDSeq',
   'CosEventDomainAdmin_RouteSeq',cosEventDomainApp,oe_CosEventDomainAdmin,
   'CosFileTransfer','CosFileTransfer_AccessLevel',
   'CosFileTransfer_CommandNotImplementedException','CosFileTransfer_Directory',
   'CosFileTransfer_Directory_impl','CosFileTransfer_File',
   'CosFileTransfer_FileIterator','CosFileTransfer_FileIterator_impl',
   'CosFileTransfer_FileList','CosFileTransfer_FileNameList',
   'CosFileTransfer_FileNotFoundException',
   'CosFileTransfer_FileTransferSession',
   'CosFileTransfer_FileTransferSession_impl','CosFileTransfer_FileWrapper',
   'CosFileTransfer_File_impl','CosFileTransfer_IllegalOperationException',
   'CosFileTransfer_ProtocolAddressList','CosFileTransfer_ProtocolSupport',
   'CosFileTransfer_RequestFailureException','CosFileTransfer_SessionException',
   'CosFileTransfer_SupportedProtocolAddresses',
   'CosFileTransfer_TransferException','CosFileTransfer_VirtualFileSystem',
   'CosFileTransfer_VirtualFileSystem_ContentList',
   'CosFileTransfer_VirtualFileSystem_impl',cosFileTransferApp,
   cosFileTransferNATIVE_file,oe_CosFileTransfer,'CosNotification',
   'CosNotification_AdminPropertiesAdmin','CosNotification_Common',
   'CosNotification_EventBatch','CosNotification_EventHeader',
   'CosNotification_EventType','CosNotification_EventTypeSeq',
   'CosNotification_FixedEventHeader','CosNotification_NamedPropertyRange',
   'CosNotification_NamedPropertyRangeSeq','CosNotification_Property',
   'CosNotification_PropertyError','CosNotification_PropertyErrorSeq',
   'CosNotification_PropertyRange','CosNotification_PropertySeq',
   'CosNotification_QoSAdmin','CosNotification_StructuredEvent',
   'CosNotification_UnsupportedAdmin','CosNotification_UnsupportedQoS',
   'CosNotifyChannelAdmin_AdminIDSeq','CosNotifyChannelAdmin_AdminLimit',
   'CosNotifyChannelAdmin_AdminLimitExceeded',
   'CosNotifyChannelAdmin_AdminNotFound','CosNotifyChannelAdmin_ChannelIDSeq',
   'CosNotifyChannelAdmin_ChannelNotFound',
   'CosNotifyChannelAdmin_ConnectionAlreadyActive',
   'CosNotifyChannelAdmin_ConnectionAlreadyInactive',
   'CosNotifyChannelAdmin_ConsumerAdmin',
   'CosNotifyChannelAdmin_ConsumerAdmin_impl',
   'CosNotifyChannelAdmin_EventChannel',
   'CosNotifyChannelAdmin_EventChannelFactory',
   'CosNotifyChannelAdmin_EventChannelFactory_impl',
   'CosNotifyChannelAdmin_EventChannel_impl',
   'CosNotifyChannelAdmin_NotConnected','CosNotifyChannelAdmin_ProxyConsumer',
   'CosNotifyChannelAdmin_ProxyIDSeq','CosNotifyChannelAdmin_ProxyNotFound',
   'CosNotifyChannelAdmin_ProxyPullConsumer',
   'CosNotifyChannelAdmin_ProxyPullSupplier',
   'CosNotifyChannelAdmin_ProxyPushConsumer',
   'CosNotifyChannelAdmin_ProxyPushSupplier',
   'CosNotifyChannelAdmin_ProxySupplier',
   'CosNotifyChannelAdmin_SequenceProxyPullConsumer',
   'CosNotifyChannelAdmin_SequenceProxyPullSupplier',
   'CosNotifyChannelAdmin_SequenceProxyPushConsumer',
   'CosNotifyChannelAdmin_SequenceProxyPushSupplier',
   'CosNotifyChannelAdmin_StructuredProxyPullConsumer',
   'CosNotifyChannelAdmin_StructuredProxyPullSupplier',
   'CosNotifyChannelAdmin_StructuredProxyPushConsumer',
   'CosNotifyChannelAdmin_StructuredProxyPushSupplier',
   'CosNotifyChannelAdmin_SupplierAdmin',
   'CosNotifyChannelAdmin_SupplierAdmin_impl','CosNotifyComm_InvalidEventType',
   'CosNotifyComm_NotifyPublish','CosNotifyComm_NotifySubscribe',
   'CosNotifyComm_PullConsumer','CosNotifyComm_PullSupplier',
   'CosNotifyComm_PushConsumer','CosNotifyComm_PushSupplier',
   'CosNotifyComm_SequencePullConsumer','CosNotifyComm_SequencePullSupplier',
   'CosNotifyComm_SequencePushConsumer','CosNotifyComm_SequencePushSupplier',
   'CosNotifyComm_StructuredPullConsumer',
   'CosNotifyComm_StructuredPullSupplier',
   'CosNotifyComm_StructuredPushConsumer',
   'CosNotifyComm_StructuredPushSupplier','CosNotifyFilter_CallbackIDSeq',
   'CosNotifyFilter_CallbackNotFound','CosNotifyFilter_ConstraintExp',
   'CosNotifyFilter_ConstraintExpSeq','CosNotifyFilter_ConstraintIDSeq',
   'CosNotifyFilter_ConstraintInfo','CosNotifyFilter_ConstraintInfoSeq',
   'CosNotifyFilter_ConstraintNotFound','CosNotifyFilter_DuplicateConstraintID',
   'CosNotifyFilter_Filter','CosNotifyFilter_FilterAdmin',
   'CosNotifyFilter_FilterFactory','CosNotifyFilter_FilterFactory_impl',
   'CosNotifyFilter_FilterIDSeq','CosNotifyFilter_FilterNotFound',
   'CosNotifyFilter_Filter_impl','CosNotifyFilter_InvalidConstraint',
   'CosNotifyFilter_InvalidGrammar','CosNotifyFilter_InvalidValue',
   'CosNotifyFilter_MappingConstraintInfo',
   'CosNotifyFilter_MappingConstraintInfoSeq',
   'CosNotifyFilter_MappingConstraintPair',
   'CosNotifyFilter_MappingConstraintPairSeq','CosNotifyFilter_MappingFilter',
   'CosNotifyFilter_MappingFilter_impl',
   'CosNotifyFilter_UnsupportedFilterableData','PullerConsumer_impl',
   'PullerSupplier_impl','PusherConsumer_impl','PusherSupplier_impl',
   cosNotificationApp,cosNotification_Filter,cosNotification_Grammar,
   cosNotification_Scanner,cosNotification_eventDB,oe_CosNotification,
   oe_CosNotificationComm_Event,oe_CosNotifyChannelAdmin,oe_CosNotifyComm,
   oe_CosNotifyFilter,oe_cosNotificationAppComm,
   'CosPropertyService_ConflictingProperty',
   'CosPropertyService_ConstraintNotSupported',
   'CosPropertyService_FixedProperty','CosPropertyService_InvalidPropertyName',
   'CosPropertyService_MultipleExceptions','CosPropertyService_Properties',
   'CosPropertyService_PropertiesIterator',
   'CosPropertyService_PropertiesIterator_impl','CosPropertyService_Property',
   'CosPropertyService_PropertyDef','CosPropertyService_PropertyDefs',
   'CosPropertyService_PropertyException',
   'CosPropertyService_PropertyExceptions','CosPropertyService_PropertyMode',
   'CosPropertyService_PropertyModes','CosPropertyService_PropertyNames',
   'CosPropertyService_PropertyNamesIterator',
   'CosPropertyService_PropertyNamesIterator_impl',
   'CosPropertyService_PropertyNotFound','CosPropertyService_PropertySet',
   'CosPropertyService_PropertySetDef',
   'CosPropertyService_PropertySetDefFactory',
   'CosPropertyService_PropertySetDefFactory_impl',
   'CosPropertyService_PropertySetDef_impl',
   'CosPropertyService_PropertySetFactory',
   'CosPropertyService_PropertySetFactory_impl',
   'CosPropertyService_PropertyTypes','CosPropertyService_ReadOnlyProperty',
   'CosPropertyService_UnsupportedMode',
   'CosPropertyService_UnsupportedProperty',
   'CosPropertyService_UnsupportedTypeCode',cosProperty,oe_CosProperty,
   'CosTime_TIO','CosTime_TIO_impl','CosTime_TimeService',
   'CosTime_TimeService_impl','CosTime_TimeUnavailable','CosTime_UTO',
   'CosTime_UTO_impl','CosTimerEvent_TimerEventHandler',
   'CosTimerEvent_TimerEventHandler_impl','CosTimerEvent_TimerEventService',
   'CosTimerEvent_TimerEventService_impl','CosTimerEvent_TimerEventT',
   'TimeBase_IntervalT','TimeBase_UtcT',cosTime,oe_CosTime,oe_CosTimerEvent,
   oe_TimeBase,'CosTransactions_Control','CosTransactions_Coordinator',
   'CosTransactions_HeuristicCommit','CosTransactions_HeuristicHazard',
   'CosTransactions_HeuristicMixed','CosTransactions_HeuristicRollback',
   'CosTransactions_Inactive','CosTransactions_InvalidControl',
   'CosTransactions_NoTransaction','CosTransactions_NotPrepared',
   'CosTransactions_NotSubtransaction','CosTransactions_PropagationContext',
   'CosTransactions_RecoveryCoordinator','CosTransactions_Resource',
   'CosTransactions_SubtransactionAwareResource',
   'CosTransactions_SubtransactionsUnavailable',
   'CosTransactions_SynchronizationUnavailable','CosTransactions_Terminator',
   'CosTransactions_Terminator_impl','CosTransactions_TransIdentity',
   'CosTransactions_TransactionFactory',
   'CosTransactions_TransactionFactory_impl','CosTransactions_Unavailable',
   'CosTransactions_WrongTransaction','CosTransactions_otid_t','ETraP_Common',
   'ETraP_Server','ETraP_Server_impl',cosTransactions,etrap_logmgr,
   oe_CosTransactions,crypto,crypto_ec_curves,dbg_debugged,dbg_icmd,dbg_idb,
   dbg_ieval,dbg_iload,dbg_iserver,dbg_istk,dbg_wx_break,dbg_wx_break_win,
   dbg_wx_code,dbg_wx_filedialog_win,dbg_wx_interpret,dbg_wx_mon,dbg_wx_mon_win,
   dbg_wx_settings,dbg_wx_src_view,dbg_wx_trace,dbg_wx_trace_win,dbg_wx_view,
   dbg_wx_win,dbg_wx_winman,debugger,i,int,dialyzer,dialyzer_analysis_callgraph,
   dialyzer_behaviours,dialyzer_callgraph,dialyzer_cl,dialyzer_cl_parse,
   dialyzer_codeserver,dialyzer_contracts,dialyzer_coordinator,
   dialyzer_dataflow,dialyzer_dep,dialyzer_explanation,dialyzer_gui_wx,
   dialyzer_options,dialyzer_plt,dialyzer_races,dialyzer_succ_typings,
   dialyzer_timing,dialyzer_typesig,dialyzer_utils,dialyzer_worker,diameter,
   diameter_app,diameter_callback,diameter_capx,diameter_codec,diameter_codegen,
   diameter_config,diameter_dbg,diameter_dict,diameter_dict_parser,
   diameter_dict_scanner,diameter_dict_util,diameter_etcp,diameter_etcp_sup,
   diameter_exprecs,diameter_gen_acct_rfc6733,diameter_gen_base_accounting,
   diameter_gen_base_rfc3588,diameter_gen_base_rfc6733,diameter_gen_relay,
   diameter_info,diameter_lib,diameter_make,diameter_misc_sup,diameter_peer,
   diameter_peer_fsm,diameter_peer_fsm_sup,diameter_reg,diameter_sctp,
   diameter_sctp_sup,diameter_service,diameter_service_sup,diameter_session,
   diameter_stats,diameter_sup,diameter_sync,diameter_tcp,diameter_tcp_sup,
   diameter_traffic,diameter_transport,diameter_transport_sup,diameter_types,
   diameter_watchdog,diameter_watchdog_sup,edoc,edoc_data,edoc_doclet,
   edoc_extract,edoc_layout,edoc_lib,edoc_macros,edoc_parser,edoc_refs,
   edoc_report,edoc_run,edoc_scanner,edoc_specs,edoc_tags,edoc_types,edoc_wiki,
   otpsgml_layout,'ELDAPv3',eldap,docgen_edoc_xml_cb,docgen_otp_specs,
   docgen_xmerl_xml_cb,erl_prim_loader,erlang,erts_internal,init,otp_ring0,
   prim_eval,prim_file,prim_inet,prim_zip,zlib,et,et_collector,et_selector,
   et_viewer,et_wx_contents_viewer,et_wx_viewer,eunit,eunit_autoexport,
   eunit_data,eunit_lib,eunit_listener,eunit_proc,eunit_serial,eunit_server,
   eunit_striptests,eunit_surefire,eunit_test,eunit_tests,eunit_tty,gs,
   gs_frontend,gs_make,gs_packer,gs_widgets,gse,gstk,gstk_arc,gstk_button,
   gstk_canvas,gstk_checkbutton,gstk_db,gstk_editor,gstk_entry,gstk_font,
   gstk_frame,gstk_generic,gstk_grid,gstk_gridline,gstk_gs,gstk_image,
   gstk_label,gstk_line,gstk_listbox,gstk_menu,gstk_menubar,gstk_menubutton,
   gstk_menuitem,gstk_oval,gstk_polygon,gstk_port_handler,gstk_radiobutton,
   gstk_rectangle,gstk_scale,gstk_text,gstk_widgets,gstk_window,tcl2erl,
   tool_file_dialog,tool_utils,cerl_cconv,cerl_closurean,cerl_hipeify,cerl_lib,
   cerl_messagean,cerl_pmatch,cerl_prettypr,cerl_to_icode,cerl_typean,
   elf_format,erl_bif_types,erl_types,hipe,hipe_adj_list,hipe_amd64_assemble,
   hipe_amd64_defuse,hipe_amd64_encode,hipe_amd64_frame,hipe_amd64_liveness,
   hipe_amd64_main,hipe_amd64_pp,hipe_amd64_ra,hipe_amd64_ra_finalise,
   hipe_amd64_ra_ls,hipe_amd64_ra_naive,hipe_amd64_ra_postconditions,
   hipe_amd64_ra_sse2_postconditions,hipe_amd64_ra_x87_ls,hipe_amd64_registers,
   hipe_amd64_specific,hipe_amd64_specific_sse2,hipe_amd64_specific_x87,
   hipe_amd64_spill_restore,hipe_amd64_x87,hipe_arm,hipe_arm_assemble,
   hipe_arm_cfg,hipe_arm_defuse,hipe_arm_encode,hipe_arm_finalise,
   hipe_arm_frame,hipe_arm_liveness_gpr,hipe_arm_main,hipe_arm_pp,hipe_arm_ra,
   hipe_arm_ra_finalise,hipe_arm_ra_ls,hipe_arm_ra_naive,
   hipe_arm_ra_postconditions,hipe_arm_registers,hipe_arm_specific,hipe_bb,
   hipe_beam_to_icode,hipe_coalescing_regalloc,hipe_consttab,hipe_data_pp,
   hipe_digraph,hipe_dominators,hipe_dot,hipe_gen_cfg,hipe_gensym,
   hipe_graph_coloring_regalloc,hipe_icode,hipe_icode2rtl,hipe_icode_bincomp,
   hipe_icode_callgraph,hipe_icode_cfg,hipe_icode_coordinator,hipe_icode_ebb,
   hipe_icode_exceptions,hipe_icode_fp,hipe_icode_heap_test,
   hipe_icode_inline_bifs,hipe_icode_instruction_counter,hipe_icode_liveness,
   hipe_icode_mulret,hipe_icode_pp,hipe_icode_primops,hipe_icode_range,
   hipe_icode_split_arith,hipe_icode_ssa,hipe_icode_ssa_const_prop,
   hipe_icode_ssa_copy_prop,hipe_icode_ssa_struct_reuse,hipe_icode_type,hipe_ig,
   hipe_ig_moves,hipe_jit,hipe_llvm,hipe_llvm_liveness,hipe_llvm_main,
   hipe_llvm_merge,hipe_ls_regalloc,hipe_main,hipe_moves,hipe_node_sets,
   hipe_optimistic_regalloc,hipe_pack_constants,hipe_ppc,hipe_ppc_assemble,
   hipe_ppc_cfg,hipe_ppc_defuse,hipe_ppc_encode,hipe_ppc_finalise,
   hipe_ppc_frame,hipe_ppc_liveness_all,hipe_ppc_liveness_fpr,
   hipe_ppc_liveness_gpr,hipe_ppc_main,hipe_ppc_pp,hipe_ppc_ra,
   hipe_ppc_ra_finalise,hipe_ppc_ra_ls,hipe_ppc_ra_naive,
   hipe_ppc_ra_postconditions,hipe_ppc_ra_postconditions_fp,hipe_ppc_registers,
   hipe_ppc_specific,hipe_ppc_specific_fp,hipe_profile,hipe_reg_worklists,
   hipe_regalloc_loop,hipe_rtl,hipe_rtl_arch,hipe_rtl_arith_32,
   hipe_rtl_arith_64,hipe_rtl_binary,hipe_rtl_binary_construct,
   hipe_rtl_binary_match,hipe_rtl_cfg,hipe_rtl_cleanup_const,
   hipe_rtl_exceptions,hipe_rtl_lcm,hipe_rtl_liveness,hipe_rtl_mk_switch,
   hipe_rtl_primops,hipe_rtl_ssa,hipe_rtl_ssa_avail_expr,
   hipe_rtl_ssa_const_prop,hipe_rtl_ssapre,hipe_rtl_symbolic,hipe_rtl_to_amd64,
   hipe_rtl_to_arm,hipe_rtl_to_llvm,hipe_rtl_to_ppc,hipe_rtl_to_sparc,
   hipe_rtl_to_x86,hipe_rtl_varmap,hipe_sdi,hipe_sparc,hipe_sparc_assemble,
   hipe_sparc_cfg,hipe_sparc_defuse,hipe_sparc_encode,hipe_sparc_finalise,
   hipe_sparc_frame,hipe_sparc_liveness_all,hipe_sparc_liveness_fpr,
   hipe_sparc_liveness_gpr,hipe_sparc_main,hipe_sparc_pp,hipe_sparc_ra,
   hipe_sparc_ra_finalise,hipe_sparc_ra_ls,hipe_sparc_ra_naive,
   hipe_sparc_ra_postconditions,hipe_sparc_ra_postconditions_fp,
   hipe_sparc_registers,hipe_sparc_specific,hipe_sparc_specific_fp,
   hipe_spillcost,hipe_spillmin,hipe_spillmin_color,hipe_spillmin_scan,
   hipe_tagscheme,hipe_temp_map,hipe_timing,hipe_vectors,hipe_x86,
   hipe_x86_assemble,hipe_x86_cfg,hipe_x86_defuse,hipe_x86_encode,
   hipe_x86_frame,hipe_x86_liveness,hipe_x86_main,hipe_x86_postpass,hipe_x86_pp,
   hipe_x86_ra,hipe_x86_ra_finalise,hipe_x86_ra_ls,hipe_x86_ra_naive,
   hipe_x86_ra_postconditions,hipe_x86_ra_x87_ls,hipe_x86_registers,
   hipe_x86_specific,hipe_x86_specific_x87,hipe_x86_spill_restore,hipe_x86_x87,
   ic,ic_array_java,ic_attribute_java,ic_cbe,ic_cclient,ic_code,ic_codegen,
   ic_constant_java,ic_cserver,ic_enum_java,ic_erl_template,ic_erlbe,ic_error,
   ic_fetch,ic_file,ic_forms,ic_genobj,ic_java_type,ic_jbe,ic_noc,ic_options,
   ic_plainbe,ic_pp,ic_pragma,ic_sequence_java,ic_struct_java,ic_symtab,
   ic_union_java,ic_util,icenum,iceval,icparse,icpreproc,icscan,icstruct,ictk,
   ictype,icunion,ftp,ftp_progress,ftp_response,ftp_sup,http_chunk,http_request,
   http_response,http_transport,http_uri,http_util,httpc,httpc_cookie,
   httpc_handler,httpc_handler_sup,httpc_manager,httpc_profile_sup,
   httpc_request,httpc_response,httpc_sup,httpd,httpd_acceptor,
   httpd_acceptor_sup,httpd_cgi,httpd_conf,httpd_connection_sup,httpd_custom,
   httpd_custom_api,httpd_esi,httpd_example,httpd_file,httpd_instance_sup,
   httpd_log,httpd_manager,httpd_misc_sup,httpd_request,httpd_request_handler,
   httpd_response,httpd_script_env,httpd_socket,httpd_sup,httpd_util,inets,
   inets_app,inets_lib,inets_service,inets_sup,inets_time_compat,inets_trace,
   mod_actions,mod_alias,mod_auth,mod_auth_dets,mod_auth_mnesia,mod_auth_plain,
   mod_auth_server,mod_browser,mod_cgi,mod_dir,mod_disk_log,mod_esi,mod_get,
   mod_head,mod_htaccess,mod_log,mod_range,mod_responsecontrol,mod_security,
   mod_security_server,mod_trace,tftp,tftp_binary,tftp_engine,tftp_file,
   tftp_lib,tftp_logger,tftp_sup,application,application_controller,
   application_master,application_starter,auth,code,code_server,disk_log,
   disk_log_1,disk_log_server,disk_log_sup,dist_ac,dist_util,erl_boot_server,
   erl_ddll,erl_distribution,erl_epmd,erl_reply,error_handler,error_logger,
   erts_debug,file,file_io_server,file_server,gen_sctp,gen_tcp,gen_udp,global,
   global_group,global_search,group,heart,hipe_unified_loader,inet,inet6_sctp,
   inet6_tcp,inet6_tcp_dist,inet6_udp,inet_config,inet_db,inet_dns,
   inet_gethost_native,inet_hosts,inet_parse,inet_res,inet_sctp,inet_tcp,
   inet_tcp_dist,inet_udp,kernel,kernel_config,net,net_adm,net_kernel,os,pg2,
   ram_file,rpc,seq_trace,standard_error,user,user_drv,user_sup,wrap_log_reader,
   megaco,megaco_ber_encoder,megaco_ber_media_gateway_control_prev3a,
   megaco_ber_media_gateway_control_prev3b,
   megaco_ber_media_gateway_control_prev3c,megaco_ber_media_gateway_control_v1,
   megaco_ber_media_gateway_control_v2,megaco_ber_media_gateway_control_v3,
   megaco_binary_encoder,megaco_binary_encoder_lib,
   megaco_binary_name_resolver_prev3a,megaco_binary_name_resolver_prev3b,
   megaco_binary_name_resolver_prev3c,megaco_binary_name_resolver_v1,
   megaco_binary_name_resolver_v2,megaco_binary_name_resolver_v3,
   megaco_binary_term_id,megaco_binary_term_id_gen,
   megaco_binary_transformer_prev3a,megaco_binary_transformer_prev3b,
   megaco_binary_transformer_prev3c,megaco_binary_transformer_v1,
   megaco_binary_transformer_v2,megaco_binary_transformer_v3,
   megaco_compact_text_encoder,megaco_compact_text_encoder_prev3a,
   megaco_compact_text_encoder_prev3b,megaco_compact_text_encoder_prev3c,
   megaco_compact_text_encoder_v1,megaco_compact_text_encoder_v2,
   megaco_compact_text_encoder_v3,megaco_config,megaco_config_misc,
   megaco_digit_map,megaco_edist_compress,megaco_encoder,
   megaco_erl_dist_encoder,megaco_erl_dist_encoder_mc,megaco_filter,
   megaco_flex_scanner,megaco_flex_scanner_handler,megaco_messenger,
   megaco_messenger_misc,megaco_misc_sup,megaco_monitor,megaco_per_encoder,
   megaco_per_media_gateway_control_prev3a,
   megaco_per_media_gateway_control_prev3b,
   megaco_per_media_gateway_control_prev3c,megaco_per_media_gateway_control_v1,
   megaco_per_media_gateway_control_v2,megaco_per_media_gateway_control_v3,
   megaco_pretty_text_encoder,megaco_pretty_text_encoder_prev3a,
   megaco_pretty_text_encoder_prev3b,megaco_pretty_text_encoder_prev3c,
   megaco_pretty_text_encoder_v1,megaco_pretty_text_encoder_v2,
   megaco_pretty_text_encoder_v3,megaco_sdp,megaco_stats,megaco_sup,megaco_tcp,
   megaco_tcp_accept,megaco_tcp_accept_sup,megaco_tcp_connection,
   megaco_tcp_connection_sup,megaco_tcp_sup,megaco_text_mini_decoder,
   megaco_text_mini_parser,megaco_text_parser_prev3a,megaco_text_parser_prev3b,
   megaco_text_parser_prev3c,megaco_text_parser_v1,megaco_text_parser_v2,
   megaco_text_parser_v3,megaco_text_scanner,megaco_timer,megaco_trans_sender,
   megaco_trans_sup,megaco_transport,megaco_udp,megaco_udp_server,
   megaco_udp_sup,megaco_user_default,mnesia,mnesia_backup,mnesia_bup,
   mnesia_checkpoint,mnesia_checkpoint_sup,mnesia_controller,mnesia_dumper,
   mnesia_event,mnesia_frag,mnesia_frag_hash,mnesia_frag_old_hash,mnesia_index,
   mnesia_kernel_sup,mnesia_late_loader,mnesia_lib,mnesia_loader,mnesia_locker,
   mnesia_log,mnesia_monitor,mnesia_recover,mnesia_registry,mnesia_schema,
   mnesia_snmp_hook,mnesia_snmp_sup,mnesia_sp,mnesia_subscr,mnesia_sup,
   mnesia_text,mnesia_tm,cdv_atom_cb,cdv_bin_cb,cdv_detail_wx,cdv_dist_cb,
   cdv_ets_cb,cdv_fun_cb,cdv_gen_cb,cdv_html_wx,cdv_info_wx,cdv_int_tab_cb,
   cdv_mem_cb,cdv_mod_cb,cdv_multi_wx,cdv_port_cb,cdv_proc_cb,cdv_sched_cb,
   cdv_table_wx,cdv_term_cb,cdv_timer_cb,cdv_virtual_list_wx,cdv_wx,
   crashdump_viewer,etop,etop_tr,etop_txt,observer,observer_alloc_wx,
   observer_app_wx,observer_html_lib,observer_lib,observer_perf_wx,
   observer_pro_wx,observer_procinfo,observer_sys_wx,observer_trace_wx,
   observer_traceoptions_wx,observer_tv_table,observer_tv_wx,observer_wx,ttb,
   ttb_et,odbc,odbc_app,odbc_sup,'CosNaming_Binding',
   'CosNaming_BindingIterator','CosNaming_BindingIterator_impl',
   'CosNaming_BindingList','CosNaming_Name','CosNaming_NameComponent',
   'CosNaming_NamingContext','CosNaming_NamingContextExt',
   'CosNaming_NamingContextExt_InvalidAddress',
   'CosNaming_NamingContextExt_impl','CosNaming_NamingContext_AlreadyBound',
   'CosNaming_NamingContext_CannotProceed',
   'CosNaming_NamingContext_InvalidName','CosNaming_NamingContext_NotEmpty',
   'CosNaming_NamingContext_NotFound','OrberApp_IFR','OrberApp_IFR_impl',any,
   cdr_decode,cdr_encode,cdrlib,corba,corba_boa,corba_object,erlang_binary,
   erlang_pid,erlang_port,erlang_ref,fixed,iop_ior,lname,lname_component,
   oe_CORBA,oe_OrberIFR,oe_cos_naming,oe_cos_naming_ext,oe_erlang,orber,
   orber_acl,orber_cosnaming_utils,orber_diagnostics,orber_env,orber_exceptions,
   orber_ifr,orber_ifr_aliasdef,orber_ifr_arraydef,orber_ifr_attributedef,
   orber_ifr_constantdef,orber_ifr_contained,orber_ifr_container,
   orber_ifr_enumdef,orber_ifr_exceptiondef,orber_ifr_fixeddef,
   orber_ifr_idltype,orber_ifr_interfacedef,orber_ifr_irobject,
   orber_ifr_moduledef,orber_ifr_operationdef,orber_ifr_orb,
   orber_ifr_primitivedef,orber_ifr_repository,orber_ifr_sequencedef,
   orber_ifr_stringdef,orber_ifr_structdef,orber_ifr_typecode,orber_ifr_typedef,
   orber_ifr_uniondef,orber_ifr_utils,orber_ifr_wstringdef,orber_iiop,
   orber_iiop_inproxy,orber_iiop_inrequest,orber_iiop_insup,orber_iiop_net,
   orber_iiop_net_accept,orber_iiop_outproxy,orber_iiop_outsup,orber_iiop_pm,
   orber_iiop_socketsup,orber_iiop_tracer,orber_iiop_tracer_silent,
   orber_iiop_tracer_stealth,orber_initial_references,orber_objectkeys,orber_pi,
   orber_request_number,orber_socket,orber_tb,orber_tc,orber_typedefs,orber_web,
   orber_web_server,cpu_sup,disksup,memsup,nteventlog,os_mon,os_mon_mib,
   os_mon_sysinfo,os_sup,ose,otp_mib,leex,yecc,yeccparser,yeccscan,egd,egd_font,
   egd_png,egd_primitives,egd_render,percept,percept_analyzer,percept_db,
   percept_graph,percept_html,percept_image,'OTP-PUB-KEY','PKCS-FRAME',
   pubkey_cert,pubkey_cert_records,pubkey_crl,pubkey_pbe,pubkey_pem,pubkey_ssh,
   public_key,reltool,reltool_app_win,reltool_fgraph,reltool_fgraph_win,
   reltool_mod_win,reltool_server,reltool_sys_win,reltool_target,reltool_utils,
   appmon_info,dbg,dyntrace,erts_alloc_config,observer_backend,percept_profile,
   runtime_tools,runtime_tools_sup,system_information,ttb_autostart,
   alarm_handler,erlsrv,format_lib_supp,misc_supp,overload,rb,rb_format_supp,
   release_handler,release_handler_1,sasl,sasl_report,sasl_report_file_h,
   sasl_report_tty_h,si,si_sasl_supp,systools,systools_lib,systools_make,
   systools_rc,systools_relup,snmp,snmp_app,snmp_app_sup,snmp_community_mib,
   snmp_conf,snmp_config,snmp_framework_mib,snmp_generic,snmp_generic_mnesia,
   snmp_index,snmp_log,snmp_mini_mib,snmp_misc,snmp_note_store,
   snmp_notification_mib,snmp_pdus,snmp_shadow_table,snmp_standard_mib,
   snmp_target_mib,snmp_user_based_sm_mib,snmp_usm,snmp_verbosity,
   snmp_view_based_acm_mib,snmpa,snmpa_acm,snmpa_agent,snmpa_agent_sup,
   snmpa_app,snmpa_authentication_service,snmpa_conf,snmpa_discovery_handler,
   snmpa_discovery_handler_default,snmpa_error,snmpa_error_io,
   snmpa_error_logger,snmpa_error_report,snmpa_local_db,snmpa_mib,
   snmpa_mib_data,snmpa_mib_data_tttn,snmpa_mib_lib,snmpa_mib_storage,
   snmpa_mib_storage_dets,snmpa_mib_storage_ets,snmpa_mib_storage_mnesia,
   snmpa_misc_sup,snmpa_mpd,snmpa_net_if,snmpa_net_if_filter,
   snmpa_network_interface,snmpa_network_interface_filter,
   snmpa_notification_delivery_info_receiver,snmpa_notification_filter,
   snmpa_set,snmpa_set_lib,snmpa_set_mechanism,snmpa_supervisor,snmpa_svbl,
   snmpa_symbolic_store,snmpa_target_cache,snmpa_trap,snmpa_usm,snmpa_vacm,
   snmpc,snmpc_lib,snmpc_mib_gram,snmpc_mib_to_hrl,snmpc_misc,snmpc_tok,snmpm,
   snmpm_conf,snmpm_config,snmpm_misc_sup,snmpm_mpd,snmpm_net_if,
   snmpm_net_if_filter,snmpm_net_if_mt,snmpm_network_interface,
   snmpm_network_interface_filter,snmpm_server,snmpm_server_sup,
   snmpm_supervisor,snmpm_user,snmpm_user_default,snmpm_user_old,snmpm_usm,ssh,
   ssh_acceptor,ssh_acceptor_sup,ssh_app,ssh_auth,ssh_bits,ssh_channel,
   ssh_channel_sup,ssh_cli,ssh_client_key_api,ssh_connection,
   ssh_connection_handler,ssh_connection_sup,ssh_daemon_channel,ssh_file,
   ssh_info,ssh_io,ssh_message,ssh_no_io,ssh_server_key_api,ssh_sftp,ssh_sftpd,
   ssh_sftpd_file,ssh_sftpd_file_api,ssh_shell,ssh_subsystem_sup,ssh_sup,
   ssh_system_sup,ssh_transport,ssh_xfer,sshc_sup,sshd_sup,dtls,dtls_connection,
   dtls_connection_sup,dtls_handshake,dtls_record,dtls_v1,inet6_tls_dist,
   inet_tls_dist,ssl,ssl_alert,ssl_app,ssl_certificate,ssl_cipher,ssl_config,
   ssl_connection,ssl_crl,ssl_crl_cache,ssl_crl_cache_api,ssl_dist_sup,
   ssl_handshake,ssl_listen_tracker_sup,ssl_manager,ssl_pkix_db,ssl_record,
   ssl_session,ssl_session_cache,ssl_session_cache_api,ssl_socket,
   ssl_srp_primes,ssl_sup,ssl_tls_dist_proxy,ssl_v2,ssl_v3,tls,tls_connection,
   tls_connection_sup,tls_handshake,tls_record,tls_v1,array,base64,beam_lib,
   binary,c,calendar,dets,dets_server,dets_sup,dets_utils,dets_v8,dets_v9,dict,
   digraph,digraph_utils,edlin,edlin_expand,epp,erl_anno,erl_bits,erl_compile,
   erl_eval,erl_expand_records,erl_internal,erl_lint,erl_parse,erl_posix_msg,
   erl_pp,erl_scan,erl_tar,error_logger_file_h,error_logger_tty_h,escript,ets,
   eval_bits,file_sorter,filelib,filename,gb_sets,gb_trees,gen,gen_event,
   gen_fsm,gen_server,io,io_lib,io_lib_format,io_lib_fread,io_lib_pretty,lib,
   lists,log_mf_h,maps,math,ms_transform,orddict,ordsets,otp_internal,pool,
   proc_lib,proplists,qlc,qlc_pt,queue,rand,random,re,sets,shell,shell_default,
   slave,sofs,string,supervisor,supervisor_bridge,sys,timer,unicode,win32reg,
   zip,epp_dodger,erl_comment_scan,erl_prettypr,erl_recomment,erl_syntax,
   erl_syntax_lib,erl_tidy,igor,merl,merl_transform,prettypr,erl2html2,
   test_server,test_server_ctrl,test_server_gl,test_server_io,test_server_node,
   test_server_sup,cover,cover_web,cprof,eprof,fprof,instrument,lcnt,make,tags,
   xref,xref_base,xref_compiler,xref_parser,xref_reader,xref_scanner,xref_utils,
   typer,webtool,webtool_sup,gl,glu,wx,wxAcceleratorEntry,wxAcceleratorTable,
   wxActivateEvent,wxArtProvider,wxAuiDockArt,wxAuiManager,wxAuiManagerEvent,
   wxAuiNotebook,wxAuiNotebookEvent,wxAuiPaneInfo,wxAuiSimpleTabArt,wxAuiTabArt,
   wxBitmap,wxBitmapButton,wxBitmapDataObject,wxBoxSizer,wxBrush,wxBufferedDC,
   wxBufferedPaintDC,wxButton,wxCalendarCtrl,wxCalendarDateAttr,wxCalendarEvent,
   wxCaret,wxCheckBox,wxCheckListBox,wxChildFocusEvent,wxChoice,wxChoicebook,
   wxClientDC,wxClipboard,wxClipboardTextEvent,wxCloseEvent,wxColourData,
   wxColourDialog,wxColourPickerCtrl,wxColourPickerEvent,wxComboBox,
   wxCommandEvent,wxContextMenuEvent,wxControl,wxControlWithItems,wxCursor,wxDC,
   wxDCOverlay,wxDataObject,wxDateEvent,wxDatePickerCtrl,wxDialog,wxDirDialog,
   wxDirPickerCtrl,wxDisplayChangedEvent,wxEraseEvent,wxEvent,wxEvtHandler,
   wxFileDataObject,wxFileDialog,wxFileDirPickerEvent,wxFilePickerCtrl,
   wxFindReplaceData,wxFindReplaceDialog,wxFlexGridSizer,wxFocusEvent,wxFont,
   wxFontData,wxFontDialog,wxFontPickerCtrl,wxFontPickerEvent,wxFrame,
   wxGBSizerItem,wxGLCanvas,wxGauge,wxGenericDirCtrl,wxGraphicsBrush,
   wxGraphicsContext,wxGraphicsFont,wxGraphicsMatrix,wxGraphicsObject,
   wxGraphicsPath,wxGraphicsPen,wxGraphicsRenderer,wxGrid,wxGridBagSizer,
   wxGridCellAttr,wxGridCellBoolEditor,wxGridCellBoolRenderer,
   wxGridCellChoiceEditor,wxGridCellEditor,wxGridCellFloatEditor,
   wxGridCellFloatRenderer,wxGridCellNumberEditor,wxGridCellNumberRenderer,
   wxGridCellRenderer,wxGridCellStringRenderer,wxGridCellTextEditor,wxGridEvent,
   wxGridSizer,wxHelpEvent,wxHtmlEasyPrinting,wxHtmlLinkEvent,wxHtmlWindow,
   wxIcon,wxIconBundle,wxIconizeEvent,wxIdleEvent,wxImage,wxImageList,
   wxInitDialogEvent,wxJoystickEvent,wxKeyEvent,wxLayoutAlgorithm,wxListBox,
   wxListCtrl,wxListEvent,wxListItem,wxListItemAttr,wxListView,wxListbook,
   wxLocale,wxLogNull,wxMDIChildFrame,wxMDIClientWindow,wxMDIParentFrame,wxMask,
   wxMaximizeEvent,wxMemoryDC,wxMenu,wxMenuBar,wxMenuEvent,wxMenuItem,
   wxMessageDialog,wxMiniFrame,wxMirrorDC,wxMouseCaptureChangedEvent,
   wxMouseCaptureLostEvent,wxMouseEvent,wxMoveEvent,wxMultiChoiceDialog,
   wxNavigationKeyEvent,wxNotebook,wxNotebookEvent,wxNotifyEvent,wxOverlay,
   wxPageSetupDialog,wxPageSetupDialogData,wxPaintDC,wxPaintEvent,wxPalette,
   wxPaletteChangedEvent,wxPanel,wxPasswordEntryDialog,wxPen,wxPickerBase,
   wxPopupTransientWindow,wxPopupWindow,wxPostScriptDC,wxPreviewCanvas,
   wxPreviewControlBar,wxPreviewFrame,wxPrintData,wxPrintDialog,
   wxPrintDialogData,wxPrintPreview,wxPrinter,wxPrintout,wxProgressDialog,
   wxQueryNewPaletteEvent,wxRadioBox,wxRadioButton,wxRegion,wxSashEvent,
   wxSashLayoutWindow,wxSashWindow,wxScreenDC,wxScrollBar,wxScrollEvent,
   wxScrollWinEvent,wxScrolledWindow,wxSetCursorEvent,wxShowEvent,
   wxSingleChoiceDialog,wxSizeEvent,wxSizer,wxSizerFlags,wxSizerItem,wxSlider,
   wxSpinButton,wxSpinCtrl,wxSpinEvent,wxSplashScreen,wxSplitterEvent,
   wxSplitterWindow,wxStaticBitmap,wxStaticBox,wxStaticBoxSizer,wxStaticLine,
   wxStaticText,wxStatusBar,wxStdDialogButtonSizer,wxStyledTextCtrl,
   wxStyledTextEvent,wxSysColourChangedEvent,wxSystemOptions,wxSystemSettings,
   wxTaskBarIcon,wxTaskBarIconEvent,wxTextAttr,wxTextCtrl,wxTextDataObject,
   wxTextEntryDialog,wxToggleButton,wxToolBar,wxToolTip,wxToolbook,
   wxTopLevelWindow,wxTreeCtrl,wxTreeEvent,wxTreebook,wxUpdateUIEvent,wxWindow,
   wxWindowCreateEvent,wxWindowDC,wxWindowDestroyEvent,wxXmlResource,wx_misc,
   wx_object,wxe_master,wxe_server,wxe_util,xmerl,xmerl_b64Bin,
   xmerl_b64Bin_scan,xmerl_eventp,xmerl_html,xmerl_lib,xmerl_otpsgml,
   xmerl_regexp,xmerl_sax_old_dom,xmerl_sax_parser,xmerl_sax_parser_latin1,
   xmerl_sax_parser_list,xmerl_sax_parser_utf16be,xmerl_sax_parser_utf16le,
   xmerl_sax_parser_utf8,xmerl_sax_simple_dom,xmerl_scan,xmerl_sgml,
   xmerl_simple,xmerl_text,xmerl_ucs,xmerl_uri,xmerl_validate,xmerl_xlate,
   xmerl_xml,xmerl_xpath,xmerl_xpath_lib,xmerl_xpath_parse,xmerl_xpath_pred,
   xmerl_xpath_scan,xmerl_xs,xmerl_xsd,xmerl_xsd_type].