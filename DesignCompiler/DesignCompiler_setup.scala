package EasonLib.DesignCompiler

case class DesignCompiler_setup (Setup_config: DesignCompiler_config){
  def get_dc_setup(Setup_config: DesignCompiler_config) :String = {
    var script = ""
    script += """
    #define_design_lib work -path ./elab
    history keep 500
    set enable_page_mode false
    set sh_enable_page_mode false
    set compile_seqmap_identify_shift_registers false
    set compile_seqmap_identify_shift_registers_with_synchronous_logic false
    set timing_enable_multiple_clocks_per_reg true
    """
    script += s"""
    set lib_path          "${Setup_config.lib_path}"
    """
    script += "set search_path       \". $lib_path\""+"\n"
    script += s"""
    set target_library    \"${Setup_config.target_library} \\
                            \"
    """
    script += "set synthetic_library \"dw_foundation.sldb\""+"\n"
    script +="""
    set link_library      " * \
                           $target_library \
                           $synthetic_library"
    """
    script
  }

  def get_dc_constraint(Setup_config: DesignCompiler_config) :String = {
    val period = (1000 / Setup_config.freq).toInt
    val setup_uncertainty = (period * 0.1).toInt
    val hold_uncertainty = (period * 0.1).toInt
    val clk_transition = (period * 0.1).toInt

    var script = ""
    script += s"""
    set     CLK_SOURCE_LATENCY  		        1
    set     CLK_SOURCE_LATENCY  		        1
    set     CLK_NETWORK_LATENCY 		        1
    set     OUT_LOAD                        0.05
    set     MAX_FANOUT                      20
    set     MAX_CAP                         0.4
    set     MAX_TRAN                        2.0
    set     PERIOD                          $period
    """
    script += """
    set ALL_IN_EX_CLK       [remove_from_collection [all_inputs] [get_ports clk]]
    set ALL_EX_OUT          [remove_from_collection [current_design] [all_outputs]]
    set ALL_EX_OUT_IN       [remove_from_collection $ALL_EX_OUT [all_inputs]]
    create_clock [get_ports clk] -name clk -period $PERIOD -waveform [list 0 [expr $PERIOD * 0.5]]
    """
    script += s"""
    set_clock_uncertainty  -setup   $setup_uncertainty       [get_clocks clk]
    set_clock_uncertainty  -hold    $hold_uncertainty        [get_clocks clk]
    set_clock_transition            $clk_transition          [get_clocks clk]
    """
    script += """
    set_clock_latency      -source 	$CLK_SOURCE_LATENCY 	   [get_clocks clk]
    set_clock_latency            	  $CLK_NETWORK_LATENCY 	   [get_clocks clk]

    set_input_delay   [expr $PERIOD * 0.3]    -clock   clk   $ALL_IN_EX_CLK  -add_delay
    set_output_delay  [expr $PERIOD * 0.5]    -clock   clk   [all_outputs]   -add_delay
    set_input_transition   1.0   $ALL_IN_EX_CLK

    set_load                        $OUT_LOAD [all_outputs]
    set_max_transition              $MAX_TRAN                $ALL_EX_OUT_IN
    set_max_fanout                  $MAX_FANOUT              $ALL_EX_OUT_IN
    set_max_capacitance             $MAX_CAP                 $ALL_EX_OUT_IN
    """
    script
  }
//   set_input_delay   [expr $PERIOD * 0.4]    -clock   clk   $ALL_IN_EX_CLK  -add_delay
//    set_output_delay  [expr $PERIOD * 0.6]    -clock   clk   [all_outputs]   -add_delay
  def get_dc_core(Setup_config: DesignCompiler_config, module_name: String, module_path: String) :String = {
    var script = ""
    script += s"""
    set TOP_MODULE ${module_name}
    set compile_enable_register_merging    true
    set compile_seqmap_propagate_constants true
    set verilogout_no_tri   "true"
    set verilogout_equation "false"
    set mv_default_level_shifter_voltage_range_infinity true

    source -e -v "dc_setup.tcl"

    set change_names_dont_change_bus_members      true
    set compile_disable_hierarchical_inverter_opt true
    set auto_insert_level_shifters_on_clocks      all
    set auto_insert_level_shifters true
    analyze -format verilog "${module_path}"  -define DC_SYN
    """
    script += """
    elaborate       $TOP_MODULE
    current_design  $TOP_MODULE
    link
    source -e -v "constraint.tcl"

    report_clock
    check_design > ./rpt/check_deisgn.rpt
    check_timing  > ./rpt/check_timing.rpt
    set_clock_gating_style -setup 0.3 -hold 0 -pos integrated -neg integrated -max_fanout 32 -control_point before -control_signal scan_enable
    check_timing
    group_path -name INPUT_GROUP -from [all_inputs ]
    group_path -name OUTPUT_GROUP -to  [all_outputs]
    set upf_create_implicit_supply_sets false

    set_fix_multiple_port_nets -all -buffer_constants
    set_fix_multiple_port_nets -all -buffer_constants [all_designs]
    compile_ultra -no_autoungroup  -no_boundary_optimization  -gate_clock
    #compile_ultra -timing_high_effort_script -no_autoungroup  -no_boundary_optimization
    #compile_ultra -timing_high_effort_script -no_autoungroup  -no_boundary_optimization -incremental

    set verilogout_no_tri   "true"
    set verilogout_equation "false"
    change_names -hier -rules verilog

    write_sdc                                ./outputs/$date/${TOP_MODULE}.sdc
    write -format ddc     -hierarchy -output ./outputs/$date/${TOP_MODULE}.ddc
    write -format verilog -hierarchy -output ./outputs/$date/${TOP_MODULE}.v
    write_link_library -out                  ./outputs/$date/link_library.txt

    report_constraint -all_vio > ./rpt/$date/constrant.rpt
    report_area   -hier        > ./rpt/$date/area.rpt
    report_constraint  -all_violators                         > ./rpt/$date/${TOP_MODULE}_constraint_all_violators.rpt
    check_timing                                              > ./rpt/$date/${TOP_MODULE}_check_timing_final.rpt
    report_timing_requirements                                > ./rpt/$date/${TOP_MODULE}_report_timing_requirements.rpt
    report_timing -transition_time -nets -attributes -nosplit > ./rpt/$date/${TOP_MODULE}_mapped_timing.rpt
    report_area -physical -nosplit -hierarchy                 > ./rpt/$date/${TOP_MODULE}_mapped_area.rpt
    report_power                                              > ./rpt/$date/${TOP_MODULE}_power.rpt
    report_cell                                               > ./rpt/$date/${TOP_MODULE}_cell.rpt
    #report_timing  -delay max -max_paths 50                   > ./rpt/$date/${TOP_MODULE}_timing_max_path.rpt
    report_timing  -delay max -path_type full_clock_expanded -sort_by slack -max_paths 50 -nosplit                   > ./rpt/$date/${TOP_MODULE}_timing_max_path.rpt
    report_timing  -delay min -path_type full_clock_expanded -sort_by slack -max_paths 50 -nosplit                   > ./rpt/$date/${TOP_MODULE}_timing_min_path.rpt
    report_timing  -delay max -path_type full_clock_expanded -sort_by group -max_paths 50 -nosplit                   > ./rpt/$date/${TOP_MODULE}_timing_group_max_path.rpt

    report_reference                                          > ./rpt/$date/${TOP_MODULE}_ref.rpt

    report_power -hierarchy -levels 3                         > ./rpt/$date/${TOP_MODULE}_hie_power.rpt
    report_power -verbose                                     > ./rpt/$date/${TOP_MODULE}_total_power.rpt
    report_clock_gating -gated -verbose                       > ./rpt/$date/${TOP_MODULE}_clock_gating_gated.rpt
    report_clock_gating -ungated -verbose                     > ./rpt/$date/${TOP_MODULE}_clock_gating_ungated.rpt
    exit
    """
    script
  }
}
