package EasonLib.PTPX

case class PTPX_setup (Setup_config: PTPX_config){
  def get_ptpx_setup(Setup_config: PTPX_config) :String = {
    var script = ""

    script += s"""
    set lib_path          "${Setup_config.lib_path}"
    """
    script += "set search_path       \". $lib_path\""+"\n"
    script += s"""
    set target_library    \"${Setup_config.target_library} \\
                            \"
    """
    script +="""
    set link_library      " * \
                           $target_library"
    """
    script
  }

  def get_ptpx_main(Setup_config: PTPX_config,
                    module_name: String, 
                    design_path: String,
                    start_time: Float,
                    end_time: Float
                    ) :String = {
    var script = ""
    script += s"""
    set DESIGN_NAME ${module_name}
    set SDC "${design_path}/syn/${module_name}/outputs/test/${module_name}.sdc"
    set NETLIST "${design_path}/syn/${module_name}/outputs/test/${module_name}.v"
    set SAD "${design_path}/simWorkspace/${module_name}/test.vcd"   
    """ 

    script += """
    set power_enable_analysis TRUE
    set power_analysis_mode time_based
    set power_model_preference nlpm
    set auto_wire_load_selection false
    set power_clock_network_include_register_clock_pin_power false

    source -e -v ./ptpx_setup.tcl
    read_verilog $NETLIST
    current_design $DESIGN_NAME
    link

    set_wire_load_mode top
    #set_wire_load_model      -name zero [current_design]
    set auto_wire_load_selection true

    source -e -v $SDC

    """
    
    script += s"read_vcd -time {${start_time} ${end_time}} "
    script += "$SAD  -strip_path \""
    script += s"TOP/${module_name}"
    script += "\"\n"
    script += """
    check_power
    update_power
    report_power -hierarchy -levels 3 > ./rpt/${run_date}/${DESIGN_NAME}_hie.rpt
    report_power -verbose > ./rpt/${run_date}/${DESIGN_NAME}_total.rpt

    report_power -hierarchy -levels 2 -sort_by total_power  > ./rpt/${run_date}/${DESIGN_NAME}_hie_level2_sort.rpt
    report_power -hierarchy -levels 3 -sort_by total_power  > ./rpt/${run_date}/${DESIGN_NAME}_hie_level3_sort.rpt
    report_power -hierarchy -levels 4 -sort_by total_power  > ./rpt/${run_date}/${DESIGN_NAME}_hie_level4_sort.rpt
    report_power -hierarchy -levels 5 -sort_by total_power  > ./rpt/${run_date}/${DESIGN_NAME}_hie_level5_sort.rpt
    report_power -hierarchy -levels 6 -sort_by total_power  > ./rpt/${run_date}/${DESIGN_NAME}_hie_level6_sort.rpt
    report_power -hierarchy -levels 7 -sort_by total_power  > ./rpt/${run_date}/${DESIGN_NAME}_hie_level7_sort.rpt
    report_power -hierarchy -levels 8 -sort_by total_power  > ./rpt/${run_date}/${DESIGN_NAME}_hie_level8_sort.rpt
    report_power -hierarchy -levels 9 -sort_by total_power  > ./rpt/${run_date}/${DESIGN_NAME}_hie_level9_sort.rpt
    report_power -hierarchy -levels 10 -sort_by total_power  > ./rpt/${run_date}/${DESIGN_NAME}_hie_level10_sort.rpt

    #report_power -verbose -cell_power {U_OCT_TOP_0/U_OCT_MEM_TOP_0} > ./rpt/${run_date}/${DESIGN_NAME}_sram_power.rpt
    report_clock_gate_savings -hierarchical -sequential > ./rpt/${run_date}/${DESIGN_NAME}_clock_gate_savings.rpt


    file copy -force ./rpt/${run_date}/${DESIGN_NAME}_hie.rpt new_hie_power.rpt
    file copy -force ./rpt/${run_date}/${DESIGN_NAME}_total.rpt new_total_power.rpt
    exit
    """
    
    script
  }
}
