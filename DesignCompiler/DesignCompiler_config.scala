package EasonLib.DesignCompiler

case class process_path (proc:Int){
  val path_map = Map(180 -> "/home/lyc/projects/ic_lib/tsmc180/Technology/TSMC/core/synopsys/db/nopower",
                     28 -> "/home/lyc/projects/ic_lib/tsmc28"
  )
  val lib_map = Map(180 -> "core_slow.db",
                    28 -> "tcbn28hpcplusbwp7t35p140ssg0p81v125c_ccs.db"
  )
}

case class DesignCompiler_config (
                                   process: Int = 180,
                                   freq: Int = 100,
                                   target_low_power: Boolean = false,
                                   target_min_area: Boolean = false,
                                   target_max_preform: Boolean = false,
                                   input_delay_ratio: Double = 0.4,
                                   output_delay_ratio: Double = 0.6

                                 ){
  val lib_path = process_path(process).path_map(process)
  val target_library  = process_path(process).lib_map(process)
}
