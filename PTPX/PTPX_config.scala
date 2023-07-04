package EasonLib.PTPX

case class process_path (proc:Int){
  val path_map = Map(180 -> "/mnt/data/projects/ic_lib/tsmc180/Technology/TSMC/core/synopsys/db/nopower",
                     28 -> "/mnt/data/projects/ic_lib/tsmc28"
  )
  val lib_map = Map(180 -> "core_typ.db",
                    28 -> "tcbn28hpcplusbwp7t35p140tt0p8v25c_ccs.db"
  )
}

case class PTPX_config (
                                   process: Int = 180,
                                   freq: Int = 100
                                 ){
  val lib_path = process_path(process).path_map(process)
  val target_library  = process_path(process).lib_map(process)
}
