from jinja2 import Environment, FileSystemLoader
import argparse 
import os

if __name__ == "__main__" : 
    env = Environment(loader=FileSystemLoader("templates"))
    makefile_template = env.get_template('Makefile')
    testbench_template = env.get_template('testbench.py')
    drivers_template = env.get_template('common_drivers.py')

    parser = argparse.ArgumentParser(description='CocoTB parsers')
    parser.add_argument('--target_path', type=str, help='cocotb run path')
    parser.add_argument('--simulator', default="verilator", type=str, help='simulator')
    parser.add_argument('--vpath',type=str, help='verilog file path')
    parser.add_argument('--vtop', type=str, help='top module')
    parser.add_argument('--inputs', nargs='+', help='module inputs')

    args = parser.parse_args()

    makefile = makefile_template.render(SIMULATOR=args.simulator, VPATH=args.vpath, VTOP=args.vtop)
    tb = testbench_template.render(inputs=args.inputs)
    libs = drivers_template.render()

    cocoPath = os.path.join(args.target_path,"cocoTest")
    module_cocoPath = os.path.join(cocoPath,args.vtop)
    if not os.path.exists(module_cocoPath):
        os.makedirs(module_cocoPath)

    with open(os.path.join(module_cocoPath,makefile_template.name),'w') as f:
        f.write(makefile)
    with open(os.path.join(module_cocoPath,testbench_template.name),'w') as f:
        f.write(tb)
    with open(os.path.join(module_cocoPath,drivers_template.name),'w') as f:
        f.write(libs)
    with open(os.path.join(module_cocoPath,"run.sh"),'w') as f:
        f.write("#/usr/bin/sh\nmake | tee cocotest.log")

    os.system(f"chmod 755 {os.path.join(module_cocoPath,'run.sh')}")