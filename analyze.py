# Given a project name, analyzes the logs to determine several timing/quality statistics
import sys
import os
from pprint import pprint
from tabulate import tabulate

print(sys.argv)

if len(sys.argv) == 1:
    print("Please provide the project folder name.")
    quit()

project_name = sys.argv[1]
auto_folder = os.path.abspath(project_name)
mono_folder = auto_folder + "_monolithic/"
hier_folder = auto_folder + "_hierarchic/"

folders = [auto_folder, mono_folder, hier_folder]

class Statistic:
    def __init__(self, name, f):
        self.name = name
        self.f = f
    
    def analyze(self, logs):
        try:
            result = self.f(logs)
            return result
        except Exception as e:
            print(f"Exception in stat `{self.name}': {e}")
            return "-"

class Logs:
    def __init__(self, name, folder):
        self.name = name
        self.folder = folder
        self.loaded = False

    def load_logs(self):
        try:
            with open(self.folder + "/yosys.log", 'r') as log:
                self.yosys = log.read().split('\n')

            with open(self.folder + "/nextpnr.err", 'r') as log:
                self.nextpnr = log.read().split('\n')
            self.loaded = True
        except FileNotFoundError:
            print(f"Cannot find logs in {self.folder}.")

# --- Helper functions

def find_first_line(log_lines, search):
    for line in log_lines:
        if search in line:
            return line

def find_lines(log_lines, search):
    lines = []
    for line in log_lines:
        if search in line:
            lines.append(line)
    return lines

def cut(line, delim=' '):
    return list(filter(len, line.split(delim)))
    
# --- Stat filters

def lut_usage(logs):
    usage_str = find_first_line(logs.nextpnr, "Total LUT4s")
    usage = int(cut(usage_str)[3].split('/')[0])
    return usage

def slice_usage(logs):
    usage_str = find_first_line(logs.nextpnr, "TRELLIS_SLICE:")
    usage = int((cut(usage_str)[3]).split('/')[0])
    return usage

def max_frequency(logs):
    freq = find_lines(logs.nextpnr, "Max frequency for clock")[-1]
    freq = cut(freq)[6]
    return freq + " MHz"

def ff_usage(logs):
    usage_str = find_first_line(logs.nextpnr, "Total DFFs")
    usage = int(cut(usage_str)[3].split('/')[0])
    return usage

def synth_time(logs):
    stat_line = find_first_line(logs.yosys, "End of script.")
    last = cut(stat_line, "CPU: user ")[1]
    time = cut(last)[0]
    return time

def heap_time(logs):
    line = find_first_line(logs.nextpnr, "HeAP Placer Time")
    return cut(line)[-1]

def sa_time(logs):
    line = find_first_line(logs.nextpnr, "SA placement time")
    return cut(line)[-1]

def rout1_time(logs):
    line = find_first_line(logs.nextpnr, "Router1 time")
    return cut(line)[-1]

# --- Stat definitions

# TODO (feature): zet een * bij de beste?
stats = [
    Statistic("LUTs used", lut_usage),
    Statistic("Slices used", slice_usage),
    Statistic("Max frequency", max_frequency),
    Statistic("Flip-flops used", ff_usage),
    Statistic("Synthesis time", synth_time),
    Statistic("HeAP placer time", heap_time),
    Statistic("SA placer time", sa_time),
    Statistic("Router1 time", rout1_time)
]

# --- Tooling it allemaal together

auto_log = Logs("Ex-PART", auto_folder)
mono_log = Logs("Monolithic", mono_folder)
hier_log = Logs("Hierarchic", hier_folder)

logs = [auto_log, mono_log, hier_log]
loaded_logs = []

for l in logs:
    l.load_logs()
    if l.loaded:
        loaded_logs.append(l)

headers = ["stat"] + list(map(lambda x: x.name, loaded_logs))
results = []

for stat in stats:
    result = [stat.name]
    for log in loaded_logs:
        v = stat.analyze(log)
        result.append(v)
    results.append(result)


print(tabulate(results, headers = headers))
