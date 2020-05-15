import subprocess

def outlook():
    return subprocess.check_output("pass show outlook", shell=True).rstrip()
