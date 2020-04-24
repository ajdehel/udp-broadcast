"""
Module to handle subprocess work and additional components required for the
example.
"""

################################################################################

import os
import signal
import subprocess

################################################################################

#===============================================================================
def start_process(command, logfilename=None, echo=True, **kwargs):
    """
    Run command and return the obj for the subprocess.
    """
    assert isinstance(command, str)
    assert isinstance(logfilename, str) or logfilename is None
    command_list = command.split()
    logfile = open(logfilename, 'w') if logfilename else subprocess.DEVNULL
    new_kwargs = dict(stdout=logfile, stderr=logfile, universal_newlines=True)
    new_kwargs.update(kwargs)
    if echo:
        print(f"`{command}`")
    try:
        process = subprocess.Popen(command_list, **new_kwargs)
    except OSError as e:
        print(f"! Error {e}")
    return process

#===============================================================================
def check_process(process):
    """
    Check to see if a process has finished execution.
    """
    return process.poll() is None

#===============================================================================
def terminate_process(process):
    """
    Send a terminate signal to a running process and return the exit code.
    """
    status = process.poll()
    if status is not None:
        return status
    else:
        process.send_signal(signal.SIGINT)
        return process.poll()

#===============================================================================
def complete_command(command, echo=False):
    """
    Run a command to completion and return the exit code.
    """
    process = start_process(command, echo=echo)
    returncode = process.wait()
    return returncode

#===============================================================================
def output_command(command, echo=False):
    """
    Run and command to completion and return its output.
    """
    process = start_process(command, echo=echo, stdout=subprocess.PIPE)
    returncode = process.wait()
    return process.communicate()

#===============================================================================
def command_and_status(command, fill=70, print_success=False):
    """
    Run a command to completion, print formatted output for success/failure,
    and return True if the returncode is 0.
    """
    returncode = complete_command(command, echo=False)
    if returncode != 0 or print_success:
        command_str = f"`{command}`"
        status_str  =  "SUCCESS" if returncode == 0 else "FAILURE"
        print(f"{command_str:.<70}{status_str}")
    return returncode == 0

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=

#===============================================================================
class IPv4Addr:
    """
    Class to simplify the manipulation of an IPv4 Address.
    """

    #---------------------------------------------------------------------------
    def __init__(self, *args):
        if len(args) == 1:
            assert isinstance(args[0], str)
            self.octets = args[0].split(".")
        else:
            assert len(args) == 4
            self.octets = args
        self.octets = [ int(octet) for octet in self.octets ]

    #---------------------------------------------------------------------------
    def __getitem__(self, index):
        return self.octets[index]

    #---------------------------------------------------------------------------
    def __setitem__(self, index, value):
        assert isinstance(value, int)
        assert value >= 0 and value <= 255
        self.octets[index] = value

    #---------------------------------------------------------------------------
    def __str__(self):
        return ".".join([str(octet) for octet in self.octets])

    #---------------------------------------------------------------------------
    def __format__(self, format_spec):
        return str(self).__format__(format_spec)

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=

#===============================================================================
def has_root_privileges():
    """
    Return True if the process currently has root privileges.
    """
    return os.geteuid() == 0

