import re, sys, subprocess


def individual_test(dir_name,root_filename):
    failure_collection = []
    ca_ref_file=dir_name+root_filename+".ca_ref"
    ca_gen_file=dir_name+root_filename+".ca"
    c_test_file=dir_name+root_filename+".c"
    try:
        subprocess.check_call("frama-c","-flatac",c_test_file)
    except CalledProcessError as (errno):
        print "[Frama-c/FLATAC failure] Call to {0} returned {1}".format(c_test_file,errno)
        failure_collection.append(c_test_file)
        return failure_collection
    try:
        subprocess.check_call("cmp",ca_gen_file,ca_ref_file)
        print "[PASSED] : {0} \n".format(c_test_file)
        return []
    
    except CalledProcessError as (errno):
        print "[Frama-c/FLATAC failure] Call to {0} returned {1}".format(c_test_file,errno)
        failure_collection.append(c_test_file)
        return failure_collection
    

def check_each_dir(dir_list):
    failed_test=[]
    dir_name=re.search('.*(?=\n)',entry)
    dir_list=sys.listdir(dirname) # List of all files in dirname
    for entry in dir_list:
        root_filename=re.search('.*(?=[.c])',entry)
        failure_list = individual_test(entry,root_filename)
        failed_test.extend(failure_list)
    return failed_test

def runtests(test_dirs):
    try:
        file_descr = open(test_dirs,'r'),
        dir_list = file_descr.readlines(),
        check_each_dir(dir_list)
        
    except IOError as (errno, strerror):
        print "I/O error({0}):{1}".format(errno, strerror),
        return false
        
