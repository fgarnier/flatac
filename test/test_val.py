# 
# This set of functions are used to perform automatic verification
# tests on the flatac frama-c plugin
# Flata-c Framac, Verimag 2012
# mail : florent dot garnier at imag dot fr
# 
# List of directory list shall be containes in the file ./test_dirs
# Each directory shall contain some pairs of files (file.c,file.ca_ref)
# This script launch frama-c -flatac on each file file.c and compare
# the output file file.ca to file.ca_ref. Test succeed iff there is
# no difference between file.ca and file.ca_ref. 

import re, sys, subprocess, os, perso_utils




def individual_test(dir_name,root_filename):
    failure_collection = []
    ca_ref_file=dir_name+root_filename+".ca_ref"
    ca_gen_file=dir_name+root_filename+".ca"
    c_test_file=dir_name+root_filename+".c"
    try:
        print 'calling frama-c -flatac on file {0}{1}.c\n'.format(dir_name,root_filename) 
        subprocess.check_call(['frama-c','-flatac',c_test_file])
        
    except subprocess.CalledProcessError as errcode:    
        print "[Frama-c/FLATAC failure] Call to {0} returned {1}".format(c_test_file,errcode)
        failure_collection.append(c_test_file)
        return failure_collection
    try:
        subprocess.check_call(["cmp",ca_gen_file,ca_ref_file])
        print "[PASSED] : {0} \n".format(c_test_file)
        return []
    
    except subprocess.CalledProcessError as errno:
        print "[ FAILED ] Call to {0} returned {1}".format(c_test_file,errno)
        failure_collection.append(c_test_file)
        return failure_collection
    

def check_each_dir(dir_list):
    print 'dir list : '
    print dir_list
    print '\n'
    failed_test=[]
    for dir_entry in dir_list: 
        dir_name_groups=re.search('.*(?=\n)',dir_entry)
        if dir_name_groups != None:
            dir_name=dir_name_groups.group(0)
            print '[VALIDITY TESTS :] Entering directory {0} \n'.format(dir_name)
            file_list=os.listdir(dir_name) # List of all files in dirname
            print 'file list is {0} \n'.format(file_list)
            for file_entry in file_list:
                root_filename_group=re.search('.*(?=[.]c$)',file_entry)
                if root_filename_group != None:
                    root_filename=root_filename_group.group(0)
                    failure_list = individual_test(dir_name,root_filename)
                    failed_test.extend(failure_list)
            
    return failed_test

def runtests(test_dirs):
    try:
        file_obj = open(test_dirs,'r')
        dir_list = file_obj.readlines()
        failed_test=check_each_dir(dir_list)
        print 'Test summary : \n'
        if len(failed_test)==0:
            print 'TEST SEQUENCE SUCCESSFUL \n'
        else:
            print 'The test below failed : \n'
            for test in failed_test:
                print '[FAILED] {0}'.format(test)
            
        
    except IOError as (errno, strerror):
        print "I/O error({0}):{1}".format(errno, strerror),
        return false
        
if __name__ == "__main__":
    print "Running test sequence \n" 
    runtests('./test_dirs')
else:
    print "Not in the main function \n"
    print "Function name : {0} \n".format(__name__)
    
