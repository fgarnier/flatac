# 
# This set of functions is used to perform automatic verification
# tests on the flatac frama-c plugin
# Flata-c Framac, Verimag 2012
# mail : florent dot garnier at imag dot fr
# 
# The list of all directories that contains the tests shall be defined
# in the file ./test_dirs
# Each directory shall contain some pairs of files (file.c,file.ca_ref)
# This script launch frama-c -flatac on each file file.c and compare
# the output file file.ca to file.ca_ref. Test succeed iff there is
# no difference between file.ca and file.ca_ref. 

import re, sys, subprocess, os, perso_utils

devnullhandle = open ('/dev/null','w')

class bcolors:
    HEADER = '\033[95m'
    OKBLUE = '\033[94m'
    OKGREEN = '\033[92m'
    WARNING = '\033[93m'
    FAIL = '\033[91m'
    ENDC = '\033[0m'

    def disable(self):
        self.HEADER = ''
        self.OKBLUE = ''
        self.OKGREEN = ''
        self.WARNING = ''
        self.FAIL = ''
        self.ENDC = ''

 

#This fonction is used to generate counter automata based modes
#of the test .c files and compare them to the reference files.

def individual_test(dir_name,root_filename):
    failure_collection = []
    ca_gen_file=dir_name+root_filename+".c.nts"
    c_test_file=dir_name+root_filename+".c"
    try:
        print 'calling frama-c -flatac on file {0}{1}.c\n'.format(dir_name,root_filename) 
        subprocess.check_call(['frama-c','-flatac',c_test_file],stdin=None, stdout=devnullhandle, stderr=None, shell=False)
        
    except subprocess.CalledProcessError as errcode:    
        print bcolors.FAIL+"[Frama-c/FLATAC failure]"+bcolors.ENDC+"Call to {0} returned {1}".format(c_test_file,errcode)
        failure_collection.append(c_test_file)
        return failure_collection
    try:
        subprocess.check_call(["parse_n_print",ca_gen_file])
        print bcolors.OKGREEN+"[PASSED]"+bcolors.ENDC+" : {0} \n".format(c_test_file)
        return []
    
    except subprocess.CalledProcessError as errno:
        print  bcolors.FAIL+"[FAILED]"+bcolors.ENDC+"Call to {0} returned {1}".format(c_test_file,errno)
        failure_collection.append(c_test_file)
        return failure_collection

# This fonction is used to compute the reference files

def build_individual_reference(dir_name,root_filename):
    c_test_file=dir_name+root_filename+".c"
    ca_gen_file=dir_name+root_filename+".c.nts"
    ca_gen_file_reference=dir_name+root_filename+".ca_ref"
    failure_collection = []
    try:
        print 'Extracting model from file {0} \n'.format(c_test_file)
        subprocess.check_call(['frama-c','-flatac',c_test_file],stdout=devnullhandle)

    except subprocess.CalledProcessError as errcode:    
        print "[Frama-c/FLATAC failure] Call to {0} returned {1}".format(c_test_file,errcode)
        failure_collection.append(c_test_file)
        return failure_collection 

    try:
        subprocess.check_call(['cp',ca_gen_file,ca_gen_file_reference])
        print "[Reference output sucessfully generated] : {0} \n".format(c_test_file)
        return []
    
    except subprocess.CalledProcessError as errno:
        print "[ Failed to copy ]   {0} to {1} : Error type : {2}".format(ca_gen_file,ca_gen_file_reference,errno)
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



def build_test_suite(dir_list):
    failed_builds=[]
    for dir_entry in dir_list: 
        dir_name_groups=re.search('.*(?=\n)',dir_entry)
        if dir_name_groups != None:
            dir_name=dir_name_groups.group(0)
            print '[BUILDING REF TESTS :] Entering directory {0} \n'.format(dir_name)
            file_list=os.listdir(dir_name) # List of all files in dirname
            print 'file list is {0} \n'.format(file_list)
            for file_entry in file_list:
                root_filename_group=re.search('.*(?=[.]c$)',file_entry)
                if root_filename_group != None:
                    root_filename=root_filename_group.group(0)
                    failure_list = build_individual_reference (dir_name,root_filename)
                    failed_builds.extend(failure_list)

    
    return failed_builds
        


def runtests(test_dirs):
    try:
        file_obj = open(test_dirs,'r')
    except IOError as (errno, strerror):
        print "I/O error({0}):{1}".format(errno, strerror),
        return false

    dir_list = file_obj.readlines()

    if len(sys.argv)==2:
        if sys.argv[1]=='--build_test':
            print 'Building test reference base \n'
            failed_test=build_test_suite(dir_list)
                
            if len(failed_test) ==0:
                print 'Test build has been completed successfuly \n'
            else:
                print 'The following test were not successfuly generated \n'
                for test in failed_test :
                    print '[Failed to build] {0} \n'.format(test)
        else:
            print 'Unknown option {0}\n The only valid option is --build_test \n Test dirs are listed inside the file test_dirs \n'.format(sys.argv[1])

    #The numbre of option is not equals to one    
    else:
        print 'Running regression test suite upon the base of examples: \n'
        failed_test=check_each_dir(dir_list)
        print 'Test summary : \n'

        if len(failed_test)==0:
            print 'TEST SEQUENCE SUCCESSFUL \n'
        else:
            print 'The test below failed : \n'
            for test in failed_test:
                print '[FAILED] {0}'.format(test)
               
        


if __name__ == "__main__":
    print "Running test sequence \n" 
    runtests('./test_dirs')
else:
    print "Not in the main function \n"
    print "Function name : {0} \n".format(__name__)
                    

