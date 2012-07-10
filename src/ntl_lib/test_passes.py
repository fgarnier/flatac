# 
# This set of functions are used to perform automatic verification
# tests for the Numerical Transition Library
# NTL lib, Verimag 2012
#
# 
# List of directory list shall be containes in the file ./test_dirs
# 
# 
# 
# 

import re, sys, subprocess, os

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



def fix_point_detection(dir_name,root_filename):
    failure_collection = []
    nts_file=dir_name+"/"+root_filename+".nts"
    nts_gen_file=dir_name+"/"+root_filename+".nts_dump"
    nts_second_pass_file=dir_name+"/"+root_filename+".nts_dump_dump"
    try:
        print 'Calling parse_n_print upon {0} '.format(nts_file)
        print 'Calling parse_n_print upon {0} '.format(nts_gen_file)
        subprocess.check_call(['./parse_n_print',nts_file])
        subprocess.check_call(['./parse_n_print',nts_gen_file])
        
    except subprocess.CalledProcessError as errcode:    
        print bcolors.FAIL+"[FAILED]"+bcolors.ENDC+"Call to {0} returned {1}".format(nts_file,errcode)
        failure_collection.append(nts_file)
        return failure_collection
    try:
        subprocess.check_call(["cmp",nts_gen_file,nts_second_pass_file])
        print bcolors.OKGREEN+"[PASSED]"+bcolors.ENDC+": {0} \n".format(nts_file)
        return []
    
    except subprocess.CalledProcessError as errno:
        print bcolors.FAIL+"[FAILED]"+bcolors.ENDC+"Call to {0} returned {1}".format(nts_file,errno)
        failure_collection.append(nts_file)
        return failure_collection
    

def check_each_dir(dir_list):
    failed_test=[]
    #print_failed_list(dir_list)
    
    for dir_entry in dir_list: 
        dir_name_groups=re.search('.*(?=\n)',dir_entry)
        dir_name=dir_name_groups.group(0)
        print 'Entering directory {0} \n'.format(dir_name)
        file_list=os.listdir(dir_name) # List of all files in dirname
        if file_list != None :
            for file_entry in file_list:
                root_filename_group=re.search('.*(?=[.]nts)',file_entry)
                if root_filename_group != None:
                    root_filename=root_filename_group.group(0)
                    failure_list = fix_point_detection(dir_name,root_filename)
                    failed_test.extend(failure_list)
                     
    return failed_test


def print_failed_list(flist):
    if len(flist) > 0 :
        for elem in flist:
            print bcolors.FAIL+'[FAILED]'+bcolors.ENDC+' test {0}'.format(elem)
    else :
        print bcolors.OKGREEN+"[NO ERROR REPORTED]"+bcolors.ENDC


def runtests(test_dirs):
    try:
        file_obj = open(test_dirs,'r')
        dir_list = file_obj.readlines()
        failed_test=check_each_dir(dir_list)
        print_failed_list(failed_test)
        if len(failed_test) == 0 :
            print bcolors.OKGREEN+'[ALL TEST PASSED]'+bcolors.ENDC
            return True
        else :
            print bcolors.FAIL+'[SOME TEST FAILED]'+bcolors.ENDC
            print_failed_list(failed_test)
            return False
        
    except IOError as (errno, strerror):
        print bcolors.FAIL+"I/O error({0}):{1}".format(errno, strerror)+bcolors.ENDC,
        return false
        
if __name__ == "__main__":
    print "Running test sequence \n" 
    if runtests('./test_dirs') :
        sys.exit(0)
    else :
        sys.exit(1)
else:
    print "Not in the main function \n"
    print "Function name : {0} \n".format(__name__)
    
