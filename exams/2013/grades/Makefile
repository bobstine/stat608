###############
#  Make file for parsing grades, building needed tool
#
#     Build the process_grades command does not work 
#     in this directory since refers to other files, so
#     build it in the tools directory.
#
#     Edit the grade file:
#       - remove the excess chars so can see on screen
#	- save as 'undecided-unix' format
#       - identify key columns (zero origin, <cntl>-x = )
#       - insert base key line as first line of answers
#
###############

path = /Users/bob/C/tools


process_grades: $(path)/process_grades.cc
	cd $(path); make process_grades; cd -; cp $(path)/process_grades .


student_scores.txt: ws217001.dat process_grades
	./process_grades -f ws217001.dat -o $@ --name-column 0 --id-column 29 --multiple-versions --questions 44 --answer-column 45 --build-indicators > summary.report

################
