/* 
    loader.lgt
*/

:- initialization((
		logtalk_load(library(basic_types_loader)), 
		logtalk_load(library(metapredicates_loader)),
		logtalk_load(demo)
	)).