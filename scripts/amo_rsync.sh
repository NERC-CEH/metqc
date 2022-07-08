SHELL=/bin/bash
rsync --perms --chmod=ug=rwX -ahrP -e 'ssh -i /home/users/bkruijt/.ssh/id_rsa_wop -p 2222' --stats --timeout=60 pollution@198.52.46.79:/home/pollution/data/DataLogr/Metmast_MainMet_30min.dat /gws/nopw/j04/ceh_generic/plevy/amo_met/server_mirror/ 
rsync --perms --chmod=ug=rwX -ahrP -e 'ssh -i /home/users/bkruijt/.ssh/id_rsa_wop -p 2222' --stats --timeout=60 pollution@198.52.46.79:/home/pollution/data/DataLogr/Metmast_MainMet_1min.dat  /gws/nopw/j04/ceh_generic/plevy/amo_met/server_mirror/ 
rsync --perms --chmod=ug=rwX -ahrP -e 'ssh -i /home/users/bkruijt/.ssh/id_rsa_wop -p 2222' --stats --timeout=60 pollution@198.52.46.79:/home/pollution/data/DataLogr/UK-AMo_BM_L02_F01.dat     /gws/nopw/j04/ceh_generic/plevy/amo_met/server_mirror/ 
rsync --perms --chmod=ug=rwX -ahrP -e 'ssh -i /home/users/bkruijt/.ssh/id_rsa_wop -p 2222' --stats --timeout=60 pollution@198.52.46.79:/home/pollution/data/DataLogr/UK-AMo_BM_L03_F01.dat     /gws/nopw/j04/ceh_generic/plevy/amo_met/server_mirror/ 
rsync --perms --chmod=ug=rwX -ahrP -e 'ssh -i /home/users/bkruijt/.ssh/id_rsa_wop -p 2222' --stats --timeout=60 pollution@198.52.46.79:/home/pollution/data/DataLogr/UK-AMo_BM_L03_F02.dat     /gws/nopw/j04/ceh_generic/plevy/amo_met/server_mirror/ 
rsync --perms --chmod=ug=rwX -ahrP -e 'ssh -i /home/users/bkruijt/.ssh/id_rsa_wop -p 2222' --stats --timeout=60 pollution@198.52.46.79:/home/pollution/data/DataLogr/UK-AMo_BM_L04_F01.dat     /gws/nopw/j04/ceh_generic/plevy/amo_met/server_mirror/ 
rsync --perms --chmod=ug=rwX -ahrP -e 'ssh -i /home/users/bkruijt/.ssh/id_rsa_wop -p 2222' --stats --timeout=60 pollution@198.52.46.79:/home/pollution/data/DataLogr/UK-AMo_BM_L04_F02.dat     /gws/nopw/j04/ceh_generic/plevy/amo_met/server_mirror/ 
rsync --perms --chmod=ug=rwX -ahrP -e 'ssh -i /home/users/bkruijt/.ssh/id_rsa_wop -p 2222' --stats --timeout=60 pollution@198.52.46.79:/home/pollution/data/DataLogr/UK-AMo_BM_L04_F03.dat     /gws/nopw/j04/ceh_generic/plevy/amo_met/server_mirror/ 
rsync --perms --chmod=ug=rwX -ahrP -e 'ssh -i /home/users/bkruijt/.ssh/id_rsa_wop -p 2222' --stats --timeout=60 pollution@198.52.46.79:/home/pollution/data/DataLogr/UK-AMo_BM_L04_F04.dat     /gws/nopw/j04/ceh_generic/plevy/amo_met/server_mirror/ 
rsync --perms --chmod=ug=rwX -ahrP -e 'ssh -i /home/users/bkruijt/.ssh/id_rsa_wop -p 2222' --stats --timeout=60 pollution@198.52.46.79:/home/pollution/data/DataLogr/UK-AMo_BM_L05_F01.dat     /gws/nopw/j04/ceh_generic/plevy/amo_met/server_mirror/ 

rsync --perms --chmod=ug=rwX -ahrP -e 'ssh -i /home/users/bkruijt/.ssh/id_rsa_wop -p 2222' --stats --timeout=60 pollution@198.52.46.79:/home/pollution/data/ICOS_EC_Mast/ICOS_data/summaries/     /gws/nopw/j04/ceh_generic/plevy/amo_met/server_mirror/ec/summaries/ 

R CMD BATCH --no-restore --no-save /gws/nopw/j04/ceh_generic/plevy/amo_met/icos_uploader.R "/gws/nopw/j04/ceh_generic/plevy/amo_met/console_icos_uploader.Rout"
R CMD BATCH --no-restore --no-save /gws/nopw/j04/ceh_generic/plevy/amo_met/render_icos_plots.R "/gws/nopw/j04/ceh_generic/plevy/amo_met/render_icos_plots.Rout"
