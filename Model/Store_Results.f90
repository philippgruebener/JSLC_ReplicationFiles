!#######################################################################
! Module Store Results
!#######################################################################

module Store_Results

    implicit none
    
contains

    subroutine store_hh_problem()
    
        use Globals
        
        ! Value Functions
        open (unit=100,file="Output/VF_EE.txt",action="write",status="replace")
        write (100,*) VF_EE
        close(100)
        
        open (unit=101,file="Output/VF_EU.txt",action="write",status="replace")
        write (101,*) VF_EU
        close(101)
        
        open (unit=102,file="Output/VF_EN.txt",action="write",status="replace")
        write (102,*) VF_EN
        close(102)
        
        open (unit=103,file="Output/VF_UE.txt",action="write",status="replace")
        write (103,*) VF_UE
        close(103)
        
        open (unit=104,file="Output/VF_NE.txt",action="write",status="replace")
        write (104,*) VF_NE
        close(104)
        
        open (unit=105,file="Output/VF_UU.txt",action="write",status="replace")
        write (105,*) VF_UU
        close(105)
        
        open (unit=106,file="Output/VF_UN.txt",action="write",status="replace")
        write (106,*) VF_UN
        close(106)
        
        open (unit=107,file="Output/VF_NU.txt",action="write",status="replace")
        write (107,*) VF_NU
        close(107)
        
        open (unit=108,file="Output/VF_NN.txt",action="write",status="replace")
        write (108,*) VF_NN
        close(108)

        open (unit=108,file="Output/VF_SS.txt",action="write",status="replace")
        write (108,*) VF_SS
        close(108)

        open (unit=108,file="Output/VF_SU.txt",action="write",status="replace")
        write (108,*) VF_SU
        close(108)

        open (unit=108,file="Output/VF_US.txt",action="write",status="replace")
        write (108,*) VF_US
        close(108)

        open (unit=108,file="Output/VF_SN.txt",action="write",status="replace")
        write (108,*) VF_SN
        close(108)

        open (unit=108,file="Output/VF_NS.txt",action="write",status="replace")
        write (108,*) VF_NS
        close(108)

        open (unit=108,file="Output/VF_SE.txt",action="write",status="replace")
        write (108,*) VF_SE
        close(108)

        open (unit=108,file="Output/VF_ES.txt",action="write",status="replace")
        write (108,*) VF_ES
        close(108)
        
        open (unit=109,file="Output/VF_R.txt",action="write",status="replace")
        write (109,*) VF_R
        close(109)
        
        ! Policy Functions
        open (unit=110,file="Output/cpol_EE.txt",action="write",status="replace")
        write (110,*) cpol_EE
        close(110)
        
        open (unit=111,file="Output/apol_EE.txt",action="write",status="replace")
        write (111,*) apol_EE
        close(111)
        
        open (unit=112,file="Output/cpol_EU.txt",action="write",status="replace")
        write (112,*) cpol_EU
        close(112)
        
        open (unit=113,file="Output/apol_EU.txt",action="write",status="replace")
        write (113,*) apol_EU
        close(113)
        
        open (unit=114,file="Output/cpol_EN.txt",action="write",status="replace")
        write (114,*) cpol_EN
        close(114)
        
        open (unit=115,file="Output/apol_EN.txt",action="write",status="replace")
        write (115,*) apol_EN
        close(115)
        
        open (unit=116,file="Output/cpol_UE.txt",action="write",status="replace")
        write (116,*) cpol_UE
        close(116)

        open (unit=117,file="Output/apol_UE.txt",action="write",status="replace")
        write (117,*) apol_UE
        close(117)
        
        open (unit=118,file="Output/cpol_NE.txt",action="write",status="replace")
        write (118,*) cpol_NE
        close(118)
        
        open (unit=119,file="Output/apol_NE.txt",action="write",status="replace")
        write (119,*) apol_NE
        close(119)

        open (unit=120,file="Output/cpol_UU.txt",action="write",status="replace")
        write (120,*) cpol_UU
        close(120)
        
        open (unit=121,file="Output/apol_UU.txt",action="write",status="replace")
        write (121,*) apol_UU
        close(121)
        
        open (unit=122,file="Output/cpol_UN.txt",action="write",status="replace")
        write (122,*) cpol_UN
        close(122)
        
        open (unit=123,file="Output/apol_UN.txt",action="write",status="replace")
        write (123,*) apol_UN
        close(123)
        
        open (unit=124,file="Output/cpol_NU.txt",action="write",status="replace")
        write (124,*) cpol_NU
        close(124)
        
        open (unit=125,file="Output/apol_NU.txt",action="write",status="replace")
        write (125,*) apol_NU
        close(125)

        open (unit=126,file="Output/cpol_NN.txt",action="write",status="replace")
        write (126,*) cpol_NN
        close(126)
        
        open (unit=127,file="Output/apol_NN.txt",action="write",status="replace")
        write (127,*) apol_NN
        close(127)

        open (unit=126,file="Output/cpol_SS.txt",action="write",status="replace")
        write (126,*) cpol_SS
        close(126)
        
        open (unit=127,file="Output/apol_SS.txt",action="write",status="replace")
        write (127,*) apol_SS
        close(127)

        open (unit=126,file="Output/cpol_SN.txt",action="write",status="replace")
        write (126,*) cpol_SN
        close(126)
        
        open (unit=127,file="Output/apol_SN.txt",action="write",status="replace")
        write (127,*) apol_SN
        close(127)

        open (unit=126,file="Output/cpol_NS.txt",action="write",status="replace")
        write (126,*) cpol_NS
        close(126)
        
        open (unit=127,file="Output/apol_NS.txt",action="write",status="replace")
        write (127,*) apol_NS
        close(127)

        open (unit=126,file="Output/cpol_SU.txt",action="write",status="replace")
        write (126,*) cpol_SU
        close(126)
        
        open (unit=127,file="Output/apol_SU.txt",action="write",status="replace")
        write (127,*) apol_SU
        close(127)

        open (unit=126,file="Output/cpol_US.txt",action="write",status="replace")
        write (126,*) cpol_US
        close(126)
        
        open (unit=127,file="Output/apol_US.txt",action="write",status="replace")
        write (127,*) apol_US
        close(127)

        open (unit=126,file="Output/cpol_SE.txt",action="write",status="replace")
        write (126,*) cpol_SE
        close(126)
        
        open (unit=127,file="Output/apol_SE.txt",action="write",status="replace")
        write (127,*) apol_SE
        close(127)

        open (unit=126,file="Output/cpol_ES.txt",action="write",status="replace")
        write (126,*) cpol_ES
        close(126)
        
        open (unit=127,file="Output/apol_ES.txt",action="write",status="replace")
        write (127,*) apol_ES
        close(127)
        
        open (unit=128,file="Output/cpol_R.txt",action="write",status="replace")
        write (128,*) cpol_R
        close(128)
    


        ! Decision Probabilities
        open (unit=129,file="Output/pi_EE_BB_EE.txt",action="write",status="replace")
        write (129,*) pi_EE_BB_EE
        close(129)
        
        open (unit=130,file="Output/pi_EE_BB_EU.txt",action="write",status="replace")
        write (130,*) pi_EE_BB_EU
        close(130)
        
        open (unit=131,file="Output/pi_EE_BB_UE.txt",action="write",status="replace")
        write (131,*) pi_EE_BB_UE
        close(131)
        
        open (unit=132,file="Output/pi_EE_BB_EN.txt",action="write",status="replace")
        write (132,*) pi_EE_BB_EN
        close(132)
        
        open (unit=133,file="Output/pi_EE_BB_NE.txt",action="write",status="replace")
        write (133,*) pi_EE_BB_NE
        close(133)
        
        open (unit=134,file="Output/pi_EE_BB_UU.txt",action="write",status="replace")
        write (134,*) pi_EE_BB_UU
        close(134)
        
        open (unit=135,file="Output/pi_EE_BB_UN.txt",action="write",status="replace")
        write (135,*) pi_EE_BB_UN
        close(135)
        
        open (unit=136,file="Output/pi_EE_BB_NU.txt",action="write",status="replace")
        write (136,*) pi_EE_BB_NU
        close(136)
        
        open (unit=137,file="Output/pi_EE_BB_NN.txt",action="write",status="replace")
        write (137,*) pi_EE_BB_NN
        close(137)
        
        open (unit=138,file="Output/pi_EX_BB_EU.txt",action="write",status="replace")
        write (138,*) pi_EX_BB_EU
        close(138)
        
        open (unit=139,file="Output/pi_EX_BB_EN.txt",action="write",status="replace")
        write (139,*) pi_EX_BB_EN
        close(139)
        
        open (unit=140,file="Output/pi_EX_BB_UU.txt",action="write",status="replace")
        write (140,*) pi_EX_BB_UU
        close(140)
        
        open (unit=141,file="Output/pi_EX_BB_UN.txt",action="write",status="replace")
        write (141,*) pi_EX_BB_UN
        close(141)
        
        open (unit=142,file="Output/pi_EX_BB_NU.txt",action="write",status="replace")
        write (142,*) pi_EX_BB_NU
        close(142)
        
        open (unit=143,file="Output/pi_EX_BB_NN.txt",action="write",status="replace")
        write (143,*) pi_EX_BB_NN
        close(143)
        
        open (unit=144,file="Output/pi_XE_BB_UE.txt",action="write",status="replace")
        write (144,*) pi_XE_BB_UE
        close(144)
        
        open (unit=145,file="Output/pi_XE_BB_NE.txt",action="write",status="replace")
        write (145,*) pi_XE_BB_NE
        close(145)
        
        open (unit=146,file="Output/pi_XE_BB_UU.txt",action="write",status="replace")
        write (146,*) pi_XE_BB_UU
        close(146)
        
        open (unit=147,file="Output/pi_XE_BB_UN.txt",action="write",status="replace")
        write (147,*) pi_XE_BB_UN
        close(147)
        
        open (unit=148,file="Output/pi_XE_BB_NU.txt",action="write",status="replace")
        write (148,*) pi_XE_BB_NU
        close(148)
        
        open (unit=149,file="Output/pi_XE_BB_NN.txt",action="write",status="replace")
        write (149,*) pi_XE_BB_NN
        close(149)
        
        open (unit=150,file="Output/pi_XX_BB_UU.txt",action="write",status="replace")
        write (150,*) pi_XX_BB_UU
        close(150)
        
        open (unit=151,file="Output/pi_XX_BB_UN.txt",action="write",status="replace")
        write (151,*) pi_XX_BB_UN
        close(151)
        
        open (unit=152,file="Output/pi_XX_BB_NU.txt",action="write",status="replace")
        write (152,*) pi_XX_BB_NU
        close(152)
        
        open (unit=153,file="Output/pi_XX_BB_NN.txt",action="write",status="replace")
        write (153,*) pi_XX_BB_NN
        close(153)



        open (unit=129,file="Output/pi_EE_BX_EE.txt",action="write",status="replace")
        write (129,*) pi_EE_BX_EE
        close(129)
        
        open (unit=130,file="Output/pi_EE_BX_ES.txt",action="write",status="replace")
        write (130,*) pi_EE_BX_ES
        close(130)
        
        open (unit=131,file="Output/pi_EE_BX_UE.txt",action="write",status="replace")
        write (131,*) pi_EE_BX_UE
        close(131)
        
        open (unit=132,file="Output/pi_EE_BX_EN.txt",action="write",status="replace")
        write (132,*) pi_EE_BX_EN
        close(132)
        
        open (unit=133,file="Output/pi_EE_BX_NE.txt",action="write",status="replace")
        write (133,*) pi_EE_BX_NE
        close(133)
        
        open (unit=134,file="Output/pi_EE_BX_US.txt",action="write",status="replace")
        write (134,*) pi_EE_BX_US
        close(134)
        
        open (unit=135,file="Output/pi_EE_BX_UN.txt",action="write",status="replace")
        write (135,*) pi_EE_BX_UN
        close(135)
        
        open (unit=136,file="Output/pi_EE_BX_NS.txt",action="write",status="replace")
        write (136,*) pi_EE_BX_NS
        close(136)
        
        open (unit=137,file="Output/pi_EE_BX_NN.txt",action="write",status="replace")
        write (137,*) pi_EE_BX_NN
        close(137)
        
        open (unit=138,file="Output/pi_EX_BX_ES.txt",action="write",status="replace")
        write (138,*) pi_EX_BX_ES
        close(138)
        
        open (unit=139,file="Output/pi_EX_BX_EN.txt",action="write",status="replace")
        write (139,*) pi_EX_BX_EN
        close(139)
        
        open (unit=140,file="Output/pi_EX_BX_US.txt",action="write",status="replace")
        write (140,*) pi_EX_BX_US
        close(140)
        
        open (unit=141,file="Output/pi_EX_BX_UN.txt",action="write",status="replace")
        write (141,*) pi_EX_BX_UN
        close(141)
        
        open (unit=142,file="Output/pi_EX_BX_NS.txt",action="write",status="replace")
        write (142,*) pi_EX_BX_NS
        close(142)
        
        open (unit=143,file="Output/pi_EX_BX_NN.txt",action="write",status="replace")
        write (143,*) pi_EX_BX_NN
        close(143)
        
        open (unit=144,file="Output/pi_XE_BX_UE.txt",action="write",status="replace")
        write (144,*) pi_XE_BX_UE
        close(144)
        
        open (unit=145,file="Output/pi_XE_BX_NE.txt",action="write",status="replace")
        write (145,*) pi_XE_BX_NE
        close(145)
        
        open (unit=146,file="Output/pi_XE_BX_US.txt",action="write",status="replace")
        write (146,*) pi_XE_BX_US
        close(146)
        
        open (unit=147,file="Output/pi_XE_BX_UN.txt",action="write",status="replace")
        write (147,*) pi_XE_BX_UN
        close(147)
        
        open (unit=148,file="Output/pi_XE_BX_NS.txt",action="write",status="replace")
        write (148,*) pi_XE_BX_NS
        close(148)
        
        open (unit=149,file="Output/pi_XE_BX_NN.txt",action="write",status="replace")
        write (149,*) pi_XE_BX_NN
        close(149)
        
        open (unit=150,file="Output/pi_XX_BX_US.txt",action="write",status="replace")
        write (150,*) pi_XX_BX_US
        close(150)
        
        open (unit=151,file="Output/pi_XX_BX_UN.txt",action="write",status="replace")
        write (151,*) pi_XX_BX_UN
        close(151)
        
        open (unit=152,file="Output/pi_XX_BX_NS.txt",action="write",status="replace")
        write (152,*) pi_XX_BX_NS
        close(152)
        
        open (unit=153,file="Output/pi_XX_BX_NN.txt",action="write",status="replace")
        write (153,*) pi_XX_BX_NN
        close(153)
        


        open (unit=129,file="Output/pi_EE_XB_EE.txt",action="write",status="replace")
        write (129,*) pi_EE_XB_EE
        close(129)
        
        open (unit=130,file="Output/pi_EE_XB_EU.txt",action="write",status="replace")
        write (130,*) pi_EE_XB_EU
        close(130)
        
        open (unit=131,file="Output/pi_EE_XB_SE.txt",action="write",status="replace")
        write (131,*) pi_EE_XB_SE
        close(131)
        
        open (unit=132,file="Output/pi_EE_XB_EN.txt",action="write",status="replace")
        write (132,*) pi_EE_XB_EN
        close(132)
        
        open (unit=133,file="Output/pi_EE_XB_NE.txt",action="write",status="replace")
        write (133,*) pi_EE_XB_NE
        close(133)
        
        open (unit=134,file="Output/pi_EE_XB_SU.txt",action="write",status="replace")
        write (134,*) pi_EE_XB_SU
        close(134)
        
        open (unit=135,file="Output/pi_EE_XB_SN.txt",action="write",status="replace")
        write (135,*) pi_EE_XB_SN
        close(135)
        
        open (unit=136,file="Output/pi_EE_XB_NU.txt",action="write",status="replace")
        write (136,*) pi_EE_XB_NU
        close(136)
        
        open (unit=137,file="Output/pi_EE_XB_NN.txt",action="write",status="replace")
        write (137,*) pi_EE_XB_NN
        close(137)
        
        open (unit=138,file="Output/pi_EX_XB_EU.txt",action="write",status="replace")
        write (138,*) pi_EX_XB_EU
        close(138)
        
        open (unit=139,file="Output/pi_EX_XB_EN.txt",action="write",status="replace")
        write (139,*) pi_EX_XB_EN
        close(139)
        
        open (unit=140,file="Output/pi_EX_XB_SU.txt",action="write",status="replace")
        write (140,*) pi_EX_XB_SU
        close(140)
        
        open (unit=141,file="Output/pi_EX_XB_SN.txt",action="write",status="replace")
        write (141,*) pi_EX_XB_SN
        close(141)
        
        open (unit=142,file="Output/pi_EX_XB_NU.txt",action="write",status="replace")
        write (142,*) pi_EX_XB_NU
        close(142)
        
        open (unit=143,file="Output/pi_EX_XB_NN.txt",action="write",status="replace")
        write (143,*) pi_EX_XB_NN
        close(143)
        
        open (unit=144,file="Output/pi_XE_XB_SE.txt",action="write",status="replace")
        write (144,*) pi_XE_XB_SE
        close(144)
        
        open (unit=145,file="Output/pi_XE_XB_NE.txt",action="write",status="replace")
        write (145,*) pi_XE_XB_NE
        close(145)
        
        open (unit=146,file="Output/pi_XE_XB_SU.txt",action="write",status="replace")
        write (146,*) pi_XE_XB_SU
        close(146)
        
        open (unit=147,file="Output/pi_XE_XB_SN.txt",action="write",status="replace")
        write (147,*) pi_XE_XB_SN
        close(147)
        
        open (unit=148,file="Output/pi_XE_XB_NU.txt",action="write",status="replace")
        write (148,*) pi_XE_XB_NU
        close(148)
        
        open (unit=149,file="Output/pi_XE_XB_NN.txt",action="write",status="replace")
        write (149,*) pi_XE_XB_NN
        close(149)
        
        open (unit=150,file="Output/pi_XX_XB_SU.txt",action="write",status="replace")
        write (150,*) pi_XX_XB_SU
        close(150)
        
        open (unit=151,file="Output/pi_XX_XB_SN.txt",action="write",status="replace")
        write (151,*) pi_XX_XB_SN
        close(151)
        
        open (unit=152,file="Output/pi_XX_XB_NU.txt",action="write",status="replace")
        write (152,*) pi_XX_XB_NU
        close(152)
        
        open (unit=153,file="Output/pi_XX_XB_NN.txt",action="write",status="replace")
        write (153,*) pi_XX_XB_NN
        close(153)


        open (unit=129,file="Output/pi_EE_XX_EE.txt",action="write",status="replace")
        write (129,*) pi_EE_XX_EE
        close(129)
        
        open (unit=130,file="Output/pi_EE_XX_ES.txt",action="write",status="replace")
        write (130,*) pi_EE_XX_ES
        close(130)
        
        open (unit=131,file="Output/pi_EE_XX_SE.txt",action="write",status="replace")
        write (131,*) pi_EE_XX_SE
        close(131)
        
        open (unit=132,file="Output/pi_EE_XX_EN.txt",action="write",status="replace")
        write (132,*) pi_EE_XX_EN
        close(132)
        
        open (unit=133,file="Output/pi_EE_XX_NE.txt",action="write",status="replace")
        write (133,*) pi_EE_XX_NE
        close(133)
        
        open (unit=134,file="Output/pi_EE_XX_SS.txt",action="write",status="replace")
        write (134,*) pi_EE_XX_SS
        close(134)
        
        open (unit=135,file="Output/pi_EE_XX_SN.txt",action="write",status="replace")
        write (135,*) pi_EE_XX_SN
        close(135)
        
        open (unit=136,file="Output/pi_EE_XX_NS.txt",action="write",status="replace")
        write (136,*) pi_EE_XX_NS
        close(136)
        
        open (unit=137,file="Output/pi_EE_XX_NN.txt",action="write",status="replace")
        write (137,*) pi_EE_XX_NN
        close(137)
        
        open (unit=138,file="Output/pi_EX_XX_ES.txt",action="write",status="replace")
        write (138,*) pi_EX_XX_ES
        close(138)
        
        open (unit=139,file="Output/pi_EX_XX_EN.txt",action="write",status="replace")
        write (139,*) pi_EX_XX_EN
        close(139)
        
        open (unit=140,file="Output/pi_EX_XX_SS.txt",action="write",status="replace")
        write (140,*) pi_EX_XX_SS
        close(140)
        
        open (unit=141,file="Output/pi_EX_XX_SN.txt",action="write",status="replace")
        write (141,*) pi_EX_XX_SN
        close(141)
        
        open (unit=142,file="Output/pi_EX_XX_NS.txt",action="write",status="replace")
        write (142,*) pi_EX_XX_NS
        close(142)
        
        open (unit=143,file="Output/pi_EX_XX_NN.txt",action="write",status="replace")
        write (143,*) pi_EX_XX_NN
        close(143)
        
        open (unit=144,file="Output/pi_XE_XX_SE.txt",action="write",status="replace")
        write (144,*) pi_XE_XX_SE
        close(144)
        
        open (unit=145,file="Output/pi_XE_XX_NE.txt",action="write",status="replace")
        write (145,*) pi_XE_XX_NE
        close(145)
        
        open (unit=146,file="Output/pi_XE_XX_SS.txt",action="write",status="replace")
        write (146,*) pi_XE_XX_SS
        close(146)
        
        open (unit=147,file="Output/pi_XE_XX_SN.txt",action="write",status="replace")
        write (147,*) pi_XE_XX_SN
        close(147)
        
        open (unit=148,file="Output/pi_XE_XX_NS.txt",action="write",status="replace")
        write (148,*) pi_XE_XX_NS
        close(148)
        
        open (unit=149,file="Output/pi_XE_XX_NN.txt",action="write",status="replace")
        write (149,*) pi_XE_XX_NN
        close(149)
        
        open (unit=150,file="Output/pi_XX_XX_SS.txt",action="write",status="replace")
        write (150,*) pi_XX_XX_SS
        close(150)
        
        open (unit=151,file="Output/pi_XX_XX_SN.txt",action="write",status="replace")
        write (151,*) pi_XX_XX_SN
        close(151)
        
        open (unit=152,file="Output/pi_XX_XX_NS.txt",action="write",status="replace")
        write (152,*) pi_XX_XX_NS
        close(152)
        
        open (unit=153,file="Output/pi_XX_XX_NN.txt",action="write",status="replace")
        write (153,*) pi_XX_XX_NN
        close(153)
    end subroutine
    

    subroutine store_firm_problem()

        use Globals
        
        ! store firm value funtions

        open (unit=100,file="Output/J_EE.txt",action="write",status="replace")
        write (100,*) J_EE
        close(100)
        
        open (unit=100,file="Output/J_EU.txt",action="write",status="replace")
        write (100,*) J_EU
        close(100)
        
        open (unit=100,file="Output/J_ES.txt",action="write",status="replace")
        write (100,*) J_ES
        close(100)
        
        open (unit=100,file="Output/J_EN.txt",action="write",status="replace")
        write (100,*) J_EN
        close(100)
        

        ! store household arrival rates

        open (unit=100,file="Output/lambda_UE_E.txt",action="write",status="replace")
        write (100,*) lambda_UE_E
        close(100)

        open (unit=100,file="Output/lambda_UE_X.txt",action="write",status="replace")
        write (100,*) lambda_UE_X
        close(100)

        open (unit=100,file="Output/lambda_UU.txt",action="write",status="replace")
        write (100,*) lambda_UU
        close(100)

        open (unit=100,file="Output/lambda_UN.txt",action="write",status="replace")
        write (100,*) lambda_UN
        close(100)

        open (unit=100,file="Output/lambda_NU.txt",action="write",status="replace")
        write (100,*) lambda_NU
        close(100)

        open (unit=100,file="Output/lambda_NN.txt",action="write",status="replace")
        write (100,*) lambda_NN
        close(100)

        open (unit=100,file="Output/lambda_SS.txt",action="write",status="replace")
        write (100,*) lambda_SS
        close(100)

        open (unit=100,file="Output/lambda_SE_E.txt",action="write",status="replace")
        write (100,*) lambda_SE_E
        close(100)

        open (unit=100,file="Output/lambda_SE_X.txt",action="write",status="replace")
        write (100,*) lambda_SE_X
        close(100)

        open (unit=100,file="Output/lambda_SN.txt",action="write",status="replace")
        write (100,*) lambda_SN
        close(100)

        open (unit=100,file="Output/lambda_NS.txt",action="write",status="replace")
        write (100,*) lambda_NS
        close(100)

        open (unit=100,file="Output/lambda_SU.txt",action="write",status="replace")
        write (100,*) lambda_SU
        close(100)

        open (unit=100,file="Output/lambda_US.txt",action="write",status="replace")
        write (100,*) lambda_US
        close(100)

        open (unit=100,file="Output/lambda_NE_E.txt",action="write",status="replace")
        write (100,*) lambda_NE_E
        close(100)

        open (unit=100,file="Output/lambda_NE_X.txt",action="write",status="replace")
        write (100,*) lambda_NE_X
        close(100)

    end subroutine
    

    subroutine store_simulation()
    
        use Globals

        ! simulation outputs
        open (unit=200,file="Output/sim_asset.txt",action="write",status="replace")
        write (200,*) sim_asset
        close(200)
        
        open (unit=203,file="Output/sim_LS.txt",action="write",status="replace")
        write (203,*) sim_LS
        close(203)
        
        open (unit=204,file="Output/sim_hh.txt",action="write",status="replace")
        write (204,*) sim_hh
        close(204)
        
        open (unit=205,file="Output/sim_hsp.txt",action="write",status="replace")
        write (205,*) sim_hsp
        close(205)
        
        open (unit=212,file="Output/sim_LS_h.txt",action="write",status="replace")
        write (212,*) sim_LS_h
        close(212)
        
        open (unit=213,file="Output/sim_LS_sp.txt",action="write",status="replace")
        write (213,*) sim_LS_sp
        close(213)
        
        ! transitions        
        open (unit=206,file="Output/trans_join.txt",action="write",status="replace")
        write (206,*) trans_join
        close(206)
        
        open (unit=207,file="Output/trans_h.txt",action="write",status="replace")
        write (207,*) trans_h
        close(207)
        
        open (unit=208,file="Output/trans_sp.txt",action="write",status="replace")
        write (208,*) trans_sp
        close(208)
        
        open (unit=209,file="Output/trans_join_age.txt",action="write",status="replace")
        write (209,*) trans_join_age
        close(209)
        
        open (unit=210,file="Output/trans_h_age.txt",action="write",status="replace")
        write (210,*) trans_h_age
        close(210)
        
        open (unit=211,file="Output/trans_sp_age.txt",action="write",status="replace")
        write (211,*) trans_sp_age
        close(211)
        
        
        open (unit=214,file="Output/trans_join_N.txt",action="write",status="replace")
        write (214,*) trans_join_N
        close(214)
        
        open (unit=215,file="Output/trans_h_N.txt",action="write",status="replace")
        write (215,*) trans_h_N
        close(215)
        
        open (unit=216,file="Output/trans_sp_N.txt",action="write",status="replace")
        write (216,*) trans_sp_N
        close(216)
        
        open (unit=217,file="Output/trans_join_age_N.txt",action="write",status="replace")
        write (217,*) trans_join_age_N
        close(217)
        
        open (unit=218,file="Output/trans_h_age_N.txt",action="write",status="replace")
        write (218,*) trans_h_age_N
        close(218)
        
        open (unit=219,file="Output/trans_sp_age_N.txt",action="write",status="replace")
        write (219,*) trans_sp_age_N
        close(219)

        open (unit=216,file="Output/trans_group_N.txt",action="write",status="replace")
        write (216,*) trans_group_N
        close(216)

        open (unit=216,file="Output/trans_group.txt",action="write",status="replace")
        write (216,*) trans_group
        close(216)

        open (unit=216,file="Output/trans_group_3_N.txt",action="write",status="replace")
        write (216,*) trans_group_3_N
        close(216)

        open (unit=216,file="Output/trans_group_3.txt",action="write",status="replace")
        write (216,*) trans_group_3
        close(216)

        open (unit=220,file="Output/ass_agg.txt",action="write",status="replace")
        write (220,*) ass_agg
        close(220)
        
        open (unit=220,file="Output/ass_agg_med.txt",action="write",status="replace")
        write (220,*) ass_agg_med
        close(220)

         open (unit=221,file="Output/dist_LS_join.txt",action="write",status="replace")
        write (221,*) dist_LS_join
        close(221)

         open (unit=222,file="Output/ass_life.txt",action="write",status="replace")
        write (222,*) ass_life
        close(222)

         open (unit=223,file="Output/inc_life.txt",action="write",status="replace")
        write (223,*) inc_life
        close(223)

         open (unit=224,file="Output/inc_life_std.txt",action="write",status="replace")
        write (224,*) inc_life_std
        close(224)

         open (unit=225,file="Output/dist_LS_indiv.txt",action="write",status="replace")
        write (225,*) dist_LS_indiv
        close(225)

        open (unit=226,file="Output/emp_spells.txt",action="write",status="replace")
        write (226,*) emp_spells
        close(226)
        
    end subroutine
    
    subroutine store_calibration()

        use Globals

        open (unit=300,file="Output/parameters.txt",action="write",status="replace")
        write (300,*) kappa, TT, TD, HH, AA, NN, beta, gam, psi_EE(1), &
                    psi_EU(1), psi_UE(1), psi_EN(1), psi_NE(1), psi_UU(1), psi_UN(1), &
                    psi_NU(1), psi_NN(1), psi_SS(1), psi_SN(1), psi_NS(1), psi_SU(1), &
                    psi_US(1), psi_SE(1), psi_ES(1), sigma_eps, delta_lev, delta_exp, lambda_u, lambda_n, &
                    pen_rep, phiup_lev, phiup_exp, phidown_lev, phidown_exp, &
                    hmin, hmax, Amin, Amax, r, phiUS, ben_rep, ben_rep, distinit_LS
        close(300)

        open (unit=301,file="Output/targets.txt",action="write",status="replace")
        write (301,*) dist_LS_join_cum, dist_LS_join_groups_cum(:,1), dist_LS_join_groups_cum(:,2), dist_LS_join_groups_cum(:,3), dist_LS_join_groups_cum(:,4), &
                      ass_agg, ass_groups, inc_agg, inc_groups, incSD_agg, incSD_groups, &
                      trans_all_3(1,:), trans_all_3(2,:), trans_all_3(3,:), &
                      dw_unemp_2, dw_unemp_6, dw_unemp_12, dw_unemp_24, dw_job_2, dw_job_5, dw_job_8
        close(301)

    end subroutine

    subroutine store_exercise_new()

        use Globals

        open (unit=400,file="Output/AWE_o_base.txt",action="write",status="replace")
        write (400,*) AWE_o_base
        close(400)

        open (unit=401,file="Output/AWE_o_lambda_age.txt",action="write",status="replace")
        write (401,*) AWE_o_lambda_age
        close(401)

        open (unit=401,file="Output/AWE_o_lambda.txt",action="write",status="replace")
        write (401,*) AWE_o_lambda
        close(401)

        open (unit=402,file="Output/AWE_o_hout.txt",action="write",status="replace")
        write (402,*) AWE_o_hout
        close(402)

        open (unit=403,file="Output/AWE_o_hout_lambda.txt",action="write",status="replace")
        write (403,*) AWE_o_hout_lambda
        close(403)

        open (unit=404,file="Output/AWE_o_hemp.txt",action="write",status="replace")
        write (404,*) AWE_o_hemp
        close(404)

        open (unit=404,file="Output/AWE_o_hemp_lambda.txt",action="write",status="replace")
        write (404,*) AWE_o_hemp_lambda
        close(404)

        open (unit=405,file="Output/AWE_o_ass.txt",action="write",status="replace")
        write (405,*) AWE_o_ass
        close(405)

        open (unit=405,file="Output/AWE_o_ass_lambda.txt",action="write",status="replace")
        write (405,*) AWE_o_ass_lambda
        close(405)

        open (unit=406,file="Output/AWE_o_age.txt",action="write",status="replace")
        write (406,*) AWE_o_age
        close(406)

        open (unit=406,file="Output/AWE_o_age_lambda.txt",action="write",status="replace")
        write (406,*) AWE_o_age_lambda
        close(406)

        open (unit=406,file="Output/AWE_o_all.txt",action="write",status="replace")
        write (406,*) AWE_o_all
        close(406)

        open (unit=406,file="Output/AWE_o_all_lambda.txt",action="write",status="replace")
        write (406,*) AWE_o_all_lambda
        close(406)

        open (unit=406,file="Output/AWE_o_sanity.txt",action="write",status="replace")
        write (406,*) AWE_o_sanity
        close(406)
        
        open (unit=400,file="Output/AWE_y_base.txt",action="write",status="replace")
        write (400,*) AWE_y_base
        close(400)

        open (unit=401,file="Output/AWE_y_lambda_age.txt",action="write",status="replace")
        write (401,*) AWE_y_lambda_age
        close(401)

        open (unit=401,file="Output/AWE_y_lambda.txt",action="write",status="replace")
        write (401,*) AWE_y_lambda
        close(401)

        open (unit=402,file="Output/AWE_y_hout.txt",action="write",status="replace")
        write (402,*) AWE_y_hout
        close(402)

        open (unit=403,file="Output/AWE_y_hout_lambda.txt",action="write",status="replace")
        write (403,*) AWE_y_hout_lambda
        close(403)

        open (unit=404,file="Output/AWE_y_hemp.txt",action="write",status="replace")
        write (404,*) AWE_y_hemp
        close(404)

        open (unit=404,file="Output/AWE_y_hemp_lambda.txt",action="write",status="replace")
        write (404,*) AWE_y_hemp_lambda
        close(404)

        open (unit=405,file="Output/AWE_y_ass.txt",action="write",status="replace")
        write (405,*) AWE_y_ass
        close(405)

        open (unit=405,file="Output/AWE_y_ass_lambda.txt",action="write",status="replace")
        write (405,*) AWE_y_ass_lambda
        close(405)

        open (unit=406,file="Output/AWE_y_age.txt",action="write",status="replace")
        write (406,*) AWE_y_age
        close(406)

        open (unit=406,file="Output/AWE_y_age_lambda.txt",action="write",status="replace")
        write (406,*) AWE_y_age_lambda
        close(406)

        open (unit=406,file="Output/AWE_y_all.txt",action="write",status="replace")
        write (406,*) AWE_y_all
        close(406)

        open (unit=406,file="Output/AWE_y_all_lambda.txt",action="write",status="replace")
        write (406,*) AWE_y_all_lambda
        close(406)

        open (unit=406,file="Output/AWE_y_sanity.txt",action="write",status="replace")
        write (406,*) AWE_y_sanity
        close(406)

    end subroutine

    subroutine store_exercise_nonStoch()

        use Globals

        ! weighted
        open (unit=400,file="Output/AWE_nsw_o_base.txt",action="write",status="replace")
        write (400,*) AWE_nsw_o_base
        close(400)

        open (unit=401,file="Output/AWE_nsw_o_lambda_age.txt",action="write",status="replace")
        write (401,*) AWE_nsw_o_lambda_age
        close(401)

        open (unit=401,file="Output/AWE_nsw_o_lambda.txt",action="write",status="replace")
        write (401,*) AWE_nsw_o_lambda
        close(401)

        open (unit=402,file="Output/AWE_nsw_o_hout.txt",action="write",status="replace")
        write (402,*) AWE_nsw_o_hout
        close(402)

        open (unit=403,file="Output/AWE_nsw_o_hout_lambda.txt",action="write",status="replace")
        write (403,*) AWE_nsw_o_hout_lambda
        close(403)

        open (unit=404,file="Output/AWE_nsw_o_hemp.txt",action="write",status="replace")
        write (404,*) AWE_nsw_o_hemp
        close(404)

        open (unit=404,file="Output/AWE_nsw_o_hemp_lambda.txt",action="write",status="replace")
        write (404,*) AWE_nsw_o_hemp_lambda
        close(404)

        open (unit=405,file="Output/AWE_nsw_o_ass.txt",action="write",status="replace")
        write (405,*) AWE_nsw_o_ass
        close(405)

        open (unit=405,file="Output/AWE_nsw_o_ass_lambda.txt",action="write",status="replace")
        write (405,*) AWE_nsw_o_ass_lambda
        close(405)

        open (unit=406,file="Output/AWE_nsw_o_age.txt",action="write",status="replace")
        write (406,*) AWE_nsw_o_age
        close(406)

        open (unit=406,file="Output/AWE_nsw_o_age_lambda.txt",action="write",status="replace")
        write (406,*) AWE_nsw_o_age_lambda
        close(406)

        open (unit=406,file="Output/AWE_nsw_o_all.txt",action="write",status="replace")
        write (406,*) AWE_nsw_o_all
        close(406)

        open (unit=406,file="Output/AWE_nsw_o_all_lambda.txt",action="write",status="replace")
        write (406,*) AWE_nsw_o_all_lambda
        close(406)

        open (unit=406,file="Output/AWE_nsw_o_sanity.txt",action="write",status="replace")
        write (406,*) AWE_nsw_o_sanity
        close(406)
        
        open (unit=400,file="Output/AWE_nsw_y_base.txt",action="write",status="replace")
        write (400,*) AWE_nsw_y_base
        close(400)

        open (unit=401,file="Output/AWE_nsw_y_lambda_age.txt",action="write",status="replace")
        write (401,*) AWE_nsw_y_lambda_age
        close(401)

        open (unit=401,file="Output/AWE_nsw_y_lambda.txt",action="write",status="replace")
        write (401,*) AWE_nsw_y_lambda
        close(401)

        open (unit=402,file="Output/AWE_nsw_y_hout.txt",action="write",status="replace")
        write (402,*) AWE_nsw_y_hout
        close(402)

        open (unit=403,file="Output/AWE_nsw_y_hout_lambda.txt",action="write",status="replace")
        write (403,*) AWE_nsw_y_hout_lambda
        close(403)

        open (unit=404,file="Output/AWE_nsw_y_hemp.txt",action="write",status="replace")
        write (404,*) AWE_nsw_y_hemp
        close(404)

        open (unit=404,file="Output/AWE_nsw_y_hemp_lambda.txt",action="write",status="replace")
        write (404,*) AWE_nsw_y_hemp_lambda
        close(404)

        open (unit=405,file="Output/AWE_nsw_y_ass.txt",action="write",status="replace")
        write (405,*) AWE_nsw_y_ass
        close(405)

        open (unit=405,file="Output/AWE_nsw_y_ass_lambda.txt",action="write",status="replace")
        write (405,*) AWE_nsw_y_ass_lambda
        close(405)

        open (unit=406,file="Output/AWE_nsw_y_age.txt",action="write",status="replace")
        write (406,*) AWE_nsw_y_age
        close(406)

        open (unit=406,file="Output/AWE_nsw_y_age_lambda.txt",action="write",status="replace")
        write (406,*) AWE_nsw_y_age_lambda
        close(406)

        open (unit=406,file="Output/AWE_nsw_y_all.txt",action="write",status="replace")
        write (406,*) AWE_nsw_y_all
        close(406)

        open (unit=406,file="Output/AWE_nsw_y_all_lambda.txt",action="write",status="replace")
        write (406,*) AWE_nsw_y_all_lambda
        close(406)

        open (unit=406,file="Output/AWE_nsw_y_sanity.txt",action="write",status="replace")
        write (406,*) AWE_nsw_y_sanity
        close(406)

        ! unweighted
        open (unit=400,file="Output/AWE_nsuw_o_base.txt",action="write",status="replace")
        write (400,*) AWE_nsuw_o_base
        close(400)

        open (unit=401,file="Output/AWE_nsuw_o_lambda_age.txt",action="write",status="replace")
        write (401,*) AWE_nsuw_o_lambda_age
        close(401)

        open (unit=401,file="Output/AWE_nsuw_o_lambda.txt",action="write",status="replace")
        write (401,*) AWE_nsuw_o_lambda
        close(401)

        open (unit=402,file="Output/AWE_nsuw_o_hout.txt",action="write",status="replace")
        write (402,*) AWE_nsuw_o_hout
        close(402)

        open (unit=403,file="Output/AWE_nsuw_o_hout_lambda.txt",action="write",status="replace")
        write (403,*) AWE_nsuw_o_hout_lambda
        close(403)

        open (unit=404,file="Output/AWE_nsuw_o_hemp.txt",action="write",status="replace")
        write (404,*) AWE_nsuw_o_hemp
        close(404)

        open (unit=404,file="Output/AWE_nsuw_o_hemp_lambda.txt",action="write",status="replace")
        write (404,*) AWE_nsuw_o_hemp_lambda
        close(404)

        open (unit=405,file="Output/AWE_nsuw_o_ass.txt",action="write",status="replace")
        write (405,*) AWE_nsuw_o_ass
        close(405)

        open (unit=405,file="Output/AWE_nsuw_o_ass_lambda.txt",action="write",status="replace")
        write (405,*) AWE_nsuw_o_ass_lambda
        close(405)

        open (unit=406,file="Output/AWE_nsuw_o_age.txt",action="write",status="replace")
        write (406,*) AWE_nsuw_o_age
        close(406)

        open (unit=406,file="Output/AWE_nsuw_o_age_lambda.txt",action="write",status="replace")
        write (406,*) AWE_nsuw_o_age_lambda
        close(406)

        open (unit=406,file="Output/AWE_nsuw_o_all.txt",action="write",status="replace")
        write (406,*) AWE_nsuw_o_all
        close(406)

        open (unit=406,file="Output/AWE_nsuw_o_all_lambda.txt",action="write",status="replace")
        write (406,*) AWE_nsuw_o_all_lambda
        close(406)

        open (unit=406,file="Output/AWE_nsuw_o_sanity.txt",action="write",status="replace")
        write (406,*) AWE_nsuw_o_sanity
        close(406)
        
        open (unit=400,file="Output/AWE_nsuw_y_base.txt",action="write",status="replace")
        write (400,*) AWE_nsuw_y_base
        close(400)

        open (unit=401,file="Output/AWE_nsuw_y_lambda_age.txt",action="write",status="replace")
        write (401,*) AWE_nsuw_y_lambda_age
        close(401)

        open (unit=401,file="Output/AWE_nsuw_y_lambda.txt",action="write",status="replace")
        write (401,*) AWE_nsuw_y_lambda
        close(401)

        open (unit=402,file="Output/AWE_nsuw_y_hout.txt",action="write",status="replace")
        write (402,*) AWE_nsuw_y_hout
        close(402)

        open (unit=403,file="Output/AWE_nsuw_y_hout_lambda.txt",action="write",status="replace")
        write (403,*) AWE_nsuw_y_hout_lambda
        close(403)

        open (unit=404,file="Output/AWE_nsuw_y_hemp.txt",action="write",status="replace")
        write (404,*) AWE_nsuw_y_hemp
        close(404)

        open (unit=404,file="Output/AWE_nsuw_y_hemp_lambda.txt",action="write",status="replace")
        write (404,*) AWE_nsuw_y_hemp_lambda
        close(404)

        open (unit=405,file="Output/AWE_nsuw_y_ass.txt",action="write",status="replace")
        write (405,*) AWE_nsuw_y_ass
        close(405)

        open (unit=405,file="Output/AWE_nsuw_y_ass_lambda.txt",action="write",status="replace")
        write (405,*) AWE_nsuw_y_ass_lambda
        close(405)

        open (unit=406,file="Output/AWE_nsuw_y_age.txt",action="write",status="replace")
        write (406,*) AWE_nsuw_y_age
        close(406)

        open (unit=406,file="Output/AWE_nsuw_y_age_lambda.txt",action="write",status="replace")
        write (406,*) AWE_nsuw_y_age_lambda
        close(406)

        open (unit=406,file="Output/AWE_nsuw_y_all.txt",action="write",status="replace")
        write (406,*) AWE_nsuw_y_all
        close(406)

        open (unit=406,file="Output/AWE_nsuw_y_all_lambda.txt",action="write",status="replace")
        write (406,*) AWE_nsuw_y_all_lambda
        close(406)

        open (unit=406,file="Output/AWE_nsuw_y_sanity.txt",action="write",status="replace")
        write (406,*) AWE_nsuw_y_sanity
        close(406)

    end subroutine

end module
