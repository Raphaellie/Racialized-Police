/*-------------------------------------------------------------------------*
 |                                                                         
 |            STATA SUPPLEMENTAL SYNTAX FILE FOR ICPSR 37323
 |        LAW ENFORCEMENT MANAGEMENT AND ADMINISTRATIVE STATISTICS
 |                             (LEMAS), 2016
 |
 |
 | This Stata missing value recode program is provided for optional use with
 | the Stata system version of this data file as distributed by ICPSR.
 | The program replaces user-defined numeric missing values (e.g., -9)
 | with generic system missing "."  Note that Stata allows you to specify
 | up to 27 unique missing value codes.  Only variables with user-defined
 | missing values are included in this program.
 |
 | To apply the missing value recodes, users need to first open the
 | Stata data file on their system, apply the missing value recodes if
 | desired, then save a new copy of the data file with the missing values
 | applied.  Users are strongly advised to use a different filename when
 | saving the new file.
 |
 *------------------------------------------------------------------------*/

replace COUNTY = "" if (COUNTY == "-3")
replace ORI9 = "" if (ORI9 == "-1")
replace FTSAUTH = . if (FTSAUTH == -9)
replace FTSWORN = . if (FTSWORN == -9)
replace FTLIM = . if (FTLIM == -9)
replace FTNON = . if (FTNON == -9)
replace TOTFTEMP = . if (TOTFTEMP == -9)
replace PTSWORN = . if (PTSWORN == -9)
replace PTLIM = . if (PTLIM == -9)
replace PTNON = . if (PTNON == -9)
replace TOTPTEMP = . if (TOTPTEMP == -9)
replace NUMRESPOFF = . if (NUMRESPOFF == -8 | NUMRESPOFF == -9)
replace NUMCPO = . if (NUMCPO == -8 | NUMCPO == -9)
replace NUMSRO = . if (NUMSRO == -8 | NUMSRO == -9)
replace FTRES_SWN = . if (FTRES_SWN == -9)
replace PTRES_SWN = . if (PTRES_SWN == -9)
replace FTRES_LIM = . if (FTRES_LIM == -9)
replace PTRES_LIM = . if (PTRES_LIM == -9)
replace FTRES_NON = . if (FTRES_NON == -9)
replace PTRES_NON = . if (PTRES_NON == -9)
replace ADMIN_SWN = . if (ADMIN_SWN == -8 | ADMIN_SWN == -9)
replace FIELD_SWN = . if (FIELD_SWN == -8 | FIELD_SWN == -9)
replace PATR_SWN = . if (PATR_SWN == -8 | PATR_SWN == -9)
replace DET_SWN = . if (DET_SWN == -8 | DET_SWN == -9)
replace TECH_SWN = . if (TECH_SWN == -8 | TECH_SWN == -9)
replace JAIL_SWN = . if (JAIL_SWN == -8 | JAIL_SWN == -9)
replace COURT_SWN = . if (COURT_SWN == -8 | COURT_SWN == -9)
replace OTHER_SWN = . if (OTHER_SWN == -8 | OTHER_SWN == -9)
replace ADMIN_LIM = . if (ADMIN_LIM == -8 | ADMIN_LIM == -9)
replace FIELD_LIM = . if (FIELD_LIM == -8 | FIELD_LIM == -9)
replace PATR_LIM = . if (PATR_LIM == -8 | PATR_LIM == -9)
replace DET_LIM = . if (DET_LIM == -8 | DET_LIM == -9)
replace TECH_LIM = . if (TECH_LIM == -8 | TECH_LIM == -9)
replace JAIL_LIM = . if (JAIL_LIM == -8 | JAIL_LIM == -9)
replace COURT_LIM = . if (COURT_LIM == -8 | COURT_LIM == -9)
replace OTHER_LIM = . if (OTHER_LIM == -8 | OTHER_LIM == -9)
replace ADMIN_NON = . if (ADMIN_NON == -8 | ADMIN_NON == -9)
replace FIELD_NON = . if (FIELD_NON == -8 | FIELD_NON == -9)
replace PATR_NON = . if (PATR_NON == -8 | PATR_NON == -9)
replace DET_NON = . if (DET_NON == -8 | DET_NON == -9)
replace TECH_NON = . if (TECH_NON == -8 | TECH_NON == -9)
replace JAIL_NON = . if (JAIL_NON == -8 | JAIL_NON == -9)
replace COURT_NON = . if (COURT_NON == -8 | COURT_NON == -9)
replace OTHER_NON = . if (OTHER_NON == -8 | OTHER_NON == -9)
replace OPBUDGET = . if (OPBUDGET == -9.00)
replace OPBUDGET_EST = . if (OPBUDGET_EST == -9)
replace FY_BEGMO = . if (FY_BEGMO == -9)
replace FY_BEGDAY = . if (FY_BEGDAY == -9)
replace FY_ENDMO = . if (FY_ENDMO == -9)
replace FY_ENDDAY = . if (FY_ENDDAY == -9)
replace ASSETFOR = . if (ASSETFOR == -9)
replace ASSETFOR_EST = . if (ASSETFOR_EST == -9)
replace PERS_EDU_MIN = . if (PERS_EDU_MIN == -9)
replace PERS_EDU_HRS = . if (PERS_EDU_HRS == -9)
replace PERS_MIL = . if (PERS_MIL == -8 | PERS_MIL == -9)
replace PERS_CITZN = . if (PERS_CITZN == -9)
replace PERS_TRN_ACAD = . if (PERS_TRN_ACAD == -9)
replace PERS_TRN_FIELD = . if (PERS_TRN_FIELD == -9)
replace PERS_TRN_INSVC = . if (PERS_TRN_INSVC == -9)
replace PERS_BACKINV = . if (PERS_BACKINV == -9)
replace PERS_CREDHIS = . if (PERS_CREDHIS == -9)
replace PERS_CRIMHIS = . if (PERS_CRIMHIS == -9)
replace PERS_DRIVHIS = . if (PERS_DRIVHIS == -9)
replace PERS_SOCMED = . if (PERS_SOCMED == -9)
replace PERS_INTERVW = . if (PERS_INTERVW == -9)
replace PERS_PERSTEST = . if (PERS_PERSTEST == -9)
replace PERS_POLY = . if (PERS_POLY == -9)
replace PERS_PSYCH = . if (PERS_PSYCH == -9)
replace PERS_VOICE = . if (PERS_VOICE == -9)
replace PERS_APTEST = . if (PERS_APTEST == -9)
replace PERS_PROBSOLV = . if (PERS_PROBSOLV == -9)
replace PERS_CULTURE = . if (PERS_CULTURE == -9)
replace PERS_CONFLICT = . if (PERS_CONFLICT == -9)
replace PERS_DRUG = . if (PERS_DRUG == -9)
replace PERS_MED = . if (PERS_MED == -9)
replace PERS_VISN = . if (PERS_VISN == -9)
replace PERS_PHYS = . if (PERS_PHYS == -9)
replace PERS_BILING_SWN = . if (PERS_BILING_SWN == -8 | PERS_BILING_SWN == -9)
replace PERS_BILING_LIM = . if (PERS_BILING_LIM == -8 | PERS_BILING_LIM == -9)
replace PERS_BILING_NON = . if (PERS_BILING_NON == -8 | PERS_BILING_NON == -9)
replace PERS_NEW_WHT = . if (PERS_NEW_WHT == -8 | PERS_NEW_WHT == -9)
replace PERS_NEW_BLK = . if (PERS_NEW_BLK == -8 | PERS_NEW_BLK == -9)
replace PERS_NEW_HSP = . if (PERS_NEW_HSP == -8 | PERS_NEW_HSP == -9)
replace PERS_NEW_IND = . if (PERS_NEW_IND == -8 | PERS_NEW_IND == -9)
replace PERS_NEW_ASN = . if (PERS_NEW_ASN == -8 | PERS_NEW_ASN == -9)
replace PERS_NEW_HAW = . if (PERS_NEW_HAW == -8 | PERS_NEW_HAW == -9)
replace PERS_NEW_TWO = . if (PERS_NEW_TWO == -8 | PERS_NEW_TWO == -9)
replace PERS_NEW_UNK = . if (PERS_NEW_UNK == -8 | PERS_NEW_UNK == -9)
replace PERS_NEW_TOTR = . if (PERS_NEW_TOTR == -8 | PERS_NEW_TOTR == -9)
replace PERS_NEW_MALE = . if (PERS_NEW_MALE == -8 | PERS_NEW_MALE == -9)
replace PERS_NEW_FEM = . if (PERS_NEW_FEM == -8 | PERS_NEW_FEM == -9)
replace PERS_NEW_TOTS = . if (PERS_NEW_TOTS == -8 | PERS_NEW_TOTS == -9)
replace PERS_SEP_WHT = . if (PERS_SEP_WHT == -8 | PERS_SEP_WHT == -9)
replace PERS_SEP_BLK = . if (PERS_SEP_BLK == -8 | PERS_SEP_BLK == -9)
replace PERS_SEP_HSP = . if (PERS_SEP_HSP == -8 | PERS_SEP_HSP == -9)
replace PERS_SEP_IND = . if (PERS_SEP_IND == -8 | PERS_SEP_IND == -9)
replace PERS_SEP_ASN = . if (PERS_SEP_ASN == -8 | PERS_SEP_ASN == -9)
replace PERS_SEP_HAW = . if (PERS_SEP_HAW == -8 | PERS_SEP_HAW == -9)
replace PERS_SEP_TWO = . if (PERS_SEP_TWO == -8 | PERS_SEP_TWO == -9)
replace PERS_SEP_UNK = . if (PERS_SEP_UNK == -8 | PERS_SEP_UNK == -9)
replace PERS_SEP_TOTR = . if (PERS_SEP_TOTR == -8 | PERS_SEP_TOTR == -9)
replace PERS_SEP_MALE = . if (PERS_SEP_MALE == -8 | PERS_SEP_MALE == -9)
replace PERS_SEP_FEM = . if (PERS_SEP_FEM == -8 | PERS_SEP_FEM == -9)
replace PERS_SEP_TOTS = . if (PERS_SEP_TOTS == -8 | PERS_SEP_TOTS == -9)
replace PERS_WHITE_MALE = . if (PERS_WHITE_MALE == -8 | PERS_WHITE_MALE == -9)
replace PERS_BLACK_MALE = . if (PERS_BLACK_MALE == -8 | PERS_BLACK_MALE == -9)
replace PERS_HISP_MALE = . if (PERS_HISP_MALE == -8 | PERS_HISP_MALE == -9)
replace PERS_AMIND_MALE = . if (PERS_AMIND_MALE == -8 | PERS_AMIND_MALE == -9)
replace PERS_ASIAN_MALE = . if (PERS_ASIAN_MALE == -8 | PERS_ASIAN_MALE == -9)
replace PERS_HAWPI_MALE = . if (PERS_HAWPI_MALE == -8 | PERS_HAWPI_MALE == -9)
replace PERS_MULTI_MALE = . if (PERS_MULTI_MALE == -8 | PERS_MULTI_MALE == -9)
replace PERS_UNK_MALE = . if (PERS_UNK_MALE == -8 | PERS_UNK_MALE == -9)
replace PERS_MALE = . if (PERS_MALE == -8 | PERS_MALE == -9)
replace PERS_WHITE_FEM = . if (PERS_WHITE_FEM == -8 | PERS_WHITE_FEM == -9)
replace PERS_BLACK_FEM = . if (PERS_BLACK_FEM == -8 | PERS_BLACK_FEM == -9)
replace PERS_HISP_FEM = . if (PERS_HISP_FEM == -8 | PERS_HISP_FEM == -9)
replace PERS_AMIND_FEM = . if (PERS_AMIND_FEM == -8 | PERS_AMIND_FEM == -9)
replace PERS_ASIAN_FEM = . if (PERS_ASIAN_FEM == -8 | PERS_ASIAN_FEM == -9)
replace PERS_HAWPI_FEM = . if (PERS_HAWPI_FEM == -8 | PERS_HAWPI_FEM == -9)
replace PERS_MULTI_FEM = . if (PERS_MULTI_FEM == -8 | PERS_MULTI_FEM == -9)
replace PERS_UNK_FEM = . if (PERS_UNK_FEM == -8 | PERS_UNK_FEM == -9)
replace PERS_FEMALE = . if (PERS_FEMALE == -8 | PERS_FEMALE == -9)
replace PERS_CHF_SEX = . if (PERS_CHF_SEX == -9)
replace PERS_CHF_RACE = . if (PERS_CHF_RACE == -9)
replace PERS_SUP_INTM_WH = . if (PERS_SUP_INTM_WH == -8 | PERS_SUP_INTM_WH == -9)
replace PERS_SUP_INTM_BK = . if (PERS_SUP_INTM_BK == -8 | PERS_SUP_INTM_BK == -9)
replace PERS_SUP_INTM_HS = . if (PERS_SUP_INTM_HS == -8 | PERS_SUP_INTM_HS == -9)
replace PERS_SUP_INTM_AI = . if (PERS_SUP_INTM_AI == -8 | PERS_SUP_INTM_AI == -9)
replace PERS_SUP_INTM_AS = . if (PERS_SUP_INTM_AS == -8 | PERS_SUP_INTM_AS == -9)
replace PERS_SUP_INTM_HA = . if (PERS_SUP_INTM_HA == -8 | PERS_SUP_INTM_HA == -9)
replace PERS_SUP_INTM_MUL = . if (PERS_SUP_INTM_MUL == -8 | PERS_SUP_INTM_MUL == -9)
replace PERS_SUP_INTM_UNK = . if (PERS_SUP_INTM_UNK == -8 | PERS_SUP_INTM_UNK == -9)
replace PERS_SUP_INTM_TOTR = . if (PERS_SUP_INTM_TOTR == -8 | PERS_SUP_INTM_TOTR == -9)
replace PERS_SUP_INTM_MALE = . if (PERS_SUP_INTM_MALE == -8 | PERS_SUP_INTM_MALE == -9)
replace PERS_SUP_INTM_FEM = . if (PERS_SUP_INTM_FEM == -8 | PERS_SUP_INTM_FEM == -9)
replace PERS_SUP_INTM_TOTS = . if (PERS_SUP_INTM_TOTS == -8 | PERS_SUP_INTM_TOTS == -9)
replace PERS_SUP_SGT_WH = . if (PERS_SUP_SGT_WH == -8 | PERS_SUP_SGT_WH == -9)
replace PERS_SUP_SGT_BK = . if (PERS_SUP_SGT_BK == -8 | PERS_SUP_SGT_BK == -9)
replace PERS_SUP_SGT_HS = . if (PERS_SUP_SGT_HS == -8 | PERS_SUP_SGT_HS == -9)
replace PERS_SUP_SGT_AI = . if (PERS_SUP_SGT_AI == -8 | PERS_SUP_SGT_AI == -9)
replace PERS_SUP_SGT_AS = . if (PERS_SUP_SGT_AS == -8 | PERS_SUP_SGT_AS == -9)
replace PERS_SUP_SGT_HA = . if (PERS_SUP_SGT_HA == -8 | PERS_SUP_SGT_HA == -9)
replace PERS_SUP_SGT_MUL = . if (PERS_SUP_SGT_MUL == -8 | PERS_SUP_SGT_MUL == -9)
replace PERS_SUP_SGT_UNK = . if (PERS_SUP_SGT_UNK == -8 | PERS_SUP_SGT_UNK == -9)
replace PERS_SUP_SGT_TOTR = . if (PERS_SUP_SGT_TOTR == -8 | PERS_SUP_SGT_TOTR == -9)
replace PERS_SUP_SGT_MALE = . if (PERS_SUP_SGT_MALE == -8 | PERS_SUP_SGT_MALE == -9)
replace PERS_SUP_SGT_FEM = . if (PERS_SUP_SGT_FEM == -8 | PERS_SUP_SGT_FEM == -9)
replace PERS_SUP_SGT_TOTS = . if (PERS_SUP_SGT_TOTS == -8 | PERS_SUP_SGT_TOTS == -9)
replace PERS_COLBAR_SWN = . if (PERS_COLBAR_SWN == -9)
replace PERS_COLBAR_LIM = . if (PERS_COLBAR_LIM == -8 | PERS_COLBAR_LIM == -9)
replace PERS_COLBAR_NON = . if (PERS_COLBAR_NON == -8 | PERS_COLBAR_NON == -9)
replace OPER_CFS = . if (OPER_CFS == -9)
replace OPER_CFS_EST = . if (OPER_CFS_EST == -9)
replace OPER_DIS = . if (OPER_DIS == -9)
replace OPER_DIS_EST = . if (OPER_DIS_EST == -9)
replace OPER_CARPAT = . if (OPER_CARPAT == -9)
replace OPER_MOTOPAT = . if (OPER_MOTOPAT == -9)
replace OPER_FOOTPAT = . if (OPER_FOOTPAT == -9)
replace OPER_HORSPAT = . if (OPER_HORSPAT == -9)
replace OPER_BIKEPAT = . if (OPER_BIKEPAT == -9)
replace OPER_SEGPAT = . if (OPER_SEGPAT == -9)
replace OPER_AIRPAT = . if (OPER_AIRPAT == -9)
replace OPER_BOATPAT = . if (OPER_BOATPAT == -9)
replace OPER_OTHPAT = . if (OPER_OTHPAT == -9)
replace OPER_OTHPAT_FLAG = . if (OPER_OTHPAT_FLAG == -8 | OPER_OTHPAT_FLAG == -9)
replace OPER_OFFROAD = . if (OPER_OFFROAD == -9)
replace OPER_SNOWMOB = . if (OPER_SNOWMOB == -9)
replace OPER_UAV = . if (OPER_UAV == -9)
replace OPER_GOLF = . if (OPER_GOLF == -9)
replace CP_MISSION = . if (CP_MISSION == -8 | CP_MISSION == -9)
replace CP_PLAN = . if (CP_PLAN == -9)
replace CP_TECH = . if (CP_TECH == -9)
replace CP_CPACAD = . if (CP_CPACAD == -9)
replace CP_TRN_NEW = . if (CP_TRN_NEW == -8 | CP_TRN_NEW == -9)
replace CP_TRN_INSRV = . if (CP_TRN_INSRV == -9)
replace CP_SARA_NUM = . if (CP_SARA_NUM == -8 | CP_SARA_NUM == -9)
replace CP_BEATS_NUM = . if (CP_BEATS_NUM == -8 | CP_BEATS_NUM == -9)
replace CP_PSP_ADVGRP = . if (CP_PSP_ADVGRP == -9)
replace CP_PSP_BUSGRP = . if (CP_PSP_BUSGRP == -9)
replace CP_PSP_LEA = . if (CP_PSP_LEA == -9)
replace CP_PSP_NEIGH = . if (CP_PSP_NEIGH == -9)
replace CP_PSP_UNIV = . if (CP_PSP_UNIV == -9)
replace CP_PSP_OTH = . if (CP_PSP_OTH == -8 | CP_PSP_OTH == -9)
replace CP_PSP_OTH_FLAG = . if (CP_PSP_OTH_FLAG == -8 | CP_PSP_OTH_FLAG == -9)
replace CP_PSP_SCHOOL = . if (CP_PSP_SCHOOL == -9)
replace CP_PSP_FAITH = . if (CP_PSP_FAITH == -9)
replace CP_PSP_HEALTH = . if (CP_PSP_HEALTH == -9)
replace CP_PSP_GOV = . if (CP_PSP_GOV == -9)
replace CP_PSP_COMM = . if (CP_PSP_COMM == -9)
replace CP_SURVEY = . if (CP_SURVEY == -9)
replace CP_NOSURV = . if (CP_NOSURV == -9)
replace CP_SURV_CRPROB = . if (CP_SURV_CRPROB == -8 | CP_SURV_CRPROB == -9)
replace CP_SURV_RESOURCE = . if (CP_SURV_RESOURCE == -8 | CP_SURV_RESOURCE == -9)
replace CP_SURV_PERFORM = . if (CP_SURV_PERFORM == -8 | CP_SURV_PERFORM == -9)
replace CP_SURV_TRAINING = . if (CP_SURV_TRAINING == -8 | CP_SURV_TRAINING == -9)
replace CP_SURV_POLICY = . if (CP_SURV_POLICY == -8 | CP_SURV_POLICY == -9)
replace EQ_PRM_AGCY = . if (EQ_PRM_AGCY == -88 | EQ_PRM_AGCY == -8 | EQ_PRM_AGCY == -9)
replace EQ_PRM_CASH = . if (EQ_PRM_CASH == -88 | EQ_PRM_CASH == -8 | EQ_PRM_CASH == -9)
replace EQ_PRM_IND = . if (EQ_PRM_IND == -88 | EQ_PRM_IND == -8 | EQ_PRM_IND == -9)
replace EQ_PRM_NOAUTH = . if (EQ_PRM_NOAUTH == -8 | EQ_PRM_NOAUTH == -9)
replace EQ_BCK_AGCY = . if (EQ_BCK_AGCY == -88 | EQ_BCK_AGCY == -8 | EQ_BCK_AGCY == -9)
replace EQ_BCK_CASH = . if (EQ_BCK_CASH == -88 | EQ_BCK_CASH == -8 | EQ_BCK_CASH == -9)
replace EQ_BCK_IND = . if (EQ_BCK_IND == -88 | EQ_BCK_IND == -8 | EQ_BCK_IND == -9)
replace EQ_BCK_NOAUTH = . if (EQ_BCK_NOAUTH == -8 | EQ_BCK_NOAUTH == -9)
replace EQ_BDYARM_AGCY = . if (EQ_BDYARM_AGCY == -88 | EQ_BDYARM_AGCY == -8 | EQ_BDYARM_AGCY == -9)
replace EQ_BDYARM_CASH = . if (EQ_BDYARM_CASH == -88 | EQ_BDYARM_CASH == -8 | EQ_BDYARM_CASH == -9)
replace EQ_BDYARM_IND = . if (EQ_BDYARM_IND == -88 | EQ_BDYARM_IND == -8 | EQ_BDYARM_IND == -9)
replace EQ_BDYARM_NOAUTH = . if (EQ_BDYARM_NOAUTH == -8 | EQ_BDYARM_NOAUTH == -9)
replace EQ_UNI_AGCY = . if (EQ_UNI_AGCY == -88 | EQ_UNI_AGCY == -8 | EQ_UNI_AGCY == -9)
replace EQ_UNI_CASH = . if (EQ_UNI_CASH == -88 | EQ_UNI_CASH == -8 | EQ_UNI_CASH == -9)
replace EQ_UNI_IND = . if (EQ_UNI_IND == -88 | EQ_UNI_IND == -8 | EQ_UNI_IND == -9)
replace EQ_UNI_NOAUTH = . if (EQ_UNI_NOAUTH == -8 | EQ_UNI_NOAUTH == -9)
replace EQ_SEMI_ON_PRIM = . if (EQ_SEMI_ON_PRIM == -88 | EQ_SEMI_ON_PRIM == -8 | EQ_SEMI_ON_PRIM == -9)
replace EQ_SEMI_ON_BACK = . if (EQ_SEMI_ON_BACK == -88 | EQ_SEMI_ON_BACK == -8 | EQ_SEMI_ON_BACK == -9)
replace EQ_SEMI_OFF = . if (EQ_SEMI_OFF == -88 | EQ_SEMI_OFF == -8 | EQ_SEMI_OFF == -9)
replace EQ_SEMI_NOAUTH = . if (EQ_SEMI_NOAUTH == -8 | EQ_SEMI_NOAUTH == -9)
replace EQ_REV_ON_PRIM = . if (EQ_REV_ON_PRIM == -88 | EQ_REV_ON_PRIM == -8 | EQ_REV_ON_PRIM == -9)
replace EQ_REV_ON_BACK = . if (EQ_REV_ON_BACK == -88 | EQ_REV_ON_BACK == -8 | EQ_REV_ON_BACK == -9)
replace EQ_REV_OFF = . if (EQ_REV_OFF == -88 | EQ_REV_OFF == -8 | EQ_REV_OFF == -9)
replace EQ_REV_NOAUTH = . if (EQ_REV_NOAUTH == -8 | EQ_REV_NOAUTH == -9)
replace EQ_SEC_FULLAUTO = . if (EQ_SEC_FULLAUTO == -88 | EQ_SEC_FULLAUTO == -8 | EQ_SEC_FULLAUTO == -9)
replace EQ_SEC_SEMIAUTO = . if (EQ_SEC_SEMIAUTO == -88 | EQ_SEC_SEMIAUTO == -8 | EQ_SEC_SEMIAUTO == -9)
replace EQ_SEC_MANUAL = . if (EQ_SEC_MANUAL == -88 | EQ_SEC_MANUAL == -8 | EQ_SEC_MANUAL == -9)
replace EQ_SEC_SHOTGUN = . if (EQ_SEC_SHOTGUN == -88 | EQ_SEC_SHOTGUN == -8 | EQ_SEC_SHOTGUN == -9)
replace EQ_SEC_OTH = . if (EQ_SEC_OTH == -88 | EQ_SEC_OTH == -8 | EQ_SEC_OTH == -9)
replace EQ_SEC_OTH_FLAG = . if (EQ_SEC_OTH_FLAG == -88 | EQ_SEC_OTH_FLAG == -8 | EQ_SEC_OTH_FLAG == -9)
replace EQ_SEC_NOAUTH = . if (EQ_SEC_NOAUTH == -8 | EQ_SEC_NOAUTH == -9)
replace EQ_SEC_HANDGUN = . if (EQ_SEC_HANDGUN == -88 | EQ_SEC_HANDGUN == -8 | EQ_SEC_HANDGUN == -9)
replace EQ_SEC_LTHLETH = . if (EQ_SEC_LTHLETH == -88 | EQ_SEC_LTHLETH == -8 | EQ_SEC_LTHLETH == -9)
replace EQ_AUTH_OHAND = . if (EQ_AUTH_OHAND == -88 | EQ_AUTH_OHAND == -8 | EQ_AUTH_OHAND == -9)
replace EQ_AUTH_CHAND = . if (EQ_AUTH_CHAND == -88 | EQ_AUTH_CHAND == -8 | EQ_AUTH_CHAND == -9)
replace EQ_AUTH_TKDWN = . if (EQ_AUTH_TKDWN == -88 | EQ_AUTH_TKDWN == -8 | EQ_AUTH_TKDWN == -9)
replace EQ_AUTH_NECK = . if (EQ_AUTH_NECK == -88 | EQ_AUTH_NECK == -8 | EQ_AUTH_NECK == -9)
replace EQ_AUTH_LEG = . if (EQ_AUTH_LEG == -88 | EQ_AUTH_LEG == -8 | EQ_AUTH_LEG == -9)
replace EQ_AUTH_OC = . if (EQ_AUTH_OC == -88 | EQ_AUTH_OC == -8 | EQ_AUTH_OC == -9)
replace EQ_AUTH_CHEM = . if (EQ_AUTH_CHEM == -88 | EQ_AUTH_CHEM == -8 | EQ_AUTH_CHEM == -9)
replace EQ_AUTH_BTN = . if (EQ_AUTH_BTN == -88 | EQ_AUTH_BTN == -8 | EQ_AUTH_BTN == -9)
replace EQ_AUTH_BLNT = . if (EQ_AUTH_BLNT == -88 | EQ_AUTH_BLNT == -8 | EQ_AUTH_BLNT == -9)
replace EQ_AUTH_CED = . if (EQ_AUTH_CED == -88 | EQ_AUTH_CED == -8 | EQ_AUTH_CED == -9)
replace EQ_AUTH_EXP = . if (EQ_AUTH_EXP == -88 | EQ_AUTH_EXP == -8 | EQ_AUTH_EXP == -9)
replace EQ_AUTH_OTH = . if (EQ_AUTH_OTH == -88 | EQ_AUTH_OTH == -8 | EQ_AUTH_OTH == -9)
replace EQ_AUTH_OTH_FLAG = . if (EQ_AUTH_OTH_FLAG == -88 | EQ_AUTH_OTH_FLAG == -8 | EQ_AUTH_OTH_FLAG == -9)
replace EQ_AUTH_DIV = . if (EQ_AUTH_DIV == -88 | EQ_AUTH_DIV == -8 | EQ_AUTH_DIV == -9)
replace EQ_AUTH_K9 = . if (EQ_AUTH_K9 == -88 | EQ_AUTH_K9 == -8 | EQ_AUTH_K9 == -9)
replace EQ_AUTH_FIREARM = . if (EQ_AUTH_FIREARM == -88 | EQ_AUTH_FIREARM == -8 | EQ_AUTH_FIREARM == -9)
replace EQ_DOC_OHAND = . if (EQ_DOC_OHAND == -88 | EQ_DOC_OHAND == -8 | EQ_DOC_OHAND == -9)
replace EQ_DOC_CHAND = . if (EQ_DOC_CHAND == -88 | EQ_DOC_CHAND == -8 | EQ_DOC_CHAND == -9)
replace EQ_DOC_TKDWN = . if (EQ_DOC_TKDWN == -88 | EQ_DOC_TKDWN == -8 | EQ_DOC_TKDWN == -9)
replace EQ_DOC_NECK = . if (EQ_DOC_NECK == -88 | EQ_DOC_NECK == -8 | EQ_DOC_NECK == -9)
replace EQ_DOC_LEG = . if (EQ_DOC_LEG == -88 | EQ_DOC_LEG == -8 | EQ_DOC_LEG == -9)
replace EQ_DOC_OC = . if (EQ_DOC_OC == -88 | EQ_DOC_OC == -8 | EQ_DOC_OC == -9)
replace EQ_DOC_CHEM = . if (EQ_DOC_CHEM == -88 | EQ_DOC_CHEM == -8 | EQ_DOC_CHEM == -9)
replace EQ_DOC_BTN = . if (EQ_DOC_BTN == -88 | EQ_DOC_BTN == -8 | EQ_DOC_BTN == -9)
replace EQ_DOC_BLNT = . if (EQ_DOC_BLNT == -88 | EQ_DOC_BLNT == -8 | EQ_DOC_BLNT == -9)
replace EQ_DOC_DIS_CED = . if (EQ_DOC_DIS_CED == -88 | EQ_DOC_DIS_CED == -8 | EQ_DOC_DIS_CED == -9)
replace EQ_DOC_USE_CED = . if (EQ_DOC_USE_CED == -88 | EQ_DOC_USE_CED == -8 | EQ_DOC_USE_CED == -9)
replace EQ_DOC_EXP = . if (EQ_DOC_EXP == -88 | EQ_DOC_EXP == -8 | EQ_DOC_EXP == -9)
replace EQ_DOC_DIS_GUN = . if (EQ_DOC_DIS_GUN == -88 | EQ_DOC_DIS_GUN == -8 | EQ_DOC_DIS_GUN == -9)
replace EQ_DOC_DCHG_GUN = . if (EQ_DOC_DCHG_GUN == -88 | EQ_DOC_DCHG_GUN == -8 | EQ_DOC_DCHG_GUN == -9)
replace EQ_DOC_OTH = . if (EQ_DOC_OTH == -88 | EQ_DOC_OTH == -8 | EQ_DOC_OTH == -9)
replace EQ_DOC_OTH_FLAG = . if (EQ_DOC_OTH_FLAG == -88 | EQ_DOC_OTH_FLAG == -8 | EQ_DOC_OTH_FLAG == -9)
replace EQ_DOC_ANYUOF = . if (EQ_DOC_ANYUOF == -88 | EQ_DOC_ANYUOF == -8 | EQ_DOC_ANYUOF == -9)
replace EQ_DOC_K9 = . if (EQ_DOC_K9 == -88 | EQ_DOC_K9 == -8 | EQ_DOC_K9 == -9)
replace EQ_DOC_INJ = . if (EQ_DOC_INJ == -88 | EQ_DOC_INJ == -8 | EQ_DOC_INJ == -9)
replace EQ_DOC_DIV = . if (EQ_DOC_DIV == -88 | EQ_DOC_DIV == -8 | EQ_DOC_DIV == -9)
replace EQ_BDYARM = . if (EQ_BDYARM == -8 | EQ_BDYARM == -9)
replace EQ_SEATBELT = . if (EQ_SEATBELT == -8 | EQ_SEATBELT == -9)
replace EQ_VEH_MRK = . if (EQ_VEH_MRK == -9)
replace EQ_VEH_OTHMRK = . if (EQ_VEH_OTHMRK == -9)
replace EQ_VEH_UNMRK = . if (EQ_VEH_UNMRK == -9)
replace EQ_VEH_OTHUNMRK = . if (EQ_VEH_OTHUNMRK == -9)
replace EQ_VEH_ARMOR = . if (EQ_VEH_ARMOR == -9)
replace EQ_VEH_ATV = . if (EQ_VEH_ATV == -9)
replace EQ_VEH_MTRCYCL = . if (EQ_VEH_MTRCYCL == -9)
replace EQ_VEH_BOAT = . if (EQ_VEH_BOAT == -9)
replace EQ_VEH_MANAIR = . if (EQ_VEH_MANAIR == -9)
replace EQ_VEH_DRONE = . if (EQ_VEH_DRONE == -9)
replace EQ_VEH_OTH = . if (EQ_VEH_OTH == -8 | EQ_VEH_OTH == -9)
replace EQ_VEH_OTH_FLAG = . if (EQ_VEH_OTH_FLAG == -8 | EQ_VEH_OTH_FLAG == -9)
replace EQ_VEH_OHV = . if (EQ_VEH_OHV == -9)
replace EQ_VEH_SNOWMOB = . if (EQ_VEH_SNOWMOB == -9)
replace EQ_VEH_BIKE = . if (EQ_VEH_BIKE == -9)
replace EQ_VEH_HUMVEE = . if (EQ_VEH_HUMVEE == -9)
replace EQ_VEH_SEGWAY = . if (EQ_VEH_SEGWAY == -9)
replace EQ_VID_FIXED = . if (EQ_VID_FIXED == -9)
replace EQ_VID_MOBILE = . if (EQ_VID_MOBILE == -9)
replace EQ_VID_CAR = . if (EQ_VID_CAR == -9)
replace EQ_VID_BWC = . if (EQ_VID_BWC == -9)
replace EQ_VID_WEAP = . if (EQ_VID_WEAP == -9)
replace EQ_VID_DRONE = . if (EQ_VID_DRONE == -9)
replace TECH_WEB_NONE = . if (TECH_WEB_NONE == -9)
replace TECH_WEB_STAT = . if (TECH_WEB_STAT == -8 | TECH_WEB_STAT == -9)
replace TECH_WEB_STOP = . if (TECH_WEB_STOP == -8 | TECH_WEB_STOP == -9)
replace TECH_WEB_ARR = . if (TECH_WEB_ARR == -8 | TECH_WEB_ARR == -9)
replace TECH_WEB_REPORT = . if (TECH_WEB_REPORT == -8 | TECH_WEB_REPORT == -9)
replace TECH_WEB_ASK = . if (TECH_WEB_ASK == -8 | TECH_WEB_ASK == -9)
replace TECH_WEB_COMPL = . if (TECH_WEB_COMPL == -8 | TECH_WEB_COMPL == -9)
replace TECH_SM_TWITTER = . if (TECH_SM_TWITTER == -9)
replace TECH_SM_FB = . if (TECH_SM_FB == -9)
replace TECH_SM_BLOG = . if (TECH_SM_BLOG == -9)
replace TECH_SM_YOUTUBE = . if (TECH_SM_YOUTUBE == -9)
replace TECH_SM_MASSNOT = . if (TECH_SM_MASSNOT == -9)
replace TECH_COMP_CRMANL = . if (TECH_COMP_CRMANL == -9)
replace TECH_COMP_SNA = . if (TECH_COMP_SNA == -9)
replace TECH_COMP_INTEL = . if (TECH_COMP_INTEL == -9)
replace TECH_COMP_INFSHR = . if (TECH_COMP_INFSHR == -9)
replace TECH_COMP_BOOK = . if (TECH_COMP_BOOK == -9)
replace TECH_CIR = . if (TECH_CIR == -8 | TECH_CIR == -9)
replace TECH_TYP_AFIS = . if (TECH_TYP_AFIS == -9)
replace TECH_TYP_FACEREC = . if (TECH_TYP_FACEREC == -9)
replace TECH_TYP_LPR = . if (TECH_TYP_LPR == -9)
replace TECH_TYP_INFR = . if (TECH_TYP_INFR == -9)
replace TECH_TYP_ENGD = . if (TECH_TYP_ENGD == -9)
replace TECH_TYP_VTRC = . if (TECH_TYP_VTRC == -9)
replace TECH_TYP_TIREDFL = . if (TECH_TYP_TIREDFL == -9)
replace TECH_TYP_GUNSHOT = . if (TECH_TYP_GUNSHOT == -9)
replace TECH_TYP_TRACE = . if (TECH_TYP_TRACE == -9)
replace TECH_TYP_BALL = . if (TECH_TYP_BALL == -9)
replace TECH_TYP_GPS = . if (TECH_TYP_GPS == -9)
replace TECH_NO_IFC = . if (TECH_NO_IFC == -9)
replace TECH_IFC_MVREC = . if (TECH_IFC_MVREC == -88 | TECH_IFC_MVREC == -8 | TECH_IFC_MVREC == -9)
replace TECH_IFC_DRVREC = . if (TECH_IFC_DRVREC == -88 | TECH_IFC_DRVREC == -8 | TECH_IFC_DRVREC == -9)
replace TECH_IFC_CRIMREC = . if (TECH_IFC_CRIMREC == -88 | TECH_IFC_CRIMREC == -8 | TECH_IFC_CRIMREC == -9)
replace TECH_IFC_WARR = . if (TECH_IFC_WARR == -88 | TECH_IFC_WARR == -8 | TECH_IFC_WARR == -9)
replace TECH_IFC_PRTORD = . if (TECH_IFC_PRTORD == -88 | TECH_IFC_PRTORD == -8 | TECH_IFC_PRTORD == -9)
replace TECH_IFC_INFSHR = . if (TECH_IFC_INFSHR == -88 | TECH_IFC_INFSHR == -8 | TECH_IFC_INFSHR == -9)
replace TECH_IFC_ADDHIS = . if (TECH_IFC_ADDHIS == -88 | TECH_IFC_ADDHIS == -8 | TECH_IFC_ADDHIS == -9)
replace TECH_IFC_GISMAP = . if (TECH_IFC_GISMAP == -88 | TECH_IFC_GISMAP == -8 | TECH_IFC_GISMAP == -9)
replace TECH_EIS = . if (TECH_EIS == -9)
replace TECH_FILE_ARR = . if (TECH_FILE_ARR == -9)
replace TECH_FILE_CFS = . if (TECH_FILE_CFS == -9)
replace TECH_FILE_COMPL = . if (TECH_FILE_COMPL == -9)
replace TECH_FILE_CRS = . if (TECH_FILE_CRS == -9)
replace TECH_FILE_GUNS = . if (TECH_FILE_GUNS == -9)
replace TECH_FILE_GANG = . if (TECH_FILE_GANG == -9)
replace TECH_FILE_INFORM = . if (TECH_FILE_INFORM == -9)
replace TECH_ILES_INTEL = . if (TECH_ILES_INTEL == -9)
replace TECH_FILE_MVSTOP = . if (TECH_FILE_MVSTOP == -9)
replace TECH_FILE_MVACC = . if (TECH_FILE_MVACC == -9)
replace TECH_FILE_PAWN = . if (TECH_FILE_PAWN == -9)
replace TECH_FILE_PRTORD = . if (TECH_FILE_PRTORD == -9)
replace TECH_FILE_PROP = . if (TECH_FILE_PROP == -9)
replace TECH_FILE_STOPS = . if (TECH_FILE_STOPS == -9)
replace TECH_FILE_UOF = . if (TECH_FILE_UOF == -9)
replace TECH_FILE_VIDEO = . if (TECH_FILE_VIDEO == -9)
replace TECH_FILE_WARR = . if (TECH_FILE_WARR == -9)
replace POL_VEHPURS = . if (POL_VEHPURS == -8 | POL_VEHPURS == -9)
replace POL_DEADFORC = . if (POL_DEADFORC == -9)
replace POL_LESSLETHAL = . if (POL_LESSLETHAL == -9)
replace POL_CONDUCT = . if (POL_CONDUCT == -9)
replace POL_MAXHRS = . if (POL_MAXHRS == -9)
replace POL_OFFDTY = . if (POL_OFFDTY == -9)
replace POL_MENTILL = . if (POL_MENTILL == -9)
replace POL_HOMELESS = . if (POL_HOMELESS == -9)
replace POL_DOMDISP = . if (POL_DOMDISP == -9)
replace POL_JUV = . if (POL_JUV == -9)
replace POL_INCUSDTH = . if (POL_INCUSDTH == -9)
replace POL_RACPROF = . if (POL_RACPROF == -9)
replace POL_COMPL = . if (POL_COMPL == -9)
replace POL_STRPSRCH = . if (POL_STRPSRCH == -9)
replace POL_TERROR = . if (POL_TERROR == -9)
replace POL_ACTSHOOT = . if (POL_ACTSHOOT == -9)
replace POL_STFRSK = . if (POL_STFRSK == -9)
replace POL_FOOT = . if (POL_FOOT == -9)
replace POL_MVSTOP = . if (POL_MVSTOP == -9)
replace POL_MSCOND = . if (POL_MSCOND == -9)
replace POL_PRISTRP = . if (POL_PRISTRP == -9)
replace POL_MASSDEM = . if (POL_MASSDEM == -9)
replace POL_REPUOF = . if (POL_REPUOF == -9)
replace POL_BWC = . if (POL_BWC == -9)
replace POL_SOCMED = . if (POL_SOCMED == -9)
replace POL_CULTAW = . if (POL_CULTAW == -9)
replace POL_INV_INJRY = . if (POL_INV_INJRY == -9)
replace POL_INV_DTH = . if (POL_INV_DTH == -9)
replace POL_INV_ICD = . if (POL_INV_ICD == -9)
replace POL_INV_DCHG_GUN = . if (POL_INV_DCHG_GUN == -9)
replace POL_CCRB = . if (POL_CCRB == -9)
replace POL_CCRB_SUBPWR = . if (POL_CCRB_SUBPWR == -8 | POL_CCRB_SUBPWR == -9)
replace POL_COMP_EXTINV = . if (POL_COMP_EXTINV == -8 | POL_COMP_EXTINV == -9)
replace ISSU_ADDR_BIAS = . if (ISSU_ADDR_BIAS == -9)
replace ISSU_ADDR_BOMB = . if (ISSU_ADDR_BOMB == -9)
replace ISSU_ADDR_CHILD = . if (ISSU_ADDR_CHILD == -9)
replace ISSU_ADDR_CRMPREV = . if (ISSU_ADDR_CRMPREV == -9)
replace ISSU_ADDR_CP = . if (ISSU_ADDR_CP == -9)
replace ISSU_ADDR_CRMANL = . if (ISSU_ADDR_CRMANL == -9)
replace ISSU_ADDR_CYBER = . if (ISSU_ADDR_CYBER == -9)
replace ISSU_ADDR_DOM = . if (ISSU_ADDR_DOM == -9)
replace ISSU_ADDR_DRUG_ED = . if (ISSU_ADDR_DRUG_ED == -9)
replace ISSU_ADDR_DRUG_ENF = . if (ISSU_ADDR_DRUG_ENF == -9)
replace ISSU_ADDR_ENV = . if (ISSU_ADDR_ENV == -9)
replace ISSU_ADDR_FIN = . if (ISSU_ADDR_FIN == -9)
replace ISSU_ADDR_GUNS = . if (ISSU_ADDR_GUNS == -9)
replace ISSU_ADDR_GANG = . if (ISSU_ADDR_GANG == -9)
replace ISSU_ADDR_HUMTRF = . if (ISSU_ADDR_HUMTRF == -9)
replace ISSU_ADDR_DUI = . if (ISSU_ADDR_DUI == -9)
replace ISSU_ADDR_IA = . if (ISSU_ADDR_IA == -9)
replace ISSU_ADDR_JUV = . if (ISSU_ADDR_JUV == -9)
replace ISSU_ADDR_MISCHD = . if (ISSU_ADDR_MISCHD == -9)
replace ISSU_ADDR_REPOFF = . if (ISSU_ADDR_REPOFF == -9)
replace ISSU_ADDR_RESRCH = . if (ISSU_ADDR_RESRCH == -9)
replace ISSU_ADDR_SCH = . if (ISSU_ADDR_SCH == -9)
replace ISSU_ADDR_SWAT = . if (ISSU_ADDR_SWAT == -9)
replace ISSU_ADDR_TERROR = . if (ISSU_ADDR_TERROR == -9)
replace ISSU_ADDR_VIC = . if (ISSU_ADDR_VIC == -9)
replace NEW_TOT_HIRES = . if (NEW_TOT_HIRES == -9)
replace NEW_TOT_SEP = . if (NEW_TOT_SEP == -9)

