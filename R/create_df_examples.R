

#' Create example datasets
#'
#' It create mock datasets that can be used to test other functions of this package.
#'
#' @param datasets one of the following: "all", "df_example", "df_example_targeted", "df_example_targeted_legend", "df_example_targeted_compounds_legend", "grad_example", "features_data_example", "featinfo_example", "melecule_to_search_example". If "all", all of the following dataframes (tibble) will be created as different objects; df_example, generic dataframe useful for testing most statistical functions; df_example_targeted, example for the first argument of get_targeted_elaboration; df_example_targeted_legend, example for the second argument of get_targeted_elaboration; df_example_targeted_compounds_legend, example for the third argument of get_targeted_elaboration; grad_example, example for get_phase_amount; features_data_example, example of a feature data table; featinfo_example, example of a featINFO table; melecule_to_search_example, example of known standards to pass to the third argument of checkmolecules_in_feat_table.   
#'
#' @return it directly creates the object(s) in the current environment.
#'
#' @export
create_df_examples <- function(datasets = c("all", "df_example", "df_example_targeted", "df_example_targeted_legend", "df_example_targeted_compounds_legend", "grad_example", "features_data_example", "featinfo_example", "melecule_to_search_example")) {
  
  if (!identical(tolower(datasets), c("all", "df_example", "df_example_targeted", "df_example_targeted_legend", "df_example_targeted_compounds_legend", "grad_example", "features_data_example", "featinfo_example", "melecule_to_search_example"))) {
    if (length(datasets) != 1) {stop('datasets must be one of "all", "df_example", "df_example_targeted", "df_example_targeted_legend", "df_example_targeted_compounds_legend", "grad_example", "features_data_example", "featinfo_example", "melecule_to_search_example"')}
    if (is.na(datasets)) {stop('datasets must be one of "all", "df_example", "df_example_targeted", "df_example_targeted_legend", "df_example_targeted_compounds_legend", "grad_example", "features_data_example", "featinfo_example", "melecule_to_search_example"')}
  }
  datasets <- tolower(datasets)
  datasets <- match.arg(datasets, c("all", "df_example", "df_example_targeted", "df_example_targeted_legend", "df_example_targeted_compounds_legend", "grad_example", "features_data_example", "featinfo_example", "melecule_to_search_example"))
  
  
  
  if (datasets == "all" | datasets == "df_example") {
    if ("df_example" %in% ls(envir=.GlobalEnv)) {warning("An object with the same name was already present in the Global Environment and it was replaced for  df_example")}
    
    assign("df_example",
           
           tibble(observation = c("obs01", "obs02", "obs03", "obs04", "obs05", "obs06", "obs07", "obs08", "obs09", "obs10", "obs11", "obs12", "obs13", "obs14", "obs15", "obs16"),
                  factvar2 = factor(c("B2", "B2", "B2", "B2", "B2", "B2", "B2", "A2", "A2", "A2", "A2", "A2", "A2", "A2", "A2", "A2"), levels = c("A2", "B2")),
                  factvar3 = factor(c("A3", "A3", "A3", "A3", "A3", "A3", "A3", "B3", "B3", "B3", "B3", "B3", "B3", "C3", "C3", "C3"), levels = c("A3", "B3", "C3")),
                  factvar4 = factor(c("B4", "B4", "A4", "A4", "A4", "B4", "A4", "C4", "A4", "B4", "C4", "D4", "D4", "C4", "A4", "D4"), levels = c("A4", "B4", "C4", "D4")),
                  factvar5 = factor(c("C5", "D5", "D5", "A5", "B5", "A5", "C5", "B5", "D5", "D5", "C5", "E5", "E5", "E5", "C5", "B5"), levels = c("A5", "B5", "C5", "D5", "E5")),
                  factvar6 = factor(c("C6", "D6", "D6", "A6", "B6", "A6", "C6", "B6", "F6", "F6", "C6", "E6", "E6", "E6", "C6", "B6"), levels = c("A6", "B6", "C6", "D6", "E6")),
                  numvar1 = c(47.8, 32.2, 31.2, 51.9, 50, 51, 50.1, 0.002, 0.006, 0.003, 0.005, 0.008, 0.004, 100000, 100005, 100006),
                  numvar2 = c(467, 454, 564, 454, 500, 501, 480, 500, 450, 567, 419, 523, 456, 501, 499, 458),
                  numvar3 = c(15, 9.8, 14.6, 10, 12.5, 11.4, 10.9, 9, 9.9, 8.6, 9.5, 8.7, 9.2, 10.6, 11.5, 13.9),
                  numvar4 = c(1, 1.5, 1.9, 10, 12, 1, 2, 1.1, 1, 1.6,  1,  9, 1, 1, 1, 1),
                  numvar4_withNA = c(1, 1.5, 1.9, NA, 12, 1, NA, 1.1, 1, 1.6, NA, 9, 1, 1, 1, 1),
                  var_onlyNA = as.numeric(c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)),
                  numvar5 = c(30, 33, 19, 43, 51, 42, 53, 36, 39, 49, 25, 26, 35, 46, 48, 30),
                  numvar6 = c(19, 19.1, 22, 26, 35, 28, 29, 24, 20, 18.6, 31, 34, 28.5, 24.3, 20.6, 23.5)),
           
           envir=.GlobalEnv)
    
    cat("\n In the Global Environment, created the object  df_example")
  }
  
  
  
  if (datasets == "all" | datasets == "df_example_targeted") {
    if ("df_example_targeted" %in% ls(envir=.GlobalEnv)) {warning("An object with the same name was already present in the Global Environment and it was replaced for  df_example_targeted")}
    
    assign("df_example_targeted",
           
           tibble(samples = c("blank", "blank", "blank", "calibration_curve_level00", "calibration_curve_level01", "calibration_curve_level02", "calibration_curve_level03", "calibration_curve_level04", "calibration_curve_level05", "calibration_curve_level06", "blank", "quality_control_low", "quality_control_medium", "quality_control_high", "blank", "unkwon_sample_001", "unkwon_sample_002", "unkwon_sample_003", "unkwon_sample_004", "unkwon_sample_005", "unkwon_sample_006", "unkwon_sample_007", "unkwon_sample_008", "unkwon_sample_009", "unkwon_sample_010", "unkwon_sample_011", "unkwon_sample_012", "unkwon_sample_013", "unkwon_sample_014", "unkwon_sample_015", "unkwon_sample_016", "unkwon_sample_017", "unkwon_sample_018", "unkwon_sample_019", "unkwon_sample_020", "unkwon_sample_021", "unkwon_sample_022",
                              "unkwon_sample_024", "unkwon_sample_025", "unkwon_sample_026", "unkwon_sample_027", "unkwon_sample_028", "unkwon_sample_029", "quality_control_low", "quality_control_medium", "quality_control_high", "unkwon_sample_030", "unkwon_sample_031", "unkwon_sample_032", "unkwon_sample_033", "unkwon_sample_034", "unkwon_sample_035", "unkwon_sample_036", "unkwon_sample_037", "unkwon_sample_038", "unkwon_sample_039", "unkwon_sample_040", "unkwon_sample_041", "unkwon_sample_042", "unkwon_sample_043", "unkwon_sample_044", "unkwon_sample_045", "unkwon_sample_046", "unkwon_sample_047", "unkwon_sample_048", "unkwon_sample_049", "unkwon_sample_050", "unkwon_sample_051", "unkwon_sample_052", "unkwon_sample_053", "quality_control_low", "quality_control_medium", "quality_control_high",
                              "blank", "calibration_curve_level00", "calibration_curve_level01", "calibration_curve_level02", "calibration_curve_level03", "calibration_curve_level04", "calibration_curve_level05", "calibration_curve_level06", "blank", "blank"),
                  molecule01 = c(0, 12, 45, 862, 916839, 2437097, 8664951, 10163671, 13164865, 16546462, 35, 2202987, 8692745, 13350486, 411, 6574368, 4460080, 3239063, 6608225, 2978163, 4769785, 1725251, 1643000, 1915443, 4028474, 8463740, 1737906, 2699846, 12288335, 3432108, 8666374, 1845916, 3475164, 2114796, 171877, 6247263, 3599212, 4894538, 3143636, 3001082, 477957, 3759349, 1880801, 2142669, 7034753, 11074961, 5894817, 1195205, 4053103, 5684014, 1442297, 5913247, 136427, 3256778, 1886390, 2259891, 1617417, 2492169, 3098920, 2014687, 2158903, 3236979, 5043779, 2281076, 3243707, 5518835, 2261516, 8773341, 1877135, 4524584, 2462859, 9862304, 10677785, 232, 7941, 793876, 2810201, 7690566, 9986602, 11788006, 16681300, 32, 234),
                  molecule01_IS = c(85009209.92, 85058548.93, 85544475.02, 84525131.7, 83570564.08, 84157195.23, 84662093.79, 85495504.12, 84600992.31, 85375589.88, 83709521.14, 85109664.21, 86331029.42, 85055146.13, 85131861.1, 84576247.55, 84655306.81, 83715299.4, 84558580.99, 85208916.29, 83750000.01, 83692850.78, 84886850.04, 84969855.99, 83921890.08, 84778700.03, 85136301.72, 85098766.57, 84183313.96, 83970979.95, 84094517, 84613546.76, 84389958.34, 83628774.69, 83227983.02, 85079470.89, 85487798.87, 84294280.65, 83585221.44, 84396505.88, 83939533.96, 84287560.52, 84480473.49, 84023956.88, 84392981.05, 84822711.03, 84865119.61, 84329670.17, 85106202.41, 83632088.06, 84392641.43, 83986705.42, 85694555.06,
                                    84807924, 84500530.54, 83411412.01, 84095142.53, 84569979.99, 83619031.17, 84795788.57, 84100533.12, 84912246.86, 84091619.84, 84889050.02, 83925483.38, 84534019.28, 84679996.4, 84082230.22, 83976479.25, 83567066.5, 84755838.7, 84486996.38, 84338270.57, 85171442.38, 84652209.65, 84590061.31, 84120338.75, 84711754.48, 84318237.92, 85032131.25, 84286658.6, 85290695.14, 84299672.46),
                  molecule02 = c(0, 12, 45, 64851, 69002371, 122279121, 217377977, 254976452, 402513548, 622652692, 35, 99479574, 228979013, 334924209, 411, 49479432, 33567066, 24377551, 49734247, 22413987, 35897940, 12984431, 12365404, 14415842, 30318745, 63699059, 13079678, 20319346, 92483391, 25830433, 65224104, 13892573, 26154477, 15916194, 1293563, 47017606, 27088074, 36836846, 23659359, 22586479, 3597162, 28293287, 14155122, 96755794, 185305208, 277838018, 44365053, 8995249, 30504113, 42778531, 10854886, 44503761, 1026767, 24510875, 14197182, 17008194, 12172861, 18756343, 23322819, 15162758, 16248150, 24361868, 37960051, 17167638, 24412504, 41535375, 17020424, 66029150, 14127533, 34052528, 111214526, 259786848, 267874036, 37560681, 59761, 59747997, 140999273, 192933548, 250534295, 360416318, 627726724, 53, 43),
                  molecule02_IS = c(512295.1211, 513397.7864, 512770.8402, 512250.3683, 513334.2721, 510583.1825, 513191.0512, 514244.7301, 512262.1409, 513297.2884, 513975.6689, 511799.5608, 514406.9595, 512997.402, 513431.8397, 511966.3603, 512366.2289, 514775.6802, 512385.0415, 512415.1878, 513122.4041, 509823.1668, 512930.811, 511789.2665, 512110.0745, 513305.3034, 514771.5609, 513356.9638, 510865.0064, 514096.0755, 512100.2755, 512395.7005, 511849.2348, 512050.2338, 514525.563, 513843.9998, 512591.6465, 511874.0499, 512415.7242, 511647.4954, 511928.3787, 513138.189, 510916.5201, 511557.5737, 512141.4671, 512590.7958, 513251.0032, 511775.3543, 513342.3401, 512882.4249, 511097.2601, 512298.3584, 509755.0589, 511405.4666, 513714.9322, 513114.4857, 512042.3189, 512988.3116, 514329.533, 512218.6786,
                                    512350.762, 513878.7595, 514106.6813, 512297.0154, 512474.8418, 512910.2278, 515342.903, 514785.3904, 510359.3703, 512993.6852, 512383.1253, 511812.0564, 514075.4563, 513558.6982, 513301.0504, 513262.0036, 511362.9277, 512155.6789, 513026.5256, 511331.1577, 511811.5578, 511741.3483, 510962.1805),
                  molecule03 = c(2, 474, 2, 184514, 3926523, 13916386, 51540480, 90682659, 132142516, 177157699, 845, 9434672, 65149313, 114351521, 784, 140779321, 95505314, 69359225, 141504322, 63772475, 102137140, 36943418, 35182159, 41016081, 86263164, 181237129, 37214418, 57812784, 263134569, 73492978, 185576203, 39527273, 74414950, 45284896, 3680458, 133774910, 77071230, 104808521, 67315820, 64263251, 10234676, 80500312, 40274279, 9176348, 52723203, 94860865, 126227846, 25593362, 86790574, 121713857, 30884419, 126622499, 2921367, 69738559, 40393948, 48391864, 34634334, 53365714, 66358293, 43141215, 46229381, 69314604, 108004274, 48845517, 69458675, 118176819, 48426662, 187866725, 40195782, 96886557, 10547619, 73914785, 91458911, 315561, 170033, 3399911, 16046896, 45744688, 89102801, 118322275, 178601367, 8585, 7),
                  molecule04 = c(2, 32, 2, 545, 57971, 154097, 547882, 642646, 832410, 1046228, 5, 139294, 549640, 844147, 411, 415695, 282009, 204805, 417836, 188308, 301592, 109087, 103886, 121113, 254719, 535160, 109887, 170710, 776988, 217011, 547972, 116717, 219734, 133718, 10868, 395012, 227577, 309480, 198771, 189757, 30221, 237702, 118922, 135480, 444805, 700266, 372727, 75572, 256276, 359398, 91196, 373893, 8626, 205925, 119276, 142892, 102269, 157579, 195944, 127388, 136507, 204673, 318917, 144232, 205099, 348954, 142995, 554736, 118691, 286088, 155726, 623590, 675153, 315561, 502, 50197, 177688, 486272, 631450, 745352, 1054753, 53, 43)),
           
           envir=.GlobalEnv)
    
    cat("\n In the Global Environment, created the object  df_example_targeted")
  }
  
  
  
  if (datasets == "all" | datasets == "df_example_targeted_legend") {
    if ("df_example_targeted_legend" %in% ls(envir=.GlobalEnv)) {warning("An object with the same name was already present in the Global Environment and it was replaced for  df_example_targeted_legend")}
    
    assign("df_example_targeted_legend",
           
           df_example_targeted_legend <- tibble(sample_name = c("blank", "blank", "blank", "calibration_curve_level00", "calibration_curve_level01", "calibration_curve_level02", "calibration_curve_level03", "calibration_curve_level04", "calibration_curve_level05", "calibration_curve_level06", "blank", "quality_control_low", "quality_control_medium", "quality_control_high", "blank", "unkwon_sample_001", "unkwon_sample_002", "unkwon_sample_003", "unkwon_sample_004", "unkwon_sample_005", "unkwon_sample_006", "unkwon_sample_007", "unkwon_sample_008", "unkwon_sample_009", "unkwon_sample_010", "unkwon_sample_011", "unkwon_sample_012", "unkwon_sample_013", "unkwon_sample_014", "unkwon_sample_015", "unkwon_sample_016", "unkwon_sample_017", "unkwon_sample_018", "unkwon_sample_019", "unkwon_sample_020", "unkwon_sample_021", "unkwon_sample_022",
                                                                "unkwon_sample_024", "unkwon_sample_025", "unkwon_sample_026", "unkwon_sample_027", "unkwon_sample_028", "unkwon_sample_029", "quality_control_low", "quality_control_medium", "quality_control_high", "unkwon_sample_030", "unkwon_sample_031", "unkwon_sample_032", "unkwon_sample_033", "unkwon_sample_034", "unkwon_sample_035", "unkwon_sample_036", "unkwon_sample_037", "unkwon_sample_038", "unkwon_sample_039", "unkwon_sample_040", "unkwon_sample_041", "unkwon_sample_042", "unkwon_sample_043", "unkwon_sample_044", "unkwon_sample_045", "unkwon_sample_046", "unkwon_sample_047", "unkwon_sample_048", "unkwon_sample_049", "unkwon_sample_050", "unkwon_sample_051", "unkwon_sample_052", "unkwon_sample_053", "quality_control_low", "quality_control_medium", "quality_control_high",
                                                                "blank", "calibration_curve_level00", "calibration_curve_level01", "calibration_curve_level02", "calibration_curve_level03", "calibration_curve_level04", "calibration_curve_level05", "calibration_curve_level06", "blank", "blank"),
                                                sample_type = c("blank", "blank", "blank", "curve", "curve", "curve", "curve", "curve", "curve", "curve", "blank", "qc", "qc", "qc", "blank", "unknown", "unknown", "unknown", "unknown", "unknown", "unknown", "unknown", "unknown", "unknown", "unknown", "unknown", "unknown", "unknown", "unknown", "unknown", "unknown", "unknown", "unknown", "unknown", "unknown", "unknown", "unknown", "unknown", "unknown", "unknown", "unknown", "unknown", "unknown", "qc", "qc", "qc", "unknown", "unknown", "unknown", "unknown", "unknown", "unknown", "unknown", "unknown", "unknown", "unknown", "unknown", "unknown", "unknown", "unknown", "unknown", "unknown", "unknown", "unknown", "unknown", "unknown", "unknown", "unknown", "unknown", "unknown", "qc", "qc", "qc", "blank", "curve", "curve", "curve", "curve", "curve", "curve", "curve", "blank", "blank"),
                                                molecule01 = c(NA, NA, NA, 0, 10, 30, 90, 120, 160, 200, NA, 25, 100, 150, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 25, 100, 150, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 25, 100, 150, NA, 0, 10, 30, 90, 120, 160, 200, NA, NA),
                                                molecule02 = c(NA, NA, NA, 0, 100, 200, 300, 400, 650, 1000, NA, 150, 350, 500, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 150, 350, 500, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 150, 350, 500, NA, 0, 100, 200, 300, 400, 650, 1000, NA, NA),
                                                molecule03 = c(NA, NA, NA, 0, 2, 8, 25, 50, 75, 100, NA, 5, 35, 60, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 5, 35, 60, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 5, 35, 60, NA, 0, 2, 8, 25, 50, 75, 100, NA, NA),
                                                molecule04 = c(NA, NA, NA, 0, 10, 30, 90, 120, 160, 200, NA, 25, 100, 150, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 25, 100, 150, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 25, 100, 150, NA, 0, 10, 30, 90, 120, 160, 200, NA, NA)),
           
           envir=.GlobalEnv)
    
    cat("\n In the Global Environment, created the object  df_example_targeted_legend")
  }
  
  
  
  if (datasets == "all" | datasets == "df_example_targeted_compounds_legend") {
    if ("df_example_targeted_compounds_legend" %in% ls(envir=.GlobalEnv)) {warning("An object with the same name was already present in the Global Environment and it was replaced for  df_example_targeted_compounds_legend")}
    
    assign("df_example_targeted_compounds_legend",
           
           tibble(compounds = c("molecule01", "molecule02", "molecule03", "molecule04"),
                  matched_IS = c("molecule01_IS", "molecule02_IS", NA, NA),
                  weighting = c("1/Y", "none", NA, "1/(Y^2)"),
                  unit = c("ng/mL", "ug/mL", "ng/mL", NA)),
           
           envir=.GlobalEnv)
    
    cat("\n In the Global Environment, created the object  df_example_targeted_compounds_legend")
  }
  
  
  
  if (datasets == "all" | datasets == "grad_example") {
    if ("grad_example" %in% ls(envir=.GlobalEnv)) {warning("An object with the same name was already present in the Global Environment and it was replaced for  grad_example")}
    
    assign("grad_example",
           
           tibble(
             minute = c(0, 0.5, 4, 7, 11, 16, 16.1, 22),
             flow = rep(0.3,8),
             acquoeos = c(80, 80, 60, 60, 0, 0, 80, 80),
             organic = c(20, 20, 40, 40, 100, 100, 20, 20)
           ),
           
           envir=.GlobalEnv)
    
    cat("\n In the Global Environment, created the object  grad_example")
  }
  
  
  
  if (datasets == "all" | datasets == "features_data_example") {
    if ("features_data_example" %in% ls(envir=.GlobalEnv)) {warning("An object with the same name was already present in the Global Environment and it was replaced for  features_data_example")}
    
    assign("features_data_example",
           
           tibble(featname = c("feat001", "feat002", "feat003", "feat004", "feat005", "feat006"),
                  sample1 = c(4684, 5823, 1822, 1515, 4135, 4875),
                  sample2 = c(2819, 5681, 1611, 3613, 2450, 3611),
                  sample3 = c(5275, 6209, 3495, 5617, 2911, 3043),
                  sample4 = c(4402, 5163, 4891, 4483, 1343, 6574)),
           
           envir=.GlobalEnv)
    
    cat("\n In the Global Environment, created the object  features_data_example")
  }
  
  
  
  if (datasets == "all" | datasets == "featinfo_example") {
    if ("featinfo_example" %in% ls(envir=.GlobalEnv)) {warning("An object with the same name was already present in the Global Environment and it was replaced for  featinfo_example")}
    
    assign("featinfo_example",
           
           tibble(featname = c("feat001", "feat002", "feat003", "feat004", "feat005", "feat006"),
                  rt = c(242, 288, 408, 528, 369, 476),
                  mz = c(120.0116, 146.0451, 203.0822, 148.0434, 164.0714, 164.0715)),
           
           envir=.GlobalEnv)
    
    cat("\n In the Global Environment, created the object  featinfo_example")
  }
  
  
  
  if (datasets == "all" | datasets == "melecule_to_search_example") {
    if ("melecule_to_search_example" %in% ls(envir=.GlobalEnv)) {warning("An object with the same name was already present in the Global Environment and it was replaced for  melecule_to_search_example")}
    
    assign("melecule_to_search_example",
           
           tibble(coumpound = c("Alanine", "Asparagine", "Cysteine", "Glutamate", "Glycine", "Leucine", "Methionine", "Phenylalanine"),
                  rt = c(146, 262, 243, 290, 169, 98, 258, 471),
                  mz = c(88.0399, 131.0457, 120.0119, 146.0453, 74.0242, 130.0868, 148.0432, 164.0712)),
           
           envir=.GlobalEnv)
    
    cat("\n In the Global Environment, created the object  melecule_to_search_example")
  }
  
  cat("\n")
  cat("\n")
}
