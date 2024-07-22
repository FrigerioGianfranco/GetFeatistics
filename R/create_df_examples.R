
#' Create example datasets
#'
#' It create mock datasets that can be used to test other functions of this package.
#'
#' @param datasets one of the following: "all", "df_example_generic", "df_example_targeted", "df_example_targeted_legend", "df_example_targeted_compounds_legend", "df_example_sample_data", "df_example_feat_intensities", "df_example_feat_info", "df_example_qc_sampletype", "df_example_melecules_to_search", "df_example_gradient".
#' \itemize{
#'  \item If "all", all of the following dataframes (tibble) will be created as different objects.
#'  \item df_example_generic: generic dataframe useful for testing some statistical functions.
#'  \item df_example_targeted: example for the first argument of get_targeted_elaboration.
#'  \item df_example_targeted_legend: example for the second argument of get_targeted_elaboration.
#'  \item df_example_targeted_compounds_legend: example for the third argument of get_targeted_elaboration.
#'  \item df_example_sample_data: example of the metadata (additional variables besides molecular data, in real life they could be age, sex, BMI) that can be used to test statistical functions.
#'  \item df_example_feat_intensities: example of featTable, with feature intensities.
#'  \item df_example_feat_info: example of featINFO, with retention times and m/z.
#'  \item df_example_qc_sampletype: example of a table to pass in the sampletype argument of the QCs_process function.
#'  \item df_example_melecules_to_search; example of known standards to pass to the third argument of checkmolecules_in_feat_table.
#'  \item df_example_gradient: example for get_phase_amount; features_data_example, example of a feature data table.
#' }
#'
#' @return it directly creates the object(s) in the current environment.
#'
#' @export
create_df_examples <- function(datasets = c("all", "df_example_generic", "df_example_targeted", "df_example_targeted_legend", "df_example_targeted_compounds_legend", "df_example_sample_data", "df_example_feat_intensities", "df_example_feat_info", "df_example_qc_sampletype", "df_example_melecules_to_search", "df_example_gradient")) {
  
  if (!identical(tolower(datasets), c("all", "df_example_generic", "df_example_targeted", "df_example_targeted_legend", "df_example_targeted_compounds_legend", "df_example_sample_data", "df_example_feat_intensities", "df_example_feat_info", "df_example_qc_sampletype", "df_example_melecules_to_search", "df_example_gradient"))) {
    if (length(datasets) != 1) {stop('datasets must be one of "all", "df_example_generic", "df_example_targeted", "df_example_targeted_legend", "df_example_targeted_compounds_legend", "df_example_sample_data", "df_example_feat_intensities", "df_example_feat_info", "df_example_qc_sampletype", "df_example_melecules_to_search", "df_example_gradient"')}
    if (is.na(datasets)) {stop('datasets must be one of "all", "df_example_generic", "df_example_targeted", "df_example_targeted_legend", "df_example_targeted_compounds_legend", "df_example_sample_data", "df_example_feat_intensities", "df_example_feat_info", "df_example_qc_sampletype", "df_example_melecules_to_search", "df_example_gradient"')}
  }
  datasets <- tolower(datasets)
  datasets <- match.arg(datasets, c("all", "df_example_generic", "df_example_targeted", "df_example_targeted_legend", "df_example_targeted_compounds_legend", "df_example_sample_data", "df_example_feat_intensities", "df_example_feat_info", "df_example_qc_sampletype", "df_example_melecules_to_search", "df_example_gradient"))
  
  
  # df_example_generic
  
  if (datasets == "all" | datasets == "df_example_generic") {
    if ("df_example_generic" %in% ls(envir=.GlobalEnv)) {warning("An object with the same name was already present in the Global Environment and it was replaced for  df_example_generic")}
    
    assign("df_example_generic",
           
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
    
    cat("\n In the Global Environment, created the object  df_example_generic")
  }
  
  
  # df_example_targeted
  
  if (datasets == "all" | datasets == "df_example_targeted") {
    if ("df_example_targeted" %in% ls(envir=.GlobalEnv)) {warning("An object with the same name was already present in the Global Environment and it was replaced for  df_example_targeted")}
    
    assign("df_example_targeted",
           
           tibble(samples = c("blank", "blank", "blank", "calibration_curve_level00", "calibration_curve_level01", "calibration_curve_level02", "calibration_curve_level03", "calibration_curve_level04", "calibration_curve_level05", "calibration_curve_level06", "blank", "quality_control_low", "quality_control_medium",
                              "quality_control_high", "blank", "unkwon_sample_001", "unkwon_sample_002", "unkwon_sample_003", "unkwon_sample_004", "unkwon_sample_005", "unkwon_sample_006", "unkwon_sample_007", "unkwon_sample_008", "unkwon_sample_009", "unkwon_sample_010", "unkwon_sample_011", "unkwon_sample_012",
                              "unkwon_sample_013", "unkwon_sample_014", "unkwon_sample_015", "unkwon_sample_016", "unkwon_sample_017", "unkwon_sample_018", "unkwon_sample_019", "unkwon_sample_020", "unkwon_sample_021", "unkwon_sample_022", "unkwon_sample_023", "unkwon_sample_024", "unkwon_sample_025",
                              "unkwon_sample_026", "unkwon_sample_027", "unkwon_sample_028", "quality_control_low", "quality_control_medium", "quality_control_high", "unkwon_sample_029", "unkwon_sample_030", "unkwon_sample_031", "unkwon_sample_032", "unkwon_sample_033", "unkwon_sample_034", "unkwon_sample_035",
                              "unkwon_sample_036", "unkwon_sample_037", "unkwon_sample_038", "unkwon_sample_039", "unkwon_sample_040", "unkwon_sample_041", "unkwon_sample_042", "unkwon_sample_043", "unkwon_sample_044", "unkwon_sample_045", "unkwon_sample_046", "unkwon_sample_047", "unkwon_sample_048",
                              "unkwon_sample_049", "unkwon_sample_050", "unkwon_sample_051", "unkwon_sample_052", "quality_control_low", "quality_control_medium", "quality_control_high", "blank", "calibration_curve_level00", "calibration_curve_level01", "calibration_curve_level02",
                              "calibration_curve_level03", "calibration_curve_level04", "calibration_curve_level05", "calibration_curve_level06", "blank", "blank"),
                  molecule01 = c(0, 12, 45, 862, 916839, 2437097, 8664951, 10163671, 13164865, 16546462, 35, 2202987, 8692745, 13350486, 411, 5299471, 2940203, 13367441, 14277443, 7587536, 9989224, 2080783, 7941531, 24155062, 15568945, 9918185, 6143023,
                                 13692452, 5186846, 58765195, 16195716, 3847369, 5362687, 15721693, 5635207, 17651039, 7905700, 9447870, 8915481, 13752458, 6811618, 3491583, 16295367, 2142669, 7034753, 11074961, 1471676, 2434731, 4342005, 3477260, 2123060,
                                 3725890, 3096286, 7418825, 2256042, 3319247, 2634442, 3385002, 4453691, 7634458, 2763587, 8247634, 5209242, 5859578, 3431933, 1992651, 3195087, 2810978, 2284248, 2233549, 2462859, 9862304, 10677785, 232, 7941, 793876, 2810201,
                                 7690566, 9986602, 11788006, 16681300, 32, 234),
                  molecule01_IS = c(85009209.92, 85058548.93, 85544475.02, 84525131.7, 83570564.08, 84157195.23, 84662093.79, 85495504.12, 84600992.31, 85375589.88, 83709521.14, 85109664.21, 86331029.42, 85055146.13, 85131861.1, 84576247.55, 84655306.81, 83715299.4,
                                    84558580.99, 85208916.29, 83750000.01, 83692850.78, 84886850.04, 84969855.99, 83921890.08, 84778700.03, 85136301.72, 85098766.57, 84183313.96, 83970979.95, 84094517, 84613546.76, 84389958.34, 83628774.69, 83227983.02, 85079470.89,
                                    85487798.87, 84294280.65, 83585221.44, 84396505.88, 83939533.96, 84287560.52, 84480473.49, 84023956.88, 84392981.05, 84822711.03, 84865119.61, 84329670.17, 85106202.41, 83632088.06, 84392641.43, 83986705.42, 85694555.06, 84807924,
                                    84500530.54, 83411412.01, 84095142.53, 84569979.99, 83619031.17, 84795788.57, 84100533.12, 84912246.86, 84091619.84, 84889050.02, 83925483.38, 84534019.28, 84679996.4, 84082230.22, 83976479.25, 83567066.5, 84755838.7, 84486996.38,
                                    84338270.57, 85171442.38, 84652209.65, 84590061.31, 84120338.75, 84711754.48, 84318237.92, 85032131.25, 84286658.6, 85290695.14, 84299672.46),
                  molecule02 = c(0, 12, 45, 64851, 69002371, 122279121, 217377977, 254976452, 402513548, 622652692, 35, 99479574, 228979013, 334924209, 411, 49479432, 33567066, 24377551, 49734247, 22413987, 35897940, 12984431, 12365404, 14415842,
                                 30318745, 63699059, 13079678, 20319346, 92483391, 25830433, 65224104, 13892573, 26154477, 15916194, 8293563, 47017606, 27088074, 36836846, 23659359, 22586479, 9597162, 28293287, 14155122, 96755794, 185305208,
                                 277838018, 44365053, 8995249, 30504113, 42778531, 10854886, 44503761, 7926767, 24510875, 14197182, 17008194, 12172861, 18756343, 23322819, 15162758, 16248150, 24361868, 37960051, 17167638, 24412504, 41535375,
                                 17020424, 66029150, 14127533, 34052528, 111214526, 259786848, 267874036, 37560681, 59761, 59747997, 140999273, 192933548, 250534295, 360416318, 627726724, 53, 43),
                  molecule02_IS = c(512295.1211, 513397.7864, 512770.8402, 512250.3683, 513334.2721, 510583.1825, 513191.0512, 514244.7301, 512262.1409, 513297.2884, 513975.6689, 511799.5608, 514406.9595, 512997.402, 513431.8397, 511966.3603,
                                    512366.2289, 514775.6802, 512385.0415, 512415.1878, 513122.4041, 509823.1668, 512930.811, 511789.2665, 512110.0745, 513305.3034, 514771.5609, 513356.9638, 510865.0064, 514096.0755, 512100.2755, 512395.7005,
                                    511849.2348, 512050.2338, 514525.563, 513843.9998, 512591.6465, 511874.0499, 512415.7242, 511647.4954, 511928.3787, 513138.189, 510916.5201, 511557.5737, 512141.4671, 512590.7958, 513251.0032, 511775.3543,
                                    513342.3401, 512882.4249, 511097.2601, 512298.3584, 509755.0589, 511405.4666, 513714.9322, 513114.4857, 512042.3189, 512988.3116, 514329.533, 512218.6786, 512350.762, 513878.7595, 514106.6813, 512297.0154,
                                    512474.8418, 512910.2278, 515342.903, 514785.3904, 510359.3703, 512993.6852, 512383.1253, 511812.0564, 514075.4563, 513558.6982, 513301.0504, 513262.0036, 511362.9277, 512155.6789, 513026.5256, 511331.1577,
                                    511811.5578, 511741.3483, 510962.1805),
                  molecule03 = c(2, 474, 2, 184514, 3926523, 13916386, 51540480, 90682659, 132142516, 177157699, 845, 9434672, 65149313, 114351521, 784, 240779, 955051, 693592, 201504, 637724, 1021378, 369439, 351821, 410168, 862630,
                                 381237, 372142, 578120, 263134, 734923, 185576, 395271, 744147, 54310727, 44959406, 49169408, 30511885, 56855184, 39263409, 58085365, 45582234, 41512525, 42374924, 9176348, 52723203, 94860865, 46290180,
                                 64998773, 65193099, 52676282, 41305172, 58821794, 63449650, 55460412, 91470730, 125907157, 103818823, 152557692, 166621889, 133587714, 102671018, 112766784, 118783482, 130382004, 107885581, 136639311,
                                 162663193, 125573709, 123297212, 78336742, 10547619, 73914785, 91458911, 315561, 170033, 3399911, 16046896, 45744688, 89102801, 118322275, 178601367, 8585, 7),
                  molecule04 = c(2, 32, 2, 545, 57971, 154097, 547882, 642646, 832410, 1046228, 5, 139294, 549640, 844147, 411, 415695, 282009, 204805, 417836, 188308, 301592, 109087, 103886, 121113, 254719, 535160,
                                 109887, 170710, 776988, 217011, 547972, 116717, 219734, 133718, 12868, 395012, 227577, 309480, 198771, 189757, 30221, 237702, 118922, 135480, 444805, 700266, 372727, 75572, 256276, 359398,
                                 91196, 373893, 13626, 205925, 119276, 142892, 102269, 157579, 195944, 127388, 136507, 204673, 318917, 144232, 205099, 348954, 142995, 554736, 118691, 286088, 155726, 623590, 675153, 315561,
                                 502, 50197, 177688, 486272, 631450, 745352, 1054753, 53, 43)),
           
           envir=.GlobalEnv)
    
    cat("\n In the Global Environment, created the object  df_example_targeted")
  }
  
  
  # df_example_targeted_legend
  
  if (datasets == "all" | datasets == "df_example_targeted_legend") {
    if ("df_example_targeted_legend" %in% ls(envir=.GlobalEnv)) {warning("An object with the same name was already present in the Global Environment and it was replaced for  df_example_targeted_legend")}
    
    assign("df_example_targeted_legend",
           
           df_example_targeted_legend <- tibble(samples = c("blank", "blank", "blank", "calibration_curve_level00", "calibration_curve_level01", "calibration_curve_level02", "calibration_curve_level03", "calibration_curve_level04", "calibration_curve_level05", "calibration_curve_level06", "blank", "quality_control_low", "quality_control_medium",
                                                            "quality_control_high", "blank", "unkwon_sample_001", "unkwon_sample_002", "unkwon_sample_003", "unkwon_sample_004", "unkwon_sample_005", "unkwon_sample_006", "unkwon_sample_007", "unkwon_sample_008", "unkwon_sample_009", "unkwon_sample_010", "unkwon_sample_011", "unkwon_sample_012",
                                                            "unkwon_sample_013", "unkwon_sample_014", "unkwon_sample_015", "unkwon_sample_016", "unkwon_sample_017", "unkwon_sample_018", "unkwon_sample_019", "unkwon_sample_020", "unkwon_sample_021", "unkwon_sample_022", "unkwon_sample_023", "unkwon_sample_024", "unkwon_sample_025",
                                                            "unkwon_sample_026", "unkwon_sample_027", "unkwon_sample_028", "quality_control_low", "quality_control_medium", "quality_control_high", "unkwon_sample_029", "unkwon_sample_030", "unkwon_sample_031", "unkwon_sample_032", "unkwon_sample_033", "unkwon_sample_034", "unkwon_sample_035",
                                                            "unkwon_sample_036", "unkwon_sample_037", "unkwon_sample_038", "unkwon_sample_039", "unkwon_sample_040", "unkwon_sample_041", "unkwon_sample_042", "unkwon_sample_043", "unkwon_sample_044", "unkwon_sample_045", "unkwon_sample_046", "unkwon_sample_047", "unkwon_sample_048",
                                                            "unkwon_sample_049", "unkwon_sample_050", "unkwon_sample_051", "unkwon_sample_052", "quality_control_low", "quality_control_medium", "quality_control_high", "blank", "calibration_curve_level00", "calibration_curve_level01", "calibration_curve_level02",
                                                            "calibration_curve_level03", "calibration_curve_level04", "calibration_curve_level05", "calibration_curve_level06", "blank", "blank"),
                                                sample_type = c("blank", "blank", "blank", "curve", "curve", "curve", "curve", "curve", "curve", "curve", "blank", "qc", "qc", "qc", "blank", "unknown", "unknown", "unknown", "unknown", "unknown", "unknown", "unknown", "unknown", "unknown", "unknown", "unknown", "unknown", "unknown", "unknown", "unknown", "unknown", "unknown", "unknown", "unknown", "unknown", "unknown", "unknown", "unknown", "unknown", "unknown", "unknown", "unknown", "unknown", "qc", "qc", "qc", "unknown", "unknown", "unknown", "unknown", "unknown", "unknown", "unknown", "unknown", "unknown", "unknown", "unknown", "unknown", "unknown", "unknown", "unknown", "unknown", "unknown", "unknown", "unknown", "unknown", "unknown", "unknown", "unknown", "unknown", "qc", "qc", "qc", "blank", "curve", "curve", "curve", "curve", "curve", "curve", "curve", "blank", "blank"),
                                                molecule01 = c(NA, NA, NA, 0, 10, 30, 90, 120, 160, 200, NA, 25, 100, 150, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 25, 100, 150, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 25, 100, 150, NA, 0, 10, 30, 90, 120, 160, 200, NA, NA),
                                                molecule02 = c(NA, NA, NA, 0, 100, 200, 300, 400, 650, 1000, NA, 150, 350, 500, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 150, 350, 500, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 150, 350, 500, NA, 0, 100, 200, 300, 400, 650, 1000, NA, NA),
                                                molecule03 = c(NA, NA, NA, 0, 2, 8, 25, 50, 75, 100, NA, 5, 35, 60, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 5, 35, 60, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 5, 35, 60, NA, 0, 2, 8, 25, 50, 75, 100, NA, NA),
                                                molecule04 = c(NA, NA, NA, 0, 10, 30, 90, 120, 160, 200, NA, 25, 100, 150, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 25, 100, 150, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 25, 100, 150, NA, 0, 10, 30, 90, 120, 160, 200, NA, NA)),
           
           envir=.GlobalEnv)
    
    cat("\n In the Global Environment, created the object  df_example_targeted_legend")
  }
  
  
  # df_example_targeted_compounds_legend
  
  if (datasets == "all" | datasets == "df_example_targeted_compounds_legend") {
    if ("df_example_targeted_compounds_legend" %in% ls(envir=.GlobalEnv)) {warning("An object with the same name was already present in the Global Environment and it was replaced for  df_example_targeted_compounds_legend")}
    
    assign("df_example_targeted_compounds_legend",
           
           tibble(compounds = c("molecule01", "molecule02", "molecule03", "molecule04"),
                  matched_IS = c("molecule01_IS", "molecule02_IS", NA, NA),
                  weighting = c("1/Y", "none", "1/(Y^2)", NA),
                  unit = c("ng/mL", "ug/mL", "ng/mL", NA)),
           
           envir=.GlobalEnv)
    
    cat("\n In the Global Environment, created the object  df_example_targeted_compounds_legend")
  }
  
  
  
  # df_example_sample_data
  
  if (datasets == "all" | datasets == "df_example_sample_data") {
    if ("df_example_sample_data" %in% ls(envir=.GlobalEnv)) {warning("An object with the same name was already present in the Global Environment and it was replaced for  df_example_sample_data")}
    
    assign("df_example_sample_data",
           
           tibble(samples = c("unkwon_sample_001", "unkwon_sample_002", "unkwon_sample_003", "unkwon_sample_004", "unkwon_sample_005", "unkwon_sample_006", "unkwon_sample_007", "unkwon_sample_008", "unkwon_sample_009", "unkwon_sample_010", "unkwon_sample_011",
                              "unkwon_sample_012", "unkwon_sample_013", "unkwon_sample_014", "unkwon_sample_015", "unkwon_sample_016", "unkwon_sample_017", "unkwon_sample_018", "unkwon_sample_019", "unkwon_sample_020", "unkwon_sample_021", "unkwon_sample_022",
                              "unkwon_sample_023", "unkwon_sample_024", "unkwon_sample_025", "unkwon_sample_026", "unkwon_sample_027", "unkwon_sample_028", "unkwon_sample_029", "unkwon_sample_030", "unkwon_sample_031", "unkwon_sample_032", "unkwon_sample_033",
                              "unkwon_sample_034", "unkwon_sample_035", "unkwon_sample_036", "unkwon_sample_037", "unkwon_sample_038", "unkwon_sample_039", "unkwon_sample_040", "unkwon_sample_041", "unkwon_sample_042", "unkwon_sample_043", "unkwon_sample_044",
                              "unkwon_sample_045", "unkwon_sample_046", "unkwon_sample_047", "unkwon_sample_048", "unkwon_sample_049", "unkwon_sample_050", "unkwon_sample_051", "unkwon_sample_052"),
                  factor_condition2lev = factor(c("F2A", "F2A", "F2A", "F2A", "F2A", "F2A", "F2A", "F2A", "F2A", "F2A", "F2A", "F2A", "F2A", "F2A", "F2A", "F2A", "F2A", "F2A", "F2A", "F2A", "F2A", "F2A", "F2A", "F2A", "F2A", "F2A", "F2A", "F2A", "F2B", "F2B", "F2B", "F2B", "F2B", "F2B", "F2B",
                                                  "F2B", "F2B", "F2B", "F2B", "F2B", "F2B", "F2B", "F2B", "F2B", "F2B", "F2B", "F2B", "F2B", "F2B", "F2B", "F2B", "F2B"),
                                                levels = c("F2A", "F2B")),
                  factor_condition3lev = factor(c("F3A", "F3A", "F3A", "F3A", "F3A", "F3A", "F3A", "F3A", "F3A", "F3A", "F3A", "F3A", "F3A", "F3A", "F3A", "F3A", "F3A", "F3A", "F3B", "F3B", "F3B", "F3B", "F3B", "F3B", "F3B", "F3B", "F3B", "F3B", "F3B", "F3B", "F3B", "F3B", "F3B", "F3B", "F3B",
                                                  "F3B", "F3C", "F3C", "F3C", "F3C", "F3C", "F3C", "F3C", "F3C", "F3C", "F3C", "F3C", "F3C", "F3C", "F3C", "F3C", "F3C"),
                                                levels = c("F3A", "F3B", "F3C")),
                  factor_condition4lev = factor(c("F4A", "F4A", "F4A", "F4A", "F4A", "F4A", "F4A", "F4A", "F4A", "F4A", "F4A", "F4A", "F4A", "F4A", "F4B", "F4B", "F4B", "F4B", "F4B", "F4B", "F4B", "F4B", "F4B", "F4B", "F4B", "F4B", "F4B", "F4B", "F4C", "F4C", "F4C", "F4C", "F4C", "F4C",
                                                  "F4C", "F4C", "F4C", "F4C", "F4C", "F4C", "F4D", "F4D", "F4D", "F4D", "F4D", "F4D", "F4D", "F4D", "F4D", "F4D", "F4D", "F4D"),
                                                levels = c("F4A", "F4B", "F4C", "F4D")),
                  numerical_condition_a = c(4144.84, 2730.27, 1830.29, 3890.55, 1894.76, 2996.01, 1166.63, 1023.71, 1179.53, 2242.16, 5851.33, 1106.37, 2028.75, 9023.63, 1972.85, 5895.74, 1257.17, 2104.12, 1482.95, 118.38, 4180.7, 2121.63, 4206.87, 2278.91, 1815.32, 289.36, 2270.71,
                                            1224.11, 3480.86, 696.85, 2441.42, 4906.19, 922.13, 3808.38, 85.84, 1978.02, 1332.19, 1575.63, 863.88, 1434.16, 1850.85, 1231.52, 1338.36, 2527.26, 3404.2, 1502.53, 2282.31, 2882.4, 1744.4, 5292.4, 1482.27, 4019.11),
                  numerical_condition_b = c(4.9, 4, 5.9, 6.1, 14.8, 27.1, 23, 23.8, 24.8, 6.7, 13.1, 10, 11.9, 26.6, 11.6, 6.8, 18.1, 4.3, 26.3, 5.4, 17, 13.4, 19.3, 22.5, 27.7, 21.5, 27.8, 15.7, 15, 24.6, 25.2, 18.5, 5.6, 15.9, 25.9, 18.5, 18.1, 21.6, 27.1, 8.1, 3.7, 6.5,
                                            15, 12.9, 8.5, 23.9, 0.6, 16.9, 17.8, 21.5, 22.1, 21.1)),
           
           envir=.GlobalEnv)
    
    cat("\n In the Global Environment, created the object  df_example_sample_data")
  }
  
  
  
  
  # df_example_feat_intensities
  
  
  if (datasets == "all" | datasets == "df_example_feat_intensities") {
    if ("df_example_feat_intensities" %in% ls(envir=.GlobalEnv)) {warning("An object with the same name was already present in the Global Environment and it was replaced for  df_example_feat_intensities")}
    
    assign("df_example_feat_intensities",
           
           tibble(featname = c("feature001", "feature002", "feature003", "feature004", "feature005", "feature006", "feature007", "feature008", "feature009"),
                  QCpooled_tot01 = c(74181229, 118220210, 110622819, 2294835, 153153, NA, 139350, 30986875, 20555071),
                  QCpooled_tot02 = c(42824388, 115639566, 127316421, 2492735, 116759, 202824643, 80736, 12910756, 21102214),
                  blank_untargeted01 = c(720501, 8321, NA, 42, NA, 20093295, NA, 13343, 23060),
                  QCpooled_tot03 = c(657266255, 106082721, 137718462, 2504627, 196216, 446030, 71090, 754989, NA),
                  QCpooled_F3A01 = c(157673074, NA, 139475, NA, NA, 139536561, 184025, 753814, 15022136),
                  QCpooled_F3B01 = c(1077950445, NA, 429458599, NA, NA, NA, 38877, 15961739, 35830266),
                  QCpooled_F3C01 = c(74998356919, NA, 6481523, NA, NA, 106199709, 46093, 16013580, 30053274),
                  unkwon_sample_001 = c(171639844, 1041717, 123288, 9409, 115952, 22432321, 43897, 2450691, 27104212),
                  unkwon_sample_002 = c(NA, 1838405, 178957, 7624, 104632, 3988, 36940, 13177910, 39761464),
                  unkwon_sample_003 = c(479485129, 1833003, 147968, 8385, 106887, 8958546, 185687, 17050951, 25144541),
                  unkwon_sample_004 = c(116543572, 2283669, 136045, 7474, 241976, 749770572, 134781, 5192236, 32142174),
                  unkwon_sample_005 = c(NA, 2295150, 156292, 8628, 43063, 20222313, 39105, 7903034, 28836529),
                  unkwon_sample_006 = c(690645472, 1298802, 162036, 8940, 156794, 46054526204, 228596, 33636399, 32056702),
                  unkwon_sample_007 = c(223439230, 1115286, 178470, 7626, 187598, NA, 104998, 1405012, 25827995),
                  unkwon_sample_008 = c(NA, 3544027, 168161, 8532, 17630, 1631141868, 86601, 26071228, NA),
                  unkwon_sample_009 = c(NA, 1798661, 94692, 9326, 34954, 126919752, 70759, 42239, 30938694),
                  unkwon_sample_010 = c(123789682, 1127499, 180938, 8986, 99536, 4949981657, 39848, 107613528, 33975576),
                  QCpooled_tot04 = c(24986186494, 110012352, 122148366, 2416478, 108955, NA, 50245, 184577, 23498554),
                  QCpooled_F3A02 = c(419746067, NA, 157781, NA, NA, 20908004232, 51523, 1853765, 27321988),
                  QCpooled_F3B02 = c(154842, NA, 464151802, NA, NA, 75458289, 139899, 1019804, 20958616),
                  QCpooled_F3C02 = c(18873460330, NA, 8841809, NA, NA, 9579535, 39647, 142840131, 22178457),
                  unkwon_sample_011 = c(68185152, 1005145, 140086, 7145, 417615, 1321517034, 57098, 198326, NA),
                  unkwon_sample_012 = c(2598513890, 1531211, 148983, 7831, 70460, 52376812369, 95238, 8544656, 22112554),
                  unkwon_sample_013 = c(60909604, 1224214, 186783, 8643, 117845, 67312, 28530, 199779, 41312482),
                  unkwon_sample_014 = c(437312607, 1345261, 186215, 7862, 486143, 21511145247, 61544, 2226593362, 21468614),
                  unkwon_sample_015 = c(401604907, 1012857, 165683, 153124, 173294, 18606731430, 62034, 904281, 17488231),
                  unkwon_sample_016 = c(241161431, 1285906, 231816, 195940, 421911, NA, 53600, 8584304, 24405196),
                  unkwon_sample_017 = c(NA, 1134261, 180686, 155267, 126466, 18771351, 75654, 20504717, 23053836),
                  unkwon_sample_018 = c(388342416, 1715623, 172867, 148114, 104999, 74426662, 35142, 6074428, 24044628),
                  unkwon_sample_019 = c(1015791106, 2016460, 372207367, 151109, 110284, 164458369, 83773, 2310300, 21625238),
                  unkwon_sample_020 = c(109393637, 921556, 280039924, 161471, 3136, NA, 81306, 1765597, 27728888),
                  QCpooled_tot05 = c(NA, 123423692, 134070297, 2520121, 147891, 3157, 155827, 54943128, 27918600),
                  QCpooled_F3A03 = c(14597420316, NA, 215491, NA, NA, NA, 107655, 333702, 22579384),
                  QCpooled_F3B03 = c(NA, NA, 489735822, NA, NA, 738958, 79799, 2362953, 21373322),
                  QCpooled_F3C03 = c(NA, NA, 7545347, NA, NA, 23497216, 77225, 14185519, 30930181),
                  unkwon_sample_021 = c(630662587, 1547646, 639835633, 172124, 142519, 13620850978, 44254, 3508420, 29754277),
                  unkwon_sample_022 = c(802625101, 2219380, 367928397, 158402, 310395, 1652474, 35578, 76372332, 29694946),
                  unkwon_sample_023 = c(NA, 2848363, 358546527, 151055, 237520, 133344206, 77223, 35131785, 33985451),
                  unkwon_sample_024 = c(1510681529, 1527085, 367187767, 182947, 107515, 7273, 72464, 6388480, 29551433),
                  unkwon_sample_025 = c(1336324290, 976814, 257303106, 156953, 56177, 13458571178, 98601, 92056843, 24861105),
                  unkwon_sample_026 = c(311071260, 3223621, 237429636, 185691, 20963, 37101, 34514, 591882763, 26892468),
                  unkwon_sample_027 = c(1602648498, 1183546, 306193731, 171138, 71590, NA, 118496, 21812360, 43930682),
                  unkwon_sample_028 = c(236296953, 1733581, 332080698, 169265, 45476, 11221051955, 76100, 942125, 25253675),
                  unkwon_sample_029 = c(149409159, 222592794, 349212241, 1091169, 710306, 2685497, 51584, 19161218, 37801343),
                  unkwon_sample_030 = c(267179622, 180525455, 352388214, 1546822, 45102, 31794057, 86068, 112059723, 27043419),
                  QCpooled_tot06 = c(NA, 114326261, 107822623, 2414452, 112025, 661744, 67997, 579974511, 22588309),
                  QCpooled_F3A04 = c(3683326236, NA, 186655, NA, NA, 54589718, 56999, 19052731, 40263662),
                  QCpooled_F3B04 = c(11906297793, NA, 416931551, NA, NA, NA, 74999, 2463221, 22096433),
                  QCpooled_F3C04 = c(4837625, NA, 7070170, NA, NA, NA, 74504, 4806458, 18245274),
                  unkwon_sample_031 = c(215634240, 116602480, 310248345, 1416051, 331617, NA, 44276, 7435470, 27887625),
                  unkwon_sample_032 = c(715756444, 225991451, 305028912, 1272400, 266776, 640204753, 123442, 17010516, 23533367),
                  unkwon_sample_033 = c(443605752, 344157347, 340276972, 1040428, 32759, 179427975, 46304, 183754, 34626716),
                  unkwon_sample_034 = c(NA, 262556676, 264783821, 1041894, 289359, 6803243383, 76563, 15009809, NA),
                  unkwon_sample_035 = c(909253142, 367963453, 366358449, 1215081, 1424, NA, 46542, 268090, 33171254),
                  unkwon_sample_036 = c(309939069, 223905109, 240945904, 1462472, 161393, 5677317, 52625, 1061000, 26768156),
                  unkwon_sample_037 = c(1318255308, 235394493, 8828622, 1026851, 85873, 609, 119178, 81766950, 21792032),
                  unkwon_sample_038 = c(NA, 284787261, 6123760, 1194032, 98832, 94395812, 87438, 1097921, 36172185),
                  unkwon_sample_039 = c(1129286946, 195742872, 11023039, 1246179, 83804, 76703, 118232, 60295847, 29676868),
                  unkwon_sample_040 = c(573280583, 384647134, 11471826, 1290217, 91468, 3967259378, 99753, 31172386, 24416550),
                  QCpooled_tot07 = c(4088312, 117348755, 107027505, 2333513, 135436, 179139850, 58556, 968510, 23361438),
                  QCpooled_F3A05 = c(285863902, NA, 248138, NA, NA, 2808, 137238, 8126828, 32795098),
                  QCpooled_F3B05 = c(6510184, NA, 378919772, NA, NA, 848693362, 67371, 5436189, 21244535),
                  QCpooled_F3C05 = c(NA, NA, 7351224, NA, NA, NA, 52484, 594330, 18674149),
                  unkwon_sample_041 = c(NA, 221985183, 10321417, 8273793, 89067, 137585, 34238, 11380886, 24927101),
                  unkwon_sample_042 = c(422850903, 228271074, 6321094, 9988637, 175287, 1393209742, 76350, 2730638, 27112385),
                  unkwon_sample_043 = c(914259424, 270995409, 4297894, 7954898, 113799, 3100249, 80048, 349247, 23272254),
                  unkwon_sample_044 = c(1230283950, 389112932, 9610675, 9440674, 104931, 2072837044, 60374, 3341721, 28923997),
                  unkwon_sample_045 = c(861569513, 261215266, 8169447, 10080034, 111678, 48772071, 39423, 1790872, 27832743),
                  unkwon_sample_046 = c(478315647, 358203750, 8407359, 9953247, 99668, 2922916338, 111225, 3983617, 25522975),
                  unkwon_sample_047 = c(374032288, 187766571, 8894085, 8036962, 136927, 45004, 59140, 8446640, 34054946),
                  unkwon_sample_048 = c(1404293433, 255651167, 12493476, 10223571, 371455, 2867845, 63791, 2545677, 24463464),
                  unkwon_sample_049 = c(425642018, 211095666, 9022728, 7481663, 121190, NA, 44736, 16611808, 24369668),
                  unkwon_sample_050 = c(NA, 246683794, 6361479, 10375571, 155753, NA, 41372, 248726541, 23036365),
                  unkwon_sample_051 = c(NA, 201424132, 7512847, 9472303, 21650, NA, 36690, 187546230, 28325866),
                  unkwon_sample_052 = c(682619261, 316833365, 8181521, 10269266, 111230, 1485942, 87779, 598180, 28213677),
                  QCpooled_tot08 = c(16255457, 121080901, 133400368, 2272625, 103269, 200608421, 44112, 44060, 26134995),
                  blank_untargeted02 = c(NA, 211, NA, 342, NA, NA, NA, 6691, NA)),
           
           envir=.GlobalEnv)
    
    cat("\n In the Global Environment, created the object  df_example_feat_intensities")
  }
  
  
  # df_example_feat_info
  
  if (datasets == "all" | datasets == "df_example_feat_info") {
    if ("df_example_feat_info" %in% ls(envir=.GlobalEnv)) {warning("An object with the same name was already present in the Global Environment and it was replaced for  df_example_feat_info")}
    
    assign("df_example_feat_info",
           
           tibble(featname = c("feature001", "feature002", "feature003", "feature004", "feature005", "feature006", "feature007", "feature008", "feature009"),
                  rt = c(242, 288, 408, 528, 465, 476, 342, 221, 593),
                  mz = c(120.0116, 146.0451, 203.0822, 148.0434, 164.0714, 164.0715, 204.09013, 104.10786, 425.3517)),
           
           envir=.GlobalEnv)
    
    cat("\n In the Global Environment, created the object  df_example_feat_info")
  }
  
  
  # df_example_qc_sampletype
  
  if (datasets == "all" | datasets == "df_example_qc_sampletype") {
    if ("df_example_qc_sampletype" %in% ls(envir=.GlobalEnv)) {warning("An object with the same name was already present in the Global Environment and it was replaced for  df_example_qc_sampletype")}
    
    assign("df_example_qc_sampletype",
           
           tibble(samples = c("QCpooled_tot01", "QCpooled_tot02", "blank_untargeted01", "QCpooled_tot03", "QCpooled_F3A01", "QCpooled_F3B01", "QCpooled_F3C01", "unkwon_sample_001", "unkwon_sample_002", "unkwon_sample_003", "unkwon_sample_004",
                              "unkwon_sample_005", "unkwon_sample_006", "unkwon_sample_007", "unkwon_sample_008", "unkwon_sample_009", "unkwon_sample_010", "QCpooled_tot04", "QCpooled_F3A02", "QCpooled_F3B02", "QCpooled_F3C02", "unkwon_sample_011",
                              "unkwon_sample_012", "unkwon_sample_013", "unkwon_sample_014", "unkwon_sample_015", "unkwon_sample_016", "unkwon_sample_017", "unkwon_sample_018", "unkwon_sample_019", "unkwon_sample_020", "QCpooled_tot05", "QCpooled_F3A03",
                              "QCpooled_F3B03", "QCpooled_F3C03", "unkwon_sample_021", "unkwon_sample_022", "unkwon_sample_023", "unkwon_sample_024", "unkwon_sample_025", "unkwon_sample_026", "unkwon_sample_027", "unkwon_sample_028", "unkwon_sample_029",
                              "unkwon_sample_030", "QCpooled_tot06", "QCpooled_F3A04", "QCpooled_F3B04", "QCpooled_F3C04", "unkwon_sample_031", "unkwon_sample_032", "unkwon_sample_033", "unkwon_sample_034", "unkwon_sample_035", "unkwon_sample_036",
                              "unkwon_sample_037", "unkwon_sample_038", "unkwon_sample_039", "unkwon_sample_040", "QCpooled_tot07", "QCpooled_F3A05", "QCpooled_F3B05", "QCpooled_F3C05", "unkwon_sample_041", "unkwon_sample_042", "unkwon_sample_043",
                              "unkwon_sample_044", "unkwon_sample_045", "unkwon_sample_046", "unkwon_sample_047", "unkwon_sample_048", "unkwon_sample_049", "unkwon_sample_050", "unkwon_sample_051", "unkwon_sample_052", "QCpooled_tot08", "blank_untargeted02"),
                  action = c("REMOVE", "REMOVE", "blank", "QC", "QC", "QC", "QC", "unknown", "unknown", "unknown", "unknown", "unknown", "unknown", "unknown", "unknown", "unknown", "unknown", "QC", "QC", "QC", "QC", "unknown", "unknown", "unknown",
                             "unknown", "unknown", "unknown", "unknown", "unknown", "unknown", "unknown", "QC", "QC", "QC", "QC", "unknown", "unknown", "unknown", "unknown", "unknown", "unknown", "unknown", "unknown", "unknown", "unknown", "QC", "QC", "QC", "QC",
                             "unknown", "unknown", "unknown", "unknown", "unknown", "unknown", "unknown", "unknown", "unknown", "unknown", "QC", "QC", "QC", "QC", "unknown", "unknown", "unknown", "unknown", "unknown", "unknown", "unknown", "unknown", "unknown",
                             "unknown", "unknown", "unknown", "QC", "blank"),
                  qc_group = c(NA, NA, NA, "tot", "F3A", "F3B", "F3C", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "tot", "F3A", "F3B", "F3C", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "tot", "F3A", "F3B", "F3C",
                               NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "tot", "F3A", "F3B", "F3C", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "tot", "F3A", "F3B", "F3C", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                               NA, NA, "tot", NA)),
           
           envir=.GlobalEnv)
    
    cat("\n In the Global Environment, created the object  df_example_qc_sampletype")
  }
  
  
  
  # df_example_melecules_to_search
  
  if (datasets == "all" | datasets == "df_example_melecules_to_search") {
    if ("df_example_melecules_to_search" %in% ls(envir=.GlobalEnv)) {warning("An object with the same name was already present in the Global Environment and it was replaced for  df_example_melecules_to_search")}
    
    assign("df_example_melecules_to_search",
           
           tibble(coumpound = c("Alanine", "Asparagine", "Cysteine", "Glutamate", "Glycine", "Leucine", "Methionine", "Phenylalanine"),
                  rt = c(146, 262, 243, 290, 169, 98, 258, 471),
                  mz = c(88.0399, 131.0457, 120.0119, 146.0453, 74.0242, 130.0868, 148.0432, 164.0712)),
           
           envir=.GlobalEnv)
    
    cat("\n In the Global Environment, created the object  df_example_melecules_to_search")
  }
  
  # df_example_gradient
  
  if (datasets == "all" | datasets == "df_example_gradient") {
    if ("df_example_gradient" %in% ls(envir=.GlobalEnv)) {warning("An object with the same name was already present in the Global Environment and it was replaced for  df_example_gradient")}
    
    assign("df_example_gradient",
           
           tibble(
             minute = c(0, 0.5, 4, 7, 11, 16, 16.1, 22),
             flow = rep(0.3,8),
             acquoeos = c(80, 80, 60, 60, 0, 0, 80, 80),
             organic = c(20, 20, 40, 40, 100, 100, 20, 20)
           ),
           
           envir=.GlobalEnv)
    
    cat("\n In the Global Environment, created the object  df_example_gradient")
  }
  
  
  cat("\n")
  cat("\n")
}

