# S7_Calibration.R
# ::rtemis::
# 2025 EDG rtemis.org

# Calibration ----
#' @title Calibration
#' 
#' @description
#' Calibration class.
#' 
#' @author EDG
#' @noRd
Calibration <- new_class(
  name = "Calibration",
  properties = list(
    algorithm = class_character,
    model = class_any,
    hyperparameters = Hyperparameters | NULL,
    calibration_metrics = class_list,
    session_info = class_any
  ),
  constructor = function(algorithm,
                         model,
                         hyperparameters,
                         calibration_metrics) {
    new_object(
      S7_object(),
      algorithm = algorithm,
      model = model,
      hyperparameters = hyperparameters,
      session_info = sessionInfo(),
      calibration_metrics = calibration_metrics
    )
  }
) # /Calibration

CalibrationCV <- new_class(
  name = "CalibrationCV",
  properties = list(
    algorithm = class_character,
    models = class_any,
    hyperparameters = Hyperparameters | NULL,
    calibration_metrics = class_list,
    session_info = class_any
  ),
  constructor = function(algorithm,
                         model,
                         hyperparameters) {
    new_object(
      S7_object(),
      algorithm = algorithm,
      model = model,
      hyperparameters = hyperparameters,
      session_info = sessionInfo()
    )
  }
) # /CalibrationCV