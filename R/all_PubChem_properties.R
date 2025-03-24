
#' All PubChem properties
#'
#' This function generates a vector with all the PubChem properties. These ones are retrieved when passing "all" to the argument properties of the function getChemData.
#'
#' @return a character vector with all the PubChem properties.
#'
#' @export
all_PubChem_properties <- function() {
  return(c("MolecularFormula", "MolecularWeight", "SMILES", "CanonicalSMILES", "IsomericSMILES", "InChI", "InChIKey", "IUPACName", "Title",
           "XLogP", "ExactMass", "MonoisotopicMass", "TPSA", "Complexity", "Charge", "HBondDonorCount", "HBondAcceptorCount", "RotatableBondCount", "HeavyAtomCount",
           "IsotopeAtomCount", "AtomStereoCount", "DefinedAtomStereoCount", "UndefinedAtomStereoCount", "BondStereoCount", "DefinedBondStereoCount", "UndefinedBondStereoCount",
           "CovalentUnitCount", "PatentCount", "PatentFamilyCount", "LiteratureCount", "Volume3D", "XStericQuadrupole3D", "YStericQuadrupole3D", "ZStericQuadrupole3D", "FeatureCount3D",
           "FeatureAcceptorCount3D", "FeatureDonorCount3D", "FeatureAnionCount3D", "FeatureCationCount3D", "FeatureRingCount3D", "FeatureHydrophobeCount3D", "ConformerModelRMSD3D", "EffectiveRotorCount3D", "ConformerCount3D", "Fingerprint2D"))
}

