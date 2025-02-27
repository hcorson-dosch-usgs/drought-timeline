source("0_config/src/authentication_helpers.R")

p0_targets <- list(
  
  # Set parent ScienceBase item ID 
  tar_target(p0_sbitem_parent, "62793493d34e8d45aa6e3ba9"),
  
  # Set child ScienceBase item ID 
  tar_target(p0_sbitem_child_1981_2020, "62f475abd34eacf5397395e8"),
  
  # Set child ScienceBase item ID 
  tar_target(p0_sbitem_child_1951_2020, "627974ccd34e8d45aa6e3c81"),
  
  # Set child ScienceBase item ID 
  tar_target(p0_sbitem_child_1921_2020, "62f463c5d34eacf5397395d9"),
  
  # Set date for re-fetching data
  # Change this if you want to repull the data
  tar_target(p0_sb_fetch_date, "2022-08-24"),
  
  # Make sure that the user can authenticate to ScienceBase for later targets that push
  # or pull. Targets that need to push/pull from ScienceBase will use this as input.
  tar_target(p0_sb_credentials_exist, has_sb_credentials())
  
)
