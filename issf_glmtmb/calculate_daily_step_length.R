#all birds
ssf_data %>% 
  filter(case_ == TRUE) %>% 
  pull(sl_) %>% 
  mean()

ssf_data %>% 
  filter(case_ == TRUE) %>% 
  pull(sl_) %>% 
  sd()

# Males
ssf_data %>% 
  filter(case_ == TRUE, Sex == "Male") %>% 
  pull(sl_) %>% 
  mean()

ssf_data %>% 
  filter(case_ == TRUE, Sex == "Male") %>% 
  pull(sl_) %>% 
  sd()

# Females
ssf_data %>% 
  filter(case_ == TRUE, Sex == "Female") %>% 
  pull(sl_) %>% 
  mean()

ssf_data %>% 
  filter(case_ == TRUE, Sex == "Female") %>% 
  pull(sl_) %>% 
  sd()
