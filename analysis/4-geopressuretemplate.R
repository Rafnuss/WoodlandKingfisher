library(GeoPressureR)


##  Run workflow step-by-step for a single tag
# See https://raphaelnussbaumer.com/GeoPressureManual/geopressuretemplate-workflow.html
id <- "22NO" #  "16LN" "16LO" "16LP" "20IK" "22NO"
# geopressuretemplate_config(id)
tag <- geopressuretemplate_tag(id)
graph <- geopressuretemplate_graph(id)
geopressuretemplate_pressurepath(id)


## Run workflow for all tags
list_id <- tail(names(yaml::yaml.load_file("config.yml", eval.expr = FALSE)), -1)

for (id in list_id){
  geopressuretemplate(id)
}
