all: plot/transport-connections.pdf


plot/transport-connections.pdf: \
		plot/ports-locations.csv \
		plot/port.png \
		plot/airport.png \
		renaming.r \
		helper-functions.r \
		data/fortified-shapefiles.rdata \
		data/imports-exports-year-to-date_impexp.rdata \
		plot/make-plot.r
	cd plot && Rscript make-plot.r


data/fortified-shapefiles.rdata: \
		data/shapefiles/Traffic\ Volumes/Traffic_All_Volumes_2012_WFS.shp \
		data/shapefiles/bigislands.shp \
		data/fortify-roads-one-off.r
	cd data && Rscript fortify-roads-one-off.r


data/imports-exports-year-to-date_impexp.rdata: \
		data/20140228_data_on_value_of_imports_by_port.csv \
		data/20140228_data_on_value_of_exports_by_port.csv \
		data/get-spreadsheets.r
	cd data && Rscript get-spreadsheets.r


