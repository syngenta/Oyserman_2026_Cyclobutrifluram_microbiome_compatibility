#!/usr/bin/env make -f

# either 'magick', 'magick convert', or 'convert' (depending on version)
MAGICK_CONVERT = magick

EPSDIR := ./results/eps
PNGDIR := ./results/png

DPI = 1200  # Springer minimum for raster images

IMGS_TO_CONVERT := $(wildcard $(EPSDIR)/*.eps)
IMG_CONVERTED := $(addprefix $(PNGDIR)/,$(notdir $(addsuffix .png,$(IMGS_TO_CONVERT))))


.DELETE_ON_ERROR:


.PHONY: all
all: $(IMG_CONVERTED)
	@echo "All conversions finished!"


# image optimisation
# requires imagemagick
$(PNGDIR)/%.png: $(EPSDIR)/%
	# EPS -> PNG
	$(MAGICK_CONVERT) \
		-density $(DPI) "$<" -background white -alpha remove -alpha off -quality 90 \
		-set comment "Source $<" \
		"$@"



.PHONY: clean
clean:
	@rm --verbose -- $(wildcard $(PNGDIR)/*.png)
	@echo "Output files cleaned!"
