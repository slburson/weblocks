# Necessary preliminaries
STYLESHEETS = reset.less variables.less

# grid.less: The Semantic Grid System.
# Why do we need this with Bootstrap?  See: https://github.com/twitter/bootstrap/issues/1636
STYLESHEETS += semantic-grid.less

# Bootstrap proper (just the parts we use)
STYLESHEETS += mixins.less scaffolding.less grid.less layouts.less \
	type.less code.less forms.less tables.less \
	buttons.less navs.less pagination.less \
	hero-unit.less utilities.less

# Finally, Weblocks additions and local customizations.
STYLESHEETS += weblocks.less

weblocks.css: $(STYLESHEETS)
	cat $(STYLESHEETS) > weblocks.tmp.less
	lessc weblocks.tmp.less > weblocks.css
	-rm weblocks.tmp.less
