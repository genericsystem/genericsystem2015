package org.genericsystem.reactor.gs;

import org.genericsystem.reactor.gstag.GSLabel.GSLabelDisplayer;
import org.genericsystem.reactor.model.ObservableListExtractor;
import org.genericsystem.reactor.model.StringExtractor;

public class GSSingleLinkComponentDisplayer extends GSSection {

	public GSSingleLinkComponentDisplayer(GSTag parent) {
		super(parent, FlexDirection.ROW);
		GSTag label = new GSLabelDisplayer(this);
	}

	public static class GSLinkComponentsDisplayer extends GSSingleLinkComponentDisplayer {

		public GSLinkComponentsDisplayer(GSTag parent) {
			super(parent);
			// TODO: filter only once.
			forEach(StringExtractor.SIMPLE_CLASS_EXTRACTOR, gs -> ObservableListExtractor.COMPONENTS.apply(gs).filtered(g -> !g.equals(gs[3])));
		}
	}

	public static class GSLinkComponentsTitleDisplayer extends GSSingleLinkComponentDisplayer {

		public GSLinkComponentsTitleDisplayer(GSTag parent) {
			super(parent);
			forEach(StringExtractor.SIMPLE_CLASS_EXTRACTOR, gs -> ObservableListExtractor.COMPONENTS.apply(gs).filtered(g -> !g.equals(gs[2])));
		}
	}

	public static class GSInstanceLinkComponentsTitleDisplayer extends GSSingleLinkComponentDisplayer {

		public GSInstanceLinkComponentsTitleDisplayer(GSTag parent) {
			super(parent);
			forEach(StringExtractor.SIMPLE_CLASS_EXTRACTOR, gs -> ObservableListExtractor.COMPONENTS.apply(gs).filtered(g -> !g.equals(gs[2].getMeta())));
		}
	}
}