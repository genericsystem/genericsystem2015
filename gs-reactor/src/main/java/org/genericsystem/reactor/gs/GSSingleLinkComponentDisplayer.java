package org.genericsystem.reactor.gs;

import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.gstag.HtmlLabel.GSLabelDisplayer;
import org.genericsystem.reactor.model.ObservableListExtractor;

public class GSSingleLinkComponentDisplayer extends GSDiv {

	public GSSingleLinkComponentDisplayer(Tag parent) {
		super(parent, FlexDirection.ROW);
		Tag label = new GSLabelDisplayer(this);
	}

	public static class GSLinkComponentsDisplayer extends GSSingleLinkComponentDisplayer {

		public GSLinkComponentsDisplayer(Tag parent) {
			super(parent);
			// TODO: filter only once.
			forEach((ObservableListExtractor) gs -> ObservableListExtractor.COMPONENTS.apply(gs).filtered(g -> !g.equals(gs[2])));
		}
	}

	public static class GSLinkComponentsTitleDisplayer extends GSSingleLinkComponentDisplayer {

		public GSLinkComponentsTitleDisplayer(Tag parent) {
			super(parent);
			forEach((ObservableListExtractor) gs -> ObservableListExtractor.COMPONENTS.apply(gs).filtered(g -> !g.equals(gs[1])));
		}
	}

	public static class GSInstanceLinkComponentsTitleDisplayer extends GSSingleLinkComponentDisplayer {

		public GSInstanceLinkComponentsTitleDisplayer(Tag parent) {
			super(parent);
			forEach((ObservableListExtractor) gs -> ObservableListExtractor.COMPONENTS.apply(gs).filtered(g -> !g.equals(gs[1].getMeta())));
		}
	}
}