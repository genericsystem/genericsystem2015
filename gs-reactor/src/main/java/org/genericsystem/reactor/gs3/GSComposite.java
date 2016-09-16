package org.genericsystem.reactor.gs3;

import org.genericsystem.reactor.RootTagImpl;
import org.genericsystem.reactor.annotations.Parent;
import org.genericsystem.reactor.annotations.ReactorDependencies;
import org.genericsystem.reactor.gs.GSDiv;
import org.genericsystem.reactor.gs2.GSCell.GSFirstRowFirstCell;
import org.genericsystem.reactor.gs3.GSComposite.GSComponent.GSContentComponent;
import org.genericsystem.reactor.gs3.GSComposite.GSComponent.GSFooterComponent;
import org.genericsystem.reactor.gs3.GSComposite.GSComponent.GSHeaderComponent;
import org.genericsystem.reactor.gs3.GSComposite.GSLabeledTable.GSLabel00;
import org.genericsystem.reactor.gs3.GSComposite.GSLabeledTable.GSLabel01;
import org.genericsystem.reactor.gs3.GSComposite.GSLabeledTable.GSLabel10;
import org.genericsystem.reactor.gs3.GSComposite.GSLabeledTable.GSLabel11;
import org.genericsystem.reactor.gs3.GSComposite.GSTable.GSFirstRow.GSFirstRowContentCell;
import org.genericsystem.reactor.gs3.GSComposite.GSTable.GSRowCell.GSContentRowContentCell;
import org.genericsystem.reactor.gs3.GSComposite.GSTable.GSRowCell.GSContentRowFirstCell;
import org.genericsystem.reactor.gstag.HtmlLabel.GSLabelDisplayer;

@ReactorDependencies(GSContentComponent.class)
public class GSComposite extends RootTagImpl {
	
	public static class GSComponent extends GSDiv {
		
		public static class GSHeaderComponent extends GSComponent {

		}

		public static class GSContentComponent extends GSComponent {

		}

		public static class GSFooterComponent extends GSComponent {

		}
	}

	@ReactorDependencies({ GSHeaderComponent.class, GSContentComponent.class })
	public static class GSHeaderComposite extends GSComposite {

	}

	

	@ReactorDependencies({ GSHeaderComponent.class, GSContentComponent.class, GSFooterComponent.class })
	public static class GSHeaderFooterComposite extends GSComposite {

	}

	@ReactorDependencies({ GSFirstRowFirstCell.class, GSContentRowContentCell.class, GSContentRowFirstCell.class, GSContentRowContentCell.class })
	public static class GSTable extends GSHeaderComposite {
		public static class GSFirstRow extends GSHeaderComposite {

			public static class GSFirstRowFirstCell extends GSDiv {

			}

			public static class GSFirstRowContentCell extends GSDiv {

			}
		}

		public static class GSRowCell extends GSHeaderComposite {

			public static class GSContentRowFirstCell extends GSHeaderComposite {

			}

			public static class GSContentRowContentCell extends GSHeaderComposite {

			}

		}
	}

	@ReactorDependencies({ GSLabel00.class, GSLabel10.class, GSLabel01.class, GSLabel11.class })
	public static class GSLabeledTable extends GSTable {

		@Parent(GSFirstRowFirstCell.class)
		public static class GSLabel00 extends GSLabelDisplayer {

		}

		@Parent(GSFirstRowContentCell.class)
		public static class GSLabel10 extends GSLabelDisplayer {

		}

		@Parent(GSContentRowFirstCell.class)
		public static class GSLabel01 extends GSLabelDisplayer {

		}

		@Parent(GSContentRowContentCell.class)
		public static class GSLabel11 extends GSLabelDisplayer {

		}
	}
}
