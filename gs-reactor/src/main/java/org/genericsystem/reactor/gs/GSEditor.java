package org.genericsystem.reactor.gs;

import org.genericsystem.api.core.ApiStatics;
import org.genericsystem.reactor.gs.GSLinks.LinkAdder;
import org.genericsystem.reactor.gs.GSLinks.LinkEditor;
import org.genericsystem.reactor.gs.GSLinks.LinkEditorWithRemoval;
import org.genericsystem.reactor.gs.GSLinks.LinkTitleDisplayer;
import org.genericsystem.reactor.gstag.GSH1;
import org.genericsystem.reactor.model.GenericModel;
import org.genericsystem.reactor.model.ObservableListExtractor;
import org.genericsystem.reactor.model.StringExtractor;

/**
 * @author Nicolas Feybesse
 *
 * @param <M>
 */
public class GSEditor extends GSComposite {

	public GSEditor(GSTag parent) {
		this(parent, FlexDirection.COLUMN);
	}

	public GSEditor(GSTag parent, FlexDirection flexDirection) {
		super(parent, flexDirection);
		addStyle("flex", "1");
	}

	@Override
	protected void header() {
		new GSSection(this, GSEditor.this.getReverseDirection()) {
			{
				addStyle("flex", "0.3");
				addStyle("background-color", "#ffa500");
				addStyle("margin-right", "1px");
				addStyle("margin-bottom", "1px");
				addStyle("color", "red");
				addStyle("justify-content", "center");
				addStyle("align-items", "center");
				new GSH1(this) {
					{
						bindText(GenericModel::getString);
					}
				};
			}
		};
	}

	@Override
	protected void sections() {

		new GSComposite(this, GSEditor.this.getReverseDirection()) {
			{
				addStyle("flex", "1");
			}

			@Override
			protected void header() {
				new GSSection(this, GSEditor.this.getDirection()) {
					{
						addStyle("flex", "0.3");
						new LinkTitleDisplayer(this, gs -> ObservableListExtractor.COMPONENTS.apply(gs).filtered(g -> !g.equals(gs[1].getMeta()))) {
							{
								addStyle("flex", "1");
								addStyle("overflow", "hidden");
								select(StringExtractor.SIMPLE_CLASS_EXTRACTOR, gs -> gs[0].getMeta());
							}
						};
						new LinkTitleDisplayer(this, gs -> ObservableListExtractor.COMPONENTS.apply(gs).filtered(g -> !g.equals(gs[1].getMeta()))) {
							{
								addStyle("flex", "1");
								addStyle("overflow", "hidden");
								forEach(StringExtractor.SIMPLE_CLASS_EXTRACTOR, ObservableListExtractor.ATTRIBUTES_OF_INSTANCES);
							}
						};
					}
				};
			}

			@Override
			protected void sections() {
				new GSSection(this, GSEditor.this.getDirection()) {
					{
						addStyle("flex", "1");
						new GSSection(this, FlexDirection.COLUMN) {
							{
								addStyle("flex", "1");
								addStyle("overflow", "hidden");
								new LinkEditor(this) {
									{
										addStyle("flex", "1");
										select(StringExtractor.SIMPLE_CLASS_EXTRACTOR, gs -> gs[0]);
									}
								};
							}
						};
						new GSSection(this, FlexDirection.COLUMN) {
							{
								forEach(StringExtractor.SIMPLE_CLASS_EXTRACTOR, ObservableListExtractor.ATTRIBUTES_OF_INSTANCES);
								addStyle("flex", "1");
								addStyle("overflow", "hidden");
								new LinkEditorWithRemoval(this) {
									{
										addStyle("flex", "1");
										forEach(StringExtractor.SIMPLE_CLASS_EXTRACTOR, ObservableListExtractor.HOLDERS);
									}
								};
								new LinkAdder(this) {
									{
										addStyle("flex", "1");
										select(StringExtractor.SIMPLE_CLASS_EXTRACTOR, gs -> ObservableListExtractor.HOLDERS.apply(gs).isEmpty() || (gs[0].getComponents().size() < 2 && !gs[0].isPropertyConstraintEnabled())
												|| (gs[0].getComponents().size() >= 2 && !gs[0].isSingularConstraintEnabled(ApiStatics.BASE_POSITION)) ? gs[0] : null);
									}
								};
							}
						};
					}
				};
			};
		};
	}
}
