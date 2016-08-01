package org.genericsystem.reactor.gs;

import org.genericsystem.api.core.ApiStatics;
import org.genericsystem.reactor.gs.GSBooleanHolderEditor.GSBooleanHolderAdder;
import org.genericsystem.reactor.gs.GSBooleanHolderEditor.GSBooleanHolderEditorWithRemoval;
import org.genericsystem.reactor.gs.GSCellDisplayer.GSCellEditor;
import org.genericsystem.reactor.gs.GSCellDisplayer.InstanceLinkTitleDisplayer;
import org.genericsystem.reactor.gs.GSHolderEditor.GSHolderAdder;
import org.genericsystem.reactor.gs.GSHolderEditor.GSHolderEditorWithRemoval;
import org.genericsystem.reactor.gs.GSLinkEditor.GSLinkAdder;
import org.genericsystem.reactor.gs.GSLinkEditor.GSLinkEditorWithRemoval;
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
						new InstanceLinkTitleDisplayer(this) {
							{
								select(StringExtractor.SIMPLE_CLASS_EXTRACTOR, gs -> gs[0].getMeta());
							}
						};
						new InstanceLinkTitleDisplayer(this) {
							{
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
								new GSCellEditor(this, GSHolderEditor::new, GSBooleanHolderEditor::new, GSLinkEditor::new) {
									{
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
								new GSCellEditor(this, GSHolderEditorWithRemoval::new, GSBooleanHolderEditorWithRemoval::new, GSLinkEditorWithRemoval::new) {
									{
										forEach(StringExtractor.SIMPLE_CLASS_EXTRACTOR, ObservableListExtractor.HOLDERS);
									}
								};
								new GSCellEditor(this, GSHolderAdder::new, GSBooleanHolderAdder::new, GSLinkAdder::new) {
									{
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
