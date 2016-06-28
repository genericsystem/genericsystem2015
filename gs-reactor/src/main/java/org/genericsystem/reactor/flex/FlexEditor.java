package org.genericsystem.reactor.flex;

import org.genericsystem.reactor.Element;
import org.genericsystem.reactor.annotation.InstanceColorize;
import org.genericsystem.reactor.html.HtmlButton;
import org.genericsystem.reactor.html.HtmlH1;
import org.genericsystem.reactor.html.HtmlHyperLink;
import org.genericsystem.reactor.html.HtmlInputText;
import org.genericsystem.reactor.html.HtmlLabel;
import org.genericsystem.reactor.model.CompositeModel;
import org.genericsystem.reactor.model.CompositeModel.StringExtractor;

import com.sun.xml.internal.ws.api.ComponentFeature.Target;

import org.genericsystem.reactor.model.InputCompositeModel;
import org.genericsystem.reactor.model.ObservableListExtractor;

/**
 * @author Nicolas Feybesse
 *
 * @param <M>
 */
public class FlexEditor extends CompositeFlexElement<InputCompositeModel> {

	public FlexEditor(Element<?> parent) {
		this(parent, FlexTag.SECTION);
	}

	public FlexEditor(Element<?> parent, FlexTag tag) {
		super(parent, tag, FlexDirection.COLUMN);
	}

	@Override
	protected void header() {
		new FlexElement<CompositeModel>(this, FlexTag.HEADER, FlexDirection.ROW) {
			{
				addStyle("background-color", "#ffa500");
				addStyle("margin-right", "1px");
				addStyle("margin-bottom", "1px");
				addStyle("color", "red");
				addStyle("justify-content", "center");
				new HtmlH1<CompositeModel>(this) {
					{
						bindText(CompositeModel::getString);
					}
				};
			}
		};
	}


	
	@Override
	protected void sections() {

			new CompositeFlexElement<CompositeModel>(this, FlexTag.SECTION, FlexDirection.ROW) {
				{
					forEach(StringExtractor.SIMPLE_CLASS_EXTRACTOR, ObservableListExtractor.ATTRIBUTES_OF_TYPE2, CompositeModel::new);
				}

				@Override
				protected void header() {

							new FlexElement<CompositeModel>(this, FlexTag.SECTION, FlexDirection.COLUMN) {
								{
									addStyle("flex", "1");
									addStyle("background-color", "#dda5e2");
									addStyle("margin-right", "1px");
									addStyle("margin-bottom", "1px");
									select_(gs -> gs[0].getComponents().size() < 2 ? gs[0] : null);
									new HtmlLabel<CompositeModel>(this).bindText(CompositeModel::getString);
								}
							};
							new FlexElement<CompositeModel>(this, FlexTag.SECTION, FlexDirection.COLUMN) {
								{
									addStyle("flex", "1");
									addPrefixBinding(modelContext -> modelContext.getObservableStyles(this).put("background-color",
											modelContext.<CompositeModel> getModel().getGeneric().getMeta().getAnnotation(InstanceColorize.class) != null ? modelContext.<CompositeModel> getModel().getString().getValue() : "#dda5e2"));
									addStyle("margin-right", "1px");
									addStyle("margin-bottom", "1px");
									forEach(StringExtractor.SIMPLE_CLASS_EXTRACTOR, gs -> ObservableListExtractor.COMPONENTS.apply(gs).filtered(g -> !g.equals(gs[1].getMeta())));									new HtmlLabel<CompositeModel>(this).bindText(CompositeModel::getString);
								}
							};
						
					}
				
			
			@Override
			protected void sections() {

				forEach(StringExtractor.SIMPLE_CLASS_EXTRACTOR, ObservableListExtractor.ATTRIBUTES_OF_INSTANCES);
				new FlexElement<CompositeModel>(this, FlexTag.SECTION, FlexDirection.ROW) {
					{
						addStyle("flex", "1");
						forEach(StringExtractor.SIMPLE_CLASS_EXTRACTOR, ObservableListExtractor.HOLDERS);
						new FlexElement<CompositeModel>(this, FlexTag.SECTION, FlexDirection.COLUMN) {
							{
								addStyle("flex", "1");
								addStyle("background-color", "#dda5e2");
								addStyle("margin-right", "1px");
								addStyle("margin-bottom", "1px");
								select_(gs -> gs[0].getComponents().size() < 2 ? gs[0] : null);
								new HtmlLabel<CompositeModel>(this).bindText(CompositeModel::getString);
							}
						};
						new FlexElement<CompositeModel>(this, FlexTag.SECTION, FlexDirection.COLUMN) {
							{
								addStyle("flex", "1");
								addPrefixBinding(modelContext -> modelContext.getObservableStyles(this).put("background-color",
										modelContext.<CompositeModel> getModel().getGeneric().getMeta().getAnnotation(InstanceColorize.class) != null ? modelContext.<CompositeModel> getModel().getString().getValue() : "#dda5e2"));
								addStyle("margin-right", "1px");
								addStyle("margin-bottom", "1px");
								forEach(StringExtractor.SIMPLE_CLASS_EXTRACTOR, gs -> ObservableListExtractor.COMPONENTS.apply(gs).filtered(g -> !g.equals(gs[2])));
								new HtmlLabel<CompositeModel>(this).bindText(CompositeModel::getString);
							}
						};
					}
				};
			}
			
			@Override
			protected void footer() {
				// TODO Auto-generated method stub
				
			}
			
			
			};


			
			
	}
	}


