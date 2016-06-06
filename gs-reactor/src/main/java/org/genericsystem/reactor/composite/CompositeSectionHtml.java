package org.genericsystem.reactor.composite;

import org.genericsystem.reactor.HtmlElement;
import org.genericsystem.reactor.composite.CompositeModel.ModelConstructor;
import org.genericsystem.reactor.composite.CompositeModel.ObservableListExtractor;
import org.genericsystem.reactor.composite.CompositeModel.StringExtractor;
import org.genericsystem.reactor.html.HtmlH1;
import org.genericsystem.reactor.html.HtmlLabel;
import org.genericsystem.reactor.html.HtmlSection;

public abstract class CompositeSectionHtml<M extends CompositeModel> extends HtmlSection<M> {

	private StringExtractor stringExtractor = StringExtractor.SIMPLE_CLASS_EXTRACTOR;
	private ObservableListExtractor observableListExtractor = ObservableListExtractor.INSTANCES;
	private ModelConstructor<CompositeModel> modelConstructor = CompositeModel::new;

	public CompositeSectionHtml(HtmlElement<?, ?> parent) {
		super(parent);
		// this.bindStyles(CompositeModel::getFlexStyles);
		addStyle("display", "flex");
		addStyle("flex-direction", "row");
		addStyle("flex-wrap", "nowrap");
		addStyle("justify-content", "center");
		addStyle("background-color", "#ffa500");
		HtmlSection<CompositeModel> subSection = new HtmlSection<CompositeModel>(this) {
			{
				addStyle("flex", "1");
				forEach(g -> getStringExtractor().apply(g), gs -> getObservableListExtractor().apply(gs), (gs, stringExtractor) -> getModelConstructor().build(gs, stringExtractor));
				new HtmlLabel<CompositeModel>(this).bindText(CompositeModel::getString);
			}
		};
		new HtmlLabel<CompositeModel>(subSection).bindText(CompositeModel::getString);
	}

	public StringExtractor getStringExtractor() {
		return stringExtractor;
	}

	public void setStringExtractor(StringExtractor stringExtractor) {
		this.stringExtractor = stringExtractor;
	}

	public ObservableListExtractor getObservableListExtractor() {
		return observableListExtractor;
	}

	public void setObservableListExtractor(ObservableListExtractor observableListExtractor) {
		this.observableListExtractor = observableListExtractor;
	}

	public ModelConstructor<CompositeModel> getModelConstructor() {
		return modelConstructor;
	}

	public void setModelConstructor(ModelConstructor<CompositeModel> modelConstructor) {
		this.modelConstructor = modelConstructor;
	}

	public static abstract class TitleCompositeSectionHtml<M extends CompositeModel> extends HtmlSection<M> {
		private StringExtractor stringExtractor = StringExtractor.SIMPLE_CLASS_EXTRACTOR;
		private ObservableListExtractor observableListExtractor = ObservableListExtractor.INSTANCES;
		private ModelConstructor<CompositeModel> modelConstructor = CompositeModel::new;

		public TitleCompositeSectionHtml(HtmlElement<?, ?> parent) {
			super(parent);
			new HtmlSection<CompositeModel>(this) {
				{
					addStyle("display", "flex");
					addStyle("flex-directionflex", "row");
					addStyle("flex-wrap", "nowrap");
					addStyle("justify-content", "center");
					addStyle("background-color", "#ffa500");
					new HtmlH1<CompositeModel>(this) {
						{
							bindText(CompositeModel::getString);
						}
					};
				};
			};
			new HtmlSection<CompositeModel>(this) {
				{
					addStyle("flex", "1");
					forEach(g -> getStringExtractor().apply(g), gs -> getObservableListExtractor().apply(gs), (gs, stringExtractor) -> getModelConstructor().build(gs, stringExtractor));
					new HtmlLabel<CompositeModel>(this).bindText(CompositeModel::getString);
				}
			};
		}

		public StringExtractor getStringExtractor() {
			return stringExtractor;
		}

		public void setStringExtractor(StringExtractor stringExtractor) {
			this.stringExtractor = stringExtractor;
		}

		public ObservableListExtractor getObservableListExtractor() {
			return observableListExtractor;
		}

		public void setObservableListExtractor(ObservableListExtractor observableListExtractor) {
			this.observableListExtractor = observableListExtractor;
		}

		public ModelConstructor<CompositeModel> getModelConstructor() {
			return modelConstructor;
		}

		public void setModelConstructor(ModelConstructor<CompositeModel> modelConstructor) {
			this.modelConstructor = modelConstructor;
		}
	}

}
