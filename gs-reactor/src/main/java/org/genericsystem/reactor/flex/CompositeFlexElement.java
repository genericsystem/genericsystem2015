package org.genericsystem.reactor.flex;

import org.genericsystem.reactor.Element;
import org.genericsystem.reactor.composite.Composite;
import org.genericsystem.reactor.composite.CompositeModel;
import org.genericsystem.reactor.html.HtmlH1;
import org.genericsystem.reactor.html.HtmlLabel;

/**
 * @author Nicolas Feybesse
 *
 */
public class CompositeFlexElement<M extends CompositeModel> extends FlexElement<M> implements Composite<M> {

	public CompositeFlexElement(Element<?> parent, FlexTag tag) {
		this(parent, tag, FlexDirection.COLUMN);
	}

	public CompositeFlexElement(Element<?> parent, FlexTag tag, FlexDirection flexDirection) {
		super(parent, tag, flexDirection);
		header();
		sections();
		footer();
	}

	protected void header() {

	}

	protected void sections() {
		new FlexElement<CompositeModel>(this, FlexTag.SECTION, FlexDirection.ROW) {
			{
				forEach(g -> getStringExtractor().apply(g), gs -> getObservableListExtractor().apply(gs),
						(gs, extractor) -> getModelConstructor().build(gs, extractor));
				new HtmlLabel<CompositeModel>(this).bindText(CompositeModel::getString);
			}
		};
	}

	protected void footer() {
	}

	public static class TitleCompositeFlexElement<M extends CompositeModel> extends CompositeFlexElement<M> {

		public TitleCompositeFlexElement(Element<?> parent, FlexTag tag, FlexDirection flexDirection) {
			super(parent, tag, flexDirection);
		}

		public TitleCompositeFlexElement(Element<?> parent, FlexTag tag) {
			this(parent, tag, FlexDirection.COLUMN);
		}

		public TitleCompositeFlexElement(Element<?> parent, FlexDirection flexDirection) {
			this(parent, FlexTag.SECTION, flexDirection);
		}

		public TitleCompositeFlexElement(Element<?> parent) {
			this(parent, FlexTag.SECTION, FlexDirection.COLUMN);
		}

		@Override
		protected void header() {
			new FlexElement<CompositeModel>(this, FlexTag.HEADER, FlexDirection.ROW) {
				{
					addStyle("justify-content", "center");
					addStyle("background-color", "#ffa500");
					new HtmlH1<CompositeModel>(this) {
						{
							bindText(CompositeModel::getString);
						}
					};
				};
			};
		}
	}

	public static class ColorTitleCompositeFlexElement<M extends CompositeModel> extends TitleCompositeFlexElement<M> {

		public ColorTitleCompositeFlexElement(Element<?> parent, FlexTag tag, FlexDirection flexDirection) {
			super(parent, tag, flexDirection);
		}

		public ColorTitleCompositeFlexElement(Element<?> parent, FlexTag tag) {
			this(parent, tag, FlexDirection.COLUMN);
		}

		public ColorTitleCompositeFlexElement(Element<?> parent, FlexDirection flexDirection) {
			this(parent, FlexTag.SECTION, flexDirection);
		}

		public ColorTitleCompositeFlexElement(Element<?> parent) {
			this(parent, FlexTag.SECTION, FlexDirection.COLUMN);
		}

		@Override
		protected void sections() {
			new FlexElement<CompositeModel>(this, FlexTag.SECTION, FlexDirection.ROW) {
				{
					FlexElement<CompositeModel> row = this;
					bindStyle("background-color");
					forEach(g -> getStringExtractor().apply(g), gs -> getObservableListExtractor().apply(gs),
							(gs, extractor) -> new CompositeModel(gs, extractor) {
								{
									getStyleProperty(row, "background-color").setValue(getString().getValue());
								}
							});
					new HtmlLabel<CompositeModel>(this).bindText(CompositeModel::getString);
				}
			};
		}
	}
}
