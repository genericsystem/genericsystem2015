package org.genericsystem.reactor.composite.table;

import java.util.Arrays;
import java.util.stream.Collectors;

import org.genericsystem.common.Generic;
import org.genericsystem.reactor.CompositeModel;
import org.genericsystem.reactor.CompositeModel.ObservableListExtractor;
import org.genericsystem.reactor.HtmlElement;
import org.genericsystem.reactor.composite.CompositeSectionHtmlTemplate.TitleCompositeSectionHtmlTemplate;
import org.genericsystem.reactor.composite.table.InstanceRowHtmlTemplate.InstanceRowHtml;
import org.genericsystem.reactor.html.HtmlH1;

import javafx.collections.FXCollections;

public abstract class TypeTableHtmlTemplate<M extends CompositeModel, COMPONENT extends TypeTableHtmlTemplate<M, COMPONENT>>
		extends TitleCompositeSectionHtmlTemplate<M, COMPONENT> {

	private ObservableListExtractor attributesExtractor = ObservableListExtractor.ATTRIBUTES;

	public TypeTableHtmlTemplate(HtmlElement<?, ?, ?> parent) {
		super(parent);
		addStyleClass("gstable");
		setObservableListExtractor(ObservableListExtractor.INSTANCES);
	}

	public ObservableListExtractor getAttributesExtractor() {
		return attributesExtractor;
	}

	@SuppressWarnings("unchecked")
	public COMPONENT setAttributesExtractor(ObservableListExtractor attributesExtractor) {
		this.attributesExtractor = attributesExtractor;
		return (COMPONENT) this;
	}

	public COMPONENT setAttributesExtractor(Class<?>... classes) {
		return this.setAttributesExtractor(
				gs -> FXCollections.observableArrayList(Arrays.stream(classes).map(gs[0].getRoot()::<Generic> find).collect(Collectors.toList())));
	}

	@Override
	protected void initSubChildren(HtmlSection<CompositeModel> parentSection) {
		new InstanceRowHtml<>(parentSection);
	}

	public static class TypeTableHtml<M extends CompositeModel> extends TypeTableHtmlTemplate<M, TypeTableHtml<M>> {
		public TypeTableHtml(HtmlElement<?, ?, ?> parent) {
			super(parent);
		}
	}

	public static class ColumnTitleTypeTableHtml<M extends CompositeModel> extends TypeTableHtmlTemplate<M, ColumnTitleTypeTableHtml<M>> {
		public ColumnTitleTypeTableHtml(HtmlElement<?, ?, ?> parent) {
			super(parent);
		}

		@Override
		protected void initChildren() {
			new HtmlH1<M>(new HtmlSection<M>(this).addStyleClass("gsrow").addStyleClass("gstitlerow")).bindText(CompositeModel::getString);
			new HtmlH1<M>(new HtmlSection<M>(this).addStyleClass("gsrow").addStyleClass("gstitlerow")).bindText(CompositeModel::getString);
			HtmlSection<CompositeModel> htmlSection = new HtmlSection<CompositeModel>(this).addStyleClass("gscell").addStyleClass("gstitlecell");
			htmlSection.forEach(g -> getStringExtractor().apply(g), gs -> getObservableListExtractor().apply(gs),
					(gs, constructor) -> getModelConstructor().build(gs, constructor)).addStyleClass("gscell");
			initSubChildren(htmlSection);
		}
	}

}
