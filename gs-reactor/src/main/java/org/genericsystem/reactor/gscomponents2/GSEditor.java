package org.genericsystem.reactor.gscomponents2;

import org.genericsystem.reactor.modelproperties.ComponentsDefaults;
import org.genericsystem.reactor.modelproperties.ConvertedValueDefaults;
import org.genericsystem.reactor.modelproperties.SelectionDefaults;

import org.genericsystem.reactor.htmltag.HtmlH2;
import org.genericsystem.reactor.htmltag.HtmlLabel;
import org.genericsystem.reactor.htmltag.HtmlLabel.GSLabelDisplayer;

import java.io.Serializable;
import java.util.List;
import java.util.stream.Collectors;

import org.genericsystem.api.core.ApiStatics;
import org.genericsystem.api.core.exceptions.RollbackException;
import org.genericsystem.common.Generic;
import org.genericsystem.defaults.tools.BindingsTools;
import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.annotations.BindText;
import org.genericsystem.reactor.annotations.Children;
import org.genericsystem.reactor.annotations.ForEach;
import org.genericsystem.reactor.annotations.Select;
import org.genericsystem.reactor.annotations.SelectModel;
import org.genericsystem.reactor.annotations.SetStringExtractor;
import org.genericsystem.reactor.annotations.Style;
import org.genericsystem.reactor.annotations.Style.FlexDirectionStyle;
import org.genericsystem.reactor.annotations.Style.ReverseFlexDirection;
import org.genericsystem.reactor.gscomponents.FlexDirection;
import org.genericsystem.reactor.gscomponents.GSCheckBoxWithValue;
import org.genericsystem.reactor.gscomponents.GSCheckBoxWithValue.GSCheckBoxEditor;
import org.genericsystem.reactor.gscomponents.GSDiv;
import org.genericsystem.reactor.gscomponents.GSInputTextWithConversion;
import org.genericsystem.reactor.gscomponents.DivWithTitle.GSTitleDiv;
import org.genericsystem.reactor.gscomponents.GSInputTextWithConversion.GSInputTextEditorWithConversion;
import org.genericsystem.reactor.gscomponents.GSSelect.GSSelectWithEmptyEntry;
import org.genericsystem.reactor.gscomponents.GSSelect.InstanceCompositeSelect;
import org.genericsystem.reactor.gscomponents2.GSCellDiv.CenteredFlexDiv;
import org.genericsystem.reactor.gscomponents2.GSCellDiv.GSActionLink;
import org.genericsystem.reactor.gscomponents2.GSCellDiv.GSComponentEditorDiv;
import org.genericsystem.reactor.gscomponents2.GSCellDiv.GSSubcellEditorDiv;
import org.genericsystem.reactor.gscomponents2.GSCellDiv.GSTitleLineCellDiv;
import org.genericsystem.reactor.gscomponents2.GSCellDiv.SubcellEditorContainerDiv;
import org.genericsystem.reactor.gscomponents2.GSCellDiv.WrappedColumnDiv;
import org.genericsystem.reactor.gscomponents2.GSEditor.EditorContent;
import org.genericsystem.reactor.gscomponents2.GSEditor.EditorContent.InstanceEdition;
import org.genericsystem.reactor.gscomponents2.GSEditor.EditorContent.InstanceEdition.InstanceAttributeEditor;
import org.genericsystem.reactor.gscomponents2.GSEditor.EditorContent.InstanceEdition.InstanceAttributeEditor.AttributeEditionColumn;
import org.genericsystem.reactor.gscomponents2.GSEditor.EditorContent.InstanceEdition.InstanceAttributeEditor.AttributeEditionColumn.SubcellAdder;
import org.genericsystem.reactor.gscomponents2.GSEditor.EditorContent.InstanceEdition.InstanceAttributeEditor.AttributeEditionColumn.SubcellAdder.BooleanHolderAdder;
import org.genericsystem.reactor.gscomponents2.GSEditor.EditorContent.InstanceEdition.InstanceAttributeEditor.AttributeEditionColumn.SubcellAdder.BooleanHolderAdder.BooleanHolderAdditionLink;
import org.genericsystem.reactor.gscomponents2.GSEditor.EditorContent.InstanceEdition.InstanceAttributeEditor.AttributeEditionColumn.SubcellAdder.BooleanHolderAdder.CheckboxContainerAddDiv;
import org.genericsystem.reactor.gscomponents2.GSEditor.EditorContent.InstanceEdition.InstanceAttributeEditor.AttributeEditionColumn.SubcellAdder.BooleanHolderAdder.CheckboxContainerAddDiv.BooleanHolderAdderInput;
import org.genericsystem.reactor.gscomponents2.GSEditor.EditorContent.InstanceEdition.InstanceAttributeEditor.AttributeEditionColumn.SubcellAdder.HolderAdder;
import org.genericsystem.reactor.gscomponents2.GSEditor.EditorContent.InstanceEdition.InstanceAttributeEditor.AttributeEditionColumn.SubcellAdder.HolderAdder.HolderAdderInput;
import org.genericsystem.reactor.gscomponents2.GSEditor.EditorContent.InstanceEdition.InstanceAttributeEditor.AttributeEditionColumn.SubcellAdder.HolderAdder.HolderAdditionLink;
import org.genericsystem.reactor.gscomponents2.GSEditor.EditorContent.InstanceEdition.InstanceAttributeEditor.AttributeEditionColumn.SubcellAdder.LinkAdder;
import org.genericsystem.reactor.gscomponents2.GSEditor.EditorContent.InstanceEdition.InstanceAttributeEditor.AttributeEditionColumn.SubcellAdder.LinkAdder.ComponentAdder;
import org.genericsystem.reactor.gscomponents2.GSEditor.EditorContent.InstanceEdition.InstanceAttributeEditor.AttributeEditionColumn.SubcellAdder.LinkAdder.ComponentAdder.ComponentAdderSelect;
import org.genericsystem.reactor.gscomponents2.GSEditor.EditorContent.InstanceEdition.InstanceAttributeEditor.AttributeEditionColumn.SubcellEditor;
import org.genericsystem.reactor.gscomponents2.GSEditor.EditorContent.InstanceEdition.InstanceAttributeEditor.AttributeEditionColumn.SubcellEditor.BooleanHolderEditor;
import org.genericsystem.reactor.gscomponents2.GSEditor.EditorContent.InstanceEdition.InstanceAttributeEditor.AttributeEditionColumn.SubcellEditor.BooleanHolderEditor.CheckboxContainerDiv;
import org.genericsystem.reactor.gscomponents2.GSEditor.EditorContent.InstanceEdition.InstanceAttributeEditor.AttributeEditionColumn.SubcellEditor.HolderEditor;
import org.genericsystem.reactor.gscomponents2.GSEditor.EditorContent.InstanceEdition.InstanceAttributeEditor.AttributeEditionColumn.SubcellEditor.HolderEditor.HolderEditorInput;
import org.genericsystem.reactor.gscomponents2.GSEditor.EditorContent.InstanceEdition.InstanceAttributeEditor.AttributeEditionColumn.SubcellEditor.LinkEditor;
import org.genericsystem.reactor.gscomponents2.GSEditor.EditorContent.InstanceEdition.InstanceAttributeEditor.AttributeEditionColumn.SubcellEditor.LinkEditor.ComponentEditor;
import org.genericsystem.reactor.gscomponents2.GSEditor.EditorContent.InstanceEdition.InstanceAttributeEditor.AttributeEditionColumn.SubcellEditor.LinkEditor.ComponentEditor.DirectRelationComponentEditor;
import org.genericsystem.reactor.gscomponents2.GSEditor.EditorContent.InstanceEdition.InstanceAttributeEditor.AttributeEditionColumn.SubcellEditor.LinkEditor.ComponentEditor.ReversedRelationDisplayer;
import org.genericsystem.reactor.gscomponents2.GSEditor.EditorContent.InstanceEdition.InstanceAttributeEditor.AttributeEditionColumn.SubcellEditor.RemovalLink;
import org.genericsystem.reactor.gscomponents2.GSEditor.EditorContent.InstanceEdition.InstanceAttributeEditor.MultiCheckbox;
import org.genericsystem.reactor.gscomponents2.GSEditor.EditorContent.InstanceEdition.InstanceAttributeEditor.MultiCheckbox.CheckboxLabel;
import org.genericsystem.reactor.gscomponents2.GSEditor.EditorContent.InstanceEdition.InstanceAttributeEditor.MultiCheckbox.CheckboxLabel.Checkbox;
import org.genericsystem.reactor.gscomponents2.GSEditor.EditorContent.InstanceEdition.InstanceNameEditorDiv;
import org.genericsystem.reactor.gscomponents2.GSEditor.EditorContent.InstanceEdition.InstanceNameEditorDiv.InstanceNameEditor;
import org.genericsystem.reactor.gscomponents2.GSEditor.EditorContent.LinkTitles;
import org.genericsystem.reactor.gscomponents2.GSEditor.EditorContent.LinkTitles.InstanceType;
import org.genericsystem.reactor.gscomponents2.GSEditor.EditorContent.LinkTitles.TypeAttribute;
import org.genericsystem.reactor.gscomponents2.GSEditor.EditorContent.LinkTitles.TypeAttribute.AttributeName;
import org.genericsystem.reactor.gscomponents2.GSEditor.EditorContent.LinkTitles.TypeAttribute.RelationName;
import org.genericsystem.reactor.gscomponents2.GSEditor.EditorContent.LinkTitles.TypeAttribute.RelationName.ComponentName;
import org.genericsystem.reactor.gscomponents2.GSEditor.EditorTitle;
import org.genericsystem.reactor.gscomponents2.GSEditor.EditorTitle.EditorTitleContent;
import org.genericsystem.reactor.model.ObservableListExtractor;
import org.genericsystem.reactor.model.ObservableListExtractor.ATTRIBUTES_OF_INSTANCES;
import org.genericsystem.reactor.model.ObservableListExtractor.OTHER_COMPONENTS_2;
import org.genericsystem.reactor.model.ObservableModelSelector;
import org.genericsystem.reactor.model.ObservableValueSelector;
import org.genericsystem.reactor.model.ObservableValueSelector.RELATION_SELECTOR;
import org.genericsystem.reactor.model.ObservableValueSelector.STRICT_ATTRIBUTE_SELECTOR;
import org.genericsystem.reactor.model.StringExtractor;

import javafx.beans.binding.Bindings;
import javafx.beans.property.Property;
import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;

@Children({ EditorTitle.class, EditorContent.class })
@Style(name = "flex", value = "1")
@FlexDirectionStyle(path = EditorContent.class, value = FlexDirection.ROW)
public class GSEditor extends GSDiv implements SelectionDefaults {

	@FlexDirectionStyle(path = EditorContent.class, value = FlexDirection.COLUMN)
	public static class HorizontalGSEditor extends GSEditor {
	}

	// Main title.
	@Children(EditorTitleContent.class)
	public static class EditorTitle extends GSTitleDiv {

		@BindText
		@SetStringExtractor(StringExtractor.TYPE_INSTANCE_EXTRACTOR.class)
		public static class EditorTitleContent extends HtmlH2 {
		}
	}

	// Content.
	@Style(name = "flex", value = "1")
	@Style(name = "height", value = "100%")
	@Children({ LinkTitles.class, InstanceEdition.class })
	public static class EditorContent extends GSDiv {
		// Line/column with the names of the attributes and components of relations.
		@ReverseFlexDirection
		@Style(name = "flex", value = "0.3")
		@Children({ InstanceType.class, TypeAttribute.class })
		public static class LinkTitles extends GSDiv {

			@Select(ObservableValueSelector.TYPE_SELECTOR.class)
			@Children(GSLabelDisplayer.class)
			public static class InstanceType extends GSTitleLineCellDiv {
			}

			@ForEach(ATTRIBUTES_OF_INSTANCES.class)
			@Style(name = "flex", value = "1")
			@FlexDirectionStyle(FlexDirection.ROW)
			@Children({ AttributeName.class, RelationName.class })
			public static class TypeAttribute extends GSDiv {

				@Select(STRICT_ATTRIBUTE_SELECTOR.class)
				@Children(GSLabelDisplayer.class)
				public static class AttributeName extends GSTitleLineCellDiv {
				}

				@Select(RELATION_SELECTOR.class)
				@Style(name = "flex", value = "1")
				@FlexDirectionStyle(FlexDirection.ROW)
				@Children(ComponentName.class)
				public static class RelationName extends GSDiv {

					@ForEach(OTHER_COMPONENTS_2.class)
					@Children(GSLabelDisplayer.class)
					public static class ComponentName extends GSTitleLineCellDiv {
					}
				}
			}
		}

		// Edition itself.
		@Style(name = "flex", value = "1")
		@ReverseFlexDirection
		@Children({ InstanceNameEditorDiv.class, InstanceAttributeEditor.class })
		public static class InstanceEdition extends GSDiv {

			// Edition of the name of the instance.
			@Children(InstanceNameEditor.class)
			public static class InstanceNameEditorDiv extends GSSubcellEditorDiv {

				@Style(name = "flex", value = "1")
				@Style(name = "height", value = "100%")
				@Style(name = "width", value = "100%")
				public static class InstanceNameEditor extends GSInputTextEditorWithConversion {
				}
			}

			// Edition of the holders/links.
			@ForEach(ObservableListExtractor.ATTRIBUTES_OF_INSTANCES.class)
			@Children({ MultiCheckbox.class, AttributeEditionColumn.class })
			public static class InstanceAttributeEditor extends WrappedColumnDiv {

				// Multiple checkboxes : for binary relations without the singular constraint.
				@Select(ObservableValueSelector.MULTICHECKBOX_SELECTOR.class)
				@Children(CheckboxLabel.class)
				public static class MultiCheckbox extends WrappedColumnDiv {

					@Style(name = "flex", value = "1 0 auto")
					@Style(name = "justify-content", value = "center")
					@Style(name = "align-items", value = "center")
					@Style(name = "text-align", value = "center")
					@BindText
					@Children(Checkbox.class)
					public static class CheckboxLabel extends HtmlLabel {

						@Override
						public void init() {
							forEach(gs -> ObservableListExtractor.SUBINSTANCES.apply(ObservableListExtractor.COMPONENTS.apply(gs).filtered(g -> !g.equals(gs[2])).stream().toArray(Generic[]::new)));
							addPrefixBinding(model -> {
								if ("Color".equals(StringExtractor.SIMPLE_CLASS_EXTRACTOR.apply(model.getGeneric().getMeta())))
									addStyle(model, "background-color", getGenericStringProperty(model).getValue());
							});
						}

						@Style(name = "float", value = "left")
						@Style(name = "vertical-align", value = "middle")
						@Style(name = "margin", value = "4px")
						public static class Checkbox extends GSCheckBoxWithValue {

							@Override
							public void init() {
								initValueProperty(context -> context.getGenerics()[2].getLink(context.getGenerics()[1], context.getGeneric()) != null ? true : false);
								storeProperty("exists", context -> {
									ObservableValue<Boolean> exists = Bindings.createBooleanBinding(() -> context.getGenerics()[2].getObservableLink(context.getGenerics()[1], context.getGeneric()).getValue() != null ? true : false,
											context.getGenerics()[2].getObservableLink(context.getGenerics()[1], context.getGeneric()));
									exists.addListener((o, v, nva) -> {
										if (!context.isDestroyed())
											getConvertedValueProperty(context).setValue(nva);
									});
									return exists;
								});
								addConvertedValueChangeListener((context, nva) -> {
									if (Boolean.TRUE.equals(nva))
										context.getGenerics()[2].setHolder(context.getGenerics()[1], null, context.getGeneric());
									if (Boolean.FALSE.equals(nva)) {
										Generic link = context.getGenerics()[2].getLink(context.getGenerics()[1], context.getGeneric());
										if (link != null)
											link.remove();
									}
								});
							}
						}
					}
				}

				// Edition of other attributes.
				@Style(name = "flex", value = "1")
				@Style(name = "flex-wrap", value = "wrap")
				@Select(ObservableValueSelector.NON_MULTICHECKBOX_SELECTOR.class)
				@Children({ SubcellEditor.class, SubcellAdder.class })
				public static class AttributeEditionColumn extends GenericColumn {

					@ForEach(ObservableListExtractor.HOLDERS.class)
					@Children({ HolderEditor.class, BooleanHolderEditor.class, LinkEditor.class, RemovalLink.class })
					public static class SubcellEditor extends SubcellEditorContainerDiv {

						// Edition of non-boolean holders.
						@Select(ObservableValueSelector.LABEL_DISPLAYER.class)
						@Children(HolderEditorInput.class)
						public static class HolderEditor extends GSSubcellEditorDiv {

							@Style(name = "flex", value = "1")
							@Style(name = "height", value = "100%")
							@Style(name = "width", value = "100%")
							public static class HolderEditorInput extends GSInputTextEditorWithConversion {
							}

						}

						// Edition of boolean holders.
						@Select(ObservableValueSelector.CHECK_BOX_DISPLAYER.class)
						@Children(CheckboxContainerDiv.class)
						public static class BooleanHolderEditor extends GSSubcellEditorDiv {

							@Children(GSCheckBoxEditor.class)
							public static class CheckboxContainerDiv extends CenteredFlexDiv {
							}
						}

						// Edition of links.
						@Style(name = "flex", value = "1")
						@FlexDirectionStyle(FlexDirection.ROW)
						@Select(ObservableValueSelector.RELATION_SELECTOR.class)
						@Children(ComponentEditor.class)
						public static class LinkEditor extends GSDiv implements ComponentsDefaults {

							@Override
							public void init() {
								createComponentsListProperty();
							}

							@ForEach(ObservableListExtractor.OTHER_COMPONENTS_2.class)
							@Children({ DirectRelationComponentEditor.class, ReversedRelationDisplayer.class })
							public static class ComponentEditor extends GSComponentEditorDiv {

								@Style(name = "flex", value = "1")
								@Style(name = "height", value = "100%")
								@Style(name = "width", value = "100%")
								@Select(ObservableValueSelector.DIRECT_RELATION_SELECTOR.class)
								public static class DirectRelationComponentEditor extends InstanceCompositeSelect {

									@Override
									public void init() {
										addPostfixBinding(model -> {
											Property<List<Property<Context>>> selectedComponents = getComponentsProperty(model);
											if (selectedComponents != null)
												selectedComponents.getValue().add(getSelectionProperty(model));
										});
									}
								}

								@Select(ObservableValueSelector.REVERSED_RELATION_SELECTOR.class)
								public static class ReversedRelationDisplayer extends GSLabelDisplayer {
								}
							}
						}

						// Hyperlink to remove a holder/link. Displayed only if there is no required constraint on the given attribute/relation,
						// or there is a required constraint but there are at least two holders/links for the given attribute/relation.
						public static class RemovalLink extends GSActionLink {

							@Override
							public void init() {
								setText("×");
								bindAction(Context::remove);
								select__(context -> {
									ObservableList<Generic> holders = ObservableListExtractor.HOLDERS.apply(context.getParent().getGenerics());
									return BindingsTools.transmitSuccessiveInvalidations(
											Bindings.createObjectBinding(() -> (!context.getParent().getGeneric().isRequiredConstraintEnabled(ApiStatics.BASE_POSITION) && holders.size() == 1) || holders.size() > 1 ? context : null, holders));
								});
							}
						}

					}

					// To add a new holder/link if it’s possible.
					@SelectModel(ObservableModelSelector.HOLDER_ADDITION_ENABLED_SELECTOR.class)
					@Children({ HolderAdder.class, BooleanHolderAdder.class, LinkAdder.class })
					public static class SubcellAdder extends SubcellEditorContainerDiv {

						// Addition of non-boolean holders.
						@Select(ObservableValueSelector.LABEL_DISPLAYER_ATTRIBUTE.class)
						@Children({ HolderAdderInput.class, HolderAdditionLink.class })
						public static class HolderAdder extends GSSubcellEditorDiv {

							@Style(name = "flex", value = "1")
							@Style(name = "height", value = "100%")
							@Style(name = "width", value = "100%")
							public static class HolderAdderInput extends GSInputTextWithConversion {

								@Override
								public void init() {
									addConvertedValueChangeListener((model, nva) -> {
										if (nva != null)
											model.getGenerics()[1].addHolder(model.getGeneric(), nva);
									});
								}
							}

							public static class HolderAdditionLink extends AdditionLink {

								@Override
								public void init() {
									bindAction(context -> addHolder(context, find(HolderAdderInput.class)));
								}
							}
						}

						// Addition of boolean holders.
						@Select(ObservableValueSelector.CHECK_BOX_DISPLAYER_ATTRIBUTE.class)
						@Children({ CheckboxContainerAddDiv.class, BooleanHolderAdditionLink.class })
						public static class BooleanHolderAdder extends GSSubcellEditorDiv {

							@Children(BooleanHolderAdderInput.class)
							public static class CheckboxContainerAddDiv extends CenteredFlexDiv {
								public static class BooleanHolderAdderInput extends GSCheckBoxWithValue {

									@Override
									public void init() {
										addConvertedValueChangeListener((model, nva) -> {
											if (nva != null)
												model.getGenerics()[1].addHolder(model.getGeneric(), nva);
										});
									}
								}
							}

							public static class BooleanHolderAdditionLink extends AdditionLink {

								@Override
								public void init() {
									bindAction(context -> addHolder(context, find(BooleanHolderAdderInput.class)));
								}
							}
						}

						// Addition of links.
						@Style(name = "flex", value = "1")
						@FlexDirectionStyle(FlexDirection.ROW)
						@Select(ObservableValueSelector.RELATION_SELECTOR.class)
						@Children(ComponentAdder.class)
						public static class LinkAdder extends GSDiv implements ComponentsDefaults {

							@Override
							public void init() {
								createComponentsListProperty();
								addPostfixBinding(model -> {
									Property<List<Property<Context>>> selectedComponents = getComponentsProperty(model);
									ChangeListener<Context> listener = (o, v, nva) -> {
										List<Generic> selectedGenerics = selectedComponents.getValue().stream().filter(obs -> obs.getValue() != null).map(obs -> obs.getValue().getGeneric()).filter(gen -> gen != null).collect(Collectors.toList());
										if (selectedGenerics.size() + 1 == model.getGeneric().getComponents().size()) {
											selectedComponents.getValue().stream().forEach(sel -> sel.setValue(null));
											try {
												model.getGenerics()[1].setHolder(model.getGeneric(), null, selectedGenerics.stream().toArray(Generic[]::new));
											} catch (RollbackException e) {
												e.printStackTrace();
											}
										}
									};
									selectedComponents.getValue().forEach(component -> component.addListener(listener));
								});
							}

							@ForEach(ObservableListExtractor.OTHER_COMPONENTS_2.class)
							@Children(ComponentAdderSelect.class)
							public static class ComponentAdder extends GSComponentEditorDiv {

								@Style(name = "flex", value = "1")
								@Style(name = "height", value = "100%")
								@Style(name = "width", value = "100%")
								@Select(ObservableValueSelector.DIRECT_RELATION_SELECTOR.class)
								public static class ComponentAdderSelect extends GSSelectWithEmptyEntry {

									@Override
									public void init() {
										addPostfixBinding(model -> {
											Property<List<Property<Context>>> selectedComponents = getComponentsProperty(model);
											if (selectedComponents != null)
												selectedComponents.getValue().add(getSelectionProperty(model));
										});
									}
								}
							}
						}
					}
				}

				// Hyperlink to create the holder. Displayed only for holders, not for links.
				public static class AdditionLink extends GSActionLink {

					@Override
					public void init() {
						setText("+");
					}

					protected void addHolder(Context context, ConvertedValueDefaults tag) {
						Property<Serializable> observable = tag.getConvertedValueProperty(context);
						if (observable.getValue() != null) {
							Serializable newValue = observable.getValue();
							observable.setValue(null);
							context.getGenerics()[1].addHolder(context.getGeneric(), newValue);
						}
					}
				}
			}
		}
	}
}
