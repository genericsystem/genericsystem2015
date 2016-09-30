package org.genericsystem.reactor.az3;

import java.io.Serializable;
import java.util.List;
import java.util.stream.Collectors;

import org.genericsystem.api.core.ApiStatics;
import org.genericsystem.api.core.exceptions.RollbackException;
import org.genericsystem.common.Generic;
import org.genericsystem.defaults.tools.BindingsTools;
import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.annotations.ForEach;
import org.genericsystem.reactor.annotations.Parent;
import org.genericsystem.reactor.annotations.ReactorDependencies;
import org.genericsystem.reactor.annotations.Select;
import org.genericsystem.reactor.annotations.Styles.AlignItems;
import org.genericsystem.reactor.annotations.Styles.Flex;
import org.genericsystem.reactor.annotations.Styles.FlexDirectionStyle;
import org.genericsystem.reactor.annotations.Styles.FlexWrap;
import org.genericsystem.reactor.annotations.Styles.Height;
import org.genericsystem.reactor.annotations.Styles.JustifyContent;
import org.genericsystem.reactor.annotations.Styles.KeepFlexDirection;
import org.genericsystem.reactor.annotations.Styles.ReverseFlexDirection;
import org.genericsystem.reactor.annotations.Styles.Style;
import org.genericsystem.reactor.annotations.Styles.Width;
import org.genericsystem.reactor.az.FlexDirection;
import org.genericsystem.reactor.az.GSCheckBoxWithValue;
import org.genericsystem.reactor.az.GSCheckBoxWithValue.GSCheckBoxEditor;
import org.genericsystem.reactor.az.GSDiv;
import org.genericsystem.reactor.az.GSInputTextWithConversion;
import org.genericsystem.reactor.az.GSInputTextWithConversion.GSInputTextEditorWithConversion;
import org.genericsystem.reactor.az.GSSelect.CompositeSelectWithEmptyEntry;
import org.genericsystem.reactor.az.GSSelect.InstanceCompositeSelect;
import org.genericsystem.reactor.az3.GSCellDiv.CenteredFlexDiv;
import org.genericsystem.reactor.az3.GSCellDiv.GSActionLink;
import org.genericsystem.reactor.az3.GSCellDiv.GSComponentEditorDiv;
import org.genericsystem.reactor.az3.GSCellDiv.GSSubcellEditorDiv;
import org.genericsystem.reactor.az3.GSCellDiv.GSTitleLineCellDiv;
import org.genericsystem.reactor.az3.GSCellDiv.SubcellEditorContainerDiv;
import org.genericsystem.reactor.az3.GSCellDiv.WrappedColumnDiv;
import org.genericsystem.reactor.az3.GSEditor.EditorContent.InstanceEdition.InstanceAttributeEditor.AttributeEditionColumn.SubcellAdder.BooleanHolderAdder.BooleanHolderAdditionLink;
import org.genericsystem.reactor.az3.GSEditor.EditorContent.InstanceEdition.InstanceAttributeEditor.AttributeEditionColumn.SubcellAdder.BooleanHolderAdder.CheckboxContainerAddDiv.BooleanHolderAdderInput;
import org.genericsystem.reactor.az3.GSEditor.EditorContent.InstanceEdition.InstanceAttributeEditor.AttributeEditionColumn.SubcellAdder.HolderAdder.HolderAdderInput;
import org.genericsystem.reactor.az3.GSEditor.EditorContent.InstanceEdition.InstanceAttributeEditor.AttributeEditionColumn.SubcellAdder.HolderAdder.HolderAdditionLink;
import org.genericsystem.reactor.az3.GSEditor.EditorContent.InstanceEdition.InstanceAttributeEditor.AttributeEditionColumn.SubcellAdder.LinkAdder.ComponentAdder.ComponentAdderSelect;
import org.genericsystem.reactor.az3.GSEditor.EditorContent.InstanceEdition.InstanceAttributeEditor.AttributeEditionColumn.SubcellEditor.BooleanHolderEditor.CheckboxContainerDiv.BooleanHolderEditorInput;
import org.genericsystem.reactor.az3.GSEditor.EditorContent.InstanceEdition.InstanceAttributeEditor.AttributeEditionColumn.SubcellEditor.HolderEditor.HolderEditorInput;
import org.genericsystem.reactor.az3.GSEditor.EditorContent.InstanceEdition.InstanceAttributeEditor.AttributeEditionColumn.SubcellEditor.LinkEditor.ComponentEditor.DirectRelationComponentEditor;
import org.genericsystem.reactor.az3.GSEditor.EditorContent.InstanceEdition.InstanceAttributeEditor.AttributeEditionColumn.SubcellEditor.LinkEditor.ComponentEditor.ReversedRelationDisplayer;
import org.genericsystem.reactor.az3.GSEditor.EditorContent.InstanceEdition.InstanceAttributeEditor.AttributeEditionColumn.SubcellEditor.RemovalLink;
import org.genericsystem.reactor.az3.GSEditor.EditorContent.InstanceEdition.InstanceAttributeEditor.MultiCheckbox.CheckboxLabel.Checkbox;
import org.genericsystem.reactor.az3.GSEditor.EditorContent.InstanceEdition.InstanceNameEditorDiv.InstanceNameEditor;
import org.genericsystem.reactor.az3.GSEditor.EditorContent.LinkTitles.InstanceType;
import org.genericsystem.reactor.az3.GSEditor.EditorContent.LinkTitles.InstanceType.TypeNameDisplayer;
import org.genericsystem.reactor.az3.GSEditor.EditorContent.LinkTitles.TypeAttribute.AttributeName.AttributeNameDisplayer;
import org.genericsystem.reactor.az3.GSEditor.EditorContent.LinkTitles.TypeAttribute.RelationName.ComponentName.ComponentNameDisplayer;
import org.genericsystem.reactor.az3.GSEditor.EditorTitle.EditorTitleContent;
import org.genericsystem.reactor.az3.TitledDiv.GSTitleDiv;
import org.genericsystem.reactor.gstag.HtmlH2;
import org.genericsystem.reactor.gstag.HtmlLabel.GSLabelDisplayer;
import org.genericsystem.reactor.model.ObservableListExtractor;
import org.genericsystem.reactor.model.ObservableListExtractor.ATTRIBUTES_OF_INSTANCES;
import org.genericsystem.reactor.model.ObservableListExtractor.OTHER_COMPONENTS_2;
import org.genericsystem.reactor.model.ObservableValueSelector.RELATION_SELECTOR;
import org.genericsystem.reactor.model.ObservableValueSelector.STRICT_ATTRIBUTE_SELECTOR;
import org.genericsystem.reactor.model.StringExtractor;
import org.genericsystem.reactor.modelproperties.ConvertedValueDefaults;
import org.genericsystem.reactor.modelproperties.SelectionDefaults;

import javafx.beans.binding.Bindings;
import javafx.beans.property.Property;
import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;

@ReactorDependencies({ EditorTitleContent.class, InstanceType.class, TypeNameDisplayer.class, AttributeNameDisplayer.class, ComponentNameDisplayer.class, InstanceNameEditor.class, Checkbox.class, ReversedRelationDisplayer.class,
		DirectRelationComponentEditor.class, BooleanHolderEditorInput.class, HolderEditorInput.class, RemovalLink.class, BooleanHolderAdderInput.class, BooleanHolderAdditionLink.class, HolderAdderInput.class, HolderAdditionLink.class,
		ComponentAdderSelect.class })
@Flex("1")
@FlexDirectionStyle(FlexDirection.ROW)
public class GSEditor extends GSDiv {

	public GSEditor() {
		super();
	}

	public GSEditor(Tag parent) {
		super(parent);
	}

	// No automatic enclosing class parent for this GSEditor extension
	@FlexDirectionStyle(FlexDirection.COLUMN)
	public static class HorizontalGSEditor extends GSEditor implements SelectionDefaults {

		public HorizontalGSEditor(Tag parent) {
			super(parent);
		}
	}

	// Main title.
	public static class EditorTitle extends GSTitleDiv {

		public static class EditorTitleContent extends HtmlH2 {

			@Override
			public void init() {
				setStringExtractor(StringExtractor.TYPE_INSTANCE_EXTRACTOR);
				bindText();
			}
		}
	}

	// Content.
	@Parent(GSEditor.class)
	@KeepFlexDirection
	@Flex("1")
	@Style(name = "height", value = "100%")
	public static class EditorContent extends GSDiv {
		// Line/column with the names of the attributes and components of relations.
		@ReverseFlexDirection
		@Flex("0.3")
		public static class LinkTitles extends GSDiv {

			public static class InstanceType extends GSTitleLineCellDiv {

				@Override
				public void init() {
					select(gs -> gs[1]);
				}

				public static class TypeNameDisplayer extends GSLabelDisplayer {

				}
			}

			@ForEach(ATTRIBUTES_OF_INSTANCES.class)
			@Flex("1")
			@FlexDirectionStyle(FlexDirection.ROW)
			public static class TypeAttribute extends GSDiv {

				@Select(STRICT_ATTRIBUTE_SELECTOR.class)
				public static class AttributeName extends GSTitleLineCellDiv {

					public static class AttributeNameDisplayer extends GSLabelDisplayer {

					}
				}

				@Select(RELATION_SELECTOR.class)
				@Flex("1")
				@FlexDirectionStyle(FlexDirection.ROW)
				public static class RelationName extends GSDiv {

					@ForEach(OTHER_COMPONENTS_2.class)
					public static class ComponentName extends GSTitleLineCellDiv {

						public static class ComponentNameDisplayer extends GSLabelDisplayer {
						}
					}
				}
			}
		}

		// Edition itself.
		@Flex("1")
		@ReverseFlexDirection
		public static class InstanceEdition extends GSDiv {

			// Edition of the name of the instance.
			public static class InstanceNameEditorDiv extends GSSubcellEditorDiv {

				@Flex("1")
				@Height("100%")
				@Width("100%")
				public static class InstanceNameEditor extends GSInputTextEditorWithConversion {
				}
			}

			// Edition of the holders/links.
			public static class InstanceAttributeEditor extends WrappedColumnDiv {

				@Override
				public void init() {
					forEach(ObservableListExtractor.ATTRIBUTES_OF_INSTANCES);
				}

				// Multiple checkboxes : for binary relations without the singular constraint.
				public static class MultiCheckbox extends WrappedColumnDiv {

					@Override
					public void init() {
						select(gs -> gs[0].getComponents().size() == 2 && !gs[0].isSingularConstraintEnabled(gs[0].getComponents().indexOf(gs[2])) ? gs[0] : null);
					}

					@Flex("1 0 auto")
					@JustifyContent("center")
					@AlignItems("center")
					@Style(name = "text-align", value = "center")
					public static class CheckboxLabel extends org.genericsystem.reactor.gstag.HtmlLabel {

						@Override
						public void init() {
							bindText();
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
				@Flex("1")
				@FlexWrap("wrap")
				public static class AttributeEditionColumn extends GenericColumn {

					@Override
					public void init() {
						select(gs -> gs[0].getComponents().size() != 2 || gs[0].isSingularConstraintEnabled(gs[0].getComponents().indexOf(gs[2])) ? gs[0] : null);
					}

					public static class SubcellEditor extends SubcellEditorContainerDiv {

						@Override
						public void init() {
							forEach(ObservableListExtractor.HOLDERS);
						}

						// Edition of non-boolean holders.
						public static class HolderEditor extends GSSubcellEditorDiv {

							@Override
							public void init() {
								select(gs -> gs[0].getComponents().size() < 2 && !Boolean.class.equals(gs[0].getInstanceValueClassConstraint()) ? gs[0] : null);
							}

							@Flex("1")
							@Height("100%")
							@Width("100%")
							public static class HolderEditorInput extends GSInputTextEditorWithConversion {
							}

						}

						// Edition of boolean holders.
						public static class BooleanHolderEditor extends GSSubcellEditorDiv {

							@Override
							public void init() {
								select(gs -> gs[0].getComponents().size() < 2 && Boolean.class.equals(gs[0].getInstanceValueClassConstraint()) ? gs[0] : null);
							}

							public static class CheckboxContainerDiv extends CenteredFlexDiv {
								@Parent(CheckboxContainerDiv.class)
								public static class BooleanHolderEditorInput extends GSCheckBoxEditor {
								}
							}

						}

						// Edition of links.
						@Flex("1")
						@FlexDirectionStyle(FlexDirection.ROW)
						public static class LinkEditor extends GSDiv implements org.genericsystem.reactor.modelproperties.ComponentsDefaults {

							@Override
							public void init() {
								select(gs -> gs[0].getComponents().size() >= 2 ? gs[0] : null);
								createComponentsListProperty();
							}

							public static class ComponentEditor extends GSComponentEditorDiv {

								@Override
								public void init() {
									forEach((ObservableListExtractor) gs -> ObservableListExtractor.COMPONENTS.apply(gs).filtered(g -> !g.equals(gs[2])));
								}

								// TODO: Finish decomposition of InstanceCompositeSelect.
								@Flex("1")
								@Height("100%")
								@Width("100%")
								public static class DirectRelationComponentEditor extends InstanceCompositeSelect {

									@Override
									public void init() {
										select(gs -> gs[1].isReferentialIntegrityEnabled(gs[1].getComponents().indexOf(gs[0])) ? gs[0] : null);
										addPostfixBinding(model -> {
											Property<List<Property<Context>>> selectedComponents = getComponentsProperty(model);
											if (selectedComponents != null)
												selectedComponents.getValue().add(getSelectionProperty(model));
										});
									}
								}

								public static class ReversedRelationDisplayer extends GSLabelDisplayer {

									@Override
									public void init() {
										select(gs -> !gs[1].isReferentialIntegrityEnabled(gs[1].getComponents().indexOf(gs[0])) && !gs[0].getLinks(gs[2]).isEmpty() ? gs[0] : null);
									}
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
					public static class SubcellAdder extends SubcellEditorContainerDiv {

						@Override
						public void init() {
							select__(model -> {
								ObservableList<Generic> holders = ObservableListExtractor.HOLDERS.apply(model.getGenerics());
								return Bindings
										.createObjectBinding(
												() -> holders.isEmpty() || (model.getGeneric().getComponents().size() < 2 && !model.getGeneric().isPropertyConstraintEnabled())
														|| (model.getGeneric().getComponents().size() >= 2 && !model.getGeneric().isSingularConstraintEnabled(ApiStatics.BASE_POSITION)) ? model : null,
												ObservableListExtractor.HOLDERS.apply(model.getGenerics()));
							});
						}

						// Addition of non-boolean holders.
						public static class HolderAdder extends GSSubcellEditorDiv {

							@Override
							public void init() {
								select(gs -> gs[0].getComponents().size() < 2 && !Boolean.class.equals(gs[0].getInstanceValueClassConstraint()) ? gs[0] : null);
							}

							@Flex("1")
							@Height("100%")
							@Width("100%")
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
								public void postfix() {
									bindAction(context -> addHolder(context, find(HolderAdderInput.class)));
								}
							}
						}

						// Addition of boolean holders.
						public static class BooleanHolderAdder extends GSSubcellEditorDiv {

							@Override
							public void init() {
								select(gs -> gs[0].getComponents().size() < 2 && Boolean.class.equals(gs[0].getInstanceValueClassConstraint()) ? gs[0] : null);
							}

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
								public void postfix() {
									bindAction(context -> addHolder(context, find(BooleanHolderAdderInput.class)));
								}
							}
						}

						// Addition of links.
						@Flex("1")
						@FlexDirectionStyle(FlexDirection.ROW)
						public static class LinkAdder extends GSDiv implements org.genericsystem.reactor.modelproperties.ComponentsDefaults {

							@Override
							public void init() {
								select(gs -> gs[0].getComponents().size() >= 2 ? gs[0] : null);
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

							public static class ComponentAdder extends GSComponentEditorDiv {

								@Override
								public void init() {
									forEach((ObservableListExtractor) gs -> ObservableListExtractor.COMPONENTS.apply(gs).filtered(g -> !g.equals(gs[2])));
								}

								// TODO: Finish decomposition of CompositeSelectWithEmptyEntry.
								@Parent(ComponentAdder.class)
								@Flex("1")
								@Height("100%")
								@Width("100%")
								public static class ComponentAdderSelect extends CompositeSelectWithEmptyEntry {

									@Override
									public void init() {
										select(gs -> gs[1].isReferentialIntegrityEnabled(gs[1].getComponents().indexOf(gs[0])) ? gs[0] : null);
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
