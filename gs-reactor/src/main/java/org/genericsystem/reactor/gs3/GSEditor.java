package org.genericsystem.reactor.gs3;

import java.io.Serializable;
import java.util.List;
import java.util.stream.Collectors;

import javafx.beans.binding.Bindings;
import javafx.beans.property.Property;
import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;

import org.genericsystem.api.core.ApiStatics;
import org.genericsystem.api.core.exceptions.RollbackException;
import org.genericsystem.common.Generic;
import org.genericsystem.defaults.tools.BindingsTools;
import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.RootTagImpl;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.annotations.Parent;
import org.genericsystem.reactor.annotations.ReactorDependencies;
import org.genericsystem.reactor.gs.GSCheckBoxWithValue;
import org.genericsystem.reactor.gs.GSCheckBoxWithValue.GSCheckBoxEditor;
import org.genericsystem.reactor.gs.GSDiv;
import org.genericsystem.reactor.gs.GSInputTextWithConversion;
import org.genericsystem.reactor.gs.GSInputTextWithConversion.GSInputTextEditorWithConversion;
import org.genericsystem.reactor.gs.GSSelect.CompositeSelectWithEmptyEntry;
import org.genericsystem.reactor.gs.GSSelect.InstanceCompositeSelect;
import org.genericsystem.reactor.gs3.FlexStyle.RowFlexStyle;
import org.genericsystem.reactor.gs3.GSEditor.EditorContent.InstanceEdition.InstanceNameEditorDiv.InstanceAttributeEditor.MultiCheckbox.AttributeEditionColumn.SubcellAdder.HolderAdder.BooleanHolderAdder.CheckboxContainerAddDiv.BooleanHolderAdderInput;
import org.genericsystem.reactor.gs3.GSEditor.EditorContent.InstanceEdition.InstanceNameEditorDiv.InstanceAttributeEditor.MultiCheckbox.AttributeEditionColumn.SubcellAdder.HolderAdder.BooleanHolderAdder.CheckboxContainerAddDiv.BooleanHolderAdditionLink;
import org.genericsystem.reactor.gs3.GSEditor.EditorContent.InstanceEdition.InstanceNameEditorDiv.InstanceAttributeEditor.MultiCheckbox.AttributeEditionColumn.SubcellAdder.HolderAdder.HolderAdderInput;
import org.genericsystem.reactor.gs3.GSEditor.EditorContent.InstanceEdition.InstanceNameEditorDiv.InstanceAttributeEditor.MultiCheckbox.AttributeEditionColumn.SubcellAdder.HolderAdder.HolderAdditionLink;
import org.genericsystem.reactor.gs3.GSEditor.EditorContent.InstanceEdition.InstanceNameEditorDiv.InstanceAttributeEditor.MultiCheckbox.AttributeEditionColumn.SubcellAdder.HolderAdder.LinkAdder.ComponentAdder.ComponentAdderSelect;
import org.genericsystem.reactor.gs3.GSEditor.EditorContent.InstanceEdition.InstanceNameEditorDiv.InstanceAttributeEditor.MultiCheckbox.AttributeEditionColumn.SubcellEditor.BooleanHolderEditor.CheckboxContainerDiv.BooleanHolderEditorInput;
import org.genericsystem.reactor.gs3.GSEditor.EditorContent.InstanceEdition.InstanceNameEditorDiv.InstanceAttributeEditor.MultiCheckbox.AttributeEditionColumn.SubcellEditor.HolderEditor.HolderEditorInput;
import org.genericsystem.reactor.gs3.GSEditor.EditorContent.InstanceEdition.InstanceNameEditorDiv.InstanceAttributeEditor.MultiCheckbox.AttributeEditionColumn.SubcellEditor.LinkEditor.ComponentEditor.DirectRelationComponentEditor;
import org.genericsystem.reactor.gs3.GSEditor.EditorContent.InstanceEdition.InstanceNameEditorDiv.InstanceAttributeEditor.MultiCheckbox.AttributeEditionColumn.SubcellEditor.LinkEditor.ComponentEditor.ReversedRelationDisplayer;
import org.genericsystem.reactor.gs3.GSEditor.EditorContent.InstanceEdition.InstanceNameEditorDiv.InstanceAttributeEditor.MultiCheckbox.AttributeEditionColumn.SubcellEditor.RemovalLink;
import org.genericsystem.reactor.gs3.GSEditor.EditorContent.InstanceEdition.InstanceNameEditorDiv.InstanceAttributeEditor.MultiCheckbox.CheckboxLabel.Checkbox;
import org.genericsystem.reactor.gs3.GSEditor.EditorContent.InstanceEdition.InstanceNameEditorDiv.InstanceNameEditor;
import org.genericsystem.reactor.gs3.GSEditor.EditorContent.LinkTitles.InstanceType;
import org.genericsystem.reactor.gs3.GSEditor.EditorContent.LinkTitles.InstanceTypeAttribute;
import org.genericsystem.reactor.gs3.GSEditor.EditorTitle.EditorTitleContent;
import org.genericsystem.reactor.gs3.GSTable.TitleRow.TypeAttribute;
import org.genericsystem.reactor.gs3.GSTable.TitleRow.TypeName;
import org.genericsystem.reactor.gstag.HtmlH2;
import org.genericsystem.reactor.gstag.HtmlHyperLink;
import org.genericsystem.reactor.gstag.HtmlLabel;
import org.genericsystem.reactor.gstag.HtmlLabel.GSLabelDisplayer;
import org.genericsystem.reactor.model.ObservableListExtractor;
import org.genericsystem.reactor.model.StringExtractor;
import org.genericsystem.reactor.modelproperties.ComponentsDefaults;
import org.genericsystem.reactor.modelproperties.ConvertedValueDefaults;
import org.genericsystem.reactor.modelproperties.SelectionDefaults;

@ReactorDependencies({ EditorTitleContent.class, InstanceType.class, InstanceTypeAttribute.class, InstanceNameEditor.class, Checkbox.class, ReversedRelationDisplayer.class, DirectRelationComponentEditor.class, BooleanHolderEditorInput.class,
		HolderEditorInput.class, RemovalLink.class, BooleanHolderAdderInput.class, BooleanHolderAdditionLink.class, HolderAdderInput.class, HolderAdditionLink.class, ComponentAdderSelect.class })
public class GSEditor extends RootTagImpl implements RowFlexStyle {

	public GSEditor(Tag parent) {
		super(parent, GSEditor.class);
	}

	// No automatic enclosing class parent for this GSTable extention
	public static class HorizontalGSEditor extends GSEditor implements ColumnFlexStyle, SelectionDefaults {

		public HorizontalGSEditor(Tag parent) {
			super(parent);
		}

		@Override
		public void style() {
			ColumnFlexStyle.super.style();
		}
	}

	@Parent(GSEditor.class)
	// Main title.
	public static class EditorTitle extends GSDiv implements TitleStyle {

		@Parent(EditorTitle.class)
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
	public static class EditorContent extends GSDiv implements EditorContentStyle {
		// Line/column with the names of the attributes and components of relations.
		@Parent(EditorContent.class)
		public static class LinkTitles extends GSDiv implements LinkTitlesStyle {
			@Parent(LinkTitles.class)
			public static class InstanceType extends TypeName {

				@Override
				public void init() {
					select(gs -> gs[1]);
				}
			}

			@Parent(LinkTitles.class)
			public static class InstanceTypeAttribute extends TypeAttribute {

				@Override
				public void init() {
					forEach(ObservableListExtractor.ATTRIBUTES_OF_INSTANCES);
				}
			}
		}

		// Edition itself.
		@Parent(EditorContent.class)
		public static class InstanceEdition extends GSDiv implements ReversedFlexStyle {

			// Edition of the name of the instance.
			@Parent(InstanceEdition.class)
			public static class InstanceNameEditorDiv extends GSDiv implements SubCellEditorStyle {

				@Parent(InstanceNameEditorDiv.class)
				public static class InstanceNameEditor extends GSInputTextEditorWithConversion implements FullSizeStyle {
				}

				// Edition of the holders/links.
				@Parent(InstanceEdition.class)
				public static class InstanceAttributeEditor extends GSDiv implements AttributeEditorStyle {

					@Override
					public void init() {
						forEach(ObservableListExtractor.ATTRIBUTES_OF_INSTANCES);
					}

					// Multiple checkboxes : for binary relations without the singular constraint.
					@Parent(InstanceAttributeEditor.class)
					public static class MultiCheckbox extends GSDiv implements MultiCheckboxStyle {

						@Override
						public void init() {
							select(gs -> gs[0].getComponents().size() == 2 && !gs[0].isSingularConstraintEnabled(gs[0].getComponents().indexOf(gs[2])) ? gs[0] : null);
						}

						@Parent(MultiCheckbox.class)
						public static class CheckboxLabel extends HtmlLabel implements CheckboxLabelStyle {

							@Override
							public void init() {
								bindText();
								forEach(gs -> ObservableListExtractor.SUBINSTANCES.apply(ObservableListExtractor.COMPONENTS.apply(gs).filtered(g -> !g.equals(gs[2])).stream().toArray(Generic[]::new)));
							}

							@Parent(CheckboxLabel.class)
							public static class Checkbox extends GSCheckBoxWithValue implements CheckboxStyle {

								@Override
								public void init() {
									initValueProperty(context -> context.getGenerics()[2].getLink(context.getGenerics()[1], context.getGeneric()) != null ? true : false);
									storeProperty(
											"exists",
											context -> {
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

						// Edition of other attributes.
						@Parent(InstanceAttributeEditor.class)
						public static class AttributeEditionColumn extends GenericColumn implements AttributeEditionColumnStyle {

							@Override
							public void init() {
								select(gs -> gs[0].getComponents().size() != 2 || gs[0].isSingularConstraintEnabled(gs[0].getComponents().indexOf(gs[2])) ? gs[0] : null);
							}

							@Parent(AttributeEditionColumn.class)
							public static class SubcellEditor extends GSDiv implements SubcellEditorContainerStyle {

								@Override
								public void init() {
									forEach(ObservableListExtractor.HOLDERS);
								}

								// Edition of non-boolean holders.
								@Parent(SubcellEditor.class)
								public static class HolderEditor extends GSDiv implements SubCellEditorStyle {

									@Override
									public void init() {
										select(gs -> gs[0].getComponents().size() < 2 && !Boolean.class.equals(gs[0].getInstanceValueClassConstraint()) ? gs[0] : null);
									}

									@Parent(HolderEditor.class)
									public static class HolderEditorInput extends GSInputTextEditorWithConversion implements FullSizeStyle {
									}

								}

								// Edition of boolean holders.
								@Parent(SubcellEditor.class)
								public static class BooleanHolderEditor extends GSDiv implements SubCellEditorStyle {

									@Override
									public void init() {
										select(gs -> gs[0].getComponents().size() < 2 && Boolean.class.equals(gs[0].getInstanceValueClassConstraint()) ? gs[0] : null);
									}

									@Parent(BooleanHolderEditor.class)
									public static class CheckboxContainerDiv extends GSDiv implements CenteredFlex {
										@Parent(CheckboxContainerDiv.class)
										public static class BooleanHolderEditorInput extends GSCheckBoxEditor {
										}
									}

								}

								// Edition of links.
								@Parent(SubcellEditor.class)
								public static class LinkEditor extends GSDiv implements RowFlexStyle, ComponentsDefaults {

									@Override
									public void init() {
										select(gs -> gs[0].getComponents().size() >= 2 ? gs[0] : null);
										createComponentsListProperty();
									}

									@Parent(LinkEditor.class)
									public static class ComponentEditor extends GSDiv implements ComponentEditorStyle {

										@Override
										public void init() {
											forEach((ObservableListExtractor) gs -> ObservableListExtractor.COMPONENTS.apply(gs).filtered(g -> !g.equals(gs[2])));
										}

										// TODO: Finish decomposition of InstanceCompositeSelect.
										@Parent(ComponentEditor.class)
										public static class DirectRelationComponentEditor extends InstanceCompositeSelect implements FullSizeStyle {

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

										@Parent(ComponentEditor.class)
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
								@Parent(SubcellEditor.class)
								public static class RemovalLink extends HtmlHyperLink implements ActionLinkStyle {

									@Override
									public void init() {
										setText("×");
										bindAction(Context::remove);
										select__(context -> {
											ObservableList<Generic> holders = ObservableListExtractor.HOLDERS.apply(context.getParent().getGenerics());
											return BindingsTools.transmitSuccessiveInvalidations(Bindings.createObjectBinding(
													() -> (!context.getParent().getGeneric().isRequiredConstraintEnabled(ApiStatics.BASE_POSITION) && holders.size() == 1) || holders.size() > 1 ? context : null, holders));
										});
									}
								}

							}

							// To add a new holder/link if it’s possible.
							@Parent(AttributeEditionColumn.class)
							public static class SubcellAdder extends GSDiv implements SubcellEditorContainerStyle {

								@Override
								public void init() {
									select__(model -> {
										ObservableList<Generic> holders = ObservableListExtractor.HOLDERS.apply(model.getGenerics());
										return Bindings.createObjectBinding(() -> holders.isEmpty() || (model.getGeneric().getComponents().size() < 2 && !model.getGeneric().isPropertyConstraintEnabled())
												|| (model.getGeneric().getComponents().size() >= 2 && !model.getGeneric().isSingularConstraintEnabled(ApiStatics.BASE_POSITION)) ? model : null, ObservableListExtractor.HOLDERS.apply(model.getGenerics()));
									});
								}

								// Addition of non-boolean holders.
								@Parent(SubcellAdder.class)
								public static class HolderAdder extends GSDiv implements SubCellEditorStyle {

									@Override
									public void init() {
										select(gs -> gs[0].getComponents().size() < 2 && !Boolean.class.equals(gs[0].getInstanceValueClassConstraint()) ? gs[0] : null);
									}

									@Parent(HolderAdder.class)
									public static class HolderAdderInput extends GSInputTextWithConversion implements FullSizeStyle {

										@Override
										public void init() {
											addConvertedValueChangeListener((model, nva) -> {
												if (nva != null)
													model.getGenerics()[1].addHolder(model.getGeneric(), nva);
											});
										}
									}

									@Parent(HolderAdder.class)
									public static class HolderAdditionLink extends AdditionLink {

										@Override
										public void postfix() {
											bindAction(context -> addHolder(context, (ConvertedValueDefaults) find(HolderAdderInput.class)));
										}
									}

									// Addition of boolean holders.
									@Parent(SubcellAdder.class)
									public static class BooleanHolderAdder extends GSDiv implements SubCellEditorStyle {

										@Override
										public void init() {
											select(gs -> gs[0].getComponents().size() < 2 && Boolean.class.equals(gs[0].getInstanceValueClassConstraint()) ? gs[0] : null);
										}

										@Parent(BooleanHolderAdder.class)
										public static class CheckboxContainerAddDiv extends GSDiv implements CenteredFlex {
											@Parent(CheckboxContainerAddDiv.class)
											public static class BooleanHolderAdderInput extends GSCheckBoxWithValue {

												@Override
												public void init() {
													addConvertedValueChangeListener((model, nva) -> {
														if (nva != null)
															model.getGenerics()[1].addHolder(model.getGeneric(), nva);
													});
												}
											}

											@Parent(BooleanHolderAdder.class)
											public static class BooleanHolderAdditionLink extends AdditionLink {

												@Override
												public void postfix() {
													bindAction(context -> addHolder(context, (ConvertedValueDefaults) find(BooleanHolderAdderInput.class)));
												}
											}
										}
									}

									// Addition of links.
									@Parent(SubcellAdder.class)
									public static class LinkAdder extends GSDiv implements RowFlexStyle, ComponentsDefaults {

										@Override
										public void init() {
											select(gs -> gs[0].getComponents().size() >= 2 ? gs[0] : null);
											createComponentsListProperty();
											addPostfixBinding(model -> {
												Property<List<Property<Context>>> selectedComponents = getComponentsProperty(model);
												ChangeListener<Context> listener = (o, v, nva) -> {
													List<Generic> selectedGenerics = selectedComponents.getValue().stream().filter(obs -> obs.getValue() != null).map(obs -> obs.getValue().getGeneric()).filter(gen -> gen != null)
															.collect(Collectors.toList());
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

										@Parent(LinkAdder.class)
										public static class ComponentAdder extends GSDiv implements ComponentEditorStyle {

											@Override
											public void init() {
												forEach((ObservableListExtractor) gs -> ObservableListExtractor.COMPONENTS.apply(gs).filtered(g -> !g.equals(gs[2])));
											}

											// TODO: Finish decomposition of CompositeSelectWithEmptyEntry.
											@Parent(ComponentAdder.class)
											public static class ComponentAdderSelect extends CompositeSelectWithEmptyEntry implements FullSizeStyle {

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

						}

						// Hyperlink to create the holder. Displayed only for holders, not for links.
						public static class AdditionLink extends HtmlHyperLink implements ActionLinkStyle {

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
	}
}
