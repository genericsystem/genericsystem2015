package org.genericsystem.example.reactor;

import org.genericsystem.common.AbstractRoot;
import org.genericsystem.common.Generic;
import org.genericsystem.reactor.composite.CompositeModel;

public class AppModel extends CompositeModel {

	public AppModel(AbstractRoot engine) {
		super(new Generic[] { engine }, StringExtractor.SIMPLE_CLASS_EXTRACTOR);
	}
}
