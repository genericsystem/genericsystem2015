package org.genericsystem.reactor.model;

import org.genericsystem.common.Root;
import org.genericsystem.common.Generic;

/**
 * @author Nicolas Feybesse
 *
 */
public class EngineModel extends GenericModel {

	public EngineModel(Root engine) {
		super(new Generic[] { engine }, StringExtractor.SIMPLE_CLASS_EXTRACTOR);
	}
}
