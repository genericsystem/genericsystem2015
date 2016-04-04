package org.genericsystem.examplejsf.phase;

import javax.faces.event.PhaseEvent;
import javax.faces.event.PhaseId;
import javax.faces.event.PhaseListener;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class MyPhaseListener implements PhaseListener {

	private static final Logger log = LoggerFactory.getLogger(MyPhaseListener.class);

	@Override
	public PhaseId getPhaseId() {
		return PhaseId.ANY_PHASE;
	}

	@Override
	public void beforePhase(PhaseEvent event) {
		log.info("before - " + event.getPhaseId().toString());
	}

	@Override
	public void afterPhase(PhaseEvent event) {
		log.info("after - " + event.getPhaseId().toString());

	}

}