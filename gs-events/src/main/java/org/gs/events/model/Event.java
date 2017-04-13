package org.gs.events.model;

import org.genericsystem.api.core.annotations.Components;
import org.genericsystem.api.core.annotations.SystemGeneric;
import org.genericsystem.geography.model.Place;

@SystemGeneric
@Components({ Place.class, Date.class, Date.class })
public class Event {

}
