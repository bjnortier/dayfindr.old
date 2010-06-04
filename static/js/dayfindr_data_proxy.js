function MockState() {
    this.state = {};

}

function MockViewer(viewerId) {
    this.id = viewerId;
    this.getId = function() {
	return viewerId;
    }
}

function MockParticipant(id, displayname, thumbnail) {
    this.id = id;
    this.displayname = displayname;
    this.thumbnail = thumbnail;

    this.getDisplayName = function() {
	return this.displayname;
    }

    this.getThumbnailUrl = function() {
	return this.thumbnail;
    }
}

function MockState(hash) {
    this.hash = hash;

    this.getKeys = function() {
	var keys = [];
	for(i in this.hash) if (this.hash.hasOwnProperty(i)) {
	    keys.push(i);
	}
	return keys;
    }

    this.get = function (key, def) {
	var value = hash[key];
	if (value == undefined) {
	    return def;
	}
	return value;
    }

    this.submitDelta = function(delta) {
	var newKeys = getKeys(delta);
	for (var i = 0; i < newKeys.length; ++i) {
	    this.hash[newKeys[i]] = delta[newKeys[i]];
	}
	updateUI();
    }
}

function MockWave(viewerId, state) {

    this.viewer = new MockViewer(viewerId);
    this.state = new MockState(state);
    this.participants = { 'bjnortier@googlewave.com' : new MockParticipant('bjnortier@googlewave.com', 'Ben Nortier', 'http://example.com'),
                          'foo@googlewave.com' : new MockParticipant('foo@googlewave.com', 'Foo', 'http://example.com') };

    this.getViewer = function() {
	return this.viewer;
    }

    this.getState = function() {
	return this.state;
    }

    this.getParticipantById = function(id) {
	return this.participants[id];
    }

}

function htmlForParticipant(participantId) {
    return participantId;
}

var dayfindr_state = {};
/*dayfindr_state['bjnortier@googlewave.com'] = ['{"year" : 2010, "month" : 2, "day" : 1}', '{"year" : 2010, "month" : 3, "day" : 15}'];
  dayfindr_state['foo@googlewave.com'] = ['{"year" : 2010, "month" : 2, "day" : 28, "message" : "I\'m foo"}'];*/

var wave = new MockWave('bjnortier@googlewave.com', dayfindr_state);


