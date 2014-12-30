'use strict'

function mVarImpl (x, f) {
    this.value = x;
    this.filled = f == null ? true : f;
    this.queues = { put: []
                  , read: []
                  , take: []
                  }
};

mVarImpl.prototype.enqueuePut = function(cb, x) {
    if (this.filled) {
        this.queues.put.push({cb: cb, value: x});
    } else {
        this.value = x;
        this.filled = true;
        cb();
        this.notifyPut();
    }
};

mVarImpl.prototype.enqueueRead = function(cb) {
    if (this.filled) {
        cb();
    } else {
        this.queues.read.push(cb);
    }
};

mVarImpl.prototype.enqueueTake = function(cb) {
    if (this.filled) {
        cb();
        this.filled = false;
        this.notifyTake();
    } else {
        this.queues.take.push(cb);
    }
};

mVarImpl.prototype.notifyPut = function() {
    this.queues.read.forEach(function(r) {
        r();
    });
    this.queues.read = [];
    if (this.queues.take.length > 0) {
        this.queues.take.shift()();
    }
};

mVarImpl.prototype.notifyTake = function() {
    if (this.queues.put.length > 0) {
        var p = this.queues.put.shift();
        this.value = p.value;
        this.filled = true;
        p.cb();
    }
};
