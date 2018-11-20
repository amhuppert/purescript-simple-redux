"use strict";

var Maybe = require('../Data.Maybe');

exports.newQueue = function() {
    var start = 0;
    var end = 0;
    var itemCount = 0;
    var queue = new Array(20);
    var size = function() {
        return itemCount;
    };
    var availableCapacity = function() {
        return queue.length - itemCount;
    };
    var enqueue = function(item) {
        if (availableCapacity() === 0) {
            var newQueue = new Array(queue.length * 2);
            for (var i = 0; i < itemCount; i++) {
                newQueue[i] = queue[(start+i) % queue.length];
            }
            start = 0;
            end = itemCount;
            queue = newQueue;
        }

        queue[end] = item;
        end = (end + 1) % queue.length;
        itemCount++;
    };
    var enqueueAll = function(items) {
        for (var i = 0; i < items.length; i++) {
            enqueue(items[i]);
        }
    };
    var dequeue = function() {
        if (itemCount > 0) {
            var head = queue[start];
            delete queue[start];
            start = (start + 1) % queue.length;
            itemCount--;
            return Maybe.Just.create(head);
        } else {
            return Maybe.Nothing.value;
        }
    };
    var toArray = function() {
        var result = new Array(itemCount);
        for (var i = 0; i < itemCount; i++) {
            var item = queue[(start+i) % queue.length];
            result[i] = item;
        };
        return result;
    };
    return {
        size: size,
        enqueue: enqueue,
        enqueueAll: enqueueAll,
        dequeue: dequeue,
        toArray: toArray
    };
};

exports.size = function(mq) {
    return function() {
        return mq.size();
    };
};

exports.enqueue = function(mq) {
    return function(item) {
        return function() {
            mq.enqueue(item);
        };
    };
};

exports.enqueueAll = function(mq) {
    return function(items) {
        return function() {
            mq.enqueueAll(items);
        };
    };
};

exports.dequeue = function(mq) {
    return function() {
        return mq.dequeue();
    };
};

exports.toArray = function(mq) {
    return function() {
        return mq.toArray();
    };
};
