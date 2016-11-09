/*
 * File: Float32Array
 *
 * Elm interop to Float32Array for webgl.
 */
var _zinggi$elm_float32array$Native_Float32Array = function() {
    var fromTuple2 = function(t) {
      return new Float32Array([t._0, t._1]);
    };
    var fromTuple3 = function(t) {
      return new Float32Array([t._0, t._1, t._2]);
    };
    var fromTuple4 = function(t) {
      return new Float32Array([t._0, t._1, t._2, t._3]);
    };

    var fromTuple4x4 = function(t) {
        return new Float32Array([
            t._0._0, t._1._0, t._2._0, t._3._0,
            t._0._1, t._1._1, t._2._1, t._3._1,
            t._0._2, t._1._2, t._2._2, t._3._2,
            t._0._3, t._1._3, t._2._3, t._3._3
        ]);
    };


    return {
        fromTuple4x4: fromTuple4x4,
        fromTuple2: fromTuple2,
        fromTuple3: fromTuple3,
        fromTuple4: fromTuple4
    };

}();