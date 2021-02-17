#version 130

in      vec2  point;
in      vec4  color;

uniform vec2  screen = vec2 (500);

out     vec4  vColor;



mat4 translated (vec2 pos) {
  return mat4 ( vec4 ( 1, 0, 0, 0 )
              , vec4 ( 0, 1, 0, 0 )
              , vec4 ( 0, 0, 0, 0 )
              , vec4 ( pos , 0, 1 )
              );
}

mat4 scaled (vec2 sca) {
  return mat4 ( vec4 ( sca.x, 0    , 0, 0 )
              , vec4 ( 0    , sca.y, 0, 0 )
              , vec4 ( 0    , 0    , 0, 0 )
              , vec4 ( 0    , 0    , 0, 1 )
              );
}

void main () {

  vColor = color;

  gl_Position = translated (vec2 (-1))
              * scaled (2 / screen)
              * vec4 (point, 0, 1);
}
