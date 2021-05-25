const pixiApp = new PIXI.Application({ 
    view: document.querySelector("#bg"),
    antialias: true,
    transparent: true,
    autoResize: true,
    resolution: devicePixelRatio,
});

function resize() {
    pixiApp.renderer.resize(window.innerWidth, window.innerHeight);
}
window.addEventListener('resize', resize);
resize();

// set coordinate system to start from center of screen
pixiApp.stage.x = pixiApp.screen.width / 2;
pixiApp.stage.y = pixiApp.screen.height / 2;

const top_edge = -pixiApp.screen.height / 2;
const left_edge = -pixiApp.screen.width / 2;


// initialize circles
const radius = 2.5;
const offset = radius * 25;
const num_rows = pixiApp.screen.height / offset + 1;
const num_cols = pixiApp.screen.width / offset;
let circles = new Array(Math.ceil(num_rows * num_cols));

for (let i = 0; i < num_cols; i += 1) {
    for (let j = 0; j < num_rows; j += 1) {
        const x = left_edge + radius * 2 + i * offset;
        const y = top_edge + radius * 2 + j * offset;

        circles.push({x: x, y: y, vx: 0, vy: 0, start_x: x, start_y: y, mouse_interact: false})
    }
}

// batch all the circles into one mesh for performance
let circle_geom = new PIXI.Graphics();
pixiApp.stage.addChild(circle_geom);

// get mouse position
let mouse_x = 0;
let mouse_y = 0;
document.onmousemove = e => {
    mouse_x = e.clientX + left_edge;
    mouse_y = e.clientY + top_edge;
};

// disable if prefers reduced motion
const mediaQuery = window.matchMedia('(prefers-reduced-motion: reduce)');
pixiApp.ticker.add(delta => {
    if (mediaQuery.matches) {
        circle_geom.clear();
        return;
    }

    // integration and damping
    circles.forEach(circle => {
        circle.x += circle.vx;
        circle.y += circle.vy;

        circle.vx *= 0.8;
        circle.vy *= 0.8;
    });

    // mouse gravity
    circles.forEach(circle => {
        let x_rad = (circle.x - mouse_x);
        let y_rad = (circle.y - mouse_y);

        let dist_sqr = x_rad * x_rad + y_rad * y_rad;

        // if the distance is too far don't compute for performance
	
        // there's a little bit of padding around actually changing the flag 
        // to make the lines look better
        circle.mouse_interact = !(dist_sqr > 11000);
        if (dist_sqr > 10000) return;


        let grav = Math.min(500 / Math.max(x_rad * x_rad + y_rad * y_rad, 1), 10);

        circle.vx += grav * Math.sign(x_rad) * delta;
        circle.vy += grav * Math.sign(y_rad) * delta;
    });

    // circle startpoint gravity
    circles.forEach(circle => {
        if (circle.mouse_interact) return;

        let x_rad = -(circle.x - circle.start_x);
        let y_rad = -(circle.y - circle.start_y);

        let dist_sqr = x_rad * x_rad + y_rad * y_rad;

        // ignore if the circle is near enough to the start point
        if (dist_sqr < 10) return;

        let grav = dist_sqr / 10000 + 0.1;

        circle.vx += grav * Math.sign(x_rad) * delta;
        circle.vy += grav * Math.sign(y_rad) * delta;
    });

    // render
    circle_geom.clear();
    circles.forEach(circle => {
        // draw circle
        circle_geom.beginFill(0xffffff).drawCircle(circle.x, circle.y, radius).endFill();

        // draw lines
        if (circle.mouse_interact) {
            let x_rad = (circle.x - mouse_x);
            let y_rad = (circle.y - mouse_y);
            let rad_sqr = x_rad * x_rad + y_rad * y_rad;

            circle_geom.lineStyle(rad_sqr / 15000, 0xffffff).moveTo(mouse_x, mouse_y).lineTo(circle.x, circle.y);
            circle_geom.position.set(0, 0);
        }
    });
});
