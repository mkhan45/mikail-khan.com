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
pixiApp.stage.x = pixiApp.screen.width / 2;
pixiApp.stage.y = pixiApp.screen.height / 2;

const top_edge = -pixiApp.screen.height / 2;
const left_edge = -pixiApp.screen.width / 2;

const radius = 2.5;
let circles = [];

{
    const offset = radius * 25;
    for (let i = 0; i < pixiApp.screen.width / offset; i += 1) {
        for (let j = 0; j < pixiApp.screen.height / offset; j += 1) {
            const x = left_edge + radius * 2 + i * offset;
            const y = top_edge + radius * 2 + j * offset;

            if (Math.abs(x) < 200 && Math.abs(y) < 175) {
                continue;
            }

            circles.push({x: x, y: y, vx: 0, vy: 0, start_x: x, start_y: y, mouse_interact: false})
        }
    }
}

let circle_geom = new PIXI.Graphics();
pixiApp.stage.addChild(circle_geom);
// circles.forEach(c => pixiApp.stage.addChild(c));

let mouse_x = 0;
let mouse_y = 0;

document.onmousemove = e => {
    mouse_x = e.clientX + left_edge;
    mouse_y = e.clientY + top_edge;
};

let tick = 0;

pixiApp.ticker.add(delta => {
    if (tick % 100 == 0) {
        console.log(delta);
    }
    tick += 1;

    // integrate and damping
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

        // a little bit of padding to make the lines look better
        if (dist_sqr > 11000) circle.mouse_interact = false;
        if (dist_sqr > 10000) return;

        circle.mouse_interact = true;

        let grav = Math.min(500 / Math.max(x_rad * x_rad + y_rad * y_rad, 1), 10);

        circle.vx += grav * Math.sign(x_rad) * delta;
        circle.vy += grav * Math.sign(y_rad) * delta;
    });

    // start point gravity
    circles.forEach(circle => {
        if (circle.mouse_interact) return;

        let x_rad = -(circle.x - circle.start_x);
        let y_rad = -(circle.y - circle.start_y);

        let dist_sqr = x_rad * x_rad + y_rad * y_rad;

        if (dist_sqr < 10) return;

        let grav = dist_sqr / 10000 + 0.1;

        circle.vx += grav * Math.sign(x_rad) * delta;
        circle.vy += grav * Math.sign(y_rad) * delta;
    });

    // render
    circle_geom.clear();
    circles.forEach(circle => {
        circle_geom.beginFill(0xffffff).drawCircle(circle.x, circle.y, radius).endFill();

        if (circle.mouse_interact) {
            let x_rad = (circle.x - mouse_x);
            let y_rad = (circle.y - mouse_y);
            let rad_sqr = x_rad * x_rad + y_rad * y_rad;

            circle_geom.lineStyle(rad_sqr / 15000, 0xffffff).moveTo(mouse_x, mouse_y).lineTo(circle.x, circle.y);
            circle_geom.position.set(0, 0);
        }
    });
});
