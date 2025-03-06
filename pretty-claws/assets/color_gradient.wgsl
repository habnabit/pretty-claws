#import bevy_render::color_operations::hsv_to_rgb
#import bevy_render::maths::PI_2
#import bevy_ui::ui_vertex_output::UiVertexOutput

@group(1) @binding(0) var<uniform> color1: vec4<f32>;
@group(1) @binding(1) var<uniform> color2: vec4<f32>;
@group(1) @binding(2) var<uniform> color3: vec4<f32>;
@group(1) @binding(5) var<uniform> color_selected: vec4<f32>;
@group(1) @binding(3) var slider_texture: texture_2d<f32>;
@group(1) @binding(4) var slider_sampler: sampler;
// @group(1) @binding(5) var<uniform> slider_ratio: f32;
@group(1) @binding(6) var<uniform> slider_position: f32;

// Based on https://en.wikipedia.org/wiki/HSL_and_HSV#HSL_to_HSV
fn hsl_to_hsv(hsl: vec3<f32>) -> vec3<f32> {
    // let value = lightness + saturation * lightness.min(1. - lightness);
    let v = hsl.z + hsl.y * min(hsl.z, 1.0 - hsl.z);
    // let saturation = if value == 0. {
    //     0.
    // } else {
    //     2. * (1. - (lightness / value))
    // };
    var s = 0.0;
    if v != 0.0 {
        s = 2.0 * (1.0 - (hsl.z / v));
    }
    return vec3(hsl.x / 360.0 * PI_2, s, v);
}

fn hsla2rgba(hsla: vec4<f32>) -> vec4<f32> {
    let color_rgb = hsv_to_rgb(hsl_to_hsv(hsla.xyz));
    return vec4<f32>(color_rgb, hsla.w);
}

@fragment
fn fragment(in: UiVertexOutput) -> @location(0) vec4<f32> {
    let slider_size = vec2<f32>(textureDimensions(slider_texture));
    let into_size = vec2<f32>(in.size);
    let slider_shrink = into_size.y / slider_size.y;
    let slider_ratio = (slider_size.x * slider_shrink) / into_size.x;
    let slider_potential_x = 1 - slider_ratio;
    let slider_start_at = slider_potential_x * slider_position;
    let slider_end_at = slider_start_at + slider_ratio;

    var textured: vec4<f32>;
    if in.uv.x >= slider_start_at && in.uv.x < slider_end_at {
        let texture_uv = vec2<f32>((in.uv.x - slider_start_at) / slider_ratio, in.uv.y);
        textured = textureSample(slider_texture, slider_sampler, texture_uv);
    }

    var rgba: vec4<f32>;
    if textured.w > 0 {
        rgba = textured * hsla2rgba(color_selected);
    } else {
        let half_ratio = slider_ratio / 2.0;
        let y_clip = 0.2;
        var color: vec4<f32>;
        if in.uv.y > y_clip && in.uv.y <= 1 - y_clip && in.uv.x > half_ratio && in.uv.x <= 1 - half_ratio {
            let half_ratio_scale = 0.5 - half_ratio;
            if in.uv.x < 0.5 {
                color = mix(color1, color2, (in.uv.x - half_ratio) / half_ratio_scale);
            } else {
                color = mix(color2, color3, (in.uv.x - 0.5) / half_ratio_scale);
            }
        }
        rgba = hsla2rgba(color);
    }
    return rgba;
}

