// © 2025 <_@habnab.it>
//
// SPDX-License-Identifier: EUPL-1.2

#![feature(try_blocks, cmp_minmax, lazy_get)]

mod animation;

use std::f32::consts::PI;

use animation::{AnimatorPlugin, SavedAnimationNode};
use bevy::{
    animation::{animated_field, AnimationTarget, AnimationTargetId, RepeatAnimation},
    input::common_conditions::input_just_pressed,
    prelude::*,
    render::render_resource::{AsBindGroup, ShaderRef},
    ui::widget::NodeImageMode,
    window::PrimaryWindow,
};
use petgraph::graph::NodeIndex;
use rand::{distr::Distribution, seq::IndexedMutRandom, Rng, SeedableRng};
use rand_chacha::ChaCha8Rng;
use uuid::Uuid;

fn main() {
    let mut app = App::new();

    app.add_plugins(DefaultPlugins)
        .add_plugins(UiMaterialPlugin::<ColorGradientMaterial>::default())
        .init_resource::<SeededRng>()
        .init_state::<ColorState>()
        .add_plugins(bevy_inspector_egui::quick::WorldInspectorPlugin::new())
        .register_asset_reflect::<ColorGradientMaterial>()
        .register_type::<ColorPicker>()
        .register_type::<ColorPickerAttribute>()
        .register_type::<ColorState>()
        .register_type::<CubehelixAnimationNode>()
        .register_type::<CubehelixClaw>()
        .register_type::<CubehelixRoot>()
        .register_type::<FoxClaw>()
        .register_type::<FoxPaw>()
        .register_type::<FoxPaws>()
        .register_type::<FoxPawSlices>()
        .register_type::<FoxPawsSpinNode>()
        .register_type::<SeededRng>()
        .register_type::<StateNodeArea>()
        .register_type::<UIBorders>()
        .add_observer(animate_cubehelix)
        .add_systems(Startup, setup)
        .add_systems(
            Update,
            (
                set_button_border,
                state_button_clicked,
                pick_button_clicked,
                color_clicked,
                remove_color_button_clicked,
                update_gradient,
                place_fox_paws.run_if(any_with_component::<PrimaryWindow>),
                (spawn_ui, spawn_fox_paws, set_button_background)
                    .chain()
                    .run_if(run_once),
                spin_fox_paws.run_if(input_just_pressed(KeyCode::KeyS)),
                update_cubehelix_color,
            ),
        )
        .add_systems(OnEnter(ColorState::Rainbow), apply_cubehelix)
        .add_systems(OnExit(ColorState::Rainbow), remove_cubehelix)
        .add_systems(OnEnter(ColorState::Pick), show_pick_buttons)
        .add_systems(OnExit(ColorState::Pick), remove_pick_buttons);

    for &state in &[ColorState::Rainbow, ColorState::Pick] {
        app.add_systems(OnEnter(state), set_button_background);
    }

    app.run();
}

#[derive(Resource, Reflect)]
#[reflect(Resource)]
#[reflect(from_reflect = false)]
struct SeededRng(#[reflect(ignore)] ChaCha8Rng);

impl FromReflect for SeededRng {
    fn from_reflect(_reflect: &dyn PartialReflect) -> Option<Self> {
        todo!()
    }
}

impl FromWorld for SeededRng {
    fn from_world(_world: &mut World) -> Self {
        SeededRng(ChaCha8Rng::from_os_rng())
    }
}

static FOX_PAW_IMAGES: &[&'static str] = &[
    "paw2.png",
    "paw2-1.png",
    "paw2-2.png",
    "paw2-3.png",
    "paw2-4.png",
    "paw2-5.png",
];
static FOX_PAW_SIZE: Vec2 = Vec2::new(172.13, 250.);

#[derive(Resource, Reflect)]
#[reflect(Resource)]
struct FoxPawSlices {
    images: Vec<Handle<Image>>,
}

#[derive(Debug, Default, Component, Reflect)]
struct FoxPaws {
    rotated: bool,
}

#[derive(Debug, Clone, Copy, Component, Reflect)]
enum FoxPaw {
    Left,
    Right,
}

impl FoxPaw {
    fn flip_x_p(&self) -> bool {
        match self {
            FoxPaw::Left => true,
            FoxPaw::Right => false,
        }
    }
}

#[derive(Debug, Component, Reflect)]
struct FoxClaw {
    paw: FoxPaw,
    digit: u8,
}

impl FoxClaw {
    fn phase(&self) -> f32 {
        let coeff = match self.paw {
            FoxPaw::Left => 5 - self.digit,
            FoxPaw::Right => 4 + self.digit,
        };
        coeff as f32 / 10.
    }
}

#[derive(Reflect, Debug, Component, Clone, Default)]
struct FoxPawsSpinNode(Option<NodeIndex>);

impl SavedAnimationNode for FoxPawsSpinNode {
    type AnimatedFrom = Transform;

    fn node_mut(&mut self) -> &mut Option<NodeIndex> {
        &mut self.0
    }
}

fn spawn_fox_paws(
    q_player: Query<Entity, With<AnimationPlayer>>,
    slices: Res<FoxPawSlices>,
    mut commands: Commands,
) {
    let Ok(player) = q_player.get_single() else {
        return;
    };
    let spawn = |parent: &mut ChildBuilder, paw: FoxPaw, transform| {
        let flip_x = paw.flip_x_p();
        parent
            .spawn((
                Sprite {
                    image: slices.images[0].clone(),
                    flip_x,
                    custom_size: Some(FOX_PAW_SIZE),
                    ..default()
                },
                transform,
                paw,
            ))
            .with_children(|parent| {
                for (e, image) in (&slices.images[1..]).into_iter().enumerate() {
                    parent.spawn((
                        Sprite {
                            image: image.clone(),
                            flip_x,
                            custom_size: Some(FOX_PAW_SIZE),
                            ..default()
                        },
                        Transform::from_xyz(0., 0., -1.),
                        FoxClaw {
                            paw,
                            digit: (e as u8) + 1,
                        },
                    ));
                }
            });
    };
    commands
        .spawn((
            FoxPaws::default(),
            FoxPawsSpinNode::default(),
            AnimationTarget {
                id: AnimationTargetId(Uuid::new_v4()),
                player,
            },
            Transform::default(),
            InheritedVisibility::VISIBLE,
        ))
        .with_children(|parent| {
            spawn(parent, FoxPaw::Left, Transform::from_xyz(-100., 0., 0.));
            spawn(parent, FoxPaw::Right, Transform::from_xyz(100., 0., 0.));
        });
}

fn place_fox_paws(
    mut q_paws: Query<&mut Transform, With<FoxPaws>>,
    q_camera: Query<&Camera, Changed<Camera>>,
) {
    let Ok(camera) = q_camera.get_single() else {
        return;
    };
    let Some(viewport) = camera.logical_viewport_rect() else {
        return;
    };
    let Ok(mut paws) = q_paws.get_single_mut() else {
        return;
    };
    let paws_box_inset = viewport.width() / 3.;
    let paws_rect = Rect::new(
        viewport.min.x + paws_box_inset,
        viewport.min.y,
        viewport.max.x,
        viewport.max.y,
    );
    let paws_loc = paws_rect.center() - viewport.center();
    paws.translation.x = paws_loc.x;
    paws.translation.y = paws_loc.y;
}

fn spin_fox_paws(
    mut q_paws: Query<(Entity, &mut FoxPaws), With<AnimationTarget>>,
    mut commands: Commands,
) {
    let Ok((paw_entity, mut paws)) = q_paws.get_single_mut() else {
        return;
    };
    paws.rotated = !paws.rotated;
    let rotation = Quat::from_rotation_z(if paws.rotated { PI } else { 0. });
    AnimatorPlugin::<FoxPawsSpinNode>::start_animation(
        &mut commands,
        paw_entity,
        RepeatAnimation::Never,
        move |transform, target| {
            let mut clip = AnimationClip::default();
            clip.add_curve_to_target(
                target,
                AnimatableCurve::new(
                    animated_field!(Transform::rotation),
                    EasingCurve::new(transform.rotation, rotation, EaseFunction::CubicInOut)
                        .reparametrize_linear(interval(0., 0.5).unwrap())
                        .unwrap(),
                ),
            );
            clip
        },
    );
}

#[derive(Resource, Reflect)]
#[reflect(Resource)]
struct UIBorders {
    texture: Handle<Image>,
    atlas_layout: Handle<TextureAtlasLayout>,
    slicer: TextureSlicer,
}

impl UIBorders {
    fn make_node(&self, index: usize, color: Color) -> ImageNode {
        ImageNode::from_atlas_image(self.texture.clone(), TextureAtlas {
            index,
            layout: self.atlas_layout.clone(),
        })
        .with_color(color)
        .with_mode(NodeImageMode::Sliced(self.slicer.clone()))
    }
}

#[derive(States, Reflect, Default, Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum ColorState {
    #[default]
    Init,
    Rainbow,
    Pick,
}

#[derive(Reflect, Debug, Component, Clone)]
struct ColorStateButton(ColorState);

fn set_button_background(
    mut q_buttons: Query<(&ColorStateButton, &mut BackgroundColor), With<Button>>,
    color_state: Res<State<ColorState>>,
) {
    for (button, mut bg) in &mut q_buttons {
        let color = if color_state.get() == &button.0 {
            Color::hsla(330., 0.8, 0.9, 0.8)
        } else {
            Color::hsla(0., 0., 0.7, 0.8)
        };
        bg.0 = color;
    }
}

fn set_button_border(
    mut q_buttons: Query<(&Interaction, &mut ImageNode), (Changed<Interaction>, With<Button>)>,
) {
    for (&interaction, mut image) in &mut q_buttons {
        let color = match interaction {
            Interaction::Pressed => Color::hsl(300., 1., 0.65),
            Interaction::Hovered => Color::hsl(330., 1., 0.55),
            Interaction::None => Color::hsl(330., 1., 0.2),
        };
        image.color = color;
    }
}

fn state_button_clicked(
    q_buttons: Query<(&Interaction, &ColorStateButton), (Changed<Interaction>, With<Button>)>,
    mut next_color_state: ResMut<NextState<ColorState>>,
) {
    for (&interaction, button) in &q_buttons {
        if let Interaction::Pressed = interaction {
            next_color_state.set(button.0);
        }
    }
}

#[derive(Reflect, Debug, Component, Clone)]
struct StateNodeArea;

fn spawn_ui(borders: Res<UIBorders>, mut commands: Commands) {
    commands.set_state(ColorState::Rainbow);
    commands
        .spawn((
            Node {
                top: Val::Px(0.),
                left: Val::Px(0.),
                height: Val::Percent(100.),
                width: Val::Vw(33.),
                display: Display::Grid,
                grid_template_rows: vec![GridTrack::min_content(), GridTrack::flex(1.0)],
                grid_template_columns: vec![GridTrack::auto()],
                ..default()
            },
            BackgroundColor(Color::hsla(0., 0., 0.5, 0.8)),
        ))
        .with_children(|parent| {
            parent
                .spawn((Node { ..default() },))
                .with_children(|parent| {
                    for &state in &[ColorState::Rainbow, ColorState::Pick] {
                        parent
                            .spawn((
                                ColorStateButton(state),
                                Button,
                                borders.make_node(8, Color::hsl(330., 1., 0.2)),
                                Node {
                                    width: Val::Percent(100.),
                                    justify_content: JustifyContent::Center,
                                    align_items: AlignItems::Center,
                                    margin: UiRect::all(Val::Px(10.)),
                                    padding: UiRect::all(Val::Px(10.)),
                                    ..default()
                                },
                                BackgroundColor(Color::hsla(0., 0., 0.5, 0.8)),
                            ))
                            .with_children(|parent| {
                                parent.spawn((
                                    Text::new(format!("{state:?}")),
                                    TextColor(Color::hsl(0., 0., 0.1)),
                                ));
                            });
                    }
                });
            parent.spawn((
                Node {
                    flex_direction: FlexDirection::Column,
                    ..default()
                },
                StateNodeArea,
            ));
        });
}

#[derive(Reflect, Debug, Clone, Copy)]
enum PickButtonAction {
    AddColor,
    Randomize,
}

#[derive(Reflect, Debug, Component, Clone)]
struct PickButton(PickButtonAction);

#[derive(Reflect, Debug, Component, Clone)]
struct RemoveColorButton(Entity);

#[derive(Reflect, Debug, Component, Clone)]
struct ColorsNodeArea;

fn show_pick_buttons(
    q_area: Query<Entity, With<StateNodeArea>>,
    borders: Res<UIBorders>,
    mut commands: Commands,
) {
    let Ok(area) = q_area.get_single() else {
        return;
    };
    commands.entity(area).with_children(|parent| {
        for &action in &[PickButtonAction::AddColor, PickButtonAction::Randomize] {
            parent
                .spawn((
                    Button,
                    PickButton(action),
                    borders.make_node(8, Color::hsl(330., 1., 0.2)),
                    Node {
                        margin: UiRect::horizontal(Val::Px(5.)).with_bottom(Val::Px(5.)),
                        padding: UiRect::all(Val::Px(10.)),
                        justify_content: JustifyContent::Center,
                        align_items: AlignItems::Center,
                        ..default()
                    },
                    BackgroundColor(Color::hsla(0., 0., 0.5, 0.8)),
                ))
                .with_children(|parent| {
                    parent.spawn((
                        Text::new(format!("{action:?}")),
                        TextColor(Color::hsl(0., 0., 0.1)),
                    ));
                });
        }
        parent.spawn((
            Node {
                flex_direction: FlexDirection::Column,
                margin: UiRect::horizontal(Val::Px(13.)),
                ..default()
            },
            ColorsNodeArea,
        ));
    });
}

fn remove_pick_buttons(q_area: Query<Entity, With<StateNodeArea>>, mut commands: Commands) {
    let Ok(area) = q_area.get_single() else {
        return;
    };
    commands.entity(area).despawn_descendants();
}

#[derive(Reflect, Debug, Component, Clone)]
struct SelectedColor {
    selected: Color,
}

fn pick_random_color<R: Rng>(rng: &mut R) -> Color {
    let hue_dist = rand::distr::Uniform::new(0., 360.).unwrap();
    let saturation_dist = rand::distr::Uniform::new(0.5, 0.9).unwrap();
    let lightness_dist = rand::distr::Uniform::new(0.7, 0.8).unwrap();
    Color::hsl(
        hue_dist.sample(rng),
        saturation_dist.sample(rng),
        lightness_dist.sample(rng),
    )
}

fn pick_button_clicked(
    q_area: Query<Entity, With<ColorsNodeArea>>,
    q_buttons: Query<(&PickButton, &Interaction), Changed<Interaction>>,
    q_colors: Query<(&SelectedColor, &Children)>,
    mut q_claws: Query<&mut Sprite, With<FoxClaw>>,
    mut q_text: Query<&mut Text>,
    borders: Res<UIBorders>,
    mut rng: ResMut<SeededRng>,
    mut commands: Commands,
) {
    let Ok(area) = q_area.get_single() else {
        return;
    };
    for (button, &interaction) in &q_buttons {
        if interaction != Interaction::Pressed {
            continue;
        }
        match button.0 {
            PickButtonAction::AddColor => {
                commands.entity(area).with_children(|parent| {
                    parent
                        .spawn(Node {
                            display: Display::Grid,
                            grid_template_columns: vec![GridTrack::flex(1.0), GridTrack::auto()],
                            ..default()
                        })
                        .with_children(|parent| {
                            let color_node = parent.parent_entity();
                            let color = pick_random_color(&mut rng.0);
                            parent
                                .spawn((
                                    Button,
                                    Node {
                                        width: Val::Percent(100.),
                                        margin: UiRect::all(Val::Px(2.)),
                                        padding: UiRect::all(Val::Px(2.)).with_left(Val::Px(5.)),
                                        ..default()
                                    },
                                    SelectedColor { selected: color },
                                    BackgroundColor(color),
                                ))
                                .with_child((Text::new("0"), TextColor(Color::hsl(0., 0., 0.1))));
                            parent
                                .spawn((
                                    Button,
                                    RemoveColorButton(color_node),
                                    borders.make_node(1, Color::hsl(330., 1., 0.2)),
                                    Node {
                                        // height: Val::Percent(100.),
                                        // height: Val::Px(23.),
                                        width: Val::Px(23.),
                                        margin: UiRect::all(Val::Px(2.)),
                                        justify_content: JustifyContent::Center,
                                        align_items: AlignItems::Center,
                                        ..default()
                                    },
                                    BackgroundColor(Color::hsla(0., 0., 0.5, 0.8)),
                                ))
                                .with_children(|parent| {
                                    parent.spawn((
                                        Text::new("x"),
                                        TextFont::from_font_size(14.),
                                        TextColor(Color::hsl(0., 0., 0.1)),
                                    ));
                                });
                        });
                });
            }
            PickButtonAction::Randomize => {
                let mut colors = q_colors
                    .iter()
                    .map(|(color, children)| (color, children, 0usize))
                    .collect::<Vec<_>>();
                if colors.is_empty() {
                    return;
                }
                for mut sprite in &mut q_claws {
                    let selection = colors.choose_mut(&mut rng.0).unwrap();
                    sprite.color = selection.0.selected;
                    selection.2 += 1;
                }
                for (_, children, count) in colors {
                    let Ok(mut text) = q_text.get_mut(children[0]) else {
                        continue;
                    };
                    **text = format!("{count}");
                }
            }
        }
    }
}

#[derive(Debug, Default)]
struct ColorClickedState {
    last_picker: Option<Entity>,
}

#[derive(Debug, Clone, Default, Component, Reflect)]
struct ColorPicker {
    color: Hsla,
}

#[derive(Debug, Copy, Clone, Reflect)]
enum HslaAttribute {
    Hue,
    Saturation,
    Lightness,
}

impl HslaAttribute {
    fn derive_poles(&self, color: Hsla) -> (Hsla, Hsla, Hsla) {
        match *self {
            HslaAttribute::Hue => (
                color.with_hue(0.),
                color.with_hue(180.),
                color.with_hue(360.),
            ),
            HslaAttribute::Saturation => (
                color.with_saturation(0.),
                color.with_saturation(0.5),
                color.with_saturation(1.),
            ),
            HslaAttribute::Lightness => (
                color.with_luminance(0.),
                color.with_luminance(0.5),
                color.with_luminance(1.),
            ),
        }
    }

    fn short_name(&self) -> &'static str {
        match *self {
            HslaAttribute::Hue => "H",
            HslaAttribute::Saturation => "S",
            HslaAttribute::Lightness => "L",
        }
    }

    fn current(&self, color: Hsla) -> f32 {
        match *self {
            HslaAttribute::Hue => color.hue / 360.,
            HslaAttribute::Saturation => color.saturation,
            HslaAttribute::Lightness => color.lightness,
        }
    }
}

impl HslaAttribute {
    const ALL: &[HslaAttribute] = &[
        HslaAttribute::Hue,
        HslaAttribute::Saturation,
        HslaAttribute::Lightness,
    ];
}

#[derive(Debug, Clone, Component, Reflect)]
struct ColorPickerAttribute {
    picker: Entity,
    attr: HslaAttribute,
    current: f32,
}

fn color_clicked(
    q_colors: Query<(&Parent, &SelectedColor)>,
    q_changed: Query<(Entity, &Interaction), Changed<Interaction>>,
    q_picker: Query<&ColorPicker>,
    mut materials: ResMut<Assets<ColorGradientMaterial>>,
    mut commands: Commands,
    mut local: Local<ColorClickedState>,
    asset_server: Res<AssetServer>,
) {
    let slider = std::cell::LazyCell::new(|| asset_server.load("slider.png"));
    for (entity, &interaction) in &q_changed {
        let Ok((parent, selected)) = q_colors.get(entity) else {
            continue;
        };
        if interaction != Interaction::Pressed {
            continue;
        }
        if let Some(last_entity) = local.last_picker.take() {
            if q_picker.contains(last_entity) {
                commands.entity(last_entity).despawn_recursive();
            }
        }
        let mut picker = Entity::PLACEHOLDER;
        commands.entity(**parent).with_children(|parent| {
            let color = selected.selected.into();
            parent
                .spawn((
                    Node {
                        grid_column: GridPlacement::span(2),
                        flex_direction: FlexDirection::Column,
                        ..default()
                    },
                    ColorPicker { color },
                ))
                .with_children(|parent| {
                    picker = parent.parent_entity();
                    for &attr in HslaAttribute::ALL {
                        let (color1, color2, color3) = attr.derive_poles(color);
                        let color1 = color1.to_vec4();
                        let color2 = color2.to_vec4();
                        let color3 = color3.to_vec4();
                        let current = attr.current(color);
                        parent
                            .spawn((
                                Node {
                                    width: Val::Percent(100.),
                                    display: Display::Grid,
                                    grid_template_columns: vec![
                                        GridTrack::auto(),
                                        GridTrack::flex(1.0),
                                    ],
                                    ..default()
                                },
                                ColorPickerAttribute {
                                    attr,
                                    current,
                                    picker,
                                },
                            ))
                            .with_children(|parent| {
                                parent
                                    .spawn((
                                        Node {
                                            margin: UiRect::all(Val::Px(1.)),
                                            padding: UiRect::all(Val::Px(3.)),
                                            ..default()
                                        },
                                        BackgroundColor(Color::hsla(0., 0., 0.9, 0.2)),
                                    ))
                                    .with_child((
                                        Text::new(attr.short_name()),
                                        TextColor(Color::hsl(0., 0., 0.1)),
                                    ));
                                parent.spawn((
                                    Node {
                                        width: Val::Percent(100.),
                                        margin: UiRect::all(Val::Px(1.)),
                                        ..default()
                                    },
                                    MaterialNode(materials.add(ColorGradientMaterial {
                                        color1,
                                        color2,
                                        color3,
                                        color_selected: color.to_vec4(),
                                        slider: (*slider).clone(),
                                        slider_ratio: 1.,
                                        slider_position: current,
                                    })),
                                ));
                            });
                    }
                });
        });
        local.last_picker = Some(picker);
    }
}

impl UiMaterial for ColorGradientMaterial {
    fn fragment_shader() -> ShaderRef {
        "color_gradient.wgsl".into()
    }
}

#[derive(Asset, TypePath, AsBindGroup, Debug, Clone, Reflect)]
#[reflect(type_path = false)]
struct ColorGradientMaterial {
    #[uniform(0)]
    color1: Vec4,
    #[uniform(1)]
    color2: Vec4,
    #[uniform(2)]
    color3: Vec4,
    #[uniform(7)]
    color_selected: Vec4,
    #[texture(3)]
    #[sampler(4)]
    slider: Handle<Image>,
    #[uniform(5)]
    slider_ratio: f32,
    #[uniform(6)]
    slider_position: f32,
}

fn update_gradient(
    q_picker: Query<&ColorPicker>,
    q_parent: Query<&ColorPickerAttribute>,
    q_nodes: Query<(&MaterialNode<ColorGradientMaterial>, &ComputedNode, &Parent)>,
    mut materials: ResMut<Assets<ColorGradientMaterial>>,
) {
    for (mat, node, parent) in &q_nodes {
        let Some(mat) = materials.get_mut(mat.id()) else {
            continue;
        };
        let Ok(attr_picker) = q_parent.get(**parent) else {
            continue;
        };
        let Ok(picker) = q_picker.get(attr_picker.picker) else {
            continue;
        };
        let node_size = node.size();
        mat.slider_position = attr_picker.current;
        mat.slider_ratio = 21. / node_size.x / node.inverse_scale_factor();
        mat.color_selected = picker.color.to_vec4();
    }
}

fn remove_color_button_clicked(
    q_buttons: Query<(&RemoveColorButton, &Interaction), Changed<Interaction>>,
    mut commands: Commands,
) {
    for (button, &interaction) in &q_buttons {
        if let Interaction::Pressed = interaction {
            commands.entity(button.0).despawn_recursive();
        }
    }
}

#[derive(Debug, Clone, Default, Component, Reflect)]
struct CubehelixRoot {
    value: f32,
}

#[derive(Debug, Clone, Default, Component, Reflect)]
struct CubehelixClaw {
    phase: f32,
    color: Color,
}

fn update_cubehelix_color(
    q_color_root: Query<&CubehelixRoot, Changed<CubehelixRoot>>,
    mut q_claw: Query<(&mut Sprite, &mut CubehelixClaw)>,
) {
    let Ok(color_root) = q_color_root.get_single() else {
        return;
    };
    for (mut sprite, mut cubehelix) in &mut q_claw {
        let value = ((*color_root).value + cubehelix.phase) % 1.;
        let mut h = (-100.).lerp(260., value);
        if h < 0. {
            h += 360.;
        }
        let half_value = if value < 0.5 {
            value * 2.
        } else {
            -(value - 1.) * 2.
        };
        let s = (0.375).lerp(0.75, half_value);
        let l = (0.35).lerp(0.8, half_value);
        cubehelix.color = Color::hsl(h, s, l);
        sprite.color = cubehelix.color;
    }
}

#[derive(Reflect, Debug, Component, Clone, Default)]
struct CubehelixAnimationNode(Option<NodeIndex>);

impl SavedAnimationNode for CubehelixAnimationNode {
    type AnimatedFrom = CubehelixRoot;

    fn node_mut(&mut self) -> &mut Option<NodeIndex> {
        &mut self.0
    }
}

fn animate_cubehelix(
    ev: Trigger<OnInsert, CubehelixRoot>,
    q_can_animate: Query<&AnimationTarget, With<CubehelixRoot>>,
    mut commands: Commands,
) {
    let Ok(_) = q_can_animate.get(ev.entity()) else {
        return;
    };
    AnimatorPlugin::<CubehelixAnimationNode>::start_animation(
        &mut commands,
        ev.entity(),
        RepeatAnimation::Forever,
        move |_, target| {
            let mut clip = AnimationClip::default();
            clip.add_curve_to_target(
                target,
                AnimatableCurve::new(
                    animated_field!(CubehelixRoot::value),
                    EasingCurve::new(0., 1., EaseFunction::Linear)
                        .reparametrize_linear(interval(0., 5.0).unwrap())
                        .unwrap(),
                ),
            );
            clip
        },
    );
}

fn apply_cubehelix(q_claws: Query<(Entity, &FoxClaw)>, mut commands: Commands) {
    for (entity, claw) in &q_claws {
        commands.entity(entity).insert(CubehelixClaw {
            phase: claw.phase(),
            ..default()
        });
    }
}

fn remove_cubehelix(q_claws: Query<Entity, With<CubehelixClaw>>, mut commands: Commands) {
    for entity in &q_claws {
        commands.entity(entity).remove::<CubehelixClaw>();
    }
}

#[derive(Bundle)]
struct CubehelixBundle {
    cubehelix: CubehelixRoot,
    target: AnimationTarget,
    tracker: CubehelixAnimationNode,
}

impl CubehelixBundle {
    fn new(player: Entity) -> Self {
        CubehelixBundle {
            cubehelix: CubehelixRoot::default(),
            target: AnimationTarget {
                id: AnimationTargetId(Uuid::new_v4()),
                player,
            },
            tracker: Default::default(),
        }
    }
}

impl Default for CubehelixBundle {
    fn default() -> Self {
        CubehelixBundle::new(Entity::PLACEHOLDER)
    }
}

fn setup(
    mut commands: Commands,
    asset_server: Res<AssetServer>,
    mut texture_atlases: ResMut<Assets<TextureAtlasLayout>>,
    mut animation_graphs: ResMut<Assets<AnimationGraph>>,
) {
    commands.spawn(Camera2d);
    let player = commands
        .spawn((
            AnimationPlayer::default(),
            AnimationGraphHandle(animation_graphs.add(AnimationGraph::new())),
        ))
        .id();

    commands.spawn(CubehelixBundle::new(player));

    commands.insert_resource({
        let images = FOX_PAW_IMAGES
            .iter()
            .map(|&path| asset_server.load(path))
            .collect();
        FoxPawSlices { images }
    });

    commands.insert_resource({
        let texture = asset_server.load("fantasy_ui_border_sheet.png");
        let atlas_layout = TextureAtlasLayout::from_grid(
            UVec2::splat(48),
            6,
            6,
            Some(UVec2::splat(4)),
            Some(UVec2::splat(2)),
        );
        let atlas_layout = texture_atlases.add(atlas_layout);
        let slicer = TextureSlicer {
            border: BorderRect::square(24.0),
            center_scale_mode: SliceScaleMode::Stretch,
            sides_scale_mode: SliceScaleMode::Stretch,
            max_corner_scale: 1.0,
        };
        UIBorders {
            texture,
            atlas_layout,
            slicer,
        }
    });
}
