// © 2025 <_@habnab.it>
//
// SPDX-License-Identifier: EUPL-1.2

#![feature(try_blocks)]
#![cfg_attr(not(debug_assertions), windows_subsystem = "windows")]

mod animation;

use std::{
    f32::consts::PI,
    sync::{
        atomic::{AtomicU32, Ordering},
        Arc,
    },
};

use animation::{AnimatorPlugin, SavedAnimationNode};
use bevy::{
    animation::{animated_field, AnimationTarget, AnimationTargetId, RepeatAnimation},
    input::{
        common_conditions::input_just_pressed,
        mouse::{MouseScrollUnit, MouseWheel},
    },
    picking::{focus::HoverMap, pointer::Location},
    prelude::*,
    render::{
        camera::NormalizedRenderTarget,
        render_resource::{AsBindGroup, ShaderRef},
    },
    ui::widget::NodeImageMode,
    utils::HashMap,
    window::PrimaryWindow,
};
use petgraph::graph::NodeIndex;
use rand::{distr::Distribution, seq::IndexedMutRandom, Rng, SeedableRng};
use rand_chacha::ChaCha8Rng;
use uuid::Uuid;

fn main() {
    #[cfg(target_family = "wasm")]
    let progress_ul: Option<_> = try {
        use wasm_bindgen::JsCast;
        web_sys::console::log_1(&"main start".into());
        let document = web_sys::window()?.document()?;
        let ul = document
            .get_element_by_id("downloading")?
            .dyn_into::<web_sys::HtmlElement>()
            .ok()?;
        ul.append_child(&document.create_element("li").ok()?.into())
            .ok()?
            .set_text_content(Some("bevy app init ..."));
        ul
    };

    let mut app = App::new();

    app.add_plugins({
        DefaultPlugins.build().set(WindowPlugin {
            primary_window: Some(Window {
                fit_canvas_to_parent: true,
                title: "pretty claws".into(),
                ..default()
            }),
            ..default()
        })
    });

    app.add_plugins(UiMaterialPlugin::<ColorGradientMaterial>::default())
        .init_resource::<SeededRng>()
        .insert_resource(bevy_pkv::PkvStore::new("pictures.cutefox", "pretty-claws"))
        .init_state::<ColorState>()
        .register_asset_reflect::<ColorGradientMaterial>()
        .register_type::<AdjustColorWeight>()
        .register_type::<AppAssets>()
        .register_type::<AssetLoaderNode>()
        .register_type::<AssetLoaderText>()
        .register_type::<ColorPicker>()
        .register_type::<ColorPickerAttribute>()
        .register_type::<ColorsNodeArea>()
        .register_type::<ColorState>()
        .register_type::<ColorStateButton>()
        .register_type::<CubehelixAnimationNode>()
        .register_type::<CubehelixClaw>()
        .register_type::<CubehelixRoot>()
        .register_type::<FoxClaw>()
        .register_type::<FoxPaw>()
        .register_type::<FoxPaws>()
        .register_type::<FoxPawsSpinNode>()
        .register_type::<HslaAttribute>()
        .register_type::<LastPersistentState>()
        .register_type::<PersistentClaw>()
        .register_type::<PersistentColor>()
        .register_type::<PersistentState>()
        .register_type::<PickButton>()
        .register_type::<PickButtonAction>()
        .register_type::<RemoveColorButton>()
        .register_type::<SeededRng>()
        .register_type::<SelectedColor>()
        .register_type::<StateNodeArea>()
        .add_observer(animate_cubehelix)
        .add_observer(click_spin_fox_paws)
        .add_observer(gradient_drag::<Pointer<Click>>)
        .add_observer(gradient_drag::<Pointer<Move>>)
        .add_observer(update_scroll_position_drag)
        .add_systems(Startup, setup)
        .add_systems(
            Update,
            (
                (
                    animation::clear_unused_animation_graphs,
                    set_button_border,
                    state_button_clicked,
                    pick_button_clicked,
                    color_clicked,
                    remove_color_button_clicked,
                    update_gradient,
                    place_fox_paws.run_if(any_with_component::<PrimaryWindow>),
                    asset_loader_update.run_if(resource_exists::<AssetBarrier>),
                    (spawn_ui, spawn_fox_paws, set_button_background)
                        .chain()
                        .run_if(assets_loaded.and(run_once)),
                    spin_fox_paws.run_if(input_just_pressed(KeyCode::KeyS)),
                    update_cubehelix_color,
                    (
                        adjust_color_weight_clicked,
                        total_color_weights,
                        show_color_caption,
                    )
                        .chain(),
                    update_scroll_position,
                ),
                save_state,
            )
                .chain(),
        )
        .add_systems(OnEnter(ColorState::Rainbow), apply_cubehelix)
        .add_systems(OnExit(ColorState::Rainbow), remove_cubehelix)
        .add_systems(OnEnter(ColorState::Pick), show_pick_buttons);

    #[cfg(feature = "inspect")]
    app.add_plugins(bevy_inspector_egui::quick::WorldInspectorPlugin::new());

    for &state in ColorState::BUTTONS {
        app.add_systems(OnEnter(state), set_button_background);
        app.add_systems(OnExit(state), clear_state_area);
    }

    #[cfg(target_family = "wasm")]
    let _: Option<_> = try {
        web_sys::console::log_1(&"bevy run".into());
        progress_ul?.style().set_property("display", "none").ok()?;
    };
    app.run();
}

#[derive(Resource, Reflect)]
#[reflect(Resource)]
struct AppAssets {
    fox_paw_images: Vec<Handle<Image>>,
    borders: Handle<Image>,
    slider: Handle<Image>,
    font: Handle<Font>,
    border_atlas_layout: Handle<TextureAtlasLayout>,
    border_slicer: TextureSlicer,
}

impl AppAssets {
    fn make_borders(&self, index: usize, color: Color) -> ImageNode {
        ImageNode::from_atlas_image(self.borders.clone_weak(), TextureAtlas {
            index,
            layout: self.border_atlas_layout.clone_weak(),
        })
        .with_color(color)
        .with_mode(NodeImageMode::Sliced(self.border_slicer.clone()))
    }

    fn text_font(&self) -> TextFont {
        TextFont::from_font(self.font.clone_weak())
    }
}

const DEFAULT_BACKGROUND_COLOR: BackgroundColor = BackgroundColor(Color::hsla(0., 0., 0.5, 0.8));
const PICKER_BACKGROUND_COLOR: BackgroundColor = BackgroundColor(Color::hsla(0., 0., 0.9, 0.2));
const DEFAULT_TEXT_COLOR: TextColor = TextColor(Color::hsl(0., 0., 0.1));
const LIGHT_TEXT_COLOR: TextColor = TextColor(Color::hsl(0., 0., 0.9));
const BUTTON_BORDER_COLOR: Color = Color::hsl(330., 1., 0.2);
const STATE_BUTTON_SELECTED_BACKGROUND: Color = Color::hsla(330., 0.8, 0.9, 0.8);
const STATE_BUTTON_BACKGROUND: Color = Color::hsla(0., 0., 0.7, 0.8);

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

#[derive(Debug, Default, Component, Reflect)]
struct FoxPaws {
    rotated: bool,
}

#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    Hash,
    Component,
    Reflect,
    serde::Serialize,
    serde::Deserialize,
)]
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

    fn direction(&self) -> Vec3 {
        match self {
            FoxPaw::Left => Vec3::new(-1., 1., 1.),
            FoxPaw::Right => Vec3::new(1., 1., 1.),
        }
    }
}

#[derive(
    Debug, Clone, Component, Reflect, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize,
)]
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
    assets: Res<AppAssets>,
    last_state: Res<LastPersistentState>,
    mut animation_graphs: ResMut<Assets<AnimationGraph>>,
    mut commands: Commands,
) {
    let prev_claws = last_state.0.as_ref().and_then(|s| match s {
        PersistentState::Pick { claws, .. } => Some(
            claws
                .iter()
                .map(|c| (&c.claw, &c.color))
                .collect::<HashMap<_, _>>(),
        ),
        _ => None,
    });
    let player = commands
        .spawn((
            AnimationPlayer::default(),
            AnimationGraphHandle(animation_graphs.add(AnimationGraph::new())),
        ))
        .id();
    let spawn = |parent: &mut ChildBuilder, paw: FoxPaw| {
        let flip_x = paw.flip_x_p();
        parent
            .spawn((
                Sprite {
                    image: assets.fox_paw_images[0].clone_weak(),
                    flip_x,
                    custom_size: Some(FOX_PAW_SIZE),
                    ..default()
                },
                Transform::from_translation(Vec3::new(100., 0., 0.) * paw.direction()),
                paw,
            ))
            .with_children(|parent| {
                for (e, image) in (&assets.fox_paw_images[1..]).into_iter().enumerate() {
                    let claw = FoxClaw {
                        paw,
                        digit: (e as u8) + 1,
                    };
                    let color = prev_claws
                        .as_ref()
                        .and_then(|m| m.get(&claw).map(|&&c| c))
                        .unwrap_or_default();
                    parent.spawn((
                        Sprite {
                            image: image.clone_weak(),
                            flip_x,
                            custom_size: Some(FOX_PAW_SIZE),
                            color,
                            ..default()
                        },
                        Transform::from_xyz(0., 0., -1.),
                        claw,
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
            spawn(parent, FoxPaw::Left);
            spawn(parent, FoxPaw::Right);
        });
}

fn place_fox_paws(
    mut q_paws: Query<&mut Transform, (With<FoxPaws>, Without<FoxPaw>, Without<FoxClaw>)>,
    mut q_ui_node: Query<&mut Node, With<UiArea>>,
    q_camera: Query<Ref<Camera>>,
    mut q_paw_sprite: Query<
        (&mut Sprite, &mut Transform, &FoxPaw),
        (Without<FoxPaws>, Without<FoxClaw>),
    >,
    mut q_claw_sprite: Query<&mut Sprite, (With<FoxClaw>, Without<FoxPaw>, Without<FoxPaws>)>,
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
    let Ok(mut ui_node) = q_ui_node.get_single_mut() else {
        return;
    };

    let viewport_size = viewport.size();
    if camera.is_changed() || paws.is_added() {
        let (paws_rect, ui_rect) = if viewport_size.x > viewport_size.y {
            let paws_box_inset = viewport_size.x * 0.4;
            let x_split = viewport.min.x + paws_box_inset;
            (
                Rect::new(x_split, viewport.min.y, viewport.max.x, viewport.max.y),
                Rect::new(viewport.min.x, viewport.min.y, x_split, viewport.max.y),
            )
        } else {
            let paws_box_inset = viewport_size.y * 0.4;
            let y_split = viewport.max.y - paws_box_inset;
            (
                Rect::new(viewport.min.x, y_split, viewport.max.x, viewport.max.y),
                Rect::new(viewport.min.x, viewport.min.y, viewport.max.x, y_split),
            )
        };
        ui_node.top = Val::Px(ui_rect.min.x);
        ui_node.left = Val::Px(ui_rect.min.y);
        ui_node.width = Val::Px(ui_rect.width());
        ui_node.height = Val::Px(ui_rect.height());
        let paws_loc = paws_rect.center() - viewport.center();
        paws.translation.x = paws_loc.x;
        paws.translation.y = -paws_loc.y;
        let paw_width = paws_rect.width() * 0.3;
        let paw_height = paw_width * FOX_PAW_SIZE.y / FOX_PAW_SIZE.x;
        let paw_sprite_size = Vec2::new(paw_width, paw_height);
        for (_, mut transform, paw) in &mut q_paw_sprite {
            transform.translation = Vec3::new(paws_rect.width() * 0.17, 0., 0.) * paw.direction();
        }
        for mut sprite in q_paw_sprite
            .iter_mut()
            .map(|t| t.0)
            .chain(&mut q_claw_sprite)
        {
            sprite.custom_size = Some(paw_sprite_size);
        }
    }
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

fn click_spin_fox_paws(
    mut ev: Trigger<Pointer<Click>>,
    q_paw: Query<&FoxPaw>,
    q_paws: Query<(Entity, &mut FoxPaws), With<AnimationTarget>>,
    commands: Commands,
) {
    if q_paw.contains(ev.entity()) && ev.event().hit.position.is_some() {
        spin_fox_paws(q_paws, commands);
        ev.propagate(false);
    }
}

#[derive(Resource, Reflect)]
#[reflect(Resource)]
struct LastPersistentState(Option<PersistentState>);

#[derive(Debug, Clone, Reflect, serde::Serialize, serde::Deserialize)]
enum PersistentState {
    Rainbow,
    Pick {
        colors: Vec<PersistentColor>,
        claws: Vec<PersistentClaw>,
    },
}

#[derive(Debug, Clone, Reflect, serde::Serialize, serde::Deserialize)]
struct PersistentColor {
    color: Color,
    weight: usize,
    n_claws: usize,
}

#[derive(Debug, Clone, Reflect, serde::Serialize, serde::Deserialize)]
struct PersistentClaw {
    claw: FoxClaw,
    color: Color,
}

impl PersistentState {
    const KEY: &'static str = "persistent-state";
}

fn save_state(
    app_state: Res<State<ColorState>>,
    q_colors: Query<Ref<SelectedColor>>,
    q_claws: Query<(Ref<FoxClaw>, Ref<Sprite>)>,
    mut pkv: ResMut<bevy_pkv::PkvStore>,
) {
    let mut any_changes = app_state.is_changed();
    let persistent = match **app_state {
        ColorState::Init => return,
        ColorState::Rainbow => PersistentState::Rainbow,
        ColorState::Pick => {
            let colors = q_colors
                .iter()
                .map(|c| {
                    if c.is_changed() {
                        any_changes = true;
                    }
                    PersistentColor {
                        color: c.selected,
                        weight: c.weight,
                        n_claws: c.n_claws,
                    }
                })
                .collect();
            let claws = q_claws
                .iter()
                .map(|(claw, sprite)| {
                    if claw.is_changed() || sprite.is_changed() {
                        any_changes = true;
                    }
                    PersistentClaw {
                        claw: claw.clone(),
                        color: sprite.color,
                    }
                })
                .collect();
            PersistentState::Pick { colors, claws }
        }
    };
    if !any_changes {
        return;
    }
    match pkv.set(PersistentState::KEY, &persistent) {
        Ok(()) => trace!("saved {persistent:#?}"),
        Err(e) => error!("couldn't persist state: {e:?}"),
    }
}

#[derive(
    States,
    Reflect,
    Default,
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    Hash,
    serde::Serialize,
    serde::Deserialize,
)]
enum ColorState {
    #[default]
    Init,
    Rainbow,
    Pick,
}

impl ColorState {
    const BUTTONS: &[ColorState] = &[ColorState::Rainbow, ColorState::Pick];
}

#[derive(Reflect, Debug, Component, Clone)]
struct ColorStateButton(ColorState);

fn set_button_background(
    mut q_buttons: Query<(&ColorStateButton, &mut BackgroundColor), With<Button>>,
    color_state: Res<State<ColorState>>,
) {
    for (button, mut bg) in &mut q_buttons {
        let color = if color_state.get() == &button.0 {
            STATE_BUTTON_SELECTED_BACKGROUND
        } else {
            STATE_BUTTON_BACKGROUND
        };
        bg.0 = color;
    }
}

fn set_button_border(
    mut q_buttons: Query<(&Interaction, &mut ImageNode), (Changed<Interaction>, With<Button>)>,
) {
    for (&interaction, mut image) in &mut q_buttons {
        let color = match interaction {
            Interaction::Pressed => BUTTON_BORDER_COLOR.lighter(0.45),
            Interaction::Hovered => BUTTON_BORDER_COLOR.lighter(0.2),
            Interaction::None => BUTTON_BORDER_COLOR,
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
struct UiArea;

#[derive(Reflect, Debug, Component, Clone)]
struct StateNodeArea;

fn spawn_ui(last_state: Res<LastPersistentState>, assets: Res<AppAssets>, mut commands: Commands) {
    commands
        .spawn((
            Node {
                // top: Val::Px(0.),
                // left: Val::Px(0.),
                // height: Val::Percent(100.),
                // width: Val::Vw(33.),
                display: Display::Grid,
                grid_template_rows: vec![GridTrack::min_content(), GridTrack::flex(1.0)],
                grid_template_columns: vec![GridTrack::auto()],
                ..default()
            },
            DEFAULT_BACKGROUND_COLOR,
            UiArea,
        ))
        .with_children(|parent| {
            parent
                .spawn((Node {
                    margin: UiRect::horizontal(Val::Px(2.)).with_top(Val::Px(3.)),
                    ..default()
                },))
                .with_children(|parent| {
                    for &state in ColorState::BUTTONS {
                        parent
                            .spawn((
                                ColorStateButton(state),
                                Button,
                                assets.make_borders(8, BUTTON_BORDER_COLOR),
                                Node {
                                    width: Val::Percent(100.),
                                    justify_content: JustifyContent::Center,
                                    align_items: AlignItems::Center,
                                    margin: UiRect::all(Val::Px(2.)),
                                    padding: UiRect::all(Val::Px(7.)),
                                    ..default()
                                },
                                DEFAULT_BACKGROUND_COLOR,
                            ))
                            .with_children(|parent| {
                                parent.spawn((
                                    Text::new(format!("{state:?}")),
                                    assets.text_font().with_font_size(19.),
                                    DEFAULT_TEXT_COLOR,
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

    let initial_state = match &last_state.0 {
        None | Some(PersistentState::Rainbow) => ColorState::Rainbow,
        Some(PersistentState::Pick { .. }) => ColorState::Pick,
    };

    commands.set_state(initial_state);
}

#[derive(Reflect, Debug, Clone, Copy)]
enum PickButtonAction {
    AddColor,
    Randomize,
}

impl PickButtonAction {
    const ALL: &[PickButtonAction] = &[PickButtonAction::AddColor, PickButtonAction::Randomize];

    fn caption(&self) -> &'static str {
        match *self {
            PickButtonAction::AddColor => "Add color",
            PickButtonAction::Randomize => "Randomize",
        }
    }
}

#[derive(Reflect, Debug, Component, Clone)]
struct PickButton(PickButtonAction);

#[derive(Reflect, Debug, Component, Clone)]
struct RemoveColorButton(Entity);

#[derive(Reflect, Debug, Component, Clone)]
struct AdjustColorWeight(Entity, isize);

#[derive(Reflect, Debug, Component, Clone)]
struct ColorsNodeArea;

fn show_pick_buttons(
    mut did_first_init: Local<bool>,
    last_state: Res<LastPersistentState>,
    q_area: Query<Entity, With<StateNodeArea>>,
    assets: Res<AppAssets>,
    mut commands: Commands,
) {
    let Ok(area) = q_area.get_single() else {
        return;
    };
    let mut colors_area = Entity::PLACEHOLDER;
    commands.entity(area).with_children(|parent| {
        parent
            .spawn(Node {
                margin: UiRect::horizontal(Val::Px(7.)).with_bottom(Val::Px(5.)),
                display: Display::Grid,
                grid_template_columns: vec![GridTrack::flex(1.0), GridTrack::flex(1.0)],
                ..default()
            })
            .with_children(|parent| {
                for &action in PickButtonAction::ALL {
                    parent
                        .spawn((
                            Button,
                            PickButton(action),
                            assets.make_borders(8, BUTTON_BORDER_COLOR),
                            Node {
                                margin: UiRect::all(Val::Px(2.)),
                                padding: UiRect::all(Val::Px(7.)),
                                justify_content: JustifyContent::Center,
                                align_items: AlignItems::Center,
                                ..default()
                            },
                            DEFAULT_BACKGROUND_COLOR,
                        ))
                        .with_children(|parent| {
                            parent.spawn((
                                Text::new(action.caption()),
                                assets.text_font().with_font_size(15.5),
                                DEFAULT_TEXT_COLOR,
                            ));
                        });
                }
            });
        colors_area = parent
            .spawn((
                Node {
                    flex_direction: FlexDirection::Column,
                    margin: UiRect::horizontal(Val::Px(13.)),
                    ..default()
                },
                ColorsNodeArea,
            ))
            .id();
    });

    if !*did_first_init {
        if let Some(PersistentState::Pick { colors, .. }) = &last_state.0 {
            for &PersistentColor {
                color,
                weight,
                n_claws,
            } in colors
            {
                add_color(&mut commands, &assets, colors_area, color, weight, n_claws);
            }
        }
        *did_first_init = true;
    }
}

fn clear_state_area(q_area: Query<Entity, With<StateNodeArea>>, mut commands: Commands) {
    let Ok(area) = q_area.get_single() else {
        return;
    };
    commands.entity(area).despawn_descendants();
}

#[derive(Reflect, Debug, Component, Clone)]
struct SelectedColor {
    selected: Color,
    weight: usize,
    total_weight: usize,
    n_claws: usize,
}

impl Default for SelectedColor {
    fn default() -> Self {
        Self {
            selected: Default::default(),
            weight: 1,
            total_weight: 1,
            n_claws: 0,
        }
    }
}

impl SelectedColor {
    fn caption(&self) -> String {
        let pct = self.weight as f32 / self.total_weight as f32 * 100.;
        format!(
            "selected: {}, weight: {} in {} ≈ {:.1}%",
            self.n_claws, self.weight, self.total_weight, pct
        )
    }
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
    mut q_colors: Query<&mut SelectedColor>,
    mut q_claws: Query<&mut Sprite, With<FoxClaw>>,
    assets: Res<AppAssets>,
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
                let color = pick_random_color(&mut rng.0);
                add_color(&mut commands, &assets, area, color, 1, 0);
            }
            PickButtonAction::Randomize => {
                let mut colors = q_colors
                    .iter_mut()
                    .map(|selected| (selected, 0usize))
                    .collect::<Vec<_>>();
                if colors.is_empty() {
                    return;
                }
                for mut sprite in &mut q_claws {
                    let selection = colors
                        .choose_weighted_mut(&mut rng.0, |s| s.0.weight)
                        .unwrap();
                    sprite.color = selection.0.selected;
                    selection.1 += 1;
                }
                for (mut selection, n_claws) in colors {
                    selection.n_claws = n_claws;
                }
            }
        }
    }
}

fn add_color(
    commands: &mut Commands,
    assets: &AppAssets,
    area: Entity,
    color: Color,
    weight: usize,
    n_claws: usize,
) {
    commands.entity(area).with_children(|parent| {
        parent
            .spawn(Node {
                display: Display::Grid,
                grid_template_columns: vec![GridTrack::flex(1.0), GridTrack::auto()],
                ..default()
            })
            .with_children(|parent| {
                let color_node = parent.parent_entity();
                parent
                    .spawn((
                        Button,
                        Node {
                            width: Val::Percent(100.),
                            margin: UiRect::all(Val::Px(2.)),
                            padding: UiRect::all(Val::Px(2.)).with_left(Val::Px(5.)),
                            ..default()
                        },
                        SelectedColor {
                            selected: color,
                            weight,
                            n_claws,
                            ..default()
                        },
                        BackgroundColor(color),
                    ))
                    .with_child((
                        Text::new(""),
                        assets.text_font().with_font_size(16.),
                        DEFAULT_TEXT_COLOR,
                    ));
                parent
                    .spawn((
                        Button,
                        RemoveColorButton(color_node),
                        assets.make_borders(1, BUTTON_BORDER_COLOR),
                        Node {
                            width: Val::Px(23.),
                            margin: UiRect::all(Val::Px(2.)).with_left(Val::Px(5.)),
                            justify_content: JustifyContent::Center,
                            align_items: AlignItems::Center,
                            ..default()
                        },
                        DEFAULT_BACKGROUND_COLOR,
                    ))
                    .with_children(|parent| {
                        parent.spawn((
                            Text::new("x"),
                            assets.text_font().with_font_size(14.),
                            DEFAULT_TEXT_COLOR,
                        ));
                    });
            });
    });
}

fn total_color_weights(mut q_colors: Query<&mut SelectedColor>) {
    let total_weight: usize = q_colors.iter().map(|s| s.weight).sum();
    for mut selection in &mut q_colors {
        if selection.bypass_change_detection().total_weight == total_weight {
            continue;
        }
        selection.total_weight = total_weight;
    }
}

fn show_color_caption(
    q_colors: Query<(&SelectedColor, &Children), Changed<SelectedColor>>,
    mut q_text: Query<(&mut Text, &mut TextColor)>,
) {
    for (selection, children) in &q_colors {
        let Ok((mut text, mut text_color)) = q_text.get_mut(children[0]) else {
            continue;
        };
        **text = selection.caption();
        *text_color = if selection.selected.luminance() >= 0.5 {
            DEFAULT_TEXT_COLOR
        } else {
            LIGHT_TEXT_COLOR
        };
    }
}

#[derive(Debug, Default)]
struct ColorClickedState {
    last_picker: Option<Entity>,
}

#[derive(Debug, Clone, Component, Reflect)]
struct ColorPicker {
    source: Entity,
    color: Hsla,
}

#[derive(Debug, Copy, Clone, Reflect)]
enum HslaAttribute {
    Hue,
    Saturation,
    Lightness,
}

impl HslaAttribute {
    const ALL: &[HslaAttribute] = &[
        HslaAttribute::Hue,
        HslaAttribute::Saturation,
        HslaAttribute::Lightness,
    ];

    fn derive_poles(&self, color: Hsla) -> (Hsla, Hsla, Hsla) {
        (
            self.with_current(0., color),
            self.with_current(0.5, color),
            self.with_current(1., color),
        )
    }

    fn derive_poles_vec4(&self, color: Hsla) -> (Vec4, Vec4, Vec4) {
        let (a, b, c) = self.derive_poles(color);
        (a.to_vec4(), b.to_vec4(), c.to_vec4())
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

    fn format_current(&self, color: Hsla) -> String {
        match *self {
            HslaAttribute::Hue => format!(" {:.1}° ", color.hue),
            HslaAttribute::Saturation => format!(" {:.1}% ", color.saturation * 100.),
            HslaAttribute::Lightness => format!(" {:.1}% ", color.lightness * 100.),
        }
    }

    fn with_current(&self, v: f32, color: Hsla) -> Hsla {
        match *self {
            HslaAttribute::Hue => color.with_hue(v * 360.),
            HslaAttribute::Saturation => color.with_saturation(v),
            HslaAttribute::Lightness => color.with_lightness(v),
        }
    }
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
    assets: Res<AppAssets>,
) {
    for (entity, &interaction) in &q_changed {
        let Ok((parent, selected)) = q_colors.get(entity) else {
            continue;
        };
        if interaction != Interaction::Pressed {
            continue;
        }
        if let Some(last_entity) = local.last_picker.take() {
            if let Ok(prev) = q_picker.get(last_entity) {
                commands.entity(last_entity).despawn_recursive();
                if prev.source == entity {
                    continue;
                }
            }
        }
        let mut picker = Entity::PLACEHOLDER;
        commands.entity(**parent).with_children(|parent| {
            let color = selected.selected.into();
            parent
                .spawn((
                    Node {
                        grid_column: GridPlacement::span(2),
                        display: Display::Grid,
                        grid_template_columns: vec![GridTrack::flex(1.0), GridTrack::flex(1.0)],
                        ..default()
                    },
                    ColorPicker {
                        source: entity,
                        color,
                    },
                ))
                .with_children(|parent| {
                    picker = parent.parent_entity();
                    for &attr in HslaAttribute::ALL {
                        let (color1, color2, color3) = attr.derive_poles_vec4(color);
                        let current = attr.current(color);
                        parent
                            .spawn((
                                Node {
                                    left: Val::Percent(2.),
                                    width: Val::Percent(96.),
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
                                            width: Val::Px(25.),
                                            justify_content: JustifyContent::Center,
                                            align_items: AlignItems::Center,
                                            ..default()
                                        },
                                        PICKER_BACKGROUND_COLOR,
                                    ))
                                    .with_child((
                                        Text::new(attr.short_name()),
                                        assets.text_font().with_font_size(21.),
                                        DEFAULT_TEXT_COLOR,
                                    ));
                                parent
                                    .spawn((
                                        Button,
                                        Node {
                                            width: Val::Percent(100.),
                                            margin: UiRect::all(Val::Px(1.)),
                                            justify_content: JustifyContent::Center,
                                            align_items: AlignItems::Center,
                                            ..default()
                                        },
                                        MaterialNode(materials.add(ColorGradientMaterial {
                                            color1,
                                            color2,
                                            color3,
                                            color_selected: color.to_vec4(),
                                            slider: assets.slider.clone_weak(),
                                            slider_position: Vec4::splat(current),
                                        })),
                                    ))
                                    .with_child((
                                        Node {
                                            top: Val::Px(7.),
                                            ..default()
                                        },
                                        Text::new(attr.format_current(color)),
                                        assets.text_font().with_font_size(13.),
                                        DEFAULT_TEXT_COLOR,
                                        BackgroundColor(Color::hsla(0., 0., 0.9, 0.5)),
                                    ));
                            });
                    }
                    parent
                        .spawn((Node {
                            left: Val::Percent(2.),
                            width: Val::Percent(96.),
                            display: Display::Grid,
                            grid_template_columns: vec![
                                GridTrack::auto(),
                                GridTrack::flex(1.0),
                                GridTrack::flex(1.0),
                            ],
                            ..default()
                        },))
                        .with_children(|parent| {
                            parent
                                .spawn((
                                    Node {
                                        margin: UiRect::all(Val::Px(1.)),
                                        padding: UiRect::all(Val::Px(3.)),
                                        justify_content: JustifyContent::Center,
                                        align_items: AlignItems::Center,
                                        ..default()
                                    },
                                    PICKER_BACKGROUND_COLOR,
                                ))
                                .with_child((
                                    Text::new("weight"),
                                    assets.text_font().with_font_size(17.5),
                                    DEFAULT_TEXT_COLOR,
                                ));

                            for weight in [-1, 1] {
                                parent
                                    .spawn((
                                        Button,
                                        AdjustColorWeight(entity, weight),
                                        assets.make_borders(1, BUTTON_BORDER_COLOR),
                                        Node {
                                            margin: UiRect::all(Val::Px(3.)),
                                            padding: UiRect::all(Val::Px(3.)),
                                            justify_content: JustifyContent::Center,
                                            align_items: AlignItems::Center,
                                            ..default()
                                        },
                                        DEFAULT_BACKGROUND_COLOR,
                                    ))
                                    .with_children(|parent| {
                                        parent.spawn((
                                            Text::new(format!("{weight:+}")),
                                            assets.text_font().with_font_size(17.),
                                            DEFAULT_TEXT_COLOR,
                                        ));
                                    });
                            }
                        });
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
    #[uniform(5)]
    color_selected: Vec4,
    #[texture(3)]
    #[sampler(4)]
    slider: Handle<Image>,
    #[uniform(6)]
    slider_position: Vec4,
}

enum BarInteraction {
    Never,
    Always,
    OnOverlap,
}

trait PointerLocation {
    fn should_interact(&self, interaction: Interaction) -> BarInteraction;
    fn location(&self) -> &Location;
}

impl PointerLocation for Pointer<Click> {
    fn should_interact(&self, interaction: Interaction) -> BarInteraction {
        if interaction == Interaction::Hovered {
            BarInteraction::Always
        } else {
            BarInteraction::OnOverlap
        }
    }

    fn location(&self) -> &Location {
        &self.pointer_location
    }
}

impl PointerLocation for Pointer<Down> {
    fn should_interact(&self, interaction: Interaction) -> BarInteraction {
        if interaction == Interaction::Hovered {
            BarInteraction::Always
        } else {
            BarInteraction::OnOverlap
        }
    }

    fn location(&self) -> &Location {
        &self.pointer_location
    }
}

impl PointerLocation for Pointer<Move> {
    fn should_interact(&self, interaction: Interaction) -> BarInteraction {
        if interaction == Interaction::Pressed {
            BarInteraction::Always
        } else {
            BarInteraction::Never
        }
    }

    fn location(&self) -> &Location {
        &self.pointer_location
    }
}

fn gradient_drag<E: PointerLocation + std::fmt::Debug>(
    ev: Trigger<E>,
    q_window: Query<&Window>,
    mut q_selected: Query<(&mut SelectedColor, &mut BackgroundColor)>,
    mut q_picker: Query<&mut ColorPicker>,
    mut q_parent: Query<&mut ColorPickerAttribute>,
    mut q_text: Query<&mut Text>,
    q_nodes: Query<
        (
            &Interaction,
            &Parent,
            &Children,
            &ComputedNode,
            &GlobalTransform,
        ),
        With<MaterialNode<ColorGradientMaterial>>,
    >,
) {
    for (&interaction, parent, children, node, global_transform) in &q_nodes {
        let test_overlap = match ev.should_interact(interaction) {
            BarInteraction::Never => continue,
            BarInteraction::Always => false,
            BarInteraction::OnOverlap => true,
        };
        let loc = ev.location();
        let NormalizedRenderTarget::Window(window) = loc.target else {
            continue;
        };
        let Ok(window) = q_window.get(window.entity()) else {
            return;
        };
        let pointer_loc = loc.position * window.scale_factor();
        // info!("event: {ev:?}");
        let node_size = node.size();
        // factor out slider size
        let slider_width = node_size.y / 34. * 21.;
        let bar_size = node_size - Vec2::new(slider_width, 0.);
        let bar_rect = Rect::from_center_size(
            global_transform.compute_transform().translation.xy(),
            bar_size,
        );
        if test_overlap && !bar_rect.contains(pointer_loc) {
            continue;
        }
        let current = ((pointer_loc.x - bar_rect.min.x) / bar_size.x).clamp(0., 1.);
        let Ok(mut attr_picker) = q_parent.get_mut(**parent) else {
            continue;
        };
        let Ok(mut picker) = q_picker.get_mut(attr_picker.picker) else {
            continue;
        };
        let Ok(mut selected) = q_selected.get_mut(picker.source) else {
            continue;
        };
        // too many updates in one system
        attr_picker.current = current;
        picker.color = attr_picker.attr.with_current(current, picker.color);
        selected.0.selected = picker.color.into();
        selected.1 .0 = selected.0.selected;
        for &child in children {
            let Ok(mut text) = q_text.get_mut(child) else {
                continue;
            };
            **text = attr_picker.attr.format_current(picker.color);
        }
    }
}

fn update_gradient(
    q_picker: Query<&ColorPicker>,
    q_parent: Query<&ColorPickerAttribute>,
    q_nodes: Query<(&MaterialNode<ColorGradientMaterial>, &Parent)>,
    mut materials: ResMut<Assets<ColorGradientMaterial>>,
) {
    for (mat, parent) in &q_nodes {
        let Some(mat) = materials.get_mut(mat.id()) else {
            continue;
        };
        let Ok(attr_picker) = q_parent.get(**parent) else {
            continue;
        };
        let Ok(picker) = q_picker.get(attr_picker.picker) else {
            continue;
        };
        (mat.color1, mat.color2, mat.color3) = attr_picker.attr.derive_poles_vec4(picker.color);
        mat.slider_position[0] = attr_picker.current;
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

fn adjust_color_weight_clicked(
    q_buttons: Query<(&AdjustColorWeight, &Interaction), Changed<Interaction>>,
    mut q_selected: Query<&mut SelectedColor>,
) {
    for (button, &interaction) in &q_buttons {
        if let Interaction::Pressed = interaction {
            let Ok(mut selected) = q_selected.get_mut(button.0) else {
                continue;
            };
            selected.weight = selected.weight.saturating_add_signed(button.1).max(1);
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

const CREDITS_FONT_SIZE: f32 = 14.;
const CREDITS_LINE_HEIGHT: f32 = CREDITS_FONT_SIZE + 1.;

fn apply_cubehelix(
    q_claws: Query<(Entity, &FoxClaw)>,
    q_area: Query<Entity, With<StateNodeArea>>,
    assets: Res<AppAssets>,
    credits: Res<Credits>,
    mut commands: Commands,
) {
    let Ok(area) = q_area.get_single() else {
        return;
    };
    for (entity, claw) in &q_claws {
        commands.entity(entity).insert(CubehelixClaw {
            phase: claw.phase(),
            ..default()
        });
    }
    commands.entity(area).with_children(|parent| {
        parent
            .spawn(Node {
                margin: UiRect::horizontal(Val::Px(7.)).with_bottom(Val::Px(5.)),
                flex_direction: FlexDirection::Column,
                align_self: AlignSelf::Stretch,
                overflow: Overflow::scroll_y(),
                ..default()
            })
            .with_children(|parent| {
                for node in &credits.nodes {
                    parent
                        .spawn((node.node.clone(), PickingBehavior {
                            should_block_lower: false,
                            is_hoverable: true,
                        }))
                        .with_children(|parent| {
                            parent
                                .spawn((Text::default(), TextLayout::default(), PickingBehavior {
                                    should_block_lower: false,
                                    is_hoverable: true,
                                }))
                                .with_children(|parent| {
                                    for bundle in &node.spans {
                                        let mut bundle = bundle.clone();
                                        bundle.font.font = assets.font.clone_weak();
                                        parent.spawn(bundle);
                                    }
                                });
                        });
                }
            });
    });
}

fn update_scroll_position(
    mut mouse_wheel_events: EventReader<MouseWheel>,
    hover_map: Res<HoverMap>,
    mut scrolled_node_query: Query<&mut ScrollPosition>,
    keyboard_input: Res<ButtonInput<KeyCode>>,
) {
    for mouse_wheel_event in mouse_wheel_events.read() {
        let (mut dx, mut dy) = match mouse_wheel_event.unit {
            MouseScrollUnit::Line => (
                mouse_wheel_event.x * CREDITS_LINE_HEIGHT,
                mouse_wheel_event.y * CREDITS_LINE_HEIGHT,
            ),
            MouseScrollUnit::Pixel => (mouse_wheel_event.x, mouse_wheel_event.y),
        };

        if keyboard_input.pressed(KeyCode::ControlLeft)
            || keyboard_input.pressed(KeyCode::ControlRight)
        {
            std::mem::swap(&mut dx, &mut dy);
        }

        for (_pointer, pointer_map) in hover_map.iter() {
            for (entity, _hit) in pointer_map.iter() {
                if let Ok(mut scroll_position) = scrolled_node_query.get_mut(*entity) {
                    scroll_position.offset_x -= dx;
                    scroll_position.offset_y -= dy;
                }
            }
        }
    }
}

fn update_scroll_position_drag(
    mut ev: Trigger<Pointer<Drag>>,
    hover_map: Res<HoverMap>,
    mut scrolled_node_query: Query<&mut ScrollPosition>,
    q_window: Query<&Window>,
) {
    let NormalizedRenderTarget::Window(window) = ev.pointer_location.target else {
        return;
    };
    let Ok(window) = q_window.get(window.entity()) else {
        return;
    };
    let delta = ev.delta / window.scale_factor();
    for (_pointer, pointer_map) in hover_map.iter() {
        for (entity, _hit) in pointer_map.iter() {
            if let Ok(mut scroll_position) = scrolled_node_query.get_mut(*entity) {
                scroll_position.offset_x -= delta.x;
                scroll_position.offset_y -= delta.y;
            }
        }
    }
    ev.propagate(false);
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

#[derive(Debug, Resource, Deref)]
struct AssetBarrier(Arc<AssetBarrierInner>);

#[derive(Debug, Deref)]
struct AssetBarrierGuard(Arc<AssetBarrierInner>);

#[derive(Debug, Resource)]
struct AssetBarrierInner {
    count: AtomicU32,
}

impl AssetBarrier {
    pub fn new() -> (AssetBarrier, AssetBarrierGuard) {
        let inner = Arc::new(AssetBarrierInner {
            count: AtomicU32::new(1),
        });
        (AssetBarrier(inner.clone()), AssetBarrierGuard(inner))
    }

    pub fn assets_remaining(&self) -> u32 {
        self.count.load(Ordering::Acquire).saturating_sub(1)
    }
}

impl Clone for AssetBarrierGuard {
    fn clone(&self) -> Self {
        self.count.fetch_add(1, Ordering::AcqRel);
        AssetBarrierGuard(self.0.clone())
    }
}

impl Drop for AssetBarrierGuard {
    fn drop(&mut self) {
        self.count.fetch_sub(1, Ordering::AcqRel);
    }
}

fn assets_loaded(barrier: Option<Res<AssetBarrier>>) -> bool {
    barrier.map(|b| b.assets_remaining()) == Some(0)
}

#[derive(Debug, Default, Component, Reflect)]

struct AssetLoaderNode;

#[derive(Debug, Default, Component, Reflect)]
struct AssetLoaderText;

fn asset_loader_update(
    asset_barrier: Res<AssetBarrier>,
    mut commands: Commands,
    q_loader_node: Query<Entity, With<AssetLoaderNode>>,
    mut q_text: Query<&mut Text, With<AssetLoaderText>>,
) {
    match asset_barrier.assets_remaining() {
        0 => {
            commands.remove_resource::<AssetBarrier>();
            let Ok(loader_node) = q_loader_node.get_single() else {
                return;
            };
            commands.entity(loader_node).despawn_recursive();
        }
        n => {
            let Ok(mut text) = q_text.get_single_mut() else {
                return;
            };
            **text = format!("loading assets: {n} remaining");
        }
    }
}

#[derive(Debug, Clone, Reflect)]
struct CreditsNode {
    node: Node,
    spans: Vec<CreditsSpanBundle>,
}

#[derive(Debug, Clone, Bundle, Reflect)]
struct CreditsSpanBundle {
    text: TextSpan,
    color: TextColor,
    font: TextFont,
}

#[derive(Resource, Reflect)]
#[reflect(Resource)]
struct Credits {
    nodes: Vec<CreditsNode>,
}

impl Credits {
    fn parse_credits() -> Self {
        use pulldown_cmark::{CowStr, Event, Options, Parser, Tag, TagEnd, TextMergeStream};
        static CREDITS: &'static str = include_str!("../../CREDITS.md");

        #[derive(Debug, Default)]
        struct MarkdownBuilder {
            nodes: Vec<CreditsNode>,
            spans: Vec<CreditsSpanBundle>,
            stack: Vec<TagEnd>,
        }

        impl MarkdownBuilder {
            fn process_event(&mut self, ev: Event) {
                match ev {
                    Event::Start(tag) => {
                        self.start_tag(tag);
                    }
                    Event::End(tag) => {
                        self.end_tag(tag);
                    }
                    Event::Text(t) => {
                        self.text(t);
                    }
                    _ => {}
                }
            }

            fn start_tag(&mut self, tag: Tag) {
                // info!("push: {tag:?}");
                self.stack.push(tag.to_end());
            }

            fn end_tag(&mut self, tag: TagEnd) {
                // info!("pop: {tag:?}");
                assert_eq!(self.stack.pop(), Some(tag));
                match tag {
                    TagEnd::Item => {
                        self.take_spans(Node {
                            margin: UiRect::left(Val::Px(15.)).with_top(Val::Px(2.)),
                            ..default()
                        });
                    }
                    TagEnd::BlockQuote(_) => {
                        self.take_spans(Node {
                            margin: UiRect::vertical(Val::Px(7.)).with_left(Val::Px(25.)),
                            ..default()
                        });
                    }
                    TagEnd::Paragraph if self.stack.is_empty() => {
                        self.take_spans(Node {
                            margin: UiRect::vertical(Val::Px(3.)),
                            ..default()
                        });
                    }
                    TagEnd::Heading(_) => {
                        self.take_spans(Node {
                            margin: UiRect::top(Val::Px(10.)).with_bottom(Val::Px(2.)),
                            ..default()
                        });
                    }
                    TagEnd::DefinitionListTitle => {
                        self.take_spans(Node {
                            margin: UiRect::top(Val::Px(5.)),
                            ..default()
                        });
                    }
                    TagEnd::DefinitionListDefinition => {
                        self.take_spans(Node {
                            margin: UiRect::left(Val::Px(10.)),
                            ..default()
                        });
                    }
                    _ if self.stack.is_empty() => {
                        self.take_spans(Node::default());
                    }
                    _ => {}
                }
            }

            fn take_spans(&mut self, node: Node) {
                let spans = std::mem::replace(&mut self.spans, Vec::new());
                if !spans.is_empty() {
                    self.nodes.push(CreditsNode { node, spans });
                }
            }

            fn text(&mut self, text: CowStr) {
                // info!("stack at: {:#?}, text: {text:?}", self.stack);
                let mut font_size = CREDITS_FONT_SIZE;
                for tag in &self.stack {
                    match tag {
                        &TagEnd::Heading(level) => {
                            let scale = 1. + (7. - (level as u8 as f32)) / 6.;
                            // info!("scaling {font_size} * {scale} = {}", font_size * scale);
                            font_size *= scale;
                        }
                        &TagEnd::DefinitionListTitle => font_size *= 1.2,
                        _ => {}
                    }
                }
                self.spans.push(CreditsSpanBundle {
                    text: TextSpan::new(text),
                    color: DEFAULT_TEXT_COLOR,
                    font: TextFont::from_font_size(font_size),
                });
            }

            fn into_credits(self) -> Credits {
                Credits { nodes: self.nodes }
            }
        }

        let parser = TextMergeStream::new(
            Parser::new_ext(
                CREDITS,
                Options::ENABLE_DEFINITION_LIST | Options::ENABLE_SMART_PUNCTUATION,
            )
            .map(|e| match e {
                Event::SoftBreak => Event::Text(" ".into()),
                e => e,
            }),
        );
        let mut builder = MarkdownBuilder::default();
        for ev in parser {
            builder.process_event(ev);
        }
        builder.into_credits()
    }
}

fn setup(
    mut commands: Commands,
    asset_server: Res<AssetServer>,
    mut texture_atlases: ResMut<Assets<TextureAtlasLayout>>,
    mut animation_graphs: ResMut<Assets<AnimationGraph>>,
    pkv: Res<bevy_pkv::PkvStore>,
) {
    commands.spawn(Camera2d);
    let player = commands
        .spawn((
            AnimationPlayer::default(),
            AnimationGraphHandle(animation_graphs.add(AnimationGraph::new())),
        ))
        .id();

    commands.spawn(CubehelixBundle::new(player));

    let asset_guard;
    commands.insert_resource({
        let barrier;
        (barrier, asset_guard) = AssetBarrier::new();
        barrier
    });

    commands
        .spawn((AssetLoaderNode, Node {
            width: Val::Percent(100.),
            height: Val::Percent(100.),
            justify_content: JustifyContent::Center,
            align_items: AlignItems::Center,
            ..default()
        }))
        .with_children(|parent| {
            parent.spawn(Node { ..default() }).with_child((
                AssetLoaderText,
                Text::new("loading assets"),
                TextFont::from_font_size(50.),
            ));
        });

    let last_state = match pkv.get(PersistentState::KEY) {
        Ok(s) => Some(s),
        Err(e) => {
            warn!("couldn't load persistent state: {e:?}");
            None
        }
    };
    commands.insert_resource(LastPersistentState(last_state));

    commands.insert_resource(Credits::parse_credits());

    commands.insert_resource({
        let fox_paw_images = FOX_PAW_IMAGES
            .iter()
            .map(|&path| asset_server.load_acquire(path, asset_guard.clone()))
            .collect();
        let borders = asset_server.load_acquire("fantasy_ui_border_sheet.png", asset_guard.clone());
        let slider = asset_server.load_acquire("slider.png", asset_guard.clone());
        let font = asset_server.load_acquire("FiraSans-Medium.ttf", asset_guard.clone());
        let atlas_layout = TextureAtlasLayout::from_grid(
            UVec2::splat(48),
            6,
            6,
            Some(UVec2::splat(4)),
            Some(UVec2::splat(2)),
        );
        let border_atlas_layout = texture_atlases.add(atlas_layout);
        let border_slicer = TextureSlicer {
            border: BorderRect::square(24.0),
            center_scale_mode: SliceScaleMode::Stretch,
            sides_scale_mode: SliceScaleMode::Stretch,
            max_corner_scale: 1.0,
        };

        AppAssets {
            fox_paw_images,
            borders,
            slider,
            font,
            border_atlas_layout,
            border_slicer,
        }
    });
}
